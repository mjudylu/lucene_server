%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @doc Lucene Interface.
%%% Use it to add/del/query documents
%%% @end
%%%-------------------------------------------------------------------
-module(lucene).
-author('elbrujohalcon@inaka.net').

%%NOTE: We let java server run for as long as it needs to run.
%%      Even if we choose a smaller timeout, it will run for a longer time anyway if it needs to.
-define(CALL_TIMEOUT, infinity).
-define(LUCENE_SERVER, {lucene_server, java_node()}).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-export([process/0]).
-export([add/1, del/1, clear/0, stop/0]).
-export([match/2, match/3, match/4, continue/2, continue/3]).

-record(state, {java_port :: port(),
                java_node :: atom()}).
-opaque state() :: #state{}.

-include("lucene.hrl").

-type page_token() :: binary().
-type metadata() :: [{page_token, page_token()} | {total_hits, non_neg_integer()} | {first_hit, non_neg_integer()}].
-type geo() :: #geo{}.
-type field_key() :: atom()|binary()|string().
-type field_value() :: number() | atom() | string() | geo().
-type doc() :: [{field_key(), field_value()},...].
-export_type([geo/0, field_key/0, field_value/0, doc/0, metadata/0, page_token/0]).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc  Starts a new monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns the pid of the lucene process
-spec process() -> pid().
process() -> gen_server:call(?LUCENE_SERVER, {pid}, ?CALL_TIMEOUT).

%% @equiv match(Query, PageSize, [])
-spec match(string(), pos_integer()) -> {[doc()], metadata()} | '$end_of_table'.
match(Query, PageSize) -> match(Query, PageSize, []).

%% @equiv match(Query, PageSize, SortFields, infinity)
-spec match(string(), pos_integer(), [atom()]) -> {[doc()], metadata()} | '$end_of_table'.
match(Query, PageSize, SortFields) -> match(Query, PageSize, SortFields, ?CALL_TIMEOUT).

%% @doc Runs a query against the lucene server
-spec match(string(), pos_integer(), [atom()], infinity | pos_integer()) -> {[doc()], metadata()} | '$end_of_table'.
match(Query, PageSize, SortFields, Timeout) -> make_call({match, normalize_unicode(Query), PageSize, SortFields}, Timeout).

%% @equiv continue(PageToken, PageSize, infinity)
-spec continue(page_token(), pos_integer()) -> {[string()], metadata()} | '$end_of_table'.
continue(PageToken, PageSize) -> continue(PageToken, PageSize, ?CALL_TIMEOUT).

%% @doc Continues a Query where it was left
-spec continue(page_token(), pos_integer(), infinity | pos_integer()) -> {[string()], metadata()} | '$end_of_table'.
continue(PageToken, PageSize, Timeout) -> make_call({continue, PageToken, PageSize}, Timeout).

%% @doc Stops the java process
-spec stop() -> ok.
stop() -> gen_server:cast(?LUCENE_SERVER, {stop}).

%% @doc Clears the whole index.
%%      <b>USE IT WITH CAUTION</b>
-spec clear() -> ok.
clear() -> gen_server:cast(?LUCENE_SERVER, {clear}).

%% @doc Registers a list of docs
-spec add([doc()]) -> ok.
add(Docs) -> gen_server:cast(?LUCENE_SERVER, {add, [normalize(Doc) || Doc <- Docs]}).

%% @doc Removes docs matching a certain query
-spec del(string()) -> ok.
del(Query) -> gen_server:cast(?LUCENE_SERVER, {del, normalize_unicode(Query)}).

%%-------------------------------------------------------------------
%% GEN_SERVER API
%%-------------------------------------------------------------------
%% @private
-spec init([]) -> {ok, state()}.
init([]) ->
  case os:find_executable("java") of
    [] ->
      throw({stop, java_missing});
    Java ->
      ThisNode = this_node(),
      JavaNode = java_node(),
      Priv = priv_dir(lucene_server),
      Classpath = string:join([otp_lib("/OtpErlang.jar") | filelib:wildcard(Priv ++ "/*.jar")], ":"),
      Port =
        erlang:open_port({spawn_executable, Java},
                         [{line,1000}, stderr_to_stdout,
                          {args, ["-classpath", Classpath,
                                  "com.tigertext.lucene.LuceneNode",
                                  ThisNode, JavaNode, erlang:get_cookie()]}]),
      wait_for_ready(#state{java_port = Port, java_node = JavaNode})
  end.

%% @private
-spec handle_call({atom(), atom(), string(), [term()]}, _From, state()) -> {noreply, state()}.
handle_call(Call, From, State) ->
  lucene_worker:run(Call, From),
  {noreply, State}.

%% @private
-spec handle_info({nodedown, atom()}, state()) -> {stop, nodedown, state()} | {noreply, state()}.
handle_info({nodedown, JavaNode}, State = #state{java_node = JavaNode}) ->
  lager:error("Java node is down!"),
  {stop, nodedown, State};
handle_info({Port, {data, {eol, JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:info("Java Log:\t~s", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {noeol, JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:info("Java Log:\t~s...", [JavaLog]),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
%% @private
-spec terminate(_, state()) -> true.
terminate(_Reason, State) -> erlang:port_close(State#state.java_port).
%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%-------------------------------------------------------------------
%% PRIVATE
%%-------------------------------------------------------------------
%% @private
priv_dir(App) ->
  case code:priv_dir(App) of
    {error, bad_name} ->
      lager:info("Couldn't find priv dir for lucene_server, using ./priv"), "./priv";
    PrivDir -> filename:absname(PrivDir)
  end.

%% @private
%% @doc returns the absolute path to the otp erlang JAR
otp_lib(Path) ->
  JPriv = priv_dir(jinterface),
  test_priv_path(Path, file:read_file_info(JPriv ++ Path), JPriv ++ Path).

test_priv_path(_, {ok, _}, Absolute_Path) -> Absolute_Path;
test_priv_path(Path, {error, _}, _) -> filename:absname(code:lib_dir() ++ Path).

this_node() -> atom_to_list(node()).

java_node() ->
  case string:tokens(atom_to_list(node()), "@") of
    [Name, Server] -> list_to_atom(Name ++ "_java@" ++ Server);
    _Node -> throw({bad_node_name, node()})
  end.

make_call(Call, Timeout) ->
  case gen_server:call(?LUCENE_SERVER, Call, Timeout) of
    {ok, Result} -> Result;
    {error, Error} -> throw(Error)
  end.

normalize_unicode(String) ->
  case lists:dropwhile(fun(Char) -> Char =< 255 end, String) of
    [] -> String;
    _ -> binary_to_list(unicode:characters_to_binary(String))
  end.

normalize(Doc) ->
  [{case Key of
      Key when is_atom(Key) -> Key;
      Key when is_list(Key) -> list_to_atom(normalize_unicode(Key));
      Key when is_binary(Key) -> binary_to_atom(Key, utf8)
    end, validate(Value)} || {Key, Value} <- Doc].

validate(#geo{lat = Lat}) when -90.0 > Lat; Lat > 90.0 ->
  throw({invalid_latitude, Lat});
validate(#geo{lng = Lng}) when -180.0 > Lng; Lng > 180.0 ->
  throw({invalid_longitude, Lng});
validate(Value) when is_list(Value) ->
  normalize_unicode(Value);
validate(Value) ->
  Value.

wait_for_ready(State = #state{java_port = Port}) ->
  receive
    {Port, {data, {eol, "READY"}}} ->
      _ = lager:info("Java node started"),
      true = link(process()),
      true = erlang:monitor_node(State#state.java_node, true),
      {ok, State};
    Info ->
      case handle_info(Info, State) of
        {noreply, NewState} ->
          wait_for_ready(NewState);
        {stop, Reason, _NewState} ->
          {stop, Reason}
      end
  end.
