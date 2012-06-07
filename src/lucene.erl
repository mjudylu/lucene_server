%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @doc Lucene Interface
%%% @end
%%%-------------------------------------------------------------------
-module(lucene).
-author('elbrujohalcon@inaka.net').

%%NOTE: We let java server run for as long as it needs to run.
%%      Even if we choose a smaller timeout, it will run for a longer time anyway if it needs to.
-define(CALL_TIMEOUT, infinity).
-define(LUCENE_SERVER, {lucene_server, lucene_server:java_node()}).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-export([process/0]).
-export([add/1, del/1, clear/0]).
-export([match/2, match/3, continue/2, continue/3]).

-record(state, {java_node :: atom()}).
-opaque state() :: #state{}.

-type page_token() :: binary().
-export_type([page_token/0]).

-type metadata() :: [{page_token, page_token()} | {total_hits, non_neg_integer()} | {first_hit, non_neg_integer()}].

-type doc() :: [{atom(), string()},...].
-export_type([doc/0]).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc  Starts a new monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link(?MODULE, [], []).

%% @doc Returns the pid of the lucene process
-spec process() -> pid().
process() -> gen_server:call(?LUCENE_SERVER, {pid}, ?CALL_TIMEOUT).

%% @equiv match(Query, PageSize, infinity)
-spec match(string(), pos_integer()) -> {[doc()], metadata()} | '$end_of_table'.
match(Query, PageSize) -> match(Query, PageSize, ?CALL_TIMEOUT).

%% @doc Runs a query against the lucene server
-spec match(doc(), pos_integer(), infinity | pos_integer()) -> {[doc()], metadata()} | '$end_of_table'.
match(Query, PageSize, Timeout) -> gen_server:call(?LUCENE_SERVER, {match, Query, PageSize}, Timeout).

%% @equiv continue(PageToken, PageSize, infinity)
-spec continue(page_token(), pos_integer()) -> {[string()], metadata()} | '$end_of_table'.
continue(PageToken, PageSize) -> continue(PageToken, PageSize, ?CALL_TIMEOUT).

%% @doc Continues a Query where it was left
-spec continue(page_token(), pos_integer(), infinity | pos_integer()) -> {[string()], metadata()} | '$end_of_table'.
continue(PageToken, PageSize, Timeout) -> gen_server:call(?LUCENE_SERVER, {continue, PageToken, PageSize}, Timeout).

%% @doc Clears the whole index
%%      USE WITH CAUTION
-spec clear() -> ok.
clear() -> gen_server:cast(?LUCENE_SERVER, {clear}).

%% @doc Registers a list of docs
-spec add([doc()]) -> ok.
add(Docs) -> gen_server:cast(?LUCENE_SERVER, {add, Docs}).

%% @doc Removes docs matching a certain query
-spec del([doc()]) -> ok.
del(Query) -> gen_server:cast(?LUCENE_SERVER, {del, Query}).

%%-------------------------------------------------------------------
%% GEN_SERVER API
%%-------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
	JavaNode = lucene_server:java_node(),
    true = link(process()),
    true = erlang:monitor_node(JavaNode, true),
    {ok, #state{java_node = JavaNode}}.

-spec handle_call(X, _From, state()) -> {stop, {unexpected_request, X}, {unexpected_request, X}, state()}.
handle_call(X, _From, State) -> {stop, {unexpected_request, X}, {unexpected_request, X}, State}.

-spec handle_info({nodedown, atom()}, state()) -> {stop, nodedown, state()} | {noreply, state()}.
handle_info({nodedown, JavaNode}, State = #state{java_node = JavaNode}) ->
    lager:error("Java node is down!"),
    {stop, nodedown, State};
handle_info(Info, State) ->
    lager:warning("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
-spec terminate(_, state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.