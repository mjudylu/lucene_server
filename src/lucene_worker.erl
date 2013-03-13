%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @doc Lucene Worker Pool to run ".erlang:" queries.
%%% @end
%%%-------------------------------------------------------------------
-module (lucene_worker).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

-record(state, {}).
-type state() :: #state{}.

%% API
-export([start_pool/0, run/2]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_pool() -> {ok, pid()}.
start_pool() ->
    Workers =
        case application:get_env(lucene_server, workers) of
            {ok, Ws} -> Ws;
            undefined -> 400
        end,
    OverrunWarning =
        case application:get_env(lucene_server, workers_timeout) of
            {ok, WT} -> WT;
            undefined -> 5000
        end,
    lager:info("Starting ~p with ~p connections and timeout: ~p", [?MODULE, Workers, OverrunWarning]),
    wpool:start_pool(
        ?MODULE,
        [{overrun_warning,  OverrunWarning},
         {workers,          Workers},
         {worker,           {?MODULE, undefined}}]).

-spec run(Call :: {atom(), atom(), string(), [term()]}, From :: {pid(), term()}) -> ok.
run(Call, From) -> wpool:cast(?MODULE, {run, Call, From}).

%%%===================================================================
%%% init, terminate, code_change callbacks
%%%===================================================================
-spec init(undefined) -> {ok, state()}.
init(undefined) -> {ok, #state{}}.
-spec terminate(atom(), state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(string(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) -> {noreply, State}.
-spec handle_call(Call, {pid(), term()}, state()) -> {stop, {unexpected_call, Call}, {unexpected_call, Call}, state()}.
handle_call(Call, _From, State) -> {stop, {unexpected_call, Call}, {unexpected_call, Call}, State}.

%%%===================================================================
%%% handle_cast callbacks
%%%===================================================================
-spec handle_cast({run, Call :: {atom(), atom(), string(), [term()]}, From :: {pid(), term()}}, state()) -> {noreply, state()}.
handle_cast({run, {Mod, Fun, Args, Values} = Call, From}, State) ->
    lager:debug("Running ~p:~p(~s, ~p).", [Mod, Fun, Args, Values]),
    Reply =
        try
            {ok, Scanned, _} = erl_scan:string(Args++"."),
            {ok, Parsed} = erl_parse:parse_exprs(Scanned),
            {value, Arguments, _} = erl_eval:exprs(Parsed, []),
            erlang:apply(Mod,Fun, Arguments ++ [[parse_value(Value) || Value <- Values]])
        catch
            _:Error ->
                lager:error("Bad RPC call (~p): ~p~n\t~p", [Call, Error, erlang:get_stacktrace()]),
                {error, Error}
        end,
    gen_server:reply(From, Reply),
    {noreply, State}.

parse_value(Value) when is_binary(Value) -> binary_to_list(Value); %% Strings are sent in as binaries to avoid conversion overheads
parse_value(Value) -> Value.