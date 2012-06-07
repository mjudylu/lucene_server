%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @author Marc <marc@tigertext.com>
%%% @doc Lucene Server for TigerText
%%% @end
%%%-------------------------------------------------------------------
-module(lucene_server).
-author('elbrujohalcon@inaka.net').
-vsn('0.1').

-behaviour(application).

%% By default, we use gen_server's traditional timeout
-define(DEFAULT_TIMEOUT, 5000).

-type metadata() :: [{string(), binary() | non_neg_integer()}].
-export_type([metadata/0]).

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([java_node/0]).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc Name of the java node
-spec java_node() -> atom().
java_node() ->
    case application:get_env(?MODULE, java_node) of
      {ok, Node} -> Node;
      undefined -> lucene@localhost
    end.

%%-------------------------------------------------------------------
%% ADMIN API
%%-------------------------------------------------------------------
%% @doc Starts the application
-spec start() -> ok | {error, {already_started, ?MODULE}}.
start() -> application:start(?MODULE).

%% @doc Stops the application
-spec stop() -> ok.
stop() -> application:stop(?MODULE).

%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @private
-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) -> lucene_server_sup:start_link().

%% @private
-spec stop(any()) -> ok.
stop(_State) -> ok.