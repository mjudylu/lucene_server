%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @author Marc <marc@tigertext.com>
%%% @doc Lucene Server Main Supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(lucene_server_sup).
-author('elbrujohalcon@inaka.net').

-behaviour(supervisor).

-export([start_link/0, init/1]).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc  Starts a new supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
%%-------------------------------------------------------------------
%% SUPERVISOR API
%%-------------------------------------------------------------------
%% @hidden
%%NOTE: If the process dies... it can't be restarted, because all information
%%		stored in the java process may or may not be lost.
-spec init([]) -> {ok, {{one_for_one, 0, 1}, [supervisor:child_spec()]}}.
init([]) ->
  {ok,
    {_SupFlags = {one_for_one, 0, 1},
      [
        {lucene, {lucene, start_link, []}, permanent, 2000, worker, [lucene]},
        {lucene_workers, {lucene_worker, start_pool, []}, permanent, 2000, supervisor, [lucene_worker]}
      ]}}.