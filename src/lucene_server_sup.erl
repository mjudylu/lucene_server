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
-spec init([]) -> {ok, {{one_for_one, 5, 60}, [supervisor:child_spec()]}}.
init([]) ->
  {ok,
    {_SupFlags = {one_for_one, 5, 60},
      [
        {lucene, {lucene, start_link, []}, permanent, 2000, worker, [lucene]}
      ]}}.