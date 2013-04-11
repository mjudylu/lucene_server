-module(lucene_SUITE).

-export([all/0, keys/1, stop/1, add_del_clear/1, save/1, complete_coverage/1, init_per_suite/1, end_per_suite/1]).

-include("lucene.hrl").

-type config() :: [{atom(), term()}].

-spec all() -> [atom()].
all() -> [add_del_clear, keys, stop, save, complete_coverage].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  lucene_server:start(),
  timer:sleep(2000),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

-spec save(config()) -> _.
save(_Config) ->
	ok = file:make_dir("test_index"),
	try
		ok = lucene:save("test_index"),
		timer:sleep(2000),
		[_|_] = filelib:wildcard("test_index/*")
	after
		[file:delete(F) || F <- filelib:wildcard("test_index/*")],
		file:del_dir("test_index")
	end.

-spec complete_coverage(config()) -> _.
complete_coverage(_Config) ->
	'wpool_pool-lucene_worker-1' ! ignored_info,
	timer:sleep(500),
	{unexpected_call, call} = wpool:call(lucene_worker, call),
	{ok, state} = lucene_worker:code_change(oldvsn, state, extra),

	OldWorkers = application:get_env(lucene_server, workers),
	OldWorkersTimeout = application:get_env(lucene_server, workers_timeout),
	case OldWorkers of
		undefined -> application:set_env(lucene_server, workers, 400);
		_ -> application:unset_env(lucene_server, workers)
	end,
	case OldWorkersTimeout of
		undefined -> application:set_env(lucene_server, workers_timeout, 5000);
		_ -> application:unset_env(lucene_server, workers_timeout)
	end,

	{error, {already_started, _}} = lucene_worker:start_pool(),

	case OldWorkers of
		undefined -> application:unset_env(lucene_server, workers);
		{ok, OW} -> application:set_env(lucene_server, workers, OW)
	end,
	case OldWorkersTimeout of
		undefined -> application:unset_env(lucene_server, workers_timeout);
		{ok, OWT} -> application:set_env(lucene_server, workers_timeout, OWT)
	end,

	lucene ! ignored_info,
	timer:sleep(500),
	ok = gen_server:cast(lucene, ignored_cast),
	{ok, state} = lucene:code_change(oldvsn, state, extra),
	LucenePath = filename:join(filename:dirname(code:priv_dir(lucene_server)), "ebin"),
	try
		true = code:del_path(LucenePath),
		[Name, Server] = string:tokens(atom_to_list(node()), "@"),
		lucene ! {nodedown, list_to_atom(Name ++ "_java@" ++ Server)},
		timer:sleep(500),
		P = spawn(lucene_server, start, []),
		timer:sleep(1000),
		%% It shouldn't have been able to start...
		true = erlang:is_process_alive(P),
		lucene ! {nodedown, list_to_atom(Name ++ "_java@" ++ Server)}
	after
		code:add_patha(LucenePath),
		whereis(lucene) =/= undefined andalso exit(whereis(lucene), kill),
		lucene_server:start()
	end,
	ok.

-spec stop(config()) -> _.
stop(_Config) ->
	ok = lucene:stop(),
	receive after 2000 -> ok end,
	lucene_server:start(),

	ok = lucene_server:stop(),
	receive after 2000 -> ok end,
	lucene_server:start().

-spec keys(config()) -> _.
keys(_Config) ->
	lucene:add([[{a, [C]}, {"l", [C]}, {<<"b">>, [C]}] || C <- lists:seq($a, $c)]),
	{R, _} = lucene:match("a:[a TO c]", 3),
	{R, _} = lucene:match("l:[a TO c]", 3),
	{R, _} = lucene:match("b:[a TO c]", 3).

-spec add_del_clear(config()) -> _.
add_del_clear(_Config) ->
	lucene:add([[{i, I}] || I <- lists:seq(1, 3)]),

	{R0, M0} = lucene:match("i:[1 TO 3]", 2),
	3 = proplists:get_value(total_hits, M0),
	2 = length(R0),
	1 = proplists:get_value(first_hit, M0),
	NP = proplists:get_value(next_page, M0),

	{R1, M1} = lucene:continue(NP, 2),
	3 = proplists:get_value(total_hits, M1),
	1 = length(R1),
	3 = proplists:get_value(first_hit, M1),
	undefined = proplists:get_value(next_page, M1),

	ok = lucene:del("i:1"),
	{R2, M2} = lucene:match("i:[1 TO 3]", 2),
	2 = proplists:get_value(total_hits, M2),
	2 = length(R2),
	1 = proplists:get_value(first_hit, M2),
	undefined = proplists:get_value(next_page, M2),

	ok = lucene:clear(),

	{R3, M3} = lucene:match("i:[1 TO 3]", 2),
	0 = proplists:get_value(total_hits, M3),
	0 = length(R3),
	undefined = proplists:get_value(next_page, M3).