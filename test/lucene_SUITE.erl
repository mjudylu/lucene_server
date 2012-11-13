-module(lucene_SUITE).

-export([all/0, keys/1, stop/1, add_del_clear/1, init_per_suite/1, end_per_suite/1]).

-include("lucene.hrl").

-type config() :: [{atom(), term()}].

-spec all() -> [atom()].
all() -> [add_del_clear, keys, stop].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  lucene_server:start(),
  timer:sleep(2000),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

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