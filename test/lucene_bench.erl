%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @doc Lucene Server Benchmarks
%%% @end
%%%-------------------------------------------------------------------
-module(lucene_bench).
-author('elbrujohalcon@inaka.net').

-export([add/2, match/6, just_match/4, concurrency/6]).
-export([sleep/2]).

-spec add(pos_integer(), pos_integer()) -> float().
add(NumDocs, NumFields) ->
	Docs =
		[	[{list_to_atom("Field" ++ integer_to_list(FieldNum)),
			  "F" ++ integer_to_list(FieldNum) ++ " D" ++ integer_to_list(DocNum)}
			 || FieldNum <- lists:seq(1, NumFields)]
		 || DocNum <- lists:seq(1, NumDocs)],
	ok = lucene:add(Docs), %% It's a cast
	{Time, Result} = timer:tc(lucene, match, ["Field1:F1*", 1]),
	lager:notice("Result: ~p~n", [Result]),
	Time/1000000.

-spec match(pos_integer(), pos_integer(), pos_integer(), pos_integer(), string(), pos_integer()) -> {float(), float()}.
match(N, C, NumDocs, NumFields, Query, PageSize) ->
	lucene:clear(),
	add(NumDocs, NumFields),
	just_match(N, C, Query, PageSize).

-spec just_match(pos_integer(), pos_integer(), string(), pos_integer()) -> {float(), float()}.
just_match(N, C, Query, PageSize) ->
	Self = self(),
	[begin
		timer:sleep(1000),
		spawn(fun() -> match(C, Query, PageSize, Self) end)
	 end || _ <- lists:seq(1, N, C)],
	Timeout = trunc(N/C) * 1000 + 60000,
	Results =
		[receive
			{Sum, Max} ->
				lager:notice("Block ~p: ~..3f / ~..3f~n", [I, Sum/C, Max]),
				{Sum, Max}
		 after Timeout ->
		 	{60.0 * C, 60.0}
		 end || I <- lists:seq(1, N, C)],
	GenAvg = lists:sum([S || {S, _} <- Results]) / N,
	GenMax = lists:max([M || {_, M} <- Results]),
	{GenAvg, GenMax}.

match(C, Query, PageSize, Parent) ->
	Self = self(),
	Pids =
		[spawn(fun() -> receive P -> P ! timer:tc(lucene, match, [Query, PageSize]) end end)
	 	 || _ <- lists:seq(1, C)],
	[Pid ! Self || Pid <- Pids],
	Times =
		[receive
			{Time, {_Docs, _Metadata}} ->
				Time/1000000
		 after 60000 ->
		 	60.0
		 end || _ <- lists:seq(1, C)],
	Parent ! {lists:sum(Times), hd(lists:reverse(lists:sort(Times)))}.

-spec concurrency(pos_integer(), pos_integer(), pos_integer(), string(), pos_integer(), pos_integer()) -> {float(), [float()]}.
concurrency(NumDocs, NumFields, N, Query, CTop, CStep) ->
	io:format("Clearing the index...~n"),
	lucene:clear(),
	io:format("Filling the index...~n"),
	add(NumDocs, NumFields),
	lager:set_loglevel(lager_console_backend, warning),
	io:format("Running the benchmarks...~n"),
	lists:foreach(
		fun(I) ->
			Field = integer_to_list((I rem NumFields) + 1),
			{Avg, Max} =
				lucene_bench:just_match(N, I, Query ++ " AND Field" ++ Field ++ ":F" ++ Field ++ "*", 10),
			io:format("~3b: ~..3f / ~..3f~n", [I, Avg, Max])
		end, lists:seq(CStep, CTop, CStep)).

-spec sleep(pos_integer(), [undefined | integer()]) -> [undefined | float()].
sleep(Ms, Is) ->
	timer:sleep(Ms),
	[element(1, string:to_float(I ++ ".0")) || [_|I] <- Is].