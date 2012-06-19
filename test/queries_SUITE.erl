%% @hidden
-module(queries_SUITE).

-export([all/0, distance/1, hsin/2]).

-include("lucene.hrl").

-type config() :: [{atom(), term()}].

-spec all() -> [atom()].
all() -> [distance].

-spec distance(config()) -> _.
distance(_Config) ->
  PageSize = 5,

  DsSouth = [[{g, #geo{lat=1.0 * I, lng=0.0}},   {d, s}, {i, I}] || I <- lists:seq(1, PageSize)],
  DsNorth = [[{g, #geo{lat=-1.0 * I, lng=0.0}},  {d, n}, {i, I}] || I <- lists:seq(1, PageSize)],
  DsEast  = [[{g, #geo{lat=0.0, lng=1.0 * I}},   {d, e}, {i, I}] || I <- lists:seq(1, PageSize)],
  DsWest  = [[{g, #geo{lat=0.0, lng=-1.0 * I}},  {d, w}, {i, I}] || I <- lists:seq(1, PageSize)],

  ok = lucene:add(DsSouth),
  ok = lucene:add(DsNorth),
  ok = lucene:add(DsWest),
  ok = lucene:add(DsEast),

  wait_for_docs(),

  _ = lager:info("south"),
  {JustSouth, _} = lucene:match("d:s AND g.near:0.0,0.0,10000", PageSize),
  true = is_sorted(distances(#geo{lat=0.0, lng=0.0}, JustSouth)),
  _ = lager:info("north"),
  {JustNorth, _} = lucene:match("d:n AND g.near:0.0,0.0,10000", PageSize),
  true = is_sorted(distances(#geo{lat=0.0, lng=0.0}, JustNorth)),

  _ = lager:info("0.0"),
  {All, _} = lucene:match("g.near:0.0,0.0,10000", PageSize*4),
  true = is_sorted(distances(#geo{lat=0.0, lng=0.0}, All)),

  _ = random:seed(erlang:now()),
  Lat = random_latitude(),
  Lng = random_longitude(),
  _ = lager:info("~f,~f", [Lat,Lng]),
  {AllRnd, _} = lucene:match(io_lib:format("g.near:~f,~f,10000", [Lat, Lng]), PageSize*4),
  true = is_sorted(distances(#geo{lat=Lat, lng=Lng}, AllRnd)),
  ok.

distances(Origin, Docs) ->
  [hsin(Origin, proplists:get_value(g, Doc)) || Doc <- Docs].

is_sorted([]) -> true;
is_sorted([_]) -> true;
is_sorted([X,Y|Rest]) when X =< Y -> is_sorted([Y|Rest]);
is_sorted([X,Y|_]) ->
  _ = lager:info("~p > ~p: ~p", [X, Y, X - Y]), false.

-spec hsin(lucene:geo(), lucene:geo()) -> float().
hsin(P1, P2) ->
  DLat = math:pi() * (P2#geo.lat - P1#geo.lat) / 180,
  DLng = math:pi() * (P2#geo.lng - P1#geo.lng) / 180,
  Lat1 = math:pi() * P1#geo.lat / 180,
  Lat2 = math:pi() * P2#geo.lat / 180,
  A = math:sin(DLat/2) * math:sin(DLat/2) + math:sin(DLng/2) * math:sin(DLng/2) * math:cos(Lat1) * math:cos(Lat2),
  erlang:round(2 * math:atan2(math:sqrt(A), math:sqrt(1-A)) * 3959 * 10000) / 10000.

random_longitude() -> (random:uniform(10000000) - 16500000) / 100000.
random_latitude() -> (random:uniform(3000000) + 2100000) / 100000.

wait_for_docs() -> wait_for_docs(10).
wait_for_docs(0) -> throw(tired_of_waiting);
wait_for_docs(I) ->
  try lucene:match("d:[* TO *]", 1)
  catch
    _:_ ->
      receive
        after 1000 ->
          _ = lager:info("Waiting ~p more times", [I]),
          wait_for_docs(I-1)
      end
  end.