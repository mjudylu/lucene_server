-module(utils_SUITE).

-export([all/0, geo/1]).

-spec all() -> [atom()].
all() -> [geo].

-type config() :: [{atom(), term()}].

-spec geo(config()) -> _.
geo(_Config) ->
	Geo = lucene_utils:geo(1.0, 2.0),
	1.0 = lucene_utils:lat(Geo),
	2.0 = lucene_utils:lng(Geo).