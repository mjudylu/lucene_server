%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @doc Lucene Utility Functions
%%% @end
%%%-------------------------------------------------------------------
-module(lucene_utils).
-author('elbrujohalcon@inaka.net').

-include("lucene.hrl").

-export([geo/2, lat/1, lng/1]).

%% @doc Creates a new {@link lucene:geo()}
-spec geo(float(), float()) -> lucene:geo().
geo(Lat, Lng) -> #geo{lat = Lat, lng = Lng}.

%% @doc Gets the latitude of a {@link lucene:geo()}
-spec lat(lucene:geo()) -> float().
lat(Geo) -> Geo#geo.lat.

%% @doc Gets the longitude of a {@link lucene:geo()}
-spec lng(lucene:geo()) -> float().
lng(Geo) -> Geo#geo.lng.