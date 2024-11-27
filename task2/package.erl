-module(package).
-include("package.hrl").
-export([generate_package/1]).
-define(MAX_PACKAGE_SIZE, 5).

generate_package(Index) ->
    Size = rand:uniform(?MAX_PACKAGE_SIZE),
    #package{index=Index, size=Size}.
