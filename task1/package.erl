-module(package).
-include("package.hrl").
-export([generate_package/1]).

generate_package(Index) ->
    #package{index=Index, size=1}.
