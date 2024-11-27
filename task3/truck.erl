-module(truck).
-include("package.hrl").
-include("truck.hrl").
-export([new/1, is_full/1, get_size/1, add_package/2]).

% Create a new truck
new(Capacity) ->
    #truck{
        capacity = Capacity,
        packages = []
    }.

% Check if the truck is full
is_full(Truck) ->
    get_size(Truck) >= Truck#truck.capacity.

% Get the total size of the packages in the truck
get_size(Truck) ->
    lists:sum([P#package.size || P <- Truck#truck.packages]).

% Add a package to the truck
add_package(Package, Truck) ->
    Truck#truck{packages = [Package | Truck#truck.packages]}.
