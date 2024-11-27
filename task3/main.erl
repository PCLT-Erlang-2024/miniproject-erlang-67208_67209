-module(main).
-include("package.hrl").
-include("truck.hrl").
-include("counter.hrl").
-export([start/0]).

-define(DEFAULT_NUM_BELTS, 3).
-define(DEFAULT_TRUCK_CAPACITY, 10).
-define(PACKAGE_GENERATION_TIME, 1000).
-define(TRUCK_REPLACEMENT_TIME, 3000).


% package generator process code here
package_generator_process(CounterProcess) ->
    receive
        {BeltProcess} -> % wait for package request
            timer:sleep(?PACKAGE_GENERATION_TIME),
            io:format("[PACKAGE] - Generating package for Belt ~p~n", [BeltProcess]),
            CounterProcess ! {self(), increment}, % increment counter
            receive {CounterProcess, CounterValue} -> % wait for counter value
                Package = package:generate_package(CounterValue),
                io:format("[PACKAGE] - Details: ~p~n", [Package]),
                BeltProcess ! {package, Package}
            end,
            package_generator_process(CounterProcess);
        stop -> true
    end.

belt_process(PackageGenerator) ->
    receive
        {truck, Truck} -> % on truck dock
            io:format("[BELT] - ~p received Truck ~p~n", [self(), Truck]),
            belt_transfer_packages(Truck, PackageGenerator),
            belt_process(PackageGenerator);
        stop ->
            io:format("[BELT] - ~p stopping~n", [self()]),
            true
    end.

% will send packages to a given Truck until 'stop'
belt_transfer_packages(TruckProcess, PackageGenerator) ->
    PackageGenerator ! {self()}, % package request
    receive
        {package, Package} -> % package result
            io:format("[BELT] - ~p sending Package ~p to Truck ~p~n", [self(), Package, TruckProcess]),
            TruckProcess ! {self(), Package},
            belt_transfer_packages(TruckProcess, PackageGenerator);
        stop ->
            io:format("[BELT] - ~p received stop signal~n", [self()]),
            true
    end.


truck_process(Truck) ->
    io:format("[TRUCK] - Occupied size: ~p~n", [truck:get_size(Truck)]),
    % Start receiving messages
    receive
        {From, Package} ->
            io:format("[TRUCK] - ~p received Package ~p from Belt ~p~n", [self(), Package, From]),
            UpdatedTruck = truck:add_package(Package, Truck),
            case truck:is_full(UpdatedTruck) of
                true ->
                    io:format("[TRUCK] - ~p is full, sending stop to Belt ~p~n", [self(), From]),
                    From ! stop,
                    timer:sleep(TRUCK_REPLACEMENT_TIME),
                    From ! {truck, self()},
                    truck_process(truck:new(?DEFAULT_TRUCK_CAPACITY));
                false ->
                    ok
            end,
            % Continue processing
            truck_process(UpdatedTruck);
        stop ->
            true
    end.

start() ->
    % code starts here
    % create DEFAULT_NUM_BELTS belt processes
    io:format("[INFO] - Starting system with ~p belts~n", [?DEFAULT_NUM_BELTS]),
    Counter = counter:new(),
    CounterProcess = spawn(fun() -> counter:counter_process(Counter) end),
    Generators = [spawn(fun() -> package_generator_process(CounterProcess) end) || _ <- lists:seq(1, ?DEFAULT_NUM_BELTS)], % Belt <- Belts
    Belts = [spawn(fun() -> belt_process(Generator) end) || Generator <- Generators],
    io:format("[INFO] - Created belts: ~p~n", [Belts]),
    io:format("[INFO] - Package generator processes started~n"),
    Trucks =[truck:new(?DEFAULT_TRUCK_CAPACITY) || _ <- lists:seq(1, ?DEFAULT_NUM_BELTS)],
    TruckProcesses = [spawn(fun() -> truck_process(Truck) end) || Truck <- Trucks],
    io:format("[INFO] - Truck processes started~n"),
    lists:zipwith(fun(Belt, Truck) -> Belt ! {truck, Truck} end, Belts, TruckProcesses).
