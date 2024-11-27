-module(counter).
-include("counter.hrl").
-export([new/0, get/1, increment/1, counter_process/1]).

counter_process(Counter) ->
    receive
        {From, get} ->
            From ! {self(), counter:get(Counter)},
            counter_process(Counter);
        {From, increment} ->
            Counter2 = counter:increment(Counter),
            From ! {self(), Counter2},
            counter_process(Counter2)
    end.

new() -> #counter{value = 0}.

get(Counter) -> Counter#counter.value.

increment(Counter) -> Counter#counter{value = Counter#counter.value + 1}.
