-module(pipeflow).

-export([get_worker/0]).

-spec get_worker() -> pid().
get_worker() ->
    [{workers, Pids}] = ets:lookup(pipeflow_worker_pool, workers),
    lists:nth(rand:uniform(length(Pids)), Pids).
