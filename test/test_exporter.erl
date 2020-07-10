-module(test_exporter).

-behaviour(pipeflow_exporter).

-export([export/2]).

export(Data, _Conf) -> {ok, Data}.
