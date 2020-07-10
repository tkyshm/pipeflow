-module(pipeflow_exporter).

-type data() :: term().

-callback export(Data :: data(), Opts :: map()) -> Result :: {ok, term()} | {error, any()}.
