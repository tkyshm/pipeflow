%%%-------------------------------------------------------------------
%% @doc pipeflow public API
%% @end
%%%-------------------------------------------------------------------

-module(pipeflow_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-define(DEFAULT_WORKERS, 4).

start(_StartType, _StartArgs) ->
    ets:new(pipeflow_worker_pool, [
        public,
        named_table,
        {read_concurrency, false},
        {write_concurrency, false}
    ]),
    set_token_scopes(),

    %% TODO: 読み取れなかったケースは落とす
    ProjectID = list_to_binary(os:getenv("GCP_PROJECT_ID")),
    Sub = list_to_binary(os:getenv("PIPEFLOW_SUBSCRIPTION")),
    Exporter = list_to_atom(os:getenv("PIPEFLOW_EXPORTER", "pipeflow_gcs_exporter")),
    ExpConf = application:get_env(pipeflow, exporter_conf, #{}),
    Ret = pipeflow_sup:start_link(ProjectID, Sub, Exporter, ExpConf),

    WorkerNum = application:get_env(pipeflow, workers, ?DEFAULT_WORKERS),
    Workers = start_workers(WorkerNum),
    ets:insert(pipeflow_worker_pool, {workers, Workers}),
    pipeflow_pubsub_fetcher:start_polling(),
    Ret.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
set_token_scopes() ->
    persistent_term:put(pipeflow_scopes,
    [
     <<"https://www.googleapis.com/auth/pubsub">>,
     <<"https://www.googleapis.com/auth/cloud-platform">>,
     <<"https://www.googleapis.com/auth/devstorage.read_write">>
    ]).

start_workers(N) -> start_workers(N, []).

start_workers(0, Acc) -> Acc;
start_workers(N, Acc) ->
    {ok, Child} = supervisor:start_child(pipeflow_worker_sup, []),
    start_workers(N-1, [Child|Acc]).
