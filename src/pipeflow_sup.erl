%%%-------------------------------------------------------------------
%% @doc pipeflow top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pipeflow_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(ProjectID, Sub, Exporter, ExpConf) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ProjectID, Sub, Exporter, ExpConf]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([ProjectID, Sub, Exporter, ExpConf]) ->
    RestartStrategy = rest_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    WorkerSup = {'pipeflow_worker_sup', {'pipeflow_worker_sup', start_link, [ProjectID, Sub, Exporter, ExpConf]},
              permanent, 2000, supervisor, ['pipeflow_worker_sup']},

    Fetcher = {'pipeflow_pubsub_fetcher', {'pipeflow_pubsub_fetcher', start_link, []},
              permanent, 2000, worker, ['pipeflow_pubsub_fetcher']},

    {ok, {SupFlags, [WorkerSup, Fetcher]}}.

%%====================================================================
%% Internal functions
%%====================================================================
