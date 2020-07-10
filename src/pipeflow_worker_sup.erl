%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2020, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2020-07-16 17:16:10.346314
%%%-------------------------------------------------------------------
-module(pipeflow_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ProjectID, Sub, Exporter, ExpConf) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ProjectID, Sub, Exporter, ExpConf]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([ProjectID, Sub, Exporter, ExpConf]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {'pipeflow_worker', {'pipeflow_worker', start_link, [ProjectID, Sub, Exporter, ExpConf]},
              Restart, Shutdown, Type, ['pipeflow_worker']},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
