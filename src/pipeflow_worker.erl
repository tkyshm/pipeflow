%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2020, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2020-07-10 16:00:02.094226
%%%-------------------------------------------------------------------
-module(pipeflow_worker).

-behaviour(gen_server).

%% API
-export([start_link/4,
         pull_message/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_continue/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(PULL_TIMEOUT, 300000).
-define(EXPORT_INTERVAL, 2000).

%% ack_ids is pull messages ack ids.
-record(state, {
    project_id = <<"">> :: binary(),
    subscription = <<"">> :: binary(),
    exporter = pipeflow_gcs_exporter :: atom(),
    exporter_config = #{} :: map(),
    ack_ids = [] :: [ack_id()],
    messages = [] :: [message()]
}).

-type message() :: any().
-type ack_id() :: binary().

%%%===================================================================
%%% API
%%%===================================================================

-spec pull_message(pid()) -> message().
pull_message(Pid) ->
    gen_server:call(Pid, pull_message, ?PULL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ProjectID, Sub, Exporter, ExpConf) ->
    gen_server:start_link(?MODULE, [ProjectID, Sub, Exporter, ExpConf], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ProjectID, Sub, Exporter, ExpConf]) ->
    {ok, #state{project_id=ProjectID, subscription=Sub, exporter=Exporter, exporter_config=ExpConf}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle continue callback
%%
%% @spec handle_continue(Continue, State) -> {noreply, State} |
%%                     {noreply, State, Timeout} |
%%                     {noreply, State, hibernate} |
%%                     {noreply, State, {continue, Continue}} |
%%                     {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_continue(ack_messages, #state{ack_ids=[]} = State) ->
    {noreply, State};
handle_continue(ack_messages, #state{project_id=PrjID, subscription=Sub, ack_ids=AckIDs} = State) ->
    case pipeflow_pubsubc:ack_messages(PrjID, Sub, AckIDs) of
        {ok, _} ->
            {noreply, State#state{ack_ids=[]}};
        {error, Reason} ->
            %% TODO: check reason
            logger:warning("failed to ack_message: ~p", [Reason]),
            %% NOTE: retry ack messages
            {noreply, State, {continue, ack_messages}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(pull_message, _From, #state{project_id=PrjID, subscription=Sub} = State) ->
    case pipeflow_pubsubc:pull_messages(PrjID, Sub) of
        {ok, no_messages} ->
            logger:debug("[debug] no pull messages"),
            {reply, {ok, no_messages}, State};
        {ok, Messages} ->
            logger:debug("[debug] got messages: ~p", [Messages]),
            erlang:send_after(?EXPORT_INTERVAL, self(), log_exports),
            {reply, {ok, success}, State#state{messages=Messages}};
        {error, Reason} ->
            logger:error("failed to pull_message: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(log_exports, #state{messages=[]} = State) ->
    erlang:send_after(?EXPORT_INTERVAL, self(), log_exports),
    {noreply, State};
handle_info(log_exports, #state{messages=Messages} = State) ->
    case do_export(Messages, State) of
        ok ->
            AckIDs = lists:map(fun(M) -> maps:get(<<"ackId">>, M) end, Messages),
            erlang:send_after(?EXPORT_INTERVAL, self(), log_exports),
            {noreply, State#state{messages=[], ack_ids=new_ack_ids(AckIDs, State)}, {continue, ack_messages}};

        no_messages ->
            erlang:send_after(?EXPORT_INTERVAL, self(), log_exports),
            {noreply, State};

        {error, Reason} ->
            logger:warning("failed to exports: ~p", [Reason]),
            erlang:send_after(?EXPORT_INTERVAL, self(), log_exports),
            {noreply, State}
    end.

new_ack_ids(AckIDs, #state{ack_ids=OldAckIds}) ->
    lists:flatten([AckIDs, OldAckIds]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    [{workers, Pids}] = ets:lookup(pipeflow_worker_pool, workers),
    NewPids = lists:delete(self(), Pids),
    ets:insert(pipeflow_worker_pool, {workers, NewPids}),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_export([], _) -> no_messages;
do_export(Messages, #state{exporter=Exporter, exporter_config=ExpConf}) ->
    RawMessages = lists:map(fun get_message_data/1, Messages),
    case erlang:apply(Exporter, export, [RawMessages, ExpConf]) of
        {ok, _} -> ok;
        {error, Reason} ->
            logger:error("[exporter:~p] failed to export messages: ~p", [Exporter, Reason]),
            {error, Reason}
    end.

get_message_data(Msg) ->
    maps:get(<<"data">>, maps:get(<<"message">>, Msg)).
