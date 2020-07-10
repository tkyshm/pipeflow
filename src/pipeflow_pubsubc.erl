-module(pipeflow_pubsubc).

-export([pull_messages/2, ack_messages/3]).

-type message() :: map().
-type ack_resp() :: map().
-type ack_id() :: binary().
-type project_id() :: binary().

-type pull_message_ret() :: {ok, [message()]} | {ok, no_messages} | {error, any()}.

-define(V1_API_ENDPOINT, "https://pubsub.googleapis.com/v1/").

% API DOC
% https://cloud.google.com/pubsub/docs/reference/rest/v1/projects.subscriptions/pull

%% POST https://pubsub.googleapis.com/v1/{subscription}:pull
-spec pull_messages(project_id(), binary()) -> pull_message_ret().
pull_messages(ProjectID, Sub) -> pull_messages(ProjectID, Sub, 10).

-spec pull_messages(project_id(), binary(), non_neg_integer()) -> pull_message_ret().
pull_messages(ProjectID, Sub, Max) ->
    AccessToken = get_access_token(),
    Headers = [
        {"content-type", <<"application/json">>},
        {"authorization", <<"Bearer ", AccessToken/binary>>}
    ],
    Body = jsone:encode(#{<<"maxMessages">> => Max}),
    Endpoint = make_endpoint(pull, ProjectID, Sub),

    handle_pull_messages_resp(do_pull_messages_request(Endpoint, Headers, Body)).

do_pull_messages_request(Endpoint, Headers, Body) ->
    try
        hackney:request(post, Endpoint, Headers, Body, [{recv_timeout, 300000}])
    of
        {ok, _Status, _Headers, CRef} ->
            %% TODO: stutus 4xx, 5xxの処理
            {ok, CRef}
    catch
        _Type:Error:Stack ->
            logger:warning("failed to handle_resp: ~p", [Error]),
            logger:warning("stacktrace: ~p", [Stack]),
            {error, Error}
    end.

handle_pull_messages_resp({ok, CRef}) ->
    try
        {ok, RespBody} = hackney:body(CRef),
        Data = jsone:decode(RespBody),
        maps:get(<<"receivedMessages">>, Data, [])
    of
        Msgs -> {ok, Msgs}
    catch
        _Type:Error:Stack ->
            logger:warning("failed to handle_resp: ~p", [Error]),
            logger:warning("stacktrace: ~p", [Stack]),
            {error, Error}
    end;
handle_pull_messages_resp({error, timeout}) -> {ok, no_messages};
handle_pull_messages_resp({error, _} = Ret) -> Ret.

% API DOChttps://cloud.google.com/pubsub/docs/reference/rest/v1/projects.subscriptions/acknowledge
%% POST https://pubsub.googleapis.com/v1/{subscription}:acknowledge
-spec ack_messages(project_id(), binary(), [ack_id()]) -> {ok, ack_resp()} | {error, any()}.
ack_messages(ProjectID, Sub, AckIDs) ->
    AccessToken = get_access_token(),
    Headers = [
        {"content-type", <<"application/json">>},
        {"authorization", <<"Bearer ", AccessToken/binary>>}
    ],
    Body = jsone:encode(#{<<"ackIds">> => AckIDs}),

    case hackney:request(post, make_endpoint(acknowledge, ProjectID, Sub), Headers, Body, []) of
        {ok, _Status, _Headers, CRef} ->
            {ok, _RespBody} = hackney:body(CRef),
            {ok, AckIDs};
        {error, Reason} ->
            {error, Reason}
    end.

make_endpoint(pull, ProjectID, Sub) ->
    Subscriptions = make_subscription(ProjectID, Sub),
    Endpoint = get_endpoint(),
    <<Endpoint/binary, Subscriptions/binary, ":pull">>;
make_endpoint(acknowledge, ProjectID, Sub) ->
    Subscriptions = make_subscription(ProjectID, Sub),
    Endpoint = get_endpoint(),
    <<Endpoint/binary, Subscriptions/binary, ":acknowledge">>.

make_subscription(ProjectID, Sub) ->
    <<"projects/", ProjectID/binary, "/subscriptions/", Sub/binary>>.

get_access_token() ->
    egoc_token:access_token(egoc:get_token(persistent_term:get(pipeflow_scopes))).

get_endpoint() ->
    list_to_binary(os:getenv("PUBSUB_ENDPOINT", ?V1_API_ENDPOINT)).
