%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2020, tkyshm
%%% @doc
%%% That module exports data which is got from cloud pubsub to gcs.
%%% Exporter Config:
%%%
%%% #{
%%%   bucket => <<"your_gcs_bucket_name">>,
%%%   log_prefix => <<"log filename prefix">>
%%% }
%%%
%%%
%%% @end
%%% Created : 2020-07-10 16:00:02.094226
%%%-------------------------------------------------------------------
-module(pipeflow_gcs_exporter).

-behaviour(pipeflow_exporter).

-export([export/2]).

-include_lib("hackney/include/hackney.hrl").

-define(API_ENDPOINT(Bucket),
        <<"https://storage.googleapis.com/upload/storage/v1/b/", Bucket/binary, "/o?uploadType=multipart">>).

-define(BOUNDARY, <<"pipeflow">>).

-spec export(pipeflow_exporter:data(), map()) -> {ok, term()} | {error, any()}.
export(Data, Conf) ->
    case gen_body(Data) of
        [] ->
            {ok, do_nothing};

        GenData ->
            Params = jsone:encode(GenData),
            AccessToken = get_access_token(),
            Bucket = maps:get(bucket, Conf),
            LogPrefix = maps:get(log_prefix, Conf, <<"pipeflow">>),
            ReqBody = jsone:encode(#{
                name => gen_name(LogPrefix)
            }),

            Headers = [
                {<<"content-type">>, <<"multipart/related; boundary=", ?BOUNDARY/binary>>},
                {<<"authorization">>, <<"Bearer ", AccessToken/binary>>}
            ],

            {ok, C} = hackney:request(post, ?API_ENDPOINT(Bucket), Headers, stream_multipart, []),
            hackney_manager:get_state(C, fun(State) ->
                hackney_manager:update_state(State#client{mp_boundary = <<?BOUNDARY/binary>>})
            end),

            try
                ExtHeaders = [{<<"content-type">>, <<"application/json; charset=utf8">>}],
                hackney:send_multipart_body(C, {data, ?BOUNDARY, ReqBody, ExtHeaders}),
                hackney:send_multipart_body(C, {data, ?BOUNDARY, Params, ExtHeaders}),

                case hackney:start_response(C) of
                    {ok, _, _, CRef} ->
                        {ok, RespBody} = hackney:body(CRef),
                        logger:debug("[debug] gcs exporter resp body: ~p", [RespBody]),
                        {ok, exported};
                    {error, Reason} ->
                        logger:warning("failed to upload to gcs '~p': ~p",[Bucket, Reason]),
                        {error, Reason}
                end
            of
                Ret -> Ret
            catch
                _Type:Err:Stack ->
                    logger:warning("failed to send request: ~p", [Err]),
                    logger:debug("stacktrace: ~p", [Stack]),
                    {error, Err}
            end
    end.

gen_body(Data) -> gen_body([], Data).

gen_body(Acc, []) -> Acc;
gen_body(Acc, [Msg|Data]) ->
    RawMsg = base64:decode(Msg),
    case jsone:try_decode(RawMsg) of
        {ok, JsonMsg, _} ->
            gen_body([JsonMsg|Acc], Data);

        {error, Reason} ->
            logger:warning("failed to decode message because its format was not json format: ~p, ~p", [Reason, RawMsg]),
            gen_body(Acc, Data)
    end.

get_access_token() ->
    egoc_token:access_token(egoc:get_token(persistent_term:get(pipeflow_scopes))).

gen_name(LogPrefix) ->
    {{Y, M, D}, {Hour, Min, Sec}} = calendar:local_time(),
    DateTime = list_to_binary([
     i_to_s(Y),
     "-",
     i_to_s(M),
     "-",
     i_to_s(D),
     "_",
     i_to_s(Hour),
     "-",
     i_to_s(Min),
     "-",
     i_to_s(Sec)
    ]),
    {ok, Hostname} = inet:gethostname(),
    BHost = list_to_binary(Hostname),
    <<LogPrefix/binary, "_", DateTime/binary, "_", BHost/binary, ".json">>.

i_to_s(N) -> list_to_binary(string:right(integer_to_list(N), 2, $0)).
