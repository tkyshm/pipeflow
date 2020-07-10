%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2020, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2020-10-12 19:13:31.256059
%%%-------------------------------------------------------------------
-module(pipeflow_worker_SUITE).


%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         t_pull_message/1
        ]).

% -include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

% -define(PROPTEST(M,F), true = proper:quickcheck(M:F())).

all() ->
    [
     {group, test}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {test, [], [
                 t_pull_message
                ]
     }
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    application:ensure_all_started(cowboy),
    test_mock:start_server(#{expires => 3600}),
    {ok, _} = application:ensure_all_started(pipeflow),
    Config.

end_per_suite(_Config) ->
    application:stop(pipeflow),
    application:stop(cowboy),
    test_mock:stop_server(),
    ok.

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->
    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

t_pull_message(_Config) ->
    [{workers, [Pid|_]}] = ets:lookup(pipeflow_worker_pool, workers),
    ?assertEqual({ok, success}, pipeflow_worker:pull_message(Pid)),
    ok.
