-module(gizmo_backend_utils).
-author('mkorszun@gmail.com').

-export([db_execute/2, db_execute/3]).

%% ###############################################################
%% SOME UTILS
%% ###############################################################

%% @doc Executes given DB action
-spec db_execute(fun(), atom()) -> term().
db_execute(Action, Group) ->
    db_execute(Action, Group, fun(R) -> R end).

%% @doc Executes given DB action and formats the results
-spec db_execute(fun(), atom(), fun()) -> term().
db_execute(Action, Group, FormatResult) ->
    try pooler:take_member(Group) of
        Connection when is_pid(Connection) ->
            Res = Action(Connection),
            pooler:return_member(Group, Connection),
            FormatResult(Res);
        error_no_members ->
            {error, error_no_members}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% ###############################################################
%% TESTS
%% ###############################################################

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

db_execute_test_() ->
    {setup,
        fun() -> application:start(meck) end,
        fun(_) -> application:stop(meck) end,
        [
            fun db_execute_ok/0,
            fun db_execute_ok_format/0,
            fun db_execute_no_members/0
        ]
    }.

db_execute_ok() ->
    meck:new(pooler),
    meck:expect(pooler, take_member, fun(_) -> self() end),
    meck:expect(pooler, return_member, fun(_,_) -> ok end),
    ?assertEqual(ok, db_execute(fun(_) -> ok end, group)),
    [{_,{pooler,take_member,[group]},_},
     {_,{pooler,return_member,[group,_]},ok}] = meck:history(pooler),
    meck:validate(pooler),
    meck:unload(pooler).

db_execute_ok_format() ->
    meck:new(pooler),
    meck:expect(pooler, take_member, fun(_) -> self() end),
    meck:expect(pooler, return_member, fun(_,_) -> ok end),
    ?assertEqual({ok, formatted}, db_execute(
        fun(_) -> ok end, group,
        fun(_) -> {ok, formatted} end)),
    [{_,{pooler,take_member,[group]},_},
     {_,{pooler,return_member,[group,_]},ok}] = meck:history(pooler),
    meck:validate(pooler),
    meck:unload(pooler).

db_execute_no_members() ->
    meck:new(pooler),
    meck:expect(pooler, take_member, fun(_) -> error_no_members end),
    ?assertEqual({error, error_no_members}, db_execute(fun(_) -> ok end, group)),
    [{_,{pooler,take_member,[group]},error_no_members}] = meck:history(pooler),
    meck:validate(pooler),
    meck:unload(pooler).

-endif.

%% ###############################################################
%% ###############################################################
%% ###############################################################
