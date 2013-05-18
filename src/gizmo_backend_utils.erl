-module(gizmo_backend_utils).
-author('mkorszun@gmail.com').

-export([db_execute/2, db_execute/3]).

%% ###############################################################
%% SOME UTILS
%% ###############################################################

-spec db_execute(fun(), atom()) -> term().
db_execute(Action, Group) ->
    db_execute(Action, Group, fun(R) -> R end).

-spec db_execute(fun(), atom(), fun()) -> term().
db_execute(Action, Group, FormatResult) ->
    try pooler:take_member(Group) of
        Connection when is_pid(Connection) ->
            Res = Action(Connection),
            pooler:return_member(Group, Connection),
            FormatResult(Res);
        {error, Reason} ->
            {error, Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################
