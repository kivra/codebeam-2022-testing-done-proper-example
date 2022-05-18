-include_lib("stdlib/include/assert.hrl").

%% Poor mans `in' operator
-define(oneof(Value, Alt1, Alt2), (Value =:= Alt1 orelse Value =:= Alt2)).

%% Poor mans `in' operator
-define(oneof(Value, Alt1, Alt2, Alt3),
    (Value =:= Alt1 orelse ?oneof(Value, Alt2, Alt3))
).

%% Poor mans `in' operator
-define(oneof(Value, Alt1, Alt2, Alt3, Alt4),
    (Value =:= Alt1 orelse ?oneof(Value, Alt2, Alt3, Alt4))
).

-define(is_assert_error(Reason),
    (tuple_size(Reason) =:= 2 andalso
        is_list(element(2, Reason)) andalso
        %% Ordered by what's (assumed to be) most common
        (?oneof(element(1, Reason), assertEqual, assertMatch, assertNotEqual) orelse
            ?oneof(element(1, Reason), assertNotMatch, assert, assertException)))
).

%% Makes `if' statements easier to read, compare
%%
%%     if is_list(Value) ->
%%       list;
%%     is_map(Value) ->
%%       map;
%%     true ->
%%       other
%%     end.
%%
%% With
%%
%%     if is_list(Value) ->
%%       list;
%%     is_map(Value) ->
%%       map;
%%     ?else ->
%%       other
%%     end.
%%
%% Much easier to understand, then the whole `true' means `false' thing.
-define(else, true).
