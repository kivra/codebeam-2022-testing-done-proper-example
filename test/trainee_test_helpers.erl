%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Module with some helper functions for running {link proper. PropEr} in
%%% {@link ct. Common Test}.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =====================================================
-module(trainee_test_helpers).

%%%_* Exports ================================================================
%%%_ * API -------------------------------------------------------------------
%% Run PropEr test in Common Test
-export([
    counterexample/3,
    quickcheck/2
]).

%% For use in counter examples.
-export([
    get_current_model_state/0,
    set_current_model_state/1,
    symbolic_get_current_model_state/0
]).

%% Generally useful stuff
-export([
    sublists/1
]).

%%%_* Types ------------------------------------------------------------------

%%%_* Includes ===============================================================
-include("internal.hrl").

%%%_* Macros =================================================================

%%%_* Types ==================================================================

%%%_* Code ===============================================================
%%%_ * API -------------------------------------------------------------------

%% @doc Bridge between {@link proper} and {@link ct} tests.
quickcheck(Config, Property) ->
    PropErOptions = proplists:get_value(proper_options, Config),
    ?assertNotEqual(
        undefined, PropErOptions, "must specify `proper_options` in CT's `Config`"
    ),
    case proper:quickcheck(Property, PropErOptions) of
        true ->
            true;
        false ->
            {fail, proper_failed};
        Failure ->
            {fail, Failure}
    end.

%% @doc Bridge between {@link proper} and {@link ct} tests.
counterexample(Config, Property, CounterExample) ->
    PropErOptions = proplists:get_value(proper_options, Config),
    ?assertNotEqual(
        undefined, PropErOptions, "must specify `proper_options` in CT's `Config`"
    ),
    case proper:check(Property, [CounterExample], PropErOptions) of
        true ->
            true;
        false ->
            {fail, proper_failed};
        Failure ->
            {fail, Failure}
    end.

%% @doc Stores the current model state in the process dictionary.
%%
%% @see symbolic_get_current_model_state/0
set_current_model_state(Model) ->
    put(current_model_state, Model).

%% @doc Get the current model state from the process dictionary.
%%
%% Must be exported for {@link proper} to be able to call it.
%%
%% @see symbolic_get_current_model_state/0
get_current_model_state() ->
    get(current_model_state).

%% @doc Symbolically get the current model state.
%%
%% For use in counter examples.
-spec symbolic_get_current_model_state() -> proper_statem:symbolic_call().
symbolic_get_current_model_state() ->
    {call, ?MODULE, get_current_model_state, []}.

%% @doc Returns a sublist of the given {@link ordsets. ordset}, including the
%% empty set and the original, full, set.
%%
%% The resulting lists are guaranteed to keep the ordering of the input list.
-spec sublists(ordsets:ordset(T)) -> [ordsets:ordset(T)].
sublists([]) ->
    [[]];
sublists([Head | Tail]) ->
    InnerSublists = sublists(Tail),
    OuterSublists = [
        [Head | Sublist]
     || Sublist <- InnerSublists
    ],
    OuterSublists ++ InnerSublists.

%%%_* Private ----------------------------------------------------------------

%%%_* Tests ==================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sublists_test_() ->
    [
        ?_assertEqual(Expected, sublists(Input))
     || {Input, Expected} <- maps:to_list(#{
            [] => [[]],
            [single] => [
                [single],
                []
            ],
            [abc, 123] => [
                [abc, 123],
                [abc],
                [123],
                []
            ],
            [first, second, third] => [
                [first, second, third],
                [first, second],
                [first, third],
                [first],
                [second, third],
                [second],
                [third],
                []
            ]
        })
    ].

-endif.
