%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc PropEr statem suite for testing TrainEE
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =====================================================
-module(trainee_SUITE).

%%%_* Behaviours =============================================================
-behaviour(ct_suite).
-behaviour(proper_statem).

%%%_* Exports ================================================================
%%%_ * Callbacks -------------------------------------------------------------
%%% Common Test callbacks
-export([
    all/0,
    end_per_suite/1,
    init_per_suite/1
    %% end_per_testcase/2,
    %% init_per_testcase/2
]).

%% Common Test test functions
-export([
    proper_trainee_test/1,
    stockholm_to_lund_test/1
]).

%% Proper statem callbacks
-export([
    command/1,
    initial_state/0,
    next_state/3,
    postcondition/3,
    precondition/2
]).

%% Proper statem calls (from the symbolic calls in {@link command/1})
-export([
    advance_time/2,
    pay/3,
    reserve/2
]).

%%%_* Types ------------------------------------------------------------------

%%%_* Includes ===============================================================
-include_lib("proper/include/proper.hrl").
-include("internal.hrl").

%%%_* Macros =================================================================
%% Stockholm 17:16 - Göteborg 20:35
-define(STOCKHOLM_GÖTEBORG, [
    <<"Stockholm">>,
    <<"Södertälje Syd"/utf8>>,
    <<"Katrineholm">>,
    <<"Skövde"/utf8>>,
    <<"Göteborg"/utf8>>
]).

%% Göteborg 17:24 - Malmö 20:00
-define(GÖTEBORG_MALMÖ, [
    <<"Göteborg"/utf8>>,
    <<"Halmstad">>,
    <<"Helsingborg">>,
    <<"Lund">>,
    <<"Malmö"/utf8>>
]).

%% Malmö 17:00 - Stockholm 21:38
-define(MALMÖ_STOCKHOLM, [
    <<"Malmö"/utf8>>,
    <<"Lund">>,
    <<"Hässleholm"/utf8>>,
    <<"Alvesta">>,
    <<"Nässjö"/utf8>>,
    <<"Mjölby"/utf8>>,
    <<"Linköping"/utf8>>,
    <<"Norrköping"/utf8>>,
    <<"Södertälje Syd"/utf8>>,
    <<"Stockholm">>
]).

%%%_* Types ==================================================================
-record(reservation, {
    start_station :: binary(),
    stop_station :: binary()
}).

-type reservation() :: #reservation{}.

-type symbolic(_Value) :: proper_statem:symbolic_var().
%% Type representing a {@link proper} symbolic value.

-type sometimes_symbolic(Value) :: Value | symbolic(Value).
%% Type representing either a {@link proper} symbolic or dynamic value.

-type model() :: #{
    reservations := [reservation()]
}.
%% Represents the model of this Proper statem test

-type symbolic_model() :: #{
    reservations := [symbolic(reservation())]
}.
%% The symbolic version of {@link model/0}

%%%_* Code ===============================================================
%%%_ * Callbacks -------------------------------------------------------------
%%_ Common Test callbacks

all() ->
    [
        stockholm_to_lund_test,
        proper_trainee_test
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(trainee),
    ok = trainee_db:add_route(?STOCKHOLM_GÖTEBORG),
    ok = trainee_db:add_route(?GÖTEBORG_MALMÖ),
    ok = trainee_db:add_route(?MALMÖ_STOCKHOLM),
    [
        {proper_options, [
            {numtests, 10},
            {max_size, 10},
            {constraint_tries, 5},
            long_result
        ]}
        | Config
    ].

end_per_suite(_Config) ->
    ok.

%% @doc Handwritten test using PropEr's counter example.
stockholm_to_lund_test(Config) ->
    trainee_test_helpers:counterexample(Config, trainee_prop(), [
        {set, {var, 1}, {call, trainee_SUITE, reserve, [<<"Stockholm">>, <<"Lund">>]}},
        {set, {var, 2},
            {call, trainee_SUITE, pay, [
                trainee_test_helpers:symbolic_get_current_model_state(),
                {var, 1},
                success
            ]}}
    ]).

%% @doc Full, automatic, PropEr test
proper_trainee_test(Config) ->
    trainee_test_helpers:quickcheck(Config, trainee_prop()).

%% @doc PropEr statem property, the main driver for this test
trainee_prop() ->
    ?FORALL(
        Commands,
        proper_statem:commands(?MODULE),
        begin
            %% Prepare to execute here, e.g. reset state or start transaction
            {History, State, Result} = proper_statem:run_commands(?MODULE, Commands),
            %% Cleanup goes here, e.g. rollback transaction
            ?WHENFAIL(
                begin
                    ct:pal("~p", [Commands]),
                    trainee_test_logging:log_proper_results(
                        Commands,
                        History,
                        State,
                        Result,
                        ?FILE,
                        ?LINE
                    )
                end,
                aggregate(command_names(Commands), Result =:= ok)
            )
        end
    ).

%%_ Proper statem callbacks
%% @doc Returns the initial model state for this Proper statem test.
-spec initial_state() -> model().
initial_state() ->
    #{
        reservations => []
    }.

%% @doc Returns a {@link proper_statem:command/1. command}
%% {@link proper_type. type}.
-spec command(symbolic_model()) -> proper_types:type().
command(#{reservations := []} = Model@) ->
    common_commands(Model@);
command(Model@) ->
    oneof([pay_command(Model@), common_commands(Model@)]).

%% @doc Returns a command that can always be done by the API caller.
-spec common_commands(symbolic_model()) -> proper_types:type().
common_commands(Model@) ->
    oneof([
        reserve_command(Model@),
        advance_time_command(Model@)
    ]).

%% @doc Returns a command for trying paying a random reservation.
-spec pay_command(symbolic_model()) -> proper_types:type().
pay_command(#{reservations := Reservations} = Model@) ->
    ?LET(
        Reservation,
        oneof(Reservations),
        {call, ?MODULE, pay, [
            Model@, Reservation, expected_payment_result_prop()
        ]}
    ).

%% @doc Returns a command for trying to reserving a random route
-spec reserve_command(symbolic_model()) -> proper_types:type().
reserve_command(_Model@) ->
    ?LET(
        Stations,
        oneof([?STOCKHOLM_GÖTEBORG, ?GÖTEBORG_MALMÖ, ?MALMÖ_STOCKHOLM]),
        {call, ?MODULE, reserve, route_prop(Stations)}
    ).

%% @doc Return a command for advancing the time, to test reservation timeouts.
-spec advance_time_command(symbolic_model()) -> proper_types:type().
advance_time_command(Model@) ->
    ?LET(
        Minutes,
        integer(1, 15),
        {call, ?MODULE, advance_time, [Model@, Minutes]}
    ).

%%_ Proper properties/generators

expected_payment_result_prop() ->
    oneof([
        success,
        fail
    ]).

route_prop(Stations) ->
    oneof(all_possible_routes(Stations)).

%%_ Proper statem callbacks

-spec precondition(symbolic_model(), proper_statem:symbolic_call()) -> boolean().
precondition(_Model@, {call, _Module, _Function, _Arguments}) ->
    true.

-spec postcondition(model(), proper_statem:symbolic_call(), term()) -> boolean().
postcondition(Model, {call, Module, Function, Arguments}, Result) ->
    try postcondition_internal(Model, {call, Module, Function, Arguments}, Result) of
        Passed ->
            trainee_test_logging:log_call(
                Module,
                Function,
                Arguments,
                Result,
                ?FILE,
                ?LINE
            ),
            case Passed of
                ok -> true;
                _ -> Passed
            end
    catch
        error:AssertionError:Stacktrace when ?is_assert_error(AssertionError) ->
            trainee_test_logging:log_postcondition_failed(
                AssertionError,
                Module,
                Function,
                Arguments,
                Result,
                ?FILE,
                ?LINE,
                Stacktrace
            ),
            false
    end.

-spec postcondition_internal(model(), proper_statem:symbolic_call(), term()) ->
    ok | boolean().
postcondition_internal(_Model, {call, ?MODULE, _Function, _Arguments}, _Result) ->
    ?assert(false, "TODO: implement postconditions").

-spec next_state(State, symbolic(term()) | term(), proper_statem:symbolic_call()) ->
    State
when
    State :: symbolic_model() | model().
next_state(Model@, Result@, {call, ?MODULE, Function, Arguments}) ->
    NextState@ = next_state_internal(Model@, Function, Arguments, Result@),
    trainee_test_helpers:set_current_model_state(NextState@),
    NextState@.

-spec next_state_internal(
    State, atom(), [symbolic(term()) | term()], symbolic(term()) | term()
) -> State when
    State :: symbolic_model() | model().
next_state_internal(
    #{reservations := Reservations} = Model@, reserve, _Arguments, Response@
) ->
    Model@#{
        reservations := [Response@ | Reservations]
    };
next_state_internal(Model@, _Function, _Arguments, _Response@) ->
    Model@.

%%_ Proper statem calls (from the symbolic calls in {@link command/1})

reserve(Start, Stop) ->
    trainee_db:reserve(Start, Stop).

pay(_Model, Reservation, _ExpectedResult) ->
    trainee_db:pay(Reservation).

advance_time(_Model, _Minutes) ->
    ok.

%%%_* Private ----------------------------------------------------------------

all_possible_routes([Start, Stop]) ->
    [[Start, Stop]];
all_possible_routes([Start | Tail]) ->
    Routes = [[Start, Stop] || Stop <- Tail],
    Routes ++ all_possible_routes(Tail).

%%%_* Tests ==================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% erlfmt-ignore
all_possible_routes_test() ->
    ?assertEqual(
        [
            [a, b], [a, c], [a, d], [a, e],
                    [b, c], [b, d], [b, e],
                            [c, d], [c, e],
                                    [d, e]
        ],
        all_possible_routes([a, b, c, d, e])
    ).

-endif.
