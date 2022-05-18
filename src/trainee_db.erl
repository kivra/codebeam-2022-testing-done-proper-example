%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The TrainEE's database
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =====================================================
-module(trainee_db).

%%%_* Behaviours =============================================================
-behaviour(gen_server).

%%%_* Exports ================================================================
%%%_ * API -------------------------------------------------------------------
%% For the supervisor
-export([
    child_spec/0,
    start_link/1
]).

%% Normal API
-export([
    add_route/1,
    reserve/2,
    pay/1
]).

%%%_ * Callbacks -------------------------------------------------------------
-export([
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1
]).

%%%_* Types ------------------------------------------------------------------
-export_type([
    seat/0,
    station/0
]).

%%%_* Includes ===============================================================

%%%_* Macros =================================================================

%%%_* Types ==================================================================
-type seat() :: #{
    start_station := station(),
    stop_station := station(),
    payment_status := pending | succeeded | failed
}.

-type station() :: binary().

%%%_* Code ===============================================================
%%%_ * API -------------------------------------------------------------------
%% @doc
child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, [#{name => ?MODULE}]}
    }.

start_link(Options) ->
    GenServerOptions = [],
    case Options of
        #{name := Name} ->
            gen_server:start_link({local, Name}, ?MODULE, [], GenServerOptions);
        _ ->
            gen_server:start_link(?MODULE, [], GenServerOptions)
    end.

%% @doc Adds the given route, i.e. list of stations, to the database
-spec add_route([station()]) -> ok.
add_route(Stations) ->
    gen_server:call(?MODULE, {add_route, Stations}).

%% @doc Tries to reserve a seat for the given route and time
-spec reserve(station(), station()) -> {ok, seat()} | {error, Reason} when
    Reason :: term().
reserve(StartStation, StopStation) ->
    Seat = #{
        start_station => StartStation,
        stop_station => StopStation,
        reserved_at => now(),
        payment_status => pending
    },
    gen_server:call(?MODULE, {reserve, Seat}).

%% @doc Pay the given reserved seat
-spec pay(seat()) -> ok | {error, Reason} when
    Reason :: term().
pay(Reservation) ->
    gen_server:call(?MODULE, {pay, Reservation}).

%%%_ * Callbacks -------------------------------------------------------------
init(_Arguments) ->
    State = #{
        routes => [],
        reservations => []
    },
    {ok, State}.

handle_call({add_route, Stations}, _From, #{routes := Routes} = State0) ->
    State1 = State0#{
        routes := [Stations | Routes]
    },
    {reply, ok, State1};
handle_call(
    {reserve,
        #{
            start_station := StartStation,
            stop_station := StopStation
        } = Seat},
    _From,
    #{routes := Routes, reservations := Reservations} = State0
) ->
    case
        find(Routes, fun(Stations) ->
            case slice_range(Stations, StartStation, StopStation) of
                {error, not_found} ->
                    false;
                {ok, InnerRoute} ->
                    find(Reservations, fun(
                        #{
                            start_station := ReservedStartStation,
                            stop_station := ReservedStopStation
                        }
                    ) ->
                        case
                            lists:member(ReservedStartStation, InnerRoute) orelse
                                lists:member(ReservedStopStation, InnerRoute)
                        of
                            true ->
                                %% Already reserved
                                false;
                            false ->
                                {ok, State0#{reservations := [Seat | Reservations]}}
                        end
                    end)
            end
        end)
    of
        {ok, State1} ->
            {reply, ok, State1};
        false ->
            {reply, {error, reserved}}
    end;
handle_call({pay, Seat0}, _From, #{reservations := Reservations0} = State0) ->
    case find_reservation_for_seat(Reservations0, Seat0) of
        false ->
            {reply, {error, not_found, State0}};
        #{payment_status := pending} ->
            Seat1 = Seat0#{payment_status := succeeded},
            Reservations1 = update_reservation(Reservations0, Seat0, Seat1),
            State1 = State0#{reservations := Reservations1},
            {reply, ok, State1};
        #{payment_status := Status} ->
            {reply, {error, {already_proccessed, Status}}, State0}
    end;
handle_call(Msg, _From, _State) ->
    error({unknown_call, Msg}).

handle_cast(Msg, _State) ->
    error({unknown_cast, Msg}).

handle_info(
    {timeout, _TimerRef, {remove_reservation, Seat}},
    #{reservations := Reservations0} = State0
) ->
    Reservations1 = lists:filter(seat_matcher(Seat), Reservations0),
    State1 = State0#{reservations := Reservations1},
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

%%%_* Private ----------------------------------------------------------------
-compile({no_auto_import, [now/0]}).

now() ->
    erlang:system_time(millisecond).

find_reservation_for_seat(Reservations, Seat) ->
    case lists:search(seat_matcher(Seat), Reservations) of
        {value, Reservation} -> Reservation;
        false -> false
    end.

update_reservation(Reservations, OldSeat, NewSeat) ->
    case lists:partition(seat_matcher(OldSeat), Reservations) of
        {[], _} -> {error, not_found};
        {_, Reservations1} -> [NewSeat | Reservations1]
    end.

seat_matcher(#{
    start_station := TargetStartStation,
    stop_station := TargetStopStation
}) ->
    fun(#{start_station := StartStation, stop_station := StopStation}) ->
        StartStation =:= TargetStartStation andalso StopStation =:= TargetStopStation
    end.

slice_range(List, Start, Stop) ->
    case lists:splitwith(fun(Element) -> Element =/= Start end, List) of
        {[], _} ->
            {error, not_found};
        {_Before, [Start | Tail]} ->
            case lists:splitwith(fun(Element) -> Element =/= Stop end, Tail) of
                {_, []} ->
                    {error, not_found};
                {Slice, [Stop | _After]} ->
                    {ok, Slice}
            end
    end.

-spec find(list(A), fun((A) -> {ok, B} | false)) -> {ok, B} | false.
find([], Predicate) when is_function(Predicate, 1) ->
    false;
find([Head | Tail], Predicate) when is_function(Predicate, 1) ->
    case Predicate(Head) of
        {ok, Value} -> {ok, Value};
        false -> find(Tail, Predicate)
    end.

%%%_* Tests ==================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
