%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc {@link trainee} application
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =====================================================
-module(trainee_app).

%%%_* Behaviours =============================================================
-behaviour(application).

%%%_* Exports ================================================================
%%%_ * API -------------------------------------------------------------------

%%%_ * Callbacks -------------------------------------------------------------
-export([
    start/2,
    stop/1
]).

%%%_* Types ------------------------------------------------------------------

%%%_* Includes ===============================================================

%%%_* Macros =================================================================

%%%_* Types ==================================================================

%%%_* Code ===============================================================
%%%_ * Callbacks -------------------------------------------------------------
start(_StartType, _StartArgs) ->
    trainee_sup:start_link().

stop(_State) ->
    ok.

%%%_ * API -------------------------------------------------------------------

%%%_* Private ----------------------------------------------------------------

%%%_* Tests ==================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
