%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_syntax_monad_writer_t).

%% API
-export([writer_t/1]).
-export([run/1]).
-export([bind/3, '>>='/3, return/2]).
-export([fail/2]).
-export([lift/2]).
-export([tell/2]).
-export([get/1, put/2, state/2]).
%%%===================================================================
%%% API
%%%===================================================================
writer_t(Inner) ->
    {?MODULE, Inner}.

run({?MODULE, Inner}) ->
    Inner.

bind({?MODULE, WTA}, KWTB, {?MODULE, IM}) ->
    writer_t(
      erl_syntax_monad:bind(
        WTA,
        fun({A, LogsA}) ->
                erl_syntax_monad:bind(
                  run(KWTB(A)),
                  fun({B, LogsB}) when is_list(LogsB) ->
                          erl_syntax_monad:return({B, LogsA ++ LogsB}, IM)
                  end, IM)
        end, IM)).

'>>='(WTA, KWTB, W) ->
    bind(WTA, KWTB, W).

return(A, {?MODULE, IM}) ->
    writer_t(erl_syntax_monad:return({A, []}, IM)).

lift(MA, {?MODULE, IM}) ->
    writer_t(erl_syntax_monad:lift_m(fun(A) -> {A, []} end, MA, IM)).

tell(Logs, {?MODULE, IM}) ->
    writer_t(erl_syntax_monad:return({ok, Logs}, IM)).

fail(E, WT) ->
    erl_syntax_monad:lift_fail(E, WT).

get(WT) ->
    erl_syntax_monad:lift_get(WT).

put(S, WT) ->
    erl_syntax_monad:lift_put(S, WT).

state(F, WT) ->
    erl_syntax_monad:lift_state(F, WT).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
