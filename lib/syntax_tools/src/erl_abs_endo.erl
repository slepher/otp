%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 13 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_abs_endo).

-include("abs_struct_name.hrl").
-export_type([endo/1, list_or_endo/1]).

-opaque endo(A) :: #{?STRUCT_KEY := ?ENDO, is_empty := boolean(), list => endo_list(A)}.
-type endo_list(A) :: fun(([A]) -> [A]).
-type list_or_endo(A) :: [A] | endo(A).

%% API
-export([endo/1, empty/0, run/1]).
-export([append/2]).
-export([map/2]).
-export([is_empty/1]).
%%%===================================================================
%%% API
%%%===================================================================
-spec empty() -> erl_abs_endo:endo(_A).
empty() ->
    #{?STRUCT_KEY => ?ENDO, is_empty => true}.

-spec endo([A]) -> endo(A).
endo([]) ->
    empty();
endo(List) when is_list(List) ->
    new(fun(List1) -> List ++ List1 end).

-spec run(endo(A)) -> [A].
run(#{?STRUCT_KEY := ?ENDO, is_empty := true}) ->
    [];
run(#{?STRUCT_KEY := ?ENDO, list := EndoList}) ->
    EndoList([]).

-spec map(fun((A) -> A), endo(A)) -> endo(A).
map(_Fun, #{?STRUCT_KEY := ?ENDO, is_empty := true} = Endo) ->
    Endo;
map(Fun, #{?STRUCT_KEY := ?ENDO, list := EndoList}) ->
    new(fun(List) -> lists:map(Fun, EndoList(List)) end).


-spec append(endo(A) | [A], endo(A) | [A]) -> endo(A).
append(List1, List2) when is_list(List1) ->
    append(erl_abs_endo:endo(List1), List2);
append(List1, List2) when is_list(List2) ->
    append(List1, erl_abs_endo:endo(List2));
append(#{?STRUCT_KEY := ?ENDO} = Endo1, #{?STRUCT_KEY := ?ENDO} = Endo2) ->
    do_append(Endo1, Endo2).

-spec is_empty(endo(_A)) -> boolean().
is_empty(#{?STRUCT_KEY := ?ENDO, is_empty := IsEmpty}) ->
    IsEmpty.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec new(endo_list(A)) -> endo(A).
new(Inner) when is_function(Inner, 1) ->
    #{?STRUCT_KEY => ?ENDO, list => Inner, is_empty => false}.

-spec do_append(endo(A), endo(A)) -> endo(A).
do_append(#{is_empty := true}, #{} = Endo2) ->
    Endo2;
do_append(#{} = Endo1, #{is_empty := true}) ->
    Endo1;
do_append(#{list := EndoList1}, #{list := EndoList2}) ->
    new(fun(List) -> EndoList1(EndoList2(List)) end).