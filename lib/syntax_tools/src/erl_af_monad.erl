%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_af_monad).

-include("af_struct_name.hrl").

%% API
-export([map_m/3, sequence_m/2, foldl_m/4]).
-export([lift_m/2, lift_m/3, bind/2, bind/3, then/2, then/3]).
-export([return/2, fail/2]).
-export([nested_withs/3, sequence_withs/3]).

-type monad_type()  :: module().
-type monad(_M, _A) :: any().

%%%===================================================================
%%% API
%%%===================================================================
-spec map_m(fun((A) -> monad(M, B)), [A], M) -> monad(M, [B]) when M :: monad_type().
map_m(F, [X|Xs], Monad) ->
    bind(F(X),
         fun(A) ->
                 bind(map_m(F, Xs, Monad),
                      fun(As) ->
                              return([A|As], Monad)
                      end, Monad)
         end, Monad);
map_m(_F, [], Monad) ->
    return([], Monad).

-spec sequence_m([monad(M, A)], M) -> monad(M, [A]) when M :: monad_type().
sequence_m(Xs, Monad) ->
    map_m(fun(A) -> A end, Xs, Monad).

-spec foldl_m(fun((A, S) -> monad(M, S)), S, [A], M) -> monad(M, S) when M :: monad_type().
foldl_m(F, Acc, [X|Xs], Monad) ->
    bind(
      F(X, Acc),
      fun(Acc1) ->
              foldl_m(F, Acc1, Xs, Monad)
      end, Monad);
foldl_m(_F, Acc, [], Monad) ->
    erl_af_monad:return(Acc, Monad).

-spec lift_m(fun((A) -> B), monad(M, A)) -> monad(M, B) when M :: monad_type().
lift_m(F, X) ->
    Monad = get_type(X),
    lift_m(F, X, Monad).

-spec lift_m(fun((A) -> B), monad(M, A), M) -> monad(M, B) when M :: monad_type().
lift_m(F, X, Monad) ->
    bind(X,
         fun(A) ->
                 return(F(A), Monad)
         end, Monad).

-spec bind(monad(M, A), fun((A) -> monad(M, B))) -> monad(M, B) when M :: monad_type().
bind(X, F) ->
    Monad = get_type(X),
    bind(X, F, Monad).

-spec bind(monad(M, A), fun((A) -> monad(M, B)), M) -> monad(M, B) when M :: monad_type().
bind(X, F, M) ->
    %% to avoid fail of this expression, just treat ok as Monad:return(ok)
    %% do([ Monad ||
    %%      io:format("hello world", []),
    %%      return(ok)
    %%   ]).
    %% the original right form should write like this:
    %% do([ Monad ||
    %%      _ = io:format("hello world", []),
    %%      return(ok)
    %%   ]).
    case X of
        ok ->
            F(ok);
        _ ->
            M:bind(X, F)
    end.

-spec then(monad(M, _A), monad(M, B)) -> monad(M, B) when M :: monad_type().
then(X, Y) ->
    Monad = get_type(X),
    then(X, Y, Monad).

-spec then(monad(M, _A), monad(M, B), M) -> monad(M, B) when M :: monad_type().
then(X, Y, M) ->
    bind(X, fun(_) -> Y end, M).

-spec return(A, M) -> monad(M, A) when M :: monad_type().
return(A, M) ->
    M:return(A).

-spec fail(_E, M) -> monad(M, _A) when M :: monad_type().
fail(MA, M) ->
    M:fail(MA).

nested_withs(Fun, Withs, Data) ->
    FinalWith = 
        lists:foldl(
            fun(With, FunAcc) ->
                fun(Data1) -> With(FunAcc, Data1) end
            end, Fun, Withs),
    FinalWith(Data).

sequence_withs(Fun, Withs, Data) ->
    lists:foldl(fun(With, DataAcc) -> With(Fun, DataAcc) end, Data, Withs).

get_type(#{?STRUCT_KEY := ?RETURN_OK}) ->
    erl_af_return;
get_type(#{?STRUCT_KEY := ?RETURN_FAIL}) ->
    erl_af_return;
get_type(#{?STRUCT_KEY := ?TRAVERSE_M}) ->
    erl_af_traverse_m;
get_type(Other) ->
    exit({unsupported_monad, Other}).
