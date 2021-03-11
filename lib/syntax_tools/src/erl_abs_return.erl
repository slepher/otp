%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%% return type of {@link erl_abs:map/3}, {@link erl_abs:reduce/4}, {@link erl_abs:map_with_state/4}, {@link erl_abs:mapfold/4}.
%%% @end
%%% Created :  8 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_abs_return).

-include("abs_struct_name.hrl").

-export_type([struct/1]).

-type struct(A) :: erl_abs_ok(A) | erl_abs_return_error().

-type erl_abs_ok(A) :: #{?STRUCT_KEY := ?RETURN_OK,
                                return := A,
                                error := erl_abs_error:struct()}.

-type erl_abs_return_error() :: #{?STRUCT_KEY := ?RETURN_FAIL,
                                  error := erl_abs_error:struct()}.

-compile({no_auto_import, [error/1]}).

-export([ok/1, ok/2, fail/0, fail/1]).
-export([warning_ok/2, warnings_ok/2, error_ok/2, errors_ok/2, error_fail/1, errors_fail/1]).
-export([run/1, run_error/1]).
-export([from_compiler/1]).
-export([to_monad/1]).
-export([to_compiler/1]).
-export([simplify/1]).
-export([bind/2, return/1]).
-export([then/2]).
-export([with_error/2]).
-export([error/1, errors/1, warning/1, warnings/1]).
-export([has_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
%% @spec ok(A) -> struct(A)
-spec ok(A) -> struct(A).
ok(Return) ->
    ok(Return, erl_abs_error:new()).

%% @spec ok(A, erl_abs_error:struct()) -> struct(A)
-spec ok(A, erl_abs_error:struct()) -> struct(A).
ok(Return, #{?STRUCT_KEY := ?ERROR_STATE} = Error) ->
    #{?STRUCT_KEY => ?RETURN_OK, return => Return, error => Error}.

-spec fail() -> struct(ok).
fail() ->
    fail(erl_abs_error:new()).

-spec fail(erl_abs_error:struct()) -> struct(ok).
fail(#{?STRUCT_KEY := ?ERROR_STATE} = Error) ->
    #{?STRUCT_KEY => ?RETURN_FAIL, error => Error}.

warning_ok(Warning, Return) ->
    then(warning(Warning), ok(Return)).

warnings_ok(Warnings, Return) when is_list(Warnings) ->
    then(warnings(Warnings), ok(Return)).

error_ok(Error, Return) ->
    then(error(Error), ok(Return)).

errors_ok(Errors, Return) when is_list(Errors) ->
    then(errors(Errors), ok(Return)).

error_fail(Error) ->
    then(error(Error), fail()).

errors_fail(Errors) when is_list(Errors) ->
    then(errors(Errors), fail()).



-spec run(struct(A)) -> {just, A} | nothing.
run(#{?STRUCT_KEY := ?RETURN_OK, return := Return}) ->
    {just, Return};
run(#{?STRUCT_KEY := ?RETURN_FAIL}) ->
    nothing.

-spec run_error(struct(_A)) -> erl_abs_error:struct().
run_error(#{?STRUCT_KEY := StructKey, error := Error})
  when (StructKey == ?RETURN_OK); (StructKey == ?RETURN_FAIL) ->
    Error.

from_compiler(CompilerReturn) ->
    erl_abs_lib:concerete(CompilerReturn, [fun from_compiler_1/1]).

from_compiler_1(Forms) when is_list(Forms) ->
    {ok, ok(Forms)};
from_compiler_1(Module) when is_atom(Module) ->
    {ok, ok(Module)};
from_compiler_1({warning, Forms, Warnings}) when is_list(Warnings) ->
    ErrorStruct = erl_abs_error:new(),
    ErrorStruct1 = erl_abs_error:append_file_warnings(Warnings, ErrorStruct),
    {ok, ok(Forms, ErrorStruct1)};
from_compiler_1({error, Errors, Warnings}) when is_list(Warnings), is_list(Errors) ->
    ErrorStruct = erl_abs_error:new(),
    ErrorStruct1 = erl_abs_error:append_file_warnings(Warnings, ErrorStruct),
    ErrorStruct2 = erl_abs_error:append_file_errors(Errors, ErrorStruct1),
    {ok, fail(ErrorStruct2)};
from_compiler_1(_Other) ->
    error.

from_return(#{?STRUCT_KEY := ?RETURN_OK} = MA) ->
    {ok, MA};
from_return(#{?STRUCT_KEY := ?RETURN_FAIL} = MA) ->
    {ok, MA};
from_return(_) ->
    error.

from_walk_return(#{?STRUCT_KEY := ?WALK_RETURN, return := Return, errors := Errors, warnings := Warnings}) ->
    {ok,
     erl_abs_return:then(
       erl_abs_return:then(
         erl_abs_return:warnings(Warnings),
         erl_abs_return:errors(Errors)),
       erl_abs_return:return(Return))};
from_walk_return(_) ->
    error.

to_monad(A) ->
    erl_abs_lib:concerete(A, [fun from_return/1, fun from_walk_return/1, fun from_compiler_1/1]).

-spec to_compiler(struct([erl_syntax:syntaxTree()])) ->
                         [erl_syntax:syntaxTree()] | {warning, [erl_syntax:syntaxTree()], erl_abs_error:compiler_error()} |
                         {error, erl_abs_error:compiler_error(), erl_abs_error:compiler_error()}.
to_compiler(#{?STRUCT_KEY := ?RETURN_OK, return := Forms, error := Error}) ->
    case erl_abs_error:realize(Error) of
        {[], []} ->
            Forms;
        {[], Warnings} ->
            {warning, Forms, Warnings};
        {Errors, Warnings} ->
            {error, Errors, Warnings}
    end;
to_compiler(#{?STRUCT_KEY := ?RETURN_FAIL, error := Error}) ->
    {Errors, Warnings} = erl_abs_error:realize(Error),
    {error, Errors, Warnings}.

simplify(#{?STRUCT_KEY := ?RETURN_OK, return := Return, error := Error}) ->
    case erl_abs_error:is_empty(Error) of
        true ->
            Return;
        false ->
            exit(erl_abs_error:printable(Error))
    end;
simplify(#{?STRUCT_KEY := ?RETURN_FAIL, error := Error}) ->
     exit(erl_abs_error:printable(Error)).

bind(MA, KMB) ->
    MA1 = to_monad(MA),
    bind_1(MA1, KMB).

return(A) ->
    ok(A).

then(MA, MB) ->
    bind(MA, fun(_) -> MB end).

with_error(F, #{?STRUCT_KEY := ?RETURN_OK, error := Error} = MA) ->
    Error1 = F(Error),
    MA#{error => Error1};
with_error(F, #{?STRUCT_KEY := ?RETURN_FAIL, error := Error} = MA) ->
    Error1 = F(Error), 
    MA#{error => Error1}.

error(Error) ->
    errors([Error]).

errors(Errors) ->
    ErrorStruct = erl_abs_error:new(),
    ErrorStruct1 = erl_abs_error:append_errors(Errors, ErrorStruct),
    ok(ok, ErrorStruct1).

warning(Warning) ->
    warnings([Warning]).

warnings(Warnings) ->
    ErrorStruct = erl_abs_error:new(),
    ErrorStruct1 = erl_abs_error:append_warnings(Warnings, ErrorStruct),
    ok(ok, ErrorStruct1).

has_error(#{error := ErrorStruct}) ->
    not erl_abs_error:is_empty_error(ErrorStruct).
%%%===================================================================
%%% Internal functions
%%%===================================================================
bind_1(#{?STRUCT_KEY := ?RETURN_OK, return := A, error := Error0}, KMB) ->
    #{error := Error1} = MB = to_monad(KMB(A)),
    Error2 = erl_abs_error:merge(Error0, Error1),
    MB#{error => Error2};
bind_1(#{?STRUCT_KEY := ?RETURN_FAIL} = MA, _KMB) ->
    MA.
