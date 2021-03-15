%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_af_do_transform).

-include("quote.hrl").

%% API
-export([parse_transform/2]).
-export([format_error/1]).
-export([do/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    TransformOpts = #{formatter => ?MODULE, traverse => post, parse_transform => true},
    erl_af:map(fun walk_node/2, Forms, TransformOpts).

format_error(non_empty_do) ->
    "A 'do' construct cannot be empty";
format_error(non_last_expression) ->
    "The last statement in a 'do' construct must be an expression";
format_error(Reason) ->
    erl_af:format_error(Reason).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk_node({call, _Line, {atom, __Line1, do},
           [{lc, _Line2, _Monad, _Comprehensions} = LCNode]}, #{}) ->
    do(LCNode, #{monad => erl_af_monad, monad_fail => erl_af_monad, bind => bind});
walk_node(Node, _Attrs) ->
    Node.

do(Ast, #{monad := MonadClass, monad_fail := MonadFailClass, bind := Bind} = Opts) 
  when is_atom(MonadClass), is_atom(MonadFailClass), is_atom(Bind) ->
    do_1(Ast, Opts);
do(_Ast, Opts) ->
    {error, {invalid_options, Opts}}.

do_1({lc, Line, {MonadType, _Line3, _MonadModule} = Monad, Comprehensions}, Opts) ->
    case lists:member(MonadType, [atom, var, tuple]) of
        true ->
            case do_comprehensions(Comprehensions, Monad, Opts) of
                Expressions when is_list(Expressions) ->
                    quote((fun() ->
                                   unquote_splicing(Expressions)
                           end)(), Line);
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, expected_monad_type}
    end;
do_1(_Ast, _Opts) ->
    {error, expected_list_comprehension}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%  'do' syntax transformation:
do_comprehensions([], _Monad, _Opts) ->
    {error, non_empty_do};
do_comprehensions([{GenerateOrMatch, _Line, _Pattern, _Expr}], _Monad, _Opts)
  when GenerateOrMatch =:= generate orelse GenerateOrMatch =:= match ->
    {error, non_last_expression};
do_comprehensions([{generate, Line, Pattern, Expr} | Exprs], Monad, #{monad := MonadClass, bind := Bind} = Opts) ->
    %% "Pattern <- Expr, Tail" where Pattern is a simple variable
    %% is transformed to
    %% "MonadClass:Bind(Expr, fun (Pattern) -> Tail end, Monad)"
    %% without a fail to match clause
    MonadAtom = {atom, Line, MonadClass},
    BindAtom = {atom, Line, Bind},
    Expr1 = update_expression(Expr, Monad, Opts),
    bind(
      bind_expression(Line, Pattern, Exprs, Monad, Opts),
      fun(BindExpr) ->
              [quote((unquote(MonadAtom)):(unquote(BindAtom))(unquote(Expr1), unquote(BindExpr), unquote(Monad)), Line)]
      end);
do_comprehensions([Expr], Monad, Opts) ->
    %% Don't do '>>' chaining on the last elem
    Expr1 = update_expression(Expr, Monad, Opts),
    [Expr1];
do_comprehensions([{match, _Line, _Pattern, _Expr} = Expr | Exprs], Monad, Opts) ->
    %% Handles 'let binding' in do expression a-la Haskell
    Expr1 = update_expression(Expr, Monad, Opts),
    bind(
      do_comprehensions(Exprs, Monad, Opts),
      fun(Exprs1) ->
              [Expr1|Exprs1]
      end);
do_comprehensions([Expr | Exprs], Monad, #{monad := MonadClass, bind := Bind} = Opts) ->
    %% "Expr, Tail" is transformed to "MonadClass:Bind(Monad, Expr, fun (_) -> Tail')"
    Line = erl_syntax:get_pos(Expr),
    MonadAtom = {atom, Line, MonadClass},
    BindAtom = {atom, Line, Bind},
    Expr1 = update_expression(Expr, Monad, Opts),
    bind(
      do_comprehensions(Exprs, Monad, Opts),
      fun(Exprs1) ->
            [quote((unquote(MonadAtom)):(unquote(BindAtom))(
                unquote(Expr1), fun(_) -> unquote_splicing(Exprs1) end, unquote(Monad)), Line)]
      end).

update_expression(Expression, Monad, #{monad := MonadClass, monad_fail := MonadFailClass}) ->
    erl_af:map(
      fun({call, Line, {atom, _Line1, fail}, [Arg]}, _Attr) ->
              MonadFailAtom = {atom, Line, MonadFailClass},
              %% 'return' calls of a particular form:
              %% return(Argument), and
              %% Transformed to:
              %% "MonadClass:return(Argument, Monad)" in monadic context
              quote((unquote(MonadFailAtom)):fail(unquote(Arg), unquote(Monad)), Line);
         ({call, Line, {atom, _Line1, return}, [Arg]}, _Attr) ->
              MonadAtom = {atom, Line, MonadClass},
              %% 'fail' calls of a particular form:
              %% fail(Argument)
              %% Transformed to:
              %% "MonadFailClass:fail(Argument, Monad)" in monadic context
              quote((unquote(MonadAtom)):return(unquote(Arg), unquote(Monad)), Line);
         (Node, _Attr) ->
              Node
      end, Expression, #{formatter => ?MODULE, simplify_return => true}).

bind_expression(Line, {var, _Line, _Var} = Pattern, Exprs, Monad, Opts) ->
    bind(
      do_comprehensions(Exprs, Monad, Opts),
      fun(Exprs1) ->
              quote(
                fun(unquote = Pattern) ->
                        unquote_splicing(Exprs1)
                end, Line)
      end);
bind_expression(Line, Pattern, Exprs, Monad, #{monad_fail := MonadFailClass} = Opts) ->
    bind(
      do_comprehensions(Exprs, Monad, Opts),
      fun(Exprs1) ->
              LineExpr = erl_af_lib:abstract_form(Line, Line),
              String = erl_af_lib:abstract_form(erl_af_lib:ast_to_string(Pattern), Line),
              MonadFailAtom = {atom, Line, MonadFailClass},
              quote(
                fun(unquote = Pattern) ->
                        unquote_splicing(Exprs1);
                   (Var) ->
                        (unquote(MonadFailAtom)):fail({monad_badmatch, Var, unquote(LineExpr), unquote(String)}, unquote(Monad))
                end, Line)
      end).

bind(Expr, K) ->
    case Expr of
        {error, Reason} ->
            {error, Reason};
        Expr ->
            K(Expr)
    end.
