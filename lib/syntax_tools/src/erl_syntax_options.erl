%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 11 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_syntax_options).

-include("erl_syntax_struct_name.hrl").

-type options() :: option() | [option()] | option_map().
-type option() :: atom() | {atom(), term()}.
-type option_map() :: #{atom() => term()}.
-type validators() :: validator() | [validator()].
-type validator() :: atom() | {atom(), term()} | validator_fun().
-type validator_fun() :: fun((term()) -> term()) | fun((term(), term()) -> term()).

%% API
-export([with_attribute/5]).
-export([forms_with_attribute/5]).
-export([options/1]).
-export([validate/2, validate/3]).
-export([by_validator/3]).
-export([get_boolean/4]).

%% Validator Functions
-export([boolean/1, number/1, atom/1]).
-export([list_of/2]).
-export([default/3, required/2]).
%%%===================================================================
%%% API
%%%===================================================================
get_boolean(Key, ReverseKey, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} when is_boolean(Value) ->
            Value;
        error ->
            case maps:find(ReverseKey, Map) of
                {ok, RValue} when is_boolean(RValue) ->
                    not RValue;
                error ->
                    Default
            end
    end.

with_attribute(Fun, Init, Forms, Attr, Opts) ->
    erl_syntax_traverse:reduce(
      fun({attribute, Line, Attr1, AttrValue} = Node, Acc, #{}) when Attr1 == Attr ->
              erl_syntax_traverse_m:bind(
                    values_apply_fun_m(Node, Fun, AttrValue, Acc, #{line => Line}),
                fun(Acc1) ->
                        erl_syntax_traverse_m:put(Acc1)
                end);
         (_Node, Acc, #{}) ->
              Acc
      end, Init, Forms, Opts#{traverse => list}).

forms_with_attribute(Fun, Init, Forms, Attr, Opts) ->
    Fun1 = update_with_attribute_f(Fun),
    erl_syntax_traverse:mapfold(
      fun({attribute, Line, Attr1, AttrValue} = Node, Acc, #{}) when Attr1 == Attr ->
              erl_syntax_traverse_m:bind(
                values_apply_fun_m(Node, Fun1, AttrValue, {[], Acc}, #{line => Line}),
                fun({Nodes, State}) ->
                        erl_syntax_traverse_m:then(
                          erl_syntax_traverse_m:nodes([Node|Nodes]),
                          erl_syntax_traverse_m:put(State))
                end);
         (Node, Acc, #{}) ->
              {Node, Acc}
      end, Init, Forms, Opts#{traverse => list}).

-spec options(options()) -> erl_syntax_base_m:erl_syntax_base_m(option_map()).
options(Atom) when is_atom(Atom) ->
    options([Atom]);
options({Key, Value}) when is_atom(Key) ->
    options([{Key, Value}]);
options(OptionList) when is_list(OptionList) ->
    erl_syntax_monad:foldl_m(
      fun({Key, Value}, Acc) when is_atom(Key) ->
              maps:put(Key, Value, Acc);
         (Key, Acc) when is_atom(Key) ->
              maps:put(Key, true, Acc);
         (Value, Acc) ->
              {warning, Acc, {invalid_option_value, Value}}
      end, maps:new(), OptionList, erl_syntax_base_m);
options(Options) when is_map(Options) ->
    options(maps:to_list(Options));
options(Options) ->
    erl_syntax_base_m:to_monad({warning, #{}, {invalid_option_value, Options}}).

-spec validate(#{atom() => validators()}, options(), #{}) -> erl_syntax_base_m:erl_syntax_base_m(option_map()).
validate(Validator, ToValidate) ->
    validate(Validator, ToValidate, #{}).

validate(Validator, ToValidate, Options) ->
    erl_syntax_base_m:bind(
      options(ToValidate),
      fun(ToValidate1) ->
              validate_1(Validator, ToValidate1, Options)
      end).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
values_apply_fun_m(Node, Fun, AttrValues, Acc, Opts) ->
      erl_syntax_walk_return:to_traverse_m(values_apply_fun(Fun, AttrValues, Acc, Opts), Node).

values_apply_fun(Fun, AttrValues, Acc, Opts) when is_list(AttrValues) ->
    case maps:get(deep_attr, Opts, true) of
        true ->
            erl_syntax_monad:foldl_m(
              fun(AttrValue, Acc1) ->
                      value_apply_fun(Fun, AttrValue, Acc1, Opts)
              end, Acc, AttrValues, erl_syntax_base_m);
        false ->
            value_apply_fun(Fun, AttrValues, Acc, Opts)
    end;
values_apply_fun(Fun, AttrValue, Acc, Opts) ->
    value_apply_fun(Fun, AttrValue, Acc, Opts).

value_apply_fun(Fun, Value, Acc, _Opts) when is_function(Fun, 2) ->
    Fun(Value, Acc);
value_apply_fun(Fun, Value, Acc, Opts) when is_function(Fun, 3) ->
    Fun(Value, Acc, Opts).

update_with_attribute_f(Fun) ->
    fun(Value, {Nodes0, Acc0}, Opts) ->
            erl_syntax_base_m:bind(
              value_apply_fun(Fun, Value, Acc0, Opts),
              fun({Nodes1, Acc1}) ->
                      erl_syntax_base_m:return({Nodes0 ++ Nodes1, Acc1})
              end)
    end.              

validate_1(ValidatorMap, ToValidate, _Options) when is_map(ValidatorMap) ->
    erl_syntax_monad:foldl_m(
      fun({Key, Validator}, Acc) ->
              case maps:find(Key, ToValidate) of
                  {ok, Value} ->
                      by_validator_acc(Validator, Key, Value, Acc, true);
                  error ->
                      by_validator_acc(Validator, Key, undefined, Acc, false)
              end
      end, maps:new(), maps:to_list(ValidatorMap), erl_syntax_base_m).

by_validator_acc(Validator, Key, Value, Acc, IsKey) ->
    case by_validator(Validator, Value, IsKey) of
        {ok, Value1} ->
            maps:put(Key, Value1, Acc);
        {error, Reason} ->
            {error, Acc, {Reason, Key, Value}};
        {warning, Reason} ->
            {warning, Acc, {Reason, Key, Value}};
        skip ->
            Acc
    end.

by_validator(Validator, Value, IsKey) ->
    Return = by_validator_1(Validator, Value, IsKey),
    update_return(Value, Return, Validator).

by_validator_1(required, Value, IsKey) ->
    required(Value, IsKey);
by_validator_1({default, Default}, Value, IsKey) ->
    default(Value, Default, IsKey);
by_validator_1([Validator|T], Value, IsKey) ->
    case by_validator(Validator, Value, IsKey) of
        {ok, Value1} ->
            by_validator(T, Value1, IsKey);
        skip ->
            by_validator(T, Value, IsKey);
        {warning, Reason} ->
            {warning, Reason};
        {error, Reason} ->
            {error, Reason}
    end;
by_validator_1(_Validator, _Value, false) ->
    skip;
by_validator_1(Validator, Value, _IsKey) when is_atom(Validator) ->
    apply(?MODULE, Validator, [Value]);
by_validator_1({Validator, Args}, Value, _IsKey) when is_atom(Validator) ->
    apply(?MODULE, Validator, [Value, Args]);
by_validator_1(Validator, Value, _IsKey) when is_function(Validator, 1) ->
    Validator(Value);

by_validator_1([], Value, _IsKey) ->
    {ok, Value}.

update_return(Value, ok, _Validator) -> 
    {ok, Value};
update_return(_Value, {ok, Value1}, _Validator) -> 
    {ok, Value1};
update_return(Value, true, _Validator) -> 
    {ok, Value};
update_return(_Value, false, _Validator) -> 
    {warning, invalid_value};
update_return(_Value, {warning, Reason}, _Validator) -> 
    {warning, Reason};
update_return(_Value, {error, Reason}, _Validator) -> 
    {error, Reason};
update_return(_Value, skip, _Validator) ->
    skip;
update_return(Value, Other, Validator) -> 
    exit({invalid_validator_return_for, Validator, Value, Other}).

%%%===================================================================
%%% validator functions
%%%===================================================================
boolean(Bool) ->
    erlang:is_boolean(Bool).

number(Number) ->
    erlang:is_number(Number).

atom(Atom) ->
    erlang:is_atom(Atom).

list_of(Values, Validator) ->
    list_of(Values, Validator, []).

list_of([H|T], Validator, Acc) ->
    case erl_syntax_options:by_validator(Validator, H, true) of
        {ok, H1} ->
            list_of(T, Validator, [H1|Acc]);
        {error, Reason} ->
            {error, Reason};
        {warning, Reason} ->
            {warning, Reason}
    end;
list_of([], _Validator, Acc) ->
    {ok, lists:reverse(Acc)};
list_of(_Other, _Validator, _Acc) ->
    {error, invalid_list}.

default(_Value, Default, false) ->
    {ok, Default};
default(Value, _Default, true) ->
    {ok, Value}.

required(_Value, false) ->
    {error, required};
required(Value, true) ->
    {ok, Value}.