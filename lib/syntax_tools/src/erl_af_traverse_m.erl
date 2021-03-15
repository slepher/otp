%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%% traverse monad for {@link erl_af:map_m/3}
%%% @end
%%% Created :  6 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_af_traverse_m).

%%%===================================================================
%%% macros
%%%===================================================================
-include("af_struct_name.hrl").

-define(STATE_OK, erl_af_traverse_m_state_ok).
-define(STATE_FAIL, erl_af_traverse_m_state_fail).

-compile({no_auto_import, [error/1, get/0, put/1, nodes/1]}).
%%%===================================================================
%%% types
%%%===================================================================
-export_type([struct/2]).

-opaque struct(S, A) :: #{?STRUCT_KEY => ?TRAVERSE_M, inner => inner_type(S, A)}.

-type inner_type(S, A) :: fun((module(), S, erl_af_error:struct()) -> state_struct(S, A)).

-type state_struct(S, A) :: 
        #{?STRUCT_KEY => ?STATE_OK,
          return => A,
          updated => boolean(),
          continue => boolean() | fun((any()) -> any()),
          state => S, 
          error => erl_af_error:struct()} |
        #{?STRUCT_KEY => ?STATE_FAIL,
          state => S, 
          error => erl_af_error:struct()}.

-type formatter() :: module().
-type line() :: integer().
-type convertable(S, A) :: erl_af_walk_return:struct(S, A) | erl_af_return:struct(A) | state_struct(S, A).

%%%===================================================================
%%% API
%%%===================================================================
-export([erl_af_traverse_m/1]).
-export([convertable_struct/1]).
-export([run/3, eval/3, exec/3]).
-export([lift_m/2, map_m/2, sequence_m/1]).
-export([bind/2, then/2, return/1]).
-export([fail/1, fail/2, fails/1]).
-export([fail_on_error/1]).
-export([with_error/2, catch_fail/2, set_fail/1, catched_nodes/1]).
-export([state/1, get/0, put/1, modify/1]).
-export([clear_continue/1, set_continue/2, listen_continue/1, listen_updated/1]).
-export([set_updated/1, nodes/1]).
-export([with_formatter/2]).
-export([warning/1, warnings/1, formatted_warnings/1, error/1, errors/1, formatted_errors/1]).
-export([update_file/1, eof/0, update_line/2]).
-export([sequence_nodes/1, deep_sequence_nodes/1, deep_r_sequence_nodes/1]).
-export([transform_mapfold_f/1]).
%%%===================================================================
%%% API
%%%===================================================================
-spec erl_af_traverse_m(convertable(S, A)) -> struct(S, A).
erl_af_traverse_m(#{?STRUCT_KEY := ?WALK_RETURN} = Map) ->
    Inner =
        fun(_Formatter, State0, ErrorStruct0) ->
                State1 = maps:get(state, Map, State0),
                Continue = maps:get(continue, Map, false),
                {Return, Updated} = return_with_updated(Map),
                Errors = maps:get(errors, Map, []),
                Warnings = maps:get(warnings, Map, []),
                Error1 = erl_af_error:append_ews(Errors, Warnings, ErrorStruct0),
                case Errors of
                    [] ->
                        state_ok(#{return => Return, state => State1,
                                   continue => Continue, updated => Updated, error => Error1});
                    _ ->
                        state_fail(#{state => State1, error => Error1})
                end
        end,
    new(Inner);
erl_af_traverse_m(#{?STRUCT_KEY := ?RETURN_OK, return := Return, error := ErrorStruct1}) ->
    Inner =
        fun(_Formatter, State, ErrorStruct0) ->
                ErrorStruct2 = erl_af_error:merge(ErrorStruct0, ErrorStruct1),
                state_ok(#{return => Return, state => State, error => ErrorStruct2})
        end,
    new(Inner);
erl_af_traverse_m(#{?STRUCT_KEY := ?RETURN_FAIL, error := ErrorStruct1}) ->
    Inner =
        fun(_Formatter, State, ErrorStruct0) ->
                ErrorStruct2 = erl_af_error:merge(ErrorStruct0, ErrorStruct1),
                state_fail(#{state => State, error => ErrorStruct2})
        end,
    new(Inner);
erl_af_traverse_m(#{?STRUCT_KEY := ?TRAVERSE_M} = MA) ->
    MA.

return_with_updated(#{node := Node}) ->
    {Node, true};
return_with_updated(#{nodes := Nodes}) ->
    {Nodes, true};
return_with_updated(#{} = WalkReturn) ->
    Return = maps:get(return, WalkReturn, ok),
    {Return, false}.

convertable_struct(#{?STRUCT_KEY := Key}) ->
    convertable_struct_key(Key);
convertable_struct(_Other) ->
    false.

convertable_struct_key(?RETURN_OK) ->
    true;
convertable_struct_key(?RETURN_FAIL) ->
    true;
convertable_struct_key(?WALK_RETURN) ->
    true;
convertable_struct_key(?TRAVERSE_M) ->
    true;
convertable_struct_key(_) ->
    false.


-spec new(inner_type(S, A)) -> struct(S, A).
new(Inner) when is_function(Inner, 3) ->
    #{?STRUCT_KEY => ?TRAVERSE_M, inner => Inner}.

state_ok(Map) ->
    update_m_state(#{?STRUCT_KEY => ?STATE_OK}, Map).

state_fail(Map) ->
    update_m_state(#{?STRUCT_KEY => ?STATE_FAIL}, Map).

-spec run(struct(S, A), formatter(), S) -> erl_af_return:struct({A, S}).
run(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    case run_0(MA, Formatter, State, erl_af_error:new()) of
        #{?STRUCT_KEY := ?STATE_OK, return := Return, state := State1, error := Error} ->
            erl_af_return:ok({Return, State1}, Error);
        #{?STRUCT_KEY := ?STATE_FAIL, error := Error} ->
            erl_af_return:fail(Error)
    end.

-spec eval(struct(S, A), formatter(), S) -> erl_af_return:struct(A).
eval(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    erl_af_monad:lift_m(fun({A, _State}) -> A end, run(MA, Formatter, State), erl_af_return).

-spec exec(struct(S, _A), formatter(), S) -> erl_af_return:struct(S).
exec(#{?STRUCT_KEY := ?TRAVERSE_M} = MA, Formatter, State) ->
    erl_af_monad:lift_m(fun({_A, State1}) -> State1 end, run(MA, Formatter, State), erl_af_return).

-spec lift_m(fun((A) -> B), struct(S, A)) -> struct(S, B).
lift_m(F, MA) ->
    erl_af_monad:lift_m(F, MA, ?MODULE).

-spec map_m(fun((A) -> struct(S, B)), [struct(S, A)]) -> struct(S, B).
map_m(F, MAs) ->
    erl_af_monad:map_m(F, MAs, ?MODULE).

-spec sequence_m([struct(S, A)]) -> struct(S, [A]).
sequence_m(MAs) ->
    erl_af_monad:sequence_m(MAs, ?MODULE).

-spec bind(struct(S, A), fun((A) -> struct(S, B))) -> struct(S, B).
bind(MA, KMB) when is_function(KMB, 1) ->
    map_m_state_ok(
      fun(Formatter, #{return := A, state := State1,
                       updated := Updated1, continue := Continue1,
                       error := Error1}) ->
              MB = run_0(KMB(A), Formatter, State1, Error1),
              case MB of
                  #{?STRUCT_KEY := ?STATE_OK, updated := Updated2, continue := Continue2} ->
                      Updated3 = Updated1 or Updated2,
                      Continue3 = merge_continue(Continue1, Continue2),
                      update_m_state(MB, #{updated => Updated3, continue => Continue3});
                  #{?STRUCT_KEY := ?STATE_FAIL} ->
                      MB
              end
      end, MA).

merge_continue(Continue1, Continue2) when is_boolean(Continue1), is_boolean(Continue2) ->
    Continue1 or Continue2;
merge_continue(Continue1, _Continue2) when is_function(Continue1) ->
    Continue1;
merge_continue(_Continue1, Continue2) when is_function(Continue2) ->
    Continue2.

-spec then(struct(S, _A), struct(S, B)) -> struct(S, B).
then(MA, MB) ->
    bind(MA, fun(_) -> MB end).

-spec return(A) -> struct(_S, A).
return(A) ->
    Inner = 
        fun(_Formatter, State, Error) ->
                state_ok(#{return => A, state => State, error => Error})
        end,
    new(Inner).

-spec fail_on_error(struct(S, A)) -> struct(S, A).
fail_on_error(MA) ->
    map_m_state_ok(
      fun(#{state := State1, error := Error} = StateOk) ->
              case erl_af_error:is_empty_error(Error) of
                  true ->
                      StateOk;
                  false ->
                      state_fail(#{state => State1, error => Error})
              end
        end, MA).

-spec fail(_E) -> struct(_S, _A).
fail(E) ->
    fails([E]).

-spec fail(_E, ?TRAVERSE_M) -> struct(_S, _A).
fail(E, ?TRAVERSE_M) ->
    fails([E]).

-spec fails([_E]) -> struct(_S, _A).
fails(Es) ->
    Inner =
        fun(_Formatter, State, Error0) ->
                Error1 = erl_af_error:append_errors(Es, Error0),
                state_fail(#{state => State, error => Error1})
        end,
    new(Inner).
%%%===================================================================
%%% state related functions.
%%%===================================================================
-spec state(fun((S) -> {A, S})) -> struct(S, A).
state(F) ->
    Inner = 
        fun(_Formatter, State0, Error) ->
                {A, State1} = F(State0),
                state_ok(#{return => A, state => State1, error => Error})
        end,
    new(Inner).

-spec modify(fun((S) -> S)) -> struct(S, ok).
modify(F) ->
    state(fun(State) -> State1 = F(State), {ok, State1} end).

-spec get() -> struct(S, S).
get() ->
    state(fun(State) -> {State, State} end).

-spec put(S) -> struct(S, ok).
put(State) ->
    state(fun(_State) -> {ok, State} end).

%%%===================================================================
%%% nodes updated continue related functions
%%%===================================================================
clear_continue(MA) ->
    set_continue(MA, false).

set_continue(MA, Continue) ->
    map_m_state_ok(
      fun(#{} = StateM) ->
              update_m_state(StateM, #{continue => Continue})
      end, MA).

set_updated(MA) ->
    map_m_state_ok(
      fun(#{} = StateM) ->
              update_m_state(StateM, #{updated => true})
      end, MA).

listen_continue(MA) ->
    map_m_state_ok(
      fun(#{return := Return, continue := Continue} = StateM) ->
              update_m_state(StateM, #{return => {Return, Continue}, continue => false})
      end, MA).

listen_updated(MA) ->
    map_m_state_ok(
      fun(#{return := Return, updated := Updated} = StateM) ->
              update_m_state(StateM, #{return => {Return, Updated}})
      end, MA).

nodes(Nodes) ->
    set_updated(return(Nodes)).
%%%===================================================================
%%% error_state related functions
%%%===================================================================

-spec with_error(fun((erl_af_error:struct())
                     -> erl_af_error:struct()),
                 struct(S, A))
                -> struct(S, A).
with_error(F, MA) ->
    map_m_state(
      fun(#{error := Error1} = MState) ->
              Error2 = F(Error1),
              update_m_state(MState, #{error => Error2})
        end, MA).

catch_fail(F, MA) ->
    map_m_state(
        fun(_Formatter, #{?STRUCT_KEY := ?STATE_OK} = StateM) ->
                StateM;
           (Formatter, #{?STRUCT_KEY := ?STATE_FAIL, state := State1, error := Error1}) ->
                run_0(F(), Formatter, State1, Error1)
        end, MA).

set_fail(MA) ->
    map_m_state(
        fun(_Formatter, #{?STRUCT_KEY := ?STATE_OK, state := State, error := Error}) ->
            state_fail(#{state => State, error => Error})
        end, MA).

catched_nodes(MA) ->
    catch_fail(fun() -> nodes([]) end, MA).

-spec modify_error(fun((erl_af_error:struct())
                     -> erl_af_error:struct()))
                -> struct(_S, _A).
modify_error(F) ->
    Inner = 
        fun(_Formatter, State, Error0) ->
                Error1 = F(Error0),
                state_ok(#{return => ok, state => State, error => Error1})
        end,
    new(Inner).

-spec with_formatter(fun((formatter()) -> formatter()), struct(S, A)) -> struct(S, A).
with_formatter(Formatter, MA) ->
    Inner = 
        fun(_Formatter0, State, Error) ->
                  run_0(MA, Formatter, State, Error)
        end,
    new(Inner).

-spec warning(term()) -> struct(_S, _A).
warning(Warning) ->
    warnings([Warning]).

-spec warnings([term()]) -> struct(_S, _A).
warnings(Warnings) ->
    modify_error(
      fun(Error) ->
              erl_af_error:append_warnings(Warnings, Error)
      end).

-spec formatted_warnings([{line(), formatter(), term()}]) -> struct(_S, _A).
formatted_warnings(Warnings) ->
    modify_error(
      fun(Error) ->
              erl_af_error:append_formatted_warnings(Warnings, Error)
      end).

-spec error(term()) -> struct(_S, _A).
error(Error) ->
    errors([Error]).

-spec errors([term()]) -> struct(_S, _A).
errors(Errors) ->
    modify_error(
      fun(Error) ->
              erl_af_error:append_errors(Errors, Error)
      end).

-spec formatted_errors([{line(), formatter(), term()}]) -> struct(_S, _A).
formatted_errors(Errors) ->
    modify_error(
      fun(Error) ->
              erl_af_error:append_formatted_errors(Errors, Error)
      end).

-spec update_file(file:filename()) -> struct(_S, _A).
update_file(File) ->
    modify_error(
      fun(Error) ->
              erl_af_error:update_file(File, Error)
      end).

-spec eof() -> struct(_S, _A).
eof() ->
    modify_error(
      fun(Error) ->
              erl_af_error:eof(Error)
      end).

-spec update_line(line(), struct(S, A)) -> struct(S, A).
update_line(Line, MA) ->
    map_m_state(
      fun(Formatter, #{error := Error0, error := Error0} = MState) ->
              case erl_af_error:no_pending(Error0) of
                  true ->
                      MState;
                  false ->
                      Error1 = erl_af_error:update_line(Line, Formatter, Error0),
                      update_m_state(MState, #{error => Error1})
              end
        end, MA).

sequence_nodes([MA|MAs]) ->
    bind(
      lift_m(
        fun(Nodes) when is_list(Nodes) -> Nodes;
           (Node) -> [Node]
        end, catched_nodes(MA)),
      fun(HeadNodes) ->
              bind(
                sequence_nodes(MAs),
                fun(TailNodes) ->
                        return(HeadNodes ++ TailNodes)
                end)
      end);
sequence_nodes([]) ->
    return([]);
sequence_nodes(MA) ->
    MA.

deep_sequence_nodes(MAss) ->
    fail_on_error(
      map_m(
        fun(NodeMs) when is_list(NodeMs) -> sequence_nodes(NodeMs);
           (NodesM) ->
                NodesM
        end, MAss)).

deep_r_sequence_nodes(MAss) ->
    lift_m(fun lists:reverse/1, deep_sequence_nodes(lists:reverse(MAss))).

transform_mapfold_f(F) ->
    fun(Node, Attr) ->
            bind(
              get(),
              fun(State) ->
                    case F(Node, State, Attr) of
                        #{?STRUCT_KEY := ?TRAVERSE_M} = TraverseM ->
                            TraverseM;
                        #{?STRUCT_KEY := ?RETURN_OK} = Return ->
                            erl_af_traverse_m:erl_af_traverse_m(Return);
                        #{?STRUCT_KEY := ?RETURN_FAIL} = Return ->
                            erl_af_traverse_m:erl_af_traverse_m(Return);
                        Return ->
                            Concereter =
                                fun() ->
                                    case Return of
                                        {Node1, State1} ->
                                            #{node => Node1, state => State1};
                                        Node1 ->
                                            #{node => Node1}
                                    end
                                end,
                            erl_af_traverse_m:erl_af_traverse_m(erl_af_walk_return:new(Concereter, Return))
                    end
              end)
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
run_0(#{?STRUCT_KEY := ?TRAVERSE_M, inner := Inner},
      Formatter, State, #{?STRUCT_KEY := ?ERROR_STATE} = Error) ->
    Inner(Formatter, State, Error).

map_m_state(F, MA) ->
    Inner = 
        fun(Formatter, State, #{?STRUCT_KEY := ?ERROR_STATE} = Error0) ->
                MState1 = run_0(MA, Formatter, State, Error0),
                MState2 = apply_map_state_m_f(F, Formatter, MState1),
                case MState2 of
                    #{?STRUCT_KEY := ?STATE_OK} ->
                        MState2;
                    #{?STRUCT_KEY := ?STATE_FAIL} ->
                        MState2;
                    _ ->
                        exit({invalid_m_state_after_map, MState1, MState2})
                end
        end,
    new(Inner).

map_m_state_ok(F, MA) ->
    map_m_state(
      fun(Formatter, #{?STRUCT_KEY := ?STATE_OK} = StateM) ->
              apply_map_state_m_f(F, Formatter, StateM);
         (_Formatter, #{?STRUCT_KEY := ?STATE_FAIL} = StateM) ->
              StateM
      end, MA).

apply_map_state_m_f(F, _Formatter, MState) when is_function(F, 1) ->
    F(MState);
apply_map_state_m_f(F, Formatter, MState) when is_function(F, 2) ->
    F(Formatter, MState).

update_m_state(#{} = State, #{} = Map) ->
    merge_struct(State, Map, #{?STATE_OK => [return, state, error, updated, continue],
                               ?STATE_FAIL => {[state, error], [return, updated, continue]}}).

merge_struct(#{?STRUCT_KEY := StructName} = Struct, Map, KeyMap) when is_map(KeyMap)->
    case maps:find(StructName, KeyMap) of
        {ok, {Keys, OptionalKeys}} ->
            merge_struct(Struct, Map, Keys, OptionalKeys);
        {ok, Keys} ->
            merge_struct(Struct, Map, Keys, []);
        error ->
            erlang:error({invalid_struct, StructName})
    end.
merge_struct(#{?STRUCT_KEY := StructName} = Struct, Map, Keys, OptionalKeys) when is_list(Keys), is_list(OptionalKeys) ->
    RestKeys = maps:keys(Map) -- Keys -- OptionalKeys -- [?STRUCT_KEY],
    case RestKeys of
        [] ->
            lists:foldl(
              fun(Key, Acc) ->
                      case maps:find(Key, Map) of
                          {ok, Value} ->
                              case validate_struct_value(Key, Value) of
                                  true ->
                                      maps:put(Key, Value, Acc);
                                  false ->
                                      erlang:error({invalid_struct_value, StructName, Key, Value})
                              end;
                          error ->
                              case maps:is_key(Key, Acc) of
                                  true ->
                                      Acc;
                                  false ->
                                      init_struct_value(Acc, Key)
                              end
                      end
              end, Struct, Keys);
        _ ->
            erlang:error({invalid_merge_keys, StructName, RestKeys, Map})
    end.

validate_struct_value(return, _Return) ->
    true;
validate_struct_value(state, _State) ->
    true;
validate_struct_value(error, #{?STRUCT_KEY := ?ERROR_STATE}) ->
    true;
validate_struct_value(updated, Updated) when is_boolean(Updated) ->
    true;
validate_struct_value(continue, Continue) when is_boolean(Continue) ->
    true;
validate_struct_value(continue, Continue) when is_function(Continue, 1) ->
    true;
validate_struct_value(_Key, _Value) ->
    false.

init_struct_value(#{} = Struct, error) ->
    Struct#{error => erl_af_error:new()};
init_struct_value(#{} = Struct, updated) ->
    Struct#{updated => false};
init_struct_value(#{} = Struct, continue) ->
    Struct#{continue => false};
init_struct_value(#{?STRUCT_KEY := StructName}, Key) ->
    erlang:error({struct_value_required, StructName, Key}).
