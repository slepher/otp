%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%% return type of traverse fun in {@link erl_abs:map/3}, {@link erl_abs:reduce/4}, {@link erl_abs:map_with_state/4}, {@link erl_abs:mapfold/4}.
%%% @end
%%% Created : 10 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_abs_walk_return).

-include("abs_struct_name.hrl").

-export_type([struct/2, convertable/2]).

-opaque struct(S, A) :: #{?STRUCT_KEY => ?WALK_RETURN,
                          return => A,
                          state => S,
                          nodes => [erl_syntax:syntaxTree()],
                          errors => [any()],
                          warnings => [any()],
                          continue => boolean()}.

-type convertable(S, A) :: convertable_map(S, A) | convertable_tuple(A).

-type convertable_map(S, A) :: #{return => A,
                                 state => S,
                                 node => erl_syntax:syntaxTree(),
                                 nodes => [erl_syntax:syntaxTree()],
                                 error => error_term(),
                                 warning => error_term(),
                                 errors => [error_term()],
                                 warnings => [error_term()],
                                 continue => boolean()}.

-type convertable_tuple(A) :: convertable_warning(A) | convertable_error(A) | {ok, A} | continue | {continue, A} | {node, A} | {nodes, [A]}.
-type convertable_warning(A) :: {warning, error_term()} | {warnings, [error_term()]} | {warning, A, error_term()} | {warnings, A, [error_term()]}.
-type convertable_error(A) :: {error, error_term()} | {errors, [error_term()]} | {error, A, error_term} | {errors, A, [error_term]}.
-type concereter(S, A) :: fun(() -> convertable_map(S, A)) | fun((term()) -> convertable_map(S, A)).
-type error_term() :: term().

-export([new/1, new/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% @spec new(convertable(S, A) | A) -> struct(S, A)
%% @see new/2
-spec new(convertable(S, A) | A) -> struct(S, A).
new(Return) ->
    new(fun() -> #{return => Return} end, Return).

%% @doc generate erl_abs_walk_return:struct(S, A) from walk function return in erl_abs:map/3 erl_abs:reduce/4, erl_abs:map_with_state/4, erl_abs:mapfold/4
%% if return type is not recognized, use DefaultConcereter to generate a map which could converted to erl_abs_walk_return:struct(S, A).
%% @end
-spec new(concereter(S, A), convertable(S, A) | term()) -> struct(S, A).
new(_Concereter, #{} = Map) ->
    Map1 = up_map(Map),
    default(Map1);
new(Concereter, Return) ->
    case erl_abs_lib:try_concerete(Return, [fun erl_abs_lib:base_concereter/1, fun walk_return_concereter/1]) of
        {ok, Map} ->
            new(Map);
        error ->
            case Concereter() of
                #{} = Map ->
                    new(Concereter, Map);
                Other ->
                    exit({expected_map_in_default_concereter, Concereter, Other})
            end
    end.

walk_return_concereter(continue) ->
    {ok, #{continue => true}};
walk_return_concereter({continue, B}) ->
    {ok, #{continue => true, return => B}};
walk_return_concereter({node, Node}) ->
    {ok, #{nodes => [Node]}};
walk_return_concereter({nodes, Nodes}) ->
    {ok, #{nodes => Nodes}};
walk_return_concereter(_Other) ->
    error.
%%%===================================================================
%%% Internal functions
%%%===================================================================
default(Map) ->
    maps:merge(#{?STRUCT_KEY => ?WALK_RETURN, return => ok, errors => [], warnings => []}, Map).

%% update convertable_map to struct map.
up_map(#{errors := Errors}) when not is_list(Errors) ->
    exit({errors_should_be_list, Errors});
up_map(#{warnings := Warnings}) when not is_list(Warnings) ->
    exit({warnings_should_be_list, Warnings});
up_map(#{nodes := Nodes}) when not is_list(Nodes) ->
    exit({nodes_should_be_list, Nodes});
up_map(#{nodes := Nodes} = Map) ->
    case erl_abs_lib:validate_node(Nodes) of
        ok ->
            Map;
        {error, Reason} ->
            maps:remove(nodes, Map#{error => {Reason, Nodes}})
    end;
up_map(#{warning := Warning} = Map) ->
    Map1 = maps:remove(warning, Map),
    Warnings = maps:get(warnings, Map, []),
    up_map(Map1#{warnings => [Warning|Warnings]});
up_map(#{error := Error} = Map) ->
    Map1 = maps:remove(error, Map),
    Errors = maps:get(errors, Map, []),
    up_map(Map1#{errors => [Error|Errors]});
up_map(#{node := Node} = Map) ->
    Map1 = maps:remove(node, Map),
    Nodes = maps:get(nodes, Map, []),
    up_map(Map1#{nodes => [Node|Nodes]});
up_map(#{} = Map) ->
    Map.
