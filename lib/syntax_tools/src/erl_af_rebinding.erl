%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_af_rebinding).

-include("af_struct_name.hrl").
-include("do.hrl").


%% API
-export([parse_transform/2, format_error/1]).

-record(rebinding_options, {global_options, fun_options}).
%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    Return =
        do([erl_af_return ||
               RebindingOptions <- load_attributes(Forms),
               erl_af_traverse_m:eval(
                 erl_af:map_m(
                   fun(Form) ->
                           walk_form(Form, RebindingOptions)
                   end, Forms, #{traverse => form}), ?MODULE, #{})
           ]),
    erl_af_return:to_compiler(Return).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% load options
%%%===================================================================
load_attributes(Forms) ->
    do([erl_af_return ||
           GlobalOptions <-
               erl_af_lib:validate_attribute_option(rebinding_validator(), ?MODULE, rebinding_all, Forms),
           FunOptions <-
               erl_af_lib:with_attribute(
                 fun(Attr, Acc) ->
                         add_fun_options(Attr, Acc)
                 end, #{}, Forms, rebinding_fun,
                 #{formatter => ?MODULE, simplify_return => false}),
           return(#rebinding_options{fun_options = FunOptions, global_options = GlobalOptions})
       ]).

add_fun_options(FName, Acc) when is_atom(FName) ->
    add_fun_options(FName, #{}, Acc);
add_fun_options({FName, Arity}, Acc) when is_atom(FName), is_integer(Arity) ->
    add_fun_options({FName, Arity}, #{}, Acc);
add_fun_options({Function, Options}, Acc) ->
    do([ erl_af_return ||
           Options1 <- erl_af_lib:validate(rebinding_validator(), Options),
           add_fun_options(Function, Options1, Acc)
       ]);
add_fun_options(Function, Acc) ->
    add_fun_options(Function, #{}, Acc).

add_fun_options(Functions, Options, Acc) when is_list(Functions) ->
    erl_af_monad:foldl_m(
      fun(Function, Acc1) ->
              add_fun_options(Function, Options, Acc1)
      end, Acc, Functions, erl_af_return);
add_fun_options({FName, Arity}, Options, Acc) when is_atom(FName), is_integer(Arity) ->
    merge_fun_options({FName, Arity}, Options, Acc);
add_fun_options(FName, Options, Acc) when is_atom(FName) ->
    merge_fun_options(FName, Options, Acc);
add_fun_options(Other, _Options, Acc) ->
    erl_af_return:warning_ok({invalid_rebinding_fun, Other}, Acc).

merge_fun_options(Function, Options, Acc) ->
    FAcc = maps:get(Function, Acc, #{}),
    erl_af_return:return(maps:put(Function, maps:merge(FAcc, Options), Acc)).

rebinding_validator() ->
    #{clause_pinned => boolean, strict => boolean, debug => boolean, non_rebinding => boolean}.

rebinding_keys() ->
    [clause_pinned, strict].
%%%===================================================================
%%% walk form
%%%===================================================================
walk_form({function, Line, Name, Arity, Clauses}, RebindingOptionsRec) ->
    case match_rebinding(Name, Arity, RebindingOptionsRec) of
        {ok, RebindingOptions} ->
            ClausesM =
                erl_af_traverse_m:map_m(
                  fun(Clause) ->
                          walk_function_clause(Clause, RebindingOptions)
                  end, Clauses),
            erl_af_traverse_m:lift_m(
              fun(Clauses1) ->
                      Function1 = {function, Line, Name, Arity, Clauses1},
                      case maps:get(debug, RebindingOptions, false) of
                          true ->
                              io:format("~s~n", [erl_af_lib:ast_safe_to_string(Function1)]);
                          false ->
                              ok
                      end,
                      Function1
              end, ClausesM);
        error ->
            erl_af_traverse_m:continue()
    end;
walk_form(_Form, _RebindingOptionsRec) ->
    erl_af_traverse_m:continue().

match_rebinding(Name, Arity, RebindingOptionsRec) ->
    RebindingOptions = find_rebinding_options(Name, Arity, RebindingOptionsRec),
    case maps:get(non_rebinding, RebindingOptions, false) of
        true ->
            error;
        false ->
            {ok, RebindingOptions}
    end.

find_rebinding_options(Name, Arity, #rebinding_options{fun_options = FunOptions,
                                                       global_options = AllOptions}) ->
    case maps:find(Name, FunOptions) of
        {ok, Options} ->
            Options;
        error ->
            case maps:find({Name, Arity}, FunOptions) of
                {ok, Options} ->
                    Options;
                error ->
                    AllOptions
            end
    end.

walk_function_clause(Clause, RebindingOptions) ->
    Opts = #{traverse => pre, simplify_return => false, attr => #{node => form, parent => fun_expr}},
    Context0 = new_context(),
    erl_af_traverse_m:set_updated(
    erl_af_traverse_m:erl_af_traverse_m(
        erl_af:map_with_state(
          fun(Node, Acc, Attr) ->
                  Attr1 = maps:merge(Attr, maps:with(rebinding_keys(), RebindingOptions)),
                  NodeType = erl_syntax:type(Node),
                  walk_node(NodeType, Node, Acc, Attr1)
          end, Context0, Clause, Opts))).

walk_node({op, _Line1, '+', {var, _Line3, _Varname} = Var},
          #{pattern := PatternType} = Context, #{node := pattern, step := pre})
  when PatternType == match_left; PatternType == clause_match ->
    Var1 = rename_pinned_var(Var, Context),
    erl_af_walk_return:new(#{node => Var1, state => Context, continue => true});

walk_node({op, _Line, _Op, _Left, _Right}, #{},
            #{step := pre, node := expression, strict := true}) ->
    Sequence =
        fun([LeftMs, OpM, RightMs]) ->
                LeftMs1 = lists:map(fun with_funcall_argument/1, LeftMs),
                RightMs1 = lists:map(fun with_funcall_argument/1, RightMs),
                with_scope_group(erl_af_traverse_m:deep_sequence_nodes([LeftMs1, OpM, RightMs1]))
        end,
    erl_af_walk_return:new(#{continue => Sequence});

%% walk function call
walk_node({call, _Line, _Function, _Args}, #{},
            #{step := pre, node := expression, strict := true}) ->
    Sequence =
        fun([FunctionM, FunctionArgMs]) ->
                FunctionArgMs1 = lists:map(fun with_funcall_argument/1, FunctionArgMs),
                with_scope_group(erl_af_traverse_m:deep_sequence_nodes([FunctionM, FunctionArgMs1]))
        end,
    erl_af_walk_return:new(#{continue => Sequence});

walk_node({tuple, _Line, _TupleElements}, #{} = Context,
            #{step := pre, node := expression, strict := true}) ->
    Sequence =
        fun([TupleElementMs]) ->
                TupleElementMs1 = lists:map(fun with_funcall_argument/1, TupleElementMs),
                with_scope_group(erl_af_traverse_m:deep_sequence_nodes([TupleElementMs1]))
        end,
    erl_af_walk_return:new(#{continue => Sequence, state => Context});

walk_node({cons, _Line, _Head, _Tail}, #{},
            #{step := pre, node := expression, strict := true}) ->
    Sequence =
        fun([HeadMs]) ->
                HeadMs1 = lists:map(fun with_funcall_argument/1, HeadMs),
                with_scope_group(erl_af_traverse_m:deep_sequence_nodes([HeadMs1]));
           ([HeadMs, TailMs]) ->
                HeadMs1 = lists:map(fun with_funcall_argument/1, HeadMs),
                TailMs1 = lists:map(fun with_funcall_argument/1, TailMs),
                with_scope_group(erl_af_traverse_m:deep_sequence_nodes([HeadMs1, TailMs1]))
        end,
    erl_af_walk_return:new(#{continue => Sequence});

walk_node({map, _Line, _MapAssociations}, #{},
            #{step := pre, node := expression, strict := true}) ->
    Sequence =
        fun([MapAssociationMs]) ->
                MapAssociationMs1 = lists:map(fun with_funcall_argument/1, MapAssociationMs),
                with_scope_group(erl_af_traverse_m:deep_sequence_nodes([MapAssociationMs1]))
        end,
    erl_af_walk_return:new(#{continue => Sequence});

walk_node({map, _Line, _Map, _MapAssociations}, #{},
            #{step := pre, node := expression, strict := true}) ->
    Sequence =
        fun([MapMs, MapAssociationMs]) ->
                MapMs1 = lists:map(fun with_funcall_argument/1, MapMs),
                MapAssociationMs1 = lists:map(fun with_funcall_argument/1, MapAssociationMs),
                with_scope_group(erl_af_traverse_m:deep_sequence_nodes([MapMs1, MapAssociationMs1]))
        end,
    erl_af_walk_return:new(#{continue => Sequence});

walk_node({record, _Line, _Name, _RecFields}, #{} = Context,
            #{step := pre, node := expression, strict := true}) ->
    Sequence =
        fun([NameMs, RecFieldMs]) ->
                RecFieldMs1 = lists:map(fun with_funcall_argument/1, RecFieldMs),
                with_scope_group(erl_af_traverse_m:deep_sequence_nodes([NameMs, RecFieldMs1]))
        end,
    erl_af_walk_return:new(#{continue => Sequence, state => Context});

walk_node({record, _Line, _Rec, _Name, _RecFields}, #{} = Context,
            #{step := pre, node := expression, strict := true}) ->
    Sequence =
        fun([RecMs, NameMs, RecFieldMs]) ->
                RecMs1 = lists:map(fun with_funcall_argument/1, RecMs),
                RecFieldMs1 = lists:map(fun with_funcall_argument/1, RecFieldMs),
                with_scope_group(erl_af_traverse_m:deep_sequence_nodes([RecMs1, NameMs, RecFieldMs1]))
        end,
    erl_af_walk_return:new(#{continue => Sequence, state => Context});

%% walk comprehension
walk_node({ComprehensionType, _Line, _Expression, _Qualifiers}, #{} = Context, #{step := pre})
  when (ComprehensionType == lc) or (ComprehensionType == bc) ->
    walk_comprehension(Context);

%% walk comprehension generate
walk_node({GenerateType, _Line, _Pattern, _Expression}, #{} = Context, #{step := pre})
    when (GenerateType == generate) or (GenerateType == b_generate) ->
    walk_generate(Context);

%% walk match
walk_node({match, _Line, _Patterns, _Expressions}, #{} = Context, #{step := pre, node := expression}) ->
    walk_match(Context);

%% walk function clause and other clauses
walk_node({clause, _Line, _Patterns, _Match, _Body}, #{} = Context, #{step := pre} = Attr) ->
    walk_clause(Context, Attr);

%% walk named fun
walk_node({named_fun, _Line, _Name, _Clauses},  #{} = Context, #{step := pre}) ->
    walk_named_fun(Context);

%% do nothing to _
walk_node({var, _Line, '_'}, #{} = Context, #{}) ->
    erl_af_walk_return:new(#{state => Context});

%% rename var if current node is expression.
walk_node({var, _Line, _Varname} = Var, #{} = Context, #{node := expression}) ->
    Var1 = rename_normal_var(Var, Context),
    {Var1, Context};

%% rename var if current node is guard.
walk_node({var, _Line, _Varname} = Var, #{} = Context, #{node := guard}) ->
    Var1 = rename_normal_var(Var, Context),
    {Var1, Context};

%% rename var if current node is clause match pattern.
walk_node({var, _Line, _Varname} = Var, #{pattern := clause_match} = Context, #{clause_pinned := true}) ->
    Var1 = rename_clause_match_var(Var, Context),
    {Var1, Context};

%% rename var if current node is clause match pattern.
walk_node({var, _Line, _Varname} = Var, #{pattern := clause_match} = Context, #{}) ->
    rebind_clause_match_var(Var, Context);

%% rebind var if current node is function pattern.
walk_node({var, _Line, _Varname} = Var, #{pattern := function_clause} = Context, #{node := pattern}) ->
    rebind_function_clause_var(Var, Context);

%% rebind var if current node is match pattern.
walk_node({var, _Line, _Varname} = Var, #{pattern := match_left} = Context, #{node := pattern}) ->
    rebind_match_left_var(Var, Context);

%% rebind var if current node is comprehension_generate pattern.
walk_node({var, _Line, _Varname} = Var,
          #{pattern := comprehension_generate} = Context, #{node := pattern}) ->
    rebind_comprehension_generate_var(Var, Context);

walk_node( Node, #{} = Context, #{step := pre, node := expression} = Attr) ->
    NodeType = erl_syntax:type(Node),
    case is_scope_group(NodeType, Attr) of
        true ->
            Sequence = 
                fun(SubtreeMs) ->
                        with_scope_group(erl_af_traverse_m:deep_sequence_nodes(SubtreeMs))
                end,
            erl_af_walk_return:new(#{continue => Sequence});
        false ->
            erl_af_walk_return:new(#{state => Context})
    end;
walk_node(_Node, Context, #{}) ->
    erl_af_walk_return:new(#{state => Context}).

%% case expression
is_scope_group(case_expr, #{}) ->
    true;
%% if expression
is_scope_group(if_expr, #{}) ->
    true;
%% catch expression
is_scope_group(catch_expr, #{}) ->
    true;
%% receive expression
is_scope_group(receive_expr, #{}) ->
    true;
%% try expression
is_scope_group(try_expr, #{}) ->
    true;
is_scope_group(_Type, #{}) ->
    false.

clause_scope_type(fun_expr) ->
    shadowed;
clause_scope_type(named_fun_expr) ->
    shadowed;
clause_scope_type(_Other) ->
    nonfun_clause.

walk_comprehension(Context) ->
    Sequence =
        fun(SubtreeMs) ->
                with_shadowed(erl_af_traverse_m:deep_r_sequence_nodes(SubtreeMs))
        end,
    erl_af_walk_return:new(#{continue => Sequence, state => Context}).

walk_generate(Context) ->
    Sequence =
        fun([PatternMs, ExpressionMs]) ->
                PatternsM1 = with_comprehension_generate_pattern(PatternMs),
                ExpressionsM1 = with_shadowed(ExpressionMs),
                %% walk expression first
                erl_af_traverse_m:deep_r_sequence_nodes([PatternsM1, ExpressionsM1])
        end,
    erl_af_walk_return:new(#{continue => Sequence, state => Context}).

walk_match(Context) ->
    Sequence =
        fun([PatternMs, ExpressionM]) ->
                PatternsM = erl_af_monad:sequence_m(PatternMs, erl_af_traverse_m),
                PatternsM1 = with_match_left_pattern(PatternsM),
                %% walk expression first
                erl_af_traverse_m:deep_r_sequence_nodes([PatternsM1, ExpressionM])
        end,
    erl_af_walk_return:new(#{continue => Sequence, state => Context}).

walk_clause(Context, #{parent := Parent}) ->
    ScopeType = clause_scope_type(Parent),
    PatternType = scope_type_pattern(ScopeType),
    Sequence =
        fun([PatternMs|RestTreesM]) ->
                PatternsM1 = with_pattern(PatternType, PatternMs),
                with_scope_type(
                  ScopeType, erl_af_traverse_m:deep_sequence_nodes([PatternsM1|RestTreesM]))
        end,
    erl_af_walk_return:new(#{continue => Sequence, state => Context}).

walk_named_fun(Context) ->
    Sequence =
        fun([NameTreeMs|RestTreeMs]) ->
                NameTreesM1 = with_function_clause_pattern(NameTreeMs),
                with_shadowed(erl_af_traverse_m:deep_sequence_nodes([NameTreesM1|RestTreeMs]))
        end,
    erl_af_walk_return:new(#{continue => Sequence, state => Context}).

rename_pinned_var(Var, Context) ->
    rename_var(Var, Context).

rename_normal_var(Var, Context) ->
    rename_var(Var, Context).

rename_clause_match_var(Var, Context) ->
    rename_var(Var, Context).

rebind_clause_match_var(Var, Context) ->
    rebind_var(Var, Context).

rebind_function_clause_var(Var, Context) ->
    rebind_var(Var, Context).

rebind_match_left_var(Var, Context) ->
    rebind_var(Var, Context).

rebind_comprehension_generate_var(Var, Context) ->
    rebind_var(Var, Context).

new_context() ->
    #{local_varnames   => ordsets:new(),
      local_renames     => maps:new(),
      global_varnames   => ordsets:new(),
      global_renames    => maps:new(),
      pattern_varnames  => ordsets:new(),
      varnames_stack    => [],
      renames_stack     => [],
      scope_group_stack => []}.

with_scope_group(NodeM) ->
    with_scope_type(scope_group, NodeM).

%% with_nonfun_clause(NodeM) ->
%%     with_scope_type(nonfun_clause, NodeM).

with_funcall_argument(NodeM) ->
    with_scope_type(funcall_argument, NodeM).

with_shadowed(NodeM) ->
    with_scope_type(shadowed, NodeM).

with_match_left_pattern(NodeM) ->
    with_pattern(match_left, NodeM).

with_function_clause_pattern(NodeM) ->
    with_pattern(function_clause, NodeM).

with_comprehension_generate_pattern(NodeM) ->
    with_pattern(comprehension_generate, NodeM).

%% with_clause_match_pattern(NodeM) ->
%%     with_pattern(clause_match, NodeM).

with_pattern(PatternType, NodeM) ->
    ScopeType = pattern_to_scope_type(PatternType),
    with_scope_type(ScopeType, NodeM).

with_scope_type(ScopeType, NodeMs) when is_list(NodeMs) ->
    NodesM = erl_af_monad:sequence_m(NodeMs, erl_af_traverse_m),
    with_scope_type(ScopeType, NodesM);
with_scope_type(ScopeType, NodeM) ->
    do([ erl_af_traverse_m ||
           erl_af_traverse_m:modify(fun(Context) -> entry_scope_type(ScopeType, Context) end),
           Node <- NodeM,
           erl_af_traverse_m:modify(fun(Context) -> exit_scope_type(ScopeType, Context) end),
           erl_af_traverse_m:return(Node)
       ]).

rebind_var({var, Line, Varname} = Var,
           #{global_varnames   := GlobalVarnames,
             local_varnames    := LocalVarnames,
             global_renames    := GlobalRenameMap,
             local_renames     := LocalRenameMap,
             pattern_varnames  := PatternVarnames
            } = Context) ->
    case ordsets:is_element(Varname, PatternVarnames) of
        true ->
            Var1 = rename_var(Var, Context),
            {Var1, Context};
        false ->
            PatternVarnames1 = ordsets:add_element(Varname, PatternVarnames),
            Context1 = Context#{pattern_varnames => PatternVarnames1},

            case ordsets:is_element(Varname, GlobalVarnames) of
                true ->
                    Varname1 = new_variable_name(Varname, GlobalVarnames),
                    Var1 = {var, Line, Varname1},
                    GlobalVarnames1 = ordsets:add_element(Varname1, GlobalVarnames),
                    LocalVarnames1 = ordsets:add_element(Varname1, LocalVarnames),
                    GlobalRenameMap1 = maps:put(Varname, Varname1, GlobalRenameMap),
                    LocalRenameMap1 = maps:put(Varname, Varname1, LocalRenameMap),

                    Context2 = Context1#{global_varnames  => GlobalVarnames1,
                                         local_varnames   => LocalVarnames1,
                                         global_renames   => GlobalRenameMap1,
                                         local_renames    => LocalRenameMap1,
                                         pattern_varnames => PatternVarnames1
                                        },
                    {Var1, Context2};
                false ->
                    GlobalVarnames1 = ordsets:add_element(Varname, GlobalVarnames),
                    LocalVarnames1 = ordsets:add_element(Varname, LocalVarnames),
                    Context2 = Context1#{global_varnames => GlobalVarnames1,
                                         local_varnames => LocalVarnames1
                                        },
                    {Var, Context2}
            end
    end.

rename_var({var, Line, Varname} = Var, #{global_renames := Renames}) ->
    case maps:find(Varname, Renames) of
        {ok, Varname1} ->
            {var, Line, Varname1};
        error ->
            Var
    end.

entry_scope_type(scope_group, Context) ->
    entry_scope_group(Context);
entry_scope_type(nonfun_clause, Context) ->
    entry_nonfun_clause(Context);
entry_scope_type(funcall_argument, Context) ->
    entry_funcall_argument(Context);
entry_scope_type(shadowed, Context) ->
    entry_shadowed(Context);
entry_scope_type(ScopeType, Context) ->
    PatternType = scope_to_pattern_type(ScopeType),
    entry_pattern(PatternType, Context).

exit_scope_type(scope_group, Context) ->
    exit_scope_group(Context);
exit_scope_type(nonfun_clause, Context) ->
    exit_nonfun_clause(Context);
exit_scope_type(funcall_argument, Context) ->
    exit_funcall_argument(Context);
exit_scope_type(shadowed, Context) ->
    exit_shadowed(Context);
exit_scope_type(ScopeType, Context) ->
    PatternType = scope_to_pattern_type(ScopeType),
    exit_pattern(PatternType, Context).

entry_scope_group(#{scope_group_stack := ScopeStack} = Context) ->
    ScokeStack1 = [{ordsets:new(), maps:new()}|ScopeStack],
    Context#{scope_group_stack => ScokeStack1}.

exit_scope_group(#{local_varnames    := LocalVarnames,
                   local_renames     := LocalRenames,
                   global_varnames   := GlobalVarnames,
                   global_renames    := GlobalRenames,
                   scope_group_stack := [{ScopeVarnames, ScopeRenames}|ScopeStack]} = Context) ->
    LocalVarnames1 = ordsets:union(LocalVarnames, ScopeVarnames),
    LocalRenames1 = maps:merge(LocalRenames, ScopeRenames),
    GlobalVarnames1 = ordsets:union(GlobalVarnames, ScopeVarnames),
    GlobalRenames1 = maps:merge(GlobalRenames, ScopeRenames),
    Context#{local_varnames    => LocalVarnames1,
             local_renames     => LocalRenames1,
             global_varnames   => GlobalVarnames1,
             global_renames    => GlobalRenames1,
             scope_group_stack => ScopeStack}.

entry_nonfun_clause(Context) ->
    Context1 = push_varname_stack(Context),
    push_rename_stack(Context1).

exit_nonfun_clause(#{local_varnames    := LocalVarnames,
                     scope_group_stack := [{ScopeVarnames, ScopeRenames}|ScopeGroupStack]} = Context) ->
    Context1 = pop_varname_stack(Context),
    Context2 = pop_rename_stack(Context1),
    ScopeVarnames1 = ordsets:union(ScopeVarnames, LocalVarnames),
    ScopeGroupStack1 = [{ScopeVarnames1, ScopeRenames}|ScopeGroupStack],
    Context2#{scope_group_stack => ScopeGroupStack1}.

entry_funcall_argument(Context) ->
    push_rename_stack(Context).

exit_funcall_argument(#{local_varnames    := LocalVarnames,
                        local_renames     := LocalRenames,
                        scope_group_stack := [{ScopeVarnames, ScopeRenames}|ScopeGroupStack]} = Context) ->
    Context1 = pop_rename_stack(Context),
    ScopeVarnames1 = ordsets:union(ScopeVarnames, LocalVarnames),
    ScopeRenames1 = maps:merge(ScopeRenames, LocalRenames),
    ScopeGroupStack1 = [{ScopeVarnames1, ScopeRenames1}|ScopeGroupStack],
    Context1#{scope_group_stack => ScopeGroupStack1}.

entry_shadowed(Context) ->
    Context1 = push_varname_stack(Context),
    push_rename_stack(Context1).

exit_shadowed(Context) ->
    Context1 = pop_varname_stack(Context),
    pop_rename_stack(Context1).

entry_pattern(PatternType, #{} = Context) ->
    Context#{pattern => PatternType, pattern_varnames => ordsets:new()}.

exit_pattern(PatternType, #{pattern := PatternType} = Context) ->
    Context1 = maps:remove(pattern, Context),
    Context1#{pattern_varnames => ordsets:new()}.

new_variable_name(Variable, Variables) ->
    new_variable_name(Variable, Variables, 1).

new_variable_name(Variable, Variables, N) ->
    Variable1 = add_suffix(Variable, N),
    case ordsets:is_element(Variable1, Variables) of
        true ->
            new_variable_name(Variable, Variables, N + 1);
        false ->
            Variable1
    end.

add_suffix(Variable, N) ->
    list_to_atom(atom_to_list(Variable) ++ "_" ++ integer_to_list(N)).

push_varname_stack(#{local_varnames := LocalVarnames, 
                     varnames_stack := VarnamesStack} = Context) ->
    LocalVarnames1 = ordsets:new(),
    VarnamesStack1 = [LocalVarnames|VarnamesStack],
    Context#{local_varnames => LocalVarnames1, 
             varnames_stack => VarnamesStack1}.

pop_varname_stack(#{varnames_stack := [LocalVarnames|ParentVarnameStack] = VarnameStack} = Context) ->
    GlobalVarnames = ordsets:union(VarnameStack),
    Context#{local_varnames  => LocalVarnames,
             global_varnames => GlobalVarnames,
             varnames_stack  => ParentVarnameStack}.

push_rename_stack(#{local_renames := LocalRenames,
                    renames_stack := RenameStack
                   } = Context) ->
    LocalRenames1 = maps:new(),
    RenameStack1 = [LocalRenames|RenameStack],
    Context#{local_renames => LocalRenames1,
             renames_stack => RenameStack1}.

pop_rename_stack(#{renames_stack := [LocalRenames|ParentRenamesStack]} = Context) ->
    GlobalRenames =
        lists:foldl(
          fun(Renames, Acc) ->
                  maps:merge(Renames, Acc)
          end, LocalRenames, ParentRenamesStack),
    Context#{local_renames  => LocalRenames,
             global_renames => GlobalRenames,
             renames_stack  => ParentRenamesStack}.

scope_type_pattern(shadowed) ->
    function_clause;
scope_type_pattern(nonfun_clause) ->
    clause_match.

pattern_to_scope_type(function_clause) ->
    function_clause_pattern;
pattern_to_scope_type(clause_match) ->
    clause_match_pattern;
pattern_to_scope_type(comprehension_generate) ->
    comprehension_generate_pattern;
pattern_to_scope_type(match_left) ->
    match_left_pattern.

scope_to_pattern_type(function_clause_pattern) ->
    function_clause;
scope_to_pattern_type(clause_match_pattern) ->
    clause_match;
scope_to_pattern_type(comprehension_generate_pattern) ->
    comprehension_generate;
scope_to_pattern_type(match_left_pattern) ->
    match_left.
