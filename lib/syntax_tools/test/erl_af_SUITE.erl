%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_af_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include("af_struct_name.hrl").
-include("merl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config1 = erl_af_test_lib:load_data_modules(Config, [sample_transformer_1]),
    Forms = erl_af_test_lib:test_module_forms(sample_1, Config1),
    [{forms, Forms}|Config].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [test_simple_map, test_reduce, test_map_with_state_node, test_map_with_state, test_map_spec, test_map_type,
     test_reduce_attr, test_with_formatter, 
     test_options, test_validator, test_with_attribute, test_forms_with_attribute,
     test_traverse_m_updated, test_map_forms, test_sequence_nodes, test_deep_sequence_children,
     test_continue_sequence_children].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_simple_map(_Config) ->
    Node0 = {atom, 1, ok},
    Monad =
        erl_af:map_m(
          fun(_Node, #{}) ->
                  erl_af_traverse_m:return(ok)
          end, Node0, #{traverse => pre}),
    erl_af_traverse_m:bind(
      erl_af_traverse_m:listen_updated(Monad),
      fun(Updated) ->
              ?assertEqual(false, Updated)
      end),
    Return = erl_af_return:simplify(erl_af_traverse_m:eval(Monad, erl_af, ok)),
    ?assertEqual(Node0, Return),
    ok.

test_reduce(Config) ->
    Forms = proplists:get_value(forms, Config),
    Baseline = erl_af_test_lib:get_baseline(function_base, Forms),
    File = erl_af_lib:analyze_forms_file(Forms),
    Return =
        erl_af:reduce(
          fun({atom, _Line, mark_1} = Node, Acc, #{}) ->
                  erl_af_walk_return:new(#{warning => mark_1, state => Acc + 1, node => Node});
             ({atom, _Line, mark_error_1}, _Acc, #{}) ->
                  {error, mark_error_1};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE, traverse => pre, simplify_return => false}),
    ErrorStruct = erl_af_return:run_error(Return),
    ?assertEqual(#{}, maps:without([file_errors, file_warnings], erl_af_error:printable(ErrorStruct))),
    FileWarnings = [{File, [{5, ?MODULE, mark_1}]}],
    FileErrors = [{File, [{2, ?MODULE, mark_error_1}]}],
    ?assertMatch({FileErrors, FileWarnings},
                 erl_af_test_lib:realize_with_baseline(Baseline, ErrorStruct)),
    ?assertEqual({just, 1}, erl_af_return:run(Return)),
    ok.

test_map_with_state_node(_Config) ->
    NodeA = {match, 10, {var, 10, 'A'}, {atom, 10, a}},
    Return =
        erl_af:map_with_state(
          fun({var, Line, 'A'}, Acc, #{}) ->
                  Node1 = {var, Line, 'B'},
                  erl_af_walk_return:new(#{state => Acc + 1, node => Node1});
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, NodeA, #{formatter => ?MODULE, traverse => pre, simplify_return => true}),
    ?assertEqual({match, 10, {var, 10, 'B'}, {atom, 10, a}}, Return),
    ok.

test_map_with_state(Config) ->
    Forms = proplists:get_value(forms, Config),
    Baseline = erl_af_test_lib:get_baseline(function_base, Forms),
    File = erl_af_lib:analyze_forms_file(Forms),
    ReturnM =
        erl_af:map_with_state(
          fun({atom, _Line, mark_1} = Node, Acc, #{}) ->
                  erl_af_walk_return:new(#{warning => mark_1, state => Acc + 1, node => Node});
             ({atom, _Line, mark_error_1}, Acc, #{}) ->
                  {{atom, _Line, mark_error_2}, Acc};
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, Forms, #{formatter => ?MODULE, traverse => pre, simplify_return => false}),
    FileWarnings = [{File, [{5, ?MODULE, mark_1}]}],
    #{'__struct__' := ?RETURN_OK, error := Error, return := _Return} = ReturnM,
    ?assertMatch({[], FileWarnings}, erl_af_test_lib:realize_with_baseline(Baseline, Error)),
    ok.

test_map_spec(_Config) ->
    Nodes = {attribute,56,spec,
             {{test_ok,0},[{type,56,'fun',[{type,56,product,[]},{atom,56,ok}]}]}},
    Nodes1 =
        erl_af:map(
          fun(Node, #{}) ->
                  Node
          end, Nodes, #{traverse => post, simplify_return => true}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_map_type(_Config) ->
    Nodes = {attribute,21,type,{test,{type,21,record,[{atom,21,test}]},[{var, 21, 'A'}]}},
    Nodes1 =
        erl_af:map(
          fun(Node, #{}) ->
                  Node
          end, Nodes, #{traverse => post, simplify_return => true}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_reduce_attr(Config) ->
    Forms = proplists:get_value(forms, Config),
    Baseline = erl_af_test_lib:get_baseline(mark_base, Forms),
    File = erl_af_lib:analyze_forms_file(Forms),
    ReturnM =
        erl_af:reduce(
          fun({attribute, _Line, mark, mark_0} = Node, Acc, #{}) ->
                  erl_af_walk_return:new(#{warning => mark_0, state => Acc + 1, node => Node});
             ({attribute, _Line, mark, mark_error_0}, _Acc, #{}) ->
                  {error, mark_error_0};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE, traverse => form, simplify_return => false}),
    #{'__struct__' := ?RETURN_OK, error := Error} = ReturnM,
    io:format("errors is ~p~n", [erl_af_error:printable(Error)]),
    FileWarnings = [{File, [{2, ?MODULE, mark_0}]}],
    FileErrors = [{File, [{1, ?MODULE, mark_error_0}]}],
    ?assertMatch({FileErrors, FileWarnings}, erl_af_test_lib:realize_with_baseline(Baseline, Error)),
    ok.

test_with_formatter(_Config) ->
    MA =
        erl_af_traverse_m:with_formatter(
          formatter_1,
          erl_af_traverse_m:update_line(
            10,
            erl_af_traverse_m:erl_af_traverse_m(
              erl_af_walk_return:new(#{return => 10, error => error_0})
             ))),
    #{error := Error} = erl_af_traverse_m:run(MA, formatter_0, ok),
    ?assertMatch([{10, formatter_1, error_0}], erl_af_error:formatted_errors(Error)),
    ok.

test_options(_Config) ->
    Return = #{a => true, e => true},
    Warnings = [{invalid_option_value, {b, c, d}}],
    OptionsMap = erl_af_lib:option_map([a, {b, c, d}, e]),
    ?assertMatch({just, Return}, erl_af_return:run(OptionsMap)),
    ?assertMatch(Warnings, erl_af_error:warnings(erl_af_return:run_error(OptionsMap))),
    ?assertMatch([], erl_af_error:errors(erl_af_return:run_error(OptionsMap))),
    ok.

test_validator(_Config) ->
    Validator = #{a => boolean,
                  b => {list_of, atom},
                  c => fun is_boolean/1,
                  d => {default, 10},
                  f => [boolean, {default_key, g}],
                  g => [boolean, {default, false}]
                 },
    Validated = erl_af_lib:validate(Validator, [a, {b,[c,d]}, {b, c, d}, e]),
    Return = #{a => true, b => [c, d], d => 10, f => false, g => false},
    Warnings = [{invalid_option_value, {b, c, d}}, {unexpected_option_keys, [e]}],
    ?assertMatch(Warnings, erl_af_error:warnings(erl_af_return:run_error(Validated))),
    ?assertMatch([], erl_af_error:errors(erl_af_return:run_error(Validated))),
    ?assertMatch({just, Return}, erl_af_return:run(Validated)),
    ok.


test_with_attribute(Config) ->
    Forms = proplists:get_value(forms, Config),
    Marks =
        erl_af_lib:with_attribute(
          fun(Attr, Acc) ->
                  [Attr|Acc]
          end, [], Forms, mark, #{simplify_return => true}),
    ?assertEqual([mark_0, mark_error_0], Marks).

test_forms_with_attribute(Config) ->
    Forms = proplists:get_value(forms, Config),
    {Forms1, Marks} =
        erl_af_lib:forms_with_attribute(
          fun(Attr, Acc, #{line := Line}) ->
                  Node = erl_af_lib:gen_attribute_node(mark_1, Line, Attr),
                  {[Node], [Attr|Acc]}
          end, [], Forms, mark, #{simplify_return => true}),
    Marks1 =
        erl_af_lib:with_attribute(
          fun(Attr, Acc) ->
                  [Attr|Acc]
          end, [], Forms1, mark_1, #{simplify_return => true}),
    ?assertEqual([mark_0, mark_error_0], Marks1),
    ?assertEqual([mark_0, mark_error_0], Marks),
    ok.

test_traverse_m_updated(Config) ->
    Forms = proplists:get_value(forms, Config),
    TraverseM =
        erl_af:map_m(
          fun(_Node, #{}) ->
                  erl_af_traverse_m:return(ok)
          end, Forms, #{traverse => post}),
    TraverseM1 =
        erl_af_monad:lift_m(
          fun({Return, Updated}) ->
                  ?assertEqual({Return, false}, {Forms, Updated}),
                  Return
          end, erl_af_traverse_m:listen_updated(TraverseM)),
    erl_af_traverse_m:eval(TraverseM1, erl_af, #{}).

test_map_forms(Config) ->
    Forms = erl_af_test_lib:test_module_forms(sample_2, Config),
    Forms1M = 
        erl_af:map_m(
          fun({attribute, _Line, mark, mark_01}) ->
                  erl_af_traverse_m:nodes(
                    erl_af_lib:gen_function(
                      test,
                      ?Q(["fun(ok_1) ->",
                          "   ok_1;"
                          "(Other) ->",
                          "   '__original__'(Other)",
                          "end"])));
             (_Node) ->
                  erl_af_traverse_m:return(ok)
          end, Forms, #{traverse => form}),
    Forms1 = erl_af_return:simplify(erl_af_traverse_m:eval(Forms1M, erl_af, ok)),
    Result = erl_af_test_lib:compile_test_forms(Forms1),
    erl_af_return:with_error(
      fun(ErrorState) ->
              ?assertEqual(#{}, erl_af_error:printable(ErrorState))
      end, Result),
    Value1 = sample_2:test(ok_1),
    Value2 = sample_2:test(ok_2),
    Value3 = sample_2:test(ok_3),
    Value4 = sample_2:test(ok_4),
    ?assertEqual({ok_1, ok_2, ok_3, ok_4}, {Value1, Value2, Value3, Value4}),
    ok.

test_sequence_nodes(_Config) ->
    Nodes = [{atom, 1, a}, {atom, 1, b}, {atom, 1, c}, {atom, 1, d}],
    NodeMs = lists:map(
               fun({_Type, _Line, a} = Node) ->
                       erl_af_traverse_m:nodes(Node);
                  ({_Type, _Line, b} = Node) ->
                       erl_af_traverse_m:nodes([Node, Node]);
                  ({_Type, _Line, c}) ->
                         erl_af_traverse_m:fail({invalid, c});
                  ({_Type, _Line, d} = Node) ->
                       erl_af_traverse_m:then(
                         erl_af_traverse_m:warning({suspecious, d}),
                         erl_af_traverse_m:return(Node))
               end, Nodes),
    NodesM = erl_af_traverse_m:sequence_nodes(NodeMs),
    Return = erl_af_traverse_m:eval(NodesM, erl_af, ok),
    erl_af_return:with_error(
      fun(ErrorStruct) ->
              ?assertEqual(#{errors => [{invalid, c}], warnings => [{suspecious, d}]}, erl_af_error:printable(ErrorStruct))
      end, Return),
    ?assertEqual({just, [{atom, 1, a}, {atom, 1, b}, {atom, 1, b}, {atom, 1, d}]}, erl_af_return:run(Return)),
    ok.

test_deep_sequence_children(_Config) ->
    Tuple1 = {tuple, 1, [{atom, 1, a}, {atom, 1, b}, {atom, 1, d}]},
    Return1 = deep_sequence_children(Tuple1),
    erl_af_return:with_error(
      fun(ErrorStruct) ->
              ?assertEqual(#{warnings => [{suspecious, d}]}, erl_af_error:printable(ErrorStruct))
      end, Return1),
    ?assertEqual({just, {tuple, 1, [{atom, 1, a}, {atom, 1, b}, {atom, 1, b}, {atom, 1, d}]}}, erl_af_return:run(Return1)),
    Tuple2 = {tuple, 1, [{atom, 1, a}, {atom, 1, b}, {atom, 1, c}, {atom, 1, d}]},
    Return2 = deep_sequence_children(Tuple2),
    erl_af_return:with_error(
      fun(ErrorStruct) ->
              ?assertEqual(#{errors => [{invalid, c}], warnings => [{suspecious, d}]}, erl_af_error:printable(ErrorStruct))
      end, Return2),
    ?assertEqual(nothing, erl_af_return:run(Return2)),
    ok.

deep_sequence_children(TopNode) ->
    Subtrees = erl_syntax:subtrees(TopNode),
    F =
        fun({_Type, _Line, a} = Node) ->
                erl_af_traverse_m:nodes(Node);
           ({_Type, _Line, b} = Node) ->
                erl_af_traverse_m:nodes([Node, Node]);
           ({_Type, _Line, c}) ->
                erl_af_traverse_m:fail({invalid, c});
           ({_Type, _Line, d} = Node) ->
                erl_af_traverse_m:then(
                  erl_af_traverse_m:warning({suspecious, d}),
                  erl_af_traverse_m:return(Node))
        end,
    SubtreesM = lists:map(fun(Subtree) -> lists:map(F, Subtree) end, Subtrees),
    TopNodeM1 =
        erl_af_traverse_m:bind(
          erl_af_traverse_m:listen_updated(erl_af_traverse_m:deep_sequence_nodes(SubtreesM)),
          fun({Subtrees1, true}) ->
                  TopNode1 = erl_af_syntax:update_subtrees(TopNode, Subtrees1),
                  erl_af_traverse_m:return(TopNode1);
             ({_, false}) ->
                  erl_af_traverse_m:return(TopNode)
          end),
    erl_af_traverse_m:eval(TopNodeM1, erl_af, ok).

test_continue_sequence_children(_Config) ->
    TopNode = {tuple, 1, [{match, 1, {var, 1, 'Var'}, {tuple, 1, [{atom, 1, a}, {atom, 1, b}]}}, {atom, 1, c}]},
    Monad =
        erl_af:map_m(
          fun({match, _Line, _Left, _Right}, _Attr) ->
                  Sequence = fun erl_af_traverse_m:deep_r_sequence_nodes/1,
                  erl_af_traverse_m:erl_af_traverse_m(erl_af_walk_return:new(#{continue => Sequence}));
             ({atom, _Line, Atom}, _Attr) ->
                  erl_af_traverse_m:modify(
                    fun(Acc) ->
                            [Atom|Acc]
                    end);
             ({var, _Line, Var},_Attr) ->
                  erl_af_traverse_m:modify(
                    fun(Acc) ->
                            [Var|Acc]
                    end);
             (_Node, _Attr) ->
                  erl_af_traverse_m:return(ok)
          end, TopNode, #{traverse => pre}),
    Monad1 =
        erl_af:map_m(
          fun({atom, _Line, Atom}, _Attr) ->
                  erl_af_traverse_m:modify(
                    fun(Acc) ->
                            [Atom|Acc]
                    end);
             ({var, _Line, Var},_Attr) ->
                  erl_af_traverse_m:modify(
                    fun(Acc) ->
                            [Var|Acc]
                    end);
             (_Node, _Attr) ->
                  erl_af_traverse_m:return(ok)
          end, TopNode, #{traverse => pre}),
    Return = erl_af_traverse_m:exec(Monad, erl_af, []),
    Return1 = erl_af_traverse_m:exec(Monad1, erl_af, []),
    ?assertEqual({just, [c, 'Var', b, a]}, erl_af_return:run(Return)),
    ?assertEqual({just, [c, b, a, 'Var']}, erl_af_return:run(Return1)),
    ok.
