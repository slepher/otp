%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_abs_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include("abs_struct_name.hrl").
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
    erlang:system_flag(backtrace_depth, 100),
    Forms = erl_abs_test_lib:test_module_forms(erl_abs_sample_1, Config),
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
    [test_reduce, test_map_with_state_node, test_map_with_state, test_map_spec, test_map_type,
     test_reduce_attr, test_with_formatter, 
     test_options, test_validator, test_with_attribute, test_forms_with_attribute,
     test_traverse_m_updated, test_map_forms].

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
test_reduce(Config) ->
    Forms = proplists:get_value(forms, Config),
    Baseline = erl_abs_test_lib:get_baseline(function_base, Forms),
    File = erl_abs_lib:analyze_forms_file(Forms),
    ReturnM =
        erl_abs:reduce(
          fun({atom, _Line, mark_1} = Node, Acc, #{}) ->
                  erl_abs_walk_return:new(#{warning => mark_1, state => Acc + 1, node => Node});
             ({atom, _Line, mark_error_1}, _Acc, #{}) ->
                  {error, mark_error_1};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE, traverse => pre, simplify_return => false}),
    #{'__struct__' := ?RETURN_FAIL, error := Error} = ReturnM,
    FileWarnings = [{File, [{5, ?MODULE, mark_1}]}],
    FileErrors = [{File, [{2, ?MODULE, mark_error_1}]}],
    ?assertMatch({FileErrors, FileWarnings},
                 erl_abs_test_lib:realize_with_baseline(Baseline, Error)),
    ok.

test_map_with_state_node(_Config) ->
    NodeA = {match, 10, {var, 10, 'A'}, {atom, 10, a}},
    Return =
        erl_abs:map_with_state(
          fun({var, Line, 'A'}, Acc, #{}) ->
                  Node1 = {var, Line, 'B'},
                  erl_abs_walk_return:new(#{state => Acc + 1, node => Node1});
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, NodeA, #{formatter => ?MODULE, traverse => pre, simplify_return => true}),
    ?assertEqual({match, 10, {var, 10, 'B'}, {atom, 10, a}}, Return),
    ok.

test_map_with_state(Config) ->
    Forms = proplists:get_value(forms, Config),
    Baseline = erl_abs_test_lib:get_baseline(function_base, Forms),
    File = erl_abs_lib:analyze_forms_file(Forms),
    ReturnM =
        erl_abs:map_with_state(
          fun({atom, _Line, mark_1} = Node, Acc, #{}) ->
                  erl_abs_walk_return:new(#{warning => mark_1, state => Acc + 1, node => Node});
             ({atom, _Line, mark_error_1}, Acc, #{}) ->
                  {{atom, _Line, mark_error_2}, Acc};
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, Forms, #{formatter => ?MODULE, traverse => pre, simplify_return => false}),
    FileWarnings = [{File, [{5, ?MODULE, mark_1}]}],
    #{'__struct__' := ?RETURN_OK, error := Error, return := _Return} = ReturnM,
    ?assertMatch({[], FileWarnings}, erl_abs_test_lib:realize_with_baseline(Baseline, Error)),
    ok.

test_map_spec(_Config) ->
    Nodes = {attribute,56,spec,
             {{test_ok,0},[{type,56,'fun',[{type,56,product,[]},{atom,56,ok}]}]}},
    Nodes1 =
        erl_abs:map(
          fun(Node, #{}) ->
                  Node
          end, Nodes, #{traverse => post, simplify_return => true}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_map_type(_Config) ->
    Nodes = {attribute,21,type,{test,{type,21,record,[{atom,21,test}]},[{var, 21, 'A'}]}},
    Nodes1 =
        erl_abs:map(
          fun(Node, #{}) ->
                  Node
          end, Nodes, #{traverse => post, simplify_return => true}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_reduce_attr(Config) ->
    Forms = proplists:get_value(forms, Config),
    Baseline = erl_abs_test_lib:get_baseline(mark_base, Forms),
    File = erl_abs_lib:analyze_forms_file(Forms),
    ReturnM =
        erl_abs:reduce(
          fun({attribute, _Line, mark, mark_0} = Node, Acc, #{}) ->
                  erl_abs_walk_return:new(#{warning => mark_0, state => Acc + 1, node => Node});
             ({attribute, _Line, mark, mark_error_0}, _Acc, #{}) ->
                  {error, mark_error_0};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE, traverse => list, simplify_return => false}),
    #{'__struct__' := ?RETURN_OK, error := Error} = ReturnM,
    FileWarnings = [{File, [{2, ?MODULE, mark_0}]}],
    FileErrors = [{File, [{1, ?MODULE, mark_error_0}]}],
    ?assertMatch({FileErrors, FileWarnings}, erl_abs_test_lib:realize_with_baseline(Baseline, Error)),
    ok.

test_with_formatter(_Config) ->
    MA =
        erl_abs_traverse_m:with_formatter(
          formatter_1,
          erl_abs_traverse_m:update_line(
            10,
            erl_abs_traverse_m:erl_abs_traverse_m(
              erl_abs_walk_return:new(#{return => 10, error => error_0})
             ))),
    #{error := Error} = erl_abs_traverse_m:run(MA, formatter_0, ok),
    ?assertMatch([{10, formatter_1, error_0}], erl_abs_error:formatted_errors(Error)),
    ok.

test_options(_Config) ->
    Return = #{a => true, e => true},
    Warnings = [{invalid_option_value, {b, c, d}}],
    OptionsMap = erl_abs_lib:option_map([a, {b, c, d}, e]),
    ?assertMatch({just, Return}, erl_abs_return:run(OptionsMap)),
    ?assertMatch(Warnings, erl_abs_error:warnings(erl_abs_return:run_error(OptionsMap))),
    ?assertMatch([], erl_abs_error:errors(erl_abs_return:run_error(OptionsMap))),
    ok.

test_validator(_Config) ->
    Validator = #{a => boolean,
                  b => {list_of, atom},
                  c => fun is_boolean/1,
                  d => {default, 10},
                  f => [boolean, {default_key, g}],
                  g => [boolean, {default, false}]
                  },
    Validated = erl_abs_lib:validate(Validator, [a, {b,[c,d]}, {b, c, d}, e]),
    Return = #{a => true, b => [c, d], d => 10, f => false, g => false},
    Warnings = [{invalid_option_value, {b, c, d}}, {unexpected_option_keys, [e]}],
    ?assertMatch(Warnings, erl_abs_error:warnings(erl_abs_return:run_error(Validated))),
    ?assertMatch([], erl_abs_error:errors(erl_abs_return:run_error(Validated))),
    ?assertMatch({just, Return}, erl_abs_return:run(Validated)),
    ok.


test_with_attribute(Config) ->
    Forms = proplists:get_value(forms, Config),
    Marks =
        erl_abs_lib:with_attribute(
          fun(Attr, Acc) ->
                  [Attr|Acc]
          end, [], Forms, mark, #{simplify_return => true}),
    ?assertEqual([mark_0, mark_error_0], Marks).

test_forms_with_attribute(Config) ->
    Forms = proplists:get_value(forms, Config),
    {Forms1, Marks} =
        erl_abs_lib:forms_with_attribute(
          fun(Attr, Acc, #{line := Line}) ->
                  Node = erl_abs_lib:gen_attribute_node(mark_1, Line, Attr),
                  {[Node], [Attr|Acc]}
          end, [], Forms, mark, #{simplify_return => true}),
    Marks1 =
        erl_abs_lib:with_attribute(
          fun(Attr, Acc) ->
                  [Attr|Acc]
          end, [], Forms1, mark_1, #{simplify_return => true}),
    ?assertEqual([mark_0, mark_error_0], Marks1),
    ?assertEqual([mark_0, mark_error_0], Marks),
    ok.

test_traverse_m_updated(Config) ->
    Forms = proplists:get_value(forms, Config),
    TraverseM =
        erl_abs:map_m(
            fun(_Node, #{}) ->
                erl_abs_traverse_m:return(ok)
            end, Forms, #{traverse => post}),
    TraverseM1 =
        erl_abs_monad:lift_m(
            fun({{Return, Nodes}, Updated}) ->
                ?assertEqual(false, Updated),
                ?assertEqual(Return, Nodes),
                Return
            end, erl_abs_traverse_m:listen_updated(erl_abs_traverse_m:listen_nodes(TraverseM))),
    erl_abs_traverse_m:eval(TraverseM1, erl_abs, #{}).

test_map_forms(Config) ->
    Forms = erl_abs_test_lib:test_module_forms(erl_abs_sample_2, Config),
    Forms1M = 
        erl_abs_lib:map_forms(
            fun({attribute, Line, mark, mark_01}) ->
                erl_abs_traverse_m:node(
                    erl_abs_lib:gen_function(test,
                        ?Q(["fun(ok_1) ->",
                            "   ok_1;"
                            "(Other) ->",
                            "   '__original__'(Other)",
                            "end"])));
                (_Node) ->
                    erl_abs_traverse_m:return(ok)
            end, Forms),
    Forms1 = erl_abs_return:simplify(erl_abs_traverse_m:eval(Forms1M, erl_abs, ok)),
    io:format("forms1 is ~ts~n",[erl_abs_lib:ast_safe_to_string(Forms1)]),
    Result = erl_abs_test_lib:compile_test_forms(Forms1),
    erl_abs_return:with_error(
        fun(ErrorState) ->
            ?assertEqual(#{}, erl_abs_error:printable(ErrorState))
        end, Result),
    Value1 = erl_abs_sample_2:test(ok_1),
    Value2 = erl_abs_sample_2:test(ok_2),
    Value3 = erl_abs_sample_2:test(ok_3),
    Value4 = erl_abs_sample_2:test(ok_4),
    ?assertEqual({ok_1, ok_2, ok_3, ok_4}, {Value1, Value2, Value3, Value4}),
    ok.
