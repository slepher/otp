%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_syntax_traverse_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("syntax_tools/include/erl_syntax_struct_name.hrl").
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
    Config.

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
     test_options, test_validator, test_with_attribute, test_forms_with_attribute].

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
test_reduce(_Config) ->
    Forms = erl_syntax_sample_1:forms(),
    File = erl_syntax_traverse_lib:file(Forms),
    ReturnM =
        erl_syntax_traverse:reduce(
          fun({atom, _Line, mark_1} = Node, Acc, #{}) ->
                  erl_syntax_walk_return:new(#{warning => mark_1, state => Acc + 1, node => Node});
             ({atom, _Line, mark_error_1}, _Acc, #{}) ->
                  {error, mark_error_1};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE, traverse => pre}),
    #{'__struct__' := erl_syntax_return_fail, error := Error} = ReturnM,
    FileWarnings = [{File, [{26, ?MODULE, mark_1}]}],
    FileErrors = [{File, [{23, ?MODULE, mark_error_1}]}],
    ?assertMatch({FileErrors, FileWarnings},
                 erl_syntax_error_state:realize(Error)),
    ok.

test_map_with_state_node(_Config) ->
    NodeA = {match, 10, {var, 10, 'A'}, {atom, 10, a}},
    Return =
        erl_syntax_traverse:map_with_state(
          fun({var, Line, 'A'}, Acc, #{}) ->
                  Node1 = {var, Line, 'B'},
                  erl_syntax_walk_return:new(#{state => Acc + 1, node => Node1});
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, NodeA, #{formatter => ?MODULE, traverse => pre}),
    ?assertEqual({match, 10, {var, 10, 'B'}, {atom, 10, a}}, Return),
    ok.

test_map_with_state(_Config) ->
    Forms = erl_syntax_sample_1:forms(),
    File = erl_syntax_traverse_lib:file(Forms),
    ReturnM =
        erl_syntax_traverse:map_with_state(
          fun({atom, _Line, mark_1} = Node, Acc, #{}) ->
                  erl_syntax_walk_return:new(#{warning => mark_1, state => Acc + 1, node => Node});
             ({atom, _Line, mark_error_1}, Acc, #{}) ->
                  {{atom, _Line, mark_error_2}, Acc};
             (Node, Acc, #{}) ->
                  {Node, Acc}
          end, 0, Forms, #{formatter => ?MODULE, traverse => pre}),
    FileWarnings = [{File, [{26, ?MODULE, mark_1}]}],
    #{'__struct__' := erl_syntax_return_ok, error := Error, return := _Return} = ReturnM,
    ?assertMatch({[], FileWarnings}, erl_syntax_error_state:realize(Error)),
    ok.

test_map_spec(_Config) ->
    Nodes = {attribute,56,spec,
             {{test_ok,0},[{type,56,'fun',[{type,56,product,[]},{atom,56,ok}]}]}},
    Nodes1 =
        erl_syntax_traverse:map(
          fun(Node, #{}) ->
                  Node
          end, Nodes, #{traverse => post}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_map_type(_Config) ->
    Nodes = {attribute,21,type,{test,{type,21,record,[{atom,21,test}]},[{var, 21, 'A'}]}},
    Nodes1 =
        erl_syntax_traverse:map(
          fun(Node, #{}) ->
                  Node
          end, Nodes, #{traverse => post}),
    ?assertEqual(Nodes, Nodes1),
    ok.

test_reduce_attr(_Config) ->
    Forms = erl_syntax_sample_1:forms(),
    File = erl_syntax_traverse_lib:file(Forms),
    ReturnM =
        erl_syntax_traverse:reduce(
          fun({attribute, _Line, mark, mark_0} = Node, Acc, #{}) ->
                  erl_syntax_walk_return:new(#{warning => mark_0, state => Acc + 1, node => Node});
             ({attribute, _Line, mark, mark_error_0}, _Acc, #{}) ->
                  {error, mark_error_0};
             (_Node, Acc, #{}) ->
                  Acc
          end, 0, Forms, #{formatter => ?MODULE, traverse => list}),
    #{'__struct__' := ?RETURN_OK, error := Error} = ReturnM,
    FileWarnings = [{File, [{17, ?MODULE, mark_0}]}],
    FileErrors = [{File, [{16, ?MODULE, mark_error_0}]}],
    ?assertMatch({FileErrors, FileWarnings}, erl_syntax_error_state:realize(Error)),
    ok.

test_with_formatter(_Config) ->
    MA =
        erl_syntax_traverse_m:with_formatter(
          formatter_1,
          erl_syntax_traverse_m:update_line(
            10,
            erl_syntax_traverse_m:erl_syntax_traverse_m(
              erl_syntax_walk_return:new(#{return => 10, error => error_0})
             ))),
    #{error := Error} = erl_syntax_traverse_m:run(MA, formatter_0, ok),
    ?assertMatch([{10, formatter_1, error_0}], erl_syntax_error_state:formatted_errors(Error)),
    ok.

test_options(_Config) ->
    Return = #{a => true, e => true},
    Warnings = [{invalid_option_value, {b, c, d}}],
    ?assertMatch(#{return := Return, warnings := Warnings}, erl_syntax_options:options([a, {b, c, d}, e])),
    ok.

test_validator(_Config) ->
    Validator = #{a => boolean,
                  b => {list_of, atom},
                  c => fun is_boolean/1,
                  d => {default, 10}},
    BaseM = erl_syntax_options:validate(Validator, [a, {b,[c,d]}, {b, c, d}, e], #{}),
    Return = #{a => true, b => [c, d], d => 10},
    Warnings = [{invalid_option_value, {b, c, d}}],
    ?assertMatch(#{return := Return, warnings := Warnings}, BaseM),
    ok.

test_with_attribute(_Config) ->
    Forms = erl_syntax_sample_1:forms(),
    Marks =
        erl_syntax_options:with_attribute(
          fun(Attr, Acc) ->
                  [Attr|Acc]
          end, [], Forms, mark, #{simplify_return => true}),
    ?assertEqual([mark_0, mark_error_0], Marks).

test_forms_with_attribute(_Config) ->
    Forms = erl_syntax_sample_1:forms(),
    {Forms1, Marks} =
        erl_syntax_options:forms_with_attribute(
          fun(Attr, Acc, #{line := Line}) ->
                  Node = erl_syntax_traverse_lib:attribute_node(mark_1, Line, Attr),
                  erl_syntax_options:attr_walk_return(#{node => Node, return => [Attr|Acc]})
          end, [], Forms, mark, #{simplify_return => true}),
    Marks1 =
        erl_syntax_options:with_attribute(
          fun(Attr, Acc) ->
                  [Attr|Acc]
          end, [], Forms1, mark_1, #{simplify_return => true}),
    ?assertEqual([mark_0, mark_error_0], Marks1),
    ?assertEqual([mark_0, mark_error_0], Marks),
    ok.
