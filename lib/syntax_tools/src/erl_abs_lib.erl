%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%% Support library for traverse abstract Erlang syntax trees.
%%% @end
%%% Created : 17 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_abs_lib).

-include_lib("abs_struct_name.hrl").

-export([replace_line/2, replace_line_zero/2, abstract_form/1, abstract_form/2,
         original_forms/2, parse_file/2, load_forms/2, compile_forms/2,
         analyze_module_attributes/2, analyze_forms_attributes/2, analyze_forms_file/1,
         analyze_forms_module/1, analyze_transform_file_line/2,
         validate_node/1, ast_safe_to_string/1, ast_to_string/1, relative_path/1,
         map_forms/2, sort_forms/1, insert_forms/2,
         gen_attribute_node/3, gen_exports/2, gen_exported_function/2, gen_function/2, merge_clauses/1,
         concerete/2, try_concerete/2, base_concereter/1,
         with_attribute/5, forms_with_attribute/5,
         option_map/1, validate/2, validate_attribute_option/4]).

-type options() :: option() | [option()] | option_map().
-type option() :: atom() | {atom(), term()}.
-type option_map() :: #{atom() => term()}.
-type validators() :: validator() | [validator()].
-type validator() :: internal_validator() | validator_fun().
-type validator_fun() :: fun((term()) -> validator_fun_return()) | fun((term(), Attrs::#{key := term(), data := term(), is_key := term()}) -> validator_fun_return()) |
                         fun((term(), Attrs::#{key := term(), data := term(), is_key := term()}, IsEmpty::boolean()) -> validator_fun_return()).
-type validator_fun_return() :: {ok, Value::term()} | {error, Reason::term()} | true | false | {warning, Reason::term()} | {warning, Value::term(), Reason::term()} | erl_abs_return:struct(Value::term()).
-type internal_validator() :: boolean | atom | integer | number | binary | {list_of, [validator()]} | {one_of, [term()]} | required | {default, Default::term()} | paired | {paired, PairedKey::atom()}.
%% =====================================================================
%% @spec replace_line(erl_parse:abstract_form(), erl_anno:line()) -> erl_parse:abstract_form()
%%
%% @doc replace line attribute of subtrees to Line.
-spec replace_line(erl_parse:abstract_form(), erl_anno:line()) -> erl_parse:abstract_form().
replace_line(Ast, Line) ->
    replace_line_cond(fun(_) -> true end, Ast, Line).

%% =====================================================================
%% @spec replace_line_zero(erl_parse:abstract_form(), erl_anno:line()) -> erl_parse:abstract_form()
%%
%% @doc Like `replace_line/2', only line attribute of subtrees which is 0 will be replaced.
%% 
%% @see replace_line/2
-spec replace_line_zero(erl_parse:abstract_form(), erl_anno:line()) -> erl_parse:abstract_form().
replace_line_zero(Ast, 0) ->
    Ast;
replace_line_zero(Ast, Line) ->
    replace_line_cond(
      fun(0) -> true;
         (_) -> false
      end, Ast, Line).

replace_line_cond(Cond, Ast, Line) when is_integer(Line) ->
    erl_abs:map(
      fun(Node, #{node := attribute}) ->
              Node;
         (Tuple, _Attr) when is_tuple(Tuple) ->
              case tuple_to_list(Tuple) of
                  [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
                      case Cond(TupleLine) of
                          true ->
                              setelement(2, Tuple, Line);
                          false ->
                              Tuple
                      end;
                  _ ->
                      Tuple
              end;
         (Node, _Attr) ->
              Node
      end, Ast, #{traverse => pre}).

%% =====================================================================
%% @spec abstract_form(term()) -> erl_syntax:syntaxTree()
%% @doc {@link erl_syntax:abstract/1} then {@link erl_syntax:revert/1}.
%%
%% @see erl_syntax:abstract/1
%% @see erl_syntax:revert/1

-spec abstract_form(term()) -> erl_syntax:syntaxTree().
abstract_form(Term) ->
    erl_syntax:revert(erl_syntax:abstract(Term)).

%% =====================================================================
%% @spec abstract_form(term(), erl_anno:line()) -> erl_syntax:syntaxTree()
%% @doc {@link abstract_form/1} then {@link replace_line/2}.
%%
%% @see abstract_form/1
%% @see replace_line/2

-spec abstract_form(term(), erl_anno:line()) -> erl_syntax:syntaxTree().
abstract_form(Term, Line) ->
    replace_line(abstract_form(Term), Line).

%% @spec original_forms(term(), erl_anno:line()) -> erl_syntax:syntaxTree()
%% @doc get original froms before all parse transform compile flags removed by read file attribute in forms and re-parse it.
-spec original_forms([erl_syntax:syntaxTree()], [compile:option()]) -> [erl_syntax:syntaxTree()].
original_forms(Forms, Opts) ->
    File = analyze_forms_file(Forms),
    parse_file(File, Opts).

%% @spec parse_file(file:filename(), [compile:option()]) -> [erl_syntax:syntaxTree()]
%% @doc get forms from file with compile opts.
-spec parse_file(file:filename(), [compile:option()]) -> [erl_syntax:syntaxTree()].
parse_file(File, Opts) ->
    Dir = filename:dirname(File),
    SourceName0 = proplists:get_value(source, Opts, File),
    SourceName = case lists:member(deterministic, Opts) of
                     true -> filename:basename(SourceName0);
                     false -> SourceName0
                 end,
    case epp:parse_file(File,
                        [{includes,[".",Dir|inc_paths(Opts)]},
                         {source_name, SourceName},
                         {macros,pre_defs(Opts)},
                         {default_encoding, utf8},
                         extra]) of
	{ok,Forms,Extra} ->
	    Encoding = proplists:get_value(encoding, Extra),
	    case find_invalid_unicode(Forms, File) of
		none ->
		    Forms;
		{invalid_unicode, File, Line} ->
		    case Encoding of
			none ->
                            Es = [{File,[{Line, compile, reparsing_invalid_unicode}]}],
                            {error, Es, []};
			_ ->
			    Forms
		    end
	    end;
	{error,E} ->
	    Es = [{File,[{none,compile,{epp,E}}]}],
	    {error, Es, []}
    end.

%% @spec compile_forms([erl_syntax:syntaxTree()], [compile:option()|without_warnings]) -> erl_abs_return:struct(module())
%% @doc compile and load forms from file with compile opts, an extra option is without_warnings, while provided, no warnings return or reported, it's useful while temperary compile part of forms on compile time and use it later.
-spec compile_forms([erl_syntax:syntaxTree()], [compile:option()|without_warnings]) -> erl_abs_return:struct(module()).
compile_forms(Forms, Opts) ->
    Opts1 =
        case proplists:get_bool(without_warnings, Opts) of
            true ->
                (Opts ++ [return_errors]) -- [return_warnings, report_warnings];
            false ->
                Opts ++ [return_errors, return_warnings]
        end,
    case compile:forms(Forms, Opts1) of
        {ok, Mod, Binary, Warnings} ->
            erl_abs_return:from_compiler({warning, {Mod, Binary}, Warnings});
        {ok, Mod, Binary} ->
            erl_abs_return:return({Mod, Binary});
        {error, Errors, Warnings} ->
            erl_abs_return:from_compiler({error, Errors, Warnings})
    end.

%% @spec load_forms([erl_syntax:syntaxTree()], [compile:option()]) -> erl_abs_return:struct(module())
%% @doc load forms as compiled module.
-spec load_forms([erl_syntax:syntaxTree()], [compile:option()]) -> erl_abs_return:struct(module()).
load_forms(Forms, Opts) ->
    erl_abs_return:bind(
      compile_forms(Forms, Opts),
      fun({Mod, Binary}) ->
              case code:load_binary(Mod, [], Binary) of
                  {module, Mod} ->
                      erl_abs_return:return(Mod);
                  {error, What} ->
                      erl_abs_return:error_fail(What)
              end
      end).

find_invalid_unicode([H|T], File0) ->
    case H of
        {attribute,_,file,{File,_}} ->
            find_invalid_unicode(T, File);
        {error,{Line,file_io_server,invalid_unicode}} ->
            {invalid_unicode,File0,Line};
        _Other ->
            find_invalid_unicode(T, File0)
    end;
find_invalid_unicode([], _) -> none.

inc_paths(Opts) ->
    [ P || {i,P} <- Opts, is_list(P) ].

pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

%% =====================================================================
%% @spec analyze_module_attributes(atom(), module()) -> [term()]
%% @doc attributes with specific name by module name.
%% @see erl_syntax_lib:analyze_forms/1.
-spec analyze_module_attributes(atom(), module()) -> [term()].
analyze_module_attributes(AttributeName, Module) ->
    Attributes = Module:module_info(attributes),
    lists:reverse(
      lists:foldl(
        fun({Attr, Value}, Acc) when Attr == AttributeName ->
                [Value|Acc];
           (_Other, Acc) ->
                Acc
        end, [], Attributes)).

%% =====================================================================
%% @spec analyze_forms_attributes(atom(), [erl_syntax:syntaxTree()]) -> [term()]
%% @doc attributes with specific name of Analyzed Forms.
%% @see erl_syntax_lib:analyze_forms/1.
-spec analyze_forms_attributes(atom(), [erl_syntax:syntaxTree()]) -> [term()].
analyze_forms_attributes(AttributeName, Forms) ->
    lists:reverse(
      with_attribute(
        fun(Attr, Acc) ->
                [Attr|Acc]
        end, [], Forms, AttributeName, #{simplify_return => true})).

%% =====================================================================
%% @spec analyze_forms_file([erl_syntax:syntaxTree()]) -> string() | undefined
%% @doc file in attribute of Analyzed Forms.
%%
%% @see erl_syntax_lib:analyze_forms/1.
-spec analyze_forms_file([erl_syntax:syntaxTree()]) -> string() | undefined.
analyze_forms_file([Form|Forms]) ->
    case erl_syntax:type(Form) of
        attribute ->
            case erl_syntax_lib:analyze_attribute(Form) of
                {file, {Filename, _Line}} ->
                    Filename;
                _ ->
                    analyze_forms_file(Forms)
            end;
        _ ->
            analyze_forms_file(Forms)
    end;
analyze_forms_file([]) ->
    undefined.

%% =====================================================================
%% @spec analyze_forms_module(term()) -> module() | undefined
%% @doc module of Analyzed Forms.
%%
%% @see erl_syntax_lib:analyze_forms/1.
-spec analyze_forms_module([erl_syntax:syntaxTree()]) -> module() | undefined.
analyze_forms_module(Forms) ->
    Analyzed = erl_syntax_lib:analyze_forms(Forms),
    proplists:get_value(module, Analyzed).

%% @spec analyze_transform_file_line(module(), [erl_syntax:syntaxTree()]) -> {file:filename(), erl_anno:line()}
%% @doc transformer and it's line number of Analyzed Forms.
-spec analyze_transform_file_line(module(), [erl_syntax:syntaxTree()]) -> {file:filename(), erl_anno:line()}.
analyze_transform_file_line(Transformer, Forms) ->
    analyze_transform_file_line(Transformer, Forms, undefined).

analyze_transform_file_line(Transformer, [Form|Forms], Filename) ->
    case erl_syntax:type(Form) of
        attribute ->
            case erl_syntax_lib:analyze_attribute(Form) of
                {file, {Filename1, _Line}} ->
                    analyze_transform_file_line(Transformer, Forms, Filename1);
                {compile, {parse_transform, Transformer}} ->
                    Line = erl_syntax:get_pos(Form),
                    {Filename, Line};
                _ ->
                    analyze_transform_file_line(Transformer, Forms, Filename)
            end;
        _ ->
            analyze_transform_file_line(Transformer, Forms, Filename)
    end;
analyze_transform_file_line(_Transformer, [], Filename) ->
    {Filename, 0}.

%% @spec validate_node([erl_syntax:syntaxTree()] | erl_syntax:syntaxTree()) -> ok | {error, invalid_abstract_node}
%% @doc validate abstract node or nodes, return {error, invalid_abstract_node} if node is invalid.
-spec validate_node([erl_syntax:syntaxTree()] | erl_syntax:syntaxTree()) -> ok | {error, invalid_abstract_node}.
validate_node([Node|T]) ->
    case validate_node(Node) of
        ok ->
            validate_node(T);
        {error, Reason} ->
            {error, Reason}
    end;
validate_node([]) ->
    ok;
validate_node(Node) ->
    try erl_syntax:type(Node) of
        _Type ->
            ok
    catch
        _:{badarg, _}:_ ->
            {error, invalid_abstract_node}
    end.

%% @spec ast_safe_to_string([erl_syntax:syntaxTree()] | erl_syntax:syntaxTree()) -> string()
%% @doc convert ast to printable string, does not raise exception when ast is invalid.
%% @see ast_to_string/1.
-spec ast_safe_to_string([erl_syntax:syntaxTree()] | erl_syntax:syntaxTree()) -> string().
ast_safe_to_string(Form) ->
    try 
        ast_to_string(Form)
    catch
        _:Exception ->
            io_lib:format("ast could not format ~p~n~p", [Exception, Form])
    end.

%% @spec ast_to_string([erl_syntax:syntaxTree()] | erl_syntax:syntaxTree()) -> string()
%% @doc convert ast to printable string.
-spec ast_to_string([erl_syntax:syntaxTree()] | erl_syntax:syntaxTree()) -> string().
ast_to_string(Forms) when is_list(Forms) ->
    erl_prettypr:format(erl_syntax:form_list(Forms));
ast_to_string(Form) ->
    erl_prettypr:format(erl_syntax:form_list([Form])).

%% @spec relative_path(filename:filename()) -> filename:filename()
%% @doc get relative path to current directory of path, useful to format filename of errors and warnings at compile time.
-spec relative_path(filename:filename()) -> filename:filename().
relative_path(Path) ->
    case file:get_cwd() of
        {ok, BasePath} ->
            string:replace(Path, BasePath ++ "/", "");
        {error, _Reason} ->
            Path
    end.

-spec map_forms(fun((erl_syntax:syntaxTree()) -> erl_abs_traverse_m:struct(_A)), [erl_syntax:syntaxTree()]) ->
                       erl_abs_traverse_m:struct(erl_syntax:syntaxTree()).
%% if a list variable is reversed, there is a R suffix after variable name.
%% if a variable is a monad, there is a M suffix after variable name.
map_forms(Fun, Forms) ->
    Functions = forms_functions(Forms),
    map_forms(Fun, Forms, Functions, erl_abs_traverse_m:return({[], []})).

map_forms(Fun, [{error, _Error} = Form|Tails], Functions, HeadsRM) ->
    map_forms(Fun, Tails, Functions, append_to_headsrm(Form, HeadsRM));
map_forms(Fun, [{warning, _Error} = Form|Tails], Functions, HeadsRM) ->
    map_forms(Fun, Tails, Functions, append_to_headsrm(Form, HeadsRM));
map_forms(Fun, [{attribute, _Line, file, {File, _Line}} = Form|Tails], Functions, HeadsRM) ->
    erl_abs_traverse_m:then(
      erl_abs_traverse_m:update_file(File),
      map_forms(Fun, Tails, Functions, append_to_headsrm(Form, HeadsRM)));
map_forms(Fun, [{eof, _Line} = Form|Tails], Functions, HeadsRM) ->
    erl_abs_traverse_m:then(
      erl_abs_traverse_m:eof(),
      map_forms(Fun, Tails, Functions, append_to_headsrm(Form, HeadsRM)));
map_forms(Fun, [Form|Tails], Functions0, HeadsRM) ->
    erl_abs_traverse_m:bind(
      HeadsRM,
      fun(HeadsR) ->
              erl_abs_traverse_m:bind(
                erl_abs_traverse_m:catch_nodes(apply_fun(Fun, Form)),
                fun([]) ->
                        map_forms(Fun, Tails, Functions0, append_to_headsrm(Form, HeadsRM));
                   (NewForms) ->
                        NewFormsFunctions = new_forms_functions(Form, NewForms),
                        {Functions1, HeadsR1, Tails2} =
                            insert_forms(NewForms, NewFormsFunctions, Functions0, HeadsR, Tails),
                        map_forms(Fun, Tails2, Functions1, erl_abs_traverse_m:return(HeadsR1))
                end)
      end);
map_forms(_Fun, [], _Acc, FormsM) ->
    erl_abs_monad:lift_m(
      fun({FHeadsR, AHeadsR}) ->
              lists:reverse(AHeadsR) ++ lists:reverse(FHeadsR)
      end, FormsM).

append_to_headsrm(Form, HeadsRM) ->
    erl_abs_monad:lift_m(fun(HeadsR) -> append_to_headsr(Form, HeadsR) end, HeadsRM).

append_to_headsr({attribute, _Line, spec, _SpecValue} = Spec, {FHeadsR, AHeadsR}) ->
    {[Spec|FHeadsR], AHeadsR};
append_to_headsr({function, _Line, _Name, _Arity, _Clauses} = Function, {FHeadsR, AHeadsR}) ->
    {[Function|FHeadsR], AHeadsR};
append_to_headsr({eof, _Line} = Eof, {FHeadsR, AHeadsR}) ->
    {[Eof|FHeadsR], AHeadsR};
append_to_headsr(Form, {[], AHeadsR}) ->
    {[], [Form|AHeadsR]};
append_to_headsr(Form, {FHeadsR, AHeadsR}) ->
    {[Form|FHeadsR], AHeadsR}.

apply_fun(Fun, Form) ->
    case Fun(Form) of
        #{?STRUCT_KEY := ?TRAVERSE_M} = Traverse ->
            Traverse;
        #{?STRUCT_KEY := ?RETURN_OK} = Return ->
            erl_abs_traverse_m:erl_abs_traverse_m(Return);
        #{?STRUCT_KEY := ?RETURN_FAIL} = Return ->
            erl_abs_traverse_m:erl_abs_traverse_m(Return);
        Return ->
            erl_abs_traverse_m:erl_abs_traverse_m(erl_abs_walk_return:new(Return))
    end.

%% =====================================================================
%% detect new forms functions
%% =====================================================================
new_forms_functions(Form, NewForms) ->
    FormFunctions = forms_functions([Form]),
    NewFormsFunctions = forms_functions(NewForms),
    ordsets:subtract(NewFormsFunctions, FormFunctions).

forms_functions(Forms) ->
    forms_functions(Forms, ordsets:new()).

forms_functions(Forms, Functions0) ->
    lists:foldl(
      fun({function, _Line, Name, Arity, _Clauses}, Acc) ->
              ordsets:add_element({Name, Arity}, Acc);
         (_Node, Acc) ->
              Acc
      end, Functions0, Forms).

%% @spec sort_forms([erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()]
%% @doc sort forms to valid order, same as insert_forms(Forms, []).
%% @see insert_forms/2.
-spec sort_forms([erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()].
sort_forms(Forms) ->
    insert_forms(Forms, []).

%% @spec insert_forms([erl_syntax:syntaxTree()], [erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()]
%% @doc insert new forms to froms with order fillow these rules
%% <ul>
%% <li>rename functions in forms which function has '__original__' call in it with same name and arity in new forms.</li>
%% <li>'__original__'(Args1, Args2, ...) will be transformed to RenamedFunction(Args1, Args2, ...).</li>
%% <li>after rename, it dose not matter function or spec with duplicated name and arity, lint will get these errors.</li>
%% <li>attribute in new forms will insert before first spec or function or eof.</li>
%% <li>spec in new forms will insert before function with same name and arity or eof.</li>
%% <li>function in new forms will insert after spec with same name and arity or insert before eof.</li>
%% <li>eof_marker in new forms will be dropped if there is an eof_marker already exists in forms.</li>
%% <li>eof_marker in new forms will insert at the end of forms if there is no eof_market in forms.</li>
%% <li>if form is marked from other file (between -file(file1) and -file(file2)), do not change this mark.(not implemented)</li>
%% </ul>
%% @end
-spec insert_forms([erl_syntax:syntaxTree()], [erl_syntax:syntaxTree()]) -> [erl_syntax:syntaxTree()].
insert_forms(NewForms, Forms) ->
    Functions = forms_functions(Forms),
    NewFormsFunctions = forms_functions(NewForms),
    HeadsR = lists:foldl(fun append_to_headsr/2, {[], []}, Forms),
    {_Functions, {FHeadsR, AHeadsR}, Tails} = insert_forms(NewForms, NewFormsFunctions, Functions, HeadsR, []),
    lists:reverse(AHeadsR) ++ lists:reverse(FHeadsR) ++ Tails.

%% merge forms rule
%% 1. rename functions which generated functions has '__original__' call in it with same name and arity.
%% 2. rename spec if new spec is generated
%% 3. for map_forms/2, code does not know how forms will change in tails
%$ 4. if it's need to adjust new generated form order, only forms in heads should be affeted
%% 5. new generated attribute should insert before first -spec in heads
%% 6. new generated spec should insert before function with same name and arity in heads
%% 7. new generated function should insert after spec with same name and arity in heads.
%% 8. if file of new generated form is different from oldone, file attribute should be created to mark file.
%% new_forms, heads, tails is original order.
%% after merge forms, Heads is reversed order, tails is original order.
insert_forms(NewForms, NewFormsFucntions, Functions, HeadsR, Tails) ->
    {Functions1, NewForms1, HeadsR1, Tails1} = merge_functions(NewForms, NewFormsFucntions, Functions, HeadsR, Tails),
    HeadsR2 = lists:foldl(fun form_insert_heads/2, HeadsR1, NewForms1),
    {Functions1, HeadsR2, Tails1}.

%% =====================================================================
%% merge functions
%% =====================================================================
merge_functions(NewForms, NewFormsFucntions, Functions, {FHeadsR, AHeadsR}, Tails) ->
    ExistsNewFunctions =
        ordsets:from_list(
          lists:filter(
            fun(NameArity) ->
                    ordsets:is_element(NameArity, Functions)
            end, ordsets:to_list(NewFormsFucntions))),
    Functions1 = ordsets:union(Functions, NewFormsFucntions),
    {Functions2, NewFormsR2, FHeadsR1, Tails1} =
        lists:foldl(
          fun({function, _Line, Name, Arity, _Clauses} = Form, {FunctionsAcc, NewFormsAcc, HeadsRAcc, TailsAcc}) ->
                  case ordsets:is_element({Name, Arity}, ExistsNewFunctions) andalso is_renamed(Arity, Form) of
                      true ->
                          NewName = new_function_name(Name, Arity, FunctionsAcc),
                          Form1 = update_call_name('__original__', NewName, Arity, Form),
                          HeadsRAcc1 = update_function_name(Name, Arity, NewName, HeadsRAcc),
                          TailsAcc1 = update_function_name(Name, Arity, NewName, TailsAcc),
                          {ordsets:add_element({NewName, Arity}, FunctionsAcc), [Form1|NewFormsAcc], HeadsRAcc1, TailsAcc1};
                      false ->
                          {FunctionsAcc, [Form|NewFormsAcc], HeadsRAcc, TailsAcc}
                  end;
             (Form, {FunctionsAcc, NewFormsAcc, HeadsAcc, TailsAcc}) ->
                  {FunctionsAcc, [Form|NewFormsAcc], HeadsAcc, TailsAcc}
          end, {Functions1, [], FHeadsR, Tails}, NewForms),
    {Functions2, lists:reverse(NewFormsR2), {FHeadsR1, AHeadsR}, Tails1}.

is_renamed(Arity, Form) ->
    erl_abs:reduce(
      fun({call, _Line1, {atom, _Line2, '__original__'}, Arguments}, Acc1, #{}) ->
              case length(Arguments) == Arity of
                  true ->
                      true;
                  false ->
                      Acc1
              end;
         (_Node, Acc1, #{}) ->
              Acc1
      end, false, Form, #{simplify_return => true, traverse => pre}).

new_function_name(FName, Arity, Functions) ->
    new_function_name(FName, Arity, Functions, 1).

new_function_name(FName, Arity, Functions, Counter) ->
    FName1 = list_to_atom(atom_to_list(FName) ++ "_" ++ integer_to_list(Counter)),
    case ordsets:is_element({FName1, Arity}, Functions) of
        true ->
            new_function_name(FName, Arity, Functions, Counter + 1);
        false ->
            FName1
    end.

update_function_name(Name, Arity, NewName, Forms) ->
    lists:map(
      fun({function, Line, FName, FArity, Clauses}) 
            when (FName == Name) andalso (FArity == Arity) ->
              Clauses1 = update_call_name(Name, NewName, Arity, Clauses),
              {function, Line, NewName, Arity, Clauses1};
         (Form) ->
              Form
      end, Forms).

update_call_name(OrignalName, NewName, Arity, Function) ->
    erl_abs:map(
      fun({call, Line, {atom, Line2, Name}, Arguments}, #{}) 
            when (Name == OrignalName) andalso (length(Arguments) == Arity) ->
              {call, Line, {atom, Line2, NewName}, Arguments};
         (Node, #{}) ->
              Node
      end, Function, #{traverse => pre, simplify_return => true}).

%% =====================================================================
%% form_insert_heads
%% =====================================================================
form_insert_heads({attribute, _Line, file, _FileValue} = File, HeadsR) ->
    append_to_headsr(File, HeadsR);
form_insert_heads({attribute, Line, export, Exports}, {FHeadsR, AHeadsR}) ->
    Exports1 = remove_duplicated_exports(Exports, FHeadsR),
    Exports2 = remove_duplicated_exports(Exports1, AHeadsR),
    case Exports2 of
        [] ->
            {FHeadsR, AHeadsR};
        _ ->
            Export = {attribute, Line, export, Exports2},
            {FHeadsR, [Export|AHeadsR]}
    end;
form_insert_heads({function, _Line1, Name, Arity, _Clauses} = Function, {FHeadsR, AHeadsR}) ->
    FHeadsR1 = insert_function_or_spec(function, Name, Arity, Function, FHeadsR),
    {FHeadsR1, AHeadsR};
form_insert_heads({attribute, _Line1, spec, {{Name, Arity}, _SpecType}} = Spec, {FHeadsR, AHeadsR}) ->
    FHeadsR1 = insert_function_or_spec(spec, Name, Arity, Spec, FHeadsR),
    {FHeadsR1, AHeadsR};
form_insert_heads({eof, _Line1}, {[{eof, _Line2} = Eof|FHeadsR], AHeadsR}) ->
    {[Eof|FHeadsR], AHeadsR};
form_insert_heads({eof, _Line} = Eof, {FHeadsR, AHeadsR}) ->
    {[Eof|FHeadsR], AHeadsR};
form_insert_heads(Attribute, {FHeadsR, AHeadsR}) ->
    {FHeadsR, [Attribute|AHeadsR]}.

remove_duplicated_exports(Exports1, [{attribute, _Line, export, Exports}|T]) ->
    Exports2 = Exports1 -- Exports,
    remove_duplicated_exports(Exports2, T);
remove_duplicated_exports(Exports, [_Form|T]) ->
    remove_duplicated_exports(Exports, T);
remove_duplicated_exports([], _Forms) ->
    [];
remove_duplicated_exports(Exports, []) ->
    Exports.

insert_function_or_spec(Type, Name, Arity, Form, HeadsR) ->
    insert_function_or_spec(Type, Name, Arity, Form, HeadsR, [], HeadsR).

insert_function_or_spec(function, Name, Arity, Function,
                        [{attribute, _Line, {{Name, Arity}, _SpecType}} = Spec|HeadsR], Tails, _InitHeadsR) ->
    lists:reverse(Tails) ++ [Function,Spec|HeadsR];
insert_function_or_spec(spec, Name, Arity, Spec,
                        [{function, _Line, Name, Arity, _Clauses} = Function|HeadsR], Tails, _InitHeadsR) ->
    lists:reverse(Tails) ++ [Function,Spec|HeadsR];
insert_function_or_spec(Type, Name, Arity, Form1, [Form|HeadsR], Tails, InitHeadsR) ->
    insert_function_or_spec(Type, Name, Arity, Form1, HeadsR, [Form|Tails], InitHeadsR);
insert_function_or_spec(_Type, _Name, _Arity, Form, [], _Tails, [{eof, _Line} = Eof|InitHeadsR]) ->
    [Eof,Form|InitHeadsR];
insert_function_or_spec(_Type, _Name, _Arity, Form, [], _Tails, InitHeadsR) ->
    [Form|InitHeadsR].

%% =====================================================================
%% @spec gen_attribute_node(atom(), integer(), term()) -> erl_syntax:syntaxTree()
%% @doc build {attribute, Line, Name, Value}.
-spec gen_attribute_node(atom(), integer(), term()) -> erl_syntax:syntaxTree().
gen_attribute_node(Name, Line, Value) when is_atom(Name), is_integer(Line) ->
    {attribute, Line, Name, Value}.

%% =====================================================================
%% @spec gen_exported_function(atom(), erl_syntax:syntaxTree()) -> [erl_syntax:syntaxTree()]
%% @doc generate function and export attribute.
%% @see gen_function/2
-spec gen_exported_function(atom(), erl_syntax:syntaxTree()) -> [erl_syntax:syntaxTree()].
gen_exported_function(Name, Fun) ->
    Function = gen_function(Name, Fun),
    Line = erl_syntax:get_pos(Function),
    FunctionFa = function_fa(Function),
    [gen_exports([FunctionFa], Line), Function]. 

%% =====================================================================
%% @spec gen_function(atom(), erl_syntax:syntaxTree() | [erl_parse:abstract_clause()] | erl_parse:abstract_clause()) -> [erl_syntax:syntaxTree()]
%% @doc generate function by name and `erl_parse' node of anonymous function or clauses or expressions.
-spec gen_function(atom(), erl_syntax:syntaxTree() | [erl_parse:abstract_clause()] | erl_parse:abstract_clause()) -> [erl_syntax:syntaxTree()].
gen_function(Name, {'fun', Line, {clauses, Clauses}}) ->
    gen_function(Name, Line, Clauses);
gen_function(Name, {named_fun, Line, {var, _, FunName1}, Clauses}) ->
    Clauses1 = 
        erl_abs:map(
          fun({var, FunNameLine, FunName2}, #{type := expression}) when FunName1 == FunName2 ->
                  {atom, FunNameLine, FunName2};
             (Node, _Attr) ->
                  Node
          end, Clauses, #{traverse => leaf, simplify => true}),
    gen_function(Name, Line, Clauses1);
gen_function(Name, [Clause|_T] = Forms) ->
    case erl_syntax:type(Clause) of
        clause ->
            Line = erl_syntax:get_pos(Clause),
            gen_function(Name, Line, Forms);
        _ ->
            Line = erl_syntax:get_pos(Clause),
            gen_function(Name, Line, [{clause, Line, [], [], Forms}])
    end;
gen_function(Name, Clause) ->
    gen_function(Name, [Clause]).

gen_function(Name, Line, Clauses) when is_list(Clauses) ->
    Arity = clause_arity(Clauses),
    {function, Line, Name, Arity, Clauses}.

function_fa({function, _Line, Name, Arity, _Clauses}) ->
    {Name, Arity}.

clause_arity([{clause, _Line, Patterns, _Guards, _Body}|_T]) ->
    length(Patterns).

-spec merge_clauses([erl_syntax:syntaxTree()]) -> erl_syntax:syntaxTree().
merge_clauses([{'fun', Line, {clauses, _}}|_T] = Nodes) ->
    NClauses =
        lists:flatten(
          lists:map(
            fun({'fun', _, {clauses, FClauses}}) ->
                    FClauses
            end, Nodes)),
    {'fun', Line, {clauses, NClauses}}.

%% it's strange to generate export attribute node by erl_syntax so hard
%% I write this down manaualy.
%% erl_syntax:revert(erl_syntax:attribute(erl_syntax:abstract(export), [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(Name), erl_syntax:integer(Arity))])])).
%% @spec gen_exports([{atom(), integer()}], erl_anno:line()) -> erl_syntax:syntaxTree()
%% @doc generate {attribute, Line, export, Exports} node.
-spec gen_exports([{atom(), integer()}], erl_anno:line()) -> erl_syntax:syntaxTree().
gen_exports(Exports, Line) when is_list(Exports) ->
    gen_attribute_node(export, Line, Exports).

%% @spec concerete(A, [fun((A) -> {ok, B} | error)]) -> B
%% @doc works like {@link try_concerete/2}, returns B or throw exception.
-spec concerete(A, [fun((A) -> {ok, B} | error)]) -> B.
concerete(A, Concereters) ->
    case try_concerete(A, Concereters) of
        {ok, B} ->
            B;
        error ->
            exit({incompatable_value, A})
    end.

%% @spec try_concerete(A, [fun((A) -> {ok, B} | error)]) -> {ok, B} | error
%% @doc try Concereters, while Concereter(A) returns {ok, B}, returns {ok, B}, while it returns error, try next concereter, if all concereter failed, return error.
-spec try_concerete(A, Concereters::[fun((A) -> {ok, B} | error)]) -> {ok, B} | error.
try_concerete(A, [Concereter|T]) ->
    case Concereter(A) of
        {ok, B} ->
            {ok, B};
        error ->
            try_concerete(A, T)
    end;
try_concerete(_A, []) ->
    error.

%% @spec base_concereter(_A) -> {ok, _B} | error
%% @doc a concereter for {@link try_concerete/2} which is used in erl_abs_walk_return and erl_abs_lib.
-spec base_concereter(_A) -> {ok, _B} | error.
base_concereter({warning, Warning}) ->
    {ok, #{warnings => [Warning]}};
base_concereter({warnings, Warnings}) ->
    {ok, #{warnings => Warnings}};
base_concereter({warning, B, Warning}) ->
    {ok, #{return => B, warnings => [Warning]}};
base_concereter({warnings, B, Warnings}) ->
    {ok, #{return => B, warnings => Warnings}};
base_concereter({error, Error}) ->
    {ok, #{errors => [Error]}};
base_concereter({errors, Errors}) when is_list(Errors) ->
    {ok, #{errors => Errors}};
base_concereter({error, B, Error}) ->
    {ok, #{return => B, errors => [Error]}};
base_concereter({errors, B, Errors}) when is_list(Errors) ->
    {ok, #{return => B, errors => Errors}};
base_concereter({ok, B}) ->
    {ok, #{return => B}};
base_concereter(ok) ->
    {ok, #{}};
base_concereter(_Other) ->
    error.

-spec with_attribute(fun((term(), State) -> State), State, [erl_syntax:syntaxTree()], #{}, #{}) -> erl_abs_return:struct(State).
with_attribute(Fun, Init, Forms, Attr, Opts) ->
    erl_abs:reduce(
      fun({attribute, Line, Attr1, AttrValue}, Acc, #{}) when Attr1 == Attr ->
              erl_abs_traverse_m:bind(
                values_apply_fun_m(Fun, AttrValue, Acc, #{line => Line}),
                fun(Acc1) ->
                        erl_abs_traverse_m:put(Acc1)
                end);
         (_Node, Acc, #{}) ->
              Acc
      end, Init, Forms, Opts#{traverse => list}).

-spec forms_with_attribute(fun((term(), State) -> State), State, [erl_syntax:syntaxTree()], #{}, #{}) -> [erl_syntax:syntaxTree()].
forms_with_attribute(Fun, Init, Forms, Attr, Opts) ->
    Fun1 = update_forms_with_attribute_f(Fun),
    erl_abs:mapfold(
      fun({attribute, Line, Attr1, AttrValue} = Node, Acc, #{}) when Attr1 == Attr ->
              erl_abs_traverse_m:bind(
                erl_abs_traverse_m:catch_nodes(values_apply_fun_m(Fun1, AttrValue, Acc, #{line => Line})),
                fun(Nodes) ->
                        erl_abs_traverse_m:nodes([Node|Nodes])
                end);
         (Node, Acc, #{}) ->
              {Node, Acc}
      end, Init, Forms, Opts#{traverse => list}).

update_forms_with_attribute_f(Fun) ->
    fun(Value, Acc, Opts) ->
            erl_abs_traverse_m:bind(
              values_apply_fun_m(Fun, Value, Acc, Opts),
              fun({Nodes, State}) ->
                      erl_abs_traverse_m:then(
                        erl_abs_traverse_m:then(
                          erl_abs_traverse_m:nodes(Nodes),
                          erl_abs_traverse_m:put(State)),
                        erl_abs_traverse_m:return(State));
                 (Return) ->
                      erl_abs_traverse_m:return(Return)
              end)
    end.

values_apply_fun_m(Fun, AttrValues, Acc, Opts) when is_list(AttrValues) ->
    case maps:get(deep_attr, Opts, true) of
        true ->
            erl_abs_monad:foldl_m(
              fun(AttrValue, Acc1) ->
                      values_apply_fun_m(Fun, AttrValue, Acc1, Opts)
              end, Acc, AttrValues, erl_abs_traverse_m);
        false ->
            value_apply_fun_m(Fun, AttrValues, Acc, Opts)
    end;
values_apply_fun_m(Fun, AttrValue, Acc, Opts) ->
    value_apply_fun_m(Fun, AttrValue, Acc, Opts).

value_apply_fun_m(Fun, Value, Acc, Opts) ->
    Return = value_apply_fun(Fun, Value, Acc, Opts),
    case Return of
        #{?STRUCT_KEY := ?RETURN_OK} ->
            erl_abs_traverse_m:erl_abs_traverse_m(Return);
        #{?STRUCT_KEY := ?RETURN_FAIL} ->
            erl_abs_traverse_m:erl_abs_traverse_m(Return);
        #{?STRUCT_KEY := ?TRAVERSE_M} ->
            Return;
        #{?STRUCT_KEY := ?WALK_RETURN} ->
            erl_abs_traverse_m:erl_abs_traverse_m(Return);
        Return ->
            erl_abs_traverse_m:return(Return)
    end.

value_apply_fun(Fun, Value, Acc, _Opts) when is_function(Fun, 2) ->
    Fun(Value, Acc);
value_apply_fun(Fun, Value, Acc, Opts) when is_function(Fun, 3) ->
    Fun(Value, Acc, Opts).

-spec option_map(options()) -> erl_abs_return:struct(option_map()).
option_map(Atom) when is_atom(Atom) ->
    option_map([Atom]);
option_map({Key, Value}) when is_atom(Key) ->
    option_map([{Key, Value}]);
option_map(OptionList) when is_list(OptionList) ->
    erl_abs_monad:foldl_m(
      fun({Key, Value}, Acc) when is_atom(Key) ->
              erl_abs_return:return(maps:put(Key, Value, Acc));
         (Key, Acc) when is_atom(Key) ->
              erl_abs_return:return(maps:put(Key, true, Acc));
         (Value, Acc) ->
              erl_abs_return:then(
                erl_abs_return:warning({invalid_option_value, Value}),
                erl_abs_return:return(Acc))
      end, maps:new(), OptionList, erl_abs_return);
option_map(Options) when is_map(Options) ->
    option_map(maps:to_list(Options));
option_map(Options) ->
    erl_abs_return:then(
      erl_abs_return:warning({invalid_option_value, Options}),
      erl_abs_return:return(#{})).

validate_attribute_option(Validator, ParseTransformer, Attribute, Forms) ->
    {MapValidator, DefaultValidator} = split_default_validator(Validator),
    erl_abs_return:bind(
      erl_abs_lib:with_attribute(
        fun(AttributeOpts, Acc) ->
                erl_abs_monad:lift_m(fun(AttributeOpts1) -> maps:merge(Acc, AttributeOpts1) end,
                                     validate(MapValidator, AttributeOpts))
        end, maps:new(), Forms, Attribute, #{formatter => ParseTransformer, simplify_return => false}),
      fun(MergedOptions) ->
              Return = validate(DefaultValidator, MergedOptions),
              {File, Line} = analyze_transform_file_line(ParseTransformer, Forms),
              erl_abs_return:with_error(
                fun(ErrorState) ->
                        ErrorState1 = erl_abs_error:update_line(Line, ParseTransformer, ErrorState),
                        erl_abs_error:update_file(File, ErrorState1)
                end, Return)
      end).

split_default_validator(Validator) ->
    maps:fold(
      fun(Key, KeyValidator, {MapValidatorAcc, DefaultValidatorAcc}) ->
              {KeyMapValidators, KeyDefaultValidators} =
                  lists:foldl(
                    fun(ValidatorListItem, {KeyMapValidatorAcc, KeyDefaultValidatorAcc}) ->
                            case match_default_validator(ValidatorListItem) of
                                true ->
                                    {KeyMapValidatorAcc, [ValidatorListItem|KeyDefaultValidatorAcc]};
                                false ->
                                    {[ValidatorListItem|KeyMapValidatorAcc], KeyDefaultValidatorAcc}
                            end
                    end, {[], []}, validator_list(KeyValidator)),
              MapValidatorAcc1 = put_validator(Key, KeyMapValidators, MapValidatorAcc),
              DefaultValidatorAcc1 = put_validator(Key, KeyDefaultValidators, DefaultValidatorAcc),
              {MapValidatorAcc1, DefaultValidatorAcc1}
      end, {maps:new(), maps:new()}, Validator).

match_default_validator({default, _}) ->
    true;
match_default_validator({default_key, _}) ->
    true;
match_default_validator(required) ->
    true;
match_default_validator(_) ->
    false.

put_validator(Key, [Validator], Map) ->
    maps:put(Key, Validator, Map);
put_validator(Key, [], Map) ->
    maps:put(Key, any, Map);
put_validator(Key, Validators, Map) ->
    maps:put(Key, Validators, Map).

%% @spec validate(#{OptionKey::atom() => validators()}, options()) -> erl_abs_return:struct(option_map())
%% @doc validate an option constructs as expected, for each key, use internal or userdef validator to validate the options value.
%% if validator is used, the default formatter should be used in last match to format validator returned warnings and errors.
%% ```-module(my_transformer).
%% -export([parse_transform/2, format_error/1]).
%% parse_transform(Forms, Options) ->
%%      %% get some errors when use validator
%%      %% FormsWithErrors = blabla...
%%      %% FormsWithErrors.
%% format_error(my_error_1) ->
%%    io_lib:format("get error_1");
%% format_error(Error) ->
%%    erl_abs:format_error(Error).'''

-spec validate(#{atom() => validators()}, options()) -> erl_abs_return:struct(option_map()).
validate(Validator, Options) ->
    erl_abs_return:bind(
      option_map(Options),
      fun(OptionMap) ->
              validate_option_map(Validator, OptionMap)
      end).

validate_option_map(ValidatorMap, OptionMap) ->
    erl_abs_return:bind(
      erl_abs_monad:foldl_m(
        fun({Key, Validator}, {RestMapAcc, ValidatedAcc}) ->
                {Value, IsKey} =
                    case maps:find(Key, OptionMap) of
                        {ok, Val} ->
                            {Val, true};
                        error ->
                            {undefined, false}
                    end,
                AccM = validate_map_value(Validator, Key, Value, OptionMap, ValidatedAcc, IsKey),
                erl_abs_monad:lift_m(fun(ValidatedAcc1) -> {maps:remove(Key, RestMapAcc), ValidatedAcc1} end, AccM)
        end, {OptionMap, maps:new()}, to_list(ValidatorMap), erl_abs_return),
      fun({RestOptionMap, ValidatedOptionMap}) ->
              case maps:keys(RestOptionMap) of
                  [] ->
                      erl_abs_return:return(ValidatedOptionMap);
                  RestKeys ->
                      erl_abs_return:then(
                        erl_abs_return:warning({unexpected_option_keys, RestKeys}),
                        erl_abs_return:return(ValidatedOptionMap))
              end
      end).


%% sort validator if there is validate dependency.
to_list(ValidatorMap) when is_map(ValidatorMap) ->
    Deps = search_deps(ValidatorMap),
    OrderedDeps = order_deps(Deps),
    FirstValidatorList =
        lists:foldl(
          fun(Key, Acc) ->
                  Validator = maps:get(Key, ValidatorMap),
                  [{Key, Validator}|Acc]
          end, [], lists:reverse(OrderedDeps)),
    FirstValidatorList ++ maps:to_list(maps:without(OrderedDeps, ValidatorMap)).

search_deps(ValidatorMap) ->
    search_deps(maps:to_list(ValidatorMap), ValidatorMap, #{}).

search_deps([{Key, Validators}|T], ValidatorMap, Acc) ->
    Acc1 =
        lists:foldl(
          fun({default_key, DefaultKey}, DefaultKeyAcc) ->
                  case maps:is_key(DefaultKey, ValidatorMap) of
                      true ->
                          maps:put(Key, DefaultKey, DefaultKeyAcc);
                      false ->
                          exit({deps_key_not_exists, DefaultKey})
                  end;
             (_Validator, DefaultKeyAcc) ->
                  DefaultKeyAcc
          end, Acc, validator_list(Validators)),
    search_deps(T, ValidatorMap, Acc1);
search_deps([], _ValidatorMap, Acc) ->
    Acc.

validator_list(Validators) when is_list(Validators) ->
    Validators;
validator_list(Validator) ->
    [Validator].

order_deps(Deps) ->
    order_deps(maps:keys(Deps), Deps, []).

order_deps([Key|T], RestDeps, Acc) ->
    {RestDeps1, KeyDeps} = follow_deps(Key, RestDeps, [], Acc),
    T1 = T -- KeyDeps,
    Acc1 = Acc ++ KeyDeps,
    order_deps(T1, RestDeps1, Acc1);
order_deps([], _RestDeps, Acc) ->
    Acc.

follow_deps(Key, RestDeps, Acc, TotalAcc) ->
    RestDeps1 = maps:remove(Key, RestDeps),
    case maps:find(Key, RestDeps) of
        {ok, DepKey} ->
            case lists:member(DepKey, [Key|Acc]) of
                false ->
                    follow_deps(DepKey, RestDeps1, [Key|Acc], TotalAcc);
                true ->
                    exit({cycle_deps_detected, [DepKey,Key|Acc]})
            end;
        error ->
            Acc1 = case lists:member(Key, TotalAcc) of
                       true ->
                           Acc;
                       false ->
                           [Key|Acc]
                   end,
            {RestDeps1, Acc1}
    end.
validate_map_value(Validator, Key, Value, ToValidate, ValidatedData, IsKey) ->
    Attrs = #{key => Key, is_key => IsKey, data => ToValidate, validated_data => ValidatedData},
    Return = validate_value(Validator, Value, Attrs),
    Return1 =
        erl_abs_monad:nested_withs(
          fun(Reason) -> 
                  {validate_key_failure, Reason, Key, Value} 
          end, [fun erl_abs_error:with_failure/2, fun erl_abs_return:with_error/2], Return),
    case erl_abs_return:has_error(Return1) of
        true ->
            erl_abs_monad:lift_m(fun(_) -> ValidatedData end, Return1);
        false ->
            erl_abs_monad:lift_m(
              fun(Value1) ->
                      case (not IsKey) and (Value1 == undefined) of
                          true ->
                              ValidatedData;
                          false ->
                              maps:put(Key, Value1, ValidatedData)
                      end
              end, Return1)
    end.

to_abs_return(_Validator, _Value, #{?STRUCT_KEY := ?RETURN_OK} = Struct) ->
    Struct;
to_abs_return(_Validator, _Value, #{?STRUCT_KEY := ?RETURN_FAIL} = Struct) ->
    Struct;
to_abs_return(_Validator, Value, ok) -> 
    erl_abs_return:return(Value);
to_abs_return(_Validator, Value, true) ->
    erl_abs_return:return(Value);
to_abs_return(Validator, _Value, error) ->
    erl_abs_return:error({invalid_value, Validator});
to_abs_return(Validator, _Value, false) ->
    erl_abs_return:error({invalid_value, Validator});
to_abs_return(_Validator, _Value, {ok, Value1}) -> 
    erl_abs_return:return(Value1);
to_abs_return(Validator, Value, {warning, Reason}) ->
    Reason1 = format_validator_reason(Validator, Reason),
    erl_abs_return:then(
      erl_abs_return:warning(Reason1),
      erl_abs_return:return(Value));
to_abs_return(Validator, _Value, {warning, Value1, Reason}) ->
    Reason1 = format_validator_reason(Validator, Reason),
    erl_abs_return:then(
      erl_abs_return:warning(Reason1),
      erl_abs_return:return(Value1));
to_abs_return(Validator, _Value, {error, Reason}) -> 
    Reason1 = format_validator_reason(Validator, Reason),
    erl_abs_return:error(Reason1);
to_abs_return(Validator, _Value, Other) ->
    erl_abs_return:error({invalid_validator_return, Validator, Other}).

format_validator_reason(Validator, invalid_validator_arg) ->
    {invalid_validator_arg, Validator};
format_validator_reason(_Validator, Reason) ->
    Reason.

validate_value(Validator, Value, Attrs) ->
    Return = validate_value_1(Validator, Value, Attrs),
    to_abs_return(Validator, Value, Return).

validate_value_1([Validator|T], Value, Attrs) ->
    Return = validate_value(Validator, Value, Attrs),
    case erl_abs_return:has_error(Return) of
        true ->
            Return;
        false ->
            erl_abs_return:bind(
              Return,
              fun(Value1) ->
                      validate_value(T, Value1, Attrs)
              end)
    end;
validate_value_1([], Value, _Attrs) ->
    {ok, Value};
validate_value_1(Validator, Value, Attrs) when is_atom(Validator) ->
    apply_validator_by_name(Validator, Value, undefined, Attrs);
validate_value_1({Validator, Args}, Value, Attrs) when is_atom(Validator) ->
    apply_validator_by_name(Validator, Value, Args, Attrs);
validate_value_1(ValidatorFun, Value, Attrs) when is_function(ValidatorFun) ->
    apply_validator_fun(ValidatorFun, Value, Attrs);
validate_value_1(Validator, _Value, _Attrs) ->
    {error, {invalid_validator, Validator}}.

apply_validator_by_name(Validator, Value, Args, Attrs) ->
    Validators = #{
                   boolean => fun is_boolean/1,
                   atom => fun is_atom/1,
                   number => fun is_number/1,
                   integer => fun is_integer/1,
                   binary => fun is_binary/1,
                   any => fun any/1,
                   'or' => fun 'or'/4,
                   one_of => fun one_of/2,
                   list_of => fun list_of/3,
                   paired => fun paired/4,
                   required => fun required/4,
                   default => fun default/4,
                   default_key => fun default_key/4
                  },
    case maps:find(Validator, Validators) of
        {ok, InternalFun} ->
            ValidatorFun = internal_to_validator_fun(InternalFun, Args),
            apply_validator_fun(ValidatorFun, Value, Attrs);
        error ->
            {error, {invalid_validator, Validator}}
    end.

internal_to_validator_fun(InternalFun, _Args) when is_function(InternalFun, 1) ->
    InternalFun;
internal_to_validator_fun(InternalFun, Args) when is_function(InternalFun, 2) ->
    fun(Value) -> InternalFun(Value, Args) end;
internal_to_validator_fun(InternalFun, Args) when is_function(InternalFun, 3) ->
    fun(Value, Attrs) -> InternalFun(Value, Args, Attrs) end;
internal_to_validator_fun(InternalFun, Args) when is_function(InternalFun, 4) ->
    fun(Value, IsEmpty, Attrs) -> InternalFun(Value, Args, IsEmpty, Attrs) end.

apply_validator_fun(ValidatorFun, Value, #{is_key := IsKey} = Attrs) when is_function(ValidatorFun, 3) ->
    IsEmpty = (not IsKey) and (Value == undefined),
    ValidatorFun(Value, IsEmpty, Attrs);
apply_validator_fun(_ValidatorFun, undefined = Value, #{is_key := false}) ->
    {ok, Value};
apply_validator_fun(ValidatorFun, Value, #{} = Attrs) when is_function(ValidatorFun, 2) ->
    ValidatorFun(Value, Attrs);
apply_validator_fun(ValidatorFun, Value, #{}) when is_function(ValidatorFun, 1) ->
    ValidatorFun(Value).

any(_Value) ->
    true.

one_of(Value, List) when is_list(List) ->
    lists:member(Value, List);
one_of(_Value, _NotList) ->
    {error, invalid_validator_arg}.

list_of([H|T], Validator, Attrs) ->
    BaseM = validate_value(Validator, H, Attrs),
    case erl_abs_return:has_error(BaseM) of
        true ->
            erl_abs_return:then(BaseM, erl_abs_return:return([]));
        false ->
            erl_abs_return:bind(
              BaseM, 
              fun(H1) -> 
                      erl_abs_return:bind(
                        list_of(T, Validator, Attrs),
                        fun(T1) ->
                                erl_abs_return:return([H1|T1])
                        end)
              end)
    end;
list_of([], _Validator, _Attrs) ->
    erl_abs_return:return([]);
list_of(_Other, _Validator, _Attrs) ->
    false.

paired(Value, undefined, IsEmpty, #{key := Key} = Attrs) ->
    ReverseKey = list_to_atom("no_" ++ atom_to_list(Key)),
    paired(Value, ReverseKey, IsEmpty, Attrs);
paired(_Value, ReverseKey, true, #{data := Data}) when is_atom(ReverseKey) ->
    case maps:find(ReverseKey, Data) of
        {ok, RValue} ->
            is_boolean(RValue);
        error ->
            false
    end;
paired(Value, ReverseKey, _IsEmpty, #{}) when is_atom(ReverseKey) ->
    is_boolean(Value);
paired(_Value, _ReverseKey, _IsEmpty, _Attrs) ->
    {error, invalid_validator_arg}.

default(_Value, Default, true, #{}) ->
    {ok, Default};
default(Value, _Default, false, #{}) ->
    {ok, Value}.

default_key(Value, DefaultKey, true, #{validated_data := ValidatedData}) ->
    DefaultValue = maps:get(DefaultKey, ValidatedData, Value),
    {ok, DefaultValue};
default_key(Value, _DefaultKey, false, #{}) ->
    {ok, Value}.

required(_Value, _Args, true, #{}) ->
    {error, required};
required(Value, _Args, false, #{}) ->
    {ok, Value}.

'or'(Value, Validators, IsKey, Attr) ->
    'or'(Value, Validators, IsKey, Attr, Validators).

'or'(Value, [Validator|T], IsKey, Attr, Validators) ->
    case validate_value(Validator, Value, Attr) of
        #{?STRUCT_KEY := ?RETURN_OK} = ReturnOk ->
            ReturnOk;
        #{?STRUCT_KEY := ?RETURN_FAIL} ->
            'or'(Value, T, IsKey, Attr, Validators)
    end;
'or'(_Value, [], _IsKey, _Attr, Validators) ->
    {error, {all_validator_failed, Validators}}.
