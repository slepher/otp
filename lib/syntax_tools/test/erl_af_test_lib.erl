%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Feb 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_af_test_lib).

-include("compile_opts.hrl").
%% API
-export([get_baseline/2, realize_with_baseline/2, test_module_forms/2, compile_test_module/2, compile_test_forms/1]).

%%%===================================================================
%%% API
%%%===================================================================
get_baseline(Mark, Forms) ->
    case erl_af_lib:with_attribute(
            fun(Mark1, _Acc, #{line := Line}) when Mark == Mark1 ->
                    Line;
               (_, Acc, #{}) ->
                    Acc
            end, undefined, Forms, baseline, #{formatter => ?MODULE, simplify_return => true}) of
        undefined ->
            Msg = io_lib:format("attribute -baseline(~p) expected", [Mark]),
            io:format("~s", [Msg]),
            exit(list_to_binary(Msg));
        Baseline when is_integer(Baseline) ->
            Baseline
    end.

realize_with_baseline(Baseline, ErrorStruct) ->
    ErrorStruct1 =
        erl_af_error:with_all_formatted_failure(
            fun({Line, Formatter, Error}) when is_integer(Line) ->
                    {Line - Baseline, Formatter, Error};
               (Error) ->
                    Error
            end, ErrorStruct),
    erl_af_error:realize(ErrorStruct1).

compile_test_module(Module, Config) ->
    Forms = test_module_forms(Module, Config),
    Opts = compile_opts(),
    erl_af_lib:load_forms(Forms, Opts).

compile_test_forms(Forms) ->
    Opts = compile_opts(),
    Opts1 = Opts -- [report_warnings, report_errors] ++ [return_warnings],
    erl_af_lib:load_forms(Forms, Opts1).

test_module_forms(Module, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, atom_to_list(Module) ++ ".erl"),
    Opts = compile_opts(),
    case erl_af_lib:parse_file(File, Opts) of
        {error, Errors, []} ->
            exit({compile_module_failed, Errors});
        Forms ->
            Forms
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
