%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%% add compile_opt() which returns the compile options when compile that file
%%% helps to compile file in test directory data_dir *_SUITE_data
%%% @end
%%% Created : 19 Feb 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_af_compile_opts_transform).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(BaseForms, Opts) ->
    OptsAst = erl_af_lib:abstract_form(Opts),
    CompileOptsForms = erl_af_lib:gen_exported_function(compile_opts, OptsAst),
    erl_af:insert_forms(CompileOptsForms, BaseForms).

format_error(Error) ->
    erl_af:format_error(Error).
