%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Feb 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_abs_sample_transformer_1).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    Return = 
        erl_abs_monad:lift_m(
            fun(_) -> Forms end,
            erl_abs_lib:with_attribute(
                fun(mark_error_0, _Acc) ->
                       {warning, error_0};
                   (mark_0, _Acc) ->
                       {warning, warning_0};
                   (_Attr, Acc) ->
                       Acc
                end, ok, Forms, mark, #{formatter => ?MODULE})),
    erl_abs_return:to_compiler(Return).

format_error(error_0) ->
    io_lib:format("get error 0", []);
format_error(warning_0) ->
    io_lib:format("get warning 0", []);
format_error(Error) ->
    erl_abs:format_error(Error).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
