%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%% struct of error created by {@link erl_af:mapfold/4}.
%%% @end
%%% Created :  9 Jul 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_af_error).

-include("af_struct_name.hrl").

-export_type([struct/0]).
-export_type([compiler_error/0]).

-type struct() :: #{'__struct__' => ?MODULE,
                    file => compile_file(),
                    errors => erl_af_endo:endo(term()),
                    warnings => erl_af_endo:endo(term()),
                    formatted_errors => erl_af_endo:endo(erl_parse:error_info()),
                    formatted_warnings => erl_af_endo:endo(erl_parse:error_info()),
                    file_errors => #{file:filename() => erl_af_endo:endo(erl_parse:error_info())},
                    file_warnings => #{file:filename() => erl_af_endo:endo(erl_parse:error_info())}
                   }.

-type compile_file() :: file:filename() | undefined.
-type compiler_error() :: [{file:filename(), [erl_parse:error_info()]}].

%% API
-export([new/0, new/1, update_line/3, update_file/2, eof/1, merge/2]).
-export([realize/1, printable/1, no_pending/1, is_empty/1, is_empty_error/1]).
%% get memebers from erl_af_error:struct().
-export([file/1, errors/1, warnings/1]).
-export([formatted_errors/1, formatted_warnings/1]).
-export([file_errors/1, file_warnings/1]).
-export([with_error/2, with_warning/2, with_failure/2]).
-export([with_formatted_error/2, with_formatted_warning/2, with_formatted_failure/2]).
-export([with_formatted_base_error/2, with_formatted_base_warning/2, with_formatted_base_failure/2]).
-export([with_file_errors/2, with_file_warnings/2, with_file_failures/2]).
-export([with_file_formatted_error/2, with_file_formatted_warning/2, with_file_formatted_failure/2]).
-export([with_file_base_error/2, with_file_base_warning/2, with_file_base_failure/2]).
-export([with_all_error/2, with_all_warning/2, with_all_failure/2]).
-export([with_all_formatted_error/2, with_all_formatted_warning/2, with_all_formatted_failure/2]).
-export([with_formatted_base/2, with_file_formatted/2]).

%% append errors to erl_af_error:struct().
-export([append_ews/3, append_error/2, append_warning/2, append_errors/2, append_warnings/2]).
-export([append_formatted_errors/2, append_formatted_warnings/2]).
-export([append_file_errors/2, append_file_warnings/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% =====================================================================
%% @spec new() -> struct()
%% @doc initialize empty struct().
-spec new() -> struct().
new() ->
    new(undefined).

%% =====================================================================
%% @spec new(compile_file()) -> struct()
%% @doc initialize struct() with File.
-spec new(compile_file()) -> struct().
new(File) ->
    #{?STRUCT_KEY => ?MODULE, file => File,
      errors => erl_af_endo:empty(), warnings => erl_af_endo:empty(),
      formatted_errors => erl_af_endo:empty(), formatted_warnings => erl_af_endo:empty(),
      file_errors => #{}, file_warnings => #{}}.

%% =====================================================================
%% @spec update_line(erl_anno:line(), module(), struct()) -> struct()
%% @doc update struct with line number and formatter, this will convert all errors and warnings to formatted_errors and formatted_warnings.
-spec update_line(erl_anno:line(), module(), struct()) -> struct().
update_line(Line, Formatter, #{?STRUCT_KEY := ?MODULE,
                               errors := Errors0, warnings := Warnings0,
                               formatted_errors := FormattedErrors0,
                               formatted_warnings := FormattedWarnings0} = Struct) ->
    FormattedErrors1 = format_errors(Line, Formatter, Errors0),
    FormattedWarnings1 = format_errors(Line, Formatter, Warnings0),
    FormattedErrors2 = erl_af_endo:append(FormattedErrors0, FormattedErrors1),
    FormattedWarnings2 = erl_af_endo:append(FormattedWarnings0, FormattedWarnings1),
    Struct#{formatted_errors => FormattedErrors2, formatted_warnings => FormattedWarnings2,
            errors => erl_af_endo:empty(), warnings => erl_af_endo:empty()}.

%% =====================================================================
%% @spec update_file(compile_file(), struct()) -> struct()
%% @doc update struct file, if file changed, will convert all formatted_errors and formatted_warnings to file_errors and file_warnings with file before, then update file.
-spec update_file(compile_file() | eof, struct()) -> struct().
update_file(File, #{?STRUCT_KEY := ?ERROR_STATE, file := undefined} = Struct) ->
    update_file(File, File, Struct);
update_file(File, #{?STRUCT_KEY := ?ERROR_STATE, file := File0} = Struct) ->
    update_file(File0, File, Struct).

%% =====================================================================
%% @spec eof(struct()) -> struct()
%% @doc update file to eof
%% @see update_file/2
-spec eof(struct()) -> struct().
eof(Struct) ->
    update_file(eof, Struct).

%% =====================================================================
%% @spec no_pending(Struct::struct()) -> boolean()
%% @doc check if errors and warnings is empty, just errors and warnings, not formatted_* and file_*.
-spec no_pending(struct()) -> boolean().
no_pending(#{?STRUCT_KEY := ?ERROR_STATE, errors := Errors, warnings := Warnings}) ->
    erl_af_endo:is_empty(Errors) and erl_af_endo:is_empty(Warnings).

%% =====================================================================
%% @spec is_empty(Struct::struct()) -> boolean()
%% @doc check if all errors and warnings is empty, include formatted_* and file_*.
-spec is_empty(struct()) -> boolean().
is_empty(#{?STRUCT_KEY := ?ERROR_STATE,
           errors := Errors, warnings := Warnings,
           formatted_errors := FormattedErrors, formatted_warnings := FormattedWarnings,
           file_errors := FErrors, file_warnings := FWarnings}) 
  when (map_size(FErrors) == 0) and (map_size(FWarnings) == 0) ->
    erl_af_endo:is_empty(Errors) and erl_af_endo:is_empty(Warnings)
    and erl_af_endo:is_empty(FormattedErrors) and erl_af_endo:is_empty(FormattedWarnings);
is_empty(_ErrorState) ->
    false.

%% =====================================================================
%% @spec is_empty_error(Struct::struct()) -> boolean()
%% @doc check if errors, formatted_errors, file_errors is empty.
-spec is_empty_error(struct()) -> boolean().
is_empty_error(#{errors := Errors,
                 formatted_errors := FormattedErrors,
                 file_errors := FErrors})
  when (map_size(FErrors) == 0) ->
    erl_af_endo:is_empty(Errors) and erl_af_endo:is_empty(FormattedErrors);
is_empty_error(_Struct) ->
    false.

%% =====================================================================
%% @spec realize(Struct::struct()) -> {compiler_error(), compiler_error()}
%% @doc get file_errors and file_warnings, which could be used by compiler.
-spec realize(struct()) -> {compiler_error(), compiler_error()}.
realize(#{?STRUCT_KEY := ?MODULE, file_errors := FileErrors, file_warnings := FileWarnings}) ->
    {realize_errors(FileErrors), realize_errors(FileWarnings)}.

printable(#{?STRUCT_KEY := ?MODULE} = Struct) ->
    maps:fold(
        fun(Key, FileErrors, Acc) when (Key == file_errors) or (Key == file_warnings) ->
            case maps:size(FileErrors) of
                0 ->
                    Acc;
                _ ->
                    FileErrors1 = maps:map(fun(_Key, Value) -> erl_af_endo:run(Value) end, FileErrors),
                    maps:put(Key, FileErrors1, Acc)
            end;
           (Key, Errors, Acc) when (Key == formatted_errors) or (Key == formatted_warnings)
                                   or (Key == errors) or (Key == warnings) ->
               case erl_af_endo:is_empty(Errors) of
                    true ->
                       Acc;
                    false ->
                        maps:put(Key, erl_af_endo:run(Errors), Acc)
                end;
           (_Key, _Value, Acc) ->
               Acc
        end, #{}, Struct).


%% =====================================================================
%% @spec merge(struct(), struct()) -> struct()
%% @doc merge two erl_af_error struct into one, first update file of Struct1 to file of Struct2, then merge all errors.
-spec merge(struct(), struct()) -> struct().
merge(#{?STRUCT_KEY := ?MODULE} = Struct1, #{?STRUCT_KEY := ?MODULE} = Struct2) ->
    Struct3 = merge_file(Struct1, Struct2),
    Struct4 = merge_file_ews(Struct3, Struct2),
    Struct5 = merge_formatted_ews(Struct4, Struct2),
    Struct6 = merge_ews(Struct5, Struct2),
    Struct6.

-spec file(struct()) -> compile_file().
file(#{?STRUCT_KEY := ?MODULE, file := File}) ->
    File.

%% =====================================================================
%% @spec errors(Struct::struct()) -> [term()]
%% @doc get errors.
-spec errors(struct()) -> [term()].
errors(#{errors := Errors}) ->
    erl_af_endo:run(Errors).

%% =====================================================================
%% @spec warnings(Struct::struct()) -> [term()]
%% @doc get warnings.
-spec warnings(struct()) -> [term()].
warnings(#{warnings := Warnings}) ->
    erl_af_endo:run(Warnings).

%% =====================================================================
%% @spec formatted_errors(Struct::struct()) -> [term()]
%% @doc get formatted_errors.
-spec formatted_errors(struct()) -> [erl_parse:parse_struct()].
formatted_errors(#{formatted_errors := Errors}) ->
    erl_af_endo:run(Errors).

%% =====================================================================
%% @spec formatted_warnings(Struct::struct()) -> [term()]
%% @doc get formatted_warnings.
-spec formatted_warnings(struct()) -> [erl_parse:parse_struct()].
formatted_warnings(#{formatted_warnings := Warnings}) ->
    erl_af_endo:run(Warnings).

%% =====================================================================
%% @spec file_errors(Struct::struct()) -> [term()]
%% @doc get file_errors.
-spec file_errors(struct()) -> compiler_error().
file_errors(#{file_errors := FileErrors}) ->
   realize_errors(FileErrors).

%% =====================================================================
%% @spec file_warnings(struct()) -> [term()]
%% @doc get file_warnings.
-spec file_warnings(Struct::struct()) -> compiler_error().
file_warnings(#{file_warnings := FileWarnings}) ->
    realize_errors(FileWarnings).

with_error(Fun, Struct) ->
   with_key(Fun, errors, fun erl_af_endo:map/2, Struct).

with_warning(Fun, Struct) ->
   with_key(Fun, warnings, fun erl_af_endo:map/2, Struct).

with_failure(Fun, Struct) ->
   erl_af_monad:sequence_withs(Fun, [fun with_error/2, fun with_warning/2], Struct).

with_formatted_error(Fun, Struct) ->
   with_key(Fun, formatted_errors, fun erl_af_endo:map/2, Struct).

with_formatted_warning(Fun, Struct) ->
   with_key(Fun, formatted_warnings, fun erl_af_endo:map/2, Struct).

with_formatted_failure(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_formatted_error/2, fun with_formatted_warning/2], Struct).

with_formatted_base_error(Fun, Struct) ->
    with_key(Fun, formatted_errors, fun with_formatted_base/2, Struct).

with_formatted_base_warning(Fun, Struct) ->
    with_key(Fun, formatted_warnings, fun with_formatted_base/2, Struct).

with_formatted_base_failure(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_formatted_base_error/2, fun with_formatted_base_warning/2], Struct).

with_formatted(Fun, {Line, Formatter, Error}) ->
    {Line, Formatter, Fun(Error)}.

with_formatted_base(Fun, FormattedErrors) ->
    erl_af_monad:nested_withs(Fun, [fun with_formatted/2, fun erl_af_endo:map/2], FormattedErrors).

with_file_errors(Fun, Struct) ->
    with_key(Fun, file_errors, fun with_file/2, Struct).

with_file_warnings(Fun, Struct) ->
    with_key(Fun, file_warnings, fun with_file/2, Struct).

with_file_failures(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_file_errors/2, fun with_file_warnings/2], Struct).

with_file_formatted_error(Fun, Struct) ->
    with_key(Fun, file_errors, fun with_file_formatted/2, Struct).

with_file_formatted_warning(Fun, Struct) ->
    with_key(Fun, file_warnings, fun with_file_formatted/2, Struct).

with_file_formatted_failure(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_file_formatted_error/2, fun with_file_formatted_warning/2], Struct).

with_file_base_error(Fun, Struct) ->
    with_key(Fun, file_errors, fun with_file_base/2, Struct).

with_file_base_warning(Fun, Struct) ->
    with_key(Fun, file_warnings, fun with_file_base/2, Struct).

with_file_base_failure(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_file_base_error/2, fun with_file_base_warning/2], Struct).

with_file(Fun, FileErrors) ->
    maps:from_list(lists:map(Fun, maps:to_list(FileErrors))).

with_file_formatted(Fun, FileErrors) ->
    erl_af_monad:nested_withs(Fun, [fun erl_af_endo:map/2, fun with_map_value/2], FileErrors).

with_file_base(Fun, FileErrors) ->
    erl_af_monad:nested_withs(Fun, [fun with_formatted/2, fun with_file_formatted/2], FileErrors).

with_all_error(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_error/2, fun with_formatted_base_error/2, fun with_file_base_error/2], Struct).

with_all_warning(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_warning/2, fun with_formatted_base_warning/2, fun with_file_base_warning/2], Struct).

with_all_failure(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_all_error/2, fun with_all_warning/2], Struct).

with_all_formatted_error(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_formatted_error/2, fun with_file_formatted_error/2], Struct).

with_all_formatted_warning(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_formatted_warning/2, fun with_file_formatted_warning/2], Struct).

with_all_formatted_failure(Fun, Struct) ->
    erl_af_monad:sequence_withs(Fun, [fun with_all_formatted_error/2, fun with_all_formatted_warning/2], Struct).

with_map_value(Fun, Map) ->
    maps:map(
        fun(_Key, Value) ->
            Fun(Value)
        end, Map).

with_key(Fun, Key, With, #{?STRUCT_KEY := ?MODULE} = Struct) ->
    Errors = maps:get(Key, Struct),
    Errors1 = With(Fun, Errors),
    Struct#{Key => Errors1}.

%% =====================================================================
%% @spec append_ews(erl_af_endo:list_or_endo(term()), erl_af_endo:list_or_endo(term()), struct()) -> struct()
%% @doc append errors and warnings to struct.
-spec append_ews(erl_af_endo:list_or_endo(term()), erl_af_endo:list_or_endo(term()), struct()) -> struct().
append_ews(Errors, Warnings, #{} = Struct) ->
    Struct1 = append_errors(Errors, Struct),
    Struct2 = append_warnings(Warnings, Struct1),
    Struct2.

%% =====================================================================
%% @spec append_error(term(), struct()) -> struct()
%% @doc append an error to struct.
-spec append_error(term(), struct()) -> struct().
append_error(Error, Struct) ->
    append_errors([Error], Struct).

%% =====================================================================
%% @spec append_warning(term(), struct()) -> struct()
%% @doc append a warning to struct.
-spec append_warning(term(), struct()) -> struct().
append_warning(Warning, Struct) ->
    append_warnings([Warning], Struct).

%% =====================================================================
%% @spec append_errors(erl_af_endo:list_or_endo(term()), struct()) -> struct()
%% @doc append errors to struct.
-spec append_errors(erl_af_endo:list_or_endo(term()), struct()) -> struct().
append_errors(Errors, #{errors := Errors0} = Struct) ->
    Errors1 = append(Errors0, Errors),
    Struct#{errors => Errors1}.

%% =====================================================================
%% @spec append_warnings(erl_af_endo:list_or_endo(term()), struct()) -> struct()
%% @doc append warnings to struct.
-spec append_warnings(erl_af_endo:list_or_endo(term()), struct()) -> struct().
append_warnings(Warnings, #{warnings := Warnings0} = Struct) ->
    Warnings1 = append(Warnings0, Warnings),
    Struct#{warnings => Warnings1}.

%% =====================================================================
%% @spec append_formatted_errors(erl_af_endo:list_or_endo(erl_parse:parse_error()), struct()) -> struct()
%% @doc append formatted_errors to struct.
-spec append_formatted_errors(erl_af_endo:list_or_endo(erl_parse:parse_error()), struct()) -> struct().
append_formatted_errors(Errors, #{formatted_errors := Errors0} = Struct) ->
    Errors1 = append(Errors0, Errors),
    Struct#{formatted_errors => Errors1}.

%% =====================================================================
%% @spec append_formatted_warnings(erl_af_endo:list_or_endo(erl_parse:parse_error()), struct()) -> struct()
%% @doc append formatted_warnings to struct.
-spec append_formatted_warnings(erl_af_endo:list_or_endo(erl_parse:parse_error()), struct()) -> struct().
append_formatted_warnings(Warnings, #{formatted_warnings := Warnings0} = Struct) ->
    Warnings1 = append(Warnings0, Warnings),
    Struct#{formatted_warnings => Warnings1}.

-spec append_file_errors([compiler_error()], struct()) -> struct().
append_file_errors(FileErrors, #{file_errors := FileErrors1} = Struct) ->
    FileErrors2 = from_compiler_errors(FileErrors),
    FileErrors3 = merge_file_errors(FileErrors1, FileErrors2),
    Struct#{file_errors => FileErrors3}.

-spec append_file_warnings([compiler_error()], struct()) -> struct().
append_file_warnings(FileWarnings, #{file_warnings := FileWarnings1} = Struct) ->
    FileWarnings2 = from_compiler_errors(FileWarnings),
    FileWarnings3 = merge_file_errors(FileWarnings1, FileWarnings2),
    Struct#{file_warnings => FileWarnings3}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

realize_errors(FileErrors) ->
    lists:map(fun({File, Errors}) -> {File, erl_af_endo:run(Errors)} end, maps:to_list(FileErrors)).

update_file(undefined, eof, #{formatted_errors := Errors, formatted_warnings := Warnings} = Struct) ->
    case is_empty(Errors) and is_empty(Warnings) of
        true ->
            Struct;
        false ->
            erlang:error({match_eof_without_file, Struct})
    end;
update_file(File0, File1, #{?STRUCT_KEY := ?ERROR_STATE,
                            formatted_errors := Errors,
                            formatted_warnings := Warnings,
                            file_errors := ErrorsWithFile,
                            file_warnings := WarningsWithFile} = Struct) ->
    case File0 == File1 of
        true ->
            Struct#{file => File1};
        false ->
            File2 = 
                case File1 of
                    eof ->
                        undefined;
                    _ ->
                        File1
                end,
            ErrorsWithFile1 = add_file_errors(File0, Errors, ErrorsWithFile),
            WarningsWithFile1 = add_file_errors(File0, Warnings, WarningsWithFile),
            Struct#{formatted_errors => erl_af_endo:empty(),
                    formatted_warnings => erl_af_endo:empty(),
                    file_errors => ErrorsWithFile1, 
                    file_warnings => WarningsWithFile1,
                    file => File2}
    end.

merge_file(#{file := undefined} = Struct, #{file := File}) ->
    Struct#{file => File};
merge_file(#{} = Struct, #{file := undefined}) ->
    Struct;
merge_file(#{file := File1} = Struct, #{file := File2}) ->
    update_file(File1, File2, Struct).

merge_file_ews(#{file_errors := FileErrors1, file_warnings := FileWarnings1} = Struct,
               #{file_errors := FileErrors2, file_warnings := FileWarnings2}) ->
    FileErrors3 = merge_file_errors(FileErrors1, FileErrors2),
    FileWarnings3 = merge_file_errors(FileWarnings1, FileWarnings2),
    Struct#{file_errors => FileErrors3, file_warnings => FileWarnings3}.

merge_formatted_ews(#{} = Struct1, #{formatted_errors := FormattedErrors, formatted_warnings := FormattedWarnings}) ->
    Struct2 = append_formatted_errors(FormattedErrors, Struct1),
    Struct3 = append_formatted_warnings(FormattedWarnings, Struct2),
    Struct3.

merge_ews(#{} = Struct1, #{errors := Errors, warnings := Warnings}) ->
    append_ews(Errors, Warnings, Struct1).

merge_file_errors(ErrorsWithFile1, ErrorsWithFile2) ->
    maps:fold(
      fun(File, Errors, ErrorsWithFileAcc) ->
              add_file_errors(File, Errors, ErrorsWithFileAcc)
      end, ErrorsWithFile1, ErrorsWithFile2).

 add_file_errors(File, Errors1, ErrorsWithFile) ->
    case erl_af_endo:is_empty(Errors1) of
        true ->
            ErrorsWithFile;
        false ->
            case maps:find(File, ErrorsWithFile) of
                {ok, Errors0} ->
                    maps:put(File, append(Errors0, Errors1), ErrorsWithFile);
                error ->
                    maps:put(File, Errors1, ErrorsWithFile)
            end
    end.

append(Errors0, Errors1) ->
    erl_af_endo:append(Errors0, Errors1).

format_errors(Line, Formatter, Errors) ->
    lists:map(fun(Error) -> {Line, Formatter, Error} end, erl_af_endo:run(Errors)).

from_compiler_errors(CompilerFileErrors) ->
    maps:from_list(
      lists:map(
        fun({File, Errors}) ->
                {File, erl_af_endo:endo(Errors)}
        end, CompilerFileErrors)).