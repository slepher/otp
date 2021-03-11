%% should not use outside.
-ifndef(ERL_ABS_STRUCT_NAME).
-define(ERL_ABS_STRUCT_NAME, true).
%% for '__struct__' in map
-define(STRUCT_KEY, '__struct__').
%% for erl_abs_walk_return.
-define(WALK_RETURN, erl_abs_walk_return).
%% for endo
-define(ENDO, erl_abs_endo).
%% for erl_abs_return.
-define(RETURN_OK, erl_abs_return_ok).
-define(RETURN_FAIL, erl_abs_return_fail).
%% for erl_abs_traverse_m.
-define(TRAVERSE_M, erl_abs_traverse_m).
%% for erl_abs_error.
-define(ERROR_STATE, erl_abs_error).
%% for erl_abs_error_ctx.
-define(ERROR_CTX, erl_abs_error_ctx).
-endif.
