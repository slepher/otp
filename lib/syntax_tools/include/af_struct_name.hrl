%% should not use outside.
-ifndef(ERL_AF_STRUCT_NAME).
-define(ERL_AF_STRUCT_NAME, true).
%% for '__struct__' in map
-define(STRUCT_KEY, '__struct__').
%% for erl_af_walk_return.
-define(WALK_RETURN, erl_af_walk_return).
%% for endo
-define(ENDO, erl_af_endo).
%% for erl_af_return.
-define(RETURN_OK, erl_af_return_ok).
-define(RETURN_FAIL, erl_af_return_fail).
%% for erl_af_traverse_m.
-define(TRAVERSE_M, erl_af_traverse_m).
%% for erl_af_error.
-define(ERROR_STATE, erl_af_error).
%% for erl_af_error_ctx.
-define(ERROR_CTX, erl_af_error_ctx).
-endif.
