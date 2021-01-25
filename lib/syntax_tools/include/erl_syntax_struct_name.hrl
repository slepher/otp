%% just used in astranaut project, should not use outside.
-ifndef(ERL_SYNTAX_STRUCT_NAME).
-define(ERL_SYNTAX_STRUCT_NAME, true).
%% for '__struct__' in map
-define(STRUCT_KEY, '__struct__').
%% for erl_syntax_walk_return.
-define(WALK_RETURN, erl_syntax_walk_return).
%% for endo
-define(ENDO, erl_syntax_endo).
%% for erl_syntax_base_m.
-define(BASE_M, erl_syntax_base_m).
%% for erl_syntax_return_m.
-define(RETURN_OK, erl_syntax_return_ok).
-define(RETURN_FAIL, erl_syntax_return_fail).
%% for erl_syntax_traverse_m.
-define(TRAVERSE_M, erl_syntax_traverse_m).
%% for erl_syntax_error_state.
-define(ERROR_STATE, erl_syntax_error_state).
%% for erl_syntax_error_ctx.
-define(ERROR_CTX, erl_syntax_error_ctx).
-endif.
