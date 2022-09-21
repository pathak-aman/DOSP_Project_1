-module(constants).
-export([get_prefix_constant/0]).
-define(PREFIX_CONSTANT, "adobra;kjsdfk").

get_prefix_constant() ->
    ?PREFIX_CONSTANT.