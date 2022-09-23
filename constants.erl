-module(constants).
-export([get_prefix_constant/0]).
-define(PREFIX_CONSTANT, "dipali_aman;").

get_prefix_constant() ->
    ?PREFIX_CONSTANT.