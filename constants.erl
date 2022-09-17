-module(constants).
-export([get_prefix_constant/0]).
-define(PREFIX_CONSTANT, "qwerty123!").

get_prefix_constant() ->
    ?PREFIX_CONSTANT.