-module(main).
-export([start/0]).
-import(generating_hash, [generate_hash/1]).

start() ->
    {ok,K} = io:read("Enter K: "),
    io:format("K = : ~s~n", [integer_to_list(K)]),
    Answer = string:strip(io:get_line("Enter string to be hashed: "), right, $\n),
    generating_hash:generate_hash(Answer).