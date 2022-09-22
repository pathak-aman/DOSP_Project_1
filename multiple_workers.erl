-module(multiple_workers).
-import(string,[len/1,find/2]).
-import(generating_hash, [generate_hash/1]).
-import(constants, [get_prefix_constant/0]).
-export([start_ping/1, start_pong/0,  ping/4, pong/0]).


is_leading_zero_K(Hashed_string, K) when K > 0 ->
    % io:format("hashed string is: ~p~n", [Hashed_string]),
    Result = case K =< len(Hashed_string) of
     true ->
        A = "0",
     Prefix = lists:concat(lists:duplicate(K, A)),
    %  io:format("prefix is: ~p~n", [Prefix]),
     string:find(Hashed_string, Prefix) =:= Hashed_string;
    false -> false
    end,
    Result.

mine(K, Pong_Node, Num) ->
    Hash = generate_hash(integer_to_list(Num)),
    case is_leading_zero_K(Hash, K) of 
        true -> 
            StringToHashed = string:concat(get_prefix_constant(), integer_to_list(Num)),
            {pong, Pong_Node} ! {StringToHashed, Hash, self()},
            Success = 1,
            true;
        false ->
            Success = 0
        end,
    Success.


for(K, Pong_Node, Num, 0) ->
    {pong, Pong_Node} ! finished,
    io:format("~p~n is now dead", [self()]);


for (K, Pong_Node, Start, Subproblems) ->
    mine(K, Pong_Node, Start),
    for(K, Pong_Node, Start-1, Subproblems - 1).

ping(Current_Worker, Subproblems, K, Pong_Node) ->
    Start = Current_Worker*Subproblems - Subproblems + 1, 
    for(K, Pong_Node, Start, Subproblems).

pong() ->
    receive
        finished ->
            io:format("Stats finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong();
        {StringToHashed, Hash, Worker_PID} ->
            io:format("~nMatch Found at ~p!\tString: ~p\tSHA256: ~p~n~n", [Worker_PID, StringToHashed, Hash]),
            pong()

    end.

loop (0, Subproblems, K, Pong_Node) ->
    io:format(">>>>>>>>> All workers have been spawned!");

loop (Num_Workers, Subproblems, K, Pong_Node) ->
    spawn(multiple_workers, ping, [Num_Workers, Subproblems, K, Pong_Node]),
    loop(Num_Workers-1, Subproblems, K, Pong_Node).


start_pong() ->
    register(pong, spawn(single_worker, pong, [])).

start_ping(Pong_Node) ->
    {ok, [K]} = io:fread("Enter K:", "~d"),
    {ok, [Num_Workers]} = io:fread("Enter Number of workers:", "~d"),
    {ok, [Subproblems]} = io:fread("Enter Number of sub-problems a single worker handles:", "~d"),
    loop(Num_Workers, Subproblems, K, Pong_Node),
    io:format("Check your mined coins on the server!").
    % spawn(single_worker, ping, [K, Pong_Node]).