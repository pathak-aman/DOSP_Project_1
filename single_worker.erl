-module(single_worker).
-import(string,[len/1,find/2]).
-import(generating_hash, [generate_hash/1]).
-import(constants, [get_prefix_constant/0]).
-export([start_ping/1, start_pong/0,  ping/2, pong/0]).
-define(workers, 5).


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


for (K, Pong_Node, Num,Times) ->
    mine(K, Pong_Node, Num),
    for(K, Pong_Node, Num-1, Times - 1).

ping(K, Pong_Node) ->
    for(K, Pong_Node, 100, 100).

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

start_pong() ->
    register(pong, spawn(single_worker, pong, [])).

start_ping(Pong_Node) ->
    {ok, [K]} = io:fread("Enter K:", "~d"),
    spawn(single_worker, ping, [K, Pong_Node]).