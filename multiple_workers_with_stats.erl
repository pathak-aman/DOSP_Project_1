-module(multiple_workers_with_stats).
-import(string,[len/1,find/2]).
-import(generating_hash, [generate_hash/1]).
-import(constants, [get_prefix_constant/0]).
-export([start_ping/1, start_pong/0,  ping/5, pong/2]).
-compile(export_all).

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
            Success = true,
            true;
        false ->
            Success = false
        end,
    Success.


for(_K, Pong_Node, _Num, Num_Coins_Found, 0, Workers, Prob) ->
    io:format("The process ~p mined ~p Bitcoins!~n", [self(), Num_Coins_Found]),
    {pong, Pong_Node} ! {self(), Num_Coins_Found, Workers,Pong_Node, Prob};


for (K, Pong_Node, Start, Num_Coins_Found, Subproblems, Workers, Prob) ->
    Success = mine(K, Pong_Node, Start),
    case Success of
        true -> 
            {pong, Pong_Node} ! add,
            for(K, Pong_Node, Start-1, Num_Coins_Found+1, Subproblems - 1, Workers,Prob);
        false ->
            for(K, Pong_Node, Start-1, Num_Coins_Found, Subproblems - 1, Workers,Prob)
        end,
    Success.

ping(Current_Worker, Subproblems, K, Pong_Node, Workers) ->
    Start = Current_Worker*Subproblems - Subproblems + 1, 
    for(K, Pong_Node, Start, 0, Subproblems, Workers, Subproblems).

pong(Coin_Counter, Worker_Counter) ->
    receive        
        {StringToHashed, Hash, Worker_PID} ->
            io:format("Match Found at ~p!\tString: ~p\tSHA256: ~p~n", [Worker_PID, StringToHashed, Hash]),
            pong(Coin_Counter, Worker_Counter);
        
        add ->
            counters:add(Coin_Counter, 1, 1),
            pong(Coin_Counter, Worker_Counter);


        {Worker_PID, Num_Coins_Found, Workers, _Pong_Node, Prob} -> 
            io:format(">>>>>>>>>>>>>>>> \tThe worker process ~p mined ~p Bitcoins!~n", [Worker_PID, Num_Coins_Found]),
            counters:add(Worker_Counter,1,1),
            case counters:get(Worker_Counter,1) == Workers of
                true ->
                    Mined = counters:get(Coin_Counter,1),
                    Tot_String = Workers*Prob,
                    {_,Time_Since_Last_Call} = statistics(runtime),
                    
                    {_,Real_Time_Since_Last_Call}  = statistics(wall_clock),
                    Time_in_microseconds = Time_Since_Last_Call * 1000,

                    io:format("~n-------------------------------------------------------~nFinal Stats~n-------------------------------------------------------~nTotal Bitcoin Mined: ~p~nTotal Workers: ~p~nTotal String Checked: ~p~nSuccess Rate: ~p~n~n~n", [Mined, Workers,Tot_String, Mined/Tot_String]),
                    io:format("Total CPU time spent : ~p~n", [Time_in_microseconds]),
                    io:format("Total Real time spent : ~p~n", [Real_Time_Since_Last_Call*1000]);
                false ->
                    pong(Coin_Counter, Worker_Counter)
            end
    end.

loop (0, _Subproblems, _K, _Pong_Node, _Workers) ->
    io:format(">>>>>>>>> All workers have been spawned!~nWait untill the server displays the final stats of mining!~n");
    

loop (Num_Workers, Subproblems, K, Pong_Node, Workers) ->
    spawn(multiple_workers_with_stats, ping, [Num_Workers, Subproblems, K, Pong_Node, Workers]),
    loop(Num_Workers-1, Subproblems, K, Pong_Node,Workers).


start_pong() ->
    Coin_Counter = counters:new(1, [write_concurrency]),
    Worker_Counter = counters:new(1, [write_concurrency]),
    register(pong, spawn(multiple_workers_with_stats, pong, [Coin_Counter, Worker_Counter])).

start_ping(Pong_Node) ->
    {ok, [K]} = io:fread("Enter K:", "~d"),
    {ok, [Num_Workers]} = io:fread("Enter Number of workers:", "~d"),
    {ok, [Subproblems]} = io:fread("Enter Number of sub-problems a single worker handles:", "~d"),
    {A,_} = timer:tc(?MODULE, loop,[Num_Workers, Subproblems, K, Pong_Node,Num_Workers]),
    statistics(runtime),
    statistics(wall_clock),
    %{_,Time_Since_Last_Call} = statistics(runtime),
    %{_,Real_Time_Since_Last_Call}  = statistics(wall_clock),
    %Time_in_microseconds = Time_Since_Last_Call * 1000,
    %io:format("Total Real time spent : ~p~p~n", [A*1000,Real_Time_Since_Last_Call]),
    %io:format("Total CPU time spent : ~p~n", [Time_in_microseconds]),
    io:format("Check your mined coins on the server!~n").