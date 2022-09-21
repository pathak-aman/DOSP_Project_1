-module(test).
-compile(export_all).
-import(string,[len/1,find/2]).
-import(generating_hash, [generate_hash/1]).
-import(constants, [get_prefix_constant/0]).

% Contols how many strings will one worker handle
-define(factor, 100000).
% controls number of workers
-define(workers, 30).

% Matches the number of leading number with K
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

%% Worker process:
%% 
% Core logic for the worker node.
% Logic:
%   Generate random string
%   Finds Hash
%   Calls is_leading_zero_K
%   Returns result
% Success keeps track if r
 hasher(X) ->
    {Num, K} = X,
    Hash = generate_hash(integer_to_list(Num)),
    case is_leading_zero_K(Hash, K) of 
        true -> 
            Final_String = string:concat(get_prefix_constant(), integer_to_list(Num)),
            io:format("~nMatch Found!\tString: ~p\tSHA256: ~p~n~n", [Final_String, Hash]),
            Success = 1,
            true;
        false ->
            Success = 0
        end,
    Success.
    


%% Returns the results of a worker
say(From, N, 0, K, Num_Coins_Found,Num_Coins_Searched) ->
    % io:format("4"),
    From ! [K, N,self(), Num_Coins_Found, Num_Coins_Searched];

%% Feeds the workers    
say(From, N, B, K, 0, 0) ->
    Items = lists:seq(N*?factor - ?factor + 1, N*?factor),
    % io:format("~p~n", [Items]),
    Ks = lists:duplicate(?factor, K),
    Zipped = lists:zip(Items, Ks),
    Response = lists:map(fun hasher/1,Zipped),
    Num_Coins_Found = lists:sum(Response),
    Num_Coins_Searched = ?factor,
    say(From, N-1, B-1, K, Num_Coins_Found, Num_Coins_Searched).


%%  Main process:
loop(N) ->
    % io:format("1"),
    {ok, [K]} = io:fread("Enter K: ", "~d"),
    
    loop(N, N, K).

loop(0, Times, _K) ->
    % io:format("2b"),
    display_results(Times);

loop(N, Times, K) ->
    % io:format("~p",[?K]),
    spawn(test, say, [self(), N, 1, K, 0, 0]),
    % io:format("2c"),
    loop(N-1, Times, K).
 
display_results(0) -> 
    % io:format("7"),
    ok;
display_results(Times) ->
    receive
        Result ->
            % io:format("5"),
            io:format(">>>>>>>>>>>>>> K, Worker#, ProcessID, #Coins_Found, #Coins_Searched : ~w~n", [Result])
    end,
    % io:format("6"),
    display_results(Times-1).

%%  Test:
run() ->
    io:format("---------------------------------------------------~n"),
    io:format("Num of workers: ~p~nNumber of string a single worker works on :~p~n", [?workers,?factor]),
    io:format("---------------------------------------------------~n"),
    loop(?workers).