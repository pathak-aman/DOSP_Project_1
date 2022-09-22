-module(system).

% Compiler

-compile([{nowarn_unused_function, [randint/0]}]).

% Exports

-export([start/0]).

% Macros

-define(MASTERS, 2).

-define(WORKERS, 4).

-define(TIMEOUT, 2000).

-define(RANDGEN, system_randgen).

% Records

-record(state, {randgen_pid, masters, workers, master_index=1}).

-record(master, {id, worker_count}).

% Types

-type master_index() :: integer().

%% start/0

%% @doc Starts the worker simulation.

-spec start () -> ok.

start() ->
    io:format("* Initializing~n"),
    Sup = spawn_link(fun () -> supervisor(#state{masters=?MASTERS, workers=?WORKERS}) end),
    Sup ! {self(), start_randgen},
    receive randgen_started -> ok end, % block until randgen is initialized
    Sup ! initialize,
    ok.
    
%% supervisor/1

%% @doc The main application supervisor, in charge of spawning the master processes, which in turn
%% start the worker processes.

-spec supervisor( State :: #state{} ) -> no_return().

supervisor(State) when is_record(State, state) ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', _, master_finished} ->
            NewIndex = State#state.master_index + 1,
            NewState = State#state{master_index=NewIndex},
            start_masters(1, State#state.workers, NewIndex),
            supervisor(NewState);
        {Pid, start_randgen} when is_pid(Pid) ->
            RandgenPid = State#state.randgen_pid,
            case (erlang:is_pid(RandgenPid) andalso erlang:is_process_alive(RandgenPid)) of
                true ->
                    Pid ! randgen_started,
                    supervisor(State);
                false ->
                    io:format("* Starting randgen~n"),
                    RandPid = spawn_link(fun () -> randgen(now()) end), % using now/0 as seed
                    register(?RANDGEN, RandPid),
                    NewState = State#state{randgen_pid=RandPid},
                    Pid ! randgen_started,
                    supervisor(NewState);
                _Other ->
                    supervisor(State)
            end;
        initialize ->
            % [1] already increments index
            NewIndex = start_masters(State#state.masters, State#state.workers, State#state.master_index) - 1, % [1]
            NewState = State#state{master_index=NewIndex}, 
            supervisor(NewState);
        _ ->
            supervisor(State)
    end;
    
supervisor(Other) ->
    {invalid_state, Other}.

%% start_masters/3

%% @doc Starts a given number of master processes.

-spec start_masters( Masters :: integer(), Workers :: integer(), Index :: master_index() ) -> master_index().

start_masters(0, _, LastIndex) ->
    LastIndex;

start_masters(N, W, Index) ->
    io:format("-> [~p] Master started~n", [Index]),
    Pid = spawn_link(fun () -> master(#master{id=Index,worker_count=W}) end),
    Pid ! {start_workers, W},
    start_masters(N-1, W, Index+1).

%% start_workers/2

%% @doc Starts a given number of workers.

-spec start_workers ( Workers :: integer(), MasterIndex :: master_index() ) -> ok.

start_workers(0, _) ->
    ok;

start_workers(N, M) ->
    Pid = spawn(fun () -> worker(M) end),
    erlang:monitor(process, Pid),
    io:format("-> [~p] Started worker ~p~n", [M, Pid]),
    start_workers(N-1, M),
    ok.

%% master/1

%% @doc The master loop. It's in charge of spawing a given number of workers and supervising them.

-spec master ( State :: #master{} ) -> no_return().

master(State) when is_record(State, master) ->
    Workers = State#master.worker_count,
    case Workers of
        0 ->
            io:format("<- [~p] Master going home~n", [State#master.id]),
            exit(master_finished);
        _ ->
            receive
                {start_workers, N} ->
                    start_workers(N, State#master.id),
                    master(State);
                {'DOWN', _, process, _, normal} ->
                    NewState = State#master{worker_count=(Workers-1)},
                    master(NewState);
                {'DOWN', _, process, _, Reason} ->
                    exit(Reason);
                _Other -> master(State)
            end
    end.

% worker/1

%% @doc The worker loop. Runs for a given time and then exits cleanly.

-spec worker ( MasterIndex :: master_index() ) -> no_return().
    
worker(M) ->
    Timeout = randint(?TIMEOUT),
    receive after Timeout ->
        io:format("<- [~p] Worker leaving after ~pms~n", [M, Timeout])
    end.
    
% randint/0

%% @doc Generates a random float from `0.0 .. 1.0'

-spec randint () -> no_return().

randint() ->
    ?RANDGEN ! self(),
    receive
        {randgen, Float} -> Float
    after
        1000 -> {error, randgen_timeout}
    end.

% randint/1

%% @doc Generates a random integer from 1 to N.

-spec randint ( N :: integer() ) -> no_return().

randint(N) ->
    ?RANDGEN ! {self(), N},
    receive
        {randgen, Int} -> Int
    after
        1000 -> {error, randgen_timeout}
    end.
    
% randgen/1

%% @doc The random number generator process loop.
%%
%% Receives messages, allowing a random number from 0 to 1
%% to be passed, or a random number from 1 to N.

-spec randgen ( State :: rand:ran() ) -> no_return().

randgen(State) ->
    receive
        {From, N} when is_pid(From) ->
            {Int, NewState} = rand:uniform_s(N, State),
            From ! {randgen, Int},
            randgen(NewState);
        From when is_pid(From) ->
            {Float, NewState} = rand:uniform_s(State),
            From ! {randgen, Float},
            randgen(NewState);
        _Other ->
            randgen(State)
    end.
