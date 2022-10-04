-module(bitcoin).

-export([connect/1, start/1, boss/3, worker/1]).

-define(SIZE, 64).
-define(QUANTITY, 10).


% I find this funciton at https://tsharju.github.io/2009/11/07/generating-random-strings-in-erlang/
generateString(Length)->
	ALLCH = "0123456789abcdefghijklmnopqrstuvwxyz:;=-",
	lists:foldl(fun(_, Acc) ->
			    [lists:nth(random:uniform(length(ALLCH)), ALLCH)]
		        ++ Acc
	            end,
	            [],
	            lists:seq(1, Length)).

mine(Worker, Number, BossNode, 0) ->
	io:format("Worker ~p finished mining~n", [Worker]),
	{boss, BossNode} ! {keepMineOrNot, self()},
	worker(BossNode);

mine(Worker, Number, BossNode, Times) ->

	Prefix = "pliang",
	% Random seed for random stuff
	random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
    % Get random string
	Tmp = generateString(random:uniform(random:uniform(?SIZE))),

	% Hash the string and convert the hash string to "String" type
    <<Hash:256>> = crypto:hash(sha256, string:concat(Prefix, Tmp)),
    Key = string:right(integer_to_list(Hash, 16), 64, $0),
    L = string:to_lower(Key),

    % Compare the first K number from the hash string to K 0s	
	A = lists:sublist(L, Number),
	B = lists:flatten(lists:duplicate(Number, "0")),

	% If find the code, send message to boss
	if
		A == B ->
			{boss, BossNode} ! {find, self(), string:concat(Prefix, Tmp), L},
			mine(Worker, Number, BossNode, Times-1);

		true ->
			% Send a message to boss to determine the worker need to keep mining or not
			% If call mine() directly here, worker can't receive any message.
			{boss, BossNode} ! {keepMineOrNot, self()},
			worker(BossNode)
	end.

% Worker Listener
worker(BossNode) ->

    receive
        {mine, NumberOfZeros} ->
        	mine(self(), NumberOfZeros, BossNode, 1);
        stop ->
        	io:format("Worker ~p stop~n", [self()]),
        	{_,Time} = statistics(runtime),
			CPU_time = Time / 1000,
			io:format("~p~n", [Time]),
        	{boss, BossNode} ! finished,
        	exit(normal);
        {keepMine, NumberOfZeros} ->
        	mine(self(), NumberOfZeros, BossNode, 1)
    end.

% Boss listener
boss(NumberOfZeros, HashList, WorkerList) ->
    receive
        finished ->

        	if
        		length(WorkerList) == 0 ->
		            io:format("boss finished~n"),
					{_,Time2} = statistics(wall_clock),

					timer:sleep(2000),
					Run_time = Time2 / 1000,
					io:format("real time: ~p seconds\n", [Run_time]),
		            exit(normal);
		    	true ->
		    		boss(NumberOfZeros, HashList, WorkerList)
		    end;


        {keepMineOrNot, WorkerPid} -> 
        	
        	if 
        		length(HashList) == 0 ->
        			WorkerPid ! {keepMine, NumberOfZeros},
        			boss(NumberOfZeros, HashList, WorkerList);
        		true ->
        			WorkerPid ! stop,
        			NewWorkerList = lists:delete(WorkerPid, WorkerList),
        			boss(NumberOfZeros, HashList, NewWorkerList)
        	end;

        {worker, WorkerPid} ->
            io:format("boss assign worker ~p~n", [WorkerPid]),
            NewWorkerList = [WorkerPid | WorkerList],
            WorkerPid ! {mine, NumberOfZeros},
            boss(NumberOfZeros, HashList, NewWorkerList);

        {find, Who, Str, HashStr} ->
				
			io:format("Worker ~p:~n", [Who]),
			io:format("Hash: ~s\nMy string is: ~s\n", [HashStr, Str]),
			NewHashList = store(Str, HashStr, HashList),
			boss(NumberOfZeros, NewHashList, WorkerList)
    end.


store(Str, HashStr, HashList) ->
	case length(HashList) of
		0 -> 
			[{Str, HashStr} | HashStr];
		fasle -> 
			HashStr
	end.

% Boss(Server) start
start(NumberOfZeros) -> 
    register(boss, spawn(bitcoin, boss, [NumberOfZeros, [], []])),
    connect(node()),
    statistics(runtime).
% Worker(Client) connect to Boss and mine coin
connect(BossNode) ->
    {boss, BossNode} ! {worker, spawn(bitcoin, worker, [BossNode])}.