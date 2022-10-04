-module(main).
-compile(export_all).

%%% Worker API
getWorkerName(ID) ->
	list_to_atom("worker" ++ lists:flatten(io_lib:format("~p", [ID]))).

getRandomNeighbor(N, List) ->

	% If the type is full network, and there is 100000 node, it is not possible for all nodes to
	% store all the neighbor, so we store the number of nodes in list. When came into this fun
	% only two cases that the List contain only 1 data, one is all network only a node 0, or it
	% is full network with number of nodes store in the list.
	if
		N == 1 ->
			Num = lists:nth(1, List),
			if 
				Num == 1->
					1;
				true ->
					Pos = rand:uniform(Num),
					Pos
			end;
		true->
		    Pos = rand:uniform(N),
		    lists:nth(Pos, List)
	end.

getRandExcept(ID, NeighborList, NumOfWorker) ->
	N = rand:uniform(NumOfWorker),
	case lists:member(N, NeighborList) of
		true ->
			if 
				N+1 == NumOfWorker ->
					[];
				true ->
					getRandExcept(ID, NeighborList, NumOfWorker)
			end;
		false ->
			if
				N == ID ->
					getRandExcept(ID, NeighborList, NumOfWorker);
				true ->
					N
			end
	end.

getFull(From, To, Except, List) ->

	if 
		From == To ->
			List;
		From == Except ->
			getFull(From+1, To, Except, List);
		true ->
			NewList = lists:append(List, [From]),
			getFull(From+1, To, Except, NewList)
	end.

appendNeighbor(ID, N, List, I, J) ->
	NewList = lists:append(List, [ID+I*N+J]),
	NewList.

getGrid(ID, N, Type, List, I, J) ->
	case Type of
		"Imperfect-3D-Grid" ->
			% In I = -1 layer
			if 
				I == -1 ->
					if
						ID == 1 ->
							getGrid(ID, N, Type, List, I+1, 0);
						(ID + I*N) =< 0 ->
							getGrid(ID, N, Type, List, I+1, -1);
						J == -1 ->
							if 
								ID rem N == 1 ->
									getGrid(ID, N, Type, List, I, 0);
								true ->
									NewList = appendNeighbor(ID, N, List, I, J),
									getGrid(ID, N, Type, NewList, I, J+1)
							end;

						J == 0 ->
							NewList = appendNeighbor(ID, N, List, I, J),
							getGrid(ID, N, Type, NewList, I, J+1);

						J == 1 ->
							if
								ID rem N == 0 ->
									getGrid(ID, N, Type, List, I+1, -1);
								true ->
									NewList = appendNeighbor(ID, N, List, I, J),
									getGrid(ID, N, Type, NewList, I+1, -1)
							end;
						true ->
							doNothing
					end;
				% I = 0 Layer
				I == 0 ->
					
					if

						J == -1 ->
							if 
								ID rem N == 1 ->
									getGrid(ID, N, Type, List, I, 0);
								true ->
									NewList = appendNeighbor(ID, N, List, I, J),
									getGrid(ID, N, Type, NewList, I, J+1)
							end;

						J == 0 ->
							getGrid(ID, N, Type, List, I, J+1);

						J == 1 ->
							if
								ID rem N == 0 ->
									getGrid(ID, N, Type, List, I+1, -1);
								true->
									NewList = appendNeighbor(ID, N, List, I, J),
									getGrid(ID, N, Type, NewList, I+1, -1)
							end;
						true ->
							doNothing
					end;
				I == 1 ->

					if
						ID + N > N*N ->
							List;
						J == -1 ->
							if 
								ID rem N == 1 ->
									getGrid(ID, N, Type, List, I, 0);
								true ->
									NewList = appendNeighbor(ID, N, List, I, J),
									getGrid(ID, N, Type, NewList, I, J+1)
							end;

						J == 0 ->
							NewList = appendNeighbor(ID, N, List, I, J),
							getGrid(ID, N, Type, NewList, I, J+1);

						J == 1 ->
							if
								ID rem N == 0 ->
									List;
								true->
									NewList = appendNeighbor(ID, N, List, I, J),
									NewList
							end;
						true ->
							doNothing
					end;
				true ->
					doNothing
			end;

		"3D-Grid"->
			if 
				I == -1 ->
					if 
						ID - N =< 0 ->
							getGrid(ID, N, Type, List, I+1, -1);
						true ->
							NewList = appendNeighbor(ID, N, List, I, 0),
							getGrid(ID, N, Type, NewList, I+1, -1)
					end;
				I == 0 ->
					if

						J == -1 ->
							if 
								ID rem N == 1 ->
									getGrid(ID, N, Type, List, I, 0);
								true ->
									NewList = appendNeighbor(ID, N, List, I, J),
									getGrid(ID, N, Type, NewList, I, J+1)
							end;

						J == 0 ->
							getGrid(ID, N, Type, List, I, J+1);

						J == 1 ->
							if
								ID rem N == 0 ->
									getGrid(ID, N, Type, List, I+1, -1);
								true->
									NewList = appendNeighbor(ID, N, List, I, J),
									getGrid(ID, N, Type, NewList, I+1, -1)
							end;
						true ->
							doNothing
					end;
				I == 1 ->
					if
						ID + N > N*N ->
							List;
						true ->
							NewList = appendNeighbor(ID, N, List, I, 0),
							NewList
					end;
				true ->
					doNothing
			end
	end.

% return a neigbor list
getNeighborList(Type, NumOfWorker, ID) ->

	if 
		NumOfWorker == 1 ->
			List = [ID],
			List;
		true ->
			doNothing
	end,

	N = trunc(math:floor(math:sqrt(NumOfWorker))),

	case Type of
		"Full-Network" ->
			RetList = [NumOfWorker],
			RetList;
		"3D-Grid" ->
			RetList = getGrid(ID, N, Type, [], -1, 0),
			RetList;
		"Line" ->
			if 
				ID == 0 ->
					RetList = [ID+1],
					RetList;
				ID == NumOfWorker-1 ->
					RetList = [ID-1],
					RetList;
				true ->
					RetList = [ID-1, ID+1],
					RetList
			end;
		"Imperfect-3D-Grid" ->
			FirstList = getGrid(ID, N, Type, [], -1, -1),
			RandID = getRandExcept(ID, FirstList, NumOfWorker),

			case is_integer(RandID) of
				true ->
					RetList = FirstList ++ [RandID],
					RetList;
				false ->
					FirstList
			end
	end.

%%%
%%% Gossip
%% TODO 座標化
worker(WorkerName, ID, Times, NeighborList)->

	receive
		{rumor, FromID} ->
			% io:format("~p receive rumor from ~p, receive rumor ~p times~n", [WorkerName, getWorkerName(FromID),Times+1]),			
			if 
				Times+1 == 10 ->
					io:format("~p terminate the process because it receive ~p times of rumor~n", [WorkerName, Times+1]),
					termination ! terminate;
				true ->
					RID = getRandomNeighbor(length(NeighborList), NeighborList),
					NName = getWorkerName(RID),
					NName ! {rumor, ID},
					worker(WorkerName, ID, Times+1, NeighborList)
			end
	end.

%%% Push-Sum
worker(WorkerName, ID, Rounds, NeighborList, S, W, PrevRatio) ->

	receive
		{rumor, FromID, S_, W_} ->

			% io:format("~p receive rumor from ~p~n", [WorkerName, getWorkerName(FromID)]),
			NewS = S + S_,
			NewW = W + W_,
			NewRatio = NewS/NewW,

			RID = getRandomNeighbor(length(NeighborList), NeighborList),
			NName = getWorkerName(RID),

			if
				abs(NewRatio - PrevRatio) < 0.001 ->
					if
						Rounds+1 == 3 ->
							io:format("~p terminate. Current ratio is ~.8f, Previous ratio is ~.8f, diff is ~.8f~n", [WorkerName, NewRatio, PrevRatio, abs(NewRatio-PrevRatio)]),
							termination ! terminate;
						true ->
							NName ! {rumor, ID, NewS/2, NewW/2},
							worker(WorkerName, ID, Rounds+1, NeighborList, NewS/2, NewW/2, NewRatio)
					end;
				true ->
					NName ! {rumor, ID, NewS/2, NewW/2},
					worker(WorkerName, ID, 0, NeighborList, NewS/2, NewW/2, NewRatio)
			end;
		start ->
			io:format("~p start sending rumor~n", [WorkerName]),
			RID = getRandomNeighbor(length(NeighborList), NeighborList),
			NName = getWorkerName(RID),
			NName ! {rumor, ID, S/2, W/2},
			worker(WorkerName, ID, Rounds, NeighborList, S/2, W/2, PrevRatio)
	end.

%%% create worker
create(TotalWorkers, ID, 0, Topoplgy, Algorithm) ->
	
	io:format("Create worker Finish~n"),
	case Algorithm of
		"Gossip" ->
			WorkerName = getWorkerName(rand:uniform(TotalWorkers)),
			WorkerName ! {rumor, "Server"};
		"Push-Sum" ->
			WorkerName = getWorkerName(rand:uniform(TotalWorkers)),
			WorkerName ! start
	end;

create(TotalWorkers, ID, Cnt, Topoplgy, Algorithm) ->

	WorkerName = getWorkerName(ID),
	NeighborList = getNeighborList(Topoplgy, TotalWorkers, ID),

	case Algorithm of

		"Gossip" ->
			register(WorkerName, spawn(main, worker, [WorkerName, ID, 0, NeighborList]));
		"Push-Sum" ->
			register(WorkerName, spawn(main, worker, [WorkerName, ID, 0, NeighborList, ID, 1, 10000]));
		true ->
			ohho
	end, 
	create(TotalWorkers, ID+1, Cnt-1, Topoplgy, Algorithm).
%%%

start(NumOfWorker, Topoplgy, Algorithm) ->

	TimeStart = erlang:monotonic_time()/10000,
	register(termination, spawn(main, stop, [TimeStart])),
	create(NumOfWorker, 1, NumOfWorker, Topoplgy, Algorithm).

stop(TimeStart) ->

	receive
		terminate ->
			TimeEnd = erlang:monotonic_time()/10000,
			RunTime = TimeEnd - TimeStart,
			io:format("Total time is ~p~n", [RunTime]),
			erlang:halt()
	end.