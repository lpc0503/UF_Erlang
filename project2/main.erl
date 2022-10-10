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

		"2D-Grid"->
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
		"2D-Grid" ->
			RetList = getGrid(ID, N, Type, [], -1, 0),
			RetList;
		"Line" ->
			if 
				ID == 1 ->
					RetList = [ID+1],
					RetList;
				ID == NumOfWorker ->
					RetList = [ID],
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

sendRumor(ID, NeighborList) ->

	RID = getRandomNeighbor(length(NeighborList), NeighborList),
	NName = getWorkerName(RID),

	NName ! {work, self()},
	receive 
		work ->
			NName ! {rumor, ID},
			sendRumor(ID, NeighborList);
		{delete, ToDelete} ->
			NewNeighborList = lists:delete(NeighborList, ToDelete),
			sendRumor(ID, NewNeighborList)
	end.
%%%
%%% Gossip
%% TODO 座標化
worker(WorkerName, ID, Times, NeighborList)->

	receive
		{rumor, FromID} ->

			if
				Times == 0 ->
					Pid = spawn(main, sendRumor, [ID, NeighborList]),
					worker(WorkerName, ID, Times+1, NeighborList);
				true ->
					if 
						Times+1 >= 10 ->
							termination ! {terminate, WorkerName, Times + 1},
							worker(WorkerName, ID, Times, NeighborList);
						true ->
							worker(WorkerName, ID, Times+1, NeighborList)
					end
			end;
		{work, SubActor} ->
			if
				Times == 10 ->
					SubActor ! {delete, WorkerName},
					worker(WorkerName, ID, Times, NeighborList);
				true ->
					SubActor ! work,
					worker(WorkerName, ID, Times, NeighborList)
			end
	end.

%%% Push-Sum
worker(WorkerName, ID, Rounds, NeighborList, S, W, PrevRatio) ->

	receive
		{rumor, S_, W_} ->

			NewS = S + S_,
			NewW = W + W_,
			NewRatio = NewS/NewW,

			RID = getRandomNeighbor(length(NeighborList), NeighborList),
			NName = getWorkerName(RID),
			NName ! {rumor, NewS/2, NewW/2},
			if
				abs(NewRatio - PrevRatio) =< 0.001 ->
					if
						Rounds+1 == 3 ->
							termination ! {terminate, NewRatio},
							worker(WorkerName, ID, 0, NeighborList, NewS/2, NewW/2, NewRatio);
						true ->
							worker(WorkerName, ID, Rounds+1, NeighborList, NewS/2, NewW/2, NewRatio)
					end;
				true ->
					worker(WorkerName, ID, 0, NeighborList, NewS/2, NewW/2, NewRatio)
			end;
		start ->
			RID = getRandomNeighbor(length(NeighborList), NeighborList),
			NName = getWorkerName(RID),
			NName ! {rumor, S/2, W/2},
			worker(WorkerName, ID, Rounds, NeighborList, S/2, W/2, PrevRatio)
	end.


pushSumSend(TotalWorkers, ID, 0) ->
	every_worker_start_sending_rumor;

pushSumSend(TotalWorkers, ID, Count) ->
		WorkerName = getWorkerName(Count),
		WorkerName ! start,
		pushSumSend(TotalWorkers, ID, Count-1).


%%% create worker
create(TotalWorkers, ID, 0, Topoplgy, Algorithm) ->
	
	io:format("Create worker Finish~n"),
	case Algorithm of
		"Gossip" ->
			WorkerName = getWorkerName(rand:uniform(TotalWorkers)),
			WorkerName ! {rumor, "Server"};
		"Push-Sum" ->
			pushSumSend(TotalWorkers, ID, TotalWorkers)
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
	register(termination, spawn(main, stop, [TimeStart, 0, 10000])),

	if 
		(Topoplgy == "2D-Grid") or (Topoplgy == "Imperfect-3D-Grid") ->
			N = trunc(math:floor(math:sqrt(NumOfWorker))),
			TotalWorkers = N*N,
			create(TotalWorkers, 1, TotalWorkers, Topoplgy, Algorithm);
		true ->
			create(NumOfWorker, 1, NumOfWorker, Topoplgy, Algorithm)
	end.

stop(TimeStart, Times, PrevRatio) ->

	receive
		{terminate, Ratio} ->
			if 
				TimeStart == -1 ->
					stop(TimeStart, Times, PrevRatio);
				true ->
					TimeEnd = erlang:monotonic_time()/10000,
					RunTime = TimeEnd - TimeStart,
					io:format("Total time is ~.2f, Ratio is ~.10f~n", [RunTime, Ratio]),
					stop(-1, Times, Ratio)
			end;
		{terminate, WorkerName, Time} ->
			if
				Times == 10 ->
					TimeEnd = erlang:monotonic_time()/10000,
					RunTime = TimeEnd - TimeStart,
					io:format("Total time is ~p~n", [RunTime]),
					stop(TimeStart, Times+1, PrevRatio);
				true ->
					stop(TimeStart, Times+1, PrevRatio)
			end
	end.