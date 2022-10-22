-module(chord).
-compile(export_all).

-define(M, 15).

getNode(ID) ->
	list_to_atom("node" ++ lists:flatten(io_lib:format("~p", [ID]))).

getKey(ID) ->
	list_to_atom("key" ++ lists:flatten(io_lib:format("~p", [ID]))).

getRandNum(N) ->
	rand:uniform(N).

getNodeInList(NodeList) ->
	R = getRandNum(length(NodeList)),
	lists:nth(R, NodeList).

createNode() ->
	Pid = spawn(chord, node, [nil, nil, nil, [], 0]),
	NodeID = sha1(pidToString(Pid)),
	NodeName = getNode(NodeID),
	register(NodeName, Pid),
	NodeID.

printALLNode(NodeList, 0) ->
	edfs;

printALLNode(NodeList, Size) ->
	P = lists:nth(Size, NodeList),
	N = getNode(P),
	N ! print,
	printALLNode(NodeList, Size-1).

stabilize(NodeList, Size, Index) ->

	if 
		Size - Index == -1 ->
			done;
		true ->

			NID = lists:nth(Index, NodeList),
			NodeName = getNode(NID),
			if
				Size == 1 ->
					NodeName ! {stabilize, NID, NID, NID};
				Index == 1 ->
					Successor = lists:nth(Index+1, NodeList),
					Predecessor = lists:nth(Size, NodeList),
					NodeName ! {stabilize, NID, Successor, Predecessor};
				Index == Size -> 
					Successor = lists:nth(1, NodeList),
					Predecessor = lists:nth(Index-1, NodeList),
					NodeName ! {stabilize, NID, Successor, Predecessor};
				true ->
					Successor = lists:nth(Index+1, NodeList),
					Predecessor = lists:nth(Index-1, NodeList),
					NodeName ! {stabilize, NID, Successor, Predecessor}
			end,
			stabilize(NodeList, Size, Index+1)
	end.

fixTable(NodeList, Size, ListIndex, FingerIndex) ->
	if 
		Size - ListIndex == -1 ->
			done;
		FingerIndex == ?M ->
			fixTable(NodeList, Size, ListIndex+1, 0);
		true ->

			NID = lists:nth(ListIndex, NodeList),
			NodeName = getNode(NID),
			N = round(math:pow(2, FingerIndex)),
			D = round(math:pow(2, ?M)),
			Next = (NID + N) rem D,
			NodeName ! {fixTable, Next},
			fixTable(NodeList, Size, ListIndex, FingerIndex+1)
	end.

clearTable(NodeList, 0) ->
	done;

clearTable(NodeList, Size) ->
	NID = lists:nth(Size, NodeList),
	NodeName = getNode(NID),
	NodeName ! clearFingerTable,
	clearTable(NodeList, Size-1).

sendRequestToNodes(NodeList, 0) ->
	sendFinished;

sendRequestToNodes(NodeList, Size) ->
	Node = lists:nth(Size, NodeList),
	NodeName = getNode(Node),
	Key = getRandNum(round(math:pow(2, ?M))),
	io:format("Send Key ~p to Node ~p~n", [Key, Node]),
	NodeName ! {sendRequest, Key, 0, Node},
	sendRequestToNodes(NodeList, Size-1).

calAvg(NodeList, 0) ->
	done;

calAvg(NodeList, Size) ->
	Node = lists:nth(Size, NodeList),
	NodeName = getNode(Node),
	NodeName ! getHop,
	calAvg(NodeList, Size-1).

% 0 for Hop, 1 for NumOfNodes, 2 for NumOfRequest
output(Data, StoreData0, StoreData1, StoreData2, StoreData3) ->
	receive 
		{storeData0, Data0} ->
			output(Data, StoreData0, StoreData1, StoreData2, StoreData3);
		{storeData1, Data1} ->
			output(Data, StoreData0, Data1, StoreData2, StoreData3);
		{storeData2, Data2} ->
			output(Data, StoreData0, StoreData1, Data2, StoreData3);
		{storeData3, Data3} ->
			output(Data, StoreData0, StoreData1, StoreData2, Data3);
		print ->
			Tmp = StoreData0 / StoreData2,
			Ans = Tmp / StoreData1,
			io:format("Average Hop Per Nodes(Total/NumOfRequest) is ~.6f~nAverage Hop Per Request(Total/(NumOfNodes*NumOfRequest)) is ~.6f~n", [Tmp, Ans]),
			output(Data, StoreData0, StoreData1, StoreData2, StoreData3);
		clear ->
			output(0, 0, 0, 0, 0);
		{storeData0, add, Data0, NID} ->
			io:format("Node~p total hop is ~p, average hop is ~.6f~n", [NID, Data0, Data0/StoreData2]),
			output(Data, StoreData0 + Data0, StoreData1, StoreData2, StoreData3)
	end.

chordRing(NodeList) ->

	receive
		join ->
			NodeID = createNode(),
			NewList = NodeList ++ [NodeID],
			chordRing(NewList);
		sort ->
			NewNodeList = lists:sort(NodeList),
			chordRing(NewNodeList);
		stabilize -> % let all the node in list stablize
			NewNodeList = lists:sort(NodeList),
			stabilize(NewNodeList, length(NewNodeList), 1),
			chordRing(NewNodeList);
		fixTable ->
			clearTable(NodeList, length(NodeList)),
			fixTable(NodeList, length(NodeList), 1, 0),
			chordRing(NodeList);
		sendRequest ->
			sendRequestToNodes(NodeList, length(NodeList)),
			chordRing(NodeList);
		print ->
			printALLNode(NodeList, length(NodeList)),
			chordRing(NodeList);
		calAvg ->
			calAvg(NodeList, length(NodeList)),
			chordRing(NodeList);
		printAvg ->
			output ! print,
			chordRing(NodeList);
		{return, TotalHop, NID} ->
			output ! {storeData0, add, TotalHop, NID},
			chordRing(NodeList)
	end.

node(NID, Successor, Predecessor, FingerTable, TotalHop) ->

	receive
		{stabilize, NewID, NewSuccessor, NewPredecessor}-> 
			node(NewID, NewSuccessor, NewPredecessor, FingerTable, TotalHop);
		print ->
			io:format("NID ~p, Successor ~p, Predecessor ~p~nFingerTable ~p~n, TotalHop ~p~n", [NID, Successor, Predecessor,FingerTable, TotalHop]),
			node(NID, Successor, Predecessor, FingerTable, TotalHop);
		{fixTable, findSuccessor, TargetID, WhoAsk} ->
			SuccessorName = getNode(Successor),
			ToName = getNode(WhoAsk),
			if
				(NID < TargetID) and (TargetID =< Successor) ->
					ToName ! {find, Successor, TargetID};
				true ->

					if 
						Successor < NID ->

							if 
								NID < TargetID ->
									ToName! {find, Successor, TargetID};
								TargetID =< Successor ->
									ToName ! {find, Successor, TargetID};
								true ->
									SuccessorName ! {fixTable, findSuccessor, TargetID, WhoAsk}
							end;
						true ->
							SuccessorName ! {fixTable, findSuccessor, TargetID, WhoAsk}
					end
			end,
			node(NID, Successor, Predecessor, FingerTable, TotalHop);
		{fixTable, TargetID} ->
			if 
				(NID == Successor) and (NID == Predecessor) -> % only one node
					NewFingerTable = FingerTable ++ [{getKey(TargetID), NID}],
					SortedNewFingerTable = lists:keysort(2, NewFingerTable),
					node(NID, Successor, Predecessor, SortedNewFingerTable, TotalHop);
				(NID == TargetID) ->
					NewFingerTable = FingerTable ++ [{getKey(TargetID), NID}],
					SortedNewFingerTable = lists:keysort(2, NewFingerTable),
					node(NID, Successor, Predecessor, SortedNewFingerTable, TotalHop);					
				(NID < TargetID) and (TargetID =< Successor) ->
					NewFingerTable = FingerTable ++ [{getKey(TargetID), Successor}],
					SortedNewFingerTable = lists:keysort(2, NewFingerTable),
					node(NID, Successor, Predecessor, SortedNewFingerTable, TotalHop);
				true ->

					if
						Successor < NID ->

							if
								NID < TargetID ->
									NewFingerTable = FingerTable ++ [{getKey(TargetID), Successor}],
									SortedNewFingerTable = lists:keysort(2, NewFingerTable),
									node(NID, Successor, Predecessor, SortedNewFingerTable, TotalHop);
								TargetID =< Successor ->
									NewFingerTable = FingerTable ++ [{getKey(TargetID), Successor}],
									SortedNewFingerTable = lists:keysort(2, NewFingerTable),
									node(NID, Successor, Predecessor, SortedNewFingerTable, TotalHop);	
								true ->		
									SuccessorName = getNode(Successor),
									SuccessorName ! {fixTable, findSuccessor, TargetID, NID},
									node(NID, Successor, Predecessor, FingerTable, TotalHop)
							end;
						true ->							
							SuccessorName = getNode(Successor),
							SuccessorName ! {fixTable, findSuccessor, TargetID, NID},
							node(NID, Successor, Predecessor, FingerTable, TotalHop)
					end
			end;
		{find, NewSuccessor, TargetID} ->
			NewFingerTable = FingerTable ++ [{getKey(TargetID), NewSuccessor}],
			SortedNewFingerTable = lists:keysort(2, NewFingerTable),
			node(NID, Successor, Predecessor, SortedNewFingerTable, TotalHop);

		{sendRequest, Key, Hop, Start} ->

			StartName = getNode(Start),

			if
				NID == Key ->
					io:format("Node~p find Key ~p is store in ~p~n", [Start, Key, NID]),
					StartName ! {findKey, Hop};					
				(NID < Key) and (Key =< Successor) ->
					io:format("Node~p find Key ~p is store in ~p~n", [Start, Key, Successor]),
					StartName ! {findKey, Hop};
				true ->
					if 
						Successor < NID ->
							if 
								NID < Key ->
									io:format("Node~p find Key ~p is store in ~p~n", [Start, Key, Successor]),
									StartName ! {findKey, Hop};
								Key =< Successor ->
									io:format("Node~p find Key ~p is store in ~p~n", [Start, Key, Successor]),
									StartName ! {findKey, Hop};
								true ->
									ClosetNode = closetPreceding(FingerTable, length(FingerTable), Key, Successor, NID),
									ClosetName = getNode(ClosetNode),
									ClosetName ! {sendRequest, Key, Hop+1, Start}
							end;
						true ->
							ClosetNode = closetPreceding(FingerTable, length(FingerTable), Key, Successor, NID),
							ClosetName = getNode(ClosetNode),
							ClosetName ! {sendRequest, Key, Hop+1, Start}
					end
			end,
			node(NID, Successor, Predecessor, FingerTable, TotalHop);
		{findKey, Hop} ->
			node(NID, Successor, Predecessor, FingerTable, TotalHop+Hop);
		getHop ->
			chordRing ! {return, TotalHop, NID},
			node(NID, Successor, Predecessor, FingerTable, TotalHop);
		clearFingerTable ->
			node(NID, Successor, Predecessor, [], TotalHop)
	end.

closetPreceding(FingerTable, 0, Key, Successor, NID) ->
	Successor;

closetPreceding(FingerTable, Size, Key, Successor, NID) ->
	N = lists:nth(Size, FingerTable),
	{A, B} = N,
	if
		B == NID ->
			closetPreceding(FingerTable, Size-1, Key, Successor, NID);
		Key > B ->
			B;
		true ->
			closetPreceding(FingerTable, Size-1, Key, Successor, NID)
	end.

createLoop(0) ->
	timer:sleep(500),
	chordRing ! stabilize,
	chordRing ! fixTable;

createLoop(NumOfNodes) ->
	chordRing ! join,
	createLoop(NumOfNodes-1).

sendRequest(0) ->
	sendFinished;

sendRequest(NumOfRequest) ->
	chordRing ! sendRequest,
	sendRequest(NumOfRequest-1).

stepStart() ->
	register(chordRing, spawn(chord, chordRing, [[]])),
	register(output, spawn(chord, output, [0, 0, 0, 0, 0])).

start(NumOfNodes, NumOfRequest) ->
	register(chordRing, spawn(chord, chordRing, [[]])),
	register(output, spawn(chord, output, [0, 0, NumOfNodes, NumOfRequest, 0])),
	createLoop(NumOfNodes),
	sendRequest(NumOfRequest),
	timer:sleep(500),
	chordRing ! calAvg,
	timer:sleep(500),
	chordRing ! printAvg.

pidToString(Pid) ->
	PidStr = pid_to_list(Pid).

sha1(String) ->
	<<Hash:256>> = crypto:hash(sha256, String),
    Hash rem round(math:pow(2, ?M)).