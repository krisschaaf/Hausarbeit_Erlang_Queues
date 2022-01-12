-module(hbq).

-export([initHBQ/2]).

% Starten der HBQ auf der Node der HBQ (oder entfernt starten)
initHBQ(DLQLimit, HBQName) ->
	%//TODO erzeuge Datei 
	NodeString = util:to_String(node()),
	Datei = "HBQ-DLQ@"++NodeString++".log",
	DLQ = dlq:initDLQ(DLQLimit, Datei),
    HBQPid = spawn(hbq, loop, [[], DLQ, Datei, 1]),	%1, da dies der derzeitige Index ist 
	register(HBQName, HBQPid),
    HBQPid.	%ProzessID wird zurückgegeben 

loop(HBQ, DLQ, Datei, Pos) ->
	receive
		{From, {request, pushHBQ, [NNr, Msg, TSclientout]}} ->
			ExpNr = dlq:expectedNr(DLQ),
			if 
				NNr < ExpNr ->
					From ! {reply, nnr<expNrDLQ}
			end,
			Size = getSize(HBQ, 0),
			if
				Size < (MaxSize*2/3)
		% deliverMsg
		{From, {request, deliverMsg, NNr, ToClient}} ->
			TSdlqout = erlang:timestamp(),
			SendeNr = dlq:deliverMSG(NNr, ToClient, DLQ, Datei),
			From ! {reply, SendeNr},
			loop(HBQ, DLQ, Datei, Pos);
		% listDLQ
		{From, {request, listDLQ}}	->	
			List = dlq:listDLQ(DLQ),
			util:logging(Datei, List),	%//TODO: List ordentlich in logging Datei schreiben
			From ! {self(), ok},
			loop(HBQ, DLQ, Datei, Pos);
		% listHBQ
		{From, {request, listHBQ}} ->
			List = listHBQHelp(HBQ, []),
			util:logging(Datei, List),	%//TODO: List ordentlich in logging Datei schreiben
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Pos);
		% delHBQ
		{From, {dellHBQ}} -> 
		case exit(HBQName) of	% funktioniert nur mit try catch! Ansonsten exit/2
				true ->  
					From ! {self(), ok};
				false ->
					From ! {self(), exiting_failed},
					loop(HBQ, DLQ, Datei)
			end;
		terminate ->
			ok
	end.

%HILFSFUNKTIONEN

listHBQHelp([], List) -> List;
listHBQHelp(TempHBQ, List) -> 
	{SmallestElem, NewHBQ} = removeFirst(TempHBQ),
	listHBQHelp(NewHBQ, [List | SmallestElem]).

%@returns neue Queue welche übergebenes Element enthält
insertToQueue(Element, Queue) -> //TODO

removeFirst(HBQ) 

%returns kleinste Nachrichtennummer der HBQ (also Wurzelelement)
%Wichtig! In dieser Funktion werden keine Elemente aus HBQ entfernt
getSmallestNNr(Queue) -> 
	[MSGNr | Tail] = getSmallestMsg(Queue),
	MSGNr.