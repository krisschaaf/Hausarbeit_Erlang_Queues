-module(hbq).

-export([initHBQ/2, loop/1]).

% Starten der HBQ auf der Node der HBQ (oder entfernt starten)
initHBQ(DLQLimit, HBQName) ->
	%//TODO erzeuge Datei 
	Datei = "HBQ.log",
	DLQ = initDLQ(DLQLimit, Datei),
    HBQ = spawn(hbq, loop, [[], DLQLimit, DLQ, Datei, 1]),	%1, da dies der derzeitige Index ist 
	register(HBQName, HBQ),
    HBQ.	%ProzessID wird zurückgegeben 

loop(HBQ, DLQLimit, DLQ, Datei, Pos) ->
	receive
		{From, {request, pushHBQ, [Nnr, Msg, TSclientout]}} ->
			NewQueue = insertToQueue([Nnr, Msg, TSclientout], HBQ);
			From ! {self(), ok},
			loop(NewQueue, DLQLimit, DLQ, Datei);	
		% @return SendeNr (tatsächlich gesendet)
		% über Funktionsaufruf der DLQ die MSG aus dieser Queue löschen und in die DLQ anfügen
		{From, {request, deliverMsg, Nnr, ToClient}} ->
			case isMember(Nnr, HBQ) of	%member/2 darf nicht genutzt werden
				true ->
					From ! {self(), {ok, Nnr}},
					dlq:push2DLQ(//TODO);
					loop(deleteFrom(Nnr, HBQ), DLQLimit, DLQ, Datei);	%darf nicht genutzt werden 
					% -> gibt Liste ohne Element zurück
				false ->
					From ! {self(), msg_not_found},
					loop(HBQ, DLQLimit, DLQ, Datei)
			end;
		%//TODO Funktion aufteilen in zwei Unterfunktionen
		{From, {listADT}}	->	
			From ! {self(), ok},
			loop(HBQ, DLQLimit, DLQ, Datei);
		{From, {dellHBQ}} -> 
		case exit(HBQName) of	% funktioniert nur mit try catch! Ansonsten exit/2
				true ->  
					From ! {self(), ok};
				false ->
					From ! {self(), exiting_failed},
					loop(HBQ, DLQLimit, DLQ, Datei)
			end;
		terminate ->
			ok
	end.

%HILFSFUNKTIONEN

%@returns neue Queue welche übergebenes Element enthält
insertToQueue(Element, Queue) -> //TODO

%returns kleinste Nachrichtennummer der HBQ (also Wurzelelement)
%Wichtig! In dieser Funktion werden keine Elemente aus HBQ entfernt
getSmallestNnr(Queue) -> 
	[MSGNr | Tail] = getSmallestMsg(Queue),
	MSGNr.

%returns kleinste Nachricht mit allen Elementen der HBQ (also Wurzelelement)
%returns neu strukturierte HBQ ohne Wurzelelement
getSmallestMsg(Queue) -> //TODO

%returns boolean ob HBQ übergebenes Element enthält (ja -> true)
isElemOfHBQ(Element, Queue) ->	//TODO

%returns neue Queue welche das übergebenen Element nicht mehr enthält 
deleteFromHBQ(Element, Queue) -> //TODO

%returns Größe der übergebenen HBQ
getSizeHBQ(Queue, TempSize) -> //TODO

%returns boolean ob übergebene HBQ leer ist (ja -> true)
isEmptyHBQ(Queue) -> //TODO

%% -------------------------------------------
% Schreibt ggf auf den Bildschirm und in eine Datei
% nebenlÃ¤ufig zur Beschleunigung
% Beispielaufruf: logging('FileName.log',"Textinhalt"),
%
% logging(_Datei,_Inhalt) -&gt; ok;
% Schreibt Inhal in Datei, nebenlÃ¤ufig!
logging(Datei,Inhalt) -&gt; Known = erlang:whereis(logklc),
						 case Known of
							undefined -&gt; PIDlogklc = spawn(fun() -&gt; logloop(0) end),
										 erlang:register(logklc,PIDlogklc);
							_NotUndef -&gt; ok
						 end,
						 logklc ! {Datei,Inhalt},
						 ok.