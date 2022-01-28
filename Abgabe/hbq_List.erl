-module(hbq_List).

-export([initHBQ/2]).

% @author: Kristoffer Schaaf
% In dieser Implementierung der Holdback Queue wird die interne Queue als Liste umgesetzt
% Außerdem wird mit Pattern Matching gearbeitet

% Starten der HBQ 
initHBQ(DLQLimit, HBQName) ->
	{ok, NodeString} = inet:gethostname(),
	Datei = "HBQ-DLQ@"++NodeString++".log",
	DLQ = dlq:initDLQ(DLQLimit, Datei),
    HBQPid = spawn(fun() -> loop([], DLQ, Datei, 0, DLQLimit) end),	% 0, da dies die Initialisierungsgröße der Liste ist 
	register(HBQName, HBQPid),
    HBQPid.	%ProzessID wird zurückgegeben 

loop(HBQ, DLQ, Datei, Size, DLQLimit) ->
	receive
		% pushHBQ
		{From, {request, pushHBQ, [NNr, Msg, TSclientout]}} ->
			ExpNr = dlq:expectedNr(DLQ),
			pushHBQ([NNr, Msg, TSclientout, erlang:timestamp()], ExpNr, HBQ, DLQ, Datei, Size, DLQLimit, From);
		% deliverMsg
		{From, {request, deliverMSG, NNr, ToClient}} ->
			SendeNr = dlq:deliverMSG(NNr, ToClient, DLQ, Datei),
			From ! {reply, SendeNr},
			loop(HBQ, DLQ, Datei, Size, DLQLimit);
		% listDLQ
		{From, {request, listDLQ}}	->	
			List = dlq:listDLQ(DLQ),
			ListSize = getListSize(List, 0),
			util:logging(Datei, "dlq>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)++"\n"),	
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Size, DLQLimit);
		% listHBQ
		{From, {request, listHBQ}} ->
			List = listHBQHelp(HBQ, []),
			ListSize = getListSize(List, 0),
			util:logging(Datei, "HBQ>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)++"\n"),	
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Size, DLQLimit);
		% delHBQ
		{From, {request, dellHBQ}} -> 
			From ! {reply, ok}
	end.

% Diese Funktion prüft den Inhalt der Holdback Queue auf Auslieferbarkeit 
% Wenn Elemente für die Delivery Queue geeignet sind, werden diese ausgeliefert
% Nach Terminierung dieser Funktion wird wieder die loop/5 Funktion aufgerufen, um sicherzustellen, dass der Prozess nicht beendet wird
checkHBQ([], DLQ, Datei, Size, DLQLimit) -> 
	util:logging(Datei, "HBQ>>> HBQ wurde komplett in DLQ uebertragen.\n"),
			loop([], DLQ, Datei, Size, DLQLimit);
checkHBQ(HBQ, DLQ, Datei, Size, DLQLimit)	->
	checkHBQTm(HBQ, DLQ, Datei, Size, DLQLimit, dlq:expectedNr(DLQ)).
checkHBQTm([[NNr, Msg, TSclientout, TShbqin]|Tail], DLQ, Datei, Size, DLQLimit, ExpNr) when NNr == ExpNr ->
	NewDLQ = dlq:push2DLQ([NNr, Msg, TSclientout, TShbqin], DLQ, Datei),
	checkHBQ(Tail, NewDLQ, Datei, Size-1, DLQLimit); 
checkHBQTm([[NNr, _Msg, _TSclientout, _TShbqin]|Tail], DLQ, Datei, Size, DLQLimit, ExpNr) when NNr < ExpNr ->
	checkHBQ(Tail, DLQ, Datei, Size-1, DLQLimit);
checkHBQTm(HBQ, DLQ, Datei, Size, DLQLimit, _ExpNr) ->
	loop(HBQ, DLQ, Datei, Size, DLQLimit).

%-----Hilfsfunktionen-----%

% Diese Funktion ist eine Hilfsfunktion für das Pushen einer Nachricht in die Holdback Queue 
% Nach Terminierung wird die checkHBQ Funktion geöffnet um nach jeder Veränderung der HBQ die Auslieferbarkeit der Elemente zu prüfen
pushHBQ([NNr, _Msg, _TSclientout, _TShbqin], ExpNr, HBQ, DLQ, Datei, Size, DLQLimit, From) when NNr < ExpNr -> 
	From ! {reply, nnr<expNrDLQ},
	checkHBQ(HBQ, DLQ, Datei, Size, DLQLimit);
pushHBQ([NNr, Msg, TSclientout, TShbqin], _ExpNr, HBQ, DLQ, Datei, Size, DLQLimit, From) when Size < (DLQLimit*2/3) ->
	NewHBQ = insertToHBQ([NNr, Msg, TSclientout, TShbqin], HBQ),
	util:logging(Datei, "HBQ>>> Nachricht "++util:to_String(NNr)++" in HBQ eingefuegt.\n"),
	From ! {reply, ok},
	checkHBQ(NewHBQ, DLQ, Datei, Size+1, DLQLimit);
pushHBQ([NNr, Msg, TSclientout, TShbqin], ExpNr, [[SNNr, SMsg, STSclientout, STShbqin]|TempHBQ], DLQ, Datei, Size, DLQLimit, From) ->
	DLQMsg = [SNNr, SMsg, STSclientout, STShbqin],
	pushHBQHelp([NNr, Msg, TSclientout, TShbqin], ExpNr, DLQ, Datei, Size, DLQLimit, DLQMsg, SNNr, TempHBQ, From).

pushHBQHelp([NNr, Msg, TSclientout, TShbqin], ExpNr, DLQ, Datei, Size, DLQLimit, DLQMsg, SNNr, TempHBQ, From) when SNNr == ExpNr ->
	NewDLQ = dlq:push2DLQ(DLQMsg, DLQ, Datei),
	NewHBQ = insertToHBQ([NNr, Msg, TSclientout, TShbqin], TempHBQ),
	util:logging(Datei, "HBQ>>> Nachricht "++util:to_String(NNr)++" in HBQ eingefuegt.\n"),
	From ! {reply, ok},
	checkHBQ(NewHBQ, NewDLQ, Datei, Size, DLQLimit);
pushHBQHelp([NNr, Msg, TSclientout, TShbqin], ExpNr, DLQ, Datei, Size, DLQLimit, DLQMsg, SNNr, TempHBQ, From) ->
	ErrorLog = "**Fehlernachricht fuer Nachrichtennummern "++util:to_String(ExpNr)++" bis "++util:to_String(SNNr-1)++" um "++util:to_String(erlang:timestamp()),
	util:logging(Datei, "HBQ>>>Fehlernachricht fuer Nachrichten "++util:to_String(ExpNr)++" bis "++util:to_String(SNNr-1)++" generiert.\n"),
	TempDLQ = dlq:push2DLQ([SNNr-1, ErrorLog, unkn, erlang:timestamp()], DLQ, Datei),
	NewDLQ = dlq:push2DLQ(DLQMsg, TempDLQ, Datei),
	NewHBQ = insertToHBQ([NNr, Msg, TSclientout, TShbqin], TempHBQ),
	util:logging(Datei, "HBQ>>> Nachricht "++util:to_String(NNr)++" in HBQ eingefuegt.\n"),
	From ! {reply, ok},
	checkHBQ(NewHBQ, NewDLQ, Datei, Size, DLQLimit).

% @returns Liste, welche alle Nachrichtennummern der übergebenen HBQ enthält
listHBQHelp([], List) -> List;
listHBQHelp([[NNr, _Msg, _TSclientout, _TShbqin]|NewHBQ], List) -> listHBQHelp(NewHBQ, List++[NNr]).

% @returns Listengröße der übergebenen Liste 
getListSize([], Size) -> Size;
getListSize([_Head|Tail], Size) -> getListSize(Tail, Size+1).

%@returns neue Queue welche übergebenes Element enthält
% Beim Einfügen hat die Nachricht die Struktur eines Tupels mit [NNr, Msg, TSclientout, TShbqin]
% Element wandert so lange durch die Liste bis es größer als Vorgaenger und kleiner als der Nachfolger ist
insertToHBQ([NNrN, MsgN, TSclientoutN, TShbqinN], []) -> [[NNrN, MsgN, TSclientoutN, TShbqinN]];
insertToHBQ([NNrN, MsgN, TSclientoutN, TShbqinN], [[NNr, Msg, TSclientout, TShbqin] | Tail]) when NNrN < NNr ->
	[ [NNrN, MsgN, TSclientoutN, TShbqinN] |[ [NNr, Msg, TSclientout, TShbqin] | Tail]];
insertToHBQ([NNrN, MsgN, TSclientoutN, TShbqinN], [[NNr, Msg, TSclientout, TShbqin] | Tail]) ->
	[[NNr, Msg, TSclientout, TShbqin]|insertToHBQ([NNrN, MsgN, TSclientoutN, TShbqinN], Tail)].
