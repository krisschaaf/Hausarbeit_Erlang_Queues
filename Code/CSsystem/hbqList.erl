-module(hbq).

-export([initHBQ/2]).

% Starten der HBQ auf der Node der HBQ (oder entfernt starten)
initHBQ(DLQLimit, HBQName) ->
	{ok, NodeString} = inet:gethostname(),
	Datei = "HBQ-DLQ@"++NodeString++".log",
	DLQ = dlq:initDLQ(DLQLimit, Datei),
	case erlang:whereis(dlqPID) of
		undefined -> dlqPIDnotdefined;
		_Undef -> ok
	end, 
    HBQPid = spawn(fun() -> loop([], DLQ, Datei, 1) end),	%1, da dies der derzeitige Index ist 
	register(HBQName, HBQPid),
    HBQPid.	%ProzessID wird zurückgegeben 

checkHBQ(HBQ, DLQ, Datei, Size)	->
	case HBQ of
		[] -> 
			util:logging(Datei, "HBQ>>> HBQ wurde komplett in DLQ uebertragen.\n"),
			loop(HBQ, DLQ, Datei, Size);
		_Default ->
			[[NNr, _Msg, _TSclientout, _TShbqin]| _Tail] = HBQ,
			ExpNr = dlq:expectedNr(DLQ),
			if
				NNr == ExpNr ->
					[DLQMsg|NewHBQ] = HBQ,
					NewDLQ = dlq:push2DLQ(DLQMsg, DLQ, Datei),
					checkHBQ(NewHBQ, NewDLQ, Datei, Size-1); 
				NNr < ExpNr ->
					[_SmallestElement|NewHBQ] = HBQ,
					checkHBQ(NewHBQ, DLQ, Datei, Size-1);
				true ->
					loop(HBQ, DLQ, Datei, Size)
			end
	end.

loop(HBQ, DLQ, Datei, Size) ->
	receive
		{From, {request, pushHBQ, [NNr, Msg, TSclientout]}} ->
			TShbqin = erlang:timestamp(),
			HBQMsg = [NNr, Msg, TSclientout, TShbqin],
			ExpNr = dlq:expectedNr(DLQ),
			if 
				NNr < ExpNr ->
					From ! {reply, nnr<expNrDLQ},
					checkHBQ(HBQ, DLQ, Datei, Size);
				true ->
					{MaxSize, _ActSize, _Q} = DLQ,
					if
						Size < (MaxSize*2/3) ->
							NewHBQ = insertToHBQ(HBQMsg, HBQ),
							NNrString = util:to_String(NNr),
							util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
							From ! {reply, ok},
							checkHBQ(NewHBQ, DLQ, Datei, Size+1);
						true -> 
							case HBQ of 
								{} -> 
									NewDLQ = dlq:push2DLQ([NNr, Msg, TSclientout, erlang:timestamp()], DLQ, Datei),
									From ! {reply, ok},
									checkHBQ(HBQ, NewDLQ, Datei, Size);
								_Default -> 
									[[SNNr, SMsg, STSClientout, STShbqin]| TempHBQ] = HBQ,
									DLQMsg = [SNNr, SMsg, STSClientout, STShbqin],
									if
										SNNr == ExpNr ->
											NewDLQ = dlq:push2DLQ(DLQMsg, DLQ, Datei),
											NewHBQ = insertToHBQ(HBQMsg, TempHBQ),
											NNrString = util:to_String(NNr),
											util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
											From ! {reply, ok},
											checkHBQ(NewHBQ, NewDLQ, Datei, Size);
										true ->									
											MSGNr = SNNr-1,
											TSError = erlang:timestamp(),
											ErrorLog = "**Fehlernachricht fuer Nachrichtennummern "++util:to_String(ExpNr)++" bis "++util:to_String(MSGNr)++" um "++util:to_String(TSError),
											% erst die Fehlermeldung loggen, da in push2DLQ(ErrorMessage) die MSGNr geloggt wird 
											util:logging(Datei, "HBQ>>>Fehlernachricht fuer Nachrichten "++util:to_String(ExpNr)++" bis "++util:to_String(MSGNr)++" generiert.\n"),
											ErrorMessage = [MSGNr, ErrorLog, unkn, TSError],
											TempDLQ = dlq:push2DLQ(ErrorMessage, DLQ, Datei),
											NewDLQ = dlq:push2DLQ(DLQMsg, TempDLQ, Datei),
											NewHBQ = insertToHBQ(HBQMsg, TempHBQ),
											NNrString = util:to_String(NNr),
											util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
											From ! {reply, ok},
											checkHBQ(NewHBQ, NewDLQ, Datei, Size)
									end
							end
					end
			end;
		% deliverMsg
		{From, {request, deliverMSG, NNr, ToClient}} ->
			SendeNr = dlq:deliverMSG(NNr, ToClient, DLQ, Datei),
			From ! {reply, SendeNr},
			loop(HBQ, DLQ, Datei, Size);
		% listDLQ
		{From, {request, listDLQ}}	->	
			List = dlq:listDLQ(DLQ),
			ListSize = getListSize(List, 0),
			util:logging(Datei, "dlq>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)++"\n"),	
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Size);
		% listHBQ
		{From, {request, listHBQ}} ->
			List = listHBQHelp(HBQ, []),
			ListSize = getListSize(List, 0),
			util:logging(Datei, "HBQ>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)++"\n"),	
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Size);
		% delHBQ
		{From, {setHBQ, NewHBQ, NewSize}} ->
			From ! {reply, ok},
			loop(NewHBQ, DLQ, Datei, NewSize);
		{From, getHBQSize} ->
			From ! {reply, HBQ, Size},
			loop(HBQ, DLQ, Datei, Size);
		{From, {setDLQ, NewDLQ}} ->
			From ! {reply, ok},
			loop(HBQ, NewDLQ, Datei, Size);
		{From, getDLQ} ->
			From ! {reply, DLQ},
			loop(HBQ, DLQ, Datei, Size);
		{From, {request, dellHBQ}} -> 
			From ! {reply, ok}
	end.

%HILFSFUNKTIONEN

% HBQ Aufbau: [[NNr, Msg, TSclientout, TShbqin]| Tail]
listHBQHelp([], List) -> List;
listHBQHelp([[NNr, _Msg, _TSclientout, _TShbqin]|NewHBQ], List) -> 
	listHBQHelp(NewHBQ, List++[NNr]).

getListSize([], Size) -> Size;
getListSize([_Head|Tail], Size) -> getListSize(Tail, Size+1).

%@returns neue Queue welche übergebenes Element enthält
% Beim Einfügen hat die Nachricht die Struktur eines Tupels mit [NNr, Msg, TSclientout, TShbqin]
% Element wandert so lange durch die Liste bis es größer als Vorgaenger und kleiner als der Nachfolger ist
insertToHBQ([NNrN, MsgN, TSclientoutN, TShbqinN], []) -> [[NNrN, MsgN, TSclientoutN, TShbqinN]];
insertToHBQ([NNrN, MsgN, TSclientoutN, TShbqinN], [[NNr, Msg, TSclientout, TShbqin] | Tail]) ->
	% erstes Element wird vorne eingefügt 
	if 
		NNrN < NNr ->
			[ [NNrN, MsgN, TSclientoutN, TShbqinN] |[ [NNr, Msg, TSclientout, TShbqin] | Tail]];
		true -> 
			[[NNr, Msg, TSclientout, TShbqin]|insertToHBQ([NNrN, MsgN, TSclientoutN, TShbqinN], Tail)]
	end.