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
    HBQPid = spawn(fun() -> loop({}, DLQ, Datei, 1) end),	%1, da dies der derzeitige Index ist 
	register(HBQName, HBQPid),
    HBQPid.	%ProzessID wird zurückgegeben 

checkHBQ(HBQ, DLQ, Datei, Pos)	->
	case HBQ of
		{} -> 
			util:logging(Datei, "HBQ>>> HBQ wurde komplett in DLQ uebertragen.\n"),
			loop(HBQ, DLQ, Datei, Pos);
		_Default ->
			{[NNr, _Msg, _TSclientout, _TShbqin],_H,_LT,_RT} = HBQ,
			ExpNr = dlq:expectedNr(DLQ),
			if
				NNr == ExpNr ->
					{DLQMsg, NewHBQ} = removeFirst(HBQ),
					NewDLQ = dlq:push2DLQ(DLQMsg, DLQ, Datei),
					checkHBQ(NewHBQ, NewDLQ, Datei, Pos-1); 
				NNr < ExpNr ->
					{_SmallestElement, NewHBQ} = removeFirst(HBQ),
					checkHBQ(NewHBQ, DLQ, Datei, Pos-1);
				true ->
					loop(HBQ, DLQ, Datei, Pos)
			end
	end.

loop(HBQ, DLQ, Datei, Pos) ->
	receive
		{From, {request, pushHBQ, [NNr, Msg, TSclientout]}} ->
			TShbqin = erlang:timestamp(),
			HBQMsg = [NNr, Msg, TSclientout, TShbqin],
			ExpNr = dlq:expectedNr(DLQ),
			if 
				NNr < ExpNr ->
					From ! {reply, nnr<expNrDLQ},
					checkHBQ(HBQ, DLQ, Datei, Pos);
				true ->
					{MaxSize, _ActSize, _Q} = DLQ,
					if
						% Position ist die aktuelle Größe der Queue + 1
						Pos < (MaxSize*2/3) ->
							NewHBQ = insertToHBQ(HBQMsg, HBQ, Pos),
							NNrString = util:to_String(NNr),
							util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
							From ! {reply, ok},
							checkHBQ(NewHBQ, DLQ, Datei, Pos+1);
						true -> 
							{[SNNr, _SMsg, _STSClientout, _STShbqin], TempHBQ} = removeFirst(HBQ),
							DLQMsg = [SNNr, _SMsg, _STSClientout, _STShbqin],
							if
								SNNr == ExpNr ->
									NewDLQ = dlq:push2DLQ(DLQMsg, DLQ, Datei),
									NewHBQ = insertToHBQ(HBQMsg, TempHBQ, Pos),
									NNrString = util:to_String(NNr),
									util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
									From ! {reply, ok},
									checkHBQ(NewHBQ, NewDLQ, Datei, Pos);
								true ->									
									MSGNr = SNNr-1,
									TSError = erlang:timestamp(),
									ErrorLog = "**Fehlernachricht fuer Nachrichtennummern "++util:to_String(ExpNr)++" bis "++util:to_String(MSGNr)++" um "++util:to_String(TSError),
									% erst die Fehlermeldung loggen, da in push2DLQ(ErrorMessage) die MSGNr geloggt wird 
									util:logging(Datei, "HBQ>>>Fehlernachricht fuer Nachrichten "++util:to_String(ExpNr)++" bis "++util:to_String(MSGNr)++" generiert.\n"),
									ErrorMessage = [MSGNr, ErrorLog, unkn, TSError],
									TempDLQ = dlq:push2DLQ(ErrorMessage, DLQ, Datei),
									NewDLQ = dlq:push2DLQ(DLQMsg, TempDLQ, Datei),
									NewHBQ = insertToHBQ(HBQMsg, TempHBQ, Pos),
									NNrString = util:to_String(NNr),
									util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
									From ! {reply, ok},
									checkHBQ(NewHBQ, NewDLQ, Datei, Pos)
							end
					end
			end;
		% deliverMsg
		{From, {request, deliverMsg, NNr, ToClient}} ->
			SendeNr = dlq:deliverMSG(NNr, ToClient, DLQ, Datei),
			From ! {reply, SendeNr},
			loop(HBQ, DLQ, Datei, Pos);
		% listDLQ
		{From, {request, listDLQ}}	->	
			List = dlq:listDLQ(DLQ),
			ListSize = getListSize(List, 0),
			util:logging(Datei, "dlq>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)++"\n"),	
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Pos);
		% listHBQ
		{From, {request, listHBQ}} ->
			List = listHBQHelp(HBQ, []),
			ListSize = getListSize(List, 0),
			util:logging(Datei, "HBQ>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)++"\n"),	
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Pos);
		% delHBQ
		{From, {setHBQ, NewHBQ, NewPos}} ->
			From ! {reply, ok},
			loop(NewHBQ, DLQ, Datei, NewPos);
		{From, getHBQPos} ->
			From ! {reply, HBQ, Pos},
			loop(HBQ, DLQ, Datei, Pos);
		{From, {setDLQ, NewDLQ}} ->
			From ! {reply, ok},
			loop(HBQ, NewDLQ, Datei, Pos);
		{From, getDLQ} ->
			From ! {reply, DLQ},
			loop(HBQ, DLQ, Datei, Pos);
		{From, {dellHBQ}} -> 
			From ! {reply, ok}
		after 5000 ->
			if 
				Pos >= 2 -> removeAll(HBQ, DLQ, Datei, Pos);
				true -> loop(HBQ, DLQ, Datei, Pos)
			end
	end.

%HILFSFUNKTIONEN

% wird nur aufgerufen, wenn die Elemente nicht der expNr der DLQ entsprechen, also nur zwei verschiedene Fälle prüfen:
% 1): SmallestElem > expNr DLQ -> Fehlermeldung und einfügen
% 2): SmallestElem == expNr DLQ -> einfügen (nachdem Fehlermeldung erstellt wurde)
removeAll(HBQ, DLQ, Datei, Pos) ->
	if
		Pos-1 /= 0 ->
			{[SNNr, SMsg, STSClientout, STShbqin], NewHBQ} = removeFirst(HBQ),
			ExpNr = dlq:expectedNr(DLQ),
			if 
				SNNr > ExpNr ->
					MSGNr = SNNr-1,
					TSError = erlang:timestamp(),
					ErrorLog = "**Fehlernachricht fuer Nachrichtennummern "++util:to_String(ExpNr)++" bis "++util:to_String(MSGNr)++" um "++util:to_String(TSError),
					% erst die Fehlermeldung loggen, da in push2DLQ(ErrorMessage) die MSGNr geloggt wird 
					util:logging(Datei, "HBQ>>>Fehlernachricht fuer Nachrichten "++util:to_String(ExpNr)++" bis "++util:to_String(MSGNr)++" generiert.\n"),
					ErrorMessage = [MSGNr, ErrorLog, unkn, TSError],
					TempDLQ = dlq:push2DLQ(ErrorMessage, DLQ, Datei),
					NewDLQ = dlq:push2DLQ([SNNr, SMsg, STSClientout, STShbqin], TempDLQ, Datei),
					removeAll(NewHBQ, NewDLQ, Datei, Pos-1);
				true ->
					NewDLQ = dlq:push2DLQ([SNNr, SMsg, STSClientout, STShbqin], DLQ, Datei),
					removeAll(NewHBQ, NewDLQ, Datei, Pos-1)	
			end;
		true -> ok
	end.


listHBQHelp({}, List) -> List;
listHBQHelp(TempHBQ, List) -> 
	{[NNr, _Msg, _TSclientout, _TShbqin], NewHBQ} = removeFirst(TempHBQ),
	listHBQHelp(NewHBQ, List++[NNr]).

getListSize([], Size) -> Size;
getListSize([_Head|Tail], Size) -> getListSize(Tail, Size+1).

%@returns neue Queue welche übergebenes Element enthält
% Beim Einfügen hat die Nachricht die Struktur eines Tupels mit [NNr, Msg, TSclientout, TShbqin]
insertToHBQ(NewMessage, HBQ, Pos) ->
	PosList = sortv:calcPath(Pos),
	NewHBQ = insertHBQ(HBQ, NewMessage, PosList),
	NewHBQ.

% gibt neu strukturierte HBQ mit eingefügtem Element zurück
insertHBQ(_HBQ, Elem, []) -> {Elem, 1, {}, {}};
insertHBQ({[NNr, Msg, TSclientout, TShbqin], _Hoehe, LTB, RTB}, Elem, [r|T]) ->
    {[NNrR, MsgR, TSclientoutR, TShbqinR], HoeheR, LTBR, RTBR} = insertHBQ(RTB, Elem, T),
    if
        NNrR < NNr ->
            {[NNrR, MsgR, TSclientoutR, TShbqinR], max(getHoehe(LTB), HoeheR)+1, LTB, {[NNr, Msg, TSclientout, TShbqin], HoeheR, LTBR, RTBR}};
        true ->
            {[NNr, Msg, TSclientout, TShbqin], max(getHoehe(LTB), HoeheR)+1, LTB, {[NNrR, MsgR, TSclientoutR, TShbqinR], HoeheR, LTBR, RTBR}}
    end;
insertHBQ({[NNr, Msg, TSclientout, TShbqin], _Hoehe, LTB, RTB}, Elem, [l|T]) ->
    {[NNrL, MsgL, TSclientoutL, TShbqinL], HoeheL, LTBL, RTBL} = insertHBQ(LTB, Elem, T),
    if
        NNrL < NNr ->
            {[NNrL, MsgL, TSclientoutL, TShbqinL], max(HoeheL, getHoehe(RTB))+1, {[NNr, Msg, TSclientout, TShbqin], HoeheL, LTBL, RTBL}, RTB};
        true ->
            {[NNr, Msg, TSclientout, TShbqin], max(HoeheL, getHoehe(RTB))+1, {[NNrL, MsgL, TSclientoutL, TShbqinL], HoeheL, LTBL, RTBL}, RTB}
    end.

% gibt Wurzelelement aus HBQ und neu strukturierte HBQ ohne dieses Element zurück
removeFirst(HBQ) ->
	{LastMsg, NewHBQ} = removeLast(HBQ),
	case NewHBQ of
		{} -> {LastMsg, {}};
		{Elem, Hoehe, LT, RT} ->
			{Elem, hbqSeep({LastMsg, Hoehe, LT, RT})}
	end. 

% gibt Element aus HBQ mit höchstem Index und neu strukturierte HBQ ohne dieses Element zurück
removeLast({E,_H,{},{}}) -> {E, {}};
removeLast({E,_H,LT,{}}) ->      
	{NewE, NewLT} = removeLast(LT),
	{NewE, {E, getHoehe(NewLT)+1, NewLT, {} } };
removeLast({E,_H,LT,RT}) ->
	HLT = getHoehe(LT),
	HRT = getHoehe(RT),
	if
		(HLT > HRT) ->
			{NewE, NewLT} = removeLast(LT),
			{NewE, {E, max(getHoehe(NewLT), getHoehe(RT))+1, NewLT, RT} };
		true -> 
			{NewE, NewRT} = removeLast(RT),
			{NewE, {E, max(getHoehe(LT), getHoehe(NewRT))+1, LT, NewRT} }
	end.

% gibt neu strukturierte HBQ zurück
hbqSeep({}) -> {};
hbqSeep({Elem, Hoehe, {}, {}}) -> {Elem, Hoehe, {}, {}};	
hbqSeep({[NNr, Msg, TSclientout, TShbqin], Hoehe, {}, {[NNrR, MsgR, TSclientoutR, TShbqinR], HoeheR, RTR, LTR}}) when NNr > NNrR ->
	{[NNrR, MsgR, TSclientoutR, TShbqinR], Hoehe, {}, {[NNr, Msg, TSclientout, TShbqin], HoeheR, RTR, LTR}};	
hbqSeep({[NNr, Msg, TSclientout, TShbqin], Hoehe, {[NNrL, MsgL, TSclientoutL, TShbqinL], HoeheL, RTL, LTL}, {}}) when NNr > NNrL ->
	{[NNrL, MsgL, TSclientoutL, TShbqinL], Hoehe, {[NNr, Msg, TSclientout, TShbqin], HoeheL, RTL, LTL}, {}};	
hbqSeep({[NNr, Msg, TSclientout, TShbqin], Hoehe, {[NNrL, MsgL, TSclientoutL, TShbqinL], HoeheL, RTL, LTL}, {[NNrR, MsgR, TSclientoutR, TShbqinR], HoeheR, RTR, LTR}}) when  NNr > NNrR, NNrR < NNrL ->
	{[NNrR, MsgR, TSclientoutR, TShbqinR], Hoehe, {[NNrL, MsgL, TSclientoutL, TShbqinL], HoeheL, RTL, LTL}, hbqSeep({[NNr, Msg, TSclientout, TShbqin], HoeheR, RTR, LTR})};
hbqSeep({[NNr, Msg, TSclientout, TShbqin], Hoehe, {[NNrL, MsgL, TSclientoutL, TShbqinL], HoeheL, RTL, LTL}, {[NNrR, MsgR, TSclientoutR, TShbqinR], HoeheR, RTR, LTR}}) when NNr > NNrL ->
	{[NNrL, MsgL, TSclientoutL, TShbqinL], Hoehe, hbqSeep({[NNr, Msg, TSclientout, TShbqin], HoeheL, RTL, LTL}), {[NNrR, MsgR, TSclientoutR, TShbqinR], HoeheR, RTR, LTR}};	
hbqSeep({Elem, Hoehe, LT, RT}) -> {Elem, Hoehe, LT, RT}.

% Gibt Hoehe des übergebenen Elements in HBQ zurueck
getHoehe({}) -> 0;
getHoehe({_E, Hoehe, _LT, _RT}) -> Hoehe.