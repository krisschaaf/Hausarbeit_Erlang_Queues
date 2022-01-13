-module(hbq).

-export([initHBQ/2]).

% Starten der HBQ auf der Node der HBQ (oder entfernt starten)
initHBQ(DLQLimit, HBQName) ->
	{ok, NodeString} = inet:gethostname(),
	Datei = "HBQ-DLQ@"++NodeString++".log",
	DLQ = dlq:initDLQ(DLQLimit, Datei),
    HBQPid = spawn(hbq, loop, [[], DLQ, Datei, 1]),	%1, da dies der derzeitige Index ist 
	register(HBQName, HBQPid),
    HBQPid.	%ProzessID wird zurückgegeben 

checkHBQ(HBQ, DLQ, Datei, Pos)	->
	case HBQ of
		[] -> util:logging(Datei, "HBQ>>> HBQ wurde komplett in DLQ uebertragen.")
	end,
	{[FirstElem|_MsgRest],_H,_LT,_RT} = HBQ,
	ExpNr = dlq:expectedNr(DLQ),
	if
		FirstElem == ExpNr ->
			{SmallestElement, NewHBQ} = removeFirst(HBQ),
			NewDLQ = dlq:push2DLQ(SmallestElement, DLQ, Datei),
			checkHBQ(NewHBQ, NewDLQ, Datei, Pos-1);
		FirstElem < ExpNr ->
			{_SmallestElement, NewHBQ} = removeFirst(HBQ),
			checkHBQ(NewHBQ, DLQ, Datei, Pos-1);
		true ->
			loop(HBQ, DLQ, Datei, Pos)
	end.

loop(HBQ, DLQ, Datei, Pos) ->
	receive
		{From, {request, pushHBQ, [NNr, Msg, TSclientout]}} ->
			ExpNr = dlq:expectedNr(DLQ),
			if 
				NNr < ExpNr ->
					From ! {reply, nnr<expNrDLQ},
					checkHBQ(HBQ, DLQ, Datei, Pos);
				true ->
					dlqPID ! {request, getMaxSize},
					receive 
						{reply, MaxSize} -> ok
					end,	
					if
						Pos-1 < (MaxSize*2/3) ->
							NewHBQ = insertToHBQ([NNr, Msg, TSclientout], HBQ, Pos),
							NNrString = util:to_String(NNr),
							util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
							From ! {reply, ok},
							checkHBQ(NewHBQ, DLQ, Datei, Pos+1);
						true -> 
							{SmallestElement, TempHBQ} = removeFirst(HBQ),
							if
								SmallestElement == ExpNr ->
									NewDLQ = dlq:push2DLQ(SmallestElement, DLQ, Datei),
									NewHBQ = insertToHBQ([NNr, Msg, TSclientout], TempHBQ, Pos),
									NNrString = util:to_String(NNr),
									util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
									From ! {reply, ok},
									checkHBQ(NewHBQ, NewDLQ, Datei, Pos);
								true ->
									[Head|_Tail] = SmallestElement,
									MSGNr = Head-1,
									TSError = erlang:timestamp(),
									ErrorLog = "**Fehlernachricht fuer Nachrichtennummern "++util:to_String(ExpNr)++" bis "++util:to_String(MSGNr)++" um "++util:to_String(TSError),
									% erst die Fehlermeldung loggen, da in push2DLQ(newMessage) die MSGNr geloggt wird 
									util:logging(Datei, "HBQ>>>Fehlernachricht fuer Nachrichten "++util:to_String(ExpNr)++" bis "++util:to_String(MSGNr)++" generiert."),
									NewMessage = [MSGNr, ErrorLog, TSError],
									TempDLQ = dlq:push2DLQ(NewMessage, DLQ, Datei),
									NewDLQ = dlq:push2DLQ(SmallestElement, TempDLQ, Datei),
									NewHBQ = insertToHBQ([NNr, Msg, TSclientout], TempHBQ, Pos),
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
			util:logging(Datei, "dlq>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)),	
			From ! {self(), ok},
			loop(HBQ, DLQ, Datei, Pos);
		% listHBQ
		{From, {request, listHBQ}} ->
			List = listHBQHelp(HBQ, []),
			ListSize = getListSize(List, 0),
			util:logging(Datei, "HBQ>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)),	
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Pos);
		% delHBQ
		{From, {dellHBQ}} -> 
			From ! {self(), ok}
	end.

%HILFSFUNKTIONEN

listHBQHelp([], List) -> List;
listHBQHelp(TempHBQ, List) -> 
	{SmallestElem, NewHBQ} = removeFirst(TempHBQ),
	listHBQHelp(NewHBQ, [List | SmallestElem]).

getListSize([], Size) -> Size;
getListSize([Head|Tail], Size) -> getListSize(Tail, Size+1).

% gibt kleinstes Element (>= ExpNr) und neue HBQ ohne dieses Element zurück
synchronizeHBQ(HBQ, ExpNr) ->
	{SmallestElem, TempHBQ} = removeFirst(HBQ),
	if 
		SmallestElem >= ExpNr -> {SmallestElem, TempHBQ};
		true -> synchronizeHBQ(TempHBQ, ExpNr)
	end.

%@returns neue Queue welche übergebenes Element enthält
insertToHBQ(NewMessage, HBQ, Pos) ->
	PosList = sortv:calcPath(Pos),
	NewHBQ = insertHBQ(HBQ, NewMessage, PosList),
	NewHBQ.

% gibt neu strukturierte HBQ mit eingefügtem Element zurück
insertHBQ(_HBQ, Elem, []) -> {Elem, 1, {}, {}};
insertHBQ({E, _Hoehe, LTB, RTB}, Elem, [r|T]) ->
    {ER, HoeheR, LTBR, RTBR} = insertHBQ(RTB, Elem, T),
    if
        ER < E ->
            {ER, max(getHoehe(LTB), HoeheR)+1, LTB, {E, HoeheR, LTBR, RTBR}};
        true ->
            {E, max(getHoehe(LTB), HoeheR)+1, LTB, {ER, HoeheR, LTBR, RTBR}}
    end;
insertHBQ({E, _Hoehe, LTB, RTB}, Elem, [l|T]) ->
    {EL, HoeheL, LTBL, RTBL} = insertHBQ(LTB, Elem, T),
    if
        EL < E ->
            {EL, max(HoeheL, getHoehe(RTB))+1, {E, HoeheL, LTBL, RTBL}, RTB};
        true ->
            {E, max(HoeheL, getHoehe(RTB))+1, {EL, HoeheL, LTBL, RTBL}, RTB}
    end.

% gibt Wurzelelement aus HBQ und neu strukturierte HBQ ohne dieses Element zurück
removeFirst(HBQ) ->
	{LastElem, NewHBQ} = removeLast(HBQ),
	case NewHBQ of
		{} -> {LastElem, {}};
		{Elem, Hoehe, LT, RT} ->
			{Elem, hbqSeep({LastElem, Hoehe, LT, RT})}
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
hbqSeep({Elem, Hoehe, {}, {ElemR, HoeheR, RTR, LTR}}) when Elem > ElemR ->
	{ElemR, Hoehe, {}, {Elem, HoeheR, RTR, LTR}};	
hbqSeep({Elem, Hoehe, {ElemL, HoeheL, RTL, LTL}, {}}) when Elem > ElemL ->
	{ElemL, Hoehe, {Elem, HoeheL, RTL, LTL}, {}};	
hbqSeep({Elem, Hoehe, {ElemL, HoeheL, RTL, LTL}, {ElemR, HoeheR, RTR, LTR}}) when  Elem > ElemR, ElemR < ElemL ->
	{ElemR, Hoehe, {ElemL, HoeheL, RTL, LTL}, hbqSeep({Elem, HoeheR, RTR, LTR})};
hbqSeep({Elem, Hoehe, {ElemL, HoeheL, RTL, LTL}, {ElemR, HoeheR, RTR, LTR}}) when Elem > ElemL ->
	{ElemL, Hoehe, hbqSeep({Elem, HoeheL, RTL, LTL}), {ElemR, HoeheR, RTR, LTR}};	
hbqSeep({Elem, Hoehe, LT, RT}) -> {Elem, Hoehe, LT, RT}.

% Gibt Hoehe des übergebenen Elements in HBQ zurueck
getHoehe({}) -> 0;
getHoehe({_E, Hoehe, _LT, _RT}) -> Hoehe.