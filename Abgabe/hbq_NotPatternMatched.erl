-module(hbq).

-export([initHBQ/2]).

% @author: Kristoffer Schaaf
% In dieser Implementierung der Holdback Queue wird die interne Queue als Heap umgesetzt
% Außerdem wird ohne Pattern Matching gearbeitet

% Starten der HBQ 
initHBQ(DLQLimit, HBQName) ->
	{ok, NodeString} = inet:gethostname(),
	Datei = "HB-DLQ@"++NodeString++".log",
	DLQ = dlq:initDLQ(DLQLimit, Datei),
	case erlang:whereis(dlqPID) of
		undefined -> dlqPIDnotdefined;
		_Def -> ok
	end, 
    HBQPid = spawn(fun() -> loop({}, DLQ, Datei, 1, DLQLimit) end),	%1, da dies der derzeitige Index ist 
	register(HBQName, HBQPid),
    HBQPid.	%ProzessID 
		
loop(HBQ, DLQ, Datei, Pos, DLQLimit) ->
	receive
		% pushHBQ
		{From, {request, pushHBQ, [NNr, Msg, TSclientout]}} ->
			TShbqin = erlang:timestamp(),
			HBQMsg = [NNr, Msg, TSclientout, TShbqin],
			ExpNr = dlq:expectedNr(DLQ),
			if 
				NNr < ExpNr ->	% Nachricht ist zu klein für HBQ und wird verworfen 
					From ! {reply, nnr<expNrDLQ},
					checkHBQ(HBQ, DLQ, Datei, Pos, DLQLimit);
				true ->
					if
						% Position ist die aktuelle Größe der Queue + 1
						Pos < (DLQLimit*2/3) ->
							NewHBQ = insertToHBQ(HBQMsg, HBQ, Pos),	% Nachricht wird in HBQ eingefügt und keine Nachricht muss aus HBQ entfernt werden
							NNrString = util:to_String(NNr),
							util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
							From ! {reply, ok},
							checkHBQ(NewHBQ, DLQ, Datei, Pos+1, DLQLimit);
						true -> % Limit der HBQ erreicht und kleinste Nachricht wird entfernt
							{[SNNr, _SMsg, _STSClientout, _STShbqin], TempHBQ} = removeFirst(HBQ),
							DLQMsg = [SNNr, _SMsg, _STSClientout, _STShbqin],
							if
								SNNr == ExpNr ->	% kleinste Nachricht wird an DLQ gesendet und neue Nachricht wird in HBQ eingefügt 
									NewDLQ = dlq:push2DLQ(DLQMsg, DLQ, Datei),
									NewHBQ = insertToHBQ(HBQMsg, TempHBQ, Pos),
									NNrString = util:to_String(NNr),
									util:logging(Datei, "HBQ>>> Nachricht "++NNrString++" in HBQ eingefuegt.\n"),
									From ! {reply, ok},
									checkHBQ(NewHBQ, NewDLQ, Datei, Pos, DLQLimit);
								true ->	% kleinste Nachricht der HBQ entspricht nicht der nächsten erwarteten Nummer der DLQ und Lücke wird aufgefüllt				
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
									checkHBQ(NewHBQ, NewDLQ, Datei, Pos, DLQLimit)
							end
					end
			end;
		% deliverMsg
		{From, {request, deliverMSG, NNr, ToClient}} ->
			SendeNr = dlq:deliverMSG(NNr, ToClient, DLQ, Datei),	% eigentliches Senden wird über DLQ realisiert
			From ! {reply, SendeNr},
			loop(HBQ, DLQ, Datei, Pos, DLQLimit);
		% listDLQ
		{From, {request, listDLQ}}	->	
			List = dlq:listDLQ(DLQ),
			ListSize = dlq:lengthDLQ(DLQ),
			util:logging(Datei, "dlq>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)++"\n"),	
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Pos, DLQLimit);
		% listHBQ
		{From, {request, listHBQ}} ->
			List = listHBQHelp(HBQ, []),
			ListSize = getListSize(List, 0),
			util:logging(Datei, "HBQ>>> Content("++util:to_String(ListSize)++"): "++util:to_String(List)++"\n"),	
			From ! {reply, ok},
			loop(HBQ, DLQ, Datei, Pos, DLQLimit);
		% delHBQ
		{From, {request, dellHBQ}} -> 
			From ! {reply, ok}
	end.

% Diese Funktion prüft den Inhalt der Holdback Queue auf Auslieferbarkeit 
% Wenn Elemente für die Delivery Queue geeignet sind, werden diese ausgeliefert
% Nach Terminierung dieser Funktion wird wieder die loop/5 Funktion aufgerufen, um sicherzustellen, dass der Prozess nicht beendet wird
checkHBQ(HBQ, DLQ, Datei, Pos, DLQLimit)	->
	case HBQ of
		{} -> %leere HBQ 
			util:logging(Datei, "HBQ>>> HBQ wurde komplett in DLQ uebertragen.\n"),
			loop(HBQ, DLQ, Datei, Pos, DLQLimit);
		_Default ->
			{[NNr, _Msg, _TSclientout, _TShbqin],_H,_LT,_RT} = HBQ,
			ExpNr = dlq:expectedNr(DLQ),
			if	
				NNr == ExpNr ->	% Elemente der HBQ sind kompatibel für DLQ und werden ausgeliefert
					{DLQMsg, NewHBQ} = removeFirst(HBQ),
					NewDLQ = dlq:push2DLQ(DLQMsg, DLQ, Datei),
					checkHBQ(NewHBQ, NewDLQ, Datei, Pos-1, DLQLimit); 
				NNr < ExpNr ->		% Elemente sind zu klein für DLQ und werden aus HBQ gelöscht
					{_SmallestElement, NewHBQ} = removeFirst(HBQ),
					checkHBQ(NewHBQ, DLQ, Datei, Pos-1, DLQLimit);
				true ->
					loop(HBQ, DLQ, Datei, Pos, DLQLimit)
			end
	end.
	
%-----Hilfsfunktionen-----%

% @returns Liste, welche alle Nachrichtennummern der übergebenen HBQ enthält
listHBQHelp({}, List) -> List;
listHBQHelp(TempHBQ, List) -> 
	{[NNr, _Msg, _TSclientout, _TShbqin], NewHBQ} = removeFirst(TempHBQ),
	listHBQHelp(NewHBQ, List++[NNr]).

% @returns Listengröße der übergebenen Liste 
getListSize([], Size) -> Size;
getListSize([_Head|Tail], Size) -> getListSize(Tail, Size+1).

%----Heap Funktionen----%

%@returns neue Queue welche übergebenes Element enthält
% Beim Einfügen hat die Nachricht die Struktur eines Tupels mit [NNr, Msg, TSclientout, TShbqin]
insertToHBQ(NewMessage, HBQ, Pos) ->
	PosList = calcPath(Pos),
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

% -------------------------------------------------------------------------------
% sortv.erl
% @author: Prof Dr Christoph Klauck, HAW Hamburg 

% Kodierung des Feldes: Nachfolger von Position i ist 2*i links und 2*i+1 rechts
% berechnet den Pfad zur ersten leeren Position
% l steht fuer links, r fuer rechts
% Beispiel: sort:calcPath(1). --> []
% 		sort:calcPath(2). --> [l]
% 		sort:calcPath(3). --> [r]
% 		sort:calcPath(4). --> [l,l]
% 		sort:calcPath(5). --> [l,r]
% 		sort:calcPath(6). --> [r,l] 
% 		sort:calcPath(7). --> [r,r] 
calcPath(Number) -> calcPath(Number,[]).
% aktuelle Position ist Wurzel
calcPath(1,Accu) -> Accu;
% aktuelle Position ist gerade
calcPath(Number,Accu) when Number rem 2 =:= 0 -> calcPath(Number div 2,[l|Accu]);
% aktuelle Position ist ungerade
calcPath(Number,Accu) when Number rem 2 =/= 0 -> calcPath((Number-1) div 2,[r|Accu]).	