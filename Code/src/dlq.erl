-module(dlq).

-export([initDLQ/2,expectedNr/1,push2DLQ/3, deliverMSG/4, listDLQ/1, lengthDLQ/1, delDLQ/1]).

% Initialisieren der DLQ
initDLQ(Size, Datei) -> 
    DLQ = spawn(fun() -> loop(Size, Datei, 0) end),
    register(dlqPID, DLQ),
    [].

loop(MaxSize, Datei, ActSize) -> 
    receive       
        {From, getVarForPush2DLQ} ->
            From ! {reply, MaxSize, ActSize},
            loop(MaxSize, Datei, ActSize);

        {From, getMaxSize} ->
            From ! {reply, MaxSize},
            loop(MaxSize, Datei, ActSize);
        {From, setMaxSize, NewSize} ->
            From ! {reply, setNewSize},
            loop(NewSize, Datei, ActSize);

        {From, getDatei} ->
            From ! {reply, Datei},
            loop(MaxSize, Datei, ActSize);
        {From, setDatei, NewDatei} ->
            From ! {reply, setNewDatei},
            loop(MaxSize, NewDatei, ActSize);

        {From, getActSize} ->
            From ! {reply, ActSize},
            loop(MaxSize, Datei, ActSize);
        {From, upperActSize} ->
            From ! {reply, upperActSize},
            loop(MaxSize, Datei, ActSize+1);
        {From, lowerActSize} ->
            From ! {reply, lowerActSize},
            loop(MaxSize, Datei, ActSize-1); 

        {From, delDLQ} ->
            From ! {reply, ok}      
    end.

% Beim erfolgreichen Löschen der übergebenen Queue wird ok zurückgegeben.
delDLQ(_Queue) ->   
        dlqPID ! {self(), delDLQ},
        receive 
            {reply, Return} -> Return
        end,
        Return. 

% Abfrage welche Nachrichtennummer in der DLQ gespeichert werden kann
expectedNr(DLQ) -> 
        case DLQ of
            [] ->   1;
            _Default ->  getLastElem(DLQ)+1
        end.

% Gibt das letzte Element der übergebenen Queue zurück (Queue hat die Struktur einer Liste)
getLastElem([{NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin}|[]]) -> NNr;
getLastElem([_Head|Tail]) -> getLastElem(Tail).

% Speichern einer Nachricht in der DLQ
% Bei überschreitung der Größe werden die ältesten Nachrichten gelöscht 
push2DLQ([NNr, Msg, TSclientout, TShbqin], Queue, Datei) -> 
        dlqPID ! {self(), getVarForPush2DLQ},
        receive 
            {_, MaxSize, ActSize} -> ok
        end,
        TSdlqin = erlang:timestamp(),
        NewMsg = {NNr, Msg++util:to_String(TSdlqin), TSclientout, TShbqin, TSdlqin},
        NNrString = util:to_String(NNr),
        if
            ActSize < MaxSize -> 
                NewQueue = Queue++[NewMsg],
                util:logging(Datei, "dlq>>> Nachricht "++NNrString++" in DLQ eingefuegt.\n"),
                dlqPID ! {self(), upperActSize},
                receive
                    {reply, Reply} -> Reply
                end;
            true ->
                [{NNrT, _MsgT, _TSclientoutT, _TShbqinT, _TSdlqinT} | Tail] = Queue,
                DelNNrString = util:to_String(NNrT),
                NewQueue = Tail++[NewMsg],
                util:logging(Datei, "dlq>>> Nachricht "++DelNNrString++" aus DLQ geloescht.\n"),                  
                util:logging(Datei, "dlq>>> Nachricht "++NNrString++" in DLQ eingefuegt.\n")  
        end,
        % Größe der Queue um 1 erhöhen
        NewQueue.

% Gibt die Größe der übergebenen Queue zurück (Queue hat die Struktur einer Liste)
getSize(TempSize, []) -> TempSize;
getSize(TempSize, [_Head|Tail]) -> getSize(TempSize+1, Tail).

% Auslieferung einer Nachricht an einen Leser-Client
deliverMSG(MSGNr, ClientPID, Queue, Datei)	-> 
    TSdlqout = erlang:timestamp(),
    {NNr, Msg, TSclientout, TShbqin, TSdlqin} = getMSGAtMSGNr(MSGNr, Queue),
    case NNr of
        -1 -> ClientPID ! {reply, [-1,nokb,0,0,0], true};
        _Default -> 
            ClientPID ! {reply, [NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], false}
    end,
    NNrString = util:to_String(NNr),
    ClientPIDString = util:to_String(ClientPID),
    util:logging(Datei,"dlq>>> Nachricht "++NNrString++" an Client "++ClientPIDString++" ausgeliefert.\r"),
    %Größe der Queue um 1 verkleinern
    dlqPID ! {self(), lowerActSize},
    receive
        {reply, Reply} -> Reply
    end,
    NNr. 

% Gibt die zu der MSGNr zugehörige Nachrichtenliste aus der übergebenen Queue aus 
getMSGAtMSGNr(_MSGNr, []) -> {-1,nokb,0,0,0};
getMSGAtMSGNr(MSGNr, [Head|Tail]) -> 
    {NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin} = Head,
    if
        MSGNr =< NNr -> Head;
        true -> getMSGAtMSGNr(MSGNr, Tail)
    end.

% Gibt Liste aller in der übergebenen Queue enthaltenen Nachrichtenummern zurück
listDLQ([]) -> [];
listDLQ(Queue) -> listDLQHelp(Queue, []).

% Hilfsfunktion für listDLQ/1
listDLQHelp([{NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin}|Tail], List) ->
    case Tail of
        [] -> List++[NNr];
        _Default -> listDLQHelp(Tail, List++[NNr])
    end.

% Gibt die Größe der übergebenen Queue zurück
lengthDLQ(Queue) -> getSize(0, Queue). 
