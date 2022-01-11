-module(dlq).

-export([initDLQ/2,expectedNr/1,push2DLQ/3, deliverMSG/4, listDLQ/1, lengthDLQ/1, delDLQ/1]).

% Initialisieren der DLQ
initDLQ(Size, Datei) -> 
    DLQ = spawn(dlq, loop, [Size, Datei]),
    register(dlqPID, DLQ),
    [].

loop(MaxSize, Datei) -> 
    receive        
        {From, getMaxSize} ->
            From ! {self(), MaxSize},
            loop(MaxSize, Datei);
        {From, setMaxSize, NewSize} ->
            From ! {self(), setNewSize},
            loop(NewSize, Datei);
        {From, getDatei} ->
            From ! {self(), Datei},
            loop(MaxSize, Datei);
        {From, setDatei, NewDatei} ->
            From ! {self(), ok},
            loop(MaxSize, NewDatei)
    end.

% Beim erfolgreichen Löschen der übergebenen Queue wird ok zurückgegeben.
delDLQ(_Queue) ->   exit(dlqPID),
                    ok.

% Abfrage welche Nachrichtennummer in der DLQ gespeichert werden kann
expectedNr(DLQ) -> 
        case DLQ of
            [] ->   1;
            default ->  getLastElem(DLQ)+1
        end.

% Speichern einer Nachricht in der DLQ
% Bei überschreitung der Größe werden die ältesten Nachrichten gelöscht 
push2DLQ([NNr, Msg, TSclientout, TShbqin], Queue, Datei) -> 
        dlqPID ! {self(), getMaxSize},
        receive 
            {_Pid, Size} -> MaxSize = Size
        end,
        TSdlqin = erlang:timestamp(),
        TempSize = getSize(0, Queue),
        NNrString = util:to_String(NNr),
        if
            TempSize < MaxSize -> 
                [Queue | [NNr, [Msg | erlang:timestamp()], TSclientout, TShbqin, TSdlqin]],
                util:logging(Datei, "dlq>>> Nachricht "++NNrString++" in DLQ eingefuegt.\n");
            true ->
                [_Head | Tail] = Queue,
                [Tail | [NNr, [Msg | erlang:timestamp()], TSclientout, TShbqin, TSdlqin]],
                util:logging(Datei, "dlq>>> Nachricht "++NNrString++" in DLQ eingefuegt.\n")  
        end.

% Gibt das letzte Element der übergebenen Queue zurück (Queue hat die Struktur einer Liste)
getLastElem([_Head|Tail]) -> getLastElem(Tail);
getLastElem([[Nnr|_MsgRest]|[]]) -> Nnr. 

% Gibt die Größe der übergebenen Queue zurück (Queue hat die Struktur einer Liste)
getSize(TempSize, []) -> TempSize;
getSize(TempSize, [_Head|Tail]) -> getSize(TempSize+1, Tail).

% Auslieferung einer Nachricht an einen Leser-Client
deliverMSG(MSGNr, ClientPID, Queue, Datei)	-> 
    TSdlqout = erlang:timestamp(),
    NewMessage = getMSGAtMSGNr(MSGNr, Queue),
    [Nnr|_Tail] = NewMessage,
    case Nnr of
        -1 -> ClientPID ! {reply, [-1,nokb,0,0,0], true};
        default -> ClientPID ! {reply, [NewMessage | TSdlqout], false}
    end,
    NnrString = util:to_String(Nnr),
    ClientPIDString = util:to_String(ClientPID),
    util:logging(Datei,"dlq>>> Nachricht "++NnrString++" an Client<"++ClientPIDString++"> ausgeliefert.\r"),
    Nnr. 

% Gibt die zu der MSGNr zugehörige Nachrichtenliste aus der übergebenen Queue aus 
getMSGAtMSGNr(_MSGNr, []) -> [-1|rest];
getMSGAtMSGNr(MSGNr, [Head|Tail]) -> 
    [MsgHead|_MsgTail] = Head,
    if
        MSGNr =< MsgHead -> Head;
        true -> getMSGAtMSGNr(MSGNr, Tail)
    end.

% Gibt Liste aller in der übergebenen Queue enthaltenen Nachrichtenummern zurück
listDLQ(Queue) -> listDLQHelp(Queue, []).

% Hilfsfunktion für listDLQ/1
listDLQHelp([Head|Tail], List) ->
    [MsgHead|_MsgTail] = Head,
    case Tail of
        [] -> [List | MsgHead];
        default -> listDLQHelp(Tail, [List | MsgHead])
    end.

% Gibt die Größe der übergebenen Queue zurück
lengthDLQ(Queue) -> getSize(0, Queue). 

