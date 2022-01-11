-module(dlq).

-export([initDLQ/2,expectedNr/1,push2DLQ/3, deliverMSG/4, listDLQ/1, lengthDLQ/1, delDLQ/1]).

% Initialisieren der DLQ
% //TODO: loggen der Datei verstehen
initDLQ(Size, _Datei) -> 
    DLQ = spawn(dlq, loop, [Size]),
    register(dlqPID, DLQ),
    [].

loop(MaxSize) -> 
    receive 
        {From, {push2DLQ, [NNr, Msg, TSclientout, TShbqin], Queue, Datei}}  -> 
            TSdlqin = erlang:timestamp(),
            TempSize = getSize(0, Queue),
            if
                TempSize < MaxSize -> 
                    From ! {self(), [Queue | [Nnr, [Msg | erlang:timestamp()], TSclientout, TShbqin, TSdlqin]]};
                    %//TODO: schreibe in logging Datei;
                true ->
                    [Head | Tail] = Queue,
                    From ! {self(), [Tail | [Nnr, [Msg | erlang:timestamp()], TSclientout, TShbqin, TSdlqin]]} 
                    %//TODO: schreibe in logging Datei      
            end,
            loop(MaxSize)
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
        dlqPIP ! {self(),{push2DLQ, [NNr, Msg, TSclientout, TShbqin], Queue, Datei}},
        receive
            {_Pid, DQueue} -> DQueue
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
    %//TODO: loggen 
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

