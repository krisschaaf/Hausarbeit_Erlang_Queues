-module(dlq).

-export([initDLQ/2,expectedNr/1,push2DLQ/3]).

% Initialisieren der DLQ
initDLQ(Size, Datei) -> 
    DLQ = spawn(dlq, loop, [Size, Datei]),
    register(DlqPID, DLQ),
    %//TODO erzeuge logging Datei mit Namen Datei
    util:logging(Datei, Inhalt),
    [].

loop(SizeMax) -> 
    receive 
        {From, {expectedNr, DLQ}} -> 
            case DLQ of
                [] ->   From ! {self(), 1},
                        loop(SizeMax);
                Default ->  From ! {self(), (getLastElem(DLQ)+1)}.
        {From, {push2DLQ}, [NNr, Msg, TSclientout, TShbqin], Queue, Datei}}  -> .
    end.


% Abfrage welche Nachrichtennummer in der DLQ gespeichert werden kann
% //TODO receive syntax einfügen
expectedNr(DLQ) -> 
    DlqPID ! {self(), {expectedNr, DLQ}},
    receive
        {Pid, NNr} -> NNr
    end. 

% Speichern einer Nachricht in der DLQ
% Bei überschreitung der Größe werden die ältesten Nachrichten gelöscht 
push2DLQ([NNr, Msg, TSclientout, TShbqin], Queue, Datei) -> 
        DlqPIP ! {self(),{push2DLQ, [NNr, Msg, TSclientout, TShbqin], Queue, Datei}},
        receive
            {Pid, DQueue} -> DQueue
        end. 

getLastElem([Head|Rest]) -> getLastElem(Tail).
getLastElem([[Nnr|MsgRest]|[]]) -> Nnr. 

% Auslieferung einer Nachricht an einen Leser-Client
deliverMSG(MSGNr, ClientPID, Queue, Datei)	-> Integer.

% Inhalt der DLQ ohne Nachrichten
listDLQ(DQueue) ->	Liste.

% Größe der Delivery Queue
lengthDLQ(DQueue) -> Integer.

% löschen der Delivery Queue
delDLQ(DQueue) -> ok. 

