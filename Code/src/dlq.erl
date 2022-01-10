-module(dlq).

-export([]).

% Initialisieren der DLQ
initDLQ(Size, Datei) -> 
    DLQ = spawn(dlq, loop, [[], Size]),
    register(dlqPID, DLQ)
    %//TODO erzeuge logging Datei mit Namen Datei
    Inhalt = ...
    util:logging(Datei, Inhalt).
    [].

loop(DLQ, SizeMax) -> 
    receive 
        {From, {expectedNr, DLQ}} -> 
            From ! {self(), expectedNr(DLQ)},
            loop(DLQ, SizeMax);
        {From, {push2DLQ}}  -> ...
    end.


% Abfrage welche Nachrichtennummer in der DLQ gespeichert werden kann
% //TODO receive syntax einfügen
expectedNr(DLQ) 	-> 

    dlqPID ! {expectedNr, DLQ} -> 
        From ! {self(), ok},
    Integer.

% Speichern einer Nachricht in der DLQ
% Bei überschreitung der Größe werden die ältesten Nachrichten gelöscht 
push2DLQ([NNr, Msg, TSclientout, TShbqin], Queue, Datei) -> 
    dlqPIP ! {push2DLQ, [NNr, Msg, TSclientout, TShbqin], Queue, Datei} ->
        From ! {self(), ok};    
        DQueue.

% Auslieferung einer Nachricht an einen Leser-Client
deliverMSG(MSGNr, ClientPID, Queue, Datei)	-> Integer.

% Inhalt der DLQ ohne Nachrichten
listDLQ(DQueue) ->	Liste.

% Größe der Delivery Queue
lengthDLQ(DQueue) -> Integer.

% löschen der Delivery Queue
delDLQ(DQueue) -> ok. 

