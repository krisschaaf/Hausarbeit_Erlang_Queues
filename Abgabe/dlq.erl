-module(dlq).

-export([initDLQ/2,expectedNr/1,push2DLQ/3, deliverMSG/4, listDLQ/1, lengthDLQ/1, delDLQ/1]).

% @author: Kristoffer Schaaf
% In diesem Modul ist die Implementierung einer Delivery Queue umgesetzt. Diese wird als aufsteigende FIFO Liste realisiert.

% Initialisieren der DLQ
initDLQ(Size, Datei) -> 
    util:logging(Datei, "dlq>>> initialisiert mit Kapazitaet "++util:to_String(Size)++".\n"),         
    {Size, 0, []}. %Tupel mit maximaler Größe, aktueller Größe und leerer Liste für Queue 

% Beim erfolgreichen Löschen der übergebenen Queue wird ok zurückgegeben.
delDLQ(_Queue) -> ok. 

% Abfrage welche Nachrichtennummer in der DLQ gespeichert werden kann
expectedNr({_MS, _AS, []}) -> 1;
expectedNr({_MS, _AS, DLQ}) -> getLastElem(DLQ)+1.

% Gibt das letzte Element der übergebenen Queue zurück (Queue hat die Struktur einer Liste)
getLastElem([[NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin]|[]]) -> NNr;
getLastElem([_Head|Tail]) -> getLastElem(Tail).

% Speichern einer Nachricht in der DLQ
% Bei überschreitung der Größe werden die ältesten Nachrichten gelöscht 
push2DLQ([NNr, Msg, TSclientout, TShbqin], {MaxSize, ActSize, DLQ}, Datei) when ActSize < MaxSize -> 
        TSdlqin = erlang:timestamp(),
        NewQueue = DLQ++[[NNr, Msg++util:to_String(TSdlqin), TSclientout, TShbqin, TSdlqin]],
        util:logging(Datei, "dlq>>> Nachricht "++util:to_String(NNr)++" in DLQ eingefuegt.\n"),
        {MaxSize, ActSize+1, NewQueue}; % Nachricht wurde hinzugefügt
push2DLQ([NNr, Msg, TSclientout, TShbqin], {MaxSize, ActSize, DLQ}, Datei) -> 
        TSdlqin = erlang:timestamp(),
        [[NNrT, _MsgT, _TSclientoutT, _TShbqinT, _TSdlqinT] | Tail] = DLQ,
        DelNNrString = util:to_String(NNrT),
        NewQueue = Tail++[[NNr, Msg++util:to_String(TSdlqin), TSclientout, TShbqin, TSdlqin]],
        util:logging(Datei, "dlq>>> Nachricht "++DelNNrString++" aus DLQ geloescht.\n"),                  
        util:logging(Datei, "dlq>>> Nachricht "++util:to_String(NNr)++" in DLQ eingefuegt.\n"),
        {MaxSize, ActSize, NewQueue}. % Nachricht würde hinzugefügt und erstes Element gelöscht 

% Auslieferung einer Nachricht an einen Leser-Client
deliverMSG(MSGNr, ClientPID, {_MaxSize, _ActSize, DLQ}, Datei)	-> 
    TSdlqout = erlang:timestamp(),
    [NNr, Msg, TSclientout, TShbqin, TSdlqin] = getMSGAtMSGNr(MSGNr, DLQ),
    case NNr of
        -1 -> ClientPID ! {reply, [-1,nokb,0,0,0], true};   % DLQ leer, Terminierungssignal
        _Default -> 
            ClientPID ! {reply, [NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], false}
    end,
    util:logging(Datei,"dlq>>> Nachricht "++util:to_String(NNr)++" an Client "++util:to_String(ClientPID)++" ausgeliefert.\r"),
    NNr. 

% Gibt die zu der MSGNr zugehörige Nachrichtenliste aus der übergebenen Queue zurück 
getMSGAtMSGNr(_MSGNr, []) -> [-1,nokb,0,0,0];
getMSGAtMSGNr(MSGNr, [[NNr, Msg, TSclientout, TShbqin, TSdlqin]|_Tail]) when MSGNr =< NNr-> 
    [NNr, Msg, TSclientout, TShbqin, TSdlqin];
getMSGAtMSGNr(MSGNr, [_Head|Tail]) -> getMSGAtMSGNr(MSGNr, Tail).

% Gibt Liste aller in der übergebenen Queue enthaltenen Nachrichtenummern zurück
listDLQ({_MaxSize, _ActSize, []}) -> [];
listDLQ({_MaxSize, _ActSize, DLQ}) -> listDLQHelp(DLQ, []).

% Hilfsfunktion für listDLQ/1
listDLQHelp([[NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin]|[]], List) -> List++[NNr];
listDLQHelp([[NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin]|Tail], List) -> listDLQHelp(Tail, List++[NNr]).

% Gibt die Größe der übergebenen Queue zurück
lengthDLQ({_MaxSize, ActSize, _DLQ}) -> ActSize. 
