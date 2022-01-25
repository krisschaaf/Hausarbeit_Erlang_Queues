-module(dlq).

-export([initDLQ/2,expectedNr/1,push2DLQ/3, deliverMSG/4, listDLQ/1, lengthDLQ/1, delDLQ/1]).

% Initialisieren der DLQ
initDLQ(Size, _Datei) -> 
    {Size, 0, []}.

% Beim erfolgreichen Löschen der übergebenen Queue wird ok zurückgegeben.
delDLQ(_Queue) ->   
    ok. 

% Abfrage welche Nachrichtennummer in der DLQ gespeichert werden kann
expectedNr({_MS, _AS, DLQ}) -> 
        case DLQ of
            [] ->   1;
            _Default ->  getLastElem(DLQ)+1
        end.

% Gibt das letzte Element der übergebenen Queue zurück (Queue hat die Struktur einer Liste)
getLastElem([{NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin}|[]]) -> NNr;
getLastElem([_Head|Tail]) -> getLastElem(Tail).

% Speichern einer Nachricht in der DLQ
% Bei überschreitung der Größe werden die ältesten Nachrichten gelöscht 
push2DLQ([NNr, Msg, TSclientout, TShbqin], {MaxSize, ActSize, DLQ}, Datei) -> 
        TSdlqin = erlang:timestamp(),
        NewMsg = {NNr, Msg++util:to_String(TSdlqin), TSclientout, TShbqin, TSdlqin},
        NNrString = util:to_String(NNr),
        if
            ActSize < MaxSize -> 
                NewQueue = DLQ++[NewMsg],
                util:logging(Datei, "dlq>>> Nachricht "++NNrString++" in DLQ eingefuegt.\n"),
                {MaxSize, ActSize+1, NewQueue};
            true ->
                [{NNrT, _MsgT, _TSclientoutT, _TShbqinT, _TSdlqinT} | Tail] = DLQ,
                DelNNrString = util:to_String(NNrT),
                NewQueue = Tail++[NewMsg],
                util:logging(Datei, "dlq>>> Nachricht "++DelNNrString++" aus DLQ geloescht.\n"),                  
                util:logging(Datei, "dlq>>> Nachricht "++NNrString++" in DLQ eingefuegt.\n"),
                {MaxSize, ActSize, NewQueue}
        end.

% Auslieferung einer Nachricht an einen Leser-Client
deliverMSG(MSGNr, ClientPID, {_MaxSize, _ActSize, DLQ}, Datei)	-> 
    TSdlqout = erlang:timestamp(),
    {NNr, Msg, TSclientout, TShbqin, TSdlqin} = getMSGAtMSGNr(MSGNr, DLQ),
    case NNr of
        -1 -> ClientPID ! {reply, [-1,nokb,0,0,0], true};
        _Default -> 
            ClientPID ! {reply, [NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], false}
    end,
    NNrString = util:to_String(NNr),
    ClientPIDString = util:to_String(ClientPID),
    util:logging(Datei,"dlq>>> Nachricht "++NNrString++" an Client "++ClientPIDString++" ausgeliefert.\r"),
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
listDLQ({_MaxSize, _ActSize, []}) -> [];
listDLQ({_MaxSize, _ActSize, DLQ}) -> listDLQHelp(DLQ, []).

% Hilfsfunktion für listDLQ/1
listDLQHelp([{NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin}|Tail], List) ->
    case Tail of
        [] -> List++[NNr];
        _Default -> listDLQHelp(Tail, List++[NNr])
    end.

% Gibt die Größe der übergebenen Queue zurück
lengthDLQ({MaxSize, _ActSize, _DLQ}) -> MaxSize. 
