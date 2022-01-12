-module(dlq).

-export([initDLQ/2,expectedNr/1,push2DLQ/3, deliverMSG/4, listDLQ/1, lengthDLQ/1, delDLQ/1]).

% Initialisieren der DLQ
initDLQ(Size, Datei) -> 
    DLQ = spawn(dlq, loop, [Size, Datei, 0]),
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
            loop(MaxSize, Datei, ActSize-1)         
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
        dlqPID ! {self(), getVarForPush2DLQ},
        receive 
            {_, MaxSize, ActSize} -> ok
        end,
        TSdlqin = erlang:timestamp(),
        % ActSize = getSize(0, Queue),
        NNrString = util:to_String(NNr),
        if
            ActSize < MaxSize -> 
                NewQueue = [Queue | [NNr, [Msg | erlang:timestamp()], TSclientout, TShbqin, TSdlqin]],
                util:logging(Datei, "dlq>>> Nachricht "++NNrString++" in DLQ eingefuegt.\n");
            else ->
                [Head | Tail] = Queue,
                DelNNrString = util:to_String(Head),
                NewQueue = [Tail | [NNr, [Msg | erlang:timestamp()], TSclientout, TShbqin, TSdlqin]],
                util:logging(Datei, "dlq>>> Nachricht "++DelNNrString++" aus DLQ geloescht.\n"),                  
                util:logging(Datei, "dlq>>> Nachricht "++NNrString++" in DLQ eingefuegt.\n")  
        end,
        % Größe der Queue um 1 erhöhen
        dlqPID ! {self(), upperActSize},
        receive
            {_, Reply} -> Reply
        end,
        NewQueue.

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
    %Größe der Queue um 1 verkleinern
    dlqPID ! {self(), lowerActSize},
    receive
        {_, Reply} -> Reply
    end,
    Nnr. 

% Gibt die zu der MSGNr zugehörige Nachrichtenliste aus der übergebenen Queue aus 
getMSGAtMSGNr(_MSGNr, []) -> [-1|rest];
getMSGAtMSGNr(MSGNr, [Head|Tail]) -> 
    [MsgHead|_MsgTail] = Head,
    if
        MSGNr =< MsgHead -> Head;
        else -> getMSGAtMSGNr(MSGNr, Tail)
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
