-module(dlq_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
%calling the test dlq_test:test().

createDLQ(String) ->
    case String of 
        empty -> [];
        one -> [{1, "Msg", erlang:timestamp(), erlang:timestamp()}];
        two -> [{1, "Msg", erlang:timestamp(), erlang:timestamp()}, {2, "Msg", erlang:timestamp(), erlang:timestamp()}];
        three -> [{1, "Msg", erlang:timestamp(), erlang:timestamp()}, {2, "Msg", erlang:timestamp(), erlang:timestamp()}, {3, "Msg", erlang:timestamp(), erlang:timestamp()}];
        firstMissing -> [{2, "Msg", erlang:timestamp(), erlang:timestamp()}, {3, "Msg", erlang:timestamp(), erlang:timestamp()}]
    end. 

createDLQ3TS(String) ->
    case String of 
        empty -> [];
        one -> [{1, "Msg", erlang:timestamp(), erlang:timestamp(), erlang:timestamp()}];
        two -> [{1, "Msg", erlang:timestamp(), erlang:timestamp(), erlang:timestamp()}, {2, "Msg", erlang:timestamp(), erlang:timestamp(), erlang:timestamp()}];
        three -> [{1, "Msg", erlang:timestamp(), erlang:timestamp(), erlang:timestamp()}, {2, "Msg", erlang:timestamp(), erlang:timestamp(), erlang:timestamp()}, {3, "Msg", erlang:timestamp(), erlang:timestamp(), erlang:timestamp()}];
        firstMissing -> [{2, "Msg", erlang:timestamp(), erlang:timestamp(), erlang:timestamp()}, {3, "Msg", erlang:timestamp(), erlang:timestamp(), erlang:timestamp()}]
    end. 
killIfRunning() ->
    Known = erlang:whereis(dlqPID),
    case Known of
       undefined -> notRunning;
       _NotUndef -> dlq:delDLQ(dlqTest)
    end.

initDLQ_test() ->
    dlq:initDLQ(2,"test.log"),

	case erlang:whereis(dlqPID) of
        undefined -> ?assert(false);
        _NotUndef -> ok
	end,

    dlqPID ! {self(), delDLQ},
    receive
        Return -> Return
    end,
    ?assertEqual(Return, {reply,ok}). 

expectedNr_test() -> 
    ?assertEqual(dlq:expectedNr(createDLQ3TS(empty)), 1),
    ?assertEqual(dlq:expectedNr(createDLQ3TS(one)), 2),
    ?assertEqual(dlq:expectedNr(createDLQ3TS(two)), 3).

push2DLQ_test() ->
    push2DLQ_test1(),
    push2DLQ_test2().

push2DLQ_test1() ->
    killIfRunning(),
    dlq:initDLQ(5, "test.log"),
    NewMessage = [2, "Msg", erlang:timestamp(), erlang:timestamp()],
    % Nachricht in leere Queue einfügen
    DLQ = dlq:push2DLQ(NewMessage, createDLQ3TS(empty), "test.log"),
    ?assertEqual(dlq:expectedNr(DLQ), 3),
    % Nachricht in Queue mit einem Element einfügen
    DLQ2 = dlq:push2DLQ(NewMessage, createDLQ3TS(one), "test.log"),
    ?assertEqual(dlq:expectedNr(DLQ2), 3),
    ?assertEqual(dlq:lengthDLQ(DLQ2), 2),
    ?assertEqual(dlq:listDLQ(DLQ2), [1,2]),
    killIfRunning().

push2DLQ_test2() -> 
    Datei = "test.log",
    % Initialisierung der globalen Variablen 
    killIfRunning(),
    dlq:initDLQ(2, Datei),

    Known = erlang:whereis(dlqPID),
    case Known of
       undefined -> notRunning;
       _NotUndef -> push2DLQ_test2Help(Datei)
    end.
    
push2DLQ_test2Help(Datei) -> 
    Msg1 = [1, "Msg", erlang:timestamp(), erlang:timestamp()],
    Msg2 = [2, "Msg", erlang:timestamp(), erlang:timestamp()],
    Msg3 = [3, "Msg", erlang:timestamp(), erlang:timestamp()],

    % DLQ mit einem Element erzeugen 
    DLQ1 = dlq:push2DLQ(Msg1, createDLQ(empty), Datei),

    % Testen ob sich die Größe der DLQ verändert hat 
    dlqPID ! {self(), getActSize},
    receive 
        {reply, Size} -> Size
    end,
    ?assertEqual(Size, 1),

    % weiteres Element hinzufügen
    DLQ2 = dlq:push2DLQ(Msg2, DLQ1, Datei),

    % testen, ob Größe mit der MaxSize übereinstimmt 
    dlqPID ! {self(), getVarForPush2DLQ},
    receive 
        {reply, MaxSize, ActSize} -> MaxSize, ActSize
    end,
    ?assert(MaxSize == ActSize),

    % Weiteres Element hinzufügen (vorderstes sollte entfernt werden)
    DLQ3 = dlq:push2DLQ(Msg3, DLQ2, Datei),

    % testen, ob Größe mit der MaxSize übereinstimmt 
    dlqPID ! {self(), getVarForPush2DLQ},
    receive 
        {reply, MaxSize2, ActSize2} -> MaxSize2, ActSize2
    end,
    ?assert(MaxSize2 == ActSize2),

    % testen, ob erstes Element entfernt wurde 
    ?assertEqual(dlq:listDLQ(DLQ3), dlq:listDLQ(createDLQ3TS(firstMissing))),

    killIfRunning().

deliverMSG_test() ->
    deliverMSG_test2(),
    deliverMSG_test1().

deliverMSG_test1() ->
    Datei = "test.log",
    Msg1 = [1, "Msg", erlang:timestamp(), erlang:timestamp()],
    Msg2 = [2, "Msg", erlang:timestamp(), erlang:timestamp()],
    Msg3 = [6, "Msg", erlang:timestamp(), erlang:timestamp()],

    % Initialisierung der globalen Variablen 
    killIfRunning(),
    dlq:initDLQ(3, Datei),

    % DLQ mit drei Elementen erzeugen 
    DLQ1 = dlq:push2DLQ(Msg3, dlq:push2DLQ(Msg2, dlq:push2DLQ(Msg1, createDLQ(empty), Datei), Datei), Datei),

    dlqPID ! {self(), getVarForPush2DLQ},
    receive 
        {reply, MaxSize, ActSize} -> MaxSize, ActSize
    end,
    ?assertEqual(MaxSize, 3),
    ?assertEqual(ActSize, 3),

    % Entfernen eines Elements 
    % Nachricht an einen selber schicken um return und send val zu vergleichen
    ReturnVal = dlq:deliverMSG(1, self(), DLQ1, Datei),
    receive 
        {reply, NewMsg, false} -> [MsgNr, _MsgT, _TSclientoutT, _TShbqinT, _TSdlqinT, _TSdlqoutT] = NewMsg 
    end,
    ?assertEqual(ReturnVal,MsgNr),

    % Testen ob bei nicht vorhandener Nachricht die nächst größere genommen wird 
    ReturnVal2 = dlq:deliverMSG(3, self(), DLQ1, Datei),
    ?assertEqual(ReturnVal2, 6),

    killIfRunning().

deliverMSG_test2() -> 
    Datei = "test.log",
    killIfRunning(),
    dlq:initDLQ(5, Datei),

    ReturnVal = dlq:deliverMSG(1, self(), createDLQ(empty), Datei),
    receive 
        Reply -> Reply
    end,
    ?assertEqual(ReturnVal, -1),
    ?assertEqual(Reply, {reply,[-1,nokb,0,0,0],true}),

    killIfRunning().

listDLQ_test() -> 
    % Für drei Elemente 
    DLQ = createDLQ3TS(three),
    DLQElem = dlq:listDLQ(DLQ),
    ?assertEqual(DLQElem, [1,2,3]),

    % Für leere DLQ 
    ?assertEqual(dlq:listDLQ(createDLQ3TS(empty)), []).

lengthDLQ_test() ->
    % Für drei Elemente 
    DLQ = createDLQ(three),
    DLQLength = dlq:lengthDLQ(DLQ),
    % Prüfen anhand der erlang lists Funktion 
    ?assertEqual(DLQLength, lists:flatlength(DLQ)),

    % Für leere DLQ 
    ?assertEqual(dlq:lengthDLQ(createDLQ(empty)), lists:flatlength(createDLQ(empty))).

delDLQ_test() ->
    Datei = "test.log",
    killIfRunning(),
    dlq:initDLQ(2, Datei),
    dlq:delDLQ(dlqTest),

	case erlang:whereis(dlqPID) of
        undefined -> ?assert(true);
        _NotUndef -> ?assert(false)
	end.



    
