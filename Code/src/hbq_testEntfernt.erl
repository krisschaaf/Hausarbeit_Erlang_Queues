-module(hbq_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

getMsgPre() -> 
    % [NNr, Msg, TSclientout]
    Msg1 = [1,"Msg", 1111],
    Msg2 = [2,"Msg", 1111],
    Msg3 = [3,"Msg", 1111],
    Msg4 = [4,"Msg", 1111],
    {Msg1, Msg2, Msg3, Msg4}.

getMsgPost() -> 
    % {NNr, Msg, TSclientout, TShbqin}
    Msg1 = [1,"Msg", 1111, 1111],
    Msg2 = [2,"Msg", 1111, 1111],
    Msg3 = [3,"Msg", 1111, 1111],
    Msg4 = [4,"Msg", 1111, 1111],
    {Msg1, Msg2, Msg3, Msg4}.

createHBQ(String) ->
    {Msg1, Msg2, Msg3, Msg4} = getMsgPost(),
    case String of
        empty -> {};
        one -> { {Msg1, 1, {}, {}} };
        two -> { {Msg1, 1, {Msg2, 2, {}, {}} , {}} };
        three -> { {Msg1, 1, {Msg2, 2, {}, {}} , {Msg3, 2, {}, {}} } };
        four ->  { {Msg1, 1, {Msg2, 2, {Msg4, 3, {}, {}}, {}}, {Msg3, 2, {}, {}} } };
        firstMissing -> { Msg2, 1, {}, {}};
        secondMissing -> { Msg3, 1, {}, {}};
        twoFirstMissing -> { {Msg2, 1, {Msg3, 2, {}, {}} , {}} };
        threeFirstMissing -> { {Msg2, 1, {Msg3, 2, {}, {}} , {Msg4, 2, {}, {}} } }
    end.

killIfRunning() ->
    DPID = erlang:whereis(dlqPID),
    case DPID of
        undefined -> ok;
        _Undef -> dlq:delDLQ(dlq)
    end,

    HPID = erlang:whereis(hbqPID),
    case HPID of
        undefined -> ok;
        _Undef2 -> hbqPID ! {self(), {dellHBQ}}
    end.

initHBQ_test() ->
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),

    HPID = erlang:whereis(hbqPID),
    case HPID of
        undefined -> ?assert(false);
        _Def -> ok
    end,

    DPID = erlang:whereis(dlqPID),
    case DPID of
        undefined -> ?assert(false);
        _Def2 -> ok
    end,

    killIfRunning().

delHBQ_test() ->
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),

    hbqPID ! {self(), {dellHBQ}},
    receive
        {reply, Ok} -> Ok
    end,

    dlq:delDLQ(d),

    ?assertEqual(ok, Ok).

pushHBQ_test() ->
    pushHBQ_test1(),
    pushHBQ_test12(),
    pushHBQ_test2(),
    pushHBQ_test3().
    %pushHBQ_test4().

% Einfügen von vier Elementen testen 
% checkHBQ beachten: wenn expNrDLQ 1 ist, werden Elemente sofort an DLQ gesendet 
pushHBQ_test1() ->
    killIfRunning(),
    % Leere HBQ erzeugen    7 Elemente insgesamt -> ab 5 Elementen wird eins entfernt 
    hbq:initHBQ(7, hbqPID),

    {MO1, MO2, MO3, MO4} = getMsgPre(),

    % Leere HBQ testen
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H0, P0} -> H0, P0
    end,
    ?assertEqual(createHBQ(empty), H0), 
    ?assertEqual(1, P0),    % Index für neues Element bei leerer HBQ = 1 

    % Einfügen für ein Element testen
    hbqPID ! {self(), {request, pushHBQ, MO2}},
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H1, P1} -> H1, P1
    end,
    %?assertEqual(createHBQ(firstMissing), H1),  %TS hbqin wird hinzugefügt, kann nicht auf gleichheit geprüft werden
    ?assertEqual(2, P1),

    % Einfügen für zwei Elemente testen
    hbqPID ! {self(), {request, pushHBQ, MO3}},
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H2, P2} -> H2, P2
    end,
    %?assertEqual(createHBQ(twoFirstMissing), H2),
    ?assertEqual(3, P2),

    % Einfügen für drei Elemente testen
    hbqPID ! {self(), {request, pushHBQ, MO4}},
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H3, P3} -> H3, P3
    end,
    %?assertEqual(createHBQ(threeFirstMissing), H3),
    ?assertEqual(4, P3),

    % Einfügen für viertes Elemente testen -> Element == expNrDLQ
    % checkHBQ -> alle Elemente werden an DLQ ausgeliefert 
    hbqPID ! {self(), {request, pushHBQ, MO1}},
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H4, P4} -> H4, P4
    end,
    %?assertEqual(createHBQ(empty), H4),
    ?assertEqual(1, P4),

    killIfRunning().

pushHBQ_test12() ->
    killIfRunning(),
    % Leere HBQ erzeugen    7 Elemente insgesamt -> ab 5 Elementen wird eins entfernt 
    hbq:initHBQ(7, hbqPID),

    {MO1, MO2, MO3, MO4} = getMsgPre(),

    % Leere HBQ testen
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H0, P0} -> H0, P0
    end,
    ?assertEqual(createHBQ(empty), H0), 
    ?assertEqual(1, P0),    % Index für neues Element bei leerer HBQ = 1 

    % Einfügen für ein Element testen
    hbqPID ! {self(), {request, pushHBQ, MO4}},
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H1, P1} -> H1, P1
    end,
    %?assertEqual(createHBQ(firstMissing), H1),  %TS hbqin wird hinzugefügt, kann nicht auf gleichheit geprüft werden
    ?assertEqual(2, P1),

    % Einfügen für zwei Elemente testen
    hbqPID ! {self(), {request, pushHBQ, MO3}},
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H2, P2} -> H2, P2
    end,
    %?assertEqual(createHBQ(twoFirstMissing), H2),
    ?assertEqual(3, P2),

    % Einfügen für drei Elemente testen
    hbqPID ! {self(), {request, pushHBQ, MO2}},
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H3, P3} -> H3, P3
    end,
    %?assertEqual(createHBQ(threeFirstMissing), H3),
    ?assertEqual(4, P3),

    % Einfügen für viertes Elemente testen -> Element == expNrDLQ
    % checkHBQ -> alle Elemente werden an DLQ ausgeliefert 
    hbqPID ! {self(), {request, pushHBQ, MO1}},
    hbqPID ! {self(), getHBQPos},
    receive 
        {reply, H4, P4} -> H4, P4
    end,
    %?assertEqual(createHBQ(empty), H4),
    ?assertEqual(1, P4),

    killIfRunning().


% Prüfen, ob Nachricht eingefügt wird, wenn size < 2/3 MaxSize
pushHBQ_test2() ->
    killIfRunning(),
    % Leere HBQ erzeugen    3 Elemente insgesamt -> ab zwei Elementen wird eins entfernt 
    hbq:initHBQ(3, hbqPID),

    {_MO1, _MO2, MO3, _MO4} = getMsgPre(),

    % Testen, ob DLQ Prozess gestartet wurde 
    case erlang:whereis(dlqPID) of
        undefined -> ?assert(false);
        _Def -> ?assert(true)
    end,

    dlqPID ! {self(), getActSize},
    receive
        {reply, DS0} -> DS0
    end, 
    ?assertEqual(0, DS0),

    % HBQ mit einem Element erzeugen 
    HBQ1 = createHBQ(one),
    hbqPID ! {setHBQ, HBQ1, 2},

    % weiteres Element in HBQ einfügen -> Erstes Element wird an DLQ ausgeliefert
    hbqPID ! {self(), {request, pushHBQ, MO3}},

    % Testen, ob erstes Element aus HBQ entfernt wurde 
    hbqPID ! {self(), getHBQPos},
    receive
        {reply, H1, P1} -> H1, P1
    end, 
    %?assertEqual(createHBQ(secondMissing), H1),
    ?assertEqual(2, P1),

    % Testen, ob Element an DLQ gesendet und eingefügt wurde 
    dlqPID ! {self(), getActSize},
    receive
        {reply, DS1} -> DS1
    end, 
    ?assertEqual(1, DS1),

    killIfRunning().

% Prüfen, ob Nachricht verworfen wird wenn kleiner als expNr 
pushHBQ_test3() ->
    killIfRunning(),
    % Leere HBQ erzeugen 
    hbq:initHBQ(3, hbqPID),

    {MO1, _MO2, _MO3, _MO4} = getMsgPre(),

    % DlQ aufsetzen mit mind einem Element > 1
    D1 = dlq_test:createDLQ(firstMissing),
    hbqPID ! {self(), {setDLQ, D1}},
    dlqPID ! {self(), upperActSize},

    % Msg1 in HBQ einfügen -> dieses Element sollte verworfen werden 
    hbqPID ! {self(), {request, pushHBQ, MO1}},
 
    hbqPID ! {self(), getHBQPos},
    receive
        {reply, H1, P1} -> H1, P1
        after 1000 -> 
            ?assert(false), 
            H1 = {},
            P1 = 1
    end,
    ?assertEqual(createHBQ(empty), H1),
    ?assertEqual(1, P1),

    killIfRunning().

% Fehlermeldung testen und Lücke in DLQ auffüllen
pushHBQ_test4() -> 
    killIfRunning(),
    % Leere HBQ erzeugen: maxGröße 3, d.h zweites Element wird direkt an DLQ ausgeliefert
    hbq:initHBQ(5, hbqPID),

    {_MO1, MO2, MO3, MO4} = getMsgPre(),
    MO5 = [5, "Msg", 1111],

    % Msg2 in HBQ einfügen -> dieses Element ist aber größer als expNrDLQ 
    hbqPID ! {self(), {request, pushHBQ, MO2}},
    hbqPID ! {self(), {request, pushHBQ, MO3}}, % maxHBQSize erreicht -> erstes Element ausliefern
    hbqPID ! {self(), {request, pushHBQ, MO4}},
    hbqPID ! {self(), {request, pushHBQ, MO5}},
    % Msg1 nicht in HBQ vorhanden, also Fehlermeldung an DLQ senden
    util:logging("test.log", ""),

    % Msg1 generien und in DLQ einfügen 

    % Msg2 in HBQ
    

    % hbqPID ! {self(), getHBQPos},
    % receive
    %     {reply, H1, P1} -> H1, P1
    %     after 1000 -> 
    %         ?assert(false), 
    %         H1 = {},
    %         P1 = 1
    % end,
    % ?assertEqual(createHBQ(empty), H1),
    % ?assertEqual(1, P1),

    killIfRunning().

% dafür muss pushHBQ funktionieren
deliverMSG_test() -> 
    killIfRunning(),
    hbq:initHBQ(7, hbqPID),
    
    {Msg1,Msg2,_Msg3,Msg4} = getMsgPre(),
    
    hbqPID ! {self(), {request, pushHBQ, Msg2}},
    hbqPID ! {self(), {request, pushHBQ, Msg4}},
    % testen ob vorhandene Nachricht gesendet wird
    hbqPid ! {self(), {request, deliverMsg, 2, self()}},
    receive 
        {reply, SendeNr} -> SendeNr
        after 1000 -> 
            ?assert(false), 
            SendeNr = 1       
    end, 
    ?assertEqual(1, SendeNr),

    % testen ob nächstgrößere Nachricht gesendet wird 
    hbqPid ! {self(), {request, deliverMsg, 3, self()}},
    receive 
        {reply, SendeNr2} -> SendeNr2
        after 1000 -> 
            ?assert(false), 
            SendeNr2 = 4
    end, 
    ?assertEqual(4, SendeNr2),

    killIfRunning().

% Da das Ergebnis in log geschrieben wird, kann das genaue outcome nicht getestet werden 
listDLQ_test() ->
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),

    DLQ = dlq_test:createDLQ(three),

    dlqPID ! {self(), {request, listDLQ}},
    receive 
        {reply, Ok} -> Ok
        after 1000 -> 
            ?assert(false), 
            Ok = ok
    end,
    ?assertEqual(ok, Ok),
    
    killIfRunning().

% Da das Ergebnis in log geschrieben wird, kann das genaue outcome nicht getestet werden 
listHBQ_test() ->
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),

    dlqPID ! {self(), {request, listHBQ}},
    receive 
        {reply, Ok} -> Ok
        after 1000 -> 
            ?assert(false), 
            Ok = ok
    end,
    ?assertEqual(ok, Ok),
    
    killIfRunning().


