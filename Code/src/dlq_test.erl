-module(dlq_test).

-include_lib("eunit/include/eunit.hrl").


%calling the test dlq_test:test().

createDLQ(String) ->
    case String of 
        empty -> [];
        one -> [[1, "Msg", erlang:timestamp(), erlang:timestamp()]];
        two -> [[1, "Msg", erlang:timestamp(), erlang:timestamp()], [2, "Msg", erlang:timestamp(), erlang:timestamp()]];
        three -> [[1, "Msg", erlang:timestamp(), erlang:timestamp()], [2, "Msg", erlang:timestamp(), erlang:timestamp()], [3, "Msg", erlang:timestamp(), erlang:timestamp()]];
        gap -> [[1, "Msg", erlang:timestamp(), erlang:timestamp()], [2, "Msg", erlang:timestamp(), erlang:timestamp()], [6, "Msg", erlang:timestamp(), erlang:timestamp()]];
        firstMissing -> [[2, "Msg", erlang:timestamp(), erlang:timestamp()], [3, "Msg", erlang:timestamp(), erlang:timestamp()]]
    end. 


initDLQ_test() ->
    dlq:initDLQ(),
    Known = erlang:whereis(dlqPID),
						 case Known of
							undefined -> ReturnVal = false;
							_NotUndef -> ReturnVal = true
						 end,
    ?assertEqual(ReturnVal, true),
    dlqPID ! {self(), delDLQ},
    receive
        Return -> Return
    end,
    ?assertEqual(Return, {reply, ok}). 

expectedNr_test() -> 
    ?assertEqual(dlq:expectedNr(createDLQ(empty)), 1),
    ?assertEqual(dlq:expectedNr(createDLQ(one)), 2),
    ?assertEqual(dlq:expectedNr(createDLQ(two)), 3).

push2DLQ_test() ->
    push2DLQ_test1(),
    push2DLQ_test2().

push2DLQ_test1() ->
    NewMessage = [2, "Msg", erlang:timestamp(), erlang:timestamp()],
    % Nachricht in leere Queue einfügen
    ?assertEqual(dlq:push2DLQ(NewMessage, createDLQ(empty), "test.log"), [[2, "Msg", erlang:timestamp(), erlang:timestamp()]]),
    % Nachricht in Queue mit einem Element einfügen
    ?assertEqual(dlq:push2DLQ(NewMessage, createDLQ(one), "test.log"), [[1, "Msg", erlang:timestamp(), erlang:timestamp()], [2, "Msg", erlang:timestamp(), erlang:timestamp()]]).

push2DLQ_test2() -> 
    Datei = "test.log",
    Msg1 = [1, "Msg", erlang:timestamp(), erlang:timestamp()],
    Msg2 = [2, "Msg", erlang:timestamp(), erlang:timestamp()],
    Msg3 = [3, "Msg", erlang:timestamp(), erlang:timestamp()],

    % Initialisierung der globalen Variablen 
    dlq:initDLQ(2, Datei),

    % DLQ mit einem Element erzeugen 
    DLQ1 = dlq:push2DLQ(Msg1, createDLQ(empty), Datei),

    % Testen ob sich die Größe der DLQ verändert hat 
    dlqPID ! {self(), getActSize},
    receive 
        {ok, Size} -> Size
    end,
    ?assertEqual(Size, 1),

    % weiteres Element hinzufügen
    DLQ2 = dlq:push2DLQ(Msg2, DLQ1, Datei),

    % testen, ob Größe mit der MaxSize übereinstimmt 
    dlqPID ! {self(), getVarForPush2DLQ},
    receive 
        {ok, MaxSize, ActSize} -> ok
    end,
    ?assert(MaxSize == ActSize),

    % Weiteres Element hinzufügen (vorderstes sollte entfernt werden)
    DLQ3 = dlq:push2DLQ(Msg3, DLQ2, Datei),

    % testen, ob Größe mit der MaxSize übereinstimmt 
    dlqPID ! {self(), getVarForPush2DLQ},
    receive 
        {ok, MaxSize, ActSize} -> ok
    end,
    ?assert(MaxSize == ActSize),

    % testen, ob erstes Element entfernt wurde 
    ?assertEqual(DLQ3, createDLQ(firstMissing)),

    dlq:delDLQ(dlq).

deliverMSG_test() ->
    Datei = "test.log",
    Msg1 = [1, "Msg", erlang:timestamp(), erlang:timestamp()],
    Msg2 = [2, "Msg", erlang:timestamp(), erlang:timestamp()],
    Msg3 = [3, "Msg", erlang:timestamp(), erlang:timestamp()],

    % Initialisierung der globalen Variablen 
    dlq:initDLQ(3, Datei),

    % DLQ mit drei Elementen erzeugen 
    DLQ1 = dlq:push2DLQ(Msg3, dlq:push2DLQ(Msg2, dlq:push2DLQ(Msg1, createDLQ(empty), Datei), Datei), Datei),

    dlqPID ! {self(), getVarForPush2DLQ},
    receive 
        {ok, MaxSize, ActSize} -> ok
    end,
    ?assertEqual(MaxSize, 3),
    ?assertEqual(ActSize, 3),

    


    
