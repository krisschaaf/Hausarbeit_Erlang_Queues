-module(hbq_test).

-include_lib("eunit/include/eunit.hrl").

getMsgPre() -> 
    % [NNr, Msg, TSclientout]
    Msg1 = [1,"Msg", erlang:timestamp()],
    Msg2 = [2,"Msg", erlang:timestamp()],
    Msg3 = [3,"Msg", erlang:timestamp()],
    Msg4 = [4,"Msg", erlang:timestamp()],
    {Msg1, Msg2, Msg3, Msg4}.

getMsgPost() -> 
    % {NNr, Msg, TSclientout, TShbqin}
    Msg1 = {1,"Msg", erlang:timestamp(), erlang:timestamp()},
    Msg2 = {2,"Msg", erlang:timestamp(), erlang:timestamp()},
    Msg3 = {3,"Msg", erlang:timestamp(), erlang:timestamp()},
    Msg4 = {4,"Msg", erlang:timestamp(), erlang:timestamp()}
    {Msg1, Msg2, Msg3, Msg4}.

buildHBQ(String) ->
    {Msg1, Msg2, Msg3, Msg4} = getMsgPost(),
    case String of
        empty -> {};
        one -> {{Msg1, 1, {}, {}}};
        two -> {{Msg1, 1, {Msg2, 1, {}, {}}, {}}};
        three -> {{Msg1, 1, {Msg2, 1, {}, {}}, {Msg3, 1, {}, {}}}};
        four ->  {{Msg1, 1, {Msg2, 1, {Msg4, 1, {}, {}, {}}, {Msg3, 1, {}, {}}}}
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
        _Undef -> hbqPID ! {self(), {dellHBQ}}
    end.

initHBQ_test() ->
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),

    HPID = erlang:whereis(hbqPID),
    case HPID of
        undefined -> pongHBQ = false;
        _Undef -> pongHBQ = true
    end,

    DPID = erlang:whereis(dlqPID),
    case DPID of
        undefined -> pongDLQ = false;
        _Undef -> pongDLQ = true
    end,

    ?assert(pongHBQ),
    ?assert(pongDLQ),
    killIfRunning().

delHBQ_test() ->
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),

    hbqPID ! {self(), {dellHBQ}},
    receive
        {reply, Ok} -> Ok
    end,

    ?assertEqual(ok, Ok).

pushHBQ_test() ->
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),

    {MO1, MO2, MO3, MO4} = getMsgPre(),
    {MI1, MI2, MI3, MI4} = getMsgPost(),
    
    HBQ = buildHBQ(empty),
    hbqPID 

    killIfRunning().

% dafür muss pushHBQ funktionieren
deliverMSG_test() -> 
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),
    
    {Msg1,Msg2,Msg3,Msg4} = getMsgPre(),
    
    hbqPID ! {self(), {request, pushHBQ, Msg1}},
    hbqPID ! {self(), {request, pushHBQ, Msg2}},
    hbqPID ! {self(), {request, pushHBQ, Msg4}},
    % testen ob vorhandene Nachricht gesendet wird
    hbqPid ! {self(), {request, deliverMsg, 1, self()}},
    receive 
        {reply, SendeNr} -> SendeNr
    end, 
    ?assertEqual(1, SendeNr),

    % testen ob nächstgrößere Nachricht gesendet wird 
    hbqPid ! {self(), {request, deliverMsg, 3, self()}},
    receive 
        {reply, SendeNr2} -> SendeNr2
    end, 
    ?assertEqual(4, SendeNr2),

    killIfRunning().

listDLQ_test() ->
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),

    DLQ = dlq_test:createDLQ(three),

    dlqPID ! {self(), {request, listDLQ}},
    receive 
        {reply, Ok} -> Ok
    end,
    ?assertEqual(ok, Ok),
    
    killIfRunning().

listHBQ_test() ->
    killIfRunning(),
    hbq:initHBQ(2, hbqPID),

    dlqPID ! {self(), {request, listHBQ}},
    receive 
        {reply, Ok} -> Ok
    end,
    ?assertEqual(ok, Ok),
    
    killIfRunning().


