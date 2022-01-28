-module(hbqf).
-export([initHBQ/2,pushHBQ/2,deliverMSG/3,listHBQ/1,listDLQ/1,dellHBQ/1,anyM/2, dummyMsg/1, dummyHBQ/2, deliverAll/3, receiveAll/1, pushOnly/2, pushOnlyAsync/2, pushDeliver/2, pushDeliverReceive/2, pushDeliverReceive/3, pushDeliverReceiveAsync/2, pushDeliverReceiveAsync/3, pushHBQonly/2, pushHBQonlyStub/2, pushHBQonlyStub/3]).

-define (Impl, hbq_list_kris).

% HBQName, so wie er in der server.cfg unter {hbqname, eingetragen ist
% DLQSize, so wie sie in der server.cfg unter {dlqlimit, eingetragen ist
initHBQ(Impl, DLQSize,HBQName) -> 
	erlang:apply(Impl, initHBQ, [DLQSize, HBQName]).
initHBQ(DLQSize,HBQName) -> 
	initHBQ(?Impl, DLQSize,HBQName).

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
% NNr, eine legale Nachrichtennummer
% Msg, ein string
% TSclientout, ein Zeitstempel mittel erlang:timestamp() erstellt
pushHBQ(HBQ,[NNr,Msg,TSclientout]) ->
	HBQ ! {self(), {request, pushHBQ, [NNr,Msg,TSclientout]}},
	receive
		Any -> Any
	end.
	% HBQ.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
% NNr, eine legale Nachrichtennummer
% ToClient, eine PID eines Clients
deliverMSG(HBQ,NNr,ToClient) ->
	HBQ ! {self(), {request,deliverMSG,NNr,ToClient}},
	receive
		Any -> Any
	end.
	% {HBQ,Answer}.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
listHBQ(HBQ) ->
	HBQ ! {self(), {request,listHBQ}},
	receive
		Any -> Any
	end.
	% HBQ.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
listDLQ(HBQ) ->
	HBQ ! {self(), {request,listDLQ}},
	receive
		Any -> Any
	end.
	% HBQ.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
dellHBQ(HBQ) ->
	HBQ ! {self(), {request,dellHBQ}},
	receive
		Any -> Any
	end.
	% Answer.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
% Message, unterliegt keiner Bedingung
anyM(HBQ,Message) ->
	HBQ ! {self(), Message}.


% Füllt die HBQ mit Einträgen aus gegebener Liste in der Reihenfolge auf - Testfunktion
% Text ist immer "Dummy"
dummyHBQ(HBQ, []) ->
	HBQ;
dummyHBQ(HBQ, [H |T]) ->
	pushHBQ(HBQ, dummyMsg(H)),
	dummyHBQ(HBQ, T).

dummyMsg(Num) ->
	[Num, "Dummy", erlang:timestamp()].

deliverAll(_HBQ, _Client, []) ->
	ok;
deliverAll(HBQ, Client, [H|T]) ->
	deliverMSG(HBQ, H, Client),
	deliverAll(HBQ, Client, T).

receiveAll([]) ->
	ok;
receiveAll([H|T]) ->
	% io:format(user, "\nI am ~p\n", [self()]),
	receive
		% {reply, _Num} = R ->
		% 	io:format(user, "received: ~p\n", [R]),
		% 	receiveAll([H|T]);
		{reply, [H, _Message, _TimeClientOut, _TimeHbqIn, _TimeDlqIn, _TimeDlqOut], _AnyMessagesLeft} = R ->
			% io:format(user, "received: ~p\n", [R]),
			receiveAll(T)
		% ;Any ->
		% 	io:format(user, "Expected: ~lp. received: ~p", [H, Any])
	end.

receiveAllDeliberate([]) ->
	ok;
receiveAllDeliberate([_H|T]) ->
	receive
		{reply, [_Num, _Message, _TimeClientOut, _TimeHbqIn, _TimeDlqIn, _TimeDlqOut], _AnyMessagesLeft} ->
			receiveAllDeliberate(T)
		;Any ->
			io:format(user, "Received: ~p", [Any])
	end.

pushOnly(List, Impl) ->
	HBQ = initHBQ(Impl, length(List), ?FUNCTION_NAME),
	dummyHBQ(HBQ, List),
	dellHBQ(HBQ),
	ok.

pushOnlyAsync(List, Impl) ->
	HBQ = initHBQ(Impl, length(List), ?FUNCTION_NAME),
	spawn(fun() -> dummyHBQ(HBQ, List) end),
	dellHBQ(HBQ),
	ok.

pushDeliver(List, Impl) ->
	HBQ = initHBQ(Impl, length(List), ?FUNCTION_NAME),
	dummyHBQ(HBQ, List),
	Self = self(),
	spawn(fun() -> deliverAll(HBQ, Self, List) end),
	dellHBQ(HBQ),
	ok.

% pushDeliverReceive(List, Impl) ->
% 	HBQ = initHBQ(Impl, length(List) * 2, ?FUNCTION_NAME),
% 	dummyHBQ(HBQ, List),
% 	Self = self(),
% 	spawn(fun() -> deliverAll(HBQ, Self, List) end),
% 	receiveAll(List),
% 	dellHBQ(HBQ),
% 	ok.

pushDeliverReceive(List, Impl) ->
	pushDeliverReceive(List, Impl, 100).
pushDeliverReceive(List, Impl, CapacityFactorPercent) ->
	Size = ceil((CapacityFactorPercent * length(List)) / 100),
	HBQ = initHBQ(Impl, Size, ?FUNCTION_NAME),
	dummyHBQ(HBQ, List),
	Self = self(),
	DeliveryList = lists:nthtail(length(List) - Size, List),
	spawn(fun() -> deliverAll(HBQ, Self, DeliveryList) end),
	receiveAllDeliberate(DeliveryList),
	dellHBQ(HBQ),
	ok.

pushDeliverReceiveAsync(List, Impl) ->
	pushDeliverReceiveAsync(List, Impl, 100).
pushDeliverReceiveAsync(List, Impl, CapacityFactorPercent) ->
	Size = ceil((CapacityFactorPercent * length(List)) / 100),
	HBQ = initHBQ(Impl, Size, ?FUNCTION_NAME),
	Self = self(),
	DeliveryList = lists:nthtail(length(List) - Size, List),
	_Filler = spawn(fun() -> dummyHBQ(HBQ, DeliveryList), deliverAll(HBQ, Self, DeliveryList) end),
	receiveAllDeliberate(DeliveryList),
	dellHBQ(HBQ),
	ok.

% Nachricht Nr. 1 fehlt und DLQ doppelte Kapazität von Eingangsliste -> nur pushHBQ wird ausgeführt
pushHBQonly(List, Impl) ->
	DeliveryList = lists:filter(fun(Element) -> Element > 1 end, List),
	HBQ = initHBQ(Impl, length(DeliveryList) * 2, ?FUNCTION_NAME),
	dummyHBQ(HBQ, DeliveryList),
	dellHBQ(HBQ),
	ok.

% Kann verwendet werden, wenn als DLQ ein Stub verwendet wird, der Nachrichten nicht einsortiert.
pushHBQonlyStub(List, Impl) ->
	pushHBQonlyStub(List, Impl, 100).
pushHBQonlyStub(List, Impl, CapacityFactorPercent) ->
	Size = ceil((CapacityFactorPercent * length(List)) / 100),
	% DeliveryList = lists:nthtail(length(List) - Size, List),
	HBQ = initHBQ(Impl, Size, ?FUNCTION_NAME),
	dummyHBQ(HBQ, List),
	dellHBQ(HBQ),
	ok.
