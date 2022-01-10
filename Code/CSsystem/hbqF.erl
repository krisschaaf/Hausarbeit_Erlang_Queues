-export([initHBQ/2,pushHBQ/2,deliverMSG/3,listHBQ/1,listDLQ/1,dellHBQ/1,anyM/2]).

% HBQName, so wie er in der server.cfg unter {hbqname, eingetragen ist
% DLQSize, so wie sie in der server.cfg unter {dlqlimit, eingetragen ist
initHBQ(DLQSize,HBQName) -&gt; 
	hbq:initHBQ(DLQSize,HBQName).

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
% NNr, eine legale Nachrichtennummer
% Msg, ein string
% TSclientout, ein Zeitstempel mittel erlang:timestamp() erstellt
pushHBQ(HBQ,[NNr,Msg,TSclientout]) -&gt;
	HBQ ! {self(), {request,pushHBQ,[NNr,Msg,TSclientout]}},
	receive
		{reply, _Answer} -&gt; done
	end,
	HBQ.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
% NNr, eine legale Nachrichtennummer
% ToClient, eine PID eines Clients
deliverMSG(HBQ,NNr,ToClient) -&gt;
	HBQ ! {self(), {request,deliverMSG,NNr,ToClient}},
	receive
		{reply, Answer} -&gt; done
	end,
	{HBQ,Answer}.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
listHBQ(HBQ) -&gt;
	HBQ ! {self(), {request,listHBQ}},
	receive
		{reply, _Answer} -&gt; done
	end,
	HBQ.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
listDLQ(HBQ) -&gt;
	HBQ ! {self(), {request,listDLQ}},
	receive
		{reply, _Answer} -&gt; done
	end,
	HBQ.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
dellHBQ(HBQ) -&gt;
	HBQ ! {self(), {request,dellHBQ}},
	receive
		{reply, Answer} -&gt; done
	end,
	Answer.

% HBQ, die PID der HBQ oder das Tupel {HBQName,HBQnode}, wie sie in der server.cfg eingetragen sind
% Message, unterliegt keiner Bedingung
anyM(HBQ,Message) -&gt;
	HBQ ! {self(), Message}.