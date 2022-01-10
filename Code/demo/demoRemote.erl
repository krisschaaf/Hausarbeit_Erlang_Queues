-module(demoRemote).
-compile(export_all).

% Name, der Name des Prozesses
start(Name) ->
	ServerPid = spawn(fun() -> loop([]) end),
    register(Name,ServerPid),
	ServerPid.

% Pid, die PID oder der Tupel {Name,Node} des Prozesses
% Query, eine zulässige Nachricht oder etwas anderes
rpc(Pid, Query) ->
     Pid ! {self(), Query},
     receive {PID,Reply} -> io:format("Received Reply from ~p:~p\n", [PID,Reply]),
                            io:format("Done\n");
	          Any -> io:format("Received Something:~p\n", [Any])
	 end.

% X nicht genutzt
loop(X) ->
     receive {{Name,Node},{centimeter,Val}} -> io:format("Received Query:~p ! ~p\n", [{Name,Node},{centimeter,Val}]),
	                        {Name,Node} ! {self() , tut:convert_length({centimeter,Val})},
							loop(X);
			 {{Name,Node},{inch,Val}} -> io:format("Received Query:~p ! ~p\n", [{Name,Node},{inch,Val}]),
	                        {Name,Node} ! {self() , tut:convert_length({inch,Val})},
							loop(X);
			 {PID,{centimeter,Val}} when is_pid(PID) -> io:format("Received Query:~p ! ~p\n", [PID,{centimeter,Val}]),
	                        PID ! {self() , tut:convert_length({centimeter,Val})},
							loop(X);
			 {PID,{inch,Val}} when is_pid(PID) -> io:format("Received Query:~p ! ~p\n", [PID,{inch,Val}]),
	                        PID ! {self() , tut:convert_length({inch,Val})},
							loop(X);
			 {PID,Query} when is_pid(PID) -> io:format("Received Query:~p ! ~p\n", [PID,Query]),
	                        PID ! {self() , Query},
							loop(X);				
	         kill -> io:format("Received kill...\n");
	         Any -> io:format("Received Something:~p\n", [Any]),
                    loop(X)
     end.
	 