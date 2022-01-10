-module(demoMSGQ).
-export([start/0]).

start( ) -> 
    Datei = "demoMSGQ.log",
    logging(Datei,"Start..."),
	Server = spawn(fun() -> logging(Datei,"+++"),loop(Datei,0),logging(Datei,"###\r\n") end),
    Text = "... done:"++pid_to_list(Server)++"\r\n",
    logging(Datei,Text),
	Server ! muster,
	Server ! muster,
	Server ! muster,
	Server ! keinmuster,
	Server ! keinmuster,
	Server ! test,
	Server ! keinmuster,
	Server ! muster,
	Server ! del,
	Server ! muster,
	Server ! naechster.

loop(Datei,N) -> 
	receive
		naechster ->
			Text = lists:concat([" naechster ",N,"|\r\n"]),
			logging(Datei,Text),
			leer(Datei,32);
		muster -> 			
			Text = lists:concat([" muster ",N,"|\r\n"]),
			logging(Datei,Text),
			loop(Datei,N+1)
	end.

leer(Datei,N) -> 
	receive
		del ->
			Text = lists:concat([" del ",N,"|\r\n"]),
			logging(Datei,Text),
			timer:sleep(2000);
		test ->
			Text = lists:concat([" test ",N,"|\r\n"]),
			logging(Datei,Text),
			leer(Datei,N+1);			
		Anything -> 			
			Text = lists:concat([" any ",N,"--",Anything,"|\r\n"]),
			logging(Datei,Text),
			leer(Datei,N+1)
	end.

logging(Datei,Text) ->
    file:write_file(Datei,Text,[append]),
    io:format(Text).
