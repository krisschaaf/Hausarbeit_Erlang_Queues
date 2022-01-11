-module(util).
%-compile(export_all).
-export([list2set/1,shuffle/1,list2string/1,slist_tointlist/1,writelist/2,readlist/1,
         sortliste/1,resortliste/1,randomliste/1,randomliste/3,
		 attachStamp/2,attachStamp/1,attachEnding/2,to_String/1,
		 float_to_int/1,atom_to_integer/1,floor/1,ceil/1,fib/1,even/1,odd/1,type_is/1,
		 counting1/1,counting/2,countread/1,countreset/1,countstop/1,
		 globalvar/1,setglobalvar/2,getglobalvar/1,globalvarreset/1,globalvarstop/1,
		 logging/2,logstop/0,timeMilliSecond/0]).

-define(MILL, 1000000).
-define(TAUS, 1000).
-define(ZERO, integer_to_list(0)).

%% -------------------------------------------
% entfernt Duplikate in der Liste
%
list2set([])    -&gt; [];
list2set([H|T]) -&gt; [H | [X || X &lt;- list2set(T), X /= H]].	

%% Mischt eine Liste
% Beispielaufruf: NeueListe = shuffle([a,b,c]),
shuffle(List) -&gt; PList = [{rand:uniform(),Elem} || Elem &lt;- List],
                 [Elem || {_,Elem}&lt;- lists:keysort(1,PList)].

%% Wandelt Liste in eine Zeichenketten Liste um
% Beispiel: util:list2string([1,2,3,4,5]). --&gt; "1 2 3 4 5 \n"
list2string([]) -&gt; "\n";
list2string([H|T]) -&gt; lists:concat([H," ",list2string(T)]).
% transformiert die Liste von Zeichenketten in eine Liste von Integer	
% Beispiel: util:slist_tointlist(["1","2","3","4","5"]). --&gt; [1,2,3,4,5]					
slist_tointlist([]) -&gt; [];
slist_tointlist([SInt|STail]) -&gt; 
        {IntS,Case} = string:to_integer(SInt), 
        case IntS of
			error -&gt; io:format("in slist_tointlist: Fehler beim transformieren: ~p\n",[Case]);
            _Any -&gt; [IntS|slist_tointlist(STail)] 
		end.

% Schreibt eine Liste von Zahlen in eine Datei
%writelist(List,Filename) -&gt; file:write_file(Filename,io_lib:format("~w",[List])).
writelist([H | T],Filename) when is_number(H) -&gt;
	{ok, IODevice} = file:open(Filename, write),
    ok = io:format(IODevice, "[~.10B",[H]),
	write_list(IODevice, T),
	file:close(IODevice);
writelist([_H | T],Filename) -&gt;
	{ok, IODevice} = file:open(Filename, write),
    ok = io:format(IODevice, "[~.10B",[0]),
	write_list(IODevice, T),
	file:close(IODevice).
write_list(IODevice, [H | T]) when is_number(H) -&gt;
    ok = io:format(IODevice, ",~.10B",[H]),
    write_list(IODevice, T);
write_list(IODevice, [_H | T]) -&gt;
    ok = io:format(IODevice, ",~.10B",[0]),
    write_list(IODevice, T);
write_list(IODevice, []) -&gt;
	ok = io:format(IODevice, "]",[]).

% Liest eine solche Liste von Zahlen aus einer Datei
readlist(Filename) -&gt; {Sign,ListBinary} = file:read_file(Filename),
                        case Sign of
					       ok -&gt; slist_tointlist(string:tokens(binary_to_list(ListBinary),"[],")); 
						   error -&gt; io:format("in readlist: Fehler beim Lesen von ~p: ~p\n",[Sign,ListBinary])
					    end.
	
%% -------------------------------------------
% Erzeugt eine sortierte Liste mit Num Zahlen
% beginnend bei 1 bis einschlieÃŸlich Num
%
sortliste(Num) -&gt;
	lists:seq(1, Num).
% Erzeugt eine umgekehrt sortierte Liste mit Num Zahlen 
% beginnend bei Num bis einschlieÃŸlich 1
resortliste(Num) -&gt;
	lists:reverse(lists:seq(1, Num)).
% Erzeugt eine unsortierte Liste mit Num Zufallszahlen im Bereich 1 bis Num
% ohne Duplikate 
randomliste(Num) -&gt;
    shuffle([X || X &lt;- lists:seq(1, Num)]).
% Erzeugt eine unsortierte Liste mit Num Zufallszahlen im Bereich Min bis Max
% Duplikate sind mÃ¶glich 
randomliste(Num,Min,Max) -&gt;
	RangeInt = Max-Min,
	lists:flatten([rand:uniform(RangeInt+1) + Min-1 || _ &lt;- lists:seq(1, Num)]).
	
%% -------------------------------------------
% setzt einen Zeitstempel an einen Namen und verbindet dies mit der Endung.
% Beispielaufruf: util:attachStamp(name,svg). --&gt; 'name163025.svg'
%                 util:attachStamp('Name','JPG'). --&gt; 'Name177902.JPG'
attachStamp(AtomName,FileEndung) -&gt;
		{_MegaSecs, _Secs, MicroSecs} = erlang:timestamp(),
		Stamp = max(MicroSecs rem ?TAUS,MicroSecs div ?TAUS),
		list_to_atom(lists:concat([AtomName, to_String(Stamp),".", FileEndung])).
% setzt einen Zeitstempel an einen Namen.
% Beispielaufruf: util:attachStamp(name). --&gt; name334000
%                 util:attachStamp('Name'). --&gt; 'Name991000'
attachStamp(AtomName) -&gt;
		{_MegaSecs, _Secs, MicroSecs} = erlang:timestamp(),
		Stamp = max(MicroSecs rem ?TAUS,MicroSecs div ?TAUS),
		list_to_atom(lists:concat([AtomName, to_String(Stamp)])).
% setzt Endung an einen Namen ohne Zeitstempel.
% Beispielaufruf: util:attachEnding(name,svg). --&gt; 'name.svg'
%                 util:attachEnding('Name','JPG'). --&gt; 'Name.JPG'
attachEnding(AtomName,FileEndung) -&gt;
		list_to_atom(lists:concat([AtomName,".", FileEndung])).

% Wandelt in eine Zeichenkette um
% Beispielaufruf: to_String(Something),
%
to_String(Etwas) -&gt;
	lists:flatten(io_lib:format("~p", [Etwas])).	

%% Transformiert float nach int
% gerundet wird kaufmÃ¤nisch: a.44 bzw. a.444 ergibt a, a.45 bzw. a.445 ergibt a+1
%
float_to_int(Float) -&gt; list_to_integer(float_to_list(Float, [{decimals, 0}])).

%% Transformiert atom Zahl nach Integer
% Bsp: atom_to_integer('42') --&gt; 42
%
atom_to_integer(X) -&gt; list_to_integer(atom_to_list(X)).

% rundet die Zahl ab
% -a.999999999999999 wird auf -(a+1) gerundet
% a.999999999999999 wird auf a gerundet
floor(X) -&gt;
	T = erlang:trunc(X),
	case (X - T) of
		Neg when Neg &lt; 0 -&gt; T - 1;
		Pos when Pos &gt; 0 -&gt; T;
		_ -&gt; T
	end.

% rundet die Zahl auf
% -a.999999999999999 wird auf -a gerundet
% a.999999999999999 wird auf a+1 gerundet
ceil(X) -&gt;
	T = erlang:trunc(X),
	case (X - T) of
		Neg when Neg &lt; 0 -&gt; T;
		Pos when Pos &gt; 0 -&gt; T + 1;
		_ -&gt; T
	end.

%% Fibonacci 2-ter Ordnung
fib(N) -&gt; fib_iter(N, 0, 1).
% iterative Implementierung
fib_iter(0, Result, _Next) -&gt; Result;
fib_iter(Iter, Result, Next) when Iter &gt; 0 -&gt;
	fib_iter(Iter-1, Next, Result+Next).		
	
%% bestimmt, ob die Zahl gerade oder ungerade ist
even(Integer) -&gt; (Integer &gt;= 0) and (Integer rem 2 =:= 0).
odd(Integer) -&gt; (Integer &gt;= 1) and (Integer rem 2 =/= 0). 
	
% Ermittelt den Typ
% Beispielaufruf: type_is(Something),
%
type_is(Something) -&gt;
    if is_atom(Something) -&gt; atom;
	   is_binary(Something) -&gt; binary;
	   is_bitstring(Something) -&gt; bitstring;
	   is_boolean(Something) -&gt; boolean;
	   is_float(Something) -&gt; float;
	   is_function(Something) -&gt; function;
	   is_integer(Something) -&gt; integer;
	   is_list(Something) -&gt; list;
	   is_number(Something) -&gt; number;
	   is_pid(Something) -&gt; pid;
	   is_port(Something) -&gt; port;
	   is_reference(Something) -&gt; reference;
	   is_tuple(Something) -&gt; tuple
	end.
	
%% -------------------------------------------
% Ein globaler ZÃ¤hler
%
%% Addiert 1
counting1(Counter) -&gt; counting(Counter,1).

%% Addiert Step						 
counting(Counter,Step) -&gt; %Known = erlang:whereis(Counter),
						% case Known of
						%	undefined -&gt; PIDcountklc = spawn(fun() -&gt; countloop(0) end),
						%				 erlang:register(Counter,PIDcountklc);
						%	_NotUndef -&gt; ok
						% end,
						% Counter ! {count,Step},
						% ok.
						case getglobalvar(Counter) of
							nil -&gt; setglobalvar(Counter,Step);
							Num -&gt; setglobalvar(Counter,max(Num,0)+Step)
						end.
%% Auslesen des Wertes
countread(Counter) -&gt; %Known = erlang:whereis(Counter),
						%case Known of
						%	undefined -&gt; 0;
						%	_NotUndef -&gt; 
						%		Counter ! {get,self()},
						%		receive
						%			{current,Num} -&gt; Num;
						%			_SomethingElse -&gt; 0
						%		end
						%end.
						max(getglobalvar(Counter),0).
%% Setzt Wert auf 0
countreset(Counter) -&gt; 	%Known = erlang:whereis(Counter),
						%case Known of
						%	undefined -&gt; PIDcountklc = spawn(fun() -&gt; countloop(0) end),
						%				erlang:register(Counter,PIDcountklc);
						%	_NotUndef -&gt; Counter ! reset, true
						%end.
						 setglobalvar(Counter,0).
%% Beendet den ZÃ¤hlprozess
countstop(Counter) -&gt; 	%Known = erlang:whereis(Counter),
						%case Known of
						%	undefined -&gt; false;
						%	_NotUndef -&gt; Counter ! kill,
						%				erlang:unregister(Counter),
						%				true
						%end.
						globalvarstop(Counter).
%% Der nebenlÃ¤ufige Prozess					
%countloop(Count) -&gt; receive
%						{count,Num} -&gt; countloop(Count + Num);
%						{get,PID} -&gt; PID ! {current,Count},
%									countloop(Count);
%						reset -&gt; countloop(0);
%						kill -&gt; true
%					end.

%% -------------------------------------------
%% Eine globale Variable
%
% startet Prozess und setzt die Variable auf den Wert nil
globalvar(VariableName) -&gt; Known = erlang:whereis(VariableName),
						 case Known of
							undefined -&gt; PIDcountklc = spawn(fun() -&gt; glvarloop(nil) end),
										 erlang:register(VariableName,PIDcountklc);
							_NotUndef -&gt; ok
						 end,
						 ok.
% Setzt die Variable auf einen Wert
setglobalvar(VariableName,Value) -&gt; Known = erlang:whereis(VariableName),
						 case Known of
							undefined -&gt; PIDcountklc = spawn(fun() -&gt; glvarloop(nil) end),
										 erlang:register(VariableName,PIDcountklc);
							_NotUndef -&gt; ok
						 end,
						 VariableName ! {writevar,Value},
						 ok.
% Liest den Wert aus der Variablen aus
getglobalvar(VariableName) -&gt; Known = erlang:whereis(VariableName),
						case Known of
							undefined -&gt; nil;
							_NotUndef -&gt; 
								VariableName ! {get,self()},
								receive
									{current,Value} -&gt; Value;
									_SomethingElse -&gt; nil
								end
						end.
% Setzt den Wert der Variablen auf nil
globalvarreset(VariableName) -&gt; 	Known = erlang:whereis(VariableName),
				case Known of
					undefined -&gt; PIDcountklc = spawn(fun() -&gt; glvarloop(nil) end),
								 erlang:register(VariableName,PIDcountklc);
					_NotUndef -&gt; VariableName ! reset
				end,
				true.
% Beendet den nebenlÃ¤ufigen Prozess 
globalvarstop(VariableName) -&gt; 	Known = erlang:whereis(VariableName),
				case Known of
					undefined -&gt; false;
					_NotUndef -&gt; VariableName ! kill,
								 erlang:unregister(VariableName),
								 true
				end.
					
glvarloop(Value) -&gt; receive
						{writevar,NewValue} -&gt; glvarloop(NewValue);
						{get,PID} -&gt; PID ! {current,Value},
									glvarloop(Value);
						reset -&gt; glvarloop(nil);
						kill -&gt; true
					end.
					
%% -------------------------------------------
% Schreibt ggf auf den Bildschirm und in eine Datei
% nebenlÃ¤ufig zur Beschleunigung
% Beispielaufruf: logging('FileName.log',"Textinhalt"),
%
% logging(_Datei,_Inhalt) -&gt; ok;
% Schreibt Inhal in Datei, nebenlÃ¤ufig!
logging(Datei,Inhalt) -&gt; Known = erlang:whereis(logklc),
						 case Known of
							undefined -&gt; PIDlogklc = spawn(fun() -&gt; logloop(0) end),
										 erlang:register(logklc,PIDlogklc);
							_NotUndef -&gt; ok
						 end,
						 logklc ! {Datei,Inhalt},
						 ok.
% Beendet den nebenlÃ¤ufigen Prozess
logstop( ) -&gt; 	Known = erlang:whereis(logklc),
				case Known of
					undefined -&gt; false;
					_NotUndef -&gt; logklc ! kill, 
								erlang:unregister(logklc),
								true
				end.
% der nebenlÃ¤ufige Prozess					
logloop(Y) -&gt; 	receive
					{Datei,Inhalt} -&gt; shell:strings(false),
									  io:format(Inhalt),
									  file:write_file(Datei,Inhalt,[append]),
									  logloop(Y+1);
					kill -&gt; true
				end.

%% Zeitstempel: 'MM.DD HH:MM:SS,SSS'
% Beispielaufruf: Text = lists:concat([Clientname," Startzeit: ",timeMilliSecond()]),
%
timeMilliSecond() -&gt;
	{_Year, Month, Day} = date(),
	{Hour, Minute, Second} = time(),
	Tag = lists:concat([klebe(Day,""),".",klebe(Month,"")," ",klebe(Hour,""),":"]),
	{_, _, MicroSecs} = erlang:timestamp(),
	Tag ++ concat([Minute,Second],":") ++ "," ++ toMilliSeconds(MicroSecs)++"|".
% Hilfsfunktionen
toMilliSeconds(MicroSecs) -&gt;
	Seconds = MicroSecs / ?MILL,
	%% Korrektur, da string:substr( float_to_list(0.234567), 3, 3). 345 ergibt
	if (Seconds &lt; 1) -&gt; CorSeconds = Seconds + 1;
	   (Seconds &gt;= 1) -&gt; CorSeconds = Seconds
	end,
	string:substr( float_to_list(CorSeconds), 3, 3).
concat(List, Between) -&gt; concat(List, Between, "").
concat([], _, Text) -&gt; Text;
concat([First|[]], _, Text) -&gt;
	concat([],"",klebe(First,Text));
concat([First|List], Between, Text) -&gt;
	concat(List, Between, string:concat(klebe(First,Text), Between)).
klebe(First,Text) -&gt; 	
	NumberList = integer_to_list(First),
	string:concat(Text,minTwo(NumberList)).	
minTwo(List) -&gt;
	case {length(List)} of
		{0} -&gt; ?ZERO ++ ?ZERO;
		{1} -&gt; ?ZERO ++ List;
		_ -&gt; List
	end.