--------------------
Compilieren der Dateien:
--------------------
Zu dem Paket gehören die Dateien
client_func.erl; clientS.erl; cmem.erl; dlq.erl; hbq.erl; client.cfg;
server_func.erl; serverS.erl; util.erl; vsutil.erl; server.cfg;

sowie:
Readme.txt;

1> make:all().
% oder
1> c(<Dateiname>).

--------------------
Starten der Nodes:
--------------------
(w)erl -(s)name <ServerName> -setcookie zummsel
(w)erl -(s)name <hbqNode-Name> -setcookie zummsel
(w)erl -(s)name <ClientName> -setcookie zummsel

--------------------
Starten der HBQ:
--------------------
Manuell startbar mit
1> hbq:initHBQ(DLQLimit,HBQName).

Mit dem Modul hbqF lassen sich einzelne Elemente ansprechen/testen.

--------------------
Starten des Servers:
--------------------
1> serverS:start( ).

% in der server.cfg:
% {latency, 60}. Zeit in Sekunden, die der Server bei Leerlauf wartet, bevor er sich beendet
% {clientlifetime,5}. Zeitspanne, in der sich an den Client erinnert wird
% {servername, wk}. Name des Servers als Atom
% {hbqname, hbq}. Name der HBQ als Atom
% {hbqnode, '<hbqNode-Name>@<NodeName>'}. Name der Node der HBQ als Atom
% {dlqlimit, 13}. Größe der DLQ

Starten des Clients:
--------------------
1> clientS:start( ).

% 'server@lab33.cpt.haw-hamburg.de': Name der Server Node, erhält man zB über node()
% ' wegen dem - bei haw-hamburg, da dies sonst als minus interpretiert wird.
% in der client.cfg:
% {clients, 2}.  Anzahl der Clients, die gestartet werden sollen
% {lifetime, 42}. Laufzeit der Clients
% {servername, wk}. Name des Servers
% {servernode, '<ServerName>@<NodeName>'}. Node des Servers
% {sendeintervall, 3}. Zeitabstand der einzelnen Nachrichten

Runterfahren:
-------------
2> Ctrl/Strg Shift G
-->q

Rücksetzen Variablen:
-------------
1> f(<VariablenName>).
2> f().

Anzeigen aller Variablen:
-------------
1> b().

Informationen zu Prozessen bzw. Modulen:
-------------
2> observer:start().
2> process_info(PID).
2> <Module>:module_info(). 
