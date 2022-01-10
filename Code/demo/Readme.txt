Starten:
--------------------
(w)erl -(s)name wk -setcookie zummsel
1> demoRemote:start(<Name>). %<Name>: Servername
1> demoMSGQ:start().


Runterfahren:
-------------
2> Ctrl/Strg Shift G
-->q

Informationen zu Prozessen:
-------------
2> observer:start().
2> process_info(PID). % process_info(self()).

Kompilieren:
-------------
1> c(<Dateiname>).

Debugger:
-------------
1> c(<Dateiname,[debug_info]>).
2> debugger:start().
% zB First Call ankreuzen
% Module>Interpret... Modul auswählen
% Step um schrittweise durchzulaufen

Entfernter Aufruf:
-------------
1> net_adm:ping('node@ProfDrKlauck').
2> PID = {host,'node@ProfDrKlauck'}.
3> demoRemote:rpc(PID,call).

Rücksetzen Variablen:
-------------
1> f(<VariablenName>).
2> f().

Anzeigen aller Variablen:
-------------
1> b().
