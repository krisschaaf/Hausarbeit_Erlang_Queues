% These:
% Die Heap HBQ ist in allen Punkten der List HBQ überlegen, außer die Elemente werden in aufsteigend sortierter Reihenfolge eingefügt!

% Konzept für die Laufzeitmessungen:

% Allgemein:
% Start Element ist immer die 1
% Parameter: (AnzahlEinzufügenderElemente, AnzahlWiederholungenDerMessungen)

% In java:
% public int[] time() {
%     int[] Timestamps = new IntArray[AnzahlEinzufügenderElemente];

%     for(int i = 0; i < AnzahlEinzufügenderElemente; i++)    {
%         int AvgTime = 0;
%         for(int j = 0; j<AnzahlWiederholungenDerMessungen; i++) {
%             double timeStart = getTimestamp();
%             //Funktionsaufruf
%             //Signal für terminierung der Funktion
%             double timeEnde = getTimestamp();
%             AvgTime += timeEnde - timeStart;
%         }
%         AvgTime /= AnzahlWiederholungenDerMessungen;
%     }
%     Timestamps[i] = AvgTime;
% }

% In Erlang:
% Zeiten in csv Datei schreiben

printCSV(create, Filename) -> 
        {ok, File} = file:open(Filename++".csv", [write]),      %beschreibbare File wird erstellt
        io:format(File, "~s~n", ["\n"]);      %und nur mit Dateikopf beschrieben 
printCSV(Element, Filename) -> 
        {ok, File} = file:open(Filename++".csv", [write]),      
        io:format(File, "~s~n", [util:toString(Element++";"),]).     

runTime(AnzahlEinzufuegenderElemente, AnzahlWiederholungenDerMessungen) ->
    EinfEnde = AnzahlEinzufuegenderElemente,
    WhdEnde = AnzahlWiederholungenDerMessungen,

    printCSV(create, "runtime"),

    % äußere Forschleife, gibt die Anzahl der einzufügenden Elemente an 
    forOut(EinfEnde, EinfEnde, F) -> forIn();
    forOut(, EinfEnde, F) -> 
        getRuntime(),
        forOut(I+1, EinfEnde)
    .

