-module(bench).
-export([mark/3, mark/4, markMS/3, markFull/5, markFull/6]).

mark(Mod, Fun, Args, a) ->
	[atom_to_list(Mod) ++ ":" ++ atom_to_list(Fun) ++ ("(") ++ util:listTOstring((Args)) ++ ") took " ++ integer_to_list(trunc(A/1000)) ++ " ms" || {A,_} <- [timer:tc(Mod, Fun, Args)]].

mark(Mod, Fun, Args) ->
	[atom_to_list(Mod) ++ ":" ++ atom_to_list(Fun) ++ " took " ++ integer_to_list(trunc(A/1000)) ++ " ms" || {A,_} <- [timer:tc(Mod, Fun, Args)]].

%% Returns duration in ms for given call
markMS(Mod, Fun, Args) ->
	{Time, _} = timer:tc(Mod, Fun, Args),
	trunc(Time/1000).

% Convenience
markFull(QtyStart, Step, QtySteps, Mod, Fun) ->
	markFull(QtyStart, Step, QtySteps, Mod, Fun, []).

% Start measurements for a single file, when no list is in AddArgs, create one
markFull(QtyStart, Step, QtySteps, Mod, Fun, AddArgs) ->
	case AddArgs of 
		[ListArg |RestArgs] ->
			case ListArg of
				auf ->
					StringArgs = "auf" ++ "_" ++ util:to_String(RestArgs),
					List = util:sortliste(QtyStart + (Step * (QtySteps)));
				ab -> 
					StringArgs = "ab" ++ "_" ++ util:to_String(RestArgs),
					List = util:resortliste(QtyStart + (Step * (QtySteps)));
				rand ->
					StringArgs = "rand" ++ "_" ++ util:to_String(RestArgs),
					List = util:randomliste(QtyStart + (Step * (QtySteps)));
				real ->
					StringArgs = "real" ++ "_" ++ util:to_String(RestArgs),
					List = kindaSortedList(QtyStart + (Step * (QtySteps)));
				% {rand, PercentDuplicates} ->
				% 	StringArgs = "rand" ++ integer_to_list(PercentDuplicates) ++ "_" ++ "_" ++ util:to_String(RestArgs),
				% 	List = util:randomliste(QtyStart + (Step * (QtySteps)), 1, floor(((100 - PercentDuplicates) / 100) * (QtyStart + (Step * (QtySteps)))));
				ProbablyList ->
					StringArgs = "" ++ util:to_String(RestArgs),
					List = ProbablyList
				end
	end,
	Args = [List |RestArgs],
	File = util:attachStamp(util:to_String(Fun) ++ "_" ++ StringArgs, "csv"),
	util:logS(File, "# ~lp / ~lp / ~lp / ~p\n", [QtyStart, Step ,QtySteps, StringArgs]),
	markFull_(File, QtyStart, Step, QtySteps, Mod, Fun, Args),
	timer:sleep(100),	% finish up file writing 
	File.

% Finished
markFull_(_File, _QtyStart, _Step, -1, _Mod, _Fun, _AddArgs) ->
	ok;
% Run single benchmark
markFull_(File, QtyStart, Step, QtySteps, Mod, Fun, [List |RestArgs]  = Args) ->
	Time = markMS(Mod, Fun, Args),
	util:logS(File, "~lp; ~lp\n", [QtyStart + (Step * (QtySteps)), Time]),
	% markFull_(File, QtyStart, Step, QtySteps - 1, Mod, Fun, [deleteFirstNFromList(List, Step) |RestArgs]).
	Max = lists:max(List),
	NewList = lists:filter(fun(Elem) -> Elem =< (Max - Step) end, List),
	markFull_(File, QtyStart, Step, QtySteps - 1, Mod, Fun, [NewList |RestArgs]).


deleteFirstNFromList([], _Num) ->
	[];
deleteFirstNFromList(List, 0) ->
	List;
deleteFirstNFromList([_H|T], N) ->
	deleteFirstNFromList(T, N-1).

% Gibt eine fast sortierte Liste zurück
kindaSortedList(Size) when is_integer(Size) ->
	kindaSortedList(util:sortliste(Size));
kindaSortedList([])->
	[];
kindaSortedList([A])->
	[A];
kindaSortedList([A, B |T]) ->
	[NewA, NewB] = case rand:uniform(2) == 1 of false -> [B, A]; true -> [A, B] end,
	[NewA |kindaSortedList([NewB |T])].