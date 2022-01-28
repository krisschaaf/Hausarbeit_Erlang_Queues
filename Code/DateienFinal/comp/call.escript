#!/usr/bin/env escript


% main(Args) ->
main([Mod, Func, Args]) ->
	{ok,Tokens,_} = erl_scan:string(Args ++ "."),
	{ok, ParsedArgs} = erl_parse:parse_term(Tokens),
	% io:format("~p\n", [ParsedArgs]),
	io:format("~p", [apply(list_to_atom(Mod), list_to_atom(Func), ParsedArgs)]).
	% apply(list_to_atom(Mod), list_to_atom(Func), ParsedArgs).
