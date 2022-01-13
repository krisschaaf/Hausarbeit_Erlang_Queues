-module(testFile).

-compile(export_all).

testIfElse(Var) -> 
    erlang:timestamp(),
    if 
        Var < 10 -> "kleiner zehn";
        true -> "größer gleich 10"
    end.

testIfElse2(Var)    ->
    if
        Var < 10 -> "kleiner 10";
        true -> ""
    end,
    "größer 10".

testSwitchCase(Var) ->
    case Var of
        1 -> 1;
        2 -> 2;
        _Default -> "höher 2"
end.