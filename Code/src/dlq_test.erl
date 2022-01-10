-module(dlq_test).

-include_lib("eunit/include/eunit.hrl").


%calling the test dlq_test:test().

intDLQ_test() -> ?AssertEqual(dlq:initDLQ() , Queue). %pseudocode
