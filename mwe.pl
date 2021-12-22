:- module(mwe, [parse_int/2]).

parse_int(Str, Num) :- number_string(Num, Str).

:- begin_tests(mwe).
:- use_module(mwe).

testcase("1", 1).
testcase("10", 10).
testcase("100000000000", 10000000000). % Oops
test("parse int", [forall(testcase(Text, Want))]) :-
  parse_int(Text, Got),
  assertion(Got = Want).

:- end_tests(mwe).
