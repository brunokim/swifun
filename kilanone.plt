:- begin_tests(kilanone).
:- use_module(kilanone).

parse_int(`1`, int("1")).
parse_int(`10`, int("10")).
parse_int(`123_456`, int("123456")).
parse_int(`1___0`, int("10")).
parse_int(`(1)`, int("1")).
parse_int(`( 1)`, int("1")).
parse_int(`(1 )`, int("1")).
parse_int(`( 1 )`, int("1")).
parse_int(`((1))`, int("1")).
test("parse int", [nondet, forall(parse_int(Text, Want)), Got = Want]) :-
    phrase(expression(Got), Text).

parse_id(`a`, id("a")).
parse_id(`abc`, id("abc")).
parse_id(`AbC`, id("AbC")).
parse_id(`_`, id("_")).
parse_id(`_a`, id("_a")).
parse_id(`a123`, id("a123")).
parse_id(`a_123`, id("a_123")).
parse_id(`a_123_`, id("a_123_")).
parse_id(`=`, id("=")).
parse_id(`?-`, id("?-")).
parse_id(`<!>`, id("<!>")).
test("parse a", [nondet, forall(parse_id(Text, Want)), Got = Want]) :-
    phrase(expression(Got), Text).

parse_binary_operation(`a+b`, operation(op(_,_,"+"), id("a"), id("b"))).
parse_binary_operation(`a +b`, operation(op(_,_,"+"), id("a"), id("b"))).
parse_binary_operation(`a+ b`, operation(op(_,_,"+"), id("a"), id("b"))).
parse_binary_operation(`a + b`, operation(op(_,_,"+"), id("a"), id("b"))).
parse_binary_operation(`a+b+c`,
    operation(op(_,_,"+"),
        operation(op(_,_,"+"),
            id("a"),
            id("b")),
        id("c"))).
parse_binary_operation(`a+b*c`,
    operation(op(_,_,"+"),
        id("a"),
        operation(op(_,_,"*"),
            id("b"),
            id("c")))).
parse_binary_operation(`a*b+c`,
    operation(op(_,_,"+"),
        operation(op(_,_,"*"),
            id("a"),
            id("b")),
        id("c"))).
parse_binary_operation(`(a+b)*c`,
    operation(op(_,_,"*"),
        operation(op(_,_,"+"),
            id("a"),
            id("b")),
        id("c"))).
parse_binary_operation(`a+b*c+d`,
    operation(op(_,_,"+"),
        operation(op(_,_,"+"),
            id("a"),
            operation(op(_,_,"*"),
                id("b"),
                id("c"))),
        id("d"))).
parse_binary_operation(`a*b+c*d`,
    operation(op(_,_,"+"),
        operation(op(_,_,"*"),
            id("a"),
            id("b")),
        operation(op(_,_,"*"),
            id("c"),
            id("d")))).
test("parse binary operation", [nondet, forall(parse_binary_operation(Text, Want)), Got = Want]) :-
    phrase(expression(Got), Text).

parse_prefix_operation(`+a`, operation(op(_,_,"+"), id("a"))).
parse_prefix_operation(`-a`, operation(op(_,_,"-"), id("a"))).
parse_prefix_operation(`+-a`,
    operation(op(_,_,"+"),
        operation(op(_,_,"-"),
            id("a")))).
parse_prefix_operation(`-+a`,
    operation(op(_,_,"-"),
        operation(op(_,_,"+"),
            id("a")))).
parse_prefix_operation(`(+)-a`,
    operation(op(_,_,"-"), id("+"), id("a"))).
test("parse prefix operation", [nondet, forall(parse_prefix_operation(Text, Want)), Got = Want]) :-
    phrase(expression(Got), Text).

:- end_tests(kilanone).
