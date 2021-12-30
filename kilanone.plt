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

parse_infix_operation(`a+b`, operation(op(_,_,"+"), id("a"), id("b"))).
parse_infix_operation(`a +b`, operation(op(_,_,"+"), id("a"), id("b"))).
parse_infix_operation(`a+ b`, operation(op(_,_,"+"), id("a"), id("b"))).
parse_infix_operation(`a + b`, operation(op(_,_,"+"), id("a"), id("b"))).
parse_infix_operation(`a+b+c`,
    operation(op(_,_,"+"),
        operation(op(_,_,"+"),
            id("a"),
            id("b")),
        id("c"))).
parse_infix_operation(`a+b*c`,
    operation(op(_,_,"+"),
        id("a"),
        operation(op(_,_,"*"),
            id("b"),
            id("c")))).
parse_infix_operation(`a*b+c`,
    operation(op(_,_,"+"),
        operation(op(_,_,"*"),
            id("a"),
            id("b")),
        id("c"))).
parse_infix_operation(`(a+b)*c`,
    operation(op(_,_,"*"),
        operation(op(_,_,"+"),
            id("a"),
            id("b")),
        id("c"))).
parse_infix_operation(`a+b*c+d`,
    operation(op(_,_,"+"),
        operation(op(_,_,"+"),
            id("a"),
            operation(op(_,_,"*"),
                id("b"),
                id("c"))),
        id("d"))).
parse_infix_operation(`a*b+c*d`,
    operation(op(_,_,"+"),
        operation(op(_,_,"*"),
            id("a"),
            id("b")),
        operation(op(_,_,"*"),
            id("c"),
            id("d")))).
test("parse infix operation", [nondet, forall(parse_infix_operation(Text, Want)), Got = Want]) :-
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

regexp_ops([
    op(600, yfx, "|"),
    op(500, xf, "?"),
    op(400, xf, "+"),
    op(300, xf, "*")
]).

parse_suffix_operation(`a*`, operation(op(_,_,"*"), id("a"))).
parse_suffix_operation(`a+`, operation(op(_,_,"+"), id("a"))).
parse_suffix_operation(`a?`, operation(op(_,_,"?"), id("a"))).
parse_suffix_operation(`a+?`,
    operation(op(_,_,"?"),
        operation(op(_,_,"+"),
            id("a")))).
parse_suffix_operation(`a|b?`,
    operation(op(_,_,"|"),
        id("a"),
        operation(op(_,_,"?"),
            id("b")))).
parse_suffix_operation(`a+|b`,
    operation(op(_,_,"|"),
        operation(op(_,_,"+"),
            id("a")),
        id("b"))).
parse_suffix_operation(`a+|b?`,
    operation(op(_,_,"|"),
        operation(op(_,_,"+"), id("a")),
        operation(op(_,_,"?"), id("b")))).
test("parse suffix operation", [nondet, forall(parse_suffix_operation(Text, Want)), Got = Want]) :-
    regexp_ops(Ops),
    phrase(expression(Ops, Got), Text).

units_ops([
    op(500, yfx, "+"),
    op(500, yfx, "-"),
    op(400, yfx, "*"),
    op(400, yfx, "/"),
    op(300, xf, "kg"),
    op(300, xf, "s"),
    op(300, xf, "m"),
    op(200, fy, "+"),
    op(200, fy, "-")
]).

parse_mixed_operation(`-10kg`,
    operation(op(_,_,"kg"),
        operation(op(_,_,"-"),
            int("10")))).
parse_mixed_operation(`+ x kg`,
    operation(op(_,_,"kg"),
        operation(op(_,_,"+"),
            id("x")))).
parse_mixed_operation(`1 + x kg`,
    operation(op(_,_,"+"),
        int("1"),
        operation(op(_,_,"kg"),
            id("x")))).
parse_mixed_operation(`1 + xkg`,
    operation(op(_,_,"+"),
        int("1"),
        id("xkg"))).
test("parse mixed operation", [nondet, forall(parse_mixed_operation(Text, Want)), Got = Want]) :-
    units_ops(Ops),
    phrase(expression(Ops, Got), Text).

:- end_tests(kilanone).
