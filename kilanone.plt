:- begin_tests(kilanone).
:- use_module(kilanone).

parse_int(`1`, int("1", 10)).
parse_int(`10`, int("10", 10)).
parse_int(`123_456`, int("123456", 10)).
parse_int(`1___0`, int("10", 10)).
parse_int(`(1)`, int("1", 10)).
parse_int(`( 1)`, int("1", 10)).
parse_int(`(1 )`, int("1", 10)).
parse_int(`( 1 )`, int("1", 10)).
parse_int(`((1))`, int("1", 10)).
parse_int(`0b0`, int("0", 2)).
parse_int(`0b1`, int("1", 2)).
parse_int(`0o075`, int("075", 8)).
parse_int(`0d123_456`, int("123456", 10)).
parse_int(`0x1ac4_BEEF`, int("1ac4BEEF", 16)).
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

parse_string(`"a"`, str("a")).
parse_string(`'a'`, str("a")).
parse_string(`'abc'`, str("abc")).
parse_string(`'and for my next trick...'`, str("and for my next trick...")).
parse_string(`'Edwin "Buzz" Aldrin'`, str("Edwin \"Buzz\" Aldrin")).
parse_string(`"Edwin \\"Buzz\\" Aldrin"`, str("Edwin \"Buzz\" Aldrin")).
parse_string(`"Hawai'i"`, str("Hawai'i")).
parse_string(`'Hawai\\'i'`, str("Hawai'i")).
parse_string(`'C:\\\\\\\\Users\\\\alice\\\\Documents\\\\'`, str("C:\\\\Users\\alice\\Documents\\")).
test("parse string", [nondet, forall(parse_string(Text, Want)), Got = Want]) :-
    phrase(expression(Got), Text).

parse_symbol(`::a`, symb([root], "a")).
parse_symbol(`:: a`, symb([root], "a")).
parse_symbol(`ns::a`, symb(["ns"], "a")).
parse_symbol(`ns :: a`, symb(["ns"], "a")).
parse_symbol(`::ns::a`, symb([root, "ns"], "a")).
parse_symbol(`ns1::ns2::a`, symb(["ns1", "ns2"], "a")).
parse_symbol(`::ns1::ns2::a`, symb([root, "ns1", "ns2"], "a")).
parse_symbol(`:: ns1 :: ns2 ::  a`, symb([root, "ns1", "ns2"], "a")).
test("parse symbol", [nondet, forall(parse_symbol(Text, Want)), Got = Want]) :-
    phrase(expression(Got), Text).

parse_block(`{}`, block([])).
parse_block(`{ }`, block([])).
parse_block(`{;}`, block([nil])).
parse_block(`{ ;}`, block([nil])).
parse_block(`{; }`, block([nil])).
parse_block(`{a}`, block([expr_stmt(id("a"))])).
parse_block(`{a;}`, block([expr_stmt(id("a")), nil])).
parse_block(`{;a}`, block([nil, expr_stmt(id("a"))])).
parse_block(`{; a}`, block([nil, expr_stmt(id("a"))])).
parse_block(`{;;a}`, block([nil, nil, expr_stmt(id("a"))])).
parse_block(`{a;b}`, block([expr_stmt(id("a")), expr_stmt(id("b"))])).
test("parse block", [nondet, forall(parse_block(Text, Want)), Got = Want]) :-
    phrase(expression(Got), Text).

parse_func(`fn[]1`, func([], int("1", 10))).
parse_func(`fn[]x`, func([], id("x"))).
parse_func(`fn[x]x`, func([id("x")], id("x"))).
parse_func(`fn[x:Int]x`, func([decl("x", id("Int"))], id("x"))).
parse_func(`fn[x:Int,]x`, func([decl("x", id("Int"))], id("x"))).
parse_func(`fn[ x:Int , ] x`, func([decl("x", id("Int"))], id("x"))).
parse_func(`fn[x, y] x`, func([id("x"), id("y")], id("x"))).
parse_func(`fn[x, y:Int] x`, func([id("x"), decl("y", id("Int"))], id("x"))).
parse_func(`fn[x, y:Int,] x`, func([id("x"), decl("y", id("Int"))], id("x"))).
parse_func(`fn[x] fn[y] (x+y)`,
    func([id("x")],
        func([id("y")],
            operation(op(_,_,"+"), id("x"), id("y"))))).
parse_func(`fn[x, y] x+y`,
    operation(op(_,_,"+"),
        func([id("x"), id("y")], id("x")),
        id("y"))).
test("parse func", [nondet, forall(parse_func(Text, Want)), Got = Want]) :-
    phrase(expression(Got), Text).

parse_call(`f()`, call(id("f"), [])).
parse_call(`f ()`, call(id("f"), [])).
parse_call(`f ( )`, call(id("f"), [])).
parse_call(`f ( )`, call(id("f"), [])).
parse_call(`obj.m1`, method(id("obj"), "m1")).
parse_call(`obj.m1()`, call(method(id("obj"), "m1"), [])).
parse_call(`obj . m1 ( )`, call(method(id("obj"), "m1"), [])).
parse_call(`obj.m1().m2()`, call(method(call(method(id("obj"), "m1"), []), "m2"), [])).
parse_call(`fn[]f()`, func([], call(id("f"), []))).
parse_call(`(fn[]f)()`, call(func([], id("f")), [])).
parse_call(`f(a)`, call(id("f"), [id("a")])).
parse_call(`f(a,)`, call(id("f"), [id("a")])).
parse_call(`f( a , )`, call(id("f"), [id("a")])).
parse_call(`f(a,b)`, call(id("f"), [id("a"), id("b")])).
parse_call(`f(a,b+c)`, call(id("f"), [id("a"), operation(op(_,_,"+"), id("b"), id("c"))])).
parse_call(`f(a,g(b))`, call(id("f"), [id("a"), call(id("g"), [id("b")])])).
test("parse call", [nondet, forall(parse_call(Text, Want)), Got = Want]) :-
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
    op(200, fy, "not"),
    op(200, fy, "+"),
    op(200, fy, "-")
]).

parse_mixed_operation(`-10kg`,
    operation(op(_,_,"kg"),
        operation(op(_,_,"-"),
            int("10", 10)))).
parse_mixed_operation(`+ x kg`,
    operation(op(_,_,"kg"),
        operation(op(_,_,"+"),
            id("x")))).
parse_mixed_operation(`1 + x kg`,
    operation(op(_,_,"+"),
        int("1", 10),
        operation(op(_,_,"kg"),
            id("x")))).
parse_mixed_operation(`not x + y kg`,
    operation(op(_,_,"+"),
        operation(op(_,_,"not"),
            id("x")),
        operation(op(_,_,"kg"),
            id("y")))).
parse_mixed_operation(`xkg + ykg`,
    operation(op(_,_,"+"),
        id("xkg"),
        id("ykg"))).
parse_mixed_operation(`notx + noty`,
    operation(op(_,_,"+"),
        id("notx"),
        id("noty"))).
test("parse mixed operation", [nondet, forall(parse_mixed_operation(Text, Want)), Got = Want]) :-
    units_ops(Ops),
    phrase(expression(Ops, Got), Text).

% -----

parse_assignment(`a := x`, assign(symb([], "a"), id("x"))).
parse_assignment(`a:=x`, assign(symb([], "a"), id("x"))).
parse_assignment(`::a := x`, assign(symb([root], "a"), id("x"))).
parse_assignment(`obj::a := x`, assign(symb(["obj"], "a"), id("x"))).
parse_assignment(`obj::a := obj::x`, assign(symb(["obj"], "a"), symb(["obj"], "x"))).
test("parse assignment", [nondet, forall(parse_assignment(Text, Want)), Got = Want]) :-
    phrase(program(prog([Got])), Text).

parse_declaration(`b: Bool`, decl("b", id("Bool"))).
parse_declaration(`b:Bool`, decl("b", id("Bool"))).
parse_declaration(`is_stuff:X`, decl("is_stuff", id("X"))).
parse_declaration(`this_or_that: X+Y`,
    decl("this_or_that",
        operation(op(_,_,"+"),
            id("X"),
            id("Y")))).
test("parse declaration", [nondet, forall(parse_declaration(Text, Want)), Got = Want]) :-
    phrase(program(prog([Got])), Text).

% -----

parse_program(`
    fibo := fn[x:Int] {
        if(x < 2,
            fn[] 1,
            fn[] {
                a := x - 2;
                b := x - 1;
                fibo(a) + fibo(b)
            },
        )
    };
    fibo(10)
`, prog([
    assign(
        symb([], "fibo"),
        func([decl("x", id("Int"))], block([
            expr_stmt(call(id("if"), [
                operation(op(_,_,"<"), id("x"), int("2", 10)),
                func([], int("1", 10)),
                func([], block([
                    assign(symb([],"a"), operation(op(_,_,"-"), id("x"), int("2", 10))),
                    assign(symb([],"b"), operation(op(_,_,"-"), id("x"), int("1", 10))),
                    expr_stmt(operation(op(_,_,"+"),
                        call(id("fibo"), [id("a")]),
                        call(id("fibo"), [id("b")])))]))]))]))),
    expr_stmt(call(id("fibo"), [
        int("10", 10)]))])
).
test("parse program", [nondet, forall(parse_program(Text, Want)), Got = Want]) :-
    phrase(program(Got), Text).

% -----

parse_fail(`1_`).
parse_fail(`0b_1`).
parse_fail(`0b1_`).
parse_fail(`"""`).
parse_fail(`'''`).
parse_fail(`"\\"`).
parse_fail(`'\\'`).
parse_fail(`"a\\"`).
parse_fail(`'\\a'`).
parse_fail(`'abc`).
parse_fail(`abc'`).
parse_fail(`fn []1`).
parse_fail(`fn[,]1`).
parse_fail(`fn[1,]1`).
parse_fail(`fn[x,,]1`).
parse_fail(`obj.(a)`).
test("fail to parse expression", [nondet, fail, forall(parse_fail(Text))]) :-
    phrase(expression(_), Text).

:- end_tests(kilanone).
