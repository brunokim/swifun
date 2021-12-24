:- module(kilanone, [expression//1, expression//2, operation//3]).

:- use_module(library(ordsets)).
:- use_module(library(yall)).

:- discontiguous term_expansion/2.

% Expands into digit/1 facts.
term_expansion(digit(_), Clauses) :-
    findall(digit(Ch), string_code(_, "0123456789", Ch), Clauses).
digit(_).

% Expands into space/1 facts.
term_expansion(space(_), Clauses) :-
    findall(space(Ch), string_code(_, " \r\n\t", Ch), Clauses).
space(_).

% Expands into punct/1 facts.
term_expansion(punct(_), Clauses) :-
    findall(punct(Ch), string_code(_, "!#$%&*+-/:<=>?@\\^|~", Ch), Clauses).
punct(_).

% Expands into syntax_char/1 facts.
term_expansion(syntax_char(_), Clauses) :-
    findall(syntax_char(Ch), string_code(_, "\"'(),.;[]`{}", Ch), Clauses).
syntax_char(_).

% Expands into letter/1 facts.
term_expansion(letter(_), Clauses) :-
    findall(letter(Ch), string_code(_, "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", Ch), Clauses).
letter(_).

% -----

% Whitespace
ws --> [Ch], {space(Ch)}, ws.
ws --> [].

% -----

% Ints must start and end with a digit. Underlines must be always in the middle of an integer.
int(int(Str)) -->
    [Ch], {digit(Ch)},
    int_continue(Chars),
    {string_codes(Str, [Ch|Chars])}.

int_continue([Ch|Chars]) -->
    [Ch], {digit(Ch)}, int_continue(Chars).
int_continue(Chars) -->
    [0'_], int_continue(Chars).
int_continue([]) --> [].

% -----

identifier(id(Str)) -->
    ( alnum_identifier(Chars)
    | punct_identifier(Chars)
    ),
    {string_codes(Str, Chars)}.

alnum_identifier([Ch|Chars]) -->
    [Ch], {letter(Ch)},
    alnum_id_continue(Chars).

alnum_id_continue([Ch|Chars]) -->
    [Ch], {letter(Ch) ; digit(Ch)},
    alnum_id_continue(Chars).
alnum_id_continue([]) --> [].

punct_identifier([Ch|Chars]) -->
    [Ch], {punct(Ch)},
    punct_id_continue(Chars).

punct_id_continue([Ch|Chars]) -->
    [Ch], {punct(Ch)},
    punct_id_continue(Chars).
punct_id_continue([]) --> [].

% -----

op_associativity(none,  xfx).
op_associativity(right, xfy).
op_associativity(left,  yfx).
op_associativity(none,  fx).
op_associativity(right, fy).
op_associativity(none,  xf).
op_associativity(left,  yf).

op_pos(binary, xfx).
op_pos(binary, xfy).
op_pos(binary, yfx).
op_pos(prefix, fx). 
op_pos(prefix, fy). 
op_pos(suffix, xf). 
op_pos(suffix, yf). 

base_operators([
    op( 990, xfx, ":="),
    op( 900,  fy, "not"),
    op( 700, xfx, "<"),
    op( 700, xfx, ">"),
    op( 700, xfx, "=<"),
    op( 700, xfx, ">="),
    op( 700, xfx, "="),
    op( 700, xfx, "=="),
    op( 700, xfx, "!="),
    op( 700, xfx, "<>"),
    op( 500, yfx, "+"),
    op( 500, yfx, "-"),
    op( 500, yfx, "or"),
    op( 400, yfx, "*"),
    op( 400, yfx, "/"),
    op( 400, yfx, "and"),
    op( 200,  fy, "+"),
    op( 200,  fy, "-"),
    op( 200, xfy, "^")]).

get_operator(Ops, Pos, MaxPrecedence, Op) :-
    member(Op, Ops),
    Op = op(Precedence, Type, _),
    op_pos(Pos, Type),
    Precedence =< MaxPrecedence.

binary_operator(Ops, MaxPrecedence, Op) :-
    get_operator(Ops, binary, MaxPrecedence, Op).

prefix_operator(Ops, MaxPrecedence, Op) :-
    get_operator(Ops, prefix, MaxPrecedence, Op).

suffix_operator(Ops, MaxPrecedence, Op) :-
    get_operator(Ops, suffix, MaxPrecedence, Op).

% -----

atomic_expression(Ops, Tree) -->
    ( identifier(Tree)
    | int(Tree)
    | "(", ws, expression(Ops, Tree0), ws, ")",
      {Tree = paren(Tree0)}
    ).

atomic_expressions(Ops, [Expr|Exprs]) -->
    atomic_expression(Ops, Expr),
    ws,
    atomic_expressions(Ops, Exprs).
atomic_expressions(Ops, [Expr]) --> atomic_expression(Ops, Expr).

% Checks if a list of atomic expressions may be a possible operation, by
% checking that each pair of consecutive elements has at least one identifier.
%
% "id   id  id id" -- possible operation, e.g., x ^ - y
% "id  int  id id" -- possible operation. e.g., - 1 ^ x
% "int  id  id id" -- possible operation, e.g., 1 ^ - x
% "id  int int id" -- NOT an operation! Consecutive non-ids
valid_operation([_]).
valid_operation([E1, E2|Exprs]) :-
    (E1 = id(_) ; E2 = id(_)),
    valid_operation([E2|Exprs]).

% Extract identifiers from Exprs and put them in an ordered set.
extract_symbs(Exprs, SymbSet) :-
    include([id(_)]>>true, Exprs, Identifiers),
    maplist([id(Symb), Symb]>>true, Identifiers, Symbs),
    list_to_ord_set(Symbs, SymbSet).

% Filter the list of operators down only to those that occur in SymbSet, and put
% them in an ordered set.
filter_operators(Ops, SymbSet, OpSet) :-
    include({SymbSet}/[op(_,_,Symb)]>>ord_memberchk(Symb, SymbSet), Ops, Ops_),
    list_to_ord_set(Ops_, OpSet).

% Remove parens from expression tree after operation is parsed.
% Parens mark literal operators that should be treated just like identifiers, e.g.,
% "add := (+)" is different from "add := +".
remove_parens(paren(Expr0), Expr) :-
    remove_parens(Expr0, Expr).
remove_parens(operation(Op, Expr0), operation(Op, Expr)) :-
    remove_parens(Expr0, Expr).
remove_parens(operation(Op, Left0, Right0), operation(Op, Left, Right)) :-
    remove_parens(Left0, Left),
    remove_parens(Right0, Right).
remove_parens(int(X), int(X)).
remove_parens(id(X), id(X)).

% An expression may be a single atomic expression, or an operation appearing as a list
% of expressions.
expression(Tree) -->
    {base_operators(Ops)},
    expression(Ops, Tree).
expression(Ops, Tree) -->
    atomic_expressions(Ops, Exprs),
    {( Exprs = [Tree0] ->
       true
     ; valid_operation(Exprs),
       extract_symbs(Exprs, SymbSet),
       filter_operators(Ops, SymbSet, OpSet),
       phrase(operation(OpSet, 1200, Tree0), Exprs)
     ),
     remove_parens(Tree0, Tree)
    }.

% -----

:- table operation//3.

op_precedence(op(Precedence, Type, _), LeftPrecedence, RightPrecedence) :-
    op_associativity(Associativity, Type),
    ( Associativity = none  -> LeftPrecedence is Precedence-1, RightPrecedence is Precedence-1
    ; Associativity = right -> LeftPrecedence is Precedence-1, RightPrecedence is Precedence
    ; Associativity = left  -> LeftPrecedence is Precedence,   RightPrecedence is Precedence-1
    ).

operation(_, _, Expr) --> [Expr].

operation(OpSet, MaxPrecedence, operation(Op, Expr)) -->
    {prefix_operator(OpSet, MaxPrecedence, Op),
     op_precedence(Op, _, RightPrecedence),
     Op = op(_, _, Symb)
    },
    [id(Symb)],
    operation(OpSet, RightPrecedence, Expr).

operation(OpSet, MaxPrecedence, operation(Op, Expr)) -->
    {suffix_operator(OpSet, MaxPrecedence, Op),
     op_precedence(Op, LeftPrecedence, _),
     Op = op(_, _, Symb)
    },
    operation(OpSet, LeftPrecedence, Expr),
    [id(Symb)].

operation(OpSet, MaxPrecedence, operation(Op, Left, Right)) -->
    {binary_operator(OpSet, MaxPrecedence, Op),
     op_precedence(Op, LeftPrecedence, RightPrecedence),
     Op = op(_, _, Symb)
    },
    operation(OpSet, LeftPrecedence, Left),
    [id(Symb)],
    operation(OpSet, RightPrecedence, Right).

% -----
