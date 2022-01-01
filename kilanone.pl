:- module(kilanone, [expression//1, expression//2, statement//1, statement//2]).

:- use_module(library(ordsets)).
:- use_module(library(yall)).

% Peeks at the next character without consuming it.
peek(Ch), [Ch] --> [Ch].

% -----

:- discontiguous term_expansion/2.

% Expands into binary_digit/1 facts.
term_expansion(binary_digit(_), Clauses) :-
    findall(binary_digit(Ch), string_code(_, "01", Ch), Clauses).
binary_digit(_).

% Expands into octal_digit/1 facts.
term_expansion(octal_digit(_), Clauses) :-
    findall(octal_digit(Ch), string_code(_, "01234567", Ch), Clauses).
octal_digit(_).

% Expands into decimal_digit/1 facts.
term_expansion(decimal_digit(_), Clauses) :-
    findall(decimal_digit(Ch), string_code(_, "0123456789", Ch), Clauses).
decimal_digit(_).

% Expands into hex_digit/1 facts.
term_expansion(hex_digit(_), Clauses) :-
    findall(hex_digit(Ch), string_code(_, "0123456789abcdefABCDEF", Ch), Clauses).
hex_digit(_).

digit(X, 2) :- binary_digit(X).
digit(X, 8) :- octal_digit(X).
digit(X, 10) :- decimal_digit(X).
digit(X, 16) :- hex_digit(X).

% Expands into space/1 facts.
term_expansion(space(_), Clauses) :-
    findall(space(Ch), string_code(_, " \r\n\t", Ch), Clauses).
space(_).

% Expands into punct/1 facts.
term_expansion(punct(_), Clauses) :-
    findall(punct(Ch), string_code(_, "!#$%&*+-/<=>?@\\^|~", Ch), Clauses).
punct(_).

% Expands into syntax_char/1 facts.
term_expansion(syntax_char(_), Clauses) :-
    findall(syntax_char(Ch), string_code(_, "\"'(),.:;[]`{}", Ch), Clauses).
syntax_char(_).

% Expands into letter/1 facts.
term_expansion(letter(_), Clauses) :-
    findall(letter(Ch), string_code(_, "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", Ch), Clauses).
letter(_).

% -----

scope_separator --> "::".
assignment_operator --> ":=".
type_decl_operator --> ":".

% -----

% Whitespace
ws --> [Ch], {space(Ch)}, !, ws.
ws --> [].

% -----

% Ints must start and end with a digit. Underlines must be always in the middle of an integer.
% 
% Valid ints:
% 0
% 01
% 510
% 5_100
% 0d1234567890
% 0d1_234_567_890
% 0b0110_0001
% 0xABCD_EF01_2345_6789
% 0o775
int(int(Str, Base)) -->
    (   "0b", {Base=2}
    |   "0o", {Base=8}
    |   "0d", {Base=10}
    |   "0x", {Base=16}
    ),
    [Ch], {digit(Ch, Base)},
    int_continue(Chars, Base),
    {string_codes(Str, [Ch|Chars])}.

int(int(Str, 10)) -->
    [Ch], {digit(Ch, 10)},
    int_continue(Chars, 10),
    {string_codes(Str, [Ch|Chars])}.

int_continue([Ch|Chars], Base) -->
    [Ch], {digit(Ch, Base)}, !,
    int_continue(Chars, Base).
int_continue(Chars, Base) -->
    [0'_],
    peek(Ch), {Ch == 0'_ ; digit(Ch, Base)},
    int_continue(Chars, Base).
int_continue([], _) --> [].

% -----

identifier(id(Str)) -->
    alnum_identifier(Str)
    | punct_identifier(Str).

alnum_identifier(Str) -->
    [Ch], {letter(Ch)}, !,
    alnum_id_continue(Chars),
    {string_codes(Str, [Ch|Chars])}.

alnum_id_continue([Ch|Chars]) -->
    [Ch], {letter(Ch) ; digit(Ch, 10)}, !,
    alnum_id_continue(Chars).
alnum_id_continue([]) --> [].

punct_identifier(Str) -->
    [Ch], {punct(Ch)},
    punct_id_continue(Chars),
    {string_codes(Str, [Ch|Chars])}.

punct_id_continue([Ch|Chars]) -->
    [Ch], {punct(Ch)},
    punct_id_continue(Chars).
punct_id_continue([]) --> [].

% -----

% Valid symbols:
%   abc
%   <=>
%   ::abc
%   ::+
%   ns::abc
%   ::ns::abc
%   ns1::ns2::ns3::abc
%   
symbol(symb([], Name)) -->
    identifier(id(Name)).
symbol(symb([root], Name)) -->
    scope_separator, ws,
    identifier(id(Name)).
symbol(symb(Scopes, Name)) -->
    symbol_scopes(Scopes), ws,
    identifier(id(Name)).
symbol(symb([root|Scopes], Name)) -->
    scope_separator, ws,
    symbol_scopes(Scopes), ws,
    identifier(id(Name)).

symbol_scopes([Scope|Scopes]) -->
    alnum_identifier(Scope), ws,
    scope_separator, ws,
    symbol_scopes(Scopes).
symbol_scopes([Scope]) -->
    alnum_identifier(Scope), ws,
    scope_separator.

% -----

string_quote(0'").
string_quote(0'').

string(str(Str)) -->
    [Ch], {string_quote(Ch)},
    quoted(Ch, Chars),
    [Ch],
    {string_codes(Str, Chars)}.

quoted(Delim, [Ch|Chars]) -->
    ( [0'\\, Delim], {Ch=Delim}
    | [0'\\, 0'\\], {Ch=0'\\}
    | [Ch], {dif(Ch, Delim), dif(Ch, 0'\\)}
    ),
    quoted(Delim, Chars).
quoted(_, []) --> [].

% -----

op_associativity(none,  xfx).
op_associativity(right, xfy).
op_associativity(left,  yfx).
op_associativity(none,  fx).
op_associativity(right, fy).
op_associativity(none,  xf).
op_associativity(left,  yf).

op_pos(infix, xfx).
op_pos(infix, xfy).
op_pos(infix, yfx).
op_pos(prefix, fx). 
op_pos(prefix, fy). 
op_pos(suffix, xf). 
op_pos(suffix, yf). 

base_operators([
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

infix_operator(Ops, MaxPrecedence, Op) :-
    get_operator(Ops, infix, MaxPrecedence, Op).

prefix_operator(Ops, MaxPrecedence, Op) :-
    get_operator(Ops, prefix, MaxPrecedence, Op).

suffix_operator(Ops, MaxPrecedence, Op) :-
    get_operator(Ops, suffix, MaxPrecedence, Op).

% -----

atomic_expression(Ops, Tree) -->
    ( identifier(Tree)
    | symbol(Tree)
    | int(Tree)
    | string(Tree)
    | "(", ws, expression(Ops, Tree0), ws, ")",
      {Tree = paren(Tree0)}
    ).

% We need to check for termination first, so that when we reach the final
% symbol in a series of atomic expressions, it is accepted greedily. That is,
% the text `... abcd)` matches first ["abcd"] with rest `)`.
%
% If we use the other order, we would first fail, because we'd expect an additional
% symbol after ws. The first one to match is ["abc", "d"], which may be valid if 'd'
% is a suffix operator. This doesn't correspond to our expectations that symbols
% are consumed greedily.
atomic_expressions(Ops, [Expr]) --> atomic_expression(Ops, Expr).
atomic_expressions(Ops, [Expr|Exprs]) -->
    atomic_expression(Ops, Expr),
    ws,
    atomic_expressions(Ops, Exprs).

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

% Filter the list of operators down only to those that occur in SymbSet.
filter_operators(Ops, SymbSet, ExprOps) :-
    include({SymbSet}/[op(_,_,Symb)]>>ord_memberchk(Symb, SymbSet), Ops, ExprOps).

% Remove parens from expression tree after operation is parsed.
% Parens mark literal operators that should be treated just like identifiers, e.g.,
% "add := (+)" is different from "add := +".
remove_parens(paren(Expr0), Expr) :- !,
    remove_parens(Expr0, Expr).
remove_parens(operation(Op, Expr0), operation(Op, Expr)) :- !,
    remove_parens(Expr0, Expr).
remove_parens(operation(Op, Left0, Right0), operation(Op, Left, Right)) :- !,
    remove_parens(Left0, Left),
    remove_parens(Right0, Right).
remove_parens(X, X).

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
       filter_operators(Ops, SymbSet, ExprOps),
       phrase(operation(ExprOps, 1200, Tree0), Exprs)
     ),
     remove_parens(Tree0, Tree)
    }.

% -----

% A tabled predicate has an undefined resolution order, so we can't rely on the order of
% clauses to provide a deterministic parse tree.
%
% This is a problem for an expression like "-+a", which can be parsed as both -(+(a)), where
% '+' is a unary operator, and +(-, a), where '+' is an infix operator. We then restrict that
% operators can't appear as terminals from an operation.
% The text "-+a" is thus always parsed as -(+(a)); if we'd want for the other interpretation,
% we must write "(-)+a".
:- table operation//3.

op_precedence(op(Precedence, Type, _), LeftPrecedence, RightPrecedence) :-
    op_associativity(Associativity, Type),
    ( Associativity = none  -> LeftPrecedence is Precedence-1, RightPrecedence is Precedence-1
    ; Associativity = right -> LeftPrecedence is Precedence-1, RightPrecedence is Precedence
    ; Associativity = left  -> LeftPrecedence is Precedence,   RightPrecedence is Precedence-1
    ).

% Terminal branch.
operation(ExprOps, _, Expr) -->
    [Expr],
    {\+ (Expr = id(Symb), member(op(_, _, Symb), ExprOps))}.

% Prefix branch.
operation(ExprOps, MaxPrecedence, operation(Op, Expr)) -->
    {prefix_operator(ExprOps, MaxPrecedence, Op),
     op_precedence(Op, _, RightPrecedence),
     Op = op(_, _, Symb)
    },
    [id(Symb)],
    operation(ExprOps, RightPrecedence, Expr).

% Suffix branch.
operation(ExprOps, MaxPrecedence, operation(Op, Expr)) -->
    {suffix_operator(ExprOps, MaxPrecedence, Op),
     op_precedence(Op, LeftPrecedence, _),
     Op = op(_, _, Symb)
    },
    operation(ExprOps, LeftPrecedence, Expr),
    [id(Symb)].

% Infix branch.
operation(ExprOps, MaxPrecedence, operation(Op, Left, Right)) -->
    {infix_operator(ExprOps, MaxPrecedence, Op),
     op_precedence(Op, LeftPrecedence, RightPrecedence),
     Op = op(_, _, Symb)
    },
    operation(ExprOps, LeftPrecedence, Left),
    [id(Symb)],
    operation(ExprOps, RightPrecedence, Right).

% -----

statement(Tree) -->
    {base_operators(Ops)},
    statement(Ops, Tree).
statement(Ops, Tree) -->
    assignment(Ops, Tree)
    | declaration(Ops, Tree)
    | expression_statement(Ops, Tree).

assignment(Ops, assign(Symbol, Value)) -->
    symbol(Symbol),
    ws, assignment_operator, ws,
    expression(Ops, Value).

declaration(Ops, decl(Name, Type)) -->
    identifier(id(Name)),
    ws, type_decl_operator, ws,
    expression(Ops, Type).

expression_statement(Ops, expr_stmt(Tree)) --> expression(Ops, Tree).
