:- use_module(library(clpfd)).
:- use_module(library(yall)).

peek(X), [X] --> [X].

% Constraint is of the form [s(1), s(4), s(2)], representing a
% line containing three blocks of sizes 1, 4, and 2, in this order.
line(Length, Constraint) -->
    line(Length, Constraint, []).

% Consume constraint list depending on the cell list next two tokens:
%
% ... x x ... : empty section, don't consume a constraint.
% ... x o ... : start of block, ensure there's a constraint s(X).
% ... o x ... : end of block, the constraint s(1) is consumed.
% ... o o ... : middle of block, decrement the constraint s(N).
%
% For example, for the constraint [s(1), s(4), s(2)] in a list of length 12, we may have
%
%           1      4       2
%           -   -------   ---
%     x x x o x o o o o x o o
%     ---   ---   ---     
%      ^  ---^     ^
%      |   ^ |     |[s(3), s(2)]
%      |   | |
%      |   | |[s(4), s(2)]
%      |   |  
%      |   |[s(1), s(4), s(2)]
%      |
%      |[s(1), s(4), s(2)]
%
% The constraint list is represented as a difference list (Sizes0, Sizes1).

line(0, Sizes, Sizes) --> [].
line(1, Sizes, Sizes) --> [x].
line(1, [s(1)|Sizes], Sizes) --> [o].

line(L, [s(N)|Sizes0], Sizes1) -->
    [o], peek(o),
    {N #> 1, N1 #= N-1},
    line_(L, [s(N1)|Sizes0], Sizes1).

line(L, [s(1)|Sizes0], Sizes1) -->
    [o], peek(x),
    line_(L, Sizes0, Sizes1).

line(L, [s(N)|Sizes0], Sizes1) -->
    [x], peek(o),
    line_(L, [s(N)|Sizes0], Sizes1).

line(L, Sizes0, Sizes1) -->
    [x], peek(x),
    line_(L, Sizes0, Sizes1).

line_(L, Sizes0, Sizes1) -->
    {L #> 1, L1 #= L-1},
    line(L1, Sizes0, Sizes1).

lines(Lines, Constraints) :-
    maplist(
        [Line, Constraint]>>(
            length(Line, L),
            phrase(line(L, Constraint), Line)
        ),
        Lines,
        Constraints).

nonogram0(Table, RowConstraints, ColConstraints) :-
    % Ensure table is a list of lists with MxN elements.
    same_length(Table, RowConstraints),
    maplist(same_length(ColConstraints), Table),
    % Exercise constraints first in rows, then in cols.
    transpose(Table, Cols),
    lines(Table, RowConstraints),
    lines(Cols, ColConstraints).

% ---

test1(Table, RowConstraints, ColConstraints) :-
    Table = [
        [x, o, x, x, o],
        [x, o, o, o, x],
        [o, o, x, x, o]
    ],
    RowConstraints = [[s(1), s(2)], [s(3)], [s(2), s(1)]],
    ColConstraints = [[s(1)], [s(3)], [s(1)], [s(1)], [s(1), s(1)]].

main :-
    test1(Table, _, _),
    nonogram0(Table, RowCs, ColCs),
    writef("%w\n%w\n", [RowCs, ColCs]),
    nonogram0(Table1, RowCs, ColCs),
    maplist([Row]>>writef("%w\n", [Row]), Table1).
