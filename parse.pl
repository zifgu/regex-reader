% regex_parse(Str, Tree)    is true if Str is a valid regex and its syntax tree is Tree.
regex_parse(Str, Tree) :-
    string_chars(Str, Chars),
    re_pattern(Chars, [], Tree).

% Pattern: top-level regex
re_pattern(L0, L1, T) :- re_dis(L0, L1, T).

% Disjunction
% dis nodes have 2 children
re_dis(L0, L1, C) :- re_alt(L0, L1, C).
re_dis(L0, L3, node(dis, C1, C2)) :-
    re_alt(L0, L1, C1),
    re_bar(L1, L2),
    re_dis(L2, L3, C2).

% Alternative
% alt nodes have 2 children
re_alt(L0, L1, T) :- re_term(L0, L1, T).
re_alt(L0, L2, node(alt, T, C)) :-
    re_term(L0, L1, T),
    re_alt(L1, L2, C).

% Term
re_term(L0, L1, A) :- re_atom(L0, L1, A).
re_term(L0, L1, Q) :- re_quant(L0, L1, Q).

% Atom
re_atom(['.' | L], L, node('.', empty, empty)).
re_atom([C | L], L, node(C, empty, empty)) :- re_pattern_char(C). 
re_atom([\, 'd' | L], L, node('\\d', empty, empty)).
re_atom([\, 'D' | L], L, node('\\D', empty, empty)).
re_atom([\, 's' | L], L, node('\\s', empty, empty)).
re_atom([\, 's' | L], L, node('\\S', empty, empty)).
re_atom([\, 'w' | L], L, node('\\w', empty, empty)).
re_atom([\, 'W' | L], L, node('\\W', empty, empty)).
re_atom(L0, L3, D) :-
    re_lparen(L0, L1),
    re_dis(L1, L2, D),
    re_rparen(L2, L3).

% Quantifier (the symbol)
re_quantifier(['*' | L], L, '*').
re_quantifier(['+' | L], L, '+').
re_quantifier(['?' | L], L, '?').

% Quantifier (the expression type: atom + quantifier)
% quant nodes have 2 children - the first is the quantifier type, the second is the element
re_quant(L0, L2, node(Q, A, empty)) :-
    re_atom(L0, L1, A),
    re_quantifier(L1, L2, Q).

% PatternCharacter
re_pattern_char(C) :- not_in(C, [
    '^',
    '$',
    \,
    '.',
    '*',
    '+',
    '?',
    '(',
    ')',
    '[',
    ']',
    '{',
    '}',
    '|'
]).

% Special characters
re_lparen(['(' | L], L).
re_rparen([')' | L], L).
re_bar(['|' | L], L).

% not_in(E, L)      is true if E is not contained in L.
not_in(_, []).
not_in(E, [H | T]) :-
    E \= H,
    not_in(E, T).

/*
=======================================================
Test cases - parse succeeds
=======================================================

Atoms:
regex_parse("a", Tree).
regex_parse(".", Tree).

Alternatives:
regex_parse("ab", Tree).
regex_parse("abc", Tree).
regex_parse("a.b", Tree).

Quantifiers:
regex_parse("a*", Tree).
regex_parse("a*bc", Tree).
regex_parse("ab?c", Tree).
regex_parse("abc+", Tree).

Disjunction:
regex_parse("a|b", Tree).
regex_parse("ab|c", Tree).
regex_parse("a|bc", Tree).
regex_parse("a|b|c", Tree).
regex_parse("aa|bb|cc", Tree).
regex_parse("a|b+|d", Tree).

Parentheses:
regex_parse("(a)b", Tree).
regex_parse("a(b)", Tree).
regex_parse("(ab)c", Tree).
regex_parse("(a?b)c", Tree).
regex_parse("(ab*)c", Tree).
regex_parse("(ab)+c", Tree).
regex_parse("a(b|c)", Tree).
regex_parse("a(bc|d|e)", Tree).
regex_parse("(a|b)*", Tree).
regex_parse("(a?b)|1+", Tree).
regex_parse("(a|b)?(b|c)", Tree).

Simple real-world examples: (sources: https://cs.lmu.edu/~ray/notes/regex/)
regex_parse("gray|grey", Tree).
regex_parse("gr(a|e)y", Tree).
regex_parse("rege(x(es)?|xps?)", Tree).

=======================================================
Test cases - parse fails
=======================================================
Empty:
regex_parse("", Tree).

Improper quantifiers:
regex_parse("*", Tree).
regex_parse("*1", Tree).
regex_parse("a*?", Tree).

Improper disjunction:
regex_parse("a|+", Tree).
regex_parse("a|", Tree).
regex_parse("|ab", Tree).
regex_parse("a||b", Tree).

Improper parentheses:
regex_parse("()", Tree).
regex_parse("(+)", Tree).
regex_parse("(ab+c", Tree).
regex_parse("(a))", Tree).
regex_parse("ab)+c", Tree).
regex_parse("a((b)+c", Tree).
*/