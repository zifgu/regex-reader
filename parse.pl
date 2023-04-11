/*
=======================================================
Regex to syntax tree
=======================================================
*/

%   regex_parse(Str, Tree)    is true if Str is a valid regex and its syntax tree is Tree.
regex_parse(Str, Tree) :-
    string_chars(Str, Chars),
    re_pattern(Chars, [], Tree).

%   Patterns, i.e. top-level regexes
re_pattern(L0, L1, T) :- re_dis(L0, L1, T).

%   Disjunctions (parsed to nodes with 2 children)
re_dis(L0, L1, C) :- re_alt(L0, L1, C).
re_dis(L0, L3, node(dis, C1, C2)) :-
    re_alt(L0, L1, C1),
    re_bar(L1, L2),
    re_dis(L2, L3, C2).

%   Alternatives, i.e. concatenation (parsed to nodes with 2 children)
re_alt(L0, L1, T) :- re_term(L0, L1, T).
re_alt(L0, L2, node(alt, T, C)) :-
    re_term(L0, L1, T),
    re_alt(L1, L2, C).

%   Terms
re_term(L0, L1, A) :- re_atom(L0, L1, A).
re_term(L0, L1, Q) :- re_quant(L0, L1, Q).

%   Atoms
re_atom(['.' | L], L, node('.', empty, empty)).
re_atom([C | L], L, node(C, empty, empty)) :- re_pattern_char(C). 
re_atom([\, 'd' | L], L, node('\\d', empty, empty)).
re_atom([\, 'D' | L], L, node('\\D', empty, empty)).
re_atom([\, 's' | L], L, node('\\s', empty, empty)).
re_atom([\, 's' | L], L, node('\\S', empty, empty)).
re_atom([\, 'w' | L], L, node('\\w', empty, empty)).
re_atom([\, 'W' | L], L, node('\\W', empty, empty)).
re_atom(L0, L1, node(C, empty, empty)) :- re_character_class(L0, L1, C).
re_atom(L0, L3, D) :-
    re_lparen(L0, L1),
    re_dis(L1, L2, D),
    re_rparen(L2, L3).

%   Character classes
re_character_class(L0, L5, char_range(C1, C2)) :-
    re_lbracket(L0, L1),
    re_source_char(L1, L2, C1),
    re_dash(L2, L3),
    re_source_char(L3, L4, C2),
    re_rbracket(L4, L5).
re_character_class(L0, L6, negated_char_range(C1, C2)) :-
    re_lbracket(L0, L1),
    re_not(L1, L2),
    re_source_char(L2, L3, C1),
    re_dash(L3, L4),
    re_source_char(L4, L5, C2),
    re_rbracket(L5, L6).
re_character_class(L0, L3, chars(Cs)) :-
    re_lbracket(L0, L1),
    re_not_not(L1, _),
    re_source_chars(L1, L2, Cs),
    re_rbracket(L2, L3).
re_character_class(L0, L4, negated_chars(Cs)) :-
    re_lbracket(L0, L1),
    re_not(L1, L2),
    re_source_chars(L2, L3, Cs),
    re_rbracket(L3, L4).

%   Quantifier symbols
re_quantifier(['*' | L], L, '*').
re_quantifier(['+' | L], L, '+').
re_quantifier(['?' | L], L, '?').

%   Quantifier expressions (parsed to nodes with 1 child).
re_quant(L0, L2, node(Q, A, empty)) :-
    re_atom(L0, L1, A),
    re_quantifier(L1, L2, Q).

%   Characters allowed in general regex patterns
re_pattern_char(C) :- not_in(C, ['^', '$', \, '.', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|']).

%   Characters allowed inside character classes
re_source_char([C | L], L, C) :- not_in(C, [\, ']', '-']).

%   Sequence of characters allowed inside character classes
re_source_chars(L0, L1, [C]) :- re_source_char(L0, L1, C).
re_source_chars(L0, L2, [C | Cs]) :- 
    re_source_char(L0, L1, C),
    re_source_chars(L1, L2, Cs).

%   Various control characters matched during parsing
re_lparen(['(' | L], L).
re_rparen([')' | L], L).
re_lbracket(['[' | L], L).
re_rbracket([']' | L], L).
re_dash(['-' | L], L).
re_not(['^' | L], L).
re_not_not([C | L], L) :- C \= '^'.
re_bar(['|' | L], L).

%   not_in(E, L)      is true if character E is not contained in list L.
not_in(_, []).
not_in(E, [H | T]) :-
    E \= H,
    not_in(E, T).

%   is_in(E, L)      is true if E is contained in L.
is_in(E, [E | _]).
is_in(E, [H | T]) :-
    E \= H,
    is_in(E, T).

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

Character classes: (note that to type a backslash in Prolog, you have to type \\ - to escape it)
regex_parse("\\D", Tree).
regex_parse("\\w+", Tree).
regex_parse("\\w|a|\\d+", Tree).
regex_parse(".|[A-Z]", Tree).
regex_parse("[^A-Z]", Tree).
regex_parse("[Ab.]", Tree).
regex_parse("b[^Abc]", Tree).
regex_parse("[^Abc|^]", Tree).

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

Improper character classes:
regex_parse("[]", Tree).
regex_parse("[-Z]", Tree).
regex_parse("[A-]", Tree).
regex_parse("[^]", Tree).
regex_parse("[^A-Z", Tree).
regex_parse("Abc]", Tree).

Improper parentheses:
regex_parse("()", Tree).
regex_parse("(+)", Tree).
regex_parse("(ab+c", Tree).
regex_parse("(a))", Tree).
regex_parse("ab)+c", Tree).
regex_parse("a((b)+c", Tree).
regex_parse("[a(b]+c)", Tree).
*/