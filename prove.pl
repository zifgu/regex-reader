/*
=======================================================
Regex evaluation interface
=======================================================

Example usage:
prove("([A-Z])\w+", "Evaluate", strong, R).

Note: these predicates are not expected to work with retry.
*/

:- [dfa].
:- [parse].

%   The two versions of prove that can be used.  Strong matches only the string to the regex, loose matches any substring as well.
prove(Regex, String, strong, R) :- regex_parse(Regex, Tree), dfa_parse(Tree), prove_strong(String, R), clear ; clear, fail.
prove(Regex, String, loose, R) :- regex_parse(Regex, Tree), dfa_parse(Tree), prove_loose(String, R), clear ; clear, fail.

%   Some basic error handling for prove
prove(Regex, _, _, _) :- \+ regex_parse(Regex, _), write("Invalid or unsupported regex syntax."), fail.
prove(_, _, Q, _) :- \+ Q = strong, \+ Q = loose, write("Please specify strong or loose."), fail.

%   Strong proof code, walks through the state machine using the string and if the string ends at an accepting state returns true.
prove_strong(Str, R) :- start(S), string_chars(Str, C), prove_move_strong(C, S, Q), string_chars(R, Q).
prove_move_strong([H|[]], S, [H]) :- transition(S, C, S1), matches(C, H), accepting(S1).
prove_move_strong([H|T], S, [H|R]) :- transition(S, C, S1), matches(C, H), prove_move_strong(T, S1, R).

%   Loose proof code, walks through the state machine using the string, and if an accepting state is reached returns true.
prove_loose(Str, R) :- start(S), string_chars(Str, C), prove_move_loose(C, S, Q), string_chars(R, Q).
prove_move_loose([H|_], S, [H]) :- transition(S, C, S1), matches(C, H), accepting(S1).
prove_move_loose([H|T], S, [H|R]) :- transition(S, C, S1), matches(C, H), prove_move_loose(T, S1, R).
prove_move_loose([_|T], S, R) :- start(S), prove_move_loose(T, S, R).

%   matches(Node, Char)     is true if the character or character class Node matches Char.
matches('.', _).
matches('\\d', C) :- char_type(C, digit).
matches('\\s', C) :- char_type(C, space).
matches('\\w', C) :- char_type(C, csym).
matches('\\D', C) :- \+ matches('\\d', C).
matches('\\S', C) :- \+ matches('\\s', C).
matches('\\W', C) :- \+ matches('\\w', C).
matches(char_range(C1, C2), C) :- char_code(C1, V1), char_code(C2, V2), char_code(C, V), V1 =< V, V =< V2.
matches(negated_char_range(C1, C2), C) :- \+ matches(char_range(C1, C2), C).
matches(C, C) :- not_in(C, ['\\d', '\\s', '\\w', '\\D', '\\S', '\\W', '.']).