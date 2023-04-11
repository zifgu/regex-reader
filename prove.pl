:- [dfa].
:- [parse].

%   The two versions of prove that can be used.  Strong matches only the string to the regex, loose matches any substring as well.
prove(Regex, String, strong, R) :- regex_parse(Regex, Tree), dfa_parse(Tree), prove_strong(String, R), clear.
prove(Regex, String, loose, R) :- regex_parse(Regex, Tree), dfa_parse(Tree), prove_loose(String, R), clear.

%   Some basic error handling for prove
prove(Regex, _, _, _) :- \+ regex_parse(Regex, _), write("Invalid Regex").
prove(_, _, Q, _) :- \+ Q = strong, \+ Q = loose, write("Please specify strong or loose").

%   Strong proof code, walks through the state machine using the string and if the string ends at an accepting state returns true.
prove_strong(Str, R) :- start(S), atom_chars(Str, C), prove_move_strong(C, S, Q), atom_chars(R, Q).
prove_move_strong([H|[]], S, [H]) :- transition(S, H, S1), accepting(S1).
prove_move_strong([H|T], S, [H|R]) :- transition(S, H, S1), prove_move_strong(T, S1, R).
prove_move_strong([H|[]], S, [H]) :- transition(S, ., S1), accepting(S1).
prove_move_strong([H|T], S, [H|R]) :- transition(S, ., S1), prove_move_strong(T, S1, R).

%   Loose proof code, walks through the state machine using the string, and if an accepting state is reached returns true.
prove_loose(Str, R) :- start(S), atom_chars(Str, C), prove_move_loose(C, S, Q), atom_chars(R, Q).
prove_move_loose([H|_], S, [H]) :- transition(S, H, S1), accepting(S1).
prove_move_loose([H|T], S, [H|R]) :- transition(S, H, S1), prove_move_loose(T, S1, R).
prove_move_loose([H|_], S, [H]) :- transition(S, ., S1), accepting(S1).
prove_move_loose([H|T], S, [H|R]) :- transition(S, ., S1), prove_move_loose(T, S1, R).
prove_move_loose([_|T], S, R) :- start(S), prove_move_loose(T, S, R).


%   matches(Node, Char)     Is true if the character or character class Node matches Char.
matches('.', _).
matches(characterclass(C2), C) : char_code(C2, Y), char_code(C,Y);
matches('\\d', C) :- char_type(C, digit).
matches('\\s', C) :- char_type(C, space).
matches('\\w', C) :- char_type(C, alnum).
matches('\\w', '_').
matches('\\D', C) :- \+ matches('\\d', C).
matches('\\S', C) :- \+ matches('\\S', C).
matches('\\W', C) :- \+ matches('\\w', C).
matches(C, C) :- not_in(C, ['\\d', '\\s', '\\w', '\\D', '\\S', '\\W', '.']).



