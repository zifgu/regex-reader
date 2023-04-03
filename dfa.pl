/*
=======================================================
Regex syntax tree to DFA
=======================================================
Algorithm source: see https://www.geeksforgeeks.org/regular-expression-to-dfa/

Internal predicates:

pos_char(P, C)              Is true if position P holds symbol C
followpos(P, L)             Is true if the followpos of position P is list L
state(S)                    Is true if S is a state of the DFA
marked(S)                   Is true if S is marked (used to prevent re-visiting states)
transition(S1, C, S2)       Is true if there is a transition from S1 to S2 on symbol C
*/
:- dynamic pos_char/2, followpos/2, state/1, start/1, marked/1, transition/3.

/*
=======================================================
Step 1
=======================================================
Walk the tree, assigning an integer position to each leaf node
and computing nullable, firstpos, and lastpos for every node.
*/

% compute_intermediate(Tree, NewTree)   Is true if NewTree is Tree with nullable, firstpos, and lastpos computed at each node
%                                       Dynamically asserts pos_char into the database.
compute_intermediate(Tree, NewTree) :- compute_intermediate_1(Tree, NewTree, 0, _).

% compute_intermediate_1(Tree, NewTree, Pos, NewPos)    Is true if NewTree is Tree with nullable, firstpos, and lastpos computed at each node,
%                                                       the first leaf node in NewTree has position Pos + 1, and the last leaf node in NewTree has position NewPos.
compute_intermediate_1(empty, empty, Pos, Pos).
compute_intermediate_1(
    node(C, empty, empty),
    node(C, empty, empty, false, [NewPos], [NewPos]),
    Pos,
    NewPos
) :-
    NewPos is Pos + 1,
    assert(pos_char(NewPos, C)).
compute_intermediate_1(
    node('*', L, empty),
    node('*', NewL, empty, true, FirstPosL, LastPosL),
    Pos,
    NewPos
) :-
    compute_intermediate_1(L, NewL, Pos, NewPos),
    NewL = node(_, _, _, _, FirstPosL, LastPosL).
compute_intermediate_1(
    node(dis, L, R),
    node(dis, NewL, NewR, Nullable, FirstPos, LastPos),
    Pos,
    Pos2
) :-
    compute_intermediate_1(L, NewL, Pos, Pos1),
    NewL = node(_, _, _, NullableL, FirstPosL, LastPosL),
    compute_intermediate_1(R, NewR, Pos1, Pos2),
    NewR = node(_, _, _, NullableR, FirstPosR, LastPosR),
    or(NullableL, NullableR, Nullable),
    append(FirstPosL, FirstPosR, FirstPos),
    append(LastPosL, LastPosR, LastPos).
compute_intermediate_1(
    node(alt, L, R),
    node(alt, NewL, NewR, Nullable, FirstPos, LastPos),
    Pos,
    Pos2
) :-
    compute_intermediate_1(L, NewL, Pos, Pos1),
    NewL = node(_, _, _, NullableL, FirstPosL, LastPosL),
    compute_intermediate_1(R, NewR, Pos1, Pos2),
    NewR = node(_, _, _, NullableR, FirstPosR, LastPosR),
    and(NullableL, NullableR, Nullable),
    union_if(NullableL, FirstPosL, FirstPosR, FirstPos),
    union_if(NullableR, LastPosR, LastPosL, LastPos).

and(_, false, false).
and(false, _, false).
and(true, true, true).

or(_, true, true).
or(true, _, true).
or(false, false, false).

% union_if(Cond, L1, L2, R)     Is true if Cond is true and R = L1 U L2, or Cond is false and R = L1.
%                               Assumes L1, L2 both sorted.
union_if(true, L1, L2, R) :- union_sorted(L1, L2, R).
union_if(false, L1, _, L1).

/*
=======================================================
Step 2
=======================================================
Compute followpos - a list of positions - for every position
at a leaf node.
*/

% build_followpos(Tree)     Dynamically asserts all followpos mappings resulting from traversing the syntax tree Tree
build_followpos(empty).
build_followpos(node('*', L, empty, _, _, _)) :-
    L = node(_, _, _, _, FirstPosL, LastPosL),
    add_followpos(LastPosL, FirstPosL),
    build_followpos(L).
build_followpos(node(alt, L, R, _, _, _)) :-
    L = node(_, _, _, _, _, LastPosL),
    R = node(_, _, _, _, FirstPosR, _),
    add_followpos(LastPosL, FirstPosR),
    build_followpos(L),
    build_followpos(R).
build_followpos(node(V, L, R, _, _, _)) :-
    V \= alt,
    V \= '*',
    build_followpos(L),
    build_followpos(R).


% add_followpos(Ps, L)      Asserts, for every position P in Ps, that followpos(P) is followpos(P) U L (replacing any previous followpos rule about P)
% citation: https://stackoverflow.com/questions/37871775/prolog-replace-fact-using-fact
add_followpos([], _).
add_followpos([P | Ps], L) :-
    ( followpos(P, FollowPos)
    ->  union_sorted(L, FollowPos, NewFollowPos),
        retract(followpos(P, FollowPos)),
        assertz(followpos(P, NewFollowPos))
    ;   assertz(followpos(P, L))
    ),
    add_followpos(Ps, L).


/*
=======================================================
Step 3
=======================================================
Build the DFA. States are lists of positions, and transitions occur on reading symbols.
*/

% build_dfa(Tree)               Dynamically asserts state(...) and transition(...) resulting from parsing syntax tree Tree into a DFA.
build_dfa(Tree) :-
    compute_intermediate(Tree, NewTree),
    NewTree = node(_, _, _, _, FirstPos, _),
    build_followpos(NewTree),
    assert_once(state(FirstPos)),
    assert_once(start(FirstPos)),
    \+ while_loop.

% A construct to loop over unmarked (unvisited) states until there are no more such states. As long as this completes, it will fail.
% citation: https://www.swi-prolog.org/pldoc/man?predicate=repeat/0
while_loop :-
    repeat,
    (
        state(X),
        \+ marked(X)
    ->  assert_once(marked(X)),
        add_transitions(X, X)
    ;   !,
        fail
    ).

% add_transitions(S, Ps)        Iterates through all positions in state S and dynamically asserts the outgoing states and transitions from S.
add_transitions(_, []).
add_transitions(S, [Pos | Rest]) :-
    pos_char(Pos, C),
    findall_followpos(C, S, L),
    sort(L, U),                 % both sorts and removes duplicates
    assert_once(state(U)),
    assert_once(transition(S, C, U)),
    add_transitions(S, Rest).

% union_sorted(L1, L2, R)       Is true if R is a sorted list containing all elements of L1 and L2 (assumes L1, L2 already sorted).
union_sorted([], L, L).
union_sorted(L, [], L).
union_sorted([H|T1], [H|T2], [H|L]) :-
    union_sorted(T1, T2, L).
union_sorted([H1|T1], [H2|T2], [H1|L]) :-
    H1 < H2,
    union_sorted(T1, [H2|T2], L).
union_sorted([H1|T1], [H2|T2], [H2|L]) :-
    H2 < H1,
    union_sorted([H1|T1], T2, L).

% findall_followpos(C, L, R)    Is true if R is the concatenation of all followpos(P) such that P is in L and pos_char(P, C).
findall_followpos(_, [], []).
findall_followpos(C, [P|PRest], R) :-
    ( pos_char(P, C)
    ->  followpos(P, L),
        append(L, R0, R),
        findall_followpos(C, PRest, R0)
    ;   findall_followpos(C, PRest, R)
    ).

% assert_once(Fact)             Dynamically asserts Fact if it is not already defined.
% citation: https://stackoverflow.com/questions/42071334/prolog-avoiding-duplicate-predicates
assert_once(Fact) :-
    ( Fact
    ->  !
    ;   assertz(Fact)
    ).

/*
=======================================================
Interface
=======================================================
*/

% dfa_parse(Tree)               Declares the DFA resulting from syntax tree Tree.
%   state(S)                    = states of the DFA
%   transition(S1, C, S2)       = transitions of the DFA
%   start(S)                    = start states of the DFA
%   accepting(S)                = accepting states of the DFA
% TODO: currently not guaranteed to work on retry (untested)
dfa_parse(Tree) :-
    build_dfa(node(alt, Tree, node(end, empty, empty))).

% accepting(S)                  Is true if S is an accepting state.
%                               In this algorithm, states containing the unique end marker 'end' are accepting states.
accepting(S) :-
    state(S),
    pos_char(P, end),
    member(P, S).

% For now, must call this after every call to dfa_parse - clears all internal definitions we made.
% TODO: once we don't need to debug anymore, should actually clean up after ourselves
clear :-
    retractall(pos_char(_, _)),
    retractall(followpos(_, _)),
    retractall(state(_)),
    retractall(start(_)),
    retractall(marked(_)),
    retractall(transition(_, _, _)).