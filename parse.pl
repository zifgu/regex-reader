% regex_parse(Str, Tree)    is true if Str is a valid regex and its syntax tree is Tree.
regex_parse(Str, Tree) :-
    string_chars(Str, Chars),
    pattern(Chars, [], Tree).

% Pattern: top-level regex
pattern(L0, L1, T) :- dis(L0, L1, T).

% Disjunction
% dis nodes have 2 children
dis(L0, L1, C) :- alt(L0, L1, C).
dis(L0, L3, node(dis, C1, C2)) :-
    alt(L0, L1, C1),
    bar(L1, L2),
    dis(L2, L3, C2).

% Alternative
% alt nodes have 2 children
alt(L0, L1, T) :- term(L0, L1, T).
alt(L0, L2, node(alt, T, C)) :-
    term(L0, L1, T),
    alt(L1, L2, C).

% Term
term(L0, L1, A) :- atom(L0, L1, A).
term(L0, L1, Q) :- quant(L0, L1, Q).

% Atom
atom(['.' | L], L, node('.', empty, empty)).
atom([C | L], L, node(C, empty, empty)) :- pattern_char(C). 
atom(L0, L3, D) :-
    lparen(L0, L1),
    dis(L1, L2, D),
    rparen(L2, L3).
atom(L0, L3, D) :-
    lparenbox(L0, L1),
    dis(L1, L2, D),
    rparenbox(L2, L3).


% Quantifier (the symbol)
quantifier(['*' | L], L, '*').
quantifier(['+' | L], L, '+').
quantifier(['?' | L], L, '?').

% Quantifier (the expression type: atom + quantifier)
% quant nodes have 2 children - the first is the quantifier type, the second is the element
quant(L0, L2, node(Q, A, empty)) :-
    atom(L0, L1, A),
    quantifier(L1, L2, Q).

% PatternCharacter
pattern_char(C) :- not_in(C, [
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
lparen(['(' | L], L).
rparen([')' | L], L).
bar(['|' | L], L).
lparenbox(['[' | L], L).
rparenbox([']' | L], L).


%Specific cases format   /w,  /B, etc...
slashcases('d','D','w','W','s','S','B','b').


% not_in(E, L)      is true if E is not contained in L.
not_in(_, []).
not_in(E, [H | T]) :-
    E \= H,
    not_in(E, T).


% is_in(E, L)      is true if E is contained in L.
is_in(_, []).
is_in(E, [H | T]) :-
    E == H;
    is_in(E, T).
/*


% Quantifiers
zeroplusquantifier('*', L, R) :-
    quantifier(['*' | L], L, '*'),
    match(0, infinite, L, R, 0).


oneplusquantifier('+', L, R) :-
    quantifier(['+' | L], L, '+'),
    match(1, infinite, L, R, 0).

zerooronequantifier('?', L, R) :-
    quantifier(['+' | L], L, '+'),
    match(0, 1, L, R, 0).

repeatrangequantifier(X,Y,L,R) : - 
    match(X,Y,L,R,0).


notpresent('^', L) :-
    match(0,0,L,0).

exactlyquantifier(X,L,Z) : - 
    match(X,X,L,Z,0).

% match(x,y,l,r,c) where x and y are max and min ranges L is the value counted and r is range of expression
match(_,_,L,[],_).
match(0, infinite, L, [H|T],C). 
match(1, infinite, L,[L|_],C).
match(1,_,L,[_|T],C) :- match(1,_,L,T,C).
match(X,Y,L,[H|T],C) :- not_in(L,[H|T]);
    match(X,Y,L,T,C1).
    C is C1+1,
    C >=X,
    C <=Y.
    

%between is built in predicate
betweenxandy(L, M, [output]) : - 
    (between(L, M), [output]).


%isletter
isLetter(_,[]).
isLetter(E) : - 
    alpha(E). 
    

%Slashcase coverage
isvalidbackslashcase(E,L):-
    is_in(E, slashcases).

% \s white space
iswhitespace(E,L):-
    L = " ",
    E = "\s".

% \S not a white space
iswhitespace(E,L):-
    L != " ",
    E = "\S".

% \w alphanumberic characters
isAlpha(E,L):-
    alnum(E,L).

% \W is not alphabetic characters
isNotAlphabet(E,L): - 
    not alpha(E,L).

%\d true if a Digit
isDigit(_,[]).
isDigit(E) : - 
    number(E). 

% \D true if not a digit
isDigit(_,[]).
isDigit(E) : - 
    not number(E). 


% \B is true if all of A is within a boundary \B can be in front or behind B or B2 indicates a \B is present
iswithinBoundary(B,B2,D,E,F):- 
    border(B,D,E), 
    border(B2,E,F).

border(_,_,_).
border(_,A,A2).
border(B,A,A2):- 
    A = atom;

    

    
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