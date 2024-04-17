%Abuzar Naqvi
%Q1)
div-by-xy(A, B, C) :-
	0 is A mod B,
	0 is A mod C.
%Q2)
list_prod([], 0).
list_prod([X], X).
list_prod([Head|Tail], Product) :-
    list_prod(Tail, RestProduct),
    Product is Head * RestProduct.

%Q3)
palindrome([]).
palindrome([_]).
palindrome(List) :-
    append([First|Middle], [First], List),
    palindrome(Middle).

%Q4)
secondMin(List, Min2) :-
    remove_duplicate(List, Filter),
    (   integer(Filter, Error) -> 
        format("ERROR: ~w is not a number.", [Error])
    	;length(Filter, Length),
        (   Length < 2 -> writeln("ERROR: List has fewer than two unique elements.")
        	;mysort(Filter, Sorted),
            [_, Min2|_] = Sorted,
            writeln(Min2)
        )
    ).

remove_duplicate([], []).
remove_duplicate([H|T], [H|T1]) :-
    subtract(T, [H], T2),
    remove_duplicate(T2, T1).

integer([H|_], H) :-
    \+ number(H).
integer([_|T], X) :-
    integer(T, X).
mysort([], []).

mysort([H|T], Sorted) :-
    mysort(T, SortedT),
    insert(H, SortedT, Sorted).
insert(X, [], [X]).
insert(X, [H|T], [X,H|T]) :-
    X =< H.
insert(X, [H|T1], [H|T2]) :-
    X > H,
    insert(X, T1, T2).


%Q5)
classify(_, [], [], []).
classify(even, [H|T], [H|Evens], Odds) :-
    0 is H mod 2,
    classify(even, T, Evens, Odds).
classify(even, [H|T], Evens, [H|Odds]) :-
    1 is H mod 2,
    classify(even, T, Evens, Odds).
classify(integer, [H|T], [H|Integers], N) :-
    integer(H),
    classify(integer, T, Integers, N).
classify(integer, [H|T], Integers, [H|N]) :-
    \+ integer(H),
    classify(integer, T, Integers, N).
classify(string, [H|T], [H|Strings], NotStrings) :-
    string(H),
    classify(string, T, Strings, NotStrings).
classify(string, [H|T], Strings, [H|NotStrings]) :-
    \+ string(H),
    classify(string, T, Strings, NotStrings).

%Q6)
prefix([],_). 
prefix([H|T1], [H|T2]):- 
    prefix(T1,T2). 
suffix(T,T).
suffix(A,[_|T]):-
    suffix(A,T). 
bookends(A,B,C):- 
    prefix(A,C), 
    suffix(B,C).

%Q7)
subslice([],_).
subslice(A, [H|T]):-
    prefix(A,[H|T]),
    !;subslice(A,T).

%Q8)
shift(List, 0, List).

shift(List, N, R) :-
    length(List, Length),
    M is N mod Length,
    (   M >= 0,  shift_h(List, M, R)
    	;O is Length + M,
        shift_h(List, O, R)
    ).

shift_h(List, 0, List).
shift_h([H|T], M, R) :-
    O is M - 1,
    append(T, [H], Tail),
    shift_h(Tail, O, R).

%Q9)
luhn(N):-
	luhnh(N,0,R),
	0 is R mod 10.
luhnh(0,S,Res):-
	Res is S,!.
luhnh(N, S, Res) :-
    R is N mod 100,
    Q is N // 100,
    sum(R, S2),
    N1 is S + S2,
    luhnh(Q, N1, Res).
sum(N,Res):-
	R is N mod 10,
	Q is N // 10,
	N1 is Q*2,
	N1>9 -> R1 is N1 mod 10,
	Q1 is N1 // 10,  
    Res is R1 + Q1 + R
	;R is N mod 10,
    Q is N // 10,
    N1 is Q*2, 
    Res is R + N1.

%Q10)
who(Person) :-
    people(Woman),
    member(h(_,Person,_,_,ten,_), Woman).


people(Woman):-
	length(Woman, 5),
    
    member(h(blue,_,morocco ,_,_,_),Woman),
    member(h(_,_,_ ,_,fifteen,fifty),Woman),
    left_of(h(_,_,_, _,_,thirtyFive), h(_,_,_, nephew,_,_), Woman),
	Woman = [_,_,h(_,_,_, _,_,fifty),_,_],
    Woman = [_,_,_,_,h(_,_,_, uncle,_,_)],
    next(h(_,jill,_, _,_,_), h(_,_,australia, _,_,_), Woman),
	right_of(h(_,_,_, _,twenty,_), h(_,_,_, _,_,thirtyFive), Woman),
    right_of(h(red,_,_, _,_,_), h(white,_,_, _,_,_), Woman),
    left_of(h(pink,_,_, _,_,_), h(white,_,_, _,_,_), Woman),
    left_of(h(_,_,chile, _,_,_), h(_,_,_, _,_,fourtyFive), Woman),
    some_right(h(_,_,_, _,_,fourty), h(red,_,_, _,_,_), Woman),
    right_of(h(_,chreyl,_, _,_,_), h(_,_,_, _,fifteen,_), Woman),
    member(h(_,_,chile ,brother,_,_),Woman),
	left_of(h(_,_,australia, _,_,_), h(_,_,_, grandfather,_,_), Woman),
    member(h(_,lori,_ ,_,fifteen,_),Woman),
    next(h(_,ann,_, _,_,_), h(_,_,_, brother,_,_), Woman),
    right_of(h(_,_,_, _,twentyfive,_), h(white,_,_, _,_,_), Woman),
    left_of(h(_,_,_, _,twentyfive,_), h(_,ann,_, _,_,_), Woman),
    next(h(green,_,_, _,_,_), h(_,_,_, grandfather,_,_), Woman),
    member(h(pink,jill,_ ,_,_,_),Woman),
    left_of(h(_,_,thailand, _,_,_), h(green,_,_, _,_,_), Woman),

	member(h(_,_,_,_,ten,_), Woman).


next(A, B, Ls) :- append(_, [A,B|_], Ls).
next(A, B, Ls) :- append(_, [B,A|_], Ls).

left_of(A, B, Ls) :-
    append(_, [A,B|_], Ls).

some_left(A,B,List):-
    append(Before, [B|_], List),
    memberchk(A, Before).

right_of(A, B, Ls) :-
    append(_, [B,A|_], Ls).

some_right(A,B,List):-
    append(_, [B|After], List),
    memberchk(A, After).


