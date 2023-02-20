/* YOUR CODE HERE (Problem 1, delete the following line) */
% reverseL(X,RevX) :- false.
reverseL([],[]).
reverseL([H|T], ReverseList):- reverseL(T, ReverseTail), append(ReverseTail, [H], ReverseList).
?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Problem 2, delete the following line) */
%remove_duplicates(L1,L2) :- false.
%remove_duplicates_helper([],[],[]).
%remove_duplicates_helper(X,[]).


%remove_duplicates([],[]).
%remove_duplicates([H|T], R):- member(H, T), remove_duplicates(T, R).
%remove_duplicates([H|T], [H|R]):- remove_duplicates(T, R).
remove_duplicates(L1, L2) :- remove_duplicates(L1, L2, []).

remove_duplicates([], [], _).
remove_duplicates([H|T], Acc, Seen) :- (member(H, Seen) -> (Acc = S, Seen_new = Seen) ;  (Acc = [H|S], Seen_new = [H|Seen])), remove_duplicates(T, S, Seen_new).




?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* Your CODE HERE (Problem 3, delete the following line) */
%assoc_list(L,AL) :- false.
%assoc_list([],[]).
%assoc_list([H|T],[A-B|Tail2]).
assoc_helper([],_).
assoc_helper([H|T], H-B):- assoc_helper(T, H-B1), B is B1 + 1.
assoc_helper([H|T], X-B):- X \= H, assoc_helper(T, X-B).

assoc_list([],_).
assoc_list([H|T], L):- member(H-_,L), assoc_list(T,L),!.
assoc_list([H|T], L):- assoc_helper([H|T], W), assoc_list(T,[W|L]).


?- assoc_list([1], [1-1]).
?- assoc_list([1,1,2,2,2,3,1], [1-3, 2-3, 3-1]).
?- assoc_list([1,1,4,2,2,2,3,1,1,3,1], X).

/* YOUR CODE HERE (Problem 4, delete the following line) */
%intersectionL(L1,L2,L3) :- false.
intersectionL(_, [], []).
intersectionL([], _, []).
intersectionL([H1|T1], L2, [H1|Stuff]):- member(H1, L2), intersectionL(T1, L2, Stuff), !.
intersectionL([_|T1], L2, Stuff):- intersectionL(T1, L2, Stuff).
?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

/* YOUR CODE HERE (Problem 5, delete the following line) */
greatest([H], H):- !, true.
greatest([H|T],B):- greatest(T, B), B >= H. 
greatest([H|T],H):- greatest(T,B), H > B.

remove_number( _, [], []).
remove_number( N, [N|T1], T1).
remove_number( N, [H1|T1], [H1|T2]) :- H1 \= N, remove_number( N, T1, T2).

maxL3([],_):-false, !.
maxL3([X],_):-false, !.
maxL3([X|Y],_):-false, !.
maxL3(L,X):- greatest(L,A), remove_number(A,L,M), greatest(M,B), remove_number(B,M,N), greatest(N,C), X is A + B + C.
%maxL3(L,X) :- false.
%first_three([One,Two, Three|_], [One,Two, Three]).
%helper_maxL3([A,B,C|_],F) :- F is (A + B + C).
%maxL3([],_) :- false, !.
%maxL3([X],_) :- false, !.
%maxL3([X,Y],_) :- false, !.
%maxL3([X,Y,Z|_], N) :- mergesort([X,Y,Z|_], L), reverseL(L, M), helper_maxL3(M,N).

?- not(maxL3([1], X)).
?- maxL3([1,2,3,4], 9).
?- maxL3([10,3,2,3,10], X).

prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

/* YOUR CODE HERE (Problem 6, delete the following line) */
%partition(L,P,S) :- false.
%assoc_helper([H|T], H-B):- assoc_helper(T, H-B1), B is B1 + 1.
%partition_helper(L,P,S,0).
%segment(A,L):- false.
%segment(A,L):-prefix(P,L), suffix(S,L), append(P,A,W), append(W,S,Z), Z = L.


%partition_helper([H|T], P, S, X):-  partition_helper(T, [P|H], T, X1), X is X1 - 1.

partition([],[],[]).
partition([H],[H],[]).
partition(L,P,S) :- prefix(P,L), suffix(S,L),length(L,W), W2 is div(W,2), W3 is W - W2,length(P,W2), length(S,W3), !.

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Problem 7, delete the following line) */
merge(X,[],X):- !.
merge([],X,X):- !.
merge([Head1|Tail1] , [Head2|Tail2] , [Head1|Tail3]):- Head1<Head2, merge(Tail1,[Head2|Tail2],Tail3).
merge([Head1|Tail1] , [Head2|Tail2] , [Head2|Tail3]):- Head1>Head2, merge([Head1|Tail1], Tail2, Tail3).
merge([Head1|Tail1] , [Head2|Tail2] , [Head1,Head2|Tail3]):-  merge(Tail1, Tail2, Tail3).
?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Problem 8, delete the following line) */
mergesort([],[]).
mergesort([X],[X]).
mergesort(L,S):- partition(L,L1,L2), mergesort(L1,Sort1), mergesort(L2, Sort2), merge(Sort1,Sort2,S).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
