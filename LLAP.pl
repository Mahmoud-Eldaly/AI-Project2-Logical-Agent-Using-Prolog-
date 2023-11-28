:- include('KB.pl').

resources(F,M,E,S1):-
    (S1=s0, food(F), materials(M), energy(E));
    (S1=result(A,S), (
        (A=b1, build1(F1,M1,E1), resources(F2,M2,E2,S), F is F2-F1, M is M2-M1, E is E2-E1);
    	(A=b2, build2(F1,M1,E1), resources(F2,M2,E2,S), F is F2-F1, M is M2-M1, E is E2-E1);
        (A=reqf, resources(F1,M,E,S), F is F1+1);
    	(A=reqm, resources(F,M1,E,S), M is M1+1);
        (A=reqe, resources(F,M,E1,S), E is E1+1))).

checkValidBuild(N,S):-
   ((N=1, build1(F,M,E)); (N=2, build2(F,M,E))),
    resources(F1,M1,E1,S),
    F1>=F, M1>=M, E1>=E.

built(N, result(A,S)):-
    built(N,S);
    ((N=1, A=b1, checkValidBuild(1,S)); (N=2, A=b2, checkValidBuild(2,S))).

check(S):-
    built(1,S), built(2,S).

ids(X,L):-
    (call_with_depth_limit(check(X),L,R), number(R));
    (call_with_depth_limit(check(X),L,R), R=depth_limit_exceeded,
    L1 is L+1, ids(X,L1)).

goal(S):-
    ids(S,1).