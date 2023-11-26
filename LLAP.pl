:- include('KB.pl').

food(X,S1):-
    (S1=s0,food(X));(S1=result(A,S),(
    (A=reqf,food(X1,S),X is X1+1);
    (A=b1,build1(F,_,_),food(X1,S),X is X1 -F);
    (A=b2,build2(F,_,_),food(X1,S),X is X1 -F);
    (A=reqm,food(X,S));
    (A=reqe,food(X,S)))).

materials(X,S1):-
    (S1=s0,materials(X));(S1=result(A,S),(
    (A=reqm,materials(X1,S),X is X1+1);
    (A=b1,build1(_,M,_),materials(X1,S),X is X1 -M);
    (A=b2,build2(_,M,_),materials(X1,S),X is X1 -M);
    (A=reqf,materials(X,S));
    (A=reqe,materials(X,S)))).

energy(X,S1):-
    (S1=s0,energy(X));(S1=result(A,S),(
    (A=reqe,energy(X1,S),X is X1+1);
    (A=b1,build1(_,_,E),energy(X1,S),X is X1 -E);
    (A=b2,build2(_,_,E),energy(X1,S),X is X1 -E);
    (A=reqm,energy(X,S));
    (A=reqf,energy(X,S)))).


checkValidB(X,S):-
   ((X=1,build1(F,M,E));(X=2,build2(F,M,E))),
    food(F1,S),materials(M1,S),energy(E1,S),
    F1>=F,M1>=M,E1>=E.

built(result(A,S), N):-
    built(S,N);
    ((N=1,A=b1,checkValidB(1,S));(N=2,A=b2,checkValidB(2,S))).

goal(S):-
    ids(S,1).

goal2(S):-
    built(S,1),built(S,2).

ids(X,L):-
    (call_with_depth_limit(goal2(X),L,R), number(R));
    (call_with_depth_limit(goal2(X),L,R), R=depth_limit_exceeded,
    L1 is L+1, ids(X,L1)).

% ids(X,L):-
%     (call_with_depth_limit(goal(X),L,R), number(R));
%     (call_with_depth_limit(goal(X),L,R), R=depth_limit_exceeded,
%     L1 is L+1, ids(X,L1)).

 %to display the whole succ state:
 %      ?- set_prolog_flag(answer_write_options,[max_depth(0)]).

 %to run:
 %      ?- ids(X,1).   
%goal(result(b1, result(reqf, result(reqm, result(reqm, result(reqe, result(b2, result(reqf, result(reqf, result(reqf, result(reqm, s0))))))))))).
%goal(result(b1, result(reqf, result(reqm, result(b2, result(reqf, result(reqm,result(reqm, result(reqf, result(reqe, result(reqf, s0))))))))))).
%goal(result(b2, result(reqf, result(reqf, result(reqf, result(reqf, result(reqm, result(reqe, result(b1, result(reqm, result(reqm, s0))))))))))).
%goal(result(b1, result(reqf, s0)))