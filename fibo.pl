fib(N, F) :- N < 2, !,  F is N + 1.
fib(2, 3) :- !. 
fib(N, F) :-  N3 is N - 3, N2 is N - 2, fib(N3, F3), 
             fib(N2, F2), F is F3 + F2 + F2.

fibo(0, F1, F2, F2) :- !.
fibo(1, F1, F2, F1) :- !.
fibo(N, F1, F2, F) :- N1 is N - 1, Fx is F1+F2, fibo(N1, Fx, F1, F).

fibx(N, F) :- fibo(N, 1, 1, F).

loop(0, Acc, Acc) :- !.
loop(N, Acc, F) :- plus(A1, Acc, 0.1), minus(N1, N, 1), loop(N1, A1, F).


