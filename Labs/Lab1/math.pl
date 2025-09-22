% Factorial

factorial(0, 1).
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, Result1),
    Result is N * Result1.


% Sum of a list
sum_list([], 0).
sum_list([H | T], Sum) :-
    sum_list(T, TailSum),
    Sum is H + TailSum.
