% List lenght 
length_list([], 0).
length_list([_|T], Length) :-
    length_list(T, TailLength),
    Length is 1 + TailLength.


% List Append
append_list([], List, List).
append_list([H|T], List2, [H|Result]) :-
    append_list(T, List2, Result).