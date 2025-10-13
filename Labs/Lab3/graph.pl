% ---------------------------------------------------
% Solving a Maze using a graph
% ---------------------------------------------------

% Graph representation

edge(a, b).
edge(b, c).
edge(b, u).
edge(c, d).
edge(d, e).
edge(e, f).
edge(f, g).
edge(g, h).
edge(f, i).
edge(i, j).
edge(j, k).
edge(k, l).
edge(l, m).
edge(m, n).
edge(n, o).
edge(o, p).
edge(n, q).
edge(q, r).
edge(r, s).
edge(s, t).
edge(t, u).
edge(u, v).
edge(v, w).
edge(w, x).
edge(x, y).

% ---------------------------------------------------
% Path-finding rule
% ---------------------------------------------------
path(Start, End, Path) :-
    traverse(Start, End, [Start], RevPath),
    reverse(RevPath, Path).

% Base case: reached destination
traverse(End, End, Visited, Visited).

% Recursive case: follow directed edges
traverse(Current, End, Visited, Path) :-
    edge(Current, Next),
    \+ member(Next, Visited),       % avoid loops
    traverse(Next, End, [Next|Visited], Path).

% ---------------------------------------------------
% Find all possible paths from enter (a) to exit (y)
% ---------------------------------------------------
all_paths(Paths) :-
    findall(Path, path(a, y, Path), Paths).

% ---------------------------------------------------
% Find the shortest path from all available ones 
% (This is an extra feature that I added, inspired by the midterm excercise :D)
% ---------------------------------------------------
shortest_path(Shortest) :-
    all_paths(Paths),
    sort_by_length(Paths, [Shortest|_]).

% Helper predicate: sort paths by their length
sort_by_length(Paths, Sorted) :-
    map_list_to_pairs(length, Paths, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

% ---------------------------------------------------
% Solve and print results (This is just like a "main function", resolves all at once)
% ---------------------------------------------------
solve_maze :-
    all_paths(Paths),
    write('--- All possible paths from enter to exit ---'), nl,
    print_paths(Paths),
    nl, write('--- Shortest path ---'), nl,
    shortest_path(Shortest),
    write(Shortest), nl.

% This part just helps to print each path in a new line :D
print_paths([]).
print_paths([P|Rest]) :-
    write(P), nl,
    print_paths(Rest).


