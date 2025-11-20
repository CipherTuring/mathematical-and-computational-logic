:- use_module(library(clpfd)).

% Australia adjacency facts
adjacent_au(wa, nt).
adjacent_au(wa, sa).
adjacent_au(nt, sa).
adjacent_au(nt, q).
adjacent_au(sa, q).
adjacent_au(sa, nsw).
adjacent_au(sa, v).
adjacent_au(q, nsw).
adjacent_au(nsw, v).

% Australia regions and edges
regions_au([wa, nt, sa, q, nsw, v, t]).

edges_au(Edges) :-
    findall(A-B, adjacent_au(A, B), Edges).

% South America adjacency facts
adjacent_sa(ar, bo).
adjacent_sa(ar, br).
adjacent_sa(ar, cl).
adjacent_sa(ar, py).
adjacent_sa(ar, uy).
adjacent_sa(bo, br).
adjacent_sa(bo, cl).
adjacent_sa(bo, py).
adjacent_sa(bo, pe).
adjacent_sa(br, co).
adjacent_sa(br, gy).
adjacent_sa(br, gfr).
adjacent_sa(br, py).
adjacent_sa(br, pe).
adjacent_sa(br, uy).
adjacent_sa(br, ve).
adjacent_sa(cl, pe).
adjacent_sa(co, ec).
adjacent_sa(co, pe).
adjacent_sa(co, ve).
adjacent_sa(ec, pe).
adjacent_sa(gy, gfr).
adjacent_sa(gy, su).
adjacent_sa(gy, ve).
adjacent_sa(gfr, su).
adjacent_sa(py, uy).

% South America regions and edges
regions_sa([ar, bo, br, cl, co, ec, gy, gfr, py, pe, su, uy, ve]).

edges_sa(Edges) :-
    findall(A-B, adjacent_sa(A, B), Edges).

% Color names mapping
color_names([1-red, 2-green, 3-blue, 4-yellow, 5-purple, 6-orange, 7-pink]).

% Core constraint model for map coloring
map_color(Regions, Vars, Edges, K) :-
    length(Regions, N),
    length(Vars, N),
    Vars ins 1..K,
    maplist(apply_constraint(Regions, Vars), Edges).

apply_constraint(Regions, Vars, A-B) :-
    nth1(IndexA, Regions, A),
    nth1(IndexB, Regions, B),
    nth1(IndexA, Vars, ColorA),
    nth1(IndexB, Vars, ColorB),
    ColorA #\= ColorB.

% Core coloring predicate with labeling
color_map(Regions, Edges, K, Vars) :-
    map_color(Regions, Vars, Edges, K),
    labeling([ffc], Vars).

% Australia coloring
colorize_au(K, Vars) :-
    regions_au(Regions),
    edges_au(Edges),
    color_map(Regions, Edges, K, Vars).

% South America coloring
colorize_sa(K, Vars) :-
    regions_sa(Regions),
    edges_sa(Edges),
    color_map(Regions, Edges, K, Vars).


% Task A: Implement min_colors/5
min_colors(Regions, Edges, MaxK, MinK, Vars) :-
    between(1, MaxK, K),
    color_map(Regions, Edges, K, Vars),
    MinK = K, !.

% Convenience predicates
min_colors_au(MaxK, MinK, Vars) :-
    regions_au(Rs),
    edges_au(Es),
    min_colors(Rs, Es, MaxK, MinK, Vars).

min_colors_sa(MaxK, MinK, Vars) :-
    regions_sa(Rs),
    edges_sa(Es),
    min_colors(Rs, Es, MaxK, MinK, Vars).

% Task B: Pretty Printing (already implemented, but enhanced)
pretty_color_by_region(Regions, Colors) :-
    color_names(Names),
    maplist(print_region_color(Names), Regions, Colors).

print_region_color(Names, Region, Color) :-
    member(Color-Name, Names),
    format('~w = ~w~n', [Region, Name]).

% Task C: Test predicates for experiments
test_au_3 :-
    colorize_au(3, Vars),
    regions_au(Rs),
    pretty_color_by_region(Rs, Vars).

test_au_4 :-
    colorize_au(4, Vars),
    regions_au(Rs),
    pretty_color_by_region(Rs, Vars).

test_sa_3 :-
    colorize_sa(3, Vars),
    regions_sa(Rs),
    pretty_color_by_region(Rs, Vars).

test_sa_4 :-
    colorize_sa(4, Vars),
    regions_sa(Rs),
    pretty_color_by_region(Rs, Vars).

% Labeling strategies examples
test_au_ffc :-
    regions_au(Regions),
    edges_au(Edges),
    map_color(Regions, Vars, Edges, 3),
    labeling([ffc], Vars),
    pretty_color_by_region(Regions, Vars).

test_sa_min :-
    regions_sa(Regions),
    edges_sa(Edges),
    map_color(Regions, Vars, Edges, 4),
    labeling([min], Vars),
    pretty_color_by_region(Regions, Vars).

% New test predicates for minimum colors
test_min_au :-
    min_colors_au(4, MinK, Vars),
    regions_au(Rs),
    format('Minimum colors for Australia: ~w~n', [MinK]),
    pretty_color_by_region(Rs, Vars).

test_min_sa :-
    min_colors_sa(6, MinK, Vars),
    regions_sa(Rs),
    format('Minimum colors for South America: ~w~n', [MinK]),
    pretty_color_by_region(Rs, Vars).

% Experiment with different labeling strategies
color_map_strategy(Regions, Edges, K, Vars, Strategy) :-
    map_color(Regions, Vars, Edges, K),
    labeling(Strategy, Vars).

min_colors_strategy(Regions, Edges, MaxK, MinK, Vars, Strategy) :-
    between(1, MaxK, K),
    color_map_strategy(Regions, Edges, K, Vars, Strategy),
    MinK = K, !.

% Test different strategies
test_strategies_au :-
    regions_au(Rs),
    edges_au(Es),
    format('Testing Australia with different labeling strategies:~n'),
    (   Strategy = [],
        min_colors_strategy(Rs, Es, 4, MinK, Vars, Strategy),
        format('Strategy []: Min colors = ~w~n', [MinK]),
        fail
    ;   Strategy = [ffc],
        min_colors_strategy(Rs, Es, 4, MinK, Vars, Strategy),
        format('Strategy [ffc]: Min colors = ~w~n', [MinK]),
        fail
    ;   Strategy = [min],
        min_colors_strategy(Rs, Es, 4, MinK, Vars, Strategy),
        format('Strategy [min]: Min colors = ~w~n', [MinK])
    ).

test_strategies_sa :-
    regions_sa(Rs),
    edges_sa(Es),
    format('Testing South America with different labeling strategies:~n'),
    (   Strategy = [],
        min_colors_strategy(Rs, Es, 6, MinK, Vars, Strategy),
        format('Strategy []: Min colors = ~w~n', [MinK]),
        fail
    ;   Strategy = [ffc],
        min_colors_strategy(Rs, Es, 6, MinK, Vars, Strategy),
        format('Strategy [ffc]: Min colors = ~w~n', [MinK]),
        fail
    ;   Strategy = [min],
        min_colors_strategy(Rs, Es, 6, MinK, Vars, Strategy),
        format('Strategy [min]: Min colors = ~w~n', [MinK])
    ).