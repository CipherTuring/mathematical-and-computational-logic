%-----------------------------------
% Maze representation
%-----------------------------------
edge(entrance, a).
edge(a, b).
edge(a, c).
edge(a, d).
edge(a, e).
edge(b, z).
edge(e, z).
edge(e, exit).
edge(z, exit).
edge(d, exit).
edge(b, exit).
edge(c, b).
blocked(a, c).
blocked(a, e).

%----------------------------------------------------------------------
% Reasoning rules
%----------------------------------------------------------------------
can_move(X, Y) :- edge(X, Y), \+ blocked(X, Y).
reason(X, Y, 'destination reached') :- Y == exit, can_move(X, Y).
reason(X, Y, 'path is open') :- can_move(X, Y), Y \== exit.
reason(X, Y, 'path is blocked') :- blocked(X, Y).

%-----------------------------------------------------------------------
% Main predicate - finds and displays all paths
%-----------------------------------------------------------------------
find_all_paths :-
    % Find all paths silently first
    findall(Path, silent_find_path(entrance, exit, Path), AllPaths),
    
    % Display each path with complete reasoning
    display_all_paths(AllPaths, 1),
    
    % Generate visualizations for each path
    generate_all_visualizations(AllPaths).

% Helper: find paths without giving an output
silent_find_path(X, Y, Path) :-
    silent_move(X, Y, [X], RevPath),
    reverse(RevPath, Path).

silent_move(X, Y, Visited, [Y|Visited]) :- 
    can_move(X, Y).

silent_move(X, Y, Visited, Path) :- 
    can_move(X, Z),
    Z \== Y,
    \+ member(Z, Visited),
    silent_move(Z, Y, [Z|Visited], Path).

% Display all paths with reasoning
display_all_paths([], _).
display_all_paths([Path | Rest], N) :-
    format('--- Beginning search for Path ~w ---~n', [N]),
    display_path_with_reasoning(Path),
    format('PATH ~w COMPLETE: ~w~n', [N, Path]),
    (   Rest \= []
    ->  format('Initiating search for next alternative path...~n~n'),
        N1 is N + 1,
        display_all_paths(Rest, N1)
    ;   format('No more paths found.~n')
    ).

% Display reasoning for a specific path
display_path_with_reasoning([Start, End]) :-
    format('Moving from ~w to ~w: ', [Start, End]),
    reason(Start, End, Reason),
    format('~w~n', [Reason]).

display_path_with_reasoning([Start, Next | Rest]) :-
    format('Exploring from ~w to ~w: ', [Start, Next]),
    reason(Start, Next, Reason),
    format('~w~n', [Reason]),
    display_path_with_reasoning([Next | Rest]).




%---------------------------------------------------------------------------------------
% GRAPH VISUALIZATION
%---------------------------------------------------------------------------------------
generate_all_visualizations(AllPaths) :-
    generate_all_visualizations(AllPaths, 1).

generate_all_visualizations([], _).
generate_all_visualizations([Path | Rest], N) :-
    generate_simple_visualization(Path, N),
    N1 is N + 1,
    generate_all_visualizations(Rest, N1).

generate_simple_visualization(Path, N) :-
    atomic_list_concat(['path_', N, '.dot'], FileName),
    open(FileName, write, Stream),
    write_graph_header(Stream, N),
    write_all_edges(Stream),
    write_highlighted_path(Stream, Path),
    format(Stream, '}~n', []),
    close(Stream),
    format('Generated visualization: ~w~n', [FileName]).

write_graph_header(Stream, N) :-
    format(Stream, 'digraph Path~w {~n', [N]),
    format(Stream, '    rankdir=TB;~n', []),
    format(Stream, '    node [shape=circle, style=filled];~n', []),
    format(Stream, '    entrance [fillcolor=green];~n', []),
    format(Stream, '    exit [fillcolor=red];~n', []),
    format(Stream, '    node [fillcolor=lightblue];~n', []).

write_all_edges(Stream) :-
    forall(edge(X, Y), (
        (blocked(X, Y) 
         -> format(Stream, '    ~w -> ~w [color=red, style=dashed, label="blocked"];~n', [X, Y])
         ;  format(Stream, '    ~w -> ~w [color=lightgray];~n', [X, Y])
        )
    )).

write_highlighted_path(_, []).
write_highlighted_path(_, [_]).
write_highlighted_path(Stream, [From, To | Rest]) :-
    format(Stream, '    ~w -> ~w [color=blue, penwidth=3.0];~n', [From, To]),
    write_highlighted_path(Stream, [To | Rest]).


get_path_color(1, blue).
get_path_color(2, green).
get_path_color(3, orange).
get_path_color(4, purple).
get_path_color(_, black).


%----------------------------------------------------------------------
% REASONING SYSTEM IMPLEMENTATION SUMMARY
%----------------------------------------------------------------------
% The reasoning system is implemented through a multi-layered logical framework that combines 
% factual knowledge with inferential rules. At the base level, the maze structure is represented 
% as a graph using edge/2 facts, while blocked/2 facts define constraints. The can_move/2 predicate 
% serves as the core inference engine, determining valid movements by checking both connectivity 
% and constraints. The reason/3 predicate then translates these logical conclusions into 
% human-readable explanations, categorizing each step as 'path is open', 'path is blocked', 
% or 'destination reached'. During path traversal, the system dynamically generates and displays 
% these explanations, creating a transparent audit trail of the AI's decision-making process. 
% This approach demonstrates symbolic AI in action, where logical deduction is coupled with 
% explanatory capabilities to create an intelligent system that not only finds solutions but 
% also articulates its reasoning at every step.
%----------------------------------------------------------------------