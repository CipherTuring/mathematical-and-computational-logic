:- use_module(library(clpfd)).

% task(Name, Duration, Resource)
task(a, 3, 1).
task(b, 2, 1).
task(c, 4, 2).
task(d, 1, 2).
task(e, 2, 1).

% precedes(PreviousTask, NextTask)
precedes(a, b).
precedes(c, d).

schedule :-
    format('Calculating optimal schedule...~n~n'),
    
    
    findall(task(Name, Duration, Resource), task(Name, Duration, Resource), Tasks),
    
    length(Tasks, N),
    length(Starts, N),
    length(Ends, N),
    
    % Domain
    Starts ins 0..100,
    
    
    set_duration_constraints(Tasks, Starts, Ends),
    
    
    set_precedence_constraints(Tasks, Starts, Ends),
    
    
    set_resource_constraints(Tasks, Starts),
    
    % Calculate makespan
    list_max(Ends, Makespan),
    
    % Optimize: minimize makespan
    append(Starts, [Makespan], AllVars),
    labeling([min(Makespan)], AllVars),
    
    % Display results
    print_schedule(Tasks, Starts, Ends, Makespan).


set_duration_constraints([], [], []).
set_duration_constraints([task(_, Duration, _)|Tasks], [Start|Starts], [End|Ends]) :-
    End #= Start + Duration,
    set_duration_constraints(Tasks, Starts, Ends).

% Precedence constraints
set_precedence_constraints(Tasks, Starts, Ends) :-
    findall(precedes(Prev, Next), precedes(Prev, Next), Precedences),
    apply_precedences(Precedences, Tasks, Starts, Ends).

apply_precedences([], _, _, _).
apply_precedences([precedes(Prev, Next)|Rest], Tasks, Starts, Ends) :-
    get_task_index(Tasks, Prev, 1, IndexPrev),
    get_task_index(Tasks, Next, 1, IndexNext),
    nth1(IndexPrev, Ends, EndPrev),
    nth1(IndexNext, Starts, StartNext),
    EndPrev #=< StartNext,
    apply_precedences(Rest, Tasks, Starts, Ends).


set_resource_constraints(Tasks, Starts) :-
    % Create rectangle list for disjoint2
    % rect(Start, Resource, Duration, 1)
    create_rectangles(Tasks, Starts, Rectangles),
    
    % Apply disjoint2 for all tasks
    disjoint2(Rectangles).

% Create rectangles for disjoint2
create_rectangles([], [], []).
create_rectangles([task(_, Duration, Resource)|Tasks], [Start|Starts], [rect(Start, Resource, Duration, 1)|Rects]) :-
    create_rectangles(Tasks, Starts, Rects).

% Get task index by name
get_task_index([task(Name, _, _)|_], Name, Index, Index).
get_task_index([_|Tasks], Name, CurrentIndex, Index) :-
    NextIndex is CurrentIndex + 1,
    get_task_index(Tasks, Name, NextIndex, Index).


list_max([X], X).
list_max([X|Xs], Max) :-
    list_max(Xs, MaxTail),
    Max #= max(X, MaxTail).

% Display the resulting schedule
print_schedule(Tasks, Starts, Ends, Makespan) :-
    format('--- OPTIMAL SCHEDULE FOUND ---~n'),
    print_task_details(Tasks, Starts, Ends),
    format('~nTOTAL TIME (Makespan): ~d~n~n', [Makespan]).

print_task_details([], [], []).
print_task_details([task(Name, Duration, Resource)|Tasks], [Start|Starts], [End|Ends]) :-
    % Convert CLPFD variables to integers for display
    atom_number(StartAtom, Start),
    atom_number(EndAtom, End),
    format('  Task: ~w (Dur: ~d, Res: ~d) |   Start: ~a   |   End: ~a~n', 
           [Name, Duration, Resource, StartAtom, EndAtom]),
    print_task_details(Tasks, Starts, Ends).

:- initialization(schedule).