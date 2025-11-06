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
    
    findall(task(Name, Duration, Resource), task(Name, Duration, Resource), Tasks),
    
    
    length(Tasks, N),
    length(Starts, N),
    length(Ends, N),
    
    % Domain
    Starts ins 0..100,
    
    
    set_duration_constraints(Tasks, Starts, Ends),
    

    set_precedence_constraints(Tasks, Starts, Ends),
    
    
    set_resource_constraints(Tasks, Starts, Ends),
    
    
    list_max(Ends, Makespan),
    
    
    append(Starts, [Makespan], AllVars),
    labeling([min(Makespan)], AllVars),
    
    
    print_schedule(Tasks, Starts, Ends, Makespan).


set_duration_constraints([], [], []).
set_duration_constraints([task(_, Duration, _)|Tasks], [Start|Starts], [End|Ends]) :-
    End #= Start + Duration,
    set_duration_constraints(Tasks, Starts, Ends).


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


set_resource_constraints(Tasks, Starts, Ends) :-
    % Group tasks by resource
    findall(Resource, task(_, _, Resource), AllResources),
    sort(AllResources, UniqueResources),
    
    
    apply_resource_constraints(UniqueResources, Tasks, Starts, Ends).

apply_resource_constraints([], _, _, _).
apply_resource_constraints([Resource|Resources], Tasks, Starts, Ends) :-
    % Get indices of tasks sharing this resource
    find_task_indices_with_resource(Tasks, Resource, 1, Indices),
    
    
    (   length(Indices, L), L > 1
    ->  constrain_non_overlap(Indices, Starts, Ends, Tasks)
    ;   true
    ),
    
    apply_resource_constraints(Resources, Tasks, Starts, Ends).


find_task_indices_with_resource([], _, _, []).
find_task_indices_with_resource([task(_, _, Res)|Tasks], Resource, Index, Result) :-
    NextIndex is Index + 1,
    find_task_indices_with_resource(Tasks, Resource, NextIndex, Rest),
    (   Res =:= Resource
    ->  Result = [Index|Rest]
    ;   Result = Rest
    ).


constrain_non_overlap([], _, _, _).
constrain_non_overlap([I|Indices], Starts, Ends, Tasks) :-
    constrain_non_overlap_pairs(I, Indices, Starts, Ends, Tasks),
    constrain_non_overlap(Indices, Starts, Ends, Tasks).


constrain_non_overlap_pairs(_, [], _, _, _).
constrain_non_overlap_pairs(I, [J|Js], Starts, Ends, Tasks) :-
    nth1(I, Starts, SI),
    nth1(I, Ends, EI),
    nth1(J, Starts, SJ),
    nth1(J, Ends, EJ),
    
    % Get durations for error messages
    nth1(I, Tasks, task(NameI, DurI, _)),
    nth1(J, Tasks, task(NameJ, DurJ, _)),
    
    % Constraint: I ends before J starts OR J ends before I starts
    EI #=< SJ #\/ EJ #=< SI,
    
    constrain_non_overlap_pairs(I, Js, Starts, Ends, Tasks).

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
    format('~n=== OPTIMAL SCHEDULE ===~n~n'),
    print_task_details(Tasks, Starts, Ends, 1),
    format('~nTotal project time (makespan): ~d time units~n~n', [Makespan]).

print_task_details([], [], [], _).
print_task_details([task(Name, Duration, Resource)|Tasks], [Start|Starts], [End|Ends], N) :-
    format('Task ~w:~n', [Name]),
    format('  Duration: ~d~n', [Duration]),
    format('  Resource: ~d~n', [Resource]),
    format('  Start: ~d~n', [Start]),
    format('  End: ~d~n', [End]),
    format('  ---~n'),
    NextN is N + 1,
    print_task_details(Tasks, Starts, Ends, NextN).


lab_example :-
    format('Lab Example: Three Tasks~n~n'),
    
    Tasks = [task(a, 3, 1), task(b, 2, 1), task(c, 4, 2)],
    Starts = [Sa, Sb, Sc],
    Ends = [Ea, Eb, Ec],
    
    % Domains
    Sa in 0..10, Sb in 0..10, Sc in 0..10,
    
    % Duration constraints
    Ea #= Sa + 3,
    Eb #= Sb + 2, 
    Ec #= Sc + 4,
    
    % Non-overlap constraints for resource 1
    Ea #=< Sb #\/ Eb #=< Sa,
    
    % Makespan
    Makespan #= max(max(Ea, Eb), Ec),
    
    % Optimization
    labeling([min(Makespan)], [Sa, Sb, Sc]),
    
    % Results
    format('Example results:~n'),
    format('Task a: Start=~d, End=~d~n', [Sa, Ea]),
    format('Task b: Start=~d, End=~d~n', [Sb, Eb]),
    format('Task c: Start=~d, End=~d~n', [Sc, Ec]),
    format('Makespan: ~d~n~n', [Makespan]).

% Execute scheduling when loading
:- initialization(main).

main :-
    format('Calculating Schedule~n'),
    format('==============================================~n~n'),
    schedule.