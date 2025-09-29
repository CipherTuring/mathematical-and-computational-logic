
% This space is made to ignore all warnings caused by discontiguous predicates

:- discontiguous has_fur/1.
:- discontiguous eats_meat/1.
:- discontiguous eats_plants/1.
:- discontiguous lays_eggs/1.

% ==============================
% Knowlege Base
% Define facts about animals and their properties
% ==============================

% Basic properties
has_fur(cat).
has_fur(dog).
lays_eggs(chicken).
lays_eggs(duck).
barks(dog).
meows(cat).

% Expansion of Knowledge Base
has_fur(bear).
has_fur(lion).
has_fur(rabbit).
has_fur(cow).
has_fur(monkey).
has_fur(horse).
has_fur(tiger).

% Animal Diet
eats_meat(lion).
eats_meat(bear).
eats_plants(bear).
eats_meat(tiger).
eats_meat(cat).
eats_meat(dog).
eats_meat(eagle).
eats_meat(penguin).
eats_meat(owl).
eats_plants(dog).
eats_plants(rabbit).
eats_plants(cow).
eats_plants(horse).
eats_plants(monkey).
eats_plants(chicken).
eats_plants(duck).

% Animal Reproduction
lays_eggs(eagle).
lays_eggs(penguin).
lays_eggs(owl).
gives_birth(lion).
gives_birth(cow).
gives_birth(dog).
gives_birth(horse).
gives_birth(tiger).
gives_birth(rabbit).
gives_birth(bear).
gives_birth(monkey).

% ==============================
% Rules for Classification
% ==============================

% Type
is_mammal(X) :- has_fur(X).
is_bird(X)   :- lays_eggs(X).

% Diet
carnivore(X) :- eats_meat(X), \+ eats_plants(X).
herbivore(X) :- eats_plants(X), \+ eats_meat(X).
omnivore(X)  :- eats_plants(X), eats_meat(X).

% Reproduction
oviparous(X)  :- lays_eggs(X).
viviparous(X) :- gives_birth(X).

% ==============================
% Interactive questions
% ==============================

ask(Question, Answer) :-
    write(Question), write(' (yes/no):'), nl,
    read(Answer).

% ==============================
% Inference Engine
% ==============================

identify_animal(_) :-
    ask('Does it have fur?', Fur),         
    ask('Does it eat meat?', Meat),
    ask('Does it eat plants?', Plants),

    % Determine type
    (Fur == yes -> Type = mammal ; Type = bird),
    
    % Determine diet
    (Meat == yes, Plants == yes -> Diet = omnivore ;
     Meat == yes, Plants \== yes -> Diet = carnivore ;
     Meat \== yes, Plants == yes -> Diet = herbivore ;
     Diet = unknown),

    % Find matching animals
    findall(A,
        (   (Type == mammal -> is_mammal(A) ; is_bird(A)),
            (Diet == carnivore -> carnivore(A) ;
             Diet == herbivore -> herbivore(A) ;
             Diet == omnivore -> omnivore(A) ;
             true)
        ),
    Animals),

    write('Possible animals: '), write(Animals), nl.


identify_by_reproduction(_) :-
    ask('Does it lay eggs?', LayEggs),
    ask('Does it give birth?', GiveBirth),
    
    % Determine reproduction type
    (LayEggs == yes -> Reproduction = oviparous ;
     GiveBirth == yes -> Reproduction = viviparous ;
     Reproduction = unknown),

    % Find all animals that match the reproduction type
    findall(A,
        (   (Reproduction == oviparous -> oviparous(A) ;
             Reproduction == viviparous -> viviparous(A) ;
             true)
        ),
    Animals),

    write('Possible animals by reproduction: '), write(Animals), nl.
