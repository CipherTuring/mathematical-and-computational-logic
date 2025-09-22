% Family Tree Knowledge Base - Facts

parent(john, mary).
parent(mary, susan).
parent(mary, bob).
parent(susan, alice).
parent(alice, tom).
parent(bob, lisa).
parent(john, david).
parent(david, emma).
parent(emma, noah).
parent(susan, olivia).

% Rules

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).