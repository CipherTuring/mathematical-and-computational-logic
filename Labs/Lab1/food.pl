% Facts
likes(alicia, burguer).
likes(juan, pasta).
likes(maria, pizza).
likes(carlos, sushi).
likes(laura, pasta).
likes(pedro, pizza).
likes(sofia, burguer).
likes(diego, sushi).

% Rules

food_friend(X, Y) :- likes(X, Food), likes(Y, Food), X \= Y.

