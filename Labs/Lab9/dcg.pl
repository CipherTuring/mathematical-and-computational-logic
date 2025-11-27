sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, adjectives, noun.


verb_phrase --> verb, noun_phrase.

adjectives --> [].
adjectives --> adjective, adjectives.

% Lexicon

% Determinantes [cite: 19]
determiner --> [the].
determiner --> [a].

% Sustantives
noun --> [cat].
noun --> [dog].
noun --> [fish].
noun --> [bird].

% Verbs
verb --> [eats].
verb --> [sees].

% Adjetives
adjective --> [big].
adjective --> [small].
adjective --> [angry].