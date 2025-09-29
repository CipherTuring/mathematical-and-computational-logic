# Lab02 - Expert Systems
An expert system is a computer program designed to make decisions or solve problems in a specific area, mimicking how a human expert would. It relies on a knowledge base (facts and information about the area) and an inference engine that applies rules to deduce solutions or recommendations. This lab focuses on creating a basic expert system in Prolog by implementing a knowledge base and an inference engine. For this lab, I proposed two expert systems: one developed in class and one of my own.

<img width="1536" height="1024" alt="image" src="https://github.com/user-attachments/assets/a2eb43ca-69ab-4ef6-ba21-6688d7e22420" />

---

## Animal Classification
This Prolog program is an expert system designed to classify animals based on their physical characteristics, diet, and reproductive method. It provides interactive questions to the user and outputs a list of possible animals that match the given answers.

### 1. Knowledge Base
The knowledge base defines **facts about animals**:

- **Physical properties**:
```prolog
has_fur(cat).
lays_eggs(chicken).
barks(dog).
meows(cat).
```
- **Diet**:
```prolog
eats_meat(lion).
eats_plants(rabbit).
```

- **Reproduction**:
```prolog
lays_eggs(eagle).
gives_birth(lion).
```

### 2. Classification Rules

Rules are used to infer animal types, diet, and reproductive methods:

- **Type classification**:
    
```prolog
is_mammal(X) :- has_fur(X). is_bird(X)   :- lays_eggs(X).
```


- **Diet classification**:
```prolog
carnivore(X) :- eats_meat(X), \+ eats_plants(X). herbivore(X) :- eats_plants(X), \+ eats_meat(X). omnivore(X)  :- eats_plants(X), eats_meat(X).
```


- **Reproduction classification**:
    

```prolog
oviparous(X)  :- lays_eggs(X). viviparous(X) :- gives_birth(X).
```

These rules allow the system to **infer animal properties** based on facts and user input.



### 3. Interactive Questions

The system interacts with the user using the `ask/2` predicate:

```prolog
ask('Does it have fur?', Fur). ask('Does it eat meat?', Meat).`
```
It reads user input (`yes` or `no`) and uses it to guide the inference process.



### 4. Inference Engine

Two main predicates handle animal identification:

#### a) `identify_animal/1`

- Asks about fur, diet (meat/plants).
    
- Determines **type** (mammal or bird) and **diet** (carnivore, herbivore, omnivore).
    
- Uses `findall/3` to list **all animals that match the given characteristics**:
    

```prolog
findall(A,         (   (Type == mammal -> is_mammal(A) ; is_bird(A)),             (Diet == carnivore -> carnivore(A) ;              Diet == herbivore -> herbivore(A) ;              Diet == omnivore -> omnivore(A) ;              true)         ),     Animals), write('Possible animals: '), write(Animals).`
```
#### b) `identify_by_reproduction/1`

- Asks about reproduction (lays eggs or gives birth).
    
- Determines **reproduction type** (`oviparous` or `viviparous`).
    
- Uses `findall/3` to list **all animals matching the reproduction method**:
    

```prolog
findall(A,         (   (Reproduction == oviparous -> oviparous(A) ;              Reproduction == viviparous -> viviparous(A) ;              true)         ),     Animals), write('Possible animals by reproduction: '), write(Animals).
```

## Example Queries

### 1. Identify animal by fur and diet

**Query:**

```prolog
?- identify_animal(_).
```

**Interaction:**

```
Does it have fur? (yes/no): yes
Does it eat meat? (yes/no): yes
Does it eat plants? (yes/no): no
```

**Result:**

```
Possible animals: [lion, tiger, cat]
```

### 2. Identify animal by omnivorous diet

**Query:**

```prolog
?- identify_animal(_).
```

**Interaction:**

```
Does it have fur? (yes/no): yes
Does it eat meat? (yes/no): yes
Does it eat plants? (yes/no): yes
```

**Result:**

```
Possible animals: [bear, monkey]
```

### 3. Identify animal by herbivorous diet

**Query:**

```prolog
?- identify_animal(_).
```

**Interaction:**

```
Does it have fur? (yes/no): yes
Does it eat meat? (yes/no): no
Does it eat plants? (yes/no): yes
```

**Result:**

```
Possible animals: [rabbit, cow, horse, monkey]
```

### 4. Identify bird

**Query:**

```prolog
?- identify_animal(_).
```

**Interaction:**

```
Does it have fur? (yes/no): no
Does it eat meat? (yes/no): yes
Does it eat plants? (yes/no): no
```

**Result:**

```
Possible animals: [eagle, penguin, owl]
```

### 5. Identify animals by reproduction method (oviparous)

**Query:**

```prolog
?- identify_by_reproduction(_).
```

**Interaction:**

```
Does it lay eggs? (yes/no): yes
Does it give birth? (yes/no): no
```

**Result:**

```
Possible animals by reproduction: [chicken, duck, eagle, penguin, owl]
```

### 6. Identify animals by reproduction method (viviparous)

**Query:**

```prolog
?- identify_by_reproduction(_).
```

**Interaction:**

```
Does it lay eggs? (yes/no): no
Does it give birth? (yes/no): yes
```

**Result:**

```
Possible animals by reproduction: [lion, cow, dog, horse, tiger, rabbit, bear, monkey]
```


---

## Study Techniques Expert System 

This Prolog program is an **expert system** that recommends the most suitable study technique for a student based on their **study goal** and **learning preferences**. It interacts with the user, collects input, and uses rules to determine the optimal technique.


### 1. Knowledge Base

The knowledge base stores the available study techniques and detailed instructions for each:

* **Techniques** are categorized by purpose:

```prolog
technique("Flashcards", memorizing).
technique("Mind Mapping", understanding).
technique("Pomodoro Technique", quick_review).
```

* **Instructions** provide a description and step-by-step guidance:

```prolog
instructions("Flashcards", "Cards with questions and answers to review concepts.", ["Write the question on one side", "Write the answer on the other side", "Review daily and reorganize depending on difficulty"]).
```

### 2. Inference Rules

Rules connect **user goals and preferences** with study techniques:

* **Memorization rules**:

```prolog
recommend("Flashcards") :- goal(memorizing), preference(short_sessions).
recommend("Spaced repetition") :- goal(memorizing), preference(long_term).
```

* **Understanding rules**:

```prolog
recommend("Mind Mapping") :- goal(understanding), preference(visual).
recommend("Self-explanation") :- goal(understanding), preference(teaching).
```

* **Quick review rules**:

```prolog
recommend("Highlighting") :- goal(quick_review), preference(visual).
recommend("Pomodoro Technique") :- goal(quick_review), preference(time_management).
```

These rules allow the system to **infer the most suitable technique** based on user input.

### 3. User Interaction

The system collects input using `ask_goal/0` and `ask_preference/0` predicates. Example:

```prolog
?- run_expert_system.
```

The system asks:

```
What is your main study goal? (1. Memorize, 2. Deep understanding, 3. Quick review)
What is your preferred learning style? (1. Short sessions, 2. Audio-based learning, ...)
```

The user enters their choices, which are stored dynamically using `assert/1`.

### 4. Inference Engine

After collecting the inputs, the system evaluates the `recommend/1` rules and outputs:

* Recommended study technique
* Description of the technique
* Step-by-step instructions

This allows the user to **understand why a technique is suggested** and how to implement it.

## Example Queries

### 1. Memorization with short sessions

**Query:**

```prolog
?- run_expert_system.
```

**Interaction:**

```
What is your main study goal?
1. Memorize
2. Deep understanding
3. Quick review
|: 1
What is your preferred learning style?
1. Short sessions
2. Audio-based learning
3. Visual methods
4. Long-term retention
5. Practical exercises
6. Teaching/explaining to others
7. Writing summaries
8. Time management
|: 1
```

**Result:**

```
Recommended technique: Flashcards
Description: Cards with questions and answers to review concepts.
Steps:
- Write the question on one side
- Write the answer on the other side
- Review daily and reorganize depending on difficulty
```

### 2. Memorization with long-term retention

**Query:**

```prolog
?- run_expert_system.
```

**Interaction:**

```
What is your main study goal?
|: 1
What is your preferred learning style?
|: 4
```

**Result:**

```
Recommended technique: Spaced repetition
Description: Reviewing material at increasing intervals to improve long-term retention.
Steps:
- Use flashcards or questions
- Review after 1 day, then 3 days, then 1 week, etc.
- Focus more on difficult items
```

### 3. Deep understanding with teaching

**Query:**

```prolog
?- run_expert_system.
```

**Interaction:**

```
What is your main study goal?
|: 2
What is your preferred learning style?
|: 6
```

**Result:**

```
Recommended technique: Self-explanation
Description: Explaining the material to yourself as if teaching someone else.
Steps:
- Read a concept or solve a problem
- Explain the reasoning out loud or in writing
- Identify gaps in understanding and clarify them
```

### 4. Quick review with visual methods

**Query:**

```prolog
?- run_expert_system.
```

**Interaction:**

```
What is your main study goal?
|: 3
What is your preferred learning style?
|: 3
```

**Result:**

```
Recommended technique: Highlighting
Description: Marking key concepts for quick review.
Steps:
- Read the material once fully
- Highlight only the essential ideas
- Use different colors for categories
```

### 5. Quick review with time management

**Query:**

```prolog
?- run_expert_system.
```

**Interaction:**

```
What is your main study goal?
|: 3
What is your preferred learning style?
|: 8
```

**Result:**

```
Recommended technique: Pomodoro Technique
Description: Time management technique with short study sessions and breaks.
Steps:
- Study for 25 minutes
- Take a 5-minute break
- Repeat 4 times
- After 4 cycles, take a long break (15–30 minutes)
```

