% ====================================
% Expert System: Study Techniques
% ====================================

% ------------------------------------
% Knowledge base: Techniques
% ------------------------------------
technique("Flashcards", memorizing).
technique("Audio review", memorizing).
technique("Spaced repetition", memorizing).
technique("Summarization", understanding).
technique("Self-explanation", understanding).
technique("Mind Mapping", understanding).
technique("Practice-problems", understanding).
technique("Highlighting", quick_review).
technique("Pomodoro Technique", quick_review).

% ------------------------------------
% Instructions for each technique
% ------------------------------------
instructions("Flashcards",
    "Cards with questions and answers to review concepts.",
    ["Write the question on one side",
     "Write the answer on the other side",
     "Review daily and reorganize depending on difficulty"]).

instructions("Audio review",
    "Listen to recorded notes or lectures to reinforce memory.",
    ["Record your notes or lectures",
     "Play them back during free time",
     "Repeat regularly to reinforce information"]).

instructions("Spaced repetition",
    "Reviewing material at increasing intervals to improve long-term retention.",
    ["Use flashcards or questions",
     "Review after 1 day, then 3 days, then 1 week, etc.",
     "Focus more on difficult items"]).

instructions("Summarization",
    "Summarizing key ideas to strengthen understanding.",
    ["Read the material carefully",
     "Write down the main points in your own words",
     "Review your summaries before exams"]).

instructions("Self-explanation",
    "Explaining the material to yourself as if teaching someone else.",
    ["Read a concept or solve a problem",
     "Explain the reasoning out loud or in writing",
     "Identify gaps in understanding and clarify them"]).

instructions("Mind Mapping",
    "A visual diagram to organize and connect concepts.",
    ["Write the main topic in the center",
     "Add branches for key subtopics",
     "Connect ideas with arrows and keywords"]).

instructions("Practice-problems",
    "Solving exercises to strengthen comprehension.",
    ["Find practice questions or problems",
     "Try solving without looking at notes",
     "Check answers and learn from mistakes"]).

instructions("Highlighting",
    "Marking key concepts for quick review.",
    ["Read the material once fully",
     "Highlight only the essential ideas",
     "Use different colors for categories"]).

instructions("Pomodoro Technique",
    "Time management technique with short study sessions and breaks.",
    ["Study for 25 minutes",
     "Take a 5-minute break",
     "Repeat 4 times",
     "After 4 cycles, take a long break (15–30 minutes)"]).

% ------------------------------------
% Inference rules
% ------------------------------------
% Memorization rules
recommend("Flashcards") :-
    goal(memorizing),
    preference(short_sessions).

recommend("Audio review") :-
    goal(memorizing),
    preference(audio).

recommend("Spaced repetition") :-
    goal(memorizing),
    preference(long_term).

% Understanding rules
recommend("Mind Mapping") :-
    goal(understanding),
    preference(visual).

recommend("Practice-problems") :-
    goal(understanding),
    preference(practical).

recommend("Self-explanation") :-
    goal(understanding),
    preference(teaching).

recommend("Summarization") :-
    goal(understanding),
    preference(writing).

% Quick review rules
recommend("Highlighting") :-
    goal(quick_review),
    preference(visual).

recommend("Pomodoro Technique") :-
    goal(quick_review),
    preference(time_management).

% ------------------------------------
% Inference Engine
% ------------------------------------
ask_goal :-
    write('What is your main study goal?'), nl,
    write('1. Memorize'), nl,
    write('2. Deep understanding'), nl,
    write('3. Quick review'), nl,
    read(Choice),
    (Choice = 1 -> assert(goal(memorizing));
     Choice = 2 -> assert(goal(understanding));
     Choice = 3 -> assert(goal(quick_review));
     write('Invalid choice.'), nl, fail).

ask_preference :-
    write('What is your preferred learning style?'), nl,
    write('1. Short sessions'), nl,
    write('2. Audio-based learning'), nl,
    write('3. Visual methods'), nl,
    write('4. Long-term retention'), nl,
    write('5. Practical exercises'), nl,
    write('6. Teaching/explaining to others'), nl,
    write('7. Writing summaries'), nl,
    write('8. Time management'), nl,
    read(Choice),
    (Choice = 1 -> assert(preference(short_sessions));
     Choice = 2 -> assert(preference(audio));
     Choice = 3 -> assert(preference(visual));
     Choice = 4 -> assert(preference(long_term));
     Choice = 5 -> assert(preference(practical));
     Choice = 6 -> assert(preference(teaching));
     Choice = 7 -> assert(preference(writing));
     Choice = 8 -> assert(preference(time_management));
     write('Invalid choice.'), nl, fail).

run_expert_system :-
    retractall(goal(_)),        % clean previous facts
    retractall(preference(_)),  % clean previous facts
    ask_goal,
    ask_preference,
    (recommend(T) ->
        write('Recommended technique: '), write(T), nl,
        instructions(T, Desc, Steps),
        write('Description: '), write(Desc), nl,
        write('Steps:'), nl,
        forall(member(S, Steps), (write('- '), write(S), nl))
    ;
        write('Sorry, no matching technique found.'), nl).
