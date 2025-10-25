# Prolog Lab 4: AI Logic Playground – Intelligent Maze Advisor

## **Overview:**

This project implements a **logical reasoning system in Prolog** to explore a maze represented as a directed graph. The program finds all possible paths from the `entrance` node to the `exit` node, showing step-by-step reasoning for each move.

**Main Features:**

* Maze representation using `edge/2` and `blocked/2`.
* `can_move/2` rule to determine possible moves.
* `reason/3` predicate that explains each movement with three possible reasons:

  1. `'destination reached'`
  2. `'path is open'`
  3. `'path is blocked'`
* Main predicate `find_all_paths/0` that finds and displays all paths from `entrance` to `exit`.
* Modular structure: separate functions for silent path search (`silent_find_path/3`) and reasoning visualization (`display_path_with_reasoning/1`).

## **Code Structure:**

1. **Maze representation:**

   ```prolog
   edge(entrance, a).
   edge(a, b).
   blocked(a, c).
   ```
2. **Reasoning rules:**

   ```prolog
   can_move(X, Y) :- edge(X, Y), \+ blocked(X, Y).
   reason(X, Y, 'destination reached') :- Y == exit, can_move(X, Y).
   ```
3. **Path search:**

   ```prolog
   find_all_paths :- ...
   silent_find_path(X, Y, Path) :- ...
   silent_move(X, Y, Visited, Path) :- ...
   ```
4. **Textual visualization:**

   ```prolog
   display_path_with_reasoning([Start, Next | Rest]) :- ...
   ```

## Graph Visualization (Extra Feature):

* Added support for **Graphviz** to generate `.dot` files.
* **Requirements:**

  1. Install Graphviz on your operating system.

     * Windows: [https://graphviz.org/download/](https://graphviz.org/download/)
     * Ubuntu/Debian: `sudo apt install graphviz`
  2. Ensure the `dot` command is in your **PATH**.
* **Meaning of colors in the graph:**

  | Color        | Element            | Meaning                                                        |
  | ------------ | ------------------ | -------------------------------------------------------------- |
  | Green        | Node `entrance`    | Start of the maze                                              |
  | Red          | Node `exit`        | Destination                                                    |
  | Light blue   | Intermediate nodes | Normal nodes                                                   |
  | Light gray   | Normal edges       | Possible moves without blocking and not part of the final path |
  | Red (dashed) | Blocked edges      | Moves not allowed (`blocked/2`)                                |
  | Blue         | Final path         | Path from `entrance` to `exit` (thick line)                    |

 3. Visualize the archives
The visualization is generated with the query:
```prolog
?- find_all_paths.
```
Each found path generates a separate `.dot` file (`path_1.dot`, `path_2.dot`, ...).

To visualize each one of these archives, you need to copy each code inside each archive and paste it in: 
[https://edotor.net/]


## **Example Queries:**

1. Find and display all paths:

   ```prolog
   ?- find_all_paths.
   ```
2. Find a specific path silently:

   ```prolog
   ?- silent_find_path(entrance, exit, Path).
   Path = [entrance, a, b, exit] ; ...
   ```
3. Check if a move is possible:

   ```prolog
   ?- can_move(a, b).
   true.
   ?- can_move(a, c).
   false.
   ```
4. Get the reason for a move:

   ```prolog
   ?- reason(b, exit, Reason).
   Reason = 'destination reached'.
   ```

## Note:
The term `silent` means that the rule is not going to give an output and only will be execute it.
