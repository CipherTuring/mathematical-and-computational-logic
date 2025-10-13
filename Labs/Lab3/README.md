# Lab 03 - Graph Traversal

## 📖 Description

In this lab, we use **Prolog** to solve a **maze represented as a directed graph**. Each node in the graph represents a position in the maze, and edges represent one-way paths connecting nodes. The program allows you to:

* Find a path from a **specific start node** to an **end node**.
* Find **all possible paths** between two nodes.
* Identify the **shortest path** from start to end.
* Print the path as a list of nodes traversed.

The maze used in this lab has a fixed **entrance** (`a`) and **exit** (`y`), but the predicates are general enough to allow searches between any two nodes in the graph.

This maze is going to be used as an example:

<img width="893" height="542" alt="graph_maze" src="https://github.com/user-attachments/assets/4dc21144-8b7a-4161-80a2-0013bcdf3efe" />


---

## ⚙️ How it Works

1. **Graph Representation:** The maze is defined using `edge/2` facts. Each fact represents a directed connection from one node to another.
2. **Path-Finding:**

   * The predicate `path(Start, End, Path)` finds a valid path from `Start` to `End`.
   * The recursive `traverse/4` predicate explores possible paths without revisiting nodes (avoiding loops).
3. **All Paths:**

   * The predicate `all_paths(Paths)` finds all paths from the entrance (`a`) to the exit (`y`).
   * A generalized version can find all paths between any two nodes.
4. **Shortest Path:**

   * The predicate `shortest_path(Shortest)` computes the shortest path among all possible paths.
5. **Solve Maze:**

   * `solve_maze/0` prints all paths from entrance to exit and highlights the shortest of these paths.

---

## 💻 Example Queries

### 1. Find a single path from entrance to exit

```prolog
?- path(a, y, Path).
Path = [a, b, u, v, w, x, y] ;
Path = [a, b, c, d, e, f, g, h, ... , y].
```

### 2. Find all paths from entrance to exit

```prolog
?- all_paths(Paths).
Paths = [
  [a, b, u, v, w, x, y],
  [a, b, c, d, e, f, g, h, ... , y]
].
```

### 3. Find the shortest path from entrance to exit

```prolog
?- shortest_path(Shortest).
Shortest = [a, b, u, v, w, x, y].
```

### 4. Find a path between two arbitrary nodes

```prolog
?- path(f, q, Path).
Path = [f, i, j, k, l, m, n, q].
```

### 5. Find all paths between two arbitrary nodes

```prolog
?- all_paths(f, y, Paths).
Paths = [
  [f, g, h, ... , y],
  [f, i, j, ... , y]
].
```
### 6. Find all paths from entrance to exit and its shortest path at once

```prolog
-? solve_maze.

[a,b,c,d,e,f,i,j,k,l,m,n,q,r,s,t,u,v,w,x,y]
[a,b,u,v,w,x,y]

--- Shortest path ---
[a,b,u,v,w,x,y]
```
