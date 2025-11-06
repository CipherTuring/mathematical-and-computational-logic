# Prolog Task Scheduling with CLP(FD)

This project implements a **task scheduling system** in **Prolog** using the **Constraint Logic Programming over Finite Domains (CLP(FD))** library.  
It computes an **optimal schedule** that minimizes the total makespan (the overall completion time) while respecting task durations, precedence relationships, and resource constraints.

---

## 🧠 Features

- Defines a set of tasks, each with:
  - **Name**
  - **Duration**
  - **Resource requirement**
- Supports **precedence constraints** (e.g., Task A must finish before Task B starts).
- Uses **disjoint2/1** to ensure no overlapping tasks share the same resource.
- **Optimizes** the schedule by minimizing the makespan.
- Displays the final schedule with start and end times for each task.

---

## 📋 Example

### Task Definitions

```prolog
task(a, 3, 1).
task(b, 2, 1).
task(c, 4, 2).
task(d, 1, 2).
task(e, 2, 1).
```

### Precedence Rules

```prolog
precedes(a, b).
precedes(c, d).
```

This means:
- Task **b** can only start after **a** ends.
- Task **d** can only start after **c** ends.

---

## ⚙️ How It Works

1. **Collect all tasks** and initialize start and end times as variables.
2. Apply:
   - Duration constraints (`End = Start + Duration`)
   - Precedence constraints (`EndPrev <= StartNext`)
   - Resource constraints (`disjoint2/1`)
3. Compute the **makespan** (maximum end time).
4. Use `labeling([min(Makespan)], AllVars)` to find the optimal assignment.
5. Print the optimal schedule.

---

## Run the Program

Just run the program and it will automatically initialize

```prolog
?- [scheduler].
```

It will:
- Display a message (“Calculating optimal schedule...”)
- Compute and show the **optimal task start and end times**
- Print the **total makespan**

---

## 🖥️ Output Example

```
Calculating optimal schedule...

--- OPTIMAL SCHEDULE FOUND ---
  Task: a (Dur: 3, Res: 1) |   Start: 0   |   End: 3
  Task: b (Dur: 2, Res: 1) |   Start: 3   |   End: 5
  Task: c (Dur: 4, Res: 2) |   Start: 0   |   End: 4
  Task: d (Dur: 1, Res: 2) |   Start: 4   |   End: 5
  Task: e (Dur: 2, Res: 1) |   Start: 5   |   End: 7

TOTAL TIME (Makespan): 7
```

