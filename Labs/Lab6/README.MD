# Prolog Task Scheduler with CLP(FD)

This project is a **constraint-based task scheduler** written in **Prolog**, using the **Constraint Logic Programming over Finite Domains (CLP(FD))** library.  
It computes an **optimal schedule** that minimizes the total project time (**makespan**) while ensuring that:

- Each task has a defined duration and resource.
- No two tasks using the same resource overlap.
- Some tasks must start only after others finish.

---

## 🧠 Features

- **Automatic scheduling** of tasks with resource and precedence constraints.
- **Makespan optimization** (minimizes total project time).
- **Non-overlapping constraint** for tasks sharing the same resource.
- Detailed output showing each task’s start and end times.
- Includes a **lab example** for demonstration purposes.

---

## 📋 Task Model

Each task is defined as:

```prolog
task(Name, Duration, Resource).
```

Example:

```prolog
task(a, 3, 1).
task(b, 2, 1).
task(c, 4, 2).
task(d, 1, 2).
task(e, 2, 1).
```

This means:
- Task **a** lasts 3 units of time and uses resource **1**.
- Task **c** lasts 4 units of time and uses resource **2**, etc.

---

## ⚙️ Precedence Rules

Defined as:

```prolog
precedes(Previous, Next).
```

Example:

```prolog
precedes(a, b).
precedes(c, d).
```

This means:
- Task **b** starts only after **a** finishes.
- Task **d** starts only after **c** finishes.

---

## 🚀 How It Works

1. **Collects all tasks** into lists of start and end variables.
2. Applies:
   - **Duration constraints** (`End = Start + Duration`)
   - **Precedence constraints** (`EndPrev <= StartNext`)
   - **Resource constraints** (no overlapping tasks using the same resource)
3. Computes the **makespan** (maximum end time).
4. Runs the **labeling process** to find the schedule minimizing the makespan.
5. Prints the optimal schedule.

---

## 🧩 Run the program

The program will run automatically at the moment of execute it:

```prolog
?- [scheduler].
```

This will:
- Compute all valid start and end times.
- Minimize the makespan.
- Print a detailed report.

---

## 📊 Output Example

```
Calculating Schedule
==============================================

=== OPTIMAL SCHEDULE ===

Task a:
  Duration: 3
  Resource: 1
  Start: 0
  End: 3
  ---
Task b:
  Duration: 2
  Resource: 1
  Start: 3
  End: 5
  ---
Task c:
  Duration: 4
  Resource: 2
  Start: 0
  End: 4
  ---
Task d:
  Duration: 1
  Resource: 2
  Start: 4
  End: 5
  ---
Task e:
  Duration: 2
  Resource: 1
  Start: 5
  End: 7
  ---

Total project time (makespan): 7 time units
```

---

## 🧪 Lab Example

You can also run a simpler demonstration:

```prolog
?- lab_example.
```

This uses three tasks with durations and resource conflicts, showing how the system enforces non-overlapping schedules.

