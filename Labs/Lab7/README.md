# Prolog Lab 07 - Map Coloring

## Overview
This Lab is about to solve the **Map Coloring Problem** using **Constraint Logic Programming over Finite Domains (CLP(FD))** in **SWI-Prolog**.  
The goal is to assign colors to regions of a map so that no two adjacent regions share the same color.

It includes two datasets:
- **Australia** (guided example)
- **South America** (extension exercise)

---

## How It Works
Each region is represented as a variable whose domain is the set of available colors (1..K).  
Constraints enforce that adjacent regions must have different colors.  
The `labeling/2` predicate searches for a valid assignment of colors.

### Key Predicates
- `regions_au/1`, `regions_sa/1` — lists of regions.
- `edges_au/1`, `edges_sa/1` — adjacency pairs.
- `map_color/4` — defines constraints for coloring.
- `colorize_au/2`, `colorize_sa/2` — runs the model.
- `pretty_color_by_region/2` — prints readable results.
- `test_*` predicates — quick demo commands.

---

## Example Queries and Outputs

### 🦘 Australia (3 colors)
```prolog
?- test_au_3.
wa = red
nt = green
sa = blue
q = red
nsw = green
v = blue
t = red
true.
```

### 🦘 Australia (4 colors)
```prolog
?- test_au_4.
wa = red
nt = green
sa = blue
q = yellow
nsw = red
v = green
t = blue
true.
```

### 🌎 South America (3 colors)
```prolog
?- test_sa_3.
ar = red
bo = green
br = blue
cl = red
co = green
ec = red
gy = blue
gfr = green
py = yellow
pe = red
su = blue
uy = green
ve = red
true.
```

### 🌎 South America (4 colors, min strategy)
```prolog
?- test_sa_min.
ar = red
bo = green
br = blue
cl = yellow
co = red
ec = green
gy = blue
gfr = yellow
py = red
pe = green
su = blue
uy = yellow
ve = red
true.
```
