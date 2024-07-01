# icfpc2024

Team "A Storm of Minds"
[ICFP Contest 2024](https://icfpcontest2024.github.io/) entry.

State of the code at the end of the lightning round ist in branch `lightning-round`.
State of the code at the end of the contest is in branch `main-round`.

## Team

We participated as a team of three, working remotely.
Each of us spent only about half of the time (i.e. 36 hours) on the contest,
using the rest for sleep, family, or sports.

## Languages used

* Python (Scripts, spaceship solver, 3D experimentation)
* Haskell (prototyping ICFP evaluator)
* Common Lisp (ICFP evaluator, lambdaman, spaceship, and efficiency solvers and tools)
* C# (Lambdaman solver)
* ICFP
* 3D

## Strategy

### ICFP Language

* Prototype evaluator written in Haskell.
* Lisp implementation with more features.
* No attempts at good efficiency or lazy/strict application,
  as these seemed not necessary for solving the problems.

### LambdaMan

* Follow shortest path to next pill (Dijkstra).
  Two independent implementations in Lisp and C#.
* Trivial compression of repeated moves
* Some handwritten solutions

### Spaceship

* Trivial solver visiting nearest to last destination, accelerating mostly.
* Small variations on solver for limited gains.
* No solutions for 23-25, which would have required optimization.
* Some handwritten solutions.

### 3D

* Only handwritten solutions.
* Used Python to experient and for an evaluator.

### Efficiency

* Solved mostly by mentally decompiling the ICFP code and
  trying to determine what it computed.
* Then either compute solution manually or with computer help.
* All tooling in Common Lisp.
* Some brute-force solving.
* 7 and 8 solved by translating to clauses and using SAT solver (minisat).
* 9-11 recoded in Lisp.
* Solved 12 by looking up the sequence in the [OEIS](https://oeis.org).

## Retrospective

A very "classic" contest we enjoyed tremendously, despite
not meeting in person and not having the full time for the contest.

Having a grasp of the problem after the first 24 hours and then having 48
hours to find good solutions is something we like.
As usual, we did well in Lightning and worse in the main round. That's likely because we
often fall into the trap of trying small improvents to existing bad approaches
instead of taking the time to come up with a good approach.
This can be see in the wonderful [journey images](https://icfpcontest2024.github.io/journey.html)
the organizers made available right after the contest.
[Here's ours](https://icfpcontest2024.github.io/img/journey/36.png).
As you can see, it's descending all the time. With some teams, it goes up
instead, which is what it should look like.

