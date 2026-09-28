---
title: Powell's derivative-free solvers with parquet_prima
---

`parquet_prima` minimises a smooth function of several variables without derivatives, using
M. J. D. Powell's model-based methods as reimplemented by
[PRIMA](https://github.com/libprima/prima). It reaches no reader, no writer and no setting: a
program with an objective in hand can `use parquet_prima` and minimise it, compiling a handful of
Fortran files and nothing of the Arrow stack. `use parquet` brings it in too. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

**Three engines, and the constraints decide which.**

| Your problem | Engine |
|---|---|
| no constraints, or bounds only | `pf_minimize_bobyqa` |
| linear constraints as well: `a*x <= b`, `a*x = b` | `pf_minimize_lincoa` |
| a constraint that is not linear in `x` | `pf_minimize_cobyla` |

## Which engine to reach for

BOBYQA and LINCOA are **local** and **model-based**. They build a quadratic that interpolates the
objective at a set of points, minimise that quadratic inside a trust region, evaluate the
objective at the result, and update the model with what they learn. On a smooth function that
buys a great deal: the model carries curvature that a direct-search method has to rediscover with
every step. COBYLA is local too, but its models — of the objective AND of every constraint — are
LINEAR, which is what lets it handle a constraint of any shape and also why it converges slowly.

- **A smooth objective in a handful of variables, with bounds** — `pf_minimize_bobyqa`. On smooth
  test problems in ten variables it costs a fraction of what the Nelder-Mead simplex costs — a
  thirtieth on a sphere, about half on Rosenbrock — and a hundredth to a thousandth of what
  differential evolution costs. The advantage grows with the dimension: in two variables the
  simplex is its equal. `MODE=evals bench/benchmark_optimize.sh` is what measures this. Reach for
  it first when each evaluation is expensive.
- **The same, with linear constraints** — `pf_minimize_lincoa`, which is the same quadratic model
  with an active-set trust-region step. It is in the same class and no dearer per evaluation.
- **A constraint that is not linear** — `pf_minimize_cobyla`, and only then. A linear model has no
  curvature, so it needs many more evaluations than the other two on the same objective; give it
  the work no other engine here can do.
- **A rugged objective with many minima** — not this page. [`pf_minimize_de`](optimization.html)
  searches a whole box and is not confined to one basin.
- **A few basins** — [`pf_minimize_multistart`](optimization.html) with a `pf_bobyqa_solver`,
  described below: BOBYQA from each of a spread of starts.
- **One variable** — [`pf_minimize_scalar`](optimization.html), Brent's method on a bracket.
- **A noisy or discontinuous objective** — none of these. A quadratic model fitted to noise is
  a model of the noise; the simplex or differential evolution degrade more gracefully.

**"Converged" means the engine's own stopping rule fired, not that the minimum is global.** It is
worth saying twice, because the counterexample is ordinary rather than contrived: from the usual
start, BOBYQA on the ten-variable Rosenbrock function stops at a second local minimum with
`f` near 4 and reports `PF_OPT_OK`. Nothing is wrong; the method found the bottom of the basin it
was in. If the objective may have more than one basin, that is what the multistart driver and
differential evolution are for.

## A first call

```fortran
use parquet_prima
use iso_fortran_env, only : real64

type(pf_optimize_info) :: info
real(real64) :: x(2), fmin, lower(2), upper(2)

x = [-1.2_real64, 1.0_real64]
lower = [-5.0_real64, -5.0_real64]
upper = [10.0_real64, 10.0_real64]
call pf_minimize_bobyqa(rosenbrock, x, fmin, lower=lower, upper=upper, &
                        rhobeg=0.5_real64, rhoend=1.0e-8_real64, info=info)
print '(a, 2f12.8, a, i0, a)', 'minimum at ', x, ' in ', info%neval, ' evaluations'
```

with the objective a module procedure:

```fortran
function rosenbrock(x) result(f)
    real(real64), intent(in) :: x(:)
    real(real64)             :: f
    f = (1.0_real64 - x(1))**2 + 100.0_real64*(x(2) - x(1)**2)**2
end function rosenbrock
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
call pf_minimize_bobyqa(f, x, fmin, [lower], [upper], [rhobeg], [rhoend], [npt], [scale], &
                        [ftarget], [max_neval], [converged], [info], [history], [context])
call pf_minimize_lincoa(f, x, fmin, [a_ineq], [b_ineq], [a_eq], [b_eq], [lower], [upper], &
                        [rhobeg], [rhoend], [npt], [scale], [ctol], [ftarget], [max_neval], &
                        [converged], [info], [history], [context])
call pf_minimize_cobyla(f, x, fmin, [a_ineq], [b_ineq], [a_eq], [b_eq], [lower], [upper], &
                        [rhobeg], [rhoend], [scale], [ctol], [ftarget], [max_neval], &
                        [converged], [info], [history], [context])
```

Every argument the three share means the same thing in each.

## Supplying the objective

Exactly as for [`parquet_optimize`](optimization.html), under the rules
[Solver conventions](solvers.html#the-function-you-supply) states once, and with the same types:
`f` is either a plain module procedure matching `pf_objective_func`, or an object extending
`pf_objective` with an `eval` binding, which is how an objective carries its own parameters — a
data table, a model, a counter. Both reach the same engine through one generic. `parquet_prima`
re-exports every shared name `parquet_optimize` declares — `pf_objective` and `pf_objective_eval`,
`pf_objective_func`, `pf_constrained_objective` with `pf_constraint_count` and
`pf_constraint_eval`, `pf_optimize_info`, `pf_optimize_history`, `pf_local_solver` with
`pf_local_run`, and the six `PF_OPT_*` codes — so a program that minimises and nothing else needs
only this one import.

**`pf_minimize_cobyla` is the exception: its objective is an object and only an object**, because a
plain function has nowhere to carry constraints. It extends `pf_constrained_objective` rather than
`pf_objective`, adding the two bindings described under
[Nonlinear constraints](#nonlinear-constraints-pf_minimize_cobyla) below.

**The objective must return a finite value at every point the engine asks about.** A NaN or an
infinity is a fatal error here, not a hint that the point is out of bounds:

```
ERROR STOP pf_minimize_bobyqa: the objective returned a non-finite value
```

PRIMA itself does something else — it replaces a NaN by a large finite value and carries on, its
"moderated extreme barrier". That is a reasonable choice for a library whose users expect a
result whatever happens, and the wrong one here: a moderated value is indistinguishable from a
real one in the answer, so a bug in the objective comes back as a minimum. Keep the objective
total over its bounds, or use [`pf_minimize_de`](optimization.html), whose population tier treats
a non-finite value as "outside my domain" and carries on.

## Bounds

`lower` and `upper` are optional and independent; absent means unbounded in that direction, and so
does a magnitude at or beyond a quarter of `huge()`, about `4.5e307`, which is what `+/-huge()`
means here. With `pf_minimize_bobyqa`, **the bounds are honoured throughout**, not just at the end:
no point that engine evaluates lies outside them, so an objective that is undefined outside its box
is never asked about the outside. The other two engines are the subject of the paragraph below.
Each pair must leave real room — `upper - lower` greater than `2*epsilon` — and the start point
must lie inside.

**The start point is used exactly as given.** BOBYQA needs the start to be at least `rhobeg` away
from every bound it is not already on; PRIMA satisfies that by default by MOVING the start and
printing a warning. This library cannot print, and a silently moved start is a poor trade, so it
does the other thing PRIMA offers: it reduces `rhobeg` to the room actually available, and reduces
`rhoend` in the same proportion. A run that started next to a bound therefore ends with a smaller
`info%rho` than the `rhoend` asked for — which is the only visible sign, and the reason `info%rho`
is worth reading.

**LINCOA and COBYLA treat a bound as one more linear constraint**, not as a box they stay inside,
and NEITHER keeps every evaluation inside it. LINCOA's trust-region iterates are feasible, but the
points it builds its first model from are the start displaced by `+/-rhobeg`, feasible or not, and
a geometry step need not be feasible either; COBYLA drives towards feasibility rather than starting
there. **An objective that is undefined outside its box therefore wants `pf_minimize_bobyqa`**,
which does keep every evaluation inside the bounds, or a reformulation that is defined everywhere.
What LINCOA guarantees is the ANSWER: it returns the best FEASIBLE point it evaluated, feasible to
`ctol`.

## Linear constraints: `pf_minimize_lincoa`

The constraints are ARRAYS, one ROW per constraint and one column per variable:

```fortran
real(real64) :: a_ineq(1, 2), b_ineq(1), a_eq(1, 2), b_eq(1)

a_ineq(1, :) = [1.0_real64, 1.0_real64]   ! x1 + x2 <= 1
b_ineq       = 1.0_real64
a_eq(1, :)   = [1.0_real64, -1.0_real64]  ! x1 - x2 = 0
b_eq         = 0.0_real64

x = 0.0_real64
call pf_minimize_lincoa(dist_from_1_2, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                        a_eq=a_eq, b_eq=b_eq, rhoend=1.0e-8_real64, info=info)
```

Each matrix goes with its right-hand side: give one without the other and the call is refused.
`lower` and `upper` may be given beside them and are folded in as two more inequalities each.
An all-zero row is refused too — it is either no constraint at all or an infeasible problem
written by accident, and neither is what anyone means to write.

**The start point must be feasible**, and a start that is not is refused. What LINCOA is worth
using for is the step: it moves inside the constraints rather than stepping out and projecting
back, so the iterates it accepts are feasible and the point it answers with is the best feasible
point it evaluated. It does evaluate infeasible points on the way — its initial model and its
geometry steps — so the objective must be defined wherever `rhobeg` can reach. PRIMA admits an
infeasible start by RELAXING the right-hand sides to include it, and warns; this library refuses
instead, because a relaxed `b` is a different problem and every violation reported afterwards would
be measured against it. `x = 0` satisfies any system with a non-negative `b_ineq` and a zero
`b_eq`, which is why the examples start there.

**A quadratic with both kinds of constraint active** shows what the engine is doing. Minimising
`|x - (1, 2, 3)|**2` subject to `x1 + x2 + x3 = 3` and `x1 >= 1`:

```fortran
a_eq(1, :)   = [1.0_real64, 1.0_real64, 1.0_real64]
b_eq         = 3.0_real64
a_ineq(1, :) = [-1.0_real64, 0.0_real64, 0.0_real64]   ! -x1 <= -1 is x1 >= 1
b_ineq       = -1.0_real64
x            = 1.0_real64                              ! feasible: sums to 3, x1 is on its bound
```

The answer is `(1, 0.5, 1.5)`: the equality pulls the free minimiser `(1, 2, 3)` onto the plane at
`(0, 1, 2)`, the inequality pushes the first coordinate back up to `1`, and the remaining two
share what is left.

## Nonlinear constraints: `pf_minimize_cobyla`

The nonlinear constraints come from the objective object, which extends `pf_constrained_objective`
and adds two bindings to `eval`:

```fortran
type, extends(pf_constrained_objective) :: disc_fit
contains
    procedure :: eval          => disc_eval          ! the objective
    procedure :: n_constraints => disc_count         ! how many values `constraints` fills
    procedure :: constraints   => disc_constraints   ! the values themselves
end type disc_fit
```

```fortran
subroutine disc_constraints(self, x, c)
    class(disc_fit), intent(inout) :: self
    real(real64), intent(in)  :: x(:)
    real(real64), intent(out) :: c(:)   ! exactly n_constraints() values
    c(1) = x(1)**2 + x(2)**2 - 1.0_real64    ! feasible inside the unit disc
end subroutine disc_constraints
```

**A point is feasible where every `c(i) <= 0`.** That is PRIMA's convention and Powell's.

**SciPy's COBYLA uses the opposite convention**, `c(x) >= 0` feasible. A program ported from
`scipy.optimize.minimize(method="COBYLA")` must negate each constraint function exactly once. This
is worth checking twice, because getting it wrong does not raise anything: the solver minimises
happily over the complement of the region you meant and returns a confident answer from the wrong
side of the boundary.

`n_constraints()` is read at the start of a call and never per evaluation — twice, in fact, once
to check it is not negative and once to size `c` — and `c` is allocated to it; **the binding must
fill exactly that many values.** Filling fewer leaves the rest undefined, and the engine will read
whatever is there as a violation or a satisfaction at random. The language cannot check this.

Linear constraints may be given as arrays beside the nonlinear ones, in the form
`pf_minimize_lincoa` takes, and bounds beside those. There is no `npt`: COBYLA's simplex has
`n + 1` vertices and no choice about it.

**An infeasible start is normal here.** COBYLA drives towards feasibility rather than requiring
it, which is the whole difference between its trust-region subproblem and LINCOA's.

## Feasibility, `ctol` and an infeasible answer

`ctol` is the violation at or below which a point counts as feasible; the default is
`sqrt(epsilon)`, about `1.5e-8`. It applies to `pf_minimize_lincoa` and `pf_minimize_cobyla`.

`info%cstrv` is the largest violation at the point returned — the largest of the bound violations,
the inequality violations, the absolute equality residuals and the nonlinear constraint values,
measured against **your** constraints in **your** units even when `scale=` had the engine working
in others. `info%status` is `PF_OPT_INFEASIBLE` exactly when `info%cstrv` exceeds `ctol`, so the
two never disagree.

**An infeasible problem is not an error.** Hand COBYLA the unit disc and `x1 >= 2` together and it
returns the least-violating point it found, with `PF_OPT_INFEASIBLE`, `converged` false, a
positive `info%cstrv` — and nothing printed:

```fortran
if (info%status == PF_OPT_INFEASIBLE) then
    print '(a, es9.2)', 'no feasible point; least violation found ', info%cstrv
end if
```

## The two radii

`rhobeg` and `rhoend` are the trust region's initial and final radii, and they are the two
arguments worth thinking about:

- **`rhobeg`** is how far the first steps reach. PRIMA's advice: about a tenth of the greatest
  change you expect in any variable. Too small and the model is fitted to a neighbourhood that
  tells it nothing; too large and the first points miss the structure entirely. Default `1`, with
  two exceptions: `pf_minimize_bobyqa` takes a quarter of the narrowest distance between the
  bounds where that is less than `1`, since its own rule forbids a `rhobeg` above half of it; and
  the two constrained engines take `10*rhoend` where you gave `rhoend` alone and that is more.
- **`rhoend`** is the accuracy you are asking for in `x`. The run stops when the radius has come
  down to it. Default `1e-6`, scaled down in proportion where the paragraph above left `rhobeg`
  below `1`: over a box of side `1`, `pf_minimize_bobyqa` defaults to `rhobeg = 0.25` and
  `rhoend = 2.5e-7`. `info%rho` is what the run actually ended at.

Both are in the units of the variable the engine works in — which is `x` itself unless you pass
`scale=`, and `y = x/scale` if you do. `rhoend` must be positive and no larger than `rhobeg`; a
reversed pair is refused rather than swapped.

`npt` is the number of interpolation points, from `n+2` to `(n+1)(n+2)/2`, default `2n+1`. The
default is the usual choice: a fuller model is more accurate per step and costs more evaluations
to maintain, and on most problems that trade does not pay. `pf_minimize_bobyqa` and
`pf_minimize_lincoa` take it; `pf_minimize_cobyla` does not.

## `scale=`: coordinates of different magnitudes

One trust-region radius governs every coordinate. A problem whose first variable is of order
`1e-3` and whose second is of order `1e3` therefore has no good single `rhobeg`, and that is what
`scale=` is for: give the characteristic magnitude of each coordinate and the engine minimises
`g(y) = f(scale*y)`, in which every coordinate is of order one.

```fortran
call pf_minimize_bobyqa(f, x, fmin, scale=[1.0e-3_real64, 1.0e3_real64], &
                        rhobeg=0.1_real64, rhoend=1.0e-8_real64, info=info)
```

Two recipes cover most cases:

1. **A boxed problem**: `scale = upper - lower`, with `rhobeg` about `0.1`. Every coordinate then
   spans one unit and the radii are fractions of the box.
2. **Nothing bounded**: `scale = abs(x0)` from a start that is of the right order in every
   coordinate.

Every element must be finite and positive. `rhobeg`, `rhoend` and `info%rho` are in units of `y`;
`x`, `history` and the objective's own argument are in yours, so an objective never sees the
engine's units. On a badly conditioned quadratic the difference is stark: with the same radii and
the same budget, the scaled run converges where the unscaled one is still two coordinate
magnitudes away.

**Constraints are transformed with it, and `info%cstrv` is not.** Give `scale=` to
`pf_minimize_lincoa` or `pf_minimize_cobyla` and the bounds and the COLUMNS of each constraint
matrix are rescaled to match, so the feasible region is the one you wrote. What comes back is
measured again in your own units, against your own constraints, so `info%cstrv` and the
`PF_OPT_INFEASIBLE` verdict taken from it mean what they would have meant without `scale=`.

## Scale the objective and the variables

`scale=` is half the story. All three engines decide that a trust-region step has failed by
comparing a predicted reduction — in the OBJECTIVE's units — against a threshold built from the
trust-region radius, which is in the VARIABLES' units. The comparison is against an absolute
constant, so it is a statement that both are of order one, and there is no warning when they are
not.

**What it looks like when they are not**: every step is declared a failure, the radius falls
straight to `rhoend`, and the run ends after a handful of evaluations with `PF_OPT_OK` — at
essentially the start point, and with `info%rho` at `rhoend`. It is a converged-looking answer to a
problem that was never searched. The cases that do it are objectives whose values are uniformly
tiny (of order `1e-20` and below) or enormous (of order `1e50` and above), and variables of the
same extremes, `scale=` included: a `scale` of `1e-8` for variables of order one is such a case.

**The two remedies, both one line at the call site:**

1. **The variables** — `scale=`, as above: give the characteristic magnitude of each coordinate.
2. **The values** — divide the objective by a typical one, `abs(f(x0))` say, inside your `eval`.
   The minimiser is unchanged by a positive constant factor; multiply `fmin` back afterwards if you
   want the value in your own units.

A chi-square in metres, or a likelihood of order `1e-12`, is the realistic case for the second, and
it is worth the line: an objective of order one converges where the same objective scaled by
`1e-20` stops at its start.

## BOBYQA under the multistart driver

`pf_bobyqa_solver` is a `pf_local_solver`, which is what
[`pf_minimize_multistart`](optimization.html) takes: a spread of starts over a box, BOBYQA from
each, the distinct minima counted.

```fortran
use parquet_prima
use parquet_optimize, only : pf_minimize_multistart

type(pf_bobyqa_solver) :: solver
type(pf_optimize_info) :: info
real(real64) :: x(2), fmin, lower(2), upper(2)

solver%rhoend = 1.0e-9_real64
lower = [-2.0_real64, -2.0_real64]
upper = [2.0_real64, 2.0_real64]
call pf_minimize_multistart(twin_wells, lower, upper, 20260916_int64, x, fmin, &
                            nstart=12, solver=solver, threads=4, info=info)
print '(a, i0, a)', 'found ', info%nminima, ' distinct minima'
```

Its four components carry the options:

| Component | Default | Meaning |
|---|---|---|
| `scale_from_box` | `.true.` | scale each coordinate by `upper - lower`, so the radii below are fractions of the box |
| `rhobeg_fraction` | `0.1` | the initial radius: in the scaled units when `scale_from_box`, else this fraction of the narrowest side; at most `0.5`, above which the call is refused |
| `rhoend` | `1e-6` | the final radius, in the same units |
| `max_neval` | `0` | evaluations per start; `0` means `500*n`, and a negative value is refused rather than read as the default |

The driver clones the objective once per thread, so a BOBYQA run under `threads > 1` sees its own
copy. The answer does not depend on the thread count: the starts come from the seed, the box and
`nstart` alone, and BOBYQA is deterministic, so the same call at one thread and at seven returns
the same bits. The engines hold no state between calls.

## What comes back

`info` is the same `pf_optimize_info` the rest of the library uses:

| Field | Meaning here |
|---|---|
| `status` | one of the `PF_OPT_*` codes below |
| `converged` | `status` is `PF_OPT_OK` or `PF_OPT_TARGET` |
| `neval` | objective evaluations |
| `niter` | trust-region iterations |
| `rho` | the trust-region radius the run ended at, in the engine's units |
| `cstrv` | constraint violation at the returned point, in your units; `0` from `pf_minimize_bobyqa`, which never violates its bounds |

The status codes, and what PRIMA reported to produce each:

| Code | Reached when |
|---|---|
| `PF_OPT_OK` | the trust-region radius came down to `rhoend` — the normal exit |
| `PF_OPT_TARGET` | a value at or below `ftarget` was found |
| `PF_OPT_LIMIT` | `max_neval` ran out, or the iteration cap did |
| `PF_OPT_ROUNDOFF` | rounding errors left the model unimprovable, or the trust-region subproblem failed |
| `PF_OPT_INFEASIBLE` | the run ended with `cstrv` above `ctol` (the two constrained engines only) |

**`max_neval` is a hard ceiling on all three engines: a run never overruns it.** It defaults to
`500*n`. Where the local engines of `parquet_optimize` may take one more step after the budget is
tested — a whole simplex step, a Brent iteration, a DE generation — PRIMA tests its count before
each evaluation, so `info%neval` is at most the `max_neval` you passed. Reaching it is
`PF_OPT_LIMIT`, not an error.

`history`, when present, holds every point evaluated and its value, in order, in your units.

## When a call is refused

Every one of these is an `error stop` carrying the entry point, the reason and your `context=`:

- fewer than one variable;
- a start that is not finite, or lies outside the bounds;
- a `lower` or `upper` whose size is not `size(x)`;
- a bound that is a NaN;
- a bound pair with no room between them;
- `rhoend` above `rhobeg`, or either not finite and positive;
- a `rhobeg` above half the narrowest distance between the bounds (`pf_minimize_bobyqa` only,
  which is where BOBYQA's model needs two distinct points per coordinate inside the box; a
  `pf_bobyqa_solver` whose `rhobeg_fraction` is above `0.5` meets the same refusal);
- an `npt` outside `[n+2, (n+1)(n+2)/2]`;
- a `scale` of the wrong length, or with an element that is not finite and positive;
- a `max_neval` below one or above `huge(1)/2`;
- an objective returning a non-finite value;
- an objective extending `pf_constrained_objective` handed to anything but `pf_minimize_cobyla`,
  which is the only engine here that reads its constraints;
- a constraint matrix without its right-hand side, or with the wrong number of rows or columns;
- an all-zero row in a constraint matrix;
- a start point that violates the linear constraints (`pf_minimize_lincoa` only);
- a negative or non-finite `ctol`;
- an `n_constraints()` below zero;
- a `constraints` binding returning a non-finite value.

**PRIMA adjusts where this refuses.** Upstream swaps a reversed pair of radii, clamps an `npt` out
of range, moves a start, reduces an oversized `rhobeg`, drops a zero constraint row, relaxes the
constraints to admit an infeasible start and treats a NaN bound as absent, warning each time. Each
of those is a good decision for a library that can warn. This tier prints nothing by design, so an
adjustment would be silent, and a silently adjusted argument is how a caller comes to believe they
asked for something they did not.

**The one adjustment kept, besides `honour_x0`**, is upstream's own reading of a bound at or beyond
about `4.5e307` in the engine's units: it means "no bound", and `+/-huge()` is the spelling a
caller reaches for when their own bound argument is not optional. That is a sentinel rather than a
number someone means literally, and recognising it before any arithmetic is also what keeps
`upper - lower` from overflowing.

## Attribution and licence

The engines here are derived from [PRIMA](https://github.com/libprima/prima) (Reference
Implementation for Powell's methods with Modernization and Amelioration, Zaikun Zhang), BSD-3-
Clause, at commit `43863c69`. The algorithms are M. J. D. Powell's:

- BOBYQA — *The BOBYQA algorithm for bound constrained optimization without derivatives*, DAMTP
  2009/NA06, University of Cambridge.
- LINCOA — no paper of its own; its model algebra is NEWUOA's (*The NEWUOA software for
  unconstrained optimization without derivatives*, 2006) and its active-set step follows *On fast
  trust region methods for quadratic models with linear constraints*, 2015.
- COBYLA — *A direct search optimization method that models the objective and constraint functions
  by linear interpolation*, 1994. SciPy 1.16 replaced its own Fortran 77 COBYLA with a translation
  of this same PRIMA code.

`src/parquet_prima_linalg.f90` carries the licence text and the nine numbered deviations shared by
the whole tier — fixed kinds in place of the preprocessor, the printing layer removed, the
development assertions removed, `pf_objective` in place of the procedure interface,
`ieee_arithmetic` in place of the hand-rolled predicates, a refusal in place of the moderated
extreme barrier, only what the three solvers reach vendored at all, a one-pass `trueloc`, this
repository's comment and line-length conventions, and an order-fixed `sum` in place of the
intrinsic, whose order is the processor's. Every other vendored file opens with a provenance block
naming the upstream files it came from, points at that list, and adds what changed beyond it.
`pf_minimize_bobyqa`, `pf_minimize_lincoa` and `pf_minimize_cobyla` have no such block: their
three files replace PRIMA's own drivers rather than vendoring them, and each says where its
defaults part from upstream's. The numerical content is upstream's, and a difference from upstream
is a defect here unless one of those lists says otherwise. One entry changes answers: LINCOA
releases a constraint from its active set once the trust-region centre has moved away from it,
where upstream keeps it and can stop short of the minimiser on that constraint while reporting
success (item 6 of `src/parquet_prima_lincob.f90`'s list).
