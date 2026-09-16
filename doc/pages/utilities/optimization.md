---
title: Optimisation: minimising a function of one or many variables
---

`parquet_optimize` minimises a function you supply — one variable on a bracket, many from a start
point, or globally over a box. It reaches no reader, no writer and no setting: a program that has
an objective in hand can `use parquet_optimize` and minimise it, compiling a handful of Fortran
files and nothing of the Arrow stack. `use parquet` brings it in too, so nothing here needs a
second import. See [Choosing a module](../operating/choosing-a-module.html) for what each entry
module costs.

Four engines, in two tiers. The **local** two start where you tell them and converge to the
nearest minimum: `pf_minimize_scalar` is Brent's method — golden-section steps with parabolic
interpolation — on a bracket you give it, and `pf_minimize_simplex` is the Nelder-Mead simplex,
ported from the `minimization` module of the `qfeet` library with its behaviour intact, so a
program moving across gets the same answers. The **global** two search a box you give them and are
not confined to one basin: `pf_minimize_de` is differential evolution over a seeded population,
and `pf_minimize_multistart` runs a local engine from a spread of starts and counts the distinct
minima it finds.

## Quick example

```fortran
use parquet_optimize
use iso_fortran_env, only : real64

type(pf_optimize_info) :: info
real(real64) :: x(2), fmin

x = [-1.0_real64, 2.5_real64]
call pf_minimize_simplex(rosenbrock, x, fmin, [0.5_real64, 0.5_real64], 1.0e-10_real64, info=info)
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
call pf_minimize_scalar(f, a, b, x, fmin, [tol], [max_neval], [info], [history], [context])
call pf_minimize_simplex(f, x, fmin, step, ftol, [atol], [max_neval], [info], [history], [context])
call pf_minimize_de(f, lower, upper, seed, x, fmin, [np], [f_weight], [cr], [ftol], [atol], &
                    [ftarget], [max_gen], [max_neval], [threads], [polish], [info], [history], &
                    [population], [context])
call pf_minimize_multistart(f, lower, upper, seed, x, fmin, [nstart], [solver], [xtol], &
                            [threads], [info], [history], [context])
```

## Supplying the objective

Either form reaches either engine, through one generic each.

A **plain function** takes the point and returns the value:

```fortran
function quartic(x) result(f)
    real(real64), intent(in) :: x(:)
    real(real64)             :: f
    f = (x(1) - 0.5_real64)**4 + x(1)**2
end function quartic
```

An **object** extends `pf_objective`, carries its parameters as components, and implements `eval`:

```fortran
type, extends(pf_objective) :: line_fit
    real(real64) :: xd(5) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
    real(real64) :: yd(5) = [2.1_real64, 3.9_real64, 6.2_real64, 7.8_real64, 10.1_real64]
    integer      :: ncall = 0
contains
    procedure :: eval => line_fit_eval
end type line_fit

function line_fit_eval(this, x) result(f)
    class(line_fit), intent(inout) :: this
    real(real64), intent(in)       :: x(:)
    real(real64)                   :: f
    f = sum((this%yd - (x(1)*this%xd + x(2)))**2)
    this%ncall = this%ncall + 1
end function line_fit_eval
```

Three things to know about either form:

- **The objective must live in a module**, plain function and objective type alike. An internal
  procedure of the main program cannot be passed as an actual argument under every compiler this
  library supports, and a type-bound procedure has to bind to a module procedure in any case.
- **`eval` may modify its own object, and you may read it afterwards.** The passed-object dummy is
  `intent(inout)`, so a counter or a cache works, and an extension must declare the same intent.
- **The objective is evaluated at an array**, even in `pf_minimize_scalar`, where that array has
  one element. One objective then serves both engines.

## One variable on a bracket

```fortran
call pf_minimize_scalar(f, a, b, x, fmin, [tol], [max_neval], [info], [history], [context])
```

`a` and `b` are the bracket, finite with `a < b`; `x` and `fmin` come back as the minimiser found
and its value. `tol` is an absolute tolerance on `x`, defaulting to `0`, which does not mean "no
tolerance": the effective tolerance is `tol/3 + sqrt(epsilon)*abs(x)`, Brent's own floor, so `0`
asks for as much accuracy as the arithmetic allows. `max_neval` defaults to 500.

The method needs only a bracket, not a single minimum inside it. Given several it converges to one
of them; given none — a minimiser outside `[a, b]` — it narrows towards the nearer end and reports
the best point it saw, which will be on that end.

## Many variables from a start point

```fortran
call pf_minimize_simplex(f, x, fmin, step, ftol, [atol], [max_neval], [info], [history], [context])
```

`x` is the start point going in and the best point found coming out; `step` gives the offset per
coordinate that builds the starting simplex, so vertex `i+1` is `x` with `step(i)` added to
coordinate `i`. No element of `step` may be zero — two vertices would sit on top of each other and
that coordinate could never move — and none may be NaN.

`ftol` is required. It is the fractional tolerance on the spread of the values at the simplex
vertices; `atol`, optional and zero by default, is the absolute one. At least one of the two must
be positive, or no convergence test could ever fire and the run could only end by exhausting its
budget. `max_neval` defaults to 5000.

## A whole box, with no start point

```fortran
call pf_minimize_de(f, lower, upper, seed, x, fmin, [np], [f_weight], [cr], [ftol], [atol], &
                    [ftarget], [max_gen], [max_neval], [threads], [polish], [info], [history], &
                    [population], [context])
```

Differential evolution keeps a population of points spread over the box and improves it a
generation at a time, so unlike the two local engines it is not confined to the basin it started
in. There is no start point to get wrong: `lower` and `upper` are the box, finite with
`lower < upper` in every coordinate, and `seed` is what makes one run repeatable.

The initial population is a **Latin hypercube** — the box divided into `np` strata per coordinate,
one point per stratum, the strata permuted independently per coordinate — so every coordinate is
covered evenly however small `np` is. Each generation then builds one trial point per individual
from three others (`a + F*(b - c)`, clipped back into the box), crosses it with the incumbent at
probability `cr`, and keeps whichever is better.

`np` defaults to `max(20, 10n)` and may not be below 4: the mutation needs three donors distinct
from the individual being improved. `f_weight` is `F`, default `0.8`, in `(0, 2]`; `cr` is the
crossover probability, default `0.9`, in `[0, 1]`. A low `cr` makes each trial differ from its
parent in one or two coordinates and is what gets DE through a very rugged objective — on
Rastrigin in five variables it is worth roughly a factor of three in evaluations.

The run stops on the first of: `ftarget` reached (`PF_OPT_TARGET`), the population's value spread
inside `ftol`/`atol` (`PF_OPT_OK`), `max_gen` generations, or `max_neval` evaluations (both
`PF_OPT_LIMIT`). `max_gen` defaults to 1000 and `max_neval` to `np*(max_gen + 1)`, so by default
the generation budget is the binding one.

`population` hands back the final population, one individual per column, and `history` the best
individual of each generation — one record per generation, not one per evaluation.

`polish` (off by default) finishes by running `pf_minimize_simplex` from the best individual, with
a step of a thousandth of each coordinate's width and the same tolerances, and counts its
evaluations in `info%neval`. It is worth reaching for when DE has found the right basin and you
want the last few digits, which DE itself spends many generations on. **It inherits the simplex's
non-finite policy**: if the objective is NaN anywhere the polish step can reach, the process
aborts, where DE alone would have stepped around it.

### A NaN is "outside my domain", not an error

Where the local engines abort on a non-finite value, `pf_minimize_de` treats one as a point it may
not go to. A trial whose value is NaN or infinite never wins its selection, and an individual that
starts non-finite is carried as `+Infinity` until something better displaces it; `info%nonfinite`
counts every non-finite value the run saw. So an objective that is only defined on part of the box
— a likelihood with a positivity constraint, say — can be minimised by handing back a NaN outside
its domain, with no guard of your own.

The exception is a box where **nothing** is finite: that is `PF_OPT_NONFINITE`, `x` comes back as
the box's centre and `fmin` as `+Infinity` (never a NaN, so a caller's own comparison is safe), and
`converged` is false.

## Many starts, one local engine

```fortran
call pf_minimize_multistart(f, lower, upper, seed, x, fmin, [nstart], [solver], [xtol], &
                            [threads], [info], [history], [context])
```

A local engine converges to whichever basin it starts in and reports success for doing so. Run it
from enough points spread over the box and every basin wide enough to catch a start gets found —
which is the cheap global method for an objective with a handful of basins, and no substitute for
`pf_minimize_de` on one with thousands.

`nstart` starts, default `max(10, 2n)`, are laid out on the same Latin hypercube construction DE's
initial population uses. **They depend on `seed`, `nstart` and the box, and on nothing else** — not
on the solver, not on `threads` — so two runs differing only in their local engine explore the same
basins and their results are comparable.

`solver` is the local engine and its options, as an object: `pf_simplex_solver` ships here, and
`pf_bobyqa_solver` will come from `parquet_prima`. Set a component to configure it; the default is
a fresh `pf_simplex_solver()`, whose own defaults (`ftol = 0`, `atol = 1e-10`, `max_neval = 5000`,
`step_fraction = 0.1`) are a valid pair as they stand.

```fortran
type(pf_simplex_solver) :: solver
solver%atol = 1.0e-12_real64
solver%step_fraction = 0.2_real64
call pf_minimize_multistart(camel, lower, upper, 42_int64, x, fmin, nstart=20, solver=solver, &
                            info=info, history=starts)
```

**The box bounds the starts, not the runs.** `pf_simplex_solver` is unbounded and may walk out of
the box on its way to a minimum; a bounded solver would not. Which one you have decides whether a
result outside the box is possible.

`xtol` decides what counts as one minimum: two results within `xtol*(upper - lower)` of each other
in **every** coordinate are merged, walking the starts in index order. It defaults to `1e-6`, which
is tighter than a local engine's own accuracy, so the default usually counts each start
separately; a value near the scale you care about (`1e-2`, say) is what makes `info%nminima` mean
"distinct basins".

`history` is the map: one record per start, its own minimum and value, before any merging. A row
whose value is not one of the distinct minima is a start whose run hit its budget, and
`info%nlimit` counts those. `info%niter` is the number of starts and `info%neval` the total over
all of them.

## Threads, and what each thread sees

`threads` is the team the population's evaluations (`pf_minimize_de`) or the local runs
(`pf_minimize_multistart`) are spread over. It defaults to 1, and it is an argument, never a
setting — nothing in this module reads a process-global knob.

**At `threads = 1` the caller's own objective object is what gets evaluated**, so a counter or a
cache it keeps is what you read back afterwards, exactly as with `pf_minimize_simplex`.

**Above 1 each thread gets its own clone**, made by sourced allocation before the region: the
dynamic type and every component come with it, an `allocatable` component is deep-copied per
thread, a `pointer` component is shared (which is fine for data nobody writes), and a component
with a finalizer is finalized once per clone. The clones are discarded when the call returns, so
**whatever the objective accumulates is not visible to the caller under `threads > 1`** —
`info%neval` is the count. An objective must therefore be safe to clone this way; one that is not
should be run at `threads = 1`.

The count is clamped to what this process's CPU affinity allows, which can emit one notice per
process naming `optimisation`; `parquet_set_verbosity("silent")` quiets it. A call from inside your
own parallel region is honoured: with nesting off the team collapses to one thread, and the answer
does not change.

## What is reproducible, and what is not

**The answer does not depend on `threads`, on the schedule, or on the machine's timing.** For a
given library version and build, `pf_minimize_de` and `pf_minimize_multistart` are functions of
`seed`, their arguments and the objective. Two rules hold that: every draw is addressed by
`(seed, generation, individual)` or `(seed, start)` and nothing is drawn outside that scope; and
every decision that combines individuals or starts is element-wise or taken by lowest index. The
same seed repeats a run exactly; a different seed gives a different one.

**What is not promised is bit equality across compilers or library versions.** The draws
themselves are frozen, but `a + F*(b - c)` is a shape a compiler may contract into a fused
multiply-add, one bit in a trial vector can flip one selection, and the paths part from there. A
later version of this library may also change the draw order, which would be a `### Changed`
entry in the CHANGELOG. So: record the seed and the version together, and do not compare two runs
across a toolchain change bit for bit.

## Convergence and tolerances

`pf_minimize_simplex` stops when the values at its vertices agree, and `pf_minimize_de` when the
values across its population do, by the same pair of tests:

- the **fractional** test, `2*abs(fhi - flo)/(abs(fhi) + abs(flo)) < ftol`, which compares the
  spread with the size of the values themselves;
- the **absolute** test, `abs(fhi - flo) < atol`, which compares it with a number you choose.

Which one to reach for follows from the objective. A fractional tolerance is scale-free and is
what you want when the minimum value is some distance from zero. **It cannot converge on an
objective whose minimum value is zero**: a fractional spread over values that are all nearly zero
stays around 1 however tight the simplex gets. A least-squares fit that can reach a perfect fit is
exactly that case, and `ftol = 0.0` with a positive `atol` is the pair for it.

`info%spread` reports the spread the run ended on, so you can see which test fired and by how much.

`pf_minimize_de` defaults to `ftol = 1e-6`, which is why an objective whose minimum is zero usually
wants `ftarget=` or a positive `atol` there too. `pf_minimize_simplex` has no `ftol` default at
all: it is a required argument, so the "at least one of the two must be positive" refusal cannot be
reached by leaving an argument out.

## The evaluation budget and what `info` reports

`max_neval` is a **soft** bound: it is tested once per engine step, so a run may overshoot it by
one step — the simplex's `n+2`, one more Brent iteration, or one whole DE generation. It cannot be
honoured below `n+1` in the simplex, or below `np` in DE, since the starting simplex or population
has to be evaluated before the budget can be tested for the first time. Running out of budget is
not an error: `info%status` is `PF_OPT_LIMIT`, `info%converged` is false, `x` and `fmin` hold the
best point seen, and nothing is printed. `pf_minimize_multistart` has no `max_neval` of its own —
the budget belongs to the solver object, and `info%nlimit` counts the runs that spent it.

`info` is a `pf_optimize_info`. Every engine fills `status` (one of the `PF_OPT_*` codes),
`converged`, `neval` and `niter`; `niter` is simplex iterations, Brent iterations, DE generations
or the number of starts, depending on which engine filled it. `spread` is the final value spread
over the simplex's vertices, DE's population, or the starts' values. `nonfinite` counts the
non-finite values DE and the multistart driver saw; `nminima` and `nlimit` belong to the
multistart driver alone. `rho` and `cstrv` belong to the trust-region engines and stay zero here.

`history` is a `pf_optimize_history` — `x(:, k)` the point and `f(k)` its value, for `k` in `1:n`.
It comes back trimmed to the records in use, so `size()` answers the same as `%n`, and it costs
memory only when you ask for it. What a record holds depends on the engine: **every evaluation in
order** for the two local engines, **the best individual of each generation** for `pf_minimize_de`
(so one record per generation, plus one for the starting population), and **one record per start**
for `pf_minimize_multistart`.

**A local engine's record is every evaluation, not the simplex.** A trial point the simplex
evaluated and then rejected is in there beside the ones it kept, and the last entry is often
exactly that — so the final simplex is not the record's tail. What the record does give you is the
whole search path, the best point (`minval(f(1:n))`, which equals `fmin`), and every vertex the run
ever held.

## Which engine to reach for

**One variable and a bracket**: Brent. It needs no step and no start point, and on a smooth
function it is the cheapest thing here by a wide margin.

**A smooth objective in a few variables, and a good start point**: the simplex. It handles a kink
at the minimum, which a model-based solver would not, and it needs only values.

**Several basins over a box, or a start point you cannot guess**: the multistart driver. A local
engine converges to whichever basin it starts in and reports success for doing so, so the answer
is to start in all of them; twenty starts over a two-variable box will usually find every basin
wide enough to catch one, at a few thousand evaluations in total.

**A rugged objective — many minima, or no useful start point at all**: `pf_minimize_de`. It costs
an order of magnitude more evaluations than the multistart driver on a function both can solve, and
it is the only one of the four that solves a function with thousands of local minima. On Rastrigin
in five variables, thirty local runs from random starts found the global minimum none of the time;
DE found it in tens of thousands of evaluations.

**A smooth objective where the last few digits matter**: DE with `polish=.true.`, which hands the
best individual to the simplex, or DE followed by your own local run from the point it returned.

## Important behaviour

- **`x` holds the best point found on return**, and `fmin` its value.
- **`neval` counts the starting simplex or population**, so it is never smaller than `n+1` for the
  simplex or `np` for DE.
- **Convergence is a statement about function VALUES, not about parameters.** Consider `(x - 1)**2`
  in one variable with a simplex whose two vertices end up at `0` and `2`. Both have value `1`,
  the spread is exactly zero, every convergence test fires, and neither vertex is the minimiser.
  `info%converged` means the engine's own stopping rule fired — nothing more.
- **A local minimum is all a local engine promises**, and on a rugged objective not even that:
  Nelder-Mead can stall on a non-minimum. No engine here restarts itself, so a suspicious result is
  worth re-running from the point it returned.
- **The global engines promise less than their name suggests too.** `pf_minimize_de` is a
  stochastic search: with enough generations it finds the global minimum of the functions it is
  meant for, and there is no budget at which it is guaranteed to. `pf_minimize_multistart` finds
  exactly the basins its starts landed in. Neither reports "this is the global minimum", because
  neither can know it.
- **A non-finite objective value aborts a LOCAL engine**, and the process with it. A NaN or an
  infinity from `eval` is not carried into a comparison, because every decision Brent and the
  simplex make is a comparison of values and the answer would otherwise be silently wrong. The
  population engines treat one as a point outside the domain instead — see the DE section above,
  and note that `pf_minimize_multistart` inherits its solver's policy, not DE's.
- **Caller mistakes abort too** (`error stop`, silently — nothing is logged first): a bracket that
  is not `a < b` with finite ends, fewer than one variable, a `step` of the wrong size or
  containing zero or NaN, a NaN in the start point, a negative or non-finite tolerance, both
  tolerances zero, a budget that is not positive or exceeds `huge(1)/2`, a box whose sizes disagree
  or whose bounds are not finite and ordered, `np` below 4, `f_weight` outside `(0, 2]`, `cr`
  outside `[0, 1]`, a `max_gen` or `nstart` below 1, a negative `xtol`, and a `threads` below 1.
  Failure to converge is **not** one of these — it is reported through `info`. `context=` adds your
  own text to any such message, capped at 100 characters.
- **Nothing here is process-global.** No entry point reads a setting, and no call holds state
  between calls, so separate minimisations may run concurrently — each at `threads = 1` when they
  are already inside your own parallel region.

## Moving from qfeet's `minimize`

The simplex is a port, and the mapping is mechanical:

| qfeet | parquet-fortran |
|---|---|
| `use minimization` | `use parquet_optimize` |
| `objective_type`, `eval(f, x)` | `pf_objective`, `eval(this, x)` with `intent(inout)` on the object |
| `minimize_func` | `pf_objective_func` |
| `call minimize(func, x, fmin, step, ftol, nfeval, atol=, max_nfeval=, converged=)` | `call pf_minimize_simplex(func, x, fmin, step, ftol, atol=, max_neval=, info=)`; then `info%neval` and `info%converged` |
| `call minimize(func, points, fvalues, ...)` (full simplex) | not carried; use the start-and-step form, with `history=` when you want the search path |
| a logged warning on running out of budget | `info%status == PF_OPT_LIMIT`, silent |
| `mlog_init` before use | nothing |

`ftol` keeps its position, so only the trailing arguments move. The algorithm is unchanged step for
step, so a migrated call reaches the same minimum by the same path; what changed is that a
non-finite value from the objective now aborts mid-run rather than only in the starting simplex.
