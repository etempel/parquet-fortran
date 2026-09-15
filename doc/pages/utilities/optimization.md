---
title: Optimisation: minimising a function of one or many variables
---

`parquet_optimize` minimises a function you supply — one variable on a bracket, or many from a
start point. It reaches no reader, no writer and no setting: a program that has an objective in
hand can `use parquet_optimize` and minimise it, compiling a handful of Fortran files and nothing
of the Arrow stack. `use parquet` brings it in too, so nothing here needs a second import. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

Two engines ship today. `pf_minimize_scalar` is Brent's method — golden-section steps with
parabolic interpolation — on a bracket you give it. `pf_minimize_simplex` is the Nelder-Mead
simplex, ported from the `minimization` module of the `qfeet` library with its behaviour intact,
so a program moving across gets the same answers.

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

## Convergence and tolerances

The simplex stops when the values at its vertices agree, by either test:

- the **fractional** test, `2*abs(fhi - flo)/(abs(fhi) + abs(flo)) < ftol`, which compares the
  spread with the size of the values themselves;
- the **absolute** test, `abs(fhi - flo) < atol`, which compares it with a number you choose.

Which one to reach for follows from the objective. A fractional tolerance is scale-free and is
what you want when the minimum value is some distance from zero. **It cannot converge on an
objective whose minimum value is zero**: a fractional spread over values that are all nearly zero
stays around 1 however tight the simplex gets. A least-squares fit that can reach a perfect fit is
exactly that case, and `ftol = 0.0` with a positive `atol` is the pair for it.

`info%spread` reports the spread the run ended on, so you can see which test fired and by how much.

## The evaluation budget and what `info` reports

`max_neval` is a **soft** bound: it is tested once per engine step, so a run may overshoot it by
one step — the simplex's `n+2`, or one more Brent iteration. It cannot be honoured below `n+1` at
all, since the starting simplex has to be evaluated before the budget can be tested for the first
time. Running out of budget is not an error: `info%status` is `PF_OPT_LIMIT`, `info%converged` is
false, `x` and `fmin` hold the best point seen, and nothing is printed.

`info` is a `pf_optimize_info`, whose components for these two engines are `status` (one of the
`PF_OPT_*` codes), `converged`, `neval`, `niter` and `spread`. The remaining components belong to
the population and trust-region engines and stay zero here.

`history` is a `pf_optimize_history` and records every evaluation in order — `x(:, k)` the point
and `f(k)` its value, for `k` in `1:n`. It comes back trimmed to the records in use, so `size()`
answers the same as `%n`, and it costs memory only when you ask for it.

**The record is every evaluation, not the simplex.** A trial point the simplex evaluated and then
rejected is in there beside the ones it kept, and the last entry is often exactly that — so the
final simplex is not the record's tail. What the record does give you is the whole search path,
the best point (`minval(f(1:n))`, which equals `fmin`), and every vertex the run ever held.

## Which engine to reach for

A smooth objective in a few variables, and a good start point: the simplex. One variable and a
bracket: Brent, which needs no step and no start point. An objective with several basins over a
box, or one whose start point you cannot guess: neither of these — a local engine converges to
whichever basin it starts in, and reports success for doing so. The global engines that answer that
problem are the next stage of this module.

## Important behaviour

- **`x` holds the best point found on return**, and `fmin` its value.
- **`neval` counts the starting simplex**, so it is never smaller than `n+1`.
- **Convergence is a statement about function VALUES, not about parameters.** Consider `(x - 1)**2`
  in one variable with a simplex whose two vertices end up at `0` and `2`. Both have value `1`,
  the spread is exactly zero, every convergence test fires, and neither vertex is the minimiser.
  `info%converged` means the engine's own stopping rule fired — nothing more.
- **A local minimum is all that is promised**, and on a rugged objective not even that: Nelder-Mead
  can stall on a non-minimum. Neither engine restarts itself, so a suspicious result is worth
  re-running from the point it returned.
- **A non-finite objective value aborts.** A NaN or an infinity from `eval` ends the process with a
  message rather than being carried into a comparison, because every decision both engines make is
  a comparison of values and the answer would otherwise be silently wrong.
- **Caller mistakes abort too** (`error stop`, silently — nothing is logged first): a bracket that
  is not `a < b` with finite ends, fewer than one variable, a `step` of the wrong size or
  containing zero or NaN, a NaN in the start point, a negative or non-finite tolerance, both
  tolerances zero, and a budget that is not positive or exceeds `huge(1)/2`. Failure to converge is
  **not** one of these — it is reported through `info`. `context=` adds your own text to any such
  message, capped at 100 characters.
- **Nothing here is process-global.** No entry point reads a setting, and no call holds state
  between calls, so separate minimisations may run concurrently.

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
