---
title: Solver conventions: callbacks, tolerances, budgets and outcomes
---

Four modules take a function you supply and work on it until a stopping rule fires:
[`parquet_integrate`](integration.html) integrates it, [`parquet_root`](root-finding.html) finds
where it crosses zero, and [`parquet_optimize`](optimization.html) and
[`parquet_prima`](prima.html) minimise it. Each of those pages covers its own engines, its own
examples and its own tables. This page states once what all four share, so that a caller who has
learned one has learned the shape of the others.

Signatures on these pages show optional arguments in square brackets, with the comma outside the
bracket. The optional outputs come in one order wherever they appear: `converged`, then `info`,
then the record (`history=`, or `points=` where it is a quadrature rule), then anything the engine
alone has — and `context` is always the last argument.

## The function you supply

### An object or a plain function

Every entry point is a generic over two forms, and the choice is about parameters rather than
taste. A **plain function** carries nothing but its argument. An **object** extends an abstract
type, carries its parameters as components, and implements one `eval` binding; the passed-object
dummy is `intent(inout)`, so `eval` may keep a counter or a cache, and an extension must declare
the same intent.

| Module | An object extends | Its binding's interface | The plain function's interface | `x` |
|---|---|---|---|---|
| [`parquet_integrate`](integration.html) | `pf_integrand` | `pf_integrand_eval(self, x) result(f)` | `pf_integrand_func(x) result(f)` | scalar |
| [`parquet_root`](root-finding.html) | `pf_rootfun` | `pf_rootfun_eval(self, x) result(f)` | `pf_rootfun_func(x) result(f)` | scalar |
| [`parquet_optimize`](optimization.html), [`parquet_prima`](prima.html) | `pf_objective`; for COBYLA, `pf_constrained_objective`, which adds `n_constraints(self)` and `constraints(self, x, c)` | `pf_objective_eval(self, x) result(f)` | `pf_objective_func(x) result(f)` | rank 1 |

The object form in full, for [`parquet_root`](root-finding.html); the other three differ only in
the type extended and the interface implemented. The counter is there to show why the passed
object is `intent(inout)`.

```fortran
module my_model
    use iso_fortran_env, only : real64
    use parquet_root, only : pf_rootfun
    implicit none

    type, extends(pf_rootfun) :: shifted_square
        real(real64) :: c = 2.0_real64 ! the constant to subtract
        integer      :: calls = 0      ! how many times eval ran
    contains
        procedure :: eval => shifted_square_eval ! f at one point
    end type shifted_square

contains

    ! f at one point, counting the call
    function shifted_square_eval(self, x) result(f)
        class(shifted_square), intent(inout) :: self ! the function object
        real(real64), intent(in)             :: x    ! where to evaluate
        real(real64)                         :: f    ! f(x)

        self%calls = self%calls + 1
        f = x*x - self%c
    end function shifted_square_eval

end module my_model
```

Using it is then a few lines, and `fn` carries its parameter in and its count out:

```fortran
type(shifted_square) :: fn
real(real64) :: x

fn%c = 2.0_real64
call pf_find_root(fn, 0.0_real64, 2.0_real64, x)
print '(a, f0.15)', "root     = ", x         ! root     = 1.414213562373095
print '(a, i0)',    "eval ran = ", fn%calls  ! eval ran = 10
```

### The names an extension repeats

**An extension must give its dummies the same names as the interface it overrides**, so those
names are part of the contract rather than a matter of style. The passed object is `self`, the
point is `x`, and the value an evaluation returns is `f`. A mismatch is a compile error, not a
silent difference: gfortran says "Dummy argument 'self' of 'eval' at (1) should be named 'this' as
to match the corresponding argument of the overridden procedure", and nagfor says "Argument 1 of
overriding type-bound procedure EVAL of type EXT is called SELF instead of THIS".

The same holds for `pf_local_run(self, f, x, fmin, lower, upper, info)`, the binding a caller
implements to give [`pf_minimize_multistart`](optimization.html) a local engine of its own. A type
carrying that binding is a **solver object**: it extends `pf_local_solver` and holds the engine's
options as components. Two come with the library — `pf_simplex_solver`
([`parquet_optimize`](optimization.html)) and `pf_bobyqa_solver` ([`parquet_prima`](prima.html)) —
and they are what the `max_neval = 0` rule below is about.

### A module procedure, never an internal one

**The callback must be a module procedure or a type-bound procedure.** An internal procedure
passed as an actual argument is a hard crash under one of this project's compilers and makes the
binary demand an executable stack under another. An object is the supported way to carry context;
a module variable is the other.

### One object per thread

`eval` may update the object, so two threads sharing one race.
[`parquet_integrate`](integration.html) and [`parquet_root`](root-finding.html) hold no state
between calls: give each thread its own object and concurrent solves are independent. The threaded
optimisers clone the objective for you, one copy per thread, so a `threads > 1` run of
[`pf_minimize_de`](optimization.html) or [`pf_minimize_multistart`](optimization.html) sees its own
copy and returns the same bits at every thread count.

## Tolerances: rtol and atol

A stopping tolerance is `rtol` (relative) or `atol` (absolute), both `real64`, and each engine
measures them on whatever its own stopping test looks at. Where both exist, `rtol` comes first in
the argument list and a run stops when EITHER is met.

| Entry point | `rtol` | `atol` | measured on |
|---|---|---|---|
| [`pf_integrate`](integration.html) | required | default 0 | the integral's error estimate, `abserr <= max(atol, rtol*abs(result))`; an `rtol` below `50*epsilon` needs a positive `atol` |
| [`pf_find_root`](root-finding.html) | default `4*epsilon`, which is also its floor | default 0 | the bracket around the root, `2*epsilon*abs(x) + max(atol, rtol*abs(x))/2` |
| [`pf_minimize_scalar`](optimization.html) | none | default 0 | the bracket around the minimiser, `atol/3 + sqrt(epsilon)*abs(x) + epsilon*(b - a)` |
| [`pf_minimize_simplex`](optimization.html) | required | default 0 | the spread of the values at the vertices |
| [`pf_minimize_de`](optimization.html) | default `1e-6` | default 0 | the spread of the population's values; `ftarget` can stop it first |
| [BOBYQA, LINCOA, COBYLA](prima.html) | none | none | `rhoend`, the final trust-region radius; `ctol` decides feasibility; `ftarget` can stop it first |

Where an engine takes both, at least one must be positive — except
[`pf_find_root`](root-finding.html), whose `rtol` is raised to its floor of `4*epsilon`, so there
is always a live test and `rtol = atol = 0` is accepted.

**A tolerance that is not a stopping test on the answer keeps a name of its own**: `ctol` for
feasibility, `rhobeg` and `rhoend` for the trust-region radii, `ftarget` for a value to stop at,
and `merge_tol` for the radius within which [`pf_minimize_multistart`](optimization.html) counts
two results as one minimum. Only the stopping tolerances are `rtol` and `atol`.

## The budget: max_neval

`max_neval` is the cap on evaluations of your function, an optional default `integer` on every
entry point that runs the engine itself. Reaching it is the module's `LIMIT` status and never an
abort; a value below 1 is refused everywhere.
[`pf_minimize_multistart`](optimization.html) is the exception and takes no `max_neval`: it runs
local solves rather than evaluating your function directly, and each start is capped by its own
solver.

| Entry point | default | Can the run overshoot it? |
|---|---|---|
| [`pf_integrate`](integration.html) | 100000 | never on a finite range, once `max_neval` covers the one rule application (21 evaluations) no range can avoid; by at most one rule application on a one-sided infinite range, and by three on `(-inf, +inf)`, which is split at zero and walked both ways |
| [`pf_find_root`](root-finding.html) | 200 | never |
| [`pf_minimize_scalar`](optimization.html) | 500 | never |
| [`pf_minimize_simplex`](optimization.html) | 5000 | by one simplex step, `n+2` evaluations |
| [`pf_minimize_de`](optimization.html) | `np*(max_gen + 1)` | by one generation |
| [BOBYQA, LINCOA, COBYLA](prima.html) | `500*n` | never |
| [`pf_minimize_multistart`](optimization.html) | each start runs to its local solver's budget | as that solver |

**In a solver OBJECT the same name means something slightly different**: `pf_simplex_solver` and
`pf_bobyqa_solver` both take `max_neval = 0` to mean the engine's own default, and both refuse a
negative value rather than reading it as that default. `0` passed as an ARGUMENT is still refused;
the zero rule belongs to the object.

## The outcome: converged, info and the status codes

Every entry point answers twice over. **`converged=`** is an optional `logical, intent(out)`: the
engine's own stopping rule fired, nothing more. **`info=`** is the long answer, a type named after
its module — `pf_integrate_info`, `pf_root_info`, `pf_optimize_info` — whose first two components
are always `status` and `converged`, and which always carries `neval`. `converged=` equals
`info%converged` on every path and is set whether or not `info` was asked for.

Nothing is printed on any of these paths, and running out of budget is not an error.

**`converged` is not a test for `status == OK`.** Reaching `ftarget` is a stopping rule like any
other, so [`pf_minimize_de`](optimization.html) and [BOBYQA, LINCOA, COBYLA](prima.html) report
`converged` with `status` set to `PF_OPT_TARGET`. Read `status` where the reason matters.

Each module keeps its own prefix and its own full table: `PF_INT_` on
[the integration page](integration.html#budget-and-outcome), `PF_ROOT_` on
[the root-finding page](root-finding.html#budget-and-outcome), and `PF_OPT_` on
[the optimisation page](optimization.html) and [the PRIMA page](prima.html). What is shared is the
spelling: `OK = 0` and `LIMIT = 1` in all three sets, one spelling for round-off
(`PF_INT_ROUNDOFF`, `PF_OPT_ROUNDOFF`) and one for a non-finite value from your function
(`PF_INT_NONFINITE`, `PF_OPT_NONFINITE`).

## The record: points and history

Ask for the search itself and you get it back, trimmed to `n`, costing memory only when you ask.
The argument is named for what the record holds: **`history=`** is an evaluation log, in order
(`pf_root_history`, `pf_optimize_history`, whose `%add(x, f)` appends one evaluation), and
**`points=`** is a quadrature rule rather than a log (`pf_integrate_points`, whose
`%append(other)` joins two records).

## What your function may return

The engines do not agree here, and the difference is deliberate: a comparison-driven local search
has nothing to do with a NaN, while a population tier can simply not select one.

| Entry point | A NaN | ... or an infinity |
|---|---|---|
| [`pf_integrate`](integration.html) | the integration ends: `PF_INT_NONFINITE`, with the point in `info%nonfinite_at` | the same |
| [`pf_find_root`](root-finding.html) | the process aborts | used as a sign |
| [`pf_minimize_scalar`, `pf_minimize_simplex`](optimization.html) | the process aborts | the process aborts |
| [`pf_minimize_de`](optimization.html) | a point outside the domain, counted in `info%nonfinite`; `PF_OPT_NONFINITE` when nothing is finite | the same |
| [`pf_minimize_multistart`](optimization.html) | its local solver's policy | the same |
| [BOBYQA, LINCOA, COBYLA](prima.html) | the process aborts, as it does for a non-finite constraint value | the same |

## What aborts

A caller mistake — a tolerance that is not a finite non-negative number, a budget below 1, a
bracket that is not a bracket — aborts rather than returning a status, because there is no answer
to return and no way for the caller to have meant it. The message names the procedure it came
from, type-qualified for a binding (`pf_simplex_solver%run: max_neval must not be negative`), and
every entry point takes an optional **`context=`**, whose text is appended to any abort message to
identify the call site. It is capped at 100 characters. One thread aborting ends the process.
