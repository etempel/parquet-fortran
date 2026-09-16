---
title: Powell's derivative-free solvers with parquet_prima
---

`parquet_prima` minimises a smooth function of several variables without derivatives, using
M. J. D. Powell's model-based methods as reimplemented by
[PRIMA](https://github.com/libprima/prima). It reaches no reader, no writer and no setting: a
program with an objective in hand can `use parquet_prima` and minimise it, compiling a handful of
Fortran files and nothing of the Arrow stack. `use parquet` brings it in too. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

**One engine so far: `pf_minimize_bobyqa`**, for a problem with bounds. `pf_minimize_lincoa`
(linear constraints) and `pf_minimize_cobyla` (nonlinear constraints) are being added; until they
are, this page describes what is here.

## Which engine to reach for

The methods here are **local** and **model-based**. They build a quadratic that interpolates the
objective at a set of points, minimise that quadratic inside a trust region, evaluate the
objective at the result, and update the model with what they learn. On a smooth function that
buys a great deal: the model carries curvature that a direct-search method has to rediscover with
every step.

- **A smooth objective in a handful of variables, with bounds** — `pf_minimize_bobyqa`. On smooth
  test problems it costs of the order of a tenth to a twentieth of what the Nelder-Mead simplex
  costs and a hundredth to a thousandth of what differential evolution costs, and the gap widens
  with the number of variables. Reach for it first when each evaluation is expensive.
- **A rugged objective with many minima** — not this page.
  [`pf_minimize_de`](optimization.html) searches a whole box and is not confined to one basin.
- **A few basins** — [`pf_minimize_multistart`](optimization.html) with a `pf_bobyqa_solver`,
  described below: BOBYQA from each of a spread of starts.
- **One variable** — [`pf_minimize_scalar`](optimization.html), Brent's method on a bracket.
- **A noisy or discontinuous objective** — neither of these. A quadratic model fitted to noise is
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
                        [ftarget], [max_neval], [info], [history], [context])
```

## Supplying the objective

Exactly as for [`parquet_optimize`](optimization.html), and with the same types: `f` is either a
plain module procedure matching `pf_objective_func`, or an object extending `pf_objective` with an
`eval` binding, which is how an objective carries its own parameters — a data table, a model, a
counter. Both reach the same engine through one generic. `parquet_prima` re-exports `pf_objective`,
`pf_optimize_info` and `pf_optimize_history`, so a program that minimises and nothing else needs
only this one import.

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

`lower` and `upper` are optional and independent; absent means unbounded in that direction. When
given, **the bounds are honoured throughout**, not just at the end: no point the engine evaluates
lies outside them, so an objective that is undefined outside its box is never asked about the
outside. Each pair must leave real room — `upper - lower` greater than `2*epsilon` — and the start
point must lie inside.

**The start point is used exactly as given.** BOBYQA needs the start to be at least `rhobeg` away
from every bound it is not already on; PRIMA satisfies that by default by MOVING the start and
printing a warning. This library cannot print, and a silently moved start is a poor trade, so it
does the other thing PRIMA offers: it reduces `rhobeg` to the room actually available, and reduces
`rhoend` in the same proportion. A run that started next to a bound therefore ends with a smaller
`info%rho` than the `rhoend` asked for — which is the only visible sign, and the reason `info%rho`
is worth reading.

## The two radii

`rhobeg` and `rhoend` are the trust region's initial and final radii, and they are the two
arguments worth thinking about:

- **`rhobeg`** is how far the first steps reach. PRIMA's advice: about a tenth of the greatest
  change you expect in any variable. Too small and the model is fitted to a neighbourhood that
  tells it nothing; too large and the first points miss the structure entirely. Default `1`.
- **`rhoend`** is the accuracy you are asking for in `x`. The run stops when the radius has come
  down to it. Default `1e-6`.

Both are in the units of the variable the engine works in — which is `x` itself unless you pass
`scale=`, and `y = x/scale` if you do. `rhoend` must be positive and no larger than `rhobeg`; a
reversed pair is refused rather than swapped.

`npt` is the number of interpolation points, from `n+2` to `(n+1)(n+2)/2`, default `2n+1`. The
default is the usual choice: a fuller model is more accurate per step and costs more evaluations
to maintain, and on most problems that trade does not pay.

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
| `rhobeg_fraction` | `0.1` | the initial radius: in the scaled units when `scale_from_box`, else this fraction of the narrowest side |
| `rhoend` | `1e-6` | the final radius, in the same units |
| `max_neval` | `0` | evaluations per start; `0` means `500*n` |

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
| `cstrv` | constraint violation; `0` here, since bounds are never violated |

The status codes, and what PRIMA reported to produce each:

| Code | Reached when |
|---|---|
| `PF_OPT_OK` | the trust-region radius came down to `rhoend` — the normal exit |
| `PF_OPT_TARGET` | a value at or below `ftarget` was found |
| `PF_OPT_LIMIT` | `max_neval` ran out, or the iteration cap did |
| `PF_OPT_ROUNDING` | rounding errors left the model unimprovable, or the trust-region subproblem failed |

`history`, when present, holds every point evaluated and its value, in order, in your units.

## When a call is refused

Every one of these is an `error stop` carrying the entry point, the reason and your `context=`:

- a start containing a NaN, or lying outside the bounds;
- a bound pair with no room between them;
- `rhoend` above `rhobeg`, or either not finite and positive;
- an `npt` outside `[n+2, (n+1)(n+2)/2]`;
- a `scale` of the wrong length, or with an element that is not finite and positive;
- a `max_neval` below one or above `huge(1)/2`;
- an objective returning a non-finite value;
- an objective extending `pf_constrained_objective`, which this engine cannot honour.

**PRIMA adjusts where this refuses.** Upstream swaps a reversed pair of radii, clamps an `npt`
out of range, moves a start and treats a bound wider than a threshold as absent, warning each
time. Each of those is a good decision for a library that can warn. This tier prints nothing by
design, so an adjustment would be silent, and a silently adjusted argument is how a caller comes
to believe they asked for something they did not.

## Attribution and licence

The engines here are derived from [PRIMA](https://github.com/libprima/prima) (Reference
Implementation for Powell's methods with Modernization and Amelioration, Zaikun Zhang), BSD-3-
Clause, at commit `43863c69`. The algorithm is M. J. D. Powell's: *The BOBYQA algorithm for bound
constrained optimization without derivatives*, DAMTP 2009/NA06, University of Cambridge.

Each `src/parquet_prima_*.f90` file opens with PRIMA's licence text and a numbered list of what
was changed in vendoring it — fixed kinds in place of the preprocessor, the printing layer
removed, the development assertions removed, `pf_objective` in place of the procedure interface,
`ieee_arithmetic` in place of the hand-rolled predicates, and a refusal in place of the moderated
extreme barrier. The numerical content is upstream's, and a difference from upstream is a defect
here unless that list says otherwise.
