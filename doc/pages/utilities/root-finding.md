---
title: Root finding: solving f(x) = 0 in one variable
---

`parquet_root` finds where a function of one `real64` variable crosses zero, on a bracket you give
it, by Brent's method. When the bracket you have does not yet contain a sign change, it widens the
bracket first, under a growth policy you state. It reaches no reader, no writer and no setting: a
program that has a function in hand can `use parquet_root` and solve it, compiling two Fortran
files and nothing of the Arrow stack. `use parquet` brings it in too, so nothing here needs a
second import. See [Choosing a module](../operating/choosing-a-module.html) for what each entry
module costs.

## Quick example

```fortran
use parquet_root
use iso_fortran_env, only : real64

type(pf_root_info) :: info
real(real64) :: x

call pf_find_root(cubic, 2.0_real64, 3.0_real64, x, info=info)
print '(a, es22.15, a, i0, a)', 'root at ', x, ' after ', info%neval, ' evaluations'
```

with the function a module procedure:

```fortran
function cubic(x) result(y)
    real(real64), intent(in) :: x
    real(real64)             :: y
    y = (x*x - 2.0_real64)*x - 5.0_real64
end function cubic
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
call pf_find_root(f, a, b, x, [expand], [rtol], [atol], [max_neval], [converged], [info], &
                  [history], [context])
```

## The function

Two ways to supply it, and the choice is about parameters rather than taste.

**A plain function** matching `pf_rootfun_func` — one `real64` in, one `real64` out — is the short
form, for a function that needs nothing but `x`.

**An object extending `pf_rootfun`** carries its parameters as components. Extend the type, add
what the function needs, and implement `eval`. Kepler's equation, `E - e*sin(E) = M`, solved for
the eccentric anomaly `E`:

```fortran
type, extends(pf_rootfun) :: kepler
    real(real64) :: mean_anomaly = 0.0_real64
    real(real64) :: eccentricity = 0.0_real64
contains
    procedure :: eval => kepler_eval
end type kepler

function kepler_eval(self, x) result(f)
    class(kepler), intent(inout) :: self
    real(real64), intent(in)     :: x
    real(real64)                 :: f
    f = x - self%eccentricity*sin(x) - self%mean_anomaly
end function kepler_eval
```

The passed-object dummy is `intent(inout)`, so `eval` may keep a call counter or a cache, and an
extension must declare the same intent. Pass the object where the function would go:

```fortran
type(kepler) :: k
real(real64) :: ecc_anomaly
k%mean_anomaly = 1.0_real64
k%eccentricity = 0.3_real64
call pf_find_root(k, 0.0_real64, 2.0_real64*acos(-1.0_real64), ecc_anomaly)
```

**The callback must be a module procedure or a type-bound procedure, never an internal one**, and
one object per thread — see [Solver conventions](solvers.html#the-function-you-supply), which
states both rules once for all four solver modules.

**Your function must not return a NaN.** A NaN has no sign, so a root finder has nothing to do
with it, and `pf_find_root` stops the program with `pf_find_root: the function returned a NaN`.
Where your function can produce one — a logarithm of a negative argument, a square root below
zero — guard it, or keep the bracket and the expansion limits away from that region. An infinity
is different: it has a sign, and it is used (below).

## The bracket

`a` and `b` are both required and both finite, with `a < b`, and `b - a` must be finite too: two
ends near `-huge` and `+huge` are refused rather than overflowing inside the search. Without an
expansion, `f(a)` and `f(b)` must differ in sign; when they do not, the answer is the status
`PF_ROOT_NO_BRACKET`, not an abort.

An exact zero is the answer the moment it is seen: `f(a) == 0` returns `a` after one evaluation,
and the same holds for `b` and for every point the search evaluates afterwards.

**Several roots in the bracket**: the method converges to one of them, and which one depends on
the values — not necessarily the smallest. When you need the smallest root above some point, grow
the bracket upward from that point (below), which stops at the first sign change it meets.

## Growing the bracket: `expand=`

Pass a `pf_bracket_expansion` and a bracket whose ends have the same sign is widened until they
do not. The type's components are the policy:

| Component | Default | Meaning |
|---|---|---|
| `mode` | `PF_EXPAND_NONE` | which ends move: one of the four codes below |
| `factor` | `2` | each try multiplies the bracket's width by this; must be finite and above 1 |
| `max_tries` | `64` | tries before giving up; `0` expands nothing |
| `lower_limit` | `-huge(1.0_real64)` | the lower end never goes below this; finite, at or below `a` |
| `upper_limit` | `huge(1.0_real64)` | the upper end never goes above this; finite, at or above `b` |

Each try takes the width `w = b - a` and moves the ends the mode names:

| `mode` | The new bracket | Evaluations per try |
|---|---|---|
| `PF_EXPAND_NONE` | no expansion: the bracket must already change sign | — |
| `PF_EXPAND_UP` | `a` held, `b` moved to `a + factor*w` | 1 |
| `PF_EXPAND_DOWN` | `b` held, `a` moved to `b - factor*w` | 1 |
| `PF_EXPAND_BOTH` | both ends `factor*w/2` either side of the midpoint, the lower end first | up to 2 |

The default object expands not at all, so a call with no `expand=` is the strict contract of a
bracketed method and a caller who wants that writes nothing.

**The expansion stops at the first sign change it sees**, and the bracket it found is the one
solved on. Growing a small bracket upward from `a` therefore finds the smallest root above `a`
that a probe steps over — the property a fixed-point search needs when its function crosses zero
twice and only the first crossing is wanted. In `PF_EXPAND_BOTH` the lower end moves and is
evaluated first, and a sign change there ends the expansion before the upper end moves.

**No end goes past its limit.** A move that would is cut short at the limit, so the last try
probes the limit itself; an end already there stays put and is not evaluated again; and when
neither end can move, the expansion ends as it does when `max_tries` runs out. No expansion
builds a bracket wider than the arithmetic can measure either, whatever the limits say.

Either way, a sign change that was never found is `PF_ROOT_NO_BRACKET`, returned rather than
aborted on, with `x` the end of the last bracket with the smaller `|f|` and `info%bracket_lo` and
`info%bracket_hi` the last bracket tried. A caller can retry with a wider policy, or report that
the data cannot support the answer.

The bracket search of the Improved Sheather-Jones bandwidth rule (`parquet_kde`'s default) is one
object — start at the smallest `t` the rule accepts, `t_min`, double the bracket, stop at 1:

```fortran
type(pf_bracket_expansion) :: grow
grow%mode        = PF_EXPAND_UP
grow%factor      = 2.0_real64
grow%upper_limit = 1.0_real64
call pf_find_root(fixed_point, t_min, 2.0_real64*t_min, t, expand=grow, info=info)
if (info%status == PF_ROOT_NO_BRACKET) then
    ! no sign change below 1: the sample is too small for this rule
end if
```

## Tolerances

`rtol` is a relative tolerance on `x`, default `4*epsilon`, which is also its floor: a smaller
value, zero included, is raised to it. `atol` is an absolute one, default 0. The search stops when
the bracket around the root is no wider than `2*t`, with

```
t = 2*epsilon*|x| + max(atol, rtol*|x|)/2
```

so `x` is within `2*t` of the root: about eight units of `epsilon*|x|` under the defaults, which is
full precision at any magnitude. A root near `1e-9` comes back with the same relative accuracy as
a root near 1 — no absolute floor is hiding in the defaults. A looser `rtol` or `atol` stops
sooner, which is worth having when each evaluation is costly.

**A root at exactly zero is the one place a relative tolerance cannot help**: the bracket around it
cannot become narrow relative to `|x|`. A simple root there is usually hit exactly, because the
interpolation lands on it; a multiple root, `x**3` say, is approached one bisection at a time and
spends the budget. Pass an `atol` when the root may be zero.

## Budget and outcome

`max_neval` bounds the function evaluations, the expansion's included; it defaults to 200, and it
is never exceeded. Brent's method converges in a handful to a few tens of evaluations on a
well-behaved function, against the fifty or so bisection alone needs for full precision, so
reaching the budget usually means the function, not the caller. Reaching it is a status, not an
error — and it is `PF_ROOT_LIMIT` even when the budget ran out while the bracket was still being
widened, because the policy was not what ran out: a bigger `max_neval` is the fix.

`converged=` is the short answer. `info=` is the long one, a `pf_root_info` carrying:

| Component | What it says |
|---|---|
| `status` | one of the `PF_ROOT_*` codes below |
| `converged` | `status == PF_ROOT_OK` |
| `neval` | function evaluations, the expansion's included |
| `niter` | Brent iterations, after the bracket was found |
| `nexpand` | expansion tries that evaluated something |
| `froot` | `f` at the returned `x` |
| `bracket_lo`, `bracket_hi` | the bracket Brent's method started from; on `PF_ROOT_NO_BRACKET`, or a budget spent while widening, the last bracket tried |

| Code | Means | `x` is | What to do |
|---|---|---|---|
| `PF_ROOT_OK` | the tolerance was met, or a value was exactly zero | the root | read `froot` if a pole is possible |
| `PF_ROOT_LIMIT` | `max_neval` ran out first | the best point reached | raise `max_neval`, or loosen the tolerance |
| `PF_ROOT_NO_BRACKET` | no sign change: none was asked for, or the expansion ran out of tries or room | the end of the last bracket where `f` is smaller in magnitude | widen the policy, or accept that there is no root in reach |

**Nothing is ever printed.** This module reads no verbosity setting and has no message stream; it
reports through these values, and speaks only by aborting when a caller contract is broken.

### An infinity is a sign

A function may return `+Infinity` or `-Infinity`, at an end or anywhere in between, and the sign is
used like any other: a function that runs off to `-Infinity` above its root brackets that root as
well as one that stays finite. An interval with an infinite end is bisected rather than
interpolated through, until that end is finite again, so an infinite value costs evaluations but
never an answer. Build an infinity with `ieee_value`, not by overflowing an expression: the
overflow raises `IEEE_OVERFLOW`, which some compilers are configured to treat as fatal.

### A pole looks like a root

A sign change across a pole — `1/(x - 0.5)` on `[0, 1]` — is found by every bracketing method as if
it were a root, and this one converges to 0.5 and reports `PF_ROOT_OK`. `info%froot` tells the two
apart: near a root it is close to zero, at a pole it is huge or infinite. Nothing here guesses a
scale and rewrites the answer, so read `froot` whenever your function could have a pole inside the
bracket.

## The evaluation record

Pass `history=` and you get back a `pf_root_history`: every point evaluated, in order, and the
value returned there — the two ends, each expansion probe, each Brent step — with `n` saying how
many there are. It is trimmed to `n`, so `x(1:n)` and `f(1:n)` are the whole record and
`size(x) == n`; `n` equals `info%neval`. It costs memory only when asked for, and asking for it
changes neither the answer nor the count. It is what shows why an expansion failed: which brackets
were probed, and what came back.

## What aborts

Every abort message begins `pf_find_root: `, and carries ` (context: ...)` when `context=` was
given. `context` is capped at 100 characters. These are caller-contract violations, checked before
the first evaluation — bar the last row, which is about your function:

| Condition | Message |
|---|---|
| `a` or `b` NaN or infinite, or `a >= b` | `the bracket must satisfy a < b with finite ends` |
| `b - a` overflows | `the bracket width must be finite` |
| `rtol` NaN, infinite or negative | `rtol must be a finite, non-negative number` |
| `atol` NaN, infinite or negative | `atol must be a finite, non-negative number` |
| `max_neval < 1` | `max_neval must be positive` |
| `expand%mode` not a `PF_EXPAND_*` code | `expand%mode must be one of PF_EXPAND_NONE, PF_EXPAND_UP, PF_EXPAND_DOWN, PF_EXPAND_BOTH` |
| `expand%factor` NaN, infinite or not above 1 | `expand%factor must be a finite number greater than 1` |
| `expand%max_tries < 0` | `expand%max_tries must not be negative` |
| a limit NaN or infinite, `lower_limit > a` or `upper_limit < b` | `the expansion limits must be finite and lie outside the initial bracket` |
| the function returned a NaN | `the function returned a NaN` |

The expansion policy is checked whether or not its `mode` expands. Every abort is taken under a
named `critical`, so one thread aborts rather than several, and the exit status stays meaningful
when the call was inside a parallel region.

## Thread safety

`parquet_root` is reentrant. It has no variable that is not a compile-time constant, every work
variable is a local of the call, and the only state that outlives a call is your own function
object. Solve from as many threads as you like, giving each its own `pf_rootfun` object; the
plain-function form shares nothing at all. **A function object may not be shared between
threads**: `eval` takes the object `intent(inout)` precisely so it may keep a counter or a cache,
and two threads evaluating one object race over it.

See [Thread safety](../operating/thread-safety.html) for how this sits beside the rest of the
library.

## The method

Brent's method (R. P. Brent, *Algorithms for Minimization without Derivatives*, 1973), in the form
of `zeroin.f` by Forsythe, Malcolm and Moler: bisection, which guarantees convergence, with a
secant or inverse-quadratic step taken instead whenever it lands well inside the bracket and
shrinks it faster than bisection would. It is written for this library rather than vendored, and
it departs from `zeroin.f` in four places: the sign test compares signs rather than forming a
product, which would underflow for two tiny values and overflow for two huge ones; an
interpolation step is taken only where every value it is formed from is finite and in range, and
a bisection otherwise, which is what makes an infinity usable as a sign and keeps every
intermediate finite; the stopping tolerance carries the relative term `rtol*|x|`; and the bracket
may be widened first. Where the values are ordinary the steps are `zeroin.f`'s.

**Minimising `f(x)**2` or `abs(f(x))` is not a substitute.**
[`pf_minimize_scalar`](../utilities/optimization.html#one-variable-on-a-bracket) is Brent's
minimiser, the sibling of this method, and it minimises: near a minimum a function is flat to
second order, so a minimiser locates its abscissa only to about the square root of `epsilon`, and
squaring the residual halves the digits again — four or so, where this method delivers sixteen.
It also throws away the sign change, which is what makes a bracketed root finder unconditionally
convergent.
