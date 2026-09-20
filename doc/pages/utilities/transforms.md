---
title: Transforms: the discrete cosine and sine transforms
---

`parquet_transform` computes the discrete cosine and sine transforms of a `real64` sequence whose
length is a power of two, and their inverses, in the convention scipy uses. It reaches no reader, no
writer and no setting: a program can `use parquet_transform` on its own, compiling two Fortran files
and nothing of the Arrow stack. `use parquet` brings it in too, so nothing here needs a second
import. See [Choosing a module](../operating/choosing-a-module.html) for what each entry module
costs.

## Quick example

```fortran
use parquet_transform
use iso_fortran_env, only : real64

real(real64) :: x(8), y(8), back(8)

x = [1.0_real64, -2.0_real64, 3.5_real64, 0.25_real64, -1.75_real64, 4.0_real64, &
     -0.5_real64, 2.25_real64]
call pf_dct(x, y)        ! y(1) is 2*sum(x) = 13.5
call pf_idct(y, back)    ! back is x again, to rounding

call pf_dst(x, y)        ! the sine transform of the same sequence
call pf_idst(y, back)    ! back is x again, to rounding
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
call pf_dct(x, y, [norm], [context])
call pf_idct(y, x, [norm], [context])
call pf_dst(x, y, [norm], [context])
call pf_idst(y, x, [norm], [context])
ok = pf_is_pow2(n)
p = pf_next_pow2(n)
```

## The convention

For a sequence `x` of length `n`, with indices from 0 in the formulas (`x(j+1)` holds `x[j]`):

```
pf_dct:   y[k] = 2 * sum_{j=0}^{n-1} x[j] * cos(pi*k*(2j+1) / (2n)),       k = 0 .. n-1
pf_idct:  x[k] = (y[0] + 2 * sum_{j=1}^{n-1} y[j] * cos(pi*j*(2k+1) / (2n))) / (2n)
pf_dst:   y[k] = 2 * sum_{j=0}^{n-1} x[j] * sin(pi*(k+1)*(2j+1) / (2n)),   k = 0 .. n-1
pf_idst:  x[k] = ((-1)**k * y[n-1]
                  + 2 * sum_{j=0}^{n-2} y[j] * sin(pi*(j+1)*(2k+1) / (2n))) / (2n)
```

`pf_dct` and `pf_dst` are the type-II transforms and `pf_idct` and `pf_idst` their exact inverses,
the type-III transforms divided by `2n`. They are `scipy.fft.dct(x)`, `idct(y)`, `dst(x)` and
`idst(y)` under scipy's defaults, which are type 2, so a coefficient computed here agrees with one
computed there to rounding.

**The two inverses differ in one term.** The cosine transform's unpaired coefficient is the
constant `y[0]`; the sine transform's is the top frequency `y[n-1]`, which alternates in sign. Read
side by side:

```
pf_idct:  x[k] = (y[0]             + 2 * sum_{j=1}^{n-1} y[j]*cos(...)) / (2n)
pf_idst:  x[k] = ((-1)**k * y[n-1] + 2 * sum_{j=0}^{n-2} y[j]*sin(...)) / (2n)
```

That one difference is the same fact as the `norm="ortho"` exception below and as the index offset
further down: the two families carry their unpaired frequency at opposite ends.

**The factor of two is part of the convention.** `y(1)` is twice the sum of the sequence, not the
sum. Code written against scipy relies on it — KDEpy's Improved Sheather-Jones bandwidth rule halves
the coefficients before squaring them — and a transform without it would put every such result out
by a constant factor while looking entirely plausible.

`x` and `y` must have the same size and must not overlap: there is no in-place form.

## Normalisation: `norm=`

- `"none"`, the default: the formulas above.
- `"ortho"`: the coefficients scaled by `sqrt(1/(4n))` at one index and by `sqrt(1/(2n))`
  elsewhere, which is scipy's `norm='ortho'`. The transform is then orthonormal: it keeps the sum
  of squares, and each inverse is the transpose of its forward transform.

**Which index carries `sqrt(1/(4n))` differs between the two families**: it is `k = 0` for `pf_dct`
and `pf_idct`, and `k = n-1` for `pf_dst` and `pf_idst`. It is the one part of the sine pair's
contract that does not follow from the cosine pair's.

Pass the same `norm` in both directions: `pf_idct` gives back the sequence `pf_dct` transformed only
when the two calls agree, and likewise for the sine pair. The token is matched case-insensitively,
and any other token is an error. scipy's `"backward"` is what this page calls `"none"`; its
`"forward"` is not offered.

## Choosing the length

`size(x)` must be a positive power of two: 1, 2, 4, and so on. Any other length is an error, not a
slower path. `pf_is_pow2(n)` answers whether `n` is one, and `pf_next_pow2(n)` gives the smallest
one at or above `n`, for `n` from 1 to `2**30`, the largest power of two a default integer holds.

**Choose the length before you build the sequence.** When the sequence is a histogram or a binned
sample, take the number of cells from `pf_next_pow2` and bin into that many:

```fortran
integer :: ncell
real(real64), allocatable :: counts(:), coef(:)

ncell = pf_next_pow2(1000)    ! 1024
allocate (counts(ncell), coef(ncell))
! ... bin the sample into all ncell cells ...
call pf_dct(counts, coef)
```

**Zero-padding a sequence you have already built is not a substitute.** Unlike a convolution, a
cosine transform does not ignore the padding: the transform of a padded sequence is the transform
of a longer sequence that steps down to zero at its end, and its coefficients mean something else.

## Composing the two transforms

Convolving a binned sample against an even kernel needs a cosine transform both ways; against an
**odd** kernel it needs a cosine transform in and an inverse **sine** transform out. Passing a
spectrum from one family to the other is not a plain hand-over:

**`pf_dct` coefficient `k` and `pf_dst` coefficient `k` are different frequencies.** The cosine
coefficient at index `k` carries frequency `k`; the sine coefficient at index `k` carries frequency
`k+1`. So a spectrum produced by `pf_dct` and consumed by `pf_idst` moves down one position, and
the top frequency has no sine coefficient at all:

```fortran
! co  holds pf_dct's coefficients, co(k+1) at frequency k
! odd holds the odd kernel's transform at those same frequencies
! sc  is what pf_idst consumes, sc(k+1) at frequency k+1
sc(1:n-1) = co(2:n)*odd(2:n)
sc(n)     = 0.0_real64        ! the top frequency has no sine coefficient
call pf_idst(sc, result)
```

**Leaving the shift out does not fail loudly.** It returns a result of the right shape and
magnitude, computed at the wrong frequencies throughout. On one worked example — a local-linear
boundary correction over 512 cells — the odd convolution agrees with a direct summation to
`7.2e-06` relative rms with the shift and to `4.1e-01` without it: five orders of magnitude, with
nothing in the output to say which you got. Assert the composition against a direct sum on a small
case rather than eyeballing the result.

## Values the transform does not screen

The transform only multiplies and adds; it never compares a data value.

- A NaN anywhere in the input makes every output value NaN, and raises no floating-point exception.
- An infinity makes output values infinite or NaN, and can raise `IEEE_INVALID` on the way.
- A sequence large enough that `n` times its largest magnitude approaches `huge` can overflow.

Under a build that stops on a floating-point exception (nagfor's default `-ieee=stop` is one), the
last two end the program. Screen the input first if it can hold either.

## What aborts

Every abort message begins with the procedure's name — `pf_dct: `, `pf_idct: `, `pf_dst: `,
`pf_idst: ` or `pf_next_pow2: ` — and the four transforms add ` (context: ...)` when `context=` was
given. `context` is capped at 100 characters. A refused `pf_dst` call names `pf_dst`, never the
cosine transform it is built on. These are caller-contract violations, checked before anything is
transformed:

| Condition | Message |
|---|---|
| the input is empty | `the sequence must not be empty` |
| the input's length is not a power of two | `the sequence length must be a power of two (got 1000; pf_next_pow2 gives 1024)` |
| `x` and `y` differ in size | `x and y must have the same size` |
| `norm` is not `"none"` or `"ortho"` | `norm must be "none" or "ortho"` |
| `pf_next_pow2` given `n < 1` | `n must be positive` |
| `pf_next_pow2` given `n` above `2**30` | `n must not exceed 1073741824, the largest power of two a default integer holds` |

The input is `x` for `pf_dct` and `pf_dst`, and `y` for `pf_idct` and `pf_idst`. The length message
names the length given and the power of two above it; for a length above `2**30`, where
`pf_next_pow2` stops, it names that power of two without the procedure.

There is no status code and no record of what happened: a transform has no budget and no
convergence, so it either runs or refuses the call. Every abort is taken under a named `critical`,
so one thread aborts rather than several, and the exit status stays meaningful when the call was
inside a parallel region.

## Thread safety

`parquet_transform` is reentrant. It has no variable that is not a compile-time constant, and each
call allocates its own workspace and releases it before returning. Transform from as many threads
as you like, each on its own arrays. See [Thread safety](../operating/thread-safety.html) for how
this sits beside the rest of the library.

## The method

`pf_dct` reorders the sequence — the even-indexed values in order, then the odd-indexed ones
reversed — takes one complex fast Fourier transform of the same length, and multiplies each output
by a twiddle factor (J. Makhoul, "A fast cosine transform in one and multiple dimensions", 1980).
`pf_idct` runs the same steps backwards. The FFT is radix-2, decimation in time, and written for
this library. Every twiddle factor is computed directly from its angle rather than by recurrence,
which keeps the rounding error growing with `log2(n)` rather than with `n`. The work grows as
`n*log2(n)`, and nothing is cached between calls.

**The sine transforms use that same engine and no other.** Negating every odd-indexed value of the
sequence, taking the cosine transform and reading the result backwards gives the sine transform:

```
pf_dst(x)[k] = pf_dct(x')[n-1-k],   where x'[j] = (-1)**j * x[j]
```

`pf_idst` runs those three steps backwards. The reversal is also what moves the `norm="ortho"`
exception from one end to the other. So a sine transform costs a cosine transform plus one pass
over the sequence, and there is no second engine, no second convention and no second set of
rounding behaviour to know about.
