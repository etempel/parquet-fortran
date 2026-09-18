---
title: Transforms: the discrete cosine transform
---

`parquet_transform` computes the discrete cosine transform of a `real64` sequence whose length is a
power of two, and its inverse, in the convention scipy uses. It reaches no reader, no writer and no
setting: a program can `use parquet_transform` on its own, compiling two Fortran files and nothing
of the Arrow stack. `use parquet` brings it in too, so nothing here needs a second import. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

## Quick example

```fortran
use parquet_transform
use iso_fortran_env, only : real64

real(real64) :: x(8), y(8), back(8)

x = [1.0_real64, -2.0_real64, 3.5_real64, 0.25_real64, -1.75_real64, 4.0_real64, &
     -0.5_real64, 2.25_real64]
call pf_dct(x, y)        ! y(1) is 2*sum(x) = 13.5
call pf_idct(y, back)    ! back is x again, to rounding
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
call pf_dct(x, y, [norm], [context])
call pf_idct(y, x, [norm], [context])
ok = pf_is_pow2(n)
p = pf_next_pow2(n)
```

## The convention

For a sequence `x` of length `n`, with indices from 0 in the formulas (`x(j+1)` holds `x[j]`):

```
pf_dct:   y[k] = 2 * sum_{j=0}^{n-1} x[j] * cos(pi*k*(2j+1) / (2n)),       k = 0 .. n-1
pf_idct:  x[k] = (y[0] + 2 * sum_{j=1}^{n-1} y[j] * cos(pi*j*(2k+1) / (2n))) / (2n)
```

`pf_dct` is the type-II transform and `pf_idct` its exact inverse, the type-III transform divided
by `2n`. They are `scipy.fft.dct(x)` and `scipy.fft.idct(y)` under scipy's defaults, which
`scipy.fftpack.dct` shares, so a coefficient computed here agrees with one computed there to
rounding.

**The factor of two is part of the convention.** `y(1)` is twice the sum of the sequence, not the
sum. Code written against scipy relies on it — KDEpy's Improved Sheather-Jones bandwidth rule halves
the coefficients before squaring them — and a transform without it would put every such result out
by a constant factor while looking entirely plausible.

`x` and `y` must have the same size and must not overlap: there is no in-place form.

## Normalisation: `norm=`

- `"none"`, the default: the formulas above.
- `"ortho"`: the coefficients scaled by `sqrt(1/(4n))` at `k = 0` and by `sqrt(1/(2n))` elsewhere,
  which is scipy's `norm='ortho'`. The transform is then orthonormal: it keeps the sum of squares,
  and `pf_idct` is the transpose of `pf_dct`.

Pass the same `norm` in both directions: `pf_idct` gives back the sequence `pf_dct` transformed only
when the two calls agree. The token is matched case-insensitively, and any other token is an error.
scipy's `"backward"` is what this page calls `"none"`; its `"forward"` is not offered.

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

## Values the transform does not screen

The transform only multiplies and adds; it never compares a data value.

- A NaN anywhere in the input makes every output value NaN, and raises no floating-point exception.
- An infinity makes output values infinite or NaN, and can raise `IEEE_INVALID` on the way.
- A sequence large enough that `n` times its largest magnitude approaches `huge` can overflow.

Under a build that stops on a floating-point exception (nagfor's default `-ieee=stop` is one), the
last two end the program. Screen the input first if it can hold either.

## What aborts

Every abort message begins with the procedure's name — `pf_dct: `, `pf_idct: ` or
`pf_next_pow2: ` — and `pf_dct` and `pf_idct` add ` (context: ...)` when `context=` was given.
`context` is capped at 100 characters. These are caller-contract violations, checked before
anything is transformed:

| Condition | Message |
|---|---|
| the input is empty | `the sequence must not be empty` |
| the input's length is not a power of two | `the sequence length must be a power of two (got 1000; pf_next_pow2 gives 1024)` |
| `x` and `y` differ in size | `x and y must have the same size` |
| `norm` is not `"none"` or `"ortho"` | `norm must be "none" or "ortho"` |
| `pf_next_pow2` given `n < 1` | `n must be positive` |
| `pf_next_pow2` given `n` above `2**30` | `n must not exceed 1073741824, the largest power of two a default integer holds` |

The input is `x` for `pf_dct` and `y` for `pf_idct`. The length message names the length given and
the power of two above it; for a length above `2**30`, where `pf_next_pow2` stops, it names that
power of two without the procedure.

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
