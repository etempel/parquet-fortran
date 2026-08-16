---
title: Random numbers
---

`parquet_random` gives you random numbers that do not depend on the order in which you ask for
them. Every value is a pure function of three coordinates — a seed, a stream index, and a position
within that stream — so iteration 5000 of a loop receives the same number whether it ran first,
last, alone, or spread across 384 cores under a dynamic schedule.

Everything here is reachable from `use parquet`.

## The problem it solves

An ordinary generator carries state. The value you get depends on how many draws came before it,
and in a parallel loop that ordering is decided by the schedule, the thread count and the machine's
timing — none of which you control and none of which reproduce:

```fortran
! Not reproducible: which value lands on which i depends on the schedule.
!$omp parallel do
do i = 1, n
    call random_number(x(i))
end do
```

Locking does not fix this. A lock makes the generator *safe*, not *reproducible* — the draws still
come out in whatever order the threads reach them. Nor does giving each thread its own generator:
the answer then depends on the thread count.

`parquet_random` removes the state instead of guarding it. Ask for the value at a coordinate and
you get that value, always:

```fortran
use parquet
integer(int64) :: seed
real(real64) :: x(n)

seed = 20260816_int64                     ! or pf_random_seed(), recorded for later

!$omp parallel do schedule(dynamic)
do i = 1, n
    x(i) = pf_random_at(seed, i)          ! same value for this i, on any schedule
end do
```

Run it again on one thread, on eight, on a different machine: `x` is identical. That property, not
speed, is the whole point of the module — see [Performance](#performance) below, which is
deliberately not a speed pitch.

## Addressing: seed, stream, draw

Every draw is `(seed, i [, draw])`. Square brackets mark an optional argument throughout this page;
they are not Fortran syntax and never appear in runnable code.

- **`seed`** — `integer(int64)`, the family of streams. Any value, including 0 and negatives.
- **`i`** — the stream index, `integer(int32)` or `integer(int64)`. Any value; an `int32` index
  sign-extends, so the two kinds give identical values. This is normally your loop index.
- **`draw`** — `integer(int64)`, a 1-based position within stream `i`. Omitted means 1.

Two streams never overlap, and neither do two seeds. If you need several independent numbers per
loop iteration, walk the `draw` axis rather than inventing stream arithmetic:

```fortran
do i = 1, n
    x(i) = pf_random_at(seed, i)                  ! draw 1
    y(i) = pf_random_at(seed, i, 2_int64)         ! draw 2, independent of x(i)
end do
```

`draw` is always `integer(int64)`, so an explicit literal is written `2_int64`; a bare `2` is
default-kind and will not compile. For `pf_random_int_at` the stream index shares its kind with the
bounds — one kind per call — so `pf_random_int_at(seed, i, 1, 6)` needs `i` to be a default integer,
and an `int64` stream index needs `int64` bounds.

## Independent families: `pf_random_key`

`pf_random_key(seed, label)` derives an independent seed. Use it when different parts of a program
should not share a stream — the noise draws and the resampling draws, say — so that changing one
does not shift the other:

```fortran
integer(int64) :: noise_seed, boot_seed

noise_seed = pf_random_key(seed, 1_int64)
boot_seed  = pf_random_key(seed, 2_int64)
```

A derived key **is** a seed, so derivations nest: `pf_random_key(pf_random_key(seed, 1), 4)` is
perfectly ordinary, and is how a hierarchy of independent families is built.

One curiosity worth knowing rather than being surprised by: **`pf_random_key(0, 0)` is 0**, and for
every seed there is exactly one label whose derived key is 0. The derivation is a bijection, so
something has to map to zero; hiding it would cost the property that makes derivation
collision-free.

## What you can draw

- **`pf_random_at(seed, i [, draw])`** — `real(real64)` in `[0, 1)`.
- **`pf_random32_at(seed, i [, draw])`** — `real(real32)` in `[0, 1)`.
- **`pf_random_bits_at(seed, i [, draw])`** — `integer(int64)`, 64 raw bits, every pattern possible.
- **`pf_random_int_at(seed, i, lo, hi [, draw])`** — a uniform integer in `[lo, hi]`.
- **`pf_random_fill_at(seed, i, v [, draw])`** — fills a rank-1 `real64` or `real32` array with
  consecutive draws.
- **`pf_random_seed()`** — a fresh, nondeterministic seed.

All the scalar draws are `pure elemental`, so they accept conformable arrays as well as scalars.
For bulk work, prefer the scalar call inside your own loop, or `pf_random_fill_at`; a whole-array
elemental call over a constructed index array is much slower than either.

### Filling several values at once

`pf_random_fill_at` fills `v` with the values at positions `draw .. draw+size(v)-1` of one stream —
the same values the matching scalar calls give, so the two forms are interchangeable:

```fortran
real(real64) :: components(3)

do i = 1, n
    call pf_random_fill_at(seed, i, components)    ! draws 1, 2, 3 of stream i
    call place_particle(components(1), components(2), components(3))
end do
```

Prefixes are prefixes: filling three values gives the first three of a six-value fill. Starting at
`draw = 4` gives exactly the tail. A zero-sized `v` is a defined no-op. Filling is cheaper than the
equivalent scalar calls because one enciphering yields two `real64` values (or four `real32`), and
a fill can use both where a scalar call uses one.

### Integers are exactly uniform

`pf_random_int_at` is **exactly** unbiased at every width, not "unbiased to within 2⁻⁶⁴". It uses a
rejection rule rather than a modulo, so no value in `[lo, hi]` is even slightly more likely than
another — including at widths above 2⁶³, where the obvious implementations silently make up to half
the requested range unreachable.

It is total: `lo > hi` is swapped rather than rejected, and `lo == hi` returns that value.

```fortran
die   = pf_random_int_at(seed, i, 1, 6)
index = pf_random_int_at(seed, i, 1_int64, huge(1_int64))    ! any width, still exact
```

## What is guaranteed, and what is not

**Guaranteed.** For a fixed seed, stream and draw, the value is fixed — for a given machine and
compiler, which is the promise the library makes. In practice it is considerably stronger than
that: the uniform, bits and integer draws have been checked bit-identical across gfortran, ifx and
flang, across arm64 and x86-64, and across all three fpm build profiles, and the test suite
asserts exactly those values wherever it runs, so a machine that disagreed would fail the suite
rather than quietly return something else.

The three fpm profiles — the default, `--profile release` and `--profile debug` — are required to
agree, and that requirement is checked by the same golden vectors under each.

`pf_random_algorithm` names the frozen contract:

```fortran
print *, pf_random_algorithm            ! philox4x32-10/v1
```

Its value changes if and only if some value the module can produce changes. Record it alongside a
seed if you need to be able to tell, years later, whether a stored result is still reproducible.

**Not guaranteed.** `pf_random_seed()` is nondeterministic by design — that is its whole job. And
sequences are per `(seed, i)`, **per procedure and per result type**: `pf_random32_at` is not a
narrowing of `pf_random_at`, and does not visit the same values. That is deliberate. A narrowed
`real64` draw can round up to exactly 1.0, which would break the `[0, 1)` promise; drawing
`real32` from its own one-word sequence cannot.

### Endpoints and identities

`pf_random_at` and `pf_random32_at` both return values in `[0, 1)`. 0.0 is attainable — with
probability 2⁻⁵³ for the `real64` form — and 1.0 is unreachable by construction, so a caller may
divide by `1 - x` but not by `x`.

`pf_random_at(seed, i, draw)` is exactly `pf_random_bits_at(seed, i, draw)`'s top 53 bits scaled
into `[0, 1)`. That identity is contract and is asserted by the suite.

**`pf_random_int_at` shares no such identity with anything**, and none should be assumed. It reads
the same two words as `pf_random_at` at the same coordinate, but a rejection moves it to a
different key, so the two coincide only when no rejection happened. It also consumes one whole
enciphering per value and uses half of it — which costs nothing today and is worth knowing if you
are counting: a future bulk integer fill will be roughly 0.6× the throughput of its `real64`
sibling for that reason.

### Not cryptographic

The generator is fast and statistically excellent, and it is **not** cryptographic. The seed is
recoverable from a handful of outputs. Do not use it for keys, tokens, passwords, nonces, or
anything else an adversary must not be able to predict — take those from a CSPRNG (your platform's
`getrandom`, `/dev/urandom`, or a library built for the purpose). `pf_random_seed()` is not one
either: it folds the clock and a call counter, both of which are guessable.

## Performance

**This module is not here to be faster than `random_number`, and on some machines it is not.**
Under the flags fpm actually passes, a scalar `pf_random_at` measures about 1.8× *slower* than a
scalar `random_number` call on gfortran, and about 1.6× faster on ifx. Which side you land on
depends on the machine and compiler.

What `random_number` cannot do at any speed is give you the same answer under a different OpenMP
schedule. That is what you are buying.

Two notes if you measure it yourself. Quote figures from `--profile release` only; and note that
"unoptimised" is a different profile per compiler — the default profile is the slow one for
gfortran and flang, while for ifx it is `--profile debug`, since ifx optimises at `-O2` by default
and fpm passes it no `-O` in either of the other two.

## A note for the curious: two multiply paths

The cipher's inner loop multiplies two 32-bit values into a 64-bit result, which does not fit in a
signed 64-bit integer. Where the compiler offers a 128-bit integer kind — gfortran and flang do —
the module forms that product in it, which makes the multiply provably in range. Where there is no
such kind — ifx — it uses the wrapping 64-bit product, under an assumption verified on that
compiler rather than assumed.

**Both paths produce identical values**, which the suite asserts on every machine it runs on, and
`pf_random_algorithm` is the same either way. The choice is made at compile time from the compiler
in use; there is nothing to configure, and deliberately no way to override it.

## See also

- [Thread safety](../operating/thread-safety.html) — why this module needs no locking at all.
- [Sorting arrays and columns](sorting.html) — `pf_sort` and friends, the other `pf_`-prefixed
  utility API.
