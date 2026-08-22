---
title: Random numbers
---

`parquet_random` gives you random numbers that do not depend on the order in which you ask for
them. Every value is a pure function of three coordinates — a seed, a stream index, and a position
within that stream — so iteration 5000 of a loop receives the same number whether it ran first,
last, alone, or spread across 384 cores under a dynamic schedule.

Everything here is reachable from `use parquet`.

**Two modules sit behind that, and the split matters only if you import them directly.**
`parquet_random` is the generator — draws, bits, integers, streams and derived seeds — and it is
the smallest import in the library: three Fortran files, no settings and no sort.
`parquet_sampling` is everything that draws from a *population*: `pf_random_perm_at`,
`pf_random_permutation`, `pf_random_subset`, `pf_random_resample`, and the whole weighted family.
It needs a sort, and takes it from `parquet_argsort`, so it costs eight files rather than three.
`use parquet` gives you both and the distinction never arises.

Neither reaches this library's C++ bindings, so neither pulls the Arrow/Parquet headers into your
Fortran build. That is a statement about the *Fortran* graph and not about linking: `link` is a
package-level key in `fpm.toml`, so every import of this package still compiles the C++ wrapper and
still links `-larrow`. See [Choosing a module](../operating/choosing-a-module.html) for the
per-module file counts and what the guarantee does and does not cover.

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
program random_quickstart
    use parquet
    use iso_fortran_env, only: int32, int64, real64
    implicit none

    integer(int32), parameter :: n = 1000000
    integer(int64) :: seed
    real(real64) :: x(n)
    integer(int32) :: i

    seed = 20260816_int64                     ! or pf_random_seed(), recorded for later

    !$omp parallel do schedule(dynamic)
    do i = 1, n
        x(i) = pf_random_at(seed, i)          ! same value for this i, on any schedule
    end do

    print *, x(1), x(500000), x(n)
end program random_quickstart
```

`use parquet` reaches every name on this page, but it does not re-export `iso_fortran_env` — the
`int64`/`real64` kinds need their own `use`, as above.

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
- **`pf_random_fill_draws(seed, i, v [, draw])`** — fills a rank-1 `real64` or `real32` array with
  consecutive draws of **one** stream.
- **`pf_random_fill_streams(seed, i0, v [, draw])`** — fills a rank-1 `real64` or `real32` array
  with **one** draw of each of consecutive streams.
- **`pf_random_seed()`** — a fresh, nondeterministic seed.

All the scalar draws are `pure elemental`, so they accept conformable arrays as well as scalars.
For bulk work, prefer the scalar call inside your own loop or one of the two fills; a whole-array
elemental call over a constructed index array is much slower than any of them.

Beyond the uniforms there are four **distributions** — exponential, normal, Gamma and Poisson. They
are addressed the same way and are documented in [Distributions](#distributions) below.

### Which fill: the two axes

The two fills walk the two axes, and the names say which:

```
                draw ->      1     2     3     4
    stream 1              [ a ]  [ b ]  [ c ]  [ d ]      pf_random_fill_draws(seed, 1, v)
      |    2                e      f      g      h        fills one ROW
      v    3                i      j      k      l
           4                m      n      o      p

                           pf_random_fill_streams(seed, 1, v)
                           fills one COLUMN (a, e, i, m)
```

Use `pf_random_fill_draws` when one thing needs several numbers — a particle needs `x`, `y`, `z`.
Use `pf_random_fill_streams` when many things need one number each, which is the bulk form of the
loop at the top of this page:

```fortran
real(real64) :: x(n)

call pf_random_fill_streams(seed, 1, x)      ! x(i) == pf_random_at(seed, i), for every i
```

Both are prefix-consistent and interchangeable with the matching scalar calls, so neither changes
a single value — they are faster ways to ask for numbers you could already have asked for.

**They are not equally cheap, and the reason is worth knowing.** One enciphering produces four
32-bit words. `pf_random_fill_draws` walks along a stream, so it spends all four on consecutive
values — two `real64`s, or four `real32`s, per enciphering. `pf_random_fill_streams` walks across
streams, and consecutive streams are *different* streams, so each value needs its own enciphering
and the remaining words belong to draws this call was not asked for. So the stream-axis fill is a
worthwhile saving over the scalar loop it replaces — roughly 1.5× — while the draw-axis fill is
about twice as fast again per value. If you need several values
per stream, ask for them along the draw axis.

Each fill has a precondition on the axis it walks: the last position it addresses must be
representable in `integer(int64)` — `draw + size(v) - 1` for one, `i0 + size(v) - 1` for the other.
Ordinary calls are nowhere near either.

**Both fills also take an integer array**, with the range given as two further required arguments:

```fortran
integer :: dice(n), lots(n)

call pf_random_fill_draws  (seed, 1, dice, 1, 6)   ! dice(k) == pf_random_int_at(seed, 1, 1, 6, k)
call pf_random_fill_streams(seed, 1, lots, 1, 6)   ! lots(k) == pf_random_int_at(seed, k, 1, 6)
```

`lo` and `hi` share the array's kind (`integer(int32)` or `integer(int64)`), and a reversed range is
swapped rather than refused, exactly as in `pf_random_int_at`. Each element is precisely the scalar
integer draw at the same coordinate, so these are interchangeable with a loop in the same way the
real-valued fills are.

**The integer draw-axis fill amortises just as the `real64` one does.** An integer draw costs one
word pair, so two consecutive draws are the two pairs of a single enciphering, and the fill reuses
the block it already has — worth roughly 1.5× against a loop of `pf_random_int_at`. The
stream-*axis* integer fill is the one that cannot amortise, since each element belongs to a
different stream and so needs its own enciphering.

### Filling several values at once

`pf_random_fill_draws` fills `v` with the values at positions `draw .. draw+size(v)-1` of one stream —
the same values the matching scalar calls give, so the two forms are interchangeable:

```fortran
real(real64) :: components(3)

do i = 1, n
    call pf_random_fill_draws(seed, i, components)    ! draws 1, 2, 3 of stream i
    call place_particle(components(1), components(2), components(3))
end do
```

Prefixes are prefixes: filling three values gives the first three of a six-value fill. Starting at
`draw = 4` gives exactly the tail. A zero-sized `v` is a defined no-op. Filling is cheaper than the
equivalent scalar calls because one enciphering yields two `real64` values (or four `real32`), and
a fill can use both where a scalar call uses one.

There is no limit on how many values you may ask for — `v` can hold billions of elements. The one
precondition is on the draw axis rather than the array: **the last position, `draw + size(v) - 1`,
has to be representable in `integer(int64)`.** A fill whose final position is `huge(int64)` exactly
is fine and is tested; asking past that is asking for draws that cannot be named, and the values
you get back are not the ones the contract describes. Since `draw` defaults to 1, an ordinary call
is nowhere near this — it matters only if you are addressing the far end of a stream deliberately.

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

## When you don't know how many numbers you need: `pf_random_stream`

Everything above answers "what is the value at this coordinate?". Some programs cannot ask that,
because how many values they need depends on the data — a rejection sampler, a random walk, a
resample of unknown length. `pf_random_stream` carries the position for you and hands out
consecutive values of one stream:

```fortran
type(pf_random_stream) :: rng
real(real64) :: x

call rng%seed(seed, i)               ! stream i of this seed, at position 1
do
    call rng%uniform(x)              ! next value; keep going as long as you like
    if (x < 0.01_real64) exit
end do
```

**It is not a second generator, and it changes no value.** A freshly seeded stream's k-th
`%uniform` is exactly `pf_random_at(seed, i, k)`. The stream is a different way of reaching the same
grid, so a program can move between the two forms freely.

The producers, with what each costs in words (positions are counted in 32-bit words, 1-based):

| call | gives | words |
|---|---|---|
| `call rng%seed(seed [, stream])` | reseeds to position 1, in O(1); `stream` defaults to **0** | — |
| `call rng%uniform(x)` | `real64` in `[0, 1)` | 2 |
| `call rng%uniform32(x)` | `real32` in `[0, 1)` | 1 |
| `call rng%bits(b)` | 64 raw bits | 2 |
| `call rng%int_range(lo, hi, r)` | exactly-unbiased integer in `[lo, hi]` | 2 |
| `call rng%fill(v)` | the next `size(v)` values | 2 or 1 each |
| `call rng%fill(v, lo, hi)` | the next `size(v)` integers | 2 each |
| `call rng%rewind([pos])` | sets the position; no argument means 1 | — |
| `rng%position()` | the current position | — |

`stream` takes either integer kind and **defaults to 0**, not to 1 — unlike `draw`, which defaults
to 1. `call rng%seed(s)` is therefore stream 0 of that seed, and is the same sequence
`pf_random_at(s, 0)` names.

`%position` is the one that is a function, because it is the one that does not advance anything.
Everything that produces a value is a subroutine — deliberately, since `rng%uniform() - rng%uniform()`
as an expression would have a sign that depends on which the compiler evaluates first, and Fortran
does not fix that order.

`%int_range` starts on a word *pair* boundary — an integer draw names a pair at a fixed grid, the
same grid `%uniform` and `%bits` use — so from an odd position, which only `%uniform32` can leave
behind, it first advances one word. That is what keeps `rng%int_range(lo, hi, r)` equal to the
`pf_random_int_at` at the same coordinate rather than re-reading a word an earlier producer already
handed out. Positions above are exact for a stream that uses one producer throughout, which is the
ordinary case.

**Seed once per loop iteration, not once per program.** This is the discipline that keeps a stream
reproducible:

```fortran
!$omp parallel do schedule(dynamic)
do i = 1, n
    block
        type(pf_random_stream) :: rng     ! see the note below on why `block`, not `private`
        real(real64) :: x
        call rng%seed(seed, i)            ! O(1), no warm-up to pay for
        ...
    end block
end do
```

What can never be reproducible is one long-lived stream consumed *across* the iterations of a
dynamically scheduled loop: the value an iteration receives then depends on how many draws ran
before it, which depends on the schedule and the thread count. That is true of every stateful
generator, not just this one — and the answer to it here is that `%seed` costs nothing, so there is
no reason to share a stream between iterations.

**Declare a per-thread stream in a `block`, not in an OpenMP `private()` clause.** This is the
library-wide rule for any derived type used per-thread, and it applies here.

**For bulk work whose length you know in advance, use `pf_random_fill_draws` instead.** It walks
blocks rather than values and is comfortably faster than even a stream loop, which is itself
faster than a loop of scalar `pf_random_at` calls. The stream is for the case where the count is not known
in advance, not a general replacement for the fills.

A stream addresses 2⁶³ words and refuses to go past that, or before its first word — asking for a
position that does not exist stops the program rather than silently wrapping. No ordinary program is
anywhere near either bound.

## Permutations and subsets, addressed the same way

The same idea extends from draws to permutations, and it is what makes sampling at scale practical.
`pf_random_perm_at(seed, m, k)` is **element `k` of a permutation of `1 .. m`**, computed in constant
time and constant memory from its coordinates alone. It touches no array, so there is no shuffled
population to hold:

```fortran
integer(int64) :: k, who
do k = 1_int64, 10_int64
    who = pf_random_perm_at(20260817_int64, 1000000000000_int64, k)   ! a trillion-row population
    print *, who
end do
```

That loop draws ten distinct rows, without replacement, from a population of a trillion — in ten
constant-time calls. A shuffle-based method would need the trillion-element array first.

**The first `n` values are a uniform random `n`-subset**, so subsets need no separate machinery: ask
for `k = 1 .. n`. Three properties follow for free rather than by construction:

- **prefix consistency** — a size-3 subset is a prefix of a size-6 one from the same `(seed, m)`;
- **a subset at `n == m` *is* the permutation**, rather than merely agreeing with it;
- **order-independence** — element `k` depends on no other element, so a loop over `k` may run in
  any order, on any number of threads, and give the same answer.

`m` and `k` take `integer(int32)` or `integer(int64)` and the result follows them; `seed` is always
`integer(int64)`. `k` outside `[1, m]` is clamped rather than reported, since this is
`pure elemental` and has no way to abort.

### The bulk forms

```fortran
call pf_random_permutation(perm, seed [, threads])   ! perm(k) = pf_random_perm_at(seed, size(perm), k)
call pf_random_subset(idx, m, seed [, threads])      ! the first size(idx) of that same permutation
```

(Square brackets mark an optional argument; they are not part of the call.)

Both fill a rank-1 `integer(int32)` or `integer(int64)` array. They are an **identity, not a second
algorithm** — the bulk form returns exactly what the scalar form returns at the same coordinates, so
the two may be mixed freely. They are several times cheaper per element, because enumerating the domain
in order supplies a split the scalar form must divide to recover, and because the width rule and the
key schedule are derived once per call instead of once per element, which a `pure elemental` function
has no way to avoid.

`pf_random_subset` requires `size(idx) <= m` and `m >= 1`, and aborts rather than truncating; an
`integer(int32)` array additionally requires `m <= huge(int32)`, since an element may be any value in
`[1, m]`. A zero-sized array is a defined no-op and is not validated.

**What a subset costs, since it is easy to expect the wrong thing in either direction.** The work is
proportional to `size(idx)` and is **independent of `m`** — ten rows out of a trillion cost ten
elements' work, which is the whole point of the construction and is what a shuffle cannot do. But it
is also a *floor*: `size(idx)` elements cost `size(idx)` elements' work no matter how small the
fraction `size(idx)/m` is, so there is no regime in which asking for a subset gets cheaper per
element. Two smaller effects sit on top and neither is worth planning around: the permutation is
built on a Feistel network over a rectangle slightly larger than `m`, so a value landing outside
`[1, m]` is re-enciphered until it lands inside — bounded, averaging well under one extra round, and
invisible unless `m` is a near-worst-case shape — and the first element of a call pays a one-off key
schedule the rest of the call reuses. In practice a subset runs at a flat few nanoseconds per element
at every `m` from thousands to 10¹⁵, which is why the tables here quote one figure rather than a
curve.

`threads=` changes only how fast the array is filled. See
[Threads for a bulk permutation](../operating/settings.html#threads-for-a-bulk-permutation) for the
cap, the work floor and the measured scaling.

### Drawing WITH replacement: `pf_random_resample`

The third member of the family, and the one whose construction is not a construction at all:

```fortran
call pf_random_resample(idx, m, seed [, stream [, threads]])   ! draws from 1..m, with replacement
```

Drawing with replacement means `size(idx)` independent uniform integers in `[1, m]` — no dedup, no
permutation, no sort — so this **is** the draw-axis integer fill under a name that says what it is
for:

```fortran
call pf_random_resample(idx, m, seed, stream)
call pf_random_fill_draws(seed, stream, idx, 1_int64, m)   ! the same values, guaranteed
```

That identity is part of the contract and is asserted by the test suite. What the name buys is
speed a caller otherwise leaves on the table: without it the obvious code is a loop of
`pf_random_int_at`, which re-enciphers a block for every value where the bulk form serves two draws
from each one — worth roughly 1.5× for identical values.

`idx` is a rank-1 `integer(int32)` or `integer(int64)` array and `m` takes either kind. `stream` is
optional and defaults to 1; it selects **which replicate** this is, so replicate `b` is reproducible
from `(seed, b)` alone, whatever order the replicates ran in:

```fortran
do b = 1, n_replicates
    call pf_random_resample(idx, nrows, seed, b)    ! replicate b, reproducible on its own
    ! ... recompute your statistic over rows idx(:) ...
end do
```

Note the siblings have **no** `stream` argument: `pf_random_permutation` and `pf_random_subset` are
keyed by `(seed, m)` alone, so independent replicates of those come from `pf_random_key(seed, b)`
instead. A resample is built on the draw axis, which carries a stream coordinate already. Both
routes work here — `stream = b` and `seed = pf_random_key(seed, b)` are equally independent.

**A narrow population is drawn on a cheaper grid, and that is part of the frozen contract.** When
the range spans 2²⁴ values or fewer — which covers essentially every resample a table-oriented
program does — an integer draw takes a single 32-bit word rather than a 64-bit pair, so one
enciphering serves four values instead of two. It is still *exactly* unbiased: the rejection test is
the 32-bit analogue of the same rule, not an approximation. It is worth a little over 2× on the
bulk fill, and it applies to `pf_random_int_at` and both integer fills alike, so
every form still agrees value for value. A range wider than 2²⁴ takes the 64-bit pair instead. The
switch is a function of `m`, which you pass, so it is deterministic and identical on every machine —
it is **not** a setting and can never become one.

One consequence worth knowing if you mix generics: a narrow integer draw shares its word with
`pf_random32_at` at the same coordinate, where a wide one shares its pair with
`pf_random_at`/`pf_random_bits_at`. See
[the stride table](#mixing-generics-on-one-stream-the-stride-table), which is where that is set out
in full — including why walking the draw axis does not separate a narrow integer from a real.

**There is deliberately no `size(idx) <= m` requirement**, which is the clearest statement of how
this differs from `pf_random_subset`. Drawing 4000 values from a population of 4000 is the ordinary
bootstrap, and drawing more than `m` is perfectly meaningful. Two preconditions do apply, and both
abort rather than truncating: `m >= 1`, and — for an `integer(int32)` array — `m <= huge(int32)`.
A zero-sized array is a defined no-op and is not validated.

**`threads=` works here too, and it is bit-identical at every thread count** — same default from
`parquet_set_random_threads`, same work floor from `parquet_set_random_parallel_min_elements`, and
the floor applies to an explicit request as well, so a small resample stays serial however many
workers you ask for. Threading splits the draws between workers, and element `k` depends only on
`(seed, stream, k)`, so which worker produced it cannot matter:

```fortran
call pf_random_resample(idx, nrows, seed, b, threads=8)   ! same values as threads=1
```

**One wart, and it is a language constraint rather than a choice: `threads=` requires an explicit
`stream`.** Both are integers in the same argument position, so a generic offering them as
alternatives there does not compile at all. Pass `stream = 1` if you only want the default
replicate — `call pf_random_resample(idx, m, seed, 1, threads=8)`. Omitting it gives a "no specific
subroutine matches" error that does not explain itself. The siblings are unaffected:
`pf_random_permutation` and `pf_random_subset` have no `stream`, so `threads=` is their fourth
argument.

Because it draws with replacement, expect duplicates: drawing `m` values from `1 .. m` leaves about
`m(1 - 1/e)`, roughly 63%, of the population represented. If you want distinct rows, you want
`pf_random_subset`.

### What the permutation is, and what it is not

**The construction is piecewise, and which half you get depends only on `m`.**

For **`m ≤ 20`** the permutation is drawn by its *rank*: one exactly uniform integer in `[0, m!)`,
then unranked into a permutation. Both steps are exact — `pf_random_int_at` is exactly uniform over
any range by construction, and unranking is a one-to-one map onto the `m!` permutations — so the
result is **exactly uniform over every one of the `m!` permutations**. 20 is where this stops
because `20!` is the last factorial that fits a 64-bit integer.

For **`m ≥ 21`** it is a sixteen-round Feistel network over `Z_a × Z_b` with cycle-walking, where
`a = ceil(sqrt(m))`, followed by a seed-driven parity correction. Exact uniformity is not available
to *any* construction with this signature there: `m!` passes 2⁶⁴ at `m = 21`, so a 64-bit seed
cannot index the possibilities. What is measured instead is that the result is indistinguishable
from a uniform permutation under an order-4 tuple statistic over the exact cell space, a parity test
over the alternating group, an all-cells chi-square at the sizes it can be forced down to, and
fixed-point counts, cycle structure, position uniformity, subset membership and a structural test
asking whether sharing an input coordinate makes two outputs share one.

`pf_random_perm_algorithm` names that whole contract — `feistel-mix2-16p/zaxzb/exact20/v2` —
**separately from `pf_random_algorithm`**, so a program that recorded the draw contract is not told
its draws changed when only the permutation did. All four facts are in the string because all four
fix the answer: the round count, the parity correction (`p`), the width rule, and the threshold
below which the Feistel is not used at all.

**Consecutive `m` are independent.** A permutation of 5 and a permutation of 6 under the same seed
are unrelated, rather than two views of one underlying draw. That holds for their parity too, which
is less automatic than it sounds: at `m = 25, 49, 81, …` — an odd number squared — the network can
only ever produce an *even* permutation, so the parity is decided entirely by the seed-driven
correction, and permutations of two such sizes would share it unless `m` were part of what decides
it. It is.

**What it costs.** A whole permutation is a few tens of nanoseconds per element on one thread and
falls to a small fraction of that once it is threaded, because element `k` depends on no other
element. The random-access form is several times more expensive per element, and that gap is the
price of statelessness: it re-derives the width rule and the whole key schedule on every call, which
the bulk form does once.
Where the coordinate addressing pays for itself is a *subset* — the first `n` elements of a
permutation of `m` cost `O(n)`, not `O(m)`, so drawing 1000 rows out of a billion does not touch
the other billion.

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
print *, pf_random_algorithm            ! philox4x32-10/v2
```

Its value changes if and only if some value the module can produce changes. Record it alongside a
seed if you need to be able to tell, years later, whether a stored result is still reproducible.
It covers more than the cipher: which words a generic reads is part of the contract too, so a
change confined to one generic's draw axis — leaving the cipher, the key and counter layout, the
word order, the rejection rule and the retry key untouched — still moves the string. That is what
makes it the one thing able to tell a stored result which mapping produced it.

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

**`pf_random_int_at` shares no such identity with anything**, and none should be assumed. Lemire's
reduction is a different function of the bits it reads, and a rejection moves it to a different key,
so it is never simply a rescaling of a draw you already have.

**Which bits it reads depends on the range**, and that is the thing to know before mixing it with
anything else. A range spanning 2²⁴ values or fewer takes a single 32-bit word — the same word
`pf_random32_at` takes at that draw — so four such values come out of one enciphering. A wider range
takes the 64-bit pair `pf_random_at` and `pf_random_bits_at` take, so two values come out of one.
[The stride table](#mixing-generics-on-one-stream-the-stride-table) below gives both.

**So do not expect two values that read the same words to be independent.** "No identity" means the
two *values* are not equal; it does not mean they are unrelated. Reading the same words makes them
the same randomness twice, and a rejection is what would separate them — but at any realistic range
that happens with probability around 2⁻⁴⁰, so in practice it never does.

Which pairing that catches depends on the range, so it is worth being concrete. A narrow integer at
draw 2 reads word 1, the high word of `pf_random_at`'s first pair, and is then simply a function of
it: `pf_random_int_at(seed, i, 1, 6, 2_int64)` equals `1 + floor(6 * pf_random_at(seed, i))` for
20000 of 20000 streams measured. At the *same* coordinate the two agree only by chance (3325 of
20000, against 3333 expected for a die), because draw 1 of the narrow integer is word 0 — the low
half of that pair, which the top-53-bit real barely uses. Neither is a pairing to rely on.

### Mixing generics on one stream: the stride table

The same-coordinate case above is one instance of a general rule, and the rule is worth knowing in
full, because the four generics do not all walk the same grid.

**One `(seed, i)` pair names one sequence of 32-bit words. The coordinate-addressed generics are
*views* of that one sequence, and there are two strides:**

| generic | words read for draw `d` (1-based) | stride |
|---|---|---|
| `pf_random32_at(seed, i, d)` | `d-1` | 1 |
| `pf_random_int_at(seed, i, lo, hi, d)`, range of 2²⁴ values or fewer | `d-1` | 1 |
| `pf_random_at(seed, i, d)`, `pf_random_bits_at(seed, i, d)` | `2d-2`, `2d-1` | 2 |
| `pf_random_int_at(seed, i, lo, hi, d)`, wider range | `2d-2`, `2d-1` | 2 |

Two rules follow, and between them they are the whole story:

1. **`pf_random_at` and `pf_random_bits_at` agree on what draw `d` means.** At one coordinate they
   are two presentations of the same 64 bits; at different coordinates they are independent. A wide
   `pf_random_int_at` reads that same pair.
2. **`pf_random32_at` has its own finer grid**, one word per value, and is deliberately not a
   narrowing of `pf_random_at`. Its draws `2d-1` and `2d` are the two halves of 64-bit draw `d`.
   **A narrow `pf_random_int_at` walks that same one-word grid** — which is every range of 2²⁴
   values or fewer, so it is the ordinary case rather than the exception. Either one mixed with a
   64-bit generic on one stream aliases across draw indices:

```
pf_random32_at(seed, i, 2d-1)  is the low word of  pf_random_bits_at(seed, i, d)
```

In practice:

```fortran
! SAFE -- different draws of the two 64-bit generics are independent
x    = pf_random_at(seed, i, 1_int64)               ! words 0,1
b    = pf_random_bits_at(seed, i, 2_int64)          ! words 2,3

! COLLIDES -- a range of 6 is narrow, so this walks the one-word grid
x    = pf_random_at(seed, i, 1_int64)               ! words 0,1
die  = pf_random_int_at(seed, i, 1, 6, 2_int64)     ! word  1  <-- half of `x`'s randomness

! COLLIDES -- pf_random32_at is on that same finer grid
y    = pf_random_at(seed, i, 2_int64)               ! words 2,3
z    = pf_random32_at(seed, i, 3_int64)             ! word  2  <-- half of `y`'s randomness

! SAFE -- word 2 onwards is untouched by `x`
x    = pf_random_at(seed, i, 1_int64)               ! words 0,1
die  = pf_random_int_at(seed, i, 1, 6, 3_int64)     ! word  2
```

The failure is silent when it happens. The values are not *equal* — the two generics scale their
words differently — so every structural check passes and only a distributional test can see it.
That is why the safest habit remains the one in the next section: give each role its own stream.

**Walking the draw axis is therefore not enough on its own.** It separates the two 64-bit generics,
and it does not separate a narrow integer from either of them — draw `d` of a narrow integer is
word `d-1`, which belongs to 64-bit draw `(d+1)/2`. Separate the *streams* or the *seeds* instead,
as below.

### Three constructions that are safe

**Use a separate stream.** Two streams never overlap, whatever draw indices you use on each:

```fortran
do i = 1, n
    x(i)   = pf_random_at(seed, 2*i)                      ! one stream for the reals
    die(i) = pf_random_int_at(seed, 2*i + 1, 1, 6)        ! another for the integers
end do
```

**Use a separate family.** `pf_random_key(seed, label)` gives each role its own seed, which is the
clearest choice when the roles are named things rather than numbered ones:

```fortran
integer(int64) :: seed_pos, seed_die
seed_pos = pf_random_key(seed, 1_int64)
seed_die = pf_random_key(seed, 2_int64)
```

**Or use `pf_random_stream`, which advances the cursor for you.** A stream keeps its own word
position, so consecutive calls consume consecutive words and you never have to work out a draw
index:

```fortran
type(pf_random_stream) :: rng
call rng%seed(seed, i)
call rng%uniform(x)                 ! consumes 2 words
call rng%int_range(1, 6, die)       ! consumes the next 2
```

**One caveat, and it is the narrow-integer rule again.** `%int_range` advances the cursor by a word
pair, but over a range of 2²⁴ values or fewer it *reads* a single word — the first of that pair. So
after a `%uniform` the die above comes from the second word of the pair `x` was built from, and is a
function of `x`: measured 20000 of 20000 at a range of 6. A stream separates repeated calls to the
*same* producer, which is the ordinary case; it does not by itself separate a narrow `%int_range`
from a `%uniform` beside it. Where that matters, use a separate stream or a separate family, both of
which are unconditional.

Otherwise this is what a stream is *for*: the coordinate-addressed forms are the right tool when you
know which value you want, and a stream is the right tool when you are consuming several per step.

### Why the remaining overlap is not simply removed

Folding a per-generic constant into the key would make the overlap impossible by construction. It
would also break the property `pf_random_stream` exists to provide — that a stream hands out exactly
the values the coordinate-addressed calls give at the same positions — because that correspondence
is possible only while every generic reads one word space.

So the overlap is the price of that correspondence, and what remains of it is one thing rather than
several: **a one-word grid and a two-word grid over the same words.** `pf_random32_at` and a narrow
`pf_random_int_at` are on the first, `pf_random_at`, `pf_random_bits_at` and a wide
`pf_random_int_at` on the second. Two values from the same grid at different draws never overlap;
two from different grids can, and the table above says exactly when.

### Not cryptographic

The generator is fast and statistically excellent, and it is **not** cryptographic. The seed is
recoverable from a handful of outputs. Do not use it for keys, tokens, passwords, nonces, or
anything else an adversary must not be able to predict — take those from a CSPRNG (your platform's
`getrandom`, `/dev/urandom`, or a library built for the purpose). `pf_random_seed()` is not one
either: it folds the clock and a call counter, both of which are guessable.

## Performance

**This module is not here to be faster than `random_number`, and on some machines it is not.**
A scalar `pf_random_at` and a scalar `random_number` call are within a factor of about two of each
other under the flags fpm actually passes, and **which one is ahead depends on your compiler** —
this library loses on one of the two mainstream Fortran compilers and wins on the other. Nothing
about that is stable enough to plan around, which is why no figure is quoted here.

What `random_number` cannot do at any speed is give you the same answer under a different OpenMP
schedule. That is what you are buying.

**If the ratio matters to you, measure it on your own machine rather than trusting anyone's
number.** `tools/benchmark_random.sh` in this repository drives exactly that comparison — bulk and
scalar, `real64` and `real32` — against the intrinsic. It is a maintainer tool rather than part of
the installed package, so run it from a checkout.

Two notes if you do. Quote figures from `--profile release` only; and note that
"unoptimised" is a different profile per compiler — the default profile is the slow one for
gfortran and flang, while for ifx it is `--profile debug`, since ifx optimises at `-O2` by default
and fpm passes it no `-O` in either of the other two.

## A note for the curious: two multiply paths

Three multiplies inside the module produce results too wide for a signed 64-bit integer: the
cipher's inner loop, which multiplies two 32-bit values into a 64-bit result; the full 128-bit
product behind every integer draw; and the one behind key derivation. Where the compiler offers a
128-bit integer kind — gfortran and flang do — the module forms each of them in it, which makes
them provably in range. Where there is no such kind — ifx — it computes them on narrower pieces,
or accepts a wrapping product under an assumption verified on that compiler rather than assumed.

**Both paths produce identical values**, which the suite asserts on every machine it runs on, and
`pf_random_algorithm` is the same either way. The choice is made at compile time from the compiler
in use; there is nothing to configure, and deliberately no way to override it.

## Distributions

Four distributions are built on the uniforms: **exponential**, **normal**, **Gamma** and
**Poisson**. Every one of them is addressed exactly as the uniforms are — a coordinate, or a walk
along a stream — and every one has its own frozen contract identifier, so a program recording
`pf_random_algorithm` is not told its uniform draws moved because a Ziggurat layer count changed.

### The promise table

This is the single source of truth for what each name promises. Read it before choosing between two
that look interchangeable, because two of them are not.

| procedure | reproducibility | bulk == stream walk? |
|---|---|---|
| `%exp`, `pf_random_exp_at`, `pf_random_fill_exp` | given libm | **yes**, to a couple of ulp — see below |
| `%exp_portable`, `pf_random_exp_portable_at`, `pf_random_fill_exp_portable` | **every platform** | **yes**, exactly |
| `%normal`, `pf_random_normal_at`, `pf_random_fill_normal` | given libm | **no** — see below |
| `%normal_portable`, `pf_random_normal_portable_at`, `pf_random_fill_normal_portable` | **every platform** | **no** |
| `%gamma(shape)` | given libm | n/a (no bulk form) |
| `%poisson(lambda)` | given libm | n/a (no bulk form) |

Two things that table is saying, and both matter more than they look:

**"Given libm" versus "every platform".** Everything in this module is *deterministic* — a value is
a pure function of its coordinates, on any machine, always. What differs is whether the last bit
survives a change of C library: a draw that reaches `log` or `exp` is bit-identical only for a given
libm, because libm is not part of any contract this project controls. The `_portable` forms avoid
that by routing through a frozen logarithm built from IEEE arithmetic alone, which no compiler may
reorder. That is a choice about **reproducibility across machines**, never about accuracy — the two
forms agree to about two units in the last place.

**A name without `_portable` is the default realisation, and every default is "given libm".** The
suffix's absence says only "this is the default", not what promise the default carries — `%gamma`
and `%poisson` have no portable twin at all.

### The exponential

`Exp(1)`, mean 1, in `[0, 36.7368]`. Multiply by a mean to rescale, or divide by a rate.

```fortran
x = pf_random_exp_at(seed, i)            ! value 1 of stream i
x = pf_random_exp_at(seed, i, 7_int64)   ! value 7
call rng%exp(x)                          ! the next one along a stream
call pf_random_fill_exp(seed, i, v)      ! v(k) is value k
```

The mapping is `-log(1 - u)` where `u` is exactly `pf_random_at(seed, i, draw)`. **`1 - u`, not `u`,
is contract**: a uniform can be exactly 0 and can never be 1, so `-log(u)` would be infinite once in
2⁵³ draws while this is finite always.

**It is the one distribution whose three tiers are the same value computed the same way**, because
it consumes a fixed two words. Everything else here is a rejection algorithm — see [Why the normal's
bulk form differs from its stream walk](#why-the-normals-bulk-form-differs-from-its-stream-walk).

One caveat, and it is the libm promise showing up inside a single program rather than across
machines: a compiler may serve `log` from a **vector** libm inside the bulk fill's loop and a scalar
one in the elemental call, and the two need not agree in the last bit. gfortran gives identical
values on all three tiers; ifx at its default `-fp-model=fast` differs on about a third of them, by
at most 2 ulp. `pf_random_exp_portable_at` has no such exposure — its logarithm is this library's
own — and its three tiers agree exactly everywhere.

**What is exact on every tier and every compiler is that a fill splits.** `v(1:3)` filled alone is
the first three of `v(1:6)`, at any chunking, on any number of threads. That is the promise the
module exists for, and it is unaffected.

**`pf_random_exp_portable_at` costs about 3x**, on a bulk fill and scalar alike. The frozen
logarithm is
twelve barriered Horner steps, each a store and a reload, so it neither vectorises nor pipelines
where a libm `log` does both. Reach for it when a stored result must reproduce across machines.

### The normal

Standard normal, mean 0 and variance 1.

```fortran
x = pf_random_normal_at(seed, i)                   ! Ziggurat; fast
x = pf_random_normal_portable_at(seed, i)          ! polar method; identical everywhere
call rng%normal(x)
call rng%normal_portable(x)
call pf_random_fill_normal(seed, i, v)
call pf_random_fill_normal_portable(seed, i, v)
```

The default is a **Ziggurat** over 256 equal-area layers, which accepts 98.5 % of candidates after a
single 64-bit read and falls back to a wedge test or a tail walk otherwise. The portable form is
**Marsaglia's polar method**, which needs only a logarithm and a square root — the first frozen, the
second required by IEEE to be correctly rounded.

The two are **independent** at the same coordinate, not two views of one draw: they derive separate
sub-streams. That is deliberate, so a program using both is not quietly correlating them.

### Why the normal's bulk form differs from its stream walk

This is the one surprise in the module, and it is forced rather than chosen.

A rejection algorithm consumes a number of words that depends on the values it drew. So nobody —
not this library, not you — can say where value `k` of a stream walk begins without having drawn the
preceding `k-1`. A bulk fill that walked a stream could therefore not be split at any boundary, by
anyone, at any speed.

The coordinate-addressed forms sidestep it by giving each `(i, draw)` its own derived sub-stream, so
every value is a pure function of its coordinates and a fill splits anywhere:

```fortran
!$omp parallel do
do chunk = 1, nchunk
    call pf_random_fill_normal(seed, i, v(lo(chunk):hi(chunk)), draw=lo(chunk))
end do
```

That gives the same answer at any thread count and any chunking, which is the module's headline
property applied to the most-used distribution. **The cost is that a loop of `%normal` and a
`pf_random_fill_normal` over one seed give different numbers.** Both are correct draws from the same
distribution; they are different realisations, exactly as `pf_weighted_draw` and
`pf_weighted_permutation` are. The promise table above says which procedures this applies to, and it
is a user obligation to take it into account.

The exponential is exempt because its consumption is fixed, which is why its row reads "yes".

### Gamma and Poisson

Both are stream-only: there is no `pf_random_gamma_at`, no bulk fill, and no `_portable` twin.

```fortran
call rng%gamma(shape, x)        ! Gamma(shape, 1) -- multiply by theta for another scale
call rng%poisson(lambda, k)     ! k is integer(int32) or integer(int64)
```

**`%gamma`** takes any `shape > 0`. The scale is 1, so `Gamma(a, theta)` is `theta * rng%gamma(a)`;
the mean and the variance are both `shape`. Below `shape = 1` the draw goes through the boost
`Gamma(a) = Gamma(a+1) * u**(1/a)`, which costs one extra uniform and a `pow` — and which is why
there is deliberately **no `%gamma_portable`**: `pow` is not frozen, so such a procedure could not
keep the promise its name would imply.

**`%poisson`** takes any `lambda >= 0` (0 always draws 0), up to about 5.8e17 — past that a drawn
count could overflow `integer(int64)`, so it aborts rather than return one, which is far beyond
where a Poisson draw means anything. Below `lambda = 10` it uses Knuth's
product of uniforms, which is exact and terminates unconditionally; at or above it, transformed
rejection, whose cost does not grow with `lambda`. **The crossover is frozen contract**, published
through `pf_poisson_algorithm`, because it decides which value comes back — it is not a tuning knob
and there is no setting for it. The `int32` result refuses a count that does not fit rather than
narrowing it; pass an `integer(int64)` for a `lambda` that large.

Both refuse a parameter outside their domain — a non-positive shape, a negative mean, a NaN in
either — rather than returning something plausible.

### The frozen contract identifiers

One per distribution family, separate from `pf_random_algorithm` and from each other:

| identifier | covers |
|---|---|
| `pf_exp_algorithm` | both exponential realisations: the mapping, the `1 - u` convention, the two-word cost, and which logarithm each form uses |
| `pf_normal_algorithm` | both normal realisations: the layer count and table construction, the polar variant, both rejection loops' draw order and cost, and the sub-stream labels |
| `pf_gamma_algorithm` | the Marsaglia–Tsang variant, the `shape < 1` boost, **and which normal the inner loop consumes** |
| `pf_poisson_algorithm` | both algorithms, the crossover `lambda`, and each one's draw order |

One more lives outside this module, and is listed here because a program recording contracts wants
all of them in one place: **`parquet_sample_algorithm`** (from `parquet_core`, so `use parquet_io` or
`use parquet` reaches it) names the row mapping `parquet_open_reader(..., sample_fraction=)` uses.
That draw is composed from `pf_random_key` and `pf_random_at` — the reader has no generator of its
own — so `pf_random_algorithm` moving would move it too, which is why it has its own string. See
[Random downsampling](../io/filter-sort-sample.html#which-rows-a-seed-selects) for the mapping.

A family's identifier names **both** of its realisations where it has two: a program recording one
string wants to know whether either form moved.

`pf_gamma_algorithm` names the normal it consumes because a change to the normal changes every Gamma
value — a program recording only Gamma's identifier would otherwise miss it.

### What a variable cost means for `%position`

`%position` stays exact for every producer, including the rejection-based ones: it reports where the
stream *is*. What stops being possible is **predicting** it. A fixed-cost producer lets you compute
where a stream will be after a known sequence of draws (`%uniform` 2 words, `%uniform32` 1, `%bits`
2, `%int_range` 2, `%exp` 2, all of the last three pair-aligned); `%normal`, `%normal_portable`,
`%gamma` and `%poisson` do not.

So checkpoint and restart — save `%position()`, `%rewind` to it later — keeps working for every
producer. Arithmetic on positions does not.

## Weighted draws without replacement

Sometimes items are not equally likely. `pf_weighted_draw` and `pf_weighted_permutation` draw
without replacement from weights: the first draw is proportional to weight, and each later one is
proportional among whatever is left.

```fortran
type(pf_weighted_draw) :: d
integer :: item
logical :: ok

call d%init(weights, seed)
do
    call d%next(item, ok)
    if (.not. ok) exit          ! every item has been drawn
    ! ... judge item; exit on acceptance ...
end do
```

`%init(weights, seed [, stream])` prepares the sampler; `%remaining()` reports how many items are
left, `%reset()` restarts the same sequence, and `%reseed(seed [, stream])` starts a different one
over the same weights. In signatures written out in prose here, square brackets mark an optional
argument. **`stream` is optional on both `%init` and `%reseed`, takes either integer kind, and
defaults to 0** — it names which sequence of that seed you want, so `%init(w, s, 3_int64)` and
`%init(w, s)` followed by `%reseed(s, 3_int64)` give the same draws.

**What the sampler refuses.** All of these abort rather than returning something plausible, so a
program that runs has not silently skipped one:

- **`%init` twice on the same sampler** — use `%reseed` for a new sequence over the same weights,
  or `%reset` to repeat the current one. This is the one that surprises, because `%reset` and
  `%reseed` make a second `%init` look like the obvious way to start over.
- **any other entry point before `%init`** — an uninitialised sampler has no weights to draw from.
- **a weight that is NaN, infinite, or negative.** A zero weight is fine and is described under
  [Zero weights](#zero-weights); those three are not.
- **every weight zero** — there is no distribution to draw from.
- **a population above `huge(int32)`** when the item is an `integer(int32)`; pass an
  `integer(int64)` item.

`pf_weighted_subset` and `pf_weighted_permutation` add one each: the first refuses
`size(idx) > size(weights)`, and the second requires `size(perm) == size(weights)` exactly.

### This is successive sampling, not proportional-to-size inclusion

The distinction catches people out, so it is worth stating before anything else. An item with twice
the weight is twice as likely to be drawn *first*. It is **not** twice as likely to appear somewhere
in the first `k`, and no construction with this interface can make it so. What you get is the
Plackett-Luce order; drawn to exhaustion it is the weighted shuffle. If you need exact
inclusion-probability-proportional-to-size, this is not it, and the right tool is a different
algorithm entirely.

### Two families, one distribution, different realizations

There are two constructions, and which one you want depends on how you consume the result. Each has
a bulk form:

```fortran
call pf_weighted_subset(idx, weights, seed [, stream])                ! the first size(idx) drawn
call pf_weighted_permutation(perm, weights, seed [, stream] [, threads])   ! the whole shuffle
```

`idx` and `perm` are rank-1 `integer(int32)` or `integer(int64)` arrays; `weights` is
`real(real64)`, one per item, and it carries the population — note this differs from
`pf_random_subset`, whose second argument is the population size. `pf_weighted_subset` has no
`threads=`, because the tree family is serial by nature.

**`pf_weighted_permutation`'s `stream` must be `integer(int64)`** — write `stream=int(j, int64)` in
a loop. This is the same argument-position wart as
[`pf_random_resample`'s](#drawing-with-replacement-pf_random_resample): an `int32` `stream` and an
`int32` `threads` are indistinguishable as a positional fourth argument, so the generic offers only
the wide kind. `pf_weighted_draw`'s `%init`/`%reseed` and `pf_weighted_subset` take either kind.

- **`pf_weighted_draw` and `pf_weighted_subset`** are backed by a segment tree. `%init` is `O(n)`,
  `%next` is `O(log n)`, and `%reset`/`%reseed` are `O(k log n)` in the draws already taken. Serial
  by nature, and there is no `threads=`.
- **`pf_weighted_permutation`** is backed by an exponential race: every item gets a key and the keys
  are sorted. `O(n)` plus a sort, parallel, and bit-identical at every thread count.

**The two give different sequences from the same seed**, and that is not a defect to be tidied away.
Both draw from successive sampling; for one seed they are two different draws from it, in the same
way two different seeds would be. So there is no prefix identity across the families — a
`pf_weighted_permutation` does not begin with what a `pf_weighted_draw` would have drawn.

Within the sequential family the identity does hold, and it is the useful one:
`pf_weighted_subset(idx, weights, seed)` **is** `size(idx)` calls to `%next`, so taking `k` and
stopping equals asking for `k`, and a subset of size `k` is a prefix of one of size `2k`.

Which to reach for: `pf_weighted_draw` when you want a few draws, or many short sequences over the
same weights; `pf_weighted_permutation` when you want whole shuffles. The gap is not small — for
many short sequences over a large population the tree is thousands of times cheaper, because it is
built from the weights alone and a new sequence only has to undo the draws already made, while the
race must rebuild every key from the new seed.

### Zero weights

A zero-weight item can never be drawn while any positive weight remains, so it lands at the end —
in **uniform random order**, on both paths. That keeps a drained sampler a genuine permutation of
every item rather than a truncated list. A zero weight is not an error; a negative one is.

"Zero-weight" means **a weight below `2.05e-307`**, not exactly zero. Below that bound the key
`-log(u)/w` would overflow, so such a weight cannot be ordered at all — and against weights of
order one its chance of being drawn is of order `10**-307`, which no finite sample distinguishes
from zero. The threshold is also what keeps the answer independent of your build's floating-point
model: a build that flushes denormals to zero (`-ffast-math`, or ifx at its defaults) would
otherwise classify the same weight differently from an IEEE build.

### Running many sequences at once

`%next` mutates the sampler's tree, so **one sampler cannot serve two threads**. Give each thread
its own, and build them inside a parallel region — the builds are independent, so `nt` of them cost
the wall-clock of one:

```fortran
type(pf_weighted_draw), allocatable :: dd(:)
integer :: nt, tid, j

nt = 1
!$ nt = omp_get_max_threads()
allocate(dd(nt))
!$omp parallel do default(shared) private(tid)
do tid = 1, nt
    call dd(tid)%init(weights, seed)
end do

!$omp parallel do default(shared) private(tid, item, ok) schedule(dynamic)
do j = 1, n_outer
    tid = 1
    !$ tid = omp_get_thread_num() + 1
    call dd(tid)%reseed(seed, stream=int(j, int64))
    do
        call dd(tid)%next(item, ok)
        if (.not. ok) exit
    end do
end do
```

Because each sequence is named by its own `(seed, stream)`, the answer does not depend on the
schedule or the thread count — `schedule(dynamic)` above costs nothing in reproducibility.

**Never put a `pf_weighted_draw` in an OpenMP `private()` clause.** A private copy is a *fresh*
object, not a copy of yours: on both gfortran and ifx the tree comes back allocated to the right
shape with uninitialised contents and every scalar reset to its default. Nothing aborts, every
drawn item still looks valid, and the answers are simply wrong. The per-thread array above avoids
the privatisation machinery entirely; `firstprivate()` copies correctly on both compilers if you
prefer it.

For many whole shuffles, parallelise **across** them rather than within one: the race's key loop is
memory-bound and scales sub-linearly, whereas independent shuffles are perfectly parallel. An
OpenMP loop over sequences, each calling `pf_weighted_permutation` with `threads=1`, beats a serial
loop over threaded calls. Nothing special is needed — the auto-threading rule already resolves to
serial inside an active parallel region.

### Reproducibility, and one build that is refused

The race turns a uniform into an exponential key with `-log(u)`, and `log` from the runtime library
is **not** reproducible: measured over two million inputs, 1.33% of values differ between gfortran
and ifx at their default flags, and 0.083% still differ under strict floating-point settings. One
differing key changes which items are drawn whenever it crosses its neighbour. So the transform is
computed inside the library from IEEE `+ - * /` only, which are correctly rounded everywhere.

Two build settings defeat that, because they license the compiler to abandon IEEE semantics
altogether: `-ffast-math` / `-Ofast` on gfortran, and ifx's default `-fp-model=fast`. The second is
the one to know about — `fpm --profile release` and `--profile debug` both pass `-fp-model=precise`
and are fine, but a bare `fpm build` passes no floating-point flag at all. Rather than let such a
build return quietly different permutations, `pf_weighted_permutation` checks on every call that it
reproduces the frozen transform, and aborts naming the cause if it does not.

## See also

- [Thread safety](../operating/thread-safety.html) — why this module needs no locking at all.
- [Sorting arrays and columns](sorting.html) — `pf_sort` and friends, the other `pf_`-prefixed
  utility API.
