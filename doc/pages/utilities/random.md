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
- **`pf_random_fill_draws(seed, i, v [, draw])`** — fills a rank-1 `real64` or `real32` array with
  consecutive draws of **one** stream.
- **`pf_random_fill_streams(seed, i0, v [, draw])`** — fills a rank-1 `real64` or `real32` array
  with **one** draw of each of consecutive streams.
- **`pf_random_seed()`** — a fresh, nondeterministic seed.

All the scalar draws are `pure elemental`, so they accept conformable arrays as well as scalars.
For bulk work, prefer the scalar call inside your own loop or one of the two fills; a whole-array
elemental call over a constructed index array is much slower than any of them.

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
worthwhile saving over the scalar loop it replaces (measured about 1.3× on gfortran and 1.5× on
ifx), while the draw-axis fill is roughly twice as fast again per value. If you need several values
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

One asymmetry to expect: **the integer draw-axis fill saves the per-element call and nothing more.**
An integer draw consumes a whole enciphering on its own — the exact-rejection rule needs a full
64-bit candidate and takes its own block — so consecutive integer draws cannot share a block the way
two consecutive `real64` values do. The stream-axis integer fill gives up nothing at all by
comparison, since that axis already spent one enciphering per value.

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
| `call rng%seed(seed [, stream])` | reseeds to position 1, in O(1) | — |
| `call rng%uniform(x)` | `real64` in `[0, 1)` | 2 |
| `call rng%uniform32(x)` | `real32` in `[0, 1)` | 1 |
| `call rng%bits(b)` | 64 raw bits | 2 |
| `call rng%int_range(lo, hi, r)` | exactly-unbiased integer in `[lo, hi]` | 4 |
| `call rng%fill(v)` | the next `size(v)` values | 2, 1 or 4 each |
| `call rng%fill(v, lo, hi)` | the next `size(v)` integers | 4 each |
| `call rng%jump(n)` | seeks `n` words, in O(1); negative seeks back | — |
| `call rng%rewind([pos])` | sets the position; no argument means 1 | — |
| `rng%position()` | the current position | — |

`%position` is the one that is a function, because it is the one that does not advance anything.
Everything that produces a value is a subroutine — deliberately, since `rng%uniform() - rng%uniform()`
as an expression would have a sign that depends on which the compiler evaluates first, and Fortran
does not fix that order.

`%int_range` starts on a block boundary — the generator produces four words at a time, and an
integer draw needs a whole group of four — so from an unaligned position it first advances to the
next boundary. That is what keeps `rng%int_range(lo, hi, r)` equal to the `pf_random_int_at` at the
same coordinate rather than re-reading words an earlier `%uniform` already handed out. Positions
above are exact for a stream that uses one producer throughout, which is the ordinary case.

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
blocks rather than values and is about 1.7× faster than even a stream loop, which is itself faster
than a loop of scalar `pf_random_at` calls. The stream is for the case where the count is not known
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
the two may be mixed freely. They are about 2.4x cheaper per element, because enumerating the domain
in order supplies a split the scalar form must divide to recover, and because the width rule and the
key schedule are derived once per call instead of once per element, which a `pure elemental` function
has no way to avoid.

`pf_random_subset` requires `size(idx) <= m` and `m >= 1`, and aborts rather than truncating; an
`integer(int32)` array additionally requires `m <= huge(int32)`, since an element may be any value in
`[1, m]`. A zero-sized array is a defined no-op and is not validated.

`threads=` changes only how fast the array is filled. See
[Threads for a bulk permutation](../operating/settings.html#threads-for-a-bulk-permutation) for the
cap, the work floor and the measured scaling.

### What the permutation is, and what it is not

The construction is a four-round Feistel network over `Z_a × Z_b` with cycle-walking, where
`a = ceil(sqrt(m))`. `pf_random_perm_algorithm` names that contract — `feistel-mix2-4/zaxzb/v1` —
**separately from `pf_random_algorithm`**, so a program that recorded the draw contract is not told
its draws changed when only the permutation did.

**It is not uniform over all `m!` permutations, and nothing with a 64-bit seed could be**: `m!`
passes 2⁶⁴ at `m = 21`, so a 64-bit seed cannot even index the possibilities. What is measured is
that it is indistinguishable from a uniform permutation under fixed-point counts, cycle structure,
position uniformity, subset membership, and a structural test that asks whether sharing an input
coordinate makes two outputs share one. The round count of four is where that last test puts the
boundary: three rounds leak detectably in every replicate, and six buy nothing measurable.

If you need a uniform shuffle of a small array with a large seed space, this is not that tool.

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

**So do not take a real and an integer at the same coordinate and expect two independent numbers.**
"No identity" means the two *values* are not equal; it does not mean they are unrelated. Reading
the same words makes them the same randomness twice, and a rejection is what would separate them —
but at any realistic range that happens with probability around 2⁻⁴⁰, so in practice it never does.
At a small range the integer is simply a function of the real: `pf_random_int_at(seed, i, 1, 6)`
equals `1 + floor(6 * pf_random_at(seed, i))` for 20000 of 20000 streams measured, where taking the
integer one draw along gives the 1-in-6 agreement independence predicts.

The fix is the one this page already recommends for any two values in one iteration — walk the draw
axis, or derive a separate family:

```fortran
do i = 1, n
    x(i)   = pf_random_at(seed, i)                       ! draw 1
    die(i) = pf_random_int_at(seed, i, 1, 6, 2_int64)    ! draw 2: independent of x(i)
end do
```

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

Three multiplies inside the module produce results too wide for a signed 64-bit integer: the
cipher's inner loop, which multiplies two 32-bit values into a 64-bit result; the full 128-bit
product behind every integer draw; and the one behind key derivation. Where the compiler offers a
128-bit integer kind — gfortran and flang do — the module forms each of them in it, which makes
them provably in range. Where there is no such kind — ifx — it computes them on narrower pieces,
or accepts a wrapping product under an assumption verified on that compiler rather than assumed.

**Both paths produce identical values**, which the suite asserts on every machine it runs on, and
`pf_random_algorithm` is the same either way. The choice is made at compile time from the compiler
in use; there is nothing to configure, and deliberately no way to override it.

## See also

- [Thread safety](../operating/thread-safety.html) — why this module needs no locking at all.
- [Sorting arrays and columns](sorting.html) — `pf_sort` and friends, the other `pf_`-prefixed
  utility API.
