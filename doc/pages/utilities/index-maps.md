---
title: Key-to-index lookup with parquet_index
---

`parquet_index` answers the two halves of "where does this thing live in my arrays?".

`pf_index_map` answers it for a key you already have: give it the key column of a table and it
tells you, in a few nanoseconds, which row a key sits in. `pf_index_pool` answers it when nothing
has a key yet: it issues 1, 2, 3, … on request and takes them back, so a program managing slots in
its own arrays never has to track which are in use.

Both are ordinary Fortran containers over plain integer arrays. Nothing here reads or writes a
Parquet file, and `use parquet_index` never reaches the C++ bindings — see
[Choosing a module](../operating/choosing-a-module.html) for what that does and does not buy you.

## Stored values are index values, and 0 means "not found"

**Every value a map stores is an integer >= 1, and every lookup answers 0 when the key is
absent.** That is the whole error protocol: there is no `found` flag to thread through a hot loop.

```fortran
use parquet_index
type(pf_index_map) :: m
integer(int64) :: j

call m%build(object_id)          ! object_id(:) is your key column
j = m%get(42_int64)
if (j > 0) then
    ! row j of your arrays is the one
end if
```

Storing a value below 1 aborts. If you genuinely need to store 0, store `value + 1`.

The contract is not arbitrary tidiness: it is what makes the hash table sentinel-free. A slot whose
value is 0 *is* an empty slot, so no key value is reserved, the full 64-bit key domain is
available, and there are no tombstones and no occupancy bitmap to keep in step with the table.

## Building a map

`%build` takes the keys you have and replaces whatever the map held. Rebuilding is allowed and
needs no special call — it releases and reconstructs.

```fortran
call m%build(keys)                      ! values default to 1..n, each key's own position
call m%build(keys, row_of)              ! or give a value per key, each >= 1
call m%build(keys, method="hash")        ! or force a backend; see below
call m%build(keys, valid=ok)            ! skip every row ok(:) marks .false.
```

Keys and values may be `integer(int32)` or `integer(int64)`, in any combination. **Every key must
be unique**; a duplicate aborts and names the offender. There is no "first wins" or "last wins"
option, because a map that silently answered for one of two rows would give you no way to find out
which.

`valid=` takes one logical per key and is the mask a nullable key column already carries. A
`.false.` row is neither stored nor counted — its key may even repeat a stored one — and the
values stay the **original row numbers** of the rows that were kept, so `m%get(key)` still answers
the row the key sits in and nothing has to be compacted first. It is the last argument, so an
existing positional call is unaffected.

For a map you fill as you go rather than in bulk, start it empty:

```fortran
call m%init()                           ! then %set or %get_or_add
call m%init(capacity=100000)            ! pre-sized, so a run of inserts does not rehash
```

## The three backends

One API, three storage layouts, and the module picks between them from your keys. `%get_method`
reports the choice.

| | direct | hash | sorted |
|---|---|---|---|
| lookup cost | two comparisons and one load | one probe, usually | `O(log n)` |
| memory | 8 bytes per slot over the key range | 16 bytes per slot, table 1.7-3.3x the keys | 16 bytes per key, exact |
| `%build` | yes | yes | single-component keys only |
| `%init` (fill as you go) | no | yes | no |
| composite keys | yes | yes | no |
| `%set` / `%get_or_add` | inside the built key range | yes | aborts |
| `%remove` | yes | yes | aborts |

The hash row is for a single-component key. A composite one stores its components alongside,
costing `8 * (ncomponents + 1)` bytes per slot — 24 for a pair, 32 for a triple — with the same
1.7-3.3x table size.

**direct** is an array indexed by the key: the fastest lookup there is, with no hashing and no
probing at all. It is chosen automatically when your keys are dense enough that the array is no
larger than the hash table would have been.

**hash** is open addressing with linear probing. The general case, and the only backend you can add
keys to one at a time.

**sorted** is sorted keys plus a binary search. The smallest footprint available — exactly 16 bytes
per key, with no slack — at logarithmic lookup cost. It is **opt-in**: the automatic choice never
selects it, and it is frozen once built, which is what buys the exact fit. Pass `method="sorted"`
when memory matters more than lookup speed.

### What the automatic choice does

`method="auto"` (the default) takes **direct** when the key range is no wider than the larger of
65536 slots and four slots per key, and **hash** otherwise. Up to that density the direct array
costs no more than the hash table and is strictly faster; past it the hash table wins on memory.

The decision is made from a scan of your keys **before anything is allocated**, so a key set like
`{1, 10**9}` goes to the hash table rather than asking for 8 GB. Two keys a billion apart is a hash
map; a hundred consecutive keys is a direct one.

You can always override it. `method="direct"` on a wide key range will try to allocate the array
and tell you how many slots that needed if it cannot.

## Looking things up

```fortran
j  = m%get(key)                      ! 0 when absent
ok = m%contains(key)                 ! exactly %get(key) > 0
call m%get_many(keys, rows)          ! one answer per key, 0 where absent; threads itself
call m%get_many(keys, rows, threads=8)   ! on a team you choose
call m%get_many(keys, rows, valid=ok)    ! a row ok(:) marks .false. answers 0, unprobed
```

`%get_many` is **the form to prefer in a hot loop**, and it is the one lookup that threads. It
converts the map object once per call rather than once per key, which on some compilers is the
difference between paying for a runtime type descriptor per lookup and paying for one per array;
and it cuts the keys into one contiguous chunk per thread and probes each chunk on its own thread,
by the same rule a build follows — automatic when you say nothing, `threads=` when you do, and
serial inside your own parallel region (see [Threads a build or a bulk lookup
uses](#threads-a-build-or-a-bulk-lookup-uses)). `rows` may be `int32` or `int64` and must have
exactly one element per key. `valid=` is the same per-key mask `%build` takes: a `.false.` row
costs one test and answers 0, so a nullable key column can be probed as it is.

**Take the answer as `int64` if your stored values can exceed `huge(int32)`.** An `int32` answer
array aborts rather than truncating when a value will not fit, and so does `%get_or_add` with an
`int32` index. `%get` is never affected — it always answers `int64`. This needs a large stored
*value*, not a large map: it is reachable with one key and an explicit `values=`.

A map that was never built answers 0 for every key rather than aborting, and costs nothing extra
for the privilege.

## Composite keys: when no single column is unique

A key can be a tuple of integers. This is for the common case where no one column identifies a
row but a combination does — an object identifier that repeats across observing bands, say:

```fortran
integer(int64) :: keys(n, 2)
type(pf_index_map) :: m
integer(int64) :: j

keys(:, 1) = object_id            ! repeats
keys(:, 2) = band                 ! repeats
call m%build(keys)                ! but the PAIR is unique

j = m%get([obj, bd])              ! look up with the same number of components
```

Bulk arrays are shaped `(n, ncomp)`: one row per key, one column per component. That is the natural
way to pass N separate arrays you already have, and it keeps each component's build scan
stride-1.

Four things to know:

- **Uniqueness is of the whole tuple.** Each component may repeat as much as it likes.
- **Order matters.** `[1, 2]` and `[2, 1]` are different keys.
- **A map has a fixed component count**, set at `%build` from the array's second extent or at
  `%init(ncomp=)`, and reported by `%ncomponents()`. Presenting a key of the wrong width aborts.
- **A single-component map and a 1-tuple are the same map**, so `m%get(k)` and `m%get([k])` find
  the same key.

The most components a key may have is `pf_index_max_components` (32), a published read-only
constant. The sorted backend does not accept composite keys and says so.

## Filling a map as you go

```fortran
call m%set(key, value)               ! insert or replace
call m%get_or_add(key, idx)          ! the key's index, assigning one if it is new
call m%remove(key)                   ! aborts if the key is not there
call m%remove(key, found)            ! or reports absence through found
```

`%get_or_add` is the dictionary-encoding primitive: stream `n` keys through it and you get dense
indexes `1 .. k` for the `k` distinct keys, in first-appearance order. It is the natural way to
factorise a key column.

```fortran
call m%init()
do i = 1, n
    call m%get_or_add(raw_key(i), code(i))     ! code(i) is now in 1..k
end do
```

`%get_or_add_many` does the same for a whole array in one call, taking the map's lock once rather
than once per key, and is the way to factorise a key column:

```fortran
call m%init()
call m%get_or_add_many(raw_key, code)            ! code(:) is now in 1..k, one per raw key
call m%get_or_add_many(raw_key, code, valid=ok)  ! a masked row gets code 0 and adds nothing
```

The codes of one call are dense — the keys new to the map take the next values above its
watermark — and are assigned in first-appearance order on the serial path this runs today. Rely
on a code being stable within the call rather than on that order: a later, partitioned build may
number the same keys in another order and still be correct. Keys, like everywhere else, may be
`int32` or `int64`, scalar or a tuple per row, and the codes may be taken as either kind — an
`int32` code array aborts rather than truncating when a code will not fit.

`%set` on a map you never built starts a hash map for you. On a **direct** map, a key outside the
range the map was built for aborts rather than silently rebuilding as a hash map — that would
change the map's memory behaviour behind your back. Rebuild, or build with `method="hash"`.

## Emptying and reusing a map

```fortran
call m%clear()        ! forget every key AND release all storage
call m%reset()        ! forget every key, keep the allocation
call m%reserve(n)     ! make room for n keys
```

`%clear` releases, matching `%clear` on `parquet_column` and `parquet_string_column`. `%reset` is
the one to reach for when you rebuild a map every iteration of an outer loop: it saves the
allocation and, on the hash backend, the rehash.

## Introspection

```fortran
n = m%nkeys()                        ! keys stored
w = m%ncomponents()                  ! components per key; 0 if never built
b = m%memory_bytes()                 ! heap held
call m%get_method(token)             ! "direct", "hash", "sorted", or "" if never built
call m%keys(list)                    ! every stored key
call m%probe_stats(max_probe)        ! mean_probe is an optional second answer
```

`%keys` gives a rank-1 list for a single-component map and a rank-2 `(nkeys, ncomp)` array for a
composite one — asking a composite map for a rank-1 list aborts and says which rank to ask for,
since the generic can only dispatch on the array you supply. It is allocated zero-length for an
empty map, never left unallocated. Order is ascending for the direct and sorted backends and
unspecified for hash; ask for the matching values with `%get_many(list, vals)`.

`%probe_stats` reports how far lookups have to walk: 1 for the direct backend, the binary search's
depth for sorted, and the real probe lengths for hash. On the hash backend it scans the whole
table, so it is a diagnostic rather than something to call in a loop; the other two answer without
touching the keys. An empty map reports 0.

## The index pool

```fortran
type(pf_index_pool) :: p
integer(int64) :: slot

slot = p%get_index()                 ! an index nobody else holds
call p%free_index(slot)              ! give it back
```

Reuse always precedes growth, so the indexes stay as dense as your live set allows: the pool only
grows its internal storage when every index up to its watermark is out. Freeing an index the pool
never issued, or freeing one twice, aborts — a double free would put the same index on the free
list twice and hand it to two owners who each believed they held the slot.

```fortran
n  = p%get_max_index()               ! the highest index handed out
n  = p%get_used_count()              ! how many are held
n  = p%get_free_index_count()        ! holes below the watermark
ok = p%is_used(idx)
call p%used_indexes(list)            ! every held index, ascending
b  = p%memory_bytes()
call p%reserve(n)
call p%clear()
```

### `%compact`, and what it changes

```fortran
call p%compact()
```

`%compact` gives back storage the pool grew, and then hands out the **smallest** free index first.
It is the answer to a pool that allocated a great deal and then released most of it:

```fortran
do i = 1, 100000000
    slot = p%get_index()
end do
! ... release 90% of them ...
call p%compact()       ! storage comes back, and allocation resumes low
```

Precisely, after `%compact`:

- **every index you still hold is still held** — `%is_used` answers exactly as it did before;
- **`%get_max_index()` becomes the highest index you actually hold**, and the free list holds
  exactly the free indexes below it, handed out smallest first, ascending;
- **the internal arrays shrink** once the watermark has fallen far enough behind them.

That ordering is the point: a pool that lost most of its content converges back onto a dense
`1 .. n` as it keeps allocating, instead of continuing upward from the old watermark. Indexes above
the new watermark come back through ordinary growth, in the same ascending order.

Later `%free_index` calls push onto the top of the rebuilt list, so the newly freed values come
back first again. `%compact` re-sorts; ordinary operation does not pay for sorting.

**Between compacts the watermark only rises.** Freeing the top index does not lower
`%get_max_index()`, because doing so would have to prune every free-list entry above the new mark
on a path that has to stay `O(1)`. `%compact` is where it tightens.

## A map and a pool together

The two compose into a dynamic keyed collection over your own arrays: the map turns an external
key into a slot number, and the pool decides which slot numbers are free.

```fortran
type(pf_index_map)  :: where_is
type(pf_index_pool) :: slots
integer(int64) :: slot
real(real64) :: payload(capacity)

call where_is%init()

! adding an object
slot = slots%get_index()
call where_is%set(object_id, slot)
payload(slot) = value

! removing one
slot = where_is%get(object_id)
if (slot > 0) then
    call where_is%remove(object_id)
    call slots%free_index(slot)
end if
```

Both halves are safe to use from inside a parallel region, so this pattern works there too.

## Threading

**Every mutation of either container is serialized internally**, so one shared map or pool may be
mutated from several threads at once. One thread taking an index from a pool while another gives
one back is a supported pattern, and so is several threads streaming keys through one map's
`%get_or_add` — each thread's returned index is unique and stable.

**Lookups on a map are lock-free.** Any number of threads may `%get`, `%contains` or `%get_many` a
map nobody is mutating, at full speed. `%get_many` also opens a team of its own for a large
enough probe (see [Threads a build or a bulk lookup uses](#threads-a-build-or-a-bulk-lookup-uses))
and stands down to serial inside your parallel region, so calling it from your own threads costs
nothing extra.

**The pool guards its queries as well as its mutations**, so `%is_used`, `%get_max_index` and the
counters all take the lock that a map lookup avoids. A pool query in a hot loop is not free the way
`%get` is; hold the answer rather than asking repeatedly.

**The one unsupported combination is a lookup racing a mutation of the same map.** Guarding `%get`
would cost it the few nanoseconds it exists for, so it is not guarded. Two patterns avoid it:

- **phase discipline** — mutate, then read, with a barrier or an `!$omp single` between the
  phases;
- **route everything through `%get_or_add`**, which is guarded and answers a plain lookup
  correctly for a key that is already present. An **absent** key is *added* rather than reported
  missing, so this suits a workload that would insert the key anyway — it is not a drop-in for
  `%get` on a read-mostly map, where it would grow the map on every miss and answer with a fresh
  index where `%get` answers 0. It also needs the hash backend: a sorted map refuses it, and a
  direct map refuses a key outside its built range.

The serialization is a single lock per type across the whole process, so two unrelated shared maps
take turns with each other as well. That is a deliberate trade: it is what keeps both types free of
lock-handle lifecycle, and so free of finalizers.

### A per-thread map or pool

**Allocate one instance per thread before the region and index it by thread number.** Neither of
the two obvious shapes is portable, on opposite compilers:

```fortran
type(pf_index_map), allocatable :: mine(:)
integer :: tid

allocate(mine(omp_get_max_threads()))
!$omp parallel do default(shared) private(i, tid)
do i = 1, n
    tid = omp_get_thread_num() + 1
    call mine(tid)%build(keys_for(i))
    ...
end do
```

A `private()` clause does **not** work: the private copy's scalar components are not reliably
default-initialised, so the first procedure that trusts one reads garbage. A block-local
declaration inside the region is what that compiler wants instead, and is what another compiler
segfaults on for any type with allocatable components. The per-thread array satisfies both.

## Threads a build or a bulk lookup uses

A `%build` threads its key scan and, on the direct backend, its scatter; a `%get_many` threads
its probe, one contiguous chunk of the keys per thread. Both resolve their team by the same rule,
over the rows they are handed.

```fortran
call m%build(keys)                   ! automatic
call m%build(keys, threads=4)        ! an explicit request, honoured
call m%build(keys, threads=1)        ! forced serial
call m%get_many(probes, rows, threads=4)         ! the same three spellings on a lookup
nt = pf_index_threads(size(keys, kind=int64))   ! what an automatic build or lookup would open
```

**The automatic answer and an explicit `threads=` are resolved differently, and only one of them is
bounded.** The automatic one is `omp_get_max_threads()` outside a parallel region and **1** inside
one — nested teams are the caller's business — capped by `parquet_set_index_threads`, then
bounded by the work available. An explicit `threads=` bypasses all three: it is honoured whatever
the size of the build and wherever it is called from, including inside somebody else's parallel
region.

| situation | threads used |
|---|---|
| automatic, ordinary serial code | `omp_get_max_threads()`, capped by `parquet_set_index_threads` |
| automatic, inside any `!$omp parallel` region | **1** — serial |
| automatic, fewer than about 8000 keys (or rows probed) | **1** — below the work floor |
| automatic, above it | about one thread per 4000 keys (or rows), up to the cap |
| explicit `threads=n`, anywhere | `n` |
| any of the above | clamped to what the process's CPU affinity allows |

The affinity clamp is the one rule with no exception: it applies to an explicit request as well, and
lowering a request to the processors actually available is a performance decision that never changes
the answer. `threads=0` is refused rather than read as "automatic".

`parquet_set_index_threads(n)` caps the automatic answer process-wide, and
`PARQUET_FORTRAN_INDEX_THREADS` does the same from the environment — see
[Settings](../operating/settings.html). A cap only ever lowers the automatic answer; pass
`threads=` to ask for more. A `method="sorted"` build sorts through `pf_argsort`, so that phase
answers to the sorting thread knobs instead, while the key scan around it follows the rule above.

Threading a build or a lookup changes how fast it answers and never what it answers.

## Performance notes

Machine-free, as everywhere in this guide; `bench/benchmark_index.sh` is what measures these on
your own hardware.

- **The direct backend is as fast as an array read**, because that is what it is. Prefer it when
  your keys are dense, which the automatic choice already does for you.
- **A hash lookup is memory-bound**, so it costs about one cache miss. Keeping the load factor at
  0.6 is what keeps the first probe usually the hit.
- **`%get_many` beats a loop of `%get`** by enough to be worth restructuring a hot loop for, and
  on a team it is the fastest probe there is: the chunks share nothing and a hash probe is one
  cache miss, so the speed-up tracks the thread count until memory bandwidth saturates.
- **`%get_or_add_many` beats a loop of `%get_or_add`** by the lock it does not take per key;
  the hashing and the inserts cost the same either way.
- **`%reserve` before a run of inserts** avoids the rehashes, which are the only part of
  incremental filling that is not amortised `O(1)`.
- **The sorted backend trades speed for memory** and is the one to reach for when a map has to fit
  somewhere the others will not.
