---
title: Key-to-index lookup with parquet_index
---

`parquet_index` answers the two halves of "where does this thing live in my arrays?".

`pf_index_map` answers it for a key you already have: give it the key column of a table and it
tells you, in a few nanoseconds, which row a key sits in — an integer key, a tuple of them, or a
string. `pf_index_multimap` answers it when the key repeats: every row a key sits in, as a range.
`pf_index_pool` answers it when nothing has a key yet: it issues 1, 2, 3, … on request and takes
them back, so a program managing slots in its own arrays never has to track which are in use.

All three are ordinary Fortran containers over plain integer arrays. Nothing here reads or writes a
Parquet file, and `use parquet_index` never reaches the C++ bindings — see
[Choosing a module](../operating/choosing-a-module.html) for what that does and does not buy you.

Two things in the library are built on them, and use nothing else to answer a lookup: the table's
[`%build_index`](../tables/table-mutate.html#looking-a-value-up-build_index), which wraps a map or
a multimap over one column's keys with a staleness check, and the row filter's
[`in`/`not_in` clause](../io/filter-sort-sample.html#membership-in-a-set-in-and-not_in), which
holds a bound set in a map — a string set in a string-keyed one — and probes each row group
through `%get_many`.

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

Keys and values may be `integer(int32)` or `integer(int64)`, in any combination, and a key may
also be a string — see [String keys](#string-keys) below. **Every key must be unique**; a
duplicate aborts and names the offender. There is no "first wins" or "last wins" option, because a
map that silently answered for one of two rows would give you no way to find out which.

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

The hash row is for a single-component key. A composite one keeps each tuple and its value as one
record per slot, costing `8 * (ncomponents + 1)` bytes — 24 for a pair, 32 for a triple — with the
same 1.7-3.3x table size, and a probe still touches one cache line for up to seven components.

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

## String keys

A map's keys can be strings, and the map is then built, probed and filled with the same calls as
an integer one:

```fortran
type(pf_index_map) :: m
type(parquet_string_column) :: names, probes, list
integer(int64), allocatable :: rows(:)

call m%build(ids)                       ! ids(:) is a character array: each element trimmed
call m%build(names)                     ! or a parquet_string_column: each element verbatim
j = m%get("obj_4711")                   ! 0 when absent; the key is taken exactly as written
call m%get_many(probes, rows)           ! one answer per element of a column, in place
call m%get_many(ids, rows)              ! or per element of a character array, each trimmed
call m%get_or_add("obj_4711", idx)      ! the dictionary-encoding primitive, over strings
call m%set("obj_4711", 9_int64)         ! insert or replace
call m%remove("obj_4711")               ! forget one key
call m%keys(list)                       ! the stored keys, as a parquet_string_column
```

**A key is its exact bytes.** `"ab"` and `"ab "` are two keys, `"AB"` a third, and the empty
string an ordinary fourth — the equality `pf_in`, `pf_match` and every sort in this library use
for strings. Two rules follow from how Fortran strings are declared. An element of a `character`
**array** is trimmed of trailing blanks on the way in, whether it is being stored or looked up,
because an array's elements share one declared length and the padding a shorter value carries
cannot be what you meant — the rule every character array argument of this library follows. A
`character` **scalar** is taken as written, so pass `trim(name)` for a blank-padded variable. A
`parquet_string_column`'s elements are taken verbatim, since the column holds exactly the bytes
that were appended to it, and a **null** element is never a key: a build skips it (the other rows
keep their row numbers, exactly as `valid=` does), a bulk lookup answers 0 for it, and
`%get_or_add_many` neither looks it up nor adds it.

**Underneath, every string map is the hash backend**, with the strings kept beside the table:
a key is hashed to 64 bits by the module's own mixer, stored under the tuple `(hash, occurrence)`,
and verified byte for byte on every hit, so no lookup ever answers on a hash match alone and no
key set is refused however the hashes fall — two strings sharing a hash simply take occurrences 0
and 1. `method=` therefore accepts only `"auto"` and `"hash"`; asking for `"direct"` or
`"sorted"` aborts, naming the rule. `%ncomponents()` reports 1 (one string is one key), and
`%memory_bytes()` counts the table and the strings, a removed key's bytes included until the map is
rebuilt or cleared.

**A map holds one kind of key for its whole life.** An integer lookup on a string map, or a string
lookup on an integer map, aborts naming which kind the map holds, and so does `%keys` asked for
the wrong shape; the only exception is a map that was never built, which answers 0 to everything.
A fresh map becomes a string map at its first string `%build`, `%set` or `%get_or_add`, and
`%init(strings=.true.)` starts one empty when you want `capacity=` first.

The multimap takes string keys the same way — `mm%build(names)`, `mm%count("obj_4711")`,
`mm%get_all("obj_4711", rows)`, `mm%probe_many(probes, offsets, matches)` and `mm%keys(list)`
— see [A key that repeats](#a-key-that-repeats-pf_index_multimap).

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

`%keys` gives a rank-1 list for a single-component map, a rank-2 `(nkeys, ncomp)` array for a
composite one, and a `parquet_string_column` for a string-keyed one — asking for the wrong shape
aborts and says which to ask for, since the generic can only dispatch on the argument you supply.
It is allocated zero-length (or an empty column) for an empty map, never left unallocated. Order
is ascending for the direct and sorted backends and unspecified for hash; ask for the matching
values with `%get_many(list, vals)`.

`%probe_stats` reports how far lookups have to walk: 1 for the direct backend, the binary search's
depth for sorted, and the real probe lengths for hash. On the hash backend it scans the whole
table, so it is a diagnostic rather than something to call in a loop; the other two answer without
touching the keys. An empty map reports 0.

## A key that repeats: `pf_index_multimap`

`pf_index_map` refuses a duplicate key by contract. When the key column is not unique — an object
identifier that repeats across observing bands, a group id, the build side of an m:m join — the
question is "which rows", and `pf_index_multimap` answers it: every position holding a key, as a
contiguous range, ascending by position.

```fortran
use parquet_index
type(pf_index_multimap) :: mm
integer(int64), allocatable :: rows(:)

call mm%build(group_id)                  ! keys may repeat; values default to 1..n
n = mm%count(42_int64)                   ! how many rows hold it; 0 when absent
j = mm%get_first(42_int64)               ! the lowest of them; 0 when absent
call mm%get_all(42_int64, rows)          ! all of them, ascending; zero-length when absent
```

Underneath it is a `pf_index_map` over the *distinct* keys, each mapped to a group id in
`1 .. ngroups`, and a CSR pair beside it — `offsets(ngroups + 1)` and `rows(nkeys)` — with group
`g`'s values at `rows(offsets(g) : offsets(g+1) - 1)`. So everything the map accepts, the multimap
accepts too: `int32` or `int64` keys, scalar or a tuple per row, string keys from a character array
or a `parquet_string_column` (under the map's own [rules](#string-keys)), `values=`, `valid=`,
`method=` and `threads=`; and the same "0 means not found" protocol on every lookup.

### Building a multimap

```fortran
call mm%build(keys)                      ! values default to the row numbers 1..n
call mm%build(keys, values)              ! or one value per row, each >= 1
call mm%build(keys, valid=ok)            ! skip every row ok(:) marks .false.
call mm%build(keys, method="hash")       ! or force the distinct-key map's backend
```

Within a group the values are in ascending order of **position**, whatever the values are: with
the default values that is ascending row number, and `%get_first` is the lowest row. A `valid=`
mask skips a row entirely — it is neither stored nor counted, and the values stay the original
row numbers of the rows that were kept — which is how a nullable key column is indexed as it is.

The backend is the map's automatic choice, applied to the **distinct** keys: a dense id column
that repeats takes the direct backend, a sparse one the hash table, and `method=` overrides as on
the map. The choice is made in two steps, so ten thousand keys spread over a billion and repeated
a thousand times each still go to a 0.5 MB hash table rather than a 320 MB direct array. In this
version the grouping pass is serial; `threads=` reaches the map built over the distinct keys and
is honoured there.

**Group ids are dense in `1 .. ngroups` and rows are ascending within a group; nothing else about
the ids is a contract.** On this version's serial pass they follow first appearance among the
unmasked rows, which a later partitioned pass will change. Rely on an id being stable for the
life of one build, never on its order.

### Looking up a key that repeats

Scalar, all lock-free:

```fortran
g  = mm%get(key)                     ! the group id, 1..ngroups; 0 when absent
n  = mm%count(key)                   ! rows holding the key; 0 when absent
j  = mm%get_first(key)               ! the value at the lowest position; 0 when absent
call mm%get_all(key, rows)           ! every value, ascending; zero-length when absent
call mm%get_range(key, lo, hi)       ! the same as a slice of %csr's rows; lo > hi when absent
```

And in bulk, on a team of their own by the rule the map's `%get_many` follows:

```fortran
call mm%get_first_many(keys, rows)               ! one value per key, 0 where absent
call mm%get_many(keys, groups)                   ! one group id per key, 0 where absent
call mm%probe_many(keys, offsets, matches)       ! EVERY match, as a CSR pair
```

All three take `valid=` (a masked key answers 0, or an empty range, unprobed), `threads=`, and a
count: `n_found=` on the first two, `n_matched=` on the third. `rows`, `groups` and `matches` may
be `int32` or `int64`; an `int32` answer aborts up front, rather than truncating, if any stored
value would not fit.

### Every match at once: `%probe_many`

`%probe_many` is `pf_match_all` on a hash engine, and the join's m:m primitive: `offsets` has one
entry per probe key plus one, `offsets(1) == 1`, and the stored values for probe `i` are
`matches(offsets(i) : offsets(i+1) - 1)` — an empty range when there are none, ascending by
position within it. Two threaded passes over the probes: the group and count of each,
prefix-summed into `offsets`; then each probe's range copied into place.

```fortran
call mm%probe_many(keys, offsets, matches, n_matched=nm, group_hit=hit)
do i = 1, size(keys)
    do p = offsets(i), offsets(i+1) - 1
        ! key i matches stored row matches(p)
    end do
end do
```

`group_hit(ngroups)` is set for every group some probe reached — how a right or outer join finds
the stored rows nothing probed, without a second structure.

**`size(matches)` counts pairs, and a pair count is a product**: a key held by a thousand stored
rows and a thousand probes contributes a million on its own. The total is
`offsets(size(keys) + 1) - 1`, accumulated in `int64` and refused rather than wrapped when it
would not fit; read it before doing anything proportional to it.

### Introspecting a multimap

```fortran
n = mm%ngroups()                     ! distinct keys
n = mm%nkeys()                       ! rows stored, repeats included
n = mm%max_multiplicity()            ! rows in the largest group; 1 means every key is unique
w = mm%ncomponents()                 ! components per key; 0 if never built
b = mm%memory_bytes()                ! heap held: the distinct-key map plus the CSR pair
call mm%get_method(token)            ! the distinct-key map's backend
call mm%keys(list)                   ! the distinct keys; pair with %get_many for their groups
call mm%csr(offsets, rows)           ! the CSR pair itself, copied out
call mm%clear()                      ! forget every key and release all storage
```

`%max_multiplicity() == 1` is the m:1 check a join makes before choosing its path. `%csr` is for
a caller that walks the ranges itself — a group-by, or a join's build side — or wants every group
at once; group `g`, the id `%get` answers, holds `rows(offsets(g) : offsets(g+1) - 1)`.

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

**A `%build` is not serialised with the rest.** It scans the keys and fills a map of its own on the
calling thread, and takes the lock only to swap that result into your object — microseconds — so
two threads building two maps run side by side, and a program preparing several maps in a parallel
loop gets the team it opened. The rule below, that a lookup must not race a mutation of the same
map, applies to the swap as it does to any mutation.

**Lookups on a map are lock-free.** Any number of threads may `%get`, `%contains` or `%get_many` a
map nobody is mutating, at full speed. `%get_many` also opens a team of its own for a large
enough probe (see [Threads a build or a bulk lookup uses](#threads-a-build-or-a-bulk-lookup-uses))
and stands down to serial inside your parallel region, so calling it from your own threads costs
nothing extra.

**A multimap follows the same rules.** Its lookups are lock-free, its bulk forms open a team by
the rule the map's `%get_many` follows and stand down inside your region, and a `%build` or
`%clear` is serialised on a lock of its own — one per multimap type, distinct from the map's, so
that the map calls a build makes underneath can take theirs.

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
take turns with each other as well — for their inserts, removals and `%get_or_add`s; a `%build`, as
above, holds the lock for its swap alone. That is a deliberate trade: it is what keeps both types
free of lock-handle lifecycle, and so free of finalizers.

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
over the rows they are handed, and so do the multimap's `%get_first_many`, `%get_many` and
`%probe_many`.

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
- **A bulk composite lookup costs about what a scalar one does.** Each slot holds the tuple and
  its value as one record, so a probe touches one cache line for up to seven components; the tuple
  hash runs one 32-bit mixing step per component on two independent chains; and pairs — a
  timestamp key, a string key underneath — have a kernel of their own. `--mode=tuple` of
  `bench/benchmark_index.sh` prints the two side by side, with each table's probe statistics.
- **`%get_many` beats a loop of `%get`** by enough to be worth restructuring a hot loop for --
  on a map larger than the cache it is about twice as fast even on one thread, because it hashes
  a block of keys first and walks the table afterwards, so a block's cache misses are in flight
  together where a loop of `%get` waits for each one -- and on a team it is the fastest probe
  there is: the chunks share nothing and a hash probe is one cache miss, so the speed-up tracks
  the thread count until memory bandwidth saturates.
- **`%get_or_add_many` beats a loop of `%get_or_add`** by the lock it does not take per key;
  the hashing and the inserts cost the same either way.
- **A string probe is a hash over the key's bytes, one table probe and one byte compare on a
  hit**, so it costs about a nanosecond per byte of key on top of an integer probe, and a build
  copies the key strings once. `%get_many` over a `parquet_string_column` reads the column's own
  buffers in place and threads like every other bulk form; `--mode=strings` of
  `bench/benchmark_index.sh` prints it beside `pf_in` over the same two columns, the sort-merge
  it replaces in the row filter's string clause.
- **A multimap probe costs one map lookup per key plus one copy per pair.** `%get_first_many` is
  the map's `%get_many` and one gather; `%probe_many` is that plus a copy of every matched range,
  so its time is proportional to the pair count, and `--mode=multimap` prints it beside the
  sort engine's figure for the same arrays.
- **`%reserve` before a run of inserts** avoids the rehashes, which are the only part of
  incremental filling that is not amortised `O(1)`.
- **The sorted backend trades speed for memory** and is the one to reach for when a map has to fit
  somewhere the others will not.
