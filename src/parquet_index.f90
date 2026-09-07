!> Fast key-to-index lookup: `pf_index_map` maps integer keys to index values, `pf_index_pool`
!> hands out and recycles unique ones.
!!
!! Two containers that answer the two halves of "where does this thing live in my arrays?".
!!
!! **`pf_index_map` answers it for a key you already have.** Give it the key column of a table and
!! it tells you, in a few nanoseconds, which row a key sits in -- replacing the `findloc` scan or
!! the hand-rolled offset array that a program otherwise grows for the purpose. A key is a single
!! integer, or an N-component tuple of integers when no single column is unique on its own
!! (`[object_id, band]` is one key; `object_id` alone repeats). Three storage backends sit behind
!! one API and the module picks between them from the keys themselves:
!!
!! * **direct** -- an array indexed by the key. The fastest lookup there is: two comparisons and
!!   one load, no hashing and no probing. Chosen automatically when the key range is dense enough
!!   that the array is no larger than the hash table would have been.
!! * **hash** -- open addressing with linear probing, for keys spread over a range too wide to
!!   index directly. The general case, and the only backend that accepts keys added one at a time.
!! * **sorted** -- sorted keys plus a binary search, 16 bytes per key with no slack at all. The
!!   smallest footprint available, at `O(log n)` per lookup, and opt-in: the automatic choice never
!!   selects it. Build-once and frozen; single-component keys only.
!!
!! **`pf_index_pool` answers it when nothing has a key yet**: it issues 1, 2, 3, ... on request and
!! takes them back, so a program managing slots in its own arrays never has to track which are in
!! use. Reuse always precedes growth, so the indexes stay dense, and `%compact` gives back the
!! storage a burst of allocation grew -- after which the smallest free index is handed out first,
!! so a pool that lost most of its content converges back onto a compact `1 .. n`.
!!
!! **Stored values are index values: integers >= 1, and 0 means "not found".** That is the whole
!! error protocol for a lookup -- there is no separate `found` flag to thread through a hot loop:
!!
!!```fortran
!! j = m%get(pix)
!! if (j > 0) then
!!     ...                 ! row j of your arrays is the one
!! end if
!!```
!!
!! It is also what makes the hash table sentinel-free: a slot whose value is 0 *is* an empty slot,
!! so no key value is reserved, there are no tombstones and no occupancy bitmap. A caller who
!! genuinely needs to store 0 stores `value + 1`. Storing anything below 1 aborts.
!!
!! **Threading.** Every mutation of either container is serialized internally, so one shared map or
!! pool may be mutated from several threads at once -- one thread taking an index from a pool while
!! another gives one back is a supported pattern, and so is several threads streaming keys through
!! one map's `%get_or_add`. Lookups on a map are lock-free and may run on any number of threads at
!! full speed, and `%get_many` opens a team of its own over the keys it is given, by the same rule
!! a build follows. The one combination that is NOT supported is a lookup racing a mutation of the
!! same map: guarding `%get` would cost it the few nanoseconds it exists for. Separate the phases,
!! or route every access through `%get_or_add`. See `doc/pages/utilities/index-maps.md`.
!!
!! **Arrow-free by construction, and that is the point of its tier.** This module reaches
!! `iso_fortran_env`, `parquet_settings_base` and -- for the sorted backend's build --
!! `parquet_argsort`, and nothing else. `use parquet_index` therefore compiles a handful of
!! Fortran files rather than the sixty-odd the reader/writer stack costs.
!! `check_parquet_index_stays_arrow_free` (tools/check_source_conventions.py) and
!! `tools/module_footprints.txt` are what keep that true: a `use` line added here can silently
!! multiply what every consumer compiles, and no test can see it happen.
!!
!! **Naming.** Everything public carries the `pf_` prefix (parquet-fortran) rather than
!! `parquet_`, the same rule as `parquet_sorting`/`parquet_random`/`parquet_spatial`: the subject
!! is a general-purpose lookup structure over plain Fortran arrays, not a Parquet file. The module
!! is named for its domain rather than for either type, so neither type's name collides with it.
module parquet_index
    use iso_fortran_env, only: int32, int64, real64
    use parquet_settings_base, only: parquet_set_verbosity, parquet_get_verbosity, &
        parquet_set_message_stream, parquet_get_message_stream, &
        parquet_set_index_threads, parquet_get_index_threads, &
        parquet_set_sort_threads, parquet_get_sort_threads, &
        parquet_set_sort_radix_path, parquet_get_sort_radix_path, &
        parquet_set_sort_counting_path, parquet_get_sort_counting_path, &
        parquet_set_sort_counting_bucket_limit, parquet_get_sort_counting_bucket_limit
    implicit none
    private

    public :: pf_index_map
    public :: pf_index_pool
    public :: pf_index_multimap
    public :: pf_index_threads
    public :: pf_index_max_components
    public :: parquet_debug_index_threads_used
    public :: parquet_debug_index_get_many_threads_used
    public :: parquet_debug_set_index_pair_limit
    !
    ! ---- Settings this module's own code reads, re-exported so a narrow import can configure it ----
    !
    !> The thread cap `%build` reads, so that a program whose only import is `use parquet_index`
    !! can bound what a build opens without naming `parquet_settings` -- which would put the C++
    !! boundary, and with it Arrow, back into an otherwise Arrow-free build. It LOWERS the
    !! automatic answer and never raises it; pass `threads=` on the call for that.
    public :: parquet_set_index_threads, parquet_get_index_threads
    !
    !> The output pair, because the affinity clamp inside the build's thread rule emits a warning
    !! through the same channel every other tier does.
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream
    !
    !> The four sorting knobs, because `method="sorted"` builds through `pf_argsort` and that sort
    !! answers to them rather than to `index_threads` -- so a sorted build reads both sets, each
    !! for the phase it owns. Re-exported on the same argument as everything above.
    public :: parquet_set_sort_threads, parquet_get_sort_threads
    public :: parquet_set_sort_radix_path, parquet_get_sort_radix_path
    public :: parquet_set_sort_counting_path, parquet_get_sort_counting_path
    public :: parquet_set_sort_counting_bucket_limit, parquet_get_sort_counting_bucket_limit

    ! ---- Backend discriminators (private: the public spelling is a token, see %get_method) ----

    !> No backend chosen yet: an `%init`ed map before its first insert, and nothing else.
    integer, parameter :: IX_NONE = 0
    !> Direct backend: an array indexed by the key, or by a mixed-radix offset over the tuple.
    !!
    !! **This is the default of the `backend` component, and that is load-bearing.** Together with
    !! the default empty range (`kmin1 = 0`, `kmax1 = -1`) it makes every lookup on an unbuilt or
    !! cleared map fail the range test and answer 0, so `%get` needs no "am I built?" branch at
    !! all -- see the range test in `ix_get_direct_scalar`.
    integer, parameter :: IX_DIRECT = 1
    !> Hash backend: open addressing, linear probing, power-of-two capacity.
    integer, parameter :: IX_HASH = 2
    !> Sorted backend: ascending keys plus a binary search. Frozen after build.
    integer, parameter :: IX_SORTED = 3

    !> Largest load factor the hash table runs at before it doubles, as a percentage.
    !!
    !! 60% is the usual choice for linear probing and is where the expected probe count for a hit
    !! is still under two. Expressed as an integer percentage rather than a real so that the
    !! comparison `100 * used > IX_MAX_LOAD_PCT * cap` is exact integer arithmetic -- a real
    !! comparison here would make the doubling point depend on rounding.
    integer(int64), parameter :: IX_MAX_LOAD_PCT = 60_int64

    !> Slots a hash table gets when `%init` is given no capacity hint.
    integer(int64), parameter :: IX_MIN_HASH_CAP = 16_int64

    !> Floor on the key range the automatic choice always calls dense enough for the direct
    !! backend, in slots.
    !!
    !! 65536 slots is 512 KiB, which any machine can spare and which lets a small map take the
    !! fastest path whatever its density -- a 100-key map over the range 1..50000 is direct, where
    !! the `4*n` term alone would have sent it to the hash table. Paired with `IX_DIRECT_PER_KEY`
    !! in `ix_choose_backend`.
    integer(int64), parameter :: IX_DIRECT_FLOOR = 65536_int64

    !> Slots per key the automatic choice will spend on the direct backend beyond the floor.
    !!
    !! At 4 slots per key the direct array costs `4 * n * 8 = 32n` bytes, against the hash table's
    !! `2n` slots of 16 bytes mid-doubling-cycle -- also `32n`. So up to this density direct is
    !! memory-neutral and strictly faster, and past it the hash table wins on memory. Both
    !! constants are deliberately NOT settings: they change how fast the library runs and never
    !! what it answers, `method=` is the per-call override that makes them non-load-bearing, and
    !! `%get_method` is how a test observes the choice.
    integer(int64), parameter :: IX_DIRECT_PER_KEY = 4_int64

    !> Mask selecting the low 32 bits. The mixer's whole overflow-freedom rests on this: every
    !! multiplicand is masked to 32 bits and every constant to 31, so no product reaches 2**63.
    integer(int64), parameter :: IX_MASK32 = 4294967295_int64

    !> Keys per thread the automatic build insists on before it opens another one.
    !!
    !! The rule is a team bounded by the work available, not a threshold on the call:
    !! `nt = min(nt_auto, n / IX_MIN_KEYS_PER_THREAD)`, the same shape
    !! `hpx_min_elements_per_thread` uses (src/parquet_healpix_bulk.f90) and for the same reason --
    !! a single element count cannot serve both an 8-core machine and a 384-thread one, but the
    !! work *one* thread needs to be worth waking is stable across both.
    !!
    !! Not a setting, deliberately: it changes only how fast a build runs, never what it answers.
    !! It needs no `parquet_debug_set_*` override either, which is worth saying because most tuning
    !! constants in this library do: threading begins at twice this many keys, so an ordinary
    !! test-sized fixture of a few tens of thousands reaches the threaded scan and scatter without
    !! anything having to force it. Keep that true if the number is ever raised.
    integer(int64), parameter :: IX_MIN_KEYS_PER_THREAD = 4096_int64

    !> Threads the LAST `%build` resolved for its own work; 1 means it ran serial.
    !!
    !! **Test-only, and the only observable a threading test of this module has.** A build's answer
    !! is identical at every thread count -- that is the point of the design -- so no assertion on
    !! the map itself can tell a `threads=4` that was honoured from one that was silently ignored.
    !! Without this counter every such test is vacuous, and a policy that never threads is the
    !! easiest bug here to write and the hardest to see.
    !!
    !! Records what `ix_threads_for` RESOLVED, not `omp_get_num_threads()` from inside the region:
    !! the decision is what is under test, not the runtime's response to it.
    !!
    !! **Written only by `ix_threads_for`, which is reached only from the two build workers, which
    !! run inside `pf_index_map_guard`** -- so the write is serialized and a concurrent build cannot
    !! interleave with one. `pf_index_threads` deliberately does NOT record: it is a public query a
    !! user may call at any time, and letting it write here would let an unrelated call clobber what
    !! a build had just reported. That is why the rule is split into `ix_threads_rule` (decides) and
    !! `ix_threads_for` (decides and records).
    integer, save :: dbg_index_threads_used = 1

    !> Threads the LAST `%get_many` resolved for its own work; 1 means it ran serial.
    !!
    !! The lookup counterpart of `dbg_index_threads_used`, and there for the same reason: a bulk
    !! lookup answers identically at every team size, so nothing about its answers can tell an
    !! honoured `threads=` from one that was ignored, or a team that was opened from one that
    !! silently collapsed to a single thread. Kept apart from the build's counter because the two
    !! are written on different paths -- the build's under `pf_index_map_guard`, this one lock-free
    !! from whichever thread called `%get_many` -- so one counter for both would let a probe clobber
    !! what a build had just reported under a test's feet, or the reverse. Written with an atomic
    !! store, so concurrent bulk lookups cannot tear it; which of them is reported is then simply
    !! the last to resolve its team, which is all a test may rely on. Written only by
    !! `ix_lookup_threads_for`, which every `%get_many` specific reaches.
    integer, save :: dbg_index_get_many_threads_used = 1

    !> Pair count above which `pf_index_multimap%probe_many` refuses to answer: `huge(int64)`
    !! unless a test has lowered it through `parquet_debug_set_index_pair_limit`.
    !!
    !! Test-only. The real ceiling is the `int64` domain, which no test can fill, and a guard no
    !! test reaches is one edit away from being silently wrong; the hook is what lets the error
    !! scenario reach it. Read once per `%probe_many` call, on the calling thread, so setting it
    !! while another thread is inside a probe is a race the scenario harness never runs.
    integer(int64), save :: dbg_index_pair_limit = huge(0_int64)

    !> Most components one key may have, and the reason the tuple paths allocate nothing.
    !!
    !! A tuple lookup has to present its key as `integer(int64)` whatever kind the caller holds, so
    !! an `int32` tuple must be widened somewhere. Widening into a FIXED-SIZE local buffer puts it
    !! on the stack unconditionally, on every compiler; an automatic array sized `size(key)` would
    !! be stack-allocated by some and heap-allocated by others, which would mean a malloc per
    !! lookup on the hot path of the one backend that exists to be fast.
    !!
    !! 32 is far above the stated use case (a two- or three-component key such as
    !! `[object_id, band]`) and costs 256 bytes of stack per tuple call. Published as a read-only
    !! constant rather than a setting, on the same rule as the `parquet_max_*` limits: making an
    !! input-sanity bound settable turns a guard into a way to overflow a buffer.
    integer, parameter :: pf_index_max_components = 32

    ! ---- Hash slot, single-component maps ----

    !> One `(key, value)` pair of a single-component hash table, kept as one 16-byte record so
    !! that a probe touches one cache line rather than two.
    !!
    !! A composite map cannot use this -- an AoS slot whose width is a runtime `ncomp` is not
    !! expressible without allocating per slot -- so it stores `hkeys(ncomp, cap)` beside
    !! `hvals(cap)` and pays the second cache line per probe. At a load factor of 0.6 the first
    !! probe is usually the hit, which is what makes that acceptable for arbitrary `ncomp` and not
    !! acceptable for the common single-component case.
    type :: ix_slot
        integer(int64) :: key = 0_int64 !! the stored key; meaningless unless `val > 0`.
        integer(int64) :: val = 0_int64 !! the stored index value; **0 marks the slot empty**.
    end type ix_slot

    ! ---- pf_index_map ----

    !> Maps integer keys, single or tuple, to index values >= 1. 0 answers "not found".
    !!
    !! Build it in bulk from the keys you have (`%build`), or start empty and add keys as you meet
    !! them (`%init` then `%set`/`%get_or_add`). Look up with `%get`, or `%get_many` for a whole
    !! array of keys -- which is the form to prefer in a hot loop, because it converts the object
    !! once per call rather than once per key.
    !!
    !! **No finalizer, deliberately.** Every component is a plain allocatable, which F2018 9.7.3.2
    !! already deallocates on scope exit, so there is nothing for a `FINAL` to do, and the type
    !! stays out of the finalizer gotcha family entirely.
    !!
    !! **A per-thread instance goes in a SHARED ARRAY indexed by thread number, allocated before
    !! the parallel region -- not in a `private()` clause and not declared block-local.** Both of
    !! the obvious shapes are unsafe, on opposite compilers, because this type has allocatable
    !! components:
    !!
    !! * `private()` was MEASURED to segfault under gfortran 15.2. The private copy's scalar
    !!   components are not default-initialised, so the first procedure that trusts one reads
    !!   garbage -- `pf_index_pool%get_index` indexes its free list at a garbage offset and dies
    !!   inside the allocator. `%build` happens to survive because it resets every component
    !!   before reading any, which makes the shape look supported until something else is called
    !!   first.
    !! * block-local inside the region is what gfortran wants, and is what ifx segfaults on for
    !!   any type with allocatable components (CLAUDE.md's ifx gotchas).
    !!
    !! So neither compiler's preferred shape is portable, and the per-thread array -- the pattern
    !! `materialize_marked_parallel` (src/parquet_tables_read.f90) already uses -- is the one that
    !! works on both. The guide page shows it.
    type :: pf_index_map
        private
        !> Which backend the storage below is in. Defaults to `IX_DIRECT` so that an unbuilt map's
        !! lookup takes the range test and answers 0 with no extra branch.
        integer :: backend = IX_DIRECT
        !> Components per key. 0 marks a map that has never been built or `%init`ed; 1 is a
        !! single-component map, for which the scalar and 1-tuple key forms are the same map.
        integer :: ncomp = 0
        !> Keys stored.
        integer(int64) :: nk = 0_int64
        !> High-water mark `%get_or_add` numbers from: the next new key gets `next_auto + 1`.
        !! `%set` raises it to any larger value stored, and `%remove` never lowers it, so an index
        !! this map has issued is never issued twice. Recycling indexes is `pf_index_pool`'s job.
        integer(int64) :: next_auto = 0_int64
        !> Direct backend, single component: the key range `kmin1 .. kmax1` that `dvals` covers.
        !! **The defaults describe an EMPTY range on purpose** -- `0 .. -1` contains no key, so
        !! every lookup on a fresh or cleared map falls out of it and answers 0.
        integer(int64) :: kmin1 = 0_int64
        integer(int64) :: kmax1 = -1_int64 !! see `kmin1`.
        !> Direct backend: one slot per key in the covered range, holding the index value, 0 where
        !! no key maps there. Sized `product(drange)` for a composite map.
        integer(int64), allocatable :: dvals(:)
        !> Direct backend: per component, the smallest and largest key seen at build, the number
        !! of distinct positions the pair spans, and the component's stride into `dvals`.
        !! `dstride(1)` is 1 and `dstride(j+1)` is `dstride(j) * drange(j)` -- an ordinary
        !! mixed-radix offset.
        !!
        !! **`dkmax` is stored rather than derived, and that is a correctness decision.** Deriving
        !! it as `dkmin + drange - 1` at lookup time is one addition that overflows exactly when
        !! the largest key is `huge`, and testing a key with `key - dkmin >= drange` instead is a
        !! subtraction that overflows for a key far below `dkmin`. Two stored bounds and two
        !! comparisons cannot overflow at all, for eight bytes per component.
        !!
        !! **Filled for a single-component map too**, alongside `kmin1`/`kmax1`, so that the same
        !! map answers both a scalar `%get(k)` and a 1-tuple `%get([k])` -- which the design makes
        !! the same question.
        integer(int64), allocatable :: dkmin(:)
        integer(int64), allocatable :: dkmax(:) !! see `dkmin`.
        integer(int64), allocatable :: drange(:) !! see `dkmin`.
        integer(int64), allocatable :: dstride(:) !! see `dkmin`.
        !> Hash backend, single component: the table. Always a power of two in size.
        type(ix_slot), allocatable :: slots(:)
        !> Hash backend, composite keys: the tuples, one column per slot, beside their values.
        integer(int64), allocatable :: hkeys(:,:)
        integer(int64), allocatable :: hvals(:) !! see `hkeys`. **0 marks the slot empty.**
        !> Slots in the hash table, i.e. `size(slots)` or `size(hvals)`. A power of two, so the
        !! slot index is `iand(h, hcap - 1)` and never a runtime-divisor `mod`.
        integer(int64) :: hcap = 0_int64
        !> Sorted backend: ascending keys and their values, exact-fit, no slack.
        integer(int64), allocatable :: skeys(:)
        integer(int64), allocatable :: svals(:) !! see `skeys`.
    contains
        !
        ! ---- Lifecycle ----
        !
        !> Build the whole map from the keys you have. Bulk, and the usual way in.
        generic :: build => &
            build_r1_k32_nov, build_r1_k32_v32, build_r1_k32_v64, &
            build_r1_k64_nov, build_r1_k64_v32, build_r1_k64_v64, &
            build_r2_k32_nov, build_r2_k32_v32, build_r2_k32_v64, &
            build_r2_k64_nov, build_r2_k64_v32, build_r2_k64_v64
        procedure, private :: build_r1_k32_nov, build_r1_k32_v32, build_r1_k32_v64
        procedure, private :: build_r1_k64_nov, build_r1_k64_v32, build_r1_k64_v64
        procedure, private :: build_r2_k32_nov, build_r2_k32_v32, build_r2_k32_v64
        procedure, private :: build_r2_k64_nov, build_r2_k64_v32, build_r2_k64_v64
        procedure :: init => map_init                   !! Start an empty map for incremental use.
        !> Pre-size for a known number of keys, so a bulk of inserts causes no rehash.
        generic :: reserve => map_reserve_i32, map_reserve_i64
        procedure, private :: map_reserve_i32, map_reserve_i64
        procedure :: clear => map_clear                 !! Forget every key and release all storage.
        procedure :: reset => map_reset                 !! Forget every key, keep the allocation.
        !
        ! ---- Lookup (the hot path; lock-free) ----
        !
        !> The index stored for a key, or 0 if there is none.
        generic :: get => get_k32, get_k64, get_t32, get_t64
        procedure, private :: get_k32, get_k64, get_t32, get_t64
        !> Whether a key is present. Sugar over `%get(...) > 0`.
        generic :: contains => has_k32, has_k64, has_t32, has_t64
        procedure, private :: has_k32, has_k64, has_t32, has_t64
        !> Look up a whole array of keys at once, on a team. The form to prefer in a hot loop.
        generic :: get_many => &
            many_r1_k32_i32, many_r1_k32_i64, many_r1_k64_i32, many_r1_k64_i64, &
            many_r2_k32_i32, many_r2_k32_i64, many_r2_k64_i32, many_r2_k64_i64
        procedure, private :: many_r1_k32_i32, many_r1_k32_i64, many_r1_k64_i32, many_r1_k64_i64
        procedure, private :: many_r2_k32_i32, many_r2_k32_i64, many_r2_k64_i32, many_r2_k64_i64
        !
        ! ---- Mutation (internally serialized) ----
        !
        !> Store a value for a key, inserting or replacing.
        generic :: set => &
            set_k32_v32, set_k32_v64, set_k64_v32, set_k64_v64, &
            set_t32_v32, set_t32_v64, set_t64_v32, set_t64_v64
        procedure, private :: set_k32_v32, set_k32_v64, set_k64_v32, set_k64_v64
        procedure, private :: set_t32_v32, set_t32_v64, set_t64_v32, set_t64_v64
        !> The key's index, assigning it the next unused one if the key is new.
        generic :: get_or_add => &
            goa_k32_i32, goa_k32_i64, goa_k64_i32, goa_k64_i64, &
            goa_t32_i32, goa_t32_i64, goa_t64_i32, goa_t64_i64
        procedure, private :: goa_k32_i32, goa_k32_i64, goa_k64_i32, goa_k64_i64
        procedure, private :: goa_t32_i32, goa_t32_i64, goa_t64_i32, goa_t64_i64
        !> The index of every key in one call, assigning the next unused one to each new key.
        generic :: get_or_add_many => &
            goam_r1_k32_i32, goam_r1_k32_i64, goam_r1_k64_i32, goam_r1_k64_i64, &
            goam_r2_k32_i32, goam_r2_k32_i64, goam_r2_k64_i32, goam_r2_k64_i64
        procedure, private :: goam_r1_k32_i32, goam_r1_k32_i64, goam_r1_k64_i32, goam_r1_k64_i64
        procedure, private :: goam_r2_k32_i32, goam_r2_k32_i64, goam_r2_k64_i32, goam_r2_k64_i64
        !> Forget one key.
        generic :: remove => rm_k32, rm_k64, rm_t32, rm_t64
        procedure, private :: rm_k32, rm_k64, rm_t32, rm_t64
        !
        ! ---- Introspection ----
        !
        procedure :: nkeys => map_nkeys                 !! Keys stored.
        procedure :: ncomponents => map_ncomponents     !! Components per key; 0 if never built.
        procedure :: get_method => map_get_method       !! The resolved backend, as a token.
        procedure :: memory_bytes => map_memory_bytes   !! Heap this map holds, in bytes.
        procedure :: probe_stats => map_probe_stats     !! Hash probe lengths, for tuning and tests.
        !> The stored keys: rank 1 for a single-component map, rank 2 for a composite one.
        generic :: keys => map_keys_r1, map_keys_r2
        procedure, private :: map_keys_r1, map_keys_r2
    end type pf_index_map

    ! ---- pf_index_pool ----

    !> Hands out unique index values 1, 2, 3, ... and takes them back. O(1) either way.
    !!
    !! For a program that manages slots in its own arrays: `%get_index` gives you one nobody else
    !! holds, `%free_index` returns it, and reuse always precedes growth, so the indexes stay as
    !! dense as the live set allows. `%compact` gives back storage a burst of allocation grew and
    !! then hands out the smallest free index first.
    !!
    !! **Every operation is serialized internally, queries included**, so a pool shared between
    !! threads is safe under any interleaving -- one thread taking while another gives back is the
    !! pattern this is for. The cost is that they take turns.
    !!
    !! No finalizer, and the same per-thread-array rule as `pf_index_map` -- see the note there,
    !! which was written from a measurement made on this type.
    type :: pf_index_pool
        private
        !> Highest index ever handed out and not since released by `%compact`. Monotone between
        !! compacts: freeing the top index does NOT lower it, because doing so would have to prune
        !! every free-list entry above the new mark on a path that must stay O(1). `%compact`
        !! lowers it to the highest index actually held.
        integer(int64) :: max_used = 0_int64
        !> Indexes currently handed out. `max_used - n_used` is the free count, in O(1).
        integer(int64) :: n_used = 0_int64
        !> Entries of `flist` in use. The list is a stack: push on free, pop on get.
        integer(int64) :: nfree = 0_int64
        !> Bits `bits` can address, i.e. `64 * size(bits)`.
        integer(int64) :: nbits = 0_int64
        !> One bit per index, set while that index is held. What makes `%free_index` able to reject
        !! a double free and `%is_used` able to answer at all.
        integer(int64), allocatable :: bits(:)
        !> The free list, as a stack of released indexes. LIFO in ordinary operation; `%compact`
        !! rebuilds it so that popping yields the smallest free index first.
        !!
        !! **`nfree` is read before this is touched, which is why an uninitialised copy of this
        !! type is fatal rather than merely wrong.** See the note on `pf_index_map` about
        !! per-thread instances: a `private()` copy under gfortran carries a garbage `nfree`, and
        !! `pool_take` then indexes this array at that offset.
        integer(int64), allocatable :: flist(:)
    contains
        procedure :: get_index => pool_get_index        !! Take an index nobody else holds.
        !> Give an index back.
        generic :: free_index => pool_free_i32, pool_free_i64
        procedure, private :: pool_free_i32, pool_free_i64
        procedure :: get_max_index => pool_get_max      !! Highest index handed out, as a watermark.
        procedure :: get_free_index_count => pool_get_free_count !! Free indexes in `1 .. max_used`.
        procedure :: get_used_count => pool_get_used    !! Indexes currently held.
        !> Whether an index is currently held.
        generic :: is_used => pool_is_used_i32, pool_is_used_i64
        procedure, private :: pool_is_used_i32, pool_is_used_i64
        procedure :: used_indexes => pool_used_indexes  !! Every held index, ascending.
        procedure :: compact => pool_compact            !! Give back grown storage; then smallest-first.
        !> Pre-size for a known number of indexes.
        generic :: reserve => pool_reserve_i32, pool_reserve_i64
        procedure, private :: pool_reserve_i32, pool_reserve_i64
        procedure :: memory_bytes => pool_memory_bytes  !! Heap this pool holds, in bytes.
        procedure :: clear => pool_clear                !! Release every index and all storage.
    end type pf_index_pool

    ! ---- pf_index_multimap ----

    !> Maps a key to EVERY position that holds it: `pf_index_map` with the uniqueness rule relaxed.
    !!
    !! Built from a key array in which keys repeat, it answers "which rows" where the map answers
    !! "which row" -- a contiguous range of stored values per key, ascending by position -- and
    !! answers the same question for a whole probe array at once in the CSR shape `pf_match_all`
    !! returns. It is the m:m half of a hash join, the engine under a table index over a key that
    !! is not unique, and the partition a group-by needs.
    !!
    !! **Storage.** A `pf_index_map` over the DISTINCT keys, whose stored value is a group id in
    !! `1 .. ngroups` (so the map's `>= 1` contract holds and every backend is available, direct
    !! included), and a CSR pair beside it: `offsets(ngroups + 1)` with `offsets(1) = 1`, and
    !! `rows(nkeys)` holding the values of group `g` at `rows(offsets(g) : offsets(g+1) - 1)`,
    !! ascending by position. Values default to the row numbers `1 .. n` or come from `values=`,
    !! are `>= 1` as the map's are, and 0 means "not found" on every lookup.
    !!
    !! **Group ids are dense in `1 .. ngroups` and rows are ascending within a group. Nothing else
    !! about the ids is a contract.** On this version's serial grouping pass they follow first
    !! appearance among the unmasked rows, which a later partitioned pass will change; rely on an
    !! id being stable for the life of one build, never on its order.
    !!
    !! **Threading.** Lookups are lock-free, like the map's. The bulk forms (`%get_first_many`,
    !! `%get_many`, `%probe_many`) open a team of their own by the rule `pf_index_map%get_many`
    !! follows and stand down to serial inside a parallel region. A build or a `%clear` is
    !! serialised on a lock of its own, `pf_index_multimap_guard`, distinct from the map's so that
    !! the map calls made underneath can take theirs. A lookup racing a build of the same
    !! multimap is not supported, for the reason the map's header gives.
    !!
    !! **No finalizer**, for the reason `pf_index_map` gives, and the same per-thread-array rule:
    !! one instance per thread in a shared array allocated before the region.
    type :: pf_index_multimap
        private
        !> The distinct keys, each mapped to its group id in `1 .. ng`.
        type(pf_index_map) :: map
        !> Group offsets into `grows`: group `g` holds `grows(goff(g) : goff(g+1) - 1)`. Length
        !! `ng + 1` with `goff(1) = 1`; allocated only by a build.
        integer(int64), allocatable :: goff(:)
        !> The stored values, grouped by key and ascending by position within a group.
        integer(int64), allocatable :: grows(:)
        !> Groups, i.e. distinct keys stored. 0 before the first build and after `%clear`.
        integer(int64) :: ng = 0_int64
        !> Rows stored, repeats included: the length of `grows`.
        integer(int64) :: nr = 0_int64
        !> Rows in the largest group. 1 means every stored key is unique; 0 when empty.
        integer(int64) :: maxmult = 0_int64
        !> The largest stored value: what decides whether an `int32` answer form may run at all.
        integer(int64) :: vmax = 0_int64
    contains
        !
        ! ---- Lifecycle ----
        !
        !> Build the whole multimap from the keys you have, repeats and all.
        generic :: build => &
            mm_build_r1_k32_nov, mm_build_r1_k32_v32, mm_build_r1_k32_v64, &
            mm_build_r1_k64_nov, mm_build_r1_k64_v32, mm_build_r1_k64_v64, &
            mm_build_r2_k32_nov, mm_build_r2_k32_v32, mm_build_r2_k32_v64, &
            mm_build_r2_k64_nov, mm_build_r2_k64_v32, mm_build_r2_k64_v64
        procedure, private :: mm_build_r1_k32_nov, mm_build_r1_k32_v32, mm_build_r1_k32_v64
        procedure, private :: mm_build_r1_k64_nov, mm_build_r1_k64_v32, mm_build_r1_k64_v64
        procedure, private :: mm_build_r2_k32_nov, mm_build_r2_k32_v32, mm_build_r2_k32_v64
        procedure, private :: mm_build_r2_k64_nov, mm_build_r2_k64_v32, mm_build_r2_k64_v64
        procedure :: clear => mm_clear                    !! Forget every key and release all storage.
        !
        ! ---- Scalar lookup (lock-free; `pure`) ----
        !
        !> The group id of a key, in `1 .. ngroups`; 0 when absent.
        generic :: get => mm_get_k32, mm_get_k64, mm_get_t32, mm_get_t64
        procedure, private :: mm_get_k32, mm_get_k64, mm_get_t32, mm_get_t64
        !> How many stored rows hold a key; 0 when absent.
        generic :: count => mm_count_k32, mm_count_k64, mm_count_t32, mm_count_t64
        procedure, private :: mm_count_k32, mm_count_k64, mm_count_t32, mm_count_t64
        !> The value at the lowest position holding a key; 0 when absent. The m:1 answer.
        generic :: get_first => mm_first_k32, mm_first_k64, mm_first_t32, mm_first_t64
        procedure, private :: mm_first_k32, mm_first_k64, mm_first_t32, mm_first_t64
        !> Every value stored for a key, ascending by position; zero-length when absent.
        generic :: get_all => &
            mm_all_k32_i32, mm_all_k32_i64, mm_all_k64_i32, mm_all_k64_i64, &
            mm_all_t32_i32, mm_all_t32_i64, mm_all_t64_i32, mm_all_t64_i64
        procedure, private :: mm_all_k32_i32, mm_all_k32_i64, mm_all_k64_i32, mm_all_k64_i64
        procedure, private :: mm_all_t32_i32, mm_all_t32_i64, mm_all_t64_i32, mm_all_t64_i64
        !> The range of `%csr`'s `rows` that holds a key; `lo > hi` when absent.
        generic :: get_range => mm_range_k32, mm_range_k64, mm_range_t32, mm_range_t64
        procedure, private :: mm_range_k32, mm_range_k64, mm_range_t32, mm_range_t64
        !
        ! ---- Bulk lookup (lock-free; on a team of their own) ----
        !
        !> `%get_first` over a whole array of keys, on a team. The m:1 form for a hot loop.
        generic :: get_first_many => &
            mm_fmany_r1_k32_i32, mm_fmany_r1_k32_i64, mm_fmany_r1_k64_i32, mm_fmany_r1_k64_i64, &
            mm_fmany_r2_k32_i32, mm_fmany_r2_k32_i64, mm_fmany_r2_k64_i32, mm_fmany_r2_k64_i64
        procedure, private :: mm_fmany_r1_k32_i32, mm_fmany_r1_k32_i64
        procedure, private :: mm_fmany_r1_k64_i32, mm_fmany_r1_k64_i64
        procedure, private :: mm_fmany_r2_k32_i32, mm_fmany_r2_k32_i64
        procedure, private :: mm_fmany_r2_k64_i32, mm_fmany_r2_k64_i64
        !> `%get` over a whole array of keys, on a team: the group id per key.
        generic :: get_many => &
            mm_many_r1_k32_i32, mm_many_r1_k32_i64, mm_many_r1_k64_i32, mm_many_r1_k64_i64, &
            mm_many_r2_k32_i32, mm_many_r2_k32_i64, mm_many_r2_k64_i32, mm_many_r2_k64_i64
        procedure, private :: mm_many_r1_k32_i32, mm_many_r1_k32_i64
        procedure, private :: mm_many_r1_k64_i32, mm_many_r1_k64_i64
        procedure, private :: mm_many_r2_k32_i32, mm_many_r2_k32_i64
        procedure, private :: mm_many_r2_k64_i32, mm_many_r2_k64_i64
        !> EVERY match between an array of probe keys and the stored keys, as a CSR pair.
        generic :: probe_many => &
            mm_probe_r1_k32_i32, mm_probe_r1_k32_i64, mm_probe_r1_k64_i32, mm_probe_r1_k64_i64, &
            mm_probe_r2_k32_i32, mm_probe_r2_k32_i64, mm_probe_r2_k64_i32, mm_probe_r2_k64_i64
        procedure, private :: mm_probe_r1_k32_i32, mm_probe_r1_k32_i64
        procedure, private :: mm_probe_r1_k64_i32, mm_probe_r1_k64_i64
        procedure, private :: mm_probe_r2_k32_i32, mm_probe_r2_k32_i64
        procedure, private :: mm_probe_r2_k64_i32, mm_probe_r2_k64_i64
        !
        ! ---- Introspection ----
        !
        procedure :: csr => mm_csr                        !! The CSR pair, copied out.
        procedure :: ngroups => mm_ngroups                !! Distinct keys stored.
        procedure :: nkeys => mm_nkeys                    !! Rows stored, repeats included.
        procedure :: ncomponents => mm_ncomponents        !! Components per key; 0 if never built.
        procedure :: max_multiplicity => mm_max_multiplicity !! Rows in the largest group.
        procedure :: memory_bytes => mm_memory_bytes      !! Heap this multimap holds, in bytes.
        procedure :: get_method => mm_get_method          !! The distinct-key map's backend, as a token.
        !> The distinct keys: rank 1 for a single-component multimap, rank 2 for a composite one.
        generic :: keys => mm_keys_r1, mm_keys_r2
        procedure, private :: mm_keys_r1, mm_keys_r2
    end type pf_index_multimap

    ! ============================================================================================
    ! Implementations. Every one lives in a submodule; the abbreviated `module procedure NAME`
    ! form there is exempt from restating these argument docs, so this file is their one home.
    ! ============================================================================================

    ! ---- pf_index_map: bulk build (12 specifics -- key kind x key rank x values kind/absent) ----
    !
    ! `values` is OPTIONAL in the API and REQUIRED in each specific that takes it, with a separate
    ! no-values specific per key shape. That split is forced rather than chosen: two specifics
    ! differing only in the KIND of an OPTIONAL argument are not distinguishable (F2018
    ! 15.4.3.4.5), so the natural single `[values(:)]` spelling is rejected as ambiguous. This is
    ! the `parquet_open_reader_base`/`_nrows_int32`/`_int64` shape, for the same reason.

    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r1_k32_nov(self, keys, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:) !! one key per element; every unmasked key must be unique.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r1_k32_nov
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r1_k32_v32(self, keys, values, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:) !! one key per element; every unmasked key must be unique.
        integer(int32), intent(in) :: values(:)
            !! index value per key, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r1_k32_v32
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r1_k32_v64(self, keys, values, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:) !! one key per element; every unmasked key must be unique.
        integer(int64), intent(in) :: values(:)
            !! index value per key, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r1_k32_v64
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r1_k64_nov(self, keys, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:) !! one key per element; every unmasked key must be unique.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r1_k64_nov
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r1_k64_v32(self, keys, values, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:) !! one key per element; every unmasked key must be unique.
        integer(int32), intent(in) :: values(:)
            !! index value per key, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r1_k64_v32
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r1_k64_v64(self, keys, values, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:) !! one key per element; every unmasked key must be unique.
        integer(int64), intent(in) :: values(:)
            !! index value per key, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r1_k64_v64
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r2_k32_nov(self, keys, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Every unmasked ROW
            !! must be unique; the individual columns may repeat freely.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r2_k32_nov
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r2_k32_v32(self, keys, values, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Every unmasked ROW
            !! must be unique; the individual columns may repeat freely.
        integer(int32), intent(in) :: values(:)
            !! index value per key, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r2_k32_v32
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r2_k32_v64(self, keys, values, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Every unmasked ROW
            !! must be unique; the individual columns may repeat freely.
        integer(int64), intent(in) :: values(:)
            !! index value per key, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r2_k32_v64
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r2_k64_nov(self, keys, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Every unmasked ROW
            !! must be unique; the individual columns may repeat freely.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r2_k64_nov
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r2_k64_v32(self, keys, values, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Every unmasked ROW
            !! must be unique; the individual columns may repeat freely.
        integer(int32), intent(in) :: values(:)
            !! index value per key, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r2_k64_v32
    end interface
    interface
        !> Builds the map from `keys`, replacing whatever it held.
        !!
        !! Rebuilding is allowed and needs no guard: this releases and reconstructs, so it is
        !! idempotent by reconstruction. A duplicate key aborts, naming it. With `valid=`, a
        !! masked row is skipped entirely: the stored values stay the ROW NUMBERS of the unmasked
        !! rows (or their `values=` entries), which is what lets a nullable key column be indexed
        !! without compacting it first.
        module subroutine build_r2_k64_v64(self, keys, values, method, threads, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Every unmasked ROW
            !! must be unique; the individual columns may repeat freely.
        integer(int64), intent(in) :: values(:)
            !! index value per key, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token: "auto" (default), "direct", "hash" or "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. ABSENT means automatic: bounded by the work, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller
            !! is already inside an OpenMP parallel region. `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted, and whose key may repeat one that is stored. Last in the list, so
            !! that an existing positional call is unaffected.
        end subroutine build_r2_k64_v64
    end interface

    ! ---- pf_index_map: incremental lifecycle ----

    interface
        !> Starts an empty map for incremental use through `%set`/`%get_or_add`.
        !!
        !! Only `"hash"` (and `"auto"`, which resolves to it) is accepted: the direct backend needs
        !! the key range up front and the sorted backend is frozen once built, so both are
        !! build-only.
        module subroutine map_init(self, capacity, method, ncomp)
        class(pf_index_map), intent(inout) :: self !! the map; reset to empty.
            integer, intent(in), optional :: capacity
            !! keys to pre-size for. A hint, so deliberately a single default-kind `integer` rather
            !! than a kind-generic pair -- an optional argument cannot be kind-generic at all, and
            !! `%reserve(n)` is the kind-generic route to a capacity above `huge(1_int32)`.
            character(len=*), intent(in), optional :: method
            !! backend token; only "auto" (default) and "hash" are accepted here.
            integer, intent(in), optional :: ncomp
            !! components per key, 1 by default, at most `pf_index_max_components`. Fixed for the
            !! map's lifetime: every later call must present a key of exactly this width.
        end subroutine map_init
    end interface
    interface
        !> Pre-sizes for `n` keys, so that a run of inserts up to `n` causes no rehash.
        module subroutine map_reserve_i32(self, n)
        class(pf_index_map), intent(inout) :: self !! the map.
            integer(int32), intent(in) :: n !! keys to make room for; must be >= 0.
        end subroutine map_reserve_i32
        !> Pre-sizes for `n` keys. See `map_reserve_i32`.
        module subroutine map_reserve_i64(self, n)
        class(pf_index_map), intent(inout) :: self !! the map.
            integer(int64), intent(in) :: n !! keys to make room for; must be >= 0.
        end subroutine map_reserve_i64
    end interface
    interface
        !> Forgets every key and releases all storage.
        !!
        !! **This RELEASES, matching `parquet_column%clear` and `parquet_string_column%clear`**, so
        !! a reader who knows those types is not surprised. `%reset` is the one that keeps the
        !! allocation. Afterwards the map is exactly as a fresh one: `%get` answers 0 for every
        !! key, because the backend selector and the empty direct range are restored too.
        module subroutine map_clear(self)
        class(pf_index_map), intent(inout) :: self !! the map.
        end subroutine map_clear
        !> Forgets every key but keeps the allocation, so refilling causes no reallocation.
        !!
        !! For a map rebuilt every iteration of an outer loop: this saves the table's allocation
        !! and, on the hash backend, its rehash. `ncomp` and the backend survive; the keys do not.
        !! A map that was never built is left alone.
        module subroutine map_reset(self)
        class(pf_index_map), intent(inout) :: self !! the map.
        end subroutine map_reset
    end interface

    ! ---- pf_index_map: lookup. Allocation-free and never guarded -- see the module's own
    ! header for the one access pattern that makes that unsafe. The scalar forms are `pure`;
    ! the bulk form is not, because it opens an OpenMP team of its own. ----

    interface
        !> The index stored for `key`, or **0 when the key is absent**.
        !!
        !! Answers 0 rather than aborting on a map that was never built, at no cost to the
        !! built path: the default empty direct range fails its own test, so there is no
        !! "am I built?" branch here at all.
        pure module function get_k32(self, key) result(idx)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int32), intent(in) :: key !! the key to look up.
            integer(int64) :: idx !! the stored value, >= 1, or 0 when not found.
        end function get_k32
    end interface
    interface
        !> The index stored for `key`, or **0 when the key is absent**.
        !!
        !! Answers 0 rather than aborting on a map that was never built, at no cost to the
        !! built path: the default empty direct range fails its own test, so there is no
        !! "am I built?" branch here at all.
        pure module function get_k64(self, key) result(idx)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: key !! the key to look up.
            integer(int64) :: idx !! the stored value, >= 1, or 0 when not found.
        end function get_k64
    end interface
    interface
        !> The index stored for `key`, or **0 when the key is absent**.
        !!
        !! Answers 0 rather than aborting on a map that was never built, at no cost to the
        !! built path: the default empty direct range fails its own test, so there is no
        !! "am I built?" branch here at all.
        pure module function get_t32(self, key) result(idx)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64) :: idx !! the stored value, >= 1, or 0 when not found.
        end function get_t32
    end interface
    interface
        !> The index stored for `key`, or **0 when the key is absent**.
        !!
        !! Answers 0 rather than aborting on a map that was never built, at no cost to the
        !! built path: the default empty direct range fails its own test, so there is no
        !! "am I built?" branch here at all.
        pure module function get_t64(self, key) result(idx)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64) :: idx !! the stored value, >= 1, or 0 when not found.
        end function get_t64
    end interface
    interface
        !> Whether `key` is present. Exactly `%get(key) > 0`, spelled for readability.
        pure module function has_k32(self, key) result(ok)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int32), intent(in) :: key !! the key to test.
            logical :: ok !! `.true.` when the key is stored.
        end function has_k32
    end interface
    interface
        !> Whether `key` is present. Exactly `%get(key) > 0`, spelled for readability.
        pure module function has_k64(self, key) result(ok)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: key !! the key to test.
            logical :: ok !! `.true.` when the key is stored.
        end function has_k64
    end interface
    interface
        !> Whether `key` is present. Exactly `%get(key) > 0`, spelled for readability.
        pure module function has_t32(self, key) result(ok)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            logical :: ok !! `.true.` when the key is stored.
        end function has_t32
    end interface
    interface
        !> Whether `key` is present. Exactly `%get(key) > 0`, spelled for readability.
        pure module function has_t64(self, key) result(ok)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            logical :: ok !! `.true.` when the key is stored.
        end function has_t64
    end interface
    interface
        !> Looks up a whole array of keys, writing 0 for each one absent.
        !!
        !! **The form to prefer in a hot loop.** It converts the map object once per call
        !! rather than once per key, which under ifx is the difference between paying for a
        !! runtime class descriptor per lookup and paying for one per array -- and it threads
        !! over the keys by the rule a build follows (`pf_index_threads`), so a large probe runs
        !! on a team while a small one, or one made from inside a parallel region, runs serially.
        !! Lock-free and allocation-free at every team size. Not `pure`, because an OpenMP
        !! directive may not appear in a pure procedure; the scalar `%get` and `%contains` are.
        module subroutine many_r1_k32_i32(self, keys, indexes, valid, threads)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int32), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int32), intent(out) :: indexes(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
        end subroutine many_r1_k32_i32
    end interface
    interface
        !> Looks up a whole array of keys, writing 0 for each one absent.
        !!
        !! **The form to prefer in a hot loop.** It converts the map object once per call
        !! rather than once per key, which under ifx is the difference between paying for a
        !! runtime class descriptor per lookup and paying for one per array -- and it threads
        !! over the keys by the rule a build follows (`pf_index_threads`), so a large probe runs
        !! on a team while a small one, or one made from inside a parallel region, runs serially.
        !! Lock-free and allocation-free at every team size. Not `pure`, because an OpenMP
        !! directive may not appear in a pure procedure; the scalar `%get` and `%contains` are.
        module subroutine many_r1_k32_i64(self, keys, indexes, valid, threads)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int32), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int64), intent(out) :: indexes(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
        end subroutine many_r1_k32_i64
    end interface
    interface
        !> Looks up a whole array of keys, writing 0 for each one absent.
        !!
        !! **The form to prefer in a hot loop.** It converts the map object once per call
        !! rather than once per key, which under ifx is the difference between paying for a
        !! runtime class descriptor per lookup and paying for one per array -- and it threads
        !! over the keys by the rule a build follows (`pf_index_threads`), so a large probe runs
        !! on a team while a small one, or one made from inside a parallel region, runs serially.
        !! Lock-free and allocation-free at every team size. Not `pure`, because an OpenMP
        !! directive may not appear in a pure procedure; the scalar `%get` and `%contains` are.
        module subroutine many_r1_k64_i32(self, keys, indexes, valid, threads)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int32), intent(out) :: indexes(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
        end subroutine many_r1_k64_i32
    end interface
    interface
        !> Looks up a whole array of keys, writing 0 for each one absent.
        !!
        !! **The form to prefer in a hot loop.** It converts the map object once per call
        !! rather than once per key, which under ifx is the difference between paying for a
        !! runtime class descriptor per lookup and paying for one per array -- and it threads
        !! over the keys by the rule a build follows (`pf_index_threads`), so a large probe runs
        !! on a team while a small one, or one made from inside a parallel region, runs serially.
        !! Lock-free and allocation-free at every team size. Not `pure`, because an OpenMP
        !! directive may not appear in a pure procedure; the scalar `%get` and `%contains` are.
        module subroutine many_r1_k64_i64(self, keys, indexes, valid, threads)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int64), intent(out) :: indexes(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
        end subroutine many_r1_k64_i64
    end interface
    interface
        !> Looks up a whole array of keys, writing 0 for each one absent.
        !!
        !! **The form to prefer in a hot loop.** It converts the map object once per call
        !! rather than once per key, which under ifx is the difference between paying for a
        !! runtime class descriptor per lookup and paying for one per array -- and it threads
        !! over the keys by the rule a build follows (`pf_index_threads`), so a large probe runs
        !! on a team while a small one, or one made from inside a parallel region, runs serially.
        !! Lock-free and allocation-free at every team size. Not `pure`, because an OpenMP
        !! directive may not appear in a pure procedure; the scalar `%get` and `%contains` are.
        module subroutine many_r2_k32_i32(self, keys, indexes, valid, threads)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples, one per ROW, shaped `(n, ncomp)`.
            integer(int32), intent(out) :: indexes(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
        end subroutine many_r2_k32_i32
    end interface
    interface
        !> Looks up a whole array of keys, writing 0 for each one absent.
        !!
        !! **The form to prefer in a hot loop.** It converts the map object once per call
        !! rather than once per key, which under ifx is the difference between paying for a
        !! runtime class descriptor per lookup and paying for one per array -- and it threads
        !! over the keys by the rule a build follows (`pf_index_threads`), so a large probe runs
        !! on a team while a small one, or one made from inside a parallel region, runs serially.
        !! Lock-free and allocation-free at every team size. Not `pure`, because an OpenMP
        !! directive may not appear in a pure procedure; the scalar `%get` and `%contains` are.
        module subroutine many_r2_k32_i64(self, keys, indexes, valid, threads)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples, one per ROW, shaped `(n, ncomp)`.
            integer(int64), intent(out) :: indexes(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
        end subroutine many_r2_k32_i64
    end interface
    interface
        !> Looks up a whole array of keys, writing 0 for each one absent.
        !!
        !! **The form to prefer in a hot loop.** It converts the map object once per call
        !! rather than once per key, which under ifx is the difference between paying for a
        !! runtime class descriptor per lookup and paying for one per array -- and it threads
        !! over the keys by the rule a build follows (`pf_index_threads`), so a large probe runs
        !! on a team while a small one, or one made from inside a parallel region, runs serially.
        !! Lock-free and allocation-free at every team size. Not `pure`, because an OpenMP
        !! directive may not appear in a pure procedure; the scalar `%get` and `%contains` are.
        module subroutine many_r2_k64_i32(self, keys, indexes, valid, threads)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples, one per ROW, shaped `(n, ncomp)`.
            integer(int32), intent(out) :: indexes(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
        end subroutine many_r2_k64_i32
    end interface
    interface
        !> Looks up a whole array of keys, writing 0 for each one absent.
        !!
        !! **The form to prefer in a hot loop.** It converts the map object once per call
        !! rather than once per key, which under ifx is the difference between paying for a
        !! runtime class descriptor per lookup and paying for one per array -- and it threads
        !! over the keys by the rule a build follows (`pf_index_threads`), so a large probe runs
        !! on a team while a small one, or one made from inside a parallel region, runs serially.
        !! Lock-free and allocation-free at every team size. Not `pure`, because an OpenMP
        !! directive may not appear in a pure procedure; the scalar `%get` and `%contains` are.
        module subroutine many_r2_k64_i64(self, keys, indexes, valid, threads)
        class(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples, one per ROW, shaped `(n, ncomp)`.
            integer(int64), intent(out) :: indexes(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
        end subroutine many_r2_k64_i64
    end interface

    ! ---- pf_index_map: mutation. Every one of these is serialized internally, so several threads
    ! may mutate one shared map at once. ----

    interface
        !> Stores `value` for `key`, inserting it or replacing what was there.
        !!
        !! Raises the `%get_or_add` watermark to `value` when that is larger, so a map filled
        !! by hand and then extended automatically never reissues an index.
        module subroutine set_k32_v32(self, key, value)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key !! the key to store under.
            integer(int32), intent(in) :: value !! the index value to store; must be >= 1.
        end subroutine set_k32_v32
    end interface
    interface
        !> Stores `value` for `key`, inserting it or replacing what was there.
        !!
        !! Raises the `%get_or_add` watermark to `value` when that is larger, so a map filled
        !! by hand and then extended automatically never reissues an index.
        module subroutine set_k32_v64(self, key, value)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key !! the key to store under.
            integer(int64), intent(in) :: value !! the index value to store; must be >= 1.
        end subroutine set_k32_v64
    end interface
    interface
        !> Stores `value` for `key`, inserting it or replacing what was there.
        !!
        !! Raises the `%get_or_add` watermark to `value` when that is larger, so a map filled
        !! by hand and then extended automatically never reissues an index.
        module subroutine set_k64_v32(self, key, value)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key !! the key to store under.
            integer(int32), intent(in) :: value !! the index value to store; must be >= 1.
        end subroutine set_k64_v32
    end interface
    interface
        !> Stores `value` for `key`, inserting it or replacing what was there.
        !!
        !! Raises the `%get_or_add` watermark to `value` when that is larger, so a map filled
        !! by hand and then extended automatically never reissues an index.
        module subroutine set_k64_v64(self, key, value)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key !! the key to store under.
            integer(int64), intent(in) :: value !! the index value to store; must be >= 1.
        end subroutine set_k64_v64
    end interface
    interface
        !> Stores `value` for `key`, inserting it or replacing what was there.
        !!
        !! Raises the `%get_or_add` watermark to `value` when that is larger, so a map filled
        !! by hand and then extended automatically never reissues an index.
        module subroutine set_t32_v32(self, key, value)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int32), intent(in) :: value !! the index value to store; must be >= 1.
        end subroutine set_t32_v32
    end interface
    interface
        !> Stores `value` for `key`, inserting it or replacing what was there.
        !!
        !! Raises the `%get_or_add` watermark to `value` when that is larger, so a map filled
        !! by hand and then extended automatically never reissues an index.
        module subroutine set_t32_v64(self, key, value)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64), intent(in) :: value !! the index value to store; must be >= 1.
        end subroutine set_t32_v64
    end interface
    interface
        !> Stores `value` for `key`, inserting it or replacing what was there.
        !!
        !! Raises the `%get_or_add` watermark to `value` when that is larger, so a map filled
        !! by hand and then extended automatically never reissues an index.
        module subroutine set_t64_v32(self, key, value)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int32), intent(in) :: value !! the index value to store; must be >= 1.
        end subroutine set_t64_v32
    end interface
    interface
        !> Stores `value` for `key`, inserting it or replacing what was there.
        !!
        !! Raises the `%get_or_add` watermark to `value` when that is larger, so a map filled
        !! by hand and then extended automatically never reissues an index.
        module subroutine set_t64_v64(self, key, value)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64), intent(in) :: value !! the index value to store; must be >= 1.
        end subroutine set_t64_v64
    end interface
    interface
        !> The index for `key`, assigning and storing the next unused one if it is new.
        !!
        !! The dictionary-encoding primitive: streaming `n` keys through it yields dense
        !! indexes `1 .. k` for the `k` distinct keys, in first-appearance order. Because it
        !! is serialized internally, several threads may stream through one shared map and
        !! each still gets a unique, stable index.
        module subroutine goa_k32_i32(self, key, idx)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key !! the key to look up or add.
            integer(int32), intent(out) :: idx !! the key's index; >= 1 always.
        end subroutine goa_k32_i32
    end interface
    interface
        !> The index for `key`, assigning and storing the next unused one if it is new.
        !!
        !! The dictionary-encoding primitive: streaming `n` keys through it yields dense
        !! indexes `1 .. k` for the `k` distinct keys, in first-appearance order. Because it
        !! is serialized internally, several threads may stream through one shared map and
        !! each still gets a unique, stable index.
        module subroutine goa_k32_i64(self, key, idx)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key !! the key to look up or add.
            integer(int64), intent(out) :: idx !! the key's index; >= 1 always.
        end subroutine goa_k32_i64
    end interface
    interface
        !> The index for `key`, assigning and storing the next unused one if it is new.
        !!
        !! The dictionary-encoding primitive: streaming `n` keys through it yields dense
        !! indexes `1 .. k` for the `k` distinct keys, in first-appearance order. Because it
        !! is serialized internally, several threads may stream through one shared map and
        !! each still gets a unique, stable index.
        module subroutine goa_k64_i32(self, key, idx)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key !! the key to look up or add.
            integer(int32), intent(out) :: idx !! the key's index; >= 1 always.
        end subroutine goa_k64_i32
    end interface
    interface
        !> The index for `key`, assigning and storing the next unused one if it is new.
        !!
        !! The dictionary-encoding primitive: streaming `n` keys through it yields dense
        !! indexes `1 .. k` for the `k` distinct keys, in first-appearance order. Because it
        !! is serialized internally, several threads may stream through one shared map and
        !! each still gets a unique, stable index.
        module subroutine goa_k64_i64(self, key, idx)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key !! the key to look up or add.
            integer(int64), intent(out) :: idx !! the key's index; >= 1 always.
        end subroutine goa_k64_i64
    end interface
    interface
        !> The index for `key`, assigning and storing the next unused one if it is new.
        !!
        !! The dictionary-encoding primitive: streaming `n` keys through it yields dense
        !! indexes `1 .. k` for the `k` distinct keys, in first-appearance order. Because it
        !! is serialized internally, several threads may stream through one shared map and
        !! each still gets a unique, stable index.
        module subroutine goa_t32_i32(self, key, idx)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int32), intent(out) :: idx !! the key's index; >= 1 always.
        end subroutine goa_t32_i32
    end interface
    interface
        !> The index for `key`, assigning and storing the next unused one if it is new.
        !!
        !! The dictionary-encoding primitive: streaming `n` keys through it yields dense
        !! indexes `1 .. k` for the `k` distinct keys, in first-appearance order. Because it
        !! is serialized internally, several threads may stream through one shared map and
        !! each still gets a unique, stable index.
        module subroutine goa_t32_i64(self, key, idx)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64), intent(out) :: idx !! the key's index; >= 1 always.
        end subroutine goa_t32_i64
    end interface
    interface
        !> The index for `key`, assigning and storing the next unused one if it is new.
        !!
        !! The dictionary-encoding primitive: streaming `n` keys through it yields dense
        !! indexes `1 .. k` for the `k` distinct keys, in first-appearance order. Because it
        !! is serialized internally, several threads may stream through one shared map and
        !! each still gets a unique, stable index.
        module subroutine goa_t64_i32(self, key, idx)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int32), intent(out) :: idx !! the key's index; >= 1 always.
        end subroutine goa_t64_i32
    end interface
    interface
        !> The index for `key`, assigning and storing the next unused one if it is new.
        !!
        !! The dictionary-encoding primitive: streaming `n` keys through it yields dense
        !! indexes `1 .. k` for the `k` distinct keys, in first-appearance order. Because it
        !! is serialized internally, several threads may stream through one shared map and
        !! each still gets a unique, stable index.
        module subroutine goa_t64_i64(self, key, idx)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64), intent(out) :: idx !! the key's index; >= 1 always.
        end subroutine goa_t64_i64
    end interface
    interface
        !> The index of every key in one call, assigning and storing the next unused one for
        !> each key that is new.
        !!
        !! `%get_or_add` over a whole array, under the map's guard once rather than once per key:
        !! the bulk dictionary-encoding primitive, and the one to factorise a key column with. A
        !! row masked off by `valid` gets the code 0 and is neither looked up nor added. The
        !! codes of one call are dense -- the `k` keys new to the map take the `k` values above
        !! its watermark -- and on this serial path they are assigned in first-appearance order;
        !! rely on a code being stable within the call rather than on that order, which a
        !! partitioned build may later change.
        module subroutine goam_r1_k32_i32(self, keys, codes, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:) !! the keys to look up or add, one per element.
            integer(int32), intent(out) :: codes(:)
            !! one index per key, >= 1, or 0 for a masked row. Must be exactly as long as
            !! `keys` has rows. A code above `huge(int32)` aborts rather than
            !! truncating; take the codes as `int64` if the map's values can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry is neither looked up nor added.
        end subroutine goam_r1_k32_i32
    end interface
    interface
        !> The index of every key in one call, assigning and storing the next unused one for
        !> each key that is new.
        !!
        !! `%get_or_add` over a whole array, under the map's guard once rather than once per key:
        !! the bulk dictionary-encoding primitive, and the one to factorise a key column with. A
        !! row masked off by `valid` gets the code 0 and is neither looked up nor added. The
        !! codes of one call are dense -- the `k` keys new to the map take the `k` values above
        !! its watermark -- and on this serial path they are assigned in first-appearance order;
        !! rely on a code being stable within the call rather than on that order, which a
        !! partitioned build may later change.
        module subroutine goam_r1_k32_i64(self, keys, codes, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:) !! the keys to look up or add, one per element.
            integer(int64), intent(out) :: codes(:)
            !! one index per key, >= 1, or 0 for a masked row. Must be exactly as long as
            !! `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry is neither looked up nor added.
        end subroutine goam_r1_k32_i64
    end interface
    interface
        !> The index of every key in one call, assigning and storing the next unused one for
        !> each key that is new.
        !!
        !! `%get_or_add` over a whole array, under the map's guard once rather than once per key:
        !! the bulk dictionary-encoding primitive, and the one to factorise a key column with. A
        !! row masked off by `valid` gets the code 0 and is neither looked up nor added. The
        !! codes of one call are dense -- the `k` keys new to the map take the `k` values above
        !! its watermark -- and on this serial path they are assigned in first-appearance order;
        !! rely on a code being stable within the call rather than on that order, which a
        !! partitioned build may later change.
        module subroutine goam_r1_k64_i32(self, keys, codes, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:) !! the keys to look up or add, one per element.
            integer(int32), intent(out) :: codes(:)
            !! one index per key, >= 1, or 0 for a masked row. Must be exactly as long as
            !! `keys` has rows. A code above `huge(int32)` aborts rather than
            !! truncating; take the codes as `int64` if the map's values can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry is neither looked up nor added.
        end subroutine goam_r1_k64_i32
    end interface
    interface
        !> The index of every key in one call, assigning and storing the next unused one for
        !> each key that is new.
        !!
        !! `%get_or_add` over a whole array, under the map's guard once rather than once per key:
        !! the bulk dictionary-encoding primitive, and the one to factorise a key column with. A
        !! row masked off by `valid` gets the code 0 and is neither looked up nor added. The
        !! codes of one call are dense -- the `k` keys new to the map take the `k` values above
        !! its watermark -- and on this serial path they are assigned in first-appearance order;
        !! rely on a code being stable within the call rather than on that order, which a
        !! partitioned build may later change.
        module subroutine goam_r1_k64_i64(self, keys, codes, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:) !! the keys to look up or add, one per element.
            integer(int64), intent(out) :: codes(:)
            !! one index per key, >= 1, or 0 for a masked row. Must be exactly as long as
            !! `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry is neither looked up nor added.
        end subroutine goam_r1_k64_i64
    end interface
    interface
        !> The index of every key in one call, assigning and storing the next unused one for
        !> each key that is new.
        !!
        !! `%get_or_add` over a whole array, under the map's guard once rather than once per key:
        !! the bulk dictionary-encoding primitive, and the one to factorise a key column with. A
        !! row masked off by `valid` gets the code 0 and is neither looked up nor added. The
        !! codes of one call are dense -- the `k` keys new to the map take the `k` values above
        !! its watermark -- and on this serial path they are assigned in first-appearance order;
        !! rely on a code being stable within the call rather than on that order, which a
        !! partitioned build may later change.
        module subroutine goam_r2_k32_i32(self, keys, codes, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples to look up or add, one per ROW, shaped `(n, ncomp)`.
            integer(int32), intent(out) :: codes(:)
            !! one index per key, >= 1, or 0 for a masked row. Must be exactly as long as
            !! `keys` has rows. A code above `huge(int32)` aborts rather than
            !! truncating; take the codes as `int64` if the map's values can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry is neither looked up nor added.
        end subroutine goam_r2_k32_i32
    end interface
    interface
        !> The index of every key in one call, assigning and storing the next unused one for
        !> each key that is new.
        !!
        !! `%get_or_add` over a whole array, under the map's guard once rather than once per key:
        !! the bulk dictionary-encoding primitive, and the one to factorise a key column with. A
        !! row masked off by `valid` gets the code 0 and is neither looked up nor added. The
        !! codes of one call are dense -- the `k` keys new to the map take the `k` values above
        !! its watermark -- and on this serial path they are assigned in first-appearance order;
        !! rely on a code being stable within the call rather than on that order, which a
        !! partitioned build may later change.
        module subroutine goam_r2_k32_i64(self, keys, codes, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples to look up or add, one per ROW, shaped `(n, ncomp)`.
            integer(int64), intent(out) :: codes(:)
            !! one index per key, >= 1, or 0 for a masked row. Must be exactly as long as
            !! `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry is neither looked up nor added.
        end subroutine goam_r2_k32_i64
    end interface
    interface
        !> The index of every key in one call, assigning and storing the next unused one for
        !> each key that is new.
        !!
        !! `%get_or_add` over a whole array, under the map's guard once rather than once per key:
        !! the bulk dictionary-encoding primitive, and the one to factorise a key column with. A
        !! row masked off by `valid` gets the code 0 and is neither looked up nor added. The
        !! codes of one call are dense -- the `k` keys new to the map take the `k` values above
        !! its watermark -- and on this serial path they are assigned in first-appearance order;
        !! rely on a code being stable within the call rather than on that order, which a
        !! partitioned build may later change.
        module subroutine goam_r2_k64_i32(self, keys, codes, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples to look up or add, one per ROW, shaped `(n, ncomp)`.
            integer(int32), intent(out) :: codes(:)
            !! one index per key, >= 1, or 0 for a masked row. Must be exactly as long as
            !! `keys` has rows. A code above `huge(int32)` aborts rather than
            !! truncating; take the codes as `int64` if the map's values can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry is neither looked up nor added.
        end subroutine goam_r2_k64_i32
    end interface
    interface
        !> The index of every key in one call, assigning and storing the next unused one for
        !> each key that is new.
        !!
        !! `%get_or_add` over a whole array, under the map's guard once rather than once per key:
        !! the bulk dictionary-encoding primitive, and the one to factorise a key column with. A
        !! row masked off by `valid` gets the code 0 and is neither looked up nor added. The
        !! codes of one call are dense -- the `k` keys new to the map take the `k` values above
        !! its watermark -- and on this serial path they are assigned in first-appearance order;
        !! rely on a code being stable within the call rather than on that order, which a
        !! partitioned build may later change.
        module subroutine goam_r2_k64_i64(self, keys, codes, valid)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples to look up or add, one per ROW, shaped `(n, ncomp)`.
            integer(int64), intent(out) :: codes(:)
            !! one index per key, >= 1, or 0 for a masked row. Must be exactly as long as
            !! `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry is neither looked up nor added.
        end subroutine goam_r2_k64_i64
    end interface
    interface
        !> Forgets one key.
        !!
        !! Without `found`, removing a key that is not there aborts; with it, absence is
        !! reported instead. Never lowers the `%get_or_add` watermark: reissuing an index is
        !! `pf_index_pool`'s business, not a map's.
        module subroutine rm_k32(self, key, found)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key !! the key to remove.
            logical, intent(out), optional :: found
            !! `.true.` when the key was present. When absent from the call, an absent key aborts.
        end subroutine rm_k32
    end interface
    interface
        !> Forgets one key.
        !!
        !! Without `found`, removing a key that is not there aborts; with it, absence is
        !! reported instead. Never lowers the `%get_or_add` watermark: reissuing an index is
        !! `pf_index_pool`'s business, not a map's.
        module subroutine rm_k64(self, key, found)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key !! the key to remove.
            logical, intent(out), optional :: found
            !! `.true.` when the key was present. When absent from the call, an absent key aborts.
        end subroutine rm_k64
    end interface
    interface
        !> Forgets one key.
        !!
        !! Without `found`, removing a key that is not there aborts; with it, absence is
        !! reported instead. Never lowers the `%get_or_add` watermark: reissuing an index is
        !! `pf_index_pool`'s business, not a map's.
        module subroutine rm_t32(self, key, found)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            logical, intent(out), optional :: found
            !! `.true.` when the key was present. When absent from the call, an absent key aborts.
        end subroutine rm_t32
    end interface
    interface
        !> Forgets one key.
        !!
        !! Without `found`, removing a key that is not there aborts; with it, absence is
        !! reported instead. Never lowers the `%get_or_add` watermark: reissuing an index is
        !! `pf_index_pool`'s business, not a map's.
        module subroutine rm_t64(self, key, found)
        class(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            logical, intent(out), optional :: found
            !! `.true.` when the key was present. When absent from the call, an absent key aborts.
        end subroutine rm_t64
    end interface

    ! ---- pf_index_map: introspection ----

    interface
        !> Keys stored.
        pure module function map_nkeys(self) result(n)
        class(pf_index_map), intent(in) :: self !! the map.
            integer(int64) :: n !! number of keys; 0 for an empty or unbuilt map.
        end function map_nkeys
        !> Components per key: 1 for a single-component map, 0 for one never built or `%init`ed.
        pure module function map_ncomponents(self) result(n)
        class(pf_index_map), intent(in) :: self !! the map.
            integer :: n !! the tuple width every key of this map must have.
        end function map_ncomponents
        !> The resolved backend as a token: `"direct"`, `"hash"`, `"sorted"`, or empty when the map
        !! has no backend yet.
        !!
        !! A subroutine with an allocatable `intent(out)` rather than a function returning
        !! `character(len=:), allocatable` -- that shape is banned project-wide because gfortran's
        !! codegen for receiving one is not thread-safe.
        pure module subroutine map_get_method(self, method)
        class(pf_index_map), intent(in) :: self !! the map.
            character(len=:), allocatable, intent(out) :: method !! the backend token; always allocated.
        end subroutine map_get_method
        !> Heap this map holds, in bytes. What makes the automatic backend choice checkable.
        pure module function map_memory_bytes(self) result(b)
        class(pf_index_map), intent(in) :: self !! the map.
            integer(int64) :: b !! bytes of allocated storage, excluding the object itself.
        end function map_memory_bytes
        !> Current probe-length statistics, by scanning the table.
        !!
        !! A public diagnostic rather than a debug hook, because it is what a caller tuning a map
        !! wants as much as what the clustering tests assert. Cold path: it walks every slot.
        !! Reports 1 for the direct backend (there is no probing) and the binary search's worst
        !! depth for the sorted one.
        pure module subroutine map_probe_stats(self, max_probe, mean_probe)
        class(pf_index_map), intent(in) :: self !! the map.
            integer(int64), intent(out) :: max_probe !! longest probe any stored key needs; 0 when empty.
            real(real64), intent(out), optional :: mean_probe !! mean over stored keys; 0 when empty.
        end subroutine map_probe_stats
        !> The stored keys of a single-component map, ascending for direct and sorted, in
        !! unspecified order for hash.
        !!
        !! Allocated zero-length when the map is empty -- never left unallocated. Ask for the
        !! matching values with `%get_many(list, vals)`, which pairs element for element.
        pure module subroutine map_keys_r1(self, list)
        class(pf_index_map), intent(in) :: self !! the map; must have `ncomponents() <= 1`.
            integer(int64), allocatable, intent(out) :: list(:) !! the keys, one per element.
        end subroutine map_keys_r1
        !> The stored keys of a composite map, shaped `(nkeys, ncomp)`. See `map_keys_r1`.
        pure module subroutine map_keys_r2(self, list)
        class(pf_index_map), intent(in) :: self !! the map.
            integer(int64), allocatable, intent(out) :: list(:,:) !! the key tuples, one per row.
        end subroutine map_keys_r2
    end interface

    ! ---- pf_index_pool. Every procedure takes the pool's guard, queries included, because they
    ! read counters a concurrent mutation is writing. ----

    interface
        !> Takes an index nobody else holds: the most recently freed one, or `max_used + 1`.
        !!
        !! Reuse always precedes growth, which is what keeps the handed-out set dense and is the
        !! original specification's "only grow when all indexes up to the maximum used are used".
        !! Hand-out order is most-recently-freed first, except immediately after `%compact`, which
        !! rebuilds the free list so the smallest free index comes first.
        module function pool_get_index(self) result(idx)
        class(pf_index_pool), intent(inout) :: self !! the pool.
            integer(int64) :: idx !! an index >= 1 that nobody else holds.
        end function pool_get_index
        !> Gives an index back, so a later `%get_index` may hand it out again.
        !!
        !! Aborts on an index out of range, never handed out, or already free -- a double free is a
        !! bug in the caller's bookkeeping and silently accepting it would let two owners believe
        !! they hold the same slot.
        module subroutine pool_free_i32(self, idx)
        class(pf_index_pool), intent(inout) :: self !! the pool.
            integer(int32), intent(in) :: idx !! the index to release; must currently be held.
        end subroutine pool_free_i32
        !> Gives an index back. See `pool_free_i32`.
        module subroutine pool_free_i64(self, idx)
        class(pf_index_pool), intent(inout) :: self !! the pool.
            integer(int64), intent(in) :: idx !! the index to release; must currently be held.
        end subroutine pool_free_i64
        !> The highest index handed out, as a watermark.
        !!
        !! **Monotone between `%compact` calls**: freeing the top index does not lower it. Lowering
        !! it eagerly would have to prune every free-list entry above the new mark, on a path that
        !! must stay O(1). `%compact` is where it tightens, to the highest index actually held.
        module function pool_get_max(self) result(n)
        class(pf_index_pool), intent(in) :: self !! the pool.
            integer(int64) :: n !! the watermark; 0 for a pool that has issued nothing.
        end function pool_get_max
        !> Free indexes in `1 .. get_max_index()`, i.e. holes below the watermark. O(1).
        module function pool_get_free_count(self) result(n)
        class(pf_index_pool), intent(in) :: self !! the pool.
            integer(int64) :: n !! `get_max_index() - get_used_count()`.
        end function pool_get_free_count
        !> Indexes currently held. O(1).
        module function pool_get_used(self) result(n)
        class(pf_index_pool), intent(in) :: self !! the pool.
            integer(int64) :: n !! how many indexes are out.
        end function pool_get_used
        !> Whether `idx` is currently held. `.false.` for anything out of range.
        module function pool_is_used_i32(self, idx) result(ok)
        class(pf_index_pool), intent(in) :: self !! the pool.
            integer(int32), intent(in) :: idx !! the index to test; any value is accepted.
            logical :: ok !! `.true.` when that index is out.
        end function pool_is_used_i32
        !> Whether `idx` is currently held. See `pool_is_used_i32`.
        module function pool_is_used_i64(self, idx) result(ok)
        class(pf_index_pool), intent(in) :: self !! the pool.
            integer(int64), intent(in) :: idx !! the index to test; any value is accepted.
            logical :: ok !! `.true.` when that index is out.
        end function pool_is_used_i64
        !> Every held index, ascending. Allocated zero-length when none are -- never unallocated.
        module subroutine pool_used_indexes(self, list)
        class(pf_index_pool), intent(in) :: self !! the pool.
            integer(int64), allocatable, intent(out) :: list(:) !! the held indexes, ascending.
        end subroutine pool_used_indexes
        !> Gives back storage the pool grew, and makes the smallest free index the next one out.
        !!
        !! Pure memory optimisation as far as the ACTIVE indexes are concerned: every index
        !! currently held is still held, and `%is_used` answers identically before and after. What
        !! does change is bookkeeping and order. `get_max_index()` becomes the highest index
        !! actually held; the free list is rebuilt to hold exactly the free indexes below it,
        !! handed out **smallest first**; and the arrays shrink when the watermark has fallen far
        !! enough behind the allocation.
        !!
        !! That ordering is the point of it: a pool that issued a great many indexes and then
        !! released most of them converges back onto a dense `1 .. n` as it keeps allocating,
        !! rather than continuing upward from the old watermark. Indexes above the new watermark
        !! come back through the increment path, in the same ascending order.
        !!
        !! Later `%free_index` calls push onto the top of the rebuilt list, so the newly freed
        !! values come back first again: `%compact` re-sorts, and ordinary operation does not pay
        !! for sorting.
        module subroutine pool_compact(self)
        class(pf_index_pool), intent(inout) :: self !! the pool.
        end subroutine pool_compact
        !> Pre-sizes for `n` indexes, so a run of `%get_index` up to `n` reallocates nothing.
        module subroutine pool_reserve_i32(self, n)
        class(pf_index_pool), intent(inout) :: self !! the pool.
            integer(int32), intent(in) :: n !! indexes to make room for; must be >= 0.
        end subroutine pool_reserve_i32
        !> Pre-sizes for `n` indexes. See `pool_reserve_i32`.
        module subroutine pool_reserve_i64(self, n)
        class(pf_index_pool), intent(inout) :: self !! the pool.
            integer(int64), intent(in) :: n !! indexes to make room for; must be >= 0.
        end subroutine pool_reserve_i64
        !> Heap this pool holds, in bytes. What makes `%compact`'s shrink observable.
        module function pool_memory_bytes(self) result(b)
        class(pf_index_pool), intent(in) :: self !! the pool.
            integer(int64) :: b !! bytes of allocated storage, excluding the object itself.
        end function pool_memory_bytes
        !> Releases every index and all storage: the pool is as new, and issues 1 next.
        module subroutine pool_clear(self)
        class(pf_index_pool), intent(inout) :: self !! the pool.
        end subroutine pool_clear
    end interface


    ! ---- pf_index_multimap: bulk build (12 specifics, split for the reason the map's are) ----

    interface
        !> Builds the multimap from `keys`, replacing whatever it held. Keys may repeat.
        !!
        !! Groups the unmasked rows by key, numbers the groups densely, and lays each group's
        !! values out ascending by position. With `valid=`, a masked row is neither stored nor
        !! counted, and the stored values stay the ROW NUMBERS of the rows that were kept (or
        !! their `values=` entries), so a nullable key column can be indexed as it is. The
        !! backend is the map's automatic choice applied to the DISTINCT keys unless `method=`
        !! says otherwise. Serialised on `pf_index_multimap_guard`; idempotent by reconstruction.
        module subroutine mm_build_r1_k32_nov(self, keys, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:) !! one key per element; repeats are the point.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r1_k32_nov
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r1_k32_v32(self, keys, values, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:) !! one key per element; repeats are the point.
        integer(int32), intent(in) :: values(:)
            !! the value stored for each row, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r1_k32_v32
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r1_k32_v64(self, keys, values, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:) !! one key per element; repeats are the point.
        integer(int64), intent(in) :: values(:)
            !! the value stored for each row, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r1_k32_v64
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r1_k64_nov(self, keys, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:) !! one key per element; repeats are the point.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r1_k64_nov
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r1_k64_v32(self, keys, values, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:) !! one key per element; repeats are the point.
        integer(int32), intent(in) :: values(:)
            !! the value stored for each row, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r1_k64_v32
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r1_k64_v64(self, keys, values, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:) !! one key per element; repeats are the point.
        integer(int64), intent(in) :: values(:)
            !! the value stored for each row, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r1_k64_v64
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r2_k32_nov(self, keys, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Rows may repeat;
            !! a key is the whole tuple.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r2_k32_nov
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r2_k32_v32(self, keys, values, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Rows may repeat;
            !! a key is the whole tuple.
        integer(int32), intent(in) :: values(:)
            !! the value stored for each row, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r2_k32_v32
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r2_k32_v64(self, keys, values, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Rows may repeat;
            !! a key is the whole tuple.
        integer(int64), intent(in) :: values(:)
            !! the value stored for each row, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r2_k32_v64
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r2_k64_nov(self, keys, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Rows may repeat;
            !! a key is the whole tuple.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r2_k64_nov
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r2_k64_v32(self, keys, values, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Rows may repeat;
            !! a key is the whole tuple.
        integer(int32), intent(in) :: values(:)
            !! the value stored for each row, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r2_k64_v32
    end interface
    interface
        !> Builds the multimap from `keys`, replacing whatever it held. See `mm_build_r1_k32_nov`.
        module subroutine mm_build_r2_k64_v64(self, keys, values, method, threads, valid)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:,:)
            !! one key per ROW, one component per column, shaped `(n, ncomp)`. Rows may repeat;
            !! a key is the whole tuple.
        integer(int64), intent(in) :: values(:)
            !! the value stored for each row, each >= 1. Defaults to `1 .. n` when absent.
        character(len=*), intent(in), optional :: method
            !! backend token for the distinct-key map: "auto" (default), "direct", "hash" or
            !! "sorted".
        integer, intent(in), optional :: threads
            !! threads the build may use. The grouping pass of this version is serial; the
            !! argument reaches the map built over the distinct keys, which honours it exactly as
            !! `pf_index_map%build` does. ABSENT means automatic; `threads=1` forces serial.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry skips that row, which is then neither stored
            !! nor counted.
        end subroutine mm_build_r2_k64_v64
    end interface

    ! ---- pf_index_multimap: lifecycle ----

    interface
        !> Forgets every key and releases all storage; the multimap is as new.
        !!
        !! Releases, matching `pf_index_map%clear`. Afterwards every lookup answers 0 or an empty
        !! range, and the introspection counts are 0.
        module subroutine mm_clear(self)
        class(pf_index_multimap), intent(inout) :: self !! the multimap.
        end subroutine mm_clear
    end interface

    ! ---- pf_index_multimap: scalar lookup. Lock-free and `pure`; allocation-free bar `%get_all` ----

    interface
        !> The group id of `key`, in `1 .. ngroups`, or **0 when the key is absent**.
        !!
        !! The id is what `%csr`'s offsets are indexed by; it is dense, stable for the life of one
        !! build, and carries no other meaning. A multimap that was never built answers 0 rather
        !! than aborting, at no cost to the built path.
        pure module function mm_get_k32(self, key) result(g)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key !! the key to look up.
            integer(int64) :: g !! the group id, >= 1, or 0 when not found.
        end function mm_get_k32
    end interface
    interface
        !> The group id of `key`, or 0 when absent. See `mm_get_k32`.
        pure module function mm_get_k64(self, key) result(g)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key !! the key to look up.
            integer(int64) :: g !! the group id, >= 1, or 0 when not found.
        end function mm_get_k64
    end interface
    interface
        !> The group id of `key`, or 0 when absent. See `mm_get_k32`.
        pure module function mm_get_t32(self, key) result(g)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64) :: g !! the group id, >= 1, or 0 when not found.
        end function mm_get_t32
    end interface
    interface
        !> The group id of `key`, or 0 when absent. See `mm_get_k32`.
        pure module function mm_get_t64(self, key) result(g)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64) :: g !! the group id, >= 1, or 0 when not found.
        end function mm_get_t64
    end interface
    interface
        !> How many stored rows hold `key`; **0 when it is absent**.
        pure module function mm_count_k32(self, key) result(n)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key !! the key to look up.
            integer(int64) :: n !! rows holding the key; 0 when none does.
        end function mm_count_k32
    end interface
    interface
        !> How many stored rows hold `key`; 0 when absent. See `mm_count_k32`.
        pure module function mm_count_k64(self, key) result(n)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key !! the key to look up.
            integer(int64) :: n !! rows holding the key; 0 when none does.
        end function mm_count_k64
    end interface
    interface
        !> How many stored rows hold `key`; 0 when absent. See `mm_count_k32`.
        pure module function mm_count_t32(self, key) result(n)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64) :: n !! rows holding the key; 0 when none does.
        end function mm_count_t32
    end interface
    interface
        !> How many stored rows hold `key`; 0 when absent. See `mm_count_k32`.
        pure module function mm_count_t64(self, key) result(n)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64) :: n !! rows holding the key; 0 when none does.
        end function mm_count_t64
    end interface
    interface
        !> The value stored at the LOWEST position holding `key`, or **0 when it is absent**.
        !!
        !! With default values that is the smallest row number the key appears in -- `pf_match`'s
        !! m:1 answer, and what a table index's `%find` forwards to. With `values=` it is the value
        !! of that same lowest row, not the smallest value.
        pure module function mm_first_k32(self, key) result(v)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key !! the key to look up.
            integer(int64) :: v !! the value at the key's lowest position, or 0 when absent.
        end function mm_first_k32
    end interface
    interface
        !> The value at the lowest position holding `key`, or 0. See `mm_first_k32`.
        pure module function mm_first_k64(self, key) result(v)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key !! the key to look up.
            integer(int64) :: v !! the value at the key's lowest position, or 0 when absent.
        end function mm_first_k64
    end interface
    interface
        !> The value at the lowest position holding `key`, or 0. See `mm_first_k32`.
        pure module function mm_first_t32(self, key) result(v)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64) :: v !! the value at the key's lowest position, or 0 when absent.
        end function mm_first_t32
    end interface
    interface
        !> The value at the lowest position holding `key`, or 0. See `mm_first_k32`.
        pure module function mm_first_t64(self, key) result(v)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64) :: v !! the value at the key's lowest position, or 0 when absent.
        end function mm_first_t64
    end interface
    interface
        !> Every value stored for `key`, ascending by position.
        !!
        !! Allocated zero-length when the key is absent -- never left unallocated -- so
        !! `size(rows)` is the only thing a caller has to test. `%get_range` is the
        !! allocation-free form of the same answer.
        pure module subroutine mm_all_k32_i32(self, key, rows)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key !! the key to look up.
            integer(int32), allocatable, intent(out) :: rows(:)
            !! the values, one per stored row holding the key, ascending by position. A stored
            !! value above `huge(int32)` aborts rather than truncating; take them as `int64` if
            !! the stored values can exceed it.
        end subroutine mm_all_k32_i32
    end interface
    interface
        !> Every value stored for `key`, ascending by position. See `mm_all_k32_i32`.
        pure module subroutine mm_all_k32_i64(self, key, rows)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key !! the key to look up.
            integer(int64), allocatable, intent(out) :: rows(:)
            !! the values, one per stored row holding the key, ascending by position.
        end subroutine mm_all_k32_i64
    end interface
    interface
        !> Every value stored for `key`, ascending by position. See `mm_all_k32_i32`.
        pure module subroutine mm_all_k64_i32(self, key, rows)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key !! the key to look up.
            integer(int32), allocatable, intent(out) :: rows(:)
            !! the values, one per stored row holding the key, ascending by position. A stored
            !! value above `huge(int32)` aborts rather than truncating; take them as `int64` if
            !! the stored values can exceed it.
        end subroutine mm_all_k64_i32
    end interface
    interface
        !> Every value stored for `key`, ascending by position. See `mm_all_k32_i32`.
        pure module subroutine mm_all_k64_i64(self, key, rows)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key !! the key to look up.
            integer(int64), allocatable, intent(out) :: rows(:)
            !! the values, one per stored row holding the key, ascending by position.
        end subroutine mm_all_k64_i64
    end interface
    interface
        !> Every value stored for `key`, ascending by position. See `mm_all_k32_i32`.
        pure module subroutine mm_all_t32_i32(self, key, rows)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int32), allocatable, intent(out) :: rows(:)
            !! the values, one per stored row holding the key, ascending by position. A stored
            !! value above `huge(int32)` aborts rather than truncating; take them as `int64` if
            !! the stored values can exceed it.
        end subroutine mm_all_t32_i32
    end interface
    interface
        !> Every value stored for `key`, ascending by position. See `mm_all_k32_i32`.
        pure module subroutine mm_all_t32_i64(self, key, rows)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64), allocatable, intent(out) :: rows(:)
            !! the values, one per stored row holding the key, ascending by position.
        end subroutine mm_all_t32_i64
    end interface
    interface
        !> Every value stored for `key`, ascending by position. See `mm_all_k32_i32`.
        pure module subroutine mm_all_t64_i32(self, key, rows)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int32), allocatable, intent(out) :: rows(:)
            !! the values, one per stored row holding the key, ascending by position. A stored
            !! value above `huge(int32)` aborts rather than truncating; take them as `int64` if
            !! the stored values can exceed it.
        end subroutine mm_all_t64_i32
    end interface
    interface
        !> Every value stored for `key`, ascending by position. See `mm_all_k32_i32`.
        pure module subroutine mm_all_t64_i64(self, key, rows)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64), allocatable, intent(out) :: rows(:)
            !! the values, one per stored row holding the key, ascending by position.
        end subroutine mm_all_t64_i64
    end interface
    interface
        !> The range of `%csr`'s `rows` array that holds `key`: the values are `rows(lo : hi)`.
        !!
        !! **`lo > hi` when the key is absent** -- `1` and `0`, so `rows(lo : hi)` is a legal empty
        !! section. The allocation-free form of `%get_all`, for a caller holding the CSR pair.
        pure module subroutine mm_range_k32(self, key, lo, hi)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key !! the key to look up.
            integer(int64), intent(out) :: lo !! first position in `rows`; 1 when the key is absent.
            integer(int64), intent(out) :: hi !! last position in `rows`; 0 when the key is absent.
        end subroutine mm_range_k32
    end interface
    interface
        !> The range of `%csr`'s `rows` holding `key`; `lo > hi` when absent. See `mm_range_k32`.
        pure module subroutine mm_range_k64(self, key, lo, hi)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key !! the key to look up.
            integer(int64), intent(out) :: lo !! first position in `rows`; 1 when the key is absent.
            integer(int64), intent(out) :: hi !! last position in `rows`; 0 when the key is absent.
        end subroutine mm_range_k64
    end interface
    interface
        !> The range of `%csr`'s `rows` holding `key`; `lo > hi` when absent. See `mm_range_k32`.
        pure module subroutine mm_range_t32(self, key, lo, hi)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64), intent(out) :: lo !! first position in `rows`; 1 when the key is absent.
            integer(int64), intent(out) :: hi !! last position in `rows`; 0 when the key is absent.
        end subroutine mm_range_t32
    end interface
    interface
        !> The range of `%csr`'s `rows` holding `key`; `lo > hi` when absent. See `mm_range_k32`.
        pure module subroutine mm_range_t64(self, key, lo, hi)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key(:)
            !! the key tuple; `size(key)` must equal `%ncomponents()`.
            integer(int64), intent(out) :: lo !! first position in `rows`; 1 when the key is absent.
            integer(int64), intent(out) :: hi !! last position in `rows`; 0 when the key is absent.
        end subroutine mm_range_t64
    end interface

    ! ---- pf_index_multimap: bulk lookup. Threaded by the rule `pf_index_map%get_many` follows,
    ! lock-free, and not `pure`, because each opens an OpenMP team of its own. ----

    interface
        !> For every key, the value at the lowest position holding it, or 0 where absent.
        !!
        !! `%get_first` over a whole array, on a team: one contiguous chunk of the keys per
        !! thread, by the rule a build follows (`pf_index_threads`), serial inside a parallel
        !! region, `threads=` to say otherwise. Lock-free. The m:1 lookup a table index over a
        !! key that is not unique forwards to, and the form to prefer in a hot loop.
        module subroutine mm_fmany_r1_k32_i32(self, keys, rows, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int32), intent(out) :: rows(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            !! A stored value above `huge(int32)` aborts rather than truncating; take the
            !! answers as `int64` if the stored values can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_fmany_r1_k32_i32
    end interface
    interface
        !> The value at the lowest position holding each key, or 0. See `mm_fmany_r1_k32_i32`.
        module subroutine mm_fmany_r1_k32_i64(self, keys, rows, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int64), intent(out) :: rows(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_fmany_r1_k32_i64
    end interface
    interface
        !> The value at the lowest position holding each key, or 0. See `mm_fmany_r1_k32_i32`.
        module subroutine mm_fmany_r1_k64_i32(self, keys, rows, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int32), intent(out) :: rows(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            !! A stored value above `huge(int32)` aborts rather than truncating; take the
            !! answers as `int64` if the stored values can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_fmany_r1_k64_i32
    end interface
    interface
        !> The value at the lowest position holding each key, or 0. See `mm_fmany_r1_k32_i32`.
        module subroutine mm_fmany_r1_k64_i64(self, keys, rows, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int64), intent(out) :: rows(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_fmany_r1_k64_i64
    end interface
    interface
        !> The value at the lowest position holding each key, or 0. See `mm_fmany_r1_k32_i32`.
        module subroutine mm_fmany_r2_k32_i32(self, keys, rows, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples to look up, one per ROW, shaped `(n, ncomp)`.
            integer(int32), intent(out) :: rows(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            !! A stored value above `huge(int32)` aborts rather than truncating; take the
            !! answers as `int64` if the stored values can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_fmany_r2_k32_i32
    end interface
    interface
        !> The value at the lowest position holding each key, or 0. See `mm_fmany_r1_k32_i32`.
        module subroutine mm_fmany_r2_k32_i64(self, keys, rows, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples to look up, one per ROW, shaped `(n, ncomp)`.
            integer(int64), intent(out) :: rows(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_fmany_r2_k32_i64
    end interface
    interface
        !> The value at the lowest position holding each key, or 0. See `mm_fmany_r1_k32_i32`.
        module subroutine mm_fmany_r2_k64_i32(self, keys, rows, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples to look up, one per ROW, shaped `(n, ncomp)`.
            integer(int32), intent(out) :: rows(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            !! A stored value above `huge(int32)` aborts rather than truncating; take the
            !! answers as `int64` if the stored values can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_fmany_r2_k64_i32
    end interface
    interface
        !> The value at the lowest position holding each key, or 0. See `mm_fmany_r1_k32_i32`.
        module subroutine mm_fmany_r2_k64_i64(self, keys, rows, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples to look up, one per ROW, shaped `(n, ncomp)`.
            integer(int64), intent(out) :: rows(:)
            !! one answer per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_fmany_r2_k64_i64
    end interface
    interface
        !> For every key, its group id, or 0 where absent.
        !!
        !! `%get` over a whole array, on the team `%get_first_many` describes. The ids index
        !! `%csr`'s offsets, which is how a caller that holds the pair walks every match itself.
        module subroutine mm_many_r1_k32_i32(self, keys, groups, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int32), intent(out) :: groups(:)
            !! one group id per key, 0 where absent. Must be exactly as long as `keys` has rows.
            !! An id above `huge(int32)` aborts rather than truncating.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_many_r1_k32_i32
    end interface
    interface
        !> The group id of each key, or 0. See `mm_many_r1_k32_i32`.
        module subroutine mm_many_r1_k32_i64(self, keys, groups, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int64), intent(out) :: groups(:)
            !! one group id per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_many_r1_k32_i64
    end interface
    interface
        !> The group id of each key, or 0. See `mm_many_r1_k32_i32`.
        module subroutine mm_many_r1_k64_i32(self, keys, groups, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int32), intent(out) :: groups(:)
            !! one group id per key, 0 where absent. Must be exactly as long as `keys` has rows.
            !! An id above `huge(int32)` aborts rather than truncating.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_many_r1_k64_i32
    end interface
    interface
        !> The group id of each key, or 0. See `mm_many_r1_k32_i32`.
        module subroutine mm_many_r1_k64_i64(self, keys, groups, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:) !! the keys to look up, one per element.
            integer(int64), intent(out) :: groups(:)
            !! one group id per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_many_r1_k64_i64
    end interface
    interface
        !> The group id of each key, or 0. See `mm_many_r1_k32_i32`.
        module subroutine mm_many_r2_k32_i32(self, keys, groups, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples to look up, one per ROW, shaped `(n, ncomp)`.
            integer(int32), intent(out) :: groups(:)
            !! one group id per key, 0 where absent. Must be exactly as long as `keys` has rows.
            !! An id above `huge(int32)` aborts rather than truncating.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_many_r2_k32_i32
    end interface
    interface
        !> The group id of each key, or 0. See `mm_many_r1_k32_i32`.
        module subroutine mm_many_r2_k32_i64(self, keys, groups, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples to look up, one per ROW, shaped `(n, ncomp)`.
            integer(int64), intent(out) :: groups(:)
            !! one group id per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_many_r2_k32_i64
    end interface
    interface
        !> The group id of each key, or 0. See `mm_many_r1_k32_i32`.
        module subroutine mm_many_r2_k64_i32(self, keys, groups, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples to look up, one per ROW, shaped `(n, ncomp)`.
            integer(int32), intent(out) :: groups(:)
            !! one group id per key, 0 where absent. Must be exactly as long as `keys` has rows.
            !! An id above `huge(int32)` aborts rather than truncating.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_many_r2_k64_i32
    end interface
    interface
        !> The group id of each key, or 0. See `mm_many_r1_k32_i32`.
        module subroutine mm_many_r2_k64_i64(self, keys, groups, valid, threads, n_found)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples to look up, one per ROW, shaped `(n, ncomp)`.
            integer(int64), intent(out) :: groups(:)
            !! one group id per key, 0 where absent. Must be exactly as long as `keys` has rows.
            logical, intent(in), optional :: valid(:)
            !! one entry per key; a `.false.` entry answers 0 without the key being looked up.
            integer, intent(in), optional :: threads
            !! threads the lookup may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_found
            !! how many keys answered something other than 0.
        end subroutine mm_many_r2_k64_i64
    end interface
    interface
        !> EVERY match between the probe keys and the stored rows, as this library's CSR pair.
        !!
        !! `offsets` has length `size(keys) + 1` and `offsets(1) == 1`; the values stored for
        !! probe `i` are `matches(offsets(i) : offsets(i+1) - 1)`, an empty range when there are
        !! none and ascending by position within it -- `pf_match_all`'s contract, on a hash
        !! engine, and the join's m:m primitive. Two threaded passes over the probes: the group
        !! and the count of each, with a prefix sum into `offsets`; then each probe copies its
        !! group's range into its own. On the team `%get_first_many` describes; lock-free.
        !!
        !! **`size(matches)` counts PAIRS, and a pair count is a product**: a key held by a
        !! thousand stored rows and a thousand probes contributes a million on its own. The total
        !! is `offsets(size(keys) + 1) - 1`, accumulated in `int64` and refused rather than
        !! wrapped when it would not fit; read it before doing anything proportional to it.
        module subroutine mm_probe_r1_k32_i32(self, keys, offsets, matches, valid, threads, &
                n_matched, group_hit)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:) !! the keys to probe with, one per element.
            integer(int64), allocatable, intent(out) :: offsets(:)
            !! length `size(keys) + 1`, starting at 1: probe `i`'s matches are
            !! `matches(offsets(i) : offsets(i+1) - 1)`.
            integer(int32), allocatable, intent(out) :: matches(:)
            !! every matching stored value, grouped by probe and ascending by position within
            !! each group. Its length is the PAIR count. A stored value above `huge(int32)`
            !! aborts rather than truncating; take the matches as `int64` if the stored values
            !! can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per probe; a `.false.` entry matches nothing, and is not looked up.
            integer, intent(in), optional :: threads
            !! threads the probe may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_matched
            !! how many probes matched at least one stored row.
            logical, allocatable, intent(out), optional :: group_hit(:)
            !! one entry per group, `.true.` for every group some unmasked probe reached: how a
            !! right or outer join finds the stored rows nothing probed, with no second structure.
        end subroutine mm_probe_r1_k32_i32
    end interface
    interface
        !> Every match between the probe keys and the stored rows, as a CSR pair. See
        !! `mm_probe_r1_k32_i32`.
        module subroutine mm_probe_r1_k32_i64(self, keys, offsets, matches, valid, threads, &
                n_matched, group_hit)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:) !! the keys to probe with, one per element.
            integer(int64), allocatable, intent(out) :: offsets(:)
            !! length `size(keys) + 1`, starting at 1: probe `i`'s matches are
            !! `matches(offsets(i) : offsets(i+1) - 1)`.
            integer(int64), allocatable, intent(out) :: matches(:)
            !! every matching stored value, grouped by probe and ascending by position within
            !! each group. Its length is the PAIR count.
            logical, intent(in), optional :: valid(:)
            !! one entry per probe; a `.false.` entry matches nothing, and is not looked up.
            integer, intent(in), optional :: threads
            !! threads the probe may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_matched
            !! how many probes matched at least one stored row.
            logical, allocatable, intent(out), optional :: group_hit(:)
            !! one entry per group, `.true.` for every group some unmasked probe reached: how a
            !! right or outer join finds the stored rows nothing probed, with no second structure.
        end subroutine mm_probe_r1_k32_i64
    end interface
    interface
        !> Every match between the probe keys and the stored rows, as a CSR pair. See
        !! `mm_probe_r1_k32_i32`.
        module subroutine mm_probe_r1_k64_i32(self, keys, offsets, matches, valid, threads, &
                n_matched, group_hit)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:) !! the keys to probe with, one per element.
            integer(int64), allocatable, intent(out) :: offsets(:)
            !! length `size(keys) + 1`, starting at 1: probe `i`'s matches are
            !! `matches(offsets(i) : offsets(i+1) - 1)`.
            integer(int32), allocatable, intent(out) :: matches(:)
            !! every matching stored value, grouped by probe and ascending by position within
            !! each group. Its length is the PAIR count. A stored value above `huge(int32)`
            !! aborts rather than truncating; take the matches as `int64` if the stored values
            !! can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per probe; a `.false.` entry matches nothing, and is not looked up.
            integer, intent(in), optional :: threads
            !! threads the probe may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_matched
            !! how many probes matched at least one stored row.
            logical, allocatable, intent(out), optional :: group_hit(:)
            !! one entry per group, `.true.` for every group some unmasked probe reached: how a
            !! right or outer join finds the stored rows nothing probed, with no second structure.
        end subroutine mm_probe_r1_k64_i32
    end interface
    interface
        !> Every match between the probe keys and the stored rows, as a CSR pair. See
        !! `mm_probe_r1_k32_i32`.
        module subroutine mm_probe_r1_k64_i64(self, keys, offsets, matches, valid, threads, &
                n_matched, group_hit)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:) !! the keys to probe with, one per element.
            integer(int64), allocatable, intent(out) :: offsets(:)
            !! length `size(keys) + 1`, starting at 1: probe `i`'s matches are
            !! `matches(offsets(i) : offsets(i+1) - 1)`.
            integer(int64), allocatable, intent(out) :: matches(:)
            !! every matching stored value, grouped by probe and ascending by position within
            !! each group. Its length is the PAIR count.
            logical, intent(in), optional :: valid(:)
            !! one entry per probe; a `.false.` entry matches nothing, and is not looked up.
            integer, intent(in), optional :: threads
            !! threads the probe may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_matched
            !! how many probes matched at least one stored row.
            logical, allocatable, intent(out), optional :: group_hit(:)
            !! one entry per group, `.true.` for every group some unmasked probe reached: how a
            !! right or outer join finds the stored rows nothing probed, with no second structure.
        end subroutine mm_probe_r1_k64_i64
    end interface
    interface
        !> Every match between the probe keys and the stored rows, as a CSR pair. See
        !! `mm_probe_r1_k32_i32`.
        module subroutine mm_probe_r2_k32_i32(self, keys, offsets, matches, valid, threads, &
                n_matched, group_hit)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples to probe with, one per ROW, shaped `(n, ncomp)`.
            integer(int64), allocatable, intent(out) :: offsets(:)
            !! length `size(keys) + 1`, starting at 1: probe `i`'s matches are
            !! `matches(offsets(i) : offsets(i+1) - 1)`.
            integer(int32), allocatable, intent(out) :: matches(:)
            !! every matching stored value, grouped by probe and ascending by position within
            !! each group. Its length is the PAIR count. A stored value above `huge(int32)`
            !! aborts rather than truncating; take the matches as `int64` if the stored values
            !! can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per probe; a `.false.` entry matches nothing, and is not looked up.
            integer, intent(in), optional :: threads
            !! threads the probe may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_matched
            !! how many probes matched at least one stored row.
            logical, allocatable, intent(out), optional :: group_hit(:)
            !! one entry per group, `.true.` for every group some unmasked probe reached: how a
            !! right or outer join finds the stored rows nothing probed, with no second structure.
        end subroutine mm_probe_r2_k32_i32
    end interface
    interface
        !> Every match between the probe keys and the stored rows, as a CSR pair. See
        !! `mm_probe_r1_k32_i32`.
        module subroutine mm_probe_r2_k32_i64(self, keys, offsets, matches, valid, threads, &
                n_matched, group_hit)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: keys(:,:)
            !! the key tuples to probe with, one per ROW, shaped `(n, ncomp)`.
            integer(int64), allocatable, intent(out) :: offsets(:)
            !! length `size(keys) + 1`, starting at 1: probe `i`'s matches are
            !! `matches(offsets(i) : offsets(i+1) - 1)`.
            integer(int64), allocatable, intent(out) :: matches(:)
            !! every matching stored value, grouped by probe and ascending by position within
            !! each group. Its length is the PAIR count.
            logical, intent(in), optional :: valid(:)
            !! one entry per probe; a `.false.` entry matches nothing, and is not looked up.
            integer, intent(in), optional :: threads
            !! threads the probe may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_matched
            !! how many probes matched at least one stored row.
            logical, allocatable, intent(out), optional :: group_hit(:)
            !! one entry per group, `.true.` for every group some unmasked probe reached: how a
            !! right or outer join finds the stored rows nothing probed, with no second structure.
        end subroutine mm_probe_r2_k32_i64
    end interface
    interface
        !> Every match between the probe keys and the stored rows, as a CSR pair. See
        !! `mm_probe_r1_k32_i32`.
        module subroutine mm_probe_r2_k64_i32(self, keys, offsets, matches, valid, threads, &
                n_matched, group_hit)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples to probe with, one per ROW, shaped `(n, ncomp)`.
            integer(int64), allocatable, intent(out) :: offsets(:)
            !! length `size(keys) + 1`, starting at 1: probe `i`'s matches are
            !! `matches(offsets(i) : offsets(i+1) - 1)`.
            integer(int32), allocatable, intent(out) :: matches(:)
            !! every matching stored value, grouped by probe and ascending by position within
            !! each group. Its length is the PAIR count. A stored value above `huge(int32)`
            !! aborts rather than truncating; take the matches as `int64` if the stored values
            !! can exceed it.
            logical, intent(in), optional :: valid(:)
            !! one entry per probe; a `.false.` entry matches nothing, and is not looked up.
            integer, intent(in), optional :: threads
            !! threads the probe may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_matched
            !! how many probes matched at least one stored row.
            logical, allocatable, intent(out), optional :: group_hit(:)
            !! one entry per group, `.true.` for every group some unmasked probe reached: how a
            !! right or outer join finds the stored rows nothing probed, with no second structure.
        end subroutine mm_probe_r2_k64_i32
    end interface
    interface
        !> Every match between the probe keys and the stored rows, as a CSR pair. See
        !! `mm_probe_r1_k32_i32`.
        module subroutine mm_probe_r2_k64_i64(self, keys, offsets, matches, valid, threads, &
                n_matched, group_hit)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: keys(:,:)
            !! the key tuples to probe with, one per ROW, shaped `(n, ncomp)`.
            integer(int64), allocatable, intent(out) :: offsets(:)
            !! length `size(keys) + 1`, starting at 1: probe `i`'s matches are
            !! `matches(offsets(i) : offsets(i+1) - 1)`.
            integer(int64), allocatable, intent(out) :: matches(:)
            !! every matching stored value, grouped by probe and ascending by position within
            !! each group. Its length is the PAIR count.
            logical, intent(in), optional :: valid(:)
            !! one entry per probe; a `.false.` entry matches nothing, and is not looked up.
            integer, intent(in), optional :: threads
            !! threads the probe may use. ABSENT means automatic: bounded by the rows, by
            !! `index_threads`, by this process's CPU affinity, and serial when the caller is
            !! already inside an OpenMP parallel region. `threads=1` forces serial.
            integer(int64), intent(out), optional :: n_matched
            !! how many probes matched at least one stored row.
            logical, allocatable, intent(out), optional :: group_hit(:)
            !! one entry per group, `.true.` for every group some unmasked probe reached: how a
            !! right or outer join finds the stored rows nothing probed, with no second structure.
        end subroutine mm_probe_r2_k64_i64
    end interface

    ! ---- pf_index_multimap: introspection. Read-only, so unguarded like the lookups ----

    interface
        !> The CSR pair itself, copied out: `offsets(ngroups + 1)` and `rows(nkeys)`.
        !!
        !! Group `g` -- the id `%get` and `%get_many` answer -- holds
        !! `rows(offsets(g) : offsets(g+1) - 1)`, ascending by position. For a caller that walks
        !! the ranges itself, a group-by or a join's build side, or wants every group at once.
        !! Both are allocated even for an empty or unbuilt multimap: `[1]` and zero-length.
        pure module subroutine mm_csr(self, offsets, rows)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
            integer(int64), allocatable, intent(out) :: offsets(:) !! group starts; length `ngroups + 1`.
            integer(int64), allocatable, intent(out) :: rows(:) !! the stored values, grouped by key.
        end subroutine mm_csr
        !> Distinct keys stored, i.e. groups; 0 for an empty or unbuilt multimap.
        pure module function mm_ngroups(self) result(n)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
            integer(int64) :: n !! the group count.
        end function mm_ngroups
        !> Rows stored, repeats included: the length of the CSR `rows` array.
        pure module function mm_nkeys(self) result(n)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
            integer(int64) :: n !! the stored row count; 0 for an empty or unbuilt multimap.
        end function mm_nkeys
        !> Components per key: 1 for a single-component multimap, 0 for one never built.
        pure module function mm_ncomponents(self) result(n)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
            integer :: n !! the tuple width every key of this multimap must have.
        end function mm_ncomponents
        !> Rows in the largest group. 1 means every stored key is unique -- the m:1 check a join
        !! makes before choosing its path -- and 0 means the multimap is empty.
        pure module function mm_max_multiplicity(self) result(n)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
            integer(int64) :: n !! the largest group's row count.
        end function mm_max_multiplicity
        !> Heap this multimap holds, in bytes: the distinct-key map's plus the CSR pair's.
        pure module function mm_memory_bytes(self) result(b)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
            integer(int64) :: b !! bytes of allocated storage, excluding the object itself.
        end function mm_memory_bytes
        !> The distinct-key map's resolved backend as a token: `"direct"`, `"hash"`, `"sorted"`,
        !! or empty when nothing has been built. A subroutine, for the reason `map_get_method` gives.
        pure module subroutine mm_get_method(self, method)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
            character(len=:), allocatable, intent(out) :: method !! the backend token; always allocated.
        end subroutine mm_get_method
        !> The distinct keys of a single-component multimap, in the order the map holds them:
        !! ascending for direct and sorted, unspecified for hash.
        !!
        !! Allocated zero-length when the multimap is empty -- never left unallocated. Pair each
        !! key with its group through `%get_many(list, groups)`, and with its rows through
        !! `%csr`.
        pure module subroutine mm_keys_r1(self, list)
        class(pf_index_multimap), intent(in) :: self !! the multimap; must have `ncomponents() <= 1`.
            integer(int64), allocatable, intent(out) :: list(:) !! the distinct keys, one per element.
        end subroutine mm_keys_r1
        !> The distinct key tuples of a composite multimap, shaped `(ngroups, ncomp)`. See
        !! `mm_keys_r1`.
        pure module subroutine mm_keys_r2(self, list)
        class(pf_index_multimap), intent(in) :: self !! the multimap.
            integer(int64), allocatable, intent(out) :: list(:,:) !! the distinct key tuples, one per row.
        end subroutine mm_keys_r2
    end interface

    ! ---- The build's thread rule, reported ----

    !> The team an automatic `%build` over `n` keys, or an automatic `%get_many` over `n` rows,
    !> would open.
    !!
    !! The resolved count, where `parquet_get_index_threads()` is only the cap that was asked for:
    !! the answer also depends on the rows available, on this process's CPU affinity, and on
    !! whether the caller is already inside a parallel region (in which case it is 1). This is what
    !! makes `index_threads` observable, and it is a pass-through onto the very rule `%build` and
    !! `%get_many` run, never a second copy of it.
    interface pf_index_threads
        module procedure pf_index_threads_i32
        module procedure pf_index_threads_i64
    end interface pf_index_threads

    interface
        !> The team an automatic build or bulk lookup over `n` rows would open. See
        !! `pf_index_threads`.
        module function pf_index_threads_i32(n) result(nt)
            integer(int32), intent(in) :: n !! keys the build, or rows the lookup, would process.
            integer :: nt !! threads it would use; 1 means serial.
        end function pf_index_threads_i32
        !> The team an automatic build or bulk lookup over `n` rows would open. See
        !! `pf_index_threads`.
        module function pf_index_threads_i64(n) result(nt)
            integer(int64), intent(in) :: n !! keys the build, or rows the lookup, would process.
            integer :: nt !! threads it would use; 1 means serial.
        end function pf_index_threads_i64
    end interface

    interface
        !> Test-only. Threads the last `%build` resolved for its own work; 1 means serial.
        !!
        !! A build answers identically at every thread count, so nothing about the resulting map
        !! can distinguish an honoured `threads=` from an ignored one. This is the only observable
        !! that can, which is what makes a threading test of this module non-vacuous.
        !!
        !! Reports the count the rule RESOLVED, not the team the runtime granted. It covers this
        !! module's own scan and scatter only: a `method="sorted"` build sorts through `pf_argsort`,
        !! which resolves its own team against the sorting knobs and reports it through
        !! `parquet_debug_sort_threads_used` instead.
        module function parquet_debug_index_threads_used() result(n)
            integer :: n !! threads the last build resolved; 1 means it ran serial.
        end function parquet_debug_index_threads_used
    end interface

    interface
        !> Test-only. Threads the last `%get_many` resolved for itself; 1 means it ran serial.
        !!
        !! The lookup twin of `parquet_debug_index_threads_used`, kept separate because the two
        !! are written from different places -- a build records under the map's guard, a bulk
        !! lookup records lock-free from whichever thread called it -- and one counter for both
        !! would let a probe overwrite what a build had just reported, or the reverse, under a
        !! test that reads it. Reports what the rule RESOLVED, not the team the runtime granted,
        !! because the decision is what a threading test of the bulk lookup is about.
        module function parquet_debug_index_get_many_threads_used() result(n)
            integer :: n !! threads the last bulk lookup resolved; 1 means it ran serial.
        end function parquet_debug_index_get_many_threads_used
    end interface

    interface
        !> Test-only. Lowers the pair count `pf_index_multimap%probe_many` refuses at, so that
        !! the overflow guard can be reached by an error scenario; 0 (or any value below 1)
        !! restores `huge(int64)`.
        !!
        !! The guard exists for an `int64` pair count, which no test can produce, and a guard no
        !! test reaches is one edit away from being silently wrong. Process-global and read per
        !! call, so set it from a single-threaded scenario only, never beside a running probe.
        module subroutine parquet_debug_set_index_pair_limit(limit)
            integer(int64), intent(in) :: limit !! the new ceiling; a value below 1 restores the default.
        end subroutine parquet_debug_set_index_pair_limit
    end interface

    ! ---- Cross-submodule private helpers ----
    !
    ! Declared here rather than contained in a submodule because their callers live in a SIBLING
    ! submodule: a private procedure contained directly in the module and called only from a
    ! submodule compiles and then fails to LINK under gfortran, and host association does not
    ! reach sideways. Every one takes a `type(pf_index_map)` dummy rather than `class` -- a `class`
    ! actual passed to a `type` dummy is free, while the reverse builds a runtime class descriptor
    ! in the caller's prologue on every call (see CLAUDE.md's typed-accessor-tier section).

    interface
        !> The index stored for a single-component key in the hash backend, or 0.
        pure module function ix_hash_find_scalar(self, key) result(v)
            type(pf_index_map), intent(in) :: self !! a map whose backend is `IX_HASH`.
            integer(int64), intent(in) :: key !! the key, already widened.
            integer(int64) :: v !! the stored value, or 0 when absent.
        end function ix_hash_find_scalar
        !> The index stored for a key tuple in the hash backend, or 0.
        pure module function ix_hash_find_tuple(self, key) result(v)
            type(pf_index_map), intent(in) :: self !! a map whose backend is `IX_HASH`.
            integer(int64), intent(in) :: key(:) !! the tuple, already widened, `size == self%ncomp`.
            integer(int64) :: v !! the stored value, or 0 when absent.
        end function ix_hash_find_tuple
        !> Inserts or replaces one single-component key in the hash backend.
        !!
        !! Separate from the tuple form rather than reached through it, because the build's insert
        !! loop and every scalar `%set` run through here: routing them via a 1-element array
        !! constructor would build a descriptor per key on the one path where that is measurable.
        module subroutine ix_hash_insert_scalar(self, key, value, is_new)
            type(pf_index_map), intent(inout) :: self !! a map whose backend is `IX_HASH`.
            integer(int64), intent(in) :: key !! the key, already widened.
            integer(int64), intent(in) :: value !! the value to store; caller has checked `>= 1`.
            logical, intent(out) :: is_new !! `.true.` when the key was not already present.
        end subroutine ix_hash_insert_scalar
        !> Removes one single-component key from the hash backend. See `ix_hash_insert_scalar`.
        module subroutine ix_hash_remove_scalar(self, key, found)
            type(pf_index_map), intent(inout) :: self !! a map whose backend is `IX_HASH`.
            integer(int64), intent(in) :: key !! the key, already widened.
            logical, intent(out) :: found !! `.true.` when the key was there to remove.
        end subroutine ix_hash_remove_scalar
        !> Inserts or replaces one key in the hash backend, growing the table when it must.
        module subroutine ix_hash_insert(self, key, value, is_new)
            type(pf_index_map), intent(inout) :: self !! a map whose backend is `IX_HASH`.
            integer(int64), intent(in) :: key(:) !! the tuple, `size == self%ncomp`.
            integer(int64), intent(in) :: value !! the value to store; caller has checked `>= 1`.
            logical, intent(out) :: is_new !! `.true.` when the key was not already present.
        end subroutine ix_hash_insert
        !> Removes one key from the hash backend by backward-shift deletion.
        !!
        !! Backward shift rather than a tombstone: on a linear-probe table it is about twenty lines
        !! and it keeps every later lookup tombstone-free, which is what a delete-heavy workload
        !! would otherwise pay for in an ever-growing rehash policy.
        module subroutine ix_hash_remove(self, key, found)
            type(pf_index_map), intent(inout) :: self !! a map whose backend is `IX_HASH`.
            integer(int64), intent(in) :: key(:) !! the tuple, `size == self%ncomp`.
            logical, intent(out) :: found !! `.true.` when the key was there to remove.
        end subroutine ix_hash_remove
        !> Sizes the hash table so `want` keys fit under the load factor, rehashing if needed.
        module subroutine ix_hash_reserve(self, want)
            type(pf_index_map), intent(inout) :: self !! a map whose backend is `IX_HASH`.
            integer(int64), intent(in) :: want !! keys to make room for.
        end subroutine ix_hash_reserve
        !> Probe-length statistics of the hash table, by scanning every stored key.
        pure module subroutine ix_hash_probe_stats(self, max_probe, mean_probe)
            type(pf_index_map), intent(in) :: self !! a map whose backend is `IX_HASH`.
            integer(int64), intent(out) :: max_probe !! longest probe any stored key needs.
            real(real64), intent(out) :: mean_probe !! mean probe length over stored keys.
        end subroutine ix_hash_probe_stats
        !> Copies every stored tuple out of the hash table, in slot order.
        pure module subroutine ix_hash_collect(self, out)
            type(pf_index_map), intent(in) :: self !! a map whose backend is `IX_HASH`.
            integer(int64), intent(out) :: out(:,:) !! shaped `(nkeys, ncomp)`; filled in slot order.
        end subroutine ix_hash_collect
        !> The index stored for a key in the sorted backend, or 0. Binary search.
        pure module function ix_sorted_find(self, key) result(v)
            type(pf_index_map), intent(in) :: self !! a map whose backend is `IX_SORTED`.
            integer(int64), intent(in) :: key !! the key, already widened.
            integer(int64) :: v !! the stored value, or 0 when absent.
        end function ix_sorted_find
        !> Builds the sorted backend: one `pf_argsort` call, a gather, and a duplicate scan.
        module subroutine ix_sorted_build(self, keys, values, threads)
            type(pf_index_map), intent(inout) :: self !! the map; its sorted storage is replaced.
            integer(int64), intent(in) :: keys(:) !! the keys, already widened; must be unique.
            integer(int64), intent(in), optional :: values(:)
            !! one value per key, each already checked `>= 1`. ABSENT means "the value for key `i`
            !! is `i`", which is the default `%build` contract -- passed through as an absence
            !! rather than materialised by the caller, so the common case allocates nothing.
            integer, intent(in), optional :: threads !! forwarded to `pf_argsort`; absent means automatic.
        end subroutine ix_sorted_build
    end interface

end module parquet_index

