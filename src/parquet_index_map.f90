!> `pf_index_map`: lifecycle, backend dispatch, the direct backend, the hash mixer and probes,
!> and the map's OpenMP guard.
!!
!! **Where the hot path is.** `%get` reaches storage through `ix_get_scalar`/`ix_get_tuple`, which
!! are `pure`, allocate nothing, write nothing and take no lock. That last property is not a
!! coincidence to be preserved by care -- it is what makes concurrent lookups safe at all, so a
!! lazy allocation or a cached anything on a read path would silently withdraw the module's
!! thread-safety contract. There is nothing on these paths to make lazy. `%get_many` dispatches on
!! the backend ONCE per chunk and then runs a typed loop over the chunk's rows -- the hash probe
!! with the slot mask and the table's base in locals -- one chunk per thread of a team it resolves
!! by the build's own rule (`ix_lookup_threads_for`); it is the one lookup that is not `pure`,
!! because an OpenMP directive may not appear in a pure procedure, and it is still lock-free and
!! allocation-free at every team size. The hash mixer and the two probes are contained in THIS
!! file, below the lookup workers, so that the compiler inlines them into those loops: a probe
!! that calls across a submodule boundary pays about three nanoseconds per key, a third of a
!! small-map probe (feature_pf_index.md, section 4.6). `parquet_index_hash.f90`, the backend's
!! mutation side, descends from this submodule and reaches the same functions by host
!! association, so both sides hash identically by construction.
!!
!! **Every mutation takes one process-wide named critical, `pf_index_map_guard`**, in the same
!! wrapper-plus-worker shape the pool uses: the public body is the guard and one call, and no
!! worker calls a public entry. A named critical is not recursive, so a nested acquisition would
!! deadlock; and branching out of a `critical` region is not conforming, so a worker that returns
!! early cannot be inlined into the guarded body. **A `%build` holds the guard for its swap
!! alone**: each forwarder builds into a LOCAL map on the calling thread -- the scan, the choice
!! of backend, the table -- and `ix_adopt` takes the guard only to move that storage into the
!! caller's object, so two threads building two maps run side by side instead of taking turns,
!! and a program preparing several maps in a parallel loop gets the team it opened. What that
!! gives up is the guard's side effect of serialising aborts, which `ix_abort` (the spec's
!! reporter) restores for every impure abort of these submodules, guarded or not; the pure checks
!! of the lock-free lookups keep their bare `error stop`, as they always had.
!!
!! **The hash build and `%get_or_add_many` thread through a partitioned insert.** On a team of
!! two or more, the hash arm of a build hands its keys to `ix_hash_build_part_1`/`_n`
!! (`parquet_index_hash.f90`): every key's home slot is computed on the team, the keys are
!! scattered into partition order, and each thread fills its own slot ranges, deferring the few
!! chains that reach a range boundary to a serial pass at the end. `%get_or_add_many` looks its
!! keys up lock-free on the team and inserts the ones not found the same way, numbered partition
!! by partition. The plan of that insert -- the partition size, the histogram and the write
!! cursors -- is `ix_part_shift`/`ix_partition_plan` below, contained here so that the string
!! submodule reaches them too; `feature_pf_index.md` section 6 item 3 is the design.
!!
!! **The typed tier, from the first line rather than as a later optimisation.** Every binding is a
!! thin forwarder onto a worker taking a `type(pf_index_map)` dummy. A `class` actual passed to a
!! `type` dummy is free; the reverse makes ifx build a runtime class descriptor in the caller's
!! prologue on every call, which for a type with this many allocatable components is tens of
!! nanoseconds and, being emitted into `.bss`, contends across threads. `%get_many` is the
!! documented hot-loop form for the same reason: it pays the object conversion once per call and
!! then runs a typed loop.
!!
!! **Widening happens into a fixed-size stack buffer.** A tuple lookup must present its key as
!! `integer(int64)`, so an `int32` tuple is widened -- into `kb(pf_index_max_components)`, whose
!! size is a compile-time constant and therefore unconditionally on the stack. An automatic array
!! sized `size(key)` would be a heap allocation per lookup on some compilers, which is the one
!! thing this path must not do. The component count is validated BEFORE the widening loop, so only
!! the elements that are about to be read are ever written.
!!
!! **`parquet_index_multi.f90` and `parquet_index_hash.f90` are DESCENDANTS of this submodule**,
!! not siblings, so that the multimap can reach the private helpers below by host association --
!! the chunk loops, the thread rule, the mask and value checks, the automatic choice -- and the
!! hash backend's mutation side can reach the mixer, rather than each being re-declared in the
!! spec as a cross-submodule interface. Anything here that a message names the type in takes an
!! optional `owner` prefix for that reason (`ix_owner_of`). `parquet_index_str.f90`, the string
!! keys of both types, descends from the multimap's in turn and reaches every helper here the
!! same way, `ix_hash_str` and the probes included; the string store's lifecycle (`ix_str_start`
!! and the two growth helpers) lives here beside the other storage lifecycle because `%init`,
!! `%reserve`, `%reset` and `%clear` need it and are implemented here.
submodule (parquet_index) parquet_index_map
    use parquet_utils, only: pf_to_lower
    use parquet_settings_base, only: parquet_auto_thread_count, parquet_clamp_to_affinity, &
        cfg_index_threads
    implicit none

    !> `method="auto"`: let `ix_choose_direct` decide. Never stored in `self%backend`.
    integer, parameter :: IX_WANT_AUTO = -1

    !> Rows and payload bytes a string store is allocated for at the least, so that a map filled
    !! one string at a time does not reallocate on each of its first few keys.
    integer(int64), parameter :: IX_STR_MIN_ROWS = 16_int64
    integer(int64), parameter :: IX_STR_MIN_BYTES = 256_int64 !! see `IX_STR_MIN_ROWS`.
    !> Payload bytes reserved per key when a string store is sized for a key count alone, i.e.
    !! before any string has been seen. A guess, corrected by doubling; it only decides how
    !! many reallocations a build of average-length keys pays.
    integer(int64), parameter :: IX_STR_BYTES_PER_KEY = 16_int64

    !> Keys one call of a bulk probe kernel takes: the home slots of a whole block are computed
    !! first and the walks follow, which is what lets a block's DRAM misses overlap
    !! (`ix_probe_1_block`). Sixteen would do for the overlap; sixty-four amortises the call and
    !! the two loops' overhead, at 512 bytes of stack for the home slots. Never a setting: it
    !! changes speed and no answer.
    integer, parameter :: IX_PROBE_BLOCK = 64

    !> Fewest slots a partition of the partitioned insert may hold, as a power of two. With
    !! chains averaging under two slots, one key in about a hundred and fifty reaches the end
    !! of a 256-slot partition and is deferred to the serial spill pass, which keeps that pass
    !! negligible while still letting a small table be cut across a whole team; a larger table
    !! gets larger partitions (`ix_part_shift`). Never a setting: it changes where keys sit and
    !! no answer.
    integer, parameter :: IX_PART_MIN_LG = 8

    !> First multiplicative constant of the 32-bit mixing step: murmur3's `0x85ebca6b` with its
    !! top bit cleared, so it is below 2**31 and every product stays below 2**63.
    !!
    !! **Odd, which is what matters about it.** Multiplication by an odd constant modulo 2**32 is a
    !! bijection, so the step loses no information; the exact value only has to avalanche well, and
    !! the clustering tests in `test/test_index.f90` are what hold it to that.
    integer(int64), parameter :: IX_C1 = 99244651_int64
    !> Second multiplicative constant: murmur3's `0xc2b2ae35` with its top bit cleared. Odd, for
    !! the reason given on `IX_C1`.
    integer(int64), parameter :: IX_C2 = 1119548469_int64
    !> Seed the scalar key hash starts from, so that a hash of zero is not zero. The golden ratio
    !! constant `0x9E3779B9`; only ever XOR-ed, never multiplied, so its width is free.
    integer(int64), parameter :: IX_SEED = 2654435769_int64
    !> Seed of the scalar key hash's tuple step and of a tuple hash's first chain, distinct from
    !! `IX_SEED` so that the per-half mixing and the per-component chaining are not the same
    !! transformation applied twice. `0x9E3779B97F4A7C15`'s low 31 bits; XOR-ed only.
    integer(int64), parameter :: IX_SEED_T = 2135587861_int64
    !> Seed of a tuple hash's SECOND chain (the components' high halves), distinct from the
    !! first chain's so that the two chains start apart. `0x3C6EF35F`; XOR-ed only.
    integer(int64), parameter :: IX_SEED_U = 1013904223_int64
    !> Seed of the STRING hash's first chain, distinct from the tuple seeds so that a string's
    !! word chain and a tuple's component chain are not one transformation. `0x61C88647`, the
    !! negated golden-ratio constant; XOR-ed only.
    integer(int64), parameter :: IX_SEED_S = 1640531527_int64

contains

    ! ============================================================================================
    ! Bulk build. Twelve forwarders onto two workers -- one for rank-1 keys, one for rank-2 -- so
    ! that an `int64` caller's key array is never copied. The `int32` forwarders must widen, which
    ! is a cost the kind conversion imposes rather than one this split introduces. Each builds
    ! into a LOCAL map on the calling thread and adopts it under the guard (`ix_adopt`), so the
    ! guard is held for the swap alone and two builds on two threads run side by side.
    ! ============================================================================================

    module procedure build_r1_k64_nov
        type(pf_index_map) :: fresh

        call ix_build_1(fresh, keys, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r1_k64_nov

    module procedure build_r1_k64_v64
        type(pf_index_map) :: fresh

        call ix_build_1(fresh, keys, values=values, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r1_k64_v64

    module procedure build_r1_k64_v32
        integer(int64), allocatable :: v(:)
        type(pf_index_map) :: fresh

        call ix_widen_values(values, v)
        call ix_build_1(fresh, keys, values=v, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r1_k64_v32

    module procedure build_r1_k32_nov
        integer(int64), allocatable :: k(:)
        type(pf_index_map) :: fresh

        call ix_widen_keys_1(keys, k)
        call ix_build_1(fresh, k, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r1_k32_nov

    module procedure build_r1_k32_v64
        integer(int64), allocatable :: k(:)
        type(pf_index_map) :: fresh

        call ix_widen_keys_1(keys, k)
        call ix_build_1(fresh, k, values=values, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r1_k32_v64

    module procedure build_r1_k32_v32
        integer(int64), allocatable :: k(:), v(:)
        type(pf_index_map) :: fresh

        call ix_widen_keys_1(keys, k)
        call ix_widen_values(values, v)
        call ix_build_1(fresh, k, values=v, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r1_k32_v32

    module procedure build_r2_k64_nov
        type(pf_index_map) :: fresh

        call ix_build_n(fresh, keys, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r2_k64_nov

    module procedure build_r2_k64_v64
        type(pf_index_map) :: fresh

        call ix_build_n(fresh, keys, values=values, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r2_k64_v64

    module procedure build_r2_k64_v32
        integer(int64), allocatable :: v(:)
        type(pf_index_map) :: fresh

        call ix_widen_values(values, v)
        call ix_build_n(fresh, keys, values=v, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r2_k64_v32

    module procedure build_r2_k32_nov
        integer(int64), allocatable :: k(:,:)
        type(pf_index_map) :: fresh

        call ix_widen_keys_n(keys, k)
        call ix_build_n(fresh, k, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r2_k32_nov

    module procedure build_r2_k32_v64
        integer(int64), allocatable :: k(:,:)
        type(pf_index_map) :: fresh

        call ix_widen_keys_n(keys, k)
        call ix_build_n(fresh, k, values=values, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r2_k32_v64

    module procedure build_r2_k32_v32
        integer(int64), allocatable :: k(:,:), v(:)
        type(pf_index_map) :: fresh

        call ix_widen_keys_n(keys, k)
        call ix_widen_values(values, v)
        call ix_build_n(fresh, k, values=v, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_r2_k32_v32

    ! ============================================================================================
    ! Incremental lifecycle
    ! ============================================================================================

    module procedure map_init
        !$omp critical (pf_index_map_guard)
        call ix_do_init(self, capacity, method, ncomp, strings)
        !$omp end critical (pf_index_map_guard)
    end procedure map_init

    module procedure map_reserve_i32
        !$omp critical (pf_index_map_guard)
        call ix_do_reserve(self, int(n, int64))
        !$omp end critical (pf_index_map_guard)
    end procedure map_reserve_i32

    module procedure map_reserve_i64
        !$omp critical (pf_index_map_guard)
        call ix_do_reserve(self, n)
        !$omp end critical (pf_index_map_guard)
    end procedure map_reserve_i64

    module procedure map_clear
        !$omp critical (pf_index_map_guard)
        call ix_reset_storage(self)
        !$omp end critical (pf_index_map_guard)
    end procedure map_clear

    module procedure map_reset
        !$omp critical (pf_index_map_guard)
        call ix_do_reset(self)
        !$omp end critical (pf_index_map_guard)
    end procedure map_reset

    ! ============================================================================================
    ! Lookup. Lock-free and allocation-free; the scalar forms `pure`, the bulk form threaded over
    ! contiguous chunks, each chunk the serial typed loop the bulk form used to be. The scalar
    ! forms test the two shape conditions inline and call the aborting checker only when one
    ! holds: under -fPIC the checker is an out-of-line call per key (CLAUDE.md, "-fPIC blocks
    ! inlining on ELF"), and the common case now makes none.
    ! ============================================================================================

    module procedure get_k32
        if (self%is_str .or. self%ncomp > 1) call ix_check_scalar_shape(self, "get")
        idx = ix_get_scalar(self, int(key, int64))
    end procedure get_k32

    module procedure get_k64
        if (self%is_str .or. self%ncomp > 1) call ix_check_scalar_shape(self, "get")
        idx = ix_get_scalar(self, key)
    end procedure get_k64

    module procedure get_t32
        integer(int64) :: kb(pf_index_max_components)
        integer :: nc, j

        idx = 0_int64
        if (self%ncomp == 0) return
        nc = ix_tuple_width(self, size(key), "get")
        do j = 1, nc
            kb(j) = int(key(j), int64)
        end do
        idx = ix_get_tuple(self, kb(1:nc))
    end procedure get_t32

    module procedure get_t64
        integer :: nc

        idx = 0_int64
        if (self%ncomp == 0) return
        nc = ix_tuple_width(self, size(key), "get")
        idx = ix_get_tuple(self, key(1:nc))
    end procedure get_t64

    module procedure has_k32
        if (self%is_str .or. self%ncomp > 1) call ix_check_scalar_shape(self, "contains")
        ok = ix_get_scalar(self, int(key, int64)) > 0_int64
    end procedure has_k32

    module procedure has_k64
        if (self%is_str .or. self%ncomp > 1) call ix_check_scalar_shape(self, "contains")
        ok = ix_get_scalar(self, key) > 0_int64
    end procedure has_k64

    module procedure has_t32
        integer(int64) :: kb(pf_index_max_components)
        integer :: nc, j

        ok = .false.
        if (self%ncomp == 0) return
        nc = ix_tuple_width(self, size(key), "contains")
        do j = 1, nc
            kb(j) = int(key(j), int64)
        end do
        ok = ix_get_tuple(self, kb(1:nc)) > 0_int64
    end procedure has_t32

    module procedure has_t64
        integer :: nc

        ok = .false.
        if (self%ncomp == 0) return
        nc = ix_tuple_width(self, size(key), "contains")
        ok = ix_get_tuple(self, key(1:nc)) > 0_int64
    end procedure has_t64

    module procedure many_r1_k32_i32
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), 1, "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k32_i32(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_r1_k32_i32

    module procedure many_r1_k32_i64
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), 1, "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k32_i64(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_r1_k32_i64

    module procedure many_r1_k64_i32
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), 1, "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k64_i32(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_r1_k64_i32

    module procedure many_r1_k64_i64
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), 1, "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k64_i64(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_r1_k64_i64

    module procedure many_r2_k32_i32
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_many_rows(self, size(keys, 1, kind=int64), size(indexes, kind=int64), size(keys, 2), &
            "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k32_i32(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_r2_k32_i32

    module procedure many_r2_k32_i64
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_many_rows(self, size(keys, 1, kind=int64), size(indexes, kind=int64), size(keys, 2), &
            "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k32_i64(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_r2_k32_i64

    module procedure many_r2_k64_i32
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_many_rows(self, size(keys, 1, kind=int64), size(indexes, kind=int64), size(keys, 2), &
            "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k64_i32(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_r2_k64_i32

    module procedure many_r2_k64_i64
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_many_rows(self, size(keys, 1, kind=int64), size(indexes, kind=int64), size(keys, 2), &
            "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k64_i64(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_r2_k64_i64

    ! ============================================================================================
    ! Mutation. Guarded.
    ! ============================================================================================

    module procedure set_k32_v32
        !$omp critical (pf_index_map_guard)
        call ix_set_scalar(self, int(key, int64), int(value, int64))
        !$omp end critical (pf_index_map_guard)
    end procedure set_k32_v32

    module procedure set_k32_v64
        !$omp critical (pf_index_map_guard)
        call ix_set_scalar(self, int(key, int64), value)
        !$omp end critical (pf_index_map_guard)
    end procedure set_k32_v64

    module procedure set_k64_v32
        !$omp critical (pf_index_map_guard)
        call ix_set_scalar(self, key, int(value, int64))
        !$omp end critical (pf_index_map_guard)
    end procedure set_k64_v32

    module procedure set_k64_v64
        !$omp critical (pf_index_map_guard)
        call ix_set_scalar(self, key, value)
        !$omp end critical (pf_index_map_guard)
    end procedure set_k64_v64

    module procedure set_t32_v32
        !$omp critical (pf_index_map_guard)
        call ix_set_tuple_i32(self, key, int(value, int64))
        !$omp end critical (pf_index_map_guard)
    end procedure set_t32_v32

    module procedure set_t32_v64
        !$omp critical (pf_index_map_guard)
        call ix_set_tuple_i32(self, key, value)
        !$omp end critical (pf_index_map_guard)
    end procedure set_t32_v64

    module procedure set_t64_v32
        !$omp critical (pf_index_map_guard)
        call ix_set_tuple(self, key, int(value, int64))
        !$omp end critical (pf_index_map_guard)
    end procedure set_t64_v32

    module procedure set_t64_v64
        !$omp critical (pf_index_map_guard)
        call ix_set_tuple(self, key, value)
        !$omp end critical (pf_index_map_guard)
    end procedure set_t64_v64

    module procedure goa_k32_i32
        integer(int64) :: v

        !$omp critical (pf_index_map_guard)
        call ix_goa_scalar(self, int(key, int64), v, .true.)
        !$omp end critical (pf_index_map_guard)
        idx = int(v, int32)
    end procedure goa_k32_i32

    module procedure goa_k32_i64
        !$omp critical (pf_index_map_guard)
        call ix_goa_scalar(self, int(key, int64), idx, .false.)
        !$omp end critical (pf_index_map_guard)
    end procedure goa_k32_i64

    module procedure goa_k64_i32
        integer(int64) :: v

        !$omp critical (pf_index_map_guard)
        call ix_goa_scalar(self, key, v, .true.)
        !$omp end critical (pf_index_map_guard)
        idx = int(v, int32)
    end procedure goa_k64_i32

    module procedure goa_k64_i64
        !$omp critical (pf_index_map_guard)
        call ix_goa_scalar(self, key, idx, .false.)
        !$omp end critical (pf_index_map_guard)
    end procedure goa_k64_i64

    module procedure goa_t32_i32
        integer(int64) :: v

        !$omp critical (pf_index_map_guard)
        call ix_goa_tuple_i32(self, key, v, .true.)
        !$omp end critical (pf_index_map_guard)
        idx = int(v, int32)
    end procedure goa_t32_i32

    module procedure goa_t32_i64
        !$omp critical (pf_index_map_guard)
        call ix_goa_tuple_i32(self, key, idx, .false.)
        !$omp end critical (pf_index_map_guard)
    end procedure goa_t32_i64

    module procedure goa_t64_i32
        integer(int64) :: v

        !$omp critical (pf_index_map_guard)
        call ix_goa_tuple(self, key, v, .true.)
        !$omp end critical (pf_index_map_guard)
        idx = int(v, int32)
    end procedure goa_t64_i32

    module procedure goa_t64_i64
        !$omp critical (pf_index_map_guard)
        call ix_goa_tuple(self, key, idx, .false.)
        !$omp end critical (pf_index_map_guard)
    end procedure goa_t64_i64

    module procedure goam_r1_k64_i64
        !$omp critical (pf_index_map_guard)
        call ix_goam_1(self, keys, codes, valid, .false., threads)
        !$omp end critical (pf_index_map_guard)
    end procedure goam_r1_k64_i64

    module procedure goam_r1_k64_i32
        integer(int64), allocatable :: c(:)

        allocate(c(size(codes, kind=int64)))
        !$omp critical (pf_index_map_guard)
        call ix_goam_1(self, keys, c, valid, .true., threads)
        !$omp end critical (pf_index_map_guard)
        codes = int(c, int32)
    end procedure goam_r1_k64_i32

    module procedure goam_r1_k32_i64
        integer(int64), allocatable :: k(:)

        call ix_widen_keys_1(keys, k)
        !$omp critical (pf_index_map_guard)
        call ix_goam_1(self, k, codes, valid, .false., threads)
        !$omp end critical (pf_index_map_guard)
    end procedure goam_r1_k32_i64

    module procedure goam_r1_k32_i32
        integer(int64), allocatable :: k(:), c(:)

        call ix_widen_keys_1(keys, k)
        allocate(c(size(codes, kind=int64)))
        !$omp critical (pf_index_map_guard)
        call ix_goam_1(self, k, c, valid, .true., threads)
        !$omp end critical (pf_index_map_guard)
        codes = int(c, int32)
    end procedure goam_r1_k32_i32

    module procedure goam_r2_k64_i64
        !$omp critical (pf_index_map_guard)
        call ix_goam_n(self, keys, codes, valid, .false., threads)
        !$omp end critical (pf_index_map_guard)
    end procedure goam_r2_k64_i64

    module procedure goam_r2_k64_i32
        integer(int64), allocatable :: c(:)

        allocate(c(size(codes, kind=int64)))
        !$omp critical (pf_index_map_guard)
        call ix_goam_n(self, keys, c, valid, .true., threads)
        !$omp end critical (pf_index_map_guard)
        codes = int(c, int32)
    end procedure goam_r2_k64_i32

    module procedure goam_r2_k32_i64
        integer(int64), allocatable :: k(:,:)

        call ix_widen_keys_n(keys, k)
        !$omp critical (pf_index_map_guard)
        call ix_goam_n(self, k, codes, valid, .false., threads)
        !$omp end critical (pf_index_map_guard)
    end procedure goam_r2_k32_i64

    module procedure goam_r2_k32_i32
        integer(int64), allocatable :: k(:,:), c(:)

        call ix_widen_keys_n(keys, k)
        allocate(c(size(codes, kind=int64)))
        !$omp critical (pf_index_map_guard)
        call ix_goam_n(self, k, c, valid, .true., threads)
        !$omp end critical (pf_index_map_guard)
        codes = int(c, int32)
    end procedure goam_r2_k32_i32

    module procedure rm_k32
        logical :: hit

        !$omp critical (pf_index_map_guard)
        call ix_remove_scalar(self, int(key, int64), hit, present(found))
        !$omp end critical (pf_index_map_guard)
        if (present(found)) found = hit
    end procedure rm_k32

    module procedure rm_k64
        logical :: hit

        !$omp critical (pf_index_map_guard)
        call ix_remove_scalar(self, key, hit, present(found))
        !$omp end critical (pf_index_map_guard)
        if (present(found)) found = hit
    end procedure rm_k64

    module procedure rm_t32
        logical :: hit

        !$omp critical (pf_index_map_guard)
        call ix_remove_tuple_i32(self, key, hit, present(found))
        !$omp end critical (pf_index_map_guard)
        if (present(found)) found = hit
    end procedure rm_t32

    module procedure rm_t64
        logical :: hit

        !$omp critical (pf_index_map_guard)
        call ix_remove_tuple(self, key, hit, present(found))
        !$omp end critical (pf_index_map_guard)
        if (present(found)) found = hit
    end procedure rm_t64

    ! ============================================================================================
    ! Introspection. Read-only, so unguarded like the lookups.
    ! ============================================================================================

    module procedure map_nkeys
        n = self%nk
    end procedure map_nkeys

    module procedure map_ncomponents
        n = self%ncomp
        ! A string map is a two-component tuple map underneath, which is not what a caller asking
        ! about ITS keys means: from the outside, one string is one key.
        if (self%is_str) n = 1
    end procedure map_ncomponents

    module procedure map_get_method
        select case (self%backend)
        case (IX_DIRECT)
            method = "direct"
        case (IX_HASH)
            method = "hash"
        case (IX_SORTED)
            method = "sorted"
        case default
            method = ""
        end select
        ! A map that has never been built or `%init`ed reports no backend at all, whatever the
        ! `backend` component happens to hold -- it defaults to `IX_DIRECT` for the benefit of the
        ! lookup fast path, not because a fresh map is a direct-backend map.
        if (self%ncomp == 0) method = ""
    end procedure map_get_method

    module procedure map_memory_bytes
        b = 0_int64
        if (allocated(self%dvals)) b = b + 8_int64 * size(self%dvals, kind=int64)
        if (allocated(self%dkmin)) b = b + 8_int64 * size(self%dkmin, kind=int64)
        if (allocated(self%dkmax)) b = b + 8_int64 * size(self%dkmax, kind=int64)
        if (allocated(self%drange)) b = b + 8_int64 * size(self%drange, kind=int64)
        if (allocated(self%dstride)) b = b + 8_int64 * size(self%dstride, kind=int64)
        if (allocated(self%slots)) b = b + 16_int64 * size(self%slots, kind=int64)
        if (allocated(self%hrec)) b = b + 8_int64 * size(self%hrec, kind=int64)
        if (allocated(self%skeys)) b = b + 8_int64 * size(self%skeys, kind=int64)
        if (allocated(self%svals)) b = b + 8_int64 * size(self%svals, kind=int64)
        if (allocated(self%soff)) b = b + 8_int64 * size(self%soff, kind=int64)
        if (allocated(self%sdat)) b = b + size(self%sdat, kind=int64)
        if (allocated(self%sval)) b = b + 8_int64 * size(self%sval, kind=int64)
    end procedure map_memory_bytes

    module procedure map_probe_stats
        integer(int64) :: span, depth
        real(real64) :: mp

        max_probe = 0_int64
        if (present(mean_probe)) mean_probe = 0.0_real64
        if (self%nk == 0_int64) return
        select case (self%backend)
        case (IX_DIRECT)
            ! There is no probing: the offset is computed, not searched.
            max_probe = 1_int64
            if (present(mean_probe)) mean_probe = 1.0_real64
        case (IX_SORTED)
            ! The binary search's worst depth, which is what "probe length" means for a structure
            ! that halves rather than walks: the smallest d with 2**d > nk.
            depth = 0_int64
            span = 1_int64
            do while (span <= self%nk)
                span = span * 2_int64
                depth = depth + 1_int64
            end do
            max_probe = depth
            if (present(mean_probe)) mean_probe = real(depth, real64)
        case (IX_HASH)
            ! `ix_hash_probe_stats` reports both, so the mean is computed and discarded when the
            ! caller did not ask for it. The alternative -- a second scan that skips the sum -- is
            ! a copy of the walk for a saving of one division on a cold path.
            call ix_hash_probe_stats(self, max_probe, mp)
            if (present(mean_probe)) mean_probe = mp
        end select
    end procedure map_probe_stats

    module procedure map_keys_r1
        integer(int64), allocatable :: pairs(:,:)

        if (self%is_str) error stop "pf_index_map%keys: this map holds string keys; " // &
            "ask for a parquet_string_column"
        if (self%ncomp > 1) error stop "pf_index_map%keys: this map has composite keys; " // &
            "ask for a rank-2 list"
        allocate(list(self%nk))
        if (self%nk == 0_int64) return
        allocate(pairs(self%nk, 1))
        call ix_collect_keys(self, pairs)
        list = pairs(:, 1)
    end procedure map_keys_r1

    module procedure map_keys_r2
        integer :: nc

        if (self%is_str) error stop "pf_index_map%keys: this map holds string keys; " // &
            "ask for a parquet_string_column"
        nc = self%ncomp
        if (nc < 1) nc = 1
        allocate(list(self%nk, nc))
        if (self%nk == 0_int64) return
        call ix_collect_keys(self, list)
    end procedure map_keys_r2

    ! ============================================================================================
    ! The build's thread rule, reported
    ! ============================================================================================

    ! Both take `ix_threads_rule`, never `ix_threads_for`: this is a public query, so recording
    ! from here would let any caller overwrite what a build had just reported and make every test
    ! reading `parquet_debug_index_threads_used` depend on nobody having asked in between.
    module procedure pf_index_threads_i32
        nt = ix_threads_rule(int(n, int64), what="build")
    end procedure pf_index_threads_i32

    module procedure pf_index_threads_i64
        nt = ix_threads_rule(n, what="build")
    end procedure pf_index_threads_i64

    module procedure parquet_debug_index_threads_used
        n = dbg_index_threads_used
    end procedure parquet_debug_index_threads_used

    module procedure parquet_debug_index_get_many_threads_used
        n = dbg_index_get_many_threads_used
    end procedure parquet_debug_index_get_many_threads_used

    module procedure parquet_debug_index_concurrent_builds
        logical :: do_reset

        do_reset = .false.
        if (present(reset)) do_reset = reset
        !$omp atomic read
        n = dbg_index_concurrent_builds
        if (do_reset) then
            !$omp atomic write
            dbg_index_concurrent_builds = dbg_index_builds_in_flight
        end if
    end procedure parquet_debug_index_concurrent_builds

    ! ============================================================================================
    ! The serialised abort, and what a build does under the guard
    ! ============================================================================================

    module procedure ix_abort
        !$omp critical (pf_index_abort_guard)
        error stop msg
        !$omp end critical (pf_index_abort_guard)
    end procedure ix_abort

    !> Records that one more build is running outside the guard, for
    !! `parquet_debug_index_concurrent_builds`; `ix_adopt` records its end.
    subroutine ix_build_begin()
        integer :: now

        !$omp atomic capture
        dbg_index_builds_in_flight = dbg_index_builds_in_flight + 1
        now = dbg_index_builds_in_flight
        !$omp end atomic
        !$omp atomic update
        dbg_index_concurrent_builds = max(dbg_index_concurrent_builds, now)
    end subroutine ix_build_begin

    !> Moves a freshly built map's storage into `self` under the guard, releasing what `self` held.
    !!
    !! **This is the whole of what a `%build` does under the lock**, and the reason a build no
    !! longer serialises with every other mutation in the process: the scan, the choice of backend
    !! and the filling of the table all happen on a local object the calling thread owns, and only
    !! this swap -- a dozen `move_alloc`s and the scalars, microseconds -- takes
    !! `pf_index_map_guard`. Two threads building two maps run at full speed side by side; a build
    !! and a lookup of the SAME map are as unsupported as they always were (the spec's threading
    !! paragraph), the window in which the lookup could see half a map now being this swap.
    !!
    !! **Every component of `pf_index_map` appears below, and `tools/check_source_conventions.py`
    !! holds that** (`pf_index_map components are adopted and reset`): a component added to the
    !! type and forgotten here would leave the built map without it, silently, on every `%build`.
    subroutine ix_adopt(self, fresh)
        type(pf_index_map), intent(inout) :: self  !! the caller's map; whatever it held is released.
        type(pf_index_map), intent(inout) :: fresh !! the built map; empty on return.

        !$omp critical (pf_index_map_guard)
        call ix_reset_storage(self)
        call move_alloc(fresh%dvals, self%dvals)
        call move_alloc(fresh%dkmin, self%dkmin)
        call move_alloc(fresh%dkmax, self%dkmax)
        call move_alloc(fresh%drange, self%drange)
        call move_alloc(fresh%dstride, self%dstride)
        call move_alloc(fresh%slots, self%slots)
        call move_alloc(fresh%hrec, self%hrec)
        call move_alloc(fresh%skeys, self%skeys)
        call move_alloc(fresh%svals, self%svals)
        call move_alloc(fresh%soff, self%soff)
        call move_alloc(fresh%sdat, self%sdat)
        call move_alloc(fresh%sval, self%sval)
        self%backend = fresh%backend
        self%ncomp = fresh%ncomp
        self%nk = fresh%nk
        self%next_auto = fresh%next_auto
        self%kmin1 = fresh%kmin1
        self%kmax1 = fresh%kmax1
        self%hcap = fresh%hcap
        self%is_str = fresh%is_str
        self%nstr = fresh%nstr
        self%nchr = fresh%nchr
        self%snext = fresh%snext
        !$omp end critical (pf_index_map_guard)
        !$omp atomic update
        dbg_index_builds_in_flight = dbg_index_builds_in_flight - 1
    end subroutine ix_adopt

    ! ============================================================================================
    ! Private workers. Everything below takes a `type(pf_index_map)` dummy, and nothing below
    ! calls a public entry above -- the named critical is not recursive, so doing so would
    ! deadlock rather than fail to build.
    ! ============================================================================================

    ! ---- Argument checks reached from the lookup path, so `pure` with static messages ----

    !> Aborts when a scalar key is presented to a composite map.
    !!
    !! The messages on this path carry no numbers, and cannot: a `pure` procedure may not perform
    !! I/O, so there is no way to format the component count into the text. That is the right
    !! trade here -- a hot path should not be building strings -- and the mutation paths, which are
    !! not `pure`, do name the offending values.
    pure subroutine ix_check_scalar_shape(self, what)
        type(pf_index_map), intent(in) :: self !! the map.
        character(len=*), intent(in) :: what   !! procedure name, for the message.

        ! The string test comes first: a string map IS a two-component map underneath, and the
        ! composite message would send the caller off to build a tuple.
        if (self%is_str) error stop "pf_index_map%" // what // &
            ": this map holds string keys; look up with a string key"
        if (self%ncomp > 1) error stop "pf_index_map%" // what // &
            ": this map has composite keys; pass the whole key tuple, not a scalar"
    end subroutine ix_check_scalar_shape

    !> Validates a presented tuple width against the map's own, and returns it.
    pure function ix_tuple_width(self, n, what) result(nc)
        type(pf_index_map), intent(in) :: self !! the map.
        integer, intent(in) :: n               !! the presented tuple length.
        character(len=*), intent(in) :: what   !! procedure name, for the message.
        integer :: nc                          !! `n`, once it is known to match.

        if (self%is_str) error stop "pf_index_map%" // what // &
            ": this map holds string keys; look up with a string key"
        if (n /= self%ncomp) error stop "pf_index_map%" // what // &
            ": the key tuple's length does not match this map's component count"
        nc = n
    end function ix_tuple_width

    !> Validates a bulk call's shapes and returns the row count to walk.
    !!
    !! The component check is skipped for a map that was never built, so a bulk lookup against one
    !! answers 0 for every key rather than aborting -- the same rule the scalar `%get` follows.
    !! Shared by `%get_many` and `%get_or_add_many`, which is what `what` is for.
    pure function ix_many_rows(self, nrows, nidx, nc, what) result(n)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: nrows    !! keys presented.
        integer(int64), intent(in) :: nidx     !! length of the answer array.
        integer, intent(in) :: nc              !! components presented per key.
        character(len=*), intent(in) :: what   !! procedure name, for the message.
        integer(int64) :: n                    !! rows to walk.

        if (nrows /= nidx) error stop "pf_index_map%" // what // &
            ": the keys and the answer array must have the same length"
        if (self%is_str) error stop "pf_index_map%" // what // &
            ": this map holds string keys; look up with string keys"
        if (self%ncomp > 0 .and. nc /= self%ncomp) error stop "pf_index_map%" // what // &
            ": the keys' component count does not match this map's"
        n = nrows
    end function ix_many_rows

    !> Checks that a `valid=` mask has exactly one entry per key.
    !!
    !! **Deliberately NOT `pure`, although it could be.** A pure subroutine whose only effect is an
    !! `error stop` is a call ifx 2026.1.1 deletes at `-O0 -check all` (fpm's debug profile): the
    !! three mask-length error scenarios then walk past the mask and die on the bounds check
    !! instead of aborting with this message, while the same binary at `-O2`, and gfortran at
    !! both profiles, abort here. Reproduced with a twelve-line program; an impure call is never
    !! deleted. Same family as CLAUDE.md's "A scenario whose abort is inside a `pure` function
    !! must USE the result" -- a guard with no result to use has to be impure instead.
    subroutine ix_check_mask_len(nmask, n, what)
        integer(int64), intent(in) :: nmask    !! entries in the mask.
        integer(int64), intent(in) :: n        !! keys presented.
        character(len=*), intent(in) :: what   !! procedure name, for the message.

        if (nmask /= n) call ix_abort("pf_index_map%" // what // &
            ": valid= must have exactly one element per key")
    end subroutine ix_check_mask_len

    !> The contiguous slice of rows `1 .. n` that chunk `c` of `nchunk` owns.
    !!
    !! Balanced to within one row, and a function of `c` alone, so that no chunk's bounds depend
    !! on another's -- which is what lets the chunk loop be a plain `parallel do`. The products
    !! stay far below `huge(int64)` for any row count an array this module is handed can have.
    pure subroutine ix_chunk_bounds(n, nchunk, c, lo, hi)
        integer(int64), intent(in) :: n        !! rows in all.
        integer, intent(in) :: nchunk          !! chunks the rows are cut into; >= 1.
        integer, intent(in) :: c               !! this chunk, in `1 .. nchunk`.
        integer(int64), intent(out) :: lo      !! first row of the chunk.
        integer(int64), intent(out) :: hi      !! last row of the chunk; `lo - 1` for an empty one.

        lo = 1_int64 + (n * int(c - 1, int64)) / int(nchunk, int64)
        hi = (n * int(c, int64)) / int(nchunk, int64)
    end subroutine ix_chunk_bounds

    ! ---- The partitioned insert's plan, shared by the hash and string submodules ----

    !> Slots per partition of a partitioned insert, as a shift: a partition holds
    !! `2**ix_part_shift(cap, nt)` slots.
    !!
    !! As large as leaves at least four partitions per thread -- enough for `schedule(dynamic)`
    !! to balance a team beside other load -- and never below `2**IX_PART_MIN_LG` slots, so
    !! that the share of probe chains reaching a boundary (about the mean chain length over the
    !! partition size) stays negligible and the serial spill pass stays short. A table smaller
    !! than that minimum is one partition, whose one boundary is the wrap at the table's end.
    pure function ix_part_shift(cap, nt) result(lg)
        integer(int64), intent(in) :: cap !! the table's slot count; a power of two.
        integer, intent(in) :: nt        !! the team.
        integer :: lg                    !! log2 of the slots per partition.
        integer(int64) :: want

        lg = trailz(cap)
        want = 4_int64 * int(nt, int64)
        do while (lg > IX_PART_MIN_LG .and. shiftr(cap, lg) < want)
            lg = lg - 1
        end do
    end function ix_part_shift

    !> Lays out a partitioned insert: from every row's partition (or -1 for a row the pass
    !! skips), where each partition's rows start and where each chunk of the row array writes
    !! its rows of each partition, so that the caller's scatter needs no synchronisation.
    !!
    !! One histogram per chunk on the team, then a prefix sum in partition-major, chunk-minor
    !! order: `pstart(p)` is where partition `p`'s rows start (0-based; `pstart(np)` is the
    !! total) and `cursor(p, c)` is where chunk `c` writes its first row of partition `p`. The
    !! rows of one partition therefore keep the caller's order -- chunk `c`'s rows precede chunk
    !! `c + 1`'s and each chunk walks in order -- which is what makes a pass deterministic for a
    !! given team. Chunks are `ix_chunk_bounds`'s, indexed by chunk rather than by thread, so
    !! a runtime that grants fewer threads than asked changes nothing.
    subroutine ix_partition_plan(pid, n, np, nt, pstart, cursor)
        integer(int32), intent(in) :: pid(:)         !! partition per row, or -1 to skip the row.
        integer(int64), intent(in) :: n              !! rows.
        integer, intent(in) :: np                    !! partitions: the table's slots over `2**lg`.
        integer, intent(in) :: nt                    !! the team; also the chunk count.
        integer(int64), intent(out) :: pstart(0:)    !! `(0:np)`; receives the partition starts.
        integer(int64), intent(out) :: cursor(0:, :) !! `(0:np-1, nt)`; receives the write cursors.
        integer(int64) :: i, lo, hi, run, cnt
        integer :: c, p

        cursor = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi, i, p) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            do i = lo, hi
                if (pid(i) < 0_int32) cycle
                p = int(pid(i))
                cursor(p, c) = cursor(p, c) + 1_int64
            end do
        end do
        run = 0_int64
        do p = 0, np - 1
            pstart(p) = run
            do c = 1, nt
                cnt = cursor(p, c)
                cursor(p, c) = run
                run = run + cnt
            end do
        end do
        pstart(np) = run
    end subroutine ix_partition_plan

    !> Aborts, with the narrowing check's own message, when a code of a threaded
    !! `%get_or_add_many` will not fit `int32`; the serial pass checks each row as it goes.
    subroutine ix_check_codes_fit(codes)
        integer(int64), intent(inout) :: codes(:) !! the finished codes; unchanged on return.
        integer(int64) :: i

        if (size(codes, kind=int64) == 0_int64) return
        if (maxval(codes) <= int(huge(0_int32), int64)) return
        do i = 1_int64, size(codes, kind=int64)
            codes(i) = ix_narrow_check(codes(i))
        end do
    end subroutine ix_check_codes_fit

    !> Narrows an index value to `int32`, aborting rather than truncating.
    pure function ix_narrow(v) result(out)
        integer(int64), intent(in) :: v !! the stored value.
        integer(int32) :: out           !! the same value as `int32`.

        if (v > int(huge(0_int32), int64)) error stop "pf_index_map: " // &
            "a stored index value is too large for an int32 result; take it as int64"
        out = int(v, int32)
    end function ix_narrow

    !> Checks that an index value fits `int32` and returns it unchanged.
    !!
    !! Used by the `int32` mutation forms so that the abort happens while the caller still holds
    !! the map's guard, which keeps at most one thread on the fatal path.
    pure function ix_narrow_check(v) result(out)
        integer(int64), intent(in) :: v !! the value about to be narrowed.
        integer(int64) :: out           !! `v`, unchanged.

        out = int(ix_narrow(v), int64)
    end function ix_narrow_check

    ! ---- Widening helpers ----

    !> Widens a rank-1 `int32` key array into a fresh `int64` one.
    subroutine ix_widen_keys_1(keys, out)
        integer(int32), intent(in) :: keys(:)                !! the caller's keys.
        integer(int64), allocatable, intent(out) :: out(:)   !! the widened copy.

        allocate(out(size(keys, kind=int64)))
        out = int(keys, int64)
    end subroutine ix_widen_keys_1

    !> Widens a rank-2 `int32` key array into a fresh `int64` one.
    subroutine ix_widen_keys_n(keys, out)
        integer(int32), intent(in) :: keys(:,:)                !! the caller's keys.
        integer(int64), allocatable, intent(out) :: out(:,:)   !! the widened copy.

        allocate(out(size(keys, 1, kind=int64), size(keys, 2, kind=int64)))
        out = int(keys, int64)
    end subroutine ix_widen_keys_n

    !> Widens an `int32` values array into a fresh `int64` one.
    subroutine ix_widen_values(values, out)
        integer(int32), intent(in) :: values(:)              !! the caller's values.
        integer(int64), allocatable, intent(out) :: out(:)   !! the widened copy.

        allocate(out(size(values, kind=int64)))
        out = int(values, int64)
    end subroutine ix_widen_values

    !> Validates a presented tuple against the map and widens it into a stack buffer.
    subroutine ix_widen_tuple(self, key, kb, nc, what)
        type(pf_index_map), intent(in) :: self  !! the map.
        integer(int32), intent(in) :: key(:)    !! the caller's tuple.
        integer(int64), intent(out) :: kb(:)    !! buffer of at least `pf_index_max_components`.
        integer, intent(out) :: nc              !! the validated component count.
        character(len=*), intent(in) :: what    !! procedure name, for the message.
        integer :: j

        nc = ix_tuple_width(self, size(key), what)
        do j = 1, nc
            kb(j) = int(key(j), int64)
        end do
    end subroutine ix_widen_tuple

    ! ---- Storage lifecycle ----

    !> Releases every allocation and restores the as-new component values.
    !!
    !! **The scalar resets are as load-bearing as the deallocations.** Restoring `backend` to
    !! `IX_DIRECT` and the range to the empty `0 .. -1` is what makes `%get` on a cleared map
    !! answer 0 instead of indexing storage that is no longer there.
    subroutine ix_reset_storage(self)
        type(pf_index_map), intent(inout) :: self !! the map.

        if (allocated(self%dvals)) deallocate(self%dvals)
        if (allocated(self%dkmin)) deallocate(self%dkmin)
        if (allocated(self%dkmax)) deallocate(self%dkmax)
        if (allocated(self%drange)) deallocate(self%drange)
        if (allocated(self%dstride)) deallocate(self%dstride)
        if (allocated(self%slots)) deallocate(self%slots)
        if (allocated(self%hrec)) deallocate(self%hrec)
        if (allocated(self%skeys)) deallocate(self%skeys)
        if (allocated(self%svals)) deallocate(self%svals)
        if (allocated(self%soff)) deallocate(self%soff)
        if (allocated(self%sdat)) deallocate(self%sdat)
        if (allocated(self%sval)) deallocate(self%sval)
        self%backend = IX_DIRECT
        self%ncomp = 0
        self%nk = 0_int64
        self%next_auto = 0_int64
        self%kmin1 = 0_int64
        self%kmax1 = -1_int64
        self%hcap = 0_int64
        self%is_str = .false.
        self%nstr = 0_int64
        self%nchr = 0_int64
        self%snext = 0_int64
    end subroutine ix_reset_storage

    !> Empties the map without releasing anything.
    subroutine ix_do_reset(self)
        type(pf_index_map), intent(inout) :: self !! the map.

        if (self%ncomp == 0) return
        select case (self%backend)
        case (IX_DIRECT)
            if (allocated(self%dvals)) self%dvals = 0_int64
        case (IX_HASH)
            if (allocated(self%slots)) self%slots(:)%val = 0_int64
            if (allocated(self%hrec)) self%hrec = 0_int64
        case (IX_SORTED)
            ! Nothing to zero: a sorted map is read through `1 .. nk`, so dropping the count is
            ! what empties it, and the arrays stay for a rebuild that will overwrite them.
            continue
        end select
        self%nk = 0_int64
        self%next_auto = 0_int64
        ! A string map's table is the `IX_HASH` arm above (its slots hold string positions);
        ! the store beside it is emptied by its two counters, its arrays kept for the refill.
        if (self%is_str) then
            self%nstr = 0_int64
            self%nchr = 0_int64
            self%snext = 0_int64
            self%soff(1) = 0_int64
        end if
    end subroutine ix_do_reset

    !> Turns a freshly reset map into an empty STRING-keyed one: the composite hash table over
    !! `(hash, occurrence)` tuples, and the string store beside it, both sized for `capacity`
    !! keys. Every string-keyed path starts a map through here, whether `%init(strings=)`, a
    !! string `%build`, or the first string `%set`/`%get_or_add` on a fresh object.
    subroutine ix_str_start(self, capacity)
        type(pf_index_map), intent(inout) :: self !! the map, already reset.
        integer(int64), intent(in) :: capacity    !! keys to make room for; 0 for the minimum.

        self%is_str = .true.
        self%ncomp = 2
        self%backend = IX_HASH
        call ix_hash_reserve(self, capacity)
        call ix_str_grow_rows(self, capacity)
        call ix_str_grow_bytes(self, capacity * IX_STR_BYTES_PER_KEY)
        self%soff(1) = 0_int64
    end subroutine ix_str_start

    !> Makes the string store's row arrays hold at least `need` strings, doubling when it grows
    !! so that a run of appends is amortised O(1). `soff` has one entry more than `sval`.
    subroutine ix_str_grow_rows(self, need)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: need        !! strings the store must be able to hold.
        integer(int64), allocatable :: noff(:), nval(:)
        integer(int64) :: have, want

        have = 0_int64
        if (allocated(self%sval)) have = size(self%sval, kind=int64)
        if (have >= need .and. have > 0_int64) return
        want = max(need, IX_STR_MIN_ROWS, 2_int64 * have)
        allocate(noff(want + 1_int64), nval(want))
        if (have > 0_int64) then
            noff(1:self%nstr + 1_int64) = self%soff(1:self%nstr + 1_int64)
            nval(1:self%nstr) = self%sval(1:self%nstr)
        else
            noff(1) = 0_int64
        end if
        call move_alloc(noff, self%soff)
        call move_alloc(nval, self%sval)
    end subroutine ix_str_grow_rows

    !> Makes the string store's payload hold at least `need` bytes, doubling when it grows.
    subroutine ix_str_grow_bytes(self, need)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: need        !! bytes the payload must be able to hold.
        character(len=1), allocatable :: ndat(:)
        integer(int64) :: have, want

        have = 0_int64
        if (allocated(self%sdat)) have = size(self%sdat, kind=int64)
        if (have >= need .and. have > 0_int64) return
        want = max(need, IX_STR_MIN_BYTES, 2_int64 * have)
        allocate(ndat(want))
        if (self%nchr > 0_int64) ndat(1:self%nchr) = self%sdat(1:self%nchr)
        call move_alloc(ndat, self%sdat)
    end subroutine ix_str_grow_bytes

    !> `%init`: an empty map ready for incremental insertion, integer- or string-keyed.
    subroutine ix_do_init(self, capacity, method, ncomp, strings)
        type(pf_index_map), intent(inout) :: self       !! the map.
        integer, intent(in), optional :: capacity       !! keys to pre-size for.
        character(len=*), intent(in), optional :: method !! backend token; hash or auto only.
        integer, intent(in), optional :: ncomp          !! components per key; 1 by default.
        logical, intent(in), optional :: strings        !! `.true.` for a string-keyed map.
        integer :: want, nc
        integer(int64) :: cap
        logical :: str

        call ix_resolve_method(method, want, .false., "init")
        str = .false.
        if (present(strings)) str = strings
        nc = 1
        if (present(ncomp)) nc = ncomp
        ! A string key has exactly one component from the caller's side, whatever the tuple
        ! underneath is, so the two arguments cannot both be meant.
        if (str .and. nc /= 1) call ix_abort("pf_index_map%init: a string key has one component; " // &
            "leave ncomp= absent (or 1) with strings=.true.")
        call ix_check_ncomp(nc, "init")
        cap = 0_int64
        if (present(capacity)) then
            if (capacity < 0) call ix_abort("pf_index_map%init: capacity must be >= 0")
            cap = int(capacity, int64)
        end if
        call ix_reset_storage(self)
        if (str) then
            call ix_str_start(self, cap)
        else
            self%ncomp = nc
            self%backend = IX_HASH
            call ix_hash_reserve(self, cap)
        end if
    end subroutine ix_do_init

    !> `%reserve`: make room for `n` keys without rehashing later.
    subroutine ix_do_reserve(self, n)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: n           !! keys to make room for.

        if (n < 0_int64) call ix_abort("pf_index_map%reserve: n must be >= 0")
        if (self%ncomp == 0) call ix_do_init(self, ncomp=1)
        if (self%backend /= IX_HASH) return
        call ix_hash_reserve(self, n)
        ! The store's row arrays follow the table; its payload cannot, since nothing is known
        ! about the strings' lengths yet, so that side keeps growing by doubling as they arrive.
        if (self%is_str) call ix_str_grow_rows(self, n)
    end subroutine ix_do_reserve

    !> Validates a component count against the module's fixed maximum.
    !!
    !! `owner` is the type-name prefix of the message, `pf_index_map%` unless the caller is the
    !! multimap (`src/parquet_index_multi.f90`), which shares this guard and must not report the
    !! wrong type. The same optional prefix is on the three guards below for the same reason.
    subroutine ix_check_ncomp(nc, what, owner)
        integer, intent(in) :: nc            !! the requested component count.
        character(len=*), intent(in) :: what !! procedure name, for the message.
        character(len=*), intent(in), optional :: owner !! type prefix; `pf_index_map%` by default.
        character(len=32) :: t
        character(len=:), allocatable :: pfx

        call ix_owner_of(owner, pfx)
        if (nc < 1) call ix_abort(pfx // what // ": a key must have at least one component")
        if (nc > pf_index_max_components) then
            write (t, "(i0)") pf_index_max_components
            call ix_abort(pfx // what // ": a key may have at most " // trim(t) // &
                " components (pf_index_max_components)")
        end if
    end subroutine ix_check_ncomp

    !> The type-name prefix a shared guard's message carries: the caller's `owner`, or the map's.
    subroutine ix_owner_of(owner, pfx)
        character(len=*), intent(in), optional :: owner       !! the caller's prefix, or absent.
        character(len=:), allocatable, intent(out) :: pfx     !! the prefix to print.

        if (present(owner)) then
            pfx = owner
        else
            pfx = "pf_index_map%"
        end if
    end subroutine ix_owner_of

    !> Resolves a `method=` token to a backend, or to `IX_WANT_AUTO`.
    !!
    !! Case-insensitive, like every other token argument in this library. `allow_frozen` is what
    !! separates `%build` from `%init`: the direct backend needs the whole key range up front and
    !! the sorted backend cannot be added to at all, so neither can start an incremental map.
    subroutine ix_resolve_method(method, want, allow_frozen, what, owner)
        character(len=*), intent(in), optional :: method !! the caller's token, or absent.
        integer, intent(out) :: want                     !! resolved backend, or `IX_WANT_AUTO`.
        logical, intent(in) :: allow_frozen              !! whether build-only backends are accepted.
        character(len=*), intent(in) :: what             !! procedure name, for the message.
        character(len=*), intent(in), optional :: owner  !! type prefix; `pf_index_map%` by default.
        character(len=16) :: tok
        character(len=:), allocatable :: pfx

        want = IX_WANT_AUTO
        if (.not. present(method)) return
        call ix_owner_of(owner, pfx)
        if (len_trim(method) > len(tok)) call ix_abort(pfx // what // &
            ": unknown method (accepted: ""auto"", ""direct"", ""hash"", ""sorted"")")
        tok = method
        call pf_to_lower(tok)
        select case (trim(tok))
        case ("auto")
            want = IX_WANT_AUTO
        case ("hash")
            want = IX_HASH
        case ("direct")
            if (.not. allow_frozen) call ix_abort(pfx // what // &
                ": method=""direct"" needs the key range up front, so it is only available " // &
                "on %build; use ""hash"" for an incremental map")
            want = IX_DIRECT
        case ("sorted")
            if (.not. allow_frozen) call ix_abort(pfx // what // &
                ": method=""sorted"" is frozen once built, so it is only available on %build; " // &
                "use ""hash"" for an incremental map")
            want = IX_SORTED
        case default
            call ix_abort(pfx // what // ": unknown method """ // trim(tok) // &
                """ (accepted: ""auto"", ""direct"", ""hash"", ""sorted"")")
        end select
    end subroutine ix_resolve_method

    ! ---- The automatic backend choice ----

    !> Slots the automatic choice is willing to spend on the direct backend for `n` keys.
    !!
    !! `max(IX_DIRECT_FLOOR, IX_DIRECT_PER_KEY * n)`, with the multiplication guarded rather than
    !! assumed: for an `n` above `huge / 4` the product would overflow, and the answer there is
    !! "no bound worth computing" rather than a wrapped negative one.
    pure function ix_budget(n) result(b)
        integer(int64), intent(in) :: n !! keys being indexed.
        integer(int64) :: b             !! slots the direct backend may use.

        if (n > huge(0_int64) / IX_DIRECT_PER_KEY) then
            b = huge(0_int64)
            return
        end if
        b = IX_DIRECT_PER_KEY * n
        if (b < IX_DIRECT_FLOOR) b = IX_DIRECT_FLOOR
    end function ix_budget

    !> Whether `lo .. hi` spans no more than `budget` positions, and how many it does span.
    !!
    !! **Written so that no intermediate can overflow, which the obvious form does.**
    !! `hi - lo + 1` overflows whenever the span exceeds `huge`, and an `int64` key set is entitled
    !! to do exactly that -- `{-huge, huge}` is two keys. So the span-exceeds-the-domain case is
    !! ruled out first, using a bound formed from `huge` and never from the most-negative constant
    !! (`feature_risks.md` Risk-125 is nagfor mis-evaluating that constant); then the subtraction
    !! is known safe; then the comparison is made BEFORE the `+ 1` that would itself overflow at
    !! the very top of the domain.
    pure subroutine ix_span_ok(lo, hi, budget, span, ok)
        integer(int64), intent(in) :: lo      !! smallest key.
        integer(int64), intent(in) :: hi      !! largest key.
        integer(int64), intent(in) :: budget  !! most positions the caller will accept; >= 1.
        integer(int64), intent(out) :: span   !! positions spanned, or 0 when that is too many.
        logical, intent(out) :: ok            !! `.true.` when the span fits the budget.
        integer(int64) :: d

        ok = .false.
        span = 0_int64
        ! NESTED, not `.and.`-ed: Fortran does not short-circuit, so the one-line form evaluates
        ! `huge(hi) + lo` for a NON-NEGATIVE `lo` too, which overflows for any `lo > 0` -- the
        ! guard's own overflow, in the guard against overflow. Silent on gfortran, ifx and flang;
        ! nagfor's `-C=intovf` aborts on it ("INTEGER(int64) overflow for 9223372036854775807 +
        ! 10"). Same class as the bounds-checking instances CLAUDE.md records under
        ! "`.and.` does not short-circuit"; the fix there is the fix here.
        if (lo < 0_int64) then
            if (hi > huge(hi) + lo) return
        end if
        d = hi - lo
        if (d > budget - 1_int64) return
        span = d + 1_int64
        ok = .true.
    end subroutine ix_span_ok

    !> Multiplies a component's span into a running product, refusing before it can overflow.
    !!
    !! `budget / span < total` says `total * span > budget` without forming the product, which is
    !! the whole point: a composite key whose components are individually small can have a product
    !! of ranges far above `huge`, and the heuristic has to reject that rather than wrap into a
    !! plausible-looking small number and then allocate against it.
    pure subroutine ix_product_ok(total, span, budget, out, ok)
        integer(int64), intent(in) :: total   !! product so far; >= 1.
        integer(int64), intent(in) :: span    !! this component's span; >= 1.
        integer(int64), intent(in) :: budget  !! most positions the caller will accept.
        integer(int64), intent(out) :: out    !! the new product, or 0 when it is too large.
        logical, intent(out) :: ok            !! `.true.` when the product still fits.

        ok = .false.
        out = 0_int64
        if (span < 1_int64) return
        if (budget / span < total) return
        out = total * span
        ok = out <= budget
    end subroutine ix_product_ok

    ! ---- The build's thread rule ----

    !> Threads a build should use for `n` keys, or a bulk lookup for `n` rows.
    !!
    !! An explicit request wins and is honoured whatever the size -- the caller has said what they
    !! want -- but is still clamped to what this process's CPU affinity allows, because opening
    !! more threads than there are processors is slower than not threading at all. Otherwise the
    !! automatic answer comes from `parquet_auto_thread_count`, which is where the
    !! serial-inside-a-parallel-region rule and the affinity clamp live, and is then bounded by the
    !! work available so that a mid-sized build gets the few threads that pay rather than a full
    !! team that does not.
    function ix_threads_rule(n, threads, what) result(nt)
        integer(int64), intent(in) :: n              !! rows the build or lookup will process.
        integer, intent(in), optional :: threads     !! the caller's request, or absent.
        character(len=*), intent(in) :: what         !! procedure name, for the message.
        integer :: nt                                !! threads to use; 1 means serial.
        integer(int64) :: nt_work

        if (present(threads)) then
            if (threads < 1) call ix_abort("pf_index_map%" // what // ": threads= must be at least 1")
            nt = parquet_clamp_to_affinity(threads, "index")
            return
        end if
        nt = parquet_auto_thread_count(cfg_index_threads, "index")
        if (n < 2_int64 * IX_MIN_KEYS_PER_THREAD) then
            nt = 1
        else
            nt_work = n / IX_MIN_KEYS_PER_THREAD
            if (nt_work < int(nt, int64)) nt = int(nt_work)
        end if
    end function ix_threads_rule

    !> The rule, and a record of what it answered for `parquet_debug_index_threads_used`.
    !!
    !! **Every build path must come through here rather than through `ix_threads_rule` directly**,
    !! or the debug counter goes stale and the threading tests that read it silently assert against
    !! the previous build. Splitting the two is what makes that structural rather than remembered:
    !! `pf_index_threads` -- a public query a user may call at any time, and from any thread --
    !! takes the rule without the record, so it cannot clobber what a build just reported.
    !!
    !! Reached from the build workers, which run OUTSIDE `pf_index_map_guard` (see `ix_adopt`),
    !! and from `%get_or_add_many`, a build of the keys it does not find; so the store below is
    !! atomic: two builds resolving at once cannot tear it, and the record is then the last to
    !! resolve.
    function ix_threads_for(n, threads, what) result(nt)
        integer(int64), intent(in) :: n              !! keys the build will process.
        integer, intent(in), optional :: threads     !! the caller's request, or absent.
        character(len=*), intent(in), optional :: what !! procedure name for the message; `build` by default.
        integer :: nt                                !! threads to use; 1 means serial.

        if (present(what)) then
            nt = ix_threads_rule(n, threads, what)
        else
            nt = ix_threads_rule(n, threads, "build")
        end if
        !$omp atomic write
        dbg_index_threads_used = nt
    end function ix_threads_for

    !> The rule for a bulk lookup over `n` rows, and a record of what it answered for
    !! `parquet_debug_index_get_many_threads_used`.
    !!
    !! The lookup twin of `ix_threads_for`, with a counter of its own for the reason the spec
    !! gives on `dbg_index_get_many_threads_used`: this runs lock-free on whichever thread called
    !! `%get_many`, so it must not share a store with the build's guarded record. The store is
    !! atomic so that two bulk lookups resolving at once cannot tear it. Every `%get_many`
    !! specific must come through here rather than through `ix_threads_rule` directly, or the
    !! counter goes stale and the threading tests that read it assert against the previous call.
    function ix_lookup_threads_for(n, threads) result(nt)
        integer(int64), intent(in) :: n              !! rows the lookup will process.
        integer, intent(in), optional :: threads     !! the caller's request, or absent.
        integer :: nt                                !! threads to use; 1 means serial.

        nt = ix_threads_rule(n, threads, "get_many")
        !$omp atomic write
        dbg_index_get_many_threads_used = nt
    end function ix_lookup_threads_for

    ! ---- Value validation ----

    !> Checks that a values array is as long as the key array.
    subroutine ix_check_values_len(nv, n, owner)
        integer(int64), intent(in) :: nv !! elements in `values`.
        integer(int64), intent(in) :: n  !! keys presented.
        character(len=*), intent(in), optional :: owner !! type prefix; `pf_index_map%` by default.
        character(len=:), allocatable :: pfx

        call ix_owner_of(owner, pfx)
        if (nv /= n) call ix_abort(pfx // "build: " // &
            "values= must have exactly one element per key")
    end subroutine ix_check_values_len

    !> Checks that every value is a Fortran index value, naming the first that is not.
    !!
    !! A row masked off by `valid` is never stored, so its value is not checked: a nullable key
    !! column's rows carry whatever sits in the null slots, and that is exactly the case the
    !! mask exists for.
    subroutine ix_check_values_range(values, valid, owner)
        integer(int64), intent(in) :: values(:)   !! the values about to be stored.
        logical, intent(in), optional :: valid(:) !! per row; a `.false.` row is not checked.
        character(len=*), intent(in), optional :: owner !! type prefix; `pf_index_map%` by default.
        integer(int64) :: i
        character(len=32) :: t, p
        character(len=:), allocatable :: pfx

        call ix_owner_of(owner, pfx)
        do i = 1_int64, size(values, kind=int64)
            if (present(valid)) then
                if (.not. valid(i)) cycle
            end if
            if (values(i) < 1_int64) then
                write (t, "(i0)") values(i)
                write (p, "(i0)") i
                call ix_abort(pfx // "build: values(" // trim(p) // ") is " // trim(t) // &
                    "; stored values must be >= 1 because 0 is how a lookup reports ""not found""")
            end if
        end do
    end subroutine ix_check_values_range

    !> Checks one value on the way in through `%set`.
    subroutine ix_check_value(v, what)
        integer(int64), intent(in) :: v      !! the value about to be stored.
        character(len=*), intent(in) :: what !! procedure name, for the message.
        character(len=32) :: t

        if (v < 1_int64) then
            write (t, "(i0)") v
            call ix_abort("pf_index_map%" // what // ": value is " // trim(t) // &
                "; stored values must be >= 1 because 0 is how a lookup reports ""not found""")
        end if
    end subroutine ix_check_value

    ! ============================================================================================
    ! The hash mixer and the lookup probes. Here, beside `%get` and the chunk loops of
    ! `%get_many`, so that the compiler inlines them into the lookup loops -- a probe that calls
    ! across a submodule boundary pays about three nanoseconds per key, a third of a small-map
    ! probe (feature_pf_index.md, section 4.6). `parquet_index_hash.f90`, the mutation side,
    ! descends from this submodule and reaches the same functions by host association, so both
    ! sides hash identically by construction; `parquet_index_str.f90` reaches `ix_hash_str` and
    ! the tuple probe the same way.
    !
    ! THE MIXER IS OVERFLOW-FREE BY CONSTRUCTION, and that is a design constraint rather than an
    ! accident of the constants. A conventional 64-bit multiplicative mixer wraps a signed
    ! multiply, which this project treats as the hazard it is: a compiler may wrap the arithmetic
    ! and STILL use the overflow's undefinedness to delete a branch somewhere else -- the confirmed
    ! ifx incident behind feature_risks.md Risk-94, which cost parquet_random a three-arm
    ! preprocessor fork and two standalone check scripts to own exactly one such site. This module
    ! avoids the entire class instead: a key is split into 32-bit halves and mixed with 32-bit x
    ! 31-bit multiplies, so every product is provably below 2**63 and no wrapping site exists at
    ! all. There is therefore nothing here to fork, nothing to check with a separate script, and
    ! the module is clean under -ftrapv, nagfor's -C=intovf and UBSan on every compiler in the
    ! fleet. A lookup is memory-bound; the few extra ALU operations disappear next to one miss.
    ! ============================================================================================

    !> Avalanches the low 32 bits of `v` into a value in `0 .. 2**32 - 1`.
    !!
    !! Murmur3's 32-bit finalizer shape: shift-xor, multiply, shift-xor, multiply, shift-xor. Each
    !! multiplicand is masked to 32 bits and each constant is below 2**31, so **every product is
    !! below 2**63** and this function cannot overflow for any input. Read the masks as part of the
    !! algorithm rather than as tidying: removing one reintroduces exactly the undefined-behaviour
    !! class the banner above is about.
    pure function ix_mix32(v) result(r)
        integer(int64), intent(in) :: v !! any value; only its low 32 bits are read.
        integer(int64) :: r             !! mixed value in `0 .. 2**32 - 1`.

        r = iand(v, IX_MASK32)
        r = ieor(r, ishft(r, -16))
        r = iand(r * IX_C1, IX_MASK32)
        r = ieor(r, ishft(r, -13))
        r = iand(r * IX_C2, IX_MASK32)
        r = ieor(r, ishft(r, -16))
    end function ix_mix32

    !> Hashes one 64-bit key into a full-width bit pattern.
    !!
    !! Both 32-bit halves are mixed and then cross-fed, so the LOW half of the result -- which is
    !! all the slot index reads -- depends on every bit of the key. That is what stops a key set
    !! that varies only in its high word (a stride of 2**32, say) from collapsing onto one slot:
    !! the final `ix_mix32(ieor(lo, hi))` is what carries the high word down.
    !!
    !! `ishft` is a bit-model intrinsic, not arithmetic, so shifting a value into the sign bit is
    !! defined and raises nothing. A negative result is an ordinary bit pattern here and `iand`
    !! with the capacity mask still yields a slot in range.
    pure function ix_hash_bits(k) result(h)
        integer(int64), intent(in) :: k !! the key to hash.
        integer(int64) :: h             !! a mixed bit pattern; only its low bits are used.
        integer(int64) :: lo, hi

        lo = iand(k, IX_MASK32)
        hi = iand(ishft(k, -32), IX_MASK32)
        lo = ix_mix32(ieor(lo, IX_SEED))
        hi = ix_mix32(ieor(hi, lo))
        lo = ix_mix32(ieor(lo, hi))
        h = ior(ishft(hi, 32), lo)
    end function ix_hash_bits

    !> The hash of a single-component key: the only hash a single-component TABLE ever uses.
    !!
    !! A single-component map may be looked up with a scalar key or with a 1-tuple, and both
    !! spellings have to find the same slot. That is guaranteed structurally rather than by two
    !! hashes agreeing: every tuple entry of the hash backend routes `ncomp <= 1` to the scalar
    !! table and this function, and `ix_hash_tuple` is never applied to a 1-tuple.
    pure function ix_hash_one(k) result(h)
        integer(int64), intent(in) :: k !! the key.
        integer(int64) :: h             !! its hash.

        h = ix_hash_bits(ieor(IX_SEED_T, k))
    end function ix_hash_one

    !> One component's step of the two-chain hash: folds a 64-bit value into both accumulators.
    !!
    !! **Two chains of one 32-bit mix each, instead of one chain of three.** The value's low half
    !! goes through the first chain and its high half through the second, each step one `ix_mix32`
    !! of the accumulator XOR the half -- so the two chains are independent and a core runs them
    !! side by side, and a component costs one mix of latency where `ix_hash_bits` per component
    !! cost three. Position-sensitive by the same argument as any chain: each step's input is the
    !! accumulated state, so `[1, 2]` and `[2, 1]` take different paths
    !! (`test_composite_order_matters` pins it).
    !!
    !! **The second chain also takes the low half, rotated.** With the halves kept apart a tuple
    !! whose components all fit 32 bits -- the common one -- would feed the second chain nothing,
    !! and two such tuples would then share the whole 64-bit state about once in 2**32 pairs
    !! rather than 2**64: a probe-length cost only (the compare still tells them apart), but a
    !! needless one for one rotate and one XOR on the chain that is not the critical path.
    !! Overflow-free like everything here: the halves are below 2**32 and only `ix_mix32`
    !! multiplies.
    pure subroutine ix_chain_step(a, b, k)
        integer(int64), intent(inout) :: a !! the low-half chain; in `0 .. 2**32 - 1`.
        integer(int64), intent(inout) :: b !! the high-half chain; in `0 .. 2**32 - 1`.
        integer(int64), intent(in) :: k    !! the component.
        integer(int64) :: lo, hi

        lo = iand(k, IX_MASK32)
        hi = iand(ishft(k, -32), IX_MASK32)
        a = ix_mix32(ieor(a, lo))
        b = ix_mix32(ieor(b, ieor(hi, ior(ishft(iand(lo, 65535_int64), 16), ishft(lo, -16)))))
    end subroutine ix_chain_step

    !> The two chains combined into one bit pattern whose low half depends on both.
    pure function ix_chain_finish(a, b) result(h)
        integer(int64), intent(in) :: a !! the low-half chain.
        integer(int64), intent(in) :: b !! the high-half chain.
        integer(int64) :: h             !! a mixed bit pattern; only its low bits are used.

        h = ior(ishft(b, 32), ix_mix32(ieor(a, b)))
    end function ix_chain_finish

    !> The hash of a key tuple of two or more components: the two chains over the components.
    !!
    !! `key` is EXPLICIT-SHAPE so that the probe hands over a stack buffer's address and nothing
    !! else; a caller holding a section gathers it first (`ix_get_tuple`, the chunk loops, the
    !! composite build).
    pure function ix_hash_tuple(key, nc) result(h)
        integer, intent(in) :: nc             !! components; at least 2.
        integer(int64), intent(in) :: key(nc) !! the tuple.
        integer(int64) :: h                   !! its hash.
        integer(int64) :: a, b
        integer :: j

        a = IX_SEED_T
        b = IX_SEED_U
        do j = 1, nc
            call ix_chain_step(a, b, key(j))
        end do
        h = ix_chain_finish(a, b)
    end function ix_hash_tuple

    !> The hash of a string's bytes: the tuple hash's two chains over the string's 8-byte words,
    !! with the length as the first component, so that no second hash family enters the module,
    !! a string and its own zero-padded extension never share a hash, and the clustering tests
    !! hold it to the integer mixer's standard. Each word is assembled little-endian from the
    !! bytes, read as unsigned, so the result is the same on every platform. Narrowed by the debug
    !! hook `parquet_debug_set_index_string_hash_bits`, which is read here and nowhere else.
    !!
    !! **`bytes` is an EXPLICIT-SHAPE dummy on purpose.** A scalar `character` actual associates
    !! with an explicit-shape `character(len=1)` array dummy by character sequence association
    !! (F2018 15.5.2.11), so this one function hashes a scalar key, an element of a `character`
    !! array and a slice of a packed payload alike, with no copy and no `transfer`. An
    !! assumed-shape dummy would refuse the scalar.
    pure function ix_hash_str(bytes, n) result(h)
        integer(int64), intent(in) :: n              !! bytes to hash; may be 0.
        character(len=1), intent(in) :: bytes(n)     !! the bytes.
        integer(int64) :: h                          !! a mixed bit pattern; only its low bits are used.
        integer(int64) :: a, b, w, i, nb
        integer :: k

        a = IX_SEED_S
        b = IX_SEED_U
        call ix_chain_step(a, b, n)
        do i = 0_int64, n - 1_int64, 8_int64
            nb = min(8_int64, n - i)
            w = 0_int64
            do k = 1, int(nb)
                w = ior(w, ishft(int(iand(iachar(bytes(i + k)), 255), int64), 8 * (k - 1)))
            end do
            call ix_chain_step(a, b, w)
        end do
        h = ix_chain_finish(a, b)
        ! The hook narrows the RESULT rather than the mixer's input, so the narrowed hashes are
        ! as evenly spread over their few bits as the full ones are over 64 -- which is what
        ! makes a test at 2 bits a test of the collision chain and not of a degenerate mixer.
        if (dbg_index_string_hash_bits > 0) then
            h = iand(h, ishft(1_int64, dbg_index_string_hash_bits) - 1_int64)
        end if
    end function ix_hash_str

    module procedure parquet_debug_set_index_string_hash_bits
        if (nbits < 1) then
            dbg_index_string_hash_bits = 0
        else
            dbg_index_string_hash_bits = min(nbits, 62)
        end if
    end procedure parquet_debug_set_index_string_hash_bits

    !> The bulk probe of a single-component table over one block of keys: every key's home slot
    !! first, then every walk.
    !!
    !! **Two passes rather than one, and the split is what halves a ten-million-key probe.** A
    !! probe at that size is a DRAM miss, and a core hides one only by having the next probes'
    !! misses in flight beside it -- which it can do for as many iterations as fit in its reorder
    !! window. One pass that hashes and walks per key puts the whole hash (some thirty
    !! instructions for a scalar key, twice that for a pair, four times for a quadruple) between
    !! one load and the next, so few iterations fit and few misses overlap: measured on the
    !! single-pass form at ten million keys, a two-component probe cost twice a scalar one and a
    !! four-component one four times, with the same probe statistics (mean 1.7) on all three.
    !! Hashing a whole block into `home` first leaves the walking pass with a few instructions per
    !! key, so its loads overlap up to the window. In cache the two shapes cost the same.
    !!
    !! `slots` is EXPLICIT-SHAPE and 0-based: the caller hands over the array's base address and
    !! `iand(h, m)` is the subscript with no correction. The walk is unbounded on purpose; see
    !! `ix_hash_table_full` in `parquet_index_hash.f90`.
    !!
    !! **The hash is written out in full here rather than reached through `ix_hash_one`, and that
    !! copy is deliberate.** Under `-fPIC` -- fpm's release profile on Linux -- gfortran inlines NO
    !! module procedure, not even one in the same file (semantic interposition: CLAUDE.md, "-fPIC
    !! blocks inlining on ELF"), so a probe spelled over `ix_hash_one` -> `ix_hash_bits` -> three
    !! `ix_mix32` paid six out-of-line calls per key, a third of a small-map probe. This copy is
    !! the same arithmetic in the same order, statement for statement; the mutation side keeps
    !! calling the named functions, so a divergence between the two would make every lookup miss
    !! every key -- which is what the whole hash-backend test suite asserts against. Keep the two
    !! in step by editing `ix_mix32`/`ix_hash_bits` first and then this, never the reverse. The
    !! one-command check that the kernel makes no call is CLAUDE.md's `objdump -dr ... | grep
    !! R_X86_64` on this file's object, restricted to the kernel's symbol.
    pure subroutine ix_probe_1_block(slots, m, n, keys, v)
        integer(int64), intent(in) :: m         !! `hcap - 1`, the slot mask.
        type(ix_slot), intent(in) :: slots(0:m) !! the table.
        integer, intent(in) :: n                !! keys in the block, `1 .. IX_PROBE_BLOCK`.
        integer(int64), intent(in) :: keys(n)   !! the keys.
        integer(int64), intent(out) :: v(n)     !! the stored value per key, or 0 when absent.
        integer(int64) :: home(IX_PROBE_BLOCK)
        integer(int64) :: s, x, lo, hi, r, k
        integer :: j

        ! Pass 1: ix_hash_one(key) = ix_hash_bits(ieor(IX_SEED_T, key)), expanded, per key.
        do j = 1, n
            x = ieor(IX_SEED_T, keys(j))
            lo = iand(x, IX_MASK32)
            hi = iand(ishft(x, -32), IX_MASK32)
            r = ieor(lo, IX_SEED)
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            lo = ieor(r, ishft(r, -16))
            r = ieor(hi, lo)
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            hi = ieor(r, ishft(r, -16))
            r = ieor(lo, hi)
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            lo = ieor(r, ishft(r, -16))
            home(j) = iand(ior(ishft(hi, 32), lo), m)
        end do
        ! Pass 2: the walks, each from its home slot to the key or to the first empty slot.
        do j = 1, n
            s = home(j)
            k = keys(j)
            do
                if (slots(s)%val == 0_int64) then
                    v(j) = 0_int64
                    exit
                end if
                if (slots(s)%key == k) then
                    v(j) = slots(s)%val
                    exit
                end if
                s = iand(s + 1_int64, m)
            end do
        end do
    end subroutine ix_probe_1_block

    !> The bulk probe of a composite table over one block of tuples: `ix_probe_1_block`'s two
    !! passes over `hrec`, with `ix_hash_tuple` -- the two chains of `ix_chain_step` and their
    !! `ix_chain_finish` -- written out in the first for the reason given there; keep it in step
    !! with those three functions the same way.
    pure subroutine ix_probe_n_block(hrec, nc, m, n, keys, v)
        integer, intent(in) :: nc                        !! components; at least 2.
        integer(int64), intent(in) :: m                  !! `hcap - 1`, the slot mask.
        integer(int64), intent(in) :: hrec(nc + 1, 0:m)  !! the records: the tuple, then its value.
        integer, intent(in) :: n                         !! tuples in the block, `1 .. IX_PROBE_BLOCK`.
        integer(int64), intent(in) :: keys(nc, n)        !! the tuples, one per column.
        integer(int64), intent(out) :: v(n)              !! the stored value per tuple, or 0 when absent.
        integer(int64) :: home(IX_PROBE_BLOCK)
        integer(int64) :: s, a, b, k, lo, hi, r
        integer :: j, c
        logical :: hit

        if (nc == 2) then
            call ix_probe_2_block(hrec, m, n, keys, v)
            return
        end if
        do j = 1, n
            a = IX_SEED_T
            b = IX_SEED_U
            do c = 1, nc
                k = keys(c, j)
                lo = iand(k, IX_MASK32)
                hi = iand(ishft(k, -32), IX_MASK32)
                r = ieor(a, lo)
                r = ieor(r, ishft(r, -16))
                r = iand(r * IX_C1, IX_MASK32)
                r = ieor(r, ishft(r, -13))
                r = iand(r * IX_C2, IX_MASK32)
                a = ieor(r, ishft(r, -16))
                r = ieor(b, ieor(hi, ior(ishft(iand(lo, 65535_int64), 16), ishft(lo, -16))))
                r = ieor(r, ishft(r, -16))
                r = iand(r * IX_C1, IX_MASK32)
                r = ieor(r, ishft(r, -13))
                r = iand(r * IX_C2, IX_MASK32)
                b = ieor(r, ishft(r, -16))
            end do
            r = ieor(a, b)
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            r = ieor(r, ishft(r, -16))
            home(j) = iand(ior(ishft(b, 32), r), m)
        end do
        do j = 1, n
            s = home(j)
            do
                if (hrec(nc + 1, s) == 0_int64) then
                    v(j) = 0_int64
                    exit
                end if
                hit = .true.
                do c = 1, nc
                    if (hrec(c, s) /= keys(c, j)) then
                        hit = .false.
                        exit
                    end if
                end do
                if (hit) then
                    v(j) = hrec(nc + 1, s)
                    exit
                end if
                s = iand(s + 1_int64, m)
            end do
        end do
    end subroutine ix_probe_n_block

    !> `ix_probe_n_block` for PAIRS -- the timestamp key's `(seconds, nanoseconds)` and the
    !! string map's `(hash, occurrence)`, so the composite case that is common -- with the record
    !! stride and the two chain steps fixed at compile time: no loop over components in either
    !! pass, and `hrec(3, s)` a constant offset rather than a multiply by a runtime width. The
    !! general kernel dispatches here on `nc == 2`; the hash is the same two chains, written out
    !! a third time for the reason `ix_probe_1_block` gives.
    pure subroutine ix_probe_2_block(hrec, m, n, keys, v)
        integer(int64), intent(in) :: m               !! `hcap - 1`, the slot mask.
        integer(int64), intent(in) :: hrec(3, 0:m)    !! the records: the pair, then its value.
        integer, intent(in) :: n                      !! pairs in the block, `1 .. IX_PROBE_BLOCK`.
        integer(int64), intent(in) :: keys(2, n)      !! the pairs, one per column.
        integer(int64), intent(out) :: v(n)           !! the stored value per pair, or 0 when absent.
        integer(int64) :: home(IX_PROBE_BLOCK)
        integer(int64) :: s, a, b, k, k1, k2, lo, hi, r
        integer :: j

        do j = 1, n
            a = IX_SEED_T
            b = IX_SEED_U
            k = keys(1, j)
            lo = iand(k, IX_MASK32)
            hi = iand(ishft(k, -32), IX_MASK32)
            r = ieor(a, lo)
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            a = ieor(r, ishft(r, -16))
            r = ieor(b, ieor(hi, ior(ishft(iand(lo, 65535_int64), 16), ishft(lo, -16))))
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            b = ieor(r, ishft(r, -16))
            k = keys(2, j)
            lo = iand(k, IX_MASK32)
            hi = iand(ishft(k, -32), IX_MASK32)
            r = ieor(a, lo)
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            a = ieor(r, ishft(r, -16))
            r = ieor(b, ieor(hi, ior(ishft(iand(lo, 65535_int64), 16), ishft(lo, -16))))
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            b = ieor(r, ishft(r, -16))
            r = ieor(a, b)
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            r = ieor(r, ishft(r, -16))
            home(j) = iand(ior(ishft(b, 32), r), m)
        end do
        do j = 1, n
            s = home(j)
            k1 = keys(1, j)
            k2 = keys(2, j)
            do
                if (hrec(3, s) == 0_int64) then
                    v(j) = 0_int64
                    exit
                end if
                if (hrec(1, s) == k1 .and. hrec(2, s) == k2) then
                    v(j) = hrec(3, s)
                    exit
                end if
                s = iand(s + 1_int64, m)
            end do
        end do
    end subroutine ix_probe_2_block

    !> The probe of one single-component key: `ix_probe_1_block`'s two passes collapsed to one,
    !! with the same written-out hash. A block of one through the kernel measured three
    !! nanoseconds dearer on a scalar `%get` (the call, two loops of one, the block arrays), which
    !! is a third of that probe in cache; this copy is the price of not paying it.
    pure function ix_probe_1(slots, m, key) result(v)
        integer(int64), intent(in) :: m         !! `hcap - 1`, the slot mask.
        type(ix_slot), intent(in) :: slots(0:m) !! the table.
        integer(int64), intent(in) :: key       !! the key.
        integer(int64) :: v                     !! the stored value, or 0 when absent.
        integer(int64) :: s, x, lo, hi, r

        x = ieor(IX_SEED_T, key)
        lo = iand(x, IX_MASK32)
        hi = iand(ishft(x, -32), IX_MASK32)
        r = ieor(lo, IX_SEED)
        r = ieor(r, ishft(r, -16))
        r = iand(r * IX_C1, IX_MASK32)
        r = ieor(r, ishft(r, -13))
        r = iand(r * IX_C2, IX_MASK32)
        lo = ieor(r, ishft(r, -16))
        r = ieor(hi, lo)
        r = ieor(r, ishft(r, -16))
        r = iand(r * IX_C1, IX_MASK32)
        r = ieor(r, ishft(r, -13))
        r = iand(r * IX_C2, IX_MASK32)
        hi = ieor(r, ishft(r, -16))
        r = ieor(lo, hi)
        r = ieor(r, ishft(r, -16))
        r = iand(r * IX_C1, IX_MASK32)
        r = ieor(r, ishft(r, -13))
        r = iand(r * IX_C2, IX_MASK32)
        lo = ieor(r, ishft(r, -16))
        s = iand(ior(ishft(hi, 32), lo), m)
        do
            if (slots(s)%val == 0_int64) then
                v = 0_int64
                return
            end if
            if (slots(s)%key == key) then
                v = slots(s)%val
                return
            end if
            s = iand(s + 1_int64, m)
        end do
    end function ix_probe_1

    !> The probe of one tuple: `ix_probe_n_block`'s two passes collapsed to one, with the same
    !! written-out two-chain hash, for the reason `ix_probe_1` gives. The string map's per-key
    !! probe (`ix_str_find`) comes through here with its `(hash, occurrence)` pair.
    pure function ix_probe_n(hrec, nc, m, key) result(v)
        integer, intent(in) :: nc                        !! components; at least 2.
        integer(int64), intent(in) :: m                  !! `hcap - 1`, the slot mask.
        integer(int64), intent(in) :: hrec(nc + 1, 0:m)  !! the records: the tuple, then its value.
        integer(int64), intent(in) :: key(nc)            !! the tuple.
        integer(int64) :: v                              !! the stored value, or 0 when absent.
        integer(int64) :: s, a, b, k, lo, hi, r
        integer :: j
        logical :: hit

        a = IX_SEED_T
        b = IX_SEED_U
        do j = 1, nc
            k = key(j)
            lo = iand(k, IX_MASK32)
            hi = iand(ishft(k, -32), IX_MASK32)
            r = ieor(a, lo)
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            a = ieor(r, ishft(r, -16))
            r = ieor(b, ieor(hi, ior(ishft(iand(lo, 65535_int64), 16), ishft(lo, -16))))
            r = ieor(r, ishft(r, -16))
            r = iand(r * IX_C1, IX_MASK32)
            r = ieor(r, ishft(r, -13))
            r = iand(r * IX_C2, IX_MASK32)
            b = ieor(r, ishft(r, -16))
        end do
        r = ieor(a, b)
        r = ieor(r, ishft(r, -16))
        r = iand(r * IX_C1, IX_MASK32)
        r = ieor(r, ishft(r, -13))
        r = iand(r * IX_C2, IX_MASK32)
        r = ieor(r, ishft(r, -16))
        s = iand(ior(ishft(b, 32), r), m)
        do
            if (hrec(nc + 1, s) == 0_int64) then
                v = 0_int64
                return
            end if
            hit = .true.
            do j = 1, nc
                if (hrec(j, s) /= key(j)) then
                    hit = .false.
                    exit
                end if
            end do
            if (hit) then
                v = hrec(nc + 1, s)
                return
            end if
            s = iand(s + 1_int64, m)
        end do
    end function ix_probe_n

    !> The index stored for a key tuple in the hash backend, or 0; a 1-tuple is the scalar lookup.
    pure function ix_hash_find_tuple(self, key) result(v)
        type(pf_index_map), intent(in) :: self           !! a map whose backend is `IX_HASH`.
        integer(int64), intent(in), contiguous :: key(:) !! the tuple, already widened, `size == self%ncomp`.
        integer(int64) :: v                              !! the stored value, or 0 when absent.

        v = 0_int64
        if (self%hcap <= 0_int64) return
        if (self%ncomp <= 1) then
            v = ix_probe_1(self%slots, self%hcap - 1_int64, key(1))
        else
            v = ix_probe_n(self%hrec, self%ncomp, self%hcap - 1_int64, key)
        end if
    end function ix_hash_find_tuple

    ! ---- Lookup workers ----

    !> The index for a single-component key, dispatching on the backend. 0 when absent.
    !!
    !! The direct arm's range test is what lets an unbuilt or cleared map answer here rather than
    !! needing a branch of its own: the as-new range is `0 .. -1`, which contains nothing.
    pure function ix_get_scalar(self, key) result(idx)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: key      !! the key, already widened.
        integer(int64) :: idx                  !! the stored value, or 0.

        select case (self%backend)
        case (IX_DIRECT)
            if (key < self%kmin1 .or. key > self%kmax1) then
                idx = 0_int64
            else
                idx = self%dvals(key - self%kmin1 + 1_int64)
            end if
        case (IX_HASH)
            if (self%hcap > 0_int64) then
                idx = ix_probe_1(self%slots, self%hcap - 1_int64, key)
            else
                idx = 0_int64
            end if
        case (IX_SORTED)
            idx = ix_sorted_find(self, key)
        case default
            idx = 0_int64
        end select
    end function ix_get_scalar

    !> The index for a key tuple, dispatching on the backend. 0 when absent.
    !!
    !! The direct arm is a mixed-radix offset. Each component is bounds-tested against the stored
    !! `dkmin`/`dkmax` pair BEFORE any arithmetic, which is why both bounds are stored rather than
    !! `dkmin` and a span: `key(j) - dkmin(j)` is a subtraction of two `int64` values and would
    !! overflow for a key far outside the built range, while a pair of comparisons cannot.
    pure function ix_get_tuple(self, key) result(idx)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: key(:)   !! the tuple, already widened and width-checked.
        integer(int64) :: idx                  !! the stored value, or 0.
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: off
        integer :: j, nc

        idx = 0_int64
        nc = self%ncomp
        if (nc == 0) return
        select case (self%backend)
        case (IX_DIRECT)
            off = 1_int64
            do j = 1, nc
                if (key(j) < self%dkmin(j) .or. key(j) > self%dkmax(j)) return
                off = off + (key(j) - self%dkmin(j)) * self%dstride(j)
            end do
            idx = self%dvals(off)
        case (IX_HASH)
            if (self%hcap <= 0_int64) return
            if (nc == 1) then
                idx = ix_probe_1(self%slots, self%hcap - 1_int64, key(1))
            else
                ! Gathered into the stack buffer, so the probe's explicit-shape dummy takes it
                ! without a contiguity check; `key` is assumed-shape and may be a section.
                do j = 1, nc
                    kb(j) = key(j)
                end do
                idx = ix_probe_n(self%hrec, nc, self%hcap - 1_int64, kb)
            end if
        case (IX_SORTED)
            idx = ix_sorted_find(self, key(1))
        end select
    end function ix_get_tuple

    !> The direct backend's slot for a single-component key, or 0 when the key is out of range.
    pure function ix_direct_off_1(self, key) result(off)
        type(pf_index_map), intent(in) :: self !! a map whose backend is `IX_DIRECT`.
        integer(int64), intent(in) :: key      !! the key.
        integer(int64) :: off                  !! the 1-based slot, or 0 when out of range.

        off = 0_int64
        if (key < self%kmin1 .or. key > self%kmax1) return
        off = key - self%kmin1 + 1_int64
    end function ix_direct_off_1

    !> The direct backend's slot for a key tuple, or 0 when any component is out of range.
    pure function ix_direct_off_n(self, key) result(off)
        type(pf_index_map), intent(in) :: self !! a map whose backend is `IX_DIRECT`.
        integer(int64), intent(in) :: key(:)   !! the tuple.
        integer(int64) :: off                  !! the 1-based slot, or 0 when out of range.
        integer :: j

        off = 1_int64
        do j = 1, self%ncomp
            if (key(j) < self%dkmin(j) .or. key(j) > self%dkmax(j)) then
                off = 0_int64
                return
            end if
            off = off + (key(j) - self%dkmin(j)) * self%dstride(j)
        end do
    end function ix_direct_off_n

    ! ---- The bulk lookup's chunk loops ----

    !> The serial `%get_many` loop over rows `lo .. hi`: `int32` keys, `int32` answers.
    !!
    !! One of eight, one per key kind x key rank x answer kind, each the typed loop its `%get_many`
    !! specific ran serially before the bulk form was threaded; the driver cuts the rows into
    !! chunks and calls one of these per chunk. **The backend is dispatched once per chunk, not
    !! once per key**: the hash arm probes with the slot mask and the table in locals through
    !! `ix_probe_1`/`ix_probe_n`, which are contained in this file so the compiler inlines them;
    !! the direct arm is the range test and one load with the bounds in locals; anything else --
    !! the sorted backend, an unbuilt map -- takes the general `ix_get_scalar`/`ix_get_tuple`
    !! per key, which is where the dispatch used to happen for every key of every backend
    !! (feature_pf_index.md, section 6 item 2). `indexes` is `intent(inout)` rather than
    !! `intent(out)` because each chunk of a threaded call writes only its own slice of the
    !! caller's array, and an `intent(out)` dummy would let the compiler treat the whole array as
    !! undefined on entry. `hv` is `present(valid)` hoisted by the driver, so the masked branch
    !! costs one well-predicted test per row and `valid` is never touched when it is absent.
    subroutine ix_many_1_k32_i32(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        integer(int32), intent(in) :: keys(:)       !! the caller's keys.
        integer(int32), intent(inout) :: indexes(:) !! the caller's answers; rows `lo .. hi` written.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: kb(IX_PROBE_BLOCK), vb(IX_PROBE_BLOCK)
        integer(int64) :: i, i0, k, m, kmin, kmax
        integer :: j, nb

        if (self%backend == IX_HASH .and. self%hcap > 0_int64) then
            ! Blocks of keys through the two-pass kernel; a masked row is probed like any other
            ! (its key is whatever the caller's array holds, and a probe of anything is harmless)
            ! and its answer zeroed afterwards, before any narrowing.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do j = 1, nb
                    kb(j) = int(keys(i0 + j - 1), int64)
                end do
                call ix_probe_1_block(self%slots, m, nb, kb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = ix_narrow(vb(j))
                end do
            end do
        else if (self%backend == IX_DIRECT) then
            kmin = self%kmin1
            kmax = self%kmax1
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
                k = int(keys(i), int64)
                if (k < kmin .or. k > kmax) then
                    indexes(i) = 0_int32
                else
                    indexes(i) = ix_narrow(self%dvals(k - kmin + 1_int64))
                end if
            end do
        else
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
                indexes(i) = ix_narrow(ix_get_scalar(self, int(keys(i), int64)))
            end do
        end if
    end subroutine ix_many_1_k32_i32

    !> The serial `%get_many` loop over rows `lo .. hi`: `int32` keys, `int64` answers.
    subroutine ix_many_1_k32_i64(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        integer(int32), intent(in) :: keys(:)       !! the caller's keys.
        integer(int64), intent(inout) :: indexes(:) !! the caller's answers; rows `lo .. hi` written.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: kb(IX_PROBE_BLOCK), vb(IX_PROBE_BLOCK)
        integer(int64) :: i, i0, k, m, kmin, kmax
        integer :: j, nb

        if (self%backend == IX_HASH .and. self%hcap > 0_int64) then
            ! Blocks of keys through the two-pass kernel; a masked row is probed like any other
            ! (its key is whatever the caller's array holds, and a probe of anything is harmless)
            ! and its answer zeroed afterwards, before any narrowing.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do j = 1, nb
                    kb(j) = int(keys(i0 + j - 1), int64)
                end do
                call ix_probe_1_block(self%slots, m, nb, kb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = vb(j)
                end do
            end do
        else if (self%backend == IX_DIRECT) then
            kmin = self%kmin1
            kmax = self%kmax1
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
                k = int(keys(i), int64)
                if (k < kmin .or. k > kmax) then
                    indexes(i) = 0_int64
                else
                    indexes(i) = self%dvals(k - kmin + 1_int64)
                end if
            end do
        else
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
                indexes(i) = ix_get_scalar(self, int(keys(i), int64))
            end do
        end if
    end subroutine ix_many_1_k32_i64

    !> The serial `%get_many` loop over rows `lo .. hi`: `int64` keys, `int32` answers.
    subroutine ix_many_1_k64_i32(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        integer(int64), intent(in) :: keys(:)       !! the caller's keys.
        integer(int32), intent(inout) :: indexes(:) !! the caller's answers; rows `lo .. hi` written.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: kb(IX_PROBE_BLOCK), vb(IX_PROBE_BLOCK)
        integer(int64) :: i, i0, k, m, kmin, kmax
        integer :: j, nb

        if (self%backend == IX_HASH .and. self%hcap > 0_int64) then
            ! Blocks of keys through the two-pass kernel; a masked row is probed like any other
            ! (its key is whatever the caller's array holds, and a probe of anything is harmless)
            ! and its answer zeroed afterwards, before any narrowing.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do j = 1, nb
                    kb(j) = keys(i0 + j - 1)
                end do
                call ix_probe_1_block(self%slots, m, nb, kb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = ix_narrow(vb(j))
                end do
            end do
        else if (self%backend == IX_DIRECT) then
            kmin = self%kmin1
            kmax = self%kmax1
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
                k = keys(i)
                if (k < kmin .or. k > kmax) then
                    indexes(i) = 0_int32
                else
                    indexes(i) = ix_narrow(self%dvals(k - kmin + 1_int64))
                end if
            end do
        else
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
                indexes(i) = ix_narrow(ix_get_scalar(self, keys(i)))
            end do
        end if
    end subroutine ix_many_1_k64_i32

    !> The serial `%get_many` loop over rows `lo .. hi`: `int64` keys, `int64` answers.
    subroutine ix_many_1_k64_i64(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        integer(int64), intent(in) :: keys(:)       !! the caller's keys.
        integer(int64), intent(inout) :: indexes(:) !! the caller's answers; rows `lo .. hi` written.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: kb(IX_PROBE_BLOCK), vb(IX_PROBE_BLOCK)
        integer(int64) :: i, i0, k, m, kmin, kmax
        integer :: j, nb

        if (self%backend == IX_HASH .and. self%hcap > 0_int64) then
            ! Blocks of keys through the two-pass kernel; a masked row is probed like any other
            ! (its key is whatever the caller's array holds, and a probe of anything is harmless)
            ! and its answer zeroed afterwards, before any narrowing.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do j = 1, nb
                    kb(j) = keys(i0 + j - 1)
                end do
                call ix_probe_1_block(self%slots, m, nb, kb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = vb(j)
                end do
            end do
        else if (self%backend == IX_DIRECT) then
            kmin = self%kmin1
            kmax = self%kmax1
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
                k = keys(i)
                if (k < kmin .or. k > kmax) then
                    indexes(i) = 0_int64
                else
                    indexes(i) = self%dvals(k - kmin + 1_int64)
                end if
            end do
        else
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
                indexes(i) = ix_get_scalar(self, keys(i))
            end do
        end if
    end subroutine ix_many_1_k64_i64

    !> The serial `%get_many` loop over rows `lo .. hi`: `int32` tuples, `int32` answers.
    subroutine ix_many_n_k32_i32(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        integer(int32), intent(in) :: keys(:,:)     !! the caller's tuples, one per row.
        integer(int32), intent(inout) :: indexes(:) !! the caller's answers; rows `lo .. hi` written.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: tb(pf_index_max_components * IX_PROBE_BLOCK)
        integer(int64) :: kb(IX_PROBE_BLOCK), vb(IX_PROBE_BLOCK)
        integer(int64) :: i, i0, m, off
        integer :: nc, j, c, nb

        nc = self%ncomp
        if (self%backend == IX_HASH .and. self%hcap > 0_int64 .and. nc > 1) then
            ! Blocks of tuples, gathered into `tb` one tuple per `nc` entries -- the `(nc, nb)`
            ! layout the kernel takes -- then the two-pass kernel; a masked row is probed like
            ! any other and its answer zeroed afterwards, before any narrowing.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do c = 1, nc
                    do j = 1, nb
                        tb((j - 1) * nc + c) = int(keys(i0 + j - 1, c), int64)
                    end do
                end do
                call ix_probe_n_block(self%hrec, nc, m, nb, tb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = ix_narrow(vb(j))
                end do
            end do
        else if (self%backend == IX_HASH .and. self%hcap > 0_int64 .and. nc == 1) then
            ! A single-component map probed with 1-tuples: the scalar table, so the scalar kernel.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do j = 1, nb
                    kb(j) = int(keys(i0 + j - 1, 1), int64)
                end do
                call ix_probe_1_block(self%slots, m, nb, kb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = ix_narrow(vb(j))
                end do
            end do
        else if (self%backend == IX_DIRECT .and. nc >= 1) then
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
                off = 1_int64
                do j = 1, nc
                    kb(j) = int(keys(i, j), int64)
                    if (kb(j) < self%dkmin(j) .or. kb(j) > self%dkmax(j)) then
                        off = 0_int64
                        exit
                    end if
                    off = off + (kb(j) - self%dkmin(j)) * self%dstride(j)
                end do
                if (off == 0_int64) then
                    indexes(i) = 0_int32
                else
                    indexes(i) = ix_narrow(self%dvals(off))
                end if
            end do
        else
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
                do j = 1, nc
                    kb(j) = int(keys(i, j), int64)
                end do
                indexes(i) = ix_narrow(ix_get_tuple(self, kb(1:nc)))
            end do
        end if
    end subroutine ix_many_n_k32_i32

    !> The serial `%get_many` loop over rows `lo .. hi`: `int32` tuples, `int64` answers.
    subroutine ix_many_n_k32_i64(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        integer(int32), intent(in) :: keys(:,:)     !! the caller's tuples, one per row.
        integer(int64), intent(inout) :: indexes(:) !! the caller's answers; rows `lo .. hi` written.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: tb(pf_index_max_components * IX_PROBE_BLOCK)
        integer(int64) :: kb(IX_PROBE_BLOCK), vb(IX_PROBE_BLOCK)
        integer(int64) :: i, i0, m, off
        integer :: nc, j, c, nb

        nc = self%ncomp
        if (self%backend == IX_HASH .and. self%hcap > 0_int64 .and. nc > 1) then
            ! Blocks of tuples, gathered into `tb` one tuple per `nc` entries -- the `(nc, nb)`
            ! layout the kernel takes -- then the two-pass kernel; a masked row is probed like
            ! any other and its answer zeroed afterwards, before any narrowing.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do c = 1, nc
                    do j = 1, nb
                        tb((j - 1) * nc + c) = int(keys(i0 + j - 1, c), int64)
                    end do
                end do
                call ix_probe_n_block(self%hrec, nc, m, nb, tb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = vb(j)
                end do
            end do
        else if (self%backend == IX_HASH .and. self%hcap > 0_int64 .and. nc == 1) then
            ! A single-component map probed with 1-tuples: the scalar table, so the scalar kernel.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do j = 1, nb
                    kb(j) = int(keys(i0 + j - 1, 1), int64)
                end do
                call ix_probe_1_block(self%slots, m, nb, kb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = vb(j)
                end do
            end do
        else if (self%backend == IX_DIRECT .and. nc >= 1) then
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
                off = 1_int64
                do j = 1, nc
                    kb(j) = int(keys(i, j), int64)
                    if (kb(j) < self%dkmin(j) .or. kb(j) > self%dkmax(j)) then
                        off = 0_int64
                        exit
                    end if
                    off = off + (kb(j) - self%dkmin(j)) * self%dstride(j)
                end do
                if (off == 0_int64) then
                    indexes(i) = 0_int64
                else
                    indexes(i) = self%dvals(off)
                end if
            end do
        else
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
                do j = 1, nc
                    kb(j) = int(keys(i, j), int64)
                end do
                indexes(i) = ix_get_tuple(self, kb(1:nc))
            end do
        end if
    end subroutine ix_many_n_k32_i64

    !> The serial `%get_many` loop over rows `lo .. hi`: `int64` tuples, `int32` answers.
    subroutine ix_many_n_k64_i32(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        integer(int64), intent(in) :: keys(:,:)     !! the caller's tuples, one per row.
        integer(int32), intent(inout) :: indexes(:) !! the caller's answers; rows `lo .. hi` written.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: tb(pf_index_max_components * IX_PROBE_BLOCK)
        integer(int64) :: kb(IX_PROBE_BLOCK), vb(IX_PROBE_BLOCK)
        integer(int64) :: i, i0, m, off
        integer :: nc, j, c, nb

        nc = self%ncomp
        if (self%backend == IX_HASH .and. self%hcap > 0_int64 .and. nc > 1) then
            ! Blocks of tuples, gathered into `tb` one tuple per `nc` entries -- the `(nc, nb)`
            ! layout the kernel takes -- then the two-pass kernel; a masked row is probed like
            ! any other and its answer zeroed afterwards, before any narrowing.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do c = 1, nc
                    do j = 1, nb
                        tb((j - 1) * nc + c) = keys(i0 + j - 1, c)
                    end do
                end do
                call ix_probe_n_block(self%hrec, nc, m, nb, tb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = ix_narrow(vb(j))
                end do
            end do
        else if (self%backend == IX_HASH .and. self%hcap > 0_int64 .and. nc == 1) then
            ! A single-component map probed with 1-tuples: the scalar table, so the scalar kernel.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do j = 1, nb
                    kb(j) = keys(i0 + j - 1, 1)
                end do
                call ix_probe_1_block(self%slots, m, nb, kb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = ix_narrow(vb(j))
                end do
            end do
        else if (self%backend == IX_DIRECT .and. nc >= 1) then
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
                off = 1_int64
                do j = 1, nc
                    kb(j) = keys(i, j)
                    if (kb(j) < self%dkmin(j) .or. kb(j) > self%dkmax(j)) then
                        off = 0_int64
                        exit
                    end if
                    off = off + (kb(j) - self%dkmin(j)) * self%dstride(j)
                end do
                if (off == 0_int64) then
                    indexes(i) = 0_int32
                else
                    indexes(i) = ix_narrow(self%dvals(off))
                end if
            end do
        else
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
                do j = 1, nc
                    kb(j) = keys(i, j)
                end do
                indexes(i) = ix_narrow(ix_get_tuple(self, kb(1:nc)))
            end do
        end if
    end subroutine ix_many_n_k64_i32

    !> The serial `%get_many` loop over rows `lo .. hi`: `int64` tuples, `int64` answers.
    subroutine ix_many_n_k64_i64(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        integer(int64), intent(in) :: keys(:,:)     !! the caller's tuples, one per row.
        integer(int64), intent(inout) :: indexes(:) !! the caller's answers; rows `lo .. hi` written.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: tb(pf_index_max_components * IX_PROBE_BLOCK)
        integer(int64) :: kb(IX_PROBE_BLOCK), vb(IX_PROBE_BLOCK)
        integer(int64) :: i, i0, m, off
        integer :: nc, j, c, nb

        nc = self%ncomp
        if (self%backend == IX_HASH .and. self%hcap > 0_int64 .and. nc > 1) then
            ! Blocks of tuples, gathered into `tb` one tuple per `nc` entries -- the `(nc, nb)`
            ! layout the kernel takes -- then the two-pass kernel; a masked row is probed like
            ! any other and its answer zeroed afterwards, before any narrowing.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do c = 1, nc
                    do j = 1, nb
                        tb((j - 1) * nc + c) = keys(i0 + j - 1, c)
                    end do
                end do
                call ix_probe_n_block(self%hrec, nc, m, nb, tb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = vb(j)
                end do
            end do
        else if (self%backend == IX_HASH .and. self%hcap > 0_int64 .and. nc == 1) then
            ! A single-component map probed with 1-tuples: the scalar table, so the scalar kernel.
            m = self%hcap - 1_int64
            do i0 = lo, hi, int(IX_PROBE_BLOCK, int64)
                nb = int(min(int(IX_PROBE_BLOCK, int64), hi - i0 + 1_int64))
                do j = 1, nb
                    kb(j) = keys(i0 + j - 1, 1)
                end do
                call ix_probe_1_block(self%slots, m, nb, kb, vb)
                if (hv) then
                    do j = 1, nb
                        if (.not. valid(i0 + j - 1)) vb(j) = 0_int64
                    end do
                end if
                do j = 1, nb
                    indexes(i0 + j - 1) = vb(j)
                end do
            end do
        else if (self%backend == IX_DIRECT .and. nc >= 1) then
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
                off = 1_int64
                do j = 1, nc
                    kb(j) = keys(i, j)
                    if (kb(j) < self%dkmin(j) .or. kb(j) > self%dkmax(j)) then
                        off = 0_int64
                        exit
                    end if
                    off = off + (kb(j) - self%dkmin(j)) * self%dstride(j)
                end do
                if (off == 0_int64) then
                    indexes(i) = 0_int64
                else
                    indexes(i) = self%dvals(off)
                end if
            end do
        else
            do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
                do j = 1, nc
                    kb(j) = keys(i, j)
                end do
                indexes(i) = ix_get_tuple(self, kb(1:nc))
            end do
        end if
    end subroutine ix_many_n_k64_i64

    ! ---- Mutation workers. Every one of these runs with the map's guard held. ----

    !> Refuses a mutation of a frozen sorted map, or of a key outside a direct map's range.
    subroutine ix_check_mutable(self, off, what)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: off      !! the resolved direct slot, or 0 when out of range.
        character(len=*), intent(in) :: what   !! procedure name, for the message.

        if (self%backend == IX_SORTED) call ix_abort("pf_index_map%" // what // &
            ": a sorted map is frozen once built; rebuild it, or build with method=""hash""")
        if (self%backend == IX_DIRECT .and. off == 0_int64) call ix_abort("pf_index_map%" // what // &
            ": this key is outside the range the direct backend was built for; rebuild the map " // &
            "with this key included, or build with method=""hash""")
    end subroutine ix_check_mutable

    !> Ensures a map reached by `%set`/`%get_or_add` before any build has somewhere to put a key.
    subroutine ix_autoinit(self, nc)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer, intent(in) :: nc                 !! components the caller's key has.

        if (self%ncomp /= 0) return
        call ix_check_ncomp(nc, "set")
        call ix_do_init(self, ncomp=nc)
    end subroutine ix_autoinit

    !> `%set` for a single-component key.
    subroutine ix_set_scalar(self, key, value)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key         !! the key, already widened.
        integer(int64), intent(in) :: value       !! the value to store.
        integer(int64) :: off
        logical :: is_new

        call ix_check_scalar_shape(self, "set")
        call ix_check_value(value, "set")
        call ix_autoinit(self, 1)
        if (self%backend == IX_DIRECT) then
            off = ix_direct_off_1(self, key)
            call ix_check_mutable(self, off, "set")
            if (self%dvals(off) == 0_int64) self%nk = self%nk + 1_int64
            self%dvals(off) = value
        else
            call ix_check_mutable(self, 1_int64, "set")
            call ix_hash_insert_scalar(self, key, value, is_new)
        end if
        if (value > self%next_auto) self%next_auto = value
    end subroutine ix_set_scalar

    !> `%set` for a key tuple.
    subroutine ix_set_tuple(self, key, value)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key(:)      !! the tuple, already widened.
        integer(int64), intent(in) :: value       !! the value to store.
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: off
        integer :: nc
        logical :: is_new

        call ix_autoinit(self, size(key))
        nc = ix_tuple_width(self, size(key), "set")
        call ix_check_value(value, "set")
        if (self%backend == IX_DIRECT) then
            off = ix_direct_off_n(self, key)
            call ix_check_mutable(self, off, "set")
            if (self%dvals(off) == 0_int64) self%nk = self%nk + 1_int64
            self%dvals(off) = value
        else
            call ix_check_mutable(self, 1_int64, "set")
            ! Gathered into the stack buffer: the caller's tuple may be a strided section, and
            ! the insert's dummy is `contiguous` (its hash takes an explicit-shape array), so
            ! the copy is made here, once, rather than by a temporary at the call.
            kb(1:nc) = key(1:nc)
            call ix_hash_insert(self, kb(1:nc), value, is_new)
        end if
        if (value > self%next_auto) self%next_auto = value
    end subroutine ix_set_tuple

    !> `%set` for an `int32` key tuple: validate, widen, then the `int64` path.
    subroutine ix_set_tuple_i32(self, key, value)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key(:)      !! the caller's tuple.
        integer(int64), intent(in) :: value       !! the value to store.
        integer(int64) :: kb(pf_index_max_components)
        integer :: nc

        call ix_autoinit(self, size(key))
        call ix_widen_tuple(self, key, kb, nc, "set")
        call ix_set_tuple(self, kb(1:nc), value)
    end subroutine ix_set_tuple_i32

    !> `%get_or_add` for a single-component key.
    subroutine ix_goa_scalar(self, key, idx, want32)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key         !! the key, already widened.
        integer(int64), intent(out) :: idx        !! the key's index.
        logical, intent(in) :: want32             !! whether the answer must fit `int32`.

        call ix_check_scalar_shape(self, "get_or_add")
        call ix_autoinit(self, 1)
        idx = ix_get_scalar(self, key)
        if (idx <= 0_int64) then
            idx = self%next_auto + 1_int64
            call ix_set_scalar(self, key, idx)
        end if
        if (want32) idx = ix_narrow_check(idx)
    end subroutine ix_goa_scalar

    !> `%get_or_add` for a key tuple.
    subroutine ix_goa_tuple(self, key, idx, want32)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key(:)      !! the tuple, already widened.
        integer(int64), intent(out) :: idx        !! the key's index.
        logical, intent(in) :: want32             !! whether the answer must fit `int32`.
        integer :: nc

        call ix_autoinit(self, size(key))
        nc = ix_tuple_width(self, size(key), "get_or_add")
        idx = ix_get_tuple(self, key(1:nc))
        if (idx <= 0_int64) then
            idx = self%next_auto + 1_int64
            call ix_set_tuple(self, key(1:nc), idx)
        end if
        if (want32) idx = ix_narrow_check(idx)
    end subroutine ix_goa_tuple

    !> `%get_or_add` for an `int32` key tuple.
    subroutine ix_goa_tuple_i32(self, key, idx, want32)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key(:)      !! the caller's tuple.
        integer(int64), intent(out) :: idx        !! the key's index.
        logical, intent(in) :: want32             !! whether the answer must fit `int32`.
        integer(int64) :: kb(pf_index_max_components)
        integer :: nc

        call ix_autoinit(self, size(key))
        call ix_widen_tuple(self, key, kb, nc, "get_or_add")
        call ix_goa_tuple(self, kb(1:nc), idx, want32)
    end subroutine ix_goa_tuple_i32

    !> `%get_or_add_many` for rank-1 keys: `%get_or_add` per row, with the guard already held.
    !!
    !! The per-row work is the scalar worker's exactly -- one lookup and, for a new key, one
    !! insert -- so what this saves over a loop of `%get_or_add` is the guard, taken once here
    !! rather than once per key. The `int32` narrowing happens per row inside the guard, as the
    !! scalar form does, so the abort for an oversized code fires with at most one thread on the
    !! fatal path. This serial pass assigns the codes in first-appearance order, which the
    !! doc-comment on the specifics says not to rely on: on a hash map and a team of two or more
    !! the rows go to `ix_hash_goam_part_1` instead -- a lock-free lookup of every key on the
    !! team, the keys not found inserted by the partitioned pass the hash build uses and numbered
    !! partition by partition -- and the `int32` check then runs once over the finished codes.
    subroutine ix_goam_1(self, keys, codes, valid, want32, threads)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:)     !! the keys, already widened.
        integer(int64), intent(out) :: codes(:)   !! one code per key; 0 for a masked row.
        logical, intent(in), optional :: valid(:) !! the caller's mask, or absent.
        logical, intent(in) :: want32             !! whether every code must fit `int32`.
        integer, intent(in), optional :: threads  !! the caller's thread request, or absent.
        integer(int64) :: i, n, idx
        integer :: nt
        logical :: hv

        call ix_check_scalar_shape(self, "get_or_add_many")
        call ix_autoinit(self, 1)
        n = ix_many_rows(self, size(keys, kind=int64), size(codes, kind=int64), 1, "get_or_add_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_or_add_many")
        nt = ix_threads_for(n, threads, "get_or_add_many")
        if (nt > 1 .and. self%backend == IX_HASH) then
            call ix_hash_goam_part_1(self, keys, codes, valid, nt)
            if (want32) call ix_check_codes_fit(codes)
            return
        end if
        !$omp atomic write
        dbg_index_spills = -1_int64
        do i = 1_int64, n
            if (hv) then
                if (.not. valid(i)) then
                    codes(i) = 0_int64
                    cycle
                end if
            end if
            call ix_goa_scalar(self, keys(i), idx, want32)
            codes(i) = idx
        end do
    end subroutine ix_goam_1

    !> `%get_or_add_many` for rank-2 keys, one tuple per row. See `ix_goam_1`.
    subroutine ix_goam_n(self, keys, codes, valid, want32, threads)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: keys(:,:)   !! the tuples, already widened, shaped `(n, ncomp)`.
        integer(int64), intent(out) :: codes(:)   !! one code per row; 0 for a masked row.
        logical, intent(in), optional :: valid(:) !! the caller's mask, or absent.
        logical, intent(in) :: want32             !! whether every code must fit `int32`.
        integer, intent(in), optional :: threads  !! the caller's thread request, or absent.
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: i, n, idx
        integer :: nc, j, nt
        logical :: hv

        nc = int(size(keys, 2))
        call ix_check_ncomp(nc, "get_or_add_many")
        call ix_autoinit(self, nc)
        n = ix_many_rows(self, size(keys, 1, kind=int64), size(codes, kind=int64), nc, "get_or_add_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_or_add_many")
        nt = ix_threads_for(n, threads, "get_or_add_many")
        if (nt > 1 .and. self%backend == IX_HASH .and. nc > 1) then
            call ix_hash_goam_part_n(self, keys, codes, valid, nt)
            if (want32) call ix_check_codes_fit(codes)
            return
        else if (nt > 1 .and. self%backend == IX_HASH) then
            ! A 1-tuple map is the scalar table underneath, and takes the scalar pass.
            call ix_hash_goam_part_1(self, keys(:, 1), codes, valid, nt)
            if (want32) call ix_check_codes_fit(codes)
            return
        end if
        !$omp atomic write
        dbg_index_spills = -1_int64
        do i = 1_int64, n
            if (hv) then
                if (.not. valid(i)) then
                    codes(i) = 0_int64
                    cycle
                end if
            end if
            do j = 1, nc
                kb(j) = keys(i, j)
            end do
            call ix_goa_tuple(self, kb(1:nc), idx, want32)
            codes(i) = idx
        end do
    end subroutine ix_goam_n

    !> `%remove` for a single-component key.
    !!
    !! The absent-key abort happens here, with the guard held, rather than in the caller after the
    !! region has been left -- so two threads removing two absent keys cannot terminate the process
    !! at the same moment, which leaves the exit status undefined under ifx.
    subroutine ix_remove_scalar(self, key, hit, has_found)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key         !! the key, already widened.
        logical, intent(out) :: hit               !! `.true.` when the key was there.
        logical, intent(in) :: has_found          !! whether the caller passed `found=`.
        integer(int64) :: off

        call ix_check_scalar_shape(self, "remove")
        hit = .false.
        if (self%backend == IX_SORTED) call ix_check_mutable(self, 1_int64, "remove")
        if (self%backend == IX_DIRECT) then
            off = ix_direct_off_1(self, key)
            if (off > 0_int64) then
                if (self%dvals(off) /= 0_int64) then
                    self%dvals(off) = 0_int64
                    self%nk = self%nk - 1_int64
                    hit = .true.
                end if
            end if
        else
            call ix_hash_remove_scalar(self, key, hit)
        end if
        call ix_removal_report(hit, has_found)
    end subroutine ix_remove_scalar

    !> `%remove` for a key tuple.
    subroutine ix_remove_tuple(self, key, hit, has_found)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: key(:)      !! the tuple, already widened.
        logical, intent(out) :: hit               !! `.true.` when the key was there.
        logical, intent(in) :: has_found          !! whether the caller passed `found=`.
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: off
        integer :: nc

        hit = .false.
        if (self%ncomp == 0) then
            call ix_removal_report(hit, has_found)
            return
        end if
        nc = ix_tuple_width(self, size(key), "remove")
        if (self%backend == IX_SORTED) call ix_check_mutable(self, 1_int64, "remove")
        if (self%backend == IX_DIRECT) then
            off = ix_direct_off_n(self, key)
            if (off > 0_int64) then
                if (self%dvals(off) /= 0_int64) then
                    self%dvals(off) = 0_int64
                    self%nk = self%nk - 1_int64
                    hit = .true.
                end if
            end if
        else
            ! Gathered for the reason `ix_set_tuple` gives.
            kb(1:nc) = key(1:nc)
            call ix_hash_remove(self, kb(1:nc), hit)
        end if
        call ix_removal_report(hit, has_found)
    end subroutine ix_remove_tuple

    !> `%remove` for an `int32` key tuple.
    subroutine ix_remove_tuple_i32(self, key, hit, has_found)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int32), intent(in) :: key(:)      !! the caller's tuple.
        logical, intent(out) :: hit               !! `.true.` when the key was there.
        logical, intent(in) :: has_found          !! whether the caller passed `found=`.
        integer(int64) :: kb(pf_index_max_components)
        integer :: nc

        hit = .false.
        if (self%ncomp == 0) then
            call ix_removal_report(hit, has_found)
            return
        end if
        call ix_widen_tuple(self, key, kb, nc, "remove")
        call ix_remove_tuple(self, kb(1:nc), hit, has_found)
    end subroutine ix_remove_tuple_i32

    !> Aborts when a key that was asked to be removed was not there and no `found=` was passed.
    subroutine ix_removal_report(hit, has_found)
        logical, intent(in) :: hit       !! whether the key was found.
        logical, intent(in) :: has_found !! whether the caller passed `found=`.

        if (hit .or. has_found) return
        call ix_abort("pf_index_map%remove: this key is not in the map; pass found= if an absent " // &
            "key is acceptable")
    end subroutine ix_removal_report

    ! ---- Key enumeration ----

    !> Copies every stored key into `out`, shaped `(nkeys, ncomp)`.
    !!
    !! The direct arm decodes each occupied slot's offset back into components, which is the
    !! mixed-radix construction run backwards: one division per component per key. Cold path, and
    !! the only place in the module that divides by a runtime value.
    pure subroutine ix_collect_keys(self, out)
        type(pf_index_map), intent(in) :: self  !! the map.
        integer(int64), intent(out) :: out(:,:) !! shaped `(nkeys, ncomp)`.
        integer(int64) :: off, k, r, q
        integer :: j

        select case (self%backend)
        case (IX_DIRECT)
            k = 0_int64
            if (.not. allocated(self%dvals)) return
            do off = 1_int64, size(self%dvals, kind=int64)
                if (self%dvals(off) == 0_int64) cycle
                k = k + 1_int64
                r = off - 1_int64
                do j = self%ncomp, 1, -1
                    q = r / self%dstride(j)
                    out(k, j) = self%dkmin(j) + q
                    r = r - q * self%dstride(j)
                end do
            end do
        case (IX_HASH)
            call ix_hash_collect(self, out)
        case (IX_SORTED)
            out(1:self%nk, 1) = self%skeys(1:self%nk)
        end select
    end subroutine ix_collect_keys

    ! ---- The builds ----

    !> Gives a zero-key map the backend it was asked for, with nothing in it.
    !!
    !! A built-but-empty map still reports its resolved `%get_method`, which is what makes
    !! `build(no keys, method="hash")` predictable rather than a special case. The direct arm keeps
    !! the empty `0 .. -1` range, so every lookup falls out of it.
    subroutine ix_finish_empty(self, want, nc)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer, intent(in) :: want               !! the resolved backend, or `IX_WANT_AUTO`.
        integer, intent(in) :: nc                 !! components per key.
        integer :: j

        select case (want)
        case (IX_HASH)
            self%backend = IX_HASH
            call ix_hash_reserve(self, 0_int64)
        case (IX_SORTED)
            self%backend = IX_SORTED
            allocate(self%skeys(0))
            allocate(self%svals(0))
        case default
            self%backend = IX_DIRECT
            allocate(self%dkmin(nc), self%dkmax(nc), self%drange(nc), self%dstride(nc))
            do j = 1, nc
                self%dkmin(j) = 0_int64
                self%dkmax(j) = -1_int64
                self%drange(j) = 0_int64
                self%dstride(j) = 1_int64
            end do
        end select
    end subroutine ix_finish_empty

    !> Fills the direct backend's geometry for a single-component map.
    subroutine ix_set_direct_geometry_1(self, lo, hi, span)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: lo          !! smallest key.
        integer(int64), intent(in) :: hi          !! largest key.
        integer(int64), intent(in) :: span        !! positions spanned.

        self%backend = IX_DIRECT
        self%kmin1 = lo
        self%kmax1 = hi
        ! Both representations are filled for a single-component map, so that the SAME map answers
        ! a scalar `%get(k)` and a 1-tuple `%get([k])`. The design makes those the same map; this
        ! is where that becomes true.
        allocate(self%dkmin(1), self%dkmax(1), self%drange(1), self%dstride(1))
        self%dkmin(1) = lo
        self%dkmax(1) = hi
        self%drange(1) = span
        self%dstride(1) = 1_int64
    end subroutine ix_set_direct_geometry_1

    !> Allocates the direct backend's slot array, reporting a refused allocation clearly.
    !!
    !! `stat=` rather than letting the runtime abort: a caller who asked for `method="direct"` over
    !! a wide key range gets told how many slots that needed, which is the one number that makes
    !! the mistake obvious. The automatic choice cannot reach this -- its whole job is to keep the
    !! span inside `ix_budget`.
    subroutine ix_alloc_direct(self, total)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: total       !! slots to allocate.
        integer :: ios
        character(len=32) :: t

        allocate(self%dvals(total), stat=ios)
        if (ios /= 0) then
            write (t, "(i0)") total
            call ix_abort("pf_index_map%build: could not allocate " // trim(t) // &
                " slots for the direct backend; use method=""hash"" for keys this widely spread")
        end if
        self%dvals = 0_int64
    end subroutine ix_alloc_direct

    !> Names the duplicate key a threaded direct scatter detected by counting.
    !!
    !! The threaded scatter writes unconditionally -- each unique key owns a distinct slot, so
    !! there is no race -- and then counts the occupied slots. A count below the key count proves
    !! two keys shared a slot, i.e. were equal, and this serial re-pass is what turns that proof
    !! into a message naming the offender. Paying for a second pass only on the error path is the
    !! right side to pay on.
    subroutine ix_name_duplicate_1(self, keys, valid)
        type(pf_index_map), intent(inout) :: self !! the map, with its slots already scattered.
        integer(int64), intent(in) :: keys(:)     !! the keys, in the caller's order.
        logical, intent(in), optional :: valid(:) !! the build's mask, or absent.
        integer(int64) :: i, off
        character(len=32) :: t, p

        self%dvals = 0_int64
        do i = 1_int64, size(keys, kind=int64)
            if (present(valid)) then
                if (.not. valid(i)) cycle
            end if
            off = keys(i) - self%kmin1 + 1_int64
            if (self%dvals(off) /= 0_int64) then
                write (t, "(i0)") keys(i)
                write (p, "(i0)") i
                call ix_abort("pf_index_map%build: duplicate key " // trim(t) // " at position " // &
                    trim(p) // " (every key must be unique)")
            end if
            self%dvals(off) = 1_int64
        end do
        call ix_abort("pf_index_map%build: duplicate keys were detected but could not be named")
    end subroutine ix_name_duplicate_1

    !> Names the duplicate tuple a threaded composite direct scatter detected. See
    !! `ix_name_duplicate_1`.
    subroutine ix_name_duplicate_n(self, keys, valid)
        type(pf_index_map), intent(inout) :: self !! the map, with its slots already scattered.
        integer(int64), intent(in) :: keys(:,:)   !! the key tuples, one per row.
        logical, intent(in), optional :: valid(:) !! the build's mask, or absent.
        integer(int64) :: i, off
        integer :: j
        character(len=32) :: p
        character(len=:), allocatable :: txt

        self%dvals = 0_int64
        do i = 1_int64, size(keys, 1, kind=int64)
            if (present(valid)) then
                if (.not. valid(i)) cycle
            end if
            off = 1_int64
            do j = 1, self%ncomp
                off = off + (keys(i, j) - self%dkmin(j)) * self%dstride(j)
            end do
            if (self%dvals(off) /= 0_int64) then
                call ix_tuple_text(keys(i, :), txt)
                write (p, "(i0)") i
                call ix_abort("pf_index_map%build: duplicate key " // txt // " at position " // &
                    trim(p) // " (every key tuple must be unique)")
            end if
            self%dvals(off) = 1_int64
        end do
        call ix_abort("pf_index_map%build: duplicate keys were detected but could not be named")
    end subroutine ix_name_duplicate_n

    !> Renders a key tuple as `[a, b, c]` for an error message.
    subroutine ix_tuple_text(key, out)
        integer(int64), intent(in) :: key(:)                !! the tuple.
        character(len=:), allocatable, intent(out) :: out   !! the rendered text.
        character(len=32) :: t
        integer :: j

        out = "["
        do j = 1, size(key)
            write (t, "(i0)") key(j)
            if (j > 1) out = out // ", "
            out = out // trim(t)
        end do
        out = out // "]"
    end subroutine ix_tuple_text

    !> Counts the rows a build will store and finds the first and last of them.
    !!
    !! Every row when `valid` is absent. With a mask, `first` is where the min/max scans start
    !! (the scan cannot start from row 1, which may be masked) and `last` is the largest row
    !! number the default values will store, which is where `%get_or_add` continues from.
    !! Both are 0 when nothing is stored, and neither is read then.
    subroutine ix_mask_extent(n, valid, nv, first, last)
        integer(int64), intent(in) :: n           !! rows presented.
        logical, intent(in), optional :: valid(:) !! the build's mask, or absent.
        integer(int64), intent(out) :: nv         !! rows that will be stored.
        integer(int64), intent(out) :: first      !! first stored row; 0 when `nv` is 0.
        integer(int64), intent(out) :: last       !! last stored row; 0 when `nv` is 0.
        integer(int64) :: i

        nv = n
        first = 1_int64
        last = n
        if (.not. present(valid)) return
        nv = count(valid, kind=int64)
        first = 0_int64
        last = 0_int64
        if (nv == 0_int64) return
        do i = 1_int64, n
            if (valid(i)) then
                first = i
                exit
            end if
        end do
        do i = n, 1_int64, -1_int64
            if (valid(i)) then
                last = i
                exit
            end if
        end do
    end subroutine ix_mask_extent

    !> The sorted backend over the unmasked rows only: compacts the keys and their values, then
    !> builds as usual.
    !!
    !! `ix_sorted_build` reads "the value of key `i` is `i`" from position and hands `pf_argsort`
    !! one contiguous array, so a mask cannot be threaded through it without touching the one
    !! backend the mask does not otherwise reach. Compacting once here costs a temporary of the
    !! stored size, which the opt-in backend's own sort dwarfs.
    subroutine ix_sorted_build_masked(self, keys, values, valid, nv, threads)
        type(pf_index_map), intent(inout) :: self         !! the map; its sorted storage is replaced.
        integer(int64), intent(in) :: keys(:)             !! the keys, already widened.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for the row numbers.
        logical, intent(in) :: valid(:)                   !! the build's mask.
        integer(int64), intent(in) :: nv                  !! rows the mask keeps; `count(valid)`.
        integer, intent(in), optional :: threads          !! forwarded to `pf_argsort`.
        integer(int64), allocatable :: ck(:), cv(:)
        integer(int64) :: i, k

        allocate(ck(nv), cv(nv))
        k = 0_int64
        do i = 1_int64, size(keys, kind=int64)
            if (.not. valid(i)) cycle
            k = k + 1_int64
            ck(k) = keys(i)
            if (present(values)) then
                cv(k) = values(i)
            else
                cv(k) = i
            end if
        end do
        call ix_sorted_build(self, ck, cv, threads)
    end subroutine ix_sorted_build_masked

    !> Builds a single-component map from a rank-1 key array.
    !!
    !! With `valid`, a masked row is skipped by every pass -- the scan, the scatter or insert, the
    !! duplicate count -- and the stored values stay the caller's row numbers, so the map answers
    !! the row a key sits in even when rows before it were masked. `nv`, the rows actually stored,
    !! is what sizes the backend choice and the table; `n`, the rows presented, is what the scan
    !! walks and what the thread rule is bounded by.
    subroutine ix_build_1(self, keys, values, method, threads, valid)
        type(pf_index_map), intent(inout) :: self        !! the map; rebuilt from scratch.
        integer(int64), intent(in) :: keys(:)            !! the keys; must be unique where unmasked.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for the row numbers.
        character(len=*), intent(in), optional :: method  !! backend token.
        integer, intent(in), optional :: threads          !! threads to build with.
        logical, intent(in), optional :: valid(:)         !! per row; `.false.` skips the row.
        integer(int64) :: n, nv, first, last, lo, hi, span, budget, i, off, cnt
        integer :: want, nt
        logical :: has_v, hv, is_new, fits, dup

        call ix_build_begin()
        n = size(keys, kind=int64)
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "build")
        has_v = present(values)
        if (has_v) then
            call ix_check_values_len(size(values, kind=int64), n)
            call ix_check_values_range(values, valid)
        end if
        call ix_resolve_method(method, want, .true., "build")
        call ix_reset_storage(self)
        self%ncomp = 1
        call ix_mask_extent(n, valid, nv, first, last)
        self%nk = nv
        if (nv == 0_int64) then
            call ix_finish_empty(self, want, 1)
            return
        end if
        if (has_v) then
            if (hv) then
                self%next_auto = maxval(values, mask=valid)
            else
                self%next_auto = maxval(values)
            end if
        else
            self%next_auto = last
        end if
        nt = ix_threads_for(n, threads)
        lo = keys(first)
        hi = keys(first)
        !$omp parallel do default(shared) private(i) reduction(min:lo) reduction(max:hi) &
        !$omp     schedule(static) num_threads(nt) if (nt > 1)
        do i = 1_int64, n
            if (hv) then
                if (.not. valid(i)) cycle
            end if
            if (keys(i) < lo) lo = keys(i)
            if (keys(i) > hi) hi = keys(i)
        end do
        if (want == IX_WANT_AUTO) then
            budget = ix_budget(nv)
            call ix_span_ok(lo, hi, budget, span, fits)
            if (fits) then
                want = IX_DIRECT
            else
                want = IX_HASH
            end if
        else if (want == IX_DIRECT) then
            call ix_span_ok(lo, hi, huge(0_int64), span, fits)
            if (.not. fits) call ix_abort(&
                "pf_index_map%build: method=""direct"" cannot cover this key range; " // &
                "it exceeds the whole int64 domain. Use method=""hash"".")
        end if
        select case (want)
        case (IX_DIRECT)
            call ix_set_direct_geometry_1(self, lo, hi, span)
            call ix_alloc_direct(self, span)
            if (has_v) then
                !$omp parallel do default(shared) private(i, off) schedule(static) &
                !$omp     num_threads(nt) if (nt > 1)
                do i = 1_int64, n
                    if (hv) then
                        if (.not. valid(i)) cycle
                    end if
                    off = keys(i) - lo + 1_int64
                    self%dvals(off) = values(i)
                end do
            else
                !$omp parallel do default(shared) private(i, off) schedule(static) &
                !$omp     num_threads(nt) if (nt > 1)
                do i = 1_int64, n
                    if (hv) then
                        if (.not. valid(i)) cycle
                    end if
                    off = keys(i) - lo + 1_int64
                    self%dvals(off) = i
                end do
            end if
            cnt = 0_int64
            !$omp parallel do default(shared) private(off) reduction(+:cnt) schedule(static) &
            !$omp     num_threads(nt) if (nt > 1)
            do off = 1_int64, span
                if (self%dvals(off) /= 0_int64) cnt = cnt + 1_int64
            end do
            if (cnt /= nv) call ix_name_duplicate_1(self, keys, valid)
        case (IX_HASH)
            self%backend = IX_HASH
            self%nk = 0_int64
            if (nt > 1) then
                call ix_hash_build_part_1(self, keys, values, valid, nv, nt, dup)
                if (.not. dup) return
                ! The partitioned pass met a duplicate but cannot say which copy is the later one
                ! in the caller's order. The serial loop below, over the emptied table, meets the
                ! same pair at its canonical position and names it exactly as a serial build does.
                self%slots(:)%val = 0_int64
                self%nk = 0_int64
            else
                call ix_hash_reserve(self, nv)
            end if
            !$omp atomic write
            dbg_index_spills = -1_int64
            do i = 1_int64, n
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                if (has_v) then
                    call ix_hash_insert_scalar(self, keys(i), values(i), is_new)
                else
                    call ix_hash_insert_scalar(self, keys(i), i, is_new)
                end if
                if (.not. is_new) call ix_report_duplicate_1(keys(i), i)
            end do
            if (nt > 1) call ix_abort("pf_index_map%build: duplicate keys were detected but " // &
                "could not be named")
        case (IX_SORTED)
            self%backend = IX_SORTED
            if (hv) then
                call ix_sorted_build_masked(self, keys, values, valid, nv, threads)
            else
                call ix_sorted_build(self, keys, values, threads)
            end if
        end select
    end subroutine ix_build_1

    !> Reports a duplicate key the serial hash insert loop met.
    subroutine ix_report_duplicate_1(key, at)
        integer(int64), intent(in) :: key !! the offending key.
        integer(int64), intent(in) :: at  !! its position in the caller's array.
        character(len=32) :: t, p

        write (t, "(i0)") key
        write (p, "(i0)") at
        call ix_abort("pf_index_map%build: duplicate key " // trim(t) // " at position " // &
            trim(p) // " (every key must be unique)")
    end subroutine ix_report_duplicate_1

    !> Reports a duplicate key tuple the serial hash insert loop met.
    subroutine ix_report_duplicate_n(key, at)
        integer(int64), intent(in) :: key(:) !! the offending tuple.
        integer(int64), intent(in) :: at     !! its row in the caller's array.
        character(len=32) :: p
        character(len=:), allocatable :: txt

        call ix_tuple_text(key, txt)
        write (p, "(i0)") at
        call ix_abort("pf_index_map%build: duplicate key " // txt // " at position " // &
            trim(p) // " (every key tuple must be unique)")
    end subroutine ix_report_duplicate_n

    !> Builds a map with `ncomp` components from a rank-2 key array shaped `(n, ncomp)`.
    !!
    !! With `valid`, a masked row is skipped by every pass, exactly as in `ix_build_1`.
    subroutine ix_build_n(self, keys, values, method, threads, valid)
        type(pf_index_map), intent(inout) :: self        !! the map; rebuilt from scratch.
        integer(int64), intent(in) :: keys(:,:)          !! keys, one per row, one component per column.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for the row numbers.
        character(len=*), intent(in), optional :: method  !! backend token.
        integer, intent(in), optional :: threads          !! threads to build with.
        logical, intent(in), optional :: valid(:)         !! per row; `.false.` skips the row.
        integer(int64) :: lo(pf_index_max_components), hi(pf_index_max_components)
        integer(int64) :: sp(pf_index_max_components), kb(pf_index_max_components)
        integer(int64) :: n, nv, first, last, budget, total, grown_total, i, off, cnt, l, h, span
        integer :: want, nt, nc, j
        logical :: has_v, hv, is_new, fits, dup

        call ix_build_begin()
        n = size(keys, 1, kind=int64)
        nc = int(size(keys, 2))
        call ix_check_ncomp(nc, "build")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "build")
        has_v = present(values)
        if (has_v) then
            call ix_check_values_len(size(values, kind=int64), n)
            call ix_check_values_range(values, valid)
        end if
        call ix_resolve_method(method, want, .true., "build")
        if (want == IX_SORTED .and. nc > 1) call ix_abort("pf_index_map%build: " // &
            "method=""sorted"" supports single-component keys only; use ""hash"" or ""direct"" " // &
            "for composite keys")
        call ix_reset_storage(self)
        self%ncomp = nc
        call ix_mask_extent(n, valid, nv, first, last)
        self%nk = nv
        if (nv == 0_int64) then
            call ix_finish_empty(self, want, nc)
            return
        end if
        if (has_v) then
            if (hv) then
                self%next_auto = maxval(values, mask=valid)
            else
                self%next_auto = maxval(values)
            end if
        else
            self%next_auto = last
        end if
        nt = ix_threads_for(n, threads)
        ! One scalar-reduction pass per component. Deliberately not an array reduction over
        ! `(nc)`: support for those differs across the compilers this library is built with, and
        ! each column here is stride-1, so the separate passes cost nothing over one fused pass.
        do j = 1, nc
            l = keys(first, j)
            h = keys(first, j)
            !$omp parallel do default(shared) private(i) reduction(min:l) reduction(max:h) &
            !$omp     schedule(static) num_threads(nt) if (nt > 1)
            do i = 1_int64, n
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                if (keys(i, j) < l) l = keys(i, j)
                if (keys(i, j) > h) h = keys(i, j)
            end do
            lo(j) = l
            hi(j) = h
        end do
        ! The product of the component spans, refused before it can overflow.
        budget = ix_budget(nv)
        if (want == IX_DIRECT) budget = huge(0_int64)
        fits = .true.
        total = 1_int64
        do j = 1, nc
            call ix_span_ok(lo(j), hi(j), budget, span, fits)
            if (.not. fits) exit
            sp(j) = span
            ! `grown_total` rather than passing `total` as both the input and the output: one
            ! variable associated with an `intent(in)` and an `intent(out)` dummy of the same call
            ! is forbidden (F2018 15.5.2.13) and no compiler here diagnoses it. It failed exactly
            ! as the standard allows -- `out = 0` cleared the running product before the input was
            ! read, so the total came out 0, the slot array was allocated empty, and the scatter
            ! wrote past it.
            call ix_product_ok(total, span, budget, grown_total, fits)
            if (.not. fits) exit
            total = grown_total
        end do
        if (want == IX_WANT_AUTO) then
            if (fits) then
                want = IX_DIRECT
            else
                want = IX_HASH
            end if
        else if (want == IX_DIRECT .and. .not. fits) then
            call ix_abort("pf_index_map%build: method=""direct"" cannot cover these key ranges; " // &
                "their product exceeds the int64 domain. Use method=""hash"".")
        end if
        select case (want)
        case (IX_DIRECT)
            self%backend = IX_DIRECT
            allocate(self%dkmin(nc), self%dkmax(nc), self%drange(nc), self%dstride(nc))
            do j = 1, nc
                self%dkmin(j) = lo(j)
                self%dkmax(j) = hi(j)
                self%drange(j) = sp(j)
            end do
            self%dstride(1) = 1_int64
            do j = 2, nc
                self%dstride(j) = self%dstride(j - 1) * self%drange(j - 1)
            end do
            if (nc == 1) then
                self%kmin1 = lo(1)
                self%kmax1 = hi(1)
            end if
            call ix_alloc_direct(self, total)
            if (has_v) then
                !$omp parallel do default(shared) private(i, j, off) schedule(static) &
                !$omp     num_threads(nt) if (nt > 1)
                do i = 1_int64, n
                    if (hv) then
                        if (.not. valid(i)) cycle
                    end if
                    off = 1_int64
                    do j = 1, nc
                        off = off + (keys(i, j) - self%dkmin(j)) * self%dstride(j)
                    end do
                    self%dvals(off) = values(i)
                end do
            else
                !$omp parallel do default(shared) private(i, j, off) schedule(static) &
                !$omp     num_threads(nt) if (nt > 1)
                do i = 1_int64, n
                    if (hv) then
                        if (.not. valid(i)) cycle
                    end if
                    off = 1_int64
                    do j = 1, nc
                        off = off + (keys(i, j) - self%dkmin(j)) * self%dstride(j)
                    end do
                    self%dvals(off) = i
                end do
            end if
            cnt = 0_int64
            !$omp parallel do default(shared) private(off) reduction(+:cnt) schedule(static) &
            !$omp     num_threads(nt) if (nt > 1)
            do off = 1_int64, total
                if (self%dvals(off) /= 0_int64) cnt = cnt + 1_int64
            end do
            if (cnt /= nv) call ix_name_duplicate_n(self, keys, valid)
        case (IX_HASH)
            self%backend = IX_HASH
            self%nk = 0_int64
            if (nt > 1 .and. nc > 1) then
                call ix_hash_build_part_n(self, keys, values, valid, nv, nt, dup)
                if (.not. dup) return
                ! As in `ix_build_1`: the serial loop names the duplicate at its canonical position.
                self%hrec = 0_int64
                self%nk = 0_int64
            else if (nt > 1) then
                ! A 1-tuple map is the scalar table underneath, and takes the scalar pass.
                call ix_hash_build_part_1(self, keys(:, 1), values, valid, nv, nt, dup)
                if (.not. dup) return
                self%slots(:)%val = 0_int64
                self%nk = 0_int64
            else
                call ix_hash_reserve(self, nv)
            end if
            !$omp atomic write
            dbg_index_spills = -1_int64
            ! The row is gathered into a contiguous buffer first: `keys(i, :)` is a strided
            ! section, and the insert's tuple hash takes an explicit-shape array, so passing the
            ! section directly would have the compiler pack it into a temporary per key anyway.
            do i = 1_int64, n
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                do j = 1, nc
                    kb(j) = keys(i, j)
                end do
                if (has_v) then
                    call ix_hash_insert(self, kb(1:nc), values(i), is_new)
                else
                    call ix_hash_insert(self, kb(1:nc), i, is_new)
                end if
                if (.not. is_new) call ix_report_duplicate_n(kb(1:nc), i)
            end do
            if (nt > 1) call ix_abort("pf_index_map%build: duplicate keys were detected but " // &
                "could not be named")
        case (IX_SORTED)
            self%backend = IX_SORTED
            ! `keys(:, 1)` is a contiguous column, so this passes the caller's own storage.
            if (hv) then
                call ix_sorted_build_masked(self, keys(:, 1), values, valid, nv, threads)
            else
                call ix_sorted_build(self, keys(:, 1), values, threads)
            end if
            self%kmin1 = lo(1)
            self%kmax1 = hi(1)
        end select
    end subroutine ix_build_n

end submodule parquet_index_map
