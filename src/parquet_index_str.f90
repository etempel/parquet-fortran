!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> String keys for `pf_index_map` and `pf_index_multimap`: every string-keyed entry of both
!> types, the string store beside the map, and the `(hash, occurrence)` chain.
!!
!! **A string map is the composite hash map, with the strings kept beside it.** A key is hashed
!! to 64 bits by `ix_hash_str` -- the module's own mixer chained over the bytes -- and stored in
!! the ordinary two-component hash table under the tuple `(hash, occurrence)`, whose VALUE is
!! the string's position `p` in a packed store (`soff`, `sdat`) the map owns; the caller's value
!! sits in `sval(p)`. A lookup hashes, probes `(hash, 0)`, and compares the stored bytes against
!! the query: equal means found, unequal means a genuine collision and the probe moves on to
!! `(hash, 1)`, and so on until a miss. One probe and one compare in the overwhelming case; no
!! lookup ever answers on a hash match alone, and no legitimate key set is ever refused, however
!! the hashes fall. That is answer F3 of feature_pandas_S7.md as written, and the reason the
!! debug hook `parquet_debug_set_index_string_hash_bits` exists: two keys found in real data share
!! a 64-bit hash about once in 2**64 pairs, so the chain is exercised by narrowing the hash in a
!! test. Keys BUILT to collide are another matter -- the hash is unkeyed, so they can be solved for
!! (`ix_hash_str`) -- and cost a lookup one compare per key of their run: correct, but slow,
!! which is why a run reaching `IX_STR_CHAIN_WARN` is reported and `%probe_stats` measures it.
!!
!! **The occurrence chain is DENSE, and removal keeps it so.** `occurrence` counts `0, 1, 2, ...`
!! with no gaps, because a lookup stops at the first miss: a removal that left a gap would hide
!! every later occurrence of that hash. So `ix_str_remove` moves the chain's LAST entry into the
!! removed one's slot and deletes the last tuple instead, which keeps the chain dense at the cost
!! of one extra probe on that cold path. The removed string's bytes stay in the store with
!! `sval = 0` until the map is rebuilt or cleared; `%memory_bytes` counts them, `%nkeys`, `%keys`
!! and every lookup do not.
!!
!! **What a key IS: its exact bytes.** An element of a `character` ARRAY is trimmed of trailing
!! blanks first, the rule every character array argument of this library follows (its elements
!! share one declared length, so the padding a shorter value carries cannot be what the caller
!! meant); a scalar `character` key is taken as written; a `parquet_string_column` element is
!! taken verbatim. That is `pf_in`'s and `pf_match`'s equality for strings -- the sort engine
!! compares like `memcmp`, so `"ab"` and `"ab "` are two keys -- and it is what lets
!! `%build_index` over a string column agree with `pf_match_all` element for element. A NULL
!! element is never a key: a build skips it, a bulk form answers 0 for it, a scalar form cannot
!! present one.
!!
!! **A descendant of `parquet_index_map`, beside `parquet_index_multi` rather than below it**, so
!! that host association reaches both the map's private helpers (the string hash and the tuple
!! probe, the guards, the thread rule, `ix_adopt`, the store's lifecycle) and the multimap's
!! (`mm_layout`, `mm_release`, the gathers and the probe passes) -- both of which live in
!! `parquet_index_map.f90`, the one scope the two leaves share. Every string-keyed `module
!! procedure` of BOTH types lives here for that reason: the multimap's string build needs the
!! map's string `%get_or_add_many` and the multimap's layout in one body. A string `%build`
!! builds into a local map and adopts it under the guard, exactly as the integer builds do.
!!
!! A sibling and not a descendant because nagfor cannot compile a submodule three levels below
!! its module when the middle one reaches any name by host association (CLAUDE.md,
!! "fortran-gotchas.md"); `parquet_index_multi` reaches dozens.
!!
!! **A `parquet_string_column` is read in place.** The bulk forms alias the column's offsets and
!! payload through `parquet_string_column_raw_buffers` and `c_f_pointer` -- no copy -- with the
!! column dummy declared `target`, so the aliases are valid for the call's duration and are never
!! kept beyond it. Whether an element is null is asked of the column through the typed accessor,
!! row by row, only when the column carries a validity bitmap at all.
submodule (parquet_index:parquet_index_map) parquet_index_str
    use iso_c_binding, only: c_ptr, c_f_pointer, c_associated, c_loc, c_null_ptr
    use parquet_strings, only: parquet_string_column_is_null, parquet_string_column_raw_buffers
    implicit none

    !> Prefix of every message this file emits for the map; the multimap's is `MM`.
    character(len=*), parameter :: SP = "pf_index_map%"

    !> A parquet_string_column's buffers, aliased for the duration of one call.
    !!
    !! `contiguous` on both pointers is what tells the compiler a slice `dat(a:b)` needs no
    !! copy-in temporary when it is passed to `ix_hash_str`'s explicit-shape dummy.
    type :: ix_str_view
        integer(int64), pointer, contiguous :: off(:) => null()   !! offsets, `off(1) = 0`, length `n + 1`.
        character(len=1), pointer, contiguous :: dat(:) => null() !! the payload; unassociated when empty.
        integer(int64) :: n = 0_int64                             !! elements.
        logical :: nulls = .false.                                !! whether the column has a validity bitmap.
    end type ix_str_view

    !> Stands in for a payload that has no bytes, so that a zero-length key is still passed as
    !! an array (`IX_NO_BYTES` with `n = 0`) rather than as a slice of an unassociated pointer.
    !! Never read.
    character(len=1), parameter :: IX_NO_BYTES(1) = [" "]

contains

    ! ============================================================================================
    ! pf_index_map: bulk build. Six forwarders onto two workers, each building into a LOCAL map
    ! and adopting it under the guard (`ix_adopt`), as the integer builds do.
    ! ============================================================================================

    module procedure build_s1_nov
        type(pf_index_map) :: fresh

        call ix_str_build_chr(fresh, keys, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_s1_nov

    module procedure build_s1_v32
        integer(int64), allocatable :: v(:)
        type(pf_index_map) :: fresh

        call ix_widen_values(values, v)
        call ix_str_build_chr(fresh, keys, values=v, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_s1_v32

    module procedure build_s1_v64
        type(pf_index_map) :: fresh

        call ix_str_build_chr(fresh, keys, values=values, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_s1_v64

    module procedure build_sc_nov
        type(pf_index_map) :: fresh

        call ix_str_build_col(fresh, keys, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_sc_nov

    module procedure build_sc_v32
        integer(int64), allocatable :: v(:)
        type(pf_index_map) :: fresh

        call ix_widen_values(values, v)
        call ix_str_build_col(fresh, keys, values=v, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_sc_v32

    module procedure build_sc_v64
        type(pf_index_map) :: fresh

        call ix_str_build_col(fresh, keys, values=values, method=method, threads=threads, valid=valid)
        call ix_adopt(self, fresh)
    end procedure build_sc_v64

    ! ============================================================================================
    ! pf_index_map: lookup. Lock-free; the scalar forms `pure`, the bulk forms on a team.
    ! ============================================================================================

    module procedure get_s
        call ix_str_check_lookup(self, "get")
        idx = ix_str_value(self, key, len(key, kind=int64))
    end procedure get_s

    module procedure has_s
        call ix_str_check_lookup(self, "contains")
        ok = ix_str_value(self, key, len(key, kind=int64)) > 0_int64
    end procedure has_s

    module procedure many_s1_i32
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_str_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_chr_i32(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_s1_i32

    module procedure many_s1_i64
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        n = ix_str_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_chr_i64(self, keys, indexes, valid, hv, lo, hi)
        end do
    end procedure many_s1_i64

    module procedure many_sc_i32
        type(ix_str_view) :: v
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        call ix_str_open_view(keys, v)
        n = ix_str_many_rows(self, v%n, size(indexes, kind=int64), "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_col_i32(self, keys, v, indexes, valid, hv, lo, hi)
        end do
    end procedure many_sc_i32

    module procedure many_sc_i64
        type(ix_str_view) :: v
        integer(int64) :: n, lo, hi
        integer :: nt, c
        logical :: hv

        call ix_str_open_view(keys, v)
        n = ix_str_many_rows(self, v%n, size(indexes, kind=int64), "get_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = ix_lookup_threads_for(n, threads)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_col_i64(self, keys, v, indexes, valid, hv, lo, hi)
        end do
    end procedure many_sc_i64

    ! ============================================================================================
    ! pf_index_map: mutation. Guarded.
    ! ============================================================================================

    module procedure set_s_v32
        !$omp critical (pf_index_map_guard)
        call ix_str_set(self, key, int(value, int64))
        !$omp end critical (pf_index_map_guard)
    end procedure set_s_v32

    module procedure set_s_v64
        !$omp critical (pf_index_map_guard)
        call ix_str_set(self, key, value)
        !$omp end critical (pf_index_map_guard)
    end procedure set_s_v64

    module procedure goa_s_i32
        integer(int64) :: v

        !$omp critical (pf_index_map_guard)
        call ix_str_goa_entry(self, key, v, .true.)
        !$omp end critical (pf_index_map_guard)
        idx = int(v, int32)
    end procedure goa_s_i32

    module procedure goa_s_i64
        !$omp critical (pf_index_map_guard)
        call ix_str_goa_entry(self, key, idx, .false.)
        !$omp end critical (pf_index_map_guard)
    end procedure goa_s_i64

    module procedure goam_s1_i32
        integer(int64), allocatable :: c(:)

        allocate(c(size(codes, kind=int64)))
        !$omp critical (pf_index_map_guard)
        call ix_str_goam_chr(self, keys, c, valid, .true., threads)
        !$omp end critical (pf_index_map_guard)
        codes = int(c, int32)
    end procedure goam_s1_i32

    module procedure goam_s1_i64
        !$omp critical (pf_index_map_guard)
        call ix_str_goam_chr(self, keys, codes, valid, .false., threads)
        !$omp end critical (pf_index_map_guard)
    end procedure goam_s1_i64

    module procedure goam_sc_i32
        integer(int64), allocatable :: c(:)

        allocate(c(size(codes, kind=int64)))
        !$omp critical (pf_index_map_guard)
        call ix_str_goam_col(self, keys, c, valid, .true., threads)
        !$omp end critical (pf_index_map_guard)
        codes = int(c, int32)
    end procedure goam_sc_i32

    module procedure goam_sc_i64
        !$omp critical (pf_index_map_guard)
        call ix_str_goam_col(self, keys, codes, valid, .false., threads)
        !$omp end critical (pf_index_map_guard)
    end procedure goam_sc_i64

    module procedure rm_s
        logical :: hit

        !$omp critical (pf_index_map_guard)
        call ix_str_rm(self, key, hit, present(found))
        !$omp end critical (pf_index_map_guard)
        if (present(found)) found = hit
    end procedure rm_s

    ! ============================================================================================
    ! pf_index_map: the keys, as a column
    ! ============================================================================================

    module procedure map_keys_s
        if (self%ncomp /= 0 .and. .not. self%is_str) call ix_abort(SP // &
            "keys: this map holds integer keys; ask for an integer list")
        call ix_str_collect(self, list)
    end procedure map_keys_s

    ! ============================================================================================
    ! pf_index_multimap: bulk build. Six forwarders onto two workers, under the multimap's guard.
    ! ============================================================================================

    module procedure mm_build_s1_nov
        !$omp critical (pf_index_multimap_guard)
        call mm_str_build_chr(self, keys, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_s1_nov

    module procedure mm_build_s1_v32
        integer(int64), allocatable :: v(:)

        call ix_widen_values(values, v)
        !$omp critical (pf_index_multimap_guard)
        call mm_str_build_chr(self, keys, values=v, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_s1_v32

    module procedure mm_build_s1_v64
        !$omp critical (pf_index_multimap_guard)
        call mm_str_build_chr(self, keys, values=values, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_s1_v64

    module procedure mm_build_sc_nov
        !$omp critical (pf_index_multimap_guard)
        call mm_str_build_col(self, keys, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_sc_nov

    module procedure mm_build_sc_v32
        integer(int64), allocatable :: v(:)

        call ix_widen_values(values, v)
        !$omp critical (pf_index_multimap_guard)
        call mm_str_build_col(self, keys, values=v, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_sc_v32

    module procedure mm_build_sc_v64
        !$omp critical (pf_index_multimap_guard)
        call mm_str_build_col(self, keys, values=values, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_sc_v64

    ! ============================================================================================
    ! pf_index_multimap: scalar lookup. `pure`; one string lookup plus the CSR arithmetic.
    ! ============================================================================================

    module procedure mm_get_s
        g = mm_group_s(self, key, "get")
    end procedure mm_get_s

    module procedure mm_count_s
        n = mm_count_of(self, mm_group_s(self, key, "count"))
    end procedure mm_count_s

    module procedure mm_first_s
        v = mm_first_of(self, mm_group_s(self, key, "get_first"))
    end procedure mm_first_s

    module procedure mm_all_s_i32
        call mm_all_of_i32(self, mm_group_s(self, key, "get_all"), rows)
    end procedure mm_all_s_i32

    module procedure mm_all_s_i64
        call mm_all_of_i64(self, mm_group_s(self, key, "get_all"), rows)
    end procedure mm_all_s_i64

    module procedure mm_range_s
        call mm_range_of(self, mm_group_s(self, key, "get_range"), lo, hi)
    end procedure mm_range_s

    ! ============================================================================================
    ! pf_index_multimap: bulk lookup. The map's string chunk loop plus the multimap's own gather
    ! or probe pass, exactly as the integer forms are built.
    ! ============================================================================================

    module procedure mm_fmany_s1_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_str_rows(self, size(keys, kind=int64), size(rows, kind=int64), "get_first_many")
        call mm_check_int32_values(self, "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_chr_i32(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i32(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_s1_i32

    module procedure mm_fmany_s1_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_str_rows(self, size(keys, kind=int64), size(rows, kind=int64), "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_chr_i64(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i64(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_s1_i64

    module procedure mm_fmany_sc_i32
        type(ix_str_view) :: v
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        call ix_str_open_view(keys, v)
        n = mm_str_rows(self, v%n, size(rows, kind=int64), "get_first_many")
        call mm_check_int32_values(self, "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_col_i32(self%map, keys, v, rows, valid, hv, lo, hi)
            call mm_first_gather_i32(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_sc_i32

    module procedure mm_fmany_sc_i64
        type(ix_str_view) :: v
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        call ix_str_open_view(keys, v)
        n = mm_str_rows(self, v%n, size(rows, kind=int64), "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_col_i64(self%map, keys, v, rows, valid, hv, lo, hi)
            call mm_first_gather_i64(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_sc_i64

    module procedure mm_many_s1_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_str_rows(self, size(keys, kind=int64), size(groups, kind=int64), "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_chr_i32(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int32, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_s1_i32

    module procedure mm_many_s1_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_str_rows(self, size(keys, kind=int64), size(groups, kind=int64), "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_chr_i64(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int64, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_s1_i64

    module procedure mm_many_sc_i32
        type(ix_str_view) :: v
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        call ix_str_open_view(keys, v)
        n = mm_str_rows(self, v%n, size(groups, kind=int64), "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_col_i32(self%map, keys, v, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int32, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_sc_i32

    module procedure mm_many_sc_i64
        type(ix_str_view) :: v
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        call ix_str_open_view(keys, v)
        n = mm_str_rows(self, v%n, size(groups, kind=int64), "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_col_i64(self%map, keys, v, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int64, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_sc_i64

    module procedure mm_probe_s1_i32
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, kind=int64)
        call mm_str_check_probe(self, "probe_many")
        call mm_check_int32_values(self, "probe_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "probe_many")
        hg = present(group_hit)
        nt = mm_lookup_threads(n, threads, "probe_many")
        allocate(g(n), offsets(n + 1_int64), base(nt + 1))
        call mm_probe_hit_buffer(self, hg, hitbuf)
        nm = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nm) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_chr_i64(self%map, keys, g, valid, hv, lo, hi)
            call mm_probe_count(self, g, offsets, lo, hi, hg, hitbuf, base(c + 1), nm)
        end do
        call mm_probe_scan(nt, base, total)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call mm_probe_offsets(offsets, lo, hi, base(c))
        end do
        offsets(1) = 1_int64
        allocate(matches(total))
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call mm_probe_copy_i32(self, g, offsets, matches, lo, hi)
        end do
        if (hg) call move_alloc(hitbuf, group_hit)
        if (present(n_matched)) n_matched = nm
    end procedure mm_probe_s1_i32

    module procedure mm_probe_s1_i64
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, kind=int64)
        call mm_str_check_probe(self, "probe_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "probe_many")
        hg = present(group_hit)
        nt = mm_lookup_threads(n, threads, "probe_many")
        allocate(g(n), offsets(n + 1_int64), base(nt + 1))
        call mm_probe_hit_buffer(self, hg, hitbuf)
        nm = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nm) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_chr_i64(self%map, keys, g, valid, hv, lo, hi)
            call mm_probe_count(self, g, offsets, lo, hi, hg, hitbuf, base(c + 1), nm)
        end do
        call mm_probe_scan(nt, base, total)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call mm_probe_offsets(offsets, lo, hi, base(c))
        end do
        offsets(1) = 1_int64
        allocate(matches(total))
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call mm_probe_copy_i64(self, g, offsets, matches, lo, hi)
        end do
        if (hg) call move_alloc(hitbuf, group_hit)
        if (present(n_matched)) n_matched = nm
    end procedure mm_probe_s1_i64

    module procedure mm_probe_sc_i32
        type(ix_str_view) :: v
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        call ix_str_open_view(keys, v)
        n = v%n
        call mm_str_check_probe(self, "probe_many")
        call mm_check_int32_values(self, "probe_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "probe_many")
        hg = present(group_hit)
        nt = mm_lookup_threads(n, threads, "probe_many")
        allocate(g(n), offsets(n + 1_int64), base(nt + 1))
        call mm_probe_hit_buffer(self, hg, hitbuf)
        nm = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nm) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_col_i64(self%map, keys, v, g, valid, hv, lo, hi)
            call mm_probe_count(self, g, offsets, lo, hi, hg, hitbuf, base(c + 1), nm)
        end do
        call mm_probe_scan(nt, base, total)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call mm_probe_offsets(offsets, lo, hi, base(c))
        end do
        offsets(1) = 1_int64
        allocate(matches(total))
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call mm_probe_copy_i32(self, g, offsets, matches, lo, hi)
        end do
        if (hg) call move_alloc(hitbuf, group_hit)
        if (present(n_matched)) n_matched = nm
    end procedure mm_probe_sc_i32

    module procedure mm_probe_sc_i64
        type(ix_str_view) :: v
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        call ix_str_open_view(keys, v)
        n = v%n
        call mm_str_check_probe(self, "probe_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "probe_many")
        hg = present(group_hit)
        nt = mm_lookup_threads(n, threads, "probe_many")
        allocate(g(n), offsets(n + 1_int64), base(nt + 1))
        call mm_probe_hit_buffer(self, hg, hitbuf)
        nm = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nm) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_str_many_col_i64(self%map, keys, v, g, valid, hv, lo, hi)
            call mm_probe_count(self, g, offsets, lo, hi, hg, hitbuf, base(c + 1), nm)
        end do
        call mm_probe_scan(nt, base, total)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call mm_probe_offsets(offsets, lo, hi, base(c))
        end do
        offsets(1) = 1_int64
        allocate(matches(total))
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt) &
        !$omp     if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call mm_probe_copy_i64(self, g, offsets, matches, lo, hi)
        end do
        if (hg) call move_alloc(hitbuf, group_hit)
        if (present(n_matched)) n_matched = nm
    end procedure mm_probe_sc_i64

    module procedure mm_keys_s
        if (self%map%ncomp /= 0 .and. .not. self%map%is_str) call ix_abort(MM // &
            "keys: this multimap holds integer keys; ask for an integer list")
        call ix_str_collect(self%map, list)
    end procedure mm_keys_s

    ! ============================================================================================
    ! Private workers. Everything below takes a `type` dummy, and nothing below calls a public
    ! entry of the type whose guard it runs under.
    ! ============================================================================================

    ! ---- Guards ----

    !> Refuses a string LOOKUP on a map that holds integer keys; an unbuilt map passes, and
    !! answers 0. `pure`, so the message is static.
    pure subroutine ix_str_check_lookup(self, what)
        type(pf_index_map), intent(in) :: self !! the map.
        character(len=*), intent(in) :: what   !! procedure name, for the message.

        if (self%ncomp /= 0 .and. .not. self%is_str) error stop SP // what // &
            ": this map holds integer keys; look up with an integer key"
    end subroutine ix_str_check_lookup

    !> Refuses a string MUTATION of a map that holds integer keys; an unbuilt map passes, and
    !! becomes a string map. Impure, being guard-only (CLAUDE.md's ifx note).
    subroutine ix_str_check_mutate(self, what)
        type(pf_index_map), intent(in) :: self !! the map.
        character(len=*), intent(in) :: what   !! procedure name, for the message.

        if (self%ncomp /= 0 .and. .not. self%is_str) call ix_abort(SP // what // &
            ": this map holds integer keys; use an integer key")
    end subroutine ix_str_check_mutate

    !> Validates a string bulk call's shapes and returns the row count to walk. `ix_many_rows`'s
    !! string twin: the answer array's length and the map's key kind, an unbuilt map passing.
    pure function ix_str_many_rows(self, nrows, nidx, what) result(n)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: nrows    !! keys presented.
        integer(int64), intent(in) :: nidx     !! length of the answer array.
        character(len=*), intent(in) :: what   !! procedure name, for the message.
        integer(int64) :: n                    !! rows to walk.

        if (nrows /= nidx) error stop SP // what // &
            ": the keys and the answer array must have the same length"
        if (self%ncomp /= 0 .and. .not. self%is_str) error stop SP // what // &
            ": this map holds integer keys; look up with integer keys"
        n = nrows
    end function ix_str_many_rows

    !> Resolves `method=` for a string build: "auto" and "hash" only, the two other backends
    !! refused by name -- a string map IS the hash table, and the direct and sorted backends
    !! have no meaning for a key that is hashed before it is stored.
    subroutine ix_str_resolve_method(method, what, owner)
        character(len=*), intent(in), optional :: method !! the caller's token, or absent.
        character(len=*), intent(in) :: what             !! procedure name, for the message.
        character(len=*), intent(in), optional :: owner  !! type prefix; `pf_index_map%` by default.
        character(len=:), allocatable :: pfx
        integer :: want

        call ix_resolve_method(method, want, .true., what, owner)
        if (want == IX_DIRECT .or. want == IX_SORTED) then
            call ix_owner_of(owner, pfx)
            call ix_abort(pfx // what // ": a string-keyed map is always hashed; method= must be " // &
                """auto"" or ""hash""")
        end if
    end subroutine ix_str_resolve_method

    !> Ensures a map reached by a string `%set`/`%get_or_add` before any build is a string map.
    subroutine ix_str_autoinit(self)
        type(pf_index_map), intent(inout) :: self !! the map.

        if (self%ncomp == 0) call ix_str_start(self, 0_int64)
    end subroutine ix_str_autoinit

    ! ---- The store ----

    !> Whether stored string `p` is exactly the bytes `b(1:n)`.
    pure function ix_str_equal(self, p, b, n) result(eq)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: p        !! a store position.
        integer(int64), intent(in) :: n        !! bytes in the query.
        character(len=1), intent(in) :: b(n)   !! the query.
        logical :: eq                          !! `.true.` when the bytes match, length included.
        integer(int64) :: a, k

        eq = .false.
        a = self%soff(p)
        if (self%soff(p + 1_int64) - a /= n) return
        do k = 1_int64, n
            if (self%sdat(a + k) /= b(k)) return
        end do
        eq = .true.
    end function ix_str_equal

    !> The store position of the string `b(1:n)`, or 0 when it is absent: the occurrence walk.
    !!
    !! An unbuilt map has no table and answers 0 here, which is what lets every string lookup
    !! answer 0 on such a map without a branch of its own.
    pure function ix_str_find(self, b, n) result(p)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: n        !! bytes in the key.
        character(len=1), intent(in) :: b(n)   !! the key.
        integer(int64) :: p                    !! its store position, or 0.
        integer(int64) :: kb(2)

        p = 0_int64
        if (self%hcap <= 0_int64) return
        kb(1) = ix_hash_str(b, n)
        kb(2) = 0_int64
        do
            p = ix_hash_find_tuple(self, kb)
            if (p == 0_int64) return
            if (ix_str_equal(self, p, b, n)) return
            kb(2) = kb(2) + 1_int64
        end do
    end function ix_str_find

    !> The caller's value for the string `b(1:n)`, or 0 when it is absent.
    pure function ix_str_value(self, b, n) result(v)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: n        !! bytes in the key.
        character(len=1), intent(in) :: b(n)   !! the key.
        integer(int64) :: v                    !! the stored value, or 0.
        integer(int64) :: p

        v = 0_int64
        p = ix_str_find(self, b, n)
        if (p > 0_int64) v = self%sval(p)
    end function ix_str_value

    !> Appends the bytes `b(1:n)` with `value` to the store and returns the new position.
    subroutine ix_str_append(self, b, n, value, p)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: n           !! bytes in the string.
        character(len=1), intent(in) :: b(n)      !! the string.
        integer(int64), intent(in) :: value       !! the caller's value for it.
        integer(int64), intent(out) :: p          !! its store position.

        call ix_str_grow_rows(self, self%nstr + 1_int64)
        if (n > 0_int64) then
            call ix_str_grow_bytes(self, self%nchr + n)
            self%sdat(self%nchr + 1_int64 : self%nchr + n) = b(1:n)
        end if
        p = self%nstr + 1_int64
        self%nchr = self%nchr + n
        self%soff(p + 1_int64) = self%nchr
        self%sval(p) = value
        self%nstr = p
    end subroutine ix_str_append

    !> Inserts `b(1:n)` under `value` -- or, when it is stored already, replaces its value if
    !! `replace` and reports it as not new. The map's guard is held by the caller.
    !!
    !! One walk down the occurrence chain serves both the lookup and the insert: a miss at
    !! `(hash, k)` is exactly the tuple the new string is stored under.
    !!
    !! **Every string that lengthens a chain comes through here**: the threaded build and the
    !! threaded `%get_or_add_many` fall back to the serial loops, which call this, the moment two
    !! keys share a hash. So this is where a chain reaching `IX_STR_CHAIN_WARN` is noticed, once
    !! per map. Nothing is refused: the answers stay right, and only the time they take grows.
    subroutine ix_str_insert(self, b, n, value, replace, p, is_new, what)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: n           !! bytes in the key.
        character(len=1), intent(in) :: b(n)      !! the key.
        integer(int64), intent(in) :: value       !! the value to store.
        logical, intent(in) :: replace            !! whether a stored key's value is overwritten.
        integer(int64), intent(out) :: p          !! the key's store position.
        logical, intent(out) :: is_new            !! `.true.` when the key was not already stored.
        character(len=*), intent(in) :: what      !! the public procedure, for the warning.
        integer(int64) :: kb(2)
        logical :: was_new
        character(len=24) :: depth

        kb(1) = ix_hash_str(b, n)
        kb(2) = 0_int64
        do
            p = ix_hash_find_tuple(self, kb)
            if (p == 0_int64) exit
            if (ix_str_equal(self, p, b, n)) then
                if (replace) self%sval(p) = value
                is_new = .false.
                return
            end if
            kb(2) = kb(2) + 1_int64
        end do
        call ix_str_append(self, b, n, value, p)
        call ix_hash_insert(self, kb, p, was_new)
        is_new = .true.
        if (kb(2) + 1_int64 >= IX_STR_CHAIN_WARN .and. .not. self%sdeep) then
            self%sdeep = .true.
            write (depth, "(i0)") kb(2) + 1_int64
            call parquet_emit_warning(SP // what // ": " // trim(depth) // " string keys share one " // &
                "64-bit hash, so a lookup of the last of them compares every one before it; keys this " // &
                "alike are almost certainly constructed, and inserting or finding them costs time " // &
                "growing with their number (%probe_stats reports the longest such run as max_hash_chain)")
        end if
    end subroutine ix_str_insert

    !> Removes the string `b(1:n)`: its tuple leaves the table with the chain kept dense, and its
    !! bytes stay in the store with `sval = 0`.
    subroutine ix_str_remove(self, b, n, hit)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: n           !! bytes in the key.
        character(len=1), intent(in) :: b(n)      !! the key.
        logical, intent(out) :: hit               !! `.true.` when the key was there.
        integer(int64) :: kb(2), kl(2), kn(2), p, plast
        logical :: f

        hit = .false.
        if (self%hcap <= 0_int64) return
        kb(1) = ix_hash_str(b, n)
        kb(2) = 0_int64
        do
            p = ix_hash_find_tuple(self, kb)
            if (p == 0_int64) return
            if (ix_str_equal(self, p, b, n)) exit
            kb(2) = kb(2) + 1_int64
        end do
        ! The chain's last occurrence: probe upward from the removed one until the first miss.
        kl = kb
        kn(1) = kb(1)
        do
            kn(2) = kl(2) + 1_int64
            if (ix_hash_find_tuple(self, kn) == 0_int64) exit
            kl = kn
        end do
        if (kl(2) > kb(2)) then
            ! Not the last of its chain: the last entry takes the removed one's slot -- a value
            ! replacement, so the key count is untouched -- and the last tuple is the one deleted,
            ! so no occurrence number is skipped.
            plast = ix_hash_find_tuple(self, kl)
            call ix_hash_insert(self, kb, plast, f)
            call ix_hash_remove(self, kl, f)
        else
            call ix_hash_remove(self, kb, f)
        end if
        self%sval(p) = 0_int64
        hit = .true.
    end subroutine ix_str_remove

    !> Names a duplicate string key a build met: the key, quoted, and its position.
    subroutine ix_str_report_duplicate(b, n, at)
        integer(int64), intent(in) :: n      !! bytes in the key.
        character(len=1), intent(in) :: b(n) !! the offending key.
        integer(int64), intent(in) :: at     !! its position in the caller's array.
        character(len=:), allocatable :: txt
        character(len=32) :: pos

        call ix_str_text(b, n, txt)
        write (pos, "(i0)") at
        call ix_abort(SP // "build: duplicate key """ // txt // """ at position " // trim(pos) // &
            " (every key must be unique)")
    end subroutine ix_str_report_duplicate    ! GCOVR_EXCL_LINE -- `ix_abort` never returns

    !> Copies bytes into a scalar string, for a message. Cold path only.
    subroutine ix_str_text(b, n, out)
        integer(int64), intent(in) :: n                   !! bytes to copy.
        character(len=1), intent(in) :: b(n)              !! the bytes.
        character(len=:), allocatable, intent(out) :: out !! receives them as one string.
        integer(int64) :: k

        allocate(character(len=n) :: out)
        do k = 1_int64, n
            out(k:k) = b(k)
        end do
    end subroutine ix_str_text

    ! ---- A string column, aliased ----

    !> Aliases a parquet_string_column's buffers for the duration of the caller.
    subroutine ix_str_open_view(sc, v)
        type(parquet_string_column), intent(in), target :: sc !! the column.
        type(ix_str_view), intent(out) :: v                   !! receives the aliases.
        type(c_ptr) :: po, pd, pv
        integer(int64) :: nchars

        call parquet_string_column_raw_buffers(sc, po, pd, pv, v%n, nchars, v%nulls)
        if (c_associated(po)) call c_f_pointer(po, v%off, [v%n + 1_int64])
        if (c_associated(pd)) call c_f_pointer(pd, v%dat, [nchars])
    end subroutine ix_str_open_view

    !> The caller's value for element `i` of a viewed column, or 0 when it is null or absent.
    function ix_str_value_row(self, sc, v, i) result(val)
        type(pf_index_map), intent(in) :: self             !! the map.
        type(parquet_string_column), intent(in) :: sc      !! the column `v` views.
        type(ix_str_view), intent(in) :: v                 !! its aliases.
        integer(int64), intent(in) :: i                    !! the element.
        integer(int64) :: val                              !! the stored value, or 0.
        integer(int64) :: a, nb

        val = 0_int64
        if (v%nulls) then
            if (parquet_string_column_is_null(sc, i)) return
        end if
        a = v%off(i)
        nb = v%off(i + 1_int64) - a
        if (nb > 0_int64) then
            val = ix_str_value(self, v%dat(a + 1_int64 : a + nb), nb)
        else
            val = ix_str_value(self, IX_NO_BYTES, 0_int64)
        end if
    end function ix_str_value_row

    ! ---- The bulk lookup's chunk loops, one per key source x answer kind ----

    !> The serial `%get_many` loop over rows `lo .. hi` of a character array, `int32` answers.
    !! `indexes` is `intent(inout)` for the reason `ix_many_1_k32_i32` gives.
    subroutine ix_str_many_chr_i32(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        character(len=*), intent(in) :: keys(:)      !! the caller's keys.
        integer(int32), intent(inout) :: indexes(:)  !! receives the answers for `lo .. hi`.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: i

        do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
            indexes(i) = ix_narrow(ix_str_value(self, keys(i), len_trim(keys(i), kind=int64)))
        end do
    end subroutine ix_str_many_chr_i32

    !> The serial `%get_many` loop over rows `lo .. hi` of a character array, `int64` answers.
    subroutine ix_str_many_chr_i64(self, keys, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        character(len=*), intent(in) :: keys(:)      !! the caller's keys.
        integer(int64), intent(inout) :: indexes(:)  !! receives the answers for `lo .. hi`.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: i

        do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
            indexes(i) = ix_str_value(self, keys(i), len_trim(keys(i), kind=int64))
        end do
    end subroutine ix_str_many_chr_i64

    !> The serial `%get_many` loop over rows `lo .. hi` of a viewed column, `int32` answers.
    subroutine ix_str_many_col_i32(self, sc, v, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        type(parquet_string_column), intent(in) :: sc !! the caller's keys.
        type(ix_str_view), intent(in) :: v           !! their aliases.
        integer(int32), intent(inout) :: indexes(:)  !! receives the answers for `lo .. hi`.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: i

        do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int32
                    cycle
                end if
            end if
            indexes(i) = ix_narrow(ix_str_value_row(self, sc, v, i))
        end do
    end subroutine ix_str_many_col_i32

    !> The serial `%get_many` loop over rows `lo .. hi` of a viewed column, `int64` answers.
    subroutine ix_str_many_col_i64(self, sc, v, indexes, valid, hv, lo, hi)
        type(pf_index_map), intent(in) :: self       !! the map.
        type(parquet_string_column), intent(in) :: sc !! the caller's keys.
        type(ix_str_view), intent(in) :: v           !! their aliases.
        integer(int64), intent(inout) :: indexes(:)  !! receives the answers for `lo .. hi`.
        logical, intent(in), optional :: valid(:)    !! the caller's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! first row this call owns.
        integer(int64), intent(in) :: hi             !! last row this call owns.
        integer(int64) :: i

        do i = lo, hi
            if (hv) then
                if (.not. valid(i)) then
                    indexes(i) = 0_int64
                    cycle
                end if
            end if
            indexes(i) = ix_str_value_row(self, sc, v, i)
        end do
    end subroutine ix_str_many_col_i64

    ! ---- Mutation workers. Every one runs with the map's guard held. ----

    !> `%set` for a string key.
    subroutine ix_str_set(self, key, value)
        type(pf_index_map), intent(inout) :: self !! the map.
        character(len=*), intent(in) :: key       !! the key, as written.
        integer(int64), intent(in) :: value       !! the value to store.
        integer(int64) :: p
        logical :: is_new

        call ix_str_check_mutate(self, "set")
        call ix_check_value(value, "set")
        call ix_str_autoinit(self)
        call ix_str_insert(self, key, len(key, kind=int64), value, .true., p, is_new, "set")
        if (value > self%snext) self%snext = value
    end subroutine ix_str_set

    !> `%get_or_add` for a string key, from its scalar entry.
    subroutine ix_str_goa_entry(self, key, idx, want32)
        type(pf_index_map), intent(inout) :: self !! the map.
        character(len=*), intent(in) :: key       !! the key, as written.
        integer(int64), intent(out) :: idx        !! the key's index.
        logical, intent(in) :: want32             !! whether the answer must fit `int32`.

        call ix_str_check_mutate(self, "get_or_add")
        call ix_str_autoinit(self)
        call ix_str_goa(self, key, len(key, kind=int64), idx, want32, "get_or_add")
    end subroutine ix_str_goa_entry

    !> `%get_or_add` for the bytes `b(1:n)`: one chain walk, inserting the next watermark value
    !! when the key is new.
    subroutine ix_str_goa(self, b, n, idx, want32, what)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: n           !! bytes in the key.
        character(len=1), intent(in) :: b(n)      !! the key.
        integer(int64), intent(out) :: idx        !! the key's index.
        logical, intent(in) :: want32             !! whether the answer must fit `int32`.
        character(len=*), intent(in) :: what      !! the public procedure, for the warning.
        integer(int64) :: p
        logical :: is_new

        call ix_str_insert(self, b, n, self%snext + 1_int64, .false., p, is_new, what)
        if (is_new) self%snext = self%snext + 1_int64
        idx = self%sval(p)
        if (want32) idx = ix_narrow_check(idx)
    end subroutine ix_str_goa

    !> `%get_or_add_many` over a character array: `ix_str_goa` per row, each row trimmed, with
    !! the guard already held. See `ix_goam_1` for the contract. On a team of two or more an
    !! EMPTY map takes the partitioned pass (`ix_str_goam_fresh`), and a map already holding
    !! keys looks every row up on the team and adds the ones not found serially.
    subroutine ix_str_goam_chr(self, keys, codes, valid, want32, threads)
        type(pf_index_map), intent(inout) :: self !! the map.
        character(len=*), intent(in) :: keys(:)   !! the keys.
        integer(int64), intent(out) :: codes(:)   !! one code per key; 0 for a masked row.
        logical, intent(in), optional :: valid(:) !! the caller's mask, or absent.
        logical, intent(in) :: want32             !! whether every code must fit `int32`.
        integer, intent(in), optional :: threads  !! the caller's thread request, or absent.
        integer(int64) :: i, n, idx, lo, hi
        integer :: nt, c
        logical :: hv, ok

        call ix_str_check_mutate(self, "get_or_add_many")
        call ix_str_autoinit(self)
        n = ix_str_many_rows(self, size(keys, kind=int64), size(codes, kind=int64), "get_or_add_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_or_add_many")
        nt = ix_threads_for(n, threads, "get_or_add_many")
        if (nt > 1) then
            if (self%nk == 0_int64 .and. self%nstr == 0_int64) then
                call ix_str_goam_fresh(self, n, valid, nt, codes, ok, keys=keys)
                if (ok) then
                    if (want32) call ix_check_codes_fit(codes)
                    return
                end if
                call ix_do_reset(self)
            else
                !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt)
                do c = 1, nt
                    call ix_chunk_bounds(n, nt, c, lo, hi)
                    call ix_str_many_chr_i64(self, keys, codes, valid, hv, lo, hi)
                end do
                !$omp atomic write
                dbg_index_spills = -1_int64
                do i = 1_int64, n
                    if (codes(i) /= 0_int64) cycle
                    if (hv) then
                        if (.not. valid(i)) cycle
                    end if
                    call ix_str_goa(self, keys(i), len_trim(keys(i), kind=int64), idx, want32, "get_or_add_many")
                    codes(i) = idx
                end do
                return
            end if
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
            call ix_str_goa(self, keys(i), len_trim(keys(i), kind=int64), idx, want32, "get_or_add_many")
            codes(i) = idx
        end do
    end subroutine ix_str_goam_chr

    !> `%get_or_add_many` over a parquet_string_column: a null element gets the code 0 and is
    !! neither looked up nor added, exactly as a masked row. Threads as `ix_str_goam_chr` does.
    subroutine ix_str_goam_col(self, sc, codes, valid, want32, threads)
        type(pf_index_map), intent(inout) :: self             !! the map.
        type(parquet_string_column), intent(in), target :: sc !! the keys.
        integer(int64), intent(out) :: codes(:)               !! one code per element; 0 when masked or null.
        logical, intent(in), optional :: valid(:)             !! the caller's mask, or absent.
        logical, intent(in) :: want32                         !! whether every code must fit `int32`.
        integer, intent(in), optional :: threads              !! the caller's thread request, or absent.
        type(ix_str_view) :: v
        logical, allocatable :: eff(:)
        integer(int64) :: i, n, idx, a, nb, lo, hi
        integer :: nt, c
        logical :: hv, ok

        call ix_str_check_mutate(self, "get_or_add_many")
        call ix_str_autoinit(self)
        call ix_str_open_view(sc, v)
        n = ix_str_many_rows(self, v%n, size(codes, kind=int64), "get_or_add_many")
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "get_or_add_many")
        nt = ix_threads_for(n, threads, "get_or_add_many")
        if (nt > 1) then
            if (self%nk == 0_int64 .and. self%nstr == 0_int64) then
                call ix_str_effective_mask(sc, v, valid, eff)
                call ix_str_goam_fresh(self, n, eff, nt, codes, ok, v=v)
                if (ok) then
                    if (want32) call ix_check_codes_fit(codes)
                    return
                end if
                call ix_do_reset(self)
            else
                !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt)
                do c = 1, nt
                    call ix_chunk_bounds(n, nt, c, lo, hi)
                    call ix_str_many_col_i64(self, sc, v, codes, valid, hv, lo, hi)
                end do
                !$omp atomic write
                dbg_index_spills = -1_int64
                do i = 1_int64, n
                    if (codes(i) /= 0_int64) cycle
                    if (hv) then
                        if (.not. valid(i)) cycle
                    end if
                    if (v%nulls) then
                        if (parquet_string_column_is_null(sc, i)) cycle
                    end if
                    a = v%off(i)
                    nb = v%off(i + 1_int64) - a
                    if (nb > 0_int64) then
                        call ix_str_goa(self, v%dat(a + 1_int64 : a + nb), nb, idx, want32, "get_or_add_many")
                    else
                        call ix_str_goa(self, IX_NO_BYTES, 0_int64, idx, want32, "get_or_add_many")
                    end if
                    codes(i) = idx
                end do
                return
            end if
        end if
        !$omp atomic write
        dbg_index_spills = -1_int64
        do i = 1_int64, n
            codes(i) = 0_int64
            if (hv) then
                if (.not. valid(i)) cycle
            end if
            if (v%nulls) then
                if (parquet_string_column_is_null(sc, i)) cycle
            end if
            a = v%off(i)
            nb = v%off(i + 1_int64) - a
            if (nb > 0_int64) then
                call ix_str_goa(self, v%dat(a + 1_int64 : a + nb), nb, idx, want32, "get_or_add_many")
            else
                call ix_str_goa(self, IX_NO_BYTES, 0_int64, idx, want32, "get_or_add_many")
            end if
            codes(i) = idx
        end do
    end subroutine ix_str_goam_col

    !> `%remove` for a string key, with the guard held; the absent-key abort happens here.
    subroutine ix_str_rm(self, key, hit, has_found)
        type(pf_index_map), intent(inout) :: self !! the map.
        character(len=*), intent(in) :: key       !! the key, as written.
        logical, intent(out) :: hit               !! `.true.` when the key was there.
        logical, intent(in) :: has_found          !! whether the caller passed `found=`.

        call ix_str_check_mutate(self, "remove")
        hit = .false.
        if (self%is_str) call ix_str_remove(self, key, len(key, kind=int64), hit)
        call ix_removal_report(hit, has_found)
    end subroutine ix_str_rm

    ! ---- The builds ----

    !> Builds a string map from a character array, each element trimmed: `ix_build_1`'s contract
    !! on the hash backend. On a team of two or more the strings are hashed and copied into the
    !! store on the team and their tuples inserted by the partitioned pass (`ix_str_build_part`);
    !! otherwise, or when that pass meets equal tuples, the serial insert loop runs.
    subroutine ix_str_build_chr(self, keys, values, method, threads, valid)
        type(pf_index_map), intent(inout) :: self         !! the map; rebuilt from scratch.
        character(len=*), intent(in) :: keys(:)           !! the keys; must be unique where unmasked.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for the row numbers.
        character(len=*), intent(in), optional :: method  !! backend token; auto or hash only.
        integer, intent(in), optional :: threads          !! the caller's thread request.
        logical, intent(in), optional :: valid(:)         !! per row; `.false.` skips the row.
        integer(int64) :: n, nv, first, last
        integer :: nt
        logical :: hv, has_v, dup

        call ix_build_begin()
        n = size(keys, kind=int64)
        hv = present(valid)
        if (hv) call ix_check_mask_len(size(valid, kind=int64), n, "build")
        has_v = present(values)
        if (has_v) then
            call ix_check_values_len(size(values, kind=int64), n)
            call ix_check_values_range(values, valid)
        end if
        call ix_str_resolve_method(method, "build")
        nt = ix_threads_for(n, threads)
        if (nt < 1) call ix_abort(SP // "build: the thread rule answered below 1")
        call ix_reset_storage(self)
        call ix_mask_extent(n, valid, nv, first, last)
        ! On a team the tuple table is allocated by the partitioned pass itself, zeroed on the
        ! team; the store's rows and payload are grown there too.
        if (nt > 1) then
            call ix_str_start(self, 0_int64)
        else
            call ix_str_start(self, nv)
        end if
        if (nv == 0_int64) return
        call ix_str_set_watermark(self, values, valid, last)
        if (nt > 1) then
            call ix_str_build_part(self, n, nv, valid, values, nt, dup, keys=keys)
            if (.not. dup) return
            ! Equal tuples: a repeated key, or two keys sharing a hash. The serial loop, over a
            ! freshly started map, walks the occurrence chain and names a real duplicate.
            call ix_reset_storage(self)
            call ix_str_start(self, nv)
            call ix_str_set_watermark(self, values, valid, last)
        end if
        call ix_str_insert_chr(self, keys, values, valid)
    end subroutine ix_str_build_chr

    !> Builds a string map from a parquet_string_column, verbatim, a null element skipped as a
    !! masked row is. See `ix_str_build_chr`.
    subroutine ix_str_build_col(self, sc, values, method, threads, valid)
        type(pf_index_map), intent(inout) :: self             !! the map; rebuilt from scratch.
        type(parquet_string_column), intent(in), target :: sc !! the keys; unique where unmasked and non-null.
        integer(int64), intent(in), optional :: values(:)     !! values, or absent for the row numbers.
        character(len=*), intent(in), optional :: method      !! backend token; auto or hash only.
        integer, intent(in), optional :: threads              !! the caller's thread request.
        logical, intent(in), optional :: valid(:)             !! per row; `.false.` skips the row.
        type(ix_str_view) :: v
        logical, allocatable :: eff(:)
        integer(int64) :: n, nv, first, last
        integer :: nt
        logical :: has_v, dup

        call ix_build_begin()
        call ix_str_open_view(sc, v)
        n = v%n
        if (present(valid)) call ix_check_mask_len(size(valid, kind=int64), n, "build")
        call ix_str_effective_mask(sc, v, valid, eff)
        has_v = present(values)
        if (has_v) then
            call ix_check_values_len(size(values, kind=int64), n)
            call ix_check_values_range(values, eff)
        end if
        call ix_str_resolve_method(method, "build")
        nt = ix_threads_for(n, threads)
        if (nt < 1) call ix_abort(SP // "build: the thread rule answered below 1")
        call ix_reset_storage(self)
        call ix_mask_extent(n, eff, nv, first, last)
        if (nt > 1) then
            call ix_str_start(self, 0_int64)
        else
            call ix_str_start(self, nv)
        end if
        if (nv == 0_int64) return
        call ix_str_set_watermark(self, values, eff, last)
        if (nt > 1) then
            call ix_str_build_part(self, n, nv, eff, values, nt, dup, v=v)
            if (.not. dup) return
            call ix_reset_storage(self)
            call ix_str_start(self, nv)
            call ix_str_set_watermark(self, values, eff, last)
        end if
        call ix_str_insert_col(self, v, values, eff)
    end subroutine ix_str_build_col

    !> Sets the `%get_or_add` watermark a build leaves: the largest value it stores.
    subroutine ix_str_set_watermark(self, values, valid, last)
        type(pf_index_map), intent(inout) :: self         !! the map.
        integer(int64), intent(in), optional :: values(:) !! the build's values, or absent.
        logical, intent(in), optional :: valid(:)         !! its effective mask, or absent.
        integer(int64), intent(in) :: last                !! the last stored row: the default values' maximum.

        if (present(values)) then
            if (present(valid)) then
                self%snext = maxval(values, mask=valid)
            else
                self%snext = maxval(values)
            end if
        else
            self%snext = last
        end if
    end subroutine ix_str_set_watermark

    !> The serial insert loop of a build from a character array: every unmasked element through
    !! `ix_str_insert`, a repeated key named at its position. Marks the spill count -1, the
    !! serial loop's mark, whether it runs as the serial arm or as the fallback of the threaded one.
    subroutine ix_str_insert_chr(self, keys, values, valid)
        type(pf_index_map), intent(inout) :: self         !! the map, started and empty.
        character(len=*), intent(in) :: keys(:)           !! the keys.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for the row numbers.
        logical, intent(in), optional :: valid(:)         !! the mask, or absent.
        integer(int64) :: i, nb, p, val
        logical :: hv, has_v, is_new

        !$omp atomic write
        dbg_index_spills = -1_int64
        hv = present(valid)
        has_v = present(values)
        do i = 1_int64, size(keys, kind=int64)
            if (hv) then
                if (.not. valid(i)) cycle
            end if
            nb = len_trim(keys(i), kind=int64)
            val = i
            if (has_v) val = values(i)
            call ix_str_insert(self, keys(i), nb, val, .false., p, is_new, "build")
            if (.not. is_new) call ix_str_report_duplicate(keys(i), nb, i)
        end do
    end subroutine ix_str_insert_chr

    !> The serial insert loop of a build from a viewed column. See `ix_str_insert_chr`.
    subroutine ix_str_insert_col(self, v, values, valid)
        type(pf_index_map), intent(inout) :: self         !! the map, started and empty.
        type(ix_str_view), intent(in) :: v                !! the column's aliases.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for the row numbers.
        logical, intent(in), optional :: valid(:)         !! the effective mask, or absent.
        integer(int64) :: i, a, nb, p, val
        logical :: hv, has_v, is_new

        !$omp atomic write
        dbg_index_spills = -1_int64
        hv = present(valid)
        has_v = present(values)
        do i = 1_int64, v%n
            if (hv) then
                if (.not. valid(i)) cycle
            end if
            a = v%off(i)
            nb = v%off(i + 1_int64) - a
            val = i
            if (has_v) val = values(i)
            if (nb > 0_int64) then
                call ix_str_insert(self, v%dat(a + 1_int64 : a + nb), nb, val, .false., p, is_new, "build")
                if (.not. is_new) call ix_str_report_duplicate(v%dat(a + 1_int64 : a + nb), nb, i)
            else
                call ix_str_insert(self, IX_NO_BYTES, 0_int64, val, .false., p, is_new, "build")
                if (.not. is_new) call ix_str_report_duplicate(IX_NO_BYTES, 0_int64, i)
            end if
        end do
    end subroutine ix_str_insert_col

    ! ---- The threaded arm: the string hash on the team, the tuples through the partitioned insert ----

    !> The bytes of row `i` of the build's or grouping's source, as a length.
    pure function ix_str_row_len(i, keys, v) result(nb)
        integer(int64), intent(in) :: i                        !! the row.
        character(len=*), intent(in), optional :: keys(:)      !! the source as a character array, or
        type(ix_str_view), intent(in), optional :: v           !! as a viewed column.
        integer(int64) :: nb                                   !! bytes in the key.

        if (present(keys)) then
            nb = len_trim(keys(i), kind=int64)
        else
            nb = v%off(i + 1_int64) - v%off(i)
        end if
    end function ix_str_row_len

    !> Whether rows `i` and `j` of the source hold the same bytes.
    pure function ix_str_rows_equal(i, j, keys, v) result(eq)
        integer(int64), intent(in) :: i                        !! one row.
        integer(int64), intent(in) :: j                        !! the other.
        character(len=*), intent(in), optional :: keys(:)      !! the source as a character array, or
        type(ix_str_view), intent(in), optional :: v           !! as a viewed column.
        logical :: eq                                          !! `.true.` when the bytes match, length included.
        integer(int64) :: ni, nj, ai, aj, k

        eq = .false.
        if (present(keys)) then
            ni = len_trim(keys(i), kind=int64)
            nj = len_trim(keys(j), kind=int64)
            if (ni /= nj) return
            eq = keys(i)(1:ni) == keys(j)(1:nj)
        else
            ai = v%off(i)
            aj = v%off(j)
            ni = v%off(i + 1_int64) - ai
            nj = v%off(j + 1_int64) - aj
            if (ni /= nj) return
            do k = 1_int64, ni
                if (v%dat(ai + k) /= v%dat(aj + k)) return
            end do
            eq = .true.
        end if
    end function ix_str_rows_equal

    !> Copies the bytes `b(1:n)` into the store at `cur + 1 .. cur + n`; takes a scalar string
    !! by sequence association, as `ix_hash_str` does.
    subroutine ix_str_put(sdat, cur, b, n)
        character(len=1), intent(inout) :: sdat(:) !! the store's payload.
        integer(int64), intent(in) :: cur          !! bytes already in use before this string.
        integer(int64), intent(in) :: n            !! bytes to copy.
        character(len=1), intent(in) :: b(n)       !! the string.

        sdat(cur + 1_int64 : cur + n) = b(1:n)
    end subroutine ix_str_put

    !> The threaded arm of a string build: the string hash of every row on the team, the store
    !! laid out in ROW order and filled on the team, and the `(hash, 0)` tuples inserted by the
    !! partitioned pass the integer builds use (`ix_hash_build_part_n`), their values the store
    !! positions.
    !!
    !! **Positions are row numbers.** A masked row (or a null element) keeps its position as a
    !! zero-length string with `sval = 0`, exactly the shape a removed string has, so that the
    !! store is laid out from per-chunk byte counts alone -- no compaction pass and no per-row
    !! position array -- and `%keys` and the key count skip such rows as they skip a removed one.
    !! Two distinct strings sharing a 64-bit hash would be two equal TUPLES here, which the
    !! partitioned pass reports as a duplicate, exactly as it reports two copies of one string;
    !! either way `dup` comes back `.true.` and the caller falls back to the serial insert loop,
    !! which walks the occurrence chain and names a real duplicate at its position. That fallback
    !! is the whole of the collision handling on this path, and the debug hook that narrows the
    !! string hash is what exercises it.
    subroutine ix_str_build_part(self, n, nv, valid, values, nt, dup, keys, v)
        type(pf_index_map), intent(inout) :: self             !! the map, started by `ix_str_start`.
        integer(int64), intent(in) :: n                       !! rows presented.
        integer(int64), intent(in) :: nv                      !! rows the mask keeps; sizes the table.
        logical, intent(in), optional :: valid(:)             !! the effective mask, or absent.
        integer(int64), intent(in), optional :: values(:)     !! values, or absent for the row numbers.
        integer, intent(in) :: nt                             !! the resolved team; at least 2.
        logical, intent(out) :: dup                           !! `.true.` when the tuple pass met equal tuples.
        character(len=*), intent(in), optional :: keys(:)     !! the keys as a character array, or
        type(ix_str_view), intent(in), optional :: v          !! as a viewed column.
        integer(int64), allocatable :: hs(:), cb(:), tk(:,:)
        integer(int64) :: i, lo, hi, a, nb, cur, total, val
        integer :: c
        logical :: hv, has_v, chr

        hv = present(valid)
        has_v = present(values)
        chr = present(keys)
        allocate(hs(n), cb(nt))
        !$omp parallel do default(shared) private(c, lo, hi, i, a, nb, cur) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            cur = 0_int64
            do i = lo, hi
                hs(i) = 0_int64
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                if (chr) then
                    nb = len_trim(keys(i), kind=int64)
                    hs(i) = ix_hash_str(keys(i), nb)
                else
                    a = v%off(i)
                    nb = v%off(i + 1_int64) - a
                    if (nb > 0_int64) then
                        hs(i) = ix_hash_str(v%dat(a + 1_int64 : a + nb), nb)
                    else
                        hs(i) = ix_hash_str(IX_NO_BYTES, 0_int64)
                    end if
                end if
                cur = cur + nb
            end do
            cb(c) = cur
        end do
        ! Chunk c's bytes start after every earlier chunk's.
        total = 0_int64
        do c = 1, nt
            cur = cb(c)
            cb(c) = total
            total = total + cur
        end do
        call ix_str_grow_rows(self, n)
        call ix_str_grow_bytes(self, total)
        !$omp parallel do default(shared) private(c, lo, hi, i, a, nb, cur, val) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            cur = cb(c)
            do i = lo, hi
                self%sval(i) = 0_int64
                self%soff(i + 1_int64) = cur
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                val = i
                if (has_v) val = values(i)
                if (chr) then
                    nb = len_trim(keys(i), kind=int64)
                    if (nb > 0_int64) call ix_str_put(self%sdat, cur, keys(i), nb)
                else
                    a = v%off(i)
                    nb = v%off(i + 1_int64) - a
                    if (nb > 0_int64) self%sdat(cur + 1_int64 : cur + nb) = v%dat(a + 1_int64 : a + nb)
                end if
                cur = cur + nb
                self%soff(i + 1_int64) = cur
                self%sval(i) = val
            end do
        end do
        self%nstr = n
        self%nchr = total
        allocate(tk(n, 2))
        tk(:, 1) = hs
        tk(:, 2) = 0_int64
        deallocate(hs)
        call ix_hash_build_part_n(self, tk, valid=valid, nv=nv, nt=nt, dup=dup)
    end subroutine ix_str_build_part

    !> The threaded `%get_or_add_many` over an EMPTY string map: the string hash of every
    !! unmasked row on the team, the `(hash, 0)` tuples through the partitioned
    !! `ix_hash_goam_part_n`, which numbers the distinct tuples densely and returns each row's
    !! code, and then the store laid out in CODE order and filled on the team -- so the code of a
    !! string is its store position, as on the serial pass (what `mm_str_rightsize` relies on).
    !!
    !! Two distinct strings sharing a hash would share a code here, so every row is compared
    !! against the first row of its code before the store is touched; one mismatch makes `ok`
    !! false, and the caller empties the map and runs the serial pass instead. The tuple table
    !! is the map's own, so on success nothing is copied twice.
    subroutine ix_str_goam_fresh(self, n, valid, nt, codes, ok, keys, v)
        type(pf_index_map), intent(inout) :: self             !! the map; empty.
        integer(int64), intent(in) :: n                       !! rows presented.
        logical, intent(in), optional :: valid(:)             !! the effective mask, or absent.
        integer, intent(in) :: nt                             !! the resolved team; at least 2.
        integer(int64), intent(out) :: codes(:)               !! one code per row; 0 for a masked one.
        logical, intent(out) :: ok                            !! `.false.` when two strings shared a hash.
        character(len=*), intent(in), optional :: keys(:)     !! the keys as a character array, or
        type(ix_str_view), intent(in), optional :: v          !! as a viewed column.
        integer(int64), allocatable :: hs(:), tk(:,:), rowof(:)
        integer(int64) :: i, r, k, lo, hi, a, nb, bad, cur
        integer :: c
        logical :: hv, chr

        hv = present(valid)
        chr = present(keys)
        ok = .true.
        allocate(hs(n))
        !$omp parallel do default(shared) private(c, lo, hi, i, a, nb) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            do i = lo, hi
                hs(i) = 0_int64
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                if (chr) then
                    hs(i) = ix_hash_str(keys(i), len_trim(keys(i), kind=int64))
                else
                    a = v%off(i)
                    nb = v%off(i + 1_int64) - a
                    if (nb > 0_int64) then
                        hs(i) = ix_hash_str(v%dat(a + 1_int64 : a + nb), nb)
                    else
                        hs(i) = ix_hash_str(IX_NO_BYTES, 0_int64)
                    end if
                end if
            end do
        end do
        allocate(tk(n, 2))
        tk(:, 1) = hs
        tk(:, 2) = 0_int64
        deallocate(hs)
        call ix_hash_goam_part_n(self, tk, codes, valid, nt)
        deallocate(tk)
        k = self%nk
        if (k == 0_int64) return
        ! The first row of every code, in one serial scan: the row whose bytes the store takes.
        allocate(rowof(k))
        rowof = 0_int64
        do i = 1_int64, n
            r = codes(i)
            if (r <= 0_int64) cycle
            if (rowof(r) == 0_int64) rowof(r) = i
        end do
        bad = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi, i, r) reduction(+:bad) schedule(static) &
        !$omp     num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            do i = lo, hi
                r = codes(i)
                if (r <= 0_int64) cycle
                if (rowof(r) == i) cycle
                if (.not. ix_str_rows_equal(i, rowof(r), keys, v)) bad = bad + 1_int64
            end do
        end do
        if (bad > 0_int64) then
            ok = .false.
            return
        end if
        call ix_str_grow_rows(self, k)
        self%soff(1) = 0_int64
        do r = 1_int64, k
            self%soff(r + 1_int64) = self%soff(r) + ix_str_row_len(rowof(r), keys, v)
        end do
        call ix_str_grow_bytes(self, self%soff(k + 1_int64))
        !$omp parallel do default(shared) private(c, lo, hi, r, i, a, nb, cur) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(k, nt, c, lo, hi)
            do r = lo, hi
                i = rowof(r)
                cur = self%soff(r)
                nb = self%soff(r + 1_int64) - cur
                if (nb > 0_int64) then
                    if (chr) then
                        call ix_str_put(self%sdat, cur, keys(i), nb)
                    else
                        a = v%off(i)
                        self%sdat(cur + 1_int64 : cur + nb) = v%dat(a + 1_int64 : a + nb)
                    end if
                end if
                self%sval(r) = r
            end do
        end do
        self%nstr = k
        self%nchr = self%soff(k + 1_int64)
        self%snext = k
    end subroutine ix_str_goam_fresh

    !> The mask a string-column build or grouping actually applies: the caller's `valid=`, with
    !! every null element masked off too. Left UNALLOCATED when there is nothing to mask, which
    !! the map's helpers read as an absent mask, so the common case pays nothing.
    subroutine ix_str_effective_mask(sc, v, valid, eff)
        type(parquet_string_column), intent(in) :: sc      !! the column.
        type(ix_str_view), intent(in) :: v                 !! its aliases.
        logical, intent(in), optional :: valid(:)          !! the caller's mask, or absent.
        logical, allocatable, intent(out) :: eff(:)        !! receives the combined mask, or stays unallocated.
        integer(int64) :: i

        if (.not. (present(valid) .or. v%nulls)) return
        allocate(eff(v%n))
        eff = .true.
        if (present(valid)) eff = valid
        if (v%nulls) then
            do i = 1_int64, v%n
                if (parquet_string_column_is_null(sc, i)) eff(i) = .false.
            end do
        end if
    end subroutine ix_str_effective_mask

    ! ---- The keys, as a column ----

    !> Copies every live string of the store into `list`, in store order, through the column's
    !! bulk append: two contiguous copies, never a per-element `%append_string`
    !! (feature_risks.md Risk-60).
    subroutine ix_str_collect(self, list)
        type(pf_index_map), intent(in) :: self                !! the map.
        type(parquet_string_column), intent(inout) :: list    !! cleared, then filled.
        integer(int64), allocatable, target :: offs(:)
        character(len=1), allocatable, target :: bytes(:)
        integer(int64) :: p, k, total, a, nb

        call list%clear()
        if (self%nk == 0_int64) return
        total = 0_int64
        do p = 1_int64, self%nstr
            if (self%sval(p) /= 0_int64) total = total + self%soff(p + 1_int64) - self%soff(p)
        end do
        allocate(offs(self%nk + 1_int64), bytes(max(total, 1_int64)))
        offs(1) = 0_int64
        k = 0_int64
        do p = 1_int64, self%nstr
            if (self%sval(p) == 0_int64) cycle
            k = k + 1_int64
            a = self%soff(p)
            nb = self%soff(p + 1_int64) - a
            if (nb > 0_int64) bytes(offs(k) + 1_int64 : offs(k) + nb) = self%sdat(a + 1_int64 : a + nb)
            offs(k + 1_int64) = offs(k) + nb
        end do
        call list%append_buffers(k, total, c_loc(offs), c_loc(bytes), c_null_ptr, .false.)
    end subroutine ix_str_collect

    ! ---- pf_index_multimap workers ----

    !> The group id of a string key, after the kind check the scalar forms share.
    pure function mm_group_s(self, key, what) result(g)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        character(len=*), intent(in) :: key         !! the key, as written.
        character(len=*), intent(in) :: what        !! procedure name, for the message.
        integer(int64) :: g                         !! the group id, or 0.

        if (self%map%ncomp /= 0 .and. .not. self%map%is_str) error stop MM // what // &
            ": this multimap holds integer keys; look up with an integer key"
        g = ix_str_value(self%map, key, len(key, kind=int64))
    end function mm_group_s

    !> Validates a string bulk call's shapes against the multimap and returns the row count.
    pure function mm_str_rows(self, nrows, nidx, what) result(n)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: nrows         !! keys presented.
        integer(int64), intent(in) :: nidx          !! length of the answer array.
        character(len=*), intent(in) :: what        !! procedure name, for the message.
        integer(int64) :: n                         !! rows to walk.

        if (nrows /= nidx) error stop MM // what // &
            ": the keys and the answer array must have the same length"
        if (self%map%ncomp /= 0 .and. .not. self%map%is_str) error stop MM // what // &
            ": this multimap holds integer keys; look up with integer keys"
        n = nrows
    end function mm_str_rows

    !> Refuses a string probe of a multimap holding integer keys. Impure, being guard-only.
    subroutine mm_str_check_probe(self, what)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        character(len=*), intent(in) :: what        !! procedure name, for the message.

        if (self%map%ncomp /= 0 .and. .not. self%map%is_str) call ix_abort(MM // what // &
            ": this multimap holds integer keys; probe with integer keys")
    end subroutine mm_str_check_probe

    !> Right-sizes the distinct-key map after a grouping pass that reserved for every row: the
    !! same rule `mm_group_hash_1` applies, a rebuild from the distinct keys when the repeats
    !! left the table mostly empty.
    !!
    !! **The rebuild keeps every group id**, by construction rather than by bookkeeping: on a map
    !! freshly filled by `%get_or_add_many` the store position of a string IS its code (both
    !! count first appearances from 1), `%keys` walks the store in position order, and `%build`
    !! numbers the column it is handed from 1 -- so the string with code `g` is element `g` of
    !! the column and is stored under `g` again.
    subroutine mm_str_rightsize(map, nv, ng, threads)
        type(pf_index_map), intent(inout) :: map  !! the distinct-key map.
        integer(int64), intent(in) :: nv          !! rows the grouping kept.
        integer(int64), intent(in) :: ng          !! groups it found.
        integer, intent(in), optional :: threads  !! forwarded to the rebuild.
        type(parquet_string_column) :: dk

        if (ng >= nv / 4_int64) return
        call map%keys(dk)
        call map%build(dk, method="hash", threads=threads)
    end subroutine mm_str_rightsize

    !> Builds a string multimap from a character array: the map's string grouping pass, then
    !! the multimap's own layout. See the file header of `parquet_index_multi.f90` for the
    !! three passes; here pass 1 is always the hash pass, since a string map is always hashed.
    subroutine mm_str_build_chr(self, keys, values, method, threads, valid)
        type(pf_index_multimap), intent(inout) :: self    !! the multimap; rebuilt from scratch.
        character(len=*), intent(in) :: keys(:)           !! the keys, each trimmed; may repeat.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for the row numbers.
        character(len=*), intent(in), optional :: method  !! backend token; auto or hash only.
        integer, intent(in), optional :: threads          !! threads to build with.
        logical, intent(in), optional :: valid(:)         !! per row; `.false.` skips the row.
        integer(int64), allocatable :: g(:)
        integer(int64) :: n, nv, ng

        n = size(keys, kind=int64)
        if (present(valid)) call mm_check_mask_len(size(valid, kind=int64), n, "build")
        if (present(values)) then
            call ix_check_values_len(size(values, kind=int64), n, MM)
            call ix_check_values_range(values, valid, MM)
        end if
        call mm_check_threads(threads, "build")
        call ix_str_resolve_method(method, "build", MM)
        call mm_release(self)
        allocate(g(n))
        call self%map%init(strings=.true.)
        if (ix_threads_rule(n, threads, "build") == 1) call self%map%reserve(n)
        call self%map%get_or_add_many(keys, g, valid=valid, threads=threads)
        ng = self%map%nkeys()
        nv = count(g > 0_int64, kind=int64)
        call mm_str_rightsize(self%map, nv, ng, threads)
        call mm_layout(self, g, values, nv, ng)
    end subroutine mm_str_build_chr

    !> Builds a string multimap from a parquet_string_column, a null element skipped. See
    !! `mm_str_build_chr`.
    subroutine mm_str_build_col(self, sc, values, method, threads, valid)
        type(pf_index_multimap), intent(inout) :: self        !! the multimap; rebuilt from scratch.
        type(parquet_string_column), intent(in), target :: sc !! the keys, verbatim; may repeat.
        integer(int64), intent(in), optional :: values(:)     !! values, or absent for the row numbers.
        character(len=*), intent(in), optional :: method      !! backend token; auto or hash only.
        integer, intent(in), optional :: threads              !! threads to build with.
        logical, intent(in), optional :: valid(:)             !! per row; `.false.` skips the row.
        type(ix_str_view) :: v
        integer(int64), allocatable :: g(:)
        logical, allocatable :: eff(:)
        integer(int64) :: n, nv, ng

        call ix_str_open_view(sc, v)
        n = v%n
        if (present(valid)) call mm_check_mask_len(size(valid, kind=int64), n, "build")
        call ix_str_effective_mask(sc, v, valid, eff)
        if (present(values)) then
            call ix_check_values_len(size(values, kind=int64), n, MM)
            call ix_check_values_range(values, eff, MM)
        end if
        call mm_check_threads(threads, "build")
        call ix_str_resolve_method(method, "build", MM)
        call mm_release(self)
        allocate(g(n))
        call self%map%init(strings=.true.)
        if (ix_threads_rule(n, threads, "build") == 1) call self%map%reserve(n)
        call self%map%get_or_add_many(sc, g, valid=valid, threads=threads)
        ng = self%map%nkeys()
        nv = count(g > 0_int64, kind=int64)
        call mm_str_rightsize(self%map, nv, ng, threads)
        call mm_layout(self, g, values, nv, ng)
    end subroutine mm_str_build_col

end submodule parquet_index_str ! GCOVR_EXCL_LINE
