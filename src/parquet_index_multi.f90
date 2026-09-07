!> `pf_index_multimap`: keys with repeats, as a `pf_index_map` over the distinct keys and a CSR
!> pair beside it.
!!
!! **A descendant of `parquet_index_map`, not a sibling.** Everything the multimap needs from the
!! map -- the lookup workers and the eight chunk loops of `%get_many`, the thread rule and its
!! debug record, the mask, value and method checks, the automatic backend rule -- is a private
!! helper contained in that submodule, and host association reaches a parent submodule's helpers
!! where it cannot reach a sibling's. The alternative was to re-declare each as a cross-submodule
!! interface in the spec, which is the shape `ix_hash_*` and `ix_sorted_*` already take for the
!! backends and which costs one interface body per helper for no gain here.
!!
!! **The build is three passes over the rows.** Pass 1 groups: every unmasked row gets the id of
!! its distinct key, `1 .. ngroups`, from the map -- `%get_or_add_many` on the hash backend, a
!! local slot array over the key range on the direct one (a dedup that is the direct build's own
!! scatter with duplicates allowed), and the hash pass followed by a rebuild for the sorted one,
!! which has no incremental form. Pass 2 counts the rows per group and prefix-sums the counts into
!! `goff`. Pass 3 scatters each row's value into its group's range with a per-group cursor,
!! walking the rows in order, which is what makes every group's values ascending by position
!! without a sort. The cursors are `goff` itself, shifted back afterwards, so the layout allocates
!! nothing beyond the pair it keeps. Pass 1 is serial in this version, for the reason the spec
!! gives on the group ids; passes 2 and 3 are serial because they cost a few nanoseconds per row
!! against the hash pass's tens, and a threaded stable scatter is the partitioned build's business.
!!
!! **The automatic backend is the map's own rule, applied to the DISTINCT keys.** A rule applied
!! to the rows presented would take a 320 MB direct table for ten thousand keys spread over a
!! billion, repeated a thousand times each, where the map itself would take a 0.5 MB hash table.
!! So the choice is made in two steps: the span of the keys is tested against the budget for the
!! rows first, which is what decides whether a slot array over the range is affordable to dedup
!! with at all; and once the distinct count is known the same test is repeated against the budget
!! for THAT count, which is the map's rule exactly, to decide what the map is finally built as.
!!
!! **Every bulk lookup is the map's chunk loop plus one gather.** `%get_first_many` fills the
!! answer array with group ids through `ix_many_*` and then replaces each id with its group's
!! first value in place; `%probe_many` does the same into a temporary, counts and prefix-sums, and
!! copies ranges. The map's loops record nothing, so the team this file resolves through
!! `ix_lookup_threads_for` stays what `parquet_debug_index_get_many_threads_used` reports.
!!
!! **Guards name this type.** The map's shared checks take an `owner` prefix for that reason; the
!! guard-only subroutines here are impure, per CLAUDE.md's note on ifx deleting a `pure` one.
submodule (parquet_index:parquet_index_map) parquet_index_multi
    implicit none

    !> Prefix of every message this file emits, and what the map's shared guards are handed.
    character(len=*), parameter :: MM = "pf_index_multimap%"

contains

    ! ============================================================================================
    ! Bulk build. Twelve forwarders onto two workers, under the multimap's own guard -- named
    ! apart from the map's, because the workers call the map's public entries, which take that
    ! one, and a named critical is not recursive.
    ! ============================================================================================

    module procedure mm_build_r1_k64_nov
        !$omp critical (pf_index_multimap_guard)
        call mm_build_1(self, keys, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r1_k64_nov

    module procedure mm_build_r1_k64_v64
        !$omp critical (pf_index_multimap_guard)
        call mm_build_1(self, keys, values=values, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r1_k64_v64

    module procedure mm_build_r1_k64_v32
        integer(int64), allocatable :: v(:)

        call ix_widen_values(values, v)
        !$omp critical (pf_index_multimap_guard)
        call mm_build_1(self, keys, values=v, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r1_k64_v32

    module procedure mm_build_r1_k32_nov
        integer(int64), allocatable :: k(:)

        call ix_widen_keys_1(keys, k)
        !$omp critical (pf_index_multimap_guard)
        call mm_build_1(self, k, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r1_k32_nov

    module procedure mm_build_r1_k32_v64
        integer(int64), allocatable :: k(:)

        call ix_widen_keys_1(keys, k)
        !$omp critical (pf_index_multimap_guard)
        call mm_build_1(self, k, values=values, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r1_k32_v64

    module procedure mm_build_r1_k32_v32
        integer(int64), allocatable :: k(:), v(:)

        call ix_widen_keys_1(keys, k)
        call ix_widen_values(values, v)
        !$omp critical (pf_index_multimap_guard)
        call mm_build_1(self, k, values=v, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r1_k32_v32

    module procedure mm_build_r2_k64_nov
        !$omp critical (pf_index_multimap_guard)
        call mm_build_n(self, keys, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r2_k64_nov

    module procedure mm_build_r2_k64_v64
        !$omp critical (pf_index_multimap_guard)
        call mm_build_n(self, keys, values=values, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r2_k64_v64

    module procedure mm_build_r2_k64_v32
        integer(int64), allocatable :: v(:)

        call ix_widen_values(values, v)
        !$omp critical (pf_index_multimap_guard)
        call mm_build_n(self, keys, values=v, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r2_k64_v32

    module procedure mm_build_r2_k32_nov
        integer(int64), allocatable :: k(:,:)

        call ix_widen_keys_n(keys, k)
        !$omp critical (pf_index_multimap_guard)
        call mm_build_n(self, k, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r2_k32_nov

    module procedure mm_build_r2_k32_v64
        integer(int64), allocatable :: k(:,:)

        call ix_widen_keys_n(keys, k)
        !$omp critical (pf_index_multimap_guard)
        call mm_build_n(self, k, values=values, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r2_k32_v64

    module procedure mm_build_r2_k32_v32
        integer(int64), allocatable :: k(:,:), v(:)

        call ix_widen_keys_n(keys, k)
        call ix_widen_values(values, v)
        !$omp critical (pf_index_multimap_guard)
        call mm_build_n(self, k, values=v, method=method, threads=threads, valid=valid)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_build_r2_k32_v32

    module procedure mm_clear
        !$omp critical (pf_index_multimap_guard)
        call mm_release(self)
        !$omp end critical (pf_index_multimap_guard)
    end procedure mm_clear

    ! ============================================================================================
    ! Scalar lookup. Lock-free, `pure`, and one map lookup plus arithmetic on the CSR pair.
    ! ============================================================================================

    module procedure mm_get_k32
        g = mm_group_k(self, int(key, int64), "get")
    end procedure mm_get_k32

    module procedure mm_get_k64
        g = mm_group_k(self, key, "get")
    end procedure mm_get_k64

    module procedure mm_get_t32
        g = mm_group_t32(self, key, "get")
    end procedure mm_get_t32

    module procedure mm_get_t64
        g = mm_group_t64(self, key, "get")
    end procedure mm_get_t64

    module procedure mm_count_k32
        n = mm_count_of(self, mm_group_k(self, int(key, int64), "count"))
    end procedure mm_count_k32

    module procedure mm_count_k64
        n = mm_count_of(self, mm_group_k(self, key, "count"))
    end procedure mm_count_k64

    module procedure mm_count_t32
        n = mm_count_of(self, mm_group_t32(self, key, "count"))
    end procedure mm_count_t32

    module procedure mm_count_t64
        n = mm_count_of(self, mm_group_t64(self, key, "count"))
    end procedure mm_count_t64

    module procedure mm_first_k32
        v = mm_first_of(self, mm_group_k(self, int(key, int64), "get_first"))
    end procedure mm_first_k32

    module procedure mm_first_k64
        v = mm_first_of(self, mm_group_k(self, key, "get_first"))
    end procedure mm_first_k64

    module procedure mm_first_t32
        v = mm_first_of(self, mm_group_t32(self, key, "get_first"))
    end procedure mm_first_t32

    module procedure mm_first_t64
        v = mm_first_of(self, mm_group_t64(self, key, "get_first"))
    end procedure mm_first_t64

    module procedure mm_all_k32_i32
        call mm_all_of_i32(self, mm_group_k(self, int(key, int64), "get_all"), rows)
    end procedure mm_all_k32_i32

    module procedure mm_all_k32_i64
        call mm_all_of_i64(self, mm_group_k(self, int(key, int64), "get_all"), rows)
    end procedure mm_all_k32_i64

    module procedure mm_all_k64_i32
        call mm_all_of_i32(self, mm_group_k(self, key, "get_all"), rows)
    end procedure mm_all_k64_i32

    module procedure mm_all_k64_i64
        call mm_all_of_i64(self, mm_group_k(self, key, "get_all"), rows)
    end procedure mm_all_k64_i64

    module procedure mm_all_t32_i32
        call mm_all_of_i32(self, mm_group_t32(self, key, "get_all"), rows)
    end procedure mm_all_t32_i32

    module procedure mm_all_t32_i64
        call mm_all_of_i64(self, mm_group_t32(self, key, "get_all"), rows)
    end procedure mm_all_t32_i64

    module procedure mm_all_t64_i32
        call mm_all_of_i32(self, mm_group_t64(self, key, "get_all"), rows)
    end procedure mm_all_t64_i32

    module procedure mm_all_t64_i64
        call mm_all_of_i64(self, mm_group_t64(self, key, "get_all"), rows)
    end procedure mm_all_t64_i64

    module procedure mm_range_k32
        call mm_range_of(self, mm_group_k(self, int(key, int64), "get_range"), lo, hi)
    end procedure mm_range_k32

    module procedure mm_range_k64
        call mm_range_of(self, mm_group_k(self, key, "get_range"), lo, hi)
    end procedure mm_range_k64

    module procedure mm_range_t32
        call mm_range_of(self, mm_group_t32(self, key, "get_range"), lo, hi)
    end procedure mm_range_t32

    module procedure mm_range_t64
        call mm_range_of(self, mm_group_t64(self, key, "get_range"), lo, hi)
    end procedure mm_range_t64

    ! ============================================================================================
    ! Bulk lookup. Each driver resolves its team, cuts the rows into one chunk per thread, and
    ! runs the map's own chunk loop over each chunk followed by this file's gather. `%get_many`
    ! is the map's loop alone, plus the count `n_found` asks for.
    ! ============================================================================================

    module procedure mm_fmany_r1_k32_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, kind=int64), size(rows, kind=int64), 1, "get_first_many")
        call mm_check_int32_values(self, "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k32_i32(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i32(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_r1_k32_i32

    module procedure mm_fmany_r1_k32_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, kind=int64), size(rows, kind=int64), 1, "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k32_i64(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i64(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_r1_k32_i64

    module procedure mm_fmany_r1_k64_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, kind=int64), size(rows, kind=int64), 1, "get_first_many")
        call mm_check_int32_values(self, "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k64_i32(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i32(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_r1_k64_i32

    module procedure mm_fmany_r1_k64_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, kind=int64), size(rows, kind=int64), 1, "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k64_i64(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i64(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_r1_k64_i64

    module procedure mm_fmany_r2_k32_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, 1, kind=int64), size(rows, kind=int64), size(keys, 2), &
            "get_first_many")
        call mm_check_int32_values(self, "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k32_i32(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i32(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_r2_k32_i32

    module procedure mm_fmany_r2_k32_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, 1, kind=int64), size(rows, kind=int64), size(keys, 2), &
            "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k32_i64(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i64(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_r2_k32_i64

    module procedure mm_fmany_r2_k64_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, 1, kind=int64), size(rows, kind=int64), size(keys, 2), &
            "get_first_many")
        call mm_check_int32_values(self, "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k64_i32(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i32(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_r2_k64_i32

    module procedure mm_fmany_r2_k64_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, 1, kind=int64), size(rows, kind=int64), size(keys, 2), &
            "get_first_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_first_many")
        nt = mm_lookup_threads(n, threads, "get_first_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k64_i64(self%map, keys, rows, valid, hv, lo, hi)
            call mm_first_gather_i64(self, rows, lo, hi, nf)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_fmany_r2_k64_i64

    module procedure mm_many_r1_k32_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, kind=int64), size(groups, kind=int64), 1, "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k32_i32(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int32, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_r1_k32_i32

    module procedure mm_many_r1_k32_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, kind=int64), size(groups, kind=int64), 1, "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k32_i64(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int64, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_r1_k32_i64

    module procedure mm_many_r1_k64_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, kind=int64), size(groups, kind=int64), 1, "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k64_i32(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int32, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_r1_k64_i32

    module procedure mm_many_r1_k64_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, kind=int64), size(groups, kind=int64), 1, "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k64_i64(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int64, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_r1_k64_i64

    module procedure mm_many_r2_k32_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, 1, kind=int64), size(groups, kind=int64), size(keys, 2), &
            "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k32_i32(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int32, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_r2_k32_i32

    module procedure mm_many_r2_k32_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, 1, kind=int64), size(groups, kind=int64), size(keys, 2), &
            "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k32_i64(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int64, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_r2_k32_i64

    module procedure mm_many_r2_k64_i32
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, 1, kind=int64), size(groups, kind=int64), size(keys, 2), &
            "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k64_i32(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int32, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_r2_k64_i32

    module procedure mm_many_r2_k64_i64
        integer(int64) :: n, lo, hi, nf
        integer :: nt, c
        logical :: hv

        n = mm_rows(self, size(keys, 1, kind=int64), size(groups, kind=int64), size(keys, 2), &
            "get_many")
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "get_many")
        nt = mm_lookup_threads(n, threads, "get_many")
        nf = 0_int64
        !$omp parallel do default(shared) private(c, lo, hi) reduction(+:nf) schedule(static) &
        !$omp     num_threads(nt) if (nt > 1)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k64_i64(self%map, keys, groups, valid, hv, lo, hi)
            nf = nf + count(groups(lo:hi) /= 0_int64, kind=int64)
        end do
        if (present(n_found)) n_found = nf
    end procedure mm_many_r2_k64_i64

    module procedure mm_probe_r1_k32_i32
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, kind=int64)
        call mm_check_probe_shape(self, 1, "probe_many")
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
            call ix_many_1_k32_i64(self%map, keys, g, valid, hv, lo, hi)
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
    end procedure mm_probe_r1_k32_i32

    module procedure mm_probe_r1_k32_i64
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, kind=int64)
        call mm_check_probe_shape(self, 1, "probe_many")
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
            call ix_many_1_k32_i64(self%map, keys, g, valid, hv, lo, hi)
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
    end procedure mm_probe_r1_k32_i64

    module procedure mm_probe_r1_k64_i32
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, kind=int64)
        call mm_check_probe_shape(self, 1, "probe_many")
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
            call ix_many_1_k64_i64(self%map, keys, g, valid, hv, lo, hi)
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
    end procedure mm_probe_r1_k64_i32

    module procedure mm_probe_r1_k64_i64
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, kind=int64)
        call mm_check_probe_shape(self, 1, "probe_many")
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
            call ix_many_1_k64_i64(self%map, keys, g, valid, hv, lo, hi)
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
    end procedure mm_probe_r1_k64_i64

    module procedure mm_probe_r2_k32_i32
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, 1, kind=int64)
        call mm_check_probe_shape(self, size(keys, 2), "probe_many")
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
            call ix_many_n_k32_i64(self%map, keys, g, valid, hv, lo, hi)
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
    end procedure mm_probe_r2_k32_i32

    module procedure mm_probe_r2_k32_i64
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, 1, kind=int64)
        call mm_check_probe_shape(self, size(keys, 2), "probe_many")
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
            call ix_many_n_k32_i64(self%map, keys, g, valid, hv, lo, hi)
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
    end procedure mm_probe_r2_k32_i64

    module procedure mm_probe_r2_k64_i32
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, 1, kind=int64)
        call mm_check_probe_shape(self, size(keys, 2), "probe_many")
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
            call ix_many_n_k64_i64(self%map, keys, g, valid, hv, lo, hi)
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
    end procedure mm_probe_r2_k64_i32

    module procedure mm_probe_r2_k64_i64
        integer(int64), allocatable :: g(:), base(:)
        logical, allocatable :: hitbuf(:)
        integer(int64) :: n, lo, hi, total, nm
        integer :: nt, c
        logical :: hv, hg

        n = size(keys, 1, kind=int64)
        call mm_check_probe_shape(self, size(keys, 2), "probe_many")
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
            call ix_many_n_k64_i64(self%map, keys, g, valid, hv, lo, hi)
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
    end procedure mm_probe_r2_k64_i64

    ! ============================================================================================
    ! Introspection. Read-only, so unguarded like the lookups.
    ! ============================================================================================

    module procedure mm_csr
        if (allocated(self%goff)) then
            offsets = self%goff
            rows = self%grows
        else
            allocate(offsets(1))
            offsets(1) = 1_int64
            allocate(rows(0))
        end if
    end procedure mm_csr

    module procedure mm_ngroups
        n = self%ng
    end procedure mm_ngroups

    module procedure mm_nkeys
        n = self%nr
    end procedure mm_nkeys

    module procedure mm_ncomponents
        n = self%map%ncomp
    end procedure mm_ncomponents

    module procedure mm_max_multiplicity
        n = self%maxmult
    end procedure mm_max_multiplicity

    ! The map is reached through its module procedures rather than its bindings here: gfortran
    ! 15.2 ICEs (a segmentation fault in the front end) on a GENERIC binding called through a
    ! derived-type component from inside a `pure module procedure`, and the specifics are the
    ! same procedures the bindings resolve to.
    module procedure mm_memory_bytes
        b = map_memory_bytes(self%map)
        if (allocated(self%goff)) b = b + 8_int64 * size(self%goff, kind=int64)
        if (allocated(self%grows)) b = b + 8_int64 * size(self%grows, kind=int64)
    end procedure mm_memory_bytes

    module procedure mm_get_method
        ! `map_get_method`'s body, repeated for the reason the `%keys` forms below give: the call
        ! ICEs gfortran 15.2 in exactly the same way (an allocatable `character` `intent(out)`
        ! beside the `class` dummy, from this pure body).
        select case (self%map%backend)
        case (IX_DIRECT)
            method = "direct"
        case (IX_HASH)
            method = "hash"
        case (IX_SORTED)
            method = "sorted"
        case default
            method = ""
        end select
        if (self%map%ncomp == 0) method = ""
    end procedure mm_get_method

    ! Both `%keys` forms repeat the map's own bodies over `ix_collect_keys` rather than calling
    ! `map_keys_r1`/`map_keys_r2`: gfortran 15.2 ICEs (a front-end segmentation fault) on a call
    ! that passes the `map` component of this `class` dummy to a `class(pf_index_map)` dummy
    ! beside an allocatable ARRAY `intent(out)` -- straight into `list` or into a local moved
    ! afterwards, at every optimisation level -- while `map_memory_bytes`, which differs only in
    ! having no `intent(out)` allocatable, compiles as written above.
    module procedure mm_keys_r1
        integer(int64), allocatable :: pairs(:,:)

        if (self%map%ncomp > 1) error stop MM // "keys: this multimap has composite keys; " // &
            "ask for a rank-2 list"
        allocate(list(self%map%nk))
        if (self%map%nk == 0_int64) return
        allocate(pairs(self%map%nk, 1))
        call ix_collect_keys(self%map, pairs)
        list = pairs(:, 1)
    end procedure mm_keys_r1

    module procedure mm_keys_r2
        integer :: nc

        nc = self%map%ncomp
        if (nc < 1) nc = 1
        allocate(list(self%map%nk, nc))
        if (self%map%nk == 0_int64) return
        call ix_collect_keys(self%map, list)
    end procedure mm_keys_r2

    ! ============================================================================================
    ! The debug hook
    ! ============================================================================================

    module procedure parquet_debug_set_index_pair_limit
        if (limit < 1_int64) then
            dbg_index_pair_limit = huge(0_int64)
        else
            dbg_index_pair_limit = limit
        end if
    end procedure parquet_debug_set_index_pair_limit

    ! ============================================================================================
    ! Private workers. Everything below takes a `type(pf_index_multimap)` dummy where it takes
    ! the multimap at all, and nothing below calls a public entry of THIS type -- the multimap's
    ! guard is not recursive either. The map's public entries are called freely: they take the
    ! map's guard, which is a different name.
    ! ============================================================================================

    ! ---- Argument checks. The `pure` ones return something the caller uses; the guard-only ones
    ! are deliberately impure (CLAUDE.md: ifx deletes a guard-only `pure` call at -O0). ----

    !> The group id of a single-component key, after the shape check the scalar forms share.
    pure function mm_group_k(self, key, what) result(g)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key           !! the key, already widened.
        character(len=*), intent(in) :: what        !! procedure name, for the message.
        integer(int64) :: g                         !! the group id, or 0.

        if (self%map%ncomp > 1) error stop MM // what // &
            ": this multimap has composite keys; pass the whole key tuple, not a scalar"
        g = ix_get_scalar(self%map, key)
    end function mm_group_k

    !> The group id of an `int32` key tuple: width-checked, widened into a stack buffer, looked up.
    pure function mm_group_t32(self, key, what) result(g)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(in) :: key(:)        !! the caller's tuple.
        character(len=*), intent(in) :: what        !! procedure name, for the message.
        integer(int64) :: g                         !! the group id, or 0.
        integer(int64) :: kb(pf_index_max_components)
        integer :: nc, j

        g = 0_int64
        if (self%map%ncomp == 0) return
        nc = mm_tuple_width(self, size(key), what)
        do j = 1, nc
            kb(j) = int(key(j), int64)
        end do
        g = ix_get_tuple(self%map, kb(1:nc))
    end function mm_group_t32

    !> The group id of an `int64` key tuple. See `mm_group_t32`.
    pure function mm_group_t64(self, key, what) result(g)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: key(:)        !! the caller's tuple.
        character(len=*), intent(in) :: what        !! procedure name, for the message.
        integer(int64) :: g                         !! the group id, or 0.
        integer :: nc

        g = 0_int64
        if (self%map%ncomp == 0) return
        nc = mm_tuple_width(self, size(key), what)
        g = ix_get_tuple(self%map, key(1:nc))
    end function mm_group_t64

    !> Validates a presented tuple width against the multimap's own, and returns it.
    pure function mm_tuple_width(self, n, what) result(nc)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer, intent(in) :: n                    !! the presented tuple length.
        character(len=*), intent(in) :: what        !! procedure name, for the message.
        integer :: nc                               !! `n`, once it is known to match.

        if (n /= self%map%ncomp) error stop MM // what // &
            ": the key tuple's length does not match this multimap's component count"
        nc = n
    end function mm_tuple_width

    !> Validates a bulk call's shapes and returns the row count to walk. See `ix_many_rows`.
    pure function mm_rows(self, nrows, nidx, nc, what) result(n)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: nrows         !! keys presented.
        integer(int64), intent(in) :: nidx          !! length of the answer array.
        integer, intent(in) :: nc                   !! components presented per key.
        character(len=*), intent(in) :: what        !! procedure name, for the message.
        integer(int64) :: n                         !! rows to walk.

        if (nrows /= nidx) error stop MM // what // &
            ": the keys and the answer array must have the same length"
        if (self%map%ncomp > 0 .and. nc /= self%map%ncomp) error stop MM // what // &
            ": the keys' component count does not match this multimap's"
        n = nrows
    end function mm_rows

    !> Checks a probe's component count against the multimap's. Impure, being guard-only.
    subroutine mm_check_probe_shape(self, nc, what)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer, intent(in) :: nc                   !! components presented per key.
        character(len=*), intent(in) :: what        !! procedure name, for the message.

        if (self%map%ncomp > 0 .and. nc /= self%map%ncomp) error stop MM // what // &
            ": the keys' component count does not match this multimap's"
    end subroutine mm_check_probe_shape

    !> Checks that a `valid=` mask has exactly one entry per key. Impure, being guard-only.
    subroutine mm_check_mask_len(nmask, n, what)
        integer(int64), intent(in) :: nmask    !! entries in the mask.
        integer(int64), intent(in) :: n        !! keys presented.
        character(len=*), intent(in) :: what   !! procedure name, for the message.

        if (nmask /= n) error stop MM // what // ": valid= must have exactly one element per key"
    end subroutine mm_check_mask_len

    !> Refuses `threads=0`, with this type's name in the message; the map's rule would name the
    !! map. Impure, being guard-only.
    subroutine mm_check_threads(threads, what)
        integer, intent(in), optional :: threads !! the caller's request, or absent.
        character(len=*), intent(in) :: what     !! procedure name, for the message.

        if (.not. present(threads)) return
        if (threads < 1) error stop MM // what // ": threads= must be at least 1"
    end subroutine mm_check_threads

    !> Refuses an `int32` answer form when any stored value would not fit. Impure, being
    !! guard-only.
    !!
    !! Checked once per call against the largest stored value rather than per element as the
    !! map's `ix_narrow` does, so a bulk answer never fails half-way through its array.
    subroutine mm_check_int32_values(self, what)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        character(len=*), intent(in) :: what        !! procedure name, for the message.

        if (self%vmax > int(huge(0_int32), int64)) error stop MM // what // &
            ": a stored value is too large for an int32 answer; take it as int64"
    end subroutine mm_check_int32_values

    !> The team for a bulk lookup over `n` rows, recorded for the debug counter.
    function mm_lookup_threads(n, threads, what) result(nt)
        integer(int64), intent(in) :: n          !! rows the lookup will process.
        integer, intent(in), optional :: threads !! the caller's request, or absent.
        character(len=*), intent(in) :: what     !! procedure name, for the message.
        integer :: nt                            !! threads to use; 1 means serial.

        call mm_check_threads(threads, what)
        nt = ix_lookup_threads_for(n, threads)
    end function mm_lookup_threads

    ! ---- Answers from a group id ----

    !> Rows in group `g`; 0 for the absent group 0.
    pure function mm_count_of(self, g) result(n)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: g             !! a group id, or 0.
        integer(int64) :: n                         !! rows in the group.

        n = 0_int64
        if (g > 0_int64) n = self%goff(g + 1_int64) - self%goff(g)
    end function mm_count_of

    !> The value at the lowest position of group `g`; 0 for the absent group 0.
    pure function mm_first_of(self, g) result(v)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: g             !! a group id, or 0.
        integer(int64) :: v                         !! the group's first value.

        v = 0_int64
        if (g > 0_int64) v = self%grows(self%goff(g))
    end function mm_first_of

    !> Every value of group `g`, as `int64`; zero-length for the absent group 0.
    pure subroutine mm_all_of_i64(self, g, rows)
        type(pf_index_multimap), intent(in) :: self         !! the multimap.
        integer(int64), intent(in) :: g                     !! a group id, or 0.
        integer(int64), allocatable, intent(out) :: rows(:) !! the group's values.
        integer(int64) :: lo, hi

        call mm_range_of(self, g, lo, hi)
        allocate(rows(max(hi - lo + 1_int64, 0_int64)))
        if (hi >= lo) rows = self%grows(lo:hi)
    end subroutine mm_all_of_i64

    !> Every value of group `g`, as `int32`. Refuses, rather than truncating, when any stored
    !! value would not fit -- the multimap-wide test, not a per-element one.
    pure subroutine mm_all_of_i32(self, g, rows)
        type(pf_index_multimap), intent(in) :: self         !! the multimap.
        integer(int64), intent(in) :: g                     !! a group id, or 0.
        integer(int32), allocatable, intent(out) :: rows(:) !! the group's values.
        integer(int64) :: lo, hi

        if (self%vmax > int(huge(0_int32), int64)) error stop MM // "get_all" // &
            ": a stored value is too large for an int32 answer; take it as int64"
        call mm_range_of(self, g, lo, hi)
        allocate(rows(max(hi - lo + 1_int64, 0_int64)))
        if (hi >= lo) rows = int(self%grows(lo:hi), int32)
    end subroutine mm_all_of_i32

    !> The range of `grows` that group `g` occupies; `1 .. 0` for the absent group 0.
    pure subroutine mm_range_of(self, g, lo, hi)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(in) :: g             !! a group id, or 0.
        integer(int64), intent(out) :: lo           !! first position; 1 when absent.
        integer(int64), intent(out) :: hi           !! last position; 0 when absent.

        if (g > 0_int64) then
            lo = self%goff(g)
            hi = self%goff(g + 1_int64) - 1_int64
        else
            lo = 1_int64
            hi = 0_int64
        end if
    end subroutine mm_range_of

    ! ---- The bulk gathers, one chunk at a time ----

    !> Replaces each group id in `rows(lo:hi)` with its group's first value, counting the hits.
    subroutine mm_first_gather_i64(self, rows, lo, hi, nf)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int64), intent(inout) :: rows(:)    !! group ids on entry, first values on exit.
        integer(int64), intent(in) :: lo            !! first row this call owns.
        integer(int64), intent(in) :: hi            !! last row this call owns.
        integer(int64), intent(inout) :: nf         !! running count of non-zero answers.
        integer(int64) :: i, g

        do i = lo, hi
            g = rows(i)
            if (g > 0_int64) then
                rows(i) = self%grows(self%goff(g))
                nf = nf + 1_int64
            end if
        end do
    end subroutine mm_first_gather_i64

    !> The `int32` form of `mm_first_gather_i64`; the driver has already checked the values fit.
    subroutine mm_first_gather_i32(self, rows, lo, hi, nf)
        type(pf_index_multimap), intent(in) :: self !! the multimap.
        integer(int32), intent(inout) :: rows(:)    !! group ids on entry, first values on exit.
        integer(int64), intent(in) :: lo            !! first row this call owns.
        integer(int64), intent(in) :: hi            !! last row this call owns.
        integer(int64), intent(inout) :: nf         !! running count of non-zero answers.
        integer(int64) :: i, g

        do i = lo, hi
            g = int(rows(i), int64)
            if (g > 0_int64) then
                rows(i) = int(self%grows(self%goff(g)), int32)
                nf = nf + 1_int64
            end if
        end do
    end subroutine mm_first_gather_i32

    !> The hit buffer a probe fills: one cleared entry per group when the caller asked for
    !! `group_hit`, and a single never-read entry when it did not.
    !!
    !! **A buffer that always exists, rather than the caller's optional dummy passed through.**
    !! Handing an ABSENT optional allocatable `group_hit` into the `parallel do` region and on to
    !! an optional dummy of `mm_probe_count` segfaults ifx 2026.1.1 at `-O0 -check all` (fpm's
    !! debug profile) inside the outlined region, while the present case, ifx release and
    !! gfortran at both profiles run it correctly -- the multimap's three `%probe_many` error
    !! scenarios found it. So the drivers fill this local and `move_alloc` it into `group_hit`
    !! afterwards, and nothing optional crosses the region. CLAUDE.md records the shape.
    subroutine mm_probe_hit_buffer(self, hg, hitbuf)
        type(pf_index_multimap), intent(in) :: self           !! the multimap.
        logical, intent(in) :: hg                             !! whether the caller wants the flags.
        logical, allocatable, intent(out) :: hitbuf(:)        !! receives the cleared buffer.

        if (hg) then
            allocate(hitbuf(self%ng))
        else
            allocate(hitbuf(1))
        end if
        hitbuf = .false.
    end subroutine mm_probe_hit_buffer

    !> `%probe_many`'s first pass over rows `lo .. hi`: the count of each probe's group into
    !! `offsets(i + 1)`, the chunk's total, the matched-probe count, and the groups reached.
    !!
    !! The overflow test is per row and against `dbg_index_pair_limit` rather than `huge`, so
    !! that the scenario harness can reach it; with the default limit the two are the same
    !! test. A partial sum that exceeds the limit proves the total does, so refusing here is
    !! never early. The `group_hit` store is atomic: several chunks may reach one group, and a
    !! plain store of `.true.` from two threads is a data race under the OpenMP memory model
    !! even where the hardware makes it benign.
    subroutine mm_probe_count(self, g, offsets, lo, hi, hg, group_hit, csum, nm)
        type(pf_index_multimap), intent(in) :: self       !! the multimap.
        integer(int64), intent(in) :: g(:)                !! the probes' group ids, 0 where absent.
        integer(int64), intent(inout) :: offsets(:)       !! receives each probe's count at `i + 1`.
        integer(int64), intent(in) :: lo                  !! first probe this call owns.
        integer(int64), intent(in) :: hi                  !! last probe this call owns.
        logical, intent(in) :: hg                         !! whether `group_hit` is to be filled.
        logical, intent(inout) :: group_hit(:)            !! per group, set where a probe reached it;
                                                          !! a one-entry dummy, never indexed, when `hg`
                                                          !! is false.
        integer(int64), intent(out) :: csum               !! the chunk's pair count.
        integer(int64), intent(inout) :: nm               !! running count of matched probes.
        integer(int64) :: i, gi, cnt, s, lim

        lim = dbg_index_pair_limit
        s = 0_int64
        do i = lo, hi
            gi = g(i)
            if (gi > 0_int64) then
                cnt = self%goff(gi + 1_int64) - self%goff(gi)
                nm = nm + 1_int64
                if (hg) then
                    !$omp atomic write
                    group_hit(gi) = .true.
                end if
            else
                cnt = 0_int64
            end if
            if (cnt > lim - s) call mm_pair_overflow()
            s = s + cnt
            offsets(i + 1_int64) = cnt
        end do
        csum = s
    end subroutine mm_probe_count

    !> Turns the per-chunk pair counts in `base(2 : nt + 1)` into each chunk's starting offset.
    subroutine mm_probe_scan(nt, base, total)
        integer, intent(in) :: nt                    !! chunks.
        integer(int64), intent(inout) :: base(:)     !! chunk totals on entry, chunk starts on exit.
        integer(int64), intent(out) :: total         !! the pair count over every chunk.
        integer(int64) :: s, lim
        integer :: c

        lim = dbg_index_pair_limit
        base(1) = 1_int64
        do c = 1, nt
            s = base(c + 1)
            if (s > lim - base(c) + 1_int64) call mm_pair_overflow()
            base(c + 1) = base(c) + s
        end do
        total = base(nt + 1) - 1_int64
    end subroutine mm_probe_scan

    !> Turns the counts in `offsets(lo + 1 : hi + 1)` into running end positions from `start`.
    !!
    !! Writes only `offsets(lo + 1 : hi + 1)`, which no other chunk touches: chunk `c`'s last
    !! write is `offsets(hi + 1)`, and chunk `c + 1`'s first is `offsets(lo' + 1)` with
    !! `lo' = hi + 1`. Writing the START of each row instead would have chunk `c + 1` write the
    !! cell chunk `c` still has to read, which is the race this shape exists to avoid.
    subroutine mm_probe_offsets(offsets, lo, hi, start)
        integer(int64), intent(inout) :: offsets(:) !! counts on entry, end positions on exit.
        integer(int64), intent(in) :: lo            !! first probe this call owns.
        integer(int64), intent(in) :: hi            !! last probe this call owns.
        integer(int64), intent(in) :: start         !! this chunk's first position in `matches`.
        integer(int64) :: i, run

        run = start
        do i = lo, hi
            run = run + offsets(i + 1_int64)
            offsets(i + 1_int64) = run
        end do
    end subroutine mm_probe_offsets

    !> `%probe_many`'s second pass over rows `lo .. hi`: each probe's group range into its own.
    subroutine mm_probe_copy_i64(self, g, offsets, matches, lo, hi)
        type(pf_index_multimap), intent(in) :: self  !! the multimap.
        integer(int64), intent(in) :: g(:)           !! the probes' group ids, 0 where absent.
        integer(int64), intent(in) :: offsets(:)     !! each probe's range in `matches`.
        integer(int64), intent(inout) :: matches(:)  !! receives the ranges this call owns.
        integer(int64), intent(in) :: lo             !! first probe this call owns.
        integer(int64), intent(in) :: hi             !! last probe this call owns.
        integer(int64) :: i, gi, a, b, o

        do i = lo, hi
            gi = g(i)
            if (gi <= 0_int64) cycle
            a = self%goff(gi)
            b = self%goff(gi + 1_int64) - 1_int64
            o = offsets(i)
            matches(o : o + b - a) = self%grows(a:b)
        end do
    end subroutine mm_probe_copy_i64

    !> The `int32` form of `mm_probe_copy_i64`; the driver has already checked the values fit.
    subroutine mm_probe_copy_i32(self, g, offsets, matches, lo, hi)
        type(pf_index_multimap), intent(in) :: self  !! the multimap.
        integer(int64), intent(in) :: g(:)           !! the probes' group ids, 0 where absent.
        integer(int64), intent(in) :: offsets(:)     !! each probe's range in `matches`.
        integer(int32), intent(inout) :: matches(:)  !! receives the ranges this call owns.
        integer(int64), intent(in) :: lo             !! first probe this call owns.
        integer(int64), intent(in) :: hi             !! last probe this call owns.
        integer(int64) :: i, gi, a, b, o, k

        do i = lo, hi
            gi = g(i)
            if (gi <= 0_int64) cycle
            a = self%goff(gi)
            b = self%goff(gi + 1_int64) - 1_int64
            o = offsets(i)
            do k = 0_int64, b - a
                matches(o + k) = int(self%grows(a + k), int32)
            end do
        end do
    end subroutine mm_probe_copy_i32

    !> Refuses a pair count the answer could not hold.
    subroutine mm_pair_overflow()
        error stop MM // "probe_many: the pair count exceeds what the answer can hold in int64; " // &
            "probe fewer keys at a time, or fewer that repeat this heavily"
    end subroutine mm_pair_overflow

    ! ---- Storage lifecycle ----

    !> Releases every allocation and restores the as-new component values.
    subroutine mm_release(self)
        type(pf_index_multimap), intent(inout) :: self !! the multimap.

        call self%map%clear()
        if (allocated(self%goff)) deallocate(self%goff)
        if (allocated(self%grows)) deallocate(self%grows)
        self%ng = 0_int64
        self%nr = 0_int64
        self%maxmult = 0_int64
        self%vmax = 0_int64
    end subroutine mm_release

    !> The CSR pair of a multimap with nothing in it: `offsets = [1]`, no rows.
    subroutine mm_layout_empty(self)
        type(pf_index_multimap), intent(inout) :: self !! the multimap.

        allocate(self%goff(1))
        self%goff(1) = 1_int64
        allocate(self%grows(0))
        self%ng = 0_int64
        self%nr = 0_int64
        self%maxmult = 0_int64
        self%vmax = 0_int64
    end subroutine mm_layout_empty

    !> Passes 2 and 3 of the build: counts per group, the prefix sum, and the stable scatter.
    !!
    !! The rows are walked in order and each one is written at its group's cursor, so within a
    !! group the values land in ascending order of position without any sort -- the property
    !! `%get_first` and the join's documented order rest on. The cursors are `goff` itself:
    !! after the scatter `goff(g)` holds the start of group `g + 1`, and one shift down restores
    !! the offsets, so the layout allocates nothing beyond the pair it keeps.
    subroutine mm_layout(self, g, values, nv, ng)
        type(pf_index_multimap), intent(inout) :: self   !! the multimap; receives the pair.
        integer(int64), intent(in) :: g(:)               !! group id per row, 0 for a masked row.
        integer(int64), intent(in), optional :: values(:) !! the values, or absent for the row numbers.
        integer(int64), intent(in) :: nv                 !! rows with a group, i.e. `count(g > 0)`.
        integer(int64), intent(in) :: ng                 !! groups.
        integer(int64) :: i, k, gi, c, v, maxm, vmax

        allocate(self%goff(ng + 1_int64))
        self%goff = 0_int64
        do i = 1_int64, size(g, kind=int64)
            gi = g(i)
            if (gi > 0_int64) self%goff(gi + 1_int64) = self%goff(gi + 1_int64) + 1_int64
        end do
        self%goff(1) = 1_int64
        maxm = 0_int64
        do k = 1_int64, ng
            c = self%goff(k + 1_int64)
            if (c > maxm) maxm = c
            self%goff(k + 1_int64) = self%goff(k) + c
        end do
        allocate(self%grows(nv))
        vmax = 0_int64
        do i = 1_int64, size(g, kind=int64)
            gi = g(i)
            if (gi <= 0_int64) cycle
            if (present(values)) then
                v = values(i)
            else
                v = i
            end if
            self%grows(self%goff(gi)) = v
            self%goff(gi) = self%goff(gi) + 1_int64
            if (v > vmax) vmax = v
        end do
        do k = ng, 2_int64, -1_int64
            self%goff(k) = self%goff(k - 1_int64)
        end do
        self%goff(1) = 1_int64
        self%ng = ng
        self%nr = nv
        self%maxmult = maxm
        self%vmax = vmax
    end subroutine mm_layout

    ! ---- Pass 1: grouping, per backend ----

    !> Groups through the map's hash backend: `%get_or_add_many` over the rows, first-appearance
    !! numbered, then a rebuild from the distinct keys when the repeats left the reserved table
    !! mostly empty.
    !!
    !! The table is reserved for every row, since the distinct count is what the pass finds
    !! out; a build side of ten million rows over ten thousand keys would otherwise be left
    !! holding a table a thousand times larger than it needs, and would report it through
    !! `%memory_bytes`. Rebuilding from the distinct keys costs one insert per DISTINCT key --
    !! by construction under a quarter of what the pass just spent -- and right-sizes it.
    subroutine mm_group_hash_1(map, keys, valid, nv, threads, g, ng)
        type(pf_index_map), intent(inout) :: map     !! the distinct-key map; rebuilt.
        integer(int64), intent(in) :: keys(:)        !! the keys, already widened.
        logical, intent(in), optional :: valid(:)    !! the build's mask, or absent.
        integer(int64), intent(in) :: nv             !! rows the mask keeps.
        integer, intent(in), optional :: threads     !! the caller's request, for the rebuild.
        integer(int64), intent(out) :: g(:)          !! receives the group id per row, 0 if masked.
        integer(int64), intent(out) :: ng            !! receives the group count.
        integer(int64), allocatable :: dkeys(:)

        call map%init()
        call map%reserve(nv)
        call map%get_or_add_many(keys, g, valid=valid)
        ng = map%nkeys()
        if (ng < nv / 4_int64) then
            call mm_distinct_1(keys, g, ng, dkeys)
            call map%build(dkeys, method="hash", threads=threads)
        end if
    end subroutine mm_group_hash_1

    !> `mm_group_hash_1` for key tuples.
    subroutine mm_group_hash_n(map, keys, valid, nv, threads, g, ng)
        type(pf_index_map), intent(inout) :: map     !! the distinct-key map; rebuilt.
        integer(int64), intent(in) :: keys(:,:)      !! the tuples, already widened, `(n, ncomp)`.
        logical, intent(in), optional :: valid(:)    !! the build's mask, or absent.
        integer(int64), intent(in) :: nv             !! rows the mask keeps.
        integer, intent(in), optional :: threads     !! the caller's request, for the rebuild.
        integer(int64), intent(out) :: g(:)          !! receives the group id per row, 0 if masked.
        integer(int64), intent(out) :: ng            !! receives the group count.
        integer(int64), allocatable :: dkeys(:,:)

        call map%init(ncomp=int(size(keys, 2)))
        call map%reserve(nv)
        call map%get_or_add_many(keys, g, valid=valid)
        ng = map%nkeys()
        if (ng < nv / 4_int64) then
            call mm_distinct_n(keys, g, ng, dkeys)
            call map%build(dkeys, method="hash", threads=threads)
        end if
    end subroutine mm_group_hash_n

    !> Groups for the sorted backend: the hash pass into a scratch map, then the sorted build
    !! over the distinct keys, which is the only build that backend has.
    subroutine mm_group_sorted_1(map, keys, valid, nv, threads, g, ng)
        type(pf_index_map), intent(inout) :: map     !! the distinct-key map; built sorted.
        integer(int64), intent(in) :: keys(:)        !! the keys, already widened.
        logical, intent(in), optional :: valid(:)    !! the build's mask, or absent.
        integer(int64), intent(in) :: nv             !! rows the mask keeps.
        integer, intent(in), optional :: threads     !! forwarded to the sorted build's sort.
        integer(int64), intent(out) :: g(:)          !! receives the group id per row, 0 if masked.
        integer(int64), intent(out) :: ng            !! receives the group count.
        type(pf_index_map) :: scratch
        integer(int64), allocatable :: dkeys(:)

        call scratch%init()
        call scratch%reserve(nv)
        call scratch%get_or_add_many(keys, g, valid=valid)
        ng = scratch%nkeys()
        call scratch%clear()
        call mm_distinct_1(keys, g, ng, dkeys)
        call map%build(dkeys, method="sorted", threads=threads)
    end subroutine mm_group_sorted_1

    !> Groups through a slot array over the key range: the direct build's scatter with repeats
    !! allowed. First-appearance numbered, like the hash pass.
    subroutine mm_group_direct_1(keys, valid, hv, lo, span, g, ng)
        integer(int64), intent(in) :: keys(:)        !! the keys, already widened.
        logical, intent(in), optional :: valid(:)    !! the build's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo             !! the smallest unmasked key.
        integer(int64), intent(in) :: span           !! positions the unmasked keys span.
        integer(int64), intent(out) :: g(:)          !! receives the group id per row, 0 if masked.
        integer(int64), intent(out) :: ng            !! receives the group count.
        integer(int64), allocatable :: slot(:)
        integer(int64) :: i, off

        call mm_alloc_slots(slot, span)
        ng = 0_int64
        do i = 1_int64, size(keys, kind=int64)
            if (hv) then
                if (.not. valid(i)) then
                    g(i) = 0_int64
                    cycle
                end if
            end if
            off = keys(i) - lo + 1_int64
            if (slot(off) == 0_int64) then
                ng = ng + 1_int64
                slot(off) = ng
            end if
            g(i) = slot(off)
        end do
    end subroutine mm_group_direct_1

    !> `mm_group_direct_1` for key tuples, over the mixed-radix offset the map's direct backend uses.
    subroutine mm_group_direct_n(keys, valid, hv, lo, stride, total, g, ng)
        integer(int64), intent(in) :: keys(:,:)      !! the tuples, already widened, `(n, ncomp)`.
        logical, intent(in), optional :: valid(:)    !! the build's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: lo(:)          !! per component, the smallest unmasked key.
        integer(int64), intent(in) :: stride(:)      !! per component, its stride into the slots.
        integer(int64), intent(in) :: total          !! slots: the product of the component spans.
        integer(int64), intent(out) :: g(:)          !! receives the group id per row, 0 if masked.
        integer(int64), intent(out) :: ng            !! receives the group count.
        integer(int64), allocatable :: slot(:)
        integer(int64) :: i, off
        integer :: j, nc

        nc = int(size(keys, 2))
        call mm_alloc_slots(slot, total)
        ng = 0_int64
        do i = 1_int64, size(keys, 1, kind=int64)
            if (hv) then
                if (.not. valid(i)) then
                    g(i) = 0_int64
                    cycle
                end if
            end if
            off = 1_int64
            do j = 1, nc
                off = off + (keys(i, j) - lo(j)) * stride(j)
            end do
            if (slot(off) == 0_int64) then
                ng = ng + 1_int64
                slot(off) = ng
            end if
            g(i) = slot(off)
        end do
    end subroutine mm_group_direct_n

    !> Allocates the dedup slot array, reporting a refused allocation as the map's build does.
    subroutine mm_alloc_slots(slot, total)
        integer(int64), allocatable, intent(out) :: slot(:) !! receives `total` zeroed slots.
        integer(int64), intent(in) :: total                 !! slots to allocate.
        integer :: ios
        character(len=32) :: t

        allocate(slot(total), stat=ios)
        if (ios /= 0) then
            write (t, "(i0)") total
            error stop MM // "build: could not allocate " // trim(t) // &
                " slots for the direct backend; use method=""hash"" for keys this widely spread"
        end if
        slot = 0_int64
    end subroutine mm_alloc_slots

    !> The distinct keys, indexed by group id, gathered from the rows.
    !!
    !! Every group is written at least once and a repeat rewrites the same key, so no
    !! first-row bookkeeping is needed; the cost is one store per row on a cold path.
    subroutine mm_distinct_1(keys, g, ng, dkeys)
        integer(int64), intent(in) :: keys(:)                  !! the keys, already widened.
        integer(int64), intent(in) :: g(:)                     !! group id per row, 0 if masked.
        integer(int64), intent(in) :: ng                       !! groups.
        integer(int64), allocatable, intent(out) :: dkeys(:)   !! receives key `dkeys(g)` per group.
        integer(int64) :: i

        allocate(dkeys(ng))
        do i = 1_int64, size(keys, kind=int64)
            if (g(i) > 0_int64) dkeys(g(i)) = keys(i)
        end do
    end subroutine mm_distinct_1

    !> `mm_distinct_1` for key tuples: `dkeys(g, :)` is group `g`'s tuple.
    subroutine mm_distinct_n(keys, g, ng, dkeys)
        integer(int64), intent(in) :: keys(:,:)                !! the tuples, already widened.
        integer(int64), intent(in) :: g(:)                     !! group id per row, 0 if masked.
        integer(int64), intent(in) :: ng                       !! groups.
        integer(int64), allocatable, intent(out) :: dkeys(:,:) !! receives `(ng, ncomp)`.
        integer(int64) :: i
        integer :: j

        allocate(dkeys(ng, size(keys, 2, kind=int64)))
        do j = 1, int(size(keys, 2))
            do i = 1_int64, size(keys, 1, kind=int64)
                if (g(i) > 0_int64) dkeys(g(i), j) = keys(i, j)
            end do
        end do
    end subroutine mm_distinct_n

    !> Builds the distinct-key map after a direct dedup: direct when asked for or when the map's
    !! own rule, now applied to the DISTINCT count, still says so; hash otherwise.
    subroutine mm_finish_distinct_1(map, keys, g, ng, want, lo, hi, threads)
        type(pf_index_map), intent(inout) :: map     !! the distinct-key map; built.
        integer(int64), intent(in) :: keys(:)        !! the keys, already widened.
        integer(int64), intent(in) :: g(:)           !! group id per row, 0 if masked.
        integer(int64), intent(in) :: ng             !! groups.
        integer, intent(in) :: want                  !! `IX_DIRECT`, or `IX_WANT_AUTO`.
        integer(int64), intent(in) :: lo             !! the smallest unmasked key.
        integer(int64), intent(in) :: hi             !! the largest unmasked key.
        integer, intent(in), optional :: threads     !! forwarded to the map's build.
        integer(int64), allocatable :: dkeys(:)
        integer(int64) :: span
        logical :: fits
        character(len=6) :: tok

        call mm_distinct_1(keys, g, ng, dkeys)
        tok = "direct"
        if (want == IX_WANT_AUTO) then
            call ix_span_ok(lo, hi, ix_budget(ng), span, fits)
            if (.not. fits) tok = "hash"
        end if
        call map%build(dkeys, method=trim(tok), threads=threads)
    end subroutine mm_finish_distinct_1

    !> `mm_finish_distinct_1` for key tuples, with the product rule.
    subroutine mm_finish_distinct_n(map, keys, g, ng, want, lo, hi, threads)
        type(pf_index_map), intent(inout) :: map     !! the distinct-key map; built.
        integer(int64), intent(in) :: keys(:,:)      !! the tuples, already widened.
        integer(int64), intent(in) :: g(:)           !! group id per row, 0 if masked.
        integer(int64), intent(in) :: ng             !! groups.
        integer, intent(in) :: want                  !! `IX_DIRECT`, or `IX_WANT_AUTO`.
        integer(int64), intent(in) :: lo(:)          !! per component, the smallest unmasked key.
        integer(int64), intent(in) :: hi(:)          !! per component, the largest unmasked key.
        integer, intent(in), optional :: threads     !! forwarded to the map's build.
        integer(int64), allocatable :: dkeys(:,:)
        logical :: fits
        character(len=6) :: tok

        call mm_distinct_n(keys, g, ng, dkeys)
        tok = "direct"
        if (want == IX_WANT_AUTO) then
            call mm_product_fits(lo, hi, ix_budget(ng), fits)
            if (.not. fits) tok = "hash"
        end if
        call map%build(dkeys, method=trim(tok), threads=threads)
    end subroutine mm_finish_distinct_n

    !> Whether the product of the component spans fits `budget`, refused before it can overflow.
    subroutine mm_product_fits(lo, hi, budget, fits, spans, total)
        integer(int64), intent(in) :: lo(:)                   !! per component, the smallest key.
        integer(int64), intent(in) :: hi(:)                   !! per component, the largest key.
        integer(int64), intent(in) :: budget                  !! most slots acceptable.
        logical, intent(out) :: fits                          !! `.true.` when the product fits.
        integer(int64), intent(out), optional :: spans(:)     !! receives each component's span.
        integer(int64), intent(out), optional :: total        !! receives the product.
        integer(int64) :: span, run, grown
        integer :: j

        fits = .true.
        run = 1_int64
        do j = 1, size(lo)
            call ix_span_ok(lo(j), hi(j), budget, span, fits)
            if (.not. fits) exit
            if (present(spans)) spans(j) = span
            call ix_product_ok(run, span, budget, grown, fits)
            if (.not. fits) exit
            run = grown
        end do
        if (present(total)) total = run
    end subroutine mm_product_fits

    ! ---- The key scans ----

    !> The smallest and largest unmasked key, scanning from the first unmasked row.
    subroutine mm_scan_1(keys, valid, hv, first, lo, hi)
        integer(int64), intent(in) :: keys(:)        !! the keys, already widened.
        logical, intent(in), optional :: valid(:)    !! the build's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: first          !! the first unmasked row.
        integer(int64), intent(out) :: lo            !! the smallest unmasked key.
        integer(int64), intent(out) :: hi            !! the largest unmasked key.
        integer(int64) :: i

        lo = keys(first)
        hi = keys(first)
        do i = first, size(keys, kind=int64)
            if (hv) then
                if (.not. valid(i)) cycle
            end if
            if (keys(i) < lo) lo = keys(i)
            if (keys(i) > hi) hi = keys(i)
        end do
    end subroutine mm_scan_1

    !> `mm_scan_1` per component of a tuple array.
    subroutine mm_scan_n(keys, valid, hv, first, lo, hi)
        integer(int64), intent(in) :: keys(:,:)      !! the tuples, already widened.
        logical, intent(in), optional :: valid(:)    !! the build's mask, or absent.
        logical, intent(in) :: hv                    !! `present(valid)`, hoisted.
        integer(int64), intent(in) :: first          !! the first unmasked row.
        integer(int64), intent(out) :: lo(:)         !! per component, the smallest unmasked key.
        integer(int64), intent(out) :: hi(:)         !! per component, the largest unmasked key.
        integer(int64) :: i
        integer :: j

        do j = 1, int(size(keys, 2))
            lo(j) = keys(first, j)
            hi(j) = keys(first, j)
            do i = first, size(keys, 1, kind=int64)
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                if (keys(i, j) < lo(j)) lo(j) = keys(i, j)
                if (keys(i, j) > hi(j)) hi(j) = keys(i, j)
            end do
        end do
    end subroutine mm_scan_n

    ! ---- The builds ----

    !> Builds a single-component multimap from a rank-1 key array. See the file header for the
    !! three passes and the two-step backend choice.
    subroutine mm_build_1(self, keys, values, method, threads, valid)
        type(pf_index_multimap), intent(inout) :: self    !! the multimap; rebuilt from scratch.
        integer(int64), intent(in) :: keys(:)             !! the keys, already widened; may repeat.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for the row numbers.
        character(len=*), intent(in), optional :: method  !! backend token.
        integer, intent(in), optional :: threads          !! threads to build with.
        logical, intent(in), optional :: valid(:)         !! per row; `.false.` skips the row.
        integer(int64), allocatable :: g(:)
        integer(int64) :: n, nv, first, last, lo, hi, span, ng
        integer :: want
        logical :: hv, fits

        n = size(keys, kind=int64)
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "build")
        if (present(values)) then
            call ix_check_values_len(size(values, kind=int64), n, MM)
            call ix_check_values_range(values, valid, MM)
        end if
        call mm_check_threads(threads, "build")
        call ix_resolve_method(method, want, .true., "build", MM)
        call mm_release(self)
        call ix_mask_extent(n, valid, nv, first, last)
        if (nv == 0_int64) then
            call self%map%build(keys(1:0), method=method, threads=threads)
            call mm_layout_empty(self)
            return
        end if
        allocate(g(n))
        select case (want)
        case (IX_HASH)
            call mm_group_hash_1(self%map, keys, valid, nv, threads, g, ng)
        case (IX_SORTED)
            call mm_group_sorted_1(self%map, keys, valid, nv, threads, g, ng)
        case default
            call mm_scan_1(keys, valid, hv, first, lo, hi)
            if (want == IX_DIRECT) then
                call ix_span_ok(lo, hi, huge(0_int64), span, fits)
                if (.not. fits) error stop MM // "build: method=""direct"" cannot cover this " // &
                    "key range; it exceeds the whole int64 domain. Use method=""hash""."
            else
                call ix_span_ok(lo, hi, ix_budget(nv), span, fits)
            end if
            if (fits) then
                call mm_group_direct_1(keys, valid, hv, lo, span, g, ng)
                call mm_finish_distinct_1(self%map, keys, g, ng, want, lo, hi, threads)
            else
                call mm_group_hash_1(self%map, keys, valid, nv, threads, g, ng)
            end if
        end select
        call mm_layout(self, g, values, nv, ng)
    end subroutine mm_build_1

    !> Builds a multimap with `ncomp` components from a rank-2 key array shaped `(n, ncomp)`.
    subroutine mm_build_n(self, keys, values, method, threads, valid)
        type(pf_index_multimap), intent(inout) :: self    !! the multimap; rebuilt from scratch.
        integer(int64), intent(in) :: keys(:,:)           !! tuples, one per row; may repeat.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for the row numbers.
        character(len=*), intent(in), optional :: method  !! backend token.
        integer, intent(in), optional :: threads          !! threads to build with.
        logical, intent(in), optional :: valid(:)         !! per row; `.false.` skips the row.
        integer(int64) :: lo(pf_index_max_components), hi(pf_index_max_components)
        integer(int64) :: sp(pf_index_max_components), st(pf_index_max_components)
        integer(int64), allocatable :: g(:)
        integer(int64) :: n, nv, first, last, total, ng, budget
        integer :: want, nc, j
        logical :: hv, fits

        n = size(keys, 1, kind=int64)
        nc = int(size(keys, 2))
        call ix_check_ncomp(nc, "build", MM)
        hv = present(valid)
        if (hv) call mm_check_mask_len(size(valid, kind=int64), n, "build")
        if (present(values)) then
            call ix_check_values_len(size(values, kind=int64), n, MM)
            call ix_check_values_range(values, valid, MM)
        end if
        call mm_check_threads(threads, "build")
        call ix_resolve_method(method, want, .true., "build", MM)
        if (want == IX_SORTED .and. nc > 1) error stop MM // "build: " // &
            "method=""sorted"" supports single-component keys only; use ""hash"" or ""direct"" " // &
            "for composite keys"
        call mm_release(self)
        call ix_mask_extent(n, valid, nv, first, last)
        if (nv == 0_int64) then
            call self%map%build(keys(1:0, :), method=method, threads=threads)
            call mm_layout_empty(self)
            return
        end if
        allocate(g(n))
        select case (want)
        case (IX_HASH)
            call mm_group_hash_n(self%map, keys, valid, nv, threads, g, ng)
        case (IX_SORTED)
            ! `nc == 1` here, so the single column is the whole key, and contiguous.
            call mm_group_sorted_1(self%map, keys(:, 1), valid, nv, threads, g, ng)
        case default
            call mm_scan_n(keys, valid, hv, first, lo(1:nc), hi(1:nc))
            budget = ix_budget(nv)
            if (want == IX_DIRECT) budget = huge(0_int64)
            call mm_product_fits(lo(1:nc), hi(1:nc), budget, fits, sp(1:nc), total)
            if (.not. fits .and. want == IX_DIRECT) error stop MM // "build: method=""direct"" " // &
                "cannot cover these key ranges; their product exceeds the int64 domain. Use " // &
                "method=""hash""."
            if (fits) then
                st(1) = 1_int64
                do j = 2, nc
                    st(j) = st(j - 1) * sp(j - 1)
                end do
                call mm_group_direct_n(keys, valid, hv, lo(1:nc), st(1:nc), total, g, ng)
                call mm_finish_distinct_n(self%map, keys, g, ng, want, lo(1:nc), hi(1:nc), threads)
            else
                call mm_group_hash_n(self%map, keys, valid, nv, threads, g, ng)
            end if
        end select
        call mm_layout(self, g, values, nv, ng)
    end subroutine mm_build_n

end submodule parquet_index_multi
