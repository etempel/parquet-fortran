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
!! nothing beyond the pair it keeps. On the hash and sorted backends pass 1 threads through the
!! map's `%get_or_add_many` -- on a team, its partitioned pass, which numbers the groups partition
!! by partition rather than by first appearance, as the spec's note on the group ids allows. On
!! the direct backend (a dense key column) pass 1 is the slot array, SERIAL at every team size and
!! first-appearance numbered: one load and one store per row, already cheaper than the threaded
!! hash pass, so `threads=` reaches only the map built over the distinct keys there. Passes 2 and 3
!! are serial on every backend because they cost a few nanoseconds per row against the hash
!! pass's tens.
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
!!
!! **String keys are not here.** `parquet_index_str.f90` is this file's SIBLING beneath
!! `parquet_index_map` and holds every string-keyed entry of the multimap beside the map's, so
!! that one body can reach the map's string `%get_or_add_many` and `mm_layout` alike. What this
!! file adds for them is one guard: every integer-keyed entry below refuses a multimap whose
!! distinct-key map holds strings, by name, before it could hash an integer against a string table.
!!
!! **The multimap's private workers are not here either**, but in `parquet_index_map.f90`: that
!! submodule is the one scope this file and `parquet_index_str.f90` share, and nagfor cannot
!! compile a submodule three levels below its module when the middle one reaches any name by host
!! association (CLAUDE.md, "fortran-gotchas.md"). This file holds the `module procedure` bodies
!! and nothing else; a new `mm_*` worker goes in `parquet_index_map.f90`.
submodule (parquet_index:parquet_index_map) parquet_index_multi
    implicit none

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

    module procedure mm_csr_i64
        if (allocated(self%goff)) then
            offsets = self%goff
            rows = self%grows
        else
            allocate(offsets(1))
            offsets(1) = 1_int64
            allocate(rows(0))
        end if
    end procedure mm_csr_i64

    module procedure mm_csr_i32
        ! Both checks up front, against the two DIFFERENT quantities the two arrays are bounded by
        ! -- see the interface. `pure`, so the aborts are bare `error stop` rather than `ix_abort`;
        ! a pure procedure may not contain the OpenMP critical that reporter carries.
        if (self%vmax > int(huge(0_int32), int64)) error stop "pf_index_multimap%csr" // &
            ": a stored value is too large for an int32 answer; take it as int64"
        if (self%nr + 1_int64 > int(huge(0_int32), int64)) error stop "pf_index_multimap%csr" // &
            ": more rows are stored than an int32 offset can name; take the offsets as int64" ! GCOVR_EXCL_LINE
        if (allocated(self%goff)) then
            allocate(offsets(size(self%goff, kind=int64)))
            offsets = int(self%goff, int32)
            allocate(rows(size(self%grows, kind=int64)))
            rows = int(self%grows, int32)
        else
            allocate(offsets(1))
            offsets(1) = 1_int32
            allocate(rows(0))
        end if
    end procedure mm_csr_i32

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

        if (self%map%is_str) error stop MM // "keys: this multimap holds string keys; " // &
            "ask for a parquet_string_column"
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

        if (self%map%is_str) error stop MM // "keys: this multimap holds string keys; " // &
            "ask for a parquet_string_column"
        nc = self%map%ncomp
        if (nc < 1) nc = 1
        allocate(list(self%map%nk, nc))
        if (self%map%nk == 0_int64) return
        call ix_collect_keys(self%map, list)
    end procedure mm_keys_r2

    ! The two int32 forms repeat their int64 siblings' bodies for the reason stated above: a call
    ! passing `self%map` to a `class(pf_index_map)` dummy beside an allocatable `intent(out)` is
    ! the gfortran 15.2 ICE, and delegating to `map_keys_r1_i32` would be exactly that call.
    module procedure mm_keys_r1_i32
        integer(int64), allocatable :: pairs(:,:)

        if (self%map%is_str) error stop MM // "keys: this multimap holds string keys; " // &
            "ask for a parquet_string_column"
        if (self%map%ncomp > 1) error stop MM // "keys: this multimap has composite keys; " // &
            "ask for a rank-2 list"
        allocate(list(self%map%nk))
        if (self%map%nk == 0_int64) return
        allocate(pairs(self%map%nk, 1))
        call ix_collect_keys(self%map, pairs)
        call ix_narrow_keys_1(pairs(:, 1), MM // "keys", list)
    end procedure mm_keys_r1_i32

    module procedure mm_keys_r2_i32
        integer(int64), allocatable :: pairs(:,:)
        integer :: nc

        if (self%map%is_str) error stop MM // "keys: this multimap holds string keys; " // &
            "ask for a parquet_string_column"
        nc = self%map%ncomp
        if (nc < 1) nc = 1
        allocate(list(self%map%nk, nc))
        if (self%map%nk == 0_int64) return
        allocate(pairs(self%map%nk, nc))
        call ix_collect_keys(self%map, pairs)
        call ix_narrow_keys_n(pairs, MM // "keys", list)
    end procedure mm_keys_r2_i32

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

end submodule parquet_index_multi
