!> `pf_index_map`: lifecycle, backend dispatch, the direct backend, and the map's OpenMP guard.
!!
!! **Where the hot path is.** `%get` reaches storage through `ix_get_scalar`/`ix_get_tuple`, which
!! are `pure`, allocate nothing, write nothing and take no lock. That last property is not a
!! coincidence to be preserved by care -- it is what makes concurrent lookups safe at all, so a
!! lazy allocation or a cached anything on a read path would silently withdraw the module's
!! thread-safety contract. There is nothing on these paths to make lazy.
!!
!! **Every mutation takes one process-wide named critical, `pf_index_map_guard`**, in the same
!! wrapper-plus-worker shape the pool uses: the public body is the guard and one call, and no
!! worker calls a public entry. A named critical is not recursive, so a nested acquisition would
!! deadlock; and branching out of a `critical` region is not conforming, so a worker that returns
!! early cannot be inlined into the guarded body. Argument validation happens inside the region,
!! which also means at most one thread can ever reach an `error stop` from this type.
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
submodule (parquet_index) parquet_index_map
    use parquet_utils, only: pf_to_lower
    use parquet_settings_base, only: parquet_auto_thread_count, parquet_clamp_to_affinity, &
        cfg_index_threads
    implicit none

    !> `method="auto"`: let `ix_choose_direct` decide. Never stored in `self%backend`.
    integer, parameter :: IX_WANT_AUTO = -1

contains

    ! ============================================================================================
    ! Bulk build. Twelve forwarders onto two workers -- one for rank-1 keys, one for rank-2 -- so
    ! that an `int64` caller's key array is never copied. The `int32` forwarders must widen, which
    ! is a cost the kind conversion imposes rather than one this split introduces.
    ! ============================================================================================

    module procedure build_r1_k64_nov
        !$omp critical (pf_index_map_guard)
        call ix_build_1(self, keys, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r1_k64_nov

    module procedure build_r1_k64_v64
        !$omp critical (pf_index_map_guard)
        call ix_build_1(self, keys, values=values, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r1_k64_v64

    module procedure build_r1_k64_v32
        integer(int64), allocatable :: v(:)

        call ix_widen_values(values, v)
        !$omp critical (pf_index_map_guard)
        call ix_build_1(self, keys, values=v, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r1_k64_v32

    module procedure build_r1_k32_nov
        integer(int64), allocatable :: k(:)

        call ix_widen_keys_1(keys, k)
        !$omp critical (pf_index_map_guard)
        call ix_build_1(self, k, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r1_k32_nov

    module procedure build_r1_k32_v64
        integer(int64), allocatable :: k(:)

        call ix_widen_keys_1(keys, k)
        !$omp critical (pf_index_map_guard)
        call ix_build_1(self, k, values=values, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r1_k32_v64

    module procedure build_r1_k32_v32
        integer(int64), allocatable :: k(:), v(:)

        call ix_widen_keys_1(keys, k)
        call ix_widen_values(values, v)
        !$omp critical (pf_index_map_guard)
        call ix_build_1(self, k, values=v, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r1_k32_v32

    module procedure build_r2_k64_nov
        !$omp critical (pf_index_map_guard)
        call ix_build_n(self, keys, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r2_k64_nov

    module procedure build_r2_k64_v64
        !$omp critical (pf_index_map_guard)
        call ix_build_n(self, keys, values=values, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r2_k64_v64

    module procedure build_r2_k64_v32
        integer(int64), allocatable :: v(:)

        call ix_widen_values(values, v)
        !$omp critical (pf_index_map_guard)
        call ix_build_n(self, keys, values=v, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r2_k64_v32

    module procedure build_r2_k32_nov
        integer(int64), allocatable :: k(:,:)

        call ix_widen_keys_n(keys, k)
        !$omp critical (pf_index_map_guard)
        call ix_build_n(self, k, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r2_k32_nov

    module procedure build_r2_k32_v64
        integer(int64), allocatable :: k(:,:)

        call ix_widen_keys_n(keys, k)
        !$omp critical (pf_index_map_guard)
        call ix_build_n(self, k, values=values, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r2_k32_v64

    module procedure build_r2_k32_v32
        integer(int64), allocatable :: k(:,:), v(:)

        call ix_widen_keys_n(keys, k)
        call ix_widen_values(values, v)
        !$omp critical (pf_index_map_guard)
        call ix_build_n(self, k, values=v, method=method, threads=threads)
        !$omp end critical (pf_index_map_guard)
    end procedure build_r2_k32_v32

    ! ============================================================================================
    ! Incremental lifecycle
    ! ============================================================================================

    module procedure map_init
        !$omp critical (pf_index_map_guard)
        call ix_do_init(self, capacity, method, ncomp)
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
    ! Lookup. `pure`, lock-free, allocation-free.
    ! ============================================================================================

    module procedure get_k32
        call ix_check_scalar_shape(self, "get")
        idx = ix_get_scalar(self, int(key, int64))
    end procedure get_k32

    module procedure get_k64
        call ix_check_scalar_shape(self, "get")
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
        call ix_check_scalar_shape(self, "contains")
        ok = ix_get_scalar(self, int(key, int64)) > 0_int64
    end procedure has_k32

    module procedure has_k64
        call ix_check_scalar_shape(self, "contains")
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
        integer(int64) :: i, n

        n = ix_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), 1)
        do i = 1_int64, n
            indexes(i) = ix_narrow(ix_get_scalar(self, int(keys(i), int64)))
        end do
    end procedure many_r1_k32_i32

    module procedure many_r1_k32_i64
        integer(int64) :: i, n

        n = ix_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), 1)
        do i = 1_int64, n
            indexes(i) = ix_get_scalar(self, int(keys(i), int64))
        end do
    end procedure many_r1_k32_i64

    module procedure many_r1_k64_i32
        integer(int64) :: i, n

        n = ix_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), 1)
        do i = 1_int64, n
            indexes(i) = ix_narrow(ix_get_scalar(self, keys(i)))
        end do
    end procedure many_r1_k64_i32

    module procedure many_r1_k64_i64
        integer(int64) :: i, n

        n = ix_many_rows(self, size(keys, kind=int64), size(indexes, kind=int64), 1)
        do i = 1_int64, n
            indexes(i) = ix_get_scalar(self, keys(i))
        end do
    end procedure many_r1_k64_i64

    module procedure many_r2_k32_i32
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: i, n
        integer :: nc, j

        n = ix_many_rows(self, size(keys, 1, kind=int64), size(indexes, kind=int64), size(keys, 2))
        nc = self%ncomp
        do i = 1_int64, n
            do j = 1, nc
                kb(j) = int(keys(i, j), int64)
            end do
            indexes(i) = ix_narrow(ix_get_tuple(self, kb(1:nc)))
        end do
    end procedure many_r2_k32_i32

    module procedure many_r2_k32_i64
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: i, n
        integer :: nc, j

        n = ix_many_rows(self, size(keys, 1, kind=int64), size(indexes, kind=int64), size(keys, 2))
        nc = self%ncomp
        do i = 1_int64, n
            do j = 1, nc
                kb(j) = int(keys(i, j), int64)
            end do
            indexes(i) = ix_get_tuple(self, kb(1:nc))
        end do
    end procedure many_r2_k32_i64

    module procedure many_r2_k64_i32
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: i, n
        integer :: nc, j

        n = ix_many_rows(self, size(keys, 1, kind=int64), size(indexes, kind=int64), size(keys, 2))
        nc = self%ncomp
        do i = 1_int64, n
            do j = 1, nc
                kb(j) = keys(i, j)
            end do
            indexes(i) = ix_narrow(ix_get_tuple(self, kb(1:nc)))
        end do
    end procedure many_r2_k64_i32

    module procedure many_r2_k64_i64
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: i, n
        integer :: nc, j

        n = ix_many_rows(self, size(keys, 1, kind=int64), size(indexes, kind=int64), size(keys, 2))
        nc = self%ncomp
        do i = 1_int64, n
            do j = 1, nc
                kb(j) = keys(i, j)
            end do
            indexes(i) = ix_get_tuple(self, kb(1:nc))
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
        if (allocated(self%hkeys)) b = b + 8_int64 * size(self%hkeys, kind=int64)
        if (allocated(self%hvals)) b = b + 8_int64 * size(self%hvals, kind=int64)
        if (allocated(self%skeys)) b = b + 8_int64 * size(self%skeys, kind=int64)
        if (allocated(self%svals)) b = b + 8_int64 * size(self%svals, kind=int64)
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

        nc = self%ncomp
        if (nc < 1) nc = 1
        allocate(list(self%nk, nc))
        if (self%nk == 0_int64) return
        call ix_collect_keys(self, list)
    end procedure map_keys_r2

    ! ============================================================================================
    ! The build's thread rule, reported
    ! ============================================================================================

    module procedure pf_index_threads_i32
        nt = ix_threads_for(int(n, int64))
    end procedure pf_index_threads_i32

    module procedure pf_index_threads_i64
        nt = ix_threads_for(n)
    end procedure pf_index_threads_i64

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

        if (self%ncomp > 1) error stop "pf_index_map%" // what // &
            ": this map has composite keys; pass the whole key tuple, not a scalar"
    end subroutine ix_check_scalar_shape

    !> Validates a presented tuple width against the map's own, and returns it.
    pure function ix_tuple_width(self, n, what) result(nc)
        type(pf_index_map), intent(in) :: self !! the map.
        integer, intent(in) :: n               !! the presented tuple length.
        character(len=*), intent(in) :: what   !! procedure name, for the message.
        integer :: nc                          !! `n`, once it is known to match.

        if (n /= self%ncomp) error stop "pf_index_map%" // what // &
            ": the key tuple's length does not match this map's component count"
        nc = n
    end function ix_tuple_width

    !> Validates a bulk lookup's shapes and returns the row count to walk.
    !!
    !! The component check is skipped for a map that was never built, so a bulk lookup against one
    !! answers 0 for every key rather than aborting -- the same rule the scalar `%get` follows.
    pure function ix_many_rows(self, nrows, nidx, nc) result(n)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: nrows    !! keys presented.
        integer(int64), intent(in) :: nidx     !! length of the answer array.
        integer, intent(in) :: nc              !! components presented per key.
        integer(int64) :: n                    !! rows to walk.

        if (nrows /= nidx) error stop "pf_index_map%get_many: " // &
            "the keys and the indexes array must have the same length"
        if (self%ncomp > 0 .and. nc /= self%ncomp) error stop "pf_index_map%get_many: " // &
            "the keys' component count does not match this map's"
        n = nrows
    end function ix_many_rows

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
        if (allocated(self%hkeys)) deallocate(self%hkeys)
        if (allocated(self%hvals)) deallocate(self%hvals)
        if (allocated(self%skeys)) deallocate(self%skeys)
        if (allocated(self%svals)) deallocate(self%svals)
        self%backend = IX_DIRECT
        self%ncomp = 0
        self%nk = 0_int64
        self%next_auto = 0_int64
        self%kmin1 = 0_int64
        self%kmax1 = -1_int64
        self%hcap = 0_int64
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
            if (allocated(self%hvals)) self%hvals = 0_int64
        case (IX_SORTED)
            ! Nothing to zero: a sorted map is read through `1 .. nk`, so dropping the count is
            ! what empties it, and the arrays stay for a rebuild that will overwrite them.
            continue
        end select
        self%nk = 0_int64
        self%next_auto = 0_int64
    end subroutine ix_do_reset

    !> `%init`: an empty map ready for incremental insertion.
    subroutine ix_do_init(self, capacity, method, ncomp)
        type(pf_index_map), intent(inout) :: self       !! the map.
        integer, intent(in), optional :: capacity       !! keys to pre-size for.
        character(len=*), intent(in), optional :: method !! backend token; hash or auto only.
        integer, intent(in), optional :: ncomp          !! components per key; 1 by default.
        integer :: want, nc

        call ix_resolve_method(method, want, .false., "init")
        nc = 1
        if (present(ncomp)) nc = ncomp
        call ix_check_ncomp(nc, "init")
        call ix_reset_storage(self)
        self%ncomp = nc
        self%backend = IX_HASH
        if (present(capacity)) then
            if (capacity < 0) error stop "pf_index_map%init: capacity must be >= 0"
            call ix_hash_reserve(self, int(capacity, int64))
        else
            call ix_hash_reserve(self, 0_int64)
        end if
    end subroutine ix_do_init

    !> `%reserve`: make room for `n` keys without rehashing later.
    subroutine ix_do_reserve(self, n)
        type(pf_index_map), intent(inout) :: self !! the map.
        integer(int64), intent(in) :: n           !! keys to make room for.

        if (n < 0_int64) error stop "pf_index_map%reserve: n must be >= 0"
        if (self%ncomp == 0) call ix_do_init(self, ncomp=1)
        if (self%backend /= IX_HASH) return
        call ix_hash_reserve(self, n)
    end subroutine ix_do_reserve

    !> Validates a component count against the module's fixed maximum.
    subroutine ix_check_ncomp(nc, what)
        integer, intent(in) :: nc            !! the requested component count.
        character(len=*), intent(in) :: what !! procedure name, for the message.
        character(len=32) :: t

        if (nc < 1) error stop "pf_index_map%" // what // ": a key must have at least one component"
        if (nc > pf_index_max_components) then
            write (t, "(i0)") pf_index_max_components
            error stop "pf_index_map%" // what // ": a key may have at most " // trim(t) // &
                " components (pf_index_max_components)"
        end if
    end subroutine ix_check_ncomp

    !> Resolves a `method=` token to a backend, or to `IX_WANT_AUTO`.
    !!
    !! Case-insensitive, like every other token argument in this library. `allow_frozen` is what
    !! separates `%build` from `%init`: the direct backend needs the whole key range up front and
    !! the sorted backend cannot be added to at all, so neither can start an incremental map.
    subroutine ix_resolve_method(method, want, allow_frozen, what)
        character(len=*), intent(in), optional :: method !! the caller's token, or absent.
        integer, intent(out) :: want                     !! resolved backend, or `IX_WANT_AUTO`.
        logical, intent(in) :: allow_frozen              !! whether build-only backends are accepted.
        character(len=*), intent(in) :: what             !! procedure name, for the message.
        character(len=16) :: tok

        want = IX_WANT_AUTO
        if (.not. present(method)) return
        if (len_trim(method) > len(tok)) error stop "pf_index_map%" // what // &
            ": unknown method (accepted: ""auto"", ""direct"", ""hash"", ""sorted"")"
        tok = method
        call pf_to_lower(tok)
        select case (trim(tok))
        case ("auto")
            want = IX_WANT_AUTO
        case ("hash")
            want = IX_HASH
        case ("direct")
            if (.not. allow_frozen) error stop "pf_index_map%" // what // &
                ": method=""direct"" needs the key range up front, so it is only available " // &
                "on %build; use ""hash"" for an incremental map"
            want = IX_DIRECT
        case ("sorted")
            if (.not. allow_frozen) error stop "pf_index_map%" // what // &
                ": method=""sorted"" is frozen once built, so it is only available on %build; " // &
                "use ""hash"" for an incremental map"
            want = IX_SORTED
        case default
            error stop "pf_index_map%" // what // ": unknown method """ // trim(tok) // &
                """ (accepted: ""auto"", ""direct"", ""hash"", ""sorted"")"
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

    !> Threads a build should use for `n` keys.
    !!
    !! An explicit request wins and is honoured whatever the size -- the caller has said what they
    !! want -- but is still clamped to what this process's CPU affinity allows, because opening
    !! more threads than there are processors is slower than not threading at all. Otherwise the
    !! automatic answer comes from `parquet_auto_thread_count`, which is where the
    !! serial-inside-a-parallel-region rule and the affinity clamp live, and is then bounded by the
    !! work available so that a mid-sized build gets the few threads that pay rather than a full
    !! team that does not.
    function ix_threads_for(n, threads) result(nt)
        integer(int64), intent(in) :: n              !! keys the build will process.
        integer, intent(in), optional :: threads     !! the caller's request, or absent.
        integer :: nt                                !! threads to use; 1 means serial.
        integer(int64) :: nt_work

        if (present(threads)) then
            if (threads < 1) error stop "pf_index_map%build: threads= must be at least 1"
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
    end function ix_threads_for

    ! ---- Value validation ----

    !> Checks that a values array is as long as the key array.
    subroutine ix_check_values_len(nv, n)
        integer(int64), intent(in) :: nv !! elements in `values`.
        integer(int64), intent(in) :: n  !! keys presented.

        if (nv /= n) error stop "pf_index_map%build: " // &
            "values= must have exactly one element per key"
    end subroutine ix_check_values_len

    !> Checks that every value is a Fortran index value, naming the first that is not.
    subroutine ix_check_values_range(values)
        integer(int64), intent(in) :: values(:) !! the values about to be stored.
        integer(int64) :: i
        character(len=32) :: t, p

        do i = 1_int64, size(values, kind=int64)
            if (values(i) < 1_int64) then
                write (t, "(i0)") values(i)
                write (p, "(i0)") i
                error stop "pf_index_map%build: values(" // trim(p) // ") is " // trim(t) // &
                    "; stored values must be >= 1 because 0 is how a lookup reports ""not found"""
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
            error stop "pf_index_map%" // what // ": value is " // trim(t) // &
                "; stored values must be >= 1 because 0 is how a lookup reports ""not found"""
        end if
    end subroutine ix_check_value

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
            idx = ix_hash_find_scalar(self, key)
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
        integer(int64) :: off
        integer :: j

        idx = 0_int64
        if (self%ncomp == 0) return
        select case (self%backend)
        case (IX_DIRECT)
            off = 1_int64
            do j = 1, self%ncomp
                if (key(j) < self%dkmin(j) .or. key(j) > self%dkmax(j)) return
                off = off + (key(j) - self%dkmin(j)) * self%dstride(j)
            end do
            idx = self%dvals(off)
        case (IX_HASH)
            idx = ix_hash_find_tuple(self, key)
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

    ! ---- Mutation workers. Every one of these runs with the map's guard held. ----

    !> Refuses a mutation of a frozen sorted map, or of a key outside a direct map's range.
    subroutine ix_check_mutable(self, off, what)
        type(pf_index_map), intent(in) :: self !! the map.
        integer(int64), intent(in) :: off      !! the resolved direct slot, or 0 when out of range.
        character(len=*), intent(in) :: what   !! procedure name, for the message.

        if (self%backend == IX_SORTED) error stop "pf_index_map%" // what // &
            ": a sorted map is frozen once built; rebuild it, or build with method=""hash"""
        if (self%backend == IX_DIRECT .and. off == 0_int64) error stop "pf_index_map%" // what // &
            ": this key is outside the range the direct backend was built for; rebuild the map " // &
            "with this key included, or build with method=""hash"""
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
            call ix_hash_insert(self, key(1:nc), value, is_new)
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
            call ix_hash_remove(self, key(1:nc), hit)
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
        error stop "pf_index_map%remove: this key is not in the map; pass found= if an absent " // &
            "key is acceptable"
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
            error stop "pf_index_map%build: could not allocate " // trim(t) // &
                " slots for the direct backend; use method=""hash"" for keys this widely spread"
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
    subroutine ix_name_duplicate_1(self, keys)
        type(pf_index_map), intent(inout) :: self !! the map, with its slots already scattered.
        integer(int64), intent(in) :: keys(:)     !! the keys, in the caller's order.
        integer(int64) :: i, off
        character(len=32) :: t, p

        self%dvals = 0_int64
        do i = 1_int64, size(keys, kind=int64)
            off = keys(i) - self%kmin1 + 1_int64
            if (self%dvals(off) /= 0_int64) then
                write (t, "(i0)") keys(i)
                write (p, "(i0)") i
                error stop "pf_index_map%build: duplicate key " // trim(t) // " at position " // &
                    trim(p) // " (every key must be unique)"
            end if
            self%dvals(off) = 1_int64
        end do
        error stop "pf_index_map%build: duplicate keys were detected but could not be named"
    end subroutine ix_name_duplicate_1

    !> Names the duplicate tuple a threaded composite direct scatter detected. See
    !! `ix_name_duplicate_1`.
    subroutine ix_name_duplicate_n(self, keys)
        type(pf_index_map), intent(inout) :: self !! the map, with its slots already scattered.
        integer(int64), intent(in) :: keys(:,:)   !! the key tuples, one per row.
        integer(int64) :: i, off
        integer :: j
        character(len=32) :: p
        character(len=:), allocatable :: txt

        self%dvals = 0_int64
        do i = 1_int64, size(keys, 1, kind=int64)
            off = 1_int64
            do j = 1, self%ncomp
                off = off + (keys(i, j) - self%dkmin(j)) * self%dstride(j)
            end do
            if (self%dvals(off) /= 0_int64) then
                call ix_tuple_text(keys(i, :), txt)
                write (p, "(i0)") i
                error stop "pf_index_map%build: duplicate key " // txt // " at position " // &
                    trim(p) // " (every key tuple must be unique)"
            end if
            self%dvals(off) = 1_int64
        end do
        error stop "pf_index_map%build: duplicate keys were detected but could not be named"
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

    !> Builds a single-component map from a rank-1 key array.
    subroutine ix_build_1(self, keys, values, method, threads)
        type(pf_index_map), intent(inout) :: self        !! the map; rebuilt from scratch.
        integer(int64), intent(in) :: keys(:)            !! the keys; must be unique.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for `1 .. n`.
        character(len=*), intent(in), optional :: method  !! backend token.
        integer, intent(in), optional :: threads          !! threads to build with.
        integer(int64) :: n, lo, hi, span, budget, i, off, cnt
        integer :: want, nt
        logical :: has_v, is_new, fits

        n = size(keys, kind=int64)
        has_v = present(values)
        if (has_v) then
            call ix_check_values_len(size(values, kind=int64), n)
            call ix_check_values_range(values)
        end if
        call ix_resolve_method(method, want, .true., "build")
        call ix_reset_storage(self)
        self%ncomp = 1
        self%nk = n
        if (n == 0_int64) then
            call ix_finish_empty(self, want, 1)
            return
        end if
        if (has_v) then
            self%next_auto = maxval(values)
        else
            self%next_auto = n
        end if
        nt = ix_threads_for(n, threads)
        lo = keys(1)
        hi = keys(1)
        !$omp parallel do default(shared) private(i) reduction(min:lo) reduction(max:hi) &
        !$omp     schedule(static) num_threads(nt) if (nt > 1)
        do i = 1_int64, n
            if (keys(i) < lo) lo = keys(i)
            if (keys(i) > hi) hi = keys(i)
        end do
        if (want == IX_WANT_AUTO) then
            budget = ix_budget(n)
            call ix_span_ok(lo, hi, budget, span, fits)
            if (fits) then
                want = IX_DIRECT
            else
                want = IX_HASH
            end if
        else if (want == IX_DIRECT) then
            call ix_span_ok(lo, hi, huge(0_int64), span, fits)
            if (.not. fits) error stop &
                "pf_index_map%build: method=""direct"" cannot cover this key range; " // &
                "it exceeds the whole int64 domain. Use method=""hash""."
        end if
        select case (want)
        case (IX_DIRECT)
            call ix_set_direct_geometry_1(self, lo, hi, span)
            call ix_alloc_direct(self, span)
            if (has_v) then
                !$omp parallel do default(shared) private(i, off) schedule(static) &
                !$omp     num_threads(nt) if (nt > 1)
                do i = 1_int64, n
                    off = keys(i) - lo + 1_int64
                    self%dvals(off) = values(i)
                end do
            else
                !$omp parallel do default(shared) private(i, off) schedule(static) &
                !$omp     num_threads(nt) if (nt > 1)
                do i = 1_int64, n
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
            if (cnt /= n) call ix_name_duplicate_1(self, keys)
        case (IX_HASH)
            self%backend = IX_HASH
            self%nk = 0_int64
            call ix_hash_reserve(self, n)
            do i = 1_int64, n
                if (has_v) then
                    call ix_hash_insert_scalar(self, keys(i), values(i), is_new)
                else
                    call ix_hash_insert_scalar(self, keys(i), i, is_new)
                end if
                if (.not. is_new) call ix_report_duplicate_1(keys(i), i)
            end do
        case (IX_SORTED)
            self%backend = IX_SORTED
            call ix_sorted_build(self, keys, values, threads)
        end select
    end subroutine ix_build_1

    !> Reports a duplicate key the serial hash insert loop met.
    subroutine ix_report_duplicate_1(key, at)
        integer(int64), intent(in) :: key !! the offending key.
        integer(int64), intent(in) :: at  !! its position in the caller's array.
        character(len=32) :: t, p

        write (t, "(i0)") key
        write (p, "(i0)") at
        error stop "pf_index_map%build: duplicate key " // trim(t) // " at position " // &
            trim(p) // " (every key must be unique)"
    end subroutine ix_report_duplicate_1

    !> Reports a duplicate key tuple the serial hash insert loop met.
    subroutine ix_report_duplicate_n(key, at)
        integer(int64), intent(in) :: key(:) !! the offending tuple.
        integer(int64), intent(in) :: at     !! its row in the caller's array.
        character(len=32) :: p
        character(len=:), allocatable :: txt

        call ix_tuple_text(key, txt)
        write (p, "(i0)") at
        error stop "pf_index_map%build: duplicate key " // txt // " at position " // &
            trim(p) // " (every key tuple must be unique)"
    end subroutine ix_report_duplicate_n

    !> Builds a map with `ncomp` components from a rank-2 key array shaped `(n, ncomp)`.
    subroutine ix_build_n(self, keys, values, method, threads)
        type(pf_index_map), intent(inout) :: self        !! the map; rebuilt from scratch.
        integer(int64), intent(in) :: keys(:,:)          !! keys, one per row, one component per column.
        integer(int64), intent(in), optional :: values(:) !! values, or absent for `1 .. n`.
        character(len=*), intent(in), optional :: method  !! backend token.
        integer, intent(in), optional :: threads          !! threads to build with.
        integer(int64) :: lo(pf_index_max_components), hi(pf_index_max_components)
        integer(int64) :: sp(pf_index_max_components)
        integer(int64) :: n, budget, total, grown_total, i, off, cnt, l, h, span
        integer :: want, nt, nc, j
        logical :: has_v, is_new, fits

        n = size(keys, 1, kind=int64)
        nc = int(size(keys, 2))
        call ix_check_ncomp(nc, "build")
        has_v = present(values)
        if (has_v) then
            call ix_check_values_len(size(values, kind=int64), n)
            call ix_check_values_range(values)
        end if
        call ix_resolve_method(method, want, .true., "build")
        if (want == IX_SORTED .and. nc > 1) error stop "pf_index_map%build: " // &
            "method=""sorted"" supports single-component keys only; use ""hash"" or ""direct"" " // &
            "for composite keys"
        call ix_reset_storage(self)
        self%ncomp = nc
        self%nk = n
        if (n == 0_int64) then
            call ix_finish_empty(self, want, nc)
            return
        end if
        if (has_v) then
            self%next_auto = maxval(values)
        else
            self%next_auto = n
        end if
        nt = ix_threads_for(n, threads)
        ! One scalar-reduction pass per component. Deliberately not an array reduction over
        ! `(nc)`: support for those differs across the compilers this library is built with, and
        ! each column here is stride-1, so the separate passes cost nothing over one fused pass.
        do j = 1, nc
            l = keys(1, j)
            h = keys(1, j)
            !$omp parallel do default(shared) private(i) reduction(min:l) reduction(max:h) &
            !$omp     schedule(static) num_threads(nt) if (nt > 1)
            do i = 1_int64, n
                if (keys(i, j) < l) l = keys(i, j)
                if (keys(i, j) > h) h = keys(i, j)
            end do
            lo(j) = l
            hi(j) = h
        end do
        ! The product of the component spans, refused before it can overflow.
        budget = ix_budget(n)
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
            error stop "pf_index_map%build: method=""direct"" cannot cover these key ranges; " // &
                "their product exceeds the int64 domain. Use method=""hash""."
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
            if (cnt /= n) call ix_name_duplicate_n(self, keys)
        case (IX_HASH)
            self%backend = IX_HASH
            self%nk = 0_int64
            call ix_hash_reserve(self, n)
            do i = 1_int64, n
                if (has_v) then
                    call ix_hash_insert(self, keys(i, :), values(i), is_new)
                else
                    call ix_hash_insert(self, keys(i, :), i, is_new)
                end if
                if (.not. is_new) call ix_report_duplicate_n(keys(i, :), i)
            end do
        case (IX_SORTED)
            self%backend = IX_SORTED
            ! `keys(:, 1)` is a contiguous column, so this passes the caller's own storage.
            call ix_sorted_build(self, keys(:, 1), values, threads)
            self%kmin1 = lo(1)
            self%kmax1 = hi(1)
        end select
    end subroutine ix_build_n

end submodule parquet_index_map
