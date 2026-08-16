!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Phase-2 Q9 probe: which construction should define `pf_random_subset`?
!!
!! Compares three ways to draw `n` DISTINCT values from `1..m`, all reproducible:
!!
!!   * **key-and-select** -- one random key per candidate, then a partial argsort. O(m) draws and
!!     O(m) memory whatever `n` is. This is what `feature_random.md` §6.3.1 specified.
!!   * **dense partial Fisher-Yates** -- `n` draws, but an O(m) array to permute.
!!   * **sparse partial Fisher-Yates** -- `n` draws and O(n) memory, the array held as a hash map
!!     of only the entries that differ from the identity.
!!
!! The two Fisher-Yates arms are the SAME algorithm with different data structures, so they must
!! agree element for element; the probe asserts that before reporting any timing, because that
!! identity is exactly what makes a dense/sparse switch a pure performance decision rather than a
!! contract change. Key-and-select deliberately returns a DIFFERENT (equally valid) subset, so it
!! is checked only for validity -- distinct, in range.
!!
!! Sorting is forced single-core via `parquet_set_sort_threads(1)`, so the comparison is
!! serial-against-serial.
!!
!! Not run by `fpm test`, by design (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark
!! tools"). Build and run with `--profile release`.
program probe_random_subset

    use iso_fortran_env, only: int32, int64, real64, output_unit
    use parquet, only: pf_random_at, pf_random_int_at, pf_random_fill_streams, &
                       pf_partial_argsort, pf_argsort, parquet_set_sort_threads

    implicit none

    integer(int64), parameter :: SEED = 20260816_int64
    integer(int64) :: m_list(3)
    real(real64) :: ratios(10)
    integer :: mi, ri
    integer :: g_rounds
    integer(int64) :: m, n

    ! Hash map for the sparse arm: open addressing, linear probing, power-of-two capacity.
    integer(int64), allocatable :: hk(:), hv(:)
    integer(int64) :: hmask

    ratios = [1.0e-5_real64, 1.0e-4_real64, 1.0e-3_real64, 1.0e-2_real64, 0.05_real64, &
              0.1_real64, 0.25_real64, 0.5_real64, 0.9_real64, 1.0_real64]
    m_list = [read_arg_int('--m=', 1000000_int64), 0_int64, 0_int64]
    g_rounds = int(read_arg_int('--rounds=', 3_int64), int32)

    call parquet_set_sort_threads(1)            ! single-core sorting, as asked

    write(output_unit, '(a)') 'probe_random_subset: n distinct values from 1..m'
    write(output_unit, '(a,i0)') 'sorting forced to 1 thread; times in ms, best of rounds = ', g_rounds
    flush(output_unit)

    do mi = 1, 1
        m = m_list(mi)
        write(output_unit, '(a,i0)') '================ m = ', m
        write(output_unit, '(a)') &
            '        n      n/m     sparse-FY      dense-FY   key+argsort    sort/sparse   sort/dense'
        do ri = 1, size(ratios)
            n = max(1_int64, int(ratios(ri) * real(m, real64), int64))
            if (n > m) n = m
            ! The key-and-select arm allocates two m-element arrays; skip the largest m for the
            ! smallest ratios only if memory would be silly. m = 1e8 needs ~1.6 GB, which is fine.
            call one_case(m, n)
        end do
        write(output_unit, '(a)') ''
    end do

contains

    !> Reads `--key=<int>` from the command line; returns `dflt` when absent.
    function read_arg_int(key, dflt) result(v)
        character(len=*), intent(in) :: key
        integer(int64), intent(in) :: dflt
        integer(int64) :: v
        character(len=64) :: buf
        integer :: k, ios
        v = dflt
        do k = 1, command_argument_count()
            call get_command_argument(k, buf)
            if (index(buf, key) == 1) then
                read(buf(len(key) + 1:), *, iostat=ios) v
                if (ios /= 0) v = dflt
                return
            end if
        end do
    end function read_arg_int

    subroutine one_case(m, n)
        integer(int64), intent(in) :: m, n
        integer(int64), allocatable :: a(:), b(:), c(:)
        real(real64) :: t_sparse, t_dense, t_sort
        integer :: rounds, r
        integer(int64) :: t0, t1, rate
        real(real64) :: best

        call system_clock(count_rate=rate)
        rounds = g_rounds

        allocate(a(n), b(n), c(n))

        best = huge(1.0_real64)
        do r = 1, rounds
            call system_clock(t0)
            call subset_sparse_fy(SEED, m, a)
            call system_clock(t1)
            best = min(best, ms(t0, t1, rate))
        end do
        t_sparse = best

        best = huge(1.0_real64)
        do r = 1, rounds
            call system_clock(t0)
            call subset_dense_fy(SEED, m, b)
            call system_clock(t1)
            best = min(best, ms(t0, t1, rate))
        end do
        t_dense = best

        best = huge(1.0_real64)
        do r = 1, rounds
            call system_clock(t0)
            call subset_key_select(SEED, m, c)
            call system_clock(t1)
            best = min(best, ms(t0, t1, rate))
        end do
        t_sort = best

        ! The two Fisher-Yates arms MUST agree exactly -- that identity is the whole argument for
        ! switching data structures on a threshold.
        call assert_same(a, b, m, n)
        call assert_valid(a, m, 'sparse')
        call assert_valid(c, m, 'key+argsort')

        write(output_unit, '(i9,f9.5,3f14.3,2f13.2)') n, real(n, real64) / real(m, real64), &
            t_sparse, t_dense, t_sort, t_sort / t_sparse, t_sort / t_dense
        flush(output_unit)
        deallocate(a, b, c)
    end subroutine one_case

    real(real64) function ms(t0, t1, rate)
        integer(int64), intent(in) :: t0, t1, rate
        ms = real(t1 - t0, real64) / real(rate, real64) * 1000.0_real64
    end function ms

    !> Sparse partial Fisher-Yates: n draws, O(n) memory.
    subroutine subset_sparse_fy(seed, m, idx)
        integer(int64), intent(in) :: seed, m
        integer(int64), intent(out) :: idx(:)
        integer(int64) :: j, r, n
        n = size(idx, kind=int64)
        call map_init(n)
        do j = 1_int64, n
            r = pf_random_int_at(seed, 0_int64, j, m, j)
            idx(j) = map_get(r, r)
            call map_set(r, map_get(j, j))
        end do
    end subroutine subset_sparse_fy

    !> Dense partial Fisher-Yates: n draws, but an O(m) array (and an O(m) initialisation).
    subroutine subset_dense_fy(seed, m, idx)
        integer(int64), intent(in) :: seed, m
        integer(int64), intent(out) :: idx(:)
        integer(int64), allocatable :: a(:)
        integer(int64) :: j, r, n
        n = size(idx, kind=int64)
        allocate(a(m))
        do j = 1_int64, m
            a(j) = j
        end do
        do j = 1_int64, n
            r = pf_random_int_at(seed, 0_int64, j, m, j)
            idx(j) = a(r)
            a(r) = a(j)
        end do
        deallocate(a)
    end subroutine subset_dense_fy

    !> Key-and-select: one key per candidate, then a single-core partial argsort.
    subroutine subset_key_select(seed, m, idx)
        integer(int64), intent(in) :: seed, m
        integer(int64), intent(out) :: idx(:)
        real(real64), allocatable :: keys(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: n
        n = size(idx, kind=int64)
        allocate(keys(m))
        call pf_random_fill_streams(seed, 1_int64, keys)
        ! Be FAIR to key-and-select: `pf_partial_argsort` is documented to degrade past a full
        ! sort as `n` approaches the size, and it does -- at n = 0.9m it measured 656 ms against a
        ! full argsort's 55 ms on m = 1e6. A real implementation would switch, so this one does.
        if (4_int64 * n >= m) then
            call pf_argsort(keys, perm)
        else
            call pf_partial_argsort(keys, perm, int(n, int32))
        end if
        idx(1:n) = perm(1:n)
        deallocate(keys, perm)
    end subroutine subset_key_select

    ! ---- the sparse map ----

    subroutine map_init(n)
        integer(int64), intent(in) :: n
        integer(int64) :: cap
        cap = 16_int64
        do while (cap < 4_int64 * max(n, 1_int64))
            cap = cap * 2_int64
        end do
        if (allocated(hk)) deallocate(hk, hv)
        allocate(hk(0:cap - 1), hv(0:cap - 1))
        hk = -1_int64
        hmask = cap - 1_int64
    end subroutine map_init

    !> XOR-shift mixing only -- no multiply, so no signed overflow anywhere in the probe.
    pure function slot_of(k, mask) result(h)
        integer(int64), intent(in) :: k, mask
        integer(int64) :: h
        h = k
        h = ieor(h, ishft(h, -31))
        h = ieor(h, ishft(h, -17))
        h = iand(h, mask)
    end function slot_of

    function map_get(k, dflt) result(v)
        integer(int64), intent(in) :: k, dflt
        integer(int64) :: v, h
        h = slot_of(k, hmask)
        do
            if (hk(h) == -1_int64) then
                v = dflt
                return
            else if (hk(h) == k) then
                v = hv(h)
                return
            end if
            h = iand(h + 1_int64, hmask)
        end do
    end function map_get

    subroutine map_set(k, v)
        integer(int64), intent(in) :: k, v
        integer(int64) :: h
        h = slot_of(k, hmask)
        do
            if (hk(h) == -1_int64 .or. hk(h) == k) then
                hk(h) = k
                hv(h) = v
                return
            end if
            h = iand(h + 1_int64, hmask)
        end do
    end subroutine map_set

    ! ---- checks ----

    subroutine assert_same(a, b, m, n)
        integer(int64), intent(in) :: a(:), b(:), m, n
        integer(int64) :: j
        do j = 1_int64, n
            if (a(j) /= b(j)) then
                write(output_unit, '(a,i0,a,i0,a,i0,a,i0)') &
                    'FATAL: sparse and dense Fisher-Yates disagree at j=', j, &
                    ' (m=', m, '): ', a(j), ' vs ', b(j)
                error stop 1
            end if
        end do
    end subroutine assert_same

    subroutine assert_valid(idx, m, what)
        integer(int64), intent(in) :: idx(:), m
        character(len=*), intent(in) :: what
        integer(int64) :: j, n
        logical, allocatable :: seen(:)
        n = size(idx, kind=int64)
        allocate(seen(m))
        seen = .false.
        do j = 1_int64, n
            if (idx(j) < 1_int64 .or. idx(j) > m) then
                write(output_unit, '(a,a,a,i0)') 'FATAL: ', what, ' produced out-of-range ', idx(j)
                error stop 1
            end if
            if (seen(idx(j))) then
                write(output_unit, '(a,a,a,i0)') 'FATAL: ', what, ' produced a duplicate ', idx(j)
                error stop 1
            end if
            seen(idx(j)) = .true.
        end do
        deallocate(seen)
    end subroutine assert_valid

end program probe_random_subset
