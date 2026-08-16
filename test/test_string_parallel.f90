!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> **`parquet_string_column`'s internally-threaded bulk operations.**
!!
!! Separate from `test_parquet_string` for the same reason `test_table_parallel` is separate from
!! `test_table`, and it cannot work any other way: a string operation resolves to ONE thread inside
!! an existing OpenMP parallel region, deliberately, since a nested region is the caller's business.
!! test-drive runs a suite's tests inside its own `!$omp parallel do`, so every test placed in a
!! parallelized suite would compare the serial path against itself and **pass while testing
!! nothing** — this project's worst failure mode, and one that already happened once here (see
!! `feature_string_parallel.md` S4, where two mutations survived for exactly this reason). This
!! suite is excluded from that parallelism in `run_tester.f90`.
!!
!! The tests also write process-global state — `parquet_set_string_threads` and the payload-floor
!! override — which is an independent reason for the exclusion.
!!
!! **Every test here is an A/B against the serial path**, because the threaded and serial forms are
!! genuinely different code (the serial one is a single pass; the threaded one is a three-phase
!! split), and the contract between them is that they produce **byte-identical** columns. An
!! equality assertion is the only thing that holds them to that.
module test_string_parallel
    use parquet
    use iso_fortran_env, only : int64
    use testdrive, only : new_unittest, unittest_type, error_type, check
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_set_num_threads
#endif
    !
    implicit none
    private
    public :: collect_tests_string_parallel
    !
    !> Payload floor low enough that a test-sized column reaches the threaded path.
    integer(int64), parameter :: TINY_FLOOR = 64_int64
    !
    !> OpenMP thread ceiling these tests raise the process to, so the threaded path is reachable
    !! whatever `OMP_NUM_THREADS` says.
    !!
    !! **It must stay at or above `parquet_strings`' private `STRING_MIN_THREADS`** (4 at the time
    !! of writing), which is the break-even below which `bulk_threads` declines to thread at all.
    !! That constant is not visible from here, so this one cannot be derived from it — but the
    !! coupling is self-reporting rather than silent: if `STRING_MIN_THREADS` ever rises above this
    !! value, every `ever_threaded` negative control in this file fires at once, and each of their
    !! messages names this parameter as the thing to raise.
    integer, parameter :: THREADS_FOR_TEST = 8
    !
contains
    !
    !> Registers this suite's tests.
    subroutine collect_tests_string_parallel(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! receives the tests.
        testsuite = [ &
            new_unittest("threaded reindex equals the serial reindex, byte for byte", &
                test_reindex_threaded_equals_serial), &
            new_unittest("threaded reindex places nulls identically across byte boundaries", &
                test_reindex_threaded_nulls), &
            new_unittest("threaded to_character equals the serial to_character", &
                test_to_character_threaded_equals_serial), &
            new_unittest("threaded build_from equals the serial build_from", &
                test_build_from_threaded_equals_serial), &
            new_unittest("threaded gather equals the serial gather", &
                test_gather_threaded_equals_serial), &
            new_unittest("threaded trim_all/strip_all equal the serial ones", &
                test_compact_threaded_equals_serial), &
            new_unittest("threaded delete_by_mask equals the serial delete_by_mask", &
                test_delete_by_mask_threaded_equals_serial), &
            new_unittest("threaded statistics equals the serial statistics", &
                test_statistics_threaded_equals_serial), &
            new_unittest("the thread floor declines below its break-even", test_thread_break_even) &
            ]
    end subroutine collect_tests_string_parallel
    !
    !> Builds a column of `n` elements with deterministic, varying lengths, optionally null every
    !! `null_every`-th element. Lengths vary so the payload split is uneven, which is where an
    !! offset bug shows.
    subroutine build(c, n, null_every)
        type(parquet_string_column), intent(inout) :: c !! receives the column.
        integer(int64), intent(in) :: n                 !! element count.
        integer(int64), intent(in) :: null_every        !! null stride; <= 0 for no nulls.
        integer(int64) :: k, l
        character(len=64) :: buf
        call c%clear()
        do k = 1_int64, n
            if (null_every > 0_int64) then
                if (mod(k, null_every) == 0_int64) then
                    call c%append_null()
                    cycle
                end if
            end if
            l = 1_int64 + mod(k*7_int64, 23_int64)
            buf = repeat(char(ichar("a") + int(mod(k, 26_int64))), int(l))
            call c%append_string(buf(1:int(l)))
        end do
    end subroutine build
    !
    !> Whether two columns are identical in every observable: rows, payload, nulls and every value.
    logical function same_column(a, b) result(res)
        type(parquet_string_column), intent(in) :: a !! first column.
        type(parquet_string_column), intent(in) :: b !! second column.
        character(len=:), allocatable :: sa, sb
        integer(int64) :: k
        res = .false.
        if (a%size() /= b%size()) return
        if (a%character_size() /= b%character_size()) return
        if (a%null_count() /= b%null_count()) return
        do k = 1_int64, a%size()
            if (a%is_null(k) .neqv. b%is_null(k)) return
            if (a%is_null(k)) cycle
            call a%get(k, sa)
            call b%get(k, sb)
            ! Nested rather than `.and.`-ed: Fortran does not short-circuit, so a length test and a
            ! content test on one line would compare two differently-sized strings.
            if (len(sa) /= len(sb)) return
            if (sa /= sb) return
        end do
        res = .true.
    end function same_column
    !
    !> Raises this process's OpenMP thread ceiling to `THREADS_FOR_TEST`, returning the previous
    !! value so the caller can put it back. **Every test whose negative control asserts
    !! `ever_threaded` must call this first.**
    !!
    !! **Why the tests must do this rather than take the environment as they find it.** A bulk
    !! operation's thread count is `min(cap, omp_get_max_threads())`, further reduced to 1 below the
    !! `STRING_MIN_THREADS` break-even — so on a machine or CI runner running with
    !! `OMP_NUM_THREADS` below that break-even, *nothing the library exposes can reach the threaded
    !! path*: neither `parquet_set_string_threads` nor the payload-floor override, because both are
    !! bounded by that same ceiling. The eight A/B tests here would then compare the serial path
    !! with itself, their negative controls would fire, and the suite would report eight failures
    !! that mean "this machine could not run the test" while looking exactly like "the threaded
    !! rebuild is broken". Measured before this existed: 8 failures at `OMP_NUM_THREADS` of 1, 2 and
    !! 3, and none at 4.
    !!
    !! `omp_set_num_threads` is the one lever that works, because it writes the `nthreads-var` ICV
    !! that `omp_get_max_threads()` reads — `OMP_NUM_THREADS` only supplies that ICV's *initial*
    !! value, so a test may raise it afterwards. This does not weaken any assertion: the arms still
    !! compare a genuinely threaded rebuild against a genuinely serial one, and `ever_threaded`
    !! still fails the test if the threaded arm silently declined. It removes a dependency on the
    !! ambient environment, which on a 4+-thread machine was being satisfied by luck.
    !!
    !! Writing a process-global ICV is safe here for the same reason the suite may write
    !! `parquet_set_string_threads` at all: `string_parallel` is excluded from test-drive's own
    !! `!$omp parallel do` in `run_tester.f90`. Do not copy this into a parallelized suite.
    !!
    !! Under a build without OpenMP this is a no-op and the threaded path does not exist to be
    !! tested — see `collect_tests_string_parallel` for how that case is handled.
    integer function borrow_threads() result(saved)
        saved = 1
#ifdef _OPENMP
        saved = omp_get_max_threads()
        if (saved < THREADS_FOR_TEST) call omp_set_num_threads(THREADS_FOR_TEST)
#endif
    end function borrow_threads
    !
    !> Restores what `borrow_threads` replaced, so the ceiling does not leak into later suites.
    !!
    !! Called on each test's success path only. An assertion failure returns early and leaves the
    !! ceiling raised, which is deliberate: the run is already red at that point, and adding a
    !! restore to every early return would put cleanup between an assertion and its `return` in
    !! twenty-odd places for no benefit a failing run can use.
    subroutine return_threads(saved)
        integer, intent(in) :: saved                     !! value `borrow_threads` reported.
#ifdef _OPENMP
        if (saved < THREADS_FOR_TEST) call omp_set_num_threads(saved)
#else
        associate (unused => saved); end associate
#endif
    end subroutine return_threads
    !
    !> **The core contract: the threaded rebuild and the serial one produce identical columns.**
    !!
    !! Both arms reindex the same source with the same permutation; one is forced serial with
    !! `parquet_set_string_threads(1)`, the other left automatic with the payload floor lowered so it
    !! actually threads. The **negative control is asserting that the threaded arm really did
    !! thread** — without it, a floor or a gate that silently declined would make this compare the
    !! serial path against itself and pass while testing nothing.
    !!
    !! Row counts are swept across byte boundaries (not multiples of 8) because the validity split is
    !! byte-aligned and an off-by-one there is invisible at round numbers.
    subroutine test_reindex_threaded_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, ser, par
        integer(int64), allocatable :: perm(:)
        integer(int64) :: n, k
        integer :: threaded_n
        logical :: ever_threaded
        integer :: saved_threads
        !
        ever_threaded = .false.
        saved_threads = borrow_threads()
        do n = 1000_int64, 1007_int64
            call build(src, n, 0_int64)
            allocate(perm(n))
            do k = 1_int64, n
                perm(k) = n - k + 1_int64       ! reversal: every element moves
            end do
            !
            call parquet_debug_set_string_min_bytes(0_int64)
            call parquet_set_string_threads(1)
            ser = src%clone()
            call ser%reindex_trusted(perm)
            !
            call parquet_set_string_threads(0)
            call parquet_debug_set_string_min_bytes(TINY_FLOOR)
            par = src%clone()
            threaded_n = parquet_debug_string_bulk_threads(par)
            if (threaded_n > 1) ever_threaded = .true.
            call par%reindex_trusted(perm)
            !
            call parquet_debug_set_string_min_bytes(0_int64)
            call check(error, same_column(ser, par), "threaded reindex must equal the serial reindex")
            if (allocated(error)) return
            call check(error, par%validate(), "the threaded result satisfies the class invariants")
            if (allocated(error)) return
            deallocate(perm)
        end do
        call return_threads(saved_threads)
        call check(error, ever_threaded, &
            "negative control: at least one arm must actually have threaded, or this compares serial with serial. " // &
            "If this fires on every test in this suite at once, THREADS_FOR_TEST is below STRING_MIN_THREADS")
    end subroutine test_reindex_threaded_equals_serial
    !
    !> **`%statistics` is threaded too, and it is the one whose wrong answer is quietest.**
    !!
    !! The other six threaded operations rebuild a column, so a bad split shows up as wrong bytes or
    !! a wrong row count. This one only computes `min_len`/`max_len`, via an OpenMP `reduction` over
    !! `thread_row_ranges` — so a mis-seeded `lo`/`hi` sentinel, or a range split that skipped a row,
    !! returns a plausible number and corrupts nothing. Nothing downstream would notice.
    !!
    !! Same shape as its six siblings: one arm forced serial, one with the payload floor lowered so
    !! it really threads, and `ever_threaded` as the **negative control** — without it a gate that
    !! silently declined would make this compare the serial path against itself.
    !!
    !! **The fixture is the whole test, and the obvious one is worthless.** `build`'s lengths cycle
    !! through a small set, so every length occurs hundreds of times and the extremes survive any
    !! number of dropped rows — a deliberate "each thread skips its last row" mutation passed
    !! against exactly that fixture. So this test instead gives the column a UNIQUE longest and a
    !! UNIQUE shortest element and parks each one on a thread-range boundary, asking
    !! `parquet_debug_string_row_ranges` where those boundaries actually fall rather than guessing.
    !! Losing either row then changes the answer, and only in the threaded arm.
    !!
    !! Nulls are included, at a stride coprime with 8 so they land at every bit position within a
    !! validity byte: the loop reads `bit_valid` per row and skips nulls, so a split that lost a row
    !! would most easily lose it at a byte boundary. The two planted extremes are kept non-null.
    subroutine test_statistics_threaded_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer(int64) :: n, s_min, s_max, p_min, p_max, s_rows, p_rows, s_nulls, p_nulls
        integer(int64) :: stride, k
        integer(int64), allocatable :: rlo(:), rhi(:)
        integer :: threaded_n, t
        logical :: ever_threaded
        integer :: saved_threads
        !
        ever_threaded = .false.
        saved_threads = borrow_threads()
        do stride = 3_int64, 7_int64, 2_int64
            do n = 1000_int64, 1005_int64
                call build(col, n, stride)
                !
                ! Where would the threaded arm split this column? Plant the unique extremes on
                ! those boundaries, so a range that runs one row short loses one of them.
                call parquet_set_string_threads(0)
                call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                threaded_n = parquet_debug_string_bulk_threads(col)
                if (threaded_n > 1) then
                    ever_threaded = .true.
                    call parquet_debug_string_row_ranges(n, threaded_n, rlo, rhi)
                    do t = 1, threaded_n
                        if (rlo(t) > rhi(t)) cycle          ! a trailing thread may get no rows
                        if (t == 1) then
                            call col%set(rhi(t), repeat("z", 97))   ! unique longest, at a boundary
                        else if (t == 2) then
                            call col%set(rhi(t), "")                ! unique shortest, at the next
                        end if
                    end do
                    ! Both planted rows must be readable, not null, or they cannot be the extremes.
                    do t = 1, min(2, threaded_n)
                        if (rlo(t) <= rhi(t)) then
                            if (col%is_null(rhi(t))) call col%set(rhi(t), "q")
                        end if
                    end do
                end if
                !
                call parquet_debug_set_string_min_bytes(0_int64)
                call parquet_set_string_threads(1)
                call col%statistics(nrows=s_rows, n_null=s_nulls, min_len=s_min, max_len=s_max)
                !
                call parquet_set_string_threads(0)
                call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                call col%statistics(nrows=p_rows, n_null=p_nulls, min_len=p_min, max_len=p_max)
                !
                call parquet_debug_set_string_min_bytes(0_int64)
                call check(error, s_min == p_min .and. s_max == p_max, &
                    "threaded statistics must report the same min_len/max_len as the serial one")
                if (allocated(error)) return
                call check(error, s_rows == p_rows .and. s_nulls == p_nulls, &
                    "threaded statistics must report the same row and null counts as the serial one")
                if (allocated(error)) return
                ! The planted maximum must actually be the maximum, or the fixture has stopped
                ! doing its job and the comparison above proves nothing.
                if (threaded_n > 1) then
                    call check(error, s_max == 97_int64, &
                        "fixture check: the planted unique longest element must be the column maximum")
                    if (allocated(error)) return
                end if
                k = 0_int64
            end do
        end do
        call check(error, ever_threaded, &
            "negative control: at least one arm must actually have threaded, or this compares serial with serial. " // &
            "If this fires on every test in this suite at once, THREADS_FOR_TEST is below STRING_MIN_THREADS")
        if (allocated(error)) return
        !
        ! An ALL-NULL column is the one input where the reduction's `hi = -1` seed is load-bearing:
        ! no element is ever visited, so the post-loop `hi < 0` branch is the only thing that turns
        ! the untouched `lo = huge(int64)` into 0. Found by mutation — re-seeding `hi` to 0 leaves
        ! every other case identical and makes min_len report huge(int64) here.
        call col%clear()
        do k = 1_int64, 1000_int64
            call col%append_null()
        end do
        call parquet_debug_set_string_min_bytes(TINY_FLOOR)
        call parquet_set_string_threads(0)
        call col%statistics(nrows=p_rows, n_null=p_nulls, min_len=p_min, max_len=p_max)
        call parquet_debug_set_string_min_bytes(0_int64)
        call check(error, p_min == 0_int64 .and. p_max == 0_int64, &
            "an all-null column must report min_len/max_len of 0, not the reduction's seed")
        if (allocated(error)) return
        call return_threads(saved_threads)
        call check(error, p_rows == 1000_int64 .and. p_nulls == 1000_int64, &
            "an all-null column must still report its row and null counts")
    end subroutine test_statistics_threaded_equals_serial
    !
    !> **The validity bitmap is the one thing a wrong split corrupts silently.**
    !!
    !! Eight rows share a byte, so two threads meeting inside one would lose each other's writes and
    !! the column would still validate — just with the wrong rows null. This drives null strides that
    !! are coprime with 8 (3, 5, 7) so nulls land at every bit position within a byte, over row
    !! counts that are not multiples of 8, and compares against the serial arm element by element.
    subroutine test_reindex_threaded_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, ser, par
        integer(int64), allocatable :: perm(:)
        integer(int64) :: n, k, stride
        logical :: ever_threaded
        integer :: saved_threads
        !
        ever_threaded = .false.
        saved_threads = borrow_threads()
        do stride = 3_int64, 7_int64, 2_int64
            do n = 997_int64, 1000_int64
                call build(src, n, stride)
                allocate(perm(n))
                do k = 1_int64, n
                    ! A stride-13 rotation rather than a reversal, so a null at bit p lands at a
                    ! different bit q in the result -- a byte-boundary bug cannot cancel out.
                    perm(k) = 1_int64 + mod(k*13_int64, n)
                end do
                !
                call parquet_debug_set_string_min_bytes(0_int64)
                call parquet_set_string_threads(1)
                ser = src%clone()
                call ser%reindex_trusted(perm)
                !
                call parquet_set_string_threads(0)
                call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                par = src%clone()
                if (parquet_debug_string_bulk_threads(par) > 1) ever_threaded = .true.
                call par%reindex_trusted(perm)
                call parquet_debug_set_string_min_bytes(0_int64)
                !
                call check(error, ser%null_count() == par%null_count(), &
                    "threaded and serial reindex must agree on the null COUNT")
                if (allocated(error)) return
                call check(error, same_column(ser, par), &
                    "threaded and serial reindex must agree on which rows are null, and on every value")
                if (allocated(error)) return
                call check(error, par%validate(), "the threaded null result satisfies the class invariants")
                if (allocated(error)) return
                deallocate(perm)
            end do
        end do
        call return_threads(saved_threads)
        call check(error, ever_threaded, "negative control: the null arms must actually have threaded " // &
            "(if every test in this suite fires at once, THREADS_FOR_TEST is below STRING_MIN_THREADS)")
    end subroutine test_reindex_threaded_nulls
    !
    !> **`to_character`'s fill loop threads with no serial twin, so this asserts a different thing
    !! from the reindex tests above.**
    !!
    !! There is no alternative code path to diff against — with one thread the same loop runs — so
    !! what a race would corrupt here is the *content* of `out`, and the reference is the same call
    !! made serially. Both the abort-on-null form and the `null_value=` form are swept, because they
    !! take different arms inside the threaded loop and only one of them writes `out(i)` whole.
    subroutine test_to_character_threaded_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: ser(:), par(:)
        integer(int64) :: n, stride
        logical :: ever_threaded
        integer :: saved_threads
        !
        ever_threaded = .false.
        saved_threads = borrow_threads()
        do stride = 0_int64, 5_int64, 5_int64      ! 0 = no nulls, 5 = every 5th null
            do n = 1021_int64, 1024_int64
                call build(col, n, stride)
                !
                call parquet_debug_set_string_min_bytes(0_int64)
                call parquet_set_string_threads(1)
                if (stride > 0_int64) then
                    call col%to_character(ser, null_value="<none>")
                else
                    call col%to_character(ser)
                end if
                !
                call parquet_set_string_threads(0)
                call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                if (parquet_debug_string_bulk_threads(col) > 1) ever_threaded = .true.
                if (stride > 0_int64) then
                    call col%to_character(par, null_value="<none>")
                else
                    call col%to_character(par)
                end if
                call parquet_debug_set_string_min_bytes(0_int64)
                !
                ! Nested rather than `.and.`-ed: Fortran does not short-circuit, so a shape test and
                ! a content test on one line would compare mismatched arrays.
                call check(error, size(ser) == size(par), "both arms must materialize the same row count")
                if (allocated(error)) return
                call check(error, len(ser) == len(par), &
                    "both arms must pad to the same length, or the maxlen scan disagreed")
                if (allocated(error)) return
                call check(error, all(ser == par), &
                    "threaded to_character must produce byte-identical elements to the serial one")
                if (allocated(error)) return
            end do
        end do
        call return_threads(saved_threads)
        call check(error, ever_threaded, &
            "negative control: at least one arm must actually have threaded, or this compares serial with serial. " // &
            "If this fires on every test in this suite at once, THREADS_FOR_TEST is below STRING_MIN_THREADS")
    end subroutine test_to_character_threaded_equals_serial
    !
    !> **`gather` keeps a serial twin because the phased shape measured 1.5x SLOWER on one thread**,
    !! so this is an A/B between two genuinely different rebuilds, like the reindex tests above.
    !!
    !! Unlike a reindex, a gather may drop elements, repeat them and change the row count, so the
    !! null *count* is recomputed rather than carried — which is the part a wrong split corrupts
    !! silently. The index lists here deliberately do all three: a subset, a reversal, and one with
    !! repeats.
    subroutine test_gather_threaded_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, ser, par
        integer(int64), allocatable :: idx(:)
        integer(int64) :: n, k, m, mode, stride
        logical :: ever_threaded
        integer :: saved_threads
        !
        ever_threaded = .false.
        saved_threads = borrow_threads()
        do stride = 0_int64, 3_int64, 3_int64
            do mode = 1_int64, 3_int64
                do n = 1021_int64, 1022_int64
                    call build(src, n, stride)
                    select case (int(mode))
                    case (1)                                   ! a strict subset, in order
                        m = n/2_int64
                        allocate(idx(m))
                        do k = 1_int64, m
                            idx(k) = 2_int64*k
                        end do
                    case (2)                                   ! everything, reversed
                        m = n
                        allocate(idx(m))
                        do k = 1_int64, m
                            idx(k) = n - k + 1_int64
                        end do
                    case default                               ! longer than the source, with repeats
                        m = n + n/3_int64
                        allocate(idx(m))
                        do k = 1_int64, m
                            idx(k) = 1_int64 + mod(k*7_int64, n)
                        end do
                    end select
                    !
                    call parquet_debug_set_string_min_bytes(0_int64)
                    call parquet_set_string_threads(1)
                    ser = src%clone()
                    call ser%gather(idx)
                    !
                    call parquet_set_string_threads(0)
                    call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                    par = src%clone()
                    if (parquet_debug_string_bulk_threads(par) > 1) ever_threaded = .true.
                    call par%gather(idx)
                    call parquet_debug_set_string_min_bytes(0_int64)
                    !
                    call check(error, ser%size() == par%size(), "both gathers must produce the same row count")
                    if (allocated(error)) return
                    call check(error, ser%null_count() == par%null_count(), &
                        "both gathers must RECOUNT the same number of nulls")
                    if (allocated(error)) return
                    call check(error, same_column(ser, par), &
                        "threaded gather must equal the serial gather in every element and null")
                    if (allocated(error)) return
                    call check(error, par%validate(), "the threaded gather satisfies the class invariants")
                    if (allocated(error)) return
                    deallocate(idx)
                end do
            end do
        end do
        call return_threads(saved_threads)
        call check(error, ever_threaded, "negative control: at least one gather arm must have threaded " // &
            "(if every test in this suite fires at once, THREADS_FOR_TEST is below STRING_MIN_THREADS)")
    end subroutine test_gather_threaded_equals_serial
    !
    !> **`build_from`'s threaded fill is a genuinely different shape from its serial one**, so this
    !! is an A/B in the same sense as the reindex tests: one loop against three phases plus a scan.
    !!
    !! The handles are taken from a column with nulls, so the run exercises the phase that writes
    !! validity bits under the byte-aligned split — the one whose failure is silent.
    subroutine test_build_from_threaded_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, ser, par
        type(parquet_string), allocatable :: h(:)
        integer(int64) :: n, stride
        logical :: ever_threaded
        integer :: saved_threads
        !
        ever_threaded = .false.
        saved_threads = borrow_threads()
        do stride = 0_int64, 3_int64, 3_int64
            do n = 1021_int64, 1024_int64
                call build(src, n, stride)
                allocate(h(n))
                call src%view_all(h)
                !
                call parquet_debug_set_string_min_bytes(0_int64)
                call parquet_set_string_threads(1)
                call ser%build_from(h)
                !
                call parquet_set_string_threads(0)
                call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                if (parquet_debug_string_bulk_threads(src) > 1) ever_threaded = .true.
                call par%build_from(h)
                call parquet_debug_set_string_min_bytes(0_int64)
                !
                call check(error, same_column(ser, par), &
                    "threaded build_from must equal the serial build_from in every element and null")
                if (allocated(error)) return
                call check(error, par%validate(), "the threaded gather satisfies the class invariants")
                if (allocated(error)) return
                deallocate(h)
            end do
        end do
        call return_threads(saved_threads)
        call check(error, ever_threaded, "negative control: at least one build_from arm must have threaded " // &
            "(if every test in this suite fires at once, THREADS_FOR_TEST is below STRING_MIN_THREADS)")
    end subroutine test_build_from_threaded_equals_serial
    !
    !> Builds a column whose elements carry leading and/or trailing blanks in every combination, so
    !! that `trim_all` and `strip_all` actually have something to remove and remove different amounts.
    !!
    !! **An all-blank element and a zero-length element are both included on purpose**: the first is
    !! the case where compaction takes an element to length 0, which is where a length computed in
    !! one phase and a copy performed in another can disagree; the second is where the leading-blank
    !! rescan in the threaded copy would run off the end if it were ever reached with `elen == 0`.
    subroutine build_padded(c, n, null_every)
        type(parquet_string_column), intent(inout) :: c !! receives the column.
        integer(int64), intent(in) :: n                 !! element count.
        integer(int64), intent(in) :: null_every        !! null stride; <= 0 for none.
        integer(int64) :: k, body
        character(len=48) :: buf
        integer :: lead, trail
        call c%clear()
        do k = 1_int64, n
            if (null_every > 0_int64) then
                if (mod(k, null_every) == 0_int64) then
                    call c%append_null()
                    cycle
                end if
            end if
            select case (int(mod(k, 6_int64)))
            case (0)
                call c%append_string("")                       ! zero length
                cycle
            case (1)
                call c%append_string("    ")                   ! all blanks -> length 0 after either
                cycle
            end select
            lead = int(mod(k, 4_int64))
            trail = int(mod(k, 3_int64))
            body = 1_int64 + mod(k*5_int64, 9_int64)
            buf = repeat(" ", lead) // repeat(char(ichar("a") + int(mod(k, 26_int64))), int(body)) &
                // repeat(" ", trail)
            call c%append_string(buf(1:lead+int(body)+trail))
        end do
    end subroutine build_padded
    !
    !> **`trim_all`/`strip_all` thread by rebuilding into fresh buffers; the serial form compacts in
    !! place.** Two genuinely different shapes, so this is an A/B in the same sense as the reindex
    !! tests, and the fixture has to contain blanks or neither arm does any work.
    !!
    !! Both operations are swept because they differ in one clause — `strip_all` also removes leading
    !! blanks, which is the only thing the threaded copy has to recompute rather than read from the
    !! prefix sum, and therefore the one place the two phases can disagree.
    subroutine test_compact_threaded_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, ser, par
        integer(int64) :: n, stride
        integer :: op
        logical :: ever_threaded
        integer :: saved_threads
        !
        ever_threaded = .false.
        saved_threads = borrow_threads()
        do stride = 0_int64, 5_int64, 5_int64
            do op = 1, 2
                do n = 1021_int64, 1024_int64
                    call build_padded(src, n, stride)
                    !
                    call parquet_debug_set_string_min_bytes(0_int64)
                    call parquet_set_string_threads(1)
                    ser = src%clone()
                    if (op == 1) then
                        call ser%trim_all()
                    else
                        call ser%strip_all()
                    end if
                    !
                    call parquet_set_string_threads(0)
                    call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                    par = src%clone()
                    if (parquet_debug_string_bulk_threads(par) > 1) ever_threaded = .true.
                    if (op == 1) then
                        call par%trim_all()
                    else
                        call par%strip_all()
                    end if
                    call parquet_debug_set_string_min_bytes(0_int64)
                    !
                    call check(error, ser%character_size() == par%character_size(), &
                        "both arms must compact to the same payload size")
                    if (allocated(error)) return
                    call check(error, same_column(ser, par), &
                        "threaded trim/strip must equal the serial one in every element and null")
                    if (allocated(error)) return
                    call check(error, par%validate(), "the threaded compaction satisfies the class invariants")
                    if (allocated(error)) return
                end do
            end do
        end do
        call return_threads(saved_threads)
        call check(error, ever_threaded, "negative control: at least one compaction arm must have threaded " // &
            "(if every test in this suite fires at once, THREADS_FOR_TEST is below STRING_MIN_THREADS)")
    end subroutine test_compact_threaded_equals_serial
    !
    !> **`delete_by_mask` is the only rebuild here whose OUTPUT ROW COUNT differs from its input's**,
    !! so its threaded form carries two cursors and derives per-thread bases rather than a
    !! per-element prefix sum. A base computed one row out shifts every later element of that thread's
    !! range — in both the payload and the offsets — so the masks below vary how many rows each
    !! thread's range contributes.
    !!
    !! Keep-all and keep-none are included because they are the degenerate ends of that arithmetic:
    !! keep-none leaves a zero-row column whose buffers still have to be well formed.
    subroutine test_delete_by_mask_threaded_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, ser, par
        logical, allocatable :: keep(:)
        integer(int64) :: n, k, stride
        integer :: mode
        logical :: ever_threaded
        integer :: saved_threads
        !
        ever_threaded = .false.
        saved_threads = borrow_threads()
        do stride = 0_int64, 3_int64, 3_int64
            do mode = 1, 4
                do n = 1021_int64, 1022_int64
                    call build_padded(src, n, stride)
                    allocate(keep(n))
                    select case (mode)
                    case (1)
                        keep = .true.                                  ! keep everything
                    case (2)
                        keep = .false.                                 ! keep nothing
                    case (3)
                        do k = 1_int64, n
                            keep(k) = mod(k, 2_int64) == 0_int64       ! every other row
                        end do
                    case default
                        do k = 1_int64, n
                            ! Uneven: whole stretches survive and whole stretches do not, so the
                            ! per-thread row counts differ sharply from each other.
                            keep(k) = mod(k/37_int64, 3_int64) /= 0_int64
                        end do
                    end select
                    !
                    call parquet_debug_set_string_min_bytes(0_int64)
                    call parquet_set_string_threads(1)
                    ser = src%clone()
                    call ser%delete_by_mask(keep)
                    !
                    call parquet_set_string_threads(0)
                    call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                    par = src%clone()
                    if (parquet_debug_string_bulk_threads(par) > 1) ever_threaded = .true.
                    call par%delete_by_mask(keep)
                    call parquet_debug_set_string_min_bytes(0_int64)
                    !
                    call check(error, ser%size() == par%size(), &
                        "both arms must keep the same number of rows")
                    if (allocated(error)) return
                    call check(error, ser%size() == count(keep), &
                        "the surviving row count must equal the mask's own count")
                    if (allocated(error)) return
                    call check(error, ser%null_count() == par%null_count(), &
                        "both arms must recount the surviving nulls identically")
                    if (allocated(error)) return
                    call check(error, same_column(ser, par), &
                        "threaded delete_by_mask must equal the serial one in every element and null")
                    if (allocated(error)) return
                    ! Both arms, not just the threaded one: they share `rebuild_validity_compacted`,
                    ! so a defect in it corrupts them identically and the equality check above cannot
                    ! see it -- only the class invariant can.
                    call check(error, ser%validate(), "the serial deletion satisfies the class invariants")
                    if (allocated(error)) return
                    call check(error, par%validate(), "the threaded deletion satisfies the class invariants")
                    if (allocated(error)) return
                    deallocate(keep)
                end do
            end do
        end do
        call return_threads(saved_threads)
        call check(error, ever_threaded, "negative control: at least one delete_by_mask arm must have threaded " // &
            "(if every test in this suite fires at once, THREADS_FOR_TEST is below STRING_MIN_THREADS)")
    end subroutine test_delete_by_mask_threaded_equals_serial
    !
    !> The thread floor declines below its break-even rather than running a slower shape on two
    !! threads.
    !!
    !! Making a rebuild splittable costs about 1.7x serially, so the parallelism has to beat that
    !! before it is worth doing at all — two threads measured as a net loss. This asserts the gate
    !! implements that, with the automatic answer as the negative control.
    subroutine test_thread_break_even(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer :: n_auto, n_two, n_three
        !
        call build(col, 2000_int64, 0_int64)
        call parquet_debug_set_string_min_bytes(TINY_FLOOR)
        !
        call parquet_set_string_threads(0)
        n_auto = parquet_debug_string_bulk_threads(col)
        call parquet_set_string_threads(2)
        n_two = parquet_debug_string_bulk_threads(col)
        call parquet_set_string_threads(3)
        n_three = parquet_debug_string_bulk_threads(col)
        !
        call parquet_set_string_threads(0)
        call parquet_debug_set_string_min_bytes(0_int64)
        !
        call check(error, n_two == 1, "a cap of 2 is below the break-even and must resolve to serial")
        if (allocated(error)) return
        call check(error, n_three == 1, "a cap of 3 is below the break-even and must resolve to serial")
        if (allocated(error)) return
        if (n_auto > 1) then
            call check(error, n_auto >= 4, &
                "negative control: when it does thread, it uses at least the break-even count")
        end if
    end subroutine test_thread_break_even
    !
end module test_string_parallel
