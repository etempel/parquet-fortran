!> Maintainer probe for the library's permutation API (`pf_random_perm_at`,
!! `pf_random_permutation`, `pf_random_subset`).
!!
!! Not a test and never run by `fpm test`: it exists to answer the two questions the suite cannot
!! afford to ask at scale -- what the bulk form costs per element against the scalar one, and
!! whether they still agree over populations far larger than a unit test may allocate. See
!! CONTRIBUTING.md's "Other tools/ helpers".
!!
!! Modes:
!!   --mode=check   identity (scalar vs bulk vs subset) and bijectivity over a sweep of `m`
!!   --mode=bench   per-element cost of the scalar form and of the bulk form, serial and automatic
!!   --mode=floor   wall ns/element by explicit thread count, for BOTH the permutation and the
!!                  subset -- what sets the work floor's default, and the trio's thread-scaling
!!                  obligation --mode=guarantee what coordinate addressing COSTS: the shipped
!!   permutation against a stream-driven Fisher-Yates (the cheapest shuffle there is, and unusable
!!                  in a parallel loop) and against key-and-argsort. The comparison is restated
!!                  here -- its original phrasing became self-referential when the construction
!!                  stopped being a Fisher-Yates.
!!
!! Always build with `--profile release`; a default-profile run measures nothing (see CLAUDE.md,
!! "Manual (never-`fpm test`) large-scale/benchmark tools").
program probe_random_perm

    use iso_fortran_env, only: int64, real64, output_unit
    use parquet_random, only: pf_random_stream, pf_random_fill_draws
    use parquet_sampling, only: pf_random_perm_at, pf_random_permutation, pf_random_subset, &
                                pf_random_perm_algorithm
    use parquet_sorting, only: pf_argsort
    use parquet_settings, only: parquet_set_random_parallel_min_elements, parquet_reset_settings

    implicit none

    character(len=32) :: mode
    integer(int64) :: seed

    mode = "check"
    seed = 20260817_int64
    call parse_args(mode, seed)

    write (output_unit, '(a)') "contract: " // pf_random_perm_algorithm
    select case (trim(mode))
    case ("check")
        call run_check(seed)
    case ("bench")
        call run_bench(seed)
    case ("floor")
        call run_floor(seed)
    case ("guarantee")
        call run_guarantee(seed)
    case default
        write (output_unit, '(a)') "unknown --mode="  // trim(mode)
    end select

contains

    !> Reads `--mode=` and `--seed=` from the command line.
    subroutine parse_args(m, s)
        character(len=*), intent(inout) :: m        !! mode name
        integer(int64), intent(inout) :: s          !! seed
        character(len=256) :: arg
        integer :: i, ios
        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            if (arg(1:7) == "--mode=") then
                m = trim(arg(8:))
            else if (arg(1:7) == "--seed=") then
                read (arg(8:), *, iostat=ios) s
            end if
        end do
    end subroutine parse_args

    !> Identity and bijectivity over a sweep of population sizes.
    subroutine run_check(s)
        integer(int64), intent(in) :: s             !! seed
        integer(int64), parameter :: sizes(*) = [1_int64, 2_int64, 3_int64, 5_int64, 7_int64, &
                                                 16_int64, 100_int64, 999_int64, 1000_int64, &
                                                 65536_int64, 1000003_int64]
        integer(int64), allocatable :: perm(:), sub(:)
        logical, allocatable :: seen(:)
        integer(int64) :: m, k, n, bad_ident, bad_sub, bad_range, bad_dup
        integer :: si
        write (output_unit, '(a)') "        m   ident   subset    range      dup"
        do si = 1, size(sizes)
            m = sizes(si)
            allocate (perm(m), seen(m))
            call pf_random_permutation(perm, s)
            bad_ident = 0_int64
            do k = 1_int64, m
                if (perm(k) /= pf_random_perm_at(s, m, k)) bad_ident = bad_ident + 1_int64
            end do
            bad_range = count(perm < 1_int64 .or. perm > m, kind=int64)
            seen = .false.
            bad_dup = 0_int64
            do k = 1_int64, m
                if (perm(k) >= 1_int64 .and. perm(k) <= m) then
                    if (seen(perm(k))) bad_dup = bad_dup + 1_int64
                    seen(perm(k)) = .true.
                end if
            end do
            n = max(1_int64, m / 3_int64)
            allocate (sub(n))
            call pf_random_subset(sub, m, s)
            bad_sub = count(sub /= perm(1:n), kind=int64)
            write (output_unit, '(i9,4i9)') m, bad_ident, bad_sub, bad_range, bad_dup
            deallocate (perm, seen, sub)
        end do
    end subroutine run_check

    !> Wall-clock seconds. **Not `cpu_time`**, which sums across threads: it reported the automatic
    !! bulk arm at 3786 ns/element where the wall figure is a fraction of a nanosecond, i.e. it
    !! turned the best result in the table into by far the worst.
    real(real64) function wall() result(t)
        integer(int64) :: c, rate
        call system_clock(c, rate)
        t = real(c, real64) / real(rate, real64)
    end function wall

    !> Per-element cost of the scalar form and of the bulk form, serial and automatically threaded.
    subroutine run_bench(s)
        integer(int64), intent(in) :: s             !! seed
        integer(int64), parameter :: sizes(*) = [1000_int64, 100000_int64, 10000000_int64, &
                                                 100000000_int64]
        integer(int64), allocatable :: perm(:), ref(:)
        integer(int64) :: m, k, guard
        integer :: si, rep
        real(real64) :: t0, scalar_ns, ser_ns, par_ns
        logical :: same
        write (output_unit, '(a)') "        m   scalar_ns  bulk1_ns  bulkN_ns   ser/par  identical"
        do si = 1, size(sizes)
            m = sizes(si)
            allocate (perm(m), ref(m))
            ! Warm both destinations' pages before anything is timed -- a fresh large array pays
            ! first-touch faults once, and whichever arm runs first would otherwise absorb them.
            perm = 0_int64
            ref = 0_int64
            scalar_ns = huge(1.0_real64)
            ser_ns = huge(1.0_real64)
            par_ns = huge(1.0_real64)
            do rep = 1, 3
                t0 = wall()
                do k = 1_int64, m
                    perm(k) = pf_random_perm_at(s, m, k)
                end do
                scalar_ns = min(scalar_ns, wall() - t0)
                t0 = wall()
                call pf_random_permutation(ref, s, threads=1)
                ser_ns = min(ser_ns, wall() - t0)
                t0 = wall()
                call pf_random_permutation(perm, s)
                par_ns = min(par_ns, wall() - t0)
            end do
            ! The claim `threads=` rests on: the thread count changes the time and nothing else.
            same = all(perm == ref)
            guard = sum(perm)
            write (output_unit, '(i9,3f10.3,f10.2,4x,l1,i21)') m, &
                scalar_ns * 1.0e9_real64 / real(m, real64), &
                ser_ns * 1.0e9_real64 / real(m, real64), &
                par_ns * 1.0e9_real64 / real(m, real64), &
                ser_ns / max(par_ns, tiny(1.0_real64)), same, guard
            deallocate (perm, ref)
        end do
    end subroutine run_bench

    !> Wall ns per element against an EXPLICIT thread count, over a size ladder.
    !!
    !! This is what decides `parquet_set_random_parallel_min_elements`' default, and it has to be
    !! measured on the machine rather than reasoned about: the work floor is a statement about team
    !! startup cost, which scales with the team, so a floor derived from a 16-thread run understates
    !! it badly on a 384-processor two-socket machine. The `threads=` argument is passed explicitly
    !! and the floor is disabled first, or the thing being measured would decline to happen.
    subroutine run_floor(s)
        integer(int64), intent(in) :: s             !! seed
        integer(int64), parameter :: sizes(*) = [1000_int64, 10000_int64, 100000_int64, &
                                                 1000000_int64, 10000000_int64]
        integer, parameter :: teams(*) = [1, 2, 4, 8, 16, 32, 64]
        integer(int64), allocatable :: perm(:), ref(:)
        integer(int64) :: m
        integer :: si, ti, rep
        real(real64) :: t0, best(size(teams))
        logical :: same
        call parquet_set_random_parallel_min_elements(0)   ! measure the team, not the guard
        write (output_unit, '(a)', advance="no") "        m"
        do ti = 1, size(teams)
            write (output_unit, '(i9)', advance="no") teams(ti)
        end do
        write (output_unit, '(a)') "   identical   (wall ns/element by thread count)"
        do si = 1, size(sizes)
            m = sizes(si)
            allocate (perm(m), ref(m))
            perm = 0_int64
            ref = 0_int64
            call pf_random_permutation(ref, s, threads=1)
            same = .true.
            do ti = 1, size(teams)
                best(ti) = huge(1.0_real64)
                do rep = 1, 5
                    t0 = wall()
                    call pf_random_permutation(perm, s, threads=teams(ti))
                    best(ti) = min(best(ti), wall() - t0)
                end do
                if (any(perm /= ref)) same = .false.
            end do
            write (output_unit, '(i9)', advance="no") m
            do ti = 1, size(teams)
                write (output_unit, '(f9.3)', advance="no") best(ti) * 1.0e9_real64 / real(m, real64)
            end do
            write (output_unit, '(4x,l1)') same
            deallocate (perm, ref)
        end do
        call parquet_reset_settings()
        call run_subset_floor(s)
    end subroutine run_floor

    !> Thread scaling for `pf_random_subset`, the trio's other shipped member.
    !!
    !! Separate from `run_floor`'s permutation ladder because the interesting axis is different: a
    !! subset's cost is `O(size(idx))` and independent of `m`, so what has to be shown is that the
    !! population may be astronomically larger than the sample without changing anything. `m` is
    !! fixed at 10**15 here -- a population no Fisher-Yates could allocate, let alone shuffle.
    subroutine run_subset_floor(s)
        integer(int64), intent(in) :: s             !! seed
        integer(int64), parameter :: sizes(*) = [1000_int64, 10000_int64, 100000_int64, &
                                                 1000000_int64, 10000000_int64]
        integer(int64), parameter :: MPOP = 1000000000000000_int64
        integer, parameter :: teams(*) = [1, 2, 4, 8, 16, 32, 64]
        integer(int64), allocatable :: idx(:), ref(:)
        integer(int64) :: n
        integer :: si, ti, rep
        real(real64) :: t0, best(size(teams))
        logical :: same
        call parquet_set_random_parallel_min_elements(0)
        write (output_unit, '(a)') ""
        write (output_unit, '(a,i0,a)') "pf_random_subset from a population of m = ", MPOP, &
            "   (wall ns/element by thread count)"
        write (output_unit, '(a)', advance="no") "        n"
        do ti = 1, size(teams)
            write (output_unit, '(i9)', advance="no") teams(ti)
        end do
        write (output_unit, '(a)') "   identical"
        do si = 1, size(sizes)
            n = sizes(si)
            allocate (idx(n), ref(n))
            idx = 0_int64
            ref = 0_int64
            call pf_random_subset(ref, MPOP, s, threads=1)
            same = .true.
            do ti = 1, size(teams)
                best(ti) = huge(1.0_real64)
                do rep = 1, 5
                    t0 = wall()
                    call pf_random_subset(idx, MPOP, s, threads=teams(ti))
                    best(ti) = min(best(ti), wall() - t0)
                end do
                if (any(idx /= ref)) same = .false.
            end do
            write (output_unit, '(i9)', advance="no") n
            do ti = 1, size(teams)
                write (output_unit, '(f9.3)', advance="no") best(ti) * 1.0e9_real64 / real(n, real64)
            end do
            write (output_unit, '(4x,l1)') same
            deallocate (idx, ref)
        end do
        call parquet_reset_settings()
    end subroutine run_subset_floor

    !> What the reproducibility guarantee costs, priced against the two things it rules out.
    !!
    !! **The comparison originally asked for, restated.** Its original phrasing --
    !! "`pf_random_permutation` (argsort-based) against a serial Fisher-Yates" -- was written when
    !! the construction was expected to be one of those two, and became self-referential when D9
    !! made it a Fisher-Yates and then the Feistel bijection replaced both.
    !! The question that survives is the one worth asking anyway:
    !!
    !!  * **arm B, stream Fisher-Yates** -- the cheapest shuffle this library can express, and the
    !!    one this module may not use. (Not the cheapest shuffle in existence: it pays for
    !!    `%int_range`, which is exactly unbiased and costs a word pair. A hand-rolled FY over a
    !!    cheaper biased draw would beat it, and would be measuring something else.) It is perfectly
    !!    reproducible run serially from a fixed seed; what it
    !!    cannot do is give element `k` without producing elements `1..k-1` first, which is exactly
    !!    what a dynamically scheduled loop needs. So `A/B` is **the price of coordinate
    !!    addressing** -- of being able to ask for element 5000 alone, on any thread, in any order.
    !!  * **arm C, key-and-argsort** -- reproducible, and the construction originally specified.
    !!    So `A/C` is **the price of the algorithm choice**, holding the guarantee fixed.
    !!
    !! Two properties no timing column can show, and they are the reason the ratios are not the
    !! whole answer: arm B cannot produce a subset without materialising the whole population, and
    !! arm C costs `O(m)` draws plus `O(m log m)` and `O(m)` memory whatever `size(idx)` is. The
    !! shipped form does `size(idx)` work from a population of any size -- which is why the subset
    !! ladder above runs against `m = 10**15`, a number neither alternative can even allocate.
    subroutine run_guarantee(s)
        integer(int64), intent(in) :: s             !! seed
        integer(int64), parameter :: sizes(*) = [10000_int64, 100000_int64, 1000000_int64, &
                                                 10000000_int64]
        integer(int64), allocatable :: perm(:), fy(:), ord(:)
        real(real64), allocatable :: keys(:)
        integer(int64) :: m, j, r, tmp, guard
        integer :: si, rep
        real(real64) :: t0, a_ser, a_par, b_ns, c_ns, c_par
        type(pf_random_stream) :: rng
        logical :: okb, okc

        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "A = shipped Feistel   B = stream Fisher-Yates   C = key-and-argsort"
        write (output_unit, '(a)') "All wall ns/element, best of 3. A_1 is threads=1; A_auto is the default team."
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "        m      A_1   A_auto        B      C_1   C_auto     A_1/B   A_1/C_1  validB validC"
        do si = 1, size(sizes)
            m = sizes(si)
            allocate (perm(m), fy(m), keys(m))
            ! Warm every destination before timing: a fresh large array pays first-touch faults
            ! once, and whichever arm ran first would otherwise absorb them and look slowest.
            perm = 0_int64
            fy = 0_int64
            keys = 0.0_real64
            a_ser = huge(1.0_real64)
            a_par = huge(1.0_real64)
            b_ns = huge(1.0_real64)
            c_ns = huge(1.0_real64)
            c_par = huge(1.0_real64)
            do rep = 1, 5
                t0 = wall()
                call pf_random_permutation(perm, s, threads=1)
                a_ser = min(a_ser, wall() - t0)

                t0 = wall()
                call pf_random_permutation(perm, s)
                a_par = min(a_par, wall() - t0)

                ! Arm B: textbook Fisher-Yates driven by a stateful stream. `tmp` is not optional --
                ! by step j, slot j may already hold a value an earlier step swapped in.
                t0 = wall()
                call rng%seed(s, 0_int64)
                do j = 1_int64, m
                    fy(j) = j
                end do
                do j = 1_int64, m
                    call rng%int_range(j, m, r)
                    tmp = fy(j)
                    fy(j) = fy(r)
                    fy(r) = tmp
                end do
                b_ns = min(b_ns, wall() - t0)

                ! Arm C: one key per candidate, then argsort. Reproducible, and O(m) whatever is
                ! wanted from it.
                ! `threads=1` is REQUIRED for the serial column, not tidiness: pf_argsort's
                ! default is auto, and it engages a team only above its own work floor -- so an
                ! unqualified call silently measures a serial sort at one size and a 384-thread one
                ! at the next, and the column then falls as m rises.
                t0 = wall()
                call pf_random_fill_draws(s, 0_int64, keys)
                call pf_argsort(keys, ord, threads=1)
                c_ns = min(c_ns, wall() - t0)

                t0 = wall()
                call pf_random_fill_draws(s, 0_int64, keys)
                call pf_argsort(keys, ord)
                c_par = min(c_par, wall() - t0)
            end do
            okb = is_perm(fy, m)
            okc = is_perm(ord, m)
            guard = perm(1) + fy(1) + ord(1)
            write (output_unit, '(i9,5f9.3,2f10.2,4x,l1,6x,l1,i22)') m, &
                a_ser * 1.0e9_real64 / real(m, real64), &
                a_par * 1.0e9_real64 / real(m, real64), &
                b_ns * 1.0e9_real64 / real(m, real64), &
                c_ns * 1.0e9_real64 / real(m, real64), &
                c_par * 1.0e9_real64 / real(m, real64), &
                a_ser / max(b_ns, tiny(1.0_real64)), &
                a_ser / max(c_ns, tiny(1.0_real64)), okb, okc, guard
            deallocate (perm, fy, keys)
            if (allocated(ord)) deallocate (ord)
        end do
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "A_1/B  > 1 is what coordinate addressing costs against the cheapest possible shuffle."
        write (output_unit, '(a)') "A_1/C  is the algorithm choice alone, with the guarantee held fixed."
        write (output_unit, '(a)') "C_auto is arm C with pf_argsort's own threading, for scale -- arm B has no"
        write (output_unit, '(a)') "threaded form at all, which is the point of the exercise."
        write (output_unit, '(a)') "validB/validC must both be T, or the arm being timed is not producing a permutation."
    end subroutine run_guarantee

    !> Is `v` a permutation of `1 .. m`? Guards the two comparison arms against timing nonsense.
    logical function is_perm(v, m) result(ok)
        integer(int64), intent(in) :: v(:)          !! candidate
        integer(int64), intent(in) :: m             !! expected population
        logical, allocatable :: seen(:)
        integer(int64) :: k
        ok = (size(v, kind=int64) == m)
        if (.not. ok) return
        allocate (seen(m))
        seen = .false.
        do k = 1_int64, m
            if (v(k) < 1_int64 .or. v(k) > m) then
                ok = .false.
                exit
            end if
            if (seen(v(k))) then
                ok = .false.
                exit
            end if
            seen(v(k)) = .true.
        end do
        deallocate (seen)
    end function is_perm

end program probe_random_perm
