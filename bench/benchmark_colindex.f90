!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Measures where `parquet_table%get_element`'s per-cell cost actually goes, for the F1
!! index/handle accessor decision (`feature_colindex.md`; run sheet
!! `feature_benchmark_colindex.md`).
!!
!! **This is the SCREENING harness, and what it measures are BOUNDS.** It deliberately needs no
!! change to the library at all, which is what makes it runnable before anything is prototyped —
!! and it is enough to kill the feature or to justify building the prototype, which is the whole
!! point of running it first. It works by measuring the two ends of the call chain against each
!! other:
!!
!! ```
!!   t%get_element(name, i, v)      table_resolve -> require_row -> select case -> values%get_at
!!   col%get_at(i, v)                                                              values%get_at
!!   p(i)  (a %col pointer)                                                        the load alone
!! ```
!!
!! `col%get_at` is called on a STANDALONE `parquet_column` the harness builds itself, so it is the
!! real library procedure across the real module boundary, with its real guards — not a replica.
!! That yields two differences that are worth more than either absolute figure:
!!
!!   * **table prologue** = `get_element` - `get_at`. Everything the table layer adds. An
!!     index or a handle can remove only part of this, so it is the **upper bound** on F1's prize.
!!   * **get_at overhead** = `get_at` - pointer read. The two guard calls into
!!     `parquet_columns_util` plus the guards themselves. This is the whole of Q9's territory.
!!
!! If the table prologue is small, F1 is dead and no prototype is needed. If it is large, the
!! prototype is worth building and the second stage of the run sheet measures it for real.
!!
!! **Splitting `get_at`'s CALL from its CHECK** needs the one library variant this campaign uses:
!! rebuild with `tools/generate_parquet_columns.py --bench-guards` and then select the arm with
!! `-DPF_BENCH_INLINE_GUARDS` or `-DPF_BENCH_NO_GUARDS`. The `getat` mode reports which variant it
!! was compiled with, so a run cannot be filed against the wrong one. Without the flag every mode
!! still runs; only that three-way split is unavailable.
!!
!! Rules this program follows, each because breaking it produced a wrong number somewhere in this
!! repository before (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark tools"):
!! every column and every destination is written through once before any timing; every figure is
!! the best of several rounds; every value read is accumulated into a checksum printed at the end,
!! so nothing is optimised away; and the wrapper passes `--profile release`, without which fpm
!! applies no optimisation at all and every number here is meaningless.
!!
!! **The row index wraps with a counter, NEVER with `mod(k - 1, nrows)`.** `nrows` is a runtime
!! value, so `mod` compiles to a 64-bit integer division — and on x86-64 that division cost
!! ~6 ns, which was *the whole of* the reported pointer-read "floor" and larger than several of
!! the quantities this harness exists to resolve. Machine C caught it by counting `idivq` in the
!! object (126 here against 0 in an identical standalone loop). Differences survived it; ratios
!! against the floor did not. If a new arm needs to walk rows, copy the
!! `i = i + 1; if (i > nrows) i = 1` shape rather than reaching for `mod`.
!!
!! Maintainer tool, never run by `fpm test`. Drive it with bench/benchmark_colindex.sh.
program benchmark_colindex
    use parquet
    use iso_fortran_env, only : int64, real64, error_unit, output_unit
    implicit none

    character(len=:), allocatable :: mode
    integer :: ncols, rounds, width
    integer(int64) :: nrows, naccess
    real(real64) :: checksum

    checksum = 0.0_real64

    call parse_arguments(mode, nrows, ncols, rounds, naccess, width)

    write(output_unit, '(a)') "=================================================================="
    write(output_unit, '(a)') "benchmark_colindex -- F1 screening (see feature_benchmark_colindex.md)"
    write(output_unit, '(a,a)') "guard variant compiled in: ", guard_variant()
    write(output_unit, '(a,a)') "stage-0b ladder rung:      ", ladder_rung()
    write(output_unit, '(a)') "=================================================================="
    write(output_unit, '(a)') ""

    select case (mode)
    case ("baseline")
        call run_baseline(nrows, rounds, naccess)
    case ("decompose")
        call run_decompose(nrows, ncols, rounds, naccess)
    case ("getat")
        call run_getat(nrows, rounds, naccess)
    case ("vector")
        call run_vector(nrows, rounds, naccess, width)
    case ("loop")
        call run_loop(nrows, ncols, rounds)
    case ("rowfinal")
        call run_rowfinal(nrows, ncols, rounds, naccess)
    case ("handle")
        call run_handle(nrows, ncols, rounds, naccess, width)
    case default
        write(error_unit, '(a)') "benchmark_colindex: unknown --mode '"//mode//"'"
        call usage()
        error stop 1
    end select

    write(output_unit, '(a)') ""
    write(output_unit, '(a,es16.8)') "checksum (keeps every arm alive; ignore the value): ", checksum

contains

    subroutine usage()
        write(output_unit, '(a)') "Usage: benchmark_colindex --mode=<mode> [options]"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  --mode=baseline   the anchors: %col floor, and %get_element vs table width"
        write(output_unit, '(a)') "  --mode=decompose  the layered bound: pointer / get_at / get_element"
        write(output_unit, '(a)') "  --mode=getat      parquet_column%get_at alone (guard variants; see --bench-guards)"
        write(output_unit, '(a)') "  --mode=vector     a *_VEC kind: does the per-call allocate dominate?"
        write(output_unit, '(a)') "  --mode=loop       a realistic 4-column loop with arithmetic"
        write(output_unit, '(a)') "  --mode=rowfinal   what constructing r = t%row(i) costs"
        write(output_unit, '(a)') "  --mode=handle     THE SHIPPED COLUMN HANDLE vs the name form (stage 1)"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  --nrows=<n>       rows per column        (default 100000)"
        write(output_unit, '(a)') "  --ncols=<n>       columns in the table   (default 40)"
        write(output_unit, '(a)') "  --rounds=<n>      rounds; best is kept   (default 5)"
        write(output_unit, '(a)') "  --access=<n>      accesses per arm       (default 2000000)"
        write(output_unit, '(a)') "  --width=<n>       elements per row, vector mode (default 4)"
    end subroutine usage

    subroutine parse_arguments(mode, nrows, ncols, rounds, naccess, width)
        character(len=:), allocatable, intent(out) :: mode !! which measurement to run.
        integer(int64), intent(out) :: nrows               !! rows per column.
        integer, intent(out) :: ncols                      !! columns in the table.
        integer, intent(out) :: rounds                     !! rounds per figure; the best is kept.
        integer(int64), intent(out) :: naccess             !! accesses per arm.
        integer, intent(out) :: width                      !! elements per row, vector mode.
        character(len=256) :: arg
        character(len=:), allocatable :: key, val
        integer :: i, eq, ios

        mode = ""
        nrows = 100000_int64
        ncols = 40
        rounds = 5
        naccess = 2000000_int64
        width = 4

        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            eq = index(arg, "=")
            if (eq <= 0) then
                if (trim(arg) == "--help" .or. trim(arg) == "-h") then
                    call usage()
                    stop 0
                end if
                cycle
            end if
            key = trim(arg(1:eq - 1))
            val = trim(arg(eq + 1:))
            select case (key)
            case ("--mode")
                mode = val
            case ("--nrows")
                read(val, *, iostat=ios) nrows
            case ("--ncols")
                read(val, *, iostat=ios) ncols
            case ("--rounds")
                read(val, *, iostat=ios) rounds
            case ("--access")
                read(val, *, iostat=ios) naccess
            case ("--width")
                read(val, *, iostat=ios) width
            case default
                write(error_unit, '(a)') "benchmark_colindex: unknown option '"//key//"'"
                error stop 1
            end select
        end do

        if (len(mode) == 0) then
            write(error_unit, '(a)') "benchmark_colindex: --mode is required"
            call usage()
            error stop 1
        end if
        if (nrows < 1_int64 .or. ncols < 1 .or. rounds < 1 .or. naccess < 1_int64 .or. width < 1) then
            write(error_unit, '(a)') "benchmark_colindex: every numeric option must be >= 1"
            error stop 1
        end if
    end subroutine parse_arguments

    !> Which stage-0b ladder rung this binary was built as, or "full-path" for the shipped code.
    !!
    !! Printed in every run's header for the same reason `guard_variant` is: rungs differ only by a
    !! compile flag, so they are otherwise indistinguishable in a log and a figure can be filed
    !! against the wrong binary with nothing to catch it. This is the harness half of the check;
    !! the wrapper's refusal to accept `LADDER=` against unscaffolded source is the other half.
    !!
    !! **Two rungs change ANSWERS, not just timings.** `NO_LOOKUP` resolves a slot by hashing one
    !! byte of the name, so it reads the wrong column by construction, and `NO_ROWINDEX_CMP` drops
    !! the automatic row-index column. The checksum is therefore NOT comparable across rungs --
    !! only the timings are, which is exactly what the ladder is for.
    function ladder_rung() result(s)
        character(len=24) :: s !! the rung name, or "full-path".
#if defined(PF_BENCH_NO_LOOKUP)
        s = "NO_LOOKUP"
#elif defined(PF_BENCH_NO_LENTRIM)
        s = "NO_LENTRIM"
#elif defined(PF_BENCH_NO_ROWINDEX_CMP)
        s = "NO_ROWINDEX_CMP"
#elif defined(PF_BENCH_NO_APPEND_CHECK)
        s = "NO_APPEND_CHECK"
#elif defined(PF_BENCH_NO_OPEN_CHECK)
        s = "NO_OPEN_CHECK"
#elif defined(PF_BENCH_NO_RESIDENCY)
        s = "NO_RESIDENCY"
#elif defined(PF_BENCH_NO_REQUIRE_ROW)
        s = "NO_REQUIRE_ROW"
#else
        s = "full-path"
#endif
    end function ladder_rung

    !> Which of the three `get_at` guard variants this binary was built with.
    !!
    !! Reported in every run's header so a figure cannot be filed against the wrong build — the
    !! three arms differ only by a compile flag, so they are otherwise indistinguishable in a log.
    function guard_variant() result(s)
        character(len=16) :: s !! "as-shipped", "inline-guards" or "no-guards".
#if defined(PF_BENCH_NO_GUARDS)
        s = "no-guards"
#elif defined(PF_BENCH_INLINE_GUARDS)
        s = "inline-guards"
#else
        s = "as-shipped"
#endif
    end function guard_variant

    !> Wall-clock seconds since an arbitrary origin.
    function now() result(t)
        real(real64) :: t !! seconds.
        integer(int64) :: c, r
        call system_clock(count=c, count_rate=r)
        t = real(c, real64)/real(r, real64)
    end function now

    !> Prints one measured arm as nanoseconds per access, and folds its checksum in.
    subroutine emit(label, secs, nops, acc)
        character(len=*), intent(in) :: label !! what was measured.
        real(real64), intent(in) :: secs      !! best round's wall time.
        integer(int64), intent(in) :: nops    !! accesses in one round.
        real(real64), intent(in) :: acc       !! the arm's checksum contribution.
        write(output_unit, '(2x,a34,f12.3,a)') label, 1.0e9_real64*secs/real(nops, real64), " ns"
        checksum = checksum + acc
    end subroutine emit

    !> Prints a derived difference between two already-reported arms.
    subroutine emit_diff(label, hi_ns, lo_ns)
        character(len=*), intent(in) :: label !! what the difference means.
        real(real64), intent(in) :: hi_ns     !! the larger figure, ns.
        real(real64), intent(in) :: lo_ns     !! the smaller figure, ns.
        write(output_unit, '(2x,a34,f12.3,a,f8.2,a)') label, hi_ns - lo_ns, " ns  (", hi_ns/lo_ns, "x)"
    end subroutine emit_diff

    !> Nanoseconds per access, from a round's wall time.
    function ns_per(secs, nops) result(ns)
        real(real64), intent(in) :: secs   !! wall time.
        integer(int64), intent(in) :: nops !! accesses.
        real(real64) :: ns                 !! nanoseconds per access.
        ns = 1.0e9_real64*secs/real(nops, real64)
    end function ns_per

    !> Fixed-width column names, all the same length so that no call site needs `trim`.
    !!
    !! A.7's own harness did this deliberately and the reason is worth keeping: a `trim` in the
    !! benchmark would be charged to the library's name lookup and inflate exactly the figure the
    !! campaign is trying to measure.
    subroutine make_names(ncols, names)
        integer, intent(in) :: ncols                              !! how many.
        character(len=8), allocatable, intent(out) :: names(:)    !! c0000000-style names.
        integer :: j
        allocate(names(ncols))
        do j = 1, ncols
            write(names(j), '(a,i5.5)') "col", j - 1
        end do
    end subroutine make_names

    !> Builds an in-memory table of `ncols` float64 columns, every column fully written.
    subroutine build_table(t, names, nrows, ncols)
        type(parquet_table), intent(out) :: t             !! receives the table.
        character(len=8), intent(in) :: names(:)          !! column names.
        integer(int64), intent(in) :: nrows               !! rows per column.
        integer, intent(in) :: ncols                      !! columns.
        real(real64), allocatable :: v(:)
        integer(int64) :: i
        integer :: j
        allocate(v(nrows))
        call parquet_new_table(t)
        do j = 1, ncols
            do i = 1_int64, nrows
                v(i) = real(j, real64) + 0.5_real64*real(i, real64)
            end do
            call t%add_column(names(j), v)
        end do
    end subroutine build_table

    !> The four column positions a realistic loop touches, scattered through the table.
    !!
    !! A.7 measured this shape rather than a single column because the maintainer's own
    !! description of the common case is "in a do loop, I work with 2-4 columns in every row".
    subroutine scattered_positions(ncols, pos)
        integer, intent(in) :: ncols   !! table width.
        integer, intent(out) :: pos(4) !! four 1-based column positions.
        integer :: k
        do k = 1, 4
            pos(k) = 1 + ((k - 1)*max(ncols - 1, 1))/3
            if (pos(k) > ncols) pos(k) = ncols
        end do
    end subroutine scattered_positions

    ! ---------------------------------------------------------------------------------------
    ! Mode: baseline -- the anchors, and how the name lookup scales with table width.
    ! ---------------------------------------------------------------------------------------

    !> The two figures every other number in this campaign is read against.
    !!
    !! On machine A these must reproduce the stage-5 measurements — a `%col` pointer read at
    !! **0.93 ns** and `%get_element` at **~24 ns** — before anything else here is believed. A
    !! benchmark that replicates library call shapes is untested code until one of its rows
    !! reproduces a figure measured independently.
    subroutine run_baseline(nrows, rounds, naccess)
        integer(int64), intent(in) :: nrows   !! rows per column.
        integer, intent(in) :: rounds         !! rounds; best kept.
        integer(int64), intent(in) :: naccess !! accesses per arm.
        integer, parameter :: widths(4) = [4, 16, 40, 128]
        type(parquet_table) :: t
        character(len=8), allocatable :: names(:)
        real(real64), pointer :: p(:)
        real(real64) :: t0, dt, best, acc, v
        integer(int64) :: i, k
        integer :: w, nc, r, pos(4), kk

        write(output_unit, '(a)') "MODE baseline -- anchors, and name lookup vs table width"
        write(output_unit, '(a,i0,a,i0,a)') "  (", nrows, " rows per column, ", naccess, " accesses per arm)"
        write(output_unit, '(a)') ""

        do w = 1, size(widths)
            nc = widths(w)
            call make_names(nc, names)
            call build_table(t, names, nrows, nc)
            call scattered_positions(nc, pos)

            ! Warm every column, and the pointer's pages, before any timing.
            call t%col(names(1), p)
            acc = 0.0_real64
            do i = 1_int64, nrows
                acc = acc + p(i)
            end do
            do kk = 1, nc
                call t%get_element(names(kk), 1_int64, v)
                acc = acc + v
            end do
            checksum = checksum + acc

            write(output_unit, '(a,i0,a)') "  --- ncols = ", nc, " ---"

            best = huge(1.0_real64)
            do r = 1, rounds
                acc = 0.0_real64
                t0 = now()
                do k = 1_int64, naccess
                    i = i + 1_int64
                if (i > nrows) i = 1_int64
                    acc = acc + p(i)
                end do
                dt = now() - t0
                if (dt < best) best = dt
            end do
            call emit("%col pointer read (the floor)", best, naccess, acc)

            best = huge(1.0_real64)
            do r = 1, rounds
                acc = 0.0_real64
                t0 = now()
                do k = 1_int64, naccess
                    i = i + 1_int64
                if (i > nrows) i = 1_int64
                    call t%get_element(names(1), i, v)
                    acc = acc + v
                end do
                dt = now() - t0
                if (dt < best) best = dt
            end do
            call emit("%get_element, first column", best, naccess, acc)

            best = huge(1.0_real64)
            do r = 1, rounds
                acc = 0.0_real64
                t0 = now()
                do k = 1_int64, naccess
                    i = i + 1_int64
                if (i > nrows) i = 1_int64
                    call t%get_element(names(nc), i, v)
                    acc = acc + v
                end do
                dt = now() - t0
                if (dt < best) best = dt
            end do
            call emit("%get_element, last column", best, naccess, acc)

            best = huge(1.0_real64)
            do r = 1, rounds
                acc = 0.0_real64
                t0 = now()
                do k = 1_int64, naccess
                    i = i + 1_int64
                if (i > nrows) i = 1_int64
                    kk = 1 + int(mod(k - 1_int64, 4_int64))
                    call t%get_element(names(pos(kk)), i, v)
                    acc = acc + v
                end do
                dt = now() - t0
                if (dt < best) best = dt
            end do
            call emit("%get_element, four scattered", best, naccess, acc)
            write(output_unit, '(a)') ""
        end do

        write(output_unit, '(a)') "  A flat figure across the four widths is the sorted name index working"
        write(output_unit, '(a)') "  as intended; a rise with ncols would mean the bisection is not engaging."
    end subroutine run_baseline

    ! ---------------------------------------------------------------------------------------
    ! Mode: decompose -- the layered bound. The central measurement of the campaign.
    ! ---------------------------------------------------------------------------------------

    !> Splits `%get_element` into the table layer, the column layer and the load itself.
    !!
    !! The two derived rows at the end are what the F1 decision rests on. **"table prologue" is
    !! an upper bound on what an index or a handle can ever remove**, because a handle still pays
    !! the open check, the append check, the residency test, the row bounds check and the kind
    !! dispatch — so if that bound is already small, the feature cannot pay whatever is built.
    subroutine run_decompose(nrows, ncols, rounds, naccess)
        integer(int64), intent(in) :: nrows   !! rows per column.
        integer, intent(in) :: ncols          !! table width.
        integer, intent(in) :: rounds         !! rounds; best kept.
        integer(int64), intent(in) :: naccess !! accesses per arm.
        type(parquet_table) :: t
        type(parquet_column) :: sc
        character(len=8), allocatable :: names(:)
        real(real64), allocatable :: raw(:)
        real(real64), pointer :: p(:)
        real(real64) :: t0, dt, best, acc, v
        real(real64) :: ns_raw, ns_ptr, ns_getat, ns_elem
        integer(int64) :: i, k
        integer :: r, pos(4), kk

        write(output_unit, '(a)') "MODE decompose -- where %get_element's time goes"
        write(output_unit, '(a,i0,a,i0,a)') "  (ncols = ", ncols, ", ", nrows, " rows per column)"
        write(output_unit, '(a,a)') "  get_at guard variant: ", guard_variant()
        write(output_unit, '(a,a)') "  ladder rung:          ", ladder_rung()
        write(output_unit, '(a)') ""

        call make_names(ncols, names)
        call build_table(t, names, nrows, ncols)
        call scattered_positions(ncols, pos)

        allocate(raw(nrows))
        do i = 1_int64, nrows
            raw(i) = 1.0_real64 + 0.5_real64*real(i, real64)
        end do

        ! A standalone parquet_column holding the same data. This is the REAL library procedure
        ! across the REAL module boundary -- not a replica -- which is what makes the get_at row
        ! evidence rather than an illustration.
        call sc%init(PK_FLOAT64, nrows)
        call sc%set_all(raw)

        ! Warm everything, in both directions, before any timing.
        call t%col(names(1), p)
        acc = 0.0_real64
        do i = 1_int64, nrows
            acc = acc + p(i) + raw(i)
        end do
        call sc%get_at(1_int64, v)
        acc = acc + v
        do kk = 1, 4
            call t%get_element(names(pos(kk)), 1_int64, v)
            acc = acc + v
        end do
        checksum = checksum + acc

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                acc = acc + raw(i)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_raw = ns_per(best, naccess)
        call emit("A  plain Fortran array read", best, naccess, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                acc = acc + p(i)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_ptr = ns_per(best, naccess)
        call emit("B  %col pointer read", best, naccess, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call sc%get_at(i, v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_getat = ns_per(best, naccess)
        call emit("C  parquet_column%get_at", best, naccess, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                kk = 1 + int(mod(k - 1_int64, 4_int64))
                call t%get_element(names(pos(kk)), i, v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_elem = ns_per(best, naccess)
        call emit("D  %get_element, four scattered", best, naccess, acc)

        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  derived:"
        call emit_diff("D-C  table prologue (F1's CEILING)", ns_elem, ns_getat)
        call emit_diff("C-B  get_at call + guards (Q9)", ns_getat, ns_ptr)
        call emit_diff("B-A  pointer vs plain array", ns_ptr, ns_raw)
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  D-C is an UPPER BOUND on what an index or handle can remove: a handle still"
        write(output_unit, '(a)') "  pays the open check, the append check, the residency test, the row bounds"
        write(output_unit, '(a)') "  check and the kind dispatch. If D-C is small, F1 cannot pay -- whatever is"
        write(output_unit, '(a)') "  built. C-B is Q9's whole territory; run --mode=getat to split it."
    end subroutine run_decompose

    ! ---------------------------------------------------------------------------------------
    ! Mode: getat -- Q9's three arms. Needs the --bench-guards library variant to be a split.
    ! ---------------------------------------------------------------------------------------

    !> `parquet_column%get_at` alone, on a standalone column, with no table anywhere near it.
    !!
    !! Run this three times, on three builds — as shipped, `-DPF_BENCH_INLINE_GUARDS`,
    !! `-DPF_BENCH_NO_GUARDS` — and read the two gaps. Shipped -> inlined is what removing the
    !! **calls** buys at zero cost to safety, because the comparisons all still happen. Inlined ->
    !! removed is everything a `_trusted` getter could add on top of that, and is expected to be
    !! small: what remains is four integer comparisons.
    subroutine run_getat(nrows, rounds, naccess)
        integer(int64), intent(in) :: nrows   !! rows in the standalone column.
        integer, intent(in) :: rounds         !! rounds; best kept.
        integer(int64), intent(in) :: naccess !! accesses per arm.
        type(parquet_column) :: sc
        real(real64), allocatable :: raw(:)
        real(real64) :: t0, dt, best, acc, v
        integer(int64) :: i, k
        integer :: r
        logical :: isn

        write(output_unit, '(a)') "MODE getat -- parquet_column%get_at, guards split"
        write(output_unit, '(a,a)') "  variant compiled in: ", guard_variant()
        write(output_unit, '(a)') "  (compare this figure across three builds; one run cannot answer Q9)"
        write(output_unit, '(a)') ""

        allocate(raw(nrows))
        do i = 1_int64, nrows
            raw(i) = 0.25_real64*real(i, real64)
        end do
        call sc%init(PK_FLOAT64, nrows)
        call sc%set_all(raw)

        acc = 0.0_real64
        call sc%get_at(1_int64, v)
        acc = acc + v
        checksum = checksum + acc

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call sc%get_at(i, v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("%get_at", best, naccess, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call sc%set_at(i, 1.5_real64)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("%set_at", best, naccess, 0.0_real64)

        acc = 0.0_real64
        best = huge(1.0_real64)
        do r = 1, rounds
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                isn = sc%is_null(i)
                if (isn) acc = acc + 1.0_real64
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("%is_null", best, naccess, acc)

        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  shipped -> inline-guards = what removing the CALLS buys, safety unchanged."
        write(output_unit, '(a)') "  inline-guards -> no-guards = all a _trusted getter could add on top."
    end subroutine run_getat

    ! ---------------------------------------------------------------------------------------
    ! Mode: vector -- does the per-call allocate dominate the 10 allocating kinds?
    ! ---------------------------------------------------------------------------------------

    !> A `*_VEC` column, where `%get_element` returns an allocatable and so allocates per call.
    !!
    !! If the bare allocate/deallocate reference is close to the `%get_element` figure, then for
    !! these kinds an index or a handle is fixing the wrong cost, and the real fix is a form
    !! taking a caller-supplied buffer — which needs no handle at all and is a different feature.
    subroutine run_vector(nrows, rounds, naccess, width)
        integer(int64), intent(in) :: nrows   !! rows per column.
        integer, intent(in) :: rounds         !! rounds; best kept.
        integer(int64), intent(in) :: naccess !! accesses per arm.
        integer, intent(in) :: width          !! elements per row.
        type(parquet_table) :: t
        type(parquet_column) :: sc
        real(real64), allocatable :: vals(:,:), buf(:), tmp(:)
        real(real64), allocatable :: got(:)
        real(real64), pointer :: p2(:,:)
        real(real64) :: t0, dt, best, acc
        integer(int64) :: i, k
        integer :: r, e

        write(output_unit, '(a)') "MODE vector -- the per-call allocate on a *_VEC kind"
        write(output_unit, '(a,i0,a,i0,a)') "  (width = ", width, ", ", nrows, " rows)"
        write(output_unit, '(a)') ""

        allocate(vals(width, nrows))
        do i = 1_int64, nrows
            do e = 1, width
                vals(e, i) = real(e, real64) + 0.125_real64*real(i, real64)
            end do
        end do
        call parquet_new_table(t)
        call t%add_column("vec00000", vals)
        call sc%init(PK_FLOAT64_VEC, nrows, width)
        call sc%set_all(vals)
        allocate(buf(width))

        ! Warm: the table's copy, the standalone column, and the destination buffer.
        acc = 0.0_real64
        call t%get_element("vec00000", 1_int64, got)
        acc = acc + got(1)
        call sc%get_at(1_int64, buf)
        acc = acc + buf(1)
        call t%col("vec00000", p2)
        acc = acc + p2(1, 1)
        checksum = checksum + acc

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                acc = acc + p2(1, i)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("%col 2-D pointer, one element", best, naccess, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call sc%get_at(i, buf)
                acc = acc + buf(1)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("%get_at into a reused buffer", best, naccess, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call t%get_element("vec00000", i, got)
                acc = acc + got(1)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("%get_element (allocates per call)", best, naccess, acc)

        ! The same %get_at as the arm above, but allocating and freeing its destination each
        ! call. The difference between the two IS the allocate/deallocate pair, isolated.
        !
        ! It has to be written this way. An `allocate`/`deallocate` around a couple of local
        ! stores is ELIDED OUTRIGHT by gfortran -- the first version of this arm reported exactly
        ! the pointer-read figure, i.e. it measured nothing at all and looked like a real result.
        ! Passing the array to a procedure in another module is what makes the allocation observable
        ! and stops the compiler removing it.
        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                allocate(tmp(width))
                call sc%get_at(i, tmp)
                acc = acc + tmp(1)
                deallocate(tmp)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("%get_at + allocate/free per call", best, naccess, acc)

        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  The last two rows differ ONLY by the per-call allocate, so their gap is the"
        write(output_unit, '(a)') "  allocation cost with nothing else in it. If that gap is most of the distance"
        write(output_unit, '(a)') "  from %get_at to %get_element, then for these 10 kinds an index or handle is"
        write(output_unit, '(a)') "  fixing the wrong cost: the fix is a form taking a caller-supplied buffer,"
        write(output_unit, '(a)') "  which needs no handle at all."
    end subroutine run_vector

    ! ---------------------------------------------------------------------------------------
    ! Mode: loop -- the only ratio that decides anything.
    ! ---------------------------------------------------------------------------------------

    !> Four columns, real arithmetic in the body, three access paths.
    !!
    !! An accessor speedup that disappears into the loop body is not a feature: this project has
    !! recorded a 31% phase share buying a 4% end-to-end change. The `%get_at` arm is the
    !! **achievable floor** for a column handle — a handle cannot be faster than resolving nothing
    !! at all and calling straight into the column — so the ratio between it and the name form is
    !! the optimistic version of F1's own loop ratio.
    subroutine run_loop(nrows, ncols, rounds)
        integer(int64), intent(in) :: nrows !! rows per column.
        integer, intent(in) :: ncols        !! table width.
        integer, intent(in) :: rounds       !! rounds; best kept.
        type(parquet_table) :: t
        type(parquet_column) :: sc(4)
        character(len=8), allocatable :: names(:)
        real(real64), allocatable :: raw(:)
        real(real64), pointer :: p1(:), p2(:), p3(:), p4(:)
        real(real64) :: t0, dt, best, acc, a, b, c, d
        real(real64) :: ns_name, ns_ptr, ns_getat
        integer(int64) :: i
        integer :: r, pos(4), kk

        write(output_unit, '(a)') "MODE loop -- 4 columns x nrows, with arithmetic in the body"
        write(output_unit, '(a,i0,a,i0,a)') "  (ncols = ", ncols, ", ", nrows, " rows; ns per ROW, 4 reads each)"
        write(output_unit, '(a)') ""

        call make_names(ncols, names)
        call build_table(t, names, nrows, ncols)
        call scattered_positions(ncols, pos)

        allocate(raw(nrows))
        do kk = 1, 4
            do i = 1_int64, nrows
                raw(i) = real(pos(kk), real64) + 0.5_real64*real(i, real64)
            end do
            call sc(kk)%init(PK_FLOAT64, nrows)
            call sc(kk)%set_all(raw)
        end do

        call t%col(names(pos(1)), p1)
        call t%col(names(pos(2)), p2)
        call t%col(names(pos(3)), p3)
        call t%col(names(pos(4)), p4)

        ! Warm every path once, so no arm pays another's first touch or page faults.
        acc = 0.0_real64
        do i = 1_int64, nrows
            acc = acc + p1(i) + p2(i) + p3(i) + p4(i)
        end do
        call t%get_element(names(pos(1)), 1_int64, a)
        call sc(1)%get_at(1_int64, b)
        checksum = checksum + acc + a + b

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            t0 = now()
            do i = 1_int64, nrows
                call t%get_element(names(pos(1)), i, a)
                call t%get_element(names(pos(2)), i, b)
                call t%get_element(names(pos(3)), i, c)
                call t%get_element(names(pos(4)), i, d)
                acc = acc + (a*b + c - d)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_name = ns_per(best, nrows)
        call emit("name form: 4x %get_element", best, nrows, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            t0 = now()
            do i = 1_int64, nrows
                call sc(1)%get_at(i, a)
                call sc(2)%get_at(i, b)
                call sc(3)%get_at(i, c)
                call sc(4)%get_at(i, d)
                acc = acc + (a*b + c - d)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_getat = ns_per(best, nrows)
        call emit("handle FLOOR: 4x %get_at", best, nrows, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            t0 = now()
            do i = 1_int64, nrows
                acc = acc + (p1(i)*p2(i) + p3(i) - p4(i))
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_ptr = ns_per(best, nrows)
        call emit("%col pointers (the real floor)", best, nrows, acc)

        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  derived:"
        call emit_diff("name form vs handle floor", ns_name, ns_getat)
        call emit_diff("name form vs %col", ns_name, ns_ptr)
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  The middle row is OPTIMISTIC for F1: a real handle also pays the open check,"
        write(output_unit, '(a)') "  the append check, the residency test and the row bounds check. If even this"
        write(output_unit, '(a)') "  ratio is under 1.15x, the feature cannot clear its own keep-or-drop rule."
    end subroutine run_loop

    ! ---------------------------------------------------------------------------------------
    ! Mode: rowfinal -- what CONSTRUCTING a row handle costs (the type has no finalizer now).
    ! ---------------------------------------------------------------------------------------

    !> `r = t%row(i)` per row, against reaching the same cell with no handle at all.
    !!
    !! This mode was built to price `row_finalize`, which has since been **deleted** — on the
    !! argument that a finalizer nullifying a pointer on a non-owning handle protects nothing,
    !! not on a figure from here (this mode cannot separate construction from finalization, which
    !! is why it could not settle it). What it still measures is what CONSTRUCTING a row handle
    !! costs, which is the number behind the guide's advice not to make one per cell.
    subroutine run_rowfinal(nrows, ncols, rounds, naccess)
        integer(int64), intent(in) :: nrows   !! rows per column.
        integer, intent(in) :: ncols          !! table width.
        integer, intent(in) :: rounds         !! rounds; best kept.
        integer(int64), intent(in) :: naccess !! accesses per arm.
        type(parquet_table) :: t
        type(parquet_table_row) :: rh
        character(len=8), allocatable :: names(:)
        real(real64) :: t0, dt, best, acc, v
        real(real64) :: ns_row, ns_elem
        integer(int64) :: i, k
        integer :: r

        write(output_unit, '(a)') "MODE rowfinal -- the cost of r = t%row(i)"
        write(output_unit, '(a,i0,a,i0,a)') "  (ncols = ", ncols, ", ", nrows, " rows per column)"
        write(output_unit, '(a)') ""

        call make_names(ncols, names)
        call build_table(t, names, nrows, ncols)

        acc = 0.0_real64
        rh = t%row(1_int64)
        call rh%get(names(1), v)
        acc = acc + v
        call t%get_element(names(1), 1_int64, v)
        acc = acc + v
        checksum = checksum + acc

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call t%get_element(names(1), i, v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_elem = ns_per(best, naccess)
        call emit("%get_element (no handle)", best, naccess, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                rh = t%row(i)
                call rh%get(names(1), v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_row = ns_per(best, naccess)
        call emit("r = t%row(i) then r%get(name,v)", best, naccess, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            rh = t%row(1_int64)
            t0 = now()
            do k = 1_int64, naccess
                call rh%get(names(1), v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("r%get alone, handle hoisted", best, naccess, acc)

        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  derived:"
        call emit_diff("handle construction per row", ns_row, ns_elem)
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  That difference is the row handle's construction, minus the one name lookup"
        write(output_unit, '(a)') "  %get_element does and r%get also does. The type carries no finalizer any more,"
        write(output_unit, '(a)') "  so what is left here is construction alone -- see --mode=handle for the column"
        write(output_unit, '(a)') "  handle, which is what a per-cell loop should be using instead."
    end subroutine run_rowfinal

    ! ---------------------------------------------------------------------------------------
    ! Mode: handle -- the SHIPPED column handle, which no campaign has measured.
    ! ---------------------------------------------------------------------------------------

    !> `parquet_table_col` against `%get_element`, on the code that ships.
    !!
    !! **Every gate figure this feature was approved on came from a PROTOTYPE** -- criterion (1)'s
    !! 1.52x-3.99x, criterion (2)'s 7.9x and criterion (3)'s +2.16% were all measured before the
    !! handle existed. This mode is stage 1 of `feature_colindex.md` section 9, and it is the first
    !! direct measurement of the thing users actually call.
    !!
    !! Two arms are load-bearing beyond the headline ratio:
    !!
    !! * **`%get_element` is the REGRESSION CONTROL.** Section 6.3 turned it into a caller of the
    !!   handle's own body, and section 9 calls this "the one that can sink the feature": if
    !!   sharing cost the name form more than 5%, the common case was made worse to improve the
    !!   uncommon one. The prototype said +2.16%; nobody has checked the shipped shape.
    !! * **`c%get` minus `get_at` is the ifx question (0d-ii).** ifx spends ~70 ns per call on
    !!   width-INDEPENDENT work against gfortran's ~25, and a handle removes the name lookup but
    !!   NOT that. If this difference is ~2.8x gfortran's under ifx, the residual is in the
    !!   per-call machinery the handle keeps -- the polymorphic passed-object dummy or the
    !!   generated body -- and it is worth chasing for every accessor in the library. If it
    !!   collapses, the residual was in the lookup and is already gone. Either answer also
    !!   explains criterion (1)'s otherwise unexplained 1.52x on ifx.
    !!
    !! **`c%index()` is how the safety rule is priced, and it needs no rebuild.** Section 9 asked
    !! for a variant with the generation check compiled out; `%index()` is `col_resolve` plus one
    !! integer load, so it measures that guard through the real code path -- with no second build,
    !! and therefore no cross-build noise floor and no semantically-altered scaffolding on `main`.
    !! `%is_valid()` brackets it from the other side.
    subroutine run_handle(nrows, ncols, rounds, naccess, width)
        integer(int64), intent(in) :: nrows   !! rows per column.
        integer, intent(in) :: ncols          !! table width.
        integer, intent(in) :: rounds         !! rounds; best kept.
        integer(int64), intent(in) :: naccess !! accesses per arm.
        integer, intent(in) :: width          !! elements per row for the vector section.
        type(parquet_table) :: t, tv
        type(parquet_table_col) :: c, c1, c2, c3, c4, cv
        type(parquet_column) :: sc
        character(len=8), allocatable :: names(:)
        real(real64), allocatable :: raw(:), vrow(:), v2(:,:)
        real(real64) :: t0, dt, best, acc, v, a, b, cc, d
        real(real64) :: ns_name, ns_hand, ns_refetch, ns_getat, ns_mkname, ns_mkpos
        real(real64) :: ns_lname, ns_lhand, ns_vrow, ns_velem
        integer(int64) :: i, k
        integer :: r, kk, pos(4), jslot, e

        write(output_unit, '(a)') "MODE handle -- the shipped parquet_table_col against the name form"
        write(output_unit, '(a,i0,a,i0,a)') "  (ncols = ", ncols, ", ", nrows, " rows per column)"
        write(output_unit, '(a)') ""

        call make_names(ncols, names)
        call build_table(t, names, nrows, ncols)
        call scattered_positions(ncols, pos)
        jslot = t%column_index(names(1))

        ! A standalone column holding the same values: the storage floor, and the arm the flag
        ! cannot touch, so it doubles as this run's cross-build control.
        allocate(raw(nrows))
        do i = 1_int64, nrows
            raw(i) = 1.0_real64 + 0.5_real64*real(i, real64)
        end do
        call sc%init(PK_FLOAT64, nrows)
        call sc%set_all(raw)

        ! Warm every path once, so no arm pays another's first touch.
        call t%column(names(1), c)
        call c%get(1_int64, v)
        call t%get_element(names(1), 1_int64, a)
        call sc%get_at(1_int64, b)
        checksum = checksum + v + a + b

        ! --- A: the name form, and the regression control -------------------------------------
        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call t%get_element(names(1), i, v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_name = ns_per(best, naccess)
        call emit("A %get_element(name,i,v)", best, naccess, acc)

        ! --- B: the feature ---------------------------------------------------------------------
        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            call t%column(names(1), c)
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call c%get(i, v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_hand = ns_per(best, naccess)
        call emit("B c%get(i,v), handle hoisted", best, naccess, acc)

        ! --- C: the anti-pattern the guide warns about ------------------------------------------
        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call t%column(names(1), c)
                call c%get(i, v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_refetch = ns_per(best, naccess)
        call emit("C c%get, handle re-fetched", best, naccess, acc)

        ! --- D: the storage floor, and this run's cross-build control ---------------------------
        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call sc%get_at(i, v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_getat = ns_per(best, naccess)
        call emit("D parquet_column%get_at (control)", best, naccess, acc)

        ! --- E/F: making a handle, by name and by position --------------------------------------
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            do k = 1_int64, naccess
                call t%column(names(1), c)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_mkname = ns_per(best, naccess)
        call emit("E c = t%column(name)", best, naccess, real(c%index(), real64))

        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            do k = 1_int64, naccess
                call t%column(jslot, c)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_mkpos = ns_per(best, naccess)
        call emit("F c = t%column(j)", best, naccess, real(c%index(), real64))

        ! --- G/H: what the staleness rule costs, without a second build -------------------------
        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            t0 = now()
            do k = 1_int64, naccess
                acc = acc + real(c%index(), real64)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("G c%index()  [col_resolve+load]", best, naccess, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            t0 = now()
            do k = 1_int64, naccess
                if (c%is_valid()) acc = acc + 1.0_real64
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        call emit("H c%is_valid()", best, naccess, acc)

        ! --- I/J: the 4-column loop, both ways --------------------------------------------------
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  4-column loop (ns per ROW, 4 reads each):"
        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            t0 = now()
            do i = 1_int64, nrows
                call t%get_element(names(pos(1)), i, a)
                call t%get_element(names(pos(2)), i, b)
                call t%get_element(names(pos(3)), i, cc)
                call t%get_element(names(pos(4)), i, d)
                acc = acc + (a*b + cc - d)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_lname = ns_per(best, nrows)
        call emit("I loop, 4x %get_element", best, nrows, acc)

        call t%column(names(pos(1)), c1)
        call t%column(names(pos(2)), c2)
        call t%column(names(pos(3)), c3)
        call t%column(names(pos(4)), c4)
        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            t0 = now()
            do i = 1_int64, nrows
                call c1%get(i, a)
                call c2%get(i, b)
                call c3%get(i, cc)
                call c4%get(i, d)
                acc = acc + (a*b + cc - d)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_lhand = ns_per(best, nrows)
        call emit("J loop, 4 hoisted handles", best, nrows, acc)

        ! --- K/L: the capability half, on a vector column ---------------------------------------
        write(output_unit, '(a)') ""
        write(output_unit, '(a,i0,a)') "  vector column, width ", width, " (ns per access):"
        allocate(v2(width, nrows))
        do i = 1_int64, nrows
            do e = 1, width
                v2(e, i) = real(e, real64) + 0.25_real64*real(i, real64)
            end do
        end do
        call parquet_new_table(tv)
        call tv%add_column("vec", v2)
        call tv%column("vec", cv)
        call cv%get(1_int64, vrow)
        call cv%get(1_int64, 1_int64, v)
        checksum = checksum + vrow(1) + v

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess/4_int64
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call cv%get(i, vrow)
                acc = acc + vrow(1)
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_vrow = ns_per(best, naccess/4_int64)
        call emit("K c%get(i, vec)  [allocates]", best, naccess/4_int64, acc)

        best = huge(1.0_real64)
        do r = 1, rounds
            acc = 0.0_real64
            i = 0_int64
            t0 = now()
            do k = 1_int64, naccess/4_int64
                i = i + 1_int64
                if (i > nrows) i = 1_int64
                call cv%get(i, 1_int64, v)
                acc = acc + v
            end do
            dt = now() - t0
            if (dt < best) best = dt
        end do
        ns_velem = ns_per(best, naccess/4_int64)
        call emit("L c%get(i, e, v)  [no array]", best, naccess/4_int64, acc)

        ! --- derived ----------------------------------------------------------------------------
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  derived:"
        call emit_diff("what a hoisted handle saves (A-B)", ns_name, ns_hand)
        call emit_diff("handle above storage floor (B-D)", ns_hand, ns_getat)
        call emit_diff("cost of re-fetching per cell (C-B)", ns_refetch, ns_hand)
        call emit_diff("name lookup, from creation (E-F)", ns_mkname, ns_mkpos)
        call emit_diff("4-col loop, name vs handle (I-J)", ns_lname, ns_lhand)
        call emit_diff("whole row vs one element (K-L)", ns_vrow, ns_velem)
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  READ THESE FIRST:"
        write(output_unit, '(a)') "   * A is the REGRESSION CONTROL. Compare it with the pre-handle"
        write(output_unit, '(a)') "     figure for the same machine; >5% worse sinks the feature."
        write(output_unit, '(a)') "   * B-D is the ifx question. gfortran ~25 ns of width-independent"
        write(output_unit, '(a)') "     work, ifx ~70. If B-D keeps that 2.8x, the residual is in the"
        write(output_unit, '(a)') "     per-call machinery a handle KEEPS, not in the name lookup."
        write(output_unit, '(a)') "   * G and H price the staleness guard with no rebuild at all."
    end subroutine run_handle

end program benchmark_colindex
