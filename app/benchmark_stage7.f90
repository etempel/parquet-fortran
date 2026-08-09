program benchmark_stage7
    !! Stage 7 gate measurements (see `feature_optimise_A7.md`). One program covering every item, so
    !! that measuring on another machine is a single command rather than a list to get wrong.
    !!
    !! **This measures the CURRENT code.** Most stage 7 items are not implemented; what is reported
    !! here is either the baseline a later "after" run is compared against, or -- for S7-2 and S7-3
    !! -- a gate that can be answered outright with no source change, which is why those two are the
    !! ones worth carrying to another machine first.
    !!
    !! Run it through `tools/benchmark_stage7.sh`, which sets `--profile release` and prints the
    !! toolchain provenance every number here has to be read against. Running the bare executable
    !! without a profile measures an unoptimised build and is meaningless.
    use parquet
    use iso_fortran_env, only: int32, int64, real32, real64, compiler_version
#ifdef _OPENMP
    use omp_lib, only: omp_get_max_threads
#endif
    implicit none

    integer, parameter :: REP = 7                    !! rounds per measurement; the best is reported
    character(len=64) :: only                        !! --only=<item>, or "all"
    integer :: nthreads

    call parse_args(only)
    nthreads = 1
#ifdef _OPENMP
    nthreads = omp_get_max_threads()
#endif
    print "(A)", "=================================================================="
    print "(A)", " parquet-fortran stage 7 gate measurements"
    print "(A)", "=================================================================="
    print "(A,A)", " compiler   : ", trim(compiler_version())
    print "(A,I0)", " omp threads: ", nthreads
    print "(A,A)", " items      : ", trim(only)
    print "(A,I0,A)", " rounds     : ", REP, " (best reported)"
    print "(A)", "------------------------------------------------------------------"

    if (want("s7-2")) call gate_s7_2()
    if (want("s7-1")) call gate_s7_1()
    if (want("s7-4")) call gate_s7_4()
    if (want("s7-5")) call gate_s7_5()
    if (want("s7-6")) call gate_s7_6()
    if (want("s7-9")) call gate_s7_9()
    print "(A)", "=================================================================="
    print "(A)", " end of stage 7 measurements"
    print "(A)", "=================================================================="

contains

    !> Whether item `tag` was selected on the command line.
    logical function want(tag)
        character(len=*), intent(in) :: tag !! item tag, e.g. "s7-2".
        want = (trim(only) == "all") .or. (index(trim(only), tag) > 0)
    end function want

    !> Reads `--only=<list>` from the command line; absent means every item.
    subroutine parse_args(sel)
        character(len=*), intent(out) :: sel !! receives the selection.
        character(len=256) :: arg
        integer :: i
        sel = "all"
        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            if (index(arg, "--only=") == 1) sel = trim(arg(8:))
        end do
    end subroutine parse_args

    !> Elapsed wall-clock seconds, from the highest-resolution clock available.
    subroutine tick(t)
        real(real64), intent(out) :: t !! seconds, from an arbitrary origin.
        integer(int64) :: c, r
        call system_clock(count=c, count_rate=r)
        t = real(c, real64)/real(r, real64)
    end subroutine tick

    ! =============================================================================================
    ! S7-2 (A.12) -- does splitting the range check out of the conversion loop pay?
    !
    ! THE ITEM THAT MUST TRAVEL. Both candidate shapes are written out here rather than measured
    ! through the library, because the library only contains the first one -- the question is what
    ! the second would cost, and the answer is a property of the compiler and the vector width, not
    ! of this project. Destinations are preallocated and prewarmed so the measurement is of the loop
    ! and not of an allocation both shapes would pay equally.
    !
    ! Sizes span the cache hierarchy deliberately: the fused loop is scalar but reads memory once,
    ! the split form vectorises but reads it twice, so the expected answer is "split wins while it
    ! fits in cache, loses once it is bandwidth-bound" and the interesting output is WHERE.
    ! =============================================================================================
    subroutine gate_s7_2()
        integer(int64), parameter :: SIZES(6) = &
            [10000_int64, 100000_int64, 1000000_int64, 4000000_int64, 16000000_int64, 64000000_int64]
        integer :: k
        print "(A)", ""
        print "(A)", "S7-2 (A.12) write-side conversions: fused loop vs split (vectorisable) form"
        print "(A)", "  ns per element. Three shapes per conversion:"
        print "(A)", "    fused  = what the library has today (range check inside the loop)"
        print "(A)", "    any    = A.12's literal proposal: if (any(...)) error stop; dst = int(src)"
        print "(A)", "    reduce = a hand-written BRANCHLESS reduction, then dst = int(src)"
        print "(A)", "  The `reduce` column exists so the gate is not decided against a strawman:"
        print "(A)", "  `any()` over a mask EXPRESSION can materialise a temporary, which would"
        print "(A)", "  lose for a reason that has nothing to do with vectorising the check."
        print "(A)", "  ratio > 1 means that split shape beats the fused loop."
        print "(A)", ""
        print "(A)", "        n    fused    any  ratio  reduce  ratio   conversion"
        do k = 1, size(SIZES)
            call one_size(SIZES(k))
        end do
        print "(A)", ""
        print "(A)", "  HOW TO READ THIS. A.12 assumed the error stop blocks vectorisation, so the"
        print "(A)", "  question is whether it actually does on THIS toolchain. Compare the fused"
        print "(A)", "  figure against one element per clock cycle (0.3 ns on a 3.2 GHz core, 0.25"
        print "(A)", "  on 4 GHz): comfortably under that means several elements are retiring per"
        print "(A)", "  cycle and the loop is already vectorised, so the premise does not hold and"
        print "(A)", "  the item should be dropped here. Do not read a single threshold off this"
        print "(A)", "  line -- the ratio columns are the decision, and a fused figure ABOVE one"
        print "(A)", "  element per cycle with ratios still below 1 means something other than"
        print "(A)", "  vectorisation is the limit and is worth saying so in the write-up."
    end subroutine gate_s7_2

    !> S7-9 -- the integrality test, old form against the shipped one.
    !!
    !! S7-2's table above CANNOT answer this: it replicates the conversion loop inside this program,
    !! so it measures whatever this file writes rather than what the library does. This gate is the
    !! A/B of the actual change -- the same fused loop twice, differing only in how it asks "is this
    !! value a whole number".
    !!
    !! `anint` on real64 means round-half-away-from-zero, for which x86-64 has no SSE/AVX
    !! instruction, so the compiler emits a libm `round()` CALL PER ELEMENT. aarch64 has FRINTA and
    !! pays nothing, which is why this gate is expected to show ~1.0 there and a large ratio on
    !! x86-64. The `new` column calls parquet_is_whole_number across a module boundary, which is if
    !! anything pessimistic: the library's own call sites sit inside parquet_core's own tree.
    subroutine gate_s7_9()
        integer(int64), parameter :: SIZES(3) = [1000000_int64, 16000000_int64, 64000000_int64]
        real(real64), allocatable :: src(:)
        integer(int32), allocatable :: d32(:)
        integer(int64), allocatable :: d64(:)
        real(real64) :: old32, new32, old64, new64
        integer(int64) :: i, n
        integer :: k
        print "(A)", ""
        print "(A)", "S7-9 the float-to-integer integrality test: shipped anint vs a round-trip candidate"
        print "(A)", "  ns per element, whole fused loop (integrality + range + convert)."
        print "(A)", "  ratio > 1 means the CANDIDATE is faster than the anint form the library ships."
        print "(A)", ""
        print "(A)", "        n  anint32  cand32  ratio  anint64  cand64  ratio"
        do k = 1, size(SIZES)
            n = SIZES(k)
            if (allocated(src)) deallocate(src, d32, d64)
            allocate(src(n), d32(n), d64(n))
            do i = 1_int64, n
                src(i) = real(mod(i, 1000_int64), real64)
            end do
            d32 = 0_int32 ; d64 = 0_int64          ! prewarm both destinations (first-touch faults)
            call bench_whole(src, d32, d64, old32, new32, old64, new64)
            print "(I9,6F8.2)", n, old32, new32, old32/new32, old64, new64, old64/new64
        end do
        print "(A)", ""
        print "(A)", "  HOW TO READ THIS. aarch64 has FRINTA, one instruction with anint's exact"
        print "(A)", "  semantics, and measures ratio ~0.4 -- i.e. the candidate LOSES there, badly."
        print "(A)", "  x86-64 has no such instruction and emits a libm round() CALL PER ELEMENT"
        print "(A)", "  (confirmed by an undefined `round` in the object), so the ratio is expected"
        print "(A)", "  to be well above 1. If it is, the two targets want different code and the"
        print "(A)", "  change has to be made per-architecture rather than unconditionally; if it"
        print "(A)", "  is not, S7-9 is dropped outright. Either way this gate, not the mechanism,"
        print "(A)", "  is what decides it."
    end subroutine gate_s7_9

    !> Both integrality forms, both target widths, best of REP rounds.
    subroutine bench_whole(src, d32, d64, old32, new32, old64, new64)
        real(real64), intent(in) :: src(:)       !! source values, all whole and in range.
        integer(int32), intent(inout) :: d32(:)  !! preallocated int32 destination.
        integer(int64), intent(inout) :: d64(:)  !! preallocated int64 destination.
        real(real64), intent(out) :: old32       !! ns/element, anint form, to int32.
        real(real64), intent(out) :: new32       !! ns/element, shipped form, to int32.
        real(real64), intent(out) :: old64       !! ns/element, anint form, to int64.
        real(real64), intent(out) :: new64       !! ns/element, shipped form, to int64.
        real(real64), parameter :: LO32 = -real(huge(0_int32), real64) - 1.0_real64
        real(real64), parameter :: HI32 = real(huge(0_int32), real64)
        real(real64), parameter :: LO64 = -9.2233720368547758e18_real64
        real(real64), parameter :: HI64 = 9.2233720368547758e18_real64
        integer(int64) :: i, n
        integer :: r
        real(real64) :: t0, t1
        n = size(src, kind=int64)
        old32 = huge(1.0_real64) ; new32 = huge(1.0_real64)
        old64 = huge(1.0_real64) ; new64 = huge(1.0_real64)
        do r = 1, REP
            call tick(t0)
            do i = 1_int64, n
                if (src(i) /= anint(src(i))) error stop "integrality"
                if (src(i) < LO32 .or. src(i) > HI32) error stop "range"
                d32(i) = int(src(i), kind=int32)
            end do
            call tick(t1)
            old32 = min(old32, (t1 - t0)*1.0e9_real64/real(n, real64))
            call tick(t0)
            do i = 1_int64, n
                if (.not. whole_roundtrip(src(i))) error stop "integrality"
                if (src(i) < LO32 .or. src(i) > HI32) error stop "range"
                d32(i) = int(src(i), kind=int32)
            end do
            call tick(t1)
            new32 = min(new32, (t1 - t0)*1.0e9_real64/real(n, real64))
            call tick(t0)
            do i = 1_int64, n
                if (src(i) /= anint(src(i))) error stop "integrality"
                if (src(i) < LO64 .or. src(i) >= HI64) error stop "range"
                d64(i) = int(src(i), kind=int64)
            end do
            call tick(t1)
            old64 = min(old64, (t1 - t0)*1.0e9_real64/real(n, real64))
            call tick(t0)
            do i = 1_int64, n
                if (.not. whole_roundtrip(src(i))) error stop "integrality"
                if (src(i) < LO64 .or. src(i) >= HI64) error stop "range"
                d64(i) = int(src(i), kind=int64)
            end do
            call tick(t1)
            new64 = min(new64, (t1 - t0)*1.0e9_real64/real(n, real64))
        end do
    end subroutine bench_whole

    !> The CANDIDATE integrality test S7-9 proposes: no `anint`, so no libm call on x86-64.
    !!
    !! Deliberately local to this program rather than called from the library. Two reasons, and
    !! both would otherwise corrupt the measurement: the library ships the `anint` form (this is
    !! the gate that decides whether it should), and a cross-module call to a library helper is
    !! not inlined, which on aarch64 cost more than the whole loop being measured.
    !!
    !! No NaN test, and none is needed: a NaN satisfies neither `>=` nor `<`, so it falls through
    !! both arms and keeps `.false.` -- the right answer -- while never reaching the `int()`
    !! conversion, which would be undefined for it. Written as two ordered comparisons rather than
    !! an `else` precisely because they are NOT exhaustive under IEEE.
    pure logical function whole_roundtrip(x) result(whole)
        real(real64), intent(in) :: x !! value to test.
        real(real64), parameter :: two52 = 4503599627370496.0_real64 !! last real64 with fractional bits.
        real(real64) :: a
        whole = .false.
        a = abs(x)
        if (a >= two52) then
            whole = .true.
        else if (a < two52) then
            whole = real(int(a, int64), real64) == a
        end if
    end function whole_roundtrip

    !> One row of the S7-2 table.
    subroutine one_size(n)
        integer(int64), intent(in) :: n !! element count.
        integer(int64), allocatable :: si(:)
        real(real64), allocatable :: sf(:)
        integer(int32), allocatable :: d32(:)
        integer(int64), allocatable :: d64(:)
        real(real64) :: a1, b1, c1, a2, b2, c2, a3, b3, c3
        integer(int64) :: i
        allocate(si(n), sf(n), d32(n), d64(n))
        do i = 1_int64, n
            si(i) = mod(i*2654435761_int64, 2000000_int64) - 1000000_int64
            sf(i) = real(mod(i*40503_int64, 2000000_int64) - 1000000_int64, real64)
        end do
        d32 = 0_int32                      ! prewarm both destinations: first-touch page faults
        d64 = 0_int64                      ! would otherwise be charged to whichever ran first
        call bench_i64_i32(si, d32, a1, b1, c1)
        call bench_f64_i32(sf, d32, a2, b2, c2)
        call bench_f64_i64(sf, d64, a3, b3, c3)
        print "(I9,F8.2,F7.2,F7.2,F8.2,F7.2,A)", n, a1, b1, a1/b1, c1, a1/c1, "   i64->i32"
        print "(I9,F8.2,F7.2,F7.2,F8.2,F7.2,A)", n, a2, b2, a2/b2, c2, a2/c2, "   f64->i32"
        print "(I9,F8.2,F7.2,F7.2,F8.2,F7.2,A)", n, a3, b3, a3/b3, c3, a3/c3, "   f64->i64"
    end subroutine one_size

    !> int64 -> int32, both shapes.
    subroutine bench_i64_i32(src, dst, fused, split, reduce)
        integer(int64), intent(in) :: src(:)   !! source values.
        integer(int32), intent(inout) :: dst(:) !! preallocated destination.
        real(real64), intent(out) :: fused      !! ns/element, current shape.
        real(real64), intent(out) :: split      !! ns/element, A.12's literal `any()` proposal.
        real(real64), intent(out) :: reduce     !! ns/element, branchless reduction + convert.
        integer(int64) :: i, n
        integer :: r
        logical :: bad
        real(real64) :: t0, t1
        n = size(src, kind=int64)
        fused = huge(1.0_real64) ; split = huge(1.0_real64) ; reduce = huge(1.0_real64)
        do r = 1, REP
            call tick(t0)
            do i = 1_int64, n                                  ! the shape the library has today
                if (src(i) < -huge(0_int32) - 1_int64 .or. src(i) > huge(0_int32)) error stop "range"
                dst(i) = int(src(i), kind=int32)
            end do
            call tick(t1)
            fused = min(fused, (t1 - t0)*1.0e9_real64/real(n, real64))
            call tick(t0)
            if (any(src < -huge(0_int32) - 1_int64 .or. src > huge(0_int32))) error stop "range"
            dst = int(src, kind=int32)
            call tick(t1)
            split = min(split, (t1 - t0)*1.0e9_real64/real(n, real64))
            call tick(t0)
            bad = .false.                                      ! branchless, no early exit
            do i = 1_int64, n
                bad = bad .or. src(i) < -huge(0_int32) - 1_int64 .or. src(i) > huge(0_int32)
            end do
            if (bad) error stop "range"
            dst = int(src, kind=int32)
            call tick(t1)
            reduce = min(reduce, (t1 - t0)*1.0e9_real64/real(n, real64))
        end do
    end subroutine bench_i64_i32

    !> float64 -> int32, both shapes. Keeps the exact-equality integrality test unchanged.
    subroutine bench_f64_i32(src, dst, fused, split, reduce)
        real(real64), intent(in) :: src(:)      !! source values.
        integer(int32), intent(inout) :: dst(:) !! preallocated destination.
        real(real64), intent(out) :: fused      !! ns/element, current shape.
        real(real64), intent(out) :: split      !! ns/element, A.12's literal `any()` proposal.
        real(real64), intent(out) :: reduce     !! ns/element, branchless reduction + convert.
        real(real64), parameter :: LO = -real(huge(0_int32), real64) - 1.0_real64
        real(real64), parameter :: HI = real(huge(0_int32), real64)
        integer(int64) :: i, n
        integer :: r
        logical :: bad
        real(real64) :: t0, t1
        n = size(src, kind=int64)
        fused = huge(1.0_real64) ; split = huge(1.0_real64) ; reduce = huge(1.0_real64)
        do r = 1, REP
            call tick(t0)
            do i = 1_int64, n
                if (src(i) /= anint(src(i))) error stop "integrality"
                if (src(i) < LO .or. src(i) > HI) error stop "range"
                dst(i) = int(src(i), kind=int32)
            end do
            call tick(t1)
            fused = min(fused, (t1 - t0)*1.0e9_real64/real(n, real64))
            call tick(t0)
            if (any(src /= anint(src))) error stop "integrality"
            if (any(src < LO .or. src > HI)) error stop "range"
            dst = int(src, kind=int32)
            call tick(t1)
            split = min(split, (t1 - t0)*1.0e9_real64/real(n, real64))
            call tick(t0)
            bad = .false.                                      ! branchless, no early exit
            do i = 1_int64, n
                bad = bad .or. src(i) /= anint(src(i)) .or. src(i) < LO .or. src(i) > HI
            end do
            if (bad) error stop "range"
            dst = int(src, kind=int32)
            call tick(t1)
            reduce = min(reduce, (t1 - t0)*1.0e9_real64/real(n, real64))
        end do
    end subroutine bench_f64_i32

    !> float64 -> int64, both shapes.
    subroutine bench_f64_i64(src, dst, fused, split, reduce)
        real(real64), intent(in) :: src(:)      !! source values.
        integer(int64), intent(inout) :: dst(:) !! preallocated destination.
        real(real64), intent(out) :: fused      !! ns/element, current shape.
        real(real64), intent(out) :: split      !! ns/element, A.12's literal `any()` proposal.
        real(real64), intent(out) :: reduce     !! ns/element, branchless reduction + convert.
        real(real64), parameter :: LO = -9.2233720368547758e18_real64
        real(real64), parameter :: HI = 9.2233720368547758e18_real64
        integer(int64) :: i, n
        integer :: r
        logical :: bad
        real(real64) :: t0, t1
        n = size(src, kind=int64)
        fused = huge(1.0_real64) ; split = huge(1.0_real64) ; reduce = huge(1.0_real64)
        do r = 1, REP
            call tick(t0)
            do i = 1_int64, n
                if (src(i) /= anint(src(i))) error stop "integrality"
                if (src(i) < LO .or. src(i) > HI) error stop "range"
                dst(i) = int(src(i), kind=int64)
            end do
            call tick(t1)
            fused = min(fused, (t1 - t0)*1.0e9_real64/real(n, real64))
            call tick(t0)
            if (any(src /= anint(src))) error stop "integrality"
            if (any(src < LO .or. src > HI)) error stop "range"
            dst = int(src, kind=int64)
            call tick(t1)
            split = min(split, (t1 - t0)*1.0e9_real64/real(n, real64))
            call tick(t0)
            bad = .false.                                      ! branchless, no early exit
            do i = 1_int64, n
                bad = bad .or. src(i) /= anint(src(i)) .or. src(i) < LO .or. src(i) > HI
            end do
            if (bad) error stop "range"
            dst = int(src, kind=int64)
            call tick(t1)
            reduce = min(reduce, (t1 - t0)*1.0e9_real64/real(n, real64))
        end do
    end subroutine bench_f64_i64

    ! =============================================================================================
    ! S7-1 (A.6 second half) -- baseline for the sort-key validity pass.
    !
    ! The pass being measured runs ONLY on a column that has nulls (stage 5's early return skips it
    ! otherwise), and its replacement would cost proportional to the NULL COUNT rather than to n --
    ! so the density sweep is the measurement, not a detail. int32 is used because it takes the
    ! counting-sort fast path, leaving the prologue visible; a float64 column's comparison sort
    ! would swamp it, which is exactly how stage 5 nearly mis-read this.
    ! =============================================================================================
    subroutine gate_s7_1()
        integer(int64), parameter :: N = 4000000_int64
        integer, parameter :: EVERY(4) = [0, 1000, 10, 2]        ! none, 0.1%, 10%, 50%
        character(len=12), parameter :: LABEL(4) = &
            [character(len=12) :: "none", "0.1%", "10%", "50%"]
        integer :: k
        print "(A)", ""
        print "(A)", "S7-1 (A.6 ii) pf_argsort over an int32 parquet_column, 4M rows, by null density"
        print "(A)", "  ms; 'none' is the stage 5 early-return path and should be the fastest"
        do k = 1, 4
            print "(A,A8,A,F9.2)", "  nulls ", trim(LABEL(k)), " : ", sort_with_nulls(N, EVERY(k))
        end do
    end subroutine gate_s7_1

    !> Best-of-REP ms for `pf_argsort` over an int32 column with every `every`-th row null.
    real(real64) function sort_with_nulls(n, every) result(best)
        integer(int64), intent(in) :: n !! row count.
        integer, intent(in) :: every    !! null every this many rows; 0 for none.
        type(parquet_column) :: c
        integer(int32), allocatable :: v(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: i
        integer :: r
        real(real64) :: t0, t1
        allocate(v(n), perm(n))
        do i = 1_int64, n
            v(i) = int(mod(i*2654435761_int64, 1000003_int64), int32)
        end do
        best = huge(1.0_real64)
        do r = 1, REP
            call c%clear()
            call c%init(PK_INT32, n)
            call c%set_all(v)
            if (every > 0) then
                do i = 1_int64, n, int(every, int64)
                    call c%set_null(i)
                end do
            end if
            call pf_argsort(c, perm)                     ! warm
            call tick(t0)
            call pf_argsort(c, perm)
            call tick(t1)
            best = min(best, (t1 - t0)*1.0e3_real64)
        end do
    end function sort_with_nulls

    ! =============================================================================================
    ! S7-4 (A.10) -- baseline for per-element validity dispatch on the TEMPORAL kinds.
    !
    ! Only the temporal and string kinds take this path; every numeric kind already walks the
    ! bitmap, and stage 6 made that walk faster still. A numeric fixture here would correctly show
    ! nothing while proving nothing, so the comparison below is temporal-against-numeric rather than
    ! before-against-after.
    ! =============================================================================================
    subroutine gate_s7_4()
        integer(int64), parameter :: N = 2000000_int64
        print "(A)", ""
        print "(A)", "S7-4 (A.10) element_validity, 2M rows -- temporal (per-element dispatch)"
        print "(A)", "  vs int32 (bitmap walk). ms; the GAP is what specialising could recover."
        print "(A,F9.2)", "  int32 (bitmap walk)      : ", validity_ms(N, PK_INT32)
        print "(A,F9.2)", "  date  (per-element)      : ", validity_ms(N, PK_DATE)
        print "(A,F9.2)", "  timestamp (per-element)  : ", validity_ms(N, PK_TIMESTAMP)
    end subroutine gate_s7_4

    !> Best-of-REP ms for `%element_validity` over an `n`-row column of `kind`, 10% null.
    real(real64) function validity_ms(n, kind) result(best)
        integer(int64), intent(in) :: n !! row count.
        integer, intent(in) :: kind     !! PK_* column kind.
        type(parquet_column) :: c
        logical, allocatable :: mask(:,:)
        integer(int64) :: i
        integer :: r
        real(real64) :: t0, t1
        best = huge(1.0_real64)
        do r = 1, REP
            call c%clear()
            call c%init(kind, n)
            do i = 1_int64, n, 10_int64
                call c%set_null(i)
            end do
            if (allocated(mask)) deallocate(mask)
            call c%element_validity(mask)              ! warm
            deallocate(mask)
            call tick(t0)
            call c%element_validity(mask)
            call tick(t1)
            best = min(best, (t1 - t0)*1.0e3_real64)
        end do
    end function validity_ms

    ! =============================================================================================
    ! S7-5 (A.13) -- baseline for refill_string_store, reached through %set_all on a character array.
    ! =============================================================================================
    subroutine gate_s7_5()
        integer(int64), parameter :: N = 1000000_int64
        type(parquet_column) :: c
        character(len=24), allocatable :: v(:)
        integer(int64) :: i
        integer :: r
        real(real64) :: t0, t1, best
        allocate(v(N))
        do i = 1_int64, N
            write(v(i), "(A,I0)") "row_", i               ! varied length: the trimming path matters
        end do
        best = huge(1.0_real64)
        do r = 1, REP
            call c%clear()
            call c%init(PK_STRING, N)
            call tick(t0)
            call c%set_all(v)
            call tick(t1)
            best = min(best, (t1 - t0)*1.0e3_real64)
        end do
        print "(A)", ""
        print "(A)", "S7-5 (A.13) building a 1M-row string column via %set_all (character(len=24))"
        print "(A,F9.2)", "  ms : ", best
    end subroutine gate_s7_5

    ! =============================================================================================
    ! S7-6 (A.14) -- baseline for pf_permute on an already-int64 permutation.
    ! =============================================================================================
    subroutine gate_s7_6()
        integer(int64), parameter :: N = 4000000_int64
        real(real64), allocatable :: v(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: i
        integer :: r
        real(real64) :: t0, t1, best
        allocate(v(N), perm(N))
        do i = 1_int64, N
            v(i) = real(i, real64)
            perm(i) = N - i + 1_int64
        end do
        best = huge(1.0_real64)
        do r = 1, REP
            call tick(t0)
            call pf_permute(v, perm)
            call tick(t1)
            best = min(best, (t1 - t0)*1.0e3_real64)
        end do
        print "(A)", ""
        print "(A)", "S7-6 (A.14) pf_permute over 4M float64 with an int64 permutation"
        print "(A,F9.2)", "  ms : ", best
    end subroutine gate_s7_6

end program benchmark_stage7
