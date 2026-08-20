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
    use iso_c_binding, only: c_int8_t, c_int32_t, c_int64_t
    use iso_fortran_env, only: int32, int64, real64, compiler_version
#ifdef _OPENMP
    use omp_lib, only: omp_get_max_threads
#endif
    implicit none

    integer, parameter :: REP = 7                    !! rounds per measurement; the best is reported
    character(len=64) :: only                        !! --only=<item>, or "all"
    integer :: nthreads

    !> Layout-identical local twin of `parquet_date`, whose own components are private. Used only by
    !> S7-3's impure-versus-class experiment; see `gate_s7_3_purity` for why a replication is needed
    !> and what ties it back to the shipped figure.
    type :: bench_date
        integer(int32) :: days = 0_int32 !! days since 1970-01-01.
        logical :: valid = .false.       !! .false. = null.
    end type bench_date

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
    if (want("s7-3")) call gate_s7_3()
    if (want("s7-4")) call gate_s7_4()
    if (want("s7-5")) call gate_s7_5()
    if (want("s7-6")) call gate_s7_6()
    if (want("s7-7")) call gate_s7_7()
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
        call whole_end_to_end()
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

    !> What share of a REAL write the integrality test is, which is what decides S7-9.
    !!
    !! The table above times the conversion loop alone. That loop is not reachable on its own: it
    !! runs inside `parquet_write_column` when the schema declares an integer column and the caller
    !! passes floats, and the rest of that call -- Arrow append, encoding, compression, file I/O --
    !! is charged to the same user. The keep-or-drop rule in feature_optimise_A7.md is explicit that
    !! a gain only reachable through a path that swamps it is dropped, so the ratio above is not on
    !! its own an argument for changing anything.
    !!
    !! Reports the whole write, and the share the integrality test accounts for at this machine's
    !! own measured cost. The CEILING on any end-to-end gain is share*(1 - 1/ratio) -- print it
    !! rather than leaving it to be estimated, because that is the number the decision needs.
    subroutine whole_end_to_end()
        integer(int64), parameter :: NROW = 4000000_int64
        character(len=*), parameter :: OUT = "test_run/s7-9-endtoend.parquet"
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64), allocatable :: src(:)
        integer(int32), allocatable :: d32(:)
        integer(int64), allocatable :: d64(:)
        real(real64) :: t0, t1, total_ms, anint32, cand32, anint64, cand64
        real(real64) :: conv_ms, share, ratio, ceiling
        integer(int64) :: i
        allocate(src(NROW), d32(NROW), d64(NROW))
        do i = 1_int64, NROW
            src(i) = real(mod(i, 1000_int64), real64)
        end do
        d32 = 0_int32 ; d64 = 0_int64
        ! This machine's own per-element costs, measured exactly as the table above, so both the
        ! share and the ratio below come from this run rather than from another machine's figures.
        call bench_whole(src, d32, d64, anint32, cand32, anint64, cand64)
        call schema%init(table="s7_9_endtoend")
        call schema%add_field("v", "int32")
        call tick(t0)
        call parquet_open_writer(writer, OUT, schema)
        call parquet_write_column(writer, "v", src)
        call parquet_close_writer(writer)
        call tick(t1)
        total_ms = (t1 - t0)*1000.0_real64
        conv_ms = anint32*real(NROW, real64)*1.0e-6_real64
        share = conv_ms/total_ms
        ratio = anint32/cand32
        ! share * (1 - 1/ratio): the fraction of the write the loop occupies, times the fraction of
        ! the loop the candidate removes. An upper bound, since it credits the change with the whole
        ! difference and charges it nothing.
        ceiling = share*(1.0_real64 - 1.0_real64/ratio)
        print "(A)", ""
        print "(A)", "  End-to-end: 4M float64 rows written through an int32 schema column."
        print "(A,F10.2)", "    whole open + parquet_write_column + close, ms : ", total_ms
        print "(A,F10.2)", "    integrality+range+convert loop alone,      ms : ", conv_ms
        print "(A,F9.1,A)", "    that loop's share of the write                : ", share*100.0_real64, " %"
        print "(A,F9.2)",   "    this machine's candidate/anint ratio (int32)  : ", ratio
        print "(A,F9.1,A)", "    CEILING on the end-to-end gain                : ", ceiling*100.0_real64, " %"
        print "(A)", "    The ceiling is share*(1 - 1/ratio) and is generous: it credits the"
        print "(A)", "    change with the whole difference and charges it nothing. Compare it"
        print "(A)", "    against the ~5% line the keep-or-drop rule uses. The conversion is"
        print "(A)", "    NOT callable on its own -- it is reached only through this write -- so"
        print "(A)", "    this number, not the loop ratio, is what the rule asks for."
    end subroutine whole_end_to_end

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
    ! S7-3 (A.11) -- THE GATE, and it can retire the item outright.
    !
    ! The eleven read-side temporal loops build one element per call: `%set_raw` for date/time,
    ! `%set_unix` for timestamp (which additionally divides and takes a modulo per element, by a
    ! divisor that is constant for the whole column). What that costs is bounded by the gap between
    ! reading a temporal column and reading the SAME PHYSICAL COLUMN as a plain integer -- same byte
    ! count, same Arrow decode, same file. Whatever separates them is the element construction, and
    ! nothing else.
    !
    ! The two pairings are chosen so the physical type matches exactly:
    !
    !   timestamp[us] <-> int64   (both INT64 physical)
    !   date          <-> int32   (both INT32 physical)
    !
    ! and the two columns of each pair are written holding the IDENTICAL integers, so encoded size,
    ! dictionary behaviour and page layout match as closely as a real file allows.
    !
    ! Read the gap, not the ratio alone: a large ratio on a cheap read is worth less than a small
    ! ratio on an expensive one, and it is the absolute milliseconds that any later change competes
    ! against. If the gap is small, the read is Arrow-decode-dominated and S7-3 drops with no source
    ! change at all -- which is the outcome this gate exists to be able to reach.
    ! =============================================================================================
    subroutine gate_s7_3()
        integer(int64), parameter :: N = 4000000_int64
        character(len=*), parameter :: file = "test_run/bench_s7_3.parquet"
        real(real64) :: t_i64, t_ts, t_i32, t_date
        call write_temporal_pairs(file, N)
        t_i64  = read_ms(file, "i64",  N, 1)
        t_ts   = read_ms(file, "ts",   N, 2)
        t_i32  = read_ms(file, "i32",  N, 3)
        t_date = read_ms(file, "date", N, 4)
        print "(A)", ""
        print "(A)", "S7-3 (A.11) temporal element construction, 4M rows, whole-column read"
        print "(A)", "  Each pair is the same physical column type holding the same integers, so"
        print "(A)", "  the GAP is the per-element construction and nothing else. ms, best of 7."
        print "(A)", ""
        print "(A,F9.2)", "  int64        (memcpy-ish)      : ", t_i64
        print "(A,F9.2)", "  timestamp[us] (%set_unix)      : ", t_ts
        print "(A,F9.2,A,F6.2,A)", "    gap                          : ", t_ts - t_i64, &
            "   (ratio ", t_ts/max(t_i64, 1.0e-9_real64), ")"
        print "(A)", ""
        print "(A,F9.2)", "  int32        (memcpy-ish)      : ", t_i32
        print "(A,F9.2)", "  date          (%set_raw)       : ", t_date
        print "(A,F9.2,A,F6.2,A)", "    gap                          : ", t_date - t_i32, &
            "   (ratio ", t_date/max(t_i32, 1.0e-9_real64), ")"
        call gate_s7_3_loops(N, t_ts - t_i64, t_date - t_i32)
    end subroutine gate_s7_3

    !> Decomposes the gap above. The end-to-end gap is NOT the per-element loop alone: the temporal
    !> read also allocates an `n`-element scratch buffer Arrow decodes into, and *always* allocates a
    !> validity buffer, where the numeric read decodes straight into the caller's array and allocates
    !> no validity buffer at all unless one was asked for. So the loop measured here is a LOWER bound
    !> on the gap, and the difference is the buffers. Both are S7-3's target, but they want different
    !> fixes, which is why they are separated before anything is written.
    !>
    !> The shipped loop and each candidate run over the same pre-populated buffers in this same file,
    !> so none of them pays a cross-module call the others do not. **The tie-down that makes this
    !> evidence rather than three confident numbers** (CLAUDE.md: a benchmark that replicates library
    !> code is untested code) is that the shipped row must come in at or below the end-to-end gap
    !> passed in; it is printed alongside for exactly that check.
    subroutine gate_s7_3_loops(n, gap_ts, gap_date)
        integer(int64), intent(in) :: n     !! element count.
        real(real64), intent(in) :: gap_ts  !! measured end-to-end timestamp gap, ms.
        real(real64), intent(in) :: gap_date!! measured end-to-end date gap, ms.
        integer(c_int32_t), allocatable :: days(:)
        integer(c_int64_t), allocatable :: vals(:)
        integer(c_int8_t), allocatable :: valid(:)
        type(parquet_date), allocatable :: d(:)
        type(parquet_timestamp), allocatable :: ts(:)
        integer(int64) :: i
        integer :: r
        real(real64) :: t0, t1, b_ship, b_elem, b_two, b_guard, b_ship_date
        allocate(days(n), vals(n), valid(n), d(n), ts(n))
        do i = 1_int64, n
            days(i) = int(mod(i, 14600_int64), c_int32_t)
            vals(i) = mod(i, 14600_int64)*86400000000_int64 + mod(i*7919_int64, 86400000000_int64)
            valid(i) = 1_c_int8_t
        end do
        call d%set_raw(days)                    ! warm both destinations' pages before any timing
        call ts%set_unix(vals, parquet_unit_micros)

        ! ---- date ----
        b_ship = huge(1.0_real64); b_elem = b_ship; b_two = b_ship; b_guard = b_ship
        do r = 1, REP
            call tick(t0)
            do i = 1_int64, n
                if (valid(i) /= 0_c_int8_t) then
                    call d(i)%set_raw(days(i))
                else
                    call d(i)%set_null()
                end if
            end do
            call tick(t1); b_ship = min(b_ship, (t1 - t0)*1.0e3_real64)
            call tick(t0)
            call d%set_raw(days)
            call tick(t1); b_elem = min(b_elem, (t1 - t0)*1.0e3_real64)
            call tick(t0)
            call d%set_raw(days)
            do i = 1_int64, n
                if (valid(i) == 0_c_int8_t) call d(i)%set_null()
            end do
            call tick(t1); b_two = min(b_two, (t1 - t0)*1.0e3_real64)
            call tick(t0)
            call d%set_raw(days)
            if (any(valid == 0_c_int8_t)) then
                do i = 1_int64, n
                    if (valid(i) == 0_c_int8_t) call d(i)%set_null()
                end do
            end if
            call tick(t1); b_guard = min(b_guard, (t1 - t0)*1.0e3_real64)
        end do
        print "(A)", ""
        print "(A)", "  -- gap decomposition: the CONSTRUCTION LOOP alone, over ready buffers --"
        print "(A)", "     (shipped must be <= the end-to-end gap above; the rest is the buffers)"
        print "(A,F9.2,A,F9.2,A)", "  date  shipped  per-element call : ", b_ship, &
            "    of a ", gap_date, " ms gap"
        print "(A,F9.2)", "  date  elemental whole-array      : ", b_elem
        print "(A,F9.2)", "  date  elemental + null fix-up    : ", b_two
        print "(A,F9.2)", "  date  elemental + any() guard    : ", b_guard
        b_ship_date = b_ship

        ! ---- timestamp ----
        b_ship = huge(1.0_real64); b_elem = b_ship; b_two = b_ship; b_guard = b_ship
        do r = 1, REP
            call tick(t0)
            do i = 1_int64, n
                if (valid(i) /= 0_c_int8_t) then
                    call ts(i)%set_unix(vals(i), parquet_unit_micros)
                else
                    call ts(i)%set_null()
                end if
            end do
            call tick(t1); b_ship = min(b_ship, (t1 - t0)*1.0e3_real64)
            call tick(t0)
            call ts%set_unix(vals, parquet_unit_micros)
            call tick(t1); b_elem = min(b_elem, (t1 - t0)*1.0e3_real64)
            call tick(t0)
            call ts%set_unix(vals, parquet_unit_micros)
            do i = 1_int64, n
                if (valid(i) == 0_c_int8_t) call ts(i)%set_null()
            end do
            call tick(t1); b_two = min(b_two, (t1 - t0)*1.0e3_real64)
            call tick(t0)
            call ts%set_unix(vals, parquet_unit_micros)
            if (any(valid == 0_c_int8_t)) then
                do i = 1_int64, n
                    if (valid(i) == 0_c_int8_t) call ts(i)%set_null()
                end do
            end if
            call tick(t1); b_guard = min(b_guard, (t1 - t0)*1.0e3_real64)
        end do
        print "(A)", ""
        print "(A,F9.2,A,F9.2,A)", "  ts    shipped  per-element call : ", b_ship, &
            "    of a ", gap_ts, " ms gap"
        print "(A,F9.2)", "  ts    elemental whole-array      : ", b_elem
        print "(A,F9.2)", "  ts    elemental + null fix-up    : ", b_two
        print "(A,F9.2)", "  ts    elemental + any() guard    : ", b_guard
        call gate_s7_3_bandwidth(n)
        call gate_s7_3_purity(n, b_ship_date)
    end subroutine gate_s7_3_loops

    !> Falsifies (or confirms) the reading that the construction loop is memory-bound rather than
    !> call- or arithmetic-bound. Each reference below moves exactly the bytes the matching loop
    !> above moves, with NO arithmetic and no procedure call of any kind -- a plain strided store.
    !> If a loop is at or near its reference, no rewrite that still writes those bytes can help,
    !> whatever it does about calls or divisors; if it is far above, the arithmetic is real.
    !>
    !> Note what a positive result would mean for A.11 specifically: its stated target is a division
    !> and a modulo per element. `set_unix` does both and `set_raw` does neither, so if the timestamp
    !> loop reaches a HIGHER byte rate than the date loop, the divisor cannot be what limits it.
    subroutine gate_s7_3_bandwidth(n)
        integer(int64), intent(in) :: n !! element count.
        integer(int64), allocatable :: dst8(:), dst16(:), src8(:)
        integer(int32), allocatable :: src4(:)
        integer(int64) :: i
        integer :: r
        real(real64) :: t0, t1, b8, b16
        allocate(dst8(n), dst16(2*n), src4(n), src8(n))
        dst8 = 0_int64; dst16 = 0_int64; src4 = 1_int32; src8 = 1_int64
        b8 = huge(1.0_real64); b16 = b8
        do r = 1, REP
            call tick(t0)                                   ! date's traffic: read 4B, write 8B
            do i = 1_int64, n
                dst8(i) = int(src4(i), int64)
            end do
            call tick(t1); b8 = min(b8, (t1 - t0)*1.0e3_real64)
            call tick(t0)                                   ! timestamp's traffic: read 8B, write 16B
            do i = 1_int64, n
                dst16(2*i-1) = src8(i)
                dst16(2*i) = 0_int64
            end do
            call tick(t1); b16 = min(b16, (t1 - t0)*1.0e3_real64)
        end do
        print "(A)", ""
        print "(A)", "  -- reference: the same bytes moved, no call and no arithmetic --"
        print "(A,F9.2)", "  date-shaped  store 8B/elem      : ", b8
        print "(A,F9.2)", "  ts-shaped    store 16B/elem     : ", b16
    end subroutine gate_s7_3_bandwidth


    !> The construction loop is 10-15x off the bytes it moves, so something per element is real.
    !> Two attributes of the shipped setters can each explain that, and they imply COMPLETELY
    !> different fixes, so this separates them before either is attempted:
    !>
    !>   * `impure` -- an impure elemental procedure must be called once per element in array
    !>     order, which blocks vectorisation and inlining. If this is the cause, the fix is one
    !>     word on each setter whose body permits it.
    !>   * `class` -- the passed-object dummy of a type-bound procedure MUST be polymorphic per the
    !>     standard, so if this is the cause the fix cannot be a type-bound procedure at all, and
    !>     the plan's option 1 (a bulk, non-type-bound entry point) is the only route.
    !>
    !> `bench_date` replicates `parquet_date`'s layout because that type's components are private.
    !> A replication is untested code (CLAUDE.md), so the tie-down is the FIRST row: the impure+class
    !> twin must reproduce the shipped figure printed above. If it does not, no other row here means
    !> anything and the replication is wrong, not the library.
    subroutine gate_s7_3_purity(n, shipped_ms)
        integer(int64), intent(in) :: n        !! element count.
        real(real64), intent(in) :: shipped_ms !! the shipped date loop's measured ms, for the tie-down.
        integer(int32), allocatable :: days(:)
        type(bench_date), allocatable :: d(:)
        integer(int64) :: i
        integer :: r
        real(real64) :: t0, t1, b_ic, b_pc, b_it, b_pt
        allocate(days(n), d(n))
        do i = 1_int64, n
            days(i) = int(mod(i, 14600_int64), int32)
        end do
        call set_ic(d, days)                                ! warm the destination's pages
        b_ic = huge(1.0_real64); b_pc = b_ic; b_it = b_ic; b_pt = b_ic
        do r = 1, REP
            call tick(t0); call set_ic(d, days); call tick(t1)
            b_ic = min(b_ic, (t1 - t0)*1.0e3_real64)
            call tick(t0); call set_ici(d, days); call tick(t1)
            b_pc = min(b_pc, (t1 - t0)*1.0e3_real64)
            call tick(t0); call set_pci(d, days); call tick(t1)
            b_it = min(b_it, (t1 - t0)*1.0e3_real64)
            call tick(t0); call set_pt(d, days); call tick(t1)
            b_pt = min(b_pt, (t1 - t0)*1.0e3_real64)
        end do
        print "(A)", ""
        print "(A)", "  -- why: impure vs class, on a layout-identical local twin of parquet_date --"
        print "(A,F9.2,A,F9.2,A)", "  impure elemental, class(t)      : ", b_ic, &
            "    (tie-down: shipped was ", shipped_ms, ")"
        print "(A,F9.2)", "  impure elemental, class, inout  : ", b_pc
        print "(A,F9.2)", "  PURE   elemental, class, inout  : ", b_it
        print "(A,F9.2)", "  PURE   elemental, type,  out    : ", b_pt
        print "(A)", "  (pure + class + intent(out) is not expressible: the standard forbids a"
        print "(A)", "   polymorphic intent(out) dummy in a pure procedure, so if purity is what"
        print "(A)", "   matters the fix cannot stay a type-bound procedure with intent(out).)"
    end subroutine gate_s7_3_purity

    !> The four twins. Each writes exactly what `date_set_raw` writes; they differ only in the two
    !> attributes under test. `intent(out)` is kept because the shipped setter has it and it is not
    !> free -- it default-initialises the element on entry.
    impure elemental subroutine set_ic(self, days)
        class(bench_date), intent(out) :: self !! receives the value.
        integer(int32), intent(in) :: days     !! day count.
        self%days = days
        self%valid = .true.
    end subroutine set_ic

    !> `intent(inout)` rather than `intent(out)`: the cell above cannot be made pure, because a pure
    !! procedure may not have a POLYMORPHIC INTENT(OUT) dummy (gfortran rejects it outright, per the
    !! standard). Since both components are assigned unconditionally, dropping to `intent(inout)` is
    !! semantically identical here and is what makes the pure cell below expressible at all.
    impure elemental subroutine set_ici(self, days)
        class(bench_date), intent(inout) :: self !! receives the value.
        integer(int32), intent(in) :: days       !! day count.
        self%days = days
        self%valid = .true.
    end subroutine set_ici

    pure elemental subroutine set_pci(self, days)
        class(bench_date), intent(inout) :: self !! receives the value.
        integer(int32), intent(in) :: days       !! day count.
        self%days = days
        self%valid = .true.
    end subroutine set_pci

    pure elemental subroutine set_pt(self, days)
        type(bench_date), intent(out) :: self !! receives the value.
        integer(int32), intent(in) :: days    !! day count.
        self%days = days
        self%valid = .true.
    end subroutine set_pt

    !> Writes the S7-3 fixture: four columns, `N` rows, paired so that `i64` holds exactly the
    !> microsecond values `ts` represents and `i32` exactly the day counts `date` represents.
    subroutine write_temporal_pairs(file, n)
        character(len=*), intent(in) :: file !! output path.
        integer(int64), intent(in) :: n      !! row count.
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        integer(int32), allocatable :: i32(:)
        integer(int64), allocatable :: i64(:)
        type(parquet_date), allocatable :: d(:)
        type(parquet_timestamp), allocatable :: ts(:)
        integer(int64) :: i
        allocate(i32(n), i64(n), d(n), ts(n))
        do i = 1_int64, n
            ! Spread over ~40 years of days and the matching microseconds, so neither column is
            ! degenerate enough for the dictionary encoder to change the comparison.
            i32(i) = int(mod(i, 14600_int64), int32)
            i64(i) = mod(i, 14600_int64)*86400000000_int64 + mod(i*7919_int64, 86400000000_int64)
            call d(i)%set_raw(i32(i))
            call ts(i)%set_unix(i64(i), parquet_unit_micros)
        end do
        call schema%init("s7_3_pairs")
        call schema%add_field("i32", "int32")
        call schema%add_field("i64", "int64")
        call schema%add_field("date", "date")
        call schema%add_field("ts", "timestamp[us]")
        call parquet_open_writer(writer, file, schema=schema)
        call parquet_write_column(writer, "i32", i32)
        call parquet_write_column(writer, "i64", i64)
        call parquet_write_column(writer, "date", d)
        call parquet_write_column(writer, "ts", ts)
        call parquet_close_writer(writer)
    end subroutine write_temporal_pairs

    !> Best-of-REP ms for a whole-column read of `col` from `file`. `which` selects the destination
    !> array's type; a fresh reader per round keeps every round identical, and the destination is
    !> written through once before the timer so no round pays its first-touch page faults.
    real(real64) function read_ms(file, col, n, which) result(best)
        character(len=*), intent(in) :: file !! fixture path.
        character(len=*), intent(in) :: col  !! column name.
        integer(int64), intent(in) :: n      !! row count.
        integer, intent(in) :: which         !! 1 = int64, 2 = timestamp, 3 = int32, 4 = date.
        type(parquet_reader) :: reader
        integer(int32), allocatable :: b32(:)
        integer(int64), allocatable :: b64(:)
        type(parquet_date), allocatable :: bd(:)
        type(parquet_timestamp), allocatable :: bts(:)
        integer :: r
        real(real64) :: t0, t1
        select case (which)
        case (1)
            allocate(b64(n)); b64 = 0_int64
        case (2)
            allocate(bts(n)); call bts(:)%set_null()
        case (3)
            allocate(b32(n)); b32 = 0_int32
        case (4)
            allocate(bd(n)); call bd(:)%set_null()
        end select
        best = huge(1.0_real64)
        do r = 1, REP
            call parquet_open_reader(reader, file)
            call tick(t0)
            select case (which)
            case (1)
                call parquet_read_column(reader, col, b64)
            case (2)
                call parquet_read_column(reader, col, bts)
            case (3)
                call parquet_read_column(reader, col, b32)
            case (4)
                call parquet_read_column(reader, col, bd)
            end select
            call tick(t1)
            call parquet_close_reader(reader)
            best = min(best, (t1 - t0)*1.0e3_real64)
        end do
    end function read_ms

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
        call gate_s7_5_where(v, N, best)
    end subroutine gate_s7_5

    !> S7-5's PRECONDITION: `%set_all` is 63 ms -- but the item only proposes replacing ONE of its
    !> two passes, so what matters is how that 63 ms divides and what the replacement's floor is.
    !>
    !> `refill_string_store` (`src/parquet_columns_string.f90`) already does the right thing in pass
    !> 1: one `len_trim` sweep giving the exact byte count, then a single exact-size `%reserve`. A.13
    !> says to keep that. Pass 2 is `n` calls to `%append_string`, and each of those re-checks two
    !> capacities that `%reserve` has just guaranteed, calls `process_bounds` to redo the trim, and
    !> copies the payload with `transfer(str(lo:hi), self%data, slen)` -- a temporary per element,
    !> since the store's payload is `character(len=1), allocatable :: data(:)`.
    !>
    !> So the rows below separate the sizing pass (which no design removes) from the rebuild (which
    !> is the whole prize), and then bound the rebuild two ways:
    !>
    !>   * `substring` -- a prefix-sum plus `payload(a:b) = v(k)(1:L)` into a `character(len=:)`
    !>     scalar. One memcpy per element, no temporary, no call. This is the FLOOR: no bulk build
    !>     can beat it, because these are the bytes that have to move.
    !>   * `transfer` -- the same loop writing into a `character(len=1)` array the way the store
    !>     actually declares its payload. The gap between the two is what `transfer` costs, and it
    !>     is the difference between "S7-5 is a bulk-API problem" and "S7-5 is a one-line problem".
    !>
    !> **S7-5 IS NOW IMPLEMENTED, so the first row is history rather than a tie-down.** Before the
    !> change, `reserve` + n x `%append_string` reproduced `%set_all` to within the sizing pass
    !> (55.5 + 7.5 against 62.8), which is what made the rest of this decomposition trustworthy.
    !> `%set_all` now routes through `%build_from`, lands at ~10 ms, and should track CANDIDATE 2 --
    !> the row it was built from. Keep the superseded row: it is the before-figure any future change
    !> here is measured against, and it costs one loop to produce.
    subroutine gate_s7_5_where(v, n, set_all_ms)
        character(len=*), intent(in) :: v(:)   !! the same fixture `%set_all` was timed on.
        integer(int64), intent(in) :: n        !! element count.
        real(real64), intent(in) :: set_all_ms !! the shipped figure, for the tie-down.
        type(parquet_string_column) :: sc
        character(len=:), allocatable :: payload
        character(len=1), allocatable :: dat(:)
        integer(int64), allocatable :: off(:)
        integer(int64) :: k, acc, nchars
        integer :: r, ln
        integer, allocatable :: lens(:)
        real(real64) :: t0, t1, b_size, b_app, b_sub, b_tr, b_cand, b_cand2
        b_size = huge(1.0_real64); b_app = b_size; b_sub = b_size; b_tr = b_size; b_cand = b_size; b_cand2 = b_size
        allocate(off(0:n), lens(n))
        nchars = 0_int64
        do k = 1_int64, n
            nchars = nchars + int(len_trim(v(k)), int64)
        end do
        allocate(character(len=nchars) :: payload)
        allocate(dat(nchars))
        do r = 1, REP
            ! --- pass 1: the sizing sweep, which every design keeps ---
            call tick(t0)
            acc = 0_int64
            do k = 1_int64, n
                acc = acc + int(len_trim(v(k)), int64)
            end do
            call tick(t1); b_size = min(b_size, (t1 - t0)*1.0e3_real64)
            if (acc /= nchars) error stop "s7-5 gate: sizing pass disagrees"
            ! --- pass 2 as shipped: reserve once, then one %append_string per element ---
            call sc%clear()
            call tick(t0)
            call sc%reserve(n, nchars)
            do k = 1_int64, n
                call sc%append_string(v(k), trim=.true.)
            end do
            call tick(t1); b_app = min(b_app, (t1 - t0)*1.0e3_real64)
            ! --- floor: prefix-sum offsets + one memcpy per element, no call, no temporary ---
            call tick(t0)
            off(0) = 0_int64
            do k = 1_int64, n
                ln = len_trim(v(k))
                off(k) = off(k-1) + int(ln, int64)
                if (ln > 0) payload(off(k-1)+1 : off(k)) = v(k)(1:ln)
            end do
            call tick(t1); b_sub = min(b_sub, (t1 - t0)*1.0e3_real64)
            ! --- the same, into the character(len=1) array the store actually declares ---
            call tick(t0)
            off(0) = 0_int64
            do k = 1_int64, n
                ln = len_trim(v(k))
                off(k) = off(k-1) + int(ln, int64)
                if (ln > 0) dat(off(k-1)+1 : off(k)) = transfer(v(k)(1:ln), dat, ln)
            end do
            call tick(t1); b_tr = min(b_tr, (t1 - t0)*1.0e3_real64)
            ! --- the CANDIDATE design, end to end: one len_trim sweep whose lengths are KEPT,
            !     a prefix sum, memcpy into a character(len=:) scalar, then ONE bulk transfer of
            !     the whole payload into the char(1) array the store declares. Two passes over the
            !     payload instead of one, both memcpy, and no per-element transfer or call.
            call tick(t0)
            acc = 0_int64
            do k = 1_int64, n
                lens(k) = len_trim(v(k))
                acc = acc + int(lens(k), int64)
            end do
            off(0) = 0_int64
            do k = 1_int64, n
                off(k) = off(k-1) + int(lens(k), int64)
                if (lens(k) > 0) payload(off(k-1)+1 : off(k)) = v(k)(1:lens(k))
            end do
            dat(1:acc) = transfer(payload, dat, int(acc))
            call tick(t1); b_cand = min(b_cand, (t1 - t0)*1.0e3_real64)
            ! --- CANDIDATE 2: no intermediate buffer at all. `values` is a contiguous array of
            !     character(len=W), so its bytes are one W*n block; sequence association lets a
            !     `character(len=1) :: flat(*)` dummy see exactly that block, after which element
            !     k's bytes are an ordinary array section and the copy into a char(1) payload is a
            !     section-to-section assignment -- no transfer, no temporary, ONE pass over the
            !     payload instead of the two candidate 1 needs.
            call tick(t0)
            call pack_via_bytes(v, v, dat, off, lens, n, len(v), acc)
            call tick(t1); b_cand2 = min(b_cand2, (t1 - t0)*1.0e3_real64)
        end do
        print "(A)", ""
        print "(A)", "  -- where does it go? ms, best of 7, same 1M x char(24) fixture --"
        print "(A,F9.2,A,F9.2,A)", "  SUPERSEDED: n x %append_string: ", b_app, &
            "   (%set_all now: ", set_all_ms, ")"
        print "(A,F9.2)", "  pass 1: len_trim sizing only : ", b_size
        print "(A,F9.2)", "  FLOOR: prefix-sum + substring: ", b_sub
        print "(A,F9.2)", "  same, transfer into char(1)  : ", b_tr
        print "(A,F9.2)", "  CANDIDATE 1, scalar + 1 xfer : ", b_cand
        print "(A,F9.2)", "  CANDIDATE 2, byte-view, 1 pass: ", b_cand2
        print "(A)", ""
        print "(A,F9.2,A)", "  bytes actually moved         : ", real(nchars, real64)/1.0e6_real64, " MB"
        print "(A,F9.2,A)", "  per-element cost of len_trim : ", b_size*1.0e6_real64/real(n, real64), " ns"
        print "(A,F9.2,A)", "  the payload memcpy alone     : ", b_sub - b_size, " ms (floor minus sizing)"
        print "(A,F9.2,A)", "  what transfer-per-element adds: ", b_tr - b_sub, " ms"
        print "(A,F6.2,A)", "  CEILING on S7-5's gain       : ", &
            100.0_real64*(set_all_ms - min(b_cand, b_cand2))/max(set_all_ms, 1.0e-9_real64), " %"
        print "(A,F6.2,A)", "  i.e. %set_all could reach     : ", min(b_cand, b_cand2), " ms"
        print "(A)", "    Measured end to end against the candidate, not derived from a formula:"
        print "(A)", "    an earlier version of this gate subtracted the sizing pass AND the floor,"
        print "(A)", "    which double-counts len_trim because the floor loop calls it too."
    end subroutine gate_s7_5_where

    !> Packs `n` trimmed elements of a character(len=w) array into `dst`, computing `off` and
    !> `lens` as it goes. `src` is declared `character(len=1) :: src(*)`, so Fortran's sequence
    !> association gives it the caller's whole W*n byte block -- which is what turns each element's
    !> payload copy into a section-to-section assignment rather than a `transfer`.
    subroutine pack_via_bytes(vals, src, dst, off, lens, n, w, total)
        character(len=*), intent(in) :: vals(*)   !! the same array, seen as elements, for len_trim.
        character(len=1), intent(in) :: src(*)    !! the caller's char(w) array, seen as bytes.
        character(len=1), intent(inout) :: dst(:) !! packed destination, at least `total` bytes.
        integer(int64), intent(out) :: off(0:)    !! receives offsets(0:n), off(0) = 0.
        integer, intent(out) :: lens(:)           !! receives each element's trimmed length.
        integer(int64), intent(in) :: n           !! element count.
        integer, intent(in) :: w                  !! declared length of one element.
        integer(int64), intent(out) :: total      !! packed byte count.
        integer(int64) :: k, base
        integer :: j
        off(0) = 0_int64
        do k = 1_int64, n
            base = (k - 1_int64)*int(w, int64)
            ! len_trim on the ELEMENT view: an intrinsic the compiler implements far better than a
            ! hand-written trailing-blank scan over the byte view, which measured 3x worse.
            j = len_trim(vals(k))
            lens(k) = j
            off(k) = off(k-1) + int(j, int64)
            if (j > 0) dst(off(k-1)+1 : off(k)) = src(base+1 : base+int(j, int64))
        end do
        total = off(n)
    end subroutine pack_via_bytes

    ! =============================================================================================
    ! S7-7 (A.4 second half) -- THE PRECONDITION, and it is allowed to retire the item.
    !
    ! A.4's proposal is to replace StringLikeAccessor's two std::function members with a
    ! dispatch-once/templated visitor across its call sites. Stage 4 already measured the FILTER
    ! path and found it does not justify the change (evaluate is 11.26 ns/row against a 40.64 ns/row
    ! decode nothing here can touch). The remaining candidate is the six padded-string READ sites,
    ! whose share has never been measured -- that path lost its staging buffer in stage 3, so the
    ! surviving indirect call per row is a larger fraction of what is left than it used to be.
    !
    ! Two phase counters in parquet_read_string_column answer it directly:
    !
    !   decode -- Arrow materialising the column. Not this library's code; no change to the
    !             accessor can touch it.
    !   copy   -- the per-row loop: one std::function get_view plus copy_string_with_padding.
    !
    ! The copy share is the CEILING, and a generous one: it credits the change with driving the
    ! indirect call to ZERO, when in reality copy_string_with_padding's memcpy and blank fill stay.
    ! If that ceiling is small, S7-7 drops here without a line of C++ being written, which is what
    ! this gate exists to be able to conclude.
    ! =============================================================================================
    subroutine gate_s7_7()
        integer(int64), parameter :: N = 2000000_int64
        integer, parameter :: W = 24
        character(len=*), parameter :: file = "test_run/bench_s7_7.parquet"
        interface
            subroutine reset_srp() bind(C, name="parquet_debug_reset_string_read_phase_nanos")
            end subroutine reset_srp
            integer(c_int64_t) function srp_decode() bind(C, name="parquet_debug_get_string_read_decode_nanos")
                import :: c_int64_t
            end function srp_decode
            integer(c_int64_t) function srp_copy() bind(C, name="parquet_debug_get_string_read_copy_nanos")
                import :: c_int64_t
            end function srp_copy
        end interface
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_schema) :: schema
        character(len=W), allocatable :: v(:), got(:)
        integer(int64) :: i, best_dec, best_cop
        integer :: r
        real(real64) :: t0, t1, best_total

        allocate(v(N), got(N))
        do i = 1_int64, N
            write(v(i), "(A,I0)") "row_", i
        end do
        call schema%init("s7_7")
        call schema%add_field("s", "string", array_size=W)
        call parquet_open_writer(writer, file, schema=schema)
        call parquet_write_column(writer, "s", v)
        call parquet_close_writer(writer)

        got = ""                                   ! warm the destination's pages before timing
        best_total = huge(1.0_real64)
        best_dec = huge(1_int64); best_cop = huge(1_int64)
        do r = 1, REP
            call reset_srp()
            call parquet_open_reader(reader, file)
            call tick(t0)
            call parquet_read_column(reader, "s", got)
            call tick(t1)
            call parquet_close_reader(reader)
            best_total = min(best_total, (t1 - t0)*1.0e3_real64)
            best_dec = min(best_dec, srp_decode())
            best_cop = min(best_cop, srp_copy())
        end do
        print "(A)", ""
        print "(A)", "S7-7 (A.4 ii) padded string read, 2M rows x character(len=24)"
        print "(A)", "  Where does parquet_read_string_column's time go? ms, best of 7."
        print "(A,F9.2)", "  whole parquet_read_column     : ", best_total
        print "(A,F9.2)", "  phase: Arrow decode           : ", real(best_dec, real64)/1.0e6_real64
        print "(A,F9.2)", "  phase: per-row copy loop      : ", real(best_cop, real64)/1.0e6_real64
        print "(A,F9.2,A)", "  per-row copy cost             : ", &
            real(best_cop, real64)/real(N, real64), " ns"
        print "(A,F6.2,A)", "  CEILING on S7-7's gain        : ", &
            100.0_real64*real(best_cop, real64)/1.0e6_real64/max(best_total, 1.0e-9_real64), " %"
        print "(A)", "    = the copy loop's whole share. Generous twice over: it credits the change"
        print "(A)", "    with removing the indirect call ENTIRELY, and charges it nothing for the"
        print "(A)", "    memcpy and blank-fill that stay. Compare against the ~5% keep-or-drop line."
        print "(A)", ""
        print "(A)", "  ALREADY DECIDED, and the ceiling above is NOT the answer. A temporary A/B ran"
        print "(A)", "  the same loop with a direct arrow::StringArray::GetView in place of the"
        print "(A)", "  std::function: 11.5 ms against 13.3 (stable to +-0.02 over four runs). So the"
        print "(A)", "  indirect call is 1.8 ms, i.e. 4.2% of the read -- BELOW the keep-or-drop line."
        print "(A)", "  S7-7 was dropped on that. The A/B was reverted; these two counters stayed,"
        print "(A)", "  so re-deciding costs one build rather than one investigation."
    end subroutine gate_s7_7

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
