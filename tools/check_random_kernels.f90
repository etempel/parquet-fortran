!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Driver for `tools/check_random_kernels.sh`: asserts `parquet_random`'s contract in whichever
!> route (e) kernel it was compiled with.
!>
!> **This file is deliberately NOT under `app/` or `test/`.** It `use`s the modules from `test/`,
!> which an `app/` target may not do, and it must be compilable on its own against a single
!> `src/` file rather than against the built library -- so it lives in `tools/`, which fpm does
!> not scan. Nothing builds it except its own shell wrapper.
!>
!> Why it exists at all: the wrapping kernel (the `#else` arm of the fork, which ships wherever
!> there is no 128-bit integer kind) is built by no routine check in this project, because every
!> compiler in the fleet except ifx has such a kind. It was miscompiled by two different gfortran
!> releases under LTO, in two DIFFERENT and non-overlapping sets of call shapes -- so this driver
!> sweeps the shapes rather than trusting any one of them, and the wrapper builds it both ways.
!>
!> Output is one machine-readable line per run, `KERNEL=... RESULT=... FAILED=...`, so the wrapper
!> can assert that the two builds really did differ rather than silently checking one kernel twice.
program check_random_kernels

    use iso_fortran_env, only: int32, int64, real32, real64, output_unit
    use parquet_random
    use test_random_vectors
    use test_random_reference

    implicit none

    integer :: failed
    character(len=8) :: kernel

    !> Loop bound for the runtime-bound sweep in `check_literal_seed_shapes`. `volatile` so no
    !! amount of interprocedural constant propagation can turn it back into a literal -- which is
    !! the entire point of that arm, and is not hypothetical: an ordinary `intent(in)` dummy
    !! carrying a literal actual argument IS restored to a literal under `-flto`, measured at 40
    !! mismatches where the same loop with a genuinely opaque bound reports none.
    integer(int64), volatile :: rt_n

    failed = 0
    rt_n = 24_int64
    ! Three arms, so this cannot be a two-way test: PF_SAFE64 and the wrapping arm both answer
    ! .false. to uses_int128, and labelling the first of them "wrapping" would make the wrapper's
    ! vacuity guard compare two arms it had mis-identified.
    if (parquet_debug_random_uses_int128()) then
        kernel = "int128"
    else if (parquet_debug_random_uses_safe64()) then
        kernel = "safe64"
    else
        kernel = "wrapping"
    end if

    call check_fork()
    call check_golden_scalar()
    call check_golden_int()
    call check_golden_key()
    call check_golden_fill()
    call check_golden_portable_dist()
    call check_literal_seed_shapes()
    call check_variable_shapes()
    call check_integer_widths()
    call check_sphere_exact_rules()

    write(output_unit, '(a,a,a,a,a,i0)') "KERNEL=", trim(kernel), &
        "  RESULT=", trim(merge("PASS", "FAIL", failed == 0)), "  FAILED=", failed
    if (failed > 0) stop 1

contains

    !> Records one failed assertion, naming it once so a failure is diagnosable from the log.
    subroutine bad(what)
        character(len=*), intent(in) :: what        !! short description of the assertion
        failed = failed + 1
        if (failed <= 10) write(output_unit, '(a)') "  [FAIL] " // what
    end subroutine bad

    !> The compiled kernel must match the compiler's actual capability -- unless the wrapper has
    !> deliberately defeated the allowlist, which is the whole point of the second build.
    subroutine check_fork()
        logical :: capable
        capable = selected_int_kind(38) > 0
        if (parquet_debug_random_uses_int128() .and. parquet_debug_random_uses_safe64()) then
            call bad("fork reports BOTH int128 and safe64 -- the #elif arms now overlap")
            return
        end if
        if (capable .and. parquet_debug_random_uses_int128()) return       ! ordinary build
        if (.not. capable .and. .not. parquet_debug_random_uses_int128()) return
        if (capable .and. .not. parquet_debug_random_uses_int128()) return ! forced wrapping/safe64
        call bad("fork reports int128 on a compiler with no 128-bit kind")
    end subroutine check_fork

    !> Every row of the scalar golden grid, in all three scalar forms.
    subroutine check_golden_scalar()
        integer :: k
        do k = 1, n_scalar
            if (pf_random_bits_at(scalar_seed(k), scalar_stream(k), scalar_draw(k)) /= scalar_bits(k)) &
                call bad("golden pf_random_bits_at")
            if (transfer(pf_random_at(scalar_seed(k), scalar_stream(k), scalar_draw(k)), 0_int64) &
                /= scalar_at_bits(k)) call bad("golden pf_random_at")
            if (transfer(pf_random32_at(scalar_seed(k), scalar_stream(k), scalar_draw(k)), 0_int32) &
                /= scalar_at32_bits(k)) call bad("golden pf_random32_at")
        end do
    end subroutine check_golden_scalar

    !> Every row of the integer golden table.
    subroutine check_golden_int()
        integer :: k
        do k = 1, n_int
            if (pf_random_int_at(int_seed(k), int_stream(k), int_lo(k), int_hi(k), int_draw(k)) /= int_value(k)) &
                call bad("golden pf_random_int_at")
        end do
    end subroutine check_golden_int

    !> Every row of the derived-key golden table.
    subroutine check_golden_key()
        integer :: k
        do k = 1, n_key
            if (pf_random_key(key_seed(k), key_label(k)) /= key_value(k)) call bad("golden pf_random_key")
        end do
    end subroutine check_golden_key

    !> Every fill case of the golden table, in both real kinds.
    subroutine check_golden_fill()
        integer :: c, k
        real(real64) :: v64(16)
        real(real32) :: v32(16)
        do c = 1, n_fill
            call pf_random_fill_draws(fill_seed(c), fill_stream(c), v64(1:fill_count(c)), fill_start(c))
            call pf_random_fill_draws(fill_seed(c), fill_stream(c), v32(1:fill_count(c)), fill_start(c))
            do k = 1, fill_count(c)
                if (transfer(v64(k), 0_int64) /= fill_at_bits(fill_first(c) + k - 1)) call bad("golden fill real64")
                if (transfer(v32(k), 0_int32) /= fill_at32_bits(fill_first(c) + k - 1)) call bad("golden fill real32")
            end do
        end do
    end subroutine check_golden_fill

    !> Every `_portable` distribution draw, which is the ONLY place this sweep can assert a
    !> floating-point transform exactly.
    !!
    !! **The default realisations are deliberately absent.** `pf_random_exp_at` routes through the
    !! intrinsic `log`, so its last bit belongs to whichever libm the configuration links and to
    !! whatever `-Ofast` is entitled to substitute for it -- asserting it here would make this
    !! script fail on a flag set it is meant to be checking. The `_portable` forms route through
    !! `parquet_expkey`'s frozen transform instead and must be bit-identical at every optimisation
    !! setting, which is exactly what this sweep is for.
    !!
    !! `tools/check_exp_key.sh` sweeps the transform in isolation; this sweeps its COMPOSITION with
    !! the generator, which is a different claim -- the `1 - u` step and the argument's provenance
    !! are here and not there.
    subroutine check_golden_portable_dist()
        integer :: k
        do k = 1, n_exp
            if (transfer(pf_random_exp_portable_at(exp_seed(k), exp_stream(k), exp_draw(k)), 0_int64) &
                /= exp_portable_bits(k)) call bad("golden pf_random_exp_portable_at")
        end do
        do k = 1, n_norm
            if (transfer(pf_random_normal_portable_at(norm_seed(k), norm_stream(k), norm_draw(k)), 0_int64) &
                /= norm_polar_bits(k)) call bad("golden pf_random_normal_portable_at")
            ! A path-1 Ziggurat draw reaches no libm either -- it is `u * zig_w(i)` and a sign --
            ! so it is exact on every configuration too, and asserting it here is what sweeps the
            ! layer table's own arithmetic across optimisation settings. Paths 2 and 3 use libm
            ! exp/log and are deliberately left to the test suite, which knows which libm it has.
            if (norm_zig_path(k) == 1_int32) then
                if (transfer(pf_random_normal_at(norm_seed(k), norm_stream(k), norm_draw(k)), 0_int64) &
                    /= norm_zig_bits(k)) call bad("golden pf_random_normal_at (path 1)")
            end if
        end do
    end subroutine check_golden_portable_dist

    !> Literal-constant seed at the call site, over three stream-range shapes that are compiled
    !> differently from one another.
    !!
    !! **The stream range is the load-bearing part of this sweep, and every claim below is a
    !! measurement rather than a deduction.** Building the wrapping kernel at `-O3 -flto` on
    !! gfortran 14.2.1, with the comparison body written out inline exactly as it is here (counts
    !! from a standalone 40-iteration sweep, so read them as fires/does-not rather than as this
    !! subroutine's own totals, which are given further down):
    !!
    !! | loop bounds | wrong |
    !! |---|---|
    !! | `1..40` -- both literal, non-negative | 40 |
    !! | `-40..-1` -- both literal, negative | 40 |
    !! | `-40..40` -- both literal, spans zero | none |
    !! | `1..rt_n` -- literal lower, opaque upper | none |
    !! | `rt_lo..rt_hi` -- both opaque | none |
    !!
    !! **So the trigger is BOTH bounds being compile-time known AND the range not spanning zero --
    !! which is narrower than the rule feature_risks.md Risk-101 states**, and narrower in the
    !! direction that matters: `do i = 1, n` over a runtime `n`, which is the module's own
    !! documented idiom and the shape a user actually writes, measured clean. That shape had never
    !! been tested on any machine before this sweep existed -- the two shapes previously measured,
    !! both-literal and both-dummy, bracket it without covering it. It is swept here anyway,
    !! because "clean on the two releases tried" is not a property of the next release.
    !!
    !! **Two traps that make a re-measurement of this silently vacuous.** The comparison body must
    !! stay written out inline in each loop: factoring the three shapes' shared body into one
    !! helper so that they "differ only in their bounds" reports ZERO for every shape on a build
    !! that fails at 240 here, because routing the body through a helper is itself a change of
    !! compiled form. And an opaque bound must be genuinely opaque -- an `intent(in)` dummy handed
    !! a literal actual argument is restored to a literal by interprocedural constant propagation
    !! under `-flto` and fires at full strength, so `rt_n` is `volatile`.
    !!
    !! One consequence a future reader must not undo: a sweep centred on zero -- the natural way to
    !! write "cover negatives too", and what `test_agreement_scalar` does with `-40..40` -- lands
    !! squarely in the clean range and detects nothing. Sweep both signs, in separate loops, and
    !! never merge them into one symmetric range.
    !!
    !! **The negative arm carries the same inner `draw` loop as the positive one, and that is not
    !! symmetry for its own sake: without it the arm was measurably blind.** Written with only its
    !! three no-draw calls it contributed ZERO on gfortran 14.2.1; with the draw loop the same range
    !! contributes **336** of this subroutine's 480, against the positive arm's 144. Do not trim it
    !! back to save three lines.
    !!
    !! **And the negative arm is not merely bigger, it is BROADER: it breaks all three scalar draws
    !! where the positive arm breaks only one.** Uncapped per-label counts at `-O3 -flto`, forced
    !! wrapping:
    !!
    !! | arm | which draws come back wrong |
    !! |---|---|
    !! | `1..24` | `pf_random32_at` only -- 96 with `draw`, 24 without, plus 24 `pf_random_at` without |
    !! | `-24..-1` | **`pf_random_bits_at`, `pf_random_at` AND `pf_random32_at`** -- 96 each with `draw` |
    !! | `1..rt_n` | none |
    !!
    !! So a summary of this fault as "`pf_random32_at` returns wrong values" describes the
    !! non-negative range only. On the negative range every scalar draw is wrong, which means the
    !! damage reaches `pf_random_bits_at` -- the rawest form the module has, and the one every other
    !! value is derived from.
    !!
    !! Note the driver caps printed failures at 10 (`bad`), so grepping its output tells you which
    !! arm fires FIRST, never which arms fire. The per-label counts above were taken by lifting that
    !! cap in a scratch copy; do that rather than reasoning from the printed lines, which is how an
    !! earlier version of this comment came to assert something it had not actually measured.
    subroutine check_literal_seed_shapes()
        integer(int64) :: i, d
        do i = 1_int64, 24_int64                       ! both bounds literal, non-negative
            do d = 1_int64, 4_int64
                if (pf_random_bits_at(12345_int64, i, d) /= ref_bits(12345_int64, i, d)) call bad("literal-seed bits")
                if (pf_random_at(12345_int64, i, d) /= ref_at(12345_int64, i, d)) call bad("literal-seed at")
                if (pf_random32_at(12345_int64, i, d) /= ref_at32(12345_int64, i, d)) call bad("literal-seed at32")
            end do
            if (pf_random_bits_at(12345_int64, i) /= ref_bits(12345_int64, i, 1_int64)) call bad("literal-seed bits, no draw")
            if (pf_random_at(12345_int64, i) /= ref_at(12345_int64, i, 1_int64)) call bad("literal-seed at, no draw")
            if (pf_random32_at(12345_int64, i) /= ref_at32(12345_int64, i, 1_int64)) call bad("literal-seed at32, no draw")
        end do
        do i = -24_int64, -1_int64                     ! both bounds literal, strictly negative
            do d = 1_int64, 4_int64
                if (pf_random_bits_at(12345_int64, i, d) /= ref_bits(12345_int64, i, d)) call bad("literal-seed bits, neg")
                if (pf_random_at(12345_int64, i, d) /= ref_at(12345_int64, i, d)) call bad("literal-seed at, neg")
                if (pf_random32_at(12345_int64, i, d) /= ref_at32(12345_int64, i, d)) call bad("literal-seed at32, neg")
            end do
            if (pf_random_bits_at(12345_int64, i) /= ref_bits(12345_int64, i, 1_int64)) call bad("literal-seed bits, neg, no draw")
            if (pf_random_at(12345_int64, i) /= ref_at(12345_int64, i, 1_int64)) call bad("literal-seed at, neg, no draw")
            if (pf_random32_at(12345_int64, i) /= ref_at32(12345_int64, i, 1_int64)) call bad("literal-seed at32, neg, no draw")
        end do
        do i = 1_int64, rt_n                           ! `do i = 1, n`: the documented idiom
            do d = 1_int64, 4_int64
                if (pf_random_bits_at(12345_int64, i, d) /= ref_bits(12345_int64, i, d)) call bad("literal-seed bits, rt")
                if (pf_random_at(12345_int64, i, d) /= ref_at(12345_int64, i, d)) call bad("literal-seed at, rt")
                if (pf_random32_at(12345_int64, i, d) /= ref_at32(12345_int64, i, d)) call bad("literal-seed at32, rt")
            end do
            if (pf_random_bits_at(12345_int64, i) /= ref_bits(12345_int64, i, 1_int64)) call bad("literal-seed bits, rt, no draw")
            if (pf_random_at(12345_int64, i) /= ref_at(12345_int64, i, 1_int64)) call bad("literal-seed at, rt, no draw")
            if (pf_random32_at(12345_int64, i) /= ref_at32(12345_int64, i, 1_int64)) call bad("literal-seed at32, rt, no draw")
        end do
    end subroutine check_literal_seed_shapes

    !> Seed, stream and draw all variables -- again with the stream range kept non-negative.
    !!
    !! Separate from the sweep above because the two are separately specialised by the compiler, and
    !! swept because neither has been shown safe rather than because both have been shown to fail.
    !! What is measured: gfortran 15.2 miscompiles literal-seed shapes, and on 14.2.1 at `-O3 -flto`
    !! every one of the 240 failures is a literal-seed one -- `variable-shape` appears zero times,
    !! and so does `golden`. An earlier version of this comment claimed 14.2.1 "was measured doing
    !! the reverse", i.e. breaking all-variable shapes while literal-seed ones stayed correct; that
    !! is contradicted by a direct measurement on the machine it describes, so it has been removed
    !! rather than corrected -- it is not known which source state it was taken against.
    !!
    !! This arm is therefore currently a sweep with no positive result behind it on either release.
    !! Keep it: it costs one loop, the two shapes really are compiled separately, and a shape that
    !! has never failed is not a shape that cannot.
    subroutine check_variable_shapes()
        integer(int64) :: s, i, d
        do s = -3_int64, 3_int64
            do i = 1_int64, 12_int64
                do d = 1_int64, 4_int64
                    if (pf_random_bits_at(s, i, d) /= ref_bits(s, i, d)) call bad("variable-shape bits")
                    if (pf_random_at(s, i, d) /= ref_at(s, i, d)) call bad("variable-shape at")
                    if (pf_random32_at(s, i, d) /= ref_at32(s, i, d)) call bad("variable-shape at32")
                end do
                if (pf_random32_at(s, i) /= ref_at32(s, i, 1_int64)) call bad("variable-shape at32, no draw")
            end do
        end do
    end subroutine check_variable_shapes

    !> The integer rule across every width regime, which is where the wrapping arithmetic
    !! (`sub64`/`add64`, and the rejection threshold) is reached at all.
    subroutine check_integer_widths()
        integer(int64), parameter :: los(11) = [ &
            0_int64, 1_int64, -100_int64, -6148914691236517205_int64, 0_int64, &
            -huge(1_int64) - 1_int64, -huge(1_int64), 7_int64, -3_int64, 0_int64, 0_int64]
        integer(int64), parameter :: his(11) = [ &
            999999_int64, 6_int64, 100_int64, 6148914691236517205_int64, huge(1_int64), &
            huge(1_int64), huge(1_int64), 7_int64, 4294967296_int64, &
            7378697629483820645_int64, 5534023222112865484_int64]
        integer :: g
        integer(int64) :: i, d, value, refv, refr
        do g = 1, 11
            do i = 1_int64, 12_int64
                do d = 1_int64, 3_int64
                    value = pf_random_int_at(12345_int64, i, los(g), his(g), d)
                    call ref_int_at(12345_int64, i, los(g), his(g), d, refv, refr)
                    if (value /= refv) call bad("integer width regime")
                end do
            end do
        end do
    end subroutine check_integer_widths

    !> The two rules `pf_random_disc_at` states EXACTLY, which no optimisation setting may bend.
    !!
    !! **A ring whose inner radius is the half turn is the antipode alone** -- this module's own
    !! refusal message for a larger `r_inner` says so in those words -- and a disc of radius `pi`
    !! must be able to reach it. Both rest on `h = 1 - cos(r)` being exactly 2 at the half turn,
    !! and `2*sin(r/2)**2` does not deliver that by itself: **gfortran from `-O2` packs this
    !! module's `sin` calls into glibc's vector sine** (`_ZGVbN2v_sin`, an `-ftree-vectorize`
    !! transformation; `nm -u` shows it replacing the scalar `sin`), which is a few-ulp routine
    !! and answers one ulp below 1 at `pi/2`.
    !!
    !! One ulp of `h` is 3e-8 of POSITION at the pole, because the point is placed at a transverse
    !! offset of `sqrt(h*(2 - h))` -- so the ring came out as a circle of that radius instead of
    !! the antipode. `fpm test --profile release` caught it and nothing in the pipeline did: the
    !! CI test job passes `FPM_FFLAGS`, which REPLACES the profile flags, so it builds at `-O0`.
    !! This driver is the one thing that compiles this module at every level, which is why the
    !! rule is asserted here as well as in `test/test_random_dist.f90`.
    subroutine check_sphere_exact_rules()
        real(real64), parameter :: PI = 3.14159265358979323846264338327950288_real64
        real(real64), parameter :: ANTIPODE(3) = [0.0_real64, 0.0_real64, -1.0_real64]
        real(real64), parameter :: ZAXIS(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        real(real64) :: v(3)
        integer(int64) :: k
        logical :: moved
        do k = 1_int64, 200_int64
            ! The outer radius is deliberately ABOVE the half turn, so the clamp is exercised too.
            v = pf_random_disc_at(20260821_int64, 37_int64, ZAXIS, 4.0_real64, k, PI)
            if (any(v /= ANTIPODE)) then
                call bad("a ring whose inner radius is the half turn is not the antipode")
                exit
            end if
        end do
        ! The control, without which the assertion above would hold for a sampler that answered the
        ! antipode to everything: the same centre at a radius that is not the half turn moves.
        moved = .false.
        do k = 1_int64, 200_int64
            v = pf_random_disc_at(20260821_int64, 33_int64, ZAXIS, PI, k)
            if (any(v /= ANTIPODE)) moved = .true.
        end do
        if (.not. moved) call bad("control: a disc of radius pi answered the antipode every time")
    end subroutine check_sphere_exact_rules

end program check_random_kernels
