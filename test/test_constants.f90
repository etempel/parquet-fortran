!> Tests for `parquet_constants`: every constant is the double it is documented to be.
!!
!! **Two tests, two different kinds of evidence.** `test_bit_patterns` holds each constant to the
!! hexadecimal bit pattern of its double, so that a changed digit, a changed divisor or a
!! re-associated expression fails by name, however small the change. `test_definitions` evaluates
!! each definition independently, in the widest real kind the processor has, and holds each
!! constant to it: exactly, where the constant is the double nearest its definition, and within the
!! documented number of ulp where it is astropy's double instead. A pattern alone would pin a
!! mistranscribed value as faithfully as a right one; a definition alone cannot tell which of two
!! neighbouring doubles astropy chose.
!!
!! **The exact comparisons are safe from the wide kind's own rounding.** In `real128` a definition
!! comes out about 1e-33 relative from its true value, far inside a double's rounding interval, so
!! it rounds to the nearest double unless the true value sits within about 1e-17 ulp of a rounding
!! tie. Measured at 80 digits, the closest any value asserted exactly here comes to a tie is 0.02 ulp
!! (`G`, whose definition is its literal); pi sits 0.22 ulp from one, `(4/11)^(1/3)` 0.40 and
!! `10^3.56` 0.33. So an exact assertion here is not the `real64` re-derivation of a
!! transcendental that `testing.md` warns about. A processor with no `real128` (flang on arm64
!! macOS, where the kind is -1) takes the other arm: the definitions are evaluated in `real64`,
!! whose `atan`, powers and divisions each carry up to an ulp, and every comparison there allows
!! `NARROW_ULPS`. Each message names the arm that ran.
!!
!! This suite is pure computation with no fixture files and no state, so it runs concurrently. Its
!! only library import is `use parquet_constants`: it is registered in `run_tester_pf.f90`, the
!! runner that executes no `bind(C)` call.
module test_constants

    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet_constants
    use iso_fortran_env, only : int64, real64, real128

    implicit none
    private

    public :: collect_tests_constants

    !> The widest real kind the processor has: `real128`, or `real64` where there is none. Naming
    !! `real128` directly would not compile there (`fortran-gotchas.md`, flang).
    integer, parameter :: WP = merge(real128, real64, real128 > 0)
    !> Whether the definitions are evaluated wider than the constants they are compared with.
    logical, parameter :: WIDE = WP /= real64
    !> The ulp every comparison of `test_definitions` allows on the `real64` arm: each definition is
    !! a few operations there, each rounded once.
    integer(int64), parameter :: NARROW_ULPS = 4_int64
    !> The arm that ran, for the messages.
    character(len=*), parameter :: ARM = merge("the real128 arm", "the real64 arm ", WIDE)

contains

    !> Registers the suite's tests.
    subroutine collect_tests_constants(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("every constant is its pinned double, to the bit", test_bit_patterns), &
            new_unittest("every constant follows its definition", test_definitions) &
            ]
    end subroutine collect_tests_constants

    !> Each constant against the bit pattern of its double.
    !>
    !> A constant defined by a literal is pinned to that literal's double. One defined by an
    !> expression is pinned to the expression folded in IEEE double arithmetic in the order it is
    !> written, which is what every compiler does to a constant expression. A pattern is exact, so
    !> a mutation that stays on the same double changes nothing here: `3.1415926535897931` is the
    !> double `PF_PI`'s literal is.
    subroutine test_bit_patterns(error)
        type(error_type), allocatable, intent(out) :: error !! set if any constant is off.
        integer :: nbad
        character(len=400) :: names

        nbad = 0
        names = ""
        ! pi as its literal, and the rest of the pi family folded from it: each is also the double
        ! nearest its exact value, which `test_definitions` asserts.
        call tally(transfer(PF_PI, 0_int64) == int(z'400921FB54442D18', int64), "PF_PI", nbad, names)
        call tally(transfer(PF_TWOPI, 0_int64) == int(z'401921FB54442D18', int64), "PF_TWOPI", &
                   nbad, names)
        call tally(transfer(PF_HALFPI, 0_int64) == int(z'3FF921FB54442D18', int64), "PF_HALFPI", &
                   nbad, names)
        call tally(transfer(PF_RAD_PER_DEG, 0_int64) == int(z'3F91DF46A2529D39', int64), &
                   "PF_RAD_PER_DEG", nbad, names)
        call tally(transfer(PF_DEG_PER_RAD, 0_int64) == int(z'404CA5DC1A63C1F8', int64), &
                   "PF_DEG_PER_RAD", nbad, names)
        call tally(transfer(PF_RAD_PER_ARCMIN, 0_int64) == int(z'3F33104B57CF96A3', int64), &
                   "PF_RAD_PER_ARCMIN", nbad, names)
        call tally(transfer(PF_RAD_PER_ARCSEC, 0_int64) == int(z'3ED455A5B2FF8F9D', int64), &
                   "PF_RAD_PER_ARCSEC", nbad, names)
        ! The literals' doubles.
        call tally(transfer(PF_C_KMS, 0_int64) == int(z'41124C41D4FDF3B6', int64), "PF_C_KMS", &
                   nbad, names)
        call tally(transfer(PF_C_MS, 0_int64) == int(z'41B1DE784A000000', int64), "PF_C_MS", &
                   nbad, names)
        call tally(transfer(PF_MPC_M, 0_int64) == int(z'449A22FF2615D608', int64), "PF_MPC_M", &
                   nbad, names)
        call tally(transfer(PF_MPC_KM, 0_int64) == int(z'43FAC394A1E11CB4', int64), "PF_MPC_KM", &
                   nbad, names)
        call tally(transfer(PF_GYR_S, 0_int64) == int(z'435C075E147DB000', int64), "PF_GYR_S", &
                   nbad, names)
        call tally(transfer(PF_G_SI, 0_int64) == int(z'3DD2589EFFED8ACC', int64), "PF_G_SI", &
                   nbad, names)
        call tally(transfer(PF_GM_SUN_SI, 0_int64) == int(z'441CC70534284D5E', int64), &
                   "PF_GM_SUN_SI", nbad, names)
        ! GM_sun / G folded, astropy's `M_sun`: 1 ulp above the double nearest the quotient, whose
        ! pattern ends in 54.
        call tally(transfer(PF_M_SUN_KG, 0_int64) == int(z'463918E618481F55', int64), &
                   "PF_M_SUN_KG", nbad, names)
        ! GM_sun / (1e6 Mpc) folded: the double nearest the quotient, and astropy's.
        call tally(transfer(PF_G_MPC_MSUN_KMS2, 0_int64) == int(z'3E3278E896A28E8A', int64), &
                   "PF_G_MPC_MSUN_KMS2", nbad, names)
        ! astropy's doubles, 3 and 1 ulp above the doubles nearest the exact values, whose patterns
        ! end in 7A and 9E.
        call tally(transfer(PF_SIGMA_SB_SI, 0_int64) == int(z'3E6E714DA26D017D', int64), &
                   "PF_SIGMA_SB_SI", nbad, names)
        call tally(transfer(PF_K_B_EV_K, 0_int64) == int(z'3F1696FE94E2CF9F', int64), &
                   "PF_K_B_EV_K", nbad, names)
        ! The doubles nearest `(4/11)^(1/3)` and `10^3.56`.
        call tally(transfer(PF_TNU_OVER_TGAMMA, 0_int64) == int(z'3FE6D72B7DCC7672', int64), &
                   "PF_TNU_OVER_TGAMMA", nbad, names)
        call tally(transfer(PF_AB_ZERO_POINT_JY, 0_int64) == int(z'40AC5D8FA3F2C1A4', int64), &
                   "PF_AB_ZERO_POINT_JY", nbad, names)

        call check(error, nbad, 0, "not the pinned double:" // trim(names))
    end subroutine test_bit_patterns

    !> Each constant against its definition, evaluated in the wide kind `WP` and rounded once.
    !>
    !> Every value is formed here from the definitions alone -- pi as `4 atan(1)`, the parsec as
    !> `648000 / pi` astronomical units, the radiation constants from the 2019 SI's exact `h`, `k`,
    !> `c` and `e` -- and never from another constant of the module, so that one wrong constant
    !> cannot carry a second along with it. The exceptions are the relations the module documents in
    !> double arithmetic, asserted as such at the end.
    subroutine test_definitions(error)
        type(error_type), allocatable, intent(out) :: error !! set if any constant is off.
        !> `GM_sun / G` in double arithmetic, which is how astropy derives `M_sun`. A parameter, so
        !! that it is folded by the front end: ifx's default floating-point model would otherwise
        !! multiply by a reciprocal (`fortran-gotchas.md`, ifx).
        real(real64), parameter :: m_sun_astropy = PF_GM_SUN_SI / PF_G_SI
        !> `1000 PF_C_KMS` and `1000 PF_MPC_KM` in double arithmetic, each exactly the metre form.
        real(real64), parameter :: c_ms_from_kms = 1000.0_real64 * PF_C_KMS
        real(real64), parameter :: mpc_m_from_km = 1000.0_real64 * PF_MPC_KM !! see `c_ms_from_kms`.
        real(WP) :: pi, mpc_m, h, k, c, e
        integer :: nbad
        character(len=400) :: names

        nbad = 0
        names = ""

        pi = 4.0_WP * atan(1.0_WP)
        call compare(PF_PI, pi, 0_int64, "PF_PI", nbad, names)
        call compare(PF_TWOPI, 2.0_WP * pi, 0_int64, "PF_TWOPI", nbad, names)
        call compare(PF_HALFPI, pi / 2.0_WP, 0_int64, "PF_HALFPI", nbad, names)
        call compare(PF_RAD_PER_DEG, pi / 180.0_WP, 0_int64, "PF_RAD_PER_DEG", nbad, names)
        call compare(PF_DEG_PER_RAD, 180.0_WP / pi, 0_int64, "PF_DEG_PER_RAD", nbad, names)
        call compare(PF_RAD_PER_ARCMIN, pi / 10800.0_WP, 0_int64, "PF_RAD_PER_ARCMIN", nbad, names)
        call compare(PF_RAD_PER_ARCSEC, pi / 648000.0_WP, 0_int64, "PF_RAD_PER_ARCSEC", nbad, names)

        call compare(PF_C_KMS, 299792.458_WP, 0_int64, "PF_C_KMS", nbad, names)
        call compare(PF_C_MS, 299792458.0_WP, 0_int64, "PF_C_MS", nbad, names)

        ! The parsec is 648000/pi astronomical units (IAU 2015 B2), the unit exact (IAU 2012 B2).
        mpc_m = 648000.0_WP / pi * 149597870700.0_WP * 1.0e6_WP
        call compare(PF_MPC_M, mpc_m, 0_int64, "PF_MPC_M", nbad, names)
        call compare(PF_MPC_KM, mpc_m / 1000.0_WP, 0_int64, "PF_MPC_KM", nbad, names)
        call compare(PF_GYR_S, 365.25_WP * 86400.0_WP * 1.0e9_WP, 0_int64, "PF_GYR_S", nbad, names)

        call compare(PF_G_SI, 6.6743e-11_WP, 0_int64, "PF_G_SI", nbad, names)
        call compare(PF_GM_SUN_SI, 1.3271244e20_WP, 0_int64, "PF_GM_SUN_SI", nbad, names)
        ! astropy's derivation lands 1 ulp above the nearest double; its budget is astropy's.
        call compare(PF_M_SUN_KG, 1.3271244e20_WP / 6.6743e-11_WP, 1_int64, "PF_M_SUN_KG", nbad, &
                     names)
        call compare(PF_G_MPC_MSUN_KMS2, 1.3271244e20_WP / (1.0e6_WP * mpc_m), 0_int64, &
                     "PF_G_MPC_MSUN_KMS2", nbad, names)

        ! The 2019 SI's exact constants. astropy's Stefan-Boltzmann double is 3 ulp above the
        ! nearest, and its k/e 1 ulp above: those are the budgets, and they are astropy's.
        h = 6.62607015e-34_WP
        k = 1.380649e-23_WP
        c = 299792458.0_WP
        e = 1.602176634e-19_WP
        call compare(PF_SIGMA_SB_SI, 2.0_WP * pi**5 * k**4 / (15.0_WP * h**3 * c**2), 3_int64, &
                     "PF_SIGMA_SB_SI", nbad, names)
        call compare(PF_K_B_EV_K, k / e, 1_int64, "PF_K_B_EV_K", nbad, names)

        call compare(PF_TNU_OVER_TGAMMA, (4.0_WP / 11.0_WP)**(1.0_WP / 3.0_WP), 0_int64, &
                     "PF_TNU_OVER_TGAMMA", nbad, names)
        ! `m_AB = -2.5 log10(f_nu) - 48.60`, f_nu in erg/s/cm2/Hz: zero at 10^(-19.44) of that
        ! unit, 1e-23 of which is a jansky.
        call compare(PF_AB_ZERO_POINT_JY, 10.0_WP**3.56_WP, 0_int64, "PF_AB_ZERO_POINT_JY", nbad, &
                     names)

        ! The relations the module documents in double arithmetic.
        call tally(PF_M_SUN_KG == m_sun_astropy, "PF_M_SUN_KG is not GM_sun/G in doubles", nbad, &
                   names)
        call tally(c_ms_from_kms == PF_C_MS, "1000 PF_C_KMS is not PF_C_MS", nbad, names)
        call tally(mpc_m_from_km == PF_MPC_M, "1000 PF_MPC_KM is not PF_MPC_M", nbad, names)

        call check(error, nbad, 0, "off its definition (" // trim(ARM) // "):" // trim(names))
    end subroutine test_definitions

    !> Tallies one constant whose double is not within `budget` ulp of its definition rounded to
    !> `real64`; the `real64` arm allows at least `NARROW_ULPS`.
    subroutine compare(got, definition, budget, name, nbad, names)
        real(real64), intent(in) :: got                !! the constant.
        real(WP), intent(in) :: definition             !! its definition, in the wide kind.
        integer(int64), intent(in) :: budget           !! the ulp it may differ by on the wide arm.
        character(len=*), intent(in) :: name           !! the constant's name, for the message.
        integer, intent(inout) :: nbad                 !! disagreements so far.
        character(len=*), intent(inout) :: names       !! their names so far.
        integer(int64) :: allowed, apart

        allowed = budget
        if (.not. WIDE) allowed = max(budget, NARROW_ULPS)
        ! Every constant is positive and finite, so their bit patterns order as their values do.
        apart = abs(transfer(got, 0_int64) - transfer(real(definition, real64), 0_int64))
        call tally(apart <= allowed, name, nbad, names)
    end subroutine compare

    !> Counts one failed assertion and appends its name.
    subroutine tally(ok, name, nbad, names)
        logical, intent(in) :: ok                      !! the assertion.
        character(len=*), intent(in) :: name           !! what failed, for the message.
        integer, intent(inout) :: nbad                 !! failures so far.
        character(len=*), intent(inout) :: names       !! their names so far.

        if (ok) return
        nbad = nbad + 1
        names = trim(names) // " " // name
    end subroutine tally

end module test_constants
