!> Tests for `parquet_transform`: the discrete cosine transform `pf_dct`, its inverse `pf_idct`, and
!> the length helpers `pf_is_pow2` and `pf_next_pow2`.
!!
!! **The convention is pinned against scipy, the one external authority.** Every other test here is
!! a self-consistency check that a uniformly wrong convention -- a dropped factor of two, a
!! half-sample phase error, the type-I or type-III transform in place of the type-II -- passes, and
!! the Improved Sheather-Jones bandwidth rule would then be out by a constant factor.
!! `test_dct_matches_scipy_golden` holds values `scipy.fft` produced, and must not be weakened.
!!
!! **The direct definition is the oracle for the rest**, summed here in `O(n**2)` with two
!! precautions that give it teeth at `n = 4096`, where it must expose a twiddle factor computed by
!! recurrence rather than directly. Every angle is reduced exactly, in integer arithmetic, before its
!! cosine is read from a table: the naive `pi*k*(2j+1)/(2n)` reaches about 1.3e4 radians there and
!! carries more error than the recurrence it is meant to catch. And the terms are summed pairwise: a
!! running sum of positive terms carries as much error again.
!!
!! **Tolerances are stated in units of `epsilon` times the scale of the rounding**: `sum(abs(x))`
!! for a forward coefficient, which is a sum of terms of that total magnitude, `sum(abs(y))/n` for
!! an inverse one, and `log2(n)` times the peak `n` for a grid cosine, whose whole weight is in one
!! coefficient. Measured under gfortran, the direct twiddles stay below a third of each; a twiddle
!! recurrence, in the FFT or in the DCT's own twiddle, exceeds it at `n = 4096` several times over.
!!
!! This suite is pure computation with no fixture files and no process-global state, so it runs
!! concurrently. Its only library import is `use parquet_transform`: it is registered in
!! `run_tester_pf.f90`, the runner that executes no `bind(C)` call, and
!! `check_test_runner_partition` requires that the files feeding that runner never reach
!! `parquet_bindings`.
module test_transform

    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet_transform
    use iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
                                             ieee_get_flag, ieee_set_flag, ieee_usual, &
                                             ieee_support_flag, ieee_underflow
#ifndef __flang__
    ! The halting-mode pair lowers to `feenableexcept`/`fedisableexcept`, which Apple's libc
    ! lacks, so flang on macOS cannot LINK a reference to either (`fortran-gotchas.md`).
    use, intrinsic :: ieee_arithmetic, only : ieee_support_halting, ieee_get_halting_mode, &
                                             ieee_set_halting_mode, ieee_overflow, ieee_invalid, &
                                             ieee_divide_by_zero
#endif

    implicit none
    private

    public :: collect_tests_transform

    !> Machine epsilon, the unit every tolerance here is stated in.
    real(real64), parameter :: EPS = epsilon(1.0_real64)
    !> pi, for the oracle's cosine table and the grid cosines.
    real(real64), parameter :: PI = 3.141592653589793238462643383279502884_real64

    !> Agreement with scipy's values, in units of `EPS` times the scale of the rounding. scipy's
    !! pocketfft takes another route to the same numbers and rounds differently in the last bits.
    real(real64), parameter :: GOLDEN_TOL = 8.0_real64
    !> Agreement with the direct definition on zero-mean data, in the same units.
    real(real64), parameter :: DIRECT_TOL = 6.0_real64
    !> Agreement with the direct definition on binned counts: all positive, so the oracle's own
    !! pairwise sum carries more error than on zero-mean data.
    real(real64), parameter :: COUNTS_TOL = 16.0_real64
    !> A round trip, and each identity the transform satisfies, in units of `EPS` times its scale.
    real(real64), parameter :: IDENTITY_TOL = 16.0_real64

    ! ---- the scipy values -----------------------------------------------------------------------
    !
    ! The cosine arrays were produced by scipy 1.17.1 (numpy 2.4.5) and the sine arrays by scipy
    ! 1.18.0 (numpy 2.5.2), with this snippet; the cosine values reproduce unchanged under 1.18.0.
    ! Type 2 is scipy's default for all four, and is written out because it is the whole point here.
    ! Both inputs are exact in binary, so no digits are lost on the way in, and `repr` round-trips
    ! every output exactly:
    !
    !   import numpy as np
    !   from scipy.fft import dct, idct, dst, idst
    !   x8 = np.array([1.0, -2.0, 3.5, 0.25, -1.75, 4.0, -0.5, 2.25])
    !   x16 = np.array([0.5, -1.25, 2.0, 3.75, -0.125, 1.5, -2.5, 0.0,
    !                   4.25, -3.0, 0.75, 1.0, -0.375, 2.5, -1.0, 0.625])
    !   for name, x in (("x8", x8), ("x16", x16)):
    !       for f in (dct, idct, dst, idst):
    !           for norm in (None, 'ortho'):
    !               print(name, f.__name__, norm,
    !                     [repr(float(v)) for v in f(x, type=2, norm=norm)])

    !> The eight-value input.
    real(real64), parameter :: X8(8) = [1.0_real64, -2.0_real64, 3.5_real64, 0.25_real64, &
                                        -1.75_real64, 4.0_real64, -0.5_real64, 2.25_real64]
    !> `scipy.fft.dct(x8)`.
    real(real64), parameter :: X8_DCT(8) = [13.5_real64, -4.721580982870801_real64, &
        1.1231869115554307_real64, -2.7348987163831557_real64, -4.596194077712559_real64, &
        4.684218385854738_real64, 22.113083257694086_real64, -3.575625839896981_real64]
    !> `scipy.fft.dct(x8, norm='ortho')`.
    real(real64), parameter :: X8_DCT_ORTHO(8) = [2.386485386504598_real64, -1.1803952457177003_real64, &
        0.2807967278888577_real64, -0.6837246790957889_real64, -1.1490485194281397_real64, &
        1.1710545964636845_real64, 5.5282708144235215_real64, -0.8939064599742452_real64]
    !> `scipy.fft.idct(x8)`.
    real(real64), parameter :: X8_IDCT(8) = [0.4015413474267966_real64, -0.4182646630507792_real64, &
        0.15386702714352857_real64, -0.39870439358345766_real64, -0.5462139850873965_real64, &
        -0.16984075525777864_real64, 1.3029568247032581_real64, 0.17465859770582856_real64]
    !> `scipy.fft.idct(x8, norm='ortho')`.
    real(real64), parameter :: X8_IDCT_ORTHO(8) = [1.7097187803004603_real64, -1.569505261609843_real64, &
        0.719021499167388_real64, -1.4912641837405571_real64, -2.081302549756312_real64, &
        -0.5758096304378411_real64, 5.3153806894063065_real64, 0.802187781416588_real64]

    !> The sixteen-value input.
    real(real64), parameter :: X16(16) = [0.5_real64, -1.25_real64, 2.0_real64, 3.75_real64, &
        -0.125_real64, 1.5_real64, -2.5_real64, 0.0_real64, 4.25_real64, -3.0_real64, 0.75_real64, &
        1.0_real64, -0.375_real64, 2.5_real64, -1.0_real64, 0.625_real64]
    !> `scipy.fft.dct(x16)`.
    real(real64), parameter :: X16_DCT(16) = [17.25_real64, 3.5049978266840625_real64, &
        2.7501621361152053_real64, -2.3575168870832206_real64, -9.019090590437209_real64, &
        -8.79819328896063_real64, -13.656157096908291_real64, 4.51471053043528_real64, &
        15.026019100214134_real64, 1.4688569288192843_real64, -4.812163768408208_real64, &
        13.047177805047422_real64, 27.653544165648768_real64, -17.639104185429947_real64, &
        -5.992834173882914_real64, 0.7693518182259589_real64]
    !> `scipy.fft.dct(x16, norm='ortho')`.
    real(real64), parameter :: X16_DCT_ORTHO(16) = [2.1562500000000004_real64, 0.619601932823103_real64, &
        0.48616457395238566_real64, -0.4167540444045865_real64, -1.5943650291584825_real64, &
        -1.5553155342035088_real64, -2.414090322043162_real64, 0.7980956077912754_real64, &
        2.65625_real64, 0.25965967374024046_real64, -0.8506784082054133_real64, &
        2.306436975323912_real64, 4.8885021508429825_real64, -3.118182545893382_real64, &
        -1.0593934207197726_real64, 0.1360034719464439_real64]
    !> `scipy.fft.idct(x16)`.
    real(real64), parameter :: X16_IDCT(16) = [0.38270706321849624_real64, 0.023781999630662326_real64, &
        0.13860048059049904_real64, -0.16051818966746706_real64, -0.21288672307419748_real64, &
        -0.3727312631617486_real64, -0.5312604533846428_real64, 0.05982141308687383_real64, &
        0.3016446754810432_real64, -0.022143387460272473_real64, -0.3687406133314707_real64, &
        0.2292243896407281_real64, 1.022729285286848_real64, -0.16058440801951054_real64, &
        -0.08452426488492978_real64, 0.004879996049088564_real64]
    !> `scipy.fft.idct(x16, norm='ortho')`.
    real(real64), parameter :: X16_IDCT_ORTHO(16) = [2.201529729229981_real64, 0.17114295802382_real64, &
        0.8206543699617324_real64, -0.8714163509895533_real64, -1.1676575117310999_real64, &
        -2.0718747775835187_real64, -2.968651300964483_real64, 0.375012667182803_real64, &
        1.7429716166833709_real64, -0.0886502631011169_real64, -2.0493002531328752_real64, &
        1.333300614978132_real64, 5.822042155906893_real64, -0.8717909385557049_real64, &
        -0.441529794647853_real64, 0.0642170787394743_real64]

    !> `scipy.fft.dst(x8, type=2)`.
    real(real64), parameter :: X8_DST(8) = [8.019924271335311_real64, -3.1214929814975125_real64, &
        4.128043779760823_real64, -6.010407640085654_real64, -11.949389035221362_real64, &
        2.9165530282272893_real64, 19.451276845476144_real64, -4.5_real64]
    !> `scipy.fft.dst(x8, type=2, norm='ortho')`.
    real(real64), parameter :: X8_DST_ORTHO(8) = [2.004981067833828_real64, -0.7803732453743781_real64, &
        1.0320109449402057_real64, -1.5026019100214134_real64, -2.9873472588053405_real64, &
        0.7291382570568223_real64, 4.862819211369036_real64, -0.7954951288348661_real64]
    !> `scipy.fft.idst(x8, type=2)`.
    real(real64), parameter :: X8_IDST(8) = [0.5532562045581587_real64, -0.033008830266542374_real64, &
        0.06532567172190966_real64, -0.14696081742669848_real64, -0.5540544599312809_real64, &
        0.672893044166802_real64, 1.0486701945300316_real64, -0.5047257855947421_real64]
    !> `scipy.fft.idst(x8, type=2, norm='ortho')`.
    real(real64), parameter :: X8_IDST_ORTHO(8) = [2.446019947067501_real64, -0.3650304499010356_real64, &
        0.49429781572250475_real64, -0.8208383985416599_real64, -1.9832227108902578_real64, &
        2.458577047832342_real64, 4.427675906954993_real64, -2.2518982712138347_real64]

    !> `scipy.fft.dst(x16, type=2)`.
    real(real64), parameter :: X16_DST(16) = [10.69312328424678_real64, 4.871191287439334_real64, &
        11.312424988130191_real64, 3.4797962268002838_real64, 1.5973789495109667_real64, &
        -9.262119411527111_real64, -19.492571243977917_real64, -0.8838834764831843_real64, &
        5.8726785327889095_real64, -4.334785188723208_real64, -16.389019058152257_real64, &
        18.85347496404699_real64, 22.977471571743706_real64, -9.890362825266507_real64, &
        4.227493753713144_real64, -3.25_real64]
    !> `scipy.fft.dst(x16, type=2, norm='ortho')`.
    real(real64), parameter :: X16_DST_ORTHO(16) = [1.8902949965886662_real64, 0.8611130979512954_real64, &
        1.999773105192752_real64, 0.6151468772794606_real64, 0.2823793718309622_real64, &
        -1.637326861012594_real64, -3.4458323273446707_real64, -0.15625_real64, &
        1.0381527035659257_real64, -0.7662890004832974_real64, -2.8971966282537567_real64, &
        3.332854999002106_real64, 4.061881490725273_real64, -1.7483856555353219_real64, &
        0.7473223751685842_real64, -0.4062500000000001_real64]
    !> `scipy.fft.idst(x16, type=2)`.
    real(real64), parameter :: X16_IDST(16) = [0.2997410849770326_real64, 0.2105536271426574_real64, &
        0.2678745688421347_real64, 0.1825922209619209_real64, -0.07949731646441177_real64, &
        -0.21699284258893303_real64, -0.6024898533008635_real64, 0.038336647868713825_real64, &
        0.3038860969650501_real64, -0.020415518047533276_real64, -0.24076972719549194_real64, &
        0.7547607994027579_real64, 0.5052397358589971_real64, -0.6044935194024832_real64, &
        0.10028675875792858_real64, -0.1025700668967245_real64]
    !> `scipy.fft.idst(x16, type=2, norm='ortho')`.
    real(real64), parameter :: X16_IDST_ORTHO(16) = [1.7413561957393857_real64, 1.145306615008374_real64, &
        1.5610919585251708_real64, 0.987133015633092_real64, -0.40394016702534635_real64, &
        -1.273261449148239_real64, -3.3624327214814285_real64, 0.17110026396782485_real64, &
        1.7648039244179992_real64, -0.16125217546237902_real64, -1.3162346889953378_real64, &
        4.223807270012155_real64, 2.9038321122459774_real64, -3.4652962994621435_real64, &
        0.6130721428872075_real64, -0.6259884842350557_real64]

contains

    !> Registers this module's tests.
    subroutine collect_tests_transform(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("pf_dct and pf_idct give scipy.fft's values under both norms", &
                         test_dct_matches_scipy_golden), &
            new_unittest("both transforms match their direct definitions from n = 1 to 4096", &
                         test_dct_matches_the_direct_definition), &
            new_unittest("pf_idct undoes pf_dct under both norms", &
                         test_dct_round_trips), &
            new_unittest("a constant c transforms to 2*n*c in y(1) and zero elsewhere", &
                         test_dct_of_a_constant), &
            new_unittest("a grid cosine of frequency m puts all its weight in y(m+1)", &
                         test_dct_of_a_grid_cosine), &
            new_unittest("the transform of a*x + b*z is a*dct(x) + b*dct(z)", &
                         test_dct_is_linear), &
            new_unittest("the orthonormal transform and its inverse keep the sum of squares", &
                         test_dct_ortho_is_parseval), &
            new_unittest("the norm tokens relate as documented and match case-insensitively", &
                         test_dct_norm_tokens_relate_as_documented), &
            new_unittest("pf_is_pow2 and pf_next_pow2 agree with a doubling count", &
                         test_dct_length_helpers), &
            new_unittest("the ISJ grid, 1024 binned counts, matches the direct definition", &
                         test_dct_isj_grid_size), &
            new_unittest("strided sections give the contiguous call's values to the bit", &
                         test_dct_accepts_strided_sections), &
            new_unittest("a NaN reaches every value of either transform", &
                         test_dct_propagates_a_nan), &
            new_unittest("pf_dst and pf_idst give scipy.fft's values under both norms", &
                         test_dst_matches_scipy_golden), &
            new_unittest("both sine transforms match their direct definitions from n = 1 to 4096", &
                         test_dst_matches_the_direct_definition), &
            new_unittest("pf_idst undoes pf_dst under both norms", &
                         test_dst_round_trips), &
            new_unittest("the orthonormal sine transform and its inverse keep the sum of squares", &
                         test_dst_ortho_is_parseval), &
            new_unittest("a grid sine of frequency m+1 puts all its weight in y(m+1)", &
                         test_dst_of_a_grid_sine), &
            new_unittest("the sine transform of a*x + b*z is a*dst(x) + b*dst(z)", &
                         test_dst_is_linear), &
            new_unittest("the sine norm tokens relate as documented, the odd one out at k = n-1", &
                         test_dst_norm_tokens_relate_as_documented), &
            new_unittest("strided sections give the contiguous sine call's values to the bit", &
                         test_dst_accepts_strided_sections), &
            new_unittest("a NaN reaches every value of either sine transform", &
                         test_dst_propagates_a_nan), &
            new_unittest("a cosine transform in and an inverse sine out give an odd convolution", &
                         test_dst_composes_with_dct), &
            new_unittest("a NaN raises no halting exception, where an infinity and huge do", &
                         test_transform_extreme_inputs_raise_the_documented_flags) &
            ]

    end subroutine collect_tests_transform

    ! ---- the tests --------------------------------------------------------------------------------

    !> **The convention itself**, against values `scipy.fft` produced: `dct` and `idct`, both norms,
    !! at `n = 8` and `n = 16`. A dropped factor of two, a half-sample phase error or the wrong type
    !! of transform moves every value by a sizeable fraction of its scale; no other test catches it.
    subroutine test_dct_matches_scipy_golden(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        call check_golden(error, X8, X8_DCT, X8_DCT_ORTHO, X8_IDCT, X8_IDCT_ORTHO, "n = 8")
        if (allocated(error)) return
        call check_golden(error, X16, X16_DCT, X16_DCT_ORTHO, X16_IDCT, X16_IDCT_ORTHO, "n = 16")

    end subroutine test_dct_matches_scipy_golden

    !> Both transforms against the `O(n**2)` definitions of the module header, on zero-mean data, at
    !! every power of two to 64 and at 4096. The small sizes catch an index error in the reordering,
    !! the butterflies or the twiddle (a length of one runs no butterfly at all); 4096 catches a
    !! twiddle factor computed by recurrence, whose error grows across a pass.
    subroutine test_dct_matches_the_direct_definition(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: SIZES(8) = [1, 2, 4, 8, 16, 32, 64, 4096]
        real(real64), allocatable :: x(:), y(:), want(:)
        character(len=80) :: at
        integer :: i, n

        do i = 1, size(SIZES)
            n = SIZES(i)
            write (at, '(a, i0)') " at n = ", n
            allocate (x(n), y(n), want(n))
            call fill_uniform(x, 1000 + i)

            call pf_dct(x, y)
            call direct_dct(x, want)
            call check(error, maxval(abs(y - want)) <= DIRECT_TOL*EPS*sum(abs(x)), &
                       "pf_dct must match the direct definition"//trim(at))
            if (allocated(error)) return

            ! The same random values, read as coefficients.
            call pf_idct(x, y)
            call direct_idct(x, want)
            call check(error, maxval(abs(y - want)) <= DIRECT_TOL*EPS*sum(abs(x))/n, &
                       "pf_idct must match the direct definition"//trim(at))
            if (allocated(error)) return
            deallocate (x, y, want)
        end do

    end subroutine test_dct_matches_the_direct_definition

    !> `pf_idct(pf_dct(x))` gives `x` back under each norm, at several lengths: the inverse's
    !! scaling, and its reading of `y[n-k]`, are right.
    subroutine test_dct_round_trips(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: SIZES(6) = [1, 2, 4, 8, 64, 1024]
        character(len=5), parameter :: NORMS(2) = ["none ", "ortho"]
        real(real64), allocatable :: x(:), y(:), back(:)
        character(len=80) :: at
        integer :: i, which, n

        do i = 1, size(SIZES)
            n = SIZES(i)
            allocate (x(n), y(n), back(n))
            call fill_uniform(x, 2000 + i)
            do which = 1, size(NORMS)
                write (at, '(a, i0, 3a)') " at n = ", n, ", norm = """, trim(NORMS(which)), """"
                call pf_dct(x, y, norm=trim(NORMS(which)))
                call pf_idct(y, back, norm=trim(NORMS(which)))
                call check(error, maxval(abs(back - x)) <= IDENTITY_TOL*EPS*maxval(abs(x)), &
                           "pf_idct(pf_dct(x)) must give x back"//trim(at))
                if (allocated(error)) return
            end do
            deallocate (x, y, back)
        end do

    end subroutine test_dct_round_trips

    !> A constant `c` has all its weight at `k = 0`: `y(1) = 2*n*c` -- the factor of two that
    !! `k = 0` carries alone under `norm="ortho"` -- and every other coefficient zero to rounding.
    subroutine test_dct_of_a_constant(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: SIZES(5) = [1, 2, 8, 64, 1024]
        real(real64), parameter :: C = 1.5_real64
        real(real64), allocatable :: x(:), y(:)
        character(len=80) :: at
        integer :: i, n

        do i = 1, size(SIZES)
            n = SIZES(i)
            write (at, '(a, i0)') " at n = ", n
            allocate (x(n), y(n))
            x = C
            call pf_dct(x, y)
            call check(error, abs(y(1) - 2*n*C) <= EPS*(2*n*C), "y(1) must be 2*n*c"//trim(at))
            if (allocated(error)) return
            if (n > 1) then
                call check(error, maxval(abs(y(2:))) <= IDENTITY_TOL*EPS*sum(abs(x)), &
                           "every coefficient but the first must vanish"//trim(at))
                if (allocated(error)) return
            end if
            deallocate (x, y)
        end do

    end subroutine test_dct_of_a_constant

    !> `x(j) = cos(pi*m*(2j-1)/(2n))` is the transform's own basis function of frequency `m`, so its
    !! transform is `n` at `y(m+1)` (`2n` at `m = 0`) and zero elsewhere. A half-sample phase error
    !! spreads that weight over neighbouring coefficients, which a round trip cannot see; at
    !! `n = 4096` the high frequencies are where a twiddle recurrence has drifted furthest.
    subroutine test_dct_of_a_grid_cosine(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: BIG_FREQUENCIES(4) = [1, 1365, 2048, 4095] ! low, odd, the midpoint, top
        integer :: m

        do m = 0, 15
            call check_grid_cosine(error, 16, m)
            if (allocated(error)) return
        end do
        do m = 1, size(BIG_FREQUENCIES)
            call check_grid_cosine(error, 4096, BIG_FREQUENCIES(m))
            if (allocated(error)) return
        end do

    end subroutine test_dct_of_a_grid_cosine

    !> `pf_dct(a*x + b*z)` is `a*pf_dct(x) + b*pf_dct(z)`, and the same for `pf_idct`: no stray term
    !! that does not scale with the data.
    subroutine test_dct_is_linear(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: SIZES(2) = [64, 1024]
        real(real64), parameter :: A = 0.75_real64, B = -2.5_real64
        real(real64), allocatable :: x(:), z(:), mix(:), tx(:), tz(:), tmix(:)
        real(real64) :: scale
        character(len=80) :: at
        integer :: i, n

        do i = 1, size(SIZES)
            n = SIZES(i)
            write (at, '(a, i0)') " at n = ", n
            allocate (x(n), z(n), mix(n), tx(n), tz(n), tmix(n))
            call fill_uniform(x, 3000 + i)
            call fill_uniform(z, 3100 + i)
            mix = A*x + B*z
            scale = abs(A)*sum(abs(x)) + abs(B)*sum(abs(z))

            call pf_dct(x, tx)
            call pf_dct(z, tz)
            call pf_dct(mix, tmix)
            call check(error, maxval(abs(tmix - (A*tx + B*tz))) <= IDENTITY_TOL*EPS*scale, &
                       "pf_dct must be linear"//trim(at))
            if (allocated(error)) return

            call pf_idct(x, tx)
            call pf_idct(z, tz)
            call pf_idct(mix, tmix)
            call check(error, maxval(abs(tmix - (A*tx + B*tz))) <= IDENTITY_TOL*EPS*scale/n, &
                       "pf_idct must be linear"//trim(at))
            if (allocated(error)) return
            deallocate (x, z, mix, tx, tz, tmix)
        end do

    end subroutine test_dct_is_linear

    !> Under `norm="ortho"` both transforms are orthonormal, so each keeps the sum of squares.
    !! `s(0)` is the one factor that differs from the rest, so a wrong one breaks the identity by
    !! exactly its own term. The sums of squares are taken pairwise, so that their own rounding is
    !! not what the tolerance measures.
    subroutine test_dct_ortho_is_parseval(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: SIZES(5) = [1, 2, 8, 64, 1024]
        real(real64), allocatable :: x(:), y(:)
        real(real64) :: energy
        character(len=80) :: at
        integer :: i, n

        do i = 1, size(SIZES)
            n = SIZES(i)
            write (at, '(a, i0)') " at n = ", n
            allocate (x(n), y(n))
            call fill_uniform(x, 4000 + i)
            energy = pairwise_sum(x**2)

            call pf_dct(x, y, norm="ortho")
            call check(error, abs(pairwise_sum(y**2) - energy) <= IDENTITY_TOL*EPS*energy, &
                       "pf_dct under norm=""ortho"" must keep the sum of squares"//trim(at))
            if (allocated(error)) return

            call pf_idct(x, y, norm="ortho")
            call check(error, abs(pairwise_sum(y**2) - energy) <= IDENTITY_TOL*EPS*energy, &
                       "pf_idct under norm=""ortho"" must keep the sum of squares"//trim(at))
            if (allocated(error)) return
            deallocate (x, y)
        end do

    end subroutine test_dct_ortho_is_parseval

    !> `"ortho"` is `"none"` scaled by `s(0) = sqrt(1/(4n))` and `s(k) = sqrt(1/(2n))`, and
    !! `pf_idct` under `"ortho"` is `pf_idct` under `"none"` of the coefficients divided by the same
    !! factors: the two norms cannot drift apart. `"none"` is what an absent `norm` means, and a
    !! token matches whatever its case and trailing blanks.
    subroutine test_dct_norm_tokens_relate_as_documented(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 64
        real(real64) :: x(N), y_none(N), y_ortho(N), y_other(N), back(N), back_other(N), s(N)

        call fill_uniform(x, 5000)
        s(1) = sqrt(1.0_real64/(4*N))
        s(2:) = sqrt(1.0_real64/(2*N))

        call pf_dct(x, y_none)
        call pf_dct(x, y_ortho, norm="ortho")
        call check(error, all(abs(y_ortho - s*y_none) <= 2*EPS*abs(s*y_none)), &
                   "pf_dct under norm=""ortho"" must be the unnormalised transform times s(k)")
        if (allocated(error)) return

        call pf_idct(x, back, norm="ortho")
        call pf_idct(x/s, back_other)
        call check(error, maxval(abs(back - back_other)) <= IDENTITY_TOL*EPS*maxval(abs(back)), &
                   "pf_idct under norm=""ortho"" must be pf_idct of the coefficients divided by s(k)")
        if (allocated(error)) return

        ! The tokens: an explicit "none" is the default, and case and trailing blanks do not matter.
        call pf_dct(x, y_other, norm="none")
        call check(error, all(y_other == y_none), "norm=""none"" must be what an absent norm means")
        if (allocated(error)) return
        call pf_dct(x, y_other, norm="NoNe")
        call check(error, all(y_other == y_none), "norm=""NoNe"" must match ""none""")
        if (allocated(error)) return
        call pf_dct(x, y_other, norm="ORTHO  ")
        call check(error, all(y_other == y_ortho), "norm=""ORTHO  "" must match ""ortho""")
        if (allocated(error)) return
        call pf_idct(x, back_other, norm="Ortho")
        call check(error, all(back_other == back), "pf_idct must match its norm token case-insensitively")

    end subroutine test_dct_norm_tokens_relate_as_documented

    !> `pf_is_pow2` and `pf_next_pow2` against a reference that doubles its way up, over every value
    !! from -3 to 2049 -- each power of two to 2048 and both its neighbours -- and at the ends of
    !! the default integer's range, where `iand(n, n - 1)` would overflow unguarded.
    subroutine test_dct_length_helpers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: TOP = 2**(digits(1) - 1)
        integer :: n, p
        character(len=80) :: at

        do n = -3, 2049
            p = 1
            do while (p < n)
                p = 2*p
            end do
            write (at, '(a, i0)') " at n = ", n
            call check(error, pf_is_pow2(n) .eqv. (n >= 1 .and. p == n), &
                       "pf_is_pow2 must agree with a doubling count"//trim(at))
            if (allocated(error)) return
            if (n >= 1) then
                call check(error, pf_next_pow2(n) == p, "pf_next_pow2 must agree with a doubling count"//trim(at))
                if (allocated(error)) return
            end if
        end do

        call check(error, .not. pf_is_pow2(-huge(1) - 1) .and. .not. pf_is_pow2(-huge(1)) .and. &
                   .not. pf_is_pow2(huge(1)), "the ends of the integer range are not powers of two")
        if (allocated(error)) return
        call check(error, pf_is_pow2(TOP) .and. .not. pf_is_pow2(TOP - 1) .and. .not. pf_is_pow2(TOP + 1), &
                   "the largest power of two a default integer holds is one, and its neighbours are not")
        if (allocated(error)) return
        call check(error, pf_next_pow2(TOP) == TOP .and. pf_next_pow2(TOP - 1) == TOP .and. &
                   pf_next_pow2(TOP/2 + 1) == TOP .and. pf_next_pow2(TOP/2) == TOP/2, &
                   "pf_next_pow2 must reach the largest power of two a default integer holds")

    end subroutine test_dct_length_helpers

    !> The size ISJ uses, on the kind of input it transforms: 1024 cells of binned counts, all
    !! positive and bell-shaped, against the direct definition.
    subroutine test_dct_isj_grid_size(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 1024
        real(real64) :: counts(N), y(N), want(N)

        call fill_counts(counts, 6000)
        call pf_dct(counts, y)
        call direct_dct(counts, want)
        call check(error, maxval(abs(y - want)) <= COUNTS_TOL*EPS*sum(counts), &
                   "pf_dct of 1024 binned counts must match the direct definition")
        if (allocated(error)) return
        ! ISJ's own first step, (y(2:)/2)**2, is what the factor of two feeds.
        call check(error, abs(y(1) - 2*sum(counts)) <= EPS*2*sum(counts), &
                   "y(1) of binned counts must be twice their total")

    end subroutine test_dct_isj_grid_size

    !> Both dummies are plain assumed-shape arrays, so a strided section is read and written in
    !! place; the answer must be the contiguous call's, to the bit.
    subroutine test_dct_accepts_strided_sections(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 64
        real(real64) :: wide(2*N), out(3*N), x(N), y(N)

        call fill_uniform(wide, 7000)
        x = wide(1::2)
        out = -1.0_real64

        call pf_dct(x, y)
        call pf_dct(wide(1::2), out(2::3))
        call check(error, all(out(2::3) == y), "pf_dct of a strided section must equal the contiguous call")
        if (allocated(error)) return
        call check(error, all(out(1::3) == -1.0_real64) .and. all(out(3::3) == -1.0_real64), &
                   "pf_dct must write only the elements of its strided output")
        if (allocated(error)) return

        call pf_idct(x, y, norm="ortho")
        call pf_idct(wide(1::2), out(2::3), norm="ortho")
        call check(error, all(out(2::3) == y), "pf_idct of a strided section must equal the contiguous call")

    end subroutine test_dct_accepts_strided_sections

    !> A NaN is not screened: it reaches every output value of either transform, whichever element
    !! it is in, and raises nothing on the way (a quiet NaN only multiplies and adds here).
    subroutine test_dct_propagates_a_nan(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 16
        real(real64) :: x(N), y(N)
        integer :: at

        do at = 1, N, 5
            call fill_uniform(x, 8000 + at)
            x(at) = ieee_value(1.0_real64, ieee_quiet_nan)
            call pf_dct(x, y)
            call check(error, all(y /= y), "a NaN in x must reach every coefficient of pf_dct")
            if (allocated(error)) return
            call pf_idct(x, y, norm="ortho")
            call check(error, all(y /= y), "a NaN in y must reach every value of pf_idct")
            if (allocated(error)) return
        end do

    end subroutine test_dct_propagates_a_nan

    ! ---- the sine transforms ----------------------------------------------------------------------
    !
    ! The same list again, one test per cosine test, because the sine pair is built ON the cosine
    ! pair and a reader comparing the two halves should find the same claims made twice. Two of
    ! them are not mirrors: `test_dst_of_a_grid_sine` pins WHICH END carries the odd coefficient,
    ! and `test_dst_composes_with_dct` pins the index offset between the two spectra.

    !> **The convention itself**, against values `scipy.fft` produced: `dst` and `idst`, both norms,
    !! at `n = 8` and `n = 16`. The sine pair is the cosine pair on an alternated, reversed sequence,
    !! so a sign pattern applied at the wrong parity, or a reversal left out, moves every value here
    !! while leaving every self-consistency test in this suite passing.
    subroutine test_dst_matches_scipy_golden(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        call check_golden_sine(error, X8, X8_DST, X8_DST_ORTHO, X8_IDST, X8_IDST_ORTHO, "n = 8")
        if (allocated(error)) return
        call check_golden_sine(error, X16, X16_DST, X16_DST_ORTHO, X16_IDST, X16_IDST_ORTHO, "n = 16")

    end subroutine test_dst_matches_scipy_golden

    !> Both sine transforms against the `O(n**2)` definitions of the module header, at every power
    !! of two to 64 and at 4096, on the same zero-mean data the cosine pair is checked on. A length
    !! of one runs no butterfly and no reversal at all, and 4096 carries the accumulated twiddle
    !! error the reversal must not disturb.
    subroutine test_dst_matches_the_direct_definition(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: SIZES(8) = [1, 2, 4, 8, 16, 32, 64, 4096]
        real(real64), allocatable :: x(:), y(:), want(:)
        character(len=80) :: at
        integer :: i, n

        do i = 1, size(SIZES)
            n = SIZES(i)
            write (at, '(a, i0)') " at n = ", n
            allocate (x(n), y(n), want(n))
            call fill_uniform(x, 1500 + i)

            call pf_dst(x, y)
            call direct_dst(x, want)
            call check(error, maxval(abs(y - want)) <= DIRECT_TOL*EPS*sum(abs(x)), &
                       "pf_dst must match the direct definition"//trim(at))
            if (allocated(error)) return

            ! The same random values, read as coefficients.
            call pf_idst(x, y)
            call direct_idst(x, want)
            call check(error, maxval(abs(y - want)) <= DIRECT_TOL*EPS*sum(abs(x))/n, &
                       "pf_idst must match the direct definition"//trim(at))
            if (allocated(error)) return
            deallocate (x, y, want)
        end do

    end subroutine test_dst_matches_the_direct_definition

    !> `pf_idst(pf_dst(x))` gives `x` back under each norm, at several lengths: the alternation is
    !! undone at the same parity it was applied, and the reversal at the same end.
    subroutine test_dst_round_trips(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: SIZES(6) = [1, 2, 4, 8, 64, 1024]
        character(len=5), parameter :: NORMS(2) = ["none ", "ortho"]
        real(real64), allocatable :: x(:), y(:), back(:)
        character(len=80) :: at
        integer :: i, which, n

        do i = 1, size(SIZES)
            n = SIZES(i)
            allocate (x(n), y(n), back(n))
            call fill_uniform(x, 2500 + i)
            do which = 1, size(NORMS)
                write (at, '(a, i0, 3a)') " at n = ", n, ", norm = """, trim(NORMS(which)), """"
                call pf_dst(x, y, norm=trim(NORMS(which)))
                call pf_idst(y, back, norm=trim(NORMS(which)))
                call check(error, maxval(abs(back - x)) <= IDENTITY_TOL*EPS*maxval(abs(x)), &
                           "pf_idst(pf_dst(x)) must give x back"//trim(at))
                if (allocated(error)) return
            end do
            deallocate (x, y, back)
        end do

    end subroutine test_dst_round_trips

    !> Under `norm="ortho"` both sine transforms are orthonormal, so each keeps the sum of squares.
    !! `s(n-1)` is the one factor that differs from the rest, so a wrong one -- or the right one at
    !! the cosine pair's end -- breaks the identity by exactly its own term.
    subroutine test_dst_ortho_is_parseval(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: SIZES(5) = [1, 2, 8, 64, 1024]
        real(real64), allocatable :: x(:), y(:)
        real(real64) :: energy
        character(len=80) :: at
        integer :: i, n

        do i = 1, size(SIZES)
            n = SIZES(i)
            write (at, '(a, i0)') " at n = ", n
            allocate (x(n), y(n))
            call fill_uniform(x, 4500 + i)
            energy = pairwise_sum(x**2)

            call pf_dst(x, y, norm="ortho")
            call check(error, abs(pairwise_sum(y**2) - energy) <= IDENTITY_TOL*EPS*energy, &
                       "the orthonormal pf_dst must keep the sum of squares"//trim(at))
            if (allocated(error)) return

            call pf_idst(x, y, norm="ortho")
            call check(error, abs(pairwise_sum(y**2) - energy) <= IDENTITY_TOL*EPS*energy, &
                       "the orthonormal pf_idst must keep the sum of squares"//trim(at))
            if (allocated(error)) return
            deallocate (x, y)
        end do

    end subroutine test_dst_ortho_is_parseval

    !> A grid sine of frequency `m+1` has all its weight in `y(m+1)`, and **the top frequency is
    !! the one that carries twice as much** -- the mirror of the cosine transform, whose doubled
    !! coefficient is the constant term at `k = 0`. An implementation that reversed nothing, or
    !! reversed the spectrum of the wrong transform, puts the peak at `n-m` instead.
    subroutine test_dst_of_a_grid_sine(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        call check_grid_sine(error, 8, 0)
        if (allocated(error)) return
        call check_grid_sine(error, 8, 7)        ! frequency n: the doubled coefficient, at the end
        if (allocated(error)) return
        call check_grid_sine(error, 64, 5)
        if (allocated(error)) return
        call check_grid_sine(error, 64, 63)      ! frequency n again, at a length with more stages
        if (allocated(error)) return
        call check_grid_sine(error, 1024, 37)

    end subroutine test_dst_of_a_grid_sine

    !> Linearity, which no sign pattern applied at the wrong parity survives: negating the odd
    !! entries of `a*x + b*z` is `a` times negating `x`'s plus `b` times negating `z`'s only when
    !! the same entries are negated in all three.
    subroutine test_dst_is_linear(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: SIZES(2) = [64, 1024]
        real(real64), parameter :: A = 0.75_real64, B = -2.5_real64
        real(real64), allocatable :: x(:), z(:), mix(:), tx(:), tz(:), tmix(:)
        real(real64) :: scale
        character(len=80) :: at
        integer :: i, n

        do i = 1, size(SIZES)
            n = SIZES(i)
            write (at, '(a, i0)') " at n = ", n
            allocate (x(n), z(n), mix(n), tx(n), tz(n), tmix(n))
            call fill_uniform(x, 3500 + i)
            call fill_uniform(z, 3600 + i)
            mix = A*x + B*z
            scale = abs(A)*sum(abs(x)) + abs(B)*sum(abs(z))

            call pf_dst(x, tx)
            call pf_dst(z, tz)
            call pf_dst(mix, tmix)
            call check(error, maxval(abs(tmix - (A*tx + B*tz))) <= IDENTITY_TOL*EPS*scale, &
                       "pf_dst must be linear"//trim(at))
            if (allocated(error)) return

            call pf_idst(x, tx)
            call pf_idst(z, tz)
            call pf_idst(mix, tmix)
            call check(error, maxval(abs(tmix - (A*tx + B*tz))) <= IDENTITY_TOL*EPS*scale/n, &
                       "pf_idst must be linear"//trim(at))
            if (allocated(error)) return
            deallocate (x, z, mix, tx, tz, tmix)
        end do

    end subroutine test_dst_is_linear

    !> `"ortho"` is `"none"` scaled by `s(k) = sqrt(1/(2n))` everywhere but the LAST coefficient,
    !! where it is `sqrt(1/(4n))` -- the opposite end from `pf_dct`, and the one line of this pair's
    !! contract a reader cannot guess from the cosine pair. The tokens match whatever their case.
    subroutine test_dst_norm_tokens_relate_as_documented(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 64
        real(real64) :: x(N), y_none(N), y_ortho(N), y_other(N), back(N), back_other(N), s(N)

        call fill_uniform(x, 5500)
        s(1:N - 1) = sqrt(0.5_real64/N)
        s(N) = sqrt(0.25_real64/N)

        call pf_dst(x, y_none)
        call pf_dst(x, y_ortho, norm="ortho")
        call check(error, all(abs(y_ortho - s*y_none) <= 2*EPS*abs(s*y_none)), &
                   "pf_dst under norm=""ortho"" must be the unnormalised transform times s(k)")
        if (allocated(error)) return

        ! The exceptional factor is at the END. Reading it as the cosine pair's, at k = 0, changes
        ! exactly two coefficients, so assert it there rather than trusting the whole-array test.
        call check(error, abs(y_ortho(N)/y_none(N) - sqrt(0.25_real64/N)) <= 2*EPS, &
                   "the last sine coefficient must carry sqrt(1/(4n))")
        if (allocated(error)) return
        call check(error, abs(y_ortho(1)/y_none(1) - sqrt(0.5_real64/N)) <= 2*EPS, &
                   "the first sine coefficient must carry sqrt(1/(2n))")
        if (allocated(error)) return

        call pf_idst(x, back, norm="ortho")
        call pf_idst(x/s, back_other)
        call check(error, maxval(abs(back - back_other)) <= IDENTITY_TOL*EPS*maxval(abs(back)), &
                   "pf_idst under norm=""ortho"" must be pf_idst of the coefficients divided by s(k)")
        if (allocated(error)) return

        call pf_dst(x, y_other, norm="none")
        call check(error, all(y_other == y_none), "norm=""none"" must be what an absent norm means")
        if (allocated(error)) return
        call pf_dst(x, y_other, norm="OrThO ")
        call check(error, all(y_other == y_ortho), "norm=""OrThO "" must match ""ortho""")
        if (allocated(error)) return
        call pf_idst(x, back_other, norm="Ortho")
        call check(error, all(back_other == back), "pf_idst must match its norm token case-insensitively")

    end subroutine test_dst_norm_tokens_relate_as_documented

    !> Both dummies are plain assumed-shape arrays, and `pf_idst` hands `pf_idct` a REVERSED
    !! section of its own input, so a strided actual meets a strided section inside. The answer
    !! must be the contiguous call's, to the bit.
    subroutine test_dst_accepts_strided_sections(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 64
        real(real64) :: wide(2*N), out(3*N), x(N), y(N)

        call fill_uniform(wide, 7500)
        x = wide(1::2)
        out = -1.0_real64

        call pf_dst(x, y)
        call pf_dst(wide(1::2), out(2::3))
        call check(error, all(out(2::3) == y), "pf_dst of a strided section must equal the contiguous call")
        if (allocated(error)) return
        call check(error, all(out(1::3) == -1.0_real64) .and. all(out(3::3) == -1.0_real64), &
                   "pf_dst must write only the elements of its strided output")
        if (allocated(error)) return

        call pf_idst(x, y, norm="ortho")
        call pf_idst(wide(1::2), out(2::3), norm="ortho")
        call check(error, all(out(2::3) == y), "pf_idst of a strided section must equal the contiguous call")

    end subroutine test_dst_accepts_strided_sections

    !> A NaN is not screened by the sine pair either: it reaches every output value, whichever
    !! element it is in. The alternation multiplies by -1 and the reversal moves values, so neither
    !! can quieten one.
    subroutine test_dst_propagates_a_nan(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 16
        real(real64) :: x(N), y(N)
        integer :: at

        do at = 1, N, 5
            call fill_uniform(x, 8500 + at)
            x(at) = ieee_value(1.0_real64, ieee_quiet_nan)
            call pf_dst(x, y)
            call check(error, all(y /= y), "a NaN in x must reach every coefficient of pf_dst")
            if (allocated(error)) return
            call pf_idst(x, y, norm="ortho")
            call check(error, all(y /= y), "a NaN in y must reach every value of pf_idst")
            if (allocated(error)) return
        end do

    end subroutine test_dst_propagates_a_nan

    !> **The index offset between the two spectra**, which is what a caller composing a cosine
    !! transform with an inverse sine transform has to get right (the guide page's "Composing the
    !! two transforms"). `pf_dct` coefficient `k` and `pf_idst` coefficient `k-1` are the same
    !! frequency, so a spectrum handed from one to the other moves down one position and the top
    !! frequency is dropped.
    !!
    !! A grid cosine of frequency `m` has all its weight at `pf_dct` index `m`; restacked and
    !! inverted it must come back as the grid SINE of that same frequency `m`. Leaving the restack
    !! out answers the sine of frequency `m+1`, an `O(1)` error that every other test here passes.
    !! The tolerance is the grid-cosine test's: a spectrum whose peak is `n`, carried through two
    !! transforms of `log2(n)` stages.
    subroutine test_dst_composes_with_dct(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 64
        integer, parameter :: MODES(3) = [1, 7, 63]
        real(real64), allocatable :: table(:)
        real(real64) :: x(N), co(N), sc(N), got(N), want(N)
        character(len=80) :: at
        integer(int64) :: j, n64, angle
        integer :: i, m

        n64 = N
        call cos_table(n64, table)
        do i = 1, size(MODES)
            m = MODES(i)
            write (at, '(a, i0)') " at m = ", m
            do j = 1, n64
                angle = modulo(int(m, int64)*(2*j - 1), 4*n64)
                x(j) = table(angle)
                want(j) = sin_at(table, angle, n64)
            end do

            ! The whole composition: transform, restack by one position, invert as a sine.
            call pf_dct(x, co)
            sc(1:N - 1) = co(2:N)
            sc(N) = 0.0_real64
            call pf_idst(sc, got)

            call check(error, maxval(abs(got - want)) <= real(trailz(N), real64)*EPS*N, &
                       "a restacked cosine spectrum must invert to the sine of the same frequency"//trim(at))
            if (allocated(error)) return
        end do

    end subroutine test_dst_composes_with_dct

    ! ---- helpers ----------------------------------------------------------------------------------

    !> One scipy input: `pf_dct` and `pf_idct` under both norms against scipy's four outputs.
    subroutine check_golden(error, x, dct_none, dct_ortho, idct_none, idct_ortho, label)
        type(error_type), allocatable, intent(out) :: error         !! test-drive's error handle.
        real(real64), intent(in)                   :: x(:)          !! scipy's input
        real(real64), intent(in)                   :: dct_none(:)   !! `dct(x)`
        real(real64), intent(in)                   :: dct_ortho(:)  !! `dct(x, norm='ortho')`
        real(real64), intent(in)                   :: idct_none(:)  !! `idct(x)`
        real(real64), intent(in)                   :: idct_ortho(:) !! `idct(x, norm='ortho')`
        character(len=*), intent(in)               :: label         !! the size, for messages

        real(real64) :: got(size(x))
        real(real64) :: scale

        scale = GOLDEN_TOL*EPS*sum(abs(x))

        call pf_dct(x, got)
        call check(error, maxval(abs(got - dct_none)) <= scale, "pf_dct must give scipy's dct(x) at "//label)
        if (allocated(error)) return
        call pf_dct(x, got, norm="ortho")
        call check(error, maxval(abs(got - dct_ortho)) <= scale, &
                   "pf_dct must give scipy's dct(x, norm='ortho') at "//label)
        if (allocated(error)) return
        call pf_idct(x, got)
        call check(error, maxval(abs(got - idct_none)) <= scale/size(x), "pf_idct must give scipy's idct(x) at "//label)
        if (allocated(error)) return
        call pf_idct(x, got, norm="ortho")
        call check(error, maxval(abs(got - idct_ortho)) <= scale, &
                   "pf_idct must give scipy's idct(x, norm='ortho') at "//label)

    end subroutine check_golden

    !> One grid cosine: `x(j) = cos(pi*m*(2j-1)/(2n))`, built through the same exact angle
    !! reduction as the oracle, and its transform's one nonzero value.
    subroutine check_grid_cosine(error, n, m)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        integer, intent(in)                        :: n     !! the length
        integer, intent(in)                        :: m     !! the frequency, from 0 to n-1

        real(real64), allocatable :: table(:), x(:), y(:), want(:)
        character(len=80) :: at
        integer(int64) :: j, n64

        n64 = n
        call cos_table(n64, table)
        allocate (x(n), y(n), want(n))
        do j = 1, n64
            x(j) = table(modulo(m*(2*j - 1), 4*n64))
        end do
        want = 0.0_real64
        want(m + 1) = merge(2.0_real64*n, 1.0_real64*n, m == 0)

        ! The peak is n, and an FFT's rounding grows with log2(n) (`trailz` of a power of two).
        call pf_dct(x, y)
        write (at, '(a, i0, a, i0)') " at n = ", n, ", m = ", m
        call check(error, maxval(abs(y - want)) <= real(trailz(n), real64)*EPS*n, &
                   "a grid cosine must transform to one nonzero coefficient"//trim(at))

    end subroutine check_grid_cosine

    !> One scipy input: `pf_dst` and `pf_idst` under both norms against scipy's four outputs.
    subroutine check_golden_sine(error, x, dst_none, dst_ortho, idst_none, idst_ortho, label)
        type(error_type), allocatable, intent(out) :: error         !! test-drive's error handle.
        real(real64), intent(in)                   :: x(:)          !! scipy's input
        real(real64), intent(in)                   :: dst_none(:)   !! `dst(x, type=2)`
        real(real64), intent(in)                   :: dst_ortho(:)  !! `dst(x, type=2, norm='ortho')`
        real(real64), intent(in)                   :: idst_none(:)  !! `idst(x, type=2)`
        real(real64), intent(in)                   :: idst_ortho(:) !! `idst(x, type=2, norm='ortho')`
        character(len=*), intent(in)               :: label         !! the size, for messages

        real(real64) :: got(size(x))
        real(real64) :: scale

        scale = GOLDEN_TOL*EPS*sum(abs(x))

        call pf_dst(x, got)
        call check(error, maxval(abs(got - dst_none)) <= scale, "pf_dst must give scipy's dst(x) at "//label)
        if (allocated(error)) return
        call pf_dst(x, got, norm="ortho")
        call check(error, maxval(abs(got - dst_ortho)) <= scale, &
                   "pf_dst must give scipy's dst(x, norm='ortho') at "//label)
        if (allocated(error)) return
        call pf_idst(x, got)
        call check(error, maxval(abs(got - idst_none)) <= scale/size(x), &
                   "pf_idst must give scipy's idst(x) at "//label)
        if (allocated(error)) return
        call pf_idst(x, got, norm="ortho")
        call check(error, maxval(abs(got - idst_ortho)) <= scale, &
                   "pf_idst must give scipy's idst(x, norm='ortho') at "//label)

    end subroutine check_golden_sine

    !> One grid sine: `x(j) = sin(pi*(m+1)*(2j-1)/(2n))`, built through the same exact angle
    !! reduction as the oracle, and its transform's one nonzero value. The frequency is `m+1`
    !! because DST-II coefficient `m` carries it; at `m = n-1` that frequency is `n`, where the
    !! sequence is `(-1)**j` and the coefficient is `2n` rather than `n`.
    subroutine check_grid_sine(error, n, m)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        integer, intent(in)                        :: n     !! the length
        integer, intent(in)                        :: m     !! the coefficient index, from 0 to n-1

        real(real64), allocatable :: table(:), x(:), y(:), want(:)
        character(len=80) :: at
        integer(int64) :: j, n64

        n64 = n
        call cos_table(n64, table)
        allocate (x(n), y(n), want(n))
        do j = 1, n64
            x(j) = sin_at(table, modulo(int(m + 1, int64)*(2*j - 1), 4*n64), n64)
        end do
        want = 0.0_real64
        want(m + 1) = merge(2.0_real64*n, 1.0_real64*n, m == n - 1)

        ! The peak is n, and an FFT's rounding grows with log2(n) (`trailz` of a power of two).
        call pf_dst(x, y)
        write (at, '(a, i0, a, i0)') " at n = ", n, ", m = ", m
        call check(error, maxval(abs(y - want)) <= real(trailz(n), real64)*EPS*n, &
                   "a grid sine must transform to one nonzero coefficient"//trim(at))

    end subroutine check_grid_sine

    !> The type-II sine transform by its definition:
    !! `y[k] = 2*sum_j x[j]*sin(pi*(k+1)*(2j+1)/(2n))`.
    !!
    !! The same exact angle reduction and pairwise sum as `direct_dct`, reading the sine from
    !! `cos_table` through `sin_at`. It shares nothing with the library's FFT.
    subroutine direct_dst(x, y)
        real(real64), intent(in)  :: x(:) !! the sequence
        real(real64), intent(out) :: y(:) !! its unnormalised type-II sine transform

        real(real64), allocatable :: table(:), terms(:)
        integer(int64) :: n, j, k

        n = size(x, kind=int64)
        call cos_table(n, table)
        allocate (terms(n))
        do k = 0, n - 1
            do j = 0, n - 1
                terms(j + 1) = x(j + 1)*sin_at(table, modulo((k + 1)*(2*j + 1), 4*n), n)
            end do
            y(k + 1) = 2.0_real64*pairwise_sum(terms)
        end do

    end subroutine direct_dst

    !> The inverse by its definition:
    !! `x[k] = ((-1)**k*y[n-1] + 2*sum_{j<n-1} y[j]*sin(pi*(j+1)*(2k+1)/(2n)))/(2n)`.
    !!
    !! The unpaired term is the TOP frequency, where `direct_idct`'s is the constant term `y[0]`.
    subroutine direct_idst(y, x)
        real(real64), intent(in)  :: y(:) !! the coefficients
        real(real64), intent(out) :: x(:) !! the sequence they are the transform of

        real(real64), allocatable :: table(:), terms(:)
        integer(int64) :: n, j, k

        n = size(y, kind=int64)
        call cos_table(n, table)
        allocate (terms(n))
        do k = 0, n - 1
            do j = 0, n - 2
                terms(j + 1) = 2.0_real64*y(j + 1)*sin_at(table, modulo((j + 1)*(2*k + 1), 4*n), n)
            end do
            terms(n) = merge(-1.0_real64, 1.0_real64, mod(k, 2_int64) == 1)*y(n)
            x(k + 1) = pairwise_sum(terms)/real(2*n, real64)
        end do

    end subroutine direct_idst

    !> `sin(pi*m/(2n))` read from `cos_table`'s entries: `sin(t) = cos(t - pi/2)`, and `pi/2` is
    !! `n` steps of `pi/(2n)`, so the sine at `m` is the cosine at `m - n`, reduced modulo `4n`.
    !! One table serves both definitions, and both keep the exact angle reduction.
    pure function sin_at(table, m, n) result(s)
        real(real64), intent(in)   :: table(0:) !! `cos_table`'s values
        integer(int64), intent(in) :: m         !! the step count, already reduced or not
        integer(int64), intent(in) :: n         !! the transform length
        real(real64)               :: s         !! `sin(pi*m/(2n))`

        s = table(modulo(m - n, 4*n))

    end function sin_at

    !> The type-II transform by its definition: `y[k] = 2*sum_j x[j]*cos(pi*k*(2j+1)/(2n))`.
    !!
    !! Each angle is reduced first -- `k*(2j+1)` modulo `4n`, exactly, in integer arithmetic -- and
    !! its cosine read from `cos_table`, so no argument passes `2*pi`; the terms are summed pairwise.
    !! It shares nothing with the library's FFT.
    subroutine direct_dct(x, y)
        real(real64), intent(in)  :: x(:) !! the sequence
        real(real64), intent(out) :: y(:) !! its unnormalised type-II transform

        real(real64), allocatable :: table(:), terms(:)
        integer(int64) :: n, j, k

        n = size(x, kind=int64)
        call cos_table(n, table)
        allocate (terms(n))
        do k = 0, n - 1
            do j = 0, n - 1
                terms(j + 1) = x(j + 1)*table(modulo(k*(2*j + 1), 4*n))
            end do
            y(k + 1) = 2.0_real64*pairwise_sum(terms)
        end do

    end subroutine direct_dct

    !> The inverse by its definition: `x[k] = (y[0] + 2*sum_{j>=1} y[j]*cos(pi*j*(2k+1)/(2n)))/(2n)`,
    !! with the same exact angle reduction and pairwise sum as `direct_dct`.
    subroutine direct_idct(y, x)
        real(real64), intent(in)  :: y(:) !! the coefficients
        real(real64), intent(out) :: x(:) !! the sequence they are the transform of

        real(real64), allocatable :: table(:), terms(:)
        integer(int64) :: n, j, k

        n = size(y, kind=int64)
        call cos_table(n, table)
        allocate (terms(n))
        do k = 0, n - 1
            terms(1) = y(1)
            do j = 1, n - 1
                terms(j + 1) = 2.0_real64*y(j + 1)*table(modulo(j*(2*k + 1), 4*n))
            end do
            x(k + 1) = pairwise_sum(terms)/real(2*n, real64)
        end do

    end subroutine direct_idct

    !> `cos(pi*m/(2n))` for `m = 0 .. 4n-1`: every cosine the two definitions need, each of an angle
    !! below `2*pi` formed with one rounding (`m/(2n)` is exact).
    subroutine cos_table(n, table)
        integer(int64), intent(in)             :: n        !! the transform length
        real(real64), allocatable, intent(out) :: table(:) !! indexed from 0

        integer(int64) :: m

        allocate (table(0:4*n - 1))
        do m = 0, 4*n - 1
            table(m) = cos(PI*(real(m, real64)/real(2*n, real64)))
        end do

    end subroutine cos_table

    !> The sum of `v` by recursive halving, whose rounding error grows with `log2(size(v))` where a
    !! running sum's grows with `size(v)`.
    recursive function pairwise_sum(v) result(total)
        real(real64), intent(in) :: v(:)  !! the terms
        real(real64)             :: total !! their sum

        integer :: i, half

        if (size(v) <= 8) then
            total = 0.0_real64
            do i = 1, size(v)
                total = total + v(i)
            end do
        else
            half = size(v)/2
            total = pairwise_sum(v(1:half)) + pairwise_sum(v(half + 1:))
        end if

    end function pairwise_sum

    !> **The guide page's whole "Values the transform does not screen" section**, which nothing
    !! asserted before: what each of its three exceptional inputs RAISES, not only what it returns.
    !!
    !! The page promises a NaN raises none of the three exceptions a build can stop on. Read over
    !! all five IEEE flags that promise is false and always was: `IEEE_INEXACT` is raised by every
    !! transform of two values or more, whatever the input, because the twiddle factors' `cos` and
    !! `sin` are inexact. So the assertion is over `ieee_usual` -- the three halting exceptions --
    !! exactly as `test_root_extreme_calls_raise_no_flag` (`test/test_root.f90`) does, and the page
    !! says so in as many words. (`parquet_skycoord` may promise the stronger thing because it
    !! SCREENS a NaN before any arithmetic; this module deliberately screens nothing, which is what
    !! its section is called.)
    !!
    !! **The negative controls are the section's other two bullets, in this same test**: an
    !! infinite input must raise something, and a sequence near `huge` must too. Without them a
    !! test that read the flags wrongly -- cleared and never re-read, or read an array nothing
    !! writes -- would pass by reading zeros for every arm. They assert THAT a flag was raised and
    !! never WHICH: which exception a site raises is not portable (`fortran-gotchas.md`).
    !!
    !! Held off, read and restored in this body and never in a helper: F2018 17.3 restores the
    !! halting modes on return from any procedure, and quietens a flag signalling on entry to one
    !! until it returns, so a helper would change and see nothing.
    subroutine test_transform_extreme_inputs_raise_the_documented_flags(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: N = 16
        real(real64) :: x(N), y(N), nan, inf
        logical :: halting(size(ieee_usual)), saved(size(ieee_usual))
        logical :: on_nan(size(ieee_usual)), on_inf(size(ieee_usual)), on_huge(size(ieee_usual))
        logical :: uf_ok, uf_was
        integer :: k

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        inf = ieee_value(1.0_real64, ieee_positive_inf)

        ! UNDERFLOW is saved and put back beside them but never asserted on: these inputs are
        ! extreme by construction and may underflow harmlessly. Naming the kind is load-bearing --
        ! the bare `ieee_support_flag(ieee_underflow)` is false under nagfor (`fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif

        ! Arm 1: a NaN at four different positions, through all four entry points.
        call ieee_set_flag(ieee_usual, .false.)
        do k = 1, N, 5
            call fill_uniform(x, 9100 + k)
            x(k) = nan
            call pf_dct(x, y)
            call pf_idct(x, y)
            call pf_dst(x, y)
            call pf_idst(x, y)
        end do
        call ieee_get_flag(ieee_usual, on_nan)

        ! Arm 2, the first negative control: an infinity in the input.
        call ieee_set_flag(ieee_usual, .false.)
        call fill_uniform(x, 9200)
        x(3) = inf
        call pf_dct(x, y)
        call ieee_get_flag(ieee_usual, on_inf)

        ! Arm 3, the second: y(1) is 2*n times the mean, so a sequence at a quarter of huge runs
        ! off the top of the range.
        call ieee_set_flag(ieee_usual, .false.)
        x = huge(1.0_real64)/4.0_real64
        call pf_dct(x, y)
        call ieee_get_flag(ieee_usual, on_huge)

        call ieee_set_flag(ieee_usual, saved .or. on_nan .or. on_inf .or. on_huge)
        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif

        call check(error, .not. any(on_nan), &
                   "a NaN input raised overflow, invalid or divide-by-zero in one of the four transforms")
        if (allocated(error)) return
        call check(error, any(on_inf), &
                   "an infinite input must raise one: without that the NaN arm above proves nothing")
        if (allocated(error)) return
        call check(error, any(on_huge), &
                   "a sequence near huge must raise one, or the overflow the page documents is not real")

    end subroutine test_transform_extreme_inputs_raise_the_documented_flags

#ifndef __flang__
    !> Can overflow, invalid and divide-by-zero all be held off around a call?
    !!
    !! nagfor halts on the three by default, so a regression that raised one would take the whole
    !! runner down before the test could assert anything. This inquiry is the only part of the
    !! bracket a helper may carry (see the test above).
    function traps_can_be_held() result(can)
        logical :: can !! `ieee_support_halting` holds for overflow, invalid and divide-by-zero

        can = ieee_support_halting(ieee_overflow) .and. ieee_support_halting(ieee_invalid) &
            .and. ieee_support_halting(ieee_divide_by_zero)

    end function traps_can_be_held
#endif

    !> Fills `x` with values on `(-1, 1)`, exact in binary, from MINSTD (Park and Miller's
    !! `s = 48271*s mod (2**31 - 1)`): the same values under every compiler, zero-mean, and with no
    !! structure the transform would single out.
    subroutine fill_uniform(x, seed)
        real(real64), intent(out) :: x(:) !! filled
        integer, intent(in)       :: seed !! selects the stream; from 1 to 2**31 - 2

        integer(int64) :: s
        integer :: i

        s = seed
        do i = 1, size(x)
            s = modulo(48271_int64*s, 2147483647_int64)
            x(i) = real(s - 1073741824_int64, real64)/1073741824.0_real64
        end do

    end subroutine fill_uniform

    !> Fills `counts` as a binned sample would: 20000 draws, each the sum of three MINSTD uniforms
    !! (a bell), counted into `size(counts)` equal cells. Integers, exact in binary.
    subroutine fill_counts(counts, seed)
        real(real64), intent(out) :: counts(:) !! the counts per cell
        integer, intent(in)       :: seed      !! selects the stream; from 1 to 2**31 - 2

        integer(int64) :: s, total, cell
        integer :: draw, part

        counts = 0.0_real64
        s = seed
        do draw = 1, 20000
            total = 0
            do part = 1, 3
                s = modulo(48271_int64*s, 2147483647_int64)
                total = total + s
            end do
            cell = (total*size(counts, kind=int64))/(3*2147483647_int64) + 1
            counts(cell) = counts(cell) + 1.0_real64
        end do

    end subroutine fill_counts

end module test_transform
