!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_stats_vectors.py
!
!> Golden expectations for `parquet_stats`, derived at 50 decimal digits by `mpmath`.
!!
!! **The oracle deliberately shares no arithmetic with the library, and none with numpy either.**
!! Every value here is computed over the fixture's exact rationals, because the accuracy claim
!! `parquet_stats` makes is precisely that it does not lose the digits a cancelling formula loses
!! -- and a reference computed the cancelling way cannot certify that. numpy, pandas and scipy
!! appear only in the generator's `--self-test`, where they confirm the model transcribes the same
!! definitions they implement.
!!
!! Each case is one population and one configuration. `G_<name>` holds the nine tier-A quantities
!! in the order `QNAME` names; `G_<name>_DEF` says which of them are DEFINED, so an undefined
!! statistic is asserted to be a quiet NaN rather than compared against a number. `G_<name>_N`
!! carries `[n_valid, n_null, n_nan]`.
!!
!! The populations themselves are recipes rather than literals -- see `golden_fixture` in
!! test/test_stats.f90, and `G_PROBE`, which is what reports a recipe that has drifted.
module test_stats_golden
    use iso_fortran_env, only : int64, real64
    implicit none
    public

    !> How many tier-A quantities each case carries.
    integer, parameter :: NQ = 9
    !> What each slot of a `G_*` array holds, in order.
    character(len=8), parameter :: QNAME(NQ) = [character(len=8) :: &
        "sum", "mean", "var", "stddev", "sem", "skew", "kurt", "min", "max"]

    !> How many probabilities each `Q_*` row carries.
    integer, parameter :: NQP = 7

    !> The first 8 values `golden_fixture` must produce. A recipe that has drifted from the
    !! generator's is reported here, as a fixture mismatch, rather than as an unexplained
    !! tolerance failure in every case at once.
    real(real64), parameter :: G_PROBE(8) = [-468.4931640625_real64, -445.29296875_real64, -406.6259765625_real64,  &
        -352.4921875_real64, -282.8916015625_real64, -197.82421875_real64, -97.2900390625_real64, 18.7109375_real64]

    !> the recipe at n=32, unweighted, ddof=1 -- the default configuration
    real(real64), parameter :: G_U32(NQ) = [-1964.7099609375_real64, -61.397186279296875_real64, 84348.783993643141_real64,  &
        290.42862116816781_real64, 51.341011869667589_real64, 0.28122217281686601_real64, -1.2292240838144326_real64,  &
        -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_U32_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_U32_N(3) = [32_int64, 0_int64, 0_int64]

    !> ddof=0: the population variance, which is what numpy's default returns
    real(real64), parameter :: G_U32_D0(NQ) = [-1964.7099609375_real64, -61.397186279296875_real64, 81712.88449384179_real64,  &
        285.85465623956833_real64, 50.532441465187055_real64, 0.28122217281686601_real64, -1.2292240838144326_real64,  &
        -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_U32_D0_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_U32_D0_N(3) = [32_int64, 0_int64, 0_int64]

    !> ddof >= n_valid: NaN, not a division by zero and not an abort
    real(real64), parameter :: G_U32_D99(NQ) = [-1964.7099609375_real64, -61.397186279296875_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.28122217281686601_real64, -1.2292240838144326_real64, -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_U32_D99_DEF(NQ) = [.true., .true., .false., .false., .false., .true., .true., .true., .true.]
    integer(int64), parameter :: G_U32_D99_N(3) = [32_int64, 0_int64, 0_int64]

    !> bias=.true.: scipy's uncorrected g1 and g2 rather than pandas' G1 and G2
    real(real64), parameter :: G_U32_BIAS(NQ) = [-1964.7099609375_real64, -61.397186279296875_real64,  &
        84348.783993643141_real64, 290.42862116816781_real64, 51.341011869667589_real64, 0.26786438747258629_real64,  &
        -1.2271993674668196_real64, -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_U32_BIAS_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_U32_BIAS_N(3) = [32_int64, 0_int64, 0_int64]

    !> excess=.false.: the raw fourth-moment ratio, 3 for a normal population
    real(real64), parameter :: G_U32_RAWK(NQ) = [-1964.7099609375_real64, -61.397186279296875_real64,  &
        84348.783993643141_real64, 290.42862116816781_real64, 51.341011869667589_real64, 0.28122217281686601_real64,  &
        1.7707759161855674_real64, -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_U32_RAWK_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_U32_RAWK_N(3) = [32_int64, 0_int64, 0_int64]

    !> the recipe at n=1000 -- the partner of SHIFT
    real(real64), parameter :: G_U1000(NQ) = [5762.814453125_real64, 5.7628144531250003_real64, 83754.969364333432_real64,  &
        289.40450819628472_real64, 9.151774110211278_real64, -0.010088184517102429_real64, -1.2561393454351846_real64,  &
        -487.1787109375_real64, 486.5712890625_real64]
    logical, parameter :: G_U1000_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_U1000_N(3) = [1000_int64, 0_int64, 0_int64]

    !> the same population offset by 1e9: variance and above must match U1000
    real(real64), parameter :: G_SHIFT(NQ) = [1000000005762.8145_real64, 1000000005.7628144_real64, 83754.969364333432_real64,  &
        289.40450819628472_real64, 9.151774110211278_real64, -0.010088184517102429_real64, -1.2561393454351846_real64,  &
        999999512.82128906_real64, 1000000486.5712891_real64]
    logical, parameter :: G_SHIFT_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_SHIFT_N(3) = [1000_int64, 0_int64, 0_int64]

    !> every weight 3, reliability: must equal U32 exactly -- the equal-weight property
    real(real64), parameter :: G_W3(NQ) = [-5894.1298828125_real64, -61.397186279296875_real64, 84348.783993643141_real64,  &
        290.42862116816781_real64, 51.341011869667589_real64, 0.28122217281686601_real64, -1.2292240838144326_real64,  &
        -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_W3_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_W3_N(3) = [32_int64, 0_int64, 0_int64]

    !> every weight 3, frequency: must NOT equal U32, because ddof is charged differently
    real(real64), parameter :: G_W3F(NQ) = [-5894.1298828125_real64, -61.397186279296875_real64, 82573.020120092755_real64,  &
        287.35521592637355_real64, 29.328068914454054_real64, 0.27213508929140717_real64, -1.2283964963631597_real64,  &
        -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_W3F_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_W3F_N(3) = [32_int64, 0_int64, 0_int64]

    !> unequal weights including zeros, reliability -- n_valid < size(values)
    real(real64), parameter :: G_WVAR(NQ) = [-3536.4697265625_real64, -56.134440104166664_real64, 69934.647865176958_real64,  &
        264.45159834112735_real64, 57.094136759123728_real64, 0.066090101054669101_real64, -1.1582108283566719_real64,  &
        -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_WVAR_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_WVAR_N(3) = [26_int64, 0_int64, 0_int64]

    !> the same weights, frequency: the two conventions separate here and only here
    real(real64), parameter :: G_WVARF(NQ) = [-3536.4697265625_real64, -56.134440104166664_real64, 67750.309145373685_real64,  &
        260.28889554756978_real64, 32.793318411930329_real64, 0.062883750184048279_real64, -1.1695950451235197_real64,  &
        -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_WVARF_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_WVARF_N(3) = [26_int64, 0_int64, 0_int64]

    !> one element in three null: n_null reports them and nothing else changes
    real(real64), parameter :: G_NULLS(NQ) = [-1537.6650390625_real64, -69.893865411931813_real64, 94798.658997252904_real64,  &
        307.89390867188797_real64, 65.643201890375423_real64, 0.43835269375815666_real64, -1.169112023899733_real64,  &
        -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_NULLS_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_NULLS_N(3) = [22_int64, 10_int64, 0_int64]

    !> every tenth value NaN, its weight NaN too -- pins the exclusion ORDER
    real(real64), parameter :: G_NANS(NQ) = [-1597.1337890625_real64, -55.073578933189658_real64, 83482.684332662029_real64,  &
        288.93370231363116_real64, 53.653641561877372_real64, 0.23887045651280137_real64, -1.1480856546434646_real64,  &
        -468.4931640625_real64, 459.5146484375_real64]
    logical, parameter :: G_NANS_DEF(NQ) = [.true., .true., .true., .true., .true., .true., .true., .true., .true.]
    integer(int64), parameter :: G_NANS_N(3) = [29_int64, 0_int64, 3_int64]

    !> the same values with skipnan=.false.: one NaN makes every answer NaN
    real(real64), parameter :: G_NANS_KEEP(NQ) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64]
    logical, parameter :: G_NANS_KEEP_DEF(NQ) = [.false., .false., .false., .false., .false., .false., .false., .false.,  &
        .false.]
    integer(int64), parameter :: G_NANS_KEEP_N(3) = [32_int64, 0_int64, 0_int64]

    !> 17 identical values: variance exactly 0, and neither shape statistic defined
    real(real64), parameter :: G_CONST(NQ) = [42.5_real64, 2.5_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 2.5_real64, 2.5_real64]
    logical, parameter :: G_CONST_DEF(NQ) = [.true., .true., .true., .true., .true., .false., .false., .true., .true.]
    integer(int64), parameter :: G_CONST_N(3) = [17_int64, 0_int64, 0_int64]

    !> one element: mean is that value, variance at ddof=1 is NaN
    real(real64), parameter :: G_SINGLE(NQ) = [3.75_real64, 3.75_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 3.75_real64, 3.75_real64]
    logical, parameter :: G_SINGLE_DEF(NQ) = [.true., .true., .false., .false., .false., .false., .false., .true., .true.]
    integer(int64), parameter :: G_SINGLE_N(3) = [1_int64, 0_int64, 0_int64]

    !> one element at ddof=0: variance is exactly 0
    real(real64), parameter :: G_SINGLE_D0(NQ) = [3.75_real64, 3.75_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 3.75_real64, 3.75_real64]
    logical, parameter :: G_SINGLE_D0_DEF(NQ) = [.true., .true., .true., .true., .true., .false., .false., .true., .true.]
    integer(int64), parameter :: G_SINGLE_D0_N(3) = [1_int64, 0_int64, 0_int64]

    !> a zero-length array: every answer NaN except the sum, n_valid 0, and NO abort
    real(real64), parameter :: G_EMPTY(NQ) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64]
    logical, parameter :: G_EMPTY_DEF(NQ) = [.true., .false., .false., .false., .false., .false., .false., .false., .false.]
    integer(int64), parameter :: G_EMPTY_N(3) = [0_int64, 0_int64, 0_int64]

    !> every element null: the empty case reached the way a per-group loop reaches it
    real(real64), parameter :: G_ALLNULL(NQ) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64]
    logical, parameter :: G_ALLNULL_DEF(NQ) = [.true., .false., .false., .false., .false., .false., .false., .false., .false.]
    integer(int64), parameter :: G_ALLNULL_N(3) = [0_int64, 8_int64, 0_int64]

    !> every weight zero: the empty case again, and numpy raises where this does not
    real(real64), parameter :: G_ALLZEROW(NQ) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64]
    logical, parameter :: G_ALLZEROW_DEF(NQ) = [.true., .false., .false., .false., .false., .false., .false., .false., .false.]
    integer(int64), parameter :: G_ALLZEROW_N(3) = [0_int64, 0_int64, 0_int64]

    !> The probabilities every Q_* row below is evaluated at.
    real(real64), parameter :: G_QPROBS(NQP) = [0.0_real64, 0.10000000000000001_real64, 0.25_real64, 0.5_real64, 0.75_real64,  &
        0.90000000000000002_real64, 1.0_real64]

    !> the default rule at n=32: Hyndman-Fan type 7, numpy's and pandas' default
    real(real64), parameter :: Q_U32_LINEAR(NQP) = [-468.4931640625_real64, -402.93837890625002_real64,  &
        -319.21826171875_real64, -114.283203125_real64, 154.67626953125_real64, 333.04912109374999_real64,  &
        459.5146484375_real64]

    !> method=lower: the order statistic at or below the position
    real(real64), parameter :: Q_U32_LOWER(NQP) = [-468.4931640625_real64, -406.6259765625_real64, -339.1826171875_real64,  &
        -131.2763671875_real64, 150.1787109375_real64, 297.11328125_real64, 459.5146484375_real64]

    !> method=higher: the order statistic at or above it
    real(real64), parameter :: Q_U32_HIGHER(NQP) = [-468.4931640625_real64, -369.75_real64, -312.5634765625_real64,  &
        -97.2900390625_real64, 168.1689453125_real64, 337.0419921875_real64, 459.5146484375_real64]

    !> method=nearest: whichever of the two is closer
    real(real64), parameter :: Q_U32_NEAREST(NQP) = [-468.4931640625_real64, -406.6259765625_real64, -312.5634765625_real64,  &
        -97.2900390625_real64, 150.1787109375_real64, 337.0419921875_real64, 459.5146484375_real64]

    !> method=midpoint: their mean
    real(real64), parameter :: Q_U32_MIDPOINT(NQP) = [-468.4931640625_real64, -388.18798828125_real64, -325.873046875_real64,  &
        -114.283203125_real64, 159.173828125_real64, 317.07763671875_real64, 459.5146484375_real64]

    !> method=inverted_cdf: a step function on the plain cumulative scale
    real(real64), parameter :: Q_U32_ICDF(NQP) = [-468.4931640625_real64, -406.6259765625_real64, -339.1826171875_real64,  &
        -131.2763671875_real64, 150.1787109375_real64, 337.0419921875_real64, 459.5146484375_real64]

    !> odd length, so the median is an element rather than an interpolation
    real(real64), parameter :: Q_U33_LINEAR(NQP) = [-468.4931640625_real64, -399.25078124999999_real64, -312.5634765625_real64,  &
        -97.2900390625_real64, 150.1787109375_real64, 329.05624999999998_real64, 459.5146484375_real64]

    !> every weight 3: must equal U32_LINEAR EXACTLY -- the equal-weight reduction
    real(real64), parameter :: Q_W3_LINEAR(NQP) = [-468.4931640625_real64, -402.93837890625002_real64, -319.21826171875_real64,  &
        -114.283203125_real64, 154.67626953125_real64, 333.04912109374999_real64, 459.5146484375_real64]

    !> unequal weights: the derived rule, with no library to cross-check against
    real(real64), parameter :: Q_WVAR_LINEAR(NQP) = [-468.4931640625_real64, -381.06422008167613_real64,  &
        -291.5205078125_real64, 11.49196337090164_real64, 133.34337573366116_real64, 304.89545898437501_real64,  &
        459.5146484375_real64]

    !> the same weights on the cumulative scale, which numpy DOES implement
    real(real64), parameter :: Q_WVAR_ICDF(NQP) = [-468.4931640625_real64, -406.6259765625_real64, -291.5205078125_real64,  &
        18.7109375_real64, 150.1787109375_real64, 337.0419921875_real64, 459.5146484375_real64]

    !> one element in three null: the quantiles are of what survives
    real(real64), parameter :: Q_NULLS_LINEAR(NQP) = [-468.4931640625_real64, -437.73867187500002_real64,  &
        -307.302734375_real64, -138.56201171875_real64, 156.029541015625_real64, 359.86367187500002_real64,  &
        459.5146484375_real64]

    !> Each M_* row is `[scale="normal", scale="raw"]`, so their ratio pins the normal scale factor.
    !> the default: centre is the population's own median, scale is "normal"
    real(real64), parameter :: M_U32(2) = [349.69938724077031_real64, 235.86865234375_real64]

    !> odd length, so the centre is an element rather than an interpolation
    real(real64), parameter :: M_U33(2) = [358.63047296816467_real64, 241.892578125_real64]

    !> an explicit centre, which skips one selection and changes the deviations
    real(real64), parameter :: M_CTR(2) = [422.6263317172436_real64, 285.05712890625_real64]

    !> unequal weights: both medians are weighted, values and deviations alike
    real(real64), parameter :: M_WVAR(2) = [306.97171108786517_real64, 207.04927272891794_real64]

    !> one element in three null: the deviations are of what survives
    real(real64), parameter :: M_NULLS(2) = [329.96658876810153_real64, 222.55908203125_real64]

    !> four wild points in thirty-six: this is the case pf_stddev gets wrong and MAD does not
    real(real64), parameter :: M_OUT(2) = [365.96240425186812_real64, 246.837890625_real64]

end module test_stats_golden ! GCOVR_EXCL_LINE
