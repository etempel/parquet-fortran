!> Tests for `parquet_cosmology`: the model, the three tables, the fallback rule beyond them, the
!> closed forms over them, the density parameters, the five inverses and the redshift conversions.
!!
!! **Every expected value comes from `test_cosmology_vectors`**, a 30-digit `mpmath` model of
!! astropy 8.0.1's `w0waCDM` emitted by `tools/generate_cosmology_reference.py`. Nothing here is a
!! number read off a Fortran run, because the failure this suite exists to catch is a plausible
!! WRONG ANSWER: drop the neutrino term from `E(z)` and `Planck18`'s comoving distance moves by
!! about a part in a thousand, which no self-consistency check would notice.
!!
!! **The tolerances are the TABLE's accuracy, and they are measured rather than chosen.** Inside
!! the table a scaled not-a-knot spline over a grid of `h = 0.008` in `zeta` carries about `4e-11`
!! on the distance and `1.4e-10` on the lookback time; beyond it the fixed 20-point rule is exact
!! to rounding but starts from the tabulated edge value, so it INHERITS that error rather than
!! improving on it. The age past the table is the one quantity that starts from nothing tabulated,
!! and it is the one asserted tightly.
!!
!! **Three tests exist because a plausible implementation passes without them.** A single-form
!! `pf_z2zeta` is right at small `z` and `3.6e-9` wrong at `z = 1e10`, so
!! `z2zeta_is_two_ulp_over_the_whole_range` sweeps the whole range rather than the small end the
!! identity was introduced for. An age formed as `%age(0) - %lookback_time(z)` passes at every
!! redshift below about 10 and has no correct digit at `1e10`, so
!! `age_is_relative_accurate_where_it_is_small` asserts at the top of the domain. And a curved
!! model's `comoving_volume` closed form keeps nothing at small `z`, which no FLAT fixture can
!! show, so `curved_volume_is_accurate_at_small_redshift` uses the open and closed models.
!!
!! This suite is pure computation with no fixture files and no process-global state, so it stays
!! out of the runner's parallelism exclusion list and runs concurrently. Its only library import
!! is `use parquet_cosmology`.
module test_cosmology

    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet_cosmology
    use test_cosmology_vectors
    use iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_get_flag, ieee_set_flag, ieee_support_flag, &
        ieee_get_halting_mode, ieee_set_halting_mode, ieee_support_halting, ieee_usual, &
        ieee_overflow, ieee_invalid, ieee_divide_by_zero, ieee_is_finite, ieee_value, &
        ieee_quiet_nan, ieee_positive_inf

    implicit none
    private

    public :: collect_tests_cosmology

    !> What a quantity read out of the table may differ from the 30-digit model by.
    !!
    !! Measured, not chosen: the worst over all 20 models, all 25 redshifts and all 15 quantities
    !! is about `4e-10`, on the volume of a curved model, where the distance's own error is cubed.
    real(real64), parameter :: TABLE_TOL = 5.0e-9_real64
    !> What the age past the table may differ by: it starts from nothing tabulated.
    real(real64), parameter :: AGE_TAIL_TOL = 1.0e-11_real64
    !> What a round trip through an inverse may lose, in the DISTANCE.
    real(real64), parameter :: INVERSE_TOL = 1.0e-13_real64
    !> What a round trip through an inverse may lose at a BLUESHIFT, where the loss is the
    !! argument's and not the iteration's.
    !!
    !! An inverse answers in `z`, and at `zeta = -10` the quantity `1 + z` is `4.5e-5`, so a
    !! `real64` `z` pins `zeta` only to `eps/(1 + z)`, about `2e-12`. The lookback time's slope
    !! `t_H/E` is of order 17 Gyr per unit `zeta` there, so `t` comes back about `2e-13` relative
    !! away -- measured `2.1e-13`, which is this bound with a factor of fifty to spare. The
    !! DISTANCE does not suffer it: `dD_C/dzeta` carries a factor `e^zeta`, which is `5e-5` at the
    !! same point, so the distance round trip stays at `4e-16` over the whole domain.
    real(real64), parameter :: BLUESHIFT_INVERSE_TOL = 1.0e-11_real64
    !> What `%age(%z_at_age(t))` may lose at a BLUESHIFT, for the same reason and a larger one.
    !!
    !! `z` pins `zeta` only to `eps/(1 + z)`, which is `8e-7` at `zeta = -22`, and the age's own
    !! relative slope there is about `0.04` per unit `zeta` -- it is four hundred Gyr and changing
    !! by seventeen. Measured worst over all twenty models: `6.0e-7`, so this bound has a factor
    !! of seventeen to spare. Above `z = 0` the same round trip holds `6.9e-15`.
    real(real64), parameter :: AGE_BLUESHIFT_TOL = 1.0e-5_real64
    !> What a round trip through `%z_at_age`, `%z_at_luminosity_distance` or `%z_at_distmod` may
    !! lose at a redshift at or above zero. Measured worst: `6.9e-15`, `1.1e-15` and `2.6e-16`.
    real(real64), parameter :: STAGE2_INVERSE_TOL = 1.0e-13_real64
    !> What `%om + %ode + %ok + %ogamma + %onu` may differ from one by. Measured worst over all
    !! twenty models and the whole domain: `2.2e-16`, which is one ulp.
    real(real64), parameter :: DENSITY_SUM_TOL = 1.0e-14_real64
    !> `ln(1 + 1e10)`, the domain's edge, as the module carries it.
    real(real64), parameter :: CEILING_ZETA = 23.02585093004047_real64

contains

    !> Registers this module's tests.
    subroutine collect_tests_cosmology(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("the eight named cosmologies reproduce every reference row", &
                         test_named_cosmologies_match_the_reference), &
            new_unittest("the user-defined grid reproduces every reference row", &
                         test_parameter_form_matches_the_reference), &
            new_unittest("every model's derived parameters match the reference", &
                         test_derived_parameters_match_the_reference), &
            new_unittest("omitting ode0 makes ok0 exactly zero, not nearly zero", &
                         test_flat_by_omission_is_exactly_flat), &
            new_unittest("a zmax=1e-6 object answers the full range on its fallback alone", &
                         test_table_agrees_with_the_fallback_rule), &
            new_unittest("zmax moves the seam and not the answer, the age included", &
                         test_zmax_moves_the_seam_not_the_answer), &
            new_unittest("the seam at the table's edge is continuous", &
                         test_seam_is_continuous), &
            new_unittest("pf_z2zeta is within two ulp over the whole range, not just small z", &
                         test_z2zeta_is_two_ulp_over_the_whole_range), &
            new_unittest("the small-redshift conversions beat log(1+z) and exp(zeta)-1", &
                         test_small_redshift_conversions_are_exact), &
            new_unittest("the z and zeta forms of the comoving distance agree to the bit", &
                         test_zeta_and_z_forms_agree_to_the_bit), &
            new_unittest("pf_z_combine is exact where the product form cancels", &
                         test_z_combine_is_exact_at_small_redshifts), &
            new_unittest("the free conversions answer at both extremes without a flag", &
                         test_free_conversions_at_the_extremes), &
            new_unittest("the age is relatively accurate where it is small", &
                         test_age_is_relative_accurate_where_it_is_small), &
            new_unittest("age and lookback time close on age(0) node by node", &
                         test_age_and_lookback_close), &
            new_unittest("a divergent age is +Infinity at every redshift, and init does not abort", &
                         test_divergent_age_is_infinity), &
            new_unittest("the three analytic universes match their closed forms", &
                         test_analytic_universes), &
            new_unittest("a curved comoving volume is accurate at small redshift", &
                         test_curved_volume_is_accurate_at_small_redshift), &
            new_unittest("every derived quantity is a closed form over D_M, not D_C", &
                         test_derived_quantities_are_closed_forms_over_dm), &
            new_unittest("D_A(z1, z2) matches the reference pairs and is signed", &
                         test_angular_diameter_distance_z1z2_matches_the_pairs), &
            new_unittest("a blueshift is answered with its sign", &
                         test_negative_redshifts_are_signed), &
            new_unittest("the inverses round trip in the distance over the whole domain", &
                         test_inverses_round_trip), &
            new_unittest("an inverse beyond the stored ceiling is NaN, and just inside is not", &
                         test_inverse_beyond_the_ceiling_is_nan), &
            new_unittest("distmod(0) is -Infinity and the angular scales are +Infinity", &
                         test_distmod_at_zero_is_minus_infinity_without_a_flag), &
            new_unittest("a NaN or an out-of-domain redshift answers NaN and raises no flag", &
                         test_nan_in_nan_out_no_flag), &
            new_unittest("every binding raises no flag over the whole domain", &
                         test_every_binding_raises_no_flag_over_the_domain), &
            new_unittest("a big-rip and a negative-ode0 model raise no flag either", &
                         test_extreme_models_raise_no_flag), &
            new_unittest("every admitted boundary value is accepted", &
                         test_init_validation_negative_controls), &
            new_unittest("describe is the documented line, field for field", &
                         test_describe_is_the_documented_line), &
            new_unittest("a name is matched without regard to case and reported canonically", &
                         test_describe_and_get_name), &
            new_unittest("a second init replaces the first, and clear unbuilds", &
                         test_second_init_replaces_the_first), &
            new_unittest("assignment clones the tables", &
                         test_assignment_clones), &
            new_unittest("the five density parameters sum to one at every redshift", &
                         test_density_parameters_sum_to_one), &
            new_unittest("each density parameter is its own term of E^2, not another", &
                         test_density_parameters_are_the_terms_of_e2), &
            new_unittest("no CMB switches neutrinos off, massive species included", &
                         test_no_cmb_switches_neutrinos_off), &
            new_unittest("w and de_density_scale are the CPL pair, exact for a constant", &
                         test_w_and_de_density_scale_are_the_cpl_pair), &
            new_unittest("critical_density is M_sun/Mpc^3 and lookback_distance is c t_L", &
                         test_critical_density_and_lookback_distance), &
            new_unittest("z_at_age round trips where age(0) minus lookback time could not", &
                         test_z_at_age_round_trips), &
            new_unittest("z_at_age outside its stored bounds, or for a divergent age, is NaN", &
                         test_z_at_age_outside_its_bounds_is_nan), &
            new_unittest("z_at_luminosity_distance and z_at_distmod round trip", &
                         test_z_at_luminosity_and_distmod_round_trip), &
            new_unittest("z_at_luminosity_distance answers forward redshifts only", &
                         test_z_at_luminosity_answers_only_forward_redshifts) &
            ]

    end subroutine collect_tests_cosmology

    ! =========================================================================================
    ! Reading the reference
    ! =========================================================================================

    !> The double a stored bit pattern holds.
    pure elemental function rv(bits) result(x)
        integer(int64), intent(in) :: bits !! the stored pattern
        real(real64)               :: x    !! the value

        x = transfer(bits, x)

    end function rv

    !> Quantity `q` of model `i` at redshift `j`.
    pure function ref(i, j, q) result(x)
        integer, intent(in) :: i !! the model
        integer, intent(in) :: j !! the redshift
        integer, intent(in) :: q !! the quantity, a `cq_*` selector
        real(real64)        :: x !! the reference value

        x = rv(crow_bits(((i - 1) * n_cz + (j - 1)) * n_cquantity + q))

    end function ref

    !> Parameter `q` of model `i`.
    pure function par(i, q) result(x)
        integer, intent(in) :: i !! the model
        integer, intent(in) :: q !! the parameter, a `cp_*` selector
        real(real64)        :: x !! the value

        x = rv(cmodel_par_bits((i - 1) * n_cparam + q))

    end function par

    !> Derived value `q` of model `i`.
    pure function der(i, q) result(x)
        integer, intent(in) :: i !! the model
        integer, intent(in) :: q !! the derived value, a `cd_*` selector
        real(real64)        :: x !! the value

        x = rv(cmodel_derived_bits((i - 1) * n_cderived + q))

    end function der

    !> Builds model `i` of the reference grid, with `zmax` if one is wanted.
    !!
    !! `ode0` and `ob0` are forwarded through UNALLOCATED allocatables where the model does not
    !! have them, which F2018 15.5.2.12 makes an absent optional argument -- the mechanism
    !! `row_validity` and the `mat_*` masks already use here. It is how one call site covers all
    !! four combinations of the two.
    subroutine build(c, i, zmax)
        type(pf_cosmology), intent(out)    :: c    !! the cosmology to build
        integer, intent(in)                :: i    !! the model's index in the grid
        real(real64), intent(in), optional :: zmax !! the table's top, if not the default

        real(real64), allocatable :: ode0_arg, ob0_arg
        real(real64), allocatable :: masses(:)

        if (.not. cmodel_flat(i)) ode0_arg = par(i, cp_ode0)
        if (cmodel_has_ob0(i)) ob0_arg = par(i, cp_ob0)
        masses = rv(cmodel_mnu_bits(cmodel_mnu_off(i):cmodel_mnu_off(i + 1) - 1))
        call c%init(h0 = par(i, cp_h0), om0 = par(i, cp_om0), ode0 = ode0_arg, &
                    tcmb0 = par(i, cp_tcmb0), neff = par(i, cp_neff), m_nu = masses, &
                    ob0 = ob0_arg, w0 = par(i, cp_w0), wa = par(i, cp_wa), &
                    name = trim(cmodel_label(i)), zmax = zmax)

    end subroutine build

    !> `got` agrees with `want` to `tol` relative, with NaN and the infinities compared as such.
    pure function agrees(got, want, tol) result(ok)
        real(real64), intent(in) :: got  !! what the library answered
        real(real64), intent(in) :: want !! the reference value
        real(real64), intent(in) :: tol  !! relative tolerance
        logical                  :: ok   !! they agree

        if (want /= want) then
            ok = got /= got
        else if (got /= got) then
            ok = .false.
        else if (.not. ieee_is_finite(want)) then
            ok = .false.
            if (.not. ieee_is_finite(got)) ok = (got > 0.0_real64) .eqv. (want > 0.0_real64)
        else if (.not. ieee_is_finite(got)) then
            ok = .false.
        else if (want == 0.0_real64) then
            ok = abs(got) <= tol
        else
            ok = abs(got - want) <= tol * abs(want)
        end if

    end function agrees

    !> Every stage-1 quantity of `c` at redshift `z`, in the reference's own order.
    function answers(c, z) result(v)
        type(pf_cosmology), intent(in) :: c              !! the cosmology
        real(real64), intent(in)       :: z              !! the redshift
        real(real64)                   :: v(n_cquantity) !! the answers

        v(cq_dc) = c%comoving_distance(z)
        v(cq_dm) = c%comoving_transverse_distance(z)
        v(cq_dl) = c%luminosity_distance(z)
        v(cq_da) = c%angular_diameter_distance(z)
        v(cq_tl) = c%lookback_time(z)
        v(cq_age) = c%age(z)
        v(cq_vc) = c%comoving_volume(z)
        v(cq_dv) = c%differential_comoving_volume(z)
        v(cq_mu) = c%distmod(z)
        v(cq_kpc_proper) = c%kpc_proper_per_arcmin(z)
        v(cq_kpc_comoving) = c%kpc_comoving_per_arcmin(z)
        v(cq_arcsec_proper) = c%arcsec_per_kpc_proper(z)
        v(cq_arcsec_comoving) = c%arcsec_per_kpc_comoving(z)
        v(cq_efunc) = c%efunc(z)
        v(cq_hubble) = c%hubble(z)
        v(cq_om) = c%om(z)
        v(cq_ode) = c%ode(z)
        v(cq_ok) = c%ok(z)
        v(cq_ogamma) = c%ogamma(z)
        v(cq_onu) = c%onu(z)
        v(cq_tcmb) = c%tcmb(z)
        v(cq_w) = c%w(z)
        v(cq_de_density_scale) = c%de_density_scale(z)
        v(cq_critical_density) = c%critical_density(z)
        v(cq_lookback_distance) = c%lookback_distance(z)

    end function answers

    !> Checks every row of the models `first` to `last` against the reference.
    subroutine check_rows(error, first, last)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        integer, intent(in)                        :: first !! the first model to check
        integer, intent(in)                        :: last  !! the last

        type(pf_cosmology) :: c
        real(real64)       :: got(n_cquantity), z
        integer            :: i, j, q, bad_i, bad_j, bad_q

        bad_i = 0
        bad_j = 0
        bad_q = 0
        do i = first, last
            call build(c, i)
            do j = 1, n_cz
                z = rv(cz_bits(j))
                got = answers(c, z)
                do q = 1, n_cquantity
                    if (.not. agrees(got(q), ref(i, j, q), TABLE_TOL)) then
                        bad_i = i
                        bad_j = j
                        bad_q = q
                    end if
                end do
            end do
        end do
        call check(error, bad_i == 0, "model " // trim(cmodel_label(max(bad_i, 1))) // &
                   " disagrees with the reference at redshift index " // itoa(bad_j) // &
                   ", quantity " // itoa(bad_q))

    end subroutine check_rows

    !> An integer as text, for a failure message.
    pure function itoa(n) result(text)
        integer, intent(in)           :: n    !! the number
        character(len=:), allocatable :: text !! its decimal text

        character(len=16) :: buf

        write (buf, '(i0)') n
        text = trim(buf)

    end function itoa

    ! =========================================================================================
    ! The reference rows
    ! =========================================================================================

    !> The eight named cosmologies, every redshift, every quantity.
    subroutine test_named_cosmologies_match_the_reference(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer :: last

        last = 0
        do while (last < n_cmodel)
            if (.not. cmodel_named(last + 1)) exit
            last = last + 1
        end do
        call check(error, last == 8, "the reference should carry eight named cosmologies")
        if (allocated(error)) return
        call check_rows(error, 1, last)

    end subroutine test_named_cosmologies_match_the_reference

    !> The user-defined grid: flat, open, closed, wCDM, w0waCDM, radiation, massive neutrinos,
    !! `neff = 0`, and the three analytic universes.
    subroutine test_parameter_form_matches_the_reference(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer :: first

        first = 1
        do while (first <= n_cmodel)
            if (.not. cmodel_named(first)) exit
            first = first + 1
        end do
        call check_rows(error, first, n_cmodel)

    end subroutine test_parameter_form_matches_the_reference

    !> `Ok0`, `Ogamma0`, `Onu0`, `T_nu0`, `D_H`, `t_H`, `age(0)` and `h`.
    subroutine test_derived_parameters_match_the_reference(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        integer            :: i

        do i = 1, n_cmodel
            call build(c, i)
            call check(error, agrees(c%ok0(), der(i, cd_ok0), 1.0e-14_real64), &
                       "ok0 of " // trim(cmodel_label(i)))
            if (allocated(error)) return
            call check(error, agrees(c%ogamma0(), der(i, cd_ogamma0), 1.0e-14_real64), &
                       "ogamma0 of " // trim(cmodel_label(i)))
            if (allocated(error)) return
            call check(error, agrees(c%onu0(), der(i, cd_onu0), 1.0e-14_real64), &
                       "onu0 of " // trim(cmodel_label(i)))
            if (allocated(error)) return
            call check(error, agrees(c%tnu0(), der(i, cd_tnu0), 1.0e-14_real64), &
                       "tnu0 of " // trim(cmodel_label(i)))
            if (allocated(error)) return
            call check(error, agrees(c%hubble_distance(), der(i, cd_dh), 1.0e-14_real64), &
                       "D_H of " // trim(cmodel_label(i)))
            if (allocated(error)) return
            call check(error, agrees(c%hubble_time(), der(i, cd_th), 1.0e-14_real64), &
                       "t_H of " // trim(cmodel_label(i)))
            if (allocated(error)) return
            call check(error, agrees(c%little_h(), der(i, cd_little_h), 1.0e-14_real64), &
                       "little_h of " // trim(cmodel_label(i)))
            if (allocated(error)) return
            ! `%odm0()` is NaN where no `ob0` was given, and `agrees` compares a NaN as one.
            call check(error, agrees(c%odm0(), der(i, cd_odm0), 1.0e-14_real64), &
                       "odm0 of " // trim(cmodel_label(i)))
            if (allocated(error)) return
            call check(error, cmodel_has_ob0(i) .eqv. (c%ob0() == c%ob0()), &
                       "ob0 must be NaN exactly where the model gives none: " // trim(cmodel_label(i)))
            if (allocated(error)) return
            call check(error, c%has_massive_nu() .eqv. cmodel_has_massive_nu(i), &
                       "has_massive_nu of " // trim(cmodel_label(i)))
            if (allocated(error)) return
            ! `%is_flat()` is `ok0 == 0` RECORDED, not "ode0 was omitted": Einstein-de Sitter
            ! is given an explicit `ode0 = 0` and is flat all the same.
            call check(error, c%is_flat() .eqv. (der(i, cd_ok0) == 0.0_real64), &
                       "is_flat of " // trim(cmodel_label(i)))
            if (allocated(error)) return
        end do

    end subroutine test_derived_parameters_match_the_reference

    !> `Ok0` set by assignment, never by `1 - Om0 - Ode0 - ...`, which lands an ulp off.
    subroutine test_flat_by_omission_is_exactly_flat(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: flat, curved

        call flat%init(h0 = 70.0_real64, om0 = 0.3_real64)
        call check(error, flat%ok0() == 0.0_real64, "ok0 must be EXACTLY zero when ode0 is omitted")
        if (allocated(error)) return
        call check(error, flat%is_flat(), "is_flat must be true when ode0 is omitted")
        if (allocated(error)) return
        call check(error, flat%ode0() == 1.0_real64 - 0.3_real64, "ode0 must be derived as 1 - om0")
        if (allocated(error)) return

        ! Supplying an ode0 an ulp off exactness gets the curved branch, which is CORRECT rather
        ! than merely tolerable: the two branches agree to the bit at that curvature.
        call curved%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = 0.7_real64)
        call check(error, .not. curved%is_flat() .or. curved%ok0() == 0.0_real64, &
                   "is_flat must be ok0 == 0 recorded, never an independent judgement")
        if (allocated(error)) return
        call check(error, abs(curved%comoving_distance(1.0_real64) &
                              - flat%comoving_distance(1.0_real64)) < 1.0e-6_real64, &
                   "a near-flat model must answer nearly the flat model's distance")

    end subroutine test_flat_by_omission_is_exactly_flat

    ! =========================================================================================
    ! The table and the fallback
    ! =========================================================================================

    !> A `zmax = 1e-6` object tabulates to `zeta_4 = 0.032` and answers everything above that on
    !! its fallback rule alone. It must agree with a full-range object over three decades.
    subroutine test_table_agrees_with_the_fallback_rule(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: full, tiny
        real(real64)       :: z, a, b
        integer            :: k

        call full%init("Planck18")
        call tiny%init("Planck18", zmax = 1.0e-6_real64)
        do k = 0, 400
            z = 0.04_real64 * (1100.0_real64 / 0.04_real64) ** (real(k, real64) / 400.0_real64)
            a = full%comoving_distance(z)
            b = tiny%comoving_distance(z)
            call check(error, agrees(b, a, 2.0e-9_real64), "D_C on the fallback at z = " // itoa(int(z)))
            if (allocated(error)) return
            a = full%lookback_time(z)
            b = tiny%lookback_time(z)
            call check(error, agrees(b, a, 2.0e-9_real64), "t_L on the fallback at z = " // itoa(int(z)))
            if (allocated(error)) return
        end do

    end subroutine test_table_agrees_with_the_fallback_rule

    !> Two objects with different `zmax` answer the same numbers, the AGE included -- the age's
    !! tail is cut at the table's edge, so it is the quantity whose construction actually differs.
    subroutine test_zmax_moves_the_seam_not_the_answer(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: small, big
        real(real64)       :: zs(4)
        integer            :: k

        zs = [0.5_real64, 3.0_real64, 50.0_real64, 900.0_real64]
        call small%init("Planck18", zmax = 5.0_real64)
        call big%init("Planck18")
        do k = 1, size(zs)
            call check(error, agrees(small%comoving_distance(zs(k)), big%comoving_distance(zs(k)), &
                                     2.0e-9_real64), "D_C across zmax at z index " // itoa(k))
            if (allocated(error)) return
            call check(error, agrees(small%lookback_time(zs(k)), big%lookback_time(zs(k)), &
                                     2.0e-9_real64), "t_L across zmax at z index " // itoa(k))
            if (allocated(error)) return
            call check(error, agrees(small%age(zs(k)), big%age(zs(k)), 2.0e-9_real64), &
                       "the AGE across zmax at z index " // itoa(k))
            if (allocated(error)) return
        end do
        call check(error, small%zmax() == 5.0_real64, "zmax() answers what the caller asked for")

    end subroutine test_zmax_moves_the_seam_not_the_answer

    !> The fallback starts from the TABULATED edge value, so the seam cannot step.
    subroutine test_seam_is_continuous(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zeta_n, lo, hi

        call c%init("Planck18", zmax = 10.0_real64)
        ! The table's top is the first multiple of the spacing at or above ln(1 + zmax).
        zeta_n = ceiling(log(11.0_real64) / 0.008_real64) * 0.008_real64
        lo = c%comoving_distance_zeta(zeta_n - 1.0e-9_real64)
        hi = c%comoving_distance_zeta(zeta_n + 1.0e-9_real64)
        call check(error, abs(hi - lo) <= 1.0e-9_real64 * abs(lo), "D_C steps at the seam")
        if (allocated(error)) return
        lo = c%lookback_time(pf_zeta2z(zeta_n - 1.0e-9_real64))
        hi = c%lookback_time(pf_zeta2z(zeta_n + 1.0e-9_real64))
        call check(error, abs(hi - lo) <= 1.0e-9_real64 * abs(lo), "t_L steps at the seam")

    end subroutine test_seam_is_continuous

    ! =========================================================================================
    ! The redshift conversions
    ! =========================================================================================

    !> `pf_z2zeta` over the WHOLE range, which is the point.
    !!
    !! A single-form `2 atanh(z/(2 + z))` is `5e-17` at `z = 1e-8` and `3.6e-9` at `z = 1e10`; a
    !! single-form `log(1 + z)` is the reverse. Each reference value here is the correctly rounded
    !! `ln(1 + z)` of the double beside it, so only an implementation that is right at both ends
    !! passes.
    subroutine test_z2zeta_is_two_ulp_over_the_whole_range(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64) :: z, zeta, want
        integer      :: k

        do k = -280, 100
            z = 10.0_real64 ** (real(k, real64) / 10.0_real64)
            if (z > 1.0e300_real64) cycle
            zeta = pf_z2zeta(z)
            ! `log1p` by its series where `z` is small, and directly where it is not: an
            ! independent route, not the implementation's own.
            want = independent_log1p(z)
            call check(error, abs(zeta - want) <= 1.0e-15_real64 * abs(want), &
                       "pf_z2zeta at decade " // itoa(k))
            if (allocated(error)) return
            if (z < 1.0_real64) then
                zeta = pf_z2zeta(-z)
                want = independent_log1p(-z)
                call check(error, abs(zeta - want) <= 1.0e-14_real64 * abs(want), &
                           "pf_z2zeta at blueshift decade " // itoa(k))
                if (allocated(error)) return
            end if
        end do

    end subroutine test_z2zeta_is_two_ulp_over_the_whole_range

    !> `ln(1 + z)` by a route the implementation does not use: the series for small `|z|`, and
    !! `log(1 + z)` where `1 + z` is exact. Deliberately slow and obviously right.
    pure function independent_log1p(z) result(v)
        real(real64), intent(in) :: z !! the argument
        real(real64)             :: v !! `ln(1 + z)`

        real(real64) :: term
        integer      :: k

        if (abs(z) > 0.25_real64) then
            v = log(1.0_real64 + z)
            return
        end if
        v = 0.0_real64
        term = 1.0_real64
        do k = 1, 60
            term = term * z
            v = v + term / real(k, real64) * merge(1.0_real64, -1.0_real64, mod(k, 2) == 1)
            ! Stop as soon as a term cannot change the sum. Running all sixty raises
            ! `IEEE_UNDERFLOW` on the way -- `z = 1e-14` reaches `1e-840` by `k = 60` -- which
            ! nagfor reports as one line at program exit attached to nothing, and which would then
            ! hide a later finding. The series has converged twenty digits earlier.
            if (abs(term) < 1.0e-20_real64 * abs(v)) exit
        end do

    end function independent_log1p

    !> The two conversions beat the naive forms where those lose digits, and round trip.
    subroutine test_small_redshift_conversions_are_exact(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64) :: z, zeta
        integer      :: k

        ! ln(1 + 1e-12) = 1e-12 - 5e-25 + ...; log(1.0 + z) answers 1e-12 exactly, 9e-5 out in
        ! the terms that matter here.
        z = 1.0e-12_real64
        zeta = pf_z2zeta(z)
        call check(error, abs(zeta - (z - 0.5_real64 * z * z)) <= 1.0e-15_real64 * z, &
                   "pf_z2zeta(1e-12) against its series")
        if (allocated(error)) return
        zeta = 1.0e-12_real64
        z = pf_zeta2z(zeta)
        call check(error, abs(z - (zeta + 0.5_real64 * zeta * zeta)) <= 1.0e-15_real64 * zeta, &
                   "pf_zeta2z(1e-12) against its series")
        if (allocated(error)) return
        ! The round trip is EXACT for every `zeta >= 0` and limited below zero by the argument,
        ! not by either conversion: a `real64` `z` pins `1 + z`, which is `e^zeta`, to `eps`
        ! ABSOLUTE, so `zeta` comes back within about `eps e^-zeta`. Asserting that bound rather
        ! than a flat number is what makes the test meaningful at both ends -- measured
        ! `1.0e-9` at `zeta = -20` against a bound of `5.6e-9`, and exactly zero at every
        ! `zeta >= 0`.
        do k = -80, 80
            zeta = 0.25_real64 * real(k, real64)
            z = pf_zeta2z(zeta)
            if (zeta == 0.0_real64) then
                call check(error, pf_z2zeta(z) == 0.0_real64, "the round trip at zeta = 0")
            else if (zeta > 0.0_real64) then
                ! 80 of the 81 points round-trip to the BIT; the one that does not is 1.5e-16
                ! out, under an ulp of its own value.
                call check(error, abs(pf_z2zeta(z) - zeta) <= 2.0_real64 * epsilon(1.0_real64) * zeta, &
                           "the round trip at zeta >= 0 must be within an ulp")
            else
                call check(error, abs(pf_z2zeta(z) - zeta) <= &
                           4.0_real64 * epsilon(1.0_real64) * exp(-zeta), &
                           "the round trip below zero must meet the representation bound")
            end if
            if (allocated(error)) return
        end do

    end subroutine test_small_redshift_conversions_are_exact

    !> `%comoving_distance(z)` is `%comoving_distance_zeta(pf_z2zeta(z))` to the BIT.
    subroutine test_zeta_and_z_forms_agree_to_the_bit(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z
        integer            :: k

        call c%init("Planck18")
        do k = -80, 100
            z = 10.0_real64 ** (real(k, real64) / 10.0_real64)
            call check(error, c%comoving_distance(z) == c%comoving_distance_zeta(pf_z2zeta(z)), &
                       "the z and zeta forms must agree to the bit at decade " // itoa(k))
            if (allocated(error)) return
        end do

    end subroutine test_zeta_and_z_forms_agree_to_the_bit

    !> `pf_z_combine` adds in `zeta`, which is what composing redshifts means.
    subroutine test_z_combine_is_exact_at_small_redshifts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64) :: z1, z2, got, want, nan, scale
        integer      :: i, j

        do i = -6, 1
            do j = -6, 1
                z1 = 10.0_real64 ** i
                z2 = 10.0_real64 ** j
                ! The tolerance is relative to the SIZE OF THE TERMS, not of their sum: with
                ! one redshift and one blueshift of the same magnitude the sum cancels to
                ! nothing, and a relative bound on it could not be met by any implementation.
                got = pf_z2zeta(pf_z_combine(z1, z2))
                want = pf_z2zeta(z1) + pf_z2zeta(z2)
                scale = abs(pf_z2zeta(z1)) + abs(pf_z2zeta(z2))
                call check(error, abs(got - want) <= 1.0e-15_real64 * scale, &
                           "pf_z_combine must add in zeta")
                if (allocated(error)) return
                ! One of them a blueshift -- only where it stays inside the domain: a `-z2`
                ! at or below -1 is not a redshift at all, and both sides are then NaN.
                if (z2 < 1.0_real64) then
                    got = pf_z2zeta(pf_z_combine(z1, -z2))
                    want = pf_z2zeta(z1) + pf_z2zeta(-z2)
                    scale = abs(pf_z2zeta(z1)) + abs(pf_z2zeta(-z2))
                    call check(error, abs(got - want) <= 1.0e-14_real64 * scale, &
                               "pf_z_combine must add in zeta for a blueshift too")
                    if (allocated(error)) return
                end if
            end do
        end do
        nan = ieee_value(nan, ieee_quiet_nan)
        got = pf_z_combine(nan, 0.5_real64)
        call check(error, got /= got, "a NaN argument must give NaN")

    end subroutine test_z_combine_is_exact_at_small_redshifts

    !> The high and low ends of both conversions, with the flags read around them.
    subroutine test_free_conversions_at_the_extremes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64) :: v, inf, nan
        logical      :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        inf = ieee_value(inf, ieee_positive_inf)
        nan = ieee_value(nan, ieee_quiet_nan)

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)

        v = pf_z2zeta(1.0e15_real64)
        v = pf_z2zeta(1.0e16_real64)
        v = pf_z2zeta(1.0e17_real64)
        v = pf_z2zeta(1.0e300_real64)
        v = pf_z2zeta(huge(1.0_real64))
        v = pf_z2zeta(inf)
        v = pf_z2zeta(nan)
        v = pf_z2zeta(-1.0_real64)
        v = pf_z2zeta(-2.0_real64)
        v = pf_zeta2z(708.0_real64)
        v = pf_zeta2z(709.0_real64)
        v = pf_zeta2z(710.0_real64)
        v = pf_zeta2z(-709.0_real64)
        v = pf_zeta2z(-710.0_real64)
        v = pf_zeta2z(-1.0e300_real64)
        v = pf_zeta2z(inf)
        v = pf_zeta2z(nan)

        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, .not. any(raised), "the free conversions must raise no IEEE flag")
        if (allocated(error)) return
        call check(error, .not. ieee_is_finite(pf_z2zeta(inf)) .and. pf_z2zeta(inf) > 0.0_real64, &
                   "pf_z2zeta(+Infinity) must be +Infinity")
        if (allocated(error)) return
        v = pf_z2zeta(-1.0_real64)
        call check(error, v /= v, "pf_z2zeta(-1) must be NaN")
        if (allocated(error)) return
        v = pf_zeta2z(710.0_real64)
        call check(error, .not. ieee_is_finite(v) .and. v > 0.0_real64, &
                   "pf_zeta2z(710) must be +Infinity, not an overflow")
        if (allocated(error)) return
        ! Below -709 the product `exp(zeta/2) sinh(zeta/2)` is `0 * Infinity`, which is NaN where
        ! the mathematics gives -1.
        call check(error, pf_zeta2z(-1.0e300_real64) == -1.0_real64, &
                   "pf_zeta2z far below zero must be exactly -1")

    end subroutine test_free_conversions_at_the_extremes

    ! =========================================================================================
    ! The age
    ! =========================================================================================

    !> The age at the TOP of the domain, where `%age(0) - %lookback_time(z)` has nothing left.
    subroutine test_age_is_relative_accurate_where_it_is_small(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        integer            :: i, j
        real(real64)       :: z, got

        do i = 1, n_cmodel
            if (cmodel_age_diverges(i)) cycle
            call build(c, i)
            do j = 1, n_cz
                z = rv(cz_bits(j))
                if (z < 100.0_real64) cycle
                got = c%age(z)
                call check(error, agrees(got, ref(i, j, cq_age), AGE_TAIL_TOL), &
                           "the age of " // trim(cmodel_label(i)) // " at redshift index " // itoa(j))
                if (allocated(error)) return
            end do
        end do

    end subroutine test_age_is_relative_accurate_where_it_is_small

    !> `%age(z) + %lookback_time(z) == %age(0)`, which the two tables are built to keep.
    subroutine test_age_and_lookback_close(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z, age0, total
        integer            :: k

        call c%init("Planck18")
        age0 = c%age(0.0_real64)
        ! ON THE NODES this is the sharp test, and it is what catches an age table built by
        ! subtracting cumulative sums: the two tables come out of the SAME interval integrals
        ! summed in opposite directions, so they close to the rounding of one sum. Measured
        ! `1.9e-14` over the whole table.
        do k = 0, 800
            z = pf_zeta2z(0.008_real64 * real(k, real64))
            total = c%age(z) + c%lookback_time(z)
            call check(error, abs(total - age0) <= 1.0e-12_real64 * age0, &
                       "age + lookback must close on age(0) at node " // itoa(k))
            if (allocated(error)) return
        end do
        ! BETWEEN the nodes the identity is only as good as the two interpolants, which is the
        ! table's own accuracy rather than the sum's: measured `1.9e-10`.
        do k = 0, 200
            z = 1.0e-3_real64 * (1000.0_real64 / 1.0e-3_real64) ** (real(k, real64) / 200.0_real64)
            total = c%age(z) + c%lookback_time(z)
            call check(error, abs(total - age0) <= 2.0e-9_real64 * age0, &
                       "age + lookback must close between the nodes too")
            if (allocated(error)) return
        end do

    end subroutine test_age_and_lookback_close

    !> de Sitter: infinitely old at every redshift, and `%init` does not abort.
    subroutine test_divergent_age_is_infinity(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zs(4), v
        integer            :: i, k
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        i = 0
        do k = 1, n_cmodel
            if (cmodel_age_diverges(k)) i = k
        end do
        call check(error, i > 0, "the reference grid must carry a divergent-age model")
        if (allocated(error)) return

        zs = [0.0_real64, 1.0_real64, 1100.0_real64, 1.0e10_real64]
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        call build(c, i)
        do k = 1, size(zs)
            v = c%age(zs(k))
        end do
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, .not. any(raised), "a divergent age must raise no IEEE flag")
        if (allocated(error)) return
        do k = 1, size(zs)
            v = c%age(zs(k))
            call check(error, .not. ieee_is_finite(v) .and. v > 0.0_real64, &
                       "a divergent age must be +Infinity at every redshift")
            if (allocated(error)) return
        end do
        ! The lookback time stays finite: it is a different integral.
        call check(error, ieee_is_finite(c%lookback_time(1.0_real64)), &
                   "the lookback time of a divergent model stays finite")

    end subroutine test_divergent_age_is_infinity

    !> Einstein-de Sitter, Milne and de Sitter against their closed forms.
    subroutine test_analytic_universes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zs(5), z, dh, th, want
        integer            :: i, k

        zs = [0.01_real64, 0.5_real64, 3.0_real64, 100.0_real64, 1100.0_real64]

        ! Einstein-de Sitter: D_C = 2 D_H (1 - 1/sqrt(1+z)), age = (2/3) t_H (1+z)^-1.5
        i = model_index("einstein_de_sitter")
        call check(error, i > 0, "einstein_de_sitter must be in the reference grid")
        if (allocated(error)) return
        call build(c, i)
        dh = c%hubble_distance()
        th = c%hubble_time()
        do k = 1, size(zs)
            z = zs(k)
            want = 2.0_real64 * dh * (1.0_real64 - 1.0_real64 / sqrt(1.0_real64 + z))
            call check(error, agrees(c%comoving_distance(z), want, TABLE_TOL), &
                       "Einstein-de Sitter D_C against its closed form")
            if (allocated(error)) return
            want = 2.0_real64 / 3.0_real64 * th * (1.0_real64 + z) ** (-1.5_real64)
            call check(error, agrees(c%age(z), want, TABLE_TOL), &
                       "Einstein-de Sitter age against its closed form")
            if (allocated(error)) return
        end do

        ! Milne: D_C = D_H ln(1+z), age = t_H/(1+z)
        i = model_index("milne")
        call check(error, i > 0, "milne must be in the reference grid")
        if (allocated(error)) return
        call build(c, i)
        dh = c%hubble_distance()
        th = c%hubble_time()
        do k = 1, size(zs)
            z = zs(k)
            call check(error, agrees(c%comoving_distance(z), dh * pf_z2zeta(z), TABLE_TOL), &
                       "Milne D_C against its closed form")
            if (allocated(error)) return
            call check(error, agrees(c%age(z), th / (1.0_real64 + z), TABLE_TOL), &
                       "Milne age against its closed form")
            if (allocated(error)) return
        end do

        ! de Sitter: D_C = D_H z, age = +Infinity
        i = model_index("de_sitter")
        call check(error, i > 0, "de_sitter must be in the reference grid")
        if (allocated(error)) return
        call build(c, i)
        dh = c%hubble_distance()
        do k = 1, size(zs)
            call check(error, agrees(c%comoving_distance(zs(k)), dh * zs(k), TABLE_TOL), &
                       "de Sitter D_C against its closed form")
            if (allocated(error)) return
        end do

    end subroutine test_analytic_universes

    !> The index of a model in the reference grid, or 0.
    pure function model_index(label) result(i)
        character(len=*), intent(in) :: label !! the model's label
        integer                      :: i     !! its index, or 0

        integer :: k

        i = 0
        do k = 1, n_cmodel
            if (trim(cmodel_label(k)) == label) i = k
        end do

    end function model_index

    ! =========================================================================================
    ! The closed forms over the tables
    ! =========================================================================================

    !> A curved `V_C` at small `z`, where the closed form keeps NOTHING.
    !!
    !! Measured in double precision for `|Ok0| = 0.1` of either sign: the closed form has no
    !! correct digit at `z = 1e-8`, is `5.6e-4` out at `1e-6` and `2.2e-7` out at `1e-4`. A FLAT
    !! fixture cannot show this: the flat branch has no subtraction in it.
    subroutine test_curved_volume_is_accurate_at_small_redshift(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z
        integer            :: i, j, k
        character(len=8)   :: labels(2)

        labels = [character(len=8) :: "open", "closed"]
        do k = 1, size(labels)
            i = model_index(trim(labels(k)))
            call check(error, i > 0, trim(labels(k)) // " must be in the reference grid")
            if (allocated(error)) return
            call build(c, i)
            do j = 1, n_cz
                z = rv(cz_bits(j))
                if (z <= 0.0_real64 .or. z > 1.0e-2_real64) cycle
                call check(error, agrees(c%comoving_volume(z), ref(i, j, cq_vc), 1.0e-9_real64), &
                           "the curved comoving volume of " // trim(labels(k)) // &
                           " at redshift index " // itoa(j))
                if (allocated(error)) return
            end do
            ! Continuity across the series/closed-form crossover, which sits where
            ! |Ok0| (D_M/D_H)^2 is 1e-3.
            do j = 1, n_cz
                z = rv(cz_bits(j))
                if (z <= 0.0_real64) cycle
                call check(error, agrees(c%comoving_volume(z), ref(i, j, cq_vc), TABLE_TOL), &
                           "the comoving volume of " // trim(labels(k)) // " across the crossover")
                if (allocated(error)) return
            end do
        end do

    end subroutine test_curved_volume_is_accurate_at_small_redshift

    !> Each derived quantity is exactly the arithmetic over `%comoving_transverse_distance`.
    !!
    !! A flat model would not see the difference: there `D_M == D_C`.
    subroutine test_derived_quantities_are_closed_forms_over_dm(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z, dm, x
        integer            :: i, k

        i = model_index("closed")
        call build(c, i)
        do k = 1, 12
            z = 0.25_real64 * real(k, real64)
            dm = c%comoving_transverse_distance(z)
            x = 1.0_real64 + z
            call check(error, agrees(c%luminosity_distance(z), exp(pf_z2zeta(z)) * dm, 1.0e-14_real64), &
                       "D_L must be (1+z) D_M")
            if (allocated(error)) return
            call check(error, agrees(c%angular_diameter_distance(z), dm / exp(pf_z2zeta(z)), &
                                     1.0e-14_real64), "D_A must be D_M/(1+z)")
            if (allocated(error)) return
            call check(error, agrees(c%differential_comoving_volume(z), &
                                     c%hubble_distance() * dm * dm / c%efunc(z), 1.0e-14_real64), &
                       "dV must be D_H D_M^2 / E")
            if (allocated(error)) return
            call check(error, agrees(c%kpc_comoving_per_arcmin(z), &
                                     1000.0_real64 * dm * (acos(-1.0_real64) / 10800.0_real64), &
                                     1.0e-14_real64), "the comoving scale must be over D_M")
            if (allocated(error)) return
            call check(error, agrees(c%distmod(z), &
                                     5.0_real64 * log10(abs(c%luminosity_distance(z))) + 25.0_real64, &
                                     1.0e-14_real64), "distmod must be over D_L")
            if (allocated(error)) return
        end do

    end subroutine test_derived_quantities_are_closed_forms_over_dm

    !> `D_A(z1, z2)` against the reference pairs, including the reversed one.
    subroutine test_angular_diameter_distance_z1z2_matches_the_pairs(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z1, z2, got
        integer            :: k, i, previous

        previous = 0
        do k = 1, n_cpair
            i = cpair_model(k)
            if (i /= previous) then
                call build(c, i)
                previous = i
            end if
            z1 = rv(cpair_z_bits(2 * k - 1))
            z2 = rv(cpair_z_bits(2 * k))
            got = c%angular_diameter_distance_z1z2(z1, z2)
            call check(error, agrees(got, rv(cpair_da_bits(k)), TABLE_TOL), &
                       "D_A(z1, z2) at pair " // itoa(k))
            if (allocated(error)) return
            if (z1 == z2) then
                call check(error, got == 0.0_real64, "D_A(z, z) must be exactly zero")
                if (allocated(error)) return
            end if
            if (z2 < z1) then
                call check(error, got < 0.0_real64, "D_A(z1, z2) must be negative when z2 < z1")
                if (allocated(error)) return
            end if
        end do

    end subroutine test_angular_diameter_distance_z1z2_matches_the_pairs

    !> A blueshift keeps its sign, and the universe is older there.
    subroutine test_negative_redshifts_are_signed(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c

        call c%init("Planck18")
        call check(error, c%comoving_distance(-0.001_real64) < 0.0_real64, &
                   "a blueshift must give a negative comoving distance")
        if (allocated(error)) return
        call check(error, c%lookback_time(-0.5_real64) < 0.0_real64, &
                   "a blueshift must give a negative lookback time")
        if (allocated(error)) return
        call check(error, c%age(-0.5_real64) > c%age(0.0_real64), &
                   "the universe must be older at a blueshift")

    end subroutine test_negative_redshifts_are_signed

    ! =========================================================================================
    ! The inverses
    ! =========================================================================================

    !> The DISTANCE round trip over the whole domain, and the redshift one where it is conditioned.
    subroutine test_inverses_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z, d, t, back
        integer            :: k

        call c%init("Planck18")
        do k = 0, 400
            ! Across the whole domain, beyond the table and below zero included.
            z = pf_zeta2z(-10.0_real64 + 33.0_real64 * real(k, real64) / 400.0_real64)
            d = c%comoving_distance(z)
            back = c%comoving_distance(c%z_at_comoving_distance(d))
            call check(error, agrees(back, d, INVERSE_TOL), &
                       "the DISTANCE round trip at step " // itoa(k))
            if (allocated(error)) return
            t = c%lookback_time(z)
            back = c%lookback_time(c%z_at_lookback_time(t))
            call check(error, agrees(back, t, merge(BLUESHIFT_INVERSE_TOL, INVERSE_TOL, &
                                                    z < 0.0_real64)), &
                       "the lookback-TIME round trip at step " // itoa(k))
            if (allocated(error)) return
        end do
        ! The REDSHIFT round trip is only as well conditioned as the forward function, which
        ! flattens like z^(-3/2): at z = 1e10 a distance carrying one ulp determines z to about
        ! one part in ten. That is the mathematics, not the implementation, so it is asserted
        ! tightly only up to z = 1e3.
        do k = 0, 200
            z = 1.0e-6_real64 * (1.0e3_real64 / 1.0e-6_real64) ** (real(k, real64) / 200.0_real64)
            back = c%z_at_comoving_distance(c%comoving_distance(z))
            call check(error, agrees(back, z, INVERSE_TOL), &
                       "the REDSHIFT round trip below z = 1e3 at step " // itoa(k))
            if (allocated(error)) return
        end do

    end subroutine test_inverses_round_trip

    !> Beyond the stored bounds an inverse is NaN; just inside them it is a number.
    subroutine test_inverse_beyond_the_ceiling_is_nan(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: ceiling_d, floor_d, ceiling_t, floor_t, v
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        ! `floor_t` is taken through a redshift on purpose: `z_at_lookback_time` answers in `z`,
        ! so the bound it screens against has to be one a `z` can reach.

        call c%init("Planck18")
        ! The ceiling IS the distance at the domain's edge, so the edge itself must answer.
        ! The FLOOR is reached through the zeta form and not through a redshift: at the blueshift
        ! end a `real64` z cannot represent its own zeta, because `1 + z` there is `1e-10` and
        ! carries only `1e-16/1e-10 = 1e-6` of relative precision. That is a property of the
        ! argument, not of this module.
        ceiling_d = c%comoving_distance(1.0e10_real64)
        floor_d = c%comoving_distance_zeta(-CEILING_ZETA)
        ceiling_t = c%lookback_time(1.0e10_real64)
        floor_t = c%lookback_time(pf_zeta2z(-CEILING_ZETA))

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        v = c%z_at_comoving_distance(ceiling_d * 1.0000001_real64)
        call check(error, v /= v, "a distance above the ceiling must be NaN")
        if (allocated(error)) return
        v = c%z_at_comoving_distance(floor_d * 1.0000001_real64)
        call check(error, v /= v, "a distance below the floor must be NaN")
        if (allocated(error)) return
        v = c%z_at_lookback_time(ceiling_t * 1.0000001_real64)
        call check(error, v /= v, "a lookback time above the ceiling must be NaN")
        if (allocated(error)) return
        v = c%z_at_lookback_time(floor_t * 1.1_real64)
        call check(error, v /= v, "a lookback time below the floor must be NaN")
        if (allocated(error)) return
        v = c%z_at_comoving_distance(ceiling_d * 0.9999999_real64)
        call check(error, v == v, "a distance just inside the ceiling must be a number")
        if (allocated(error)) return
        v = c%z_at_comoving_distance(floor_d * 0.999_real64)
        call check(error, v == v, "a distance just inside the floor must be a number")
        if (allocated(error)) return

        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)
        call check(error, .not. any(raised), "the ceiling screens must raise no IEEE flag")

    end subroutine test_inverse_beyond_the_ceiling_is_nan

    ! =========================================================================================
    ! Totality and the flags
    ! =========================================================================================

    !> `distmod(0)` is `-Infinity` and `arcsec_per_kpc_*(0)` is `+Infinity`, without a flag.
    subroutine test_distmod_at_zero_is_minus_infinity_without_a_flag(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: mu, ap, ac
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        call c%init("Planck18")
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        mu = c%distmod(0.0_real64)
        ap = c%arcsec_per_kpc_proper(0.0_real64)
        ac = c%arcsec_per_kpc_comoving(0.0_real64)
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, .not. any(raised), "distmod(0) must raise no IEEE flag")
        if (allocated(error)) return
        call check(error, .not. ieee_is_finite(mu) .and. mu < 0.0_real64, "distmod(0) must be -Infinity")
        if (allocated(error)) return
        call check(error, .not. ieee_is_finite(ap) .and. ap > 0.0_real64, &
                   "arcsec_per_kpc_proper(0) must be +Infinity")
        if (allocated(error)) return
        call check(error, .not. ieee_is_finite(ac) .and. ac > 0.0_real64, &
                   "arcsec_per_kpc_comoving(0) must be +Infinity")

    end subroutine test_distmod_at_zero_is_minus_infinity_without_a_flag

    !> A NaN, a `-1`, a catalogue's `-99` and a redshift past the ceiling: NaN, quietly.
    subroutine test_nan_in_nan_out_no_flag(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: bad(5), got(n_cquantity)
        integer            :: k, q, nbad
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        call c%init("Planck18")
        bad(1) = ieee_value(bad(1), ieee_quiet_nan)
        bad(2) = -1.0_real64
        bad(3) = -99.0_real64
        bad(4) = 1.0e11_real64
        bad(5) = -2.0_real64

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        nbad = 0
        do k = 1, size(bad)
            got = answers(c, bad(k))
            do q = 1, n_cquantity
                if (got(q) == got(q)) nbad = nbad + 1
            end do
        end do
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, nbad == 0, "every out-of-domain redshift must answer NaN")
        if (allocated(error)) return
        call check(error, .not. any(raised), "an out-of-domain redshift must raise no IEEE flag")

    end subroutine test_nan_in_nan_out_no_flag

    !> A sweep of every binding over the whole domain for six models, flags read around it.
    subroutine test_every_binding_raises_no_flag_over_the_domain(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z, got(n_cquantity), v
        integer            :: i, k
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        do i = 1, n_cmodel
            call build(c, i)
            do k = 0, 60
                z = pf_zeta2z(-23.0_real64 + 46.0_real64 * real(k, real64) / 60.0_real64)
                got = answers(c, z)
                v = c%z_at_comoving_distance(got(cq_dc))
                v = c%z_at_lookback_time(got(cq_tl))
                ! The stage-two inverses are fed their own forward answers, infinities included:
                ! a divergent age and a big rip both arrive here.
                v = c%z_at_age(got(cq_age))
                v = c%z_at_luminosity_distance(got(cq_dl))
                v = c%z_at_distmod(got(cq_mu))
            end do
        end do
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, .not. any(raised), "a sweep of every binding must raise no IEEE flag")

    end subroutine test_every_binding_raises_no_flag_over_the_domain

    !> A big-rip model and a negative-`ode0` one: the two screens of section 4.8.
    subroutine test_extreme_models_raise_no_flag(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: rip, neg
        real(real64)       :: z, got(n_cquantity)
        integer            :: k
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        call rip%init(h0 = 70.0_real64, om0 = 0.3_real64, w0 = -1.0_real64, wa = 3.0_real64)
        call neg%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = -0.5_real64)

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        do k = 0, 80
            z = pf_zeta2z(-23.0_real64 + 46.0_real64 * real(k, real64) / 80.0_real64)
            got = answers(rip, z)
            got = answers(neg, z)
        end do
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, .not. any(raised), "an extreme model must raise no IEEE flag")

    end subroutine test_extreme_models_raise_no_flag

    !> Every admitted boundary value is accepted: the negative control for section 9.4's refusals.
    subroutine test_init_validation_negative_controls(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c

        call c%init(h0 = 70.0_real64, om0 = 0.0_real64)
        call check(error, c%is_initialised(), "om0 = 0 must be admitted")
        if (allocated(error)) return
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, tcmb0 = 0.0_real64)
        call check(error, c%is_initialised(), "tcmb0 = 0 must be admitted")
        if (allocated(error)) return
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, neff = 0.0_real64)
        call check(error, c%is_initialised(), "neff = 0 with m_nu absent must be admitted")
        if (allocated(error)) return
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, neff = 0.0_real64, m_nu = [real(real64) ::])
        call check(error, c%is_initialised(), "neff = 0 with a zero-length m_nu must be admitted")
        if (allocated(error)) return
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, w0 = -3.0_real64)
        call check(error, c%is_initialised(), "w0 = -3 must be admitted")
        if (allocated(error)) return
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, w0 = 3.0_real64, wa = -3.0_real64)
        call check(error, c%is_initialised(), "w0 = 3, wa = -3 must be admitted")
        if (allocated(error)) return
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, zmax = 1.0e10_real64)
        call check(error, c%is_initialised(), "zmax = 1e10 must be admitted")
        if (allocated(error)) return
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, ob0 = 0.3_real64)
        call check(error, c%is_initialised(), "ob0 = om0 must be admitted")
        if (allocated(error)) return
        call c%init(h0 = 1.0e-10_real64, om0 = 0.3_real64)
        call check(error, c%is_initialised(), "h0 = 1e-10 must be admitted")
        if (allocated(error)) return
        call c%init(h0 = 1.0e10_real64, om0 = 0.3_real64)
        call check(error, c%is_initialised(), "h0 = 1e10 must be admitted")
        if (allocated(error)) return
        ! Every density at exactly the ceiling.
        call c%init(h0 = 70.0_real64, om0 = 1.0e6_real64, ode0 = -1.0e6_real64 + 1.0_real64)
        call check(error, c%is_initialised(), "a density at exactly 1e6 must be admitted")

    end subroutine test_init_validation_negative_controls

    ! =========================================================================================
    ! Text and lifetime
    ! =========================================================================================

    !> `%describe`'s whole line, against a literal.
    subroutine test_describe_is_the_documented_line(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology)            :: c
        character(len=:), allocatable :: text

        call c%init("Planck18")
        call c%describe(text)
        call check(error, index(text, "Planck18; H0 = ") == 1, &
                   "describe must open with the name and H0; got: " // text)
        if (allocated(error)) return
        call check(error, index(text, "; Ok0 = 0 (flat);") > 0, &
                   "a flat model's Ok0 must read 0 (flat); got: " // text)
        if (allocated(error)) return
        call check(error, index(text, " eV;") > 0, "the masses must carry their unit; got: " // text)
        if (allocated(error)) return
        call check(error, index(text, "w0 =") == 0 .and. index(text, "wa =") == 0, &
                   "a cosmological constant must omit the w0/wa pair; got: " // text)
        if (allocated(error)) return
        call check(error, index(text, "; Ob0 = ") > 0, "Ob0 must be shown; got: " // text)
        if (allocated(error)) return

        ! A curved w0waCDM with no ob0: the pair appears, Ob0 reads unknown, m_nu reads none.
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = 0.6_real64, neff = 0.0_real64, &
                    w0 = -0.9_real64, wa = 0.3_real64, name = "curved")
        call c%describe(text)
        call check(error, index(text, "curved; H0 = ") == 1, "describe must open with the label")
        if (allocated(error)) return
        call check(error, index(text, "; Ok0 = 0 (flat)") == 0, &
                   "a curved model must not read flat; got: " // text)
        if (allocated(error)) return
        call check(error, index(text, "; m_nu = none;") > 0, &
                   "no species must read none; got: " // text)
        if (allocated(error)) return
        call check(error, index(text, "; Ob0 = unknown") > 0, &
                   "an absent ob0 must read unknown; got: " // text)
        if (allocated(error)) return
        call check(error, index(text, "; w0 = ") > 0 .and. index(text, "; wa = ") > 0, &
                   "a w0waCDM model must show the pair; got: " // text)

    end subroutine test_describe_is_the_documented_line

    !> A token is matched without regard to case; the canonical spelling comes back.
    subroutine test_describe_and_get_name(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology)            :: c
        character(len=:), allocatable :: name

        call c%init("planck18")
        call c%get_name(name)
        call check(error, name == "Planck18", "a lower-case token must give the canonical name")
        if (allocated(error)) return
        call c%init("PLANCK18")
        call c%get_name(name)
        call check(error, name == "Planck18", "an upper-case token must give the canonical name")
        if (allocated(error)) return
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64)
        call c%get_name(name)
        call check(error, name == "custom", "a parameter-form object with no name must read custom")
        if (allocated(error)) return
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, name = "my_sim")
        call c%get_name(name)
        call check(error, name == "my_sim", "a caller's label must come back")

    end subroutine test_describe_and_get_name

    !> A second `%init` replaces the object; `%clear` unbuilds it.
    subroutine test_second_init_replaces_the_first(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology)            :: c
        character(len=:), allocatable :: name
        real(real64)                  :: planck_d, wmap_d

        call c%init("Planck18")
        planck_d = c%comoving_distance(1.0_real64)
        call c%init("WMAP9")
        wmap_d = c%comoving_distance(1.0_real64)
        call c%get_name(name)
        call check(error, name == "WMAP9", "a second init must replace the name")
        if (allocated(error)) return
        call check(error, wmap_d /= planck_d, "a second init must replace the tables")
        if (allocated(error)) return
        call check(error, agrees(wmap_d, ref(model_index("WMAP9"), z_index(1.0_real64), cq_dc), &
                                 TABLE_TOL), "the rebuilt object must answer WMAP9's row")
        if (allocated(error)) return
        call c%clear()
        call check(error, .not. c%is_initialised(), "clear must unbuild the object")
        if (allocated(error)) return
        call c%clear()
        call check(error, .not. c%is_initialised(), "clear must be harmless twice")

    end subroutine test_second_init_replaces_the_first

    !> The index of a redshift in the reference grid, or 0.
    pure function z_index(z) result(j)
        real(real64), intent(in) :: z !! the redshift
        integer                  :: j !! its index, or 0

        integer :: k

        j = 0
        do k = 1, n_cz
            if (rv(cz_bits(k)) == z) j = k
        end do

    end function z_index

    !> Intrinsic assignment deep-copies the tables.
    subroutine test_assignment_clones(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c1, c2
        real(real64)       :: before, after

        call c1%init("Planck18")
        c2 = c1
        before = c2%comoving_distance(1.0_real64)
        call c1%clear()
        call check(error, c2%is_initialised(), "the clone must survive the original's clear")
        if (allocated(error)) return
        after = c2%comoving_distance(1.0_real64)
        call check(error, after == before, "the clone must answer the same number afterwards")

    end subroutine test_assignment_clones

    ! =========================================================================================
    ! What the universe is made of at z
    ! =========================================================================================

    !> `Om + Ode + Ok + Ogamma + Onu` is one, at every redshift of every model.
    !!
    !! The identity holds because each is a TERM of `E^2` over `E^2`, formed at the same
    !! `x = e^zeta`. A binding that recomputed `1 + z` its own way, or that counted the neutrinos
    !! twice, would break it while still looking plausible one parameter at a time.
    subroutine test_density_parameters_sum_to_one(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z, total, e, worst
        integer            :: i, k, bad_i

        worst = 0.0_real64
        bad_i = 0
        do i = 1, n_cmodel
            call build(c, i)
            do k = 0, 60
                z = pf_zeta2z(-22.0_real64 + 45.0_real64 * real(k, real64) / 60.0_real64)
                total = c%om(z) + c%ode(z) + c%ok(z) + c%ogamma(z) + c%onu(z)
                e = c%efunc(z)
                if (e /= e) cycle
                if (.not. ieee_is_finite(e)) then
                    ! A big rip: dark energy is ALL of `E^2`, and the limit is exact.
                    if (total /= 1.0_real64) bad_i = i
                else if (abs(total - 1.0_real64) > DENSITY_SUM_TOL) then
                    bad_i = i
                    worst = max(worst, abs(total - 1.0_real64))
                end if
            end do
        end do
        call check(error, bad_i == 0, "the five density parameters must sum to one; " // &
                   trim(cmodel_label(max(bad_i, 1))) // " is the model that failed")

    end subroutine test_density_parameters_sum_to_one

    !> Each density parameter times `E^2` is the term of `E^2` it stands for, and no other.
    !!
    !! A wrong exponent -- `x^3` where the photons need `x^4` -- sums to one all the same once the
    !! others absorb it, so the sum above cannot see it; this can. `%ode` is checked through
    !! `%de_density_scale`, which ties the CPL factor to the parameter it scales.
    subroutine test_density_parameters_are_the_terms_of_e2(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z, x, e2
        integer            :: i, k

        do i = 1, n_cmodel
            call build(c, i)
            do k = 0, 12
                z = pf_zeta2z(-10.0_real64 + 20.0_real64 * real(k, real64) / 12.0_real64)
                x = 1.0_real64 + z
                e2 = c%efunc(z) ** 2
                if (e2 /= e2 .or. .not. ieee_is_finite(e2)) cycle
                call check(error, agrees(c%om(z) * e2, c%om0() * x ** 3, 1.0e-12_real64), &
                           "Om(z) E^2 must be Om0 (1+z)^3: " // trim(cmodel_label(i)))
                if (allocated(error)) return
                call check(error, agrees(c%ok(z) * e2, c%ok0() * x ** 2, 1.0e-12_real64), &
                           "Ok(z) E^2 must be Ok0 (1+z)^2: " // trim(cmodel_label(i)))
                if (allocated(error)) return
                call check(error, agrees(c%ogamma(z) * e2, c%ogamma0() * x ** 4, 1.0e-12_real64), &
                           "Ogamma(z) E^2 must be Ogamma0 (1+z)^4: " // trim(cmodel_label(i)))
                if (allocated(error)) return
                call check(error, agrees(c%ode(z) * e2, c%ode0() * c%de_density_scale(z), &
                                         1.0e-12_real64), &
                           "Ode(z) E^2 must be Ode0 f_DE(z): " // trim(cmodel_label(i)))
                if (allocated(error)) return
                ! The neutrinos ride on the photons, which is why they are not counted in `Om0` --
                ! and they vanish with the photons, or with `Neff`, which `neff_zero` has at zero
                ! while carrying a CMB.
                if (c%ogamma(z) > 0.0_real64 .and. c%neff() > 0.0_real64) then
                    call check(error, c%onu(z) > 0.0_real64, &
                               "Onu must be positive where Ogamma and Neff both are: " // &
                               trim(cmodel_label(i)))
                else
                    call check(error, c%onu(z) == 0.0_real64, &
                               "Onu must be exactly zero without photons or without Neff: " // &
                               trim(cmodel_label(i)))
                end if
                if (allocated(error)) return
            end do
        end do

    end subroutine test_density_parameters_are_the_terms_of_e2

    !> `Tcmb0 = 0` switches radiation AND neutrinos off, a MASSIVE species included.
    !!
    !! The discriminating call is `%init(h0, om0, m_nu=[0, 0, 0.06])` with `tcmb0` left at its
    !! default of zero, which astropy accepts and answers with no radiation at all. Without the
    !! first arm of `cosmology_nu_rel` it divides by a zero neutrino temperature: that raises
    !! `IEEE_DIVIDE_BY_ZERO`, then `Onu0 = Ogamma0 * Infinity` is `0 * Infinity`, and `%init`
    !! reports the resulting NaN as an out-of-range density -- so the unfixed module ABORTS here
    !! rather than failing this assertion, and under nagfor's `-ieee=stop` it traps at the
    !! division. Both are visible; neither is a wrong answer left standing.
    subroutine test_no_cmb_switches_neutrinos_off(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: masses(3), z
        integer            :: k
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        masses = [0.0_real64, 0.0_real64, 0.06_real64]

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, m_nu = masses)
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, .not. any(raised), &
                   "a massive m_nu with no CMB must raise no IEEE flag at init")
        if (allocated(error)) return
        call check(error, c%is_initialised(), "such a cosmology must build")
        if (allocated(error)) return
        call check(error, c%tnu0() == 0.0_real64 .and. c%ogamma0() == 0.0_real64 &
                   .and. c%onu0() == 0.0_real64, "no CMB means no radiation and no neutrinos")
        if (allocated(error)) return
        ! Flat by omission is still exact, which a NaN `Onu0` would have destroyed.
        call check(error, c%ode0() == 1.0_real64 - 0.3_real64 .and. c%ok0() == 0.0_real64, &
                   "Ode0 must be 1 - Om0 exactly when there is no radiation to subtract")
        if (allocated(error)) return
        do k = 0, 6
            z = pf_zeta2z(-5.0_real64 + 20.0_real64 * real(k, real64) / 6.0_real64)
            call check(error, c%ogamma(z) == 0.0_real64 .and. c%onu(z) == 0.0_real64 &
                       .and. c%tcmb(z) == 0.0_real64, &
                       "Ogamma, Onu and Tcmb must be exactly zero at every z without a CMB")
            if (allocated(error)) return
        end do
        ! The masses the caller gave are still reported, and so is the flag about them.
        call check(error, c%has_massive_nu(), "the masses given are still reported as massive")

    end subroutine test_no_cmb_switches_neutrinos_off

    !> `%w` and `%de_density_scale`: exact for a cosmological constant, CPL otherwise.
    subroutine test_w_and_de_density_scale_are_the_cpl_pair(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: lam, cpl
        real(real64)       :: z, want
        integer            :: k

        call lam%init("Planck18")
        call cpl%init(h0 = 70.0_real64, om0 = 0.3_real64, w0 = -0.9_real64, wa = 0.3_real64)

        do k = 0, 20
            z = pf_zeta2z(-15.0_real64 + 35.0_real64 * real(k, real64) / 20.0_real64)
            ! A cosmological constant is EXACT, not merely close: no arithmetic is done on it.
            call check(error, lam%w(z) == -1.0_real64, "w(z) must be exactly -1 for a constant")
            if (allocated(error)) return
            call check(error, lam%de_density_scale(z) == 1.0_real64, &
                       "f_DE must be exactly 1 for a cosmological constant")
            if (allocated(error)) return
            ! `w0 + wa (1 - 1/(1+z))` is the same function written the other way round, so a
            ! wrong sign or a missing factor shows and a last-bit difference does not.
            want = -0.9_real64 + 0.3_real64 * (1.0_real64 - 1.0_real64 / (1.0_real64 + z))
            call check(error, agrees(cpl%w(z), want, 1.0e-11_real64), "w(z) must be the CPL form")
            if (allocated(error)) return
        end do
        call check(error, cpl%w(0.0_real64) == -0.9_real64, "w(0) must be exactly w0")
        if (allocated(error)) return
        call check(error, cpl%de_density_scale(0.0_real64) == 1.0_real64, &
                   "f_DE(0) must be exactly 1")
        if (allocated(error)) return
        ! As `z -> infinity` the CPL equation of state tends to `w0 + wa`.
        call check(error, abs(cpl%w(1.0e10_real64) - (-0.6_real64)) < 1.0e-9_real64, &
                   "w must tend to w0 + wa at large z")

    end subroutine test_w_and_de_density_scale_are_the_cpl_pair

    !> `%critical_density` in M_sun/Mpc^3, and `%lookback_distance` as `c` times the lookback time.
    subroutine test_critical_density_and_lookback_distance(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        ! The textbook critical density, `2.7754e11 h^2 M_sun/Mpc^3`, carries about four digits and
        ! the constants behind it are not quite this module's; it is here as an order-of-magnitude
        ! anchor on the UNIT, which the reference rows then pin to thirty digits.
        real(real64), parameter :: RHO_CRIT_TEXTBOOK = 2.7758e11_real64
        ! `c t_L` by the physical route rather than through `D_H / t_H`: km/s times Gyr in seconds
        ! over km in a Mpc. The library forms it as `D_H t_L / t_H`, in which `H0` cancels.
        real(real64), parameter :: MPC_PER_GYR = 299792.458_real64 * 3.15576e16_real64 &
                                                 / 3.0856775814913673e19_real64

        type(pf_cosmology) :: c
        real(real64)       :: z, rho0
        integer            :: i, k

        call c%init("Planck18")
        rho0 = c%critical_density(0.0_real64)
        call check(error, abs(rho0 - RHO_CRIT_TEXTBOOK * c%little_h() ** 2) &
                   < 1.0e-3_real64 * rho0, &
                   "critical_density(0) must be the textbook 2.7758e11 h^2 M_sun/Mpc^3")
        if (allocated(error)) return

        do i = 1, n_cmodel
            call build(c, i)
            rho0 = c%critical_density(0.0_real64)
            do k = 0, 12
                z = pf_zeta2z(-10.0_real64 + 25.0_real64 * real(k, real64) / 12.0_real64)
                if (.not. ieee_is_finite(c%efunc(z))) cycle
                call check(error, agrees(c%critical_density(z), rho0 * c%efunc(z) ** 2, &
                                         1.0e-13_real64), &
                           "critical_density must scale as E^2: " // trim(cmodel_label(i)))
                if (allocated(error)) return
                call check(error, agrees(c%lookback_distance(z), MPC_PER_GYR * c%lookback_time(z), &
                                         1.0e-13_real64), &
                           "lookback_distance must be c times the lookback time: " // &
                           trim(cmodel_label(i)))
                if (allocated(error)) return
            end do
        end do

    end subroutine test_critical_density_and_lookback_distance

    ! =========================================================================================
    ! The stage-two inverses
    ! =========================================================================================

    !> `%z_at_age` over every model, asserted in the AGE, where it is conditioned both ways.
    !!
    !! The discriminating point is the TOP of the domain. At `z = 1e10` the age is `7.6e-18 Gyr`
    !! beside an `age(0)` of `13.8`, so an inverse written as `%z_at_lookback_time(%age(0) - t)`
    !! answers whatever redshift the saturated lookback time happens to name; solving on
    !! `ln(age)` recovers `1e10` to fifteen digits.
    subroutine test_z_at_age_round_trips(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z, t, back, tol
        integer            :: i, k, bad_i, bad_k

        bad_i = 0
        bad_k = 0
        do i = 1, n_cmodel
            if (cmodel_age_diverges(i)) cycle
            call build(c, i)
            do k = 0, 60
                z = pf_zeta2z(-22.0_real64 + 45.0_real64 * real(k, real64) / 60.0_real64)
                t = c%age(z)
                back = c%z_at_age(t)
                if (z >= 0.0_real64) then
                    tol = STAGE2_INVERSE_TOL
                else
                    tol = AGE_BLUESHIFT_TOL
                end if
                if (.not. agrees(c%age(back), t, tol)) then
                    bad_i = i
                    bad_k = k
                end if
                ! Where the forward function is well conditioned, the REDSHIFT comes back too.
                if (z > 1.0e-3_real64 .and. z < 1.0e3_real64) then
                    if (.not. agrees(back, z, STAGE2_INVERSE_TOL)) then
                        bad_i = i
                        bad_k = k
                    end if
                end if
            end do
        end do
        call check(error, bad_i == 0, "z_at_age must round trip; " // &
                   trim(cmodel_label(max(bad_i, 1))) // " failed at sweep index " // itoa(bad_k))
        if (allocated(error)) return

        call c%init("Planck18")
        t = c%age(1.0e10_real64)
        call check(error, t > 0.0_real64 .and. t < 1.0e-16_real64, &
                   "the age at the top of the domain should be of order 1e-17 Gyr")
        if (allocated(error)) return
        call check(error, abs(c%z_at_age(t) - 1.0e10_real64) < 1.0e-4_real64 * 1.0e10_real64, &
                   "z_at_age must recover the top of the domain, where age(0) - t cannot")
        if (allocated(error)) return
        ! The difference form, spelled out, to show what this test is protecting against.
        call check(error, c%age(0.0_real64) - t == c%age(0.0_real64), &
                   "age(0) - age(1e10) must be age(0) to the bit, which is why it is never used")

    end subroutine test_z_at_age_round_trips

    !> Beyond the stored age bounds, and for a model with no finite age, `%z_at_age` is NaN.
    subroutine test_z_at_age_outside_its_bounds_is_nan(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c, ds
        real(real64)       :: nan, small, big
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        call c%init("Planck18")
        call ds%init(h0 = 70.0_real64, om0 = 0.0_real64, ode0 = 1.0_real64)
        nan = ieee_value(nan, ieee_quiet_nan)
        small = c%age(1.0e10_real64)
        big = c%age(pf_zeta2z(-CEILING_ZETA))

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)

        call check(error, c%z_at_age(nan) /= c%z_at_age(nan), "a NaN age must answer NaN")
        if (.not. allocated(error)) then
            call check(error, c%z_at_age(0.0_real64) /= c%z_at_age(0.0_real64), &
                       "a zero age must answer NaN")
        end if
        if (.not. allocated(error)) then
            call check(error, c%z_at_age(-1.0_real64) /= c%z_at_age(-1.0_real64), &
                       "a negative age must answer NaN")
        end if
        if (.not. allocated(error)) then
            call check(error, c%z_at_age(0.5_real64 * small) /= c%z_at_age(0.5_real64 * small), &
                       "an age below the smallest the domain attains must answer NaN")
        end if
        if (.not. allocated(error)) then
            call check(error, c%z_at_age(2.0_real64 * big) /= c%z_at_age(2.0_real64 * big), &
                       "an age above the largest the domain attains must answer NaN")
        end if
        ! ... and just inside those bounds it is a number, which is the negative control.
        if (.not. allocated(error)) then
            call check(error, c%z_at_age(small) == c%z_at_age(small), &
                       "the smallest age the domain attains must answer a number")
        end if
        ! A de Sitter universe is infinitely old at every redshift, so no age names one.
        if (.not. allocated(error)) then
            call check(error, ds%z_at_age(1.0_real64) /= ds%z_at_age(1.0_real64), &
                       "a divergent age must answer NaN at every t")
        end if

        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)
        if (allocated(error)) return
        call check(error, .not. any(raised), "a refused age must raise no IEEE flag")

    end subroutine test_z_at_age_outside_its_bounds_is_nan

    !> `%z_at_luminosity_distance` and `%z_at_distmod`, asserted in `D_L` and in `mu`.
    !!
    !! The closed models in the grid go through the panel walk rather than the monotone bracket,
    !! so both paths are covered. The round trip is asserted in the QUANTITY, as stage one's are:
    !! at the top of the domain a `D_L` carrying one ulp fixes the redshift only loosely, and that
    !! is the mathematics rather than the iteration.
    subroutine test_z_at_luminosity_and_distmod_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z, d, mu, back
        integer            :: i, k, bad_i, bad_k

        bad_i = 0
        bad_k = 0
        do i = 1, n_cmodel
            call build(c, i)
            do k = 0, 50
                z = pf_zeta2z(45.0_real64 * real(k, real64) / 50.0_real64 * 0.5_real64)
                d = c%luminosity_distance(z)
                if (.not. (d > 0.0_real64)) cycle
                back = c%z_at_luminosity_distance(d)
                if (.not. agrees(c%luminosity_distance(back), d, STAGE2_INVERSE_TOL)) then
                    bad_i = i
                    bad_k = k
                end if
                mu = c%distmod(z)
                back = c%z_at_distmod(mu)
                if (.not. agrees(c%distmod(back), mu, STAGE2_INVERSE_TOL)) then
                    bad_i = i
                    bad_k = k
                end if
            end do
        end do
        call check(error, bad_i == 0, "the luminosity-distance inverses must round trip; " // &
                   trim(cmodel_label(max(bad_i, 1))) // " failed at sweep index " // itoa(bad_k))
        if (allocated(error)) return

        ! The redshift itself, where the forward function is well conditioned.
        call c%init("Planck18")
        do k = 1, 5
            z = 0.1_real64 * real(k, real64)
            call check(error, agrees(c%z_at_luminosity_distance(c%luminosity_distance(z)), z, &
                                     1.0e-12_real64), "z must come back from its D_L")
            if (allocated(error)) return
            call check(error, agrees(c%z_at_distmod(c%distmod(z)), z, 1.0e-12_real64), &
                       "z must come back from its distance modulus")
            if (allocated(error)) return
        end do

    end subroutine test_z_at_luminosity_and_distmod_round_trip

    !> A negative `D_L`, one past the ceiling, and a distance modulus that would overflow.
    subroutine test_z_at_luminosity_answers_only_forward_redshifts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: nan, top
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        call c%init("Planck18")
        nan = ieee_value(nan, ieee_quiet_nan)
        top = c%luminosity_distance(1.0e10_real64)

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)

        call check(error, c%z_at_luminosity_distance(nan) /= c%z_at_luminosity_distance(nan), &
                   "a NaN D_L must answer NaN")
        if (.not. allocated(error)) then
            ! `D_L` is negative at a blueshift and returns to zero as z -> -1, so a negative
            ! value names two redshifts or none; this binding answers z >= 0 only.
            call check(error, c%z_at_luminosity_distance(-1.0_real64) &
                       /= c%z_at_luminosity_distance(-1.0_real64), &
                       "a negative D_L must answer NaN, not the blueshift that shares it")
        end if
        if (.not. allocated(error)) then
            call check(error, c%z_at_luminosity_distance(0.0_real64) == 0.0_real64, &
                       "a zero D_L must answer z = 0 exactly")
        end if
        if (.not. allocated(error)) then
            call check(error, c%z_at_luminosity_distance(2.0_real64 * top) &
                       /= c%z_at_luminosity_distance(2.0_real64 * top), &
                       "a D_L past the domain's ceiling must answer NaN")
        end if
        if (.not. allocated(error)) then
            call check(error, c%z_at_luminosity_distance(top) == c%z_at_luminosity_distance(top), &
                       "the D_L at the ceiling itself must answer a number")
        end if
        if (.not. allocated(error)) then
            call check(error, c%z_at_distmod(nan) /= c%z_at_distmod(nan), &
                       "a NaN distance modulus must answer NaN")
        end if
        if (.not. allocated(error)) then
            ! `10^((mu - 25)/5)` would overflow well before this.
            call check(error, c%z_at_distmod(1.0e6_real64) /= c%z_at_distmod(1.0e6_real64), &
                       "a distance modulus whose D_L would overflow must answer NaN")
        end if
        if (.not. allocated(error)) then
            call check(error, c%z_at_distmod(-1.0e6_real64) == 0.0_real64, &
                       "a distance modulus whose D_L underflows must answer z = 0")
        end if

        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)
        if (allocated(error)) return
        call check(error, .not. any(raised), "a refused luminosity distance must raise no flag")

    end subroutine test_z_at_luminosity_answers_only_forward_redshifts

#ifndef __flang__
    !> Can overflow, invalid and divide-by-zero all be held off around a call?
    !!
    !! nagfor halts on the three by default, so a regression that raised one would take the whole
    !! runner down before the test could assert anything. **The hold-off bracket is written out in
    !! each test's own body, and this inquiry is the only part a helper may carry**: F2018 17.3
    !! restores the halting modes on return from any procedure other than `ieee_set_halting_mode`,
    !! and quietens a flag signalling on entry until the procedure returns, so a helper that set
    !! the modes or read the flags would change and see nothing (`fortran-gotchas.md`).
    !! `ieee_set_halting_mode` does not link under flang on macOS, which the guard is for.
    function traps_can_be_held() result(can)
        logical :: can !! `ieee_support_halting` holds for overflow, invalid and divide-by-zero

        can = ieee_support_halting(ieee_overflow) .and. ieee_support_halting(ieee_invalid) &
            .and. ieee_support_halting(ieee_divide_by_zero)

    end function traps_can_be_held
#endif

end module test_cosmology
