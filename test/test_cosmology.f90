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
!! the table a scaled quintic Hermite over a grid of `h = 0.008` in `zeta` -- a value and an
!! analytic slope at every node -- carries a few parts in `1e-15` over the whole of `z` in
!! `[1e-8, 100]`; beyond it the fixed 20-point rule is exact to rounding but starts from the
!! tabulated edge value, so it INHERITS that error rather than improving on it. The age past the
!! table is the one quantity that starts from nothing tabulated, and it is the one asserted
!! tightly.
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
    !! Measured, not chosen, and set at ten times the worst: over all 22 models, all 25 redshifts
    !! and all 25 quantities the worst residual is `6.5e-14` under gfortran and `8.0e-14` under
    !! ifx, both on the comoving volume of a curved model, where the distance's own error is
    !! cubed. The two largest contributors are not the interpolant: the first interval above
    !! `z = 0` of a model whose blueshift half is NOT tabulated (its domain ends before the floor
    !! the caller asked for), where the quintic's stencil is one-sided, and the closed forms at
    !! `z = -0.99`, where `1 + z` carries two decimal digits fewer than `z` does.
    real(real64), parameter :: TABLE_TOL = 1.0e-12_real64
    !> What the age past the table may differ by: it starts from nothing tabulated.
    !!
    !! Measured at ten times the worst, as `TABLE_TOL` is: `3.26e-15` under gfortran and `3.26e-15`
    !! under ifx, both at `z = 1e10`, where the age is `1e-17 Gyr` beside an `age(0)` of about ten
    !! -- which is exactly why this is asserted on its own rather than folded into the table's
    !! tolerance.
    real(real64), parameter :: AGE_TAIL_TOL = 4.0e-14_real64
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
    !> What `%growth_factor` may differ from the 30-digit ODE oracle by, RELATIVELY.
    !!
    !! Measured, not chosen, and set at ten times the worst, as `TABLE_TOL` is: over all 22
    !! models and all 25 redshifts the worst is `4.4e-11` under gfortran and `4.4e-11` under ifx,
    !! both on the recollapsing model at `z = -0.5`, which is `0.6` below its table's bottom and
    !! `0.4` above its own floor -- the deepest the fallback walk runs anywhere in the grid. Every
    !! other model is at `3.9e-12` or better, which is the `PFC_H/2` integrator's own `2.5e-12`.
    real(real64), parameter :: GROWTH_TOL = 4.0e-10_real64
    !> What `%growth_rate` may differ from the same oracle by, ABSOLUTELY.
    !!
    !! **`f` is a rate between 0 and 1 whose accuracy is absolute, and saying so is more honest
    !! than a relative bound that would have to be loose enough to cover the places it is tiny.**
    !! At `z = 1e10` it is `5e-7` -- the Meszaros mode's `(3/2)a/a_eq` -- and a relative error of
    !! `2.6e-7` there is an absolute `1.3e-13`; at `z = -0.99` of the CPL model it is `6e-23`, the
    !! residue of `exp(-INT q dzeta)` over an integral of about 50, where the 30-digit oracle
    !! itself moves in the fourth digit between two macro-steps. Measured worst ABSOLUTE residual
    !! over the whole grid: `2.0e-10` under gfortran and `2.0e-10` under ifx, again the
    !! recollapsing model's walk; every other model is at `1.7e-11` or better.
    real(real64), parameter :: GROWTH_RATE_TOL = 2.0e-9_real64
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
            new_unittest("zmin moves the bottom seam and not the answer, with a node at zeta = 0", &
                         test_zmin_moves_the_seam_not_the_answer), &
            new_unittest("the seam at the table's bottom edge is continuous", &
                         test_bottom_seam_is_continuous), &
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
            new_unittest("the blueshift floor is the root of the model's own E squared", &
                         test_zeta_floor_is_the_root_of_e2), &
            new_unittest("a model whose domain ends early inverts to the oracle, or to NaN", &
                         test_truncated_domain_inverses_match_the_oracle), &
            new_unittest("the tight luminosity bracket answers what the whole-domain one does", &
                         test_luminosity_bracket_is_an_identity), &
            new_unittest("the closed forms and the table share one domain", &
                         test_closed_forms_share_the_table_domain), &
            new_unittest("the build's tabulated neutrino fit changes no answer", &
                         test_tabulated_neutrino_fit_changes_no_answer), &
            new_unittest("the neutrino species sum to Onu, and Otot is exactly one when flat", &
                         test_species_split_and_otot), &
            new_unittest("clone copies what it is not given, and keeps flat models flat", &
                         test_clone_copies_and_overrides), &
            new_unittest("ten models by every binding by twenty-three edge inputs raise no flag", &
                         test_every_binding_and_model_raises_no_flag), &
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
                         test_z_at_luminosity_answers_only_forward_redshifts), &
            new_unittest("the Einstein-de Sitter growth is exactly 1/(1+z) and its rate exactly 1", &
                         test_einstein_de_sitter_growth_is_exact), &
            new_unittest("D(0) is exactly one and f(0) is the generated row", &
                         test_growth_is_anchored_at_the_origin), &
            new_unittest("the growth rate is the logarithmic derivative of the growth factor", &
                         test_growth_rate_is_the_log_derivative), &
            new_unittest("the growth pair satisfies its own equation", &
                         test_growth_satisfies_its_own_equation), &
            new_unittest("the growth factor falls with redshift and the rate stays in its range", &
                         test_growth_is_monotone_and_bounded), &
            new_unittest("zmax does not move the growth answer by a single bit", &
                         test_zmax_does_not_move_the_growth), &
            new_unittest("a truncated or big-rip model grows above its floor and is NaN below it", &
                         test_growth_stops_at_the_model_floor), &
            new_unittest("a universe with no matter has no growth to report", &
                         test_no_matter_means_no_growth) &
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

    !> The relative gap between two answers, with NaN and the infinities counted as agreeing.
    pure function gap(a, b) result(r)
        real(real64), intent(in) :: a !! one answer
        real(real64), intent(in) :: b !! the other
        real(real64)             :: r !! the relative difference, or zero where neither is finite

        r = 0.0_real64
        if (a /= a .or. b /= b) return
        if (.not. ieee_is_finite(a) .or. .not. ieee_is_finite(b)) return
        if (b == 0.0_real64) then
            r = abs(a)
        else
            r = abs(a - b) / abs(b)
        end if

    end function gap

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
        v(cq_scale_factor) = c%scale_factor(z)
        v(cq_otot) = c%otot(z)
        v(cq_ob) = c%ob(z)
        v(cq_odm) = c%odm(z)
        v(cq_tnu) = c%tnu(z)
        v(cq_absorption_distance) = c%absorption_distance(z)
        v(cq_nu_relative_density) = c%nu_relative_density(z)
        v(cq_growth) = c%growth_factor(z)
        v(cq_growth_rate) = c%growth_rate(z)

    end function answers

    !> Checks every row of the models `first` to `last` against the reference.
    subroutine check_rows(error, first, last)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        integer, intent(in)                        :: first !! the first model to check
        integer, intent(in)                        :: last  !! the last

        type(pf_cosmology) :: c
        real(real64)       :: got(n_cquantity), z
        integer            :: i, j, q, bad_i, bad_j, bad_q
        character(len=:), allocatable :: tag, tag2

        bad_i = 0
        bad_j = 0
        bad_q = 0
        do i = first, last
            call build(c, i)
            do j = 1, n_cz
                z = rv(cz_bits(j))
                got = answers(c, z)
                do q = 1, n_cquantity
                    if (.not. row_agrees(got(q), ref(i, j, q), q)) then
                        bad_i = i
                        bad_j = j
                        bad_q = q
                    end if
                end do
            end do
        end do
        call itoa(bad_j, tag)
        call itoa(bad_q, tag2)
        call check(error, bad_i == 0, "model " // trim(cmodel_label(max(bad_i, 1))) // &
                   " disagrees with the reference at redshift index " // tag // &
                   ", quantity " // tag2)

    end subroutine check_rows

    !> One quantity of one row against its reference, at the tolerance that quantity carries.
    !!
    !! The growth pair is the one place a row needs anything but `TABLE_TOL`: `D` is an integrated
    !! quantity rather than a tabulated quadrature and carries the ODE's own `2.5e-12`, and `f` is
    !! accurate ABSOLUTELY rather than relatively (`GROWTH_RATE_TOL`).
    pure function row_agrees(got, want, q) result(ok)
        real(real64), intent(in) :: got  !! what the library answered
        real(real64), intent(in) :: want !! the reference value
        integer, intent(in)      :: q    !! which quantity, a `cq_*` selector
        logical                  :: ok   !! they agree

        if (q == cq_growth) then
            ok = agrees(got, want, GROWTH_TOL)
        else if (q == cq_growth_rate) then
            if (want /= want) then
                ok = got /= got
            else if (got /= got) then
                ok = .false.
            else
                ok = abs(got - want) <= GROWTH_RATE_TOL
            end if
        else
            ok = agrees(got, want, TABLE_TOL)
        end if

    end function row_agrees

    !> An integer as text, for a failure message.
    !!
    !! A SUBROUTINE, never a function returning `character(len=:), allocatable`: gfortran keeps
    !! such a result's hidden length in a static slot shared by every thread, and test-drive
    !! dispatches this suite's tests with `!$omp parallel do` (`.claude/rules/fortran-gotchas.md`).
    pure subroutine itoa(n, text)
        integer, intent(in)                        :: n    !! the number
        character(len=:), allocatable, intent(out) :: text !! its decimal text

        character(len=16) :: buf

        write (buf, '(i0)') n
        text = trim(buf)

    end subroutine itoa

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
        character(len=:), allocatable :: tag

        call full%init("Planck18")
        call tiny%init("Planck18", zmax = 1.0e-6_real64)
        do k = 0, 400
            z = 0.04_real64 * (1100.0_real64 / 0.04_real64) ** (real(k, real64) / 400.0_real64)
            a = full%comoving_distance(z)
            b = tiny%comoving_distance(z)
            call itoa(int(z), tag)
            call check(error, agrees(b, a, 2.0e-9_real64), "D_C on the fallback at z = " // tag)
            if (allocated(error)) return
            a = full%lookback_time(z)
            b = tiny%lookback_time(z)
            call itoa(int(z), tag)
            call check(error, agrees(b, a, 2.0e-9_real64), "t_L on the fallback at z = " // tag)
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
        character(len=:), allocatable :: tag

        zs = [0.5_real64, 3.0_real64, 50.0_real64, 900.0_real64]
        call small%init("Planck18", zmax = 5.0_real64)
        call big%init("Planck18")
        do k = 1, size(zs)
            call itoa(k, tag)
            call check(error, agrees(small%comoving_distance(zs(k)), big%comoving_distance(zs(k)), &
                                     2.0e-9_real64), "D_C across zmax at z index " // tag)
            if (allocated(error)) return
            call itoa(k, tag)
            call check(error, agrees(small%lookback_time(zs(k)), big%lookback_time(zs(k)), &
                                     2.0e-9_real64), "t_L across zmax at z index " // tag)
            if (allocated(error)) return
            call itoa(k, tag)
            call check(error, agrees(small%age(zs(k)), big%age(zs(k)), 2.0e-9_real64), &
                       "the AGE across zmax at z index " // tag)
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

    !> `zmin` moves the table's bottom and not the answer, and `zeta = 0` is a node of the grid.
    !!
    !! **The node is asserted, not the comment.** The grid is the integer lattice times the
    !! spacing, so `zeta = 0` is a node and the scaled distance stored there is the closed-form
    !! limit `F(0) = D_H` exactly. Evaluate the table a decimal decade beyond where any cubic term
    !! can matter -- `zeta = 1e-300` -- and the interpolant returns that stored value untouched,
    !! so `D_C(zeta)/zeta` is `D_H` to the BIT. A grid laid from `zmin` to `zmax` without passing
    !! through the origin puts that evaluation inside an interval whose left node is elsewhere,
    !! and the ratio comes back a few ulp away.
    !!
    !! The rest is the mirror of `zmax_moves_the_seam_not_the_answer`: three objects with floors
    !! two decades apart must answer the same blueshift the same way, one of them (`zmin = 0`)
    !! tabulating no blueshift at all and reaching every one of them by the panel walk the table
    !! replaces. Each quantity is held to its OWN tolerance (`row_agrees`), because the growth
    !! rate's is absolute: `zmin = 0` reaches `zeta = -2.3` by 575 Runge-Kutta substeps and
    !! `zmin = -0.9` by the quintic's derivative over the nodes those same substeps wrote, and
    !! the two differ by that derivative's `O(h^5)`, measured `1.0e-11` -- while the growth
    !! FACTOR, which the quintic gives to `O(h^6)`, agrees to `2.4e-14` relative.
    subroutine test_zmin_moves_the_seam_not_the_answer(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: walked, shallow, deep
        real(real64)       :: zeta, z, got(n_cquantity), want(n_cquantity), ratio
        integer            :: k, q, bad_k, bad_q
        character(len=:), allocatable :: tag, tag2

        call walked%init("Planck18", zmin = 0.0_real64)
        call shallow%init("Planck18", zmin = -0.5_real64)
        call deep%init("Planck18")

        call check(error, deep%zmin() == -0.9_real64, "zmin() defaults to -0.9")
        if (allocated(error)) return
        call check(error, shallow%zmin() == -0.5_real64, "zmin() answers what the caller asked for")
        if (allocated(error)) return

        ! The node at the origin: the stored limit comes back untouched.
        ! Compared as a PRODUCT rather than as a quotient: `D_C` is `zeta` times what the
        ! interpolant returned, so the claim is that the interpolant returned `D_H`, and the same
        ! multiplication on both sides tests exactly that. Dividing instead adds a rounding of its
        ! own, which ifx and gfortran do not make the same way.
        ratio = deep%comoving_distance_zeta(1.0e-300_real64)
        call check(error, ratio == 1.0e-300_real64 * deep%hubble_distance(), &
                   "zeta = 0 must be a node, so D_C beside it is exactly zeta times D_H")
        if (allocated(error)) return
        call check(error, deep%comoving_distance_zeta(0.0_real64) == 0.0_real64, &
                   "and D_C at the node itself is exactly zero")
        if (allocated(error)) return
        call check(error, walked%comoving_distance_zeta(1.0e-300_real64) &
                   == 1.0e-300_real64 * walked%hubble_distance(), &
                   "the origin is a node whether or not the blueshift half is tabulated")
        if (allocated(error)) return

        ! Every quantity, over the blueshift half the three objects cover differently.
        bad_k = 0
        bad_q = 0
        do k = 1, 25
            zeta = -0.1_real64 * real(k, real64)
            z = pf_zeta2z(zeta)
            want = answers(walked, z)
            got = answers(deep, z)
            do q = 1, n_cquantity
                if (.not. row_agrees(got(q), want(q), q)) then
                    bad_k = k
                    bad_q = q
                end if
            end do
            if (zeta > -0.4_real64) then
                got = answers(shallow, z)
                do q = 1, n_cquantity
                    if (.not. row_agrees(got(q), want(q), q)) then
                        bad_k = k
                        bad_q = q
                    end if
                end do
            end if
        end do
        call itoa(bad_k, tag)
        call itoa(bad_q, tag2)
        call check(error, bad_k == 0, "zmin must move the seam and not the answer; sweep index " // &
                   tag // ", quantity " // tag2)
        if (allocated(error)) return

        ! **A zmin at the very bottom of the domain, for models whose tables saturate there.**
        ! An inverse table goes only as far as its abscissae strictly increase, and a big rip's
        ! `e^zeta/E` falls away faster than any power as `z -> -1`, so its lowest distance nodes
        ! carry the same double. The table is truncated where that starts; before it was, `%init`
        ! aborted for a model that is perfectly well defined.
        call deep%init("Planck18", zmin = -0.9999999999_real64)
        call check(error, deep%is_initialised(), "a zmin at the domain's floor must build")
        if (allocated(error)) return
        call check(error, agrees(deep%comoving_distance(-0.99_real64), &
                                 walked%comoving_distance(-0.99_real64), TABLE_TOL), &
                   "and answer what the panel walk answers")
        if (allocated(error)) return
        call deep%init(h0 = 70.0_real64, om0 = 0.3_real64, w0 = -1.0_real64, wa = 3.0_real64, &
                       zmin = -0.9999999999_real64)
        call check(error, deep%is_initialised(), "a big-rip model with that zmin must build too")
        if (allocated(error)) return
        call walked%init(h0 = 70.0_real64, om0 = 0.3_real64, w0 = -1.0_real64, wa = 3.0_real64, &
                         zmin = 0.0_real64)
        call check(error, agrees(deep%comoving_distance(-0.99_real64), &
                                 walked%comoving_distance(-0.99_real64), TABLE_TOL), &
                   "and answer what its own panel walk answers")
        if (allocated(error)) return
        ! That model's comoving distance SATURATES below about `z = -0.9`: the dark-energy
        ! density diverges towards `z = -1`, so `e^zeta/E` falls away faster than any power and
        ! every redshift down there is the same distance to the last bit. The inverse answers NaN
        ! rather than picking one of them, which is the honest answer and the reason the table is
        ! truncated where it is.
        call check(error, agrees(deep%comoving_distance(-0.999_real64), &
                                 deep%comoving_distance(-0.99_real64), 1.0e-14_real64), &
                   "the big rip's comoving distance really has saturated there")
        if (allocated(error)) return
        ! Planck18's has not, so the round trip still works at the same depth.
        call deep%init("Planck18", zmin = -0.9999999999_real64)
        call check(error, agrees(deep%z_at_comoving_distance( &
                                     deep%comoving_distance(-0.999999_real64)), &
                                 -0.999999_real64, 1.0e-9_real64), &
                   "a deep zmin must still invert where the distance is determined")

    end subroutine test_zmin_moves_the_seam_not_the_answer

    !> The BOTTOM seam, where the table hands over to the downward panel walk.
    !!
    !! The fallback starts from the tabulated bottom value, exactly as the upward one starts from
    !! the tabulated top, so neither seam can step. **Asserted as a SECOND difference**, not as a
    !! difference: the quantity is changing across the seam at its own slope, and a first
    !! difference over a span wide enough to straddle the seam is dominated by that slope rather
    !! than by anything the seam does. `f(x+h) + f(x-h) - 2 f(x)` cancels the slope, so what is
    !! left is the step itself plus a curvature term of order `f'' h^2`, which at `h = 1e-9` is
    !! nothing.
    subroutine test_bottom_seam_is_continuous(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zeta_m, lo, mid, hi, h

        call c%init("Planck18", zmin = -0.9_real64)
        ! The table's bottom is the first multiple of the spacing at or below ln(1 + zmin).
        zeta_m = -ceiling(-log(0.1_real64) / 0.008_real64) * 0.008_real64
        h = 1.0e-9_real64
        lo = c%comoving_distance_zeta(zeta_m - h)
        mid = c%comoving_distance_zeta(zeta_m)
        hi = c%comoving_distance_zeta(zeta_m + h)
        call check(error, abs(hi + lo - 2.0_real64 * mid) <= 3.0e-14_real64 * abs(mid), &
                   "D_C steps at the bottom seam")
        if (allocated(error)) return
        lo = c%lookback_time(pf_zeta2z(zeta_m - h))
        mid = c%lookback_time(pf_zeta2z(zeta_m))
        hi = c%lookback_time(pf_zeta2z(zeta_m + h))
        call check(error, abs(hi + lo - 2.0_real64 * mid) <= 3.0e-14_real64 * abs(mid), &
                   "t_L steps at the bottom seam")
        if (allocated(error)) return
        lo = c%age(pf_zeta2z(zeta_m - h))
        mid = c%age(pf_zeta2z(zeta_m))
        hi = c%age(pf_zeta2z(zeta_m + h))
        call check(error, abs(hi + lo - 2.0_real64 * mid) <= 3.0e-14_real64 * abs(mid), &
                   "the age steps at the bottom seam")

    end subroutine test_bottom_seam_is_continuous

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
        character(len=:), allocatable :: tag

        do k = -280, 100
            z = 10.0_real64 ** (real(k, real64) / 10.0_real64)
            if (z > 1.0e300_real64) cycle
            zeta = pf_z2zeta(z)
            ! `log1p` by its series where `z` is small, and directly where it is not: an
            ! independent route, not the implementation's own.
            want = independent_log1p(z)
            call itoa(k, tag)
            call check(error, abs(zeta - want) <= 1.0e-15_real64 * abs(want), &
                       "pf_z2zeta at decade " // tag)
            if (allocated(error)) return
            if (z < 1.0_real64) then
                zeta = pf_z2zeta(-z)
                want = independent_log1p(-z)
                call itoa(k, tag)
                call check(error, abs(zeta - want) <= 1.0e-14_real64 * abs(want), &
                           "pf_z2zeta at blueshift decade " // tag)
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
        character(len=:), allocatable :: tag

        call c%init("Planck18")
        do k = -80, 100
            z = 10.0_real64 ** (real(k, real64) / 10.0_real64)
            call itoa(k, tag)
            call check(error, c%comoving_distance(z) == c%comoving_distance_zeta(pf_z2zeta(z)), &
                       "the z and zeta forms must agree to the bit at decade " // tag)
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
        character(len=:), allocatable :: tag

        do i = 1, n_cmodel
            if (cmodel_age_diverges(i)) cycle
            call build(c, i)
            do j = 1, n_cz
                z = rv(cz_bits(j))
                if (z < 100.0_real64) cycle
                got = c%age(z)
                call itoa(j, tag)
                call check(error, agrees(got, ref(i, j, cq_age), AGE_TAIL_TOL), &
                           "the age of " // trim(cmodel_label(i)) // " at redshift index " // tag)
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
        character(len=:), allocatable :: tag

        call c%init("Planck18")
        age0 = c%age(0.0_real64)
        ! ON THE NODES this is the sharp test, and it is what catches an age table built by
        ! subtracting cumulative sums: the two tables come out of the SAME interval integrals
        ! summed in opposite directions, so they close to the rounding of one sum. Measured
        ! `1.9e-14` over the whole table.
        do k = 0, 800
            z = pf_zeta2z(0.008_real64 * real(k, real64))
            total = c%age(z) + c%lookback_time(z)
            call itoa(k, tag)
            call check(error, abs(total - age0) <= 1.0e-12_real64 * age0, &
                       "age + lookback must close on age(0) at node " // tag)
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
        character(len=:), allocatable :: tag

        labels = [character(len=8) :: "open", "closed"]
        do k = 1, size(labels)
            i = model_index(trim(labels(k)))
            call check(error, i > 0, trim(labels(k)) // " must be in the reference grid")
            if (allocated(error)) return
            call build(c, i)
            do j = 1, n_cz
                z = rv(cz_bits(j))
                if (z <= 0.0_real64 .or. z > 1.0e-2_real64) cycle
                call itoa(j, tag)
                call check(error, agrees(c%comoving_volume(z), ref(i, j, cq_vc), 1.0e-9_real64), &
                           "the curved comoving volume of " // trim(labels(k)) // &
                           " at redshift index " // tag)
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

    !> `D_A(z1, z2)` and `D_C(z1, z2)` against the reference pairs, the reversed and the close
    !! ones included.
    !!
    !! The close pairs are what `%comoving_distance_z1z2` exists for: at a separation of `1e-4` in
    !! redshift, differencing two tabulated distances keeps only the digits the pair's own span
    !! leaves, which is about eight of the sixteen.
    subroutine test_angular_diameter_distance_z1z2_matches_the_pairs(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z1, z2, got
        integer            :: k, i, previous
        character(len=:), allocatable :: tag

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
            call itoa(k, tag)
            call check(error, agrees(got, rv(cpair_da_bits(k)), TABLE_TOL), &
                       "D_A(z1, z2) at pair " // tag)
            if (allocated(error)) return
            got = c%comoving_distance_z1z2(z1, z2)
            call check(error, agrees(got, rv(cpair_dc_bits(k)), TABLE_TOL), &
                       "D_C(z1, z2) at pair " // tag)
            if (allocated(error)) return
            call check(error, agrees(got, c%comoving_distance(z2) - c%comoving_distance(z1), &
                                     1.0e-6_real64), &
                       "D_C(z1, z2) must be the difference it replaces, at pair " // tag)
            if (allocated(error)) return
            if (z1 == z2) then
                call check(error, got == 0.0_real64, &
                           "D_C(z, z) must be exactly zero")
                if (allocated(error)) return
                got = c%angular_diameter_distance_z1z2(z1, z2)
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
        character(len=:), allocatable :: tag

        call c%init("Planck18")
        do k = 0, 400
            ! Across the whole domain, beyond the table and below zero included.
            z = pf_zeta2z(-10.0_real64 + 33.0_real64 * real(k, real64) / 400.0_real64)
            d = c%comoving_distance(z)
            back = c%comoving_distance(c%z_at_comoving_distance(d))
            call itoa(k, tag)
            call check(error, agrees(back, d, INVERSE_TOL), &
                       "the DISTANCE round trip at step " // tag)
            if (allocated(error)) return
            t = c%lookback_time(z)
            back = c%lookback_time(c%z_at_lookback_time(t))
            call itoa(k, tag)
            call check(error, agrees(back, t, merge(BLUESHIFT_INVERSE_TOL, INVERSE_TOL, &
                                                    z < 0.0_real64)), &
                       "the lookback-TIME round trip at step " // tag)
            if (allocated(error)) return
        end do
        ! The REDSHIFT round trip is only as well conditioned as the forward function, which
        ! flattens like z^(-3/2): at z = 1e10 a distance carrying one ulp determines z to about
        ! one part in ten. That is the mathematics, not the implementation, so it is asserted
        ! tightly only up to z = 1e3.
        do k = 0, 200
            z = 1.0e-6_real64 * (1.0e3_real64 / 1.0e-6_real64) ** (real(k, real64) / 200.0_real64)
            back = c%z_at_comoving_distance(c%comoving_distance(z))
            call itoa(k, tag)
            call check(error, agrees(back, z, INVERSE_TOL), &
                       "the REDSHIFT round trip below z = 1e3 at step " // tag)
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

    !> A big-rip model, a negative-`ode0` one and a recollapsing closed one: the two screens of
    !! section 4.8, and the five INVERSES on each.
    !!
    !! The inverses are what makes this test the one that sees a NaN stored in a screen bound. A
    !! model whose `E^2` turns negative below `z = 0` has no comoving distance, lookback time or
    !! age down there, and an inverse that screened its argument against a NaN bound raised
    !! `IEEE_INVALID` on every call -- fatal under nagfor's default `-ieee=stop` -- and then let
    !! the argument through to a solver whose bracket was NaN-poisoned. Two of the three models
    !! here have that property; the forward sweep alone cannot show it.
    subroutine test_extreme_models_raise_no_flag(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: rip, neg, closed
        real(real64)       :: z, got(n_cquantity), v
        integer            :: k
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        call rip%init(h0 = 70.0_real64, om0 = 0.3_real64, w0 = -1.0_real64, wa = 3.0_real64)
        call neg%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = -0.5_real64)
        call closed%init(h0 = 70.0_real64, om0 = 1.5_real64, ode0 = 0.0_real64)

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
            call sweep_inverses(rip, got, v)
            got = answers(neg, z)
            call sweep_inverses(neg, got, v)
            got = answers(closed, z)
            call sweep_inverses(closed, got, v)
        end do
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, .not. any(raised), "an extreme model must raise no IEEE flag")

    end subroutine test_extreme_models_raise_no_flag

    !> `E^2` at `zeta`, written out from the parameters of a RADIATION-FREE `w0waCDM`.
    !!
    !! The closed form the model is defined by, not a reading of the library: `Om0 x^3 + Ok0 x^2`
    !! plus the CPL dark-energy term `Ode0 x^(3(1+w0+wa)) exp(-3 wa z/x)`, formed through its
    !! logarithm. The exponent is screened at the same magnitude the module screens it at, because
    !! `exp(-9e10)` is zero in `real64` and forming it raises `IEEE_UNDERFLOW` for nothing.
    pure function e2_closed(om0, ok0, ode0, w0, wa, zeta) result(v)
        real(real64), intent(in) :: om0  !! `Om0`
        real(real64), intent(in) :: ok0  !! `Ok0`
        real(real64), intent(in) :: ode0 !! `Ode0`
        real(real64), intent(in) :: w0   !! `w0`
        real(real64), intent(in) :: wa   !! `wa`
        real(real64), intent(in) :: zeta !! `ln(1 + z)`
        real(real64)             :: v    !! `E^2` there

        real(real64) :: x, ell, f_de

        x = exp(zeta)
        ell = 3.0_real64 * (1.0_real64 + w0 + wa) * zeta - 3.0_real64 * wa * (x - 1.0_real64) / x
        if (ell > 695.0_real64) then
            f_de = ieee_value(f_de, ieee_positive_inf)
        else if (ell < -695.0_real64) then
            f_de = 0.0_real64
        else
            f_de = exp(ell)
        end if
        if (ode0 == 0.0_real64) then
            v = om0 * x ** 3 + ok0 * x ** 2
        else
            v = om0 * x ** 3 + ok0 * x ** 2 + ode0 * f_de
        end if

    end function e2_closed

    !> The `zeta` at which `e2_closed` crosses zero, bisected from the whole blueshift range.
    pure function e2_root(om0, ok0, ode0, w0, wa) result(zeta)
        real(real64), intent(in) :: om0  !! `Om0`
        real(real64), intent(in) :: ok0  !! `Ok0`
        real(real64), intent(in) :: ode0 !! `Ode0`
        real(real64), intent(in) :: w0   !! `w0`
        real(real64), intent(in) :: wa   !! `wa`
        real(real64)             :: zeta !! the crossing, taken on the non-positive side

        real(real64) :: lo, hi, mid
        integer      :: k

        lo = -CEILING_ZETA
        hi = 0.0_real64
        do k = 1, 200
            mid = 0.5_real64 * (lo + hi)
            if (mid <= lo .or. mid >= hi) exit
            if (e2_closed(om0, ok0, ode0, w0, wa, mid) > 0.0_real64) then
                hi = mid
            else
                lo = mid
            end if
        end do
        zeta = lo

    end function e2_root

    !> `%zeta_floor()` is the root of the model's own `E^2`, for four models that have one.
    !!
    !! Each expected value is the root of the CLOSED FORM above, bisected in the test from the
    !! parameters, never a number read off a run. The four are the recollapsing closed universe
    !! (`E^2 = x^2 (Om0 x + Ok0)`, zero at `x = 1/3`), two negative-`Ode0` models four decades
    !! apart in how deep the crossing sits, and a closed CPL model whose dark-energy term
    !! collapses towards `z = -1` and leaves the curvature term to turn `E^2` over.
    subroutine test_zeta_floor_is_the_root_of_e2(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: want, got

        ! A recollapsing closed universe: Om0 x^3 + Ok0 x^2 with Ok0 = -0.5, zero at x = 1/3.
        call c%init(h0 = 70.0_real64, om0 = 1.5_real64, ode0 = 0.0_real64)
        got = c%zeta_floor()
        want = e2_root(1.5_real64, -0.5_real64, 0.0_real64, -1.0_real64, 0.0_real64)
        call check(error, abs(got - want) <= 1.0e-9_real64, &
                   "om0 = 1.5, ode0 = 0 must reach E^2 = 0 at ln(1/3)")
        if (allocated(error)) return
        call check(error, abs(got - log(1.0_real64 / 3.0_real64)) <= 1.0e-9_real64, &
                   "and that root is ln(1/3) in closed form")
        if (allocated(error)) return

        ! A negative dark-energy density, whose crossing sits where Om0 x^3 + Ok0 x^2 falls to
        ! |Ode0|. At -0.5 that is a tenth of the way down the blueshift range.
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = -0.5_real64)
        got = c%zeta_floor()
        want = e2_root(0.3_real64, 1.2_real64, -0.5_real64, -1.0_real64, 0.0_real64)
        call check(error, abs(got - want) <= 1.0e-9_real64, &
                   "ode0 = -0.5 must reach E^2 = 0 at the root of 0.3 x^3 + 1.2 x^2 - 0.5")
        if (allocated(error)) return

        ! The same, five decades smaller: the crossing is then most of the way down the range,
        ! and the panel walk has to reach it before the refinement scan sees anything.
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = -1.0e-6_real64)
        got = c%zeta_floor()
        want = e2_root(0.3_real64, 1.000001_real64 - 0.3_real64, -1.0e-6_real64, &
                       -1.0_real64, 0.0_real64)
        call check(error, abs(got - want) <= 1.0e-9_real64, &
                   "ode0 = -1e-6 must reach E^2 = 0 deep in the blueshift half")
        if (allocated(error)) return
        call check(error, got < -6.0_real64 .and. got > -7.0_real64, &
                   "and that floor is about zeta = -6.7, not the domain edge")
        if (allocated(error)) return

        ! A closed CPL model: f_DE collapses as z -> -1, so the negative curvature term is left
        ! alone and E^2 turns over while Ode0 is POSITIVE.
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = 1.2_real64, &
                    w0 = 3.0_real64, wa = -3.0_real64)
        got = c%zeta_floor()
        want = e2_root(0.3_real64, -0.5_real64, 1.2_real64, 3.0_real64, -3.0_real64)
        call check(error, abs(got - want) <= 1.0e-9_real64, &
                   "the closed CPL model must reach E^2 = 0 at its own root")
        if (allocated(error)) return

        ! The negative control: a model whose E^2 never vanishes reports the domain edge, not a
        ! crossing the scan invented.
        call c%init("Planck18")
        call check(error, c%zeta_floor() == -CEILING_ZETA, &
                   "Planck18 has no crossing, so its floor is the domain edge")

    end subroutine test_zeta_floor_is_the_root_of_e2

    !> The five inverses, each fed the forward answer of the same model at the same redshift.
    !!
    !! `v` is written so that no call is dead code an optimiser may delete; nothing reads it.
    subroutine sweep_inverses(c, got, v)
        type(pf_cosmology), intent(in) :: c              !! the model
        real(real64), intent(in)       :: got(:)         !! its forward answers, from `answers`
        real(real64), intent(out)      :: v              !! the last inverse's answer

        v = c%z_at_comoving_distance(got(cq_dc))
        v = c%z_at_lookback_time(got(cq_tl))
        v = c%z_at_age(got(cq_age))
        v = c%z_at_luminosity_distance(got(cq_dl))
        v = c%z_at_distmod(got(cq_mu))

    end subroutine sweep_inverses

    !> The six rows of the review's defect table, for `om0 = 0.3, ode0 = -0.5, h0 = 70`.
    !!
    !! That model's `E^2` vanishes at `z = -0.3981892`, and below it the universe does not exist.
    !! Four arguments are REACHABLE and must come back as the 40-digit oracle's redshifts, found
    !! by bisection on the same model; two are BEYOND what the model attains -- a comoving
    !! distance of `-1e6 Mpc` where it reaches about `-3827 Mpc`, and an age of `1000 Gyr` where
    !! it reaches about `27.9 Gyr` -- and must come back NaN. Before the floor was found, every
    !! one of the six answered a redshift near the domain floor at which the model itself
    !! answers NaN, and raised `IEEE_INVALID` doing it.
    subroutine test_truncated_domain_inverses_match_the_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: dc100, dc1000, tl1, age12, far_d, far_a, back
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = -0.5_real64)

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        dc100 = c%z_at_comoving_distance(-100.0_real64)
        dc1000 = c%z_at_comoving_distance(-1000.0_real64)
        tl1 = c%z_at_lookback_time(-1.0_real64)
        age12 = c%z_at_age(12.0_real64)
        far_d = c%z_at_comoving_distance(-1.0e6_real64)
        far_a = c%z_at_age(1000.0_real64)
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, .not. any(raised), &
                   "a truncated-domain model's inverses must raise no IEEE flag")
        if (allocated(error)) return
        call check(error, abs(dc100 - (-0.0229040998623_real64)) <= 1.0e-9_real64, &
                   "z_at_comoving_distance(-100) must be -0.0229040998623")
        if (allocated(error)) return
        call check(error, abs(dc1000 - (-0.19247724477_real64)) <= 1.0e-9_real64, &
                   "z_at_comoving_distance(-1000) must be -0.19247724477")
        if (allocated(error)) return
        call check(error, abs(tl1 - (-0.0653429910692_real64)) <= 1.0e-9_real64, &
                   "z_at_lookback_time(-1) must be -0.0653429910692")
        if (allocated(error)) return
        call check(error, abs(age12 - (-0.103330279274_real64)) <= 1.0e-9_real64, &
                   "z_at_age(12) must be -0.103330279274")
        if (allocated(error)) return
        call check(error, far_d /= far_d, &
                   "a comoving distance the model never reaches must answer NaN")
        if (allocated(error)) return
        call check(error, far_a /= far_a, "an age the model never reaches must answer NaN")
        if (allocated(error)) return

        ! The round trip, which needs no oracle: every redshift answered above is one the model
        ! itself has a comoving distance at, and that distance is the argument again.
        back = c%comoving_distance(dc1000)
        call check(error, abs(back + 1000.0_real64) <= 1.0e-9_real64 * 1000.0_real64, &
                   "the answered redshift must have the comoving distance that was asked for")

    end subroutine test_truncated_domain_inverses_match_the_oracle

    !> The tight luminosity bracket answers what the whole-domain one answers.
    !!
    !! `%z_at_luminosity_distance` brackets `[0, zeta_at_dc(d)]` when the comoving distance's own
    !! inverse table reaches `d`, and the whole domain `[0, ln(1 + 1e10)]` when it does not. The
    !! two must agree, and the way to make ONE model take both routes is to build it twice:
    !! `zmax = 1e-6` forces the minimum four-interval table, whose inverse reaches about
    !! `z = 0.033`, so every argument here falls outside it and takes the old route.
    !!
    !! A flat model and an OPEN one, because the bound `D_L >= D_C` that justifies the tight
    !! bracket is a statement about `Ok0 >= 0` -- for a closed model `D_M` turns over and
    !! `%z_at_luminosity_distance` keeps its upward panel walk instead.
    subroutine test_luminosity_bracket_is_an_identity(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: tight(2), wide(2)
        real(real64)       :: d, a, b, worst
        integer            :: k, model
        character(len=:), allocatable :: tag

        call tight(1)%init(h0 = 70.0_real64, om0 = 0.3_real64)
        call wide(1)%init(h0 = 70.0_real64, om0 = 0.3_real64, zmax = 1.0e-6_real64)
        call tight(2)%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = 0.6_real64)
        call wide(2)%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = 0.6_real64, &
                          zmax = 1.0e-6_real64)

        worst = 0.0_real64
        do model = 1, 2
            do k = 1, 40
                ! Luminosity distances from a few Mpc to a hundred Gpc, logarithmically spaced.
                d = 10.0_real64 ** (0.5_real64 + 0.1_real64 * real(k, real64))
                a = tight(model)%z_at_luminosity_distance(d)
                b = wide(model)%z_at_luminosity_distance(d)
                if (a /= a .or. b /= b) then
                    call itoa(k, tag)
                    call check(error, .false., "both brackets must answer a number at index " // tag)
                    return
                end if
                if (abs(a - b) / abs(b) > worst) worst = abs(a - b) / abs(b)
                ! And the answer is the one the forward binding confirms.
                if (.not. agrees(tight(model)%luminosity_distance(a), d, 1.0e-12_real64)) then
                    call itoa(k, tag)
                    call check(error, .false., "the tight bracket must invert D_L at index " // tag)
                    return
                end if
            end do
        end do
        call check(error, worst <= 1.0e-13_real64, &
                   "the two brackets must answer the same redshift")

    end subroutine test_luminosity_bracket_is_an_identity

    !> The thirteen closed forms admit exactly the redshifts the table's own bindings admit.
    !!
    !! The closed forms screen the caller's `z` against `PFC_Z_FLOOR` and form `x = 1 + z`; the
    !! table's bindings screen `pf_z2zeta(z)` against `-PFC_ZETA_CEILING`. The two must accept and
    !! refuse the same doubles, and the boundary is the interesting part: the scan below steps by
    !! SINGLE ULPS either side of it, so a one-ulp disagreement fails here rather than turning
    !! into a redshift that has a distance and no `E`.
    !!
    !! **A failure here on a machine where the suite otherwise passes is a libm difference**, not
    !! a defect in the screens: `PFC_Z_FLOOR` is the largest double whose `log(1 + z)` falls below
    !! the ceiling, and a `log` an ulp away from this one's moves that boundary by one double.
    subroutine test_closed_forms_share_the_table_domain(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: z, zs(9), e, dc
        integer            :: k, n_in
        integer(int64)     :: bits
        character(len=:), allocatable :: tag

        call c%init("Planck18")
        ! Nine doubles straddling the floor: the pattern is monotone in `z`, so stepping the bit
        ! pattern by one steps the double by one ulp.
        z = pf_zeta2z(-CEILING_ZETA)
        bits = transfer(z, bits)
        do k = 1, 9
            zs(k) = transfer(bits - int(5 - k, int64), z)
        end do

        n_in = 0
        do k = 1, 9
            e = c%efunc(zs(k))
            dc = c%comoving_distance(zs(k))
            call itoa(k, tag)
            call check(error, (e /= e) .eqv. (dc /= dc), &
                       "the closed form and the table must agree about the domain at ulp " // tag)
            if (allocated(error)) return
            if (e == e) n_in = n_in + 1
        end do
        ! The vacuity guard: the scan must straddle the boundary rather than sit on one side.
        call check(error, n_in > 0 .and. n_in < 9, &
                   "the ulp scan must straddle the floor, not sit entirely inside or outside it")
        if (allocated(error)) return

        ! And over the rest of the domain, where nothing subtle happens.
        do k = -60, 60
            z = pf_zeta2z(0.38_real64 * real(k, real64))
            e = c%efunc(z)
            dc = c%comoving_distance(z)
            call itoa(k, tag)
            call check(error, (e /= e) .eqv. (dc /= dc), &
                       "the two screens must agree over the whole domain, index " // tag)
            if (allocated(error)) return
        end do
        ! Both refuse what is outside it.
        call check(error, c%efunc(-1.0_real64) /= c%efunc(-1.0_real64) .and. &
                   c%efunc(1.0e11_real64) /= c%efunc(1.0e11_real64), &
                   "the closed forms must refuse z = -1 and z above the ceiling")

    end subroutine test_closed_forms_share_the_table_domain

    !> The build's tabulated Komatsu fit changes no answer.
    !!
    !! `%init` tabulates `nu_rel` on its own grid and reads it from inside the quadrature only;
    !! every per-query path evaluates the fit exactly, so `E(z)` is still astropy's formula rather
    !! than an interpolation of it. What has to be shown is that the SUBSTITUTION costs nothing:
    !! `parquet_debug_set_cosmology_exact_nu` builds the same model both ways and every quantity
    !! must agree to the tolerance the build itself is taken to.
    !!
    !! The bound here is `1e-12`, the build's own `PFC_RTOL`. The measurement is far below it --
    !! on gfortran the two builds agree BIT FOR BIT, because the quintic reproduces `nu_rel` to
    !! rounding and the term it sits in is a thousandth of `E^2`, so the integrand comes out the
    !! same double and the adaptive quadrature takes the same path. A model with no massive
    !! species gets no table at all (its fit is a constant), which is why this one has `m_nu`.
    subroutine test_tabulated_neutrino_fit_changes_no_answer(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: tabulated, exact
        real(real64)       :: z, worst, r
        integer            :: k

        call parquet_debug_set_cosmology_exact_nu(.false.)
        call tabulated%init("Planck18")
        call parquet_debug_set_cosmology_exact_nu(.true.)
        call exact%init("Planck18")
        call parquet_debug_set_cosmology_exact_nu(.false.)

        worst = 0.0_real64
        do k = -120, 120
            z = pf_zeta2z(0.19_real64 * real(k, real64))
            r = gap(tabulated%comoving_distance(z), exact%comoving_distance(z))
            if (r > worst) worst = r
            r = gap(tabulated%lookback_time(z), exact%lookback_time(z))
            if (r > worst) worst = r
            r = gap(tabulated%age(z), exact%age(z))
            if (r > worst) worst = r
        end do
        call check(error, worst <= 1.0e-12_real64, &
                   "the tabulated fit must change no answer beyond the build's own tolerance")
        if (allocated(error)) return

        ! The negative control: the hook has to reach the build, or the comparison above is two
        ! identical objects agreeing with themselves. A model whose fit is tabulated evaluates
        ! FEWER `pow` calls, not fewer integrands, so the count cannot show it -- what can is that
        ! `%onu` is unchanged while the hook is on, which is the property the split promises.
        call check(error, tabulated%onu0() == exact%onu0(), &
                   "the per-query neutrino density is the exact fit either way")

    end subroutine test_tabulated_neutrino_fit_changes_no_answer

    !> `%onu_species` sums to `%onu`, and `%otot` is EXACTLY one for a flat model.
    !!
    !! Neither is a recorded number. The species split is pinned by the identity it exists to
    !! respect -- the parts add up to the whole, and a massless species carries the same share of
    !! `Ogamma` as `nu_rel/N` does -- and `%otot` is pinned by the bit test that `%ok` being
    !! exactly zero by assignment makes possible.
    subroutine test_species_split_and_otot(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64), allocatable :: v(:)
        real(real64)       :: z, lead
        integer            :: k
        character(len=:), allocatable :: tag

        ! Planck18 has three species, one of them massive.
        call c%init("Planck18")
        do k = -6, 12
            z = pf_zeta2z(1.7_real64 * real(k, real64))
            call c%onu_species(z, v)
            call itoa(k, tag)
            call check(error, size(v) == 3, "three species at index " // tag)
            if (allocated(error)) return
            call check(error, agrees(v(1) + v(2) + v(3), c%onu(z), 1.0e-14_real64), &
                       "the species must sum to Onu at index " // tag)
            if (allocated(error)) return
            ! The two massless ones carry `Ogamma * A Neff / N` each.
            lead = c%ogamma(z) * 0.22710731766_real64 * (c%neff() / 3.0_real64)
            call check(error, agrees(v(1), lead, 1.0e-14_real64), &
                       "a massless species is Ogamma times A Neff/N at index " // tag)
            if (allocated(error)) return
            call check(error, v(1) == v(2), "the two massless species must be identical")
            if (allocated(error)) return
            call check(error, v(3) >= v(1), "the massive species must carry at least as much")
            if (allocated(error)) return
        end do

        ! A model with no species at all gets a zero-length array, not an unallocated one.
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, neff = 0.0_real64, &
                    m_nu = [real(real64) ::])
        call c%onu_species(0.5_real64, v)
        call check(error, allocated(v) .and. size(v) == 0, &
                   "neff = 0 must give a zero-length species array")
        if (allocated(error)) return

        ! `Otot` is EXACTLY one for a flat model, at every redshift, because `Ok` is exactly zero.
        call c%init("Planck18")
        do k = -6, 12
            z = pf_zeta2z(1.7_real64 * real(k, real64))
            call itoa(k, tag)
            call check(error, c%otot(z) == 1.0_real64, &
                       "a flat model must have Otot exactly one at index " // tag)
            if (allocated(error)) return
        end do
        ! And not one for a curved model, which is the negative control.
        call c%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = 0.6_real64)
        call check(error, c%otot(1.0_real64) /= 1.0_real64, &
                   "an open model must not have Otot exactly one")

    end subroutine test_species_split_and_otot

    !> `%clone` copies what the caller did not name, and keeps "flat by omission" flat.
    subroutine test_clone_copies_and_overrides(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: base, copy, curved, copy2
        real(real64), allocatable :: masses(:), masses2(:)
        character(len=:), allocatable :: name

        call base%init("Planck18")
        call base%clone(copy)
        call check(error, copy%h0() == base%h0() .and. copy%om0() == base%om0() .and. &
                   copy%tcmb0() == base%tcmb0() .and. copy%neff() == base%neff() .and. &
                   copy%ob0() == base%ob0() .and. copy%w0() == base%w0() .and. &
                   copy%wa() == base%wa() .and. copy%zmax() == base%zmax() .and. &
                   copy%zmin() == base%zmin(), &
                   "an unmodified clone must carry every parameter across")
        if (allocated(error)) return
        call base%m_nu(masses)
        call copy%m_nu(masses2)
        call check(error, size(masses) == size(masses2) .and. all(masses == masses2), &
                   "and the species masses with them")
        if (allocated(error)) return
        call copy%get_name(name)
        call check(error, name == "Planck18", "and the label")
        if (allocated(error)) return
        call check(error, copy%comoving_distance(1.0_real64) == base%comoving_distance(1.0_real64), &
                   "an unmodified clone must answer exactly what its source answers")
        if (allocated(error)) return

        ! Flat by omission stays flat by omission, whatever else moves.
        call check(error, base%is_flat(), "Planck18 is flat by omission")
        if (allocated(error)) return
        call base%clone(copy, h0 = 70.0_real64, om0 = 0.25_real64, name = "shifted")
        call check(error, copy%is_flat(), "a clone of a flat model is flat, by assignment")
        if (allocated(error)) return
        call check(error, copy%ok0() == 0.0_real64, "so its Ok0 is exactly zero")
        if (allocated(error)) return
        call check(error, copy%h0() == 70.0_real64 .and. copy%om0() == 0.25_real64, &
                   "and it took the parameters it was given")
        if (allocated(error)) return
        call copy%get_name(name)
        call check(error, name == "shifted", "and the label it was given")
        if (allocated(error)) return

        ! A CURVED source keeps its curvature, and a flat one can be given some.
        call curved%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = 0.6_real64)
        call curved%clone(copy2, h0 = 60.0_real64)
        call check(error, .not. copy2%is_flat() .and. copy2%ode0() == 0.6_real64, &
                   "a clone of a curved model keeps its Ode0")
        if (allocated(error)) return
        call base%clone(copy2, ode0 = 0.5_real64)
        call check(error, .not. copy2%is_flat() .and. copy2%ode0() == 0.5_real64, &
                   "naming ode0 makes the clone of a flat model curved")

    end subroutine test_clone_copies_and_overrides

    !> Ten models by every binding by twenty-three edge inputs: not one `ieee_usual` flag.
    !!
    !! The review's own edge harness, in the repository's shape. It differs from the two flag
    !! tests beside it in TWO ways, and both are load-bearing: its model list contains the two
    !! whose `E^2` turns negative at a finite blueshift, and it calls the five INVERSES as well as
    !! the forward bindings. Either one alone misses the defect that motivated it -- a bound
    !! stored as a NaN, compared against by every inverse, raising `IEEE_INVALID` on every call
    !! and letting an unreachable argument through to a solver.
    !!
    !! The inputs are the ones a caller gets wrong rather than the ones a model is defined at: a
    !! NaN, both infinities, a catalogue's `-99`, `huge`, the two doubles either side of the
    !! domain's floor and the two either side of its ceiling, and a denormal-adjacent `1e-300`.
    !! `IEEE_UNDERFLOW` is deliberately outside the assertion -- it is not in `ieee_usual`, and a
    !! `z = 1e-300` really does underflow something on the way to an answer that is right.
    subroutine test_every_binding_and_model_raises_no_flag(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zs(23), got(n_cquantity), v, floor_z
        real(real64), allocatable :: species(:)
        integer            :: i, k
        integer(int64)     :: bits
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))
        character(len=8)   :: names(8)

        names = [character(len=8) :: "Planck18", "Planck15", "Planck13", "WMAP9", "WMAP7", &
                 "WMAP5", "WMAP3", "WMAP1"]

        floor_z = pf_zeta2z(-CEILING_ZETA)
        bits = transfer(floor_z, bits)
        zs(1) = ieee_value(zs(1), ieee_quiet_nan)
        zs(2) = ieee_value(zs(2), ieee_positive_inf)
        zs(3) = -ieee_value(zs(3), ieee_positive_inf)
        zs(4) = -1.0_real64
        zs(5) = transfer(transfer(-1.0_real64, bits) - 1_int64, zs(5))   ! the double just above -1
        zs(6) = transfer(bits + 1_int64, zs(6))                          ! just inside the floor
        zs(7) = floor_z                                                  ! just outside it
        zs(8) = -0.99_real64
        zs(9) = -0.5_real64
        zs(10) = -1.0e-300_real64
        zs(11) = 0.0_real64
        zs(12) = 1.0e-300_real64
        zs(13) = 1.0e-8_real64
        zs(14) = 1.0e-3_real64
        zs(15) = 0.5_real64
        zs(16) = 2.0_real64
        zs(17) = 1100.0_real64
        zs(18) = 1.0e5_real64
        zs(19) = 1.0e10_real64
        zs(20) = 1.0e10_real64 * (1.0_real64 + 1.0e-15_real64)           ! just past the ceiling
        zs(21) = -99.0_real64
        zs(22) = huge(1.0_real64)
        zs(23) = -huge(1.0_real64)

        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        do i = 1, 10
            if (i <= 8) then
                call c%init(trim(names(i)))
            else if (i == 9) then
                ! The recollapsing closed universe: `E^2` reaches zero at `z = -2/3`.
                call c%init(h0 = 70.0_real64, om0 = 1.5_real64, ode0 = 0.0_real64)
            else
                ! A negative dark-energy density: `E^2` reaches zero at `z = -0.398`.
                call c%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = -0.5_real64)
            end if
            do k = 1, 23
                got = answers(c, zs(k))
                call sweep_inverses(c, got, v)
                v = c%comoving_distance_zeta(zs(k))
                v = c%comoving_distance_z1z2(zs(k), zs(k))
                v = c%comoving_distance_z1z2(0.0_real64, zs(k))
                v = c%angular_diameter_distance_z1z2(0.0_real64, zs(k))
                call c%onu_species(zs(k), species)
            end do
        end do
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)

        call check(error, .not. any(raised), &
                   "ten models by every binding by twenty-three edge inputs must raise no flag")
        if (allocated(error)) return
        ! The vacuity guard: the sweep must reach real answers as well as NaNs, or a screen that
        ! refused everything would pass this test.
        call check(error, got(cq_dc) /= got(cq_dc), "the last input is outside the domain")
        if (allocated(error)) return
        call c%init("Planck18")
        call check(error, c%comoving_distance(0.5_real64) > 0.0_real64, &
                   "and the sweep's models answer numbers inside it")

    end subroutine test_every_binding_and_model_raises_no_flag

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
        ! Blanks on EITHER side, not only the trailing ones a `trim` would take.
        call c%init("  Planck18")
        call c%get_name(name)
        call check(error, name == "Planck18", "a leading blank must not hide the name")
        if (allocated(error)) return
        call c%init("Planck18   ")
        call c%get_name(name)
        call check(error, name == "Planck18", "nor a trailing one")
        if (allocated(error)) return
        call c%init("   pLaNcK18  ")
        call c%get_name(name)
        call check(error, name == "Planck18", "nor both together")
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
        character(len=:), allocatable :: tag

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
        call itoa(bad_k, tag)
        call check(error, bad_i == 0, "z_at_age must round trip; " // &
                   trim(cmodel_label(max(bad_i, 1))) // " failed at sweep index " // tag)
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
        character(len=:), allocatable :: tag

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
        call itoa(bad_k, tag)
        call check(error, bad_i == 0, "the luminosity-distance inverses must round trip; " // &
                   trim(cmodel_label(max(bad_i, 1))) // " failed at sweep index " // tag)
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

    ! =========================================================================================
    ! The growth of structure
    ! =========================================================================================

    !> Einstein-de Sitter grows exactly as `1/(1 + z)`, and its rate is exactly 1.
    !!
    !! **The one model whose growth is a closed form, and the test every coefficient of the
    !! equation has to pass.** With `Om0 = 1` and nothing else, `E^2 = e^(3 zeta)` gives
    !! `dlnE^2/dzeta = 3`, so `q = 1/2` and `Om = 1`, and `f^2 + f/2 - 3/2 = 0` has the root
    !! `f = 1`: the right-hand side is identically zero, `f` never moves off the 1 the Meszaros
    !! initial condition starts it at, and `u = lnD + zeta` never moves off zero. A wrong `3/2`,
    !! a wrong `q`, a flipped sign, a transposed pair or an anchor at the wrong node each move
    !! this by more than `1e-3`.
    subroutine test_einstein_de_sitter_growth_is_exact(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zs(20), z
        integer            :: k

        zs = [-0.9_real64, -0.8_real64, -0.5_real64, -0.1_real64, -1.0e-8_real64, 0.0_real64, &
              1.0e-8_real64, 1.0e-4_real64, 0.01_real64, 0.1_real64, 0.5_real64, 1.0_real64, &
              2.0_real64, 5.0_real64, 10.0_real64, 100.0_real64, 1100.0_real64, 1.0e5_real64, &
              1.0e8_real64, 1.0e10_real64]
        call c%init(h0 = 70.0_real64, om0 = 1.0_real64, ode0 = 0.0_real64)
        do k = 1, size(zs)
            z = zs(k)
            call check(error, abs(c%growth_factor(z) * (1.0_real64 + z) - 1.0_real64) <= 1.0e-14_real64, &
                       "Einstein-de Sitter D(z)(1+z) must be 1")
            if (allocated(error)) return
            call check(error, abs(c%growth_rate(z) - 1.0_real64) <= 1.0e-14_real64, &
                       "Einstein-de Sitter f must be 1")
            if (allocated(error)) return
        end do

    end subroutine test_einstein_de_sitter_growth_is_exact

    !> `D(0)` is exactly one for every model in the grid, and `f(0)` is the generated row.
    !!
    !! Bitwise, not to a tolerance: `zeta = 0` is a node of the integer lattice, the pass
    !! subtracts that node from every other, and the table carries `lnD + zeta`, so the node at
    !! the origin is an exact zero and `exp` of it is an exact one. An anchor at node 1 instead of
    !! at the origin -- the two are the same only when the blueshift half is empty -- moves this
    !! by the whole of `lnD(zeta_m)`.
    !!
    !! `f(0)` comes from its own generated value rather than from a row, because the reference's
    !! redshift grid carries no zero: it is the number a caller forming `f sigma8` at the present
    !! day reads, so it is pinned on its own.
    subroutine test_growth_is_anchored_at_the_origin(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        integer            :: i

        do i = 1, n_cmodel
            call build(c, i)
            if (par(i, cp_om0) <= 0.0_real64) cycle
            call check(error, c%growth_factor(0.0_real64) == 1.0_real64, &
                       "D(0) must be exactly one for " // trim(cmodel_label(i)))
            if (allocated(error)) return
            call check(error, abs(c%growth_rate(0.0_real64) - der(i, cd_growth_rate0)) &
                       <= GROWTH_RATE_TOL, "f(0) must be the generated value for " // trim(cmodel_label(i)))
            if (allocated(error)) return
        end do

    end subroutine test_growth_is_anchored_at_the_origin

    !> `f = dlnD/dlna` against a central difference of `%growth_factor` itself.
    !!
    !! `dlnD/dlna` is `-(1 + z) dlnD/dz`, so the difference is taken in `zeta` where the table is
    !! uniform. *Catches*: the two bindings wired to each other's arrays, and a sign.
    subroutine test_growth_rate_is_the_log_derivative(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zeta, d, h, want, got
        integer            :: i, k
        integer, parameter :: MODELS(4) = [8, 9, 12, 16]   ! Planck18, flat_no_rad, wcdm, two_massive

        h = 1.0e-4_real64
        do i = 1, size(MODELS)
            call build(c, MODELS(i))
            do k = -6, 12
                zeta = 0.5_real64 * real(k, real64)
                d = (log(c%growth_factor(pf_zeta2z(zeta + h))) &
                     - log(c%growth_factor(pf_zeta2z(zeta - h)))) / (2.0_real64 * h)
                want = -d
                got = c%growth_rate(pf_zeta2z(zeta))
                call check(error, abs(got - want) <= 1.0e-6_real64, &
                           "f must be minus dlnD/dzeta for " // trim(cmodel_label(MODELS(i))))
                if (allocated(error)) return
            end do
        end do

    end subroutine test_growth_rate_is_the_log_derivative

    !> The growth pair put back into its own equation, with every term reached another way.
    !!
    !! `df/dzeta` by a central difference of `%growth_rate`, against `f^2 + q f - (3/2) Om` with
    !! `q = 2 - dlnE/dzeta` from a central difference of `ln %efunc` and `Om` from `%om(z)`.
    !! Nothing on the right-hand side touches the growth table, so this is the equation and not
    !! the implementation. *Catches*: a wrong or missing Komatsu derivative -- `two_massive` and
    !! the named models are the ones where it is not zero -- a wrong source, and a transposed `q`.
    subroutine test_growth_satisfies_its_own_equation(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zeta, h, fz, dfdz, q, om, resid
        integer            :: i, k
        integer, parameter :: MODELS(6) = [8, 9, 11, 12, 16, 14]

        h = 1.0e-4_real64
        do i = 1, size(MODELS)
            call build(c, MODELS(i))
            do k = -4, 12
                zeta = 0.5_real64 * real(k, real64)
                fz = c%growth_rate(pf_zeta2z(zeta))
                dfdz = (c%growth_rate(pf_zeta2z(zeta + h)) - c%growth_rate(pf_zeta2z(zeta - h))) &
                       / (2.0_real64 * h)
                q = 2.0_real64 - (log(c%efunc(pf_zeta2z(zeta + h))) &
                                  - log(c%efunc(pf_zeta2z(zeta - h)))) / (2.0_real64 * h)
                om = c%om(pf_zeta2z(zeta))
                resid = dfdz - (fz * fz + q * fz - 1.5_real64 * om)
                call check(error, abs(resid) <= 1.0e-5_real64, &
                           "the growth pair must satisfy its own equation for " // &
                           trim(cmodel_label(MODELS(i))))
                if (allocated(error)) return
            end do
        end do

    end subroutine test_growth_satisfies_its_own_equation

    !> `D` falls with redshift, exceeds one at a blueshift, and `f` stays in `(0, 1.1]`.
    !!
    !! *Catches*: a table stored backwards, and an off-by-one in the node index.
    subroutine test_growth_is_monotone_and_bounded(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zeta, d, prev, fz
        integer            :: i, k

        do i = 1, 8
            call build(c, i)
            prev = huge(1.0_real64)
            do k = -280, 2870, 7
                zeta = real(k, real64) * 0.008_real64
                d = c%growth_factor(pf_zeta2z(zeta))
                fz = c%growth_rate(pf_zeta2z(zeta))
                call check(error, d < prev, "D must fall with redshift for " // trim(cmodel_label(i)))
                if (allocated(error)) return
                call check(error, fz > 0.0_real64 .and. fz <= 1.1_real64, &
                           "f must stay in (0, 1.1] for " // trim(cmodel_label(i)))
                if (allocated(error)) return
                if (zeta < 0.0_real64) then
                    call check(error, d > 1.0_real64, &
                               "D must exceed one at a blueshift for " // trim(cmodel_label(i)))
                    if (allocated(error)) return
                end if
                prev = d
            end do
        end do

    end subroutine test_growth_is_monotone_and_bounded

    !> `zmax` moves the growth answer by NOTHING, which is stronger than the other tables manage.
    !!
    !! The growing mode is an attractor in the direction of time only, so the growth table runs
    !! from `PFC_GROWTH_TOP_NODE` downward whatever the caller asked to tabulate, and there is no
    !! fallback above it to disagree with. Two objects six decades apart in `zmax` therefore carry
    !! the SAME table, bit for bit. *Catches*: a growth grid that ends at `zeta_n`.
    subroutine test_zmax_does_not_move_the_growth(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: small, big
        real(real64)       :: zeta, z
        integer            :: k

        call small%init("Planck18", zmax = 1.0e-6_real64)
        call big%init("Planck18", zmax = 1.0e5_real64)
        do k = -280, 2870, 13
            zeta = real(k, real64) * 0.008_real64
            z = pf_zeta2z(zeta)
            call check(error, small%growth_factor(z) == big%growth_factor(z), &
                       "zmax must not move D by a bit")
            if (allocated(error)) return
            call check(error, small%growth_rate(z) == big%growth_rate(z), &
                       "zmax must not move f by a bit")
            if (allocated(error)) return
        end do

    end subroutine test_zmax_does_not_move_the_growth

    !> A model whose `E^2` runs out below some blueshift grows above its floor and is NaN below it.
    !!
    !! Two of them: the recollapsing closed universe, whose `E^2` reaches zero at `z = -2/3`, and
    !! a CPL big rip, whose dark-energy term passes the `exp` screen and makes `E^2` infinite
    !! before `z = -1`. Neither may leave a NaN anywhere above where it belongs -- the defect
    !! class the review found in the inverses -- and neither may raise an IEEE flag on the way.
    subroutine test_growth_stops_at_the_model_floor(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zeta, d, fz, e_here, e_below
        integer            :: i, k, reached, wrong
        logical            :: alive
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        ! **Where the model exists, growth exists.** `%efunc` is the discriminator, taken at the
        ! point and a little below it, because the integrator's own midpoints reach below the
        ! argument: a recollapsing universe's `E` is NaN at and under its floor, and a big rip's
        ! is `+Infinity` once the dark-energy factor has passed the `exp` screen -- two different
        ! ends of the domain, and neither of them a reason for growth to be missing above.
        reached = 0
        wrong = 0
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        do i = 1, 2
            if (i == 1) then
                call c%init(h0 = 70.0_real64, om0 = 1.5_real64, ode0 = 0.0_real64, &
                            zmin = -0.99_real64)
            else
                call c%init(h0 = 70.0_real64, om0 = 0.3_real64, w0 = -1.2_real64, wa = 0.5_real64, &
                            tcmb0 = 2.7255_real64, zmin = -0.99_real64)
            end if
            do k = -2870, 2870, 11
                zeta = real(k, real64) * 0.008_real64
                d = c%growth_factor(pf_zeta2z(zeta))
                fz = c%growth_rate(pf_zeta2z(zeta))
                e_here = c%efunc(pf_zeta2z(zeta))
                e_below = c%efunc(pf_zeta2z(zeta - 0.05_real64))
                ! The NaN screens stand alone and come first: an ordered comparison against a
                ! quiet NaN raises `IEEE_INVALID`, which is exactly what this test is reading.
                alive = .false.
                if (e_here == e_here .and. e_below == e_below) then
                    if (e_here <= huge(e_here) .and. e_below <= huge(e_below)) alive = .true.
                end if
                if (alive) then
                    reached = reached + 1
                    if (d /= d .or. fz /= fz) wrong = wrong + 1
                end if
            end do
        end do
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)
        call check(error, .not. any(raised), "a truncated model must raise no flag in growth")
        if (allocated(error)) return
        call check(error, reached > 200, "the sweep must reach a live redshift of both models")
        if (allocated(error)) return
        call check(error, wrong == 0, "growth must be a number wherever the model itself is")
        if (allocated(error)) return

        ! The recollapsing universe, in detail: finite a panel above its floor, NaN below it.
        call c%init(h0 = 70.0_real64, om0 = 1.5_real64, ode0 = 0.0_real64, zmin = -0.99_real64)
        call check(error, c%zeta_floor() < 0.0_real64, "the recollapsing model must have a floor")
        if (allocated(error)) return
        d = c%growth_factor(pf_zeta2z(c%zeta_floor() + 0.5_real64))
        fz = c%growth_rate(pf_zeta2z(c%zeta_floor() + 0.5_real64))
        call check(error, d == d .and. fz == fz .and. d > 1.0_real64, &
                   "growth above the floor must be a number")
        if (allocated(error)) return
        d = c%growth_factor(pf_zeta2z(c%zeta_floor() - 0.1_real64))
        fz = c%growth_rate(pf_zeta2z(c%zeta_floor() - 0.1_real64))
        call check(error, d /= d .and. fz /= fz, "growth below the floor must be NaN")

    end subroutine test_growth_stops_at_the_model_floor

    !> de Sitter and Milne have no matter, so both growth bindings answer a quiet NaN.
    !!
    !! The Riccati equation is not singular for `Om0 = 0` -- it would answer `f = 0` and `D = 1`
    !! everywhere, a number that means nothing -- so no table is built and both bindings say so.
    !! *Catches*: a `0/0`, and a table built where it should have been skipped.
    subroutine test_no_matter_means_no_growth(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: c
        real(real64)       :: zs(5), d, fz
        integer            :: i, k
        logical            :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        zs = [-0.9_real64, -0.5_real64, 0.0_real64, 1.0_real64, 1100.0_real64]
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        do i = 1, 2
            if (i == 1) then
                call c%init(h0 = 70.0_real64, om0 = 0.0_real64, ode0 = 1.0_real64)   ! de Sitter
            else
                call c%init(h0 = 70.0_real64, om0 = 0.0_real64, ode0 = 0.0_real64)   ! Milne
            end if
            do k = 1, size(zs)
                d = c%growth_factor(zs(k))
                fz = c%growth_rate(zs(k))
            end do
        end do
        call ieee_get_flag(ieee_usual, raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif
        call ieee_set_flag(ieee_usual, saved .or. raised)
        call check(error, .not. any(raised), "a matterless model must raise no flag in growth")
        if (allocated(error)) return

        call c%init(h0 = 70.0_real64, om0 = 0.0_real64, ode0 = 1.0_real64)
        do k = 1, size(zs)
            call check(error, c%growth_factor(zs(k)) /= c%growth_factor(zs(k)), &
                       "de Sitter must have no growth factor")
            if (allocated(error)) return
            call check(error, c%growth_rate(zs(k)) /= c%growth_rate(zs(k)), &
                       "de Sitter must have no growth rate")
            if (allocated(error)) return
        end do
        call c%init(h0 = 70.0_real64, om0 = 0.0_real64, ode0 = 0.0_real64)
        call check(error, c%growth_factor(1.0_real64) /= c%growth_factor(1.0_real64), &
                   "Milne must have no growth factor")

    end subroutine test_no_matter_means_no_growth

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
