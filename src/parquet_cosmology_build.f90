!> `%init` in both forms, the validation it does, the tabulation it builds, and the object's
!! lifetime: `%clear`, `%get_name`, `%describe` and `%m_nu`.
!!
!! This is the only impure code in `parquet_cosmology`. Every abort is taken under one named
!! `critical`, so a `%init` inside a parallel region aborts on one thread rather than racing
!! several into `error stop` at once.
!!
!! **The three tables come out of ONE set of interval integrals.** Each grid interval is
!! integrated on its own with `pf_integrate`, for `e^zeta/E` and for `1/E`. The distance and
!! lookback tables are the FORWARD cumulative sums of those; the age table is the BACKWARD
!! cumulative sums of the same `1/E` intervals, plus one tail integral past the table's edge. So
!! `age(zeta_i) + t_L(zeta_i) = age(0)` node by node, to the rounding of one sum, and no age is
!! ever formed by subtracting a large number from another large number.
submodule (parquet_cosmology) parquet_cosmology_build

    use ieee_arithmetic, only : ieee_is_finite, ieee_value, ieee_quiet_nan, ieee_positive_inf

    implicit none

contains

    ! =========================================================================================
    ! The integrand object
    ! =========================================================================================

    module procedure cosmology_integrand_eval
        f = cosmology_integrand_at(this%p, this%d, this%which, x)
    end procedure cosmology_integrand_eval

    ! =========================================================================================
    ! Aborting
    ! =========================================================================================

    !> Ends the program with `msg`, with the caller's `context` appended when there is one.
    !!
    !! Under one named `critical`, so only one thread of a concurrent `%init` aborts. `context` is
    !! capped at `PFC_CONTEXT_CAP` characters, as every module caps caller-supplied text.
    subroutine cosmology_abort(msg, context)
        character(len=*), intent(in)           :: msg     !! the message, assembled by the caller
        character(len=*), intent(in), optional :: context !! the caller's context, if any

        character(len=:), allocatable :: full

        full = msg
        if (present(context)) then
            if (len_trim(context) > PFC_CONTEXT_CAP) then
                full = full // " [" // context(1:PFC_CONTEXT_CAP) // "...]"
            else if (len_trim(context) > 0) then
                full = full // " [" // trim(context) // "]"
            end if
        end if
        !$omp critical (pf_cosmology_guard)
        error stop full
        !$omp end critical (pf_cosmology_guard)

    end subroutine cosmology_abort

    !> `value` rendered the way every message in the library renders a number.
    function shown(value) result(text)
        real(real64), intent(in)      :: value !! the value to render
        character(len=:), allocatable :: text  !! the rendered value

        call pf_to_str(value, text)

    end function shown

    ! =========================================================================================
    ! `%init`
    ! =========================================================================================

    module procedure cosmology_init_named

        integer :: which
        character(len=len(name)) :: folded
        integer :: i

        folded = name
        call pf_to_lower(folded)
        which = 0
        do i = 1, PFC_N_NAMED
            if (trim(folded) == trim(lowered(pfc_named_name(i)))) then
                which = i
                exit
            end if
        end do
        if (which == 0) then
            call cosmology_abort("pf_cosmology%init: unknown cosmology """ // capped(name) // """;" // &
                                 " the named cosmologies are Planck18, Planck15, Planck13, WMAP9," // &
                                 " WMAP7, WMAP5, WMAP3 and WMAP1", context)
        end if

        call cosmology_init_params(this, &
                                   h0 = pfc_named_h0(which), &
                                   om0 = pfc_named_om0(which), &
                                   tcmb0 = pfc_named_tcmb0(which), &
                                   neff = pfc_named_neff(which), &
                                   m_nu = pfc_named_mnu((which - 1) * PFC_NAMED_N_NU + 1: &
                                                        which * PFC_NAMED_N_NU), &
                                   ob0 = pfc_named_ob0(which), &
                                   name = trim(pfc_named_name(which)), &
                                   zmax = zmax, context = context)

    end procedure cosmology_init_named

    !> `text` folded to lower case, for the case-insensitive name match.
    function lowered(text) result(out)
        character(len=*), intent(in) :: text !! the text to fold
        character(len=len(text))     :: out  !! the folded copy

        out = text
        call pf_to_lower(out)

    end function lowered

    !> Caller text capped for a message.
    function capped(text) result(out)
        character(len=*), intent(in)  :: text !! the caller's text
        character(len=:), allocatable :: out  !! at most `PFC_CONTEXT_CAP` characters plus "..."

        if (len_trim(text) > PFC_CONTEXT_CAP) then
            out = text(1:PFC_CONTEXT_CAP) // "..."
        else
            out = trim(text)
        end if

    end function capped

    module procedure cosmology_init_params

        real(real64) :: h0_si, rho_crit0, rho_gamma0
        integer      :: i

        ! -- the arguments, in the order section 5.4 of feature_cosmology.md tabulates them ----

        if (.not. ieee_is_finite(h0) .or. h0 < PFC_H0_MIN .or. h0 > PFC_H0_MAX) then
            call cosmology_abort("pf_cosmology%init: h0 must be finite and within " // &
                                 "[1e-10, 1e10] km/s/Mpc, got " // shown(h0), context)
        end if
        if (.not. ieee_is_finite(om0) .or. om0 < 0.0_real64) then
            call cosmology_abort("pf_cosmology%init: om0 must be finite and non-negative, got " // &
                                 shown(om0), context)
        end if
        if (present(ode0)) then
            if (.not. ieee_is_finite(ode0)) then
                call cosmology_abort("pf_cosmology%init: ode0 must be finite, got " // shown(ode0), context)
            end if
        end if
        this%p%tcmb0 = 0.0_real64
        if (present(tcmb0)) then
            if (.not. ieee_is_finite(tcmb0) .or. tcmb0 < 0.0_real64) then
                call cosmology_abort("pf_cosmology%init: tcmb0 must be finite and non-negative, got " // &
                                     shown(tcmb0), context)
            end if
            this%p%tcmb0 = tcmb0
        end if
        this%p%neff = 3.04_real64
        if (present(neff)) then
            if (.not. ieee_is_finite(neff) .or. neff < 0.0_real64) then
                call cosmology_abort("pf_cosmology%init: neff must be finite and non-negative, got " // &
                                     shown(neff), context)
            end if
            this%p%neff = neff
        end if
        this%d%n_nu = int(floor(this%p%neff))
        if (present(m_nu)) then
            if (size(m_nu) /= this%d%n_nu) then
                call cosmology_abort("pf_cosmology%init: m_nu needs one finite, non-negative mass per " // &
                                     "species: floor(neff) = " // shown(real(this%d%n_nu, real64)) // &
                                     ", got " // shown(real(size(m_nu), real64)) // " entries", context)
            end if
            do i = 1, size(m_nu)
                if (.not. ieee_is_finite(m_nu(i)) .or. m_nu(i) < 0.0_real64) then
                    call cosmology_abort("pf_cosmology%init: m_nu needs one finite, non-negative mass " // &
                                         "per species: entry " // shown(real(i, real64)) // " is " // &
                                         shown(m_nu(i)), context)
                end if
            end do
            this%p%m_nu = m_nu
        else
            allocate (this%p%m_nu(this%d%n_nu))
            this%p%m_nu = 0.0_real64
        end if
        this%has_ob0 = present(ob0)
        this%p%ob0 = ieee_value(this%p%ob0, ieee_quiet_nan)
        if (present(ob0)) then
            if (.not. ieee_is_finite(ob0) .or. ob0 < 0.0_real64 .or. ob0 > om0) then
                call cosmology_abort("pf_cosmology%init: ob0 must be finite, non-negative and at most " // &
                                     "om0, got " // shown(ob0), context)
            end if
            this%p%ob0 = ob0
        end if
        this%p%w0 = -1.0_real64
        if (present(w0)) then
            if (.not. ieee_is_finite(w0) .or. abs(w0) > PFC_W_LIMIT) then
                call cosmology_abort("pf_cosmology%init: w0 must be finite and within [-3, 3], got " // &
                                     shown(w0), context)
            end if
            this%p%w0 = w0
        end if
        this%p%wa = 0.0_real64
        if (present(wa)) then
            if (.not. ieee_is_finite(wa) .or. abs(wa) > PFC_W_LIMIT) then
                call cosmology_abort("pf_cosmology%init: wa must be finite and within [-3, 3], got " // &
                                     shown(wa), context)
            end if
            this%p%wa = wa
        end if
        this%p%zmax = PFC_DEFAULT_ZMAX
        if (present(zmax)) then
            if (.not. ieee_is_finite(zmax) .or. zmax <= 0.0_real64 .or. zmax > PFC_Z_CEILING) then
                call cosmology_abort("pf_cosmology%init: zmax must be finite, positive and at most " // &
                                     "1e10, got " // shown(zmax), context)
            end if
            this%p%zmax = zmax
        end if
        this%p%h0 = h0
        this%p%om0 = om0
        if (present(name)) then
            this%label = capped(name)
        else
            this%label = "custom"
        end if

        ! -- the derived parameters -------------------------------------------------------------

        this%d%n_massless = count(this%p%m_nu == 0.0_real64)
        this%massive_nu = this%d%n_massless /= this%d%n_nu
        this%d%tnu0 = pfc_nu_temp_ratio * this%p%tcmb0
        h0_si = this%p%h0 * 1000.0_real64 / pfc_mpc_m
        rho_crit0 = 3.0_real64 * h0_si ** 2 / (8.0_real64 * acos(-1.0_real64) * pfc_g_si)
        ! Kept in M_sun/Mpc^3, which is `%critical_density`'s unit: the solar mass is the IAU 2015
        ! nominal `GM_sun` over this module's own `G`, which is how astropy derives `M_sun` too.
        this%d%rho_crit0 = rho_crit0 * pfc_mpc_m ** 3 / (pfc_gm_sun / pfc_g_si)
        if (this%p%tcmb0 == 0.0_real64) then
            ! `Tcmb0 = 0` switches radiation AND neutrinos off entirely, as astropy does, so the
            ! radiation term is never formed and no `0 * Infinity` arises from an undefined T_nu0.
            this%d%ogamma0 = 0.0_real64
        else
            rho_gamma0 = 4.0_real64 * pfc_sigma_sb * this%p%tcmb0 ** 4 / pfc_c_ms ** 3
            this%d%ogamma0 = rho_gamma0 / rho_crit0
        end if
        this%d%nu_rel0 = cosmology_nu_rel(this%p, this%d, 1.0_real64)
        this%d%onu0 = this%d%ogamma0 * this%d%nu_rel0

        if (present(ode0)) then
            this%p%ode0 = ode0
            this%d%ok0 = 1.0_real64 - om0 - ode0 - this%d%ogamma0 - this%d%onu0
            this%flat = this%d%ok0 == 0.0_real64
        else
            ! Flat by omission: `Ok0` is set by ASSIGNMENT, never by subtraction, so `%is_flat()`
            ! is a bit test that cannot fail and the flat branch of `D_M` and `V_C` is exact.
            this%p%ode0 = 1.0_real64 - om0 - this%d%ogamma0 - this%d%onu0
            this%d%ok0 = 0.0_real64
            this%flat = .true.
        end if

        ! -- the ceiling on the DERIVED densities -------------------------------------------------
        !
        ! Stated here rather than on the arguments because `ogamma0` and `ok0` are derived: an
        ! absurd `tcmb0`, `neff` or `ode0` is caught by its EFFECT rather than by a ceiling
        ! invented for it. `nu_rel` is largest at z = 0, so one check at z = 0 bounds it
        ! everywhere. Every named cosmology has all four below 1.1.

        call check_density(this, "om0", this%p%om0, context)
        call check_density(this, "ode0", this%p%ode0, context)
        call check_density(this, "ok0", this%d%ok0, context)
        call check_density(this, "ogamma0 (1 + nu_rel(0))", &
                           this%d%ogamma0 * (1.0_real64 + this%d%nu_rel0), context)

        this%d%dh = pfc_c_kms / this%p%h0
        this%d%th = pfc_mpc_km / pfc_gyr_s / this%p%h0

        call cosmology_tabulate(this, context)
        this%ready = .true.

    end procedure cosmology_init_params

    !> Refuses a derived density above `PFC_DENSITY_CEILING` in magnitude.
    subroutine check_density(this, which, value, context)
        class(pf_cosmology), intent(in)        :: this    !! the cosmology being built
        character(len=*), intent(in)           :: which   !! the name to report
        real(real64), intent(in)               :: value   !! the derived value
        character(len=*), intent(in), optional :: context !! the caller's context

        if (.not. ieee_is_finite(value) .or. abs(value) > PFC_DENSITY_CEILING) then
            call cosmology_abort("pf_cosmology%init: every density parameter must be at most 1e6 in " // &
                                 "magnitude: " // which // " is " // shown(value), context)
        end if

    end subroutine check_density

    ! =========================================================================================
    ! The tabulation
    ! =========================================================================================

    !> Builds the five interpolants and the stored bounds.
    subroutine cosmology_tabulate(this, context)
        class(pf_cosmology), intent(inout)     :: this    !! the cosmology being built
        character(len=*), intent(in), optional :: context !! the caller's context

        type(cosmology_integrand) :: integ
        type(pf_tolerance)        :: tol
        real(real64), allocatable :: zeta(:), dist(:), time(:), age(:), fval(:), gval(:), aval(:)
        real(real64), allocatable :: inv_d(:), inv_t(:), step_t(:)
        real(real64)              :: tail, acc
        integer                   :: n, i, md, mt

        tol%rtol = PFC_RTOL
        tol%atol = 0.0_real64
        integ%p = this%p
        integ%d = this%d
        pfc_neval = 0

        n = max(PFC_MIN_INTERVALS, ceiling(log(1.0_real64 + this%p%zmax) / PFC_H))
        this%d%zeta_n = real(n, real64) * PFC_H
        allocate (zeta(n + 1), dist(n + 1), time(n + 1), age(n + 1), step_t(n))
        allocate (fval(n + 1), gval(n + 1), aval(n + 1), inv_d(n + 1), inv_t(n + 1))
        do i = 0, n
            zeta(i + 1) = real(i, real64) * PFC_H
        end do

        ! One set of interval integrals; the tables are their cumulative sums.
        dist(1) = 0.0_real64
        time(1) = 0.0_real64
        do i = 1, n
            dist(i + 1) = dist(i) + one_interval(this, integ, tol, PFC_INT_DISTANCE, &
                                                 zeta(i), zeta(i + 1), context)
            ! The `1/E` intervals are KEPT, not only accumulated: the age needs them individually.
            step_t(i) = one_interval(this, integ, tol, PFC_INT_TIME, zeta(i), zeta(i + 1), context)
            time(i + 1) = time(i) + step_t(i)
        end do

        ! The age: the BACKWARD cumulative sums of the SAME `1/E` intervals, plus the tail.
        this%age_diverges = age_integral_diverges(this)
        if (this%age_diverges) then
            this%d%age0 = ieee_value(this%d%age0, ieee_positive_inf)
        else
            tail = age_tail_integral(this, integ, tol, context)
            ! ACCUMULATED FROM THE TOP DOWN over the interval integrals themselves, never as
            ! `time(n + 1) - time(i)`. That difference is the cancellation this whole design
            ! exists to avoid, and it reappears here for a large `zmax`: by `zeta = 23` the
            ! lookback time has saturated, so both terms are the same double and every age node
            ! above that comes out identically equal to the tail. Summed this way each node is a
            ! sum of POSITIVE terms and its relative error is the interval tolerance.
            acc = tail
            age(n + 1) = this%d%th * acc
            do i = n, 1, -1
                acc = acc + step_t(i)
                age(i) = this%d%th * acc
            end do
            this%d%age0 = age(1)
        end if

        ! The SCALED functions: dividing out the zero at the origin is what makes the relative
        ! accuracy uniform down to z = 1e-8. `F(0)` and `G(0)` are the limits, since `E(0) = 1`.
        fval(1) = this%d%dh
        gval(1) = this%d%th
        inv_d(1) = 1.0_real64 / this%d%dh
        inv_t(1) = 1.0_real64 / this%d%th
        do i = 2, n + 1
            fval(i) = this%d%dh * dist(i) / zeta(i)
            gval(i) = this%d%th * time(i) / zeta(i)
            inv_d(i) = zeta(i) / (this%d%dh * dist(i))
            inv_t(i) = zeta(i) / (this%d%th * time(i))
        end do
        dist = this%d%dh * dist
        time = this%d%th * time

        call this%f%init(zeta, fval, method="cubic", bc="not_a_knot", outside="extrapolate", &
                         context="pf_cosmology%init: the comoving distance table")
        call this%g%init(zeta, gval, method="cubic", bc="not_a_knot", outside="extrapolate", &
                         context="pf_cosmology%init: the lookback time table")
        ! THE INVERSE TABLES GO ONLY AS FAR AS THEIR ABSCISSAE STRICTLY INCREASE. For a large
        ! `zmax` they stop short: by `zeta = 23` the lookback time has saturated, its interval
        ! integrals are `1e-18` against a total of order one, and consecutive nodes are the SAME
        ! double -- which `pf_interp_1d%init` rightly refuses. Truncating is not a loss of
        ! accuracy, because a quantity that no longer changes cannot determine a redshift: above
        ! the truncation the inverse falls through to the bracketed solve, which is what it would
        ! have done beyond the table anyway.
        md = strictly_increasing_prefix(dist)
        mt = strictly_increasing_prefix(time)
        call this%zd%init(dist(1:md), inv_d(1:md), method="cubic", bc="not_a_knot", &
                          outside="extrapolate", &
                          context="pf_cosmology%init: the comoving distance inverse")
        call this%zt%init(time(1:mt), inv_t(1:mt), method="cubic", bc="not_a_knot", &
                          outside="extrapolate", &
                          context="pf_cosmology%init: the lookback time inverse")
        this%d%d_inv_top = dist(md)
        this%d%t_inv_top = time(mt)
        this%d%zeta_d_inv = zeta(md)
        this%d%zeta_t_inv = zeta(mt)
        if (.not. this%age_diverges) then
            ! The age has no zero at the origin to divide out and spans four decades instead, so
            ! what is tabulated is its LOGARITHM: the same device, for the same reason.
            do i = 1, n + 1
                aval(i) = log(age(i))
            end do
            call this%a%init(zeta, aval, method="cubic", bc="not_a_knot", outside="extrapolate", &
                             context="pf_cosmology%init: the age table")
        end if

        this%d%d_n = dist(n + 1)
        this%d%t_n = time(n + 1)

        ! The four bounds the inverses screen against, computed once so each screen is one
        ! comparison rather than a walk.
        this%d%d_ceiling = this%d%d_n + this%d%dh &
                           * cosmology_walk(this%p, this%d, PFC_INT_DISTANCE, this%d%zeta_n, PFC_ZETA_CEILING)
        this%d%t_ceiling = this%d%t_n + this%d%th &
                           * cosmology_walk(this%p, this%d, PFC_INT_TIME, this%d%zeta_n, PFC_ZETA_CEILING)
        this%d%d_floor = this%d%dh &
                         * cosmology_walk(this%p, this%d, PFC_INT_DISTANCE, 0.0_real64, -PFC_ZETA_CEILING)
        this%d%t_floor = this%d%th &
                         * cosmology_walk(this%p, this%d, PFC_INT_TIME, 0.0_real64, -PFC_ZETA_CEILING)

        ! The age's own three bounds, which `%z_at_age` screens and brackets against. The age
        ! DECREASES with `zeta`, so `a_ceiling` -- the age AT the ceiling, as `d_ceiling` is the
        ! distance there -- is the SMALLEST age the domain attains and `a_floor` the largest.
        ! Below zero the age is `age(0) - t_L` with `t_L` negative, so those two terms ADD.
        if (this%age_diverges) then
            this%d%a_n = ieee_value(this%d%a_n, ieee_positive_inf)
            this%d%a_ceiling = this%d%a_n
            this%d%a_floor = this%d%a_n
        else
            this%d%a_n = age(n + 1)
            this%d%a_ceiling = this%d%th * cosmology_age_tail(this%p, this%d, PFC_ZETA_CEILING)
            this%d%a_floor = this%d%age0 - this%d%t_floor
        end if

    end subroutine cosmology_tabulate

    !> How many leading entries of `v` strictly increase; never fewer than four, which is what
    !! `not_a_knot` needs.
    !!
    !! A table that saturates is not an error: see the note at the call site.
    pure function strictly_increasing_prefix(v) result(m)
        real(real64), intent(in) :: v(:) !! the candidate abscissae, `v(1)` smallest
        integer                  :: m    !! the usable length

        integer :: i

        m = size(v)
        do i = 2, size(v)
            if (.not. v(i) > v(i - 1)) then
                m = i - 1
                exit
            end if
        end do
        m = max(m, min(size(v), PFC_MIN_INTERVALS + 1))

    end function strictly_increasing_prefix

    !> One interval integral, in units of `D_H` or `t_H`. Every status but `PF_INT_OK` aborts.
    function one_interval(this, integ, tol, which, a, b, context) result(v)
        class(pf_cosmology), intent(in)          :: this    !! the cosmology being built
        type(cosmology_integrand), intent(inout) :: integ   !! the integrand object
        type(pf_tolerance), intent(in)           :: tol     !! the tolerance
        integer, intent(in)                      :: which   !! which integrand
        real(real64), intent(in)                 :: a       !! the interval's lower edge in `zeta`
        real(real64), intent(in)                 :: b       !! its upper edge
        character(len=*), intent(in), optional   :: context !! the caller's context
        real(real64)                             :: v       !! the interval's integral

        type(pf_integration_info) :: info
        logical                   :: ok

        integ%which = which
        if (pfc_max_neval > 0) then
            v = pf_integrate(integ, a, b, tol, max_neval=pfc_max_neval, converged=ok, info=info)
        else
            v = pf_integrate(integ, a, b, tol, converged=ok, info=info)
        end if
        pfc_neval = pfc_neval + info%neval
        if (.not. ok) call report_interval(this, info, which, a, b, context)

    end function one_interval

    !> Turns a failed interval into the right message: a NaN integrand is "no big bang", anything
    !! else is a table that did not converge.
    subroutine report_interval(this, info, which, a, b, context)
        class(pf_cosmology), intent(in)        :: this    !! the cosmology being built
        type(pf_integration_info), intent(in)  :: info    !! what the integrator reported
        integer, intent(in)                    :: which   !! which integrand
        real(real64), intent(in)               :: a       !! the interval's lower edge
        real(real64), intent(in)               :: b       !! its upper edge
        character(len=*), intent(in), optional :: context !! the caller's context

        character(len=:), allocatable :: what

        if (info%status == PF_INT_BAD_VALUE) then
            ! `non_finite_at` is in the INTEGRATION variable, so it is converted back to a
            ! redshift before anyone reads it.
            call cosmology_abort("pf_cosmology%init: this cosmology has no big bang: E(z)^2 is not " // &
                                 "positive at z = " // shown(z_of_point(which, info%non_finite_at)), context)
        end if
        if (which == PFC_INT_DISTANCE) then
            what = "distance"
        else if (which == PFC_INT_TIME) then
            what = "lookback"
        else
            what = "age"
        end if
        call cosmology_abort("pf_cosmology%init: the " // what // " table did not converge on [" // &
                             shown(pf_zeta2z(a)) // ", " // shown(pf_zeta2z(b)) // "] (pf_integrate status " // &
                             shown(real(info%status, real64)) // ")", context)
        ! `this` is taken so that a future message may name the model; nothing reads it yet.
        if (this%ready) return

    end subroutine report_interval

    !> The redshift an integration point stands for: `zeta` for two of the integrands, `b` for the
    !! age tail, where the scale factor is `b^2`.
    function z_of_point(which, x) result(z)
        integer, intent(in)      :: which !! which integrand
        real(real64), intent(in) :: x     !! the point the integrator reported
        real(real64)             :: z     !! the redshift there

        if (which == PFC_INT_AGE_TAIL) then
            if (x > 0.0_real64) then
                z = 1.0_real64 / (x * x) - 1.0_real64
            else
                z = ieee_value(z, ieee_positive_inf)
            end if
        else
            z = pf_zeta2z(x)
        end if

    end function z_of_point

    !> Whether the age integral diverges, decided from the model rather than from a status.
    !!
    !! `age = t_H INT_0^a da'/(a' E)`. As `a -> 0` the integrand is `a^(p-1)` where `E ~ a^-p`:
    !! matter gives `p = 3/2`, curvature `p = 1`, radiation `p = 2`, and dark energy
    !! `p = 3(1 + w0 + wa)/2`. It diverges only when NOTHING grows, which is a predicate on the
    !! parameters, not a quadrature outcome -- so the de Sitter case never depends on which status
    !! the engine happens to report for an endpoint singularity. `age_tail_integral` still accepts
    !! `PF_INT_DIVERGENT` as a second opinion.
    function age_integral_diverges(this) result(yes)
        class(pf_cosmology), intent(in) :: this !! the cosmology being built
        logical                         :: yes  !! the age is infinite at every redshift

        yes = this%p%om0 == 0.0_real64 .and. this%d%ogamma0 == 0.0_real64 &
              .and. this%d%ok0 == 0.0_real64 &
              .and. 3.0_real64 * (1.0_real64 + this%p%w0 + this%p%wa) <= 0.0_real64

    end function age_integral_diverges

    !> The age past the table's edge, in units of `t_H`.
    !!
    !! Integrated in `b = sqrt(a)` rather than in the scale factor: the substitution is what makes
    !! the integrand polynomial-like at the origin in both regimes -- `2 b^3 / sqrt(Ogamma0)` with
    !! radiation, `2 b^2 / sqrt(Om0)` without -- where the plain `da/(a E)` form has a square-root
    !! corner that costs an adaptive rule its convergence. The range is finite, so the integrator
    !! never walks outward and never asks for `E` at a `zeta` that overflows.
    function age_tail_integral(this, integ, tol, context) result(v)
        class(pf_cosmology), intent(inout)       :: this    !! the cosmology being built
        type(cosmology_integrand), intent(inout) :: integ   !! the integrand object
        type(pf_tolerance), intent(in)           :: tol     !! the tolerance
        character(len=*), intent(in), optional   :: context !! the caller's context
        real(real64)                             :: v       !! the tail, in units of `t_H`

        type(pf_integration_info) :: info
        logical                   :: ok
        real(real64)              :: b_top

        integ%which = PFC_INT_AGE_TAIL
        b_top = exp(-0.5_real64 * this%d%zeta_n)
        if (pfc_max_neval > 0) then
            v = pf_integrate(integ, 0.0_real64, b_top, tol, max_neval=pfc_max_neval, converged=ok, info=info)
        else
            v = pf_integrate(integ, 0.0_real64, b_top, tol, converged=ok, info=info)
        end if
        pfc_neval = pfc_neval + info%neval
        if (.not. ok) then
            if (info%status == PF_INT_DIVERGENT) then
                ! Documented, not aborted, and right at every redshift: such a model is
                ! infinitely old throughout. `age_integral_diverges` normally reaches this first.
                this%age_diverges = .true.
                v = 0.0_real64
                return
            end if
            call report_interval(this, info, PFC_INT_AGE_TAIL, 0.0_real64, b_top, context)
        end if

    end function age_tail_integral

    ! =========================================================================================
    ! The object's lifetime
    ! =========================================================================================

    module procedure cosmology_clear

        ! Every component is reset EXPLICITLY. A default structure constructor `pf_cosmology()`
        ! would be rejected by ifx (error #6053), because `pf_interp_1d` has private components in
        ! another module.
        call this%f%clear()
        call this%g%clear()
        call this%a%clear()
        call this%zd%clear()
        call this%zt%clear()
        this%p = cosmology_params()
        this%d = cosmology_derived()
        if (allocated(this%label)) deallocate (this%label)
        this%flat = .false.
        this%massive_nu = .false.
        this%has_ob0 = .false.
        this%age_diverges = .false.
        this%ready = .false.

    end procedure cosmology_clear

    module procedure cosmology_get_name

        if (.not. this%ready) error stop "pf_cosmology%get_name: the cosmology is not initialised"
        name = this%label

    end procedure cosmology_get_name

    module procedure cosmology_m_nu

        if (.not. this%ready) error stop "pf_cosmology%m_nu: the cosmology is not initialised"
        allocate (masses(this%d%n_nu))
        masses = this%p%m_nu

    end procedure cosmology_m_nu

    module procedure cosmology_describe

        integer :: i

        if (.not. this%ready) error stop "pf_cosmology%describe: the cosmology is not initialised"
        text = this%label
        text = text // "; H0 = " // shown(this%p%h0) // " km/s/Mpc"
        text = text // "; Om0 = " // shown(this%p%om0)
        text = text // "; Ode0 = " // shown(this%p%ode0)
        if (this%flat) then
            text = text // "; Ok0 = 0 (flat)"
        else
            text = text // "; Ok0 = " // shown(this%d%ok0)
        end if
        text = text // "; Tcmb0 = " // shown(this%p%tcmb0) // " K"
        text = text // "; Neff = " // shown(this%p%neff)
        if (this%d%n_nu == 0) then
            text = text // "; m_nu = none"
        else
            text = text // "; m_nu = "
            do i = 1, this%d%n_nu
                if (i > 1) text = text // ", "
                text = text // shown(this%p%m_nu(i))
            end do
            text = text // " eV"
        end if
        if (this%has_ob0) then
            text = text // "; Ob0 = " // shown(this%p%ob0)
        else
            text = text // "; Ob0 = unknown"
        end if
        ! The pair is omitted entirely for a cosmological constant, which is every named cosmology.
        if (.not. (this%p%w0 == -1.0_real64 .and. this%p%wa == 0.0_real64)) then
            text = text // "; w0 = " // shown(this%p%w0)
            text = text // "; wa = " // shown(this%p%wa)
        end if

    end procedure cosmology_describe

    ! =========================================================================================
    ! The test-only hooks
    ! =========================================================================================

    module procedure parquet_debug_set_cosmology_max_neval
        pfc_max_neval = budget
    end procedure parquet_debug_set_cosmology_max_neval

    module procedure parquet_debug_cosmology_neval
        n = pfc_neval
    end procedure parquet_debug_cosmology_neval

end submodule parquet_cosmology_build
