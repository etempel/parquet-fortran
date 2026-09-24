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

    ! Every message below renders its numbers with `pf_to_str` into a LOCAL, and `capped` is a
    ! subroutine, because a FUNCTION returning `character(len=:), allocatable` is not thread-safe
    ! under gfortran: the result's hidden length is kept in a STATIC slot shared by every thread,
    ! read once to size the `realloc` and again to size the `memmove`. Two concurrent `%init`
    ! calls then take each other's length -- a truncated or over-long string, and a copy past the
    ! allocation. `%init` is called on several threads at once by design (`test_cosmology_omp`),
    ! and this shape had already produced an intermittent wrong `%get_name`. See
    ! `.claude/rules/fortran-gotchas.md`, "Never write a function returning
    ! `character(len=:), allocatable`".

    ! =========================================================================================
    ! `%init`
    ! =========================================================================================

    module procedure cosmology_init_named

        integer :: which
        character(len=len(name)) :: folded
        integer :: i
        character(len=:), allocatable :: shown_name

        ! `adjustl` before the fold, so that LEADING blanks are ignored as trailing ones already
        ! are: `init("  Planck18")` and `init("Planck18  ")` name the same cosmology.
        folded = adjustl(name)
        call pf_to_lower(folded)
        which = 0
        do i = 1, PFC_N_NAMED
            if (trim(folded) == trim(lowered(pfc_named_name(i)))) then
                which = i
                exit
            end if
        end do
        if (which == 0) then
            call capped(name, shown_name)
            call cosmology_abort("pf_cosmology%init: unknown cosmology """ // shown_name // """;" // &
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
                                   zmax = zmax, zmin = zmin, context = context)

    end procedure cosmology_init_named

    !> `text` folded to lower case, for the case-insensitive name match.
    function lowered(text) result(out)
        character(len=*), intent(in) :: text !! the text to fold
        character(len=len(text))     :: out  !! the folded copy

        out = text
        call pf_to_lower(out)

    end function lowered

    !> Caller text capped for a message.
    subroutine capped(text, out)
        character(len=*), intent(in)               :: text !! the caller's text
        character(len=:), allocatable, intent(out) :: out  !! at most `PFC_CONTEXT_CAP` characters plus "..."

        if (len_trim(text) > PFC_CONTEXT_CAP) then
            out = text(1:PFC_CONTEXT_CAP) // "..."
        else
            out = trim(text)
        end if

    end subroutine capped

    module procedure cosmology_init_params

        real(real64) :: h0_si, rho_crit0, rho_gamma0
        integer      :: i
        character(len=:), allocatable :: num, num2

        ! -- the arguments, in the order section 5.4 of feature_cosmology.md tabulates them ----

        if (.not. ieee_is_finite(h0) .or. h0 < PFC_H0_MIN .or. h0 > PFC_H0_MAX) then
            call pf_to_str(h0, num)
            call cosmology_abort("pf_cosmology%init: h0 must be finite and within " // &
                                 "[1e-10, 1e10] km/s/Mpc, got " // num, context)
        end if
        if (.not. ieee_is_finite(om0) .or. om0 < 0.0_real64) then
            call pf_to_str(om0, num)
            call cosmology_abort("pf_cosmology%init: om0 must be finite and non-negative, got " // &
                                 num, context)
        end if
        if (present(ode0)) then
            if (.not. ieee_is_finite(ode0)) then
                call pf_to_str(ode0, num)
                call cosmology_abort("pf_cosmology%init: ode0 must be finite, got " // num, context)
            end if
        end if
        this%p%tcmb0 = 0.0_real64
        if (present(tcmb0)) then
            if (.not. ieee_is_finite(tcmb0) .or. tcmb0 < 0.0_real64) then
                call pf_to_str(tcmb0, num)
                call cosmology_abort("pf_cosmology%init: tcmb0 must be finite and non-negative, got " // &
                                     num, context)
            end if
            this%p%tcmb0 = tcmb0
        end if
        this%p%neff = 3.04_real64
        if (present(neff)) then
            if (.not. ieee_is_finite(neff) .or. neff < 0.0_real64) then
                call pf_to_str(neff, num)
                call cosmology_abort("pf_cosmology%init: neff must be finite and non-negative, got " // &
                                     num, context)
            end if
            this%p%neff = neff
        end if
        this%d%n_nu = int(floor(this%p%neff))
        if (present(m_nu)) then
            if (size(m_nu) /= this%d%n_nu) then
                call pf_to_str(real(this%d%n_nu, real64), num)
                call pf_to_str(real(size(m_nu), real64), num2)
                call cosmology_abort("pf_cosmology%init: m_nu needs one finite, non-negative mass per " // &
                                     "species: floor(neff) = " // num // &
                                     ", got " // num2 // " entries", context)
            end if
            do i = 1, size(m_nu)
                if (.not. ieee_is_finite(m_nu(i)) .or. m_nu(i) < 0.0_real64) then
                    call pf_to_str(real(i, real64), num)
                    call pf_to_str(m_nu(i), num2)
                    call cosmology_abort("pf_cosmology%init: m_nu needs one finite, non-negative mass " // &
                                         "per species: entry " // num // " is " // &
                                         num2, context)
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
                call pf_to_str(ob0, num)
                call cosmology_abort("pf_cosmology%init: ob0 must be finite, non-negative and at most " // &
                                     "om0, got " // num, context)
            end if
            this%p%ob0 = ob0
        end if
        this%p%w0 = -1.0_real64
        if (present(w0)) then
            if (.not. ieee_is_finite(w0) .or. abs(w0) > PFC_W_LIMIT) then
                call pf_to_str(w0, num)
                call cosmology_abort("pf_cosmology%init: w0 must be finite and within [-3, 3], got " // &
                                     num, context)
            end if
            this%p%w0 = w0
        end if
        this%p%wa = 0.0_real64
        if (present(wa)) then
            if (.not. ieee_is_finite(wa) .or. abs(wa) > PFC_W_LIMIT) then
                call pf_to_str(wa, num)
                call cosmology_abort("pf_cosmology%init: wa must be finite and within [-3, 3], got " // &
                                     num, context)
            end if
            this%p%wa = wa
        end if
        this%p%zmax = PFC_DEFAULT_ZMAX
        if (present(zmax)) then
            if (.not. ieee_is_finite(zmax) .or. zmax <= 0.0_real64 .or. zmax > PFC_Z_CEILING) then
                call pf_to_str(zmax, num)
                call cosmology_abort("pf_cosmology%init: zmax must be finite, positive and at most " // &
                                     "1e10, got " // num, context)
            end if
            this%p%zmax = zmax
        end if
        this%p%zmin = PFC_DEFAULT_ZMIN
        if (present(zmin)) then
            if (.not. ieee_is_finite(zmin) .or. zmin <= -1.0_real64 .or. zmin > 0.0_real64) then
                call pf_to_str(zmin, num)
                call cosmology_abort("pf_cosmology%init: zmin must be finite and within " // &
                                     "(-1, 0], got " // num, context)
            end if
            this%p%zmin = zmin
        end if
        this%p%h0 = h0
        this%p%om0 = om0
        if (present(name)) then
            call capped(name, this%label)
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

        call sound_scales(this)

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

        character(len=:), allocatable :: num

        if (.not. ieee_is_finite(value) .or. abs(value) > PFC_DENSITY_CEILING) then
            call pf_to_str(value, num)
            call cosmology_abort("pf_cosmology%init: every density parameter must be at most 1e6 in " // &
                                 "magnitude: " // which // " is " // num, context)
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
        real(real64), allocatable :: absd(:), xval(:), dxval(:)
        real(real64), allocatable :: dfval(:), dgval(:), daval(:)
        real(real64), allocatable :: inv_d(:), inv_t(:), step_t(:)
        real(real64)              :: tail, acc, d_low, t_low, step_d, step_x, e_prime0, inv_e
        integer                   :: n, m, m_ok, lost, nt, zero, i, md, mt, ma
        integer                   :: md_lo, mt_lo, ma_hi

        tol%rtol = PFC_RTOL
        tol%atol = 0.0_real64
        pfc_neval = 0

        ! The model's own bottom, found BEFORE anything is integrated: the blueshift bounds are
        ! taken at it, and nothing below it is evaluated at all.
        this%d%zeta_floor = cosmology_find_floor(this%p, this%d)
        integ%p = this%p
        integ%d = this%d

        n = max(PFC_MIN_INTERVALS, ceiling(log(1.0_real64 + this%p%zmax) / PFC_H))
        ! **The grid is the INTEGER LATTICE times `PFC_H`, from `-m` to `n`.** Taking the nodes
        ! from the lattice rather than from the two ends is what puts a node at `zeta = 0`
        ! EXACTLY: the spacing stays uniform across the origin, `%comoving_distance(0)` stays
        ! exactly zero, and the scaled functions' stored limits at the origin are node values
        ! rather than something an interpolant reconstructs.
        !
        ! `m` is what the caller asked for, ROUNDED UP to a node and then CLAMPED by the model's
        ! own floor, so that no node and no interval reaches a `zeta` at which `E^2` is not
        ! positive. A model whose blueshift half ends early therefore tabulates only as far as it
        ! exists, and `%init` never aborts for it -- below the last node every binding falls
        ! through to the panel walk, which answers the NaN that redshift deserves.
        !
        ! **The clamp keeps a whole PANEL clear of the floor, not just a node.** Where `E^2`
        ! vanishes linearly the tabulated functions have a square-root branch point, whose sixth
        ! derivative -- the one the quintic's error is proportional to -- diverges there. Measured
        ! on the `Tcmb0 = 20 K` model, whose derived `Ode0` is slightly negative and whose floor
        ! is at `zeta = -1.054`: interpolating up to a node a fifth of a panel above it answers
        ! `6.4e-9` relative where the panel walk over the same point answers `3.7e-10`, and the
        ! table only becomes the better of the two about one panel out. So the last panel before
        ! the floor is left to the walk, which is what answered it before any of it was tabulated.
        m = ceiling(-log(1.0_real64 + this%p%zmin) / PFC_H)
        if (m < 0) m = 0
        m = min(m, max(0, int((-this%d%zeta_floor - PFC_PANEL) / PFC_H)))
        this%d%zeta_n = real(n, real64) * PFC_H
        allocate (zeta(n + m + 1), dist(n + m + 1), time(n + m + 1), age(n + m + 1), step_t(n + m))
        allocate (fval(n + m + 1), gval(n + m + 1), aval(n + m + 1))
        allocate (absd(n + m + 1), xval(n + m + 1), dxval(n + m + 1))
        allocate (dfval(n + m + 1), dgval(n + m + 1), daval(n + m + 1))
        allocate (inv_d(n + m + 1), inv_t(n + m + 1))
        do i = -m, n
            zeta(i + m + 1) = real(i, real64) * PFC_H
        end do
        zero = m + 1

        ! **The Komatsu fit, tabulated for the QUADRATURE only.** It is two `pow` calls per
        ! massive species and about 45% of a default build; the table is value plus analytic slope
        ! at every node of this same grid, read by the same quintic the distance table uses, and
        ! the flag that switches it on is set on the INTEGRAND's copy of the derived record and
        ! never on the object's -- so no per-query path reads it and `E(z)` stays astropy's
        ! formula evaluated exactly. A model with no massive species has a CONSTANT fit and gains
        ! nothing, so it gets no table.
        if (.not. pfc_exact_nu .and. this%massive_nu .and. this%d%ogamma0 /= 0.0_real64) then
            allocate (integ%d%nu_tab(n + m + 1), integ%d%nu_slope(n + m + 1))
            do i = 1, n + m + 1
                integ%d%nu_tab(i) = cosmology_nu_rel(this%p, this%d, exp(zeta(i)))
                integ%d%nu_slope(i) = cosmology_dnu_rel(this%p, this%d, exp(zeta(i)))
            end do
            integ%d%nu_zeta0 = zeta(1)
            integ%d%nu_zeta1 = zeta(n + m + 1)
            integ%d%nu_n = n + m + 1
            integ%d%use_nu_tab = .true.
        end if

        ! One set of interval integrals; the tables are their cumulative sums. The two halves are
        ! accumulated OUTWARD FROM THE ORIGIN, never across it, so every node is a sum of
        ! same-signed terms and the blueshift half never differences two positive totals.
        dist(zero) = 0.0_real64
        time(zero) = 0.0_real64
        absd(zero) = 0.0_real64
        do i = zero, n + m
            dist(i + 1) = dist(i) + one_interval(this, integ, tol, PFC_INT_DISTANCE, &
                                                 zeta(i), zeta(i + 1), context)
            ! The `1/E` intervals are KEPT, not only accumulated: the age needs them individually.
            step_t(i) = one_interval(this, integ, tol, PFC_INT_TIME, zeta(i), zeta(i + 1), context)
            time(i + 1) = time(i) + step_t(i)
            ! The FOURTH integral, `e^(3 zeta)/E`, whose cumulative sum is the absorption
            ! distance. It is not a combination of the other two, so it is integrated beside them
            ! -- about half as much work again, which N4 of the review's plan accepted rather than
            ! leave one binding a hundred times slower than its neighbours.
            absd(i + 1) = absd(i) + one_interval(this, integ, tol, PFC_INT_ABSORPTION, &
                                                 zeta(i), zeta(i + 1), context)
        end do
        ! **Downward the intervals STOP rather than abort** (the blueshift half is a convenience,
        ! not a contract): an interval that does not converge, or that returns a NaN, ends the
        ! table there and the nodes below it are dropped. Nothing else in `%init` may be lenient
        ! this way -- an interval above zero that fails is a model the caller asked for and did
        ! not get -- which is why this loop does not go through `one_interval`.
        m_ok = 0
        do i = zero - 1, 1, -1
            if (.not. blueshift_interval(integ, tol, zeta(i), zeta(i + 1), step_d, step_t(i), &
            ! `m` is clamped so the table's bottom node stands a whole `PFC_PANEL` clear of the
            ! model's floor, and a panel reaching the floor is the only thing that makes a
            ! blueshift interval fail to converge. A starved evaluation budget does not reach it
            ! either: the redshift half is integrated first and aborts first, probed from 100 to
            ! 12800 evaluations over every corner model with a floor inside the domain.
            ! GCOVR_EXCL_START
                                         step_x)) exit
            ! GCOVR_EXCL_STOP
            dist(i) = dist(i + 1) - step_d
            time(i) = time(i + 1) - step_t(i)
            absd(i) = absd(i + 1) - step_x
            m_ok = zero - i
        end do
        if (m_ok < m) then
            ! Re-base the arrays on the nodes that survived, so that index 1 is the table's bottom
            ! for everything below.
            ! the nodes below a failed blueshift interval are what this re-bases away, and no
            ! interval fails; see the `exit` above.
            ! GCOVR_EXCL_START
            lost = m - m_ok
            zeta(1:n + m_ok + 1) = zeta(lost + 1:n + m + 1)
            dist(1:n + m_ok + 1) = dist(lost + 1:n + m + 1)
            time(1:n + m_ok + 1) = time(lost + 1:n + m + 1)
            absd(1:n + m_ok + 1) = absd(lost + 1:n + m + 1)
            step_t(1:n + m_ok) = step_t(lost + 1:n + m)
            m = m_ok
            zero = m + 1
            ! GCOVR_EXCL_STOP
        end if
        this%d%zeta_m = zeta(1)

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
            age(n + m + 1) = this%d%th * acc
            do i = n + m, 1, -1
                acc = acc + step_t(i)
                age(i) = this%d%th * acc
            end do
            this%d%age0 = age(zero)
        end if

        ! The SCALED functions: dividing out the zero at the origin is what makes the relative
        ! accuracy uniform down to z = 1e-8. `F(0)` and `G(0)` are the limits, since `E(0) = 1`.
        ! They carry through to the blueshift half unchanged: `D_C` and `zeta` change sign
        ! together, so `F = D_C/zeta` is positive and smooth ACROSS the origin rather than merely
        ! up to it.
        !
        ! **Each node carries a SLOPE beside its value**, which is what makes the interpolant a
        ! quintic rather than a cubic. The unscaled slopes are closed form --
        ! `dD_C/dzeta = D_H e^zeta/E` and `dt_L/dzeta = t_H/E` -- so a node costs one more `E`,
        ! about two per cent of the build. The scaled slope follows from `D_C = zeta F`:
        ! `F' = (D_C' - F)/zeta`, a difference that cancels as `zeta -> 0`, so the node AT the
        ! origin takes the limit of the series instead. `D_C/D_H = g(0) zeta + g'(0) zeta^2/2 +
        ! ...` with `g = e^zeta/E`, so `F(0) = D_H g(0) = D_H` and `F'(0) = D_H g'(0)/2 =
        ! D_H (1 - E'(0))/2`; the lookback time's `q = 1/E` gives `G'(0) = -t_H E'(0)/2` the same
        ! way. `E'(0)` is `dE^2/dzeta` at the origin over twice `E(0) = 1`.
        e_prime0 = 0.5_real64 * cosmology_de2_dzeta(this%p, this%d, 0.0_real64)
        fval(zero) = this%d%dh
        gval(zero) = this%d%th
        ! The absorption distance's integrand is `1/E` at the origin, so its scaled limit is one.
        xval(zero) = 1.0_real64
        dxval(zero) = 0.5_real64 * (3.0_real64 - e_prime0)
        inv_d(zero) = 1.0_real64 / this%d%dh
        inv_t(zero) = 1.0_real64 / this%d%th
        do i = 1, n + m + 1
            if (i == zero) cycle
            fval(i) = this%d%dh * dist(i) / zeta(i)
            gval(i) = this%d%th * time(i) / zeta(i)
            xval(i) = absd(i) / zeta(i)
            inv_d(i) = zeta(i) / (this%d%dh * dist(i))
            inv_t(i) = zeta(i) / (this%d%th * time(i))
        end do
        do i = 1, n + m + 1
            call node_slopes(this, zeta(i), fval(i), gval(i), dfval(i), dgval(i), inv_e)
            if (.not. this%age_diverges) daval(i) = -this%d%th * inv_e / age(i)
            if (zeta(i) /= 0.0_real64) &
                dxval(i) = (exp(zeta(i)) ** 3 * inv_e - xval(i)) / zeta(i)
        end do
        ! The node at the origin, where the scaled slope's difference cancels, takes the limit.
        dfval(zero) = 0.5_real64 * this%d%dh * (1.0_real64 - e_prime0)
        dgval(zero) = -0.5_real64 * this%d%th * e_prime0
        dist = this%d%dh * dist
        time = this%d%th * time

        nt = n + m + 1
        this%d%n_tab = nt
        this%fv = fval(1:nt)
        this%fd = dfval(1:nt)
        this%gv = gval(1:nt)
        this%gd = dgval(1:nt)
        this%xv = xval(1:nt)
        this%xd = dxval(1:nt)
        ! THE INVERSE TABLES GO ONLY AS FAR AS THEIR ABSCISSAE STRICTLY INCREASE. For a large
        ! `zmax` they stop short: by `zeta = 23` the lookback time has saturated, its interval
        ! integrals are `1e-18` against a total of order one, and consecutive nodes are the SAME
        ! double -- which `pf_interp_1d%init` rightly refuses. Truncating is not a loss of
        ! accuracy, because a quantity that no longer changes cannot determine a redshift: above
        ! the truncation the inverse falls through to the bracketed solve, which is what it would
        ! have done beyond the table anyway.
        ! The run is taken AROUND THE ORIGIN and can stop short at EITHER end. At the top it is
        ! the lookback time that saturates, as the note above says. At the bottom it is the
        ! distance, for a model whose `E` diverges towards `z = -1`: a big rip's `e^zeta/E` falls
        ! away faster than any power, so with a deep `zmin` its lowest nodes carry the same double
        ! and `pf_interp_1d%init` rightly refuses them. `Planck18` with `zmin = -0.9999999999` is
        ! the reachable case, and before the run was taken from the origin outward it aborted
        ! `%init` for a model that is perfectly well defined.
        call increasing_run(dist(1:nt), zero, md_lo, md)
        call increasing_run(time(1:nt), zero, mt_lo, mt)
        this%d%has_d_inv = md - md_lo + 1 >= PFC_MIN_INTERVALS + 1
        this%d%has_t_inv = mt - mt_lo + 1 >= PFC_MIN_INTERVALS + 1
        if (this%d%has_d_inv) then
            call this%zd%init(dist(md_lo:md), inv_d(md_lo:md), method="cubic", bc="not_a_knot", &
                              outside="extrapolate", &
                              context="pf_cosmology%init: the comoving distance inverse")
            this%d%d_inv_top = dist(md)
            this%d%zeta_d_inv = zeta(md)
            this%d%d_inv_bot = dist(md_lo)
            this%d%zeta_d_bot = zeta(md_lo)
        end if
        if (this%d%has_t_inv) then
            call this%zt%init(time(mt_lo:mt), inv_t(mt_lo:mt), method="cubic", bc="not_a_knot", &
                              outside="extrapolate", &
                              context="pf_cosmology%init: the lookback time inverse")
            this%d%t_inv_top = time(mt)
            this%d%zeta_t_inv = zeta(mt)
            this%d%t_inv_bot = time(mt_lo)
            this%d%zeta_t_bot = zeta(mt_lo)
        end if
        if (.not. this%age_diverges) then
            ! The age has no zero at the origin to divide out and spans four decades instead, so
            ! what is tabulated is its LOGARITHM: the same device, for the same reason. Its slope
            ! is closed form too -- `d(ln age)/dzeta = -(t_H/E)/age`, because `age + t_L` does not
            ! depend on `zeta` -- and needs no limit at the origin, the age having no zero there.
            do i = 1, nt
                aval(i) = log(age(i))
            end do
            this%av = aval(1:nt)
            this%ad = daval(1:nt)
            ! THE AGE'S OWN INVERSE, `zeta` against `-ln(age)`, which increases with `zeta` as the
            ! age falls. No scaling: the age has no zero to divide out, so the abscissa is the
            ! quantity itself and the ordinate is `zeta`.
            !
            ! It is the BOTTOM end this one can lose, where `zd` and `zt` lose the top: towards
            ! `z = -1` a model whose dark energy grows without bound stops ageing at all -- its
            ! `1/E` intervals underflow the sum -- so consecutive nodes carry the same age and
            ! `pf_interp_1d%init` rightly refuses them. Below the truncation `%z_at_age` falls
            ! through to the bracketed solve, which is what it would have done anyway.
            do i = 1, nt
                aval(i) = -aval(i)
            end do
            call increasing_run(aval(1:nt), zero, ma, ma_hi)
            if (ma_hi - ma + 1 >= PFC_MIN_INTERVALS + 1) then
                call this%za%init(aval(ma:ma_hi), zeta(ma:ma_hi), method="cubic", bc="not_a_knot", &
                                  outside="extrapolate", &
                                  context="pf_cosmology%init: the age inverse")
                this%d%a_inv_bot = aval(ma)
                this%d%a_inv_top = aval(ma_hi)
                this%d%zeta_a_bot = zeta(ma)
                this%d%zeta_a_top = zeta(ma_hi)
                this%d%has_age_inv = .true.
            end if
        end if

        this%d%d_n = dist(nt)
        this%d%t_n = time(nt)
        this%d%d_m = dist(1)
        this%d%t_m = time(1)
        this%d%x_n = absd(nt)
        this%d%x_m = absd(1)

        ! The four bounds the inverses screen against, computed once so each screen is one
        ! comparison rather than a walk. The two at the CEILING are the fixed panel walk's, whose
        ! integrand is smooth there; the two at the FLOOR are not, because a model whose `E^2`
        ! vanishes at `zeta_floor` leaves an inverse-square-root endpoint no fixed rule resolves.
        this%d%d_ceiling = this%d%d_n + this%d%dh &
                           * cosmology_walk(this%p, this%d, PFC_INT_DISTANCE, this%d%zeta_n, PFC_ZETA_CEILING)
        this%d%t_ceiling = this%d%t_n + this%d%th &
                           * cosmology_walk(this%p, this%d, PFC_INT_TIME, this%d%zeta_n, PFC_ZETA_CEILING)
        ! The two at the floor start from the table's BOTTOM node and its tabulated values, so
        ! that the part of the range the table already covers is the table's own answer and only
        ! what lies below it is integrated again.
        call floor_bounds(integ, this%d%zeta_m, this%d%zeta_floor, d_low, t_low, this%d%zeta_bottom)
        this%d%d_floor = this%d%d_m + this%d%dh * d_low
        this%d%t_floor = this%d%t_m + this%d%th * t_low

        ! The age's own three bounds, which `%z_at_age` screens and brackets against. The age
        ! DECREASES with `zeta`, so `a_ceiling` -- the age AT the ceiling, as `d_ceiling` is the
        ! distance there -- is the SMALLEST age the domain attains and `a_floor` the largest.
        ! Below zero the age is `age(0) - t_L` with `t_L` negative, so those two terms ADD.
        if (this%age_diverges) then
            this%d%a_n = ieee_value(this%d%a_n, ieee_positive_inf)
            this%d%a_ceiling = this%d%a_n
            this%d%a_floor = this%d%a_n
        else
            this%d%a_n = age(nt)
            this%d%a_ceiling = this%d%th * cosmology_age_tail(this%p, this%d, PFC_ZETA_CEILING)
            this%d%a_floor = this%d%age0 - this%d%t_floor
        end if

        call growth_pass(this)

    end subroutine cosmology_tabulate

    !> `R0` and the sound horizon's integration scale, both fixed by the parameters alone.
    !!
    !! **`sound_c` is what makes a FIXED rule work for every admitted model.** The integrand of
    !! `cosmology_sound_tail` carries two factors of the form `1 / sqrt(1 + (b/s)^2)`: the
    !! baryon-photon one at `s = 1/sqrt(R0)`, and the matter-radiation one at `s = sqrt(Or0/Om0)`,
    !! which is `sqrt(a_eq)`. Each puts a branch point at `b = i s`, so a Gauss panel wider than
    !! the SMALLER of the two converges slowly however analytic the integrand is on the real axis.
    !! Both scales run with the model: `1/sqrt(R0)` is `0.017` for `Planck18` and `2.3e-05` at
    !! `Tcmb0 = 0.1 K`, which the module admits. Substituting `b = sound_c sinh(v)` with
    !! `sound_c` the smaller of the two moves EVERY one of those branch points to an imaginary
    !! part of at least `pi/2` in `v`, whatever the model, because `asinh(i y)` has imaginary part
    !! `arcsin(y)` for `y <= 1` and exactly `pi/2` for `y > 1`. That bound is what
    !! `PFC_SOUND_PANEL` is chosen against.
    !!
    !! A model with neither baryons nor matter has neither scale and needs no substitution; there
    !! `sound_c` is one, and `b = sinh(v)` is a harmless change of variable.
    subroutine sound_scales(this)
        class(pf_cosmology), intent(inout) :: this !! the cosmology being built

        real(real64) :: or0, b_eq

        this%d%sound_r0_root = ieee_value(this%d%sound_r0_root, ieee_quiet_nan)
        this%d%sound_c = 1.0_real64
        ! No photons, or no baryon density given: `%sound_horizon` never reaches the integrand,
        ! so neither scale is formed and the division by a zero `Ogamma0` never happens. Under
        ! nagfor's default `-ieee=stop` that division would end the process, not raise a flag.
        if (this%d%ogamma0 <= 0.0_real64) return
        if (.not. this%has_ob0) return

        ! `sqrt(R0)` in one step: `3 Ob0 / (4 Ogamma0)` overflows for an `Ogamma0` below about
        ! `1e-300`, which `Tcmb0` may reach, and the ratio of the two roots never does.
        this%d%sound_r0_root = sqrt(3.0_real64 * this%p%ob0) / sqrt(4.0_real64 * this%d%ogamma0)
        or0 = this%d%ogamma0 * (1.0_real64 + pfc_komatsu_a * this%p%neff)
        if (this%d%sound_r0_root > 0.0_real64) this%d%sound_c = 1.0_real64 / this%d%sound_r0_root
        if (this%p%om0 > 0.0_real64 .and. or0 > 0.0_real64) then
            ! The ratio of the two roots, never the root of the ratio, for the same reason
            ! `sound_r0_root` above is formed that way: `or0 / om0` overflows for a subnormal
            ! `om0` beside an ordinary radiation density, and the overflow reaches the CALLER as
            ! a raised `IEEE_OVERFLOW` -- which ends the process under nagfor's default
            ! `-ieee=stop` -- even though the infinity it produces is caught below. Each root on
            ! its own is `1e3` at most over `2.2e-162` at least, so this cannot overflow.
            b_eq = sqrt(or0) / sqrt(this%p%om0)
            ! `ob0 = 0` leaves `R0 = 0` and no baryon scale at all, so equality is the only one.
            if (this%d%sound_r0_root <= 0.0_real64 .or. b_eq < this%d%sound_c) this%d%sound_c = b_eq
        end if
        ! A scale that is not a positive finite number resolves nothing, and `sound_c sinh(v)`
        ! must stay a number. Both scales are now formed as a ratio of roots, so neither can
        ! overflow and no admitted model reaches the substitution below; it is kept because the
        ! alternative to a scale of one is an infinity at every node and an integral that is a
        ! NaN, which is not a failure worth leaving to a future edit of either formula.
        if (.not. (this%d%sound_c > 0.0_real64 .and. this%d%sound_c < huge(1.0_real64))) &
            ! GCOVR_EXCL_START
            this%d%sound_c = 1.0_real64
        ! GCOVR_EXCL_STOP

    end subroutine sound_scales

    !> The linear growth pair, integrated DOWNWARD onto the same integer lattice.
    !!
    !! **The one table `zmax` does not move.** The growing mode is an attractor in the direction
    !! of time and only in that direction, so the pass starts at `PFC_GROWTH_TOP_NODE` -- the
    !! first node above the domain's ceiling -- whatever the caller asked to tabulate, and there
    !! is no fallback above it to write. Integrated the other way the same equation amplifies its
    !! own rounding by `2.4e-07` at `z = 1100` and by a factor of 25 at `z = 1e10`.
    !!
    !! **The initial condition is exact rather than approximate.** Deep in the past every model is
    !! matter plus radiation -- dark energy and curvature are below `1e-30` of `E^2` at
    !! `a = 1e-10` -- and there the growing mode is the Meszaros (1974) solution `D = 1 + (3/2) y`
    !! with `y = a / a_eq`, so `f = (3/2)y / (1 + (3/2)y)`. `Or0` is the RELATIVISTIC radiation
    !! density, `Ogamma0 (1 + 0.2271 Neff)`, because at `a = 1e-10` every species is relativistic
    !! however massive it is today. Moving the starting redshift from `1e10` to `1e14` moves the
    !! normalised `D` by at most `7.1e-13` anywhere in the domain; the pure-matter mode `D = a`
    !! imposed at the same place moves it by `1.9e-04`.
    !!
    !! **`D(0) = 1` is an anchor, not a division.** The table carries `u = lnD + zeta` -- the
    !! departure from matter domination, the same device as `D_C/zeta` and `ln(age)` -- accumulated
    !! from the top with an arbitrary offset, and the node AT the origin is subtracted from every
    !! node afterwards, which the integer lattice makes an exact node rather than something an
    !! interpolant reconstructs. `u(0) = 0` then makes `D(0)` exactly one.
    subroutine growth_pass(this)
        class(pf_cosmology), intent(inout) :: this !! the cosmology being built

        real(real64), allocatable :: tmp(:)
        real(real64) :: f, u, q0, s0, hs, z0, zt, or0, y, anchor
        integer      :: i, j, nodes, m, zero, bottom

        this%has_growth = .false.
        this%d%n_growth = 0
        ! A universe with no matter has no matter perturbations to grow. The Riccati equation is
        ! not singular there -- it would answer `f = 0` and `D = 1` everywhere, a number that
        ! means nothing -- so no table is built and both bindings answer a quiet NaN. de Sitter
        ! and Milne take this path.
        if (this%p%om0 <= 0.0_real64) return

        m = nint(-this%d%zeta_m / PFC_H)
        nodes = PFC_GROWTH_TOP_NODE + m + 1
        zero = m + 1
        allocate (this%gwv(nodes), this%gwd(nodes))
        zt = real(PFC_GROWTH_TOP_NODE, real64) * PFC_H

        or0 = this%d%ogamma0 * (1.0_real64 + pfc_komatsu_a * this%p%neff)
        if (or0 > 0.0_real64) then
            y = 1.5_real64 * (this%p%om0 / or0) * exp(-zt)
            f = y / (1.0_real64 + y)
        else
            ! No radiation at all: the same expression's limit, without the division.
            f = 1.0_real64
        end if
        u = 0.0_real64
        this%gwv(nodes) = u
        this%gwd(nodes) = 1.0_real64 - f

        hs = PFC_H / real(PFC_GROWTH_SUBSTEPS, real64)
        call cosmology_growth_coef(this%p, this%d, zt, q0, s0)
        bottom = nodes
        do i = nodes - 1, 1, -1
            ! The substeps start from the node ABOVE, formed from the lattice rather than
            ! accumulated, so 6000 steps do not walk the grid off its own nodes.
            z0 = real(i - m, real64) * PFC_H
            do j = 1, PFC_GROWTH_SUBSTEPS
                call cosmology_growth_step(this%p, this%d, z0 - real(j - 1, real64) * hs, -hs, &
                                           q0, s0, f, u)
            end do
            ! A model whose `E^2` is not positive, or has overflowed, at the node below ends the
            ! table there: the NaN would otherwise propagate up nothing and down everything.
            if (f /= f) exit
            this%gwv(i) = u
            this%gwd(i) = 1.0_real64 - f
            bottom = i
        end do

        ! Without the node at the origin there is nothing to anchor to, so there is no table.
        if (bottom > zero) then
            ! the walk can only stop ABOVE the origin where `E^2` is not positive at a `zeta`
            ! greater than zero, and `%init` has already refused such a model with `this cosmology
            ! has no big bang` while building the distance table.
            ! GCOVR_EXCL_START
            deallocate (this%gwv, this%gwd)
            return
            ! GCOVR_EXCL_STOP
        end if
        anchor = this%gwv(zero)
        do i = bottom, nodes
            this%gwv(i) = this%gwv(i) - anchor
        end do
        if (bottom > 1) then
            tmp = this%gwv(bottom:nodes)
            call move_alloc(tmp, this%gwv)
            tmp = this%gwd(bottom:nodes)
            call move_alloc(tmp, this%gwd)
        end if
        this%d%n_growth = nodes - bottom + 1
        this%d%zeta_gw_m = real(bottom - 1 - m, real64) * PFC_H
        this%has_growth = .true.

    end subroutine growth_pass

    !> The scaled tables' slopes at one node, and `1/E` there for the age's own slope.
    !!
    !! One `E` evaluation per node, which is the whole extra cost of the quintic. `F' = (D_C' -
    !! F)/zeta` is a difference that cancels as `zeta -> 0`, so the caller overwrites the node AT
    !! the origin with the limit rather than asking for it here.
    subroutine node_slopes(this, zeta, fval, gval, dfval, dgval, inv_e)
        class(pf_cosmology), intent(in) :: this  !! the cosmology being built
        real(real64), intent(in)        :: zeta  !! the node
        real(real64), intent(in)        :: fval  !! `D_C(zeta)/zeta` there
        real(real64), intent(in)        :: gval  !! `t_L(zeta)/zeta` there
        real(real64), intent(out)       :: dfval !! `dF/dzeta` there
        real(real64), intent(out)       :: dgval !! `dG/dzeta` there
        real(real64), intent(out)       :: inv_e !! `1/E` there, for the age's slope

        real(real64) :: e2, dc_prime, tl_prime

        e2 = cosmology_e2(this%p, this%d, zeta)
        inv_e = 1.0_real64 / sqrt(e2)
        dc_prime = this%d%dh * exp(zeta) * inv_e
        tl_prime = this%d%th * inv_e
        if (zeta == 0.0_real64) then
            dfval = 0.0_real64
            dgval = 0.0_real64
        else
            dfval = (dc_prime - fval) / zeta
            dgval = (tl_prime - gval) / zeta
        end if

    end subroutine node_slopes


    !> One interval of the BLUESHIFT half, reporting whether it converged instead of aborting.
    !!
    !! The redshift half's intervals go through `one_interval`, which ends the program when the
    !! quadrature does not converge: a caller who asked for `zmax = 1100` and cannot have it must
    !! be told. The blueshift half is different in kind -- it is there to make a negative redshift
    !! FAST, and every one of them is answered by the panel walk whether or not a node exists --
    !! so an interval that fails simply ends the table, and the nodes below it are dropped.
    function blueshift_interval(integ, tol, a, b, step_d, step_t, step_x) result(ok)
        type(cosmology_integrand), intent(inout) :: integ  !! the integrand object
        type(pf_tolerance), intent(in)           :: tol    !! the tolerance
        real(real64), intent(in)                 :: a      !! the interval's lower edge in `zeta`
        real(real64), intent(in)                 :: b      !! its upper edge
        real(real64), intent(out)                :: step_d !! the distance integrand's integral
        real(real64), intent(out)                :: step_t !! the time integrand's integral
        real(real64), intent(out)                :: step_x !! the absorption integrand's integral
        logical                                  :: ok     !! all three converged and are finite

        logical :: ok_t, ok_x

        step_d = floor_panel(integ, tol, PFC_INT_DISTANCE, a, b, ok)
        step_t = floor_panel(integ, tol, PFC_INT_TIME, a, b, ok_t)
        step_x = floor_panel(integ, tol, PFC_INT_ABSORPTION, a, b, ok_x)
        ok = ok .and. ok_t .and. ok_x

    end function blueshift_interval

    !> The bottom of the model's own domain: the largest `zeta < 0` at which `E^2 <= 0`, or
    !! `-PFC_ZETA_CEILING` when `E^2` stays positive all the way down.
    !!
    !! A recollapsing closed universe (`om0 = 1.5, ode0 = 0`, whose `E^2` vanishes at `z = -2/3`)
    !! and any model with a negative `ode0` reach zero at a finite blueshift; below it the
    !! universe does not exist, and every quantity there is a NaN. Nothing else computes this, so
    !! before it existed `%init` walked straight past the crossing and stored NaN in `d_floor`,
    !! `t_floor` and `a_floor` -- which the inverses then compared against.
    !!
    !! `E^2` is walked DOWN from `zeta = 0` in `PFC_PANEL`-wide steps, one `cosmology_e2` call
    !! each and no quadrature at all. The first step that is not positive is refined over twenty
    !! equally spaced points inside its own panel, scanned from the TOP down so that the LARGEST
    !! crossing in the panel is the one taken, and the bracket is then bisected to about `1e-12`.
    !! About fifty `E^2` evaluations, against tens of thousands in the tabulation that follows.
    !!
    !! A model whose `E^2` dips below zero and returns to positive strictly INSIDE one panel,
    !! with both panel edges positive, is not found here. Such a model is not left to give a wrong
    !! answer: its interval integrals stop converging where the dip is, which truncates the table
    !! at that interval, and every binding below the dip answers the NaN `cosmology_e2` gives for
    !! a non-positive `E^2`.
    function cosmology_find_floor(p, d) result(zeta_floor)
        type(cosmology_params), intent(in)  :: p          !! the parameters
        type(cosmology_derived), intent(in) :: d          !! the derived values, up to the tables
        real(real64)                        :: zeta_floor !! the bottom, in `zeta`

        integer, parameter :: REFINE = 20 !! points inside the first panel that is not positive
        integer, parameter :: BISECT = 60 !! bisection cap; about 35 halvings reach `1e-12`

        real(real64) :: hi, lo, mid, top, bottom
        integer      :: i, j, panels
        logical      :: found

        zeta_floor = -PFC_ZETA_CEILING
        panels = int(PFC_ZETA_CEILING / PFC_PANEL) + 2
        hi = 0.0_real64
        lo = 0.0_real64
        found = .false.
        do i = 1, panels
            lo = max(hi - PFC_PANEL, -PFC_ZETA_CEILING)
            if (.not. positive_e2(p, d, lo)) then
                found = .true.
                exit
            end if
            if (lo <= -PFC_ZETA_CEILING) exit
            hi = lo
        end do
        if (.not. found) return

        ! `[lo, hi]` holds at least one crossing: positive at `hi`, not at `lo`. The refinement
        ! scan runs downward over the panel's INTERIOR points and stops at the first that is not
        ! positive, which is the largest of them; if every interior point is positive the bracket
        ! is the last of them and `lo` itself.
        top = hi
        bottom = lo
        do j = 1, REFINE - 1
            mid = hi + (lo - hi) * real(j, real64) / real(REFINE, real64)
            if (.not. positive_e2(p, d, mid)) then
                bottom = mid
                exit
            end if
            top = mid
        end do
        do j = 1, BISECT
            if (top - bottom <= 1.0e-12_real64) exit
            mid = 0.5_real64 * (top + bottom)
            if (mid <= bottom .or. mid >= top) exit
            if (positive_e2(p, d, mid)) then
                top = mid
            else
                bottom = mid
            end if
        end do
        zeta_floor = bottom

    end function cosmology_find_floor

    !> Whether the model exists at `zeta`: `E^2` there is positive.
    !!
    !! A NaN answers `.false.`, so a model that cannot be evaluated at a point is treated as one
    !! that does not reach it. `cosmology_e2` screens its own NaN, so the comparison below is
    !! reached only with a value that is a number or a signed infinity.
    function positive_e2(p, d, zeta) result(yes)
        type(cosmology_params), intent(in)  :: p    !! the parameters
        type(cosmology_derived), intent(in) :: d    !! the derived values
        real(real64), intent(in)            :: zeta !! `ln(1 + z)`
        logical                             :: yes  !! `E^2 > 0` there

        real(real64) :: v

        v = cosmology_e2(p, d, zeta)
        if (v /= v) then
            ! `cosmology_e2` cannot answer a NaN for a `zeta` that is not one: it returns
            ! `+Infinity` as soon as the dark-energy term overflows, and inside the admitted
            ! parameter box no two terms of `E^2` overflow with opposite signs.
            ! GCOVR_EXCL_START
            yes = .false.
            ! GCOVR_EXCL_STOP
        else
            yes = v > 0.0_real64
        end if

    end function positive_e2

    !> The comoving distance and the lookback time at the domain's floor, in units of `D_H` and
    !! `t_H`: the two bounds `%z_at_comoving_distance` and `%z_at_lookback_time` screen against.
    !!
    !! Taken with the ADAPTIVE rule rather than `cosmology_walk`'s fixed 20-point one, because at
    !! a floor where `E^2` vanishes linearly the integrand grows like `(zeta - zeta_floor)^(-1/2)`
    !! -- integrable, but not by any fixed polynomial rule. Panel by panel rather than in one
    !! call, and BOTH integrands over the same panel before either moves on, so that a panel that
    !! does not converge stops both at the same place and one `reached` describes both bounds.
    !!
    !! Asked for at the table's own `PFC_RTOL` and ACCEPTED at `PFC_FLOOR_RTOL`, because these are
    !! SCREEN bounds and not tabulated values: they decide whether an argument is reachable at
    !! all, over distances of thousands of Mpc and times of tens of Gyr. The singular panel of a
    !! truncated model is where the difference tells -- the comoving-distance integral over the
    !! last panel of the recollapsing closed universe reports `PF_INT_NO_CONVERGENCE` against
    !! `1e-12` with an error estimate of `1.2e-10` relative, and refusing it would cut the model's
    !! inverses off at `z = -0.632` when its universe reaches `z = -0.667`.
    !!
    !! A panel that does not converge is not an abort. The bounds returned are the values at the
    !! last `zeta` reached, and the inverses answer NaN for the sliver between it and
    !! `zeta_floor` rather than a redshift they cannot justify.
    subroutine floor_bounds(integ, zeta_start, zeta_floor, dist, time, reached)
        type(cosmology_integrand), intent(inout) :: integ      !! the integrand object
        real(real64), intent(in)                 :: zeta_start !! the table's bottom node
        real(real64), intent(in)                 :: zeta_floor !! the model's bottom, in `zeta`
        real(real64), intent(out)                :: dist       !! `(D_C(reached) - D_C(zeta_start)) / D_H`
        real(real64), intent(out)                :: time       !! `(t_L(reached) - t_L(zeta_start)) / t_H`
        real(real64), intent(out)                :: reached    !! how far down both converged

        type(pf_tolerance) :: tol
        real(real64)       :: hi, lo, piece_d, piece_t
        logical            :: ok_d, ok_t
        integer            :: i, panels

        ! Below the table's bottom node there is no tabulated `nu_rel`, so the fit is evaluated
        ! exactly here whatever the build's flag says. `cosmology_e2` would fall back on its own
        ! range test; clearing the flag says so at the place that knows why.
        integ%d%use_nu_tab = .false.
        tol%rtol = PFC_RTOL
        tol%atol = 0.0_real64
        dist = 0.0_real64
        time = 0.0_real64
        reached = zeta_start
        panels = int(2.0_real64 * PFC_ZETA_CEILING / PFC_PANEL) + 2
        hi = zeta_start
        do i = 1, panels
            if (hi <= zeta_floor) exit
            lo = max(hi - PFC_PANEL, zeta_floor)
            if (zeta_floor > -PFC_ZETA_CEILING .and. lo - zeta_floor < PFC_PANEL) then
                ! Within one panel of a REAL floor, where the integrand grows like
                ! `(zeta - zeta_floor)^(-1/2)`: the adaptive rule, at several hundred evaluations.
                piece_d = floor_panel(integ, tol, PFC_INT_DISTANCE, lo, hi, ok_d)
                piece_t = floor_panel(integ, tol, PFC_INT_TIME, lo, hi, ok_t)
                if (.not. (ok_d .and. ok_t)) return
            else
                ! Everywhere else the integrand is smooth and the fixed 20-point rule is exact to
                ! rounding -- 2e-16 over a unit panel, measured -- for forty evaluations rather
                ! than the adaptive rule's several hundred. It is the same rule the FORWARD
                ! fallback uses below the table, so the screen bound and the quantity it screens
                ! are formed the same way over the same range.
                piece_d = cosmology_panel(integ%p, integ%d, PFC_INT_DISTANCE, lo, hi)
                piece_t = cosmology_panel(integ%p, integ%d, PFC_INT_TIME, lo, hi)
                if (piece_d /= piece_d .or. piece_t /= piece_t) return
            end if
            ! `pf_integrate` refuses `a > b`, so each panel is taken upward and SUBTRACTED: the
            ! integral from `zeta_start` DOWN to `lo` is the negative of the integral up from it.
            dist = dist - piece_d
            time = time - piece_t
            reached = lo
            hi = lo
        end do

    end subroutine floor_bounds

    !> One adaptive panel of `floor_bounds`, reporting whether it converged instead of aborting.
    function floor_panel(integ, tol, which, a, b, ok) result(v)
        type(cosmology_integrand), intent(inout) :: integ !! the integrand object
        type(pf_tolerance), intent(in)           :: tol   !! the tolerance
        integer, intent(in)                      :: which !! which integrand
        real(real64), intent(in)                 :: a     !! the panel's lower edge in `zeta`
        real(real64), intent(in)                 :: b     !! its upper edge
        logical, intent(out)                     :: ok    !! the integral converged and is finite
        real(real64)                             :: v     !! the panel's integral

        type(pf_integration_info) :: info

        integ%which = which
        if (pfc_max_neval > 0) then
            v = pf_integrate(integ, a, b, tol, max_neval=pfc_max_neval, converged=ok, info=info)
        else
            v = pf_integrate(integ, a, b, tol, converged=ok, info=info)
        end if
        pfc_neval = pfc_neval + info%neval
        ! The two ROUND-OFF statuses are accepted on the error estimate the engine itself reports,
        ! because they mean "this is as close as double arithmetic gets", not "the integrand
        ! misbehaved". Every other status, `PF_INT_BAD_VALUE` and `PF_INT_DIVERGENT` included, is
        ! refused whatever the estimate says.
        if (.not. ok .and. (info%status == PF_INT_ROUNDOFF .or. info%status == PF_INT_NO_CONVERGENCE)) &
            ok = info%abserr <= PFC_FLOOR_RTOL * abs(v)
        if (v /= v) ok = .false.

    end function floor_panel

    !> The longest strictly increasing run of `v` that CONTAINS index `anchor`.
    !!
    !! **An inverse table goes only as far as its abscissae strictly increase, in BOTH
    !! directions.** Each of the three quantities inverted here saturates somewhere: the lookback
    !! time and the age stop changing above `zeta` of about 23, where their interval integrals are
    !! `1e-18` against a total of order one, and the comoving distance stops changing at the bottom
    !! of a deep blueshift table for a model whose `E` diverges towards `z = -1`. Where two
    !! consecutive nodes carry the same double, `pf_interp_1d%init` refuses them -- rightly, since
    !! a quantity that no longer changes cannot determine a redshift.
    !!
    !! Truncating costs no accuracy: outside the run the inverse falls through to the bracketed
    !! solve, which is what it would have done beyond the table anyway. The run is taken around
    !! the ORIGIN because that is the one node every table has and the one place none of them
    !! saturates -- the steps there are of order `h`, twelve decades above the spacing of a double
    !! -- and because a run taken from one end inward would stop at the other end's saturation and
    !! never reach the origin at all.
    pure subroutine increasing_run(v, anchor, lo, hi)
        real(real64), intent(in) :: v(:)   !! the candidate abscissae
        integer, intent(in)      :: anchor !! an index the run must contain
        integer, intent(out)     :: lo     !! the run's first index
        integer, intent(out)     :: hi     !! its last

        lo = min(max(anchor, 1), size(v))
        hi = lo
        do while (lo > 1)
            if (.not. v(lo) > v(lo - 1)) exit
            lo = lo - 1
        end do
        do while (hi < size(v))
            if (.not. v(hi + 1) > v(hi)) exit
            hi = hi + 1
        end do

    end subroutine increasing_run

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
        character(len=:), allocatable :: num, num2, num3

        if (info%status == PF_INT_BAD_VALUE) then
            ! `non_finite_at` is in the INTEGRATION variable, so it is converted back to a
            ! redshift before anyone reads it.
            call pf_to_str(z_of_point(which, info%non_finite_at), num)
            call cosmology_abort("pf_cosmology%init: this cosmology has no big bang: E(z)^2 is not " // &
                                 "positive at z = " // num, context)
        end if
        if (which == PFC_INT_DISTANCE) then
            what = "distance"
        ! the three redshift-half integrals are taken in order and share one evaluation budget,
        ! so the DISTANCE one is always the first to fail and `report_interval` is never
        ! reached naming another; `error_scenarios.f90`'s `cosmology_init_table_not_converged`
        ! is what drives it.
        ! GCOVR_EXCL_START
        else if (which == PFC_INT_TIME) then
            what = "lookback"
        ! GCOVR_EXCL_STOP
        else
            ! the three redshift-half integrals are taken in order and share one evaluation budget,
            ! so the DISTANCE one is always the first to fail and `report_interval` is never
            ! reached naming another; `error_scenarios.f90`'s `cosmology_init_table_not_converged`
            ! is what drives it.
            ! GCOVR_EXCL_START
            what = "age"
            ! GCOVR_EXCL_STOP
        end if
        call pf_to_str(pf_zeta2z(a), num)
        call pf_to_str(pf_zeta2z(b), num2)
        call pf_to_str(real(info%status, real64), num3)
        call cosmology_abort("pf_cosmology%init: the " // what // " table did not converge on [" // &
                             num // ", " // num2 // "] (pf_integrate status " // &
                             num3 // ")", context)
        ! `this` is taken so that a future message may name the model; nothing reads it yet.
        ! `cosmology_abort` on the line above does not return.
        ! GCOVR_EXCL_START
        if (this%ready) return
        ! GCOVR_EXCL_STOP

    end subroutine report_interval

    !> The redshift an integration point stands for: `zeta` for two of the integrands, `b` for the
    !! age tail, where the scale factor is `b^2`.
    function z_of_point(which, x) result(z)
        integer, intent(in)      :: which !! which integrand
        real(real64), intent(in) :: x     !! the point the integrator reported
        real(real64)             :: z     !! the redshift there

        if (which == PFC_INT_AGE_TAIL) then
            ! The three redshift-half integrals are taken in order and share one evaluation
            ! budget, so the DISTANCE one is always the first to fail and `report_interval` is
            ! never reached naming the age tail; `error_scenarios.f90`'s
            ! `cosmology_init_table_not_converged` is what drives this procedure, through the
            ! distance table. The `which` test above is evaluated on every call and is NOT
            ! excluded with the body it guards.
            ! GCOVR_EXCL_START
            if (x > 0.0_real64) then
                z = 1.0_real64 / (x * x) - 1.0_real64
            else
                z = ieee_value(z, ieee_positive_inf)
            end if
            ! GCOVR_EXCL_STOP
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
    !!
    !! **This integral is also what protects the UPPER half of the domain from a model whose
    !! `E^2` turns negative above `zmax`.** Its range in `b` is `[0, e^(-zeta_n/2)]`, that is
    !! every scale factor below the table's edge, so it visits every redshift from `zmax` to
    !! infinity; a non-positive `E^2` anywhere up there comes back as `PF_INT_BAD_VALUE` and
    !! `report_interval` turns it into "this cosmology has no big bang", naming the redshift.
    !! `om0 = 0, ode0 = 2, w0 = -0.366` with `zmax = 1100` is the reachable case, and it aborts
    !! naming `z = 4420`, four times beyond the table. A later change that skips the tail -- for a
    !! model already flagged divergent, say -- or that narrows its range removes that protection,
    !! and nothing else in `%init` looks above `zmax` at all.
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
        ! The tail integrates in `b = sqrt(a)` over scale factors from zero, that is over every
        ! `zeta` ABOVE the table's top, where there is no tabulated `nu_rel`: the exact fit.
        integ%d%use_nu_tab = .false.
        b_top = exp(-0.5_real64 * this%d%zeta_n)
        if (pfc_max_neval > 0) then
            v = pf_integrate(integ, 0.0_real64, b_top, tol, max_neval=pfc_max_neval, converged=ok, info=info)
        else
            v = pf_integrate(integ, 0.0_real64, b_top, tol, converged=ok, info=info)
        end if
        pfc_neval = pfc_neval + info%neval
        if (.not. ok) then
            ! `age_integral_diverges` sets the flag before the tail integral is taken, so the
            ! tail itself never reports `PF_INT_DIVERGENT`, and no corner model starves the
            ! budget it is given here.
            ! GCOVR_EXCL_START
            if (info%status == PF_INT_DIVERGENT) then
                ! Documented, not aborted, and right at every redshift: such a model is
                ! infinitely old throughout. `age_integral_diverges` normally reaches this first.
                this%age_diverges = .true.
                v = 0.0_real64
                return
            end if
            call report_interval(this, info, PFC_INT_AGE_TAIL, 0.0_real64, b_top, context)
            ! GCOVR_EXCL_STOP
        end if

    end function age_tail_integral

    ! =========================================================================================
    ! The object's lifetime
    ! =========================================================================================

    module procedure cosmology_clone

        real(real64), allocatable :: ode0_arg, ob0_arg
        real(real64), allocatable :: masses(:)
        character(len=:), allocatable :: label

        if (.not. this%ready) error stop "pf_cosmology%clone: the cosmology is not initialised"

        ! **Absent stays absent.** An UNALLOCATED allocatable actual makes an `optional` dummy
        ! absent (F2018 15.5.2.12), which is how "the source had no `ode0`" and "the source had no
        ! `ob0`" are carried through one call rather than through a cascade of four.
        if (present(ode0)) then
            ode0_arg = ode0
        else if (.not. this%flat) then
            ode0_arg = this%p%ode0
        end if
        if (present(ob0)) then
            ob0_arg = ob0
        else if (this%has_ob0) then
            ob0_arg = this%p%ob0
        end if
        if (present(m_nu)) then
            masses = m_nu
        else
            masses = this%p%m_nu
        end if
        if (present(name)) then
            label = name
        else
            label = this%label
        end if

        call cosmology_init_params(out, &
                                   h0 = merge_real(h0, this%p%h0), &
                                   om0 = merge_real(om0, this%p%om0), &
                                   ode0 = ode0_arg, &
                                   tcmb0 = merge_real(tcmb0, this%p%tcmb0), &
                                   neff = merge_real(neff, this%p%neff), &
                                   m_nu = masses, &
                                   ob0 = ob0_arg, &
                                   w0 = merge_real(w0, this%p%w0), &
                                   wa = merge_real(wa, this%p%wa), &
                                   name = label, &
                                   zmax = merge_real(zmax, this%p%zmax), &
                                   zmin = merge_real(zmin, this%p%zmin), &
                                   context = context)

    end procedure cosmology_clone

    !> The caller's value when there is one, the source's otherwise.
    !!
    !! `merge` would evaluate an absent `given`, which is not permitted.
    pure function merge_real(given, fallback) result(v)
        real(real64), intent(in), optional :: given    !! what the caller named, if anything
        real(real64), intent(in)           :: fallback !! what the source carries
        real(real64)                       :: v        !! the one to build with

        if (present(given)) then
            v = given
        else
            v = fallback
        end if

    end function merge_real

    module procedure cosmology_clear

        ! Every component is reset EXPLICITLY. A default structure constructor `pf_cosmology()`
        ! would be rejected by ifx (error #6053), because `pf_interp_1d` has private components in
        ! another module.
        if (allocated(this%fv)) deallocate (this%fv)
        if (allocated(this%fd)) deallocate (this%fd)
        if (allocated(this%gv)) deallocate (this%gv)
        if (allocated(this%gd)) deallocate (this%gd)
        if (allocated(this%av)) deallocate (this%av)
        if (allocated(this%ad)) deallocate (this%ad)
        if (allocated(this%xv)) deallocate (this%xv)
        if (allocated(this%xd)) deallocate (this%xd)
        if (allocated(this%gwv)) deallocate (this%gwv)
        if (allocated(this%gwd)) deallocate (this%gwd)
        call this%zd%clear()
        call this%zt%clear()
        call this%za%clear()
        this%p = cosmology_params()
        this%d = cosmology_derived()
        if (allocated(this%label)) deallocate (this%label)
        this%flat = .false.
        this%massive_nu = .false.
        this%has_ob0 = .false.
        this%age_diverges = .false.
        this%has_growth = .false.
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
        character(len=:), allocatable :: num

        if (.not. this%ready) error stop "pf_cosmology%describe: the cosmology is not initialised"
        text = this%label
        call pf_to_str(this%p%h0, num)
        text = text // "; H0 = " // num // " km/s/Mpc"
        call pf_to_str(this%p%om0, num)
        text = text // "; Om0 = " // num
        call pf_to_str(this%p%ode0, num)
        text = text // "; Ode0 = " // num
        if (this%flat) then
            text = text // "; Ok0 = 0 (flat)"
        else
            call pf_to_str(this%d%ok0, num)
            text = text // "; Ok0 = " // num
        end if
        call pf_to_str(this%p%tcmb0, num)
        text = text // "; Tcmb0 = " // num // " K"
        call pf_to_str(this%p%neff, num)
        text = text // "; Neff = " // num
        if (this%d%n_nu == 0) then
            text = text // "; m_nu = none"
        else
            text = text // "; m_nu = "
            do i = 1, this%d%n_nu
                if (i > 1) text = text // ", "
                call pf_to_str(this%p%m_nu(i), num)
                text = text // num
            end do
            text = text // " eV"
        end if
        if (this%has_ob0) then
            call pf_to_str(this%p%ob0, num)
            text = text // "; Ob0 = " // num
        else
            text = text // "; Ob0 = unknown"
        end if
        ! The pair is omitted entirely for a cosmological constant, which is every named cosmology.
        if (.not. (this%p%w0 == -1.0_real64 .and. this%p%wa == 0.0_real64)) then
            call pf_to_str(this%p%w0, num)
            text = text // "; w0 = " // num
            call pf_to_str(this%p%wa, num)
            text = text // "; wa = " // num
        end if

    end procedure cosmology_describe

    ! =========================================================================================
    ! The test-only hooks
    ! =========================================================================================

    module procedure parquet_debug_set_cosmology_max_neval
        pfc_max_neval = budget
    end procedure parquet_debug_set_cosmology_max_neval

    module procedure parquet_debug_set_cosmology_exact_nu
        pfc_exact_nu = exact
    end procedure parquet_debug_set_cosmology_exact_nu

    module procedure parquet_debug_cosmology_neval
        n = pfc_neval
    end procedure parquet_debug_cosmology_neval

end submodule parquet_cosmology_build
