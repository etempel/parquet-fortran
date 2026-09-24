!> The stopwatch and the fixtures `benchmark_cosmology` times.
!!
!! Every redshift column is drawn once from `parquet_random` under one fixed seed, so two runs time
!! the same queries. The arrays are allocatable: a million redshifts is eight megabytes, more than
!! a default stack holds.
module benchmark_cosmology_kernels

    use iso_fortran_env, only : real64, int64
    use parquet_random, only : pf_random_fill_draws

    implicit none
    private

    public :: log_spaced_redshifts, watch_seconds, MIN_LAP

    !> Seconds a timed lap lasts at least. A lap repeats its job until it has run this long, since
    !! a clock counting microseconds resolves a shorter one too coarsely.
    real(real64), parameter :: MIN_LAP = 0.05_real64

contains

    !> `n` redshifts drawn log-uniformly over `[lo, hi]`, in a random order.
    !!
    !! A random order is the honest one: a survey's catalogue is not sorted by redshift, and a
    !! sorted column would let the interpolant's bracket search hit every time.
    subroutine log_spaced_redshifts(n, lo, hi, z)
        integer, intent(in)                    :: n     !! how many
        real(real64), intent(in)               :: lo    !! the smallest redshift
        real(real64), intent(in)               :: hi    !! the largest
        real(real64), allocatable, intent(out) :: z(:)  !! the column

        real(real64) :: log_lo, log_hi
        integer      :: i

        allocate (z(n))
        call pf_random_fill_draws(20260920_int64, 1_int64, z)
        log_lo = log(lo)
        log_hi = log(hi)
        do i = 1, n
            z(i) = exp(log_lo + (log_hi - log_lo) * z(i))
        end do

    end subroutine log_spaced_redshifts

    !> Seconds on the wall, from whichever clock this build has.
    function watch_seconds() result(t)
        real(real64) :: t !! seconds; only differences of it mean anything

        integer(int64) :: ticks, rate

        call system_clock(ticks, rate)
        t = real(ticks, real64) / real(rate, real64)

    end function watch_seconds

end module benchmark_cosmology_kernels

!> What `parquet_cosmology` costs: building a cosmology against its `zmax`, a query inside the
!> table against one beyond it, each binding's own cost, and the two inverses.
!!
!! Driven by `bench/benchmark_cosmology.sh`, which documents the modes and reads the figures the
!! guide's words rest on. Name the machine and the toolchain on any figure taken from it
!! (`tools/machine_report.sh` prints both); no page carries a machine name.
program benchmark_cosmology

    use iso_fortran_env, only : real64, int64, output_unit, error_unit
    use parquet_cosmology
    use benchmark_cosmology_kernels

    implicit none

    character(len=32) :: mode
    integer           :: n

    call parse_arguments(mode, n)

    select case (trim(mode))
    case ("build")
        call run_build()
    case ("eval")
        call run_eval(n)
    case ("bindings")
        call run_bindings(n)
    case ("inverse")
        call run_inverse(n)
    case ("sound")
        call run_sound(n)
    case default
        write (error_unit, '(a)') "benchmark_cosmology: unknown mode '" // trim(mode) // "'"
        error stop 1
    end select

contains

    !> Reads the mode and the column length from the command line.
    subroutine parse_arguments(mode, n)
        character(len=*), intent(out) :: mode !! the mode to run
        integer, intent(out)          :: n    !! redshifts per timed column

        character(len=32) :: buf
        integer           :: ios

        mode = "build"
        n = 1000000
        if (command_argument_count() >= 1) call get_command_argument(1, mode)
        if (command_argument_count() >= 2) then
            call get_command_argument(2, buf)
            read (buf, *, iostat=ios) n
            if (ios /= 0 .or. n < 1) then
                write (error_unit, '(a)') "benchmark_cosmology: bad column length '" // trim(buf) // "'"
                error stop 1
            end if
        end if

    end subroutine parse_arguments

    !> `%init` against `zmax`: the table's node count, the build's cost and its integrand count.
    subroutine run_build()
        real(real64), parameter :: ZMAXES(7) = [1.0_real64, 5.0_real64, 20.0_real64, 100.0_real64, &
                                                1100.0_real64, 1.0e5_real64, 1.0e10_real64]
        type(pf_cosmology) :: c
        real(real64)       :: t0, t1, elapsed
        integer            :: i, laps, neval

        write (output_unit, '(a)') "# %init against zmax. nodes is ceil(ln(1+zmax)/0.008), the fixed"
        write (output_unit, '(a)') "# grid spacing; neval is integrand evaluations, from the debug counter."
        write (output_unit, '(a12,a10,a14,a14,a12)') "zmax", "nodes", "build_ms", "neval", "ns_per_eval"
        do i = 1, size(ZMAXES)
            laps = 0
            elapsed = 0.0_real64
            t0 = watch_seconds()
            do while (elapsed < MIN_LAP)
                call c%init("Planck18", zmax=ZMAXES(i))
                laps = laps + 1
                elapsed = watch_seconds() - t0
            end do
            t1 = watch_seconds()
            neval = parquet_debug_cosmology_neval()
            write (output_unit, '(es12.0,i10,f14.3,i14,f12.2)') ZMAXES(i), &
                ceiling(log(1.0_real64 + ZMAXES(i)) / 0.008_real64), &
                1000.0_real64 * (t1 - t0) / real(laps, real64), neval, &
                1.0e9_real64 * (t1 - t0) / real(laps, real64) / real(max(neval, 1), real64)
        end do

    end subroutine run_build

    !> A query inside the table against one beyond it: the whole point of the fallback rule.
    subroutine run_eval(n)
        integer, intent(in) :: n !! redshifts per timed column

        type(pf_cosmology) :: c

        call c%init("Planck18")
        write (output_unit, '(a)') "# %comoving_distance over a column of n redshifts, random order."
        write (output_unit, '(a)') "# The table ends at z = 1100; rows above it are the panel rule."
        write (output_unit, '(a24,a16,a16)') "range", "ns_per_query", "total_ms"

        call timed_column(c, n, 1.0e-4_real64, 1.0e3_real64, "inside the table")
        call timed_column(c, n, 2.0e3_real64, 1.0e5_real64, "one panel beyond")
        call timed_column(c, n, 1.0e9_real64, 1.0e10_real64, "sixteen panels beyond")
        ! The blueshift half is tabulated down to `zmin`, default -0.9: the first of these rows is
        ! a table read and the second is the panel walk below it, which is the pair the default
        ! was chosen from.
        call timed_column(c, n, -0.8_real64, -1.0e-3_real64, "a blueshift, tabulated")
        call timed_column(c, n, -0.999_real64, -0.95_real64, "a blueshift, below the table")
        ! The same two for GROWTH, which is where the difference bites: below the table the growth
        ! bindings continue the Runge-Kutta pass at two substeps per grid interval, where the
        ! distance lays one twenty-point panel per unit. The ratio between these two rows is what
        ! the guide page's "pass a lower `zmin=` if you query growth below it" is about.
        call timed_growth(c, n, -0.8_real64, -1.0e-3_real64, "growth, tabulated")
        call timed_growth(c, n, -0.999_real64, -0.95_real64, "growth, below the table")

    end subroutine run_eval

    !> The four sound-horizon bindings, which are the only ones here that are not table reads.
    !!
    !! **A mode of its own, and a short column, because these are microseconds where every other
    !! binding is nanoseconds.** `%sound_horizon` lays panels of a twenty-point rule over the whole
    !! sound-crossing history at every call, and `%z_drag` does that inside a solve; a column of
    !! `n` would run for minutes and say nothing the first thousand does not. The redshift is
    !! swept because the panel COUNT follows the interval's own length: `asinh(b_top/sound_c)` is
    !! about 4.8 at `z = 0` and 1.3 at `z = 1100`, so the top of the table is the cheapest place
    !! to ask and a deep blueshift the dearest.
    subroutine run_sound(n)
        integer, intent(in) :: n !! redshifts per timed column, capped hard below

        type(pf_cosmology) :: c
        real(real64), allocatable :: z(:), out(:)
        real(real64)       :: t0, t1, elapsed, v
        integer            :: laps, m, k
        character(len=28)  :: label

        call c%init("Planck18")
        m = max(1, min(n, 2000))
        allocate (out(m))
        write (output_unit, '(a)') "# The sound horizon over a column of m redshifts, and the three scalars."
        write (output_unit, '(a)') "# Nothing here is tabulated: every row is a fresh panel rule."
        write (output_unit, '(a28,a16,a16)') "binding", "us_per_query", "column"
        do k = 1, 4
            select case (k)
            case (1)
                call log_spaced_redshifts(m, 1.0e-3_real64, 1.0e3_real64, z)
                label = "%sound_horizon, z < 1000"
            case (2)
                call log_spaced_redshifts(m, 1.0e3_real64, 1.0e5_real64, z)
                label = "%sound_horizon, z > 1000"
            case (3)
                call log_spaced_redshifts(m, 0.5_real64, 0.999_real64, z)
                z = -z
                label = "%sound_horizon, blueshift"
            case (4)
                call log_spaced_redshifts(m, 1.0e-3_real64, 1.0e3_real64, z)
                label = "%sound_horizon, scalar"
            end select
            laps = 0
            elapsed = 0.0_real64
            t0 = watch_seconds()
            do while (elapsed < MIN_LAP)
                if (k == 4) then
                    ! The same work one element at a time, which is how a program actually calls
                    ! it: the elemental form over a column is not what this binding is for.
                    do laps = 1, m
                        out(laps) = c%sound_horizon(z(laps))
                    end do
                    laps = 1
                else
                    out = c%sound_horizon(z)
                    laps = laps + 1
                end if
                elapsed = watch_seconds() - t0
                if (k == 4) exit
            end do
            t1 = watch_seconds()
            write (output_unit, '(a28,f16.3,i16)') label, &
                1.0e6_real64 * (t1 - t0) / real(max(laps, 1), real64) / real(m, real64), m
            if (out(1) /= out(1)) write (output_unit, '(a)') "# (a NaN reached the column)"
        end do

        ! The three scalars, each timed over its own repeat count: `%r_drag` and `%z_eq` are
        ! closed forms and `%z_drag` is a solve over the integral above.
        do k = 1, 3
            laps = 0
            elapsed = 0.0_real64
            t0 = watch_seconds()
            do while (elapsed < MIN_LAP)
                select case (k)
                case (1)
                    label = "%r_drag"
                    v = c%r_drag()
                case (2)
                    label = "%z_eq"
                    v = c%z_eq()
                case (3)
                    label = "%z_drag"
                    v = c%z_drag()
                end select
                laps = laps + 1
                elapsed = watch_seconds() - t0
            end do
            t1 = watch_seconds()
            write (output_unit, '(a28,f16.3,i16)') label, &
                1.0e6_real64 * (t1 - t0) / real(laps, real64), 1
            if (v /= v) write (output_unit, '(a)') "# (a NaN was returned)"
        end do

    end subroutine run_sound

    !> Times `%growth_factor` over one column and prints its row.
    subroutine timed_growth(cosmo, n, lo, hi, label)
        type(pf_cosmology), intent(in) :: cosmo !! the cosmology
        integer, intent(in)            :: n     !! redshifts in the column
        real(real64), intent(in)       :: lo    !! the smallest redshift
        real(real64), intent(in)       :: hi    !! the largest
        character(len=*), intent(in)   :: label !! the row's name

        real(real64), allocatable :: z(:), d(:)
        real(real64)              :: t0, t1, elapsed
        integer                   :: laps, m

        ! A column below the table costs hundreds of microseconds per element, so this row takes a
        ! SHORTER column: a million of them would run for minutes and say nothing more.
        m = n
        if (lo < -0.9_real64) m = max(1, n / 500)
        call log_spaced_redshifts(m, -hi, -lo, z)
        z = -z
        allocate (d(m))
        laps = 0
        elapsed = 0.0_real64
        t0 = watch_seconds()
        do while (elapsed < MIN_LAP)
            d = cosmo%growth_factor(z)
            laps = laps + 1
            elapsed = watch_seconds() - t0
        end do
        t1 = watch_seconds()
        write (output_unit, '(a24,f16.2,f16.3)') label, &
            1.0e9_real64 * (t1 - t0) / real(laps, real64) / real(m, real64), &
            1000.0_real64 * (t1 - t0) / real(laps, real64)
        if (d(1) /= d(1)) write (output_unit, '(a)') "# (a NaN reached the column)"

    end subroutine timed_growth

    !> Times `%comoving_distance` over one column and prints its row.
    !!
    !! A sibling rather than an internal procedure of `run_eval`: Fortran has no nested `contains`.
    subroutine timed_column(cosmo, n, lo, hi, label)
        type(pf_cosmology), intent(in) :: cosmo !! the cosmology
        integer, intent(in)            :: n     !! redshifts in the column
        real(real64), intent(in)       :: lo    !! the smallest redshift
        real(real64), intent(in)       :: hi    !! the largest
        character(len=*), intent(in)   :: label !! the row's name

        real(real64), allocatable :: z(:), d(:)
        real(real64)              :: t0, t1, elapsed
        integer                   :: laps

        if (lo < 0.0_real64) then
            call log_spaced_redshifts(n, -hi, -lo, z)
            z = -z
        else
            call log_spaced_redshifts(n, lo, hi, z)
        end if
        allocate (d(n))
        laps = 0
        elapsed = 0.0_real64
        t0 = watch_seconds()
        do while (elapsed < MIN_LAP)
            d = cosmo%comoving_distance(z)
            laps = laps + 1
            elapsed = watch_seconds() - t0
        end do
        t1 = watch_seconds()
        write (output_unit, '(a24,f16.2,f16.3)') label, &
            1.0e9_real64 * (t1 - t0) / real(laps, real64) / real(n, real64), &
            1000.0_real64 * (t1 - t0) / real(laps, real64)
        ! `d` is read so the loop above cannot be optimised away.
        if (d(1) /= d(1)) write (output_unit, '(a)') "# (a NaN reached the column)"

    end subroutine timed_column

    !> Each binding's own cost over one column inside the table.
    subroutine run_bindings(n)
        integer, intent(in) :: n !! redshifts per timed column

        type(pf_cosmology)        :: c
        real(real64), allocatable :: z(:), out(:)
        real(real64)              :: t0, t1, elapsed
        integer                   :: laps, k
        character(len=28)         :: label

        call c%init("Planck18")
        call log_spaced_redshifts(n, 1.0e-3_real64, 1.0e3_real64, z)
        allocate (out(n))
        write (output_unit, '(a)') "# Each binding over the same column of n redshifts, inside the table."
        write (output_unit, '(a28,a16)') "binding", "ns_per_query"
        do k = 1, 14
            laps = 0
            elapsed = 0.0_real64
            t0 = watch_seconds()
            do while (elapsed < MIN_LAP)
                select case (k)
                case (1)
                    label = "%comoving_distance"
                    out = c%comoving_distance(z)
                case (2)
                    label = "%luminosity_distance"
                    out = c%luminosity_distance(z)
                case (3)
                    label = "%lookback_time"
                    out = c%lookback_time(z)
                case (4)
                    label = "%age"
                    out = c%age(z)
                case (5)
                    label = "%efunc"
                    out = c%efunc(z)
                case (6)
                    label = "%distmod"
                    out = c%distmod(z)
                case (7)
                    label = "%comoving_volume"
                    out = c%comoving_volume(z)
                case (8)
                    label = "pf_z2zeta"
                    out = pf_z2zeta(z)
                case (9)
                    label = "%absorption_distance"
                    out = c%absorption_distance(z)
                case (10)
                    label = "%comoving_distance_z1z2"
                    out = c%comoving_distance_z1z2(0.5_real64 * z, z)
                case (11)
                    label = "%otot"
                    out = c%otot(z)
                case (12)
                    label = "%nu_relative_density"
                    out = c%nu_relative_density(z)
                case (13)
                    label = "%growth_factor"
                    out = c%growth_factor(z)
                case (14)
                    label = "%growth_rate"
                    out = c%growth_rate(z)
                end select
                laps = laps + 1
                elapsed = watch_seconds() - t0
            end do
            t1 = watch_seconds()
            write (output_unit, '(a28,f16.2)') label, &
                1.0e9_real64 * (t1 - t0) / real(laps, real64) / real(n, real64)
            if (out(1) /= out(1)) write (output_unit, '(a)') "# (a NaN reached the column)"
        end do

    end subroutine run_bindings

    !> The inverses, on the table and beyond it.
    subroutine run_inverse(n)
        integer, intent(in) :: n !! redshifts per timed column

        type(pf_cosmology)        :: c
        real(real64), allocatable :: z(:), d(:), back(:)
        real(real64)              :: t0, t1, elapsed
        integer                   :: laps

        call c%init("Planck18")
        call log_spaced_redshifts(n, 1.0e-3_real64, 1.0e3_real64, z)
        allocate (d(n), back(n))
        d = c%comoving_distance(z)
        write (output_unit, '(a)') "# The inverses. On the table it is one interpolant read plus one"
        write (output_unit, '(a)') "# Newton step; beyond it, a bracketed solve over the panel rule."
        write (output_unit, '(a28,a16)') "call", "ns_per_query"

        laps = 0
        elapsed = 0.0_real64
        t0 = watch_seconds()
        do while (elapsed < MIN_LAP)
            back = c%z_at_comoving_distance(d)
            laps = laps + 1
            elapsed = watch_seconds() - t0
        end do
        t1 = watch_seconds()
        write (output_unit, '(a28,f16.2)') "%z_at_comoving_distance", &
            1.0e9_real64 * (t1 - t0) / real(laps, real64) / real(n, real64)
        write (output_unit, '(a,es12.4)') "# worst relative round trip in z: ", &
            maxval(abs(back - z) / z)

        d = c%lookback_time(z)
        laps = 0
        elapsed = 0.0_real64
        t0 = watch_seconds()
        do while (elapsed < MIN_LAP)
            back = c%z_at_lookback_time(d)
            laps = laps + 1
            elapsed = watch_seconds() - t0
        end do
        t1 = watch_seconds()
        write (output_unit, '(a28,f16.2)') "%z_at_lookback_time", &
            1.0e9_real64 * (t1 - t0) / real(laps, real64) / real(n, real64)
        write (output_unit, '(a,es12.4)') "# worst relative round trip in z: ", &
            maxval(abs(back - z) / z)

        ! The three that had no inverse table of their own: the age now has one, and the two
        ! luminosity inverses bracket from the distance table rather than from the whole domain.
        d = c%age(z)
        laps = 0
        elapsed = 0.0_real64
        t0 = watch_seconds()
        do while (elapsed < MIN_LAP)
            back = c%z_at_age(d)
            laps = laps + 1
            elapsed = watch_seconds() - t0
        end do
        t1 = watch_seconds()
        write (output_unit, '(a28,f16.2)') "%z_at_age", &
            1.0e9_real64 * (t1 - t0) / real(laps, real64) / real(n, real64)
        write (output_unit, '(a,es12.4)') "# worst relative round trip in z: ", &
            maxval(abs(back - z) / z)

        d = c%luminosity_distance(z)
        laps = 0
        elapsed = 0.0_real64
        t0 = watch_seconds()
        do while (elapsed < MIN_LAP)
            back = c%z_at_luminosity_distance(d)
            laps = laps + 1
            elapsed = watch_seconds() - t0
        end do
        t1 = watch_seconds()
        write (output_unit, '(a28,f16.2)') "%z_at_luminosity_distance", &
            1.0e9_real64 * (t1 - t0) / real(laps, real64) / real(n, real64)
        write (output_unit, '(a,es12.4)') "# worst relative round trip in z: ", &
            maxval(abs(back - z) / z)

        d = c%distmod(z)
        laps = 0
        elapsed = 0.0_real64
        t0 = watch_seconds()
        do while (elapsed < MIN_LAP)
            back = c%z_at_distmod(d)
            laps = laps + 1
            elapsed = watch_seconds() - t0
        end do
        t1 = watch_seconds()
        write (output_unit, '(a28,f16.2)') "%z_at_distmod", &
            1.0e9_real64 * (t1 - t0) / real(laps, real64) / real(n, real64)
        write (output_unit, '(a,es12.4)') "# worst relative round trip in z: ", &
            maxval(abs(back - z) / z)

    end subroutine run_inverse

end program benchmark_cosmology
