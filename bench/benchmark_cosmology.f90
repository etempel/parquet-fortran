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

    end subroutine run_eval

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
        do k = 1, 12
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
