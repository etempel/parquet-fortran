!> The bulk forms: every conversion over whole arrays, optionally threaded.
!!
!! **The only file in this tier containing an OpenMP directive**, which is why it is a file of its
!! own rather than more of `parquet_healpix_core`. Each procedure is the same three steps --
!! validate once, resolve a thread count once, then loop calling the scalar form -- so the
!! arithmetic lives in exactly one place and a bulk result cannot drift from its scalar twin.
!!
!! **Bit-identical at every thread count.** Every element is a function of its own inputs alone,
!! there is no reduction anywhere, and the schedule is static -- so an exact `==` is the right
!! assertion for the tests here, unlike an OpenMP reduction over reals, where it would not be.
!!
!! **The thread rule is `parquet_auto_thread_count`'s, not this module's.** That is the point of
!! reaching `parquet_settings_base` at all: a caller's own `!$omp parallel do` inside somebody
!! else's region opens a nested team, and a caller's own thread count ignores the CPU affinity
!! mask -- which on a bound process turns 64 requested threads into 64 threads time-sharing
!! however few processors the mask allows, slower than not threading. One rule, in one place, for
!! every tier in this library.
submodule(parquet_healpix) parquet_healpix_bulk
    implicit none

    !> Below this many elements the AUTOMATIC path stays serial.
    !!
    !! Not a setting, deliberately: it changes only how fast the library runs, never what it
    !! answers, and no caller has asked to control it -- admitting it as a knob would mean a
    !! default assertion, a round trip, an observed effect with a negative control, a reset, a
    !! printed row and an environment variable, for a number nobody names.
    !!
    !! **PROVISIONAL.** A thread-scaling measurement needs an idle machine and has not been taken;
    !! this value is a conservative placeholder chosen so that small calls cannot be made slower by
    !! threading them, and it is to be replaced by the measured crossover point (feature
    !! `feature_healpix_tier_b.md` section 7.8, target T2). An explicit `threads=` overrides it,
    !! on the rule that an explicit argument always wins.
    integer(int64), parameter :: hpx_parallel_min_elements = 20000_int64

contains

    module procedure hpx_threads
        integer :: n_req

        if (present(threads)) then
            if (threads < 1) error stop what // ": threads= must be at least 1"
            ! An explicit request is honoured whatever the size -- the caller has said what they
            ! want -- and is still clamped to what the affinity mask allows, because opening more
            ! threads than there are processors is slower than not threading at all.
            nt = parquet_clamp_to_affinity(threads, "healpix")
            return
        end if
        ! No cap of this tier's own: it has no thread setting, and section 6 of the design keeps it
        ! that way. `omp_get_level() == 0` inside `parquet_auto_thread_count` is what makes a call
        ! from inside somebody else's parallel region serial rather than nested.
        n_req = parquet_auto_thread_count(0, "healpix")
        nt = n_req
        if (n < hpx_parallel_min_elements) nt = 1
    end procedure hpx_threads

    module procedure hpx_check_bulk_sizes
        character(len=:), allocatable :: t_want, t_got, t_which
        integer :: k

        do k = 1, size(sizes)
            if (sizes(k) == n) cycle
            call hpx_itoa(n, t_want)
            call hpx_itoa(sizes(k), t_got)
            call hpx_itoa(int(k, int64), t_which)
            error stop what // ": every array must have the same extent; the first is " // &
                t_want // " but array " // t_which // " has " // t_got
        end do
    end procedure hpx_check_bulk_sizes

    module procedure hpx_ang2pix_ring_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        n = int(size(theta), int64)
        call hpx_check_bulk_sizes(n, [int(size(phi), int64), int(size(ipix), int64)], "pf_ang2pix_ring_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            call bad_nside(int(nside, int64), hpx_nside_max_i32, "pf_ang2pix_ring_bulk")
        nt = hpx_threads(threads, n, "pf_ang2pix_ring_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_ang2pix_ring(nside, theta(k), phi(k), ipix(k))
        end do
    end procedure hpx_ang2pix_ring_bulk_i32

    module procedure hpx_ang2pix_ring_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        n = int(size(theta), int64)
        call hpx_check_bulk_sizes(n, [int(size(phi), int64), int(size(ipix), int64)], "pf_ang2pix_ring_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) &
            call bad_nside(nside, hpx_nside_max, "pf_ang2pix_ring_bulk")
        nt = hpx_threads(threads, n, "pf_ang2pix_ring_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_ang2pix_ring(nside, theta(k), phi(k), ipix(k))
        end do
    end procedure hpx_ang2pix_ring_bulk_i64

    module procedure hpx_ang2pix_nest_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        n = int(size(theta), int64)
        call hpx_check_bulk_sizes(n, [int(size(phi), int64), int(size(ipix), int64)], "pf_ang2pix_nest_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            call bad_nside(int(nside, int64), hpx_nside_max_i32, "pf_ang2pix_nest_bulk")
        nt = hpx_threads(threads, n, "pf_ang2pix_nest_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_ang2pix_nest(nside, theta(k), phi(k), ipix(k))
        end do
    end procedure hpx_ang2pix_nest_bulk_i32

    module procedure hpx_ang2pix_nest_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        n = int(size(theta), int64)
        call hpx_check_bulk_sizes(n, [int(size(phi), int64), int(size(ipix), int64)], "pf_ang2pix_nest_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) &
            call bad_nside(nside, hpx_nside_max, "pf_ang2pix_nest_bulk")
        nt = hpx_threads(threads, n, "pf_ang2pix_nest_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_ang2pix_nest(nside, theta(k), phi(k), ipix(k))
        end do
    end procedure hpx_ang2pix_nest_bulk_i64

    module procedure hpx_pix2ang_ring_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        n = int(size(ipix), int64)
        call hpx_check_bulk_sizes(n, [int(size(theta), int64), int(size(phi), int64)], "pf_pix2ang_ring_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            call bad_nside(int(nside, int64), hpx_nside_max_i32, "pf_pix2ang_ring_bulk")
        nt = hpx_threads(threads, n, "pf_pix2ang_ring_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_pix2ang_ring(nside, ipix(k), theta(k), phi(k))
        end do
    end procedure hpx_pix2ang_ring_bulk_i32

    module procedure hpx_pix2ang_ring_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        n = int(size(ipix), int64)
        call hpx_check_bulk_sizes(n, [int(size(theta), int64), int(size(phi), int64)], "pf_pix2ang_ring_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) &
            call bad_nside(nside, hpx_nside_max, "pf_pix2ang_ring_bulk")
        nt = hpx_threads(threads, n, "pf_pix2ang_ring_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_pix2ang_ring(nside, ipix(k), theta(k), phi(k))
        end do
    end procedure hpx_pix2ang_ring_bulk_i64

    module procedure hpx_pix2ang_nest_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        n = int(size(ipix), int64)
        call hpx_check_bulk_sizes(n, [int(size(theta), int64), int(size(phi), int64)], "pf_pix2ang_nest_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            call bad_nside(int(nside, int64), hpx_nside_max_i32, "pf_pix2ang_nest_bulk")
        nt = hpx_threads(threads, n, "pf_pix2ang_nest_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_pix2ang_nest(nside, ipix(k), theta(k), phi(k))
        end do
    end procedure hpx_pix2ang_nest_bulk_i32

    module procedure hpx_pix2ang_nest_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        n = int(size(ipix), int64)
        call hpx_check_bulk_sizes(n, [int(size(theta), int64), int(size(phi), int64)], "pf_pix2ang_nest_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) &
            call bad_nside(nside, hpx_nside_max, "pf_pix2ang_nest_bulk")
        nt = hpx_threads(threads, n, "pf_pix2ang_nest_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_pix2ang_nest(nside, ipix(k), theta(k), phi(k))
        end do
    end procedure hpx_pix2ang_nest_bulk_i64

    module procedure hpx_vec2pix_ring_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        n = int(size(vec, 2), int64)
        if (size(vec, 1) /= 3) error stop "pf_vec2pix_ring_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(ipix), int64)], "pf_vec2pix_ring_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            call bad_nside(int(nside, int64), hpx_nside_max_i32, "pf_vec2pix_ring_bulk")
        nt = hpx_threads(threads, n, "pf_vec2pix_ring_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_vec2pix_ring(nside, vec(:, k), ipix(k))
        end do
    end procedure hpx_vec2pix_ring_bulk_i32

    module procedure hpx_vec2pix_ring_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        n = int(size(vec, 2), int64)
        if (size(vec, 1) /= 3) error stop "pf_vec2pix_ring_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(ipix), int64)], "pf_vec2pix_ring_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) &
            call bad_nside(nside, hpx_nside_max, "pf_vec2pix_ring_bulk")
        nt = hpx_threads(threads, n, "pf_vec2pix_ring_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_vec2pix_ring(nside, vec(:, k), ipix(k))
        end do
    end procedure hpx_vec2pix_ring_bulk_i64

    module procedure hpx_vec2pix_nest_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        n = int(size(vec, 2), int64)
        if (size(vec, 1) /= 3) error stop "pf_vec2pix_nest_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(ipix), int64)], "pf_vec2pix_nest_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            call bad_nside(int(nside, int64), hpx_nside_max_i32, "pf_vec2pix_nest_bulk")
        nt = hpx_threads(threads, n, "pf_vec2pix_nest_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_vec2pix_nest(nside, vec(:, k), ipix(k))
        end do
    end procedure hpx_vec2pix_nest_bulk_i32

    module procedure hpx_vec2pix_nest_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        n = int(size(vec, 2), int64)
        if (size(vec, 1) /= 3) error stop "pf_vec2pix_nest_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(ipix), int64)], "pf_vec2pix_nest_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) &
            call bad_nside(nside, hpx_nside_max, "pf_vec2pix_nest_bulk")
        nt = hpx_threads(threads, n, "pf_vec2pix_nest_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_vec2pix_nest(nside, vec(:, k), ipix(k))
        end do
    end procedure hpx_vec2pix_nest_bulk_i64

    module procedure hpx_pix2vec_ring_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        n = int(size(ipix), int64)
        if (size(vec, 1) /= 3) error stop "pf_pix2vec_ring_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(vec, 2), int64)], "pf_pix2vec_ring_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            call bad_nside(int(nside, int64), hpx_nside_max_i32, "pf_pix2vec_ring_bulk")
        nt = hpx_threads(threads, n, "pf_pix2vec_ring_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_pix2vec_ring(nside, ipix(k), vec(:, k))
        end do
    end procedure hpx_pix2vec_ring_bulk_i32

    module procedure hpx_pix2vec_ring_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        n = int(size(ipix), int64)
        if (size(vec, 1) /= 3) error stop "pf_pix2vec_ring_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(vec, 2), int64)], "pf_pix2vec_ring_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) &
            call bad_nside(nside, hpx_nside_max, "pf_pix2vec_ring_bulk")
        nt = hpx_threads(threads, n, "pf_pix2vec_ring_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_pix2vec_ring(nside, ipix(k), vec(:, k))
        end do
    end procedure hpx_pix2vec_ring_bulk_i64

    module procedure hpx_pix2vec_nest_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        n = int(size(ipix), int64)
        if (size(vec, 1) /= 3) error stop "pf_pix2vec_nest_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(vec, 2), int64)], "pf_pix2vec_nest_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            call bad_nside(int(nside, int64), hpx_nside_max_i32, "pf_pix2vec_nest_bulk")
        nt = hpx_threads(threads, n, "pf_pix2vec_nest_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_pix2vec_nest(nside, ipix(k), vec(:, k))
        end do
    end procedure hpx_pix2vec_nest_bulk_i32

    module procedure hpx_pix2vec_nest_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        n = int(size(ipix), int64)
        if (size(vec, 1) /= 3) error stop "pf_pix2vec_nest_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(vec, 2), int64)], "pf_pix2vec_nest_bulk")
        if (n == 0_int64) return
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) &
            call bad_nside(nside, hpx_nside_max, "pf_pix2vec_nest_bulk")
        nt = hpx_threads(threads, n, "pf_pix2vec_nest_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_pix2vec_nest(nside, ipix(k), vec(:, k))
        end do
    end procedure hpx_pix2vec_nest_bulk_i64

    module procedure hpx_ang2vec_bulk
        integer(int64) :: n, k
        integer :: nt

        n = int(size(theta), int64)
        if (size(vec, 1) /= 3) error stop "pf_ang2vec_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(phi), int64), int(size(vec, 2), int64)], &
                                  "pf_ang2vec_bulk")
        if (n == 0_int64) return
        nt = hpx_threads(threads, n, "pf_ang2vec_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_ang2vec(theta(k), phi(k), vec(:, k))
        end do
    end procedure hpx_ang2vec_bulk

    module procedure hpx_vec2ang_bulk
        integer(int64) :: n, k
        integer :: nt

        n = int(size(vec, 2), int64)
        if (size(vec, 1) /= 3) error stop "pf_vec2ang_bulk: vec must be shaped (3, n)"
        call hpx_check_bulk_sizes(n, [int(size(theta), int64), int(size(phi), int64)], &
                                  "pf_vec2ang_bulk")
        if (n == 0_int64) return
        nt = hpx_threads(threads, n, "pf_vec2ang_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call pf_vec2ang(vec(:, k), theta(k), phi(k))
        end do
    end procedure hpx_vec2ang_bulk

    !> Aborts because `nside` is not usable at the caller's integer kind.
    !>
    !> The bulk forms validate `nside` where the elemental forms they wrap do not, and that
    !> asymmetry is deliberate: the check is once per array rather than once per element, and the
    !> realistic mistake at a bulk call site is a bad `nside` read from a file, which a loop of a
    !> total procedure turns into millions of silently wrong pixels.
    subroutine bad_nside(nside, limit, what)
        integer(int64), intent(in) :: nside !! the offending resolution parameter.
        integer(int64), intent(in) :: limit !! the ceiling for the caller's integer kind.
        character(len=*), intent(in) :: what !! the calling entry point, for the message.
        character(len=:), allocatable :: t_got, t_lim

        call hpx_itoa(nside, t_got)
        call hpx_itoa(limit, t_lim)
        error stop what // ": nside must be a positive power of two at most " // t_lim // &
            ", got " // t_got
    end subroutine bad_nside

end submodule parquet_healpix_bulk
