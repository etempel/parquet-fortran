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

    !> Elements per thread the automatic path insists on before it opens another one.
    !!
    !! **The rule is a team size bounded by the work available, not a threshold on the call.** A
    !! single element count cannot serve both a small machine and a large one: set it high enough
    !! that a 192-thread team is never opened below its own crossover and an 8-core machine loses
    !! threading entirely for every array beneath that; set it low enough for the 8-core machine
    !! and the large one takes a measured regression on the default path. What is stable across
    !! both is the work *one* thread needs to be worth waking, so that is what this number is, and
    !! the team follows from it: `nt = min(nt_auto, n / hpx_min_elements_per_thread)`. Threading
    !! then begins at twice this many elements (the first point at which two threads are
    !! justified) and reaches the full team only when there is full-team work to do.
    !!
    !! Not a setting, deliberately: it changes only how fast the library runs, never what it
    !! answers, and no caller has asked to control it -- admitting it as a knob would mean a
    !! default assertion, a round trip, an observed effect with a negative control, a reset, a
    !! printed row and an environment variable, for a number nobody names.
    !!
    !! An explicit `threads=` overrides all of it, on the rule that an explicit argument always
    !! wins.
    !! **Measured, not guessed.** `bench/benchmark_healpix.sh --mode=cross` walks a ladder from
    !! 100 to 10^6 elements and reports, per form and per team size, the first rung at which
    !! `threads=N` is strictly faster than `threads=1`. Across all 18 specifics on machine B
    !! (gfortran 15.2.1, 384 processors, idle) the worst two-thread crossover was **1000 elements**
    !! -- 500 per thread -- and every larger team crossed over at 100-2000 elements, i.e. 16-31 per
    !! thread. The value below is twice the worst of those, so the serial-to-threaded transition
    !! carries a factor of two in hand and every larger team carries thirty to sixty.
    integer(int64), parameter :: hpx_min_elements_per_thread = 1000_int64

    !> The largest team the AUTOMATIC path will open, whatever the machine offers.
    !!
    !! Measured rather than assumed: see `feature_healpix_tier_b.md` section 15. Past this size
    !! libgomp's own fork/join cost grows faster than the work another thread removes, for a tier
    !! whose per-element work is tens of nanoseconds. A caller who wants the whole machine can
    !! still ask for it with `threads=`.
    integer, parameter :: hpx_max_auto_threads = 64

contains

    !> The smallest array the automatic path will hand to a team of `nt`.
    !!
    !! The threshold stated as a function of the team, which is the form the rule is easiest to
    !! reason about: `hpx_threads` applies it the other way round, deriving the team from the
    !! array. Both say that every thread opened gets `hpx_min_elements_per_thread` elements.
    pure function hpx_parallel_min_elements(nt) result(nmin)
        integer, intent(in) :: nt !! team size being considered, >= 1.
        integer(int64) :: nmin !! elements below which that team is not worth opening.

        nmin = hpx_min_elements_per_thread * int(nt, int64)
    end function hpx_parallel_min_elements

    !> The cap the automatic path hands to `parquet_auto_thread_count`: this tier's measured
    !! ceiling, lowered by `healpix_threads` when the user has set one.
    !!
    !! **The `min` is what makes the knob a cap rather than a request.** Taking the user's value
    !! outright would let `parquet_set_healpix_threads(1024)` raise the ceiling that
    !! `hpx_max_auto_threads` exists to impose. The index tier's `index_threads` does replace its
    !! tier's ceiling (`ix_auto_cap`, src/parquet_index_map.f90), because there the ceiling is a
    !! default the user may move; here it is a limit. A caller who genuinely wants more passes
    !! `threads=` on the call, which bypasses this path entirely.
    pure function hpx_auto_cap() result(cap)
        integer :: cap !! cap to pass on; always >= 1.

        cap = hpx_max_auto_threads
        if (cfg_healpix_threads > 0 .and. cfg_healpix_threads < cap) cap = cfg_healpix_threads
    end function hpx_auto_cap

    module procedure hpx_threads_n64
        ! A pass-through onto the one resolver, deliberately: a second copy of the rule here could
        ! answer differently from what a bulk call actually does, which is the one thing this
        ! procedure exists to rule out. `what` is only ever used for the explicit-request abort,
        ! which this path cannot reach because it passes no request.
        nt = hpx_threads(n=n, what="pf_healpix_threads")
    end procedure hpx_threads_n64

    module procedure hpx_threads_n32
        ! Widening cannot overflow, so this specific exists only so that a default integer literal
        ! resolves -- `pf_healpix_threads(1000)` rather than `pf_healpix_threads(1000_int64)`.
        nt = hpx_threads(n=int(n, int64), what="pf_healpix_threads")
    end procedure hpx_threads_n32

    module procedure hpx_threads
        integer :: n_req
        integer(int64) :: nt_work

        if (present(threads)) then
            if (threads < 1) error stop what // ": threads= must be at least 1"
            ! An explicit request is honoured whatever the size -- the caller has said what they
            ! want -- and is still clamped to what the affinity mask allows, because opening more
            ! threads than there are processors is slower than not threading at all.
            nt = parquet_clamp_to_affinity(threads, "healpix")
            return
        end if
        ! `omp_get_level() == 0` inside `parquet_auto_thread_count` is what makes a call from
        ! inside somebody else's parallel region serial rather than nested; the cap is this tier's
        ! own and is passed through the same helper rather than applied afterwards, so that the
        ! affinity clamp still has the last word.
        !
        ! The cap is the SMALLER of this tier's measured ceiling and the user's `healpix_threads`,
        ! which is what makes the knob a cap rather than a request: it can only lower the automatic
        ! answer. `cfg_healpix_threads == 0` means automatic and leaves the ceiling alone. Asking
        ! for more than the ceiling is done with an explicit `threads=`, handled above.
        n_req = parquet_auto_thread_count(hpx_auto_cap(), "healpix")
        ! Bound the team by the work available. Asking for one thread per full block of
        ! `hpx_min_elements_per_thread` is the same statement as requiring at least
        ! `hpx_parallel_min_elements(nt)` elements before a team of `nt` is opened, and it
        ! degrades correctly in both directions: on a small machine `n_req` binds and this never
        ! fires, while on a large one a mid-sized array gets the few threads that pay rather than
        ! a full team that does not.
        if (n < hpx_parallel_min_elements(2)) then
            ! Not even a second thread is justified. This is the direct successor of the single
            ! constant this rule replaced, and the only difference is that the number it compares
            ! against is now derived from the team rather than fixed.
            nt = 1
        else
            nt_work = n / hpx_min_elements_per_thread
            nt = n_req
            if (nt_work < int(nt, int64)) nt = int(nt_work)
        end if
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
