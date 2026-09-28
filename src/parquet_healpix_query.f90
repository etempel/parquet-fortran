!> `pf_query_disc`: the ring walk, its argument validation, and the two public kinds.
!!
!! **The one intricate body in this module, and the reason it has a file of its own.** Enumerating
!! a disc on the pixelisation is closed-form rather than a search: the disc spans a contiguous band
!! of rings, and on each ring it spans a contiguous arc, so the whole answer is a walk over rings
!! emitting one run of indices each. Its cost is proportional to the pixels it emits rather than to
!! the pixels it examines, which is what a spatial index cannot match.
!!
!! **Where the exactness lives.** The membership rule is stated once, on a ring at a time: a pixel
!! is in the disc when the wrapped longitude difference between its centre and the disc's does not
!! exceed the half-width `dphi` that ring's latitude allows. That is the dot-product test
!! `centre . v0 >= cos(radius)` transformed through one `acos` per RING -- so a disc of a thousand
!! pixels evaluates about fifty inverse cosines rather than a thousand, and there is exactly one
!! rounding boundary in the whole query rather than one per pixel. The walk then only has to find
!! the ends of each arc, because within a ring the comparison is monotone in the longitude
!! difference: the interior needs no test at all, and an integer store is the entire per-pixel cost
!! of a RING result.
!!
!! **Why the band and the arc are deliberately generous.** The ring band is widened by one ring on
!! each side and each arc by one pixel at each end, and the ends are then trimmed by the membership
!! rule itself. So neither the band arithmetic nor the arc arithmetic has to be exact -- being
!! generous costs a handful of membership tests per query and being wrong is impossible, where a
!! tight-but-subtly-wrong bound would silently drop a pixel at the rim.
!!
!! **No IEEE exception escapes on validated input, which is the feature.** Every square root takes
!! an argument made non-negative by construction, every `acos` takes a clamped one, the single
!! division is guarded by the branch above it, and NaN cannot enter because the entry points reject
!! it. A caller running under `-ffpe-trap=zero,invalid` needs no guard around these calls -- which
!! is the whole reason this module exists rather than a dependency.
submodule(parquet_healpix) parquet_healpix_query
    implicit none

    !> How many ring runs `pf_query_disc_alloc` will record on its counting walk.
    !!
    !! The self-sizing form counts, allocates exactly, then emits. Recording where each run of
    !! pixels landed during the count turns the second pass from a repeat of the whole ring walk
    !! into pure stores -- measured at 22.7 microseconds against 10.4 for one disc at `nside` 1024
    !! and 2 degrees, so a little over half the call. A disc reaches at most two runs per ring, so
    !! this covers a disc spanning 512 rings; beyond that the walk is repeated as before, which is
    !! why this is a size rather than a limit. 32 kB of stack, per call and per thread.
    !!
    !! **Overflow shows as `nruns > hpx_alloc_runs_max`, not as a sentinel.** The walk counts every
    !! run whether or not it had room to record it, so the two callers below compare the count
    !! against this size to decide whether a replay is possible -- the same true-count contract
    !! `nlist` and `pf_query_disc_runs` both publish.
    integer(int64), parameter :: hpx_alloc_runs_max = 1024_int64

contains

    !> Writes one run of consecutive positions on one ring into the output, in either scheme and
    !> either integer kind.
    !>
    !> Shared by the walk itself and by `pf_query_disc_alloc`'s replay of its recorded runs, so
    !> that there is one statement of what a run of pixels looks like in the output rather than
    !> two that have to be kept agreeing.
    subroutine hpx_emit_run(nside, scheme, jr, first_idx, jstart, count, offset, out32, out64)
        integer(int64), intent(in) :: nside !! resolution parameter.
        integer, intent(in) :: scheme !! `PF_HP_RING` or `PF_HP_NEST`.
        integer(int64), intent(in) :: jr !! the ring, indexed from the north pole.
        integer(int64), intent(in) :: first_idx !! RING index of that ring's first pixel.
        integer(int64), intent(in) :: jstart !! first index within the ring, 0-based.
        integer(int64), intent(in) :: count !! how many consecutive pixels to write.
        integer(int64), intent(in) :: offset !! elements already written to the output.
        integer(int32), intent(inout), optional :: out32(:) !! int32 destination, or absent.
        integer(int64), intent(inout), optional :: out64(:) !! int64 destination, or absent.
        integer(int64) :: k

        ! The scheme and the output kind are decided once per run rather than per pixel, so a RING
        ! result costs one integer store per pixel and a NEST result one stepped Morton code.
        if (scheme == PF_HP_NEST) then
            call hpx_ringij2nest_run(nside, jr, jstart, count, offset, out32=out32, out64=out64)
        else if (present(out32)) then
            do k = 0_int64, count - 1_int64
                out32(offset + k + 1_int64) = int(first_idx + jstart + k, int32)
            end do
        else
            do k = 0_int64, count - 1_int64
                out64(offset + k + 1_int64) = first_idx + jstart + k
            end do
        end if
    end subroutine hpx_emit_run

    module procedure hpx_check_disc_args
        character(len=:), allocatable :: got, limit
        real(real64) :: scale

        if (.not. hpx_nside_ok(nside, nside_max)) then
            call hpx_itoa(nside, got)
            call hpx_itoa(nside_max, limit)
            error stop what // ": nside must be a positive power of two at most " // limit // &
                ", got " // got
        end if
        if (ieee_is_nan(radius)) error stop what // ": radius is NaN"
        if (radius < 0.0_real64) then
            call hpx_rtoa(radius, got)
            error stop what // ": radius must be at least zero, got " // got
        end if
        if (ieee_is_nan(vec(1)) .or. ieee_is_nan(vec(2)) .or. ieee_is_nan(vec(3))) then
            error stop what // ": the disc centre vector holds a NaN"
        end if
        ! Tested on the LARGEST COMPONENT rather than on the squared length. A direction is scale
        ! invariant, so `[1e-300, 0, 1e-300]` names a perfectly good one -- but squaring it
        ! underflows to zero, which a squared-length test rejects as a zero vector while raising
        ! IEEE_UNDERFLOW on the way. The mirror case overflows: `[1e300, 0, 1e300]` squares to
        ! infinity. Neither is exotic enough to be worth refusing, and refusing them by accident is
        ! worse than either. The walk normalises the same way for the same reason.
        scale = max(abs(vec(1)), abs(vec(2)), abs(vec(3)))
        if (.not. ieee_is_finite(scale)) then
            error stop what // ": the disc centre vector is not finite"
        end if
        if (scale <= 0.0_real64) then
            error stop what // ": the disc centre vector has zero length"
        end if
        if (scheme /= PF_HP_RING .and. scheme /= PF_HP_NEST) then
            call hpx_itoa(int(scheme, int64), got)
            error stop what // ": scheme must be PF_HP_RING (0) or PF_HP_NEST (1), got " // got
        end if
    end procedure hpx_check_disc_args

    module procedure hpx_query_disc_core
        real(real64) :: v0(3), vnorm, scale, z0, st0, phi0, r, cosr, sinr
        real(real64) :: zmax, zmin, zr, strr, denom, num, a, dphi, w, winv, half
        integer(int64) :: irmin, irmax, i, first, nr, shifted, jlo, jhi, cnt, tail, nr_prev
        integer(int64) :: runs_cap
        logical :: whole, counting, recording

        ! Neither output present is the COUNTING mode: the same walk with its stores switched off,
        ! which is what makes `pf_query_disc_count` unable to disagree with `pf_query_disc` by
        ! construction rather than by test.
        counting = .not. (present(out32) .or. present(out64))
        nr_prev = -1_int64
        recording = (present(runs64) .or. present(runs32)) .and. present(nruns)
        runs_cap = 0_int64
        if (present(runs64)) runs_cap = size(runs64, 2, int64)
        if (present(runs32)) runs_cap = size(runs32, 2, int64)
        if (present(nruns)) nruns = 0_int64
        nlist = 0_int64

        ! ---- The query direction, once ----
        ! Scaled by the largest component before squaring, so that a direction given in very small
        ! or very large components normalises to the same unit vector as any other -- see
        ! `hpx_check_disc_args`, which validates on the same quantity for the same reason.
        scale = max(abs(vec(1)), abs(vec(2)), abs(vec(3)))
        v0 = vec / scale
        vnorm = sqrt(v0(1) * v0(1) + v0(2) * v0(2) + v0(3) * v0(3))
        v0 = v0 / vnorm
        z0 = max(-1.0_real64, min(1.0_real64, v0(3)))
        st0 = sqrt(max(0.0_real64, (1.0_real64 - z0) * (1.0_real64 + z0)))
        ! A disc centred exactly on a POLE has v0(1) == v0(2) == 0, and ATAN2(0, 0) is prohibited
        ! by F2018 16.9.16 -- so this is a guard against non-conforming code on an entirely
        ! ordinary input, not against a caller mistake. nagfor returns NaN and raises
        ! IEEE_INVALID there (terminating the process under its default -ieee=stop); gfortran, ifx
        ! and flang return 0 and raise nothing, which is why the pole case went unnoticed.
        ! Longitude is undefined at a pole and every branch below that uses phi0 is guarded by
        ! st0 == 0, so any finite value is correct; zero is the conventional one.
        if (v0(1) == 0.0_real64 .and. v0(2) == 0.0_real64) then
            phi0 = 0.0_real64
        else
            phi0 = atan2(v0(2), v0(1))
            if (phi0 < 0.0_real64) phi0 = phi0 + PF_TWOPI
        end if

        r = min(radius, PF_PI)
        ! Inclusive mode is the exact walk at an enlarged radius, and the contract follows by
        ! construction rather than by an argument about the enumeration: a pixel overlapping the
        ! disc holds a point within r of the centre, that point is within max_pixrad of its own
        ! pixel's centre, so that centre is within r + max_pixrad and the enlarged exact walk
        ! returns it. The published upper bound is the same quantity, so both halves of the
        ! contract are the one radius below.
        if (inclusive) r = min(r + hpx_max_pixrad(nside), PF_PI)
        cosr = cos(r)

        ! ---- The band of rings the disc can reach ----
        !
        ! `cos(theta0 -+ r)` by the angle-addition identity, `z0 cos r +- st0 sin r`, with the two
        ! clamps as comparisons of `z0` against `+-cos r`: `theta0 <= r`, the disc holding the
        ! north pole, is `z0 >= cos r`, and `theta0 + r >= pi` is `z0 <= -cos r`. One `sin` where
        ! the direct form took an `acos` and two `cos`, and no worse: the band is widened by a ring
        ! on each side below, so a last-ulp difference from `cos(acos(z0) - r)` cannot change which
        ! pixels come back (verified identical on 16 000 discs at four resolutions).
        sinr = sin(r)
        if (z0 >= cosr) then
            zmax = 1.0_real64
        else
            zmax = z0 * cosr + st0 * sinr
        end if
        if (z0 <= -cosr) then
            zmin = -1.0_real64
        else
            zmin = z0 * cosr - st0 * sinr
        end if
        irmin = max(1_int64, hpx_ring_above(nside, zmax) - 1_int64)
        irmax = min(4_int64 * nside - 1_int64, hpx_ring_above(nside, zmin) + 1_int64)

        do i = irmin, irmax
            call hpx_ring_first(nside, i, first, nr, shifted)
            zr = hpx_ring_z(nside, i)
            strr = sqrt(max(0.0_real64, (1.0_real64 - zr) * (1.0_real64 + zr)))
            denom = st0 * strr
            num = cosr - z0 * zr
            whole = .false.
            dphi = 0.0_real64
            if (denom <= 0.0_real64) then
                ! The query centre is at a pole, so every pixel of a ring is equidistant from it
                ! and the ring is wholly in or wholly out. `strr` cannot vanish -- no ring centre
                ! sits at |z| = 1 -- so this branch is exactly the polar case, and it is also what
                ! keeps the division below from ever seeing a zero denominator.
                if (num > 0.0_real64) cycle
                whole = .true.
            else
                a = num / denom
                if (a <= -1.0_real64) then
                    ! Even the ring's farthest pixel is inside the disc.
                    whole = .true.
                else if (a > 1.0_real64) then
                    ! Even the ring's CLOSEST pixel is outside it, so nothing on this ring can be
                    ! in. Leaving `dphi` at zero instead and letting the trim decide would be
                    ! wrong, not merely slow: a pixel whose centre shares the disc's exact
                    ! longitude has a longitude difference of zero, so `abs(d) <= dphi` admits it
                    ! -- and an equator-centred disc sitting on the phi = 0 seam has exactly such a
                    ! pixel on every unshifted ring it reaches.
                    cycle
                else
                    ! a == 1 is the knife edge, and acos gives zero there: only a centre sharing
                    ! the disc's longitude exactly is in, which is the correct answer.
                    dphi = acos(a)
                end if
            end if

            ! `w` and `winv` depend only on the ring's LENGTH, and consecutive rings very often
            ! share one: every ring of the equatorial belt has 4*nside pixels, and the belt is
            ! most of the sphere. Recomputing them only when `nr` changes turns two divisions per
            ! ring into one comparison for every ring after the first of each run. Measured on the
            ! gate's own shape (nside 1024, RING, exact, 0.5 deg), the pair was about 13 % of the
            ! per-ring cost.
            !
            ! `winv` is the reciprocal the arc ends below divide by three times. It is formed as a
            ! multiplication rather than a division, and that -- like the reciprocal itself -- is
            ! NOT bit-identical: it can move an end by one pixel. It is safe for the reason this
            ! file's header already gives: the arc is deliberately generous and then trimmed by
            ! the membership rule, so the arc arithmetic does not have to be exact. `w` is a
            ! different matter and stays an exact division, because the rule itself is written in
            ! terms of it.
            if (nr /= nr_prev) then
                w = PF_TWOPI / real(nr, real64)
                winv = real(nr, real64) * hpx_inv_twopi
                nr_prev = nr
            end if
            half = 0.5_real64 * real(shifted, real64)

            if (whole) then
                jlo = 0_int64
                cnt = nr
            else
                ! A generous arc, then trimmed by the membership rule. The division only finds
                ! the ends; the rule decides them.
                jlo = floor((phi0 - dphi) * winv - half, int64) - 1_int64
                jhi = ceiling((phi0 + dphi) * winv - half, int64) + 1_int64
                if (jhi - jlo + 1_int64 >= nr) then
                    ! The window has grown past a full ring, which on a short ring the two-pixel
                    ! margin alone can do. It must still be TRIMMED rather than emitted whole: only
                    ! the `a <= -1` branch above establishes that a ring lies entirely inside the
                    ! disc, and emitting an untrimmed ring here returns pixels the disc does not
                    ! contain. So the window is recentred on the disc's own longitude and clamped
                    ! to exactly one revolution, which puts its two ends at the largest longitude
                    ! difference on the ring -- where the trim below starts.
                    jlo = nint(phi0 * winv - half, int64) - nr / 2_int64
                    jhi = jlo + nr - 1_int64
                end if
                do while (jlo <= jhi)
                    if (in_disc(jlo)) exit
                    jlo = jlo + 1_int64
                end do
                do while (jhi > jlo)
                    if (in_disc(jhi)) exit
                    jhi = jhi - 1_int64
                end do
                if (jlo > jhi) cycle
                cnt = jhi - jlo + 1_int64
                ! Wrap the run's start into 0 .. nr-1 by adjustment rather than by `modulo`, whose
                ! int64 form is a hardware integer division -- about 13 % of the per-ring cost on
                ! the gate's shape, for a value that is at most one revolution out. `jlo` is
                ! bounded by construction (the arc spans at most a full ring plus its two-pixel
                ! margin, and the recentring branch above clamps to one revolution), so each loop
                ! runs at most once in practice; they are loops rather than `if`s so that the
                ! result is `modulo` unconditionally, not merely within that bound.
                do while (jlo < 0_int64)
                    jlo = jlo + nr
                end do
                do while (jlo >= nr)
                    jlo = jlo - nr
                end do
            end if

            if (nlist + cnt > cap) call report_full(cnt)
            if (jlo + cnt <= nr) then
                call emit_block(i, first, jlo, cnt)
            else
                ! The arc crosses the phi = 0 seam. Emitting the low block first is what keeps the
                ! RING result ascending overall, which is half of this procedure's ordering promise.
                tail = jlo + cnt - nr
                call emit_block(i, first, 0_int64, tail)
                call emit_block(i, first, jlo, nr - jlo)
            end if
        end do

    contains

        !> Whether the pixel at index `j` of the current ring lies within the disc.
        !>
        !> The membership rule, and the exact specification of `inclusive = .false.`. `j` may lie
        !> outside `0 .. nr-1`: the longitude it names is the same angle a whole revolution away,
        !> which is what lets the caller test an arc that straddles the seam without wrapping first.
        pure logical function in_disc(j) result(inside)
            integer(int64), intent(in) :: j !! index within the current ring; any integer.
            real(real64) :: d

            d = (real(j, real64) + half) * w - phi0
            ! **The wrap is written out rather than left to `modulo`, and that is not a micro-
            ! optimisation.** gfortran compiles real `modulo` to the legacy x87 partial-remainder
            ! sequence (`fprem`/`fnstsw`/`sahf`), which `perf` measured at **71% of this whole
            ! walk** -- one source line accounting for nearly half of `pf_query_disc`. Replacing it
            ! made the query 1.3-1.6x faster with byte-identical output.
            !
            ! It is exact rather than merely close, which is what makes it safe on a disc rim where
            ! a last-bit difference decides membership. `d` is within one revolution of the target
            ! interval (the arc spans at most a full ring plus a two-pixel margin), so one step
            ! suffices; the subtract is exact by Sterbenz's lemma and the add is the same single
            ! rounding `modulo` itself performs. The `modulo` fallbacks keep the equivalence
            ! unconditional rather than resting on that bound.
            d = d + PF_PI
            if (d < 0.0_real64) then
                d = d + PF_TWOPI
                if (d < 0.0_real64) d = modulo(d, PF_TWOPI)
            else if (d >= PF_TWOPI) then
                ! UNREACHABLE, and kept so that the wrap is unconditional rather than resting on a
                ! bound. `d` here is `(j + half)*w - phi0 + pi` for a `j` the trim tests, and the
                ! largest such `j` is the arc's own `jhi = ceiling(B) + 1`, for
                ! `B = (phi0 + dphi)*winv - half`; that gives `d = pi + dphi + w*(1 + f)` with
                ! `f = ceiling(B) - B` in [0, 1), so reaching `2*pi` needs
                ! `dphi*nr/pi >= nr - 2 - 2f`. But the window is recentred and clamped to one
                ! revolution as soon as `ceiling(B) - floor(A) + 3 >= nr`, whose left side is
                ! `dphi*nr/pi + f + a + 3` for `a = A - floor(A)` in [0, 1) -- and under that same
                ! premise it is at least `nr + 1 - f`, which exceeds `nr`. So every arc wide enough
                ! to reach here is recentred first, and a recentred window's largest `d` is
                ! `e + 2*pi - w` for `|e| <= w/2`, half a pixel short. Measured on 1.69e10 `in_disc`
                ! evaluations over nside 1 .. 64, 61 latitudes, 41 longitudes and 400 radii in both
                ! modes: the largest `d` reached was 6.2706 against `2*pi` = 6.2832.
                ! GCOVR_EXCL_START
                d = d - PF_TWOPI
                if (d >= PF_TWOPI) d = modulo(d, PF_TWOPI)
                ! GCOVR_EXCL_STOP
            end if
            d = d - PF_PI
            inside = abs(d) <= dphi
        end function in_disc

        !> Appends `count` pixels of ring `jr`, starting at index `jstart` within that ring.
        subroutine emit_block(jr, first_idx, jstart, count)
            integer(int64), intent(in) :: jr !! the ring, indexed from the north pole.
            integer(int64), intent(in) :: first_idx !! RING index of that ring's first pixel.
            integer(int64), intent(in) :: jstart !! first index within the ring, 0-based.
            integer(int64), intent(in) :: count !! how many consecutive pixels to append.

            ! **`nruns` counts every run, whether or not there was room to store it**, so a
            ! caller detects a short buffer by comparing it against the buffer's own size -- the
            ! same true-count contract `nlist` has, and what `pf_query_disc_runs` publishes.
            ! Overflowing is not an error on either path: `pf_query_disc_alloc` falls back to a
            ! second walk, and a public caller re-queries with a larger buffer.
            if (recording) then
                nruns = nruns + 1_int64
                if (nruns <= runs_cap) then
                    if (present(runs32)) then
                        ! The RING pixel form: one run is `first .. first + count - 1`. It is
                        ! meaningless for NEST, where a run is not contiguous -- which is why
                        ! `pf_query_disc_runs` is RING-only rather than taking a `scheme`.
                        runs32(1, nruns) = int(first_idx + jstart, int32)
                        runs32(2, nruns) = int(count, int32)
                    else if (size(runs64, 1) == 2) then
                        runs64(1, nruns) = first_idx + jstart
                        runs64(2, nruns) = count
                    else
                        ! The walk's own form, which `pf_query_disc_alloc` replays. It carries the
                        ! ring and the intra-ring offset because a NEST replay needs both.
                        runs64(1, nruns) = jr
                        runs64(2, nruns) = first_idx
                        runs64(3, nruns) = jstart
                        runs64(4, nruns) = count
                    end if
                end if
            end if
            if (counting) then
                ! Nothing to store, and nothing about the count depends on the scheme: a bijection
                ! between the two numberings cannot change how many pixels there are.
                nlist = nlist + count
                return
            end if
            call hpx_emit_run(nside, scheme, jr, first_idx, jstart, count, nlist, &
                              out32=out32, out64=out64)
            nlist = nlist + count
        end subroutine emit_block

        !> Aborts because `listpix` cannot hold the result.
        subroutine report_full(needed)
            integer(int64), intent(in) :: needed !! pixels the run about to be emitted holds.
            character(len=:), allocatable :: t_cap, t_have, t_nside, t_rad

            call hpx_itoa(cap, t_cap)
            call hpx_itoa(nlist + needed, t_have)
            call hpx_itoa(nside, t_nside)
            call hpx_rtoa(radius, t_rad)
            ! The capacity, the count reached so far, and the query that produced it: without the
            ! last two a caller cannot size the buffer without guessing, which is the whole reason
            ! this abort exists rather than a silent truncation.
            error stop what // ": listpix holds " // t_cap // " elements but the disc needs " // &
                "at least " // t_have // " (nside " // t_nside // ", radius " // t_rad // " rad)"
        end subroutine report_full

    end procedure hpx_query_disc_core

    module procedure hpx_query_disc_i64
        integer :: sch
        logical :: inc

        sch = PF_HP_RING
        if (present(scheme)) sch = scheme
        inc = .false.
        if (present(inclusive)) inc = inclusive
        call hpx_check_disc_args(nside, hpx_nside_max, vec, radius, sch, "pf_query_disc")
        call hpx_query_disc_core(nside, vec, radius, sch, inc, nlist, &
                                 int(size(listpix), int64), "pf_query_disc", out64=listpix)
    end procedure hpx_query_disc_i64

    module procedure hpx_query_disc_i32
        integer :: sch
        logical :: inc
        integer(int64) :: n64

        sch = PF_HP_RING
        if (present(scheme)) sch = scheme
        inc = .false.
        if (present(inclusive)) inc = inclusive
        ! The int32 ceiling is checked here rather than inside the walk, because it is a property
        ! of the CALLER's integer kind rather than of the pixelisation: nside 16384 is perfectly
        ! valid and simply cannot be addressed with a 32-bit pixel index.
        call hpx_check_disc_args(int(nside, int64), hpx_nside_max_i32, vec, radius, sch, &
                                 "pf_query_disc")
        call hpx_query_disc_core(int(nside, int64), vec, radius, sch, inc, n64, &
                                 int(size(listpix), int64), "pf_query_disc", out32=listpix)
        nlist = int(n64, int32)
    end procedure hpx_query_disc_i32

    ! ---- Counting, and the self-sizing form ----
    !
    ! Both reach the same walk as `pf_query_disc`. Nothing here re-derives a ring bound, an arc or
    ! a membership rule -- three implementations of "which pixels are in this disc" would be three
    ! things to keep agreeing, and the count's whole value is that it answers the same question the
    ! list does.

    module procedure hpx_query_disc_count_i64
        integer :: sch
        logical :: inc

        sch = PF_HP_RING
        if (present(scheme)) sch = scheme
        inc = .false.
        if (present(inclusive)) inc = inclusive
        call hpx_check_disc_args(nside, hpx_nside_max, vec, radius, sch, "pf_query_disc_count")
        ! No output array, so the walk counts; `huge` as the capacity makes the abort unreachable,
        ! which it must be -- there is no buffer here to be too small. The count itself cannot
        ! overflow either kind: the largest possible answer is npix, 805306368 at the int32 ceiling
        ! and 3458764513820540928 at the int64 one, both comfortably inside.
        call hpx_query_disc_core(nside, vec, radius, sch, inc, nlist, huge(0_int64), &
                                 "pf_query_disc_count")
    end procedure hpx_query_disc_count_i64

    module procedure hpx_query_disc_count_i32
        integer :: sch
        logical :: inc
        integer(int64) :: n64

        sch = PF_HP_RING
        if (present(scheme)) sch = scheme
        inc = .false.
        if (present(inclusive)) inc = inclusive
        call hpx_check_disc_args(int(nside, int64), hpx_nside_max_i32, vec, radius, sch, &
                                 "pf_query_disc_count")
        call hpx_query_disc_core(int(nside, int64), vec, radius, sch, inc, n64, huge(0_int64), &
                                 "pf_query_disc_count")
        nlist = int(n64, int32)
    end procedure hpx_query_disc_count_i32

    module procedure hpx_query_disc_max_count_i64
        nmax = hpx_disc_max_count(nside, hpx_nside_max, radius, inclusive)
    end procedure hpx_query_disc_max_count_i64

    module procedure hpx_query_disc_max_count_i32
        nmax = int(hpx_disc_max_count(int(nside, int64), hpx_nside_max_i32, radius, inclusive), int32)
    end procedure hpx_query_disc_max_count_i32

    !> The position-free pixel-count bound both `pf_query_disc_max_count` kinds return.
    !>
    !> **The bound is a packing argument, not a geometric enumeration.** Every pixel has area
    !> `4*pi/npix` exactly and lies wholly within `pf_max_pixrad(nside)` of its own centre, so a
    !> pixel the query can return lies entirely inside the cap of radius `t` about the disc
    !> centre, where `t` is the membership radius plus one `max_pixrad`. Those pixels are
    !> disjoint, so `n * 4*pi/npix <= 2*pi*(1 - cos t)`, i.e. `n <= npix * sin(t/2)**2`. Written
    !> as a half-angle sine rather than as `(1 - cos t)/2` because the latter cancels to nothing
    !> for the small `t` that matter most here.
    !>
    !> **`inclusive` widens the MEMBERSHIP radius, which is a second `max_pixrad` on top of the
    !> containment one.** `pf_query_disc`'s published contract for that mode is that no pixel
    !> whose centre lies farther than `radius + max_pixrad` is returned, so the centres in play
    !> reach that far and their pixels reach one `max_pixrad` beyond it.
    !>
    !> **The margins are what stop a rounding error inverting the bound**, which would be the one
    !> failure worth having: a buffer sized from an under-estimate overflows. The relative term
    !> covers `real(npix, real64)` losing the low bits of an `npix` above 2**53 (it reaches
    !> 3.5e18 at the `nside` ceiling) and the rounding of `sin`; the absolute term covers a small
    !> count, where a relative margin is worth nothing. Both are far below what any caller would
    !> notice, and the clamp to `npix` keeps the whole-sphere answer exact.
    function hpx_disc_max_count(nside, nside_max, radius, inclusive) result(nmax)
        integer(int64), intent(in) :: nside !! resolution parameter, checked here.
        integer(int64), intent(in) :: nside_max !! the ceiling for the caller's integer kind.
        real(real64), intent(in) :: radius !! disc radius, radians, checked here.
        logical, intent(in), optional :: inclusive !! bound the overlap superset; default `.false.`.
        integer(int64) :: nmax !! upper bound on the pixel count, at any position.

        real(real64), parameter :: rel_margin = 1.0e-12_real64 !! covers real64 rounding of the product.
        integer(int64), parameter :: abs_margin = 8_int64 !! covers rounding where the count is tiny.
        !> A placeholder direction, so that the shared validator can check `nside` and `radius`
        !! with the same messages the rest of the family uses. It is a `parameter` rather than an
        !! array constructor at the call because an explicit-shape dummy makes ifx build an array
        !! temporary for a constructor, and report one on every call under `-check`.
        real(real64), parameter :: axis(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        real(real64) :: t, npix_r, bound
        integer(int64) :: npix
        logical :: inc

        inc = .false.
        if (present(inclusive)) inc = inclusive
        ! `axis` and PF_HP_RING are both valid by construction, so only nside and radius can fire.
        call hpx_check_disc_args(nside, nside_max, axis, radius, PF_HP_RING, "pf_query_disc_max_count")
        npix = 12_int64 * nside * nside
        t = radius + hpx_max_pixrad(nside)
        if (inc) t = t + hpx_max_pixrad(nside)
        ! **Past pi the formula turns around and becomes an UNDER-estimate**, which is the one
        ! way this routine could do real damage: `sin(t/2)**2` peaks at `t = pi` and decreases
        ! after it, so a hemisphere-and-a-half disc would be bounded below its true count and the
        ! caller's buffer would overflow. Clamping here rather than clamping `t` also keeps `sin`
        ! away from a `radius` of infinity, which passes validation -- only NaN and a negative are
        ! refused -- and would otherwise reach `sin(infinity)`: a NaN, and IEEE_INVALID raised,
        ! which ends a NAG process. The cap is the whole sphere either way, so the answer is
        ! exactly `npix` and no arithmetic is needed. (The negated spelling is belt-and-braces
        ! against a NaN `t`; `t > PF_PI` behaves identically for every input reachable today,
        ! confirmed by mutation.)
        if (.not. (t < PF_PI)) then
            nmax = npix
            return
        end if
        npix_r = real(npix, real64)
        bound = npix_r * sin(0.5_real64 * t)**2
        nmax = min(npix, ceiling(bound * (1.0_real64 + rel_margin), int64) + abs_margin)
    end function hpx_disc_max_count

    module procedure hpx_query_disc_alloc_i64
        integer :: sch
        logical :: inc
        integer(int64) :: cap, nruns, runs(4, hpx_alloc_runs_max)

        sch = PF_HP_RING
        if (present(scheme)) sch = scheme
        inc = .false.
        if (present(inclusive)) inc = inclusive
        call hpx_check_disc_args(nside, hpx_nside_max, vec, radius, sch, "pf_query_disc_alloc")
        ! Count, allocate exactly, emit. Counting first buys an exact size with no allocation
        ! inside the ring loop and no over-allocation -- against a doubling buffer, which would
        ! allocate mid-walk and could hand back twice the memory the answer needs.
        !
        ! **The counting walk also records where each run of pixels landed**, so the emitting pass
        ! is a replay of those runs rather than a second walk of the geometry. That is the whole
        ! difference between this form costing `count + walk` and costing `count + stores`;
        ! measured, it halves the call. A disc too large to record falls back to the second walk,
        ! which is why `hpx_alloc_runs_max` is a buffer size and not a limit on anything.
        call hpx_query_disc_core(nside, vec, radius, sch, inc, nlist, huge(0_int64), &
                                 "pf_query_disc_alloc", runs64=runs, nruns=nruns)
        ! Allocated and ZERO-LENGTH when the disc is empty, never unallocated, so that `size()` is
        ! the only thing a caller ever tests.
        allocate (listpix(nlist))
        if (nlist > 0_int64) then
            if (nruns > 0_int64 .and. nruns <= hpx_alloc_runs_max) then
                call replay_runs(nside, sch, runs, nruns, out64=listpix)
            else
                ! `cap` is a COPY of the count, not `nlist` itself: passing one variable to both an
                ! `intent(out)` and an `intent(in)` dummy of the same call is illegal aliasing
                ! (F2018 15.5.2.13), and gfortran resolves it by zeroing the variable on entry --
                ! so the capacity would read as 0 and the walk would abort on its first pixel.
                cap = nlist
                call hpx_query_disc_core(nside, vec, radius, sch, inc, nlist, cap, &
                                         "pf_query_disc_alloc", out64=listpix)
            end if
        end if
    end procedure hpx_query_disc_alloc_i64

    module procedure hpx_query_disc_alloc_i32
        integer :: sch
        logical :: inc
        integer(int64) :: n64, cap, nruns, runs(4, hpx_alloc_runs_max)

        sch = PF_HP_RING
        if (present(scheme)) sch = scheme
        inc = .false.
        if (present(inclusive)) inc = inclusive
        call hpx_check_disc_args(int(nside, int64), hpx_nside_max_i32, vec, radius, sch, &
                                 "pf_query_disc_alloc")
        call hpx_query_disc_core(int(nside, int64), vec, radius, sch, inc, n64, huge(0_int64), &
                                 "pf_query_disc_alloc", runs64=runs, nruns=nruns)
        allocate (listpix(n64))
        if (n64 > 0_int64) then
            if (nruns > 0_int64 .and. nruns <= hpx_alloc_runs_max) then
                call replay_runs(int(nside, int64), sch, runs, nruns, out32=listpix)
            else
                ! A copy, for the aliasing reason `hpx_query_disc_alloc_i64` states.
                cap = n64
                call hpx_query_disc_core(int(nside, int64), vec, radius, sch, inc, n64, cap, &
                                         "pf_query_disc_alloc", out32=listpix)
            end if
        end if
        nlist = int(n64, int32)
    end procedure hpx_query_disc_alloc_i32

    ! ---- The run form ----
    !
    ! The same walk once more, with its per-pixel stores switched off and its per-RUN record
    ! switched on. Nothing here re-derives a ring bound, an arc or a membership rule, for the
    ! reason the counting form's banner above gives -- the runs ARE the walk's own, so a run list
    ! cannot disagree with the pixel list `pf_query_disc` would return for the same query.

    module procedure hpx_query_disc_runs_i64
        logical :: inc
        integer(int64) :: nlist

        inc = .false.
        if (present(inclusive)) inc = inclusive
        call hpx_check_disc_args(nside, hpx_nside_max, vec, radius, PF_HP_RING, &
                                 "pf_query_disc_runs")
        call hpx_check_runs_rows(size(runs, 1), "pf_query_disc_runs")
        ! No pixel output, so the walk stores no pixels and `huge` makes the capacity abort
        ! unreachable -- there is no pixel buffer here to be too small. The RUN buffer's capacity
        ! is deliberately not an abort either: `nruns` reports the true count and the caller
        ! decides whether to grow and re-query. `nlist` is discarded; `pf_query_disc_count`
        ! answers that question and this one does not duplicate it.
        call hpx_query_disc_core(nside, vec, radius, PF_HP_RING, inc, nlist, huge(0_int64), &
                                 "pf_query_disc_runs", runs64=runs, nruns=nruns)
    end procedure hpx_query_disc_runs_i64

    module procedure hpx_query_disc_runs_i32
        logical :: inc
        integer(int64) :: nlist, nruns64

        inc = .false.
        if (present(inclusive)) inc = inclusive
        ! The int32 ceiling is a property of the CALLER's integer kind rather than of the
        ! pixelisation, exactly as it is for `pf_query_disc`: nside 16384 is valid and simply
        ! cannot be addressed with a 32-bit pixel index. Checking it here is what makes the
        ! narrowing store in the walk safe.
        call hpx_check_disc_args(int(nside, int64), hpx_nside_max_i32, vec, radius, PF_HP_RING, &
                                 "pf_query_disc_runs")
        call hpx_check_runs_rows(size(runs, 1), "pf_query_disc_runs")
        call hpx_query_disc_core(int(nside, int64), vec, radius, PF_HP_RING, inc, nlist, &
                                 huge(0_int64), "pf_query_disc_runs", runs32=runs, nruns=nruns64)
        ! A run count cannot overflow int32: it is at most two per ring and there are fewer than
        ! 4*nside rings, so nside 8192 -- the int32 ceiling -- bounds it by 65536.
        nruns = int(nruns64, int32)
    end procedure hpx_query_disc_runs_i32

    !> Aborts unless a run buffer has the two rows the contract requires.
    !>
    !> A caller who passes the four-row shape this module records internally would otherwise get
    !> its first two rows filled and the other two left undefined, which reads as a working call.
    !> The row count IS the contract, so it is checked rather than assumed.
    subroutine hpx_check_runs_rows(nrows, what)
        integer, intent(in) :: nrows !! rows of the caller's run buffer.
        character(len=*), intent(in) :: what !! calling entry point, for the message.
        character(len=:), allocatable :: got

        if (nrows /= 2) then
            call hpx_itoa(int(nrows, int64), got)
            error stop what // ": runs must have exactly 2 rows (first pixel, length), got " // got
        end if
    end subroutine hpx_check_runs_rows

    !> Writes the pixels of every recorded run, in the order the walk emitted them.
    !>
    !> The ordering promise of `pf_query_disc` is a property of the walk, and this preserves it
    !> for free by replaying the runs in the order they were recorded rather than reconstructing
    !> one.
    subroutine replay_runs(nside, scheme, runs, nruns, out32, out64)
        integer(int64), intent(in) :: nside !! resolution parameter.
        integer, intent(in) :: scheme !! `PF_HP_RING` or `PF_HP_NEST`.
        integer(int64), intent(in) :: runs(:,:) !! the recorded runs, four rows per `hpx_emit_run`.
        integer(int64), intent(in) :: nruns !! how many columns of `runs` are in use.
        integer(int32), intent(inout), optional :: out32(:) !! int32 destination, or absent.
        integer(int64), intent(inout), optional :: out64(:) !! int64 destination, or absent.
        integer(int64) :: k, filled

        filled = 0_int64
        do k = 1_int64, nruns
            call hpx_emit_run(nside, scheme, runs(1, k), runs(2, k), runs(3, k), runs(4, k), &
                              filled, out32=out32, out64=out64)
            filled = filled + runs(4, k)
        end do
    end subroutine replay_runs

end submodule parquet_healpix_query
