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
        real(real64) :: v0(3), vnorm, scale, z0, st0, phi0, r, cosr, theta0
        real(real64) :: zmax, zmin, zr, strr, denom, num, a, dphi, w, winv, half
        integer(int64) :: irmin, irmax, i, first, nr, shifted, jlo, jhi, cnt, tail
        logical :: whole, counting, recording

        ! Neither output present is the COUNTING mode: the same walk with its stores switched off,
        ! which is what makes `pf_query_disc_count` unable to disagree with `pf_query_disc` by
        ! construction rather than by test.
        counting = .not. (present(out32) .or. present(out64))
        recording = present(runs) .and. present(nruns)
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
            if (phi0 < 0.0_real64) phi0 = phi0 + hpx_twopi
        end if

        r = min(radius, hpx_pi)
        ! Inclusive mode is the exact walk at an enlarged radius, and the contract follows by
        ! construction rather than by an argument about the enumeration: a pixel overlapping the
        ! disc holds a point within r of the centre, that point is within max_pixrad of its own
        ! pixel's centre, so that centre is within r + max_pixrad and the enlarged exact walk
        ! returns it. The published upper bound is the same quantity, so both halves of the
        ! contract are the one radius below.
        if (inclusive) r = min(r + hpx_max_pixrad(nside), hpx_pi)
        cosr = cos(r)

        ! ---- The band of rings the disc can reach ----
        theta0 = acos(z0)
        zmax = cos(max(theta0 - r, 0.0_real64))
        zmin = cos(min(theta0 + r, hpx_pi))
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

            w = hpx_twopi / real(nr, real64)
            ! The arc ends below divide by `w` three times; one reciprocal serves all three. This
            ! is the one arithmetic change in this walk that is NOT bit-identical -- it can move an
            ! end by one pixel -- and it is safe for the reason this file's header already gives:
            ! the arc is deliberately generous and then trimmed by the membership rule, so the arc
            ! arithmetic does not have to be exact. The rule itself is untouched.
            winv = real(nr, real64) / hpx_twopi
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
                jlo = modulo(jlo, nr)
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
            d = d + hpx_pi
            if (d < 0.0_real64) then
                d = d + hpx_twopi
                if (d < 0.0_real64) d = modulo(d, hpx_twopi)
            else if (d >= hpx_twopi) then
                d = d - hpx_twopi
                if (d >= hpx_twopi) d = modulo(d, hpx_twopi)
            end if
            d = d - hpx_pi
            inside = abs(d) <= dphi
        end function in_disc

        !> Appends `count` pixels of ring `jr`, starting at index `jstart` within that ring.
        subroutine emit_block(jr, first_idx, jstart, count)
            integer(int64), intent(in) :: jr !! the ring, indexed from the north pole.
            integer(int64), intent(in) :: first_idx !! RING index of that ring's first pixel.
            integer(int64), intent(in) :: jstart !! first index within the ring, 0-based.
            integer(int64), intent(in) :: count !! how many consecutive pixels to append.

            ! Recording happens whether or not this walk is a counting one, but in practice only a
            ! counting walk is ever asked for it -- it is `pf_query_disc_alloc`'s way of not
            ! repeating the geometry on its second pass. Running out of room is not an error: the
            ! count is still right, and the caller falls back to walking again.
            if (recording) then
                if (nruns < hpx_alloc_runs_max) then
                    nruns = nruns + 1_int64
                    runs(1, nruns) = jr
                    runs(2, nruns) = first_idx
                    runs(3, nruns) = jstart
                    runs(4, nruns) = count
                else
                    nruns = -1_int64
                    recording = .false.
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
                                 "pf_query_disc_alloc", runs=runs, nruns=nruns)
        ! Allocated and ZERO-LENGTH when the disc is empty, never unallocated, so that `size()` is
        ! the only thing a caller ever tests.
        allocate (listpix(nlist))
        if (nlist > 0_int64) then
            if (nruns > 0_int64) then
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
                                 "pf_query_disc_alloc", runs=runs, nruns=nruns)
        allocate (listpix(n64))
        if (n64 > 0_int64) then
            if (nruns > 0_int64) then
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
