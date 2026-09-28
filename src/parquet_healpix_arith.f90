!> Closed-form arithmetic on `nside` and on angles: the part of `parquet_healpix` with no walk.
!!
!! Nothing here touches a ring, a face or a Morton code. It is the grid arithmetic
!! (`pf_nside2npix` and its family), the angle/vector conversions, the ring accessors and the
!! squared-chord pair -- each a handful of lines, each `pure`, each total.
!!
!! **Every function in this file reports an out-of-domain argument by returning -1** (or -2 for
!! `pf_ring2z`, whose valid results include -1), because a `pure` procedure may not contain an
!! `error stop` -- that is an image control statement -- and the D4 rule keeps these total. The
!! module's own doc-comments state the rule; this note records why there was no third option.
submodule(parquet_healpix) parquet_healpix_arith
    implicit none

contains

    ! ---- Angles and unit vectors ----

    module procedure hpx_ang2vec
        real(real64) :: st

        st = sin(theta)
        vec(1) = st * cos(phi)
        vec(2) = st * sin(phi)
        vec(3) = cos(theta)
    end procedure hpx_ang2vec

    module procedure hpx_vec_unit
        real(real64) :: scale, n

        ! Scale by the largest component BEFORE anything is squared -- see this procedure's
        ! doc-comment in the parent module for the two measured failures that rule out squaring
        ! first. `scale` is the largest magnitude present, so each quotient is in [-1, 1] and the
        ! sum of their squares is in [0, 3]: neither underflow nor overflow is reachable.
        scale = max(abs(vec(1)), abs(vec(2)), abs(vec(3)))
        if (scale <= 0.0_real64) then
            x = 0.0_real64
            y = 0.0_real64
            z = 0.0_real64
            return
        end if
        x = vec(1) / scale
        y = vec(2) / scale
        z = vec(3) / scale
        n = sqrt(x * x + y * y + z * z)
        x = x / n
        y = y / n
        z = z / n
        z = max(-1.0_real64, min(1.0_real64, z))
    end procedure hpx_vec_unit

    module procedure hpx_xy2phi
        if (x == 0.0_real64 .and. y == 0.0_real64) then
            phi = 0.0_real64
            return
        end if
        phi = atan2(y, x)
        if (phi < 0.0_real64) phi = phi + PF_TWOPI
    end procedure hpx_xy2phi

    module procedure hpx_vec2ang
        real(real64) :: x, y, z, tr

        call hpx_vec_unit(vec, x, y, z)
        tr = sqrt(x * x + y * y)
        ! Colatitude as atan2(transverse, z), never as acos(z): an `acos` loses half its digits
        ! wherever z is within an ulp or two of +-1, which is exactly where the pixelisation puts
        ! its first and last rings. Measured: for `[1e-8, 0, 1]` the quotient z/|v| rounds to
        ! exactly 1.0 and `acos` returns 0, where the true colatitude is 1e-8 -- and that is what
        ! healpy 1.20.0 returns for this input.
        !
        ! Both arguments vanish only for the zero vector, which `hpx_vec_unit` maps to (0, 0, 0);
        ! the guard is what keeps `atan2(0, 0)` -- prohibited, and fatal under nagfor -- out of
        ! reach for it.
        if (tr == 0.0_real64 .and. z == 0.0_real64) then
            theta = 0.0_real64
        else
            theta = atan2(tr, z)
        end if
        phi = hpx_xy2phi(x, y)
    end procedure hpx_vec2ang

    ! ---- Grid arithmetic ----

    module procedure hpx_nside2npix_i64
        npix = -1_int64
        if (hpx_nside_ok(nside, hpx_nside_max)) npix = 12_int64 * nside * nside
    end procedure hpx_nside2npix_i64

    module procedure hpx_nside2npix_i32
        ! The int32 ceiling is 8192 rather than 2**29, and it is an arithmetic fact as well as an
        ! API rule here: 12*16384**2 is 3221225472, which does not fit a signed 32-bit integer.
        npix = -1_int32
        if (hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) npix = 12_int32 * nside * nside
    end procedure hpx_nside2npix_i32

    module procedure hpx_npix2nside_i64
        nside = hpx_npix2nside(npix, hpx_nside_max)
    end procedure hpx_npix2nside_i64

    module procedure hpx_npix2nside_i32
        nside = int(hpx_npix2nside(int(npix, int64), hpx_nside_max_i32), int32)
    end procedure hpx_npix2nside_i32

    module procedure hpx_nside2order_i64
        order = -1_int64
        if (hpx_nside_ok(nside, hpx_nside_max)) order = int(trailz(nside), int64)
    end procedure hpx_nside2order_i64

    module procedure hpx_nside2order_i32
        order = -1_int32
        if (hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) order = int(trailz(nside), int32)
    end procedure hpx_nside2order_i32

    module procedure hpx_order2nside_i64
        nside = -1_int64
        if (order >= 0_int64 .and. order <= hpx_order_max) nside = ishft(1_int64, int(order))
    end procedure hpx_order2nside_i64

    module procedure hpx_order2nside_i32
        ! Capped at order 13 (nside 8192), not at 29. The value 2**29 is perfectly representable
        ! in int32, so this bound is the API ceiling rather than an overflow -- an int32 nside
        ! above 8192 is one no int32 entry point of this module would accept, and manufacturing
        ! one here would only move the failure somewhere less obvious.
        nside = -1_int32
        if (order >= 0_int32 .and. order <= hpx_order_max_i32) nside = ishft(1_int32, int(order))
    end procedure hpx_order2nside_i32

    module procedure hpx_nside2pixarea_i64
        area = -1.0_real64
        if (hpx_nside_ok(nside, hpx_nside_max)) &
            area = PF_PI / (3.0_real64 * real(nside, real64) * real(nside, real64))
    end procedure hpx_nside2pixarea_i64

    module procedure hpx_nside2pixarea_i32
        area = -1.0_real64
        if (hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            area = PF_PI / (3.0_real64 * real(nside, real64) * real(nside, real64))
    end procedure hpx_nside2pixarea_i32

    module procedure hpx_nside2resol_i64
        resol = -1.0_real64
        if (hpx_nside_ok(nside, hpx_nside_max)) resol = hpx_sqrt_pi_third / real(nside, real64)
    end procedure hpx_nside2resol_i64

    module procedure hpx_nside2resol_i32
        resol = -1.0_real64
        if (hpx_nside_ok(int(nside, int64), hpx_nside_max_i32)) &
            resol = hpx_sqrt_pi_third / real(nside, real64)
    end procedure hpx_nside2resol_i32

    ! ---- Rings ----

    module procedure hpx_pix2ring_ring_i64
        iring = hpx_pix2ring(nside, ipix, hpx_nside_max)
    end procedure hpx_pix2ring_ring_i64

    module procedure hpx_pix2ring_ring_i32
        iring = int(hpx_pix2ring(int(nside, int64), int(ipix, int64), hpx_nside_max_i32), int32)
    end procedure hpx_pix2ring_ring_i32

    module procedure hpx_pix2ring_nest_i64
        integer(int64) :: p

        iring = -1_int64
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) return
        if (ipix < 0_int64 .or. ipix >= 12_int64 * nside * nside) return
        call pf_nest2ring(nside, ipix, p)
        iring = hpx_pix2ring(nside, p, hpx_nside_max)
    end procedure hpx_pix2ring_nest_i64

    module procedure hpx_pix2ring_nest_i32
        integer(int64) :: p, ns

        iring = -1_int32
        ns = int(nside, int64)
        if (.not. hpx_nside_ok(ns, hpx_nside_max_i32)) return
        if (ipix < 0_int32 .or. int(ipix, int64) >= 12_int64 * ns * ns) return
        call pf_nest2ring(ns, int(ipix, int64), p)
        iring = int(hpx_pix2ring(ns, p, hpx_nside_max_i32), int32)
    end procedure hpx_pix2ring_nest_i32

    module procedure hpx_ring2z_i64
        z = -2.0_real64
        if (.not. hpx_nside_ok(nside, hpx_nside_max)) return
        if (iring < 1_int64 .or. iring > 4_int64 * nside - 1_int64) return
        z = hpx_ring_z(nside, iring)
    end procedure hpx_ring2z_i64

    module procedure hpx_ring2z_i32
        integer(int64) :: ns

        z = -2.0_real64
        ns = int(nside, int64)
        if (.not. hpx_nside_ok(ns, hpx_nside_max_i32)) return
        if (iring < 1_int32 .or. int(iring, int64) > 4_int64 * ns - 1_int64) return
        z = hpx_ring_z(ns, int(iring, int64))
    end procedure hpx_ring2z_i32

    ! ---- Chords ----

    module procedure hpx_chord2_from_angle
        real(real64) :: s

        ! Both guards exist to keep the result MONOTONE in the angle, which is the whole property
        ! the substitution `sum((v1-v2)**2) <= chord2(r)` for `pf_angdist(v1,v2) <= r` rests on.
        ! Unguarded, `2*sin(angle/2)` stops rising at pi and comes back down, so a radius beyond a
        ! half turn produced a SMALLER bound than one just under it -- selecting fewer points the
        ! wider the caller asked. And a negative radius has to match nothing, which a squared chord
        ! cannot express by itself: `sin` is odd and the square makes it positive again, so the
        ! unguarded form turned `r = -1` into the bound for `r = +1`.
        !
        ! Written as two comparisons rather than `min(angle, PF_PI)` so that a NaN angle still
        ! yields a NaN: MIN with a NaN operand may return the other one, which would silently turn
        ! an undefined radius into an all-sky one.
        if (angle < 0.0_real64) then
            chord2 = -1.0_real64
            return
        end if
        if (angle >= PF_PI) then
            chord2 = 4.0_real64
            return
        end if
        s = 2.0_real64 * sin(0.5_real64 * angle)
        chord2 = s * s
    end procedure hpx_chord2_from_angle

    module procedure hpx_angle_from_chord2
        ! Both clamps are load-bearing rather than defensive: a `chord2` a rounding below zero
        ! would put `sqrt` outside its domain, and one a rounding above four would put `asin`
        ! outside its, and either raises IEEE_INVALID -- which is the one thing this module
        ! promises never to do.
        angle = 2.0_real64 * asin(min(1.0_real64, 0.5_real64 * sqrt(max(0.0_real64, chord2))))
    end procedure hpx_angle_from_chord2

    ! ---- File-local helpers ----
    !
    ! Contained rather than declared: nothing outside this file calls them, and a submodule's own
    ! contained procedures are reachable by its descendants through host association, so the
    ! private-module-procedure linkage trap does not apply here.

    !> Resolution parameter of a pixel count, or -1 when `npix` is not `12 * 4**order`.
    !>
    !> **Pure integer arithmetic, with no square root.** The floating-point route -- `nint(sqrt(
    !> real(npix)/12))` plus an integer verification -- is in fact exact at every order this
    !> module accepts, because a valid `npix` is `3 * 2**(2*order+2)`, whose mantissa is two bits;
    !> that was checked rather than assumed. It is avoided anyway: the exactness rests on a
    !> three-step argument that is invisible in the code and that an innocuous edit breaks (a
    !> `real32` intermediate, a hoisted constant of the wrong kind, a dropped verification), while
    !> the form below needs no argument at all and yields the order as a by-product.
    pure function hpx_npix2nside(npix, limit) result(nside)
        integer(int64), intent(in) :: npix !! the pixel count to validate and invert.
        integer(int64), intent(in) :: limit !! the largest `nside` the caller's kind accepts.
        integer(int64) :: nside !! the resolution parameter, or -1.
        integer(int64) :: q
        integer :: tz

        nside = -1_int64
        if (npix <= 0_int64) return
        if (modulo(npix, 12_int64) /= 0_int64) return
        q = npix / 12_int64
        if (iand(q, q - 1_int64) /= 0_int64) return    ! a power of two ...
        tz = trailz(q)
        if (iand(tz, 1) /= 0) return                   ! ... and an even one, i.e. a power of four
        nside = ishft(1_int64, tz / 2)
        ! UNREACHABLE through either public kind, and kept deliberately. The first valid pixel
        ! count above a kind's ceiling is not representable in that kind: at int64 the ceiling is
        ! order 29 (npix 3458764513820540928) and order 30's count is 13835058055282163712, past
        ! `huge`; at int32 the ceiling is order 13 (805306368) and order 14's is 3221225472, past
        ! `huge` there. So no caller can present a well-formed count this clause would reject.
        ! It stays because it is the only thing that would still hold if a ceiling ever moved, and
        ! removing it would make that change silently wrong rather than loudly so.
        !
        ! The excluded statement is a one-line `if`, so gcov counts the line as hit whenever the
        ! CONDITION is evaluated -- which is on every call. A gcov attribution artifact, not a
        ! stale exclusion; the assignment itself never runs.
        ! GCOVR_EXCL_START
        if (nside > limit) nside = -1_int64
        ! GCOVR_EXCL_STOP
    end function hpx_npix2nside

    !> Ring of a RING-scheme pixel, or -1 when `nside` or `ipix` is out of domain.
    pure function hpx_pix2ring(nside, ipix, limit) result(iring)
        integer(int64), intent(in) :: nside !! resolution parameter.
        integer(int64), intent(in) :: ipix !! a RING pixel index.
        integer(int64), intent(in) :: limit !! the largest `nside` the caller's kind accepts.
        integer(int64) :: iring !! the ring, `1 .. 4*nside-1`, or -1.
        integer(int64) :: j, nr, shifted

        iring = -1_int64
        if (.not. hpx_nside_ok(nside, limit)) return
        if (ipix < 0_int64 .or. ipix >= 12_int64 * nside * nside) return
        call hpx_ring_decompose(nside, ipix, iring, j, nr, shifted)
    end function hpx_pix2ring

end submodule parquet_healpix_arith
