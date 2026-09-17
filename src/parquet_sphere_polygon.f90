!> `pf_sky_polygon`: building a polygon, its areas, its even-odd containment, and its candidate walk.
!!
!! **One containment routine serves both edge rules.** Under `PF_EDGE_RADEC` the polygon is planar in
!! `(ra, dec)` as written; under `PF_EDGE_GREAT_CIRCLE` every vertex is projected gnomonically onto
!! the plane tangent at the cap centre, `(x/z, y/z)` in the cap's frame, which maps every great
!! circle to a straight line. `sky_even_odd` then decides both. **The projection maps a direction
!! and its antipode to the same point**, so a direction on the far side of the centre is rejected
!! before it is projected; without that a convex quadrilateral reports twice its pixels.
!!
!! **Every small quantity is formed directly.** The chart area is Green's theorem taken about the
!! lowest vertex declination with each edge's mean sine written as `sin(mid)*sin(h)/h`, so neither a
!! near-level edge nor a small polygon near a pole cancels; the great-circle area is the triple
!! product in the cap's own frame, `x1*y2 - y1*x2`, which carries a small triangle's area to the
!! precision its vertices have rather than to an ulp of the unit vectors.
submodule (parquet_sphere) parquet_sphere_polygon
    implicit none

contains

    ! ---- Private workers ----

    !> Whether `(x, y)` lies inside the polygon `(px, py)`: the even-odd rule, a ray towards `+x`.
    !!
    !! The intersection is computed only for an edge the ray's line crosses, so its denominator is
    !! never zero; the two tests are nested rather than joined with `.and.`, which does not
    !! short-circuit.
    pure function sky_even_odd(px, py, x, y) result(inside)
        real(real64), intent(in) :: px(:) !! the vertices' abscissae.
        real(real64), intent(in) :: py(:) !! the vertices' ordinates, the size of `px`.
        real(real64), intent(in) :: x !! the test point's abscissa.
        real(real64), intent(in) :: y !! the test point's ordinate.
        logical :: inside !! whether the point is inside.
        integer(int64) :: k, j, n

        n = size(px, kind=int64)
        inside = .false.
        j = n
        do k = 1_int64, n
            if ((py(k) > y) .neqv. (py(j) > y)) then
                if (x < px(k) + (y - py(k)) * (px(j) - px(k)) / (py(j) - py(k))) inside = .not. inside
            end if
            j = k
        end do
    end function sky_even_odd

    !> Whether the unit vector `v` lies inside a great-circle polygon.
    !!
    !! Every point of an admitted polygon is within 89.9 degrees of the cap centre, so a direction
    !! whose cosine from the centre is below half that bound is outside before it is projected. That
    !! excludes the far hemisphere -- the antipode the projection would confuse with the point -- and
    !! keeps `x/z` finite for a direction at right angles to the centre.
    pure function sky_gc_inside(this, v) result(inside)
        class(pf_sky_polygon), intent(in) :: this !! a built great-circle polygon.
        real(real64), intent(in) :: v(3) !! a unit vector.
        logical :: inside !! whether it is inside.
        real(real64) :: x, y, z

        inside = .false.
        z = v(1) * this%centre(1) + v(2) * this%centre(2) + v(3) * this%centre(3)
        if (z <= 0.5_real64 * sky_hemisphere_cos) return
        x = v(1) * this%e1(1) + v(2) * this%e1(2) + v(3) * this%e1(3)
        y = v(1) * this%e2(1) + v(2) * this%e2(2) + v(3) * this%e2(3)
        inside = sky_even_odd(this%px, this%py, x / z, y / z)
    end function sky_gc_inside

    !> `sin(h)/h`, 1 at 0.
    pure function sky_sinc(h) result(r)
        real(real64), intent(in) :: h !! an angle, radians, in `[-pi/2, pi/2]`.
        real(real64) :: r !! `sin(h)/h`.

        if (h == 0.0_real64) then
            r = 1.0_real64
        else
            r = sin(h) / h
        end if
    end function sky_sinc

    !> `sin(h)/h - 1`, by its series below 0.25 in magnitude, where the difference would cancel.
    pure function sky_sinc_m1(h) result(r)
        real(real64), intent(in) :: h !! an angle, radians, in `[-pi/2, pi/2]`.
        real(real64) :: r !! `sin(h)/h - 1`.
        real(real64) :: h2

        if (abs(h) < 0.25_real64) then
            ! -h2/3! + h2**2/5! - ... through the h**12 term; the first omitted term is below 1e-18
            ! of the result at the boundary.
            h2 = h * h
            r = -h2 / 6.0_real64 * (1.0_real64 - h2 / 20.0_real64 * (1.0_real64 - h2 / 42.0_real64 * &
                (1.0_real64 - h2 / 72.0_real64 * (1.0_real64 - h2 / 110.0_real64 * (1.0_real64 - h2 / 156.0_real64)))))
        else
            r = sin(h) / h - 1.0_real64
        end if
    end function sky_sinc_m1

    !> The area of a chart polygon, steradians: Green's theorem on `cos(dec) d(dec) d(ra)`.
    !!
    !! The integral of `-(sin(dec) - sin(ref)) d(ra)` round the boundary, `ref` the lowest vertex
    !! declination; the `sin(ref)` part integrates to zero round a closed polygon, and subtracting it
    !! per edge is what keeps a small polygon near a pole from cancelling. Along an edge the mean of
    !! `sin(dec)` is `sin(mid)*sinc(h)`, `mid` and `h` the half sum and half difference of its end
    !! declinations, and `sin(mid)*sinc(h) - sin(ref)` is formed as
    !! `(sin(mid) - sin(ref))*sinc(h) + sin(ref)*(sinc(h) - 1)` with the first difference a product.
    pure function sky_chart_area(ra, dec, dec_lo) result(a)
        real(real64), intent(in) :: ra(:) !! the vertices' right ascensions, degrees, as written.
        real(real64), intent(in) :: dec(:) !! the vertices' declinations, degrees.
        real(real64), intent(in) :: dec_lo !! the lowest vertex declination, degrees.
        real(real64) :: a !! the area, steradians.
        real(real64) :: ref, sref, cref, total, d1, d2, mid, h, dra
        integer(int64) :: k, j, n

        call sky_dec_sin_cos(dec_lo, sref, cref)
        ref = dec_lo * sky_deg2rad
        n = size(ra, kind=int64)
        total = 0.0_real64
        do k = 1_int64, n
            j = k + 1_int64
            if (k == n) j = 1_int64
            dra = (ra(j) - ra(k)) * sky_deg2rad
            d1 = dec(k) * sky_deg2rad
            d2 = dec(j) * sky_deg2rad
            mid = 0.5_real64 * (d1 + d2)
            h = 0.5_real64 * (d2 - d1)
            total = total + dra * (2.0_real64 * cos(0.5_real64 * (mid + ref)) * sin(0.5_real64 * (mid - ref)) * &
                                   sky_sinc(h) + sref * sky_sinc_m1(h))
        end do
        a = abs(total)
    end function sky_chart_area

    !> The polygon walk: the point of draw `d` and how many candidates it took. Every entry point's body.
    pure subroutine sky_polygon_walk(who, this, seed, i, d, ra, dec, ncand)
        character(len=*), intent(in) :: who !! the entry point, for the messages.
        class(pf_sky_polygon), intent(in) :: this !! the polygon.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int64), intent(in) :: d !! the draw, at least 1.
        real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`.
        real(real64), intent(out) :: dec !! declination, degrees.
        integer(int64), intent(out) :: ncand !! candidates drawn, the accepted one included.
        integer(int64) :: key, k
        real(real64) :: uv(2), s, ra_c, dec_c, v(3)

        ra = 0.0_real64
        dec = 0.0_real64
        ncand = 0_int64
        if (.not. this%set) error stop who // ": %init has not run"
        key = pf_random_key(pf_random_key(seed, sky_polygon_label), d)
        if (this%rule == PF_EDGE_RADEC) then
            do k = 1_int64, sky_candidate_cap
                ! Candidate k is the two halves of block k - 1 of `(key, i)`: one enciphering.
                call pf_random_fill_draws(key, i, uv, k + k - 1_int64)
                ra_c = this%ra_lo + uv(1) * (this%ra_hi - this%ra_lo)
                s = min(max(this%sin_lo + uv(2) * this%sin_span, -1.0_real64), 1.0_real64)
                dec_c = asin(s) * sky_rad2deg
                if (sky_even_odd(this%px, this%py, ra_c, dec_c)) then
                    ra = pf_wrap_deg(ra_c)
                    dec = dec_c
                    ncand = k
                    return
                end if
            end do
        else
            do k = 1_int64, sky_candidate_cap
                v = pf_random_disc_at(key, i, this%centre, this%cap_radius, k)
                if (sky_gc_inside(this, v)) then
                    call sky_unit_radec(v, ra, dec)
                    ncand = k
                    return
                end if
            end do
        end if
        error stop who // ": 100000 candidates rejected -- the acceptance rate %init admitted cannot produce " // &
            "this; report it"
    end subroutine sky_polygon_walk

    ! ---- Construction ----

    module procedure sky_polygon_init
        character(len=*), parameter :: who = "pf_sky_polygon%init"
        real(real64), allocatable :: u(:, :), gx(:), gy(:), gz(:)
        real(real64) :: area, bound, f, floor, lo, hi, sc, cc, total, c(3), w(3), scale, ang
        integer(int64) :: n, k, j
        integer :: rule, m
        logical :: bad
        character(len=3) :: what
        character(len=24) :: floor_text

        if (this%set) error stop who // ": already initialised; call %clear first"
        rule = PF_EDGE_RADEC
        if (present(edges)) rule = edges
        if (rule /= PF_EDGE_RADEC .and. rule /= PF_EDGE_GREAT_CIRCLE) then
            error stop who // ": edges must be PF_EDGE_RADEC (0) or PF_EDGE_GREAT_CIRCLE (1), got " // &
                trim(sky_int_text(int(rule, int64)))
        end if
        n = size(ra, kind=int64)
        if (n < 3_int64 .or. size(dec, kind=int64) /= n) then
            error stop who // ": at least 3 vertices are needed, ra and dec of one size (got " // &
                trim(sky_int_text(n)) // " and " // trim(sky_int_text(size(dec, kind=int64))) // ")"
        end if
        do k = 1_int64, n
            bad = ra(k) /= ra(k) .or. dec(k) /= dec(k)
            if (.not. bad) bad = .not. (abs(ra(k)) <= huge(ra(k)) .and. dec(k) >= -90.0_real64 .and. &
                                        dec(k) <= 90.0_real64)
            if (bad) then
                error stop who // ": vertex " // trim(sky_int_text(k)) // " must be finite with dec in " // &
                    "[-90, 90] (got " // trim(sky_real_text(ra(k))) // ", " // trim(sky_real_text(dec(k))) // ")"
            end if
        end do

        allocate (this%ra(n), this%dec(n), this%px(n), this%py(n))
        this%ra = ra
        this%dec = dec
        this%ra_lo = minval(ra)
        this%ra_hi = maxval(ra)
        this%dec_lo = minval(dec)
        this%dec_hi = maxval(dec)

        if (rule == PF_EDGE_RADEC) then
            if (this%ra_hi - this%ra_lo > 360.0_real64) then
                error stop who // ": the vertices span " // trim(sky_real_text(this%ra_hi - this%ra_lo)) // &
                    " degrees of RA; write a polygon crossing RA = 0 continuously (350, 370), and no " // &
                    "polygon may span more than 360"
            end if
            this%px = ra
            this%py = dec
            call sky_dec_sin_cos(this%dec_lo, sc, cc)
            this%sin_lo = sc
            lo = this%dec_lo * sky_deg2rad
            hi = this%dec_hi * sky_deg2rad
            this%sin_span = 2.0_real64 * cos(0.5_real64 * (hi + lo)) * sin(0.5_real64 * (hi - lo))
            area = sky_chart_area(ra, dec, this%dec_lo)
            bound = (this%ra_hi - this%ra_lo) * sky_deg2rad * this%sin_span
            what = "box"
        else
            allocate (u(3, n), gx(n), gy(n), gz(n))
            w = 0.0_real64
            do k = 1_int64, n
                u(:, k) = sky_radec_unit(ra(k), dec(k))
                w = w + u(:, k)
            end do
            ! The cap centre is the normalised vertex sum, scaled by its largest component first so a
            ! sum that nearly cancels still normalises; one that cancels exactly names no direction.
            scale = max(abs(w(1)), abs(w(2)), abs(w(3)))
            if (scale <= 0.0_real64) then
                error stop who // ": vertex 1 is 9.0000000E+01 degrees from the vertices' mean direction; every " // &
                    "vertex must be within 89.9 degrees of it (the polygon must fit an open hemisphere) -- split it"
            end if
            c = w / scale
            c = c / sqrt(c(1) * c(1) + c(2) * c(2) + c(3) * c(3))
            do k = 1_int64, n
                gz(k) = u(1, k) * c(1) + u(2, k) * c(2) + u(3, k) * c(3)
                if (.not. (gz(k) > sky_hemisphere_cos)) then
                    w = [u(2, k) * c(3) - u(3, k) * c(2), u(3, k) * c(1) - u(1, k) * c(3), &
                         u(1, k) * c(2) - u(2, k) * c(1)]
                    ang = atan2(sqrt(w(1) * w(1) + w(2) * w(2) + w(3) * w(3)), gz(k)) * sky_rad2deg
                    error stop who // ": vertex " // trim(sky_int_text(k)) // " is " // trim(sky_real_text(ang)) // &
                        " degrees from the vertices' mean direction; every vertex must be within 89.9 degrees " // &
                        "of it (the polygon must fit an open hemisphere) -- split it"
                end if
            end do
            ! The right-handed frame `(e1, e2, c)` by `parquet_random`'s disc rule: `e1` is `c` crossed
            ! with the axis of its smallest component (the lowest on a tie), normalised, `e2 = c x e1`.
            m = 1
            if (abs(c(2)) < abs(c(m))) m = 2
            if (abs(c(3)) < abs(c(m))) m = 3
            select case (m)
            case (1)
                w = [0.0_real64, c(3), -c(2)]
            case (2)
                w = [-c(3), 0.0_real64, c(1)]
            case default
                w = [c(2), -c(1), 0.0_real64]
            end select
            this%e1 = w / sqrt(w(1) * w(1) + w(2) * w(2) + w(3) * w(3))
            this%e2 = [c(2) * this%e1(3) - c(3) * this%e1(2), c(3) * this%e1(1) - c(1) * this%e1(3), &
                       c(1) * this%e1(2) - c(2) * this%e1(1)]
            this%centre = c
            this%cap_radius = 0.0_real64
            do k = 1_int64, n
                gx(k) = u(1, k) * this%e1(1) + u(2, k) * this%e1(2) + u(3, k) * this%e1(3)
                gy(k) = u(1, k) * this%e2(1) + u(2, k) * this%e2(2) + u(3, k) * this%e2(3)
                this%px(k) = gx(k) / gz(k)
                this%py(k) = gy(k) / gz(k)
                this%cap_radius = max(this%cap_radius, atan2(hypot(gx(k), gy(k)), gz(k)))
            end do
            ! The signed excesses of the triangles (c, v_k, v_k+1), by the triple-product form
            ! `2*atan2(c.(a x b), 1 + c.a + a.b + b.c)`, with `c.(a x b) = x_a*y_b - y_a*x_b` in the
            ! cap's own frame.
            total = 0.0_real64
            do k = 1_int64, n
                j = k + 1_int64
                if (k == n) j = 1_int64
                total = total + 2.0_real64 * atan2(gx(k) * gy(j) - gy(k) * gx(j), &
                    1.0_real64 + gz(k) + gz(j) + (gx(k) * gx(j) + gy(k) * gy(j) + gz(k) * gz(j)))
            end do
            area = abs(total)
            bound = 4.0_real64 * sky_pi * sin(0.5_real64 * this%cap_radius)**2
            what = "cap"
        end if

        if (.not. (area > 0.0_real64 .and. bound > 0.0_real64)) then
            error stop who // ": the polygon has zero area"
        end if
        f = area / bound
        floor = sky_acceptance_floor
        floor_text = "1e-3"
        if (sky_floor_override > 0.0_real64) then
            floor = sky_floor_override
            floor_text = sky_real_text(floor)
        end if
        if (f < floor) then
            error stop who // ": the polygon covers " // trim(sky_real_text(f)) // " of its bounding " // what // &
                ", below the " // trim(floor_text) // " floor; split it into pieces"
        end if
        this%area_v = area
        this%bound_area = bound
        this%rule = rule
        this%set = .true.
    end procedure sky_polygon_init

    module procedure sky_polygon_clear
        if (allocated(this%ra)) deallocate (this%ra)
        if (allocated(this%dec)) deallocate (this%dec)
        if (allocated(this%px)) deallocate (this%px)
        if (allocated(this%py)) deallocate (this%py)
        this%set = .false.
        this%rule = -1
        this%ra_lo = 0.0_real64
        this%ra_hi = 0.0_real64
        this%dec_lo = 0.0_real64
        this%dec_hi = 0.0_real64
        this%sin_lo = 0.0_real64
        this%sin_span = 0.0_real64
        this%centre = 0.0_real64
        this%e1 = 0.0_real64
        this%e2 = 0.0_real64
        this%cap_radius = 0.0_real64
        this%area_v = 0.0_real64
        this%bound_area = 0.0_real64
    end procedure sky_polygon_clear

    ! ---- Accessors ----

    module procedure sky_polygon_is_set
        ok = this%set
    end procedure sky_polygon_is_set

    module procedure sky_polygon_size
        n = 0_int64
        if (this%set) n = size(this%ra, kind=int64)
    end procedure sky_polygon_size

    module procedure sky_polygon_edges
        rule = this%rule
    end procedure sky_polygon_edges

    module procedure sky_polygon_area
        if (.not. this%set) error stop "pf_sky_polygon%area: %init has not run"
        a = this%area_v
    end procedure sky_polygon_area

    module procedure sky_polygon_area_deg2
        if (.not. this%set) error stop "pf_sky_polygon%area_deg2: %init has not run"
        a = this%area_v * sky_rad2deg * sky_rad2deg
    end procedure sky_polygon_area_deg2

    module procedure sky_polygon_acceptance
        if (.not. this%set) error stop "pf_sky_polygon%acceptance: %init has not run"
        f = min(this%area_v / this%bound_area, 1.0_real64)
    end procedure sky_polygon_acceptance

    module procedure sky_polygon_bounds
        if (.not. this%set) error stop "pf_sky_polygon%bounds: %init has not run"
        ra_lo = this%ra_lo
        ra_hi = this%ra_hi
        dec_lo = this%dec_lo
        dec_hi = this%dec_hi
    end procedure sky_polygon_bounds

    module procedure sky_polygon_contains
        real(real64) :: ra_w, u(3)

        if (.not. this%set) error stop "pf_sky_polygon%contains: %init has not run"
        inside = .false.
        if (ra /= ra .or. dec /= dec) return
        if (.not. (abs(ra) <= huge(ra) .and. dec >= -90.0_real64 .and. dec <= 90.0_real64)) return
        if (this%rule == PF_EDGE_RADEC) then
            ! Into the polygon's own range, `[ra_lo, ra_lo + 360)`, where its vertices are written.
            ra_w = this%ra_lo + pf_wrap_deg(ra - this%ra_lo)
            inside = sky_even_odd(this%px, this%py, ra_w, dec)
        else
            ! A named local rather than the function result: ifx copies a result reaching an
            ! explicit-shape dummy through a temporary, and says so on every call under `-check all`.
            u = sky_radec_unit(ra, dec)
            inside = sky_gc_inside(this, u)
        end if
    end procedure sky_polygon_contains

    ! ---- Draws ----

    module procedure sky_polygon_random_at_i32
        integer(int64) :: ncand

        call sky_polygon_walk("pf_sky_polygon%random_at", this, seed, int(i, int64), sky_draw(draw), ra, dec, ncand)
    end procedure sky_polygon_random_at_i32

    module procedure sky_polygon_random_at_i64
        integer(int64) :: ncand

        call sky_polygon_walk("pf_sky_polygon%random_at", this, seed, i, sky_draw(draw), ra, dec, ncand)
    end procedure sky_polygon_random_at_i64

    !> `%random_fill`'s body, for either stream-index kind.
    pure subroutine sky_polygon_fill(this, seed, i, ra, dec, draw)
        class(pf_sky_polygon), intent(in) :: this !! the polygon.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        real(real64), intent(out) :: ra(:) !! right ascensions, degrees.
        real(real64), intent(out) :: dec(:) !! declinations, degrees; the size of `ra`.
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        character(len=*), parameter :: who = "pf_sky_polygon%random_fill"
        integer(int64) :: n, k, d0, ncand

        if (.not. this%set) error stop who // ": %init has not run"
        n = size(ra, kind=int64)
        if (size(dec, kind=int64) /= n) then
            error stop who // ": ra and dec must have the same size (got " // trim(sky_int_text(n)) // " and " // &
                trim(sky_int_text(size(dec, kind=int64))) // ")"
        end if
        if (n == 0_int64) return
        d0 = sky_fill_start(who, draw, n)
        do k = 1_int64, n
            call sky_polygon_walk(who, this, seed, i, d0 + (k - 1_int64), ra(k), dec(k), ncand)
        end do
    end subroutine sky_polygon_fill

    module procedure sky_polygon_random_fill_i32
        call sky_polygon_fill(this, seed, int(i, int64), ra, dec, draw)
    end procedure sky_polygon_random_fill_i32

    module procedure sky_polygon_random_fill_i64
        call sky_polygon_fill(this, seed, i, ra, dec, draw)
    end procedure sky_polygon_random_fill_i64

    module procedure sky_polygon_random_next
        integer(int64) :: seed, stream, d, ncand

        call sky_take_block("pf_sky_polygon%random_next", rng, seed, stream, d)
        call sky_polygon_walk("pf_sky_polygon%random_next", this, seed, stream, d, ra, dec, ncand)
    end procedure sky_polygon_random_next

    ! ---- Test-only hooks ----

    module procedure parquet_debug_sphere_polygon_draw
        call sky_polygon_walk("parquet_debug_sphere_polygon_draw", poly, seed, i, sky_draw(draw), ra, dec, ncand)
    end procedure parquet_debug_sphere_polygon_draw

    module procedure parquet_debug_set_sphere_acceptance_floor
        sky_floor_override = 0.0_real64
        if (floor /= floor) return
        if (floor > 0.0_real64) sky_floor_override = floor
    end procedure parquet_debug_set_sphere_acceptance_floor

end submodule parquet_sphere_polygon
