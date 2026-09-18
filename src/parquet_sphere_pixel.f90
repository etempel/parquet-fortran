!> Points in HEALPix pixels and masks: `pf_random_pixel_*` and `pf_random_mask_*`.
!!
!! **One draw serves both families, and neither rejects.** A HEALPix pixel is a square in the
!! projection plane and that projection is equal-area, so the two uniforms of one block, read
!! through `%pix2vec_offset` as a position across and along that square, are a direction uniform
!! over the pixel per unit solid angle. The pixel family and the mask's point family make the same
!! draw under different labels, and the mask's choice of a listed pixel is a fixed-cost
!! `pf_random_int_at` under a third.
!!
!! **A cap and a rejection test would be both slower and, at the finest resolutions, less correct.**
!! Accepting a candidate on `%vec2pix(v) == ipix` samples the set that test admits rather than the
!! pixel, and within a pixel of a pole at `nside` above about `2**21` those differ: `z` there sits
!! closer to 1 than a double resolves, so `%vec2pix` cannot name the pixel a direction is in.
!!
!! **A grid is read only through its public bindings**, `%is_set`, `%order`, `%pix2vec`, `%vec2pix`,
!! `%max_pixrad` and `%vec2radec`, all `pure`; the pixel count comes from `%order` because
!! `%get_npix` is not.
submodule (parquet_sphere) parquet_sphere_pixel
    implicit none

contains

    ! ---- Private workers ----

    !> The pixel count of a grid the samplers accept; aborts, naming `who`, on an unbuilt grid and on
    !! one finer than `nside = 2**24`.
    pure function sky_grid_npix(who, grid) result(npix)
        character(len=*), intent(in) :: who !! the entry point, for the messages.
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64) :: npix !! `12*nside**2`.
        integer(int32) :: order
        integer(int64) :: nside

        if (.not. grid%is_set()) then
            error stop who // ": the grid has not been built; call %init(nside, scheme) first"
        end if
        order = grid%order()
        nside = ishft(1_int64, order)
        if (order > sky_pixel_order_max) then
            error stop who // ": nside " // trim(sky_int_text(nside)) // " exceeds 2**24, above which a unit " // &
                "vector cannot name a pixel near a pole (see the HEALPix page)"
        end if
        npix = 12_int64 * nside * nside
    end function sky_grid_npix

    !> The point of draw `d` inside pixel `ipix`, under `label`, and the candidates it took.
    !!
    !! **This family does not reject.** A HEALPix pixel is a square in the projection plane and that
    !! projection is equal-area, so the two uniforms of one block, read as a position across and
    !! along that square, ARE a direction uniform over the pixel per unit solid angle --
    !! `%pix2vec_offset` is the inverse projection. Nothing is drawn that has to be thrown away, so
    !! `ncand` is 1 for every draw and exists only for the debug hook, which reports it.
    pure subroutine sky_pixel_walk(who, grid, label, seed, i, ipix, d, v, ncand)
        character(len=*), intent(in) :: who !! the entry point, for the messages.
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64), intent(in) :: label !! the family's label.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int64), intent(in) :: ipix !! the pixel, in the grid's scheme.
        integer(int64), intent(in) :: d !! the draw, at least 1.
        real(real64), intent(out) :: v(3) !! a unit vector uniform inside the pixel.
        integer(int64), intent(out) :: ncand !! candidates drawn; always 1, since none is rejected.
        real(real64) :: uv(2)
        integer(int64) :: npix, key

        ncand = 0_int64
        npix = sky_grid_npix(who, grid)
        if (ipix < 0_int64 .or. ipix >= npix) then
            error stop who // ": ipix " // trim(sky_int_text(ipix)) // " is outside [0, " // &
                trim(sky_int_text(npix)) // ")"
        end if
        ! The two halves of block 0 of the per-draw key: one enciphering, as every other family's
        ! first candidate is.
        key = pf_random_key(pf_random_key(seed, label), d)
        call pf_random_fill_draws(key, i, uv, 1_int64)
        call grid%pix2vec_offset(ipix, uv(1), uv(2), v)
        ncand = 1_int64
    end subroutine sky_pixel_walk

    !> A pixel list's length, checked to be at least 1 against a grid of `npix` pixels.
    pure function sky_mask_nonempty(who, npix, n) result(nlist)
        character(len=*), intent(in) :: who !! the entry point, for the message.
        integer(int64), intent(in) :: npix !! the grid's pixel count, for the message.
        integer(int64), intent(in) :: n !! the list's size.
        integer(int64) :: nlist !! `n`, at least 1.

        if (n < 1_int64) then
            error stop who // ": pixels must be non-empty with every entry in [0, " // trim(sky_int_text(npix)) // &
                ") (the list is empty)"
        end if
        nlist = n
    end function sky_mask_nonempty

    !> Entry `k` of a pixel list, value `ipix`, checked to lie in `[0, npix)`; aborts naming it otherwise.
    pure function sky_mask_entry_ok(who, npix, k, ipix) result(ok)
        character(len=*), intent(in) :: who !! the entry point, for the message.
        integer(int64), intent(in) :: npix !! the grid's pixel count.
        integer(int64), intent(in) :: k !! the entry's 1-based position in the list.
        integer(int64), intent(in) :: ipix !! the entry's value.
        integer(int64) :: ok !! `ipix`, which is in `[0, npix)`.

        if (ipix < 0_int64 .or. ipix >= npix) then
            error stop who // ": pixels must be non-empty with every entry in [0, " // trim(sky_int_text(npix)) // &
                ") (entry " // trim(sky_int_text(k)) // " is " // trim(sky_int_text(ipix)) // ")"
        end if
        ok = ipix
    end function sky_mask_entry_ok

    !> The mask's point at draw `d` from an `int32` list of checked length `nlist`: the choice, the
    !! chosen entry's check, then the point family's walk inside it.
    pure function sky_mask_pick_l32(who, grid, npix, seed, i, pixels, nlist, d) result(v)
        character(len=*), intent(in) :: who !! the entry point, for the messages.
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64), intent(in) :: npix !! the grid's pixel count.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int32), intent(in) :: pixels(:) !! the pixel list.
        integer(int64), intent(in) :: nlist !! `size(pixels)`, at least 1.
        integer(int64), intent(in) :: d !! the draw, at least 1.
        real(real64) :: v(3) !! a unit vector uniform over the listed pixels.
        integer(int64) :: j, ipix, ncand

        j = pf_random_int_at(pf_random_key(seed, sky_mask_choice_label), i, 1_int64, nlist, d)
        ipix = sky_mask_entry_ok(who, npix, j, int(pixels(j), int64))
        call sky_pixel_walk(who, grid, sky_mask_point_label, seed, i, ipix, d, v, ncand)
    end function sky_mask_pick_l32

    !> The mask's point at draw `d` from an `int64` list. See `sky_mask_pick_l32`.
    pure function sky_mask_pick_l64(who, grid, npix, seed, i, pixels, nlist, d) result(v)
        character(len=*), intent(in) :: who !! the entry point, for the messages.
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64), intent(in) :: npix !! the grid's pixel count.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int64), intent(in) :: pixels(:) !! the pixel list.
        integer(int64), intent(in) :: nlist !! `size(pixels)`, at least 1.
        integer(int64), intent(in) :: d !! the draw, at least 1.
        real(real64) :: v(3) !! a unit vector uniform over the listed pixels.
        integer(int64) :: j, ipix, ncand

        j = pf_random_int_at(pf_random_key(seed, sky_mask_choice_label), i, 1_int64, nlist, d)
        ipix = sky_mask_entry_ok(who, npix, j, pixels(j))
        call sky_pixel_walk(who, grid, sky_mask_point_label, seed, i, ipix, d, v, ncand)
    end function sky_mask_pick_l64

    !> A scalar mask form's point for an `int32` list: the grid and the list's length checked, then
    !! the pick, which checks only the entry it chooses -- so a long list costs nothing per draw.
    pure function sky_mask_l32(who, grid, seed, i, pixels, d) result(v)
        character(len=*), intent(in) :: who !! the entry point, for the messages.
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int32), intent(in) :: pixels(:) !! the pixel list.
        integer(int64), intent(in) :: d !! the draw, at least 1.
        real(real64) :: v(3) !! a unit vector uniform over the listed pixels.
        integer(int64) :: npix

        npix = sky_grid_npix(who, grid)
        v = sky_mask_pick_l32(who, grid, npix, seed, i, pixels, sky_mask_nonempty(who, npix, size(pixels, kind=int64)), d)
    end function sky_mask_l32

    !> A scalar mask form's point for an `int64` list. See `sky_mask_l32`.
    pure function sky_mask_l64(who, grid, seed, i, pixels, d) result(v)
        character(len=*), intent(in) :: who !! the entry point, for the messages.
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int64), intent(in) :: pixels(:) !! the pixel list.
        integer(int64), intent(in) :: d !! the draw, at least 1.
        real(real64) :: v(3) !! a unit vector uniform over the listed pixels.
        integer(int64) :: npix

        npix = sky_grid_npix(who, grid)
        v = sky_mask_pick_l64(who, grid, npix, seed, i, pixels, sky_mask_nonempty(who, npix, size(pixels, kind=int64)), d)
    end function sky_mask_l64

    !> A fill's check of every entry of an `int32` list, before anything is drawn. Returns the list's
    !! length, which the fill draws its choices over.
    pure function sky_mask_list_l32(who, npix, pixels) result(nlist)
        character(len=*), intent(in) :: who !! the entry point, for the messages.
        integer(int64), intent(in) :: npix !! the grid's pixel count.
        integer(int32), intent(in) :: pixels(:) !! the pixel list.
        integer(int64) :: nlist !! `size(pixels)`, at least 1.
        integer(int64) :: k, ipix

        ! The check is written out rather than calling `sky_mask_entry_ok`, whose result nothing here
        ! would use: a `pure` call with an unused result may be deleted, and the check with it.
        nlist = sky_mask_nonempty(who, npix, size(pixels, kind=int64))
        do k = 1_int64, nlist
            ipix = int(pixels(k), int64)
            if (ipix < 0_int64 .or. ipix >= npix) then
                error stop who // ": pixels must be non-empty with every entry in [0, " // trim(sky_int_text(npix)) // &
                    ") (entry " // trim(sky_int_text(k)) // " is " // trim(sky_int_text(ipix)) // ")"
            end if
        end do
    end function sky_mask_list_l32

    !> A fill's check of every entry of an `int64` list. See `sky_mask_list_l32`.
    pure function sky_mask_list_l64(who, npix, pixels) result(nlist)
        character(len=*), intent(in) :: who !! the entry point, for the messages.
        integer(int64), intent(in) :: npix !! the grid's pixel count.
        integer(int64), intent(in) :: pixels(:) !! the pixel list.
        integer(int64) :: nlist !! `size(pixels)`, at least 1.
        integer(int64) :: k, ipix

        ! The check is written out rather than calling `sky_mask_entry_ok`, whose result nothing here
        ! would use: a `pure` call with an unused result may be deleted, and the check with it.
        nlist = sky_mask_nonempty(who, npix, size(pixels, kind=int64))
        do k = 1_int64, nlist
            ipix = pixels(k)
            if (ipix < 0_int64 .or. ipix >= npix) then
                error stop who // ": pixels must be non-empty with every entry in [0, " // trim(sky_int_text(npix)) // &
                    ") (entry " // trim(sky_int_text(k)) // " is " // trim(sky_int_text(ipix)) // ")"
            end if
        end do
    end function sky_mask_list_l64

    !> `pf_random_fill_mask`'s body for an `int32` list.
    pure subroutine sky_fill_mask_l32(grid, seed, i, pixels, v, draw)
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int32), intent(in) :: pixels(:) !! the pixel list.
        real(real64), intent(out) :: v(:, :) !! shaped `(3, n)`.
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        character(len=*), parameter :: who = "pf_random_fill_mask"
        integer(int64) :: n, k, d0, npix, nlist

        if (size(v, 1, kind=int64) /= 3_int64) then
            error stop who // ": v must be shaped (3, n) (got " // trim(sky_int_text(size(v, 1, kind=int64))) // " rows)"
        end if
        n = size(v, 2, kind=int64)
        if (n == 0_int64) return
        npix = sky_grid_npix(who, grid)
        nlist = sky_mask_list_l32(who, npix, pixels)
        d0 = sky_fill_start(who, draw, n)
        do k = 1_int64, n
            v(:, k) = sky_mask_pick_l32(who, grid, npix, seed, i, pixels, nlist, d0 + (k - 1_int64))
        end do
    end subroutine sky_fill_mask_l32

    !> `pf_random_fill_mask`'s body for an `int64` list.
    pure subroutine sky_fill_mask_l64(grid, seed, i, pixels, v, draw)
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int64), intent(in) :: pixels(:) !! the pixel list.
        real(real64), intent(out) :: v(:, :) !! shaped `(3, n)`.
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        character(len=*), parameter :: who = "pf_random_fill_mask"
        integer(int64) :: n, k, d0, npix, nlist

        if (size(v, 1, kind=int64) /= 3_int64) then
            error stop who // ": v must be shaped (3, n) (got " // trim(sky_int_text(size(v, 1, kind=int64))) // " rows)"
        end if
        n = size(v, 2, kind=int64)
        if (n == 0_int64) return
        npix = sky_grid_npix(who, grid)
        nlist = sky_mask_list_l64(who, npix, pixels)
        d0 = sky_fill_start(who, draw, n)
        do k = 1_int64, n
            v(:, k) = sky_mask_pick_l64(who, grid, npix, seed, i, pixels, nlist, d0 + (k - 1_int64))
        end do
    end subroutine sky_fill_mask_l64

    !> `pf_random_fill_mask_radec`'s body for an `int32` list.
    pure subroutine sky_fill_mask_radec_l32(grid, seed, i, pixels, ra, dec, draw)
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int32), intent(in) :: pixels(:) !! the pixel list.
        real(real64), intent(out) :: ra(:) !! right ascensions, degrees, grid's frame.
        real(real64), intent(out) :: dec(:) !! declinations, degrees, grid's frame.
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        character(len=*), parameter :: who = "pf_random_fill_mask_radec"
        integer(int64) :: n, k, d0, npix, nlist
        real(real64) :: v(3)

        n = size(ra, kind=int64)
        if (size(dec, kind=int64) /= n) then
            error stop who // ": ra and dec must have the same size (got " // trim(sky_int_text(n)) // " and " // &
                trim(sky_int_text(size(dec, kind=int64))) // ")"
        end if
        if (n == 0_int64) return
        npix = sky_grid_npix(who, grid)
        nlist = sky_mask_list_l32(who, npix, pixels)
        d0 = sky_fill_start(who, draw, n)
        do k = 1_int64, n
            v = sky_mask_pick_l32(who, grid, npix, seed, i, pixels, nlist, d0 + (k - 1_int64))
            call grid%vec2radec(v, ra(k), dec(k))
        end do
    end subroutine sky_fill_mask_radec_l32

    !> `pf_random_fill_mask_radec`'s body for an `int64` list.
    pure subroutine sky_fill_mask_radec_l64(grid, seed, i, pixels, ra, dec, draw)
        type(pf_healpix_grid), intent(in) :: grid !! the grid.
        integer(int64), intent(in) :: seed !! the stream family's seed.
        integer(int64), intent(in) :: i !! the stream index.
        integer(int64), intent(in) :: pixels(:) !! the pixel list.
        real(real64), intent(out) :: ra(:) !! right ascensions, degrees, grid's frame.
        real(real64), intent(out) :: dec(:) !! declinations, degrees, grid's frame.
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        character(len=*), parameter :: who = "pf_random_fill_mask_radec"
        integer(int64) :: n, k, d0, npix, nlist
        real(real64) :: v(3)

        n = size(ra, kind=int64)
        if (size(dec, kind=int64) /= n) then
            error stop who // ": ra and dec must have the same size (got " // trim(sky_int_text(n)) // " and " // &
                trim(sky_int_text(size(dec, kind=int64))) // ")"
        end if
        if (n == 0_int64) return
        npix = sky_grid_npix(who, grid)
        nlist = sky_mask_list_l64(who, npix, pixels)
        d0 = sky_fill_start(who, draw, n)
        do k = 1_int64, n
            v = sky_mask_pick_l64(who, grid, npix, seed, i, pixels, nlist, d0 + (k - 1_int64))
            call grid%vec2radec(v, ra(k), dec(k))
        end do
    end subroutine sky_fill_mask_radec_l64

    ! ---- Points in one pixel ----

    module procedure sky_pixel_at_i32_i32
        integer(int64) :: ncand

        call sky_pixel_walk("pf_random_pixel_at", grid, sky_pixel_label, seed, int(i, int64), int(ipix, int64), &
                            sky_draw(draw), v, ncand)
    end procedure sky_pixel_at_i32_i32

    module procedure sky_pixel_at_i32_i64
        integer(int64) :: ncand

        call sky_pixel_walk("pf_random_pixel_at", grid, sky_pixel_label, seed, int(i, int64), ipix, &
                            sky_draw(draw), v, ncand)
    end procedure sky_pixel_at_i32_i64

    module procedure sky_pixel_at_i64_i32
        integer(int64) :: ncand

        call sky_pixel_walk("pf_random_pixel_at", grid, sky_pixel_label, seed, i, int(ipix, int64), &
                            sky_draw(draw), v, ncand)
    end procedure sky_pixel_at_i64_i32

    module procedure sky_pixel_at_i64_i64
        integer(int64) :: ncand

        call sky_pixel_walk("pf_random_pixel_at", grid, sky_pixel_label, seed, i, ipix, sky_draw(draw), v, ncand)
    end procedure sky_pixel_at_i64_i64

    module procedure sky_pixel_radec_at_i32_i32
        real(real64) :: v(3)
        integer(int64) :: ncand

        call sky_pixel_walk("pf_random_pixel_radec_at", grid, sky_pixel_label, seed, int(i, int64), &
                            int(ipix, int64), sky_draw(draw), v, ncand)
        call grid%vec2radec(v, ra, dec)
    end procedure sky_pixel_radec_at_i32_i32

    module procedure sky_pixel_radec_at_i32_i64
        real(real64) :: v(3)
        integer(int64) :: ncand

        call sky_pixel_walk("pf_random_pixel_radec_at", grid, sky_pixel_label, seed, int(i, int64), ipix, &
                            sky_draw(draw), v, ncand)
        call grid%vec2radec(v, ra, dec)
    end procedure sky_pixel_radec_at_i32_i64

    module procedure sky_pixel_radec_at_i64_i32
        real(real64) :: v(3)
        integer(int64) :: ncand

        call sky_pixel_walk("pf_random_pixel_radec_at", grid, sky_pixel_label, seed, i, int(ipix, int64), &
                            sky_draw(draw), v, ncand)
        call grid%vec2radec(v, ra, dec)
    end procedure sky_pixel_radec_at_i64_i32

    module procedure sky_pixel_radec_at_i64_i64
        real(real64) :: v(3)
        integer(int64) :: ncand

        call sky_pixel_walk("pf_random_pixel_radec_at", grid, sky_pixel_label, seed, i, ipix, sky_draw(draw), &
                            v, ncand)
        call grid%vec2radec(v, ra, dec)
    end procedure sky_pixel_radec_at_i64_i64

    ! ---- Points over a pixel list ----

    module procedure sky_mask_at_i32_i32
        v = sky_mask_l32("pf_random_mask_at", grid, seed, int(i, int64), pixels, sky_draw(draw))
    end procedure sky_mask_at_i32_i32

    module procedure sky_mask_at_i32_i64
        v = sky_mask_l64("pf_random_mask_at", grid, seed, int(i, int64), pixels, sky_draw(draw))
    end procedure sky_mask_at_i32_i64

    module procedure sky_mask_at_i64_i32
        v = sky_mask_l32("pf_random_mask_at", grid, seed, i, pixels, sky_draw(draw))
    end procedure sky_mask_at_i64_i32

    module procedure sky_mask_at_i64_i64
        v = sky_mask_l64("pf_random_mask_at", grid, seed, i, pixels, sky_draw(draw))
    end procedure sky_mask_at_i64_i64

    module procedure sky_mask_radec_at_i32_i32
        real(real64) :: v(3)

        v = sky_mask_l32("pf_random_mask_radec_at", grid, seed, int(i, int64), pixels, sky_draw(draw))
        call grid%vec2radec(v, ra, dec)
    end procedure sky_mask_radec_at_i32_i32

    module procedure sky_mask_radec_at_i32_i64
        real(real64) :: v(3)

        v = sky_mask_l64("pf_random_mask_radec_at", grid, seed, int(i, int64), pixels, sky_draw(draw))
        call grid%vec2radec(v, ra, dec)
    end procedure sky_mask_radec_at_i32_i64

    module procedure sky_mask_radec_at_i64_i32
        real(real64) :: v(3)

        v = sky_mask_l32("pf_random_mask_radec_at", grid, seed, i, pixels, sky_draw(draw))
        call grid%vec2radec(v, ra, dec)
    end procedure sky_mask_radec_at_i64_i32

    module procedure sky_mask_radec_at_i64_i64
        real(real64) :: v(3)

        v = sky_mask_l64("pf_random_mask_radec_at", grid, seed, i, pixels, sky_draw(draw))
        call grid%vec2radec(v, ra, dec)
    end procedure sky_mask_radec_at_i64_i64

    module procedure sky_fill_mask_i32_i32
        call sky_fill_mask_l32(grid, seed, int(i, int64), pixels, v, draw)
    end procedure sky_fill_mask_i32_i32

    module procedure sky_fill_mask_i32_i64
        call sky_fill_mask_l64(grid, seed, int(i, int64), pixels, v, draw)
    end procedure sky_fill_mask_i32_i64

    module procedure sky_fill_mask_i64_i32
        call sky_fill_mask_l32(grid, seed, i, pixels, v, draw)
    end procedure sky_fill_mask_i64_i32

    module procedure sky_fill_mask_i64_i64
        call sky_fill_mask_l64(grid, seed, i, pixels, v, draw)
    end procedure sky_fill_mask_i64_i64

    module procedure sky_fill_mask_radec_i32_i32
        call sky_fill_mask_radec_l32(grid, seed, int(i, int64), pixels, ra, dec, draw)
    end procedure sky_fill_mask_radec_i32_i32

    module procedure sky_fill_mask_radec_i32_i64
        call sky_fill_mask_radec_l64(grid, seed, int(i, int64), pixels, ra, dec, draw)
    end procedure sky_fill_mask_radec_i32_i64

    module procedure sky_fill_mask_radec_i64_i32
        call sky_fill_mask_radec_l32(grid, seed, i, pixels, ra, dec, draw)
    end procedure sky_fill_mask_radec_i64_i32

    module procedure sky_fill_mask_radec_i64_i64
        call sky_fill_mask_radec_l64(grid, seed, i, pixels, ra, dec, draw)
    end procedure sky_fill_mask_radec_i64_i64

    ! ---- Stream forms ----

    module procedure sky_pixel_next_i32
        integer(int64) :: seed, stream, d, ncand

        call sky_take_block("pf_random_pixel_next", rng, seed, stream, d)
        call sky_pixel_walk("pf_random_pixel_next", grid, sky_pixel_label, seed, stream, int(ipix, int64), d, v, ncand)
    end procedure sky_pixel_next_i32

    module procedure sky_pixel_next_i64
        integer(int64) :: seed, stream, d, ncand

        call sky_take_block("pf_random_pixel_next", rng, seed, stream, d)
        call sky_pixel_walk("pf_random_pixel_next", grid, sky_pixel_label, seed, stream, ipix, d, v, ncand)
    end procedure sky_pixel_next_i64

    module procedure sky_pixel_radec_next_i32
        real(real64) :: v(3)
        integer(int64) :: seed, stream, d, ncand

        call sky_take_block("pf_random_pixel_radec_next", rng, seed, stream, d)
        call sky_pixel_walk("pf_random_pixel_radec_next", grid, sky_pixel_label, seed, stream, int(ipix, int64), d, &
                            v, ncand)
        call grid%vec2radec(v, ra, dec)
    end procedure sky_pixel_radec_next_i32

    module procedure sky_pixel_radec_next_i64
        real(real64) :: v(3)
        integer(int64) :: seed, stream, d, ncand

        call sky_take_block("pf_random_pixel_radec_next", rng, seed, stream, d)
        call sky_pixel_walk("pf_random_pixel_radec_next", grid, sky_pixel_label, seed, stream, ipix, d, v, ncand)
        call grid%vec2radec(v, ra, dec)
    end procedure sky_pixel_radec_next_i64

    module procedure sky_mask_next_i32
        integer(int64) :: seed, stream, d

        call sky_take_block("pf_random_mask_next", rng, seed, stream, d)
        v = sky_mask_l32("pf_random_mask_next", grid, seed, stream, pixels, d)
    end procedure sky_mask_next_i32

    module procedure sky_mask_next_i64
        integer(int64) :: seed, stream, d

        call sky_take_block("pf_random_mask_next", rng, seed, stream, d)
        v = sky_mask_l64("pf_random_mask_next", grid, seed, stream, pixels, d)
    end procedure sky_mask_next_i64

    module procedure sky_mask_radec_next_i32
        integer(int64) :: seed, stream, d
        real(real64) :: v(3)

        call sky_take_block("pf_random_mask_radec_next", rng, seed, stream, d)
        v = sky_mask_l32("pf_random_mask_radec_next", grid, seed, stream, pixels, d)
        call grid%vec2radec(v, ra, dec)
    end procedure sky_mask_radec_next_i32

    module procedure sky_mask_radec_next_i64
        integer(int64) :: seed, stream, d
        real(real64) :: v(3)

        call sky_take_block("pf_random_mask_radec_next", rng, seed, stream, d)
        v = sky_mask_l64("pf_random_mask_radec_next", grid, seed, stream, pixels, d)
        call grid%vec2radec(v, ra, dec)
    end procedure sky_mask_radec_next_i64

    ! ---- Test-only hook ----

    module procedure parquet_debug_sphere_pixel_draw
        call sky_pixel_walk("parquet_debug_sphere_pixel_draw", grid, sky_pixel_label, seed, i, ipix, sky_draw(draw), &
                            v, ncand)
    end procedure parquet_debug_sphere_pixel_draw

end submodule parquet_sphere_pixel
