!> `pf_healpix_grid`: the grid object, and the RA/Dec layer that lives on it and nowhere else.
!!
!! **Almost every procedure here is one call to a free procedure declared in the parent module.**
!! That is the design rather than an accident: the object and the free API then agree by
!! construction rather than by test. The delegation costs a call and nothing else: see the type's
!! own doc-comment in the parent module for the measurement, including why the ifx spread from
!! 0.66x to 1.09x is the benchmark's free column changing shape under that compiler rather than
!! anything this file does.
!!
!! **Only two things are computed here rather than delegated.** The degree/radian scaling, and the
!! declination reflection `theta = pi/2 -/+ dec` that `PF_HP_DEC_NORTH` and `PF_HP_DEC_SOUTH`
!! select between. Both are a multiply and a subtract, and both are confined to `hpx_grid_theta`
!! and `hpx_grid_dec` at the bottom of this file, so there is one copy of each to get right.
!!
!! **Three rules govern what a binding does with a grid `%init` has never run on**, and they are
!! the module's existing rules rather than new ones:
!!
!! * a binding whose result could never legitimately be -1 returns -1: the pixel-valued ones, and
!!   `%order`, `%pixarea`, `%resol` and `%max_pixrad`, whose real results are all positive;
!! * a binding reporting a DIRECTION -- `%pix2ang`, `%pix2vec`, `%pix2radec`, `%radec2vec`,
!!   `%vec2radec` -- returns `hpx_grid_unset_real` (-999), because -1 is an ordinary declination or
!!   vector component there and would go unnoticed. -999 is outside the range of every angle this
!!   module returns, and it is answered HERE rather than delegated, because delegating would divide
!!   by `nside = 0` and raise `IEEE_DIVIDE_BY_ZERO`;
!! * a disc or bulk binding aborts, being a once-per-query entry point where validation belongs.
!!
!! The same three apply when an int32 output is asked of a grid finer than `nside = 8192`, where
!! the index simply does not fit: -1, -999, or an abort naming the int64 alternative.
submodule(parquet_healpix) parquet_healpix_grid
    implicit none

contains

    ! ---- Construction ----

    module procedure hpx_grid_init_i32
        call hpx_grid_build(this, int(nside, int64), hpx_nside_max_i32, scheme, "pf_healpix_grid%init", frame)
    end procedure hpx_grid_init_i32

    module procedure hpx_grid_init_i64
        call hpx_grid_build(this, nside, hpx_nside_max, scheme, "pf_healpix_grid%init", frame)
    end procedure hpx_grid_init_i64

    ! ---- Accessors with no kind question ----

    module procedure hpx_grid_is_set
        ok = this%nside_v > 0_int64
    end procedure hpx_grid_is_set

    module procedure hpx_grid_order
        order = this%order_v
    end procedure hpx_grid_order

    module procedure hpx_grid_scheme
        scheme = this%scheme_id
    end procedure hpx_grid_scheme

    module procedure hpx_grid_frame
        frame = this%frame_id
    end procedure hpx_grid_frame

    module procedure hpx_grid_pixarea
        if (this%nside_v <= 0_int64) then
            area = -1.0_real64
        else
            area = pf_nside2pixarea(this%nside_v)
        end if
    end procedure hpx_grid_pixarea

    module procedure hpx_grid_resol
        if (this%nside_v <= 0_int64) then
            resol = -1.0_real64
        else
            resol = pf_nside2resol(this%nside_v)
        end if
    end procedure hpx_grid_resol

    module procedure hpx_grid_max_pixrad
        if (this%nside_v <= 0_int64) then
            r = -1.0_real64
        else
            r = pf_max_pixrad(this%nside_v)
        end if
    end procedure hpx_grid_max_pixrad

    ! ---- Accessors that carry a kind question ----

    module procedure hpx_grid_get_nside_i32
        call hpx_grid_fit_i32(this%nside_v, "pf_healpix_grid%get_nside", nside)
    end procedure hpx_grid_get_nside_i32

    module procedure hpx_grid_get_nside_i64
        nside = this%nside_v
    end procedure hpx_grid_get_nside_i64

    module procedure hpx_grid_get_npix_i32
        call hpx_grid_fit_i32(this%npix_v, "pf_healpix_grid%get_npix", npix)
    end procedure hpx_grid_get_npix_i32

    module procedure hpx_grid_get_npix_i64
        npix = this%npix_v
    end procedure hpx_grid_get_npix_i64

    ! ---- Native conversions ----

    module procedure hpx_grid_ang2pix_i32
        if (.not. hpx_grid_fits_i32(this)) then
            ipix = -1_int32
        else if (this%scheme_id == PF_HP_NEST) then
            call pf_ang2pix_nest(int(this%nside_v, int32), theta, phi, ipix)
        else
            call pf_ang2pix_ring(int(this%nside_v, int32), theta, phi, ipix)
        end if
    end procedure hpx_grid_ang2pix_i32

    module procedure hpx_grid_ang2pix_i64
        if (this%nside_v <= 0_int64) then
            ipix = -1_int64
        else if (this%scheme_id == PF_HP_NEST) then
            call pf_ang2pix_nest(this%nside_v, theta, phi, ipix)
        else
            call pf_ang2pix_ring(this%nside_v, theta, phi, ipix)
        end if
    end procedure hpx_grid_ang2pix_i64

    module procedure hpx_grid_pix2ang_i32
        if (.not. hpx_grid_fits_i32(this)) then
            theta = hpx_grid_unset_real
            phi = hpx_grid_unset_real
        else if (this%scheme_id == PF_HP_NEST) then
            call pf_pix2ang_nest(int(this%nside_v, int32), ipix, theta, phi)
        else
            call pf_pix2ang_ring(int(this%nside_v, int32), ipix, theta, phi)
        end if
    end procedure hpx_grid_pix2ang_i32

    module procedure hpx_grid_pix2ang_i64
        if (this%nside_v <= 0_int64) then
            theta = hpx_grid_unset_real
            phi = hpx_grid_unset_real
        else if (this%scheme_id == PF_HP_NEST) then
            call pf_pix2ang_nest(this%nside_v, ipix, theta, phi)
        else
            call pf_pix2ang_ring(this%nside_v, ipix, theta, phi)
        end if
    end procedure hpx_grid_pix2ang_i64

    module procedure hpx_grid_vec2pix_i32
        if (.not. hpx_grid_fits_i32(this)) then
            ipix = -1_int32
        else if (this%scheme_id == PF_HP_NEST) then
            call pf_vec2pix_nest(int(this%nside_v, int32), vec, ipix)
        else
            call pf_vec2pix_ring(int(this%nside_v, int32), vec, ipix)
        end if
    end procedure hpx_grid_vec2pix_i32

    module procedure hpx_grid_vec2pix_i64
        if (this%nside_v <= 0_int64) then
            ipix = -1_int64
        else if (this%scheme_id == PF_HP_NEST) then
            call pf_vec2pix_nest(this%nside_v, vec, ipix)
        else
            call pf_vec2pix_ring(this%nside_v, vec, ipix)
        end if
    end procedure hpx_grid_vec2pix_i64

    module procedure hpx_grid_pix2vec_i32
        if (.not. hpx_grid_fits_i32(this)) then
            vec = hpx_grid_unset_real
        else if (this%scheme_id == PF_HP_NEST) then
            call pf_pix2vec_nest(int(this%nside_v, int32), ipix, vec)
        else
            call pf_pix2vec_ring(int(this%nside_v, int32), ipix, vec)
        end if
    end procedure hpx_grid_pix2vec_i32

    module procedure hpx_grid_pix2vec_i64
        if (this%nside_v <= 0_int64) then
            vec = hpx_grid_unset_real
        else if (this%scheme_id == PF_HP_NEST) then
            call pf_pix2vec_nest(this%nside_v, ipix, vec)
        else
            call pf_pix2vec_ring(this%nside_v, ipix, vec)
        end if
    end procedure hpx_grid_pix2vec_i64

    module procedure hpx_grid_pix2vec_off_i32
        call hpx_grid_pix2vec_off_i64(this, int(ipix, int64), dx, dy, vec)
    end procedure hpx_grid_pix2vec_off_i32

    module procedure hpx_grid_pix2vec_off_i64
        integer(int64) :: ipn

        if (this%nside_v <= 0_int64) then
            vec = hpx_grid_unset_real
        else
            ! The projection is defined on the NEST layout: a RING index is converted first, exactly
            ! as `pf_pix2vec_nest` goes the other way for its own centre.
            ipn = ipix
            if (this%scheme_id /= PF_HP_NEST) call pf_ring2nest(this%nside_v, ipix, ipn)
            call hpx_pix2vec_offset_nest(this%nside_v, ipn, dx, dy, vec)
        end if
    end procedure hpx_grid_pix2vec_off_i64

    ! ---- The RA/Dec layer ----

    module procedure hpx_grid_radec2pix_i32
        call this%ang2pix(hpx_grid_theta(this%frame_id, dec), ra * hpx_deg2rad, ipix)
    end procedure hpx_grid_radec2pix_i32

    module procedure hpx_grid_radec2pix_i64
        call this%ang2pix(hpx_grid_theta(this%frame_id, dec), ra * hpx_deg2rad, ipix)
    end procedure hpx_grid_radec2pix_i64

    module procedure hpx_grid_pix2radec_i32
        real(real64) :: theta, phi

        if (.not. hpx_grid_fits_i32(this)) then
            ra = hpx_grid_unset_real
            dec = hpx_grid_unset_real
            return
        end if
        call this%pix2ang(ipix, theta, phi)
        call hpx_grid_from_ang(this%frame_id, theta, phi, ra, dec)
    end procedure hpx_grid_pix2radec_i32

    module procedure hpx_grid_pix2radec_i64
        real(real64) :: theta, phi

        if (this%nside_v <= 0_int64) then
            ra = hpx_grid_unset_real
            dec = hpx_grid_unset_real
            return
        end if
        call this%pix2ang(ipix, theta, phi)
        call hpx_grid_from_ang(this%frame_id, theta, phi, ra, dec)
    end procedure hpx_grid_pix2radec_i64

    module procedure hpx_grid_radec2vec
        call pf_ang2vec(hpx_grid_theta(this%frame_id, dec), ra * hpx_deg2rad, vec)
    end procedure hpx_grid_radec2vec

    module procedure hpx_grid_vec2radec
        real(real64) :: theta, phi

        call pf_vec2ang(vec, theta, phi)
        call hpx_grid_from_ang(this%frame_id, theta, phi, ra, dec)
    end procedure hpx_grid_vec2radec

    ! ---- Disc queries ----

    module procedure hpx_grid_disc_i32
        call hpx_grid_require_i32(this, "pf_healpix_grid%query_disc")
        call pf_query_disc(int(this%nside_v, int32), vec, radius, listpix, nlist, &
                           scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_i32

    module procedure hpx_grid_disc_i64
        call hpx_grid_require(this, "pf_healpix_grid%query_disc")
        call pf_query_disc(this%nside_v, vec, radius, listpix, nlist, &
                           scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_i64

    module procedure hpx_grid_disc_count_i32
        call hpx_grid_require_i32(this, "pf_healpix_grid%query_disc_count")
        call pf_query_disc_count(int(this%nside_v, int32), vec, radius, nlist, &
                                 scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_count_i32

    module procedure hpx_grid_disc_count_i64
        call hpx_grid_require(this, "pf_healpix_grid%query_disc_count")
        call pf_query_disc_count(this%nside_v, vec, radius, nlist, &
                                 scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_count_i64

    module procedure hpx_grid_disc_alloc_i32
        call hpx_grid_require_i32(this, "pf_healpix_grid%query_disc_alloc")
        call pf_query_disc_alloc(int(this%nside_v, int32), vec, radius, listpix, nlist, &
                                 scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_alloc_i32

    module procedure hpx_grid_disc_alloc_i64
        call hpx_grid_require(this, "pf_healpix_grid%query_disc_alloc")
        call pf_query_disc_alloc(this%nside_v, vec, radius, listpix, nlist, &
                                 scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_alloc_i64

    module procedure hpx_grid_disc_rd_i32
        real(real64) :: centre(3)

        call hpx_grid_require_i32(this, "pf_healpix_grid%query_disc_radec")
        call this%radec2vec(ra, dec, centre)
        call pf_query_disc(int(this%nside_v, int32), centre, radius_deg * hpx_deg2rad, listpix, &
                           nlist, scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_rd_i32

    module procedure hpx_grid_disc_rd_i64
        real(real64) :: centre(3)

        call hpx_grid_require(this, "pf_healpix_grid%query_disc_radec")
        call this%radec2vec(ra, dec, centre)
        call pf_query_disc(this%nside_v, centre, radius_deg * hpx_deg2rad, listpix, nlist, &
                           scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_rd_i64

    module procedure hpx_grid_disc_rd_count_i32
        real(real64) :: centre(3)

        call hpx_grid_require_i32(this, "pf_healpix_grid%query_disc_radec_count")
        call this%radec2vec(ra, dec, centre)
        call pf_query_disc_count(int(this%nside_v, int32), centre, radius_deg * hpx_deg2rad, &
                                 nlist, scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_rd_count_i32

    module procedure hpx_grid_disc_rd_count_i64
        real(real64) :: centre(3)

        call hpx_grid_require(this, "pf_healpix_grid%query_disc_radec_count")
        call this%radec2vec(ra, dec, centre)
        call pf_query_disc_count(this%nside_v, centre, radius_deg * hpx_deg2rad, nlist, &
                                 scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_rd_count_i64

    module procedure hpx_grid_disc_rd_alloc_i32
        real(real64) :: centre(3)

        call hpx_grid_require_i32(this, "pf_healpix_grid%query_disc_radec_alloc")
        call this%radec2vec(ra, dec, centre)
        call pf_query_disc_alloc(int(this%nside_v, int32), centre, radius_deg * hpx_deg2rad, &
                                 listpix, nlist, scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_rd_alloc_i32

    module procedure hpx_grid_disc_rd_alloc_i64
        real(real64) :: centre(3)

        call hpx_grid_require(this, "pf_healpix_grid%query_disc_radec_alloc")
        call this%radec2vec(ra, dec, centre)
        call pf_query_disc_alloc(this%nside_v, centre, radius_deg * hpx_deg2rad, listpix, nlist, &
                                 scheme=this%scheme_id, inclusive=inclusive)
    end procedure hpx_grid_disc_rd_alloc_i64

    ! ---- Threaded forms ----

    module procedure hpx_grid_ang2pix_bulk_i32
        call hpx_grid_require_i32(this, "pf_healpix_grid%ang2pix_bulk")
        if (this%scheme_id == PF_HP_NEST) then
            call pf_ang2pix_nest_bulk(int(this%nside_v, int32), theta, phi, ipix, threads)
        else
            call pf_ang2pix_ring_bulk(int(this%nside_v, int32), theta, phi, ipix, threads)
        end if
    end procedure hpx_grid_ang2pix_bulk_i32

    module procedure hpx_grid_ang2pix_bulk_i64
        call hpx_grid_require(this, "pf_healpix_grid%ang2pix_bulk")
        if (this%scheme_id == PF_HP_NEST) then
            call pf_ang2pix_nest_bulk(this%nside_v, theta, phi, ipix, threads)
        else
            call pf_ang2pix_ring_bulk(this%nside_v, theta, phi, ipix, threads)
        end if
    end procedure hpx_grid_ang2pix_bulk_i64

    module procedure hpx_grid_pix2ang_bulk_i32
        call hpx_grid_require_i32(this, "pf_healpix_grid%pix2ang_bulk")
        if (this%scheme_id == PF_HP_NEST) then
            call pf_pix2ang_nest_bulk(int(this%nside_v, int32), ipix, theta, phi, threads)
        else
            call pf_pix2ang_ring_bulk(int(this%nside_v, int32), ipix, theta, phi, threads)
        end if
    end procedure hpx_grid_pix2ang_bulk_i32

    module procedure hpx_grid_pix2ang_bulk_i64
        call hpx_grid_require(this, "pf_healpix_grid%pix2ang_bulk")
        if (this%scheme_id == PF_HP_NEST) then
            call pf_pix2ang_nest_bulk(this%nside_v, ipix, theta, phi, threads)
        else
            call pf_pix2ang_ring_bulk(this%nside_v, ipix, theta, phi, threads)
        end if
    end procedure hpx_grid_pix2ang_bulk_i64

    module procedure hpx_grid_vec2pix_bulk_i32
        call hpx_grid_require_i32(this, "pf_healpix_grid%vec2pix_bulk")
        if (this%scheme_id == PF_HP_NEST) then
            call pf_vec2pix_nest_bulk(int(this%nside_v, int32), vec, ipix, threads)
        else
            call pf_vec2pix_ring_bulk(int(this%nside_v, int32), vec, ipix, threads)
        end if
    end procedure hpx_grid_vec2pix_bulk_i32

    module procedure hpx_grid_vec2pix_bulk_i64
        call hpx_grid_require(this, "pf_healpix_grid%vec2pix_bulk")
        if (this%scheme_id == PF_HP_NEST) then
            call pf_vec2pix_nest_bulk(this%nside_v, vec, ipix, threads)
        else
            call pf_vec2pix_ring_bulk(this%nside_v, vec, ipix, threads)
        end if
    end procedure hpx_grid_vec2pix_bulk_i64

    module procedure hpx_grid_pix2vec_bulk_i32
        call hpx_grid_require_i32(this, "pf_healpix_grid%pix2vec_bulk")
        if (this%scheme_id == PF_HP_NEST) then
            call pf_pix2vec_nest_bulk(int(this%nside_v, int32), ipix, vec, threads)
        else
            call pf_pix2vec_ring_bulk(int(this%nside_v, int32), ipix, vec, threads)
        end if
    end procedure hpx_grid_pix2vec_bulk_i32

    module procedure hpx_grid_pix2vec_bulk_i64
        call hpx_grid_require(this, "pf_healpix_grid%pix2vec_bulk")
        if (this%scheme_id == PF_HP_NEST) then
            call pf_pix2vec_nest_bulk(this%nside_v, ipix, vec, threads)
        else
            call pf_pix2vec_ring_bulk(this%nside_v, ipix, vec, threads)
        end if
    end procedure hpx_grid_pix2vec_bulk_i64

    module procedure hpx_grid_radec2pix_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        call hpx_grid_require_i32(this, "pf_healpix_grid%radec2pix_bulk")
        n = int(size(ra), int64)
        call hpx_check_bulk_sizes(n, [int(size(dec), int64), int(size(ipix), int64)], &
                                  "pf_healpix_grid%radec2pix_bulk")
        if (n == 0_int64) return
        nt = hpx_threads(threads, n, "pf_healpix_grid%radec2pix_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call this%radec2pix(ra(k), dec(k), ipix(k))
        end do
    end procedure hpx_grid_radec2pix_bulk_i32

    module procedure hpx_grid_radec2pix_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        call hpx_grid_require(this, "pf_healpix_grid%radec2pix_bulk")
        n = int(size(ra), int64)
        call hpx_check_bulk_sizes(n, [int(size(dec), int64), int(size(ipix), int64)], &
                                  "pf_healpix_grid%radec2pix_bulk")
        if (n == 0_int64) return
        nt = hpx_threads(threads, n, "pf_healpix_grid%radec2pix_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call this%radec2pix(ra(k), dec(k), ipix(k))
        end do
    end procedure hpx_grid_radec2pix_bulk_i64

    module procedure hpx_grid_pix2radec_bulk_i32
        integer(int64) :: n, k
        integer :: nt

        call hpx_grid_require_i32(this, "pf_healpix_grid%pix2radec_bulk")
        n = int(size(ipix), int64)
        call hpx_check_bulk_sizes(n, [int(size(ra), int64), int(size(dec), int64)], &
                                  "pf_healpix_grid%pix2radec_bulk")
        if (n == 0_int64) return
        nt = hpx_threads(threads, n, "pf_healpix_grid%pix2radec_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call this%pix2radec(ipix(k), ra(k), dec(k))
        end do
    end procedure hpx_grid_pix2radec_bulk_i32

    module procedure hpx_grid_pix2radec_bulk_i64
        integer(int64) :: n, k
        integer :: nt

        call hpx_grid_require(this, "pf_healpix_grid%pix2radec_bulk")
        n = int(size(ipix), int64)
        call hpx_check_bulk_sizes(n, [int(size(ra), int64), int(size(dec), int64)], &
                                  "pf_healpix_grid%pix2radec_bulk")
        if (n == 0_int64) return
        nt = hpx_threads(threads, n, "pf_healpix_grid%pix2radec_bulk")
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k) if (nt > 1)
        do k = 1_int64, n
            call this%pix2radec(ipix(k), ra(k), dec(k))
        end do
    end procedure hpx_grid_pix2radec_bulk_i64

    ! ---- Resolution change and comparison ----

    module procedure hpx_grid_at_nside_i32
        g = hpx_grid_derive(this, int(nside, int64), hpx_nside_max_i32)
    end procedure hpx_grid_at_nside_i32

    module procedure hpx_grid_at_nside_i64
        g = hpx_grid_derive(this, nside, hpx_nside_max)
    end procedure hpx_grid_at_nside_i64

    module procedure hpx_grid_at_order_i32
        g = hpx_grid_derive(this, hpx_grid_pow2(int(order, int64)), hpx_nside_max)
    end procedure hpx_grid_at_order_i32

    module procedure hpx_grid_at_order_i64
        g = hpx_grid_derive(this, hpx_grid_pow2(order), hpx_nside_max)
    end procedure hpx_grid_at_order_i64

    module procedure hpx_grid_ud_ord_i32
        if (this%nside_v <= 0_int64 .or. this%scheme_id /= PF_HP_NEST) then
            ipix_out = -1_int32
        else
            call pf_ud_pix_nest(ipix, this%order_v, order_out, ipix_out)
        end if
    end procedure hpx_grid_ud_ord_i32

    module procedure hpx_grid_ud_ord_i64
        if (this%nside_v <= 0_int64 .or. this%scheme_id /= PF_HP_NEST) then
            ipix_out = -1_int64
        else
            call pf_ud_pix_nest(ipix, int(this%order_v, int64), order_out, ipix_out)
        end if
    end procedure hpx_grid_ud_ord_i64

    module procedure hpx_grid_ud_grid_i32
        if (grid_out%nside_v <= 0_int64 .or. grid_out%scheme_id /= PF_HP_NEST) then
            ipix_out = -1_int32
        else
            call this%ud_pix(ipix, grid_out%order_v, ipix_out)
        end if
    end procedure hpx_grid_ud_grid_i32

    module procedure hpx_grid_ud_grid_i64
        if (grid_out%nside_v <= 0_int64 .or. grid_out%scheme_id /= PF_HP_NEST) then
            ipix_out = -1_int64
        else
            call this%ud_pix(ipix, int(grid_out%order_v, int64), ipix_out)
        end if
    end procedure hpx_grid_ud_grid_i64

    module procedure hpx_grid_eq
        same = this%nside_v == other%nside_v .and. this%scheme_id == other%scheme_id &
               .and. this%frame_id == other%frame_id
    end procedure hpx_grid_eq

    module procedure hpx_grid_ne
        diff = .not. (this%nside_v == other%nside_v .and. this%scheme_id == other%scheme_id &
                      .and. this%frame_id == other%frame_id)
    end procedure hpx_grid_ne

    ! ---- Private helpers ----
    !
    ! Contained in this submodule rather than declared in the parent module: every caller is in
    ! this file, and a private procedure contained directly in the MODULE would be given internal
    ! linkage by gfortran and then fail to link from any submodule that called it.

    !> Validates and fills a grid. The whole of `%init`, shared by both kinds.
    !>
    !> `nside_max` is the ceiling for the kind the caller used, so the message an int32 caller sees
    !> names 8192 rather than 2**29 -- the limit that actually applies to them.
    subroutine hpx_grid_build(this, nside, nside_max, scheme, what, frame)
        class(pf_healpix_grid), intent(out) :: this !! the grid to fill; every field is reset first.
        integer(int64), intent(in) :: nside !! the requested resolution parameter.
        integer(int64), intent(in) :: nside_max !! the ceiling for the caller's integer kind.
        integer, intent(in) :: scheme !! `PF_HP_RING` or `PF_HP_NEST`.
        character(len=*), intent(in) :: what !! the entry point, for every message.
        integer, intent(in), optional :: frame !! declination convention; absent means north.
        character(len=:), allocatable :: got, limit

        if (.not. hpx_nside_ok(nside, nside_max)) then
            call hpx_itoa(nside, got)
            call hpx_itoa(nside_max, limit)
            error stop what // ": nside must be a positive power of two at most " // limit // &
                ", got " // got
        end if
        if (scheme /= PF_HP_RING .and. scheme /= PF_HP_NEST) then
            call hpx_itoa(int(scheme, int64), got)
            error stop what // ": scheme must be PF_HP_RING (0) or PF_HP_NEST (1), got " // got
        end if
        this%frame_id = PF_HP_DEC_NORTH
        if (present(frame)) then
            if (frame /= PF_HP_DEC_NORTH .and. frame /= PF_HP_DEC_SOUTH) then
                call hpx_itoa(int(frame, int64), got)
                error stop what // ": frame must be PF_HP_DEC_NORTH (0) or PF_HP_DEC_SOUTH (1), " // &
                    "got " // got
            end if
            this%frame_id = frame
        end if
        this%nside_v = nside
        this%order_v = int(trailz(nside), int32)
        this%npix_v = 12_int64 * nside * nside
        this%scheme_id = scheme
    end subroutine hpx_grid_build

    !> A grid at `nside` carrying `src`'s scheme and frame, or an unbuilt one if `nside` is invalid.
    !>
    !> `pure`, so it reports an invalid `nside` by returning a grid whose `%is_set()` is `.false.`
    !> rather than by aborting -- see `%at_nside`'s doc-comment for why that trade is taken.
    pure function hpx_grid_derive(src, nside, nside_max) result(g)
        class(pf_healpix_grid), intent(in) :: src !! the grid whose scheme and frame are kept.
        integer(int64), intent(in) :: nside !! the requested resolution parameter.
        integer(int64), intent(in) :: nside_max !! the ceiling for the caller's integer kind.
        type(pf_healpix_grid) :: g !! the derived grid, built or not.

        g%scheme_id = src%scheme_id
        g%frame_id = src%frame_id
        if (.not. hpx_nside_ok(nside, nside_max)) return
        g%nside_v = nside
        g%order_v = int(trailz(nside), int32)
        g%npix_v = 12_int64 * nside * nside
    end function hpx_grid_derive

    !> `2**order`, or 0 for an order outside `0 .. 29` so that the caller's validation rejects it.
    pure function hpx_grid_pow2(order) result(nside)
        integer(int64), intent(in) :: order !! the resolution order.
        integer(int64) :: nside !! `2**order`, or 0 when `order` is out of range.

        if (order < 0_int64 .or. order > hpx_order_max) then
            nside = 0_int64
        else
            nside = ishft(1_int64, int(order))
        end if
    end function hpx_grid_pow2

    !> Colatitude of a declination, in the given convention. One of this file's two computations.
    pure function hpx_grid_theta(frame, dec) result(theta)
        integer(int32), intent(in) :: frame !! `PF_HP_DEC_NORTH` or `PF_HP_DEC_SOUTH`.
        real(real64), intent(in) :: dec !! declination, degrees.
        real(real64) :: theta !! colatitude, radians.

        if (frame == PF_HP_DEC_SOUTH) then
            theta = (90.0_real64 + dec) * hpx_deg2rad
        else
            theta = (90.0_real64 - dec) * hpx_deg2rad
        end if
    end function hpx_grid_theta

    !> Declination of a colatitude, in the given convention. The exact inverse of `hpx_grid_theta`.
    pure function hpx_grid_dec(frame, theta) result(dec)
        integer(int32), intent(in) :: frame !! `PF_HP_DEC_NORTH` or `PF_HP_DEC_SOUTH`.
        real(real64), intent(in) :: theta !! colatitude, radians.
        real(real64) :: dec !! declination, degrees.

        if (frame == PF_HP_DEC_SOUTH) then
            dec = theta * hpx_rad2deg - 90.0_real64
        else
            dec = 90.0_real64 - theta * hpx_rad2deg
        end if
    end function hpx_grid_dec

    !> `(theta, phi)` in radians to `(ra, dec)` in degrees, in the given convention.
    !>
    !> The `ra >= 360` fold is not redundant. `pf_pix2ang_*` returns `phi` strictly below `2*pi`,
    !> but `phi * (180/pi)` can round up to exactly 360, which is outside the `[0, 360)` this
    !> promises; one compare puts it back.
    pure subroutine hpx_grid_from_ang(frame, theta, phi, ra, dec)
        integer(int32), intent(in) :: frame !! `PF_HP_DEC_NORTH` or `PF_HP_DEC_SOUTH`.
        real(real64), intent(in) :: theta !! colatitude, radians.
        real(real64), intent(in) :: phi !! longitude, radians, in `[0, 2*pi)`.
        real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`.
        real(real64), intent(out) :: dec !! declination, degrees.

        ra = phi * hpx_rad2deg
        if (ra >= 360.0_real64) ra = ra - 360.0_real64
        dec = hpx_grid_dec(frame, theta)
    end subroutine hpx_grid_from_ang

    !> Whether this grid is built AND fine enough to be addressed with `integer(int32)` indices.
    pure function hpx_grid_fits_i32(this) result(ok)
        class(pf_healpix_grid), intent(in) :: this !! the grid.
        logical :: ok !! `.true.` when an int32 pixel index can hold every pixel of it.

        ok = this%nside_v > 0_int64 .and. this%nside_v <= hpx_nside_max_i32
    end function hpx_grid_fits_i32

    !> Aborts unless `%init` has run on this grid.
    subroutine hpx_grid_require(this, what)
        class(pf_healpix_grid), intent(in) :: this !! the grid.
        character(len=*), intent(in) :: what !! the entry point, for the message.

        if (this%nside_v <= 0_int64) then
            error stop what // ": this grid has not been built; call %init(nside, scheme) first"
        end if
    end subroutine hpx_grid_require

    !> Aborts unless `%init` has run AND the grid is addressable with `integer(int32)` indices.
    subroutine hpx_grid_require_i32(this, what)
        class(pf_healpix_grid), intent(in) :: this !! the grid.
        character(len=*), intent(in) :: what !! the entry point, for the message.
        character(len=:), allocatable :: got, limit

        call hpx_grid_require(this, what)
        if (this%nside_v > hpx_nside_max_i32) then
            call hpx_itoa(this%nside_v, got)
            call hpx_itoa(hpx_nside_max_i32, limit)
            error stop what // ": this grid's nside " // got // " exceeds " // limit // &
                ", the largest an integer(int32) index can address; use integer(int64) arguments"
        end if
    end subroutine hpx_grid_require_i32

    !> Narrows an `integer(int64)` grid quantity to `integer(int32)`, or aborts if it does not fit.
    subroutine hpx_grid_fit_i32(value, what, out)
        integer(int64), intent(in) :: value !! the quantity to narrow.
        character(len=*), intent(in) :: what !! the entry point, for the message.
        integer(int32), intent(out) :: out !! the narrowed value.
        character(len=:), allocatable :: got, limit

        if (value > int(huge(0_int32), int64)) then
            call hpx_itoa(value, got)
            call hpx_itoa(int(huge(0_int32), int64), limit)
            error stop what // ": " // got // " exceeds the largest integer(int32), " // limit // &
                "; take this value in an integer(int64) instead"
        end if
        out = int(value, int32)
    end subroutine hpx_grid_fit_i32

end submodule parquet_healpix_grid
