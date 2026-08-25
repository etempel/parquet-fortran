!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for `parquet_spatial`: `pf_spatial_index` and its ball, bulk and periodic queries.
!>
!> Four things shape this suite:
!>
!> * **The oracle is a brute-force O(n^2) scan, and it has to be.** Every other check available here
!>   -- a bulk form against a single query, a periodic query against a shifted one -- compares one
!>   part of the grid against another, so a defect in the cell walk itself would satisfy all of
!>   them at once. Only a scan that never touches the grid is independent of what is under test.
!> * **The answers must not depend on the cell size, and that is asserted directly.** The whole
!>   tuner is a performance mechanism, so a cell size that changes an ANSWER is a defect no
!>   benchmark would notice. `test_answers_survive_any_cell` forces cells across two orders of
!>   magnitude and demands the same rows back every time.
!> * **A probe that never runs passes every correctness test ever written for it**, because the
!>   answers do not depend on the cell. `parquet_debug_spatial_probe_count` is the negative control
!>   that separates "the probe chose well" from "the probe did not run", and both directions are
!>   asserted.
!> * **Periodic boundaries are tested by TRANSLATION INVARIANCE**, not by a face-crossing spot
!>   check. Shifting every point by an arbitrary vector modulo the box must leave every neighbour
!>   set identical up to that shift; a naive implementation passes a spot check and fails this.
!>
!> Tests here allocate their own arrays and write no files, so nothing needs a per-test fixture
!> name -- but several drive the process-global debug hooks, which is why `run_tester.f90` excludes
!> this suite from test-drive's per-test parallelism.
module test_spatial
    use parquet
    use iso_fortran_env, only : int32, int64, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    implicit none
    private

    public :: collect_tests_parquet_spatial

    !> Seed for every fixture here. Fixed so a failure is reproducible.
    integer(int64), parameter :: fixture_seed = 20260825_int64

contains

    !> Registers every test in this suite.
    subroutine collect_tests_parquet_spatial(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("ball search matches a brute-force scan", test_ball_matches_brute_force), &
            new_unittest("a query outside the cloud finds nothing", test_ball_outside_finds_nothing), &
            new_unittest("a radius covering everything returns every row", test_ball_covers_all), &
            new_unittest("a one-point cloud and coincident points build and answer", test_ball_degenerate_clouds), &
            new_unittest("within reports the true count when the buffer is short", test_within_short_buffer), &
            new_unittest("within fills distances that match the coordinates", test_within_distances), &
            new_unittest("count_within agrees with within", test_count_within_agrees), &
            new_unittest("an int32 buffer returns the same rows as an int64 one", test_within_int32_buffer), &
            new_unittest("a 2D index matches a brute-force scan", test_2d_matches_brute_force), &
            new_unittest("a 2D index has exactly one cell along z", test_2d_grid_is_flat), &
            new_unittest("a flat, a collinear and a single-point cloud all build", test_degenerate_axes_build), &
            new_unittest("all_within reproduces N single queries", test_all_within_matches_singles), &
            new_unittest("all_within accepts one radius per point", test_all_within_per_point_radius), &
            new_unittest("count_all_within agrees with all_within", test_count_all_agrees), &
            new_unittest("pairs_within reproduces all_within with i < j", test_pairs_match_all_within), &
            new_unittest("pairs_within is symmetric under a per-point radius", &
                         test_pairs_per_point_radius_is_symmetric), &
            new_unittest("a uniform radius vector gives the scalar pair list", &
                         test_pairs_uniform_vector_matches_scalar), &
            new_unittest("segment, cylinder and cone match a brute-force scan", test_axis_matches_brute_force), &
            new_unittest("the three axis shapes accept the right points by hand", test_axis_shapes_by_hand), &
            new_unittest("a cone with equal radii is exactly the cylinder", test_cone_equal_radii_is_cylinder), &
            new_unittest("a zero-length axis is exactly the ball", test_axis_degenerate_is_a_ball), &
            new_unittest("an axis outside the cloud, and one longer than it", test_axis_outside_and_overlong), &
            new_unittest("an axis query works on a 2D index", test_axis_2d), &
            new_unittest("axis distances and a short buffer behave", test_axis_dist_and_short_buffer), &
            new_unittest("a sky index matches a haversine scan at the poles and across 0h", &
                         test_sky_matches_brute_force), &
            new_unittest("sky separations come back in degrees", test_sky_distances_are_degrees), &
            new_unittest("a sky index reports its radius in degrees", test_sky_metadata), &
            new_unittest("the sky self-join reproduces N single sky queries", test_sky_bulk_matches_singles), &
            new_unittest("sky pairs and counts agree, and pairs stay symmetric", test_sky_bulk_pairs_and_counts), &
            new_unittest("a threaded bulk sweep equals the serial one", test_bulk_threaded_matches_serial), &
            new_unittest("a periodic index matches a minimum-image scan", test_periodic_matches_brute_force), &
            new_unittest("a periodic index is translation invariant", test_periodic_translation_invariant), &
            new_unittest("a periodic 2D index matches a minimum-image scan", test_periodic_2d), &
            new_unittest("a periodic grid tiles the box exactly, whatever cell was asked for", &
                test_periodic_cells_tile_the_box), &
            new_unittest("the probe runs, and a forced cell stops it", test_probe_runs_and_can_be_stopped), &
            new_unittest("the same points give the same cell twice", test_probe_is_deterministic), &
            new_unittest("every cell size gives the same answers", test_answers_survive_any_cell), &
            new_unittest("an explicit cell is honoured", test_explicit_cell_is_honoured), &
            new_unittest("the cell count stays under the counting-path ceiling", test_cell_count_clamped), &
            new_unittest("a radius list collapses to its moment ratio", test_effective_radius_moment_ratio), &
            new_unittest("rebuild is a no-op on unchanged data and rebuilds on changed", test_rebuild_detects_change), &
            new_unittest("rebuild_for re-tunes without the caller's arrays", test_rebuild_for_retunes), &
            new_unittest("a distant query radius rebuilds, a near one does not", test_auto_rebuild), &
            new_unittest("copy=.false. answers exactly as copy=.true.", test_copy_false_matches), &
            new_unittest("the metadata queries report what was built", test_metadata_queries) &
            ]
    end subroutine collect_tests_parquet_spatial

    ! ---- Fixtures and the brute-force oracle ----

    !> A deterministic uniform cloud in `[0, side)^3`, or in `[0, side)^2` when `flat`.
    subroutine make_cloud(n, side, flat, x, y, z)
        integer(int64), intent(in) :: n !! how many points.
        real(real64), intent(in) :: side !! box side.
        logical, intent(in) :: flat !! .true. puts every point on z = 0.
        real(real64), allocatable, intent(out) :: x(:) !! x of every point.
        real(real64), allocatable, intent(out) :: y(:) !! y of every point.
        real(real64), allocatable, intent(out) :: z(:) !! z of every point.
        integer(int64) :: i

        allocate (x(n), y(n), z(n))
        do i = 1_int64, n
            x(i) = side * pf_random_at(fixture_seed, i, 1_int64)
            y(i) = side * pf_random_at(fixture_seed, i, 2_int64)
            if (flat) then
                z(i) = 0.0_real64
            else
                z(i) = side * pf_random_at(fixture_seed, i, 3_int64)
            end if
        end do
    end subroutine make_cloud

    !> Every row within `r` of `p`, found by scanning every point. The only oracle here that does
    !> not go through the grid, and so the only one a defect in the cell walk cannot satisfy.
    subroutine brute_within(x, y, z, p, r, wrap, out, m)
        real(real64), intent(in) :: x(:) !! x of every point.
        real(real64), intent(in) :: y(:) !! y of every point.
        real(real64), intent(in) :: z(:) !! z of every point.
        real(real64), intent(in) :: p(3) !! the query point.
        real(real64), intent(in) :: r !! the search radius.
        real(real64), intent(in) :: wrap(3) !! box length per periodic axis, 0 on a free one.
        integer(int64), allocatable, intent(out) :: out(:) !! the rows found, ascending.
        integer(int64), intent(out) :: m !! how many rows were found.
        integer(int64) :: i, n
        real(real64) :: dx, dy, dz

        n = size(x, kind=int64)
        allocate (out(n))
        m = 0_int64
        do i = 1_int64, n
            dx = x(i) - p(1)
            dy = y(i) - p(2)
            dz = z(i) - p(3)
            if (wrap(1) > 0.0_real64) dx = dx - wrap(1) * anint(dx / wrap(1))
            if (wrap(2) > 0.0_real64) dy = dy - wrap(2) * anint(dy / wrap(2))
            if (wrap(3) > 0.0_real64) dz = dz - wrap(3) * anint(dz / wrap(3))
            if (dx * dx + dy * dy + dz * dz <= r * r) then
                m = m + 1_int64
                out(m) = i
            end if
        end do
    end subroutine brute_within

    !> Whether two row lists hold the same rows, in any order.
    logical function same_rows(a, na, b, nb) result(same)
        integer(int64), intent(in) :: a(:) !! the first list.
        integer(int64), intent(in) :: na !! how many entries of `a` count.
        integer(int64), intent(in) :: b(:) !! the second list.
        integer(int64), intent(in) :: nb !! how many entries of `b` count.
        integer(int64), allocatable :: p(:), q(:)

        same = .false.
        if (na /= nb) return
        if (na == 0_int64) then
            same = .true.
            return
        end if
        call pf_sort(a(1:na), p)
        call pf_sort(b(1:nb), q)
        same = all(p == q)
    end function same_rows

    !> Every row inside an axis-shaped region, found by scanning every point.
    !>
    !> **This oracle shares the accept TEST with the implementation on purpose** -- that formula is
    !> the definition of the shape, and what is under test here is the cell walk: whether a
    !> per-slab traversal finds every point a full scan would. The accept test itself is checked
    !> independently by `test_axis_shapes_by_hand`, which asserts membership for points placed at
    !> known offsets from a known axis.
    subroutine brute_axis(x, y, z, p1, p2, r1, r2, clamp, out, m)
        real(real64), intent(in) :: x(:) !! x of every point.
        real(real64), intent(in) :: y(:) !! y of every point.
        real(real64), intent(in) :: z(:) !! z of every point.
        real(real64), intent(in) :: p1(3) !! one end of the axis.
        real(real64), intent(in) :: p2(3) !! the other end.
        real(real64), intent(in) :: r1 !! radius at `p1`.
        real(real64), intent(in) :: r2 !! radius at `p2`.
        logical, intent(in) :: clamp !! .true. gives round ends, .false. flat ones.
        integer(int64), allocatable, intent(out) :: out(:) !! the rows found, ascending.
        integer(int64), intent(out) :: m !! how many rows were found.
        integer(int64) :: i, n
        real(real64) :: dv(3), q(3), w(3), dd, tp, rad, d2

        n = size(x, kind=int64)
        allocate (out(n))
        m = 0_int64
        dv = p2 - p1
        dd = dot_product(dv, dv)
        do i = 1_int64, n
            q = [x(i), y(i), z(i)] - p1
            if (dd <= 0.0_real64) then
                d2 = dot_product(q, q)
                rad = max(r1, r2)
            else
                tp = dot_product(q, dv) / dd
                if (clamp) then
                    tp = min(max(tp, 0.0_real64), 1.0_real64)
                else if (tp < 0.0_real64 .or. tp > 1.0_real64) then
                    cycle
                end if
                rad = r1 + tp * (r2 - r1)
                w = q - tp * dv
                d2 = dot_product(w, w)
            end if
            if (d2 <= rad * rad) then
                m = m + 1_int64
                out(m) = i
            end if
        end do
    end subroutine brute_axis

    !> A deterministic sky catalogue that deliberately piles points where a naive implementation
    !> breaks: at both poles and straddling 0h, plus a scattered background.
    subroutine make_sky(n, ra, dec)
        integer(int64), intent(in) :: n !! how many points; a multiple of 4 is tidiest.
        real(real64), allocatable, intent(out) :: ra(:) !! right ascension, degrees, in [0, 360).
        real(real64), allocatable, intent(out) :: dec(:) !! declination, degrees, in [-90, 90].
        integer(int64) :: i, q

        allocate (ra(n), dec(n))
        do i = 1_int64, n
            q = mod(i - 1_int64, 4_int64)
            select case (q)
            case (0)
                ! North polar cap: every right ascension, all within 3 degrees of the pole.
                ra(i) = 360.0_real64 * pf_random_at(fixture_seed, i, 21_int64)
                dec(i) = 90.0_real64 - 3.0_real64 * pf_random_at(fixture_seed, i, 22_int64)
            case (1)
                ! South polar cap.
                ra(i) = 360.0_real64 * pf_random_at(fixture_seed, i, 23_int64)
                dec(i) = -90.0_real64 + 3.0_real64 * pf_random_at(fixture_seed, i, 24_int64)
            case (2)
                ! Straddling 0h: a 6-degree band centred on it, wrapped into [0, 360).
                ra(i) = modulo(357.0_real64 + 6.0_real64 * pf_random_at(fixture_seed, i, 25_int64), 360.0_real64)
                dec(i) = -3.0_real64 + 6.0_real64 * pf_random_at(fixture_seed, i, 26_int64)
            case default
                ra(i) = 360.0_real64 * pf_random_at(fixture_seed, i, 27_int64)
                dec(i) = -60.0_real64 + 120.0_real64 * pf_random_at(fixture_seed, i, 28_int64)
            end select
        end do
    end subroutine make_sky

    !> The angular separation between two sky positions, in degrees, by the HAVERSINE formula.
    !>
    !> **Deliberately not the library's route.** The implementation converts to unit vectors and
    !> compares chords; this works in `(ra, dec)` throughout, so an error in the conversion cannot
    !> hide in both. It is also the formula an astronomer would check the answer with by hand.
    real(real64) function sky_sep(ra1, dec1, ra2, dec2) result(sep)
        real(real64), intent(in) :: ra1 !! right ascension of the first point, degrees.
        real(real64), intent(in) :: dec1 !! declination of the first point, degrees.
        real(real64), intent(in) :: ra2 !! right ascension of the second point, degrees.
        real(real64), intent(in) :: dec2 !! declination of the second point, degrees.
        real(real64), parameter :: d2r = 0.017453292519943295_real64
        real(real64) :: sd, sr, h

        sd = sin(0.5_real64 * (dec2 - dec1) * d2r)
        sr = sin(0.5_real64 * (ra2 - ra1) * d2r)
        h = sd * sd + cos(dec1 * d2r) * cos(dec2 * d2r) * sr * sr
        sep = 2.0_real64 * asin(min(sqrt(max(h, 0.0_real64)), 1.0_real64)) / d2r
    end function sky_sep

    !> Every row within `rsky` degrees of `(ra0, dec0)`, by scanning the whole catalogue.
    subroutine brute_sky(ra, dec, ra0, dec0, rsky, out, m)
        real(real64), intent(in) :: ra(:) !! right ascension of every point, degrees.
        real(real64), intent(in) :: dec(:) !! declination of every point, degrees.
        real(real64), intent(in) :: ra0 !! right ascension of the query point, degrees.
        real(real64), intent(in) :: dec0 !! declination of the query point, degrees.
        real(real64), intent(in) :: rsky !! the angular search radius, degrees.
        integer(int64), allocatable, intent(out) :: out(:) !! the rows found, ascending.
        integer(int64), intent(out) :: m !! how many rows were found.
        integer(int64) :: i, n

        n = size(ra, kind=int64)
        allocate (out(n))
        m = 0_int64
        do i = 1_int64, n
            if (sky_sep(ra0, dec0, ra(i), dec(i)) <= rsky) then
                m = m + 1_int64
                out(m) = i
            end if
        end do
    end subroutine brute_sky

    ! ---- Ball search ----

    !> Ball search against the brute-force oracle, over several radii and query points.
    subroutine test_ball_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        integer :: ir
        real(real64) :: p(3), r, radii(3)

        n = 800_int64
        radii = [0.05_real64, 0.15_real64, 0.4_real64]
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=radii)
        allocate (got(n))
        do ir = 1, 3
            r = radii(ir)
            do q = 1_int64, 25_int64
                p(1) = pf_random_at(fixture_seed + 7_int64, q, 1_int64)
                p(2) = pf_random_at(fixture_seed + 7_int64, q, 2_int64)
                p(3) = pf_random_at(fixture_seed + 7_int64, q, 3_int64)
                mg = sx%within(p, r, got)
                call brute_within(x, y, z, p, r, [0.0_real64, 0.0_real64, 0.0_real64], want, mw)
                call check(error, same_rows(got, mg, want, mw), &
                    "ball search must return exactly the rows a brute-force scan finds")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_ball_matches_brute_force

    !> A query far outside the cloud finds nothing, and does not walk off the grid doing it.
    subroutine test_ball_outside_finds_nothing(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(16), m
        type(pf_spatial_index) :: sx

        call make_cloud(200_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.1_real64)
        m = sx%within([50.0_real64, 50.0_real64, 50.0_real64], 0.1_real64, got)
        call check(error, m == 0_int64, "a query far outside the cloud must find nothing")
        if (allocated(error)) return
        m = sx%within([-50.0_real64, 0.5_real64, 0.5_real64], 0.1_real64, got)
        call check(error, m == 0_int64, "a query below the grid on one axis must find nothing")
    end subroutine test_ball_outside_finds_nothing

    !> A radius that encloses the whole cloud returns every row exactly once.
    subroutine test_ball_covers_all(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:)
        integer(int64) :: n, m
        type(pf_spatial_index) :: sx

        n = 300_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.05_real64)
        allocate (got(n))
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 10.0_real64, got)
        call check(error, m == n, "a radius enclosing the cloud must return every row")
        if (allocated(error)) return
        call check(error, same_rows(got, m, [(m, m=1_int64, n)], n), &
            "a radius enclosing the cloud must return each row exactly once")
    end subroutine test_ball_covers_all

    !> A single point, and a cloud whose points are all identical, both build and answer.
    subroutine test_ball_degenerate_clouds(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64) :: one(1), many(64)
        integer(int64) :: got(64), m
        type(pf_spatial_index) :: sx

        one = 3.0_real64
        call sx%build(one, one, one, radius=1.0_real64)
        m = sx%within([3.0_real64, 3.0_real64, 3.0_real64], 0.5_real64, got)
        call check(error, m == 1_int64, "a one-point cloud must return its single row")
        if (allocated(error)) return
        m = sx%within([9.0_real64, 3.0_real64, 3.0_real64], 0.5_real64, got)
        call check(error, m == 0_int64, "a one-point cloud must return nothing for a distant query")
        if (allocated(error)) return

        many = 2.5_real64
        call sx%build(many, many, many, radius=1.0_real64)
        m = sx%within([2.5_real64, 2.5_real64, 2.5_real64], 0.1_real64, got)
        call check(error, m == 64_int64, "coincident points must all be found at their own position")
    end subroutine test_ball_degenerate_clouds

    !> A buffer shorter than the answer is filled as far as it reaches, and the TRUE count comes
    !> back so the caller can size and retry rather than guess.
    subroutine test_within_short_buffer(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: small(4), big(400), m_small, m_big
        type(pf_spatial_index) :: sx
        real(real64) :: p(3)

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.3_real64)
        p = 0.5_real64
        m_big = sx%within(p, 0.3_real64, big)
        m_small = sx%within(p, 0.3_real64, small)
        call check(error, m_small == m_big, "a short buffer must still report the true count")
        if (allocated(error)) return
        call check(error, m_big > 4_int64, "the fixture must actually overflow the short buffer")
        if (allocated(error)) return
        call check(error, same_rows(small, 4_int64, big(1:4), 4_int64), &
            "the first entries of a short buffer must be rows the full answer also holds")
    end subroutine test_within_short_buffer

    !> Every reported distance matches the coordinates of the row it belongs to.
    subroutine test_within_distances(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(400), m, k, i
        real(real64) :: d(400), p(3), want
        type(pf_spatial_index) :: sx

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.25_real64)
        p = [0.4_real64, 0.6_real64, 0.55_real64]
        m = sx%within(p, 0.25_real64, got, dist=d)
        call check(error, m > 0_int64, "the distance fixture must find some neighbours")
        if (allocated(error)) return
        do k = 1_int64, m
            i = got(k)
            want = sqrt((x(i) - p(1))**2 + (y(i) - p(2))**2 + (z(i) - p(3))**2)
            call check(error, abs(d(k) - want) <= 1.0e-12_real64, &
                "a reported distance must match the coordinates of the row it belongs to")
            if (allocated(error)) return
            call check(error, d(k) <= 0.25_real64 + 1.0e-12_real64, &
                "a reported distance must not exceed the search radius")
            if (allocated(error)) return
        end do
    end subroutine test_within_distances

    !> `%count_within` answers exactly what `%within` counts.
    subroutine test_count_within_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(500), m, c, q
        real(real64) :: p(3)
        type(pf_spatial_index) :: sx

        call make_cloud(500_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        do q = 1_int64, 20_int64
            p(1) = pf_random_at(fixture_seed + 11_int64, q, 1_int64)
            p(2) = pf_random_at(fixture_seed + 11_int64, q, 2_int64)
            p(3) = pf_random_at(fixture_seed + 11_int64, q, 3_int64)
            m = sx%within(p, 0.2_real64, got)
            c = sx%count_within(p, 0.2_real64)
            call check(error, c == m, "count_within must agree with within's count")
            if (allocated(error)) return
        end do
    end subroutine test_count_within_agrees

    !> The int32 buffer overload returns the same rows as the int64 one.
    subroutine test_within_int32_buffer(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int32) :: got32(400)
        integer(int64) :: got64(400), m32, m64
        real(real64) :: p(3)
        type(pf_spatial_index) :: sx

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.25_real64)
        p = 0.5_real64
        m64 = sx%within(p, 0.25_real64, got64)
        m32 = sx%within(p, 0.25_real64, got32)
        call check(error, m32 == m64, "the two buffer kinds must report the same count")
        if (allocated(error)) return
        call check(error, m64 > 0_int64, "the int32-buffer fixture must find some neighbours")
        if (allocated(error)) return
        call check(error, same_rows(int(got32(1:m32), kind=int64), m32, got64, m64), &
            "the two buffer kinds must report the same rows")
    end subroutine test_within_int32_buffer

    ! ---- Two dimensions ----

    !> The 2D index -- the 3D one with `z` omitted -- against the brute-force oracle.
    subroutine test_2d_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        real(real64) :: p(2), p3(3)

        n = 700_int64
        call make_cloud(n, 1.0_real64, .true., x, y, z)
        call sx%build(x, y, radius=0.12_real64)
        allocate (got(n))
        do q = 1_int64, 30_int64
            p(1) = pf_random_at(fixture_seed + 3_int64, q, 1_int64)
            p(2) = pf_random_at(fixture_seed + 3_int64, q, 2_int64)
            p3 = [p(1), p(2), 0.0_real64]
            mg = sx%within(p, 0.12_real64, got)
            call brute_within(x, y, z, p3, 0.12_real64, [0.0_real64, 0.0_real64, 0.0_real64], want, mw)
            call check(error, same_rows(got, mg, want, mw), &
                "a 2D index must return exactly the rows a 2D brute-force scan finds")
            if (allocated(error)) return
        end do
    end subroutine test_2d_matches_brute_force

    !> The grid really is flat: `nz == 1`, so the walk's k loop runs once and no cells are wasted.
    subroutine test_2d_grid_is_flat(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: nx, ny, nz

        call make_cloud(500_int64, 1.0_real64, .true., x, y, z)
        call sx%build(x, y, radius=0.1_real64)
        call sx%grid(nx, ny, nz)
        call check(error, nz == 1_int64, "a 2D index must have exactly one cell along z")
        if (allocated(error)) return
        call check(error, nx > 1_int64 .and. ny > 1_int64, &
            "a 2D index must still divide x and y into several cells")
        if (allocated(error)) return
        call check(error, sx%ndim() == 2, "a 2D index must report two dimensions")
        if (allocated(error)) return
        call check(error, sx%cells() == nx * ny, "a 2D index's cell count must be nx*ny")
    end subroutine test_2d_grid_is_flat

    !> The three degenerate shapes the provisional bucketing's cube root cannot handle on its own:
    !> a flat cloud, a collinear one and a single repeated point. Each must build and answer.
    subroutine test_degenerate_axes_build(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64) :: x(200), y(200), zz(200)
        integer(int64) :: got(200), m, i
        type(pf_spatial_index) :: sx

        do i = 1_int64, 200_int64
            x(i) = pf_random_at(fixture_seed + 5_int64, i, 1_int64)
            y(i) = pf_random_at(fixture_seed + 5_int64, i, 2_int64)
        end do
        zz = 0.0_real64
        ! Flat: a zero-extent z, which is what makes `(bbox volume / n)^(1/3)` zero.
        call sx%build(x, y, zz, radius=0.1_real64)
        m = sx%within([0.5_real64, 0.5_real64, 0.0_real64], 0.1_real64, got)
        call check(error, m > 0_int64, "a flat 3D cloud must build and find neighbours")
        if (allocated(error)) return
        ! Collinear: two zero-extent axes.
        y = 0.0_real64
        call sx%build(x, y, zz, radius=0.1_real64)
        m = sx%within([0.5_real64, 0.0_real64, 0.0_real64], 0.1_real64, got)
        call check(error, m > 0_int64, "a collinear cloud must build and find neighbours")
        if (allocated(error)) return
        ! Every axis degenerate.
        x = 1.0_real64
        call sx%build(x, y, zz, radius=0.1_real64)
        m = sx%within([1.0_real64, 0.0_real64, 0.0_real64], 0.1_real64, got)
        call check(error, m == 200_int64, "a fully coincident cloud must find every row")
        if (allocated(error)) return
        call check(error, sx%cells() == 1_int64, "a fully coincident cloud must collapse to one cell")
    end subroutine test_degenerate_axes_build

    ! ---- Bulk queries ----

    !> The CSR self-join reproduces N separate single queries, which makes its correctness
    !> independent of the bulk two-pass machinery.
    subroutine test_all_within_matches_singles(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: offs(:), nb(:), got(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, m, s0, e0
        real(real64) :: r

        n = 600_int64
        r = 0.15_real64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=r)
        call sx%all_within(r, offs, nb)
        call check(error, size(offs, kind=int64) == n + 1_int64, "the CSR offsets must be one longer than the cloud")
        if (allocated(error)) return
        call check(error, offs(1) == 1_int64, "the CSR offsets must start at 1")
        if (allocated(error)) return
        allocate (got(n))
        do i = 1_int64, n
            m = sx%within([x(i), y(i), z(i)], r, got)
            s0 = offs(i)
            e0 = offs(i + 1_int64) - 1_int64
            call check(error, same_rows(nb(s0:e0), e0 - s0 + 1_int64, got, m), &
                "each CSR row must hold exactly what a single query for that point returns")
            if (allocated(error)) return
        end do
    end subroutine test_all_within_matches_singles

    !> A per-point radius array is applied per point, not uniformly.
    subroutine test_all_within_per_point_radius(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rr(:)
        integer(int64), allocatable :: offs(:), nb(:), got(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, m
        logical :: varies

        n = 400_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rr(n))
        do i = 1_int64, n
            rr(i) = 0.05_real64 + 0.25_real64 * pf_random_at(fixture_seed + 13_int64, i, 1_int64)
        end do
        call sx%build(x, y, z, radius=rr)
        call sx%all_within(rr, offs, nb)
        allocate (got(n))
        varies = .false.
        do i = 1_int64, n
            m = sx%within([x(i), y(i), z(i)], rr(i), got)
            call check(error, same_rows(nb(offs(i):offs(i + 1_int64) - 1_int64), &
                offs(i + 1_int64) - offs(i), got, m), &
                "a per-point radius must be applied to the point it belongs to")
            if (allocated(error)) return
            if (m /= offs(2) - offs(1)) varies = .true.
        end do
        ! Without this the test would pass just as happily against an implementation that used
        ! radii(1) for every point, since row 1 would then still agree.
        call check(error, varies, "the per-point fixture must produce different counts for different rows")
    end subroutine test_all_within_per_point_radius

    !> `%count_all_within` agrees with the CSR the same sweep would build.
    subroutine test_count_all_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: offs(:), nb(:), counts(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%all_within(0.2_real64, offs, nb)
        call sx%count_all_within(0.2_real64, counts)
        call check(error, size(counts, kind=int64) == n, "count_all_within must return one count per point")
        if (allocated(error)) return
        do i = 1_int64, n
            call check(error, counts(i) == offs(i + 1_int64) - offs(i), &
                "count_all_within must agree with the CSR row length for every point")
            if (allocated(error)) return
        end do
    end subroutine test_count_all_agrees

    !> The edge list holds every neighbouring pair exactly once, with `i < j`.
    subroutine test_pairs_match_all_within(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: offs(:), nb(:), pi(:), pj(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, k, want_pairs, csr_total

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.18_real64)
        call sx%all_within(0.18_real64, offs, nb)
        call sx%pairs_within(0.18_real64, pi, pj)
        csr_total = offs(n + 1_int64) - 1_int64
        ! The CSR counts every point as its own neighbour and every pair from both ends, so the
        ! edge list must be exactly (csr_total - n) / 2 long. That is an independent count rather
        ! than a restatement of the same sweep.
        want_pairs = (csr_total - n) / 2_int64
        call check(error, size(pi, kind=int64) == want_pairs, &
            "the edge list must hold each neighbouring pair exactly once")
        if (allocated(error)) return
        call check(error, want_pairs > 0_int64, "the pair fixture must actually find some pairs")
        if (allocated(error)) return
        call check(error, all(pi < pj), "every reported pair must have i < j")
        if (allocated(error)) return
        do k = 1_int64, min(want_pairs, 200_int64)
            i = pi(k)
            call check(error, any(nb(offs(i):offs(i + 1_int64) - 1_int64) == pj(k)), &
                "every reported pair must appear in the CSR row of its lower endpoint")
            if (allocated(error)) return
        end do
    end subroutine test_pairs_match_all_within

    !> With one radius per point, the edge list is every pair within `max(r_i, r_j)` -- exactly.
    !>
    !> **This is the test that pins the symmetric rule.** A per-point radius makes "who was doing
    !> the searching" observable: emitting from the lower row with its own radius drops every pair
    !> that only the larger ball reaches, and the result still looks like a perfectly ordinary
    !> neighbour list. `want_low` counts what that rule would have produced and the fixture is
    !> asserted to separate the two, so this cannot pass against a sweep that ignores the ranking.
    subroutine test_pairs_per_point_radius_is_symmetric(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rv(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :), got(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, b, k, want_max, want_low
        real(real64) :: d2, rmax, rlow

        n = 300_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        do a = 1_int64, n
            rv(a) = 0.03_real64 + 0.22_real64 * pf_random_at(fixture_seed, a, 11_int64)
        end do
        allocate (want(n, n), got(n, n))
        want = .false.
        got = .false.
        want_max = 0_int64
        want_low = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                d2 = (x(a) - x(b))**2 + (y(a) - y(b))**2 + (z(a) - z(b))**2
                rmax = max(rv(a), rv(b))
                if (d2 <= rmax * rmax) then
                    want(a, b) = .true.
                    want_max = want_max + 1_int64
                end if
                ! The rule a sweep applies when the LOWER row always does the searching with its
                ! own radius: the negative control for everything below.
                rlow = rv(a)
                if (d2 <= rlow * rlow) want_low = want_low + 1_int64
            end do
        end do
        call check(error, want_max > want_low, &
            "the fixture must separate the symmetric rule from the lower-row-searches rule")
        if (allocated(error)) return
        call sx%build(x, y, z, radius=rv)
        call sx%pairs_within(rv, pi, pj)
        call check(error, size(pi, kind=int64) == want_max, &
            "the edge list must hold every pair within max(r_i, r_j), and no others")
        if (allocated(error)) return
        call check(error, all(pi < pj), "every reported pair must have i < j")
        if (allocated(error)) return
        do k = 1_int64, size(pi, kind=int64)
            got(pi(k), pj(k)) = .true.
        end do
        ! Set equality, not just a count: a count alone cannot tell a duplicated pair plus a
        ! missing one from the right answer.
        call check(error, all(got .eqv. want), &
            "the edge list must be exactly the pairs within max(r_i, r_j)")
    end subroutine test_pairs_per_point_radius_is_symmetric

    !> A radius vector whose entries are all equal gives the scalar form's answer, pair for pair.
    !>
    !> The ranking machinery only runs on the vector path, so without this a defect in it that
    !> happens to preserve the pair COUNT would show up nowhere: the symmetric test above varies
    !> the radii, and every other pair test takes the scalar path and never builds a rank at all.
    subroutine test_pairs_uniform_vector_matches_scalar(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rv(:)
        integer(int64), allocatable :: si(:), sj(:), vi(:), vj(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n
        real(real64), parameter :: r = 0.18_real64

        n = 400_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        rv = r
        call sx%build(x, y, z, radius=r)
        call sx%pairs_within(r, si, sj)
        call sx%pairs_within(rv, vi, vj)
        call check(error, size(si, kind=int64) > 0_int64, "the fixture must actually find some pairs")
        if (allocated(error)) return
        call check(error, size(vi, kind=int64) == size(si, kind=int64), &
            "a uniform radius vector must find the same number of pairs as the scalar form")
        if (allocated(error)) return
        call check(error, all(vi == si) .and. all(vj == sj), &
            "a uniform radius vector must give the scalar form's pair list, pair for pair")
    end subroutine test_pairs_uniform_vector_matches_scalar

    ! ---- Segment, cylinder and cone ----

    !> All three axis shapes agree with a full scan, over axes in every orientation that matters.
    !>
    !> **The orientations are the point.** The walk picks the axis the segment travels furthest
    !> along and slabs across it, so which branch runs depends on the direction -- an axis parallel
    !> to x, to y and to z each takes a different one, and a diagonal is the case a bounding-box
    !> walk would degenerate on. All four are here, plus a steep cone whose narrow end must not
    !> walk the wide end's cells.
    subroutine test_axis_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw
        integer :: c
        real(real64) :: p1(3), p2(3), r1, r2
        logical :: clamp

        n = 4000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.12_real64)
        allocate (got(n))
        do c = 1, 15
            ! Five axes x three shapes: along x, along y, along z, a body diagonal, and a short
            ! off-centre one. The shape cycles capsule / cylinder / cone.
            select case (mod(c - 1, 5) + 1)
            case (1)
                p1 = [0.1_real64, 0.5_real64, 0.5_real64]
                p2 = [0.9_real64, 0.5_real64, 0.5_real64]
            case (2)
                p1 = [0.5_real64, 0.1_real64, 0.5_real64]
                p2 = [0.5_real64, 0.9_real64, 0.5_real64]
            case (3)
                p1 = [0.5_real64, 0.5_real64, 0.1_real64]
                p2 = [0.5_real64, 0.5_real64, 0.9_real64]
            case (4)
                p1 = [0.05_real64, 0.05_real64, 0.05_real64]
                p2 = [0.95_real64, 0.95_real64, 0.95_real64]
            case default
                p1 = [0.3_real64, 0.7_real64, 0.2_real64]
                p2 = [0.45_real64, 0.6_real64, 0.35_real64]
            end select
            r1 = 0.12_real64
            r2 = 0.12_real64
            clamp = .false.
            select case ((c - 1) / 5)
            case (0)
                clamp = .true.
                mg = sx%within_segment(p1, p2, r1, got)
            case (1)
                mg = sx%within_cylinder(p1, p2, r1, got)
            case default
                r1 = 0.02_real64
                r2 = 0.22_real64
                mg = sx%within_cone(p1, p2, r1, r2, got)
            end select
            call brute_axis(x, y, z, p1, p2, r1, r2, clamp, want, mw)
            call check(error, mw > 0_int64, "every axis fixture must find some points")
            if (allocated(error)) return
            call check(error, same_rows(got, mg, want, mw), &
                "an axis-shaped query must return exactly what a full scan returns")
            if (allocated(error)) return
        end do
    end subroutine test_axis_matches_brute_force

    !> The three shapes accept the points they should, at offsets worked out by hand.
    !>
    !> **The independent check on the accept test**, which `brute_axis` deliberately shares with
    !> the implementation. Everything here is placed relative to the unit axis along x, so the
    !> expected answer is arithmetic a reader can redo: a point beyond an end is INSIDE the capsule
    !> and OUTSIDE the cylinder, which is the entire difference between them, and the cone's radius
    !> at parameter `t` is `r1 + t*(r2 - r1)`.
    subroutine test_axis_shapes_by_hand(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64) :: x(7), y(7), z(7)
        integer(int64) :: got(7), m
        type(pf_spatial_index) :: sx
        real(real64), parameter :: p1(3) = [0.0_real64, 0.0_real64, 0.0_real64]
        real(real64), parameter :: p2(3) = [1.0_real64, 0.0_real64, 0.0_real64]

        ! The axis runs from (0,0,0) to (1,0,0); the capsule and cylinder use r = 0.1 and the cone
        ! runs from 0.05 to 0.2, so its radius at parameter t is 0.05 + 0.15*t. Every offset below
        ! clears the nearest threshold by at least 4%, so nothing here rests on an exact tie.
        !
        !  1: t = 0.5, 0.05 out  -- in all three (cone radius there is 0.125)
        !  2: t = 0.5, 0.12 out  -- outside r = 0.1, inside the cone's 0.125
        !  3: 0.05 BEFORE the near end -- capsule yes; cylinder and cone reject t < 0
        !  4: 0.05 BEYOND the far end  -- capsule yes; cylinder and cone reject t > 1
        !  5: 0.2 before the near end  -- outside everything
        !  6: t = 0.1, 0.12 out  -- cone radius there is 0.065, so outside the cone too
        !  7: t = 0.9, 0.12 out  -- cone radius there is 0.185, so inside the cone only
        x = [0.5_real64, 0.5_real64, -0.05_real64, 1.05_real64, -0.2_real64, 0.1_real64, 0.9_real64]
        y = [0.05_real64, 0.12_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.12_real64, 0.12_real64]
        z = 0.0_real64
        call sx%build(x, y, z, radius=0.1_real64)

        m = sx%within_segment(p1, p2, 0.1_real64, got)
        call check(error, same_rows(got, m, [1_int64, 3_int64, 4_int64], 3_int64), &
            "the capsule must take the mid-axis point and both points just beyond the ends")
        if (allocated(error)) return

        m = sx%within_cylinder(p1, p2, 0.1_real64, got)
        call check(error, same_rows(got, m, [1_int64], 1_int64), &
            "the cylinder must reject both points beyond its flat ends")
        if (allocated(error)) return

        m = sx%within_cone(p1, p2, 0.05_real64, 0.2_real64, got)
        call check(error, same_rows(got, m, [1_int64, 2_int64, 7_int64], 3_int64), &
            "the cone must accept by its own radius at each point's parameter along the axis")
    end subroutine test_axis_shapes_by_hand

    !> `%within_cone` with `r1 == r2` is `%within_cylinder`, row for row.
    !>
    !> The cone is the general routine and the cylinder is it restricted, so this asserts the
    !> restriction rather than re-testing the walk: a defect in the radius interpolation that
    !> happened to preserve the count would still show up here.
    subroutine test_cone_equal_radii_is_cylinder(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: a(:), b(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, ma, mb
        real(real64), parameter :: p1(3) = [0.15_real64, 0.2_real64, 0.8_real64]
        real(real64), parameter :: p2(3) = [0.85_real64, 0.7_real64, 0.1_real64]

        n = 3000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.13_real64)
        allocate (a(n), b(n))
        ma = sx%within_cylinder(p1, p2, 0.13_real64, a)
        mb = sx%within_cone(p1, p2, 0.13_real64, 0.13_real64, b)
        call check(error, ma > 0_int64, "the fixture must find some points")
        if (allocated(error)) return
        call check(error, mb == ma, "a cone with equal radii must find as many points as the cylinder")
        if (allocated(error)) return
        call check(error, all(a(1:ma) == b(1:mb)), &
            "a cone with equal radii must return the cylinder's rows in the same order")
    end subroutine test_cone_equal_radii_is_cylinder

    !> A zero-length axis reduces to a ball, in all three shapes, exactly as `%within` reports it.
    !>
    !> A documented reduction rather than an accident, so it is asserted rather than assumed -- and
    !> against `%within`, which reaches the answer by a completely different walk.
    subroutine test_axis_degenerate_is_a_ball(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: ball(:), seg(:), cyl(:), cone(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mb, ms, mc, mk
        real(real64), parameter :: p(3) = [0.4_real64, 0.55_real64, 0.6_real64]

        n = 3000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64)
        allocate (ball(n), seg(n), cyl(n), cone(n))
        mb = sx%within(p, 0.15_real64, ball)
        ms = sx%within_segment(p, p, 0.15_real64, seg)
        mc = sx%within_cylinder(p, p, 0.15_real64, cyl)
        ! The cone takes the LARGER of its two radii when the axis has no length.
        mk = sx%within_cone(p, p, 0.05_real64, 0.15_real64, cone)
        call check(error, mb > 0_int64, "the fixture must find some points")
        if (allocated(error)) return
        call check(error, same_rows(seg, ms, ball, mb), &
            "a zero-length segment must return exactly the ball's rows")
        if (allocated(error)) return
        call check(error, same_rows(cyl, mc, ball, mb), &
            "a zero-length cylinder must return exactly the ball's rows")
        if (allocated(error)) return
        call check(error, same_rows(cone, mk, ball, mb), &
            "a zero-length cone must return the ball of its larger radius")
    end subroutine test_axis_degenerate_is_a_ball

    !> An axis that misses the cloud finds nothing; one far longer than the box finds the same
    !> points as one that merely spans it.
    !>
    !> The overlong case is what a bounding-box walk gets wrong: the region's AABB is then the
    !> whole grid, so the walk degenerates to a full scan exactly where the shape is most
    !> selective. Answers cannot see that, which is why the cheap cell-count check is here too.
    subroutine test_axis_outside_and_overlong(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:), long(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, mw, ml

        n = 3000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.1_real64)
        allocate (got(n), long(n))
        ! Wholly outside, and parallel to the cloud rather than pointing away from it.
        m = sx%within_segment([-5.0_real64, -5.0_real64, -5.0_real64], &
                              [-5.0_real64, -5.0_real64, 5.0_real64], 0.1_real64, got)
        call check(error, m == 0_int64, "an axis that misses the cloud entirely must find nothing")
        if (allocated(error)) return
        ! Spanning the cloud, then the same line extended far past it in both directions.
        m = sx%within_cylinder([0.0_real64, 0.5_real64, 0.5_real64], &
                               [1.0_real64, 0.5_real64, 0.5_real64], 0.1_real64, got)
        ml = sx%within_cylinder([-50.0_real64, 0.5_real64, 0.5_real64], &
                                [51.0_real64, 0.5_real64, 0.5_real64], 0.1_real64, long)
        call brute_axis(x, y, z, [-50.0_real64, 0.5_real64, 0.5_real64], &
                        [51.0_real64, 0.5_real64, 0.5_real64], 0.1_real64, 0.1_real64, .false., want, mw)
        call check(error, m > 0_int64, "the spanning cylinder must find some points")
        if (allocated(error)) return
        call check(error, same_rows(long, ml, want, mw), &
            "an axis far longer than the box must still agree with a full scan")
        if (allocated(error)) return
        call check(error, same_rows(long, ml, got, m), &
            "extending an axis past the cloud must not change which points it holds")
    end subroutine test_axis_outside_and_overlong

    !> A 2D index answers an axis query, with the third coordinate never entering it.
    subroutine test_axis_2d(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, mw

        n = 2000_int64
        call make_cloud(n, 1.0_real64, .true., x, y, z)
        call sx%build(x, y, radius=0.12_real64)
        allocate (got(n))
        m = sx%within_segment([0.1_real64, 0.2_real64], [0.9_real64, 0.8_real64], 0.12_real64, got)
        ! `z` is all zeros from `make_cloud`, so the 3D oracle answers the 2D question unchanged.
        call brute_axis(x, y, z, [0.1_real64, 0.2_real64, 0.0_real64], &
                        [0.9_real64, 0.8_real64, 0.0_real64], 0.12_real64, 0.12_real64, .true., want, mw)
        call check(error, mw > 0_int64, "the 2D axis fixture must find some points")
        if (allocated(error)) return
        call check(error, same_rows(got, m, want, mw), &
            "a 2D axis query must return exactly what a full scan returns")
    end subroutine test_axis_2d

    !> `dist` reports the distance to the axis, and a short buffer still reports the true count.
    subroutine test_axis_dist_and_short_buffer(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), dd(:)
        integer(int64), allocatable :: got(:)
        integer(int32), allocatable :: got32(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, mshort, k
        real(real64) :: want
        real(real64), parameter :: p1(3) = [0.1_real64, 0.5_real64, 0.5_real64]
        real(real64), parameter :: p2(3) = [0.9_real64, 0.5_real64, 0.5_real64]

        n = 2000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64)
        allocate (got(n), dd(n), got32(n))
        m = sx%within_cylinder(p1, p2, 0.15_real64, got, dist=dd)
        call check(error, m > 0_int64, "the fixture must find some points")
        if (allocated(error)) return
        do k = 1_int64, m
            ! The axis is parallel to x here, so the distance to it is the distance in (y, z) --
            ! computed without reference to the implementation's projection.
            want = sqrt((y(got(k)) - 0.5_real64)**2 + (z(got(k)) - 0.5_real64)**2)
            call check(error, abs(dd(k) - want) < 1.0e-12_real64, &
                "dist must report the perpendicular distance to the axis")
            if (allocated(error)) return
        end do
        ! An int32 buffer finds the same rows, and a short one still reports the true count.
        mshort = sx%within_cylinder(p1, p2, 0.15_real64, got32(1:2))
        call check(error, mshort == m, &
            "a short buffer must still report the true count so a caller can size and retry")
        if (allocated(error)) return
        mshort = sx%within_cylinder(p1, p2, 0.15_real64, got32)
        call check(error, mshort == m .and. all(int(got32(1:m), kind=int64) == got(1:m)), &
            "an int32 buffer must return the same rows as an int64 one")
    end subroutine test_axis_dist_and_short_buffer

    ! ---- The sky metric ----

    !> A sky index agrees with a haversine scan, at both poles and across 0h.
    !>
    !> **Those two regions are the whole reason the conversion exists.** A query written directly
    !> in `(ra, dec)` -- comparing right ascensions as if they were a Cartesian coordinate -- passes
    !> perfectly well in the middle of the sky and fails at exactly these two places, so a fixture
    !> that avoids them proves nothing. The assertions below therefore also check that the answers
    !> SPAN the discontinuity: a 0h query must return points on both sides of it, and a polar query
    !> must return points whose right ascensions are far apart.
    subroutine test_sky_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, mw, k, lowside, highside
        integer :: c
        real(real64) :: ra0, dec0, rq, lo_ra, hi_ra

        n = 6000_int64
        call make_sky(n, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.5_real64)
        allocate (got(n))
        do c = 1, 6
            rq = 1.5_real64
            select case (c)
            case (1)
                ra0 = 0.5_real64          ! just east of 0h
                dec0 = 0.0_real64
            case (2)
                ra0 = 359.5_real64        ! just west of 0h
                dec0 = 0.0_real64
            case (3)
                ra0 = 0.0_real64          ! the north pole itself
                dec0 = 90.0_real64
            case (4)
                ra0 = 217.0_real64        ! the south pole, approached from a different meridian
                dec0 = -90.0_real64
            case (5)
                ra0 = 123.4_real64        ! an ordinary place, as a control
                dec0 = 45.6_real64
                ! The scattered background is thin -- a quarter of the catalogue over the whole
                ! sky -- so a 1.5-degree circle there is usually empty. A wider one keeps the
                ! control meaningful without leaving the band the index was tuned for.
                rq = 10.0_real64
            case default
                ra0 = 88.0_real64
                dec0 = 88.5_real64        ! near, but not at, the pole
            end select
            m = sx%within_sky(ra0, dec0, rq, got)
            call brute_sky(ra, dec, ra0, dec0, rq, want, mw)
            call check(error, mw > 0_int64, "every sky fixture must find some points")
            if (allocated(error)) return
            call check(error, same_rows(got, m, want, mw), &
                "a sky query must return exactly what a haversine scan returns")
            if (allocated(error)) return
        end do

        ! The 0h query must straddle the seam, or it is not testing the seam.
        m = sx%within_sky(0.0_real64, 0.0_real64, 1.5_real64, got)
        lowside = 0_int64
        highside = 0_int64
        do k = 1_int64, m
            if (ra(got(k)) < 180.0_real64) lowside = lowside + 1_int64
            if (ra(got(k)) >= 180.0_real64) highside = highside + 1_int64
        end do
        call check(error, lowside > 0_int64 .and. highside > 0_int64, &
            "a query at 0h must find points on both sides of the wrap, or the fixture tests nothing")
        if (allocated(error)) return

        ! The polar query must gather points from right ascensions far apart, which is the other
        ! case a naive (ra, dec) comparison gets wrong.
        m = sx%within_sky(0.0_real64, 90.0_real64, 2.0_real64, got)
        call check(error, m > 0_int64, "the polar query must find points")
        if (allocated(error)) return
        lo_ra = minval(ra(got(1:m)))
        hi_ra = maxval(ra(got(1:m)))
        call check(error, hi_ra - lo_ra > 180.0_real64, &
            "a query at the pole must reach every meridian, or the fixture tests nothing")
    end subroutine test_sky_matches_brute_force

    !> `dist_deg` is an angular separation in degrees, never a chord.
    subroutine test_sky_distances_are_degrees(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), dd(:)
        integer(int64), allocatable :: got(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, k
        real(real64) :: want
        real(real64), parameter :: ra0 = 12.0_real64, dec0 = -30.0_real64, rsky = 2.0_real64

        n = 3000_int64
        call make_sky(n, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=rsky)
        allocate (got(n), dd(n))
        m = sx%within_sky(ra0, dec0, rsky, got, dist_deg=dd)
        call check(error, m > 0_int64, "the fixture must find some points")
        if (allocated(error)) return
        do k = 1_int64, m
            want = sky_sep(ra0, dec0, ra(got(k)), dec(got(k)))
            call check(error, abs(dd(k) - want) < 1.0e-9_real64, &
                "dist_deg must equal the haversine separation in degrees")
            if (allocated(error)) return
            ! A chord would be numerically smaller than the angle in degrees for any radius this
            ! size, so this also catches a conversion that was simply left out.
            call check(error, dd(k) <= rsky + 1.0e-9_real64, &
                "every reported separation must be within the requested angular radius")
            if (allocated(error)) return
        end do
    end subroutine test_sky_distances_are_degrees

    !> A sky index reports its metric, and its effective radius back in degrees.
    subroutine test_sky_metadata(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: sx, eu
        real(real64), allocatable :: x(:), y(:), z(:)

        call make_sky(1000_int64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.25_real64)
        call check(error, sx%metric() == PF_METRIC_SKY, "%build_sky must set the sky metric")
        if (allocated(error)) return
        call check(error, sx%ndim() == 3, "a sky index is a 3D index over unit vectors")
        if (allocated(error)) return
        call check(error, .not. sx%is_periodic(), "a sky index has no periodic box")
        if (allocated(error)) return
        ! One radius, so the moment ratio is that radius -- and it must come back in the units it
        ! was given, not as the chord the index actually tunes on.
        call check(error, abs(sx%effective_radius() - 1.25_real64) < 1.0e-9_real64, &
            "%effective_radius must report degrees on a sky index")
        if (allocated(error)) return
        call check(error, sx%cell_size() > 0.0_real64 .and. sx%cell_size() < 2.0_real64, &
            "%cell_size stays in unit-vector space, so it cannot exceed the sphere's diameter")
        if (allocated(error)) return
        call make_cloud(500_int64, 1.0_real64, .false., x, y, z)
        call eu%build(x, y, z, radius=0.2_real64)
        call check(error, eu%metric() == PF_METRIC_EUCLIDEAN, "%build must leave the Euclidean metric")
    end subroutine test_sky_metadata

    !> `%all_within_sky` is exactly a loop of `%within_sky`, catalogue-wide.
    !>
    !> The equality test the `spatial_sky_bulk_refused` scenario asked for when the bulk sky forms
    !> landed. It runs over the same pole-and-0h fixture as the single-query test, so the bulk path
    !> is checked at the two places the conversion exists for rather than only in open sky.
    subroutine test_sky_bulk_matches_singles(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64), allocatable :: offs(:), nb(:), one(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, m, lo, hi, total
        real(real64), parameter :: rq = 1.5_real64

        n = 3000_int64
        call make_sky(n, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=rq)
        call sx%all_within_sky(rq, offs, nb)
        call check(error, size(offs, kind=int64) == n + 1_int64, &
            "the CSR offsets must have one entry per point plus a sentinel")
        if (allocated(error)) return
        total = offs(n + 1_int64) - 1_int64
        call check(error, total >= n, "every point is its own neighbour, so the CSR cannot be shorter than n")
        if (allocated(error)) return
        allocate (one(n))
        do i = 1_int64, n
            m = sx%within_sky(ra(i), dec(i), rq, one)
            lo = offs(i)
            hi = offs(i + 1_int64) - 1_int64
            call check(error, same_rows(nb(lo:hi), hi - lo + 1_int64, one, m), &
                "each CSR row must hold exactly what a single sky query at that point returns")
            if (allocated(error)) return
        end do
    end subroutine test_sky_bulk_matches_singles

    !> The sky pair list and counts agree with the CSR, and pairs stay symmetric in degrees.
    !>
    !> **The per-point half is the one worth having.** The chord is strictly increasing in the
    !> angle, so ranking by descending chord is ranking by descending angle -- but that is an
    !> argument, and this asserts it: the edge list is compared against an O(n^2) haversine scan
    !> under `separation <= max(deg_i, deg_j)`, with a count of what the lower-row-searches rule
    !> would have produced beside it so the fixture is known to separate the two.
    subroutine test_sky_bulk_pairs_and_counts(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), rv(:)
        integer(int64), allocatable :: offs(:), nb(:), pi(:), pj(:), counts(:)
        logical, allocatable :: want(:, :), got(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, a, b, k, csr_total, want_max, want_low
        real(real64) :: sep
        real(real64), parameter :: rq = 1.5_real64

        n = 800_int64
        call make_sky(n, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=rq)

        ! Scalar radius: counts, CSR and the pair list must be three views of one answer.
        call sx%all_within_sky(rq, offs, nb)
        call sx%count_all_within_sky(rq, counts)
        call sx%pairs_within_sky(rq, pi, pj)
        do i = 1_int64, n
            call check(error, counts(i) == offs(i + 1_int64) - offs(i), &
                "count_all_within_sky must agree with the CSR row length for every point")
            if (allocated(error)) return
        end do
        csr_total = offs(n + 1_int64) - 1_int64
        call check(error, size(pi, kind=int64) == (csr_total - n) / 2_int64, &
            "the sky edge list must hold each neighbouring pair exactly once")
        if (allocated(error)) return
        call check(error, all(pi < pj), "every reported sky pair must have i < j")
        if (allocated(error)) return

        ! Per-point radii, in degrees, against an independent haversine oracle.
        allocate (rv(n))
        do i = 1_int64, n
            rv(i) = 0.3_real64 + 2.2_real64 * pf_random_at(fixture_seed, i, 31_int64)
        end do
        allocate (want(n, n), got(n, n))
        want = .false.
        got = .false.
        want_max = 0_int64
        want_low = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                sep = sky_sep(ra(a), dec(a), ra(b), dec(b))
                if (sep <= max(rv(a), rv(b))) then
                    want(a, b) = .true.
                    want_max = want_max + 1_int64
                end if
                if (sep <= rv(a)) want_low = want_low + 1_int64
            end do
        end do
        call check(error, want_max > want_low, &
            "the fixture must separate the symmetric rule from the lower-row-searches rule")
        if (allocated(error)) return
        call sx%pairs_within_sky(rv, pi, pj)
        call check(error, size(pi, kind=int64) == want_max, &
            "the sky edge list must hold every pair within max(deg_i, deg_j), and no others")
        if (allocated(error)) return
        do k = 1_int64, size(pi, kind=int64)
            got(pi(k), pj(k)) = .true.
        end do
        call check(error, all(got .eqv. want), &
            "the sky edge list must be exactly the pairs within max(deg_i, deg_j)")
    end subroutine test_sky_bulk_pairs_and_counts

    !> A threaded bulk sweep answers exactly as the serial one.
    !>
    !> **Skipped without OpenMP rather than passed**: both arms would run the same serial code, so
    !> the equality would hold for the wrong reason and the test would report a green that means
    !> nothing. The negative control is `parquet_debug_spatial_threads_used`, which is what
    !> separates "the threaded arm agreed" from "no team was ever opened".
    subroutine test_bulk_threaded_matches_serial(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: o1(:), n1(:), o2(:), n2(:)
        type(pf_spatial_index) :: sx
        integer :: used1, used4
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without a team both arms run the same serial sweep, so " // &
            "the equality below would hold for the wrong reason")
        return
#endif
        call make_cloud(2000_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.1_real64)
        call sx%all_within(0.1_real64, o1, n1, threads=1)
        used1 = parquet_debug_spatial_threads_used()
        call sx%all_within(0.1_real64, o2, n2, threads=4)
        used4 = parquet_debug_spatial_threads_used()
        if (used4 < 2) then
            call skip_test(error, "needs at least two processors: the affinity clamp resolved the " // &
                "four-thread arm to a team of one, so both arms ran serially")
            return
        end if
        call check(error, used1 == 1, "an explicit threads=1 must resolve to a team of one")
        if (allocated(error)) return
        call check(error, all(o1 == o2), "a threaded sweep must produce the same CSR offsets as a serial one")
        if (allocated(error)) return
        call check(error, size(n1, kind=int64) == size(n2, kind=int64), &
            "a threaded sweep must produce the same number of neighbours as a serial one")
        if (allocated(error)) return
        call check(error, all(n1 == n2), "a threaded sweep must produce the same neighbours as a serial one")
    end subroutine test_bulk_threaded_matches_serial

    ! ---- Periodic boundaries ----

    !> A periodic index against a brute-force scan that applies the minimum image itself.
    subroutine test_periodic_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        real(real64) :: p(3), side, r, wrap(3)

        n = 700_int64
        side = 4.0_real64
        r = 0.7_real64
        wrap = side
        call make_cloud(n, side, .false., x, y, z)
        call sx%build(x, y, z, radius=r, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
            box_hi=[side, side, side])
        call check(error, sx%is_periodic(), "an index given a box must report itself periodic")
        if (allocated(error)) return
        allocate (got(n))
        do q = 1_int64, 40_int64
            ! Query points deliberately ON the faces and corners, where a non-wrapping walk gives
            ! the right answer for the interior and the wrong one here.
            p(1) = side * real(mod(q, 4_int64), kind=real64) / 4.0_real64
            p(2) = side * pf_random_at(fixture_seed + 17_int64, q, 1_int64)
            p(3) = side * pf_random_at(fixture_seed + 17_int64, q, 2_int64)
            mg = sx%within(p, r, got)
            call brute_within(x, y, z, p, r, wrap, want, mw)
            call check(error, same_rows(got, mg, want, mw), &
                "a periodic query must return exactly the rows a minimum-image scan finds")
            if (allocated(error)) return
        end do
    end subroutine test_periodic_matches_brute_force

    !> Translation invariance: shifting every point by an arbitrary vector modulo the box must
    !> leave every neighbour set identical.
    !>
    !> This is what catches a grid whose cells do not tile the box exactly -- the seam cell would
    !> be a different width, and every query crossing it wrong by a fraction of a cell. A
    !> face-crossing spot check passes against that; this does not.
    subroutine test_periodic_translation_invariant(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), x2(:), y2(:), z2(:)
        integer(int64), allocatable :: o1(:), n1(:), o2(:), n2(:)
        type(pf_spatial_index) :: a, b
        integer(int64) :: n, i
        real(real64) :: side, r, sh(3)

        n = 900_int64
        side = 3.0_real64
        r = 0.45_real64
        ! A shift that is not a whole number of cells, on purpose.
        sh = [1.2345_real64, 0.6789_real64, 2.0101_real64]
        call make_cloud(n, side, .false., x, y, z)
        allocate (x2(n), y2(n), z2(n))
        do i = 1_int64, n
            x2(i) = modulo(x(i) + sh(1), side)
            y2(i) = modulo(y(i) + sh(2), side)
            z2(i) = modulo(z(i) + sh(3), side)
        end do
        call a%build(x, y, z, radius=r, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], box_hi=[side, side, side])
        call b%build(x2, y2, z2, radius=r, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], box_hi=[side, side, side])
        call a%all_within(r, o1, n1)
        call b%all_within(r, o2, n2)
        call check(error, all(o1 == o2), &
            "translating every point inside a periodic box must not change any neighbour COUNT")
        if (allocated(error)) return
        do i = 1_int64, n
            call check(error, same_rows(n1(o1(i):o1(i + 1_int64) - 1_int64), o1(i + 1_int64) - o1(i), &
                n2(o2(i):o2(i + 1_int64) - 1_int64), o2(i + 1_int64) - o2(i)), &
                "translating every point inside a periodic box must not change any neighbour SET")
            if (allocated(error)) return
        end do
    end subroutine test_periodic_translation_invariant

    !> A periodic 2D index: `box_lo`/`box_hi` of rank 2, with z left free.
    subroutine test_periodic_2d(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        real(real64) :: p(2), p3(3), side, r, wrap(3)

        n = 600_int64
        side = 2.0_real64
        r = 0.35_real64
        wrap = [side, side, 0.0_real64]
        call make_cloud(n, side, .true., x, y, z)
        call sx%build(x, y, radius=r, box_lo=[0.0_real64, 0.0_real64], box_hi=[side, side])
        allocate (got(n))
        do q = 1_int64, 25_int64
            p(1) = side * pf_random_at(fixture_seed + 23_int64, q, 1_int64)
            p(2) = side * pf_random_at(fixture_seed + 23_int64, q, 2_int64)
            p3 = [p(1), p(2), 0.0_real64]
            mg = sx%within(p, r, got)
            call brute_within(x, y, z, p3, r, wrap, want, mw)
            call check(error, same_rows(got, mg, want, mw), &
                "a periodic 2D query must return exactly the rows a minimum-image scan finds")
            if (allocated(error)) return
        end do
    end subroutine test_periodic_2d

    !> A periodic grid's cells must tile the box EXACTLY, whatever cell size was asked for.
    !>
    !> **This is asserted directly rather than through an answer**, because the answer is a poor
    !> detector of it: a grid that fails to tile folds its last partial slab into cell 0, which the
    !> wrapped walk still visits, so the exact distance test filters the strays and the query comes
    !> back right. A mutation replacing `L/nx` with the requested `h` was confirmed to survive every
    !> answer-based periodic test here, including translation invariance. The property is real -- the
    !> seam cell is a different width from every other -- and only its ARITHMETIC can see it, so the
    !> assertion is `nx * cell_x == L` to the last bit.
    subroutine test_periodic_cells_tile_the_box(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: nx, ny, nz
        real(real64) :: cx, cy, cz, box(3)
        integer :: k
        real(real64), parameter :: asked(4) = [0.7_real64, 0.31_real64, 1.1_real64, 0.4999_real64]

        box = [3.0_real64, 2.0_real64, 5.0_real64]
        call make_cloud(800_int64, 3.0_real64, .false., x, y, z)
        z = z * box(3) / 3.0_real64
        y = y * box(2) / 3.0_real64
        do k = 1, size(asked)
            ! Deliberately cells that divide NONE of the three sides, and a box that is not cubic
            ! so the three axes must each get their own answer.
            call sx%build(x, y, z, radius=0.25_real64, cell=asked(k), &
                box_lo=[0.0_real64, 0.0_real64, 0.0_real64], box_hi=box)
            call sx%grid(nx, ny, nz)
            call sx%cell_sides(cx, cy, cz)
            call check(error, real(nx, kind=real64) * cx == box(1), &
                "a periodic grid's x cells must tile the box exactly")
            if (allocated(error)) return
            call check(error, real(ny, kind=real64) * cy == box(2), &
                "a periodic grid's y cells must tile the box exactly")
            if (allocated(error)) return
            call check(error, real(nz, kind=real64) * cz == box(3), &
                "a periodic grid's z cells must tile the box exactly")
            if (allocated(error)) return
            ! And the side must be at least what was asked for -- rounding the cell count DOWN is
            ! what keeps the reach a query computes from under-covering.
            call check(error, cx >= asked(k) .and. cy >= asked(k) .and. cz >= asked(k), &
                "tiling must adjust the cell side UP, never down")
            if (allocated(error)) return
        end do
        ! Negative control: a non-periodic index must NOT adjust the side, or `cell=` would stop
        ! meaning what it says on the path where nothing has to tile.
        call sx%build(x, y, z, radius=0.25_real64, cell=0.7_real64)
        call sx%cell_sides(cx, cy, cz)
        call check(error, cx == 0.7_real64 .and. cy == 0.7_real64 .and. cz == 0.7_real64, &
            "a non-periodic index must use the cell side it was given, on every axis")
    end subroutine test_periodic_cells_tile_the_box

    ! ---- The cell-size tuner ----

    !> The probe runs on an ordinary build, and a forced cell stops it.
    !>
    !> The second half is the negative control: without it this would pass just as happily against
    !> a counter that is incremented unconditionally.
    subroutine test_probe_runs_and_can_be_stopped(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: ncand

        call parquet_debug_reset_spatial_counters()
        call make_cloud(4000_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.08_real64)
        ncand = parquet_debug_spatial_probe_count()
        call check(error, ncand >= 3_int64, "an ordinary build must evaluate at least the three bracket candidates")
        if (allocated(error)) return

        call sx%build(x, y, z, radius=0.08_real64, cell=0.1_real64)
        call check(error, parquet_debug_spatial_probe_count() == 0_int64, &
            "an explicit cell= must stop the probe running at all")
        if (allocated(error)) return

        call parquet_debug_set_spatial_cell(0.12_real64)
        call sx%build(x, y, z, radius=0.08_real64)
        call check(error, parquet_debug_spatial_probe_count() == 0_int64, &
            "a forced debug cell must stop the probe running at all")
        if (allocated(error)) return
        call check(error, abs(sx%cell_size() - 0.12_real64) <= 1.0e-12_real64, &
            "a forced debug cell must be the cell the index uses")
        call parquet_debug_reset_spatial_counters()
    end subroutine test_probe_runs_and_can_be_stopped

    !> The probe counts work rather than timing it, so the same points must give the same cell.
    subroutine test_probe_is_deterministic(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: a, b
        real(real64) :: h1, h2

        call parquet_debug_reset_spatial_counters()
        call make_cloud(3000_int64, 1.0_real64, .false., x, y, z)
        call a%build(x, y, z, radius=0.1_real64)
        h1 = a%cell_size()
        call b%build(x, y, z, radius=0.1_real64)
        h2 = b%cell_size()
        call check(error, h1 == h2, "a deterministic probe must choose the same cell for the same points")
        if (allocated(error)) return
        call check(error, h1 > 0.0_real64, "the chosen cell must be positive")
    end subroutine test_probe_is_deterministic

    !> The answers must not depend on the cell size. This is the property the whole tuner is
    !> allowed to trade against, and the one thing it must never change.
    subroutine test_answers_survive_any_cell(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        integer :: ic
        real(real64) :: cells(5), p(3), r

        n = 700_int64
        r = 0.17_real64
        cells = [0.02_real64, 0.06_real64, 0.2_real64, 0.6_real64, 3.0_real64]
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (got(n))
        do ic = 1, 5
            call sx%build(x, y, z, radius=r, cell=cells(ic))
            do q = 1_int64, 15_int64
                p(1) = pf_random_at(fixture_seed + 29_int64, q, 1_int64)
                p(2) = pf_random_at(fixture_seed + 29_int64, q, 2_int64)
                p(3) = pf_random_at(fixture_seed + 29_int64, q, 3_int64)
                mg = sx%within(p, r, got)
                call brute_within(x, y, z, p, r, [0.0_real64, 0.0_real64, 0.0_real64], want, mw)
                call check(error, same_rows(got, mg, want, mw), &
                    "the rows a query returns must not depend on the cell size the index was built with")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_answers_survive_any_cell

    !> An explicit cell is used as given, and comes back from `%cell_size` ready to pass on.
    subroutine test_explicit_cell_is_honoured(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: a, b
        integer(int64) :: nx, ny, nz
        real(real64) :: ext

        call make_cloud(1000_int64, 1.0_real64, .false., x, y, z)
        call a%build(x, y, z, radius=0.1_real64, cell=0.25_real64)
        call check(error, abs(a%cell_size() - 0.25_real64) <= 1.0e-12_real64, &
            "an explicit cell= must be the cell the index uses")
        if (allocated(error)) return
        call a%grid(nx, ny, nz)
        ! The grid must be the MINIMAL cover of the data's own bounding box -- which is very
        ! slightly under the unit cube for a random fixture, so asserting a literal count here
        ! would be asserting a property of the sample rather than of the grid.
        ext = maxval(x) - minval(x)
        call check(error, real(nx, kind=real64) * 0.25_real64 >= ext, &
            "the grid must span the data's extent along x")
        if (allocated(error)) return
        call check(error, real(nx - 1_int64, kind=real64) * 0.25_real64 <= ext, &
            "the grid must not carry a whole spare cell beyond the data's extent along x")
        if (allocated(error)) return
        call check(error, nx > 1_int64 .and. ny > 1_int64 .and. nz > 1_int64, &
            "a unit-cube cloud at cell 0.25 must divide into several cells on every axis")
        if (allocated(error)) return
        ! Handing %cell_size() straight back is the documented way to reuse a tuned cell.
        call b%build(x, y, z, radius=0.1_real64, cell=a%cell_size())
        call check(error, b%cell_size() == a%cell_size(), &
            "a cell taken from one index must reproduce itself when passed to another")
    end subroutine test_explicit_cell_is_honoured

    !> The cell count is clamped so the bucketing keeps `pf_argsort`'s counting fast path.
    subroutine test_cell_count_clamped(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n

        n = 2000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        ! A cell this fine asks for a million cells for two thousand points; the clamp must
        ! coarsen it rather than build a grid whose bucketing loses the O(n) path.
        call sx%build(x, y, z, radius=0.1_real64, cell=0.01_real64)
        call check(error, sx%cells() <= (3_int64 * n) / 10_int64, &
            "the cell count must be clamped to at most 0.3 cells per point")
        if (allocated(error)) return
        call check(error, sx%cell_size() > 0.01_real64, &
            "a clamped build must report the coarsened cell, not the one that was asked for")
        if (allocated(error)) return
        ! And the clamp must not fire when it is not needed.
        call sx%build(x, y, z, radius=0.1_real64, cell=0.25_real64)
        call check(error, abs(sx%cell_size() - 0.25_real64) <= 1.0e-12_real64, &
            "a cell that needs no clamping must be left exactly as given")
    end subroutine test_cell_count_clamped

    !> A radius list collapses to `sum(r^3)/sum(r^2)`, and a single radius to itself.
    subroutine test_effective_radius_moment_ratio(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        real(real64) :: rr(3), want

        call make_cloud(500_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call check(error, abs(sx%effective_radius() - 0.2_real64) <= 1.0e-12_real64, &
            "one radius must collapse to itself")
        if (allocated(error)) return
        rr = [0.1_real64, 0.2_real64, 0.5_real64]
        want = sum(rr**3) / sum(rr**2)
        call sx%build(x, y, z, radius=rr)
        call check(error, abs(sx%effective_radius() - want) <= 1.0e-12_real64, &
            "a radius list must collapse to its cubic-over-quadratic moment ratio")
    end subroutine test_effective_radius_moment_ratio

    !> `%rebuild` is a no-op on unchanged data and a real rebuild on changed data.
    subroutine test_rebuild_detects_change(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw
        logical :: did
        real(real64) :: p(3)

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64)
        call sx%rebuild(x, y, z, rebuilt=did)
        call check(error, .not. did, "rebuild must not rebuild when every coordinate is unchanged")
        if (allocated(error)) return
        ! One element differs, and it is deliberately a SMALL change: a checksum would be entitled
        ! to collide, an element-wise comparison cannot.
        x(n / 2_int64) = x(n / 2_int64) + 0.37_real64
        call sx%rebuild(x, y, z, rebuilt=did)
        call check(error, did, "rebuild must rebuild when a single coordinate has moved")
        if (allocated(error)) return
        allocate (got(n))
        p = 0.5_real64
        mg = sx%within(p, 0.15_real64, got)
        call brute_within(x, y, z, p, 0.15_real64, [0.0_real64, 0.0_real64, 0.0_real64], want, mw)
        call check(error, same_rows(got, mg, want, mw), &
            "after a rebuild the index must answer about the NEW coordinates")
    end subroutine test_rebuild_detects_change

    !> `%rebuild_for` re-tunes over the points already stored, with the caller supplying nothing.
    subroutine test_rebuild_for_retunes(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw
        real(real64) :: h_before, p(3)

        n = 3000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.02_real64)
        h_before = sx%cell_size()
        call sx%rebuild_for(0.4_real64)
        call check(error, sx%cell_size() /= h_before, &
            "rebuild_for a much larger radius must choose a different cell")
        if (allocated(error)) return
        call check(error, abs(sx%effective_radius() - 0.4_real64) <= 1.0e-12_real64, &
            "rebuild_for must REPLACE the recorded radius, not add to it")
        if (allocated(error)) return
        allocate (got(n))
        p = 0.5_real64
        mg = sx%within(p, 0.4_real64, got)
        call brute_within(x, y, z, p, 0.4_real64, [0.0_real64, 0.0_real64, 0.0_real64], want, mw)
        call check(error, same_rows(got, mg, want, mw), &
            "a re-tuned index must still answer exactly what a brute-force scan finds")
    end subroutine test_rebuild_for_retunes

    !> A bulk query whose radius disagrees badly with the build rebuilds; a nearby one does not.
    !>
    !> The second half is the negative control. Without it the test would pass against an
    !> implementation that rebuilt on every bulk call, which is the failure mode that would make
    !> the mechanism a performance cliff rather than a safety net.
    subroutine test_auto_rebuild(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: counts(:)
        type(pf_spatial_index) :: sx
        logical :: warn_was

        warn_was = parquet_get_spatial_rebuild_warning()
        call parquet_set_spatial_rebuild_warning(.false.)
        call parquet_debug_reset_spatial_counters()
        call make_cloud(3000_int64, 1.0_real64, .false., x, y, z)

        call sx%build(x, y, z, radius=0.01_real64)
        call sx%count_all_within(0.012_real64, counts)
        call check(error, parquet_debug_spatial_rebuilds() == 0_int64, &
            "a bulk query at nearly the built radius must not rebuild")
        if (allocated(error)) return

        call sx%count_all_within(0.5_real64, counts)
        call check(error, parquet_debug_spatial_rebuilds() == 1_int64, &
            "a bulk query at a radius far from the built one must rebuild exactly once")
        if (allocated(error)) return
        call check(error, sx%effective_radius() > 0.01_real64, &
            "an automatic rebuild must fold the new radius into the recorded ones")

        call parquet_set_spatial_rebuild_warning(warn_was)
        call parquet_debug_reset_spatial_counters()
    end subroutine test_auto_rebuild

    !> A no-copy index answers exactly as a copying one.
    subroutine test_copy_false_matches(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable, target :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: a, b
        integer(int64) :: n, ma, mb, q
        real(real64) :: p(3)

        n = 600_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call a%build(x, y, z, radius=0.15_real64, cell=0.2_real64)
        call b%build(x, y, z, radius=0.15_real64, cell=0.2_real64, copy=.false.)
        allocate (got(n), want(n))
        do q = 1_int64, 20_int64
            p(1) = pf_random_at(fixture_seed + 31_int64, q, 1_int64)
            p(2) = pf_random_at(fixture_seed + 31_int64, q, 2_int64)
            p(3) = pf_random_at(fixture_seed + 31_int64, q, 3_int64)
            ma = a%within(p, 0.15_real64, want)
            mb = b%within(p, 0.15_real64, got)
            call check(error, same_rows(got, mb, want, ma), &
                "a copy=.false. index must return exactly the rows a copying one does")
            if (allocated(error)) return
        end do
    end subroutine test_copy_false_matches

    !> The metadata queries report what was built.
    subroutine test_metadata_queries(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: nx, ny, nz

        call check(error, .not. sx%is_built(), "a fresh index must report itself unbuilt")
        if (allocated(error)) return
        call make_cloud(1000_int64, 2.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call check(error, sx%is_built(), "a built index must report itself built")
        if (allocated(error)) return
        call check(error, sx%size() == 1000_int64, "the index must report how many points it holds")
        if (allocated(error)) return
        call check(error, sx%ndim() == 3, "a 3D index must report three dimensions")
        if (allocated(error)) return
        call check(error, sx%metric() == PF_METRIC_EUCLIDEAN, "an ordinary build must report the Euclidean metric")
        if (allocated(error)) return
        call check(error, .not. sx%is_periodic(), "an index built without a box must not report itself periodic")
        if (allocated(error)) return
        call sx%grid(nx, ny, nz)
        call check(error, sx%cells() == nx * ny * nz, "the cell count must be the product of the axis counts")
        if (allocated(error)) return
        call sx%clear()
        call check(error, .not. sx%is_built(), "a cleared index must report itself unbuilt")
        if (allocated(error)) return
        call check(error, sx%size() == 0_int64, "a cleared index must hold no points")
    end subroutine test_metadata_queries

end module test_spatial
