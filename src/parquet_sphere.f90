!> Random points in sky regions, and the RA/Dec geometry a sky sampler needs.
!!
!! Three things, all on the unit sphere and all built on `parquet_random`'s points-on-a-sphere
!! family:
!!
!! * **`pf_sky_polygon`**: a polygon given by `(ra, dec)` vertices in degrees, with edges straight
!!   in the RA/Dec chart (`PF_EDGE_RADEC`) or along great circles (`PF_EDGE_GREAT_CIRCLE`). It
!!   answers `%contains`, `%area` and `%acceptance`, and draws points uniformly per unit solid
!!   angle inside it, by coordinate or along a `pf_random_stream`.
!! * **Points in HEALPix pixels and masks**: `pf_random_pixel_at` draws uniformly inside one pixel
!!   of a `pf_healpix_grid`, `pf_random_mask_at` uniformly over a list of pixels.
!! * **Deterministic geometry**: `pf_radec2vec`/`pf_vec2radec` in a named declination frame, and
!!   the Fibonacci grid. The frame-free RA/Dec geometry -- separations, offsets and position
!!   angles -- is `parquet_skycoord`'s.
!!
!! **A polygon rejects; a pixel does not.** A polygon's candidate is drawn uniformly over a region
!! bounding it -- the RA/Dec box, or the cap about the vertices' mean direction -- and kept when it
!! lies inside. A HEALPix pixel needs no such thing: it is a square in the equal-area projection, so
!! two uniforms read as a position inside that square ARE a point uniform over the pixel. Either
!! way the candidates of draw `d` are draws `1, 2, 3, ...` of the per-draw key
!! `pf_random_key(pf_random_key(seed, <family label>), d)` on stream `i`, so a value is a pure
!! function of its coordinates however many candidates it took, a fill splits anywhere, and a
!! stream form takes exactly one block per value. `pf_sky_region_algorithm` names all of it.
!!
!! **Where the declination frame enters, and where it does not.** A polygon's vertices and its
!! points are both `(ra, dec)`, and the mirrored convention (`theta = pi/2 + dec`) is a reflection
!! that maps great circles, containment and solid angle onto themselves, so `pf_sky_polygon` and
!! `pf_fibonacci_grid_radec` take no frame and compute in the standard one. A frame is named
!! wherever a VECTOR or a PIXEL crosses the interface: `pf_radec2vec`, `pf_vec2radec` and
!! `pf_fibonacci_grid` take `frame=`, and the pixel and mask samplers read the frame their
!! `pf_healpix_grid` was built with.
!!
!! **Arrow-free, settings-free and silent.** It reaches `parquet_random`, `parquet_healpix`,
!! `parquet_utils` and `parquet_constants` only (`check_parquet_sphere_stays_arrow_free`), reads no
!! knob and prints nothing, so it re-exports no setting. It does re-export the four HEALPix selectors and the two types its
!! procedures take, so a program whose only import is `use parquet_sphere` can build a grid and a
!! stream.
!!
!! **Thread safety.** Everything that reads a polygon or a grid is `pure` over `intent(in)`
!! objects, so one polygon built before a parallel region serves the whole team. `%init` and
!! `%clear` change the object and must not race with a reader. The one module variable is the
!! test-only acceptance-floor override, which no library code writes.
module parquet_sphere
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use parquet_random, only: pf_random_stream, pf_random_key, pf_random_fill_draws, pf_random_int_at, &
        pf_random_pair_spare_at, pf_random_disc_cap
    use parquet_healpix, only: pf_healpix_grid, PF_HP_RING, PF_HP_NEST, PF_HP_DEC_NORTH, &
        PF_HP_DEC_SOUTH, parquet_set_verbosity, parquet_get_verbosity, &
        parquet_set_message_stream, parquet_get_message_stream, &
        parquet_set_healpix_threads, parquet_get_healpix_threads
    use parquet_utils, only: pf_wrap_deg
    use parquet_constants, only: PF_PI, PF_RAD_PER_DEG, PF_DEG_PER_RAD
    implicit none
    private

    ! ---- The polygon ----
    public :: pf_sky_polygon
    public :: PF_EDGE_RADEC, PF_EDGE_GREAT_CIRCLE
    public :: pf_sky_region_algorithm
    ! ---- Points in HEALPix pixels and masks ----
    public :: pf_random_pixel_at, pf_random_pixel_radec_at
    public :: pf_random_mask_at, pf_random_mask_radec_at
    public :: pf_random_fill_mask, pf_random_fill_mask_radec
    public :: pf_random_pixel_next, pf_random_pixel_radec_next
    public :: pf_random_mask_next, pf_random_mask_radec_next
    ! ---- Deterministic geometry ----
    public :: pf_radec2vec, pf_vec2radec
    public :: pf_fibonacci_grid, pf_fibonacci_grid_radec
    ! ---- Re-exported, so one import is enough to call everything above ----
    !
    !> The two HEALPix schemes and the two declination frames, so a program importing this module
    !! alone can build a `pf_healpix_grid` and name a frame.
    public :: PF_HP_RING, PF_HP_NEST, PF_HP_DEC_NORTH, PF_HP_DEC_SOUTH
    !> The two types the samplers take, so a program importing this module alone can declare them.
    public :: pf_healpix_grid, pf_random_stream
    !
    !> The output pair, on the same argument. Re-exporting `pf_healpix_grid` re-exports its `_bulk`
    !! bindings, and those resolve a thread count through `parquet_clamp_to_affinity`, which says
    !! so once per process when this process's CPU affinity is narrower than the count asked for.
    !! So a program whose only import is `use parquet_sphere` can be made to print, and without
    !! these it could not silence it without naming `parquet_settings` -- which would put the C++
    !! boundary, and with it Arrow, back into an otherwise Arrow-free build.
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream
    !
    !> The thread cap those same `_bulk` bindings read when `threads=` is absent
    !! (`cfg_healpix_threads`, through `pf_healpix_threads`). It is the HEALPix tier's knob because
    !! it is the HEALPix tier's work; this module opens no team of its own.
    public :: parquet_set_healpix_threads, parquet_get_healpix_threads
    ! ---- Test-only observation and override hooks ----
    public :: parquet_debug_sphere_polygon_draw, parquet_debug_sphere_pixel_draw
    public :: parquet_debug_set_sphere_acceptance_floor

    ! ---- Edge rules ----

    !> Edges are straight lines in the `(ra, dec)` chart, with `ra` as written.
    !!
    !! The region is the planar polygon in right ascension and declination, so an edge between two
    !! vertices at one declination follows that parallel. Sampled uniformly per unit solid angle all
    !! the same, never per unit of the chart. `%init`'s default.
    integer, parameter :: PF_EDGE_RADEC = 0
    !> Edges are the minor great-circle arcs between consecutive vertices.
    !!
    !! Every vertex must lie within 89.9 degrees of the vertices' mean direction, which is how
    !! "within an open hemisphere" is decided.
    integer, parameter :: PF_EDGE_GREAT_CIRCLE = 1

    ! ---- The contract identifier ----

    !> Identifies the value contract of every region sampler: `pf_sky_polygon`'s `%random_*`,
    !! `pf_random_pixel_*` and `pf_random_mask_*`.
    !!
    !! It names what decides a value: the candidate distributions (sine-uniform over the RA/Dec box,
    !! and `pf_random_disc_at` over a cap, whose own values `pf_sphere_algorithm` freezes), the
    !! per-draw key and the candidate order, the containment rules (even-odd in the chart, and
    !! even-odd on the gnomonic projection about the cap centre with the far hemisphere excluded),
    !! the pixel's own square in the equal-area projection (`pf_healpix_grid%pix2vec_offset`), the
    !! mask's choice draw, the 1e-3 acceptance floor and the 100000-candidate cap. The
    !! deterministic geometry has no identifier: it is arithmetic, and its accuracy is asserted
    !! rather than frozen.
    character(len=*), parameter :: pf_sky_region_algorithm = "region:box+cap/gnomonic-evenodd/1e-3/v1"

    ! ---- Family labels ----
    !
    ! One `pf_random_key` label per family (`feature_risks.md` Risk-2: a new construction takes its
    ! own label), distinct from each other, from `parquet_random`'s sphere labels and from 0. The
    ! mask's point is deliberately NOT the pixel family's point for the chosen pixel.

    !> Label of `pf_sky_polygon`'s draws.
    integer(int64), parameter :: sky_polygon_label = 6296613622937029849_int64
    !> Label of `pf_random_pixel_at` and its forms.
    integer(int64), parameter :: sky_pixel_label = 4487668616495134178_int64
    !> Label of the mask's choice of a listed pixel.
    integer(int64), parameter :: sky_mask_choice_label = 2365686713864110313_int64
    !> Label of the mask's point inside the chosen pixel.
    integer(int64), parameter :: sky_mask_point_label = 3124069785697503286_int64

    ! ---- Limits ----

    !> The smallest `%acceptance` `%init` admits: a candidate walk then averages at most 1000 blocks.
    real(real64), parameter :: sky_acceptance_floor = 1.0e-3_real64
    !> The candidates a walk may reject before it aborts. At the floor that is `exp(-100)` per draw.
    integer(int64), parameter :: sky_candidate_cap = 100000_int64
    !> The candidates `%init` measures a self-intersecting polygon's even-odd area with, `2**18`.
    integer(int64), parameter :: sky_area_samples = 262144_int64
    !> The modulus of the measurement lattice, `2**53`: every step is exact in `int64` and in `real64`.
    integer(int64), parameter :: sky_lattice_mod = 9007199254740992_int64
    !> The lattice's first increment, `2**53` over the plastic number, made odd so its period is full.
    integer(int64), parameter :: sky_lattice_a1 = 6799333552837831_int64
    !> The lattice's second increment, `2**53` over the plastic number squared.
    integer(int64), parameter :: sky_lattice_a2 = 5132665044399055_int64
    !> The largest `pf_healpix_grid` resolution order the pixel and mask samplers accept, `2**24`:
    !! from about `2**25` a unit vector can no longer name a pixel near a pole.
    integer(int32), parameter :: sky_pixel_order_max = 24_int32
    !> `cos(89.9 degrees)`, the hemisphere rule's bound on a vertex's cosine from the mean direction.
    real(real64), parameter :: sky_hemisphere_cos = 1.7453283658983088e-3_real64
    !> The component magnitude above which `x*x + y*y` cannot go subnormal, so `hypot` is not needed.
    !!
    !! The squares underflow below about `1.5e-154`; this sits four decades clear of it.
    real(real64), parameter :: sky_hypot_safe = 1.0e-150_real64

    ! ---- Mathematical constants ----

    !> `360/phi` with `phi` the golden ratio: the Fibonacci grid's longitude step, in degrees.
    real(real64), parameter :: sky_golden_step_deg = 222.49223594996214535365126037162972_real64

    !> The acceptance floor `%init` applies when positive; 0 means `sky_acceptance_floor`.
    !!
    !! **Test-only**, set through `parquet_debug_set_sphere_acceptance_floor` from an out-of-process
    !! error scenario and nowhere else, so no test running in parallel ever reads a changed value.
    real(real64), save :: sky_floor_override = 0.0_real64

    ! ---- The polygon type ----

    !> A polygon on the sky, from `(ra, dec)` vertices in degrees, with uniform random points inside.
    !!
    !! ```fortran
    !! type(pf_sky_polygon) :: poly
    !! call poly%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], &
    !!                [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
    !! call poly%random_at(seed, 1_int64, ra, dec)   ! elemental: an index array draws a catalogue
    !! ```
    !!
    !! * **Vertices are read as written.** `ra` may run below 0 or above 360, so a polygon crossing
    !!   `ra = 0` is written continuously (`350, 370` rather than `350, 10`), and a declination band
    !!   around the whole sky is `0, 360, 360, 0` at two declinations. Under `PF_EDGE_RADEC` the RA
    !!   extent may not exceed 360. Orientation does not matter.
    !! * **Two edge rules.** `PF_EDGE_RADEC` (the default) is the planar polygon in the chart;
    !!   `PF_EDGE_GREAT_CIRCLE` joins the vertices by minor great-circle arcs, and needs every vertex
    !!   within 89.9 degrees of the vertices' mean direction.
    !! * **Containment is the even-odd rule**, so a self-intersecting polygon is accepted and sampled
    !!   by it. `%area` is exact for a simple polygon; for a self-intersecting one (`%is_simple()` is
    !!   `.false.`) it is the even-odd area, measured by a fixed Monte Carlo, because both exact
    !!   integrals are signed sums that cancel to nothing when the lobes balance. A point on an edge
    !!   is in or out arbitrarily.
    !! * **The acceptance floor.** `%init` refuses a polygon covering less than 1e-3 of its bounding
    !!   region (the RA/Dec box, or the cap about the vertices' mean direction): a thin diagonal
    !!   strip is split into pieces instead. The floor is applied to the same quantity `%acceptance`
    !!   returns, so a self-intersecting polygon is judged on what it actually samples.
    !!
    !! **Allocatable components, and so an ifx rule**: never declare one in a `block` inside an
    !! OpenMP parallel region. Build it before the region and read it inside; every reading binding
    !! is `pure` over `intent(in)`. No finalizer: the language releases the arrays.
    type :: pf_sky_polygon
        private
        !> Whether `%init` has run.
        logical :: set = .false.
        !> `PF_EDGE_RADEC` or `PF_EDGE_GREAT_CIRCLE`, or -1 before `%init`.
        integer :: rule = -1
        !> The vertices' right ascensions, degrees, as written.
        real(real64), allocatable :: ra(:)
        !> The vertices' declinations, degrees.
        real(real64), allocatable :: dec(:)
        !> The vertices in the containment test's plane: `ra` as written, or the gnomonic `x`.
        real(real64), allocatable :: px(:)
        !> The vertices in the containment test's plane: `dec`, or the gnomonic `y`.
        real(real64), allocatable :: py(:)
        !> The vertex box: smallest right ascension as written, degrees.
        real(real64) :: ra_lo = 0.0_real64
        !> The vertex box: largest right ascension as written, degrees.
        real(real64) :: ra_hi = 0.0_real64
        !> The vertex box: smallest declination, degrees.
        real(real64) :: dec_lo = 0.0_real64
        !> The vertex box: largest declination, degrees.
        real(real64) :: dec_hi = 0.0_real64
        !> `sin(dec_lo)`, where a chart candidate's sine starts.
        real(real64) :: sin_lo = 0.0_real64
        !> `sin(dec_hi) - sin(dec_lo)`, formed as a product so a thin box keeps its height.
        real(real64) :: sin_span = 0.0_real64
        !> The cap centre: the normalised sum of the vertices' unit vectors, standard frame.
        real(real64) :: centre(3) = 0.0_real64
        !> The first vector of the right-handed frame `(e1, e2, centre)`.
        real(real64) :: e1(3) = 0.0_real64
        !> The second vector of that frame.
        real(real64) :: e2(3) = 0.0_real64
        !> The cap radius: the largest vertex angle from `centre`, radians.
        real(real64) :: cap_radius = 0.0_real64
        !> The polygon's area, steradians.
        real(real64) :: area_v = 0.0_real64
        !> The bounding region's area, steradians: the box, or the cap.
        real(real64) :: bound_area = 0.0_real64
        !> Whether two non-adjacent edges cross, so `%area` is measured rather than integrated.
        logical :: self_int = .false.
    contains
        procedure, non_overridable :: init => sky_polygon_init        !! Builds the polygon; validates.
        procedure, non_overridable :: clear => sky_polygon_clear      !! Releases it; `%init` may run again.
        procedure, non_overridable :: is_set => sky_polygon_is_set    !! Whether `%init` has run.
        procedure, non_overridable :: size => sky_polygon_size        !! The vertex count, or 0.
        procedure, non_overridable :: edges => sky_polygon_edges      !! The edge rule, or -1.
        procedure, non_overridable :: is_simple => sky_polygon_is_simple  !! Whether no two edges cross.
        procedure, non_overridable :: area => sky_polygon_area        !! Area, steradians.
        procedure, non_overridable :: area_deg2 => sky_polygon_area_deg2  !! Area, square degrees.
        procedure, non_overridable :: acceptance => sky_polygon_acceptance  !! Area over bounding area.
        procedure, non_overridable :: bounds => sky_polygon_bounds    !! The vertex box.
        procedure, non_overridable :: contains => sky_polygon_contains  !! Whether a position is inside.
        procedure, private, non_overridable :: sky_polygon_random_at_i32 !! `%random_at`, `int32` stream.
        procedure, private, non_overridable :: sky_polygon_random_at_i64 !! `%random_at`, `int64` stream.
        generic :: random_at => sky_polygon_random_at_i32, sky_polygon_random_at_i64 !! A uniform point at `(seed, i, draw)`.
        procedure, private, non_overridable :: sky_polygon_random_fill_i32 !! `%random_fill`, `int32` stream.
        procedure, private, non_overridable :: sky_polygon_random_fill_i64 !! `%random_fill`, `int64` stream.
        generic :: random_fill => sky_polygon_random_fill_i32, sky_polygon_random_fill_i64 !! Consecutive points of one stream.
        procedure, non_overridable :: random_next => sky_polygon_random_next  !! Next point of a stream.
    end type pf_sky_polygon

    ! ---- Generic interfaces: points in HEALPix pixels and masks ----

    !> A direction uniformly distributed inside one HEALPix pixel: value `draw` (default 1) of stream `i`.
    !!
    !! `v = pf_random_pixel_at(grid, seed, i, ipix, [draw])`. `grid` is a built `pf_healpix_grid` of
    !! `nside` at most `2**24`, whose scheme names the pixel; `seed` and `draw` are `integer(int64)`;
    !! `i` is `integer(int32)` or `integer(int64)`, and so, independently, is `ipix`, which must lie
    !! in `[0, npix)`. The result is a HEALPix-native unit vector, as `%pix2vec` returns. **Nothing
    !! is rejected**: the two uniforms of one block are read through `%pix2vec_offset` as a position
    !! inside the pixel's own square in the equal-area projection, so one enciphering is one point.
    !! At the finest resolutions `%vec2pix` may name an immediate neighbour for a point within
    !! rounding of a pixel boundary near a pole, where a unit vector no longer resolves which pixel
    !! it is in; the point is uniform over the pixel regardless, and stays within `%max_pixrad` of
    !! its centre. `draw` below 1 clamps to 1. An unbuilt grid, a finer one and an index out of
    !! range abort, naming this procedure. `pure`, not `elemental`
    !! (a rank-1 result cannot be); `pf_random_pixel_radec_at` is elemental.
    interface pf_random_pixel_at
        module procedure sky_pixel_at_i32_i32
        module procedure sky_pixel_at_i32_i64
        module procedure sky_pixel_at_i64_i32
        module procedure sky_pixel_at_i64_i64
    end interface pf_random_pixel_at

    !> `pf_random_pixel_at`'s point as `(ra, dec)` in degrees, in the grid's declination frame.
    !!
    !! `call pf_random_pixel_radec_at(grid, seed, i, ipix, ra, dec, [draw])`: the same point, not a
    !! second draw, read through `grid%vec2radec`. `ra` in `[0, 360)` and `dec` in `[-90, 90]` are
    !! `intent(out)`; everything else as for `pf_random_pixel_at`. `pure elemental` over `i`, `ipix`
    !! and `draw`, so one statement draws a point in every pixel of a list.
    interface pf_random_pixel_radec_at
        module procedure sky_pixel_radec_at_i32_i32
        module procedure sky_pixel_radec_at_i32_i64
        module procedure sky_pixel_radec_at_i64_i32
        module procedure sky_pixel_radec_at_i64_i64
    end interface pf_random_pixel_radec_at

    !> A direction uniformly distributed over the union of a list of HEALPix pixels.
    !!
    !! `v = pf_random_mask_at(grid, seed, i, pixels, [draw])`. `pixels` is a non-empty rank-1 array
    !! of pixel indices in the grid's scheme, `integer(int32)` or `integer(int64)` independently of
    !! `i`. A duplicate counts twice, so a weight built by repetition is honoured. The listed pixel
    !! is `pixels(pf_random_int_at(pf_random_key(seed, <choice label>), i, 1, size(pixels), draw))`
    !! -- uniform over the list, since every pixel has one area -- and the point inside it comes from
    !! its own family, not `pf_random_pixel_at`'s. An entry is checked when it is chosen, so a long
    !! list costs nothing per draw; the fills check the whole list first. Everything else as for
    !! `pf_random_pixel_at`.
    interface pf_random_mask_at
        module procedure sky_mask_at_i32_i32
        module procedure sky_mask_at_i32_i64
        module procedure sky_mask_at_i64_i32
        module procedure sky_mask_at_i64_i64
    end interface pf_random_mask_at

    !> `pf_random_mask_at`'s point as `(ra, dec)` in degrees, in the grid's declination frame.
    !!
    !! `call pf_random_mask_radec_at(grid, seed, i, pixels, ra, dec, [draw])`: the same point, not a
    !! second draw. `pure`, not `elemental`, since `pixels` is an array; `pf_random_fill_mask_radec`
    !! draws many.
    interface pf_random_mask_radec_at
        module procedure sky_mask_radec_at_i32_i32
        module procedure sky_mask_radec_at_i32_i64
        module procedure sky_mask_radec_at_i64_i32
        module procedure sky_mask_radec_at_i64_i64
    end interface pf_random_mask_radec_at

    !> Fills the columns of `v` with consecutive mask points of one stream, starting at `draw`.
    !!
    !! `call pf_random_fill_mask(grid, seed, i, pixels, v, [draw])`: `v` is `real(real64)`, shaped
    !! `(3, n)`, `intent(out)`, and column `k` is `pf_random_mask_at(grid, seed, i, pixels, draw+k-1)`
    !! exactly, so a fill split anywhere agrees with a whole one. The grid and the whole list are
    !! checked before anything is drawn; `v` must have three rows even with no columns, and
    !! `draw + n - 1` must not exceed `huge(int64)`. No `threads=`: wrap the loop yourself.
    interface pf_random_fill_mask
        module procedure sky_fill_mask_i32_i32
        module procedure sky_fill_mask_i32_i64
        module procedure sky_fill_mask_i64_i32
        module procedure sky_fill_mask_i64_i64
    end interface pf_random_fill_mask

    !> Fills `ra` and `dec` with consecutive mask points of one stream, in the grid's frame, in degrees.
    !!
    !! `call pf_random_fill_mask_radec(grid, seed, i, pixels, ra, dec, [draw])`: rank-1 arrays of one
    !! size, element `k` exactly `pf_random_mask_radec_at(..., draw+k-1)`. The rest as for
    !! `pf_random_fill_mask`.
    interface pf_random_fill_mask_radec
        module procedure sky_fill_mask_radec_i32_i32
        module procedure sky_fill_mask_radec_i32_i64
        module procedure sky_fill_mask_radec_i64_i32
        module procedure sky_fill_mask_radec_i64_i64
    end interface pf_random_fill_mask_radec

    !> The next point in one HEALPix pixel along a `pf_random_stream`, taking one block.
    !!
    !! `call pf_random_pixel_next(grid, rng, ipix, v)`: aligns the stream to a block, reads its seed
    !! and stream index through `%address`, and returns `pf_random_pixel_at(grid, seed, stream, ipix,
    !! d)` for the block `d` it took, whatever the number of candidates. `ipix` is `integer(int32)` or
    !! `integer(int64)`.
    interface pf_random_pixel_next
        module procedure sky_pixel_next_i32
        module procedure sky_pixel_next_i64
    end interface pf_random_pixel_next

    !> `pf_random_pixel_next`'s point as `(ra, dec)` in degrees, in the grid's frame, taking one block.
    !!
    !! `call pf_random_pixel_radec_next(grid, rng, ipix, ra, dec)`.
    interface pf_random_pixel_radec_next
        module procedure sky_pixel_radec_next_i32
        module procedure sky_pixel_radec_next_i64
    end interface pf_random_pixel_radec_next

    !> The next point over a list of HEALPix pixels along a `pf_random_stream`, taking one block.
    !!
    !! `call pf_random_mask_next(grid, rng, pixels, v)`: `pf_random_mask_at` at the stream's own
    !! coordinates and the block `d` it took.
    interface pf_random_mask_next
        module procedure sky_mask_next_i32
        module procedure sky_mask_next_i64
    end interface pf_random_mask_next

    !> `pf_random_mask_next`'s point as `(ra, dec)` in degrees, in the grid's frame, taking one block.
    !!
    !! `call pf_random_mask_radec_next(grid, rng, pixels, ra, dec)`.
    interface pf_random_mask_radec_next
        module procedure sky_mask_radec_next_i32
        module procedure sky_mask_radec_next_i64
    end interface pf_random_mask_radec_next

    ! ---- Generic interfaces: deterministic geometry ----

    !> A Fibonacci (golden-spiral) grid of `n` quasi-uniform directions, as unit vectors.
    !!
    !! `call pf_fibonacci_grid(n, vec, [frame])`: `n` is `integer(int32)` or `integer(int64)` and at
    !! least 1; `vec` is `real(real64)`, shaped `(3, n)`, `intent(out)`. Point `k` (from 0) has
    !! latitude `asin(1 - 2*(k + 1/2)/n)` and longitude `2*pi*(k + 1/2)/phi`, `phi` the golden
    !! ratio -- astropy's `golden_spiral_grid`, the half step in the longitude included -- so both
    !! poles are avoided and neighbours sit about `sqrt(4*pi/n)` apart. The same points as
    !! `pf_fibonacci_grid_radec`, in `frame` (default `PF_HP_DEC_NORTH`). Aborts on `n < 1`, on a
    !! `vec` of any other shape and on an unknown frame.
    interface pf_fibonacci_grid
        module procedure sky_fibonacci_i32
        module procedure sky_fibonacci_i64
    end interface pf_fibonacci_grid

    !> The Fibonacci grid of `n` points as `(ra, dec)` in degrees.
    !!
    !! `call pf_fibonacci_grid_radec(n, ra, dec)`: `ra` in `[0, 360)` and `dec` in `(-90, 90)`, rank-1
    !! `real(real64)` arrays of size `n`, `intent(out)`. The grid is defined in longitude and
    !! latitude, so this form needs no frame. Aborts on `n < 1` and on arrays of any other size.
    interface pf_fibonacci_grid_radec
        module procedure sky_fibonacci_radec_i32
        module procedure sky_fibonacci_radec_i64
    end interface pf_fibonacci_grid_radec

    ! ---- Interfaces: deterministic geometry ----
    !
    ! Implemented in submodule parquet_sphere_geom.

    interface
        !> A sky position in degrees as a unit vector, in a named declination frame.
        !!
        !! `(cos(dec)*cos(ra), cos(dec)*sin(ra), sin(dec))`, with the third component negated under
        !! `PF_HP_DEC_SOUTH`. **A declination of exactly +/-90 is the pole `(0, 0, +/-1)` by rule**,
        !! whatever `ra` says: `cos(90 * pi/180)` is `6.1e-17` rather than 0, a representation limit
        !! no formulation removes. **Total**: `ra` may take any value, a `dec` outside `[-90, 90]`
        !! is read as the direction it names, and a NaN argument gives NaN components without
        !! raising a flag; an infinite one raises `IEEE_INVALID`, as a sine of infinity must. Only
        !! an unknown `frame` aborts.
        pure module subroutine pf_radec2vec(ra, dec, vec, frame)
            real(real64), intent(in) :: ra !! right ascension, degrees; any value.
            real(real64), intent(in) :: dec !! declination, degrees.
            real(real64), intent(out) :: vec(3) !! the unit vector of that direction.
            integer, intent(in), optional :: frame !! `PF_HP_DEC_NORTH` (default) or `PF_HP_DEC_SOUTH`.
        end subroutine pf_radec2vec

        !> A direction as a sky position in degrees, in a named declination frame.
        !!
        !! `vec` need not have unit length: it is scaled by its largest component first, so
        !! `[1e-300, 0, 1e-300]` is a direction. `dec = atan2(z, hypot(x, y))`, never `asin`, which
        !! loses half its digits near a pole, negated under `PF_HP_DEC_SOUTH`. **The right ascension
        !! of a pole is 0 by rule**, and the zero vector answers `(0, 0)`. **Total**: a NaN component
        !! gives NaN outputs without raising a flag, and an infinite component is read as the
        !! direction of the infinite components alone. Only an unknown `frame` aborts.
        pure module subroutine pf_vec2radec(vec, ra, dec, frame)
            real(real64), intent(in) :: vec(3) !! a direction; any length.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! declination, degrees, in `[-90, 90]`.
            integer, intent(in), optional :: frame !! `PF_HP_DEC_NORTH` (default) or `PF_HP_DEC_SOUTH`.
        end subroutine pf_vec2radec

        !> `pf_fibonacci_grid` for an `integer(int32)` count.
        pure module subroutine sky_fibonacci_i32(n, vec, frame)
            integer(int32), intent(in) :: n !! the number of points; at least 1.
            real(real64), intent(out) :: vec(:, :) !! shaped `(3, n)`: column `k` is point `k`.
            integer, intent(in), optional :: frame !! `PF_HP_DEC_NORTH` (default) or `PF_HP_DEC_SOUTH`.
        end subroutine sky_fibonacci_i32

        !> `pf_fibonacci_grid` for an `integer(int64)` count.
        pure module subroutine sky_fibonacci_i64(n, vec, frame)
            integer(int64), intent(in) :: n !! the number of points; at least 1.
            real(real64), intent(out) :: vec(:, :) !! shaped `(3, n)`: column `k` is point `k`.
            integer, intent(in), optional :: frame !! `PF_HP_DEC_NORTH` (default) or `PF_HP_DEC_SOUTH`.
        end subroutine sky_fibonacci_i64

        !> `pf_fibonacci_grid_radec` for an `integer(int32)` count.
        pure module subroutine sky_fibonacci_radec_i32(n, ra, dec)
            integer(int32), intent(in) :: n !! the number of points; at least 1.
            real(real64), intent(out) :: ra(:) !! right ascensions, degrees, in `[0, 360)`; size `n`.
            real(real64), intent(out) :: dec(:) !! declinations, degrees; size `n`.
        end subroutine sky_fibonacci_radec_i32

        !> `pf_fibonacci_grid_radec` for an `integer(int64)` count.
        pure module subroutine sky_fibonacci_radec_i64(n, ra, dec)
            integer(int64), intent(in) :: n !! the number of points; at least 1.
            real(real64), intent(out) :: ra(:) !! right ascensions, degrees, in `[0, 360)`; size `n`.
            real(real64), intent(out) :: dec(:) !! declinations, degrees; size `n`.
        end subroutine sky_fibonacci_radec_i64
    end interface

    ! ---- Interfaces: helpers shared by the three submodules ----
    !
    ! Implemented in submodule parquet_sphere_geom; private.

    interface
        !> A `real64` as message text, in `es` form so no leading zero or exponent width varies by compiler.
        pure module function sky_real_text(x) result(t)
            real(real64), intent(in) :: x !! the value to render.
            character(len=24) :: t !! the rendering, left-justified.
        end function sky_real_text

        !> An `int64` as message text.
        pure module function sky_int_text(n) result(t)
            integer(int64), intent(in) :: n !! the value to render.
            character(len=24) :: t !! the rendering, left-justified.
        end function sky_int_text

        !> The sign a declination frame gives the third vector component: +1 for `PF_HP_DEC_NORTH`
        !! (and an absent `frame`), -1 for `PF_HP_DEC_SOUTH`; any other value aborts naming `who`.
        pure module function sky_frame_sign(who, frame) result(sgn)
            character(len=*), intent(in) :: who !! the entry point, for the message.
            integer, intent(in), optional :: frame !! the caller's frame selector.
            real(real64) :: sgn !! +1 or -1.
        end function sky_frame_sign

        !> The sine and cosine of a declination in degrees, exactly `(+/-1, 0)` at `+/-90`.
        pure module subroutine sky_dec_sin_cos(dec, sd, cd)
            real(real64), intent(in) :: dec !! a declination, degrees; not NaN.
            real(real64), intent(out) :: sd !! its sine.
            real(real64), intent(out) :: cd !! its cosine.
        end subroutine sky_dec_sin_cos

        !> A position in degrees as a unit vector in the standard frame, the pole exact. No NaN screen.
        pure module function sky_radec_unit(ra, dec) result(v)
            real(real64), intent(in) :: ra !! right ascension, degrees.
            real(real64), intent(in) :: dec !! declination, degrees.
            real(real64) :: v(3) !! the unit vector.
        end function sky_radec_unit

        !> A nonzero, finite vector as `(ra, dec)` in degrees, standard frame, the pole's `ra` 0.
        pure module subroutine sky_unit_radec(v, ra, dec)
            real(real64), intent(in) :: v(3) !! a direction; nonzero, finite, of moderate length.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! declination, degrees, in `[-90, 90]`.
        end subroutine sky_unit_radec

        !> Reserves the next whole block of `rng`, block-aligned as `parquet_random`'s sphere
        !! producers are, and returns the stream's seed, its index, and the block's draw index.
        pure module subroutine sky_take_block(who, rng, seed, stream, d)
            character(len=*), intent(in) :: who !! the entry point, for the message.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            integer(int64), intent(out) :: seed !! the seed the stream was given.
            integer(int64), intent(out) :: stream !! the stream index it was given.
            integer(int64), intent(out) :: d !! the 1-based draw index of the block taken.
        end subroutine sky_take_block

        !> The 1-based draw index of a coordinate form: absent means 1, and below 1 clamps to 1.
        pure module function sky_draw(draw) result(d)
            integer(int64), intent(in), optional :: draw !! the caller's `draw`, present or not.
            integer(int64) :: d !! a draw index of at least 1.
        end function sky_draw

        !> The first draw of a fill of `n` values: `draw` clamped to at least 1, refused when
        !! `draw + n - 1` would pass `huge(int64)`.
        pure module function sky_fill_start(who, draw, n) result(d)
            character(len=*), intent(in) :: who !! the entry point, for the message.
            integer(int64), intent(in), optional :: draw !! the caller's starting draw.
            integer(int64), intent(in) :: n !! how many values the fill draws; at least 1.
            integer(int64) :: d !! the first draw index.
        end function sky_fill_start
    end interface

    ! ---- Interfaces: the polygon ----
    !
    ! Implemented in submodule parquet_sphere_polygon.

    interface
        !> Builds the polygon from vertices in degrees, in order.
        !!
        !! `call poly%init(ra, dec, [edges], [strict])`: `ra` and `dec` are rank-1 `real(real64)`
        !! arrays of one size, at least 3; `edges` is `PF_EDGE_RADEC` (default) or
        !! `PF_EDGE_GREAT_CIRCLE`. **It aborts**, naming itself, on: a polygon already built
        !! (`%clear` first); an unknown edge rule; fewer than 3 vertices or arrays of different
        !! sizes; a non-finite vertex or a declination outside `[-90, 90]`; under `PF_EDGE_RADEC` an
        !! RA extent above 360; under `PF_EDGE_GREAT_CIRCLE` a vertex 89.9 degrees or more from the
        !! vertices' mean direction; a polygon of zero area; and a polygon covering less than 1e-3 of
        !! its bounding region.
        !!
        !! **`strict = .true.` adds one refusal**: a chart polygon that looks like a band written the
        !! short way across `ra = 0` -- an RA extent above 180 whose vertices take only two clusters,
        !! one at each end of `[0, 360]` -- is refused rather than read as its own complement. It is
        !! off by default because the same shape is how a legitimate whole-sky band is written; see
        !! the "Vertices are read as written" section of the guide page.
        module subroutine sky_polygon_init(this, ra, dec, edges, strict)
            class(pf_sky_polygon), intent(inout) :: this !! the polygon; must not be built yet.
            real(real64), intent(in) :: ra(:) !! the vertices' right ascensions, degrees, as written.
            real(real64), intent(in) :: dec(:) !! the vertices' declinations, degrees, in `[-90, 90]`.
            integer, intent(in), optional :: edges !! `PF_EDGE_RADEC` (default) or `PF_EDGE_GREAT_CIRCLE`.
            logical, intent(in), optional :: strict !! `.true.` refuses a suspected short-way RA band; default `.false.`.
        end subroutine sky_polygon_init

        !> Releases the polygon's arrays and resets it, so `%init` may run again. Safe on an unbuilt one.
        pure module subroutine sky_polygon_clear(this)
            class(pf_sky_polygon), intent(inout) :: this !! the polygon to reset.
        end subroutine sky_polygon_clear

        !> Whether `%init` has run since the polygon was declared or cleared.
        pure elemental module function sky_polygon_is_set(this) result(ok)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            logical :: ok !! `.true.` once built.
        end function sky_polygon_is_set

        !> The number of vertices, or 0 before `%init`.
        pure elemental module function sky_polygon_size(this) result(n)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            integer(int64) :: n !! the vertex count.
        end function sky_polygon_size

        !> The edge rule, `PF_EDGE_RADEC` or `PF_EDGE_GREAT_CIRCLE`, or -1 before `%init`.
        pure elemental module function sky_polygon_edges(this) result(rule)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            integer :: rule !! the edge rule `%init` was given.
        end function sky_polygon_edges

        !> Whether no two non-adjacent edges of the polygon cross. Aborts before `%init`.
        !!
        !! `.false.` says the polygon is self-intersecting, and so that `%area` and `%acceptance` are
        !! the Monte Carlo measurements described there rather than exact integrals. Decided once by
        !! `%init`, by an `O(n^2)` test over the edge pairs in the plane the containment test uses.
        pure elemental module function sky_polygon_is_simple(this) result(simple)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            logical :: simple !! `.true.` when no two non-adjacent edges cross.
        end function sky_polygon_is_simple

        !> The polygon's area in steradians. Aborts before `%init`.
        !!
        !! Exact to rounding for a simple polygon. `PF_EDGE_RADEC`: Green's theorem on
        !! `cos(dec) d(dec) d(ra)`, taken about the lowest vertex declination so a small polygon
        !! near a pole keeps its digits. `PF_EDGE_GREAT_CIRCLE`: the signed sum of the spherical
        !! excesses of the triangles from the cap centre, by the triple-product form in the cap's
        !! own frame.
        !!
        !! **For a self-intersecting polygon (`%is_simple()` is `.false.`) it is the area the
        !! even-odd rule samples, measured**: neither integral answers that question -- both are
        !! signed sums, which cancel to nothing on a polygon whose lobes balance -- so `%init` counts
        !! how many of `2**18` points of the `R2` low-discrepancy lattice over the bounding region
        !! fall inside (`sky_polygon_measure`). The value is a pure function of the polygon -- the
        !! lattice needs no key, and no caller's seed enters it -- and is accurate to about one part
        !! in ten thousand, better on a polygon whose boundary is shorter.
        pure elemental module function sky_polygon_area(this) result(a)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            real(real64) :: a !! the area, steradians.
        end function sky_polygon_area

        !> The polygon's area in square degrees, `%area() * (180/pi)**2`. Aborts before `%init`.
        pure elemental module function sky_polygon_area_deg2(this) result(a)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            real(real64) :: a !! the area, square degrees.
        end function sky_polygon_area_deg2

        !> The fraction of candidates a draw keeps: `%area()` over the bounding region's area.
        !!
        !! The bounding region is the vertex box `(ra_hi - ra_lo)*(sin(dec_hi) - sin(dec_lo))` for
        !! `PF_EDGE_RADEC`, and the cap about the vertices' mean direction through the farthest
        !! vertex for `PF_EDGE_GREAT_CIRCLE`. At least 1e-3 on any polygon `%init` built; a draw
        !! averages `1/%acceptance()` candidates. **That identity holds for a self-intersecting
        !! polygon too**, because `%area` is then the measured even-odd area rather than a signed
        !! sum. Aborts before `%init`.
        pure elemental module function sky_polygon_acceptance(this) result(f)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            real(real64) :: f !! the acceptance, in `[1e-3, 1]`.
        end function sky_polygon_acceptance

        !> The vertex box, degrees: right ascension as written, so `ra_hi` may exceed 360.
        !!
        !! Under `PF_EDGE_GREAT_CIRCLE` an edge may bulge beyond the box's declinations; the box is
        !! the vertices'. Aborts before `%init`.
        pure module subroutine sky_polygon_bounds(this, ra_lo, ra_hi, dec_lo, dec_hi)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            real(real64), intent(out) :: ra_lo !! smallest vertex right ascension, as written.
            real(real64), intent(out) :: ra_hi !! largest vertex right ascension, as written.
            real(real64), intent(out) :: dec_lo !! smallest vertex declination.
            real(real64), intent(out) :: dec_hi !! largest vertex declination.
        end subroutine sky_polygon_bounds

        !> Whether `(ra, dec)` in degrees lies inside the polygon, by the even-odd rule.
        !!
        !! `ra` may take any value: it is wrapped into the polygon's own range, `[ra_lo, ra_lo + 360)`.
        !! A non-finite argument and a declination outside `[-90, 90]` are outside. Under
        !! `PF_EDGE_GREAT_CIRCLE` a position on the far side of the cap centre's hemisphere is outside
        !! before any projection. A point on an edge is in or out arbitrarily. Aborts before `%init`.
        !! `pure elemental`.
        pure elemental module function sky_polygon_contains(this, ra, dec) result(inside)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            real(real64), intent(in) :: ra !! right ascension, degrees; any value.
            real(real64), intent(in) :: dec !! declination, degrees.
            logical :: inside !! whether the position is inside.
        end function sky_polygon_contains

        !> `%random_at` for an `integer(int32)` stream index. See the `int64` specific.
        pure elemental module subroutine sky_polygon_random_at_i32(this, seed, i, ra, dec, draw)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends, so any value is valid.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! declination, degrees.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_polygon_random_at_i32

        !> A position uniformly distributed per unit solid angle inside the polygon: value `draw` of stream `i`.
        !!
        !! `call poly%random_at(seed, i, ra, dec, [draw])`. Candidates are uniform over the bounding
        !! region -- `ra` uniform and `sin(dec)` uniform over the vertex box, or `pf_random_disc_at`
        !! over the cap -- and the first inside is kept; they are the draws `1, 2, ...` of the key
        !! `pf_random_key(pf_random_key(seed, <polygon label>), draw)`, so the value is a pure
        !! function of `(seed, i, draw)`. **The polygon does not enter the key**: two polygons at one
        !! coordinate are two transforms of one candidate stream, and overlapping ones give related
        !! points; derive `pf_random_key(seed, <a label of your own>)` per polygon for independence.
        !! `draw` below 1 clamps to 1. Aborts before `%init`, and after 100000 rejected candidates,
        !! which `%init`'s floor makes a defect signal (`exp(-100)` per draw at the floor).
        !! `pure elemental` over `i` and `draw`.
        pure elemental module subroutine sky_polygon_random_at_i64(this, seed, i, ra, dec, draw)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index; every value is valid.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! declination, degrees.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_polygon_random_at_i64

        !> `%random_fill` for an `integer(int32)` stream index. See the `int64` specific.
        pure module subroutine sky_polygon_random_fill_i32(this, seed, i, ra, dec, draw)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends, so any value is valid.
            real(real64), intent(out) :: ra(:) !! right ascensions, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec(:) !! declinations, degrees; the size of `ra`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_polygon_random_fill_i32

        !> Fills `ra` and `dec` with consecutive points of one stream, starting at `draw`.
        !!
        !! `call poly%random_fill(seed, i, ra, dec, [draw])`: element `k` is exactly
        !! `%random_at(seed, i, ra, dec, draw+k-1)`, so a fill split anywhere agrees with a whole one.
        !! Arrays of different sizes abort; `draw + n - 1` must not exceed `huge(int64)`. No
        !! `threads=`: wrap the loop yourself, the values do not depend on how.
        pure module subroutine sky_polygon_random_fill_i64(this, seed, i, ra, dec, draw)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index; every value is valid.
            real(real64), intent(out) :: ra(:) !! right ascensions, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec(:) !! declinations, degrees; the size of `ra`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_polygon_random_fill_i64

        !> The next point inside the polygon along a `pf_random_stream`, taking one block.
        !!
        !! `call poly%random_next(rng, ra, dec)`: aligns the stream to a block, and returns
        !! `%random_at(seed, stream, ra, dec, d)` at the stream's own seed and index for the block
        !! `d` it took, however many candidates that value needed, so `%position` advances by one
        !! block per point.
        pure module subroutine sky_polygon_random_next(this, rng, ra, dec)
            class(pf_sky_polygon), intent(in) :: this !! the polygon.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`.
            real(real64), intent(out) :: dec !! declination, degrees.
        end subroutine sky_polygon_random_next

        !> Runs `%random_at`'s walk and reports how many candidates it took. **Test-only.**
        !!
        !! Public because the walk's rejection count is the one observable that tells a walk that
        !! rejects from one that accepts everything, and it lives behind the polygon's private
        !! components. The point equals `%random_at`'s exactly. `pure`, holds no state, and is called
        !! from no library code.
        pure module subroutine parquet_debug_sphere_polygon_draw(poly, seed, i, draw, ra, dec, ncand)
            type(pf_sky_polygon), intent(in) :: poly !! the polygon.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int64), intent(in) :: draw !! 1-based value index; below 1 clamps to 1.
            real(real64), intent(out) :: ra !! right ascension, degrees, as `%random_at` gives it.
            real(real64), intent(out) :: dec !! declination, degrees, as `%random_at` gives it.
            integer(int64), intent(out) :: ncand !! candidates drawn, the accepted one included.
        end subroutine parquet_debug_sphere_polygon_draw

        !> Overrides the acceptance floor `%init` applies; `floor <= 0` restores 1e-3. **Test-only.**
        !!
        !! Exists so the candidate cap's abort can be reached by an out-of-process error scenario,
        !! which admits a polygon far below the floor and draws from it. Process-global and not
        !! thread-safe: never call it from a test that runs beside others.
        module subroutine parquet_debug_set_sphere_acceptance_floor(floor)
            real(real64), intent(in) :: floor !! the new floor, or a value `<= 0` for the default.
        end subroutine parquet_debug_set_sphere_acceptance_floor
    end interface

    ! ---- Interfaces: points in HEALPix pixels and masks ----
    !
    ! Implemented in submodule parquet_sphere_pixel.

    interface
        !> `pf_random_pixel_at`, `int32` stream index, `int32` pixel index.
        pure module function sky_pixel_at_i32_i32(grid, seed, i, ipix, draw) result(v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int32), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
            real(real64) :: v(3) !! a unit vector uniform inside the pixel.
        end function sky_pixel_at_i32_i32

        !> `pf_random_pixel_at`, `int32` stream index, `int64` pixel index.
        pure module function sky_pixel_at_i32_i64(grid, seed, i, ipix, draw) result(v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int64), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
            real(real64) :: v(3) !! a unit vector uniform inside the pixel.
        end function sky_pixel_at_i32_i64

        !> `pf_random_pixel_at`, `int64` stream index, `int32` pixel index.
        pure module function sky_pixel_at_i64_i32(grid, seed, i, ipix, draw) result(v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int32), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
            real(real64) :: v(3) !! a unit vector uniform inside the pixel.
        end function sky_pixel_at_i64_i32

        !> `pf_random_pixel_at`, `int64` stream index, `int64` pixel index.
        pure module function sky_pixel_at_i64_i64(grid, seed, i, ipix, draw) result(v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int64), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
            real(real64) :: v(3) !! a unit vector uniform inside the pixel.
        end function sky_pixel_at_i64_i64

        !> `pf_random_pixel_radec_at`, `int32` stream index, `int32` pixel index.
        pure elemental module subroutine sky_pixel_radec_at_i32_i32(grid, seed, i, ipix, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int32), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_pixel_radec_at_i32_i32

        !> `pf_random_pixel_radec_at`, `int32` stream index, `int64` pixel index.
        pure elemental module subroutine sky_pixel_radec_at_i32_i64(grid, seed, i, ipix, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int64), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_pixel_radec_at_i32_i64

        !> `pf_random_pixel_radec_at`, `int64` stream index, `int32` pixel index.
        pure elemental module subroutine sky_pixel_radec_at_i64_i32(grid, seed, i, ipix, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int32), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_pixel_radec_at_i64_i32

        !> `pf_random_pixel_radec_at`, `int64` stream index, `int64` pixel index.
        pure elemental module subroutine sky_pixel_radec_at_i64_i64(grid, seed, i, ipix, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int64), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_pixel_radec_at_i64_i64

        !> `pf_random_mask_at`, `int32` stream index, `int32` pixel list.
        pure module function sky_mask_at_i32_i32(grid, seed, i, pixels, draw) result(v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
            real(real64) :: v(3) !! a unit vector uniform over the listed pixels.
        end function sky_mask_at_i32_i32

        !> `pf_random_mask_at`, `int32` stream index, `int64` pixel list.
        pure module function sky_mask_at_i32_i64(grid, seed, i, pixels, draw) result(v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
            real(real64) :: v(3) !! a unit vector uniform over the listed pixels.
        end function sky_mask_at_i32_i64

        !> `pf_random_mask_at`, `int64` stream index, `int32` pixel list.
        pure module function sky_mask_at_i64_i32(grid, seed, i, pixels, draw) result(v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
            real(real64) :: v(3) !! a unit vector uniform over the listed pixels.
        end function sky_mask_at_i64_i32

        !> `pf_random_mask_at`, `int64` stream index, `int64` pixel list.
        pure module function sky_mask_at_i64_i64(grid, seed, i, pixels, draw) result(v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
            real(real64) :: v(3) !! a unit vector uniform over the listed pixels.
        end function sky_mask_at_i64_i64

        !> `pf_random_mask_radec_at`, `int32` stream index, `int32` pixel list.
        pure module subroutine sky_mask_radec_at_i32_i32(grid, seed, i, pixels, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_mask_radec_at_i32_i32

        !> `pf_random_mask_radec_at`, `int32` stream index, `int64` pixel list.
        pure module subroutine sky_mask_radec_at_i32_i64(grid, seed, i, pixels, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_mask_radec_at_i32_i64

        !> `pf_random_mask_radec_at`, `int64` stream index, `int32` pixel list.
        pure module subroutine sky_mask_radec_at_i64_i32(grid, seed, i, pixels, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_mask_radec_at_i64_i32

        !> `pf_random_mask_radec_at`, `int64` stream index, `int64` pixel list.
        pure module subroutine sky_mask_radec_at_i64_i64(grid, seed, i, pixels, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
            integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1.
        end subroutine sky_mask_radec_at_i64_i64

        !> `pf_random_fill_mask`, `int32` stream index, `int32` pixel list.
        pure module subroutine sky_fill_mask_i32_i32(grid, seed, i, pixels, v, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: v(:, :) !! shaped `(3, n)`: column `k` is draw `draw+k-1`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_fill_mask_i32_i32

        !> `pf_random_fill_mask`, `int32` stream index, `int64` pixel list.
        pure module subroutine sky_fill_mask_i32_i64(grid, seed, i, pixels, v, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: v(:, :) !! shaped `(3, n)`: column `k` is draw `draw+k-1`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_fill_mask_i32_i64

        !> `pf_random_fill_mask`, `int64` stream index, `int32` pixel list.
        pure module subroutine sky_fill_mask_i64_i32(grid, seed, i, pixels, v, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: v(:, :) !! shaped `(3, n)`: column `k` is draw `draw+k-1`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_fill_mask_i64_i32

        !> `pf_random_fill_mask`, `int64` stream index, `int64` pixel list.
        pure module subroutine sky_fill_mask_i64_i64(grid, seed, i, pixels, v, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: v(:, :) !! shaped `(3, n)`: column `k` is draw `draw+k-1`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_fill_mask_i64_i64

        !> `pf_random_fill_mask_radec`, `int32` stream index, `int32` pixel list.
        pure module subroutine sky_fill_mask_radec_i32_i32(grid, seed, i, pixels, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra(:) !! right ascensions, degrees, grid's frame.
            real(real64), intent(out) :: dec(:) !! declinations, degrees, grid's frame; the size of `ra`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_fill_mask_radec_i32_i32

        !> `pf_random_fill_mask_radec`, `int32` stream index, `int64` pixel list.
        pure module subroutine sky_fill_mask_radec_i32_i64(grid, seed, i, pixels, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int32), intent(in) :: i !! stream index; sign-extends.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra(:) !! right ascensions, degrees, grid's frame.
            real(real64), intent(out) :: dec(:) !! declinations, degrees, grid's frame; the size of `ra`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_fill_mask_radec_i32_i64

        !> `pf_random_fill_mask_radec`, `int64` stream index, `int32` pixel list.
        pure module subroutine sky_fill_mask_radec_i64_i32(grid, seed, i, pixels, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra(:) !! right ascensions, degrees, grid's frame.
            real(real64), intent(out) :: dec(:) !! declinations, degrees, grid's frame; the size of `ra`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_fill_mask_radec_i64_i32

        !> `pf_random_fill_mask_radec`, `int64` stream index, `int64` pixel list.
        pure module subroutine sky_fill_mask_radec_i64_i64(grid, seed, i, pixels, ra, dec, draw)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra(:) !! right ascensions, degrees, grid's frame.
            real(real64), intent(out) :: dec(:) !! declinations, degrees, grid's frame; the size of `ra`.
            integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1.
        end subroutine sky_fill_mask_radec_i64_i64

        !> `pf_random_pixel_next` with an `int32` pixel index.
        pure module subroutine sky_pixel_next_i32(grid, rng, ipix, v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            integer(int32), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            real(real64), intent(out) :: v(3) !! a unit vector uniform inside the pixel.
        end subroutine sky_pixel_next_i32

        !> `pf_random_pixel_next` with an `int64` pixel index.
        pure module subroutine sky_pixel_next_i64(grid, rng, ipix, v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            integer(int64), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            real(real64), intent(out) :: v(3) !! a unit vector uniform inside the pixel.
        end subroutine sky_pixel_next_i64

        !> `pf_random_pixel_radec_next` with an `int32` pixel index.
        pure module subroutine sky_pixel_radec_next_i32(grid, rng, ipix, ra, dec)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            integer(int32), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
        end subroutine sky_pixel_radec_next_i32

        !> `pf_random_pixel_radec_next` with an `int64` pixel index.
        pure module subroutine sky_pixel_radec_next_i64(grid, rng, ipix, ra, dec)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            integer(int64), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
        end subroutine sky_pixel_radec_next_i64

        !> `pf_random_mask_next` with an `int32` pixel list.
        pure module subroutine sky_mask_next_i32(grid, rng, pixels, v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: v(3) !! a unit vector uniform over the listed pixels.
        end subroutine sky_mask_next_i32

        !> `pf_random_mask_next` with an `int64` pixel list.
        pure module subroutine sky_mask_next_i64(grid, rng, pixels, v)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: v(3) !! a unit vector uniform over the listed pixels.
        end subroutine sky_mask_next_i64

        !> `pf_random_mask_radec_next` with an `int32` pixel list.
        pure module subroutine sky_mask_radec_next_i32(grid, rng, pixels, ra, dec)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            integer(int32), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
        end subroutine sky_mask_radec_next_i32

        !> `pf_random_mask_radec_next` with an `int64` pixel list.
        pure module subroutine sky_mask_radec_next_i64(grid, rng, pixels, ra, dec)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            type(pf_random_stream), intent(inout) :: rng !! the stream to advance by one block.
            integer(int64), intent(in) :: pixels(:) !! pixel indices in the grid's scheme; non-empty.
            real(real64), intent(out) :: ra !! right ascension, degrees, in `[0, 360)`, grid's frame.
            real(real64), intent(out) :: dec !! declination, degrees, grid's frame.
        end subroutine sky_mask_radec_next_i64

        !> Runs `pf_random_pixel_at`'s draw and reports how many candidates it took. **Test-only.**
        !!
        !! Public for the reason `parquet_debug_sphere_polygon_draw` gives; the point equals
        !! `pf_random_pixel_at`'s exactly. **`ncand` is 1 for every draw**, because this family does
        !! not reject; the argument is what lets a test assert that rather than assume it. `pure`,
        !! holds no state, and is called from no library code.
        pure module subroutine parquet_debug_sphere_pixel_draw(grid, seed, i, ipix, draw, v, ncand)
            type(pf_healpix_grid), intent(in) :: grid !! a built grid, `nside` at most `2**24`.
            integer(int64), intent(in) :: seed !! the stream family's seed.
            integer(int64), intent(in) :: i !! stream index.
            integer(int64), intent(in) :: ipix !! a pixel index in the grid's scheme, in `[0, npix)`.
            integer(int64), intent(in) :: draw !! 1-based value index; below 1 clamps to 1.
            real(real64), intent(out) :: v(3) !! the point, as `pf_random_pixel_at` gives it.
            integer(int64), intent(out) :: ncand !! candidates drawn; always 1, since none is rejected.
        end subroutine parquet_debug_sphere_pixel_draw
    end interface

end module parquet_sphere ! GCOVR_EXCL_LINE
