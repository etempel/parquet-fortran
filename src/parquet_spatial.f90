!> A uniform-grid spatial index over two or three plain Fortran coordinate arrays.
!!
!! `pf_spatial_index` buckets points into a regular grid of cubic cells and answers "which points
!! lie within `r` of this point" by walking only the cells the query ball can reach. The algorithm
!! is the one `../qfeet/src/cubesort.f90` implements and this module's grid is derived from --
!! *cubesort* is its name and that is the acknowledgement -- but nothing here depends on qfeet, and
!! the bucketing goes through `pf_argsort` from the tier below rather than a second counting sort.
!!
!! **The engine choice was measured, not assumed** (`feature_pandas_S3.md`): against nanoflann's
!! KD-tree over 1M uniform points in a 100^3 box, a grid built in 0.025 s against 0.18-0.20 s and
!! answered a radius query in 4.27 us against 6.74 -- on a uniform cloud, a clustered one and a
!! sparse wedge alike. The one operation a tree does better is k-nearest, and `%nearest` answers it
!! on this grid rather than reopening the engine question -- by a ball that expands until it holds
!! enough points, which is exact for the reason `spatial_shell_search` sets out.
!!
!! **This module is Arrow-free by construction and that is the point of its tier.** It reaches
!! `parquet_argsort`, `parquet_healpix` and `parquet_settings_base` and nothing else, so
!! `use parquet_spatial` in a downstream project compiles fifteen Fortran files rather than the
!! fifty-odd `use parquet_io` costs. `check_parquet_spatial_stays_arrow_free` (tools/check_source_conventions.py) and
!! `tools/module_footprints.txt` are what keep that true -- a `use` line added here can silently
!! multiply what every consumer compiles, and no test can see it happen.
!!
!! **The cell size is MEASURED at build time, not fitted.** A cost model
!! (`spatial_model_cell`) supplies a bracket and a deterministic work-count probe
!! (`spatial_choose_cell`) picks the winner from it; the model's own constant spans a factor of 2.7
!! across the four fixtures it was calibrated on, and a bad cell costs up to 20x, so measuring is
!! the difference between a heuristic that is usually right and one that is right. `cell=` on
!! `%build` overrides the whole mechanism for a caller who has measured something themselves.
!!
!! **Two dimensions are the three-dimensional index with `z` omitted**, not a second type or a
!! second walk: a zero `z` array gives a bounding box of zero extent in z, so `nz` collapses to 1
!! and the walk's `k` loop runs exactly once. Measured cost about 22%; see
!! `feature_pandas_S3_cubesort.md`, capability (f).
module parquet_spatial
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use parquet_argsort, only: pf_argsort, &
        parquet_set_sort_threads, parquet_get_sort_threads, &
        parquet_set_sort_radix_path, parquet_get_sort_radix_path, &
        parquet_set_sort_counting_path, parquet_get_sort_counting_path, &
        parquet_set_sort_counting_bucket_limit, parquet_get_sort_counting_bucket_limit
    use parquet_settings_base, only: cfg_spatial_threads, cfg_spatial_rebuild_warning, &
        parquet_set_spatial_threads, parquet_get_spatial_threads, &
        parquet_set_spatial_rebuild_warning, parquet_get_spatial_rebuild_warning, &
        parquet_set_verbosity, parquet_get_verbosity, &
        parquet_set_message_stream, parquet_get_message_stream, &
        parquet_clamp_to_affinity, parquet_emit_warning, parquet_output_is_suppressed
    ! **The HEALPix sky backend's whole dependency, and the one import that grows what a
    ! `use parquet_spatial` consumer compiles** -- from 9 Fortran files to 15. That was weighed
    ! and accepted: both modules are Arrow-free, so no C++ or Arrow boundary moves and the cost is
    ! compile time for six small leaf files, and the alternative was a second index type with a
    ! duplicated walk, annulus, ranking and `sorted=` contract. `tools/module_footprints.txt`
    ! records the increase; do not "fix" a footprint failure by editing that file.
    !
    ! `pf_query_disc_runs` is public in `parquet_healpix` for THIS caller and is privatised again
    ! in `src/parquet.f90`, so it does not reach a `use parquet` program -- see
    ! `check_facade_hides_healpix_run_query`.
    use parquet_healpix, only: pf_vec2pix_ring, pf_query_disc_runs, pf_nside2resol, pf_max_pixrad
    implicit none
    private

    public :: pf_spatial_index
    public :: pf_connected_components
    public :: PF_METRIC_EUCLIDEAN, PF_METRIC_SKY
    public :: PF_SKY_GRID3D, PF_SKY_HEALPIX
    !
    ! ---- Test-only observation and override hooks ----
    !
    public :: parquet_debug_spatial_probe_count
    public :: parquet_debug_spatial_work
    public :: parquet_debug_spatial_pixels_visited
    public :: parquet_debug_set_spatial_nside
    public :: parquet_debug_set_spatial_run_buffer
    public :: parquet_debug_spatial_rebuilds
    public :: parquet_debug_spatial_threads_used
    public :: parquet_debug_set_spatial_cell
    public :: parquet_debug_set_spatial_shell_start
    public :: parquet_debug_spatial_shell_rounds
    public :: parquet_debug_reset_spatial_counters
    !
    ! ---- Settings this module's own code reads, re-exported so a narrow import can configure it ----
    !
    !> Its own two knobs. `spatial_threads` caps a bulk query's team; `spatial_rebuild_warning`
    !! governs whether an automatic rebuild says so.
    public :: parquet_set_spatial_threads, parquet_get_spatial_threads
    public :: parquet_set_spatial_rebuild_warning, parquet_get_spatial_rebuild_warning
    !> The sorting knobs, because `%build` buckets through `pf_argsort` and every one of them
    !! governs which path that takes -- `sort_counting_bucket_limit` is read directly by the
    !! cell-count clamp.
    public :: parquet_set_sort_threads, parquet_get_sort_threads
    public :: parquet_set_sort_radix_path, parquet_get_sort_radix_path
    public :: parquet_set_sort_counting_path, parquet_get_sort_counting_path
    public :: parquet_set_sort_counting_bucket_limit, parquet_get_sort_counting_bucket_limit
    !> The output pair, because the automatic-rebuild warning emits from this module.
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream

    !> Labels the connected components of an undirected graph given as an edge list -- which is
    !! exactly what `%pairs_within` returns, so **Friends-of-Friends is these two calls**.
    !!
    !! **A module procedure rather than a type-bound one, and it lives here despite having no
    !! spatial content at all.** A union-find over an edge list is pure graph work; it is in this
    !! module because `use parquet_spatial` costs fifteen Fortran files against `parquet_sorting`'s
    !! twenty-one, so a caller who wants only connected components pays less here than anywhere
    !! else it could sensibly go -- and because a group finder then needs one `use` rather than two.
    interface pf_connected_components
        module procedure components_n32
        module procedure components_n64
    end interface pf_connected_components

    ! ---- Metric identifiers ----

    !> An ordinary Euclidean index over two or three coordinates. What `%build` produces.
    integer, parameter :: PF_METRIC_EUCLIDEAN = 1
    !> A unit-vector index over `(RA, Dec)`, queried by angular radius. What `%build_sky` produces.
    integer, parameter :: PF_METRIC_SKY = 2

    !> The uniform 3D grid over unit vectors: `%build_sky`'s backend unless another is asked for.
    !!
    !! **The default, permanently.** A backend decides only how the candidate set is narrowed,
    !! never what the answer is -- but it does decide the order an UNSORTED result comes back in,
    !! so a default that changed later would silently reorder every existing program's results.
    integer, parameter :: PF_SKY_GRID3D = 1
    !> The HEALPix pixelisation of the sphere, as `%build_sky`'s backend. Ask for it with `backend=`.
    !!
    !! **What it trades.** The 3D grid buckets unit vectors in a cube, and a sphere occupies a
    !! zero-thickness shell of that cube -- so at a million points about 95% of its cells can never
    !! hold anything, and the cell size is floored far above what a small query radius wants.
    !! HEALPix has no empty pixels by construction, so it tests far fewer candidates per hit. It
    !! pays a fixed per-query cost for the disc walk that the grid's integer cell arithmetic does
    !! not, so it wins at larger radii and loses at very small ones; `bench/benchmark_spatial.sh`
    !! is how to find the crossover on a given machine.
    integer, parameter :: PF_SKY_HEALPIX = 2

    !> The largest HEALPix resolution parameter this backend will consider.
    !!
    !! `parquet_healpix`'s own int64 ceiling, restated here because that module keeps its limit
    !! private. Nothing reaches it in practice -- the buckets-per-point cap binds first for any
    !! point count a machine can hold -- so its job is to bound the two doubling loops in
    !! `spatial_set_nside` rather than to express a policy.
    integer(int64), parameter :: spatial_max_nside = 2_int64 ** 29

    !> Degrees to radians, for the sky metric's conversions.
    real(real64), parameter :: spatial_deg2rad = 0.017453292519943295_real64
    !> Radians to degrees, the inverse of `spatial_deg2rad`.
    real(real64), parameter :: spatial_rad2deg = 57.29577951308232_real64
    !> The largest angular radius a sky query will accept, in degrees.
    !!
    !! **Not a correctness limit -- the chord mapping is exact all the way to 180 degrees.** It is
    !! a usefulness one: past a hemisphere the ball covers most of the catalogue, the grid has
    !! nothing left to prune, and a query that returns almost every row is not a neighbour search.
    !! Refusing says so, where returning everything slowly would not.
    real(real64), parameter :: spatial_max_sky_deg = 90.0_real64

    ! ---- Tuning constants. None of these is a setting; see feature_pandas_S3_cubesort.md ----

    !> Centre of the cell-size bracket, `h = kappa * (r_eff / ((4/3) pi rho))^(1/(1+ndim))`.
    !!
    !! A property of the machine (the fourth root of a ratio of a cell-step cost to a point-test
    !! cost), not of the data, and NOT a setting: the probe measures the answer, so there is
    !! nothing left for a caller to configure. Calibrated at 2.9 against four measured sweeps, where
    !! the implied value ranged 1.68 to 4.51 -- which is precisely why a fitted constant is only the
    !! centre of a bracket here and never the answer.
    real(real64), parameter :: spatial_kappa = 2.9_real64
    !> Candidate cell sizes are `h/2, h, 2h`; this is the ratio.
    real(real64), parameter :: spatial_probe_step = 2.0_real64
    !> How many extra candidates the probe may step out when the bracket's winner is an endpoint.
    integer, parameter :: spatial_probe_widen = 3
    !> Probe queries per candidate. A few hundred is enough and the draw is deterministic.
    integer, parameter :: spatial_probe_points = 256
    !> Relative cost of stepping to one more cell against distance-testing one more point.
    !!
    !! The probe ranks candidate cell sizes by `A*cells_visited + B*points_tested`, and **only the
    !! RATIO matters** -- scaling both leaves the cheapest candidate unchanged -- so `B` is 1 by
    !! definition and `A` carries the whole content. `A/B = 2` says a cell step costs about what two
    !! point tests cost, which is plausible for what each one is: a cell step is two loads from a
    !! prefix-sum array at a stride, latency-bound; a point test is three contiguous loads and nine
    !! flops, throughput-bound.
    !!
    !! **Fitted on machine A (arm64, NEON, gfortran 15.2) against five 1M-point fixtures**, timing
    !! and counting the SAME workload -- cloud points, radii cycling the declared list -- because a
    !! ratio fitted between two different workloads is a ratio for neither. Mean and worst penalty
    !! of each ratio's pick against that fixture's timed optimum, over uniform, clustered, wedge,
    !! sphere and flat:
    !!
    !!     A/B     0.25   0.5     1      2      4      8     16
    !!     mean    8.0%   8.0%   8.0%   2.5%   4.4%   7.1%  18.2%
    !!     worst  14.0%  14.0%  14.0%   6.1%  15.2%  15.2%  36.1%
    !!
    !! **2 is the unique best on both counts**, and the usable band is roughly 2 to 4 rather than
    !! the 32x span an earlier fit against the gate's recorded sweeps suggested. It is still far
    !! more forgiving than the `kappa` it replaced, which sets the cell LINEARLY and so had to be
    !! right within about 1.3x -- this one enters only as a tie-break between candidates a factor
    !! apart. Re-derive it elsewhere with `bench/benchmark_spatial.sh --mode=ab`; it is a ratio of a
    !! latency-bound cost to a throughput-bound one, which is what differs across NEON, AVX2 and
    !! AVX-512.
    real(real64), parameter :: spatial_work_a = 2.0_real64
    real(real64), parameter :: spatial_work_b = 1.0_real64 !! see spatial_work_a.
    !> Ceiling on cells per point.
    !!
    !! **Not a tuning preference: below this the bucketing keeps `pf_argsort`'s counting fast path.**
    !! That path needs a key range under `0.3 * n` when serial (`0.01 * n` at two threads), and it is
    !! that gate -- not the 2^22 bucket limit -- that binds first. cubesort's own budget rule admits
    !! `8 * n` cells, which is 27x past this and would never see the counting path; do not copy it.
    real(real64), parameter :: spatial_max_cells_per_point = 0.3_real64
    !> Ratio of the refinement step, applied once around the bracket's winner.
    !!
    !! **The bracket alone leaves a granularity error, and it was measured rather than predicted.**
    !! Ranking three candidates two apart picks the best of the three -- and on the gate's sparse
    !! wedge fixture the true optimum sat between two of them, so the probe was RIGHT about its own
    !! candidates and still 12.9% off the swept best. Two more candidates at sqrt(2) either side of
    !! the winner halve that granularity for about a third of one extra build, which is the
    !! cheapest accuracy available anywhere in this module.
    real(real64), parameter :: spatial_refine_step = 1.4142135623730951_real64
    !> Safety factor on the expanding ball's first radius.
    !!
    !! The radius that would hold `k+1` points at the index's measured density, times this. Above 1
    !! because overshooting costs a few extra distance tests and undershooting costs a whole round;
    !! **not a setting**, because it changes how many rounds the expansion takes and never what it
    !! returns, so nothing a caller can observe distinguishes one value from another.
    real(real64), parameter :: spatial_shell_safety = 1.25_real64
    !> Extra growth applied on top of the count-based rescale when the ball came back short.
    !!
    !! The rescale alone can stall: a ball holding `k-1` points asks for a factor of about 1, so
    !! without this the radius would creep and the loop would not terminate in bounded time. Same
    !! reasoning as `spatial_shell_safety` for why it is not a setting.
    real(real64), parameter :: spatial_shell_grow = 1.15_real64
    !> How far the cell a query radius would choose may drift from the cell in use before a bulk
    !! query rebuilds the index. A factor on the CELL, not on the radius; `h` goes as the fourth
    !! root of `r`, so this is a very wide band in radius terms and is meant to be.
    real(real64), parameter :: spatial_rebuild_factor = 2.0_real64

    !> A uniform-grid spatial index: points bucketed into cubic cells, queried by radius.
    !!
    !! **Not finalizable, deliberately.** Every component is allocatable and Fortran deallocates
    !! them at scope exit anyway (F2018 9.7.3.2); there is no C++ handle and no OpenMP lock to
    !! release. Two compiler hazards this project has recorded make that worth stating rather than
    !! leaving implicit: a `FINAL` bound to a separate module procedure ICEs nagfor for every
    !! sibling submodule, and gfortran does not reliably default-initialise a `private()` copy of a
    !! finalizable type. Keeping it plain is what makes an index usable per-thread on every compiler
    !! in the fleet. **Do not add a finalizer.**
    !!
    !! **It has allocatable components, so it must not be declared in a `block` inside an OpenMP
    !! parallel region on ifx** -- that is the `for_alloc_private` segfault class. Use `private()`,
    !! which ifx handles and which is safe here precisely because the type is not finalizable. In
    !! practice an index is built once and queried read-only by many threads, so it is shared.
    type :: pf_spatial_index
        private
        integer(int64) :: npts = 0_int64        !! points stored.
        integer(int64) :: grid_n(3) = 1_int64   !! cells along x, y, z. int64 so the product is safe.
        integer(int64) :: n_cells = 0_int64     !! grid_n(1)*grid_n(2)*grid_n(3).
        integer :: ncoord = 3                   !! 2 or 3: how many coordinates the caller supplied.
        integer :: dims_eff = 3                 !! non-degenerate axes the points actually occupy: 1, 2 or 3.
        integer :: metric_id = PF_METRIC_EUCLIDEAN !! PF_METRIC_EUCLIDEAN or PF_METRIC_SKY.
        integer :: backend_id = PF_SKY_GRID3D   !! PF_SKY_GRID3D or PF_SKY_HEALPIX; sky indexes only.
        integer(int64) :: nside_v = 0_int64     !! HEALPix resolution parameter; 0 on a 3D-grid index.
        integer(int64) :: npix_v = 0_int64      !! 12*nside_v**2; 0 on a 3D-grid index.
        logical :: built_ok = .false.           !! whether %build has run.
        logical :: owns = .true.                !! .true. holds copies, .false. points at the caller's arrays.
        logical :: periodic_on = .false.        !! whether any axis wraps.
        logical :: warned = .false.             !! whether the rebuild warning has been said for this index.
        real(real64) :: lo(3) = 0.0_real64      !! grid origin: the bounding-box corner, or box_lo.
        real(real64) :: hi(3) = 0.0_real64      !! the opposite corner.
        real(real64) :: cell(3) = 1.0_real64    !! cell side per axis; equal unless the box is periodic.
        real(real64) :: cell_inv(3) = 1.0_real64 !! 1/cell, precomputed.
        real(real64) :: cell_side = 1.0_real64  !! the scalar cell asked for or chosen, before per-axis tiling.
        real(real64) :: wrap(3) = 0.0_real64    !! box length on a periodic axis, 0 on a free one.
        real(real64) :: wrap_inv(3) = 0.0_real64 !! 1/wrap, or 0 -- lets the minimum image be branch-free.
        real(real64) :: rho = 0.0_real64        !! density measured at build, in points per unit ndim-volume.
        real(real64) :: r2sum = 0.0_real64      !! sum of r^2 over every radius this index has been given.
        real(real64) :: r3sum = 0.0_real64      !! sum of r^3 over the same, so r_eff = r3sum/r2sum.
        real(real64), allocatable :: xs(:)      !! x of stored point k. Reordered into cell order when owns.
        real(real64), allocatable :: ys(:)      !! y of stored point k.
        real(real64), allocatable :: zs(:)      !! z of stored point k; all zero for a 2D index.
        real(real64), pointer, contiguous :: px(:) => null() !! the caller's x when owns is .false.
        real(real64), pointer, contiguous :: py(:) => null() !! the caller's y when owns is .false.
        real(real64), pointer, contiguous :: pz(:) => null() !! the caller's z when owns is .false.
        integer(int64), allocatable :: start(:) !! dense: cell c holds stored points start(c) : start(c+1)-1.
        integer(int64), allocatable :: idx(:)   !! the caller's row index of stored point k.
    contains
        procedure, private :: bind_build_r0 !! %build with one radius.
        procedure, private :: bind_build_r1 !! %build with a list of radii.
        !> Builds the index over `x`, `y` and optionally `z`. `radius=` is mandatory.
        generic :: build => bind_build_r0, bind_build_r1
        procedure, private :: bind_build_sky_r0 !! %build_sky with one angular radius.
        procedure, private :: bind_build_sky_r1 !! %build_sky with a list of angular radii.
        !> Builds a SKY index over `(ra, dec)` in degrees. `radius_deg=` is mandatory.
        generic :: build_sky => bind_build_sky_r0, bind_build_sky_r1
        procedure, private :: bind_rebuild_r0 !! %rebuild with one radius.
        procedure, private :: bind_rebuild_r1 !! %rebuild with a list of radii.
        !> Validate-and-rebuild against possibly-new coordinates; a no-op when they are unchanged.
        generic :: rebuild => bind_rebuild_r0, bind_rebuild_r1
        procedure, private :: bind_rebuild_for_r0 !! %rebuild_for with one radius.
        procedure, private :: bind_rebuild_for_r1 !! %rebuild_for with a list of radii.
        !> Re-tunes the cell size for a new radius over the same points.
        generic :: rebuild_for => bind_rebuild_for_r0, bind_rebuild_for_r1
        procedure :: clear => bind_clear !! Releases everything and returns the index to unbuilt.
        procedure :: size => bind_size !! How many points the index holds.
        procedure :: cell_size => bind_cell_size !! The cell side chosen, ready to pass back as `cell=`.
        procedure :: cells => bind_cells !! How many cells the grid has.
        procedure :: cell_sides => bind_cell_sides !! The cell side per axis, as the grid uses it.
        procedure :: grid => bind_grid !! Cells along each axis.
        procedure :: metric => bind_metric !! PF_METRIC_EUCLIDEAN or PF_METRIC_SKY.
        procedure :: backend => bind_backend !! PF_SKY_GRID3D or PF_SKY_HEALPIX.
        procedure :: nside => bind_nside !! HEALPix resolution parameter, or 0 on a 3D-grid index.
        procedure :: npix => bind_npix !! HEALPix pixel count, or 0 on a 3D-grid index.
        procedure :: ndim => bind_ndim !! 2 or 3.
        procedure :: is_built => bind_is_built !! Whether %build has run.
        procedure :: is_periodic => bind_is_periodic !! Whether the index wraps at the box faces.
        procedure :: effective_radius => bind_effective_radius !! The radius the cell size was tuned for.
        procedure, private :: bind_within_i32 !! %within into an int32 buffer.
        procedure, private :: bind_within_i64 !! %within into an int64 buffer.
        !> Points within `r` of `p`, into a caller-owned buffer. Returns the TRUE count.
        generic :: within => bind_within_i32, bind_within_i64
        procedure :: count_within => bind_count_within !! How many points lie within `r` of `p`.
        procedure, private :: bind_sky_i32 !! %within_sky into an int32 buffer.
        procedure, private :: bind_sky_i64 !! %within_sky into an int64 buffer.
        !> Points within `rsky_deg` degrees of `(ra, dec)`. Sky indexes only.
        generic :: within_sky => bind_sky_i32, bind_sky_i64
        procedure :: count_within_sky => bind_count_sky !! How many points lie within `rsky_deg` of `(ra, dec)`.
        procedure, private :: bind_seg_i32 !! %within_segment into an int32 buffer.
        procedure, private :: bind_seg_i64 !! %within_segment into an int64 buffer.
        !> Points within `r` of the SEGMENT `p1`-`p2`: a capsule, round ends included.
        generic :: within_segment => bind_seg_i32, bind_seg_i64
        procedure, private :: bind_cyl_i32 !! %within_cylinder into an int32 buffer.
        procedure, private :: bind_cyl_i64 !! %within_cylinder into an int64 buffer.
        !> Points within `r` of the axis `p1`-`p2` AND between its ends: flat caps.
        generic :: within_cylinder => bind_cyl_i32, bind_cyl_i64
        procedure, private :: bind_cone_i32 !! %within_cone into an int32 buffer.
        procedure, private :: bind_cone_i64 !! %within_cone into an int64 buffer.
        !> Points inside the truncated cone from radius `r1` at `p1` to `r2` at `p2`.
        !!
        !! The general form of the three: `r1 == r2` is exactly `%within_cylinder`.
        generic :: within_cone => bind_cone_i32, bind_cone_i64
        procedure, private :: bind_all_within_r0 !! %all_within with one radius.
        procedure, private :: bind_all_within_r1 !! %all_within with one radius per point.
        !> Every point's neighbours, as one CSR structure. The primary bulk query.
        !!
        !! DIRECTED: row `i`'s list holds the points within `radius(i)` of it, so a per-point
        !! radius gives a list that is deliberately not symmetric. `%pairs_within` is the
        !! symmetric one.
        generic :: all_within => bind_all_within_r0, bind_all_within_r1
        procedure, private :: bind_pairs_within_r0 !! %pairs_within with one radius.
        procedure, private :: bind_pairs_within_r1 !! %pairs_within with one radius per point.
        !> Every neighbouring pair once, as two parallel index arrays with `i < j`.
        !!
        !! SYMMETRIC: a pair qualifies when EITHER ball reaches the other, `d <= max(r_i, r_j)`,
        !! so it does not matter which endpoint would have been doing the searching.
        generic :: pairs_within => bind_pairs_within_r0, bind_pairs_within_r1
        procedure, private :: bind_count_all_r0 !! %count_all_within with one radius.
        procedure, private :: bind_count_all_r1 !! %count_all_within with one radius per point.
        !> How many neighbours each point has, without materialising them.
        !!
        !! DIRECTED, exactly as `%all_within` -- it is that query's row lengths. Do NOT use it to
        !! size a `%pairs_within` result: the two count different things under a per-point radius.
        generic :: count_all_within => bind_count_all_r0, bind_count_all_r1
        procedure, private :: bind_all_sky_r0 !! %all_within_sky with one angular radius.
        procedure, private :: bind_all_sky_r1 !! %all_within_sky with one angular radius per point.
        !> Every point's neighbours on the sky, as one CSR structure. Radii in degrees.
        generic :: all_within_sky => bind_all_sky_r0, bind_all_sky_r1
        procedure, private :: bind_pairs_sky_r0 !! %pairs_within_sky with one angular radius.
        procedure, private :: bind_pairs_sky_r1 !! %pairs_within_sky with one angular radius per point.
        !> Every neighbouring pair on the sky once, with `i < j`. Radii in degrees.
        generic :: pairs_within_sky => bind_pairs_sky_r0, bind_pairs_sky_r1
        procedure, private :: bind_count_all_sky_r0 !! %count_all_within_sky with one angular radius.
        procedure, private :: bind_count_all_sky_r1 !! %count_all_within_sky with one per point.
        !> How many neighbours each point has on the sky. Radii in degrees.
        generic :: count_all_within_sky => bind_count_all_sky_r0, bind_count_all_sky_r1
        procedure, private :: bind_near_k32_i32 !! %nearest, int32 k, int32 buffer.
        procedure, private :: bind_near_k32_i64 !! %nearest, int32 k, int64 buffer.
        procedure, private :: bind_near_k64_i32 !! %nearest, int64 k, int32 buffer.
        procedure, private :: bind_near_k64_i64 !! %nearest, int64 k, int64 buffer.
        !> The `k` points nearest `p`, always ordered by increasing distance.
        generic :: nearest => bind_near_k32_i32, bind_near_k32_i64, bind_near_k64_i32, bind_near_k64_i64
        procedure, private :: bind_near_sky_k32_i32 !! %nearest_sky, int32 k, int32 buffer.
        procedure, private :: bind_near_sky_k32_i64 !! %nearest_sky, int32 k, int64 buffer.
        procedure, private :: bind_near_sky_k64_i32 !! %nearest_sky, int64 k, int32 buffer.
        procedure, private :: bind_near_sky_k64_i64 !! %nearest_sky, int64 k, int64 buffer.
        !> The `k` points nearest `(ra, dec)` on the sky, ordered by increasing separation.
        generic :: nearest_sky => bind_near_sky_k32_i32, bind_near_sky_k32_i64, &
            bind_near_sky_k64_i32, bind_near_sky_k64_i64
        procedure, private :: bind_kth_k32 !! %kth_distance with an int32 k.
        procedure, private :: bind_kth_k64 !! %kth_distance with an int64 k.
        !> Every point's distance to its `k`-th nearest OTHER point, at once.
        generic :: kth_distance => bind_kth_k32, bind_kth_k64
        procedure, private :: bind_kth_sky_k32 !! %kth_distance_sky with an int32 k.
        procedure, private :: bind_kth_sky_k64 !! %kth_distance_sky with an int64 k.
        !> Every point's angular distance to its `k`-th nearest OTHER point, in degrees.
        generic :: kth_distance_sky => bind_kth_sky_k32, bind_kth_sky_k64
    end type pf_spatial_index

    ! ---- Test-only state. Process-global by necessity: a hook over Fortran-side state has no ----
    ! ---- C++ side to hide in, which is why these are public procedures over saved variables.  ----

    !> Cell size forced by `parquet_debug_set_spatial_cell`; <= 0 means "not forced".
    real(real64), save :: dbg_cell = -1.0_real64
    !> How many candidates the most recent tuning run evaluated. 0 means the probe did not run.
    integer(int64), save :: dbg_probe_count = 0_int64
    !> How many automatic rebuilds have happened since the counters were reset.
    integer(int64), save :: dbg_rebuilds = 0_int64
    !> HEALPix resolution forced by `parquet_debug_set_spatial_nside`; <= 0 means "not forced".
    integer(int64), save :: dbg_nside = 0_int64
    !> Run-buffer columns the HEALPix walk may use before it allocates; <= 0 means "the whole
    !! stack buffer".
    !!
    !! Forced by `parquet_debug_set_spatial_run_buffer`, and the only way to reach the walk's
    !! allocating fallback at a test-sized fixture: overflowing the real 512-column buffer needs a
    !! disc spanning more than 256 rings, which in turn needs a resolution the buckets-per-point
    !! cap will not grant to anything smaller than about a million points.
    integer(int64), save :: dbg_run_buf = 0_int64
    !> Pixels the HEALPix candidate walk has covered since the counters were reset.
    !!
    !! Written ONLY by the HEALPix arm of `spatial_scan`, deliberately: staying at zero is what
    !! makes it a discriminator between the two backends rather than merely a measurement, and it
    !! keeps the shipped 3D walk's inner loop untouched. Updated atomically because the bulk
    !! sweeps call the scan from an OpenMP team; it is still process-global, so it is meaningful
    !! for a serial query and the `spatial` suite is already excluded from test-drive's own
    !! parallelism.
    integer(int64), save :: dbg_pixels_visited = 0_int64
    !> The team size the most recent bulk query resolved.
    integer, save :: dbg_threads_used = 0
    !> Initial shell radius forced by `parquet_debug_set_spatial_shell_start`; <= 0 means "not forced".
    real(real64), save :: dbg_shell_start = -1.0_real64
    !> Expansion rounds accumulated by every shell search since the counters were reset.
    integer(int64), save :: dbg_shell_rounds = 0_int64

    ! ---- Shared argument checks (parquet_spatial_build.f90) ----

    interface
        !> Aborts unless every element of `a` is a finite number.
        !!
        !! **A non-finite coordinate is refused rather than carried, and the reason is not
        !! tidiness.** Nothing downstream can place one: the grid index is `int((v - lo) * inv)`,
        !! and `int()` of a NaN raises IEEE_INVALID, as do the `min`/`max` this module uses to
        !! clamp a walk to the grid -- both compile to instructions (`cvttsd2si`,
        !! `minsd`/`maxsd`) that signal on a quiet NaN. nagfor unmasks the IEEE traps by default
        !! (`-ieee=stop`), so before this check a NaN coordinate killed the process with
        !! "Arithmetic exception" naming nothing at all, and every other compiler in the fleet
        !! silently returned an answer computed from a garbage cell index. The module already
        !! refuses a NaN radius, a NaN `dec` and a NaN HEALPix disc centre; this closes the same
        !! gap for the coordinates and the query point.
        !!
        !! `abs(a) <= huge(...)` rather than `ieee_is_finite`: it is quiet on a NaN (an ordinary
        !! comparison, not a `min`), it rejects both infinities in the same test, and it is the
        !! idiom the rest of the library already screens infinities with.
        module subroutine spatial_check_finite(a, what, argname)
            real(real64), intent(in) :: a(:) !! the values to check; may be empty.
            character(len=*), intent(in) :: what !! the entry point's name, for the message.
            character(len=*), intent(in) :: argname !! what the values are, for the message.
        end subroutine spatial_check_finite
    end interface

    ! ---- Build, rebuild and the grid itself (parquet_spatial_build.f90) ----

    interface
        !> Builds `self` over the caller's coordinates. The single worker every `%build` specific
        !! reaches, taking the radius hint as an already-flattened array.
        module subroutine spatial_build_worker(self, x, y, z, radii, cell, box_lo, box_hi, copy, &
                                              backend, nside)
            type(pf_spatial_index), intent(inout), target :: self !! the index to fill.
            real(real64), intent(in), target :: x(:) !! x of every point.
            real(real64), intent(in), target :: y(:) !! y of every point.
            real(real64), intent(in), optional, target :: z(:) !! z; absent gives a 2D index.
            real(real64), intent(in) :: radii(:) !! the radii later queries will use; all must be > 0.
            real(real64), intent(in), optional :: cell !! an explicit cell side; disables tuning.
            real(real64), intent(in), optional :: box_lo(:) !! periodic box corner; with box_hi turns wrapping on.
            real(real64), intent(in), optional :: box_hi(:) !! the opposite periodic box corner.
            logical, intent(in), optional :: copy !! .false. points at the caller's arrays instead of copying.
            !> `PF_SKY_GRID3D` (default) or `PF_SKY_HEALPIX`. Passed only by
            !! `spatial_build_sky_worker`: `%build` takes no `backend=` at all, a Euclidean cloud
            !! having no sphere to pixelate. It arrives here rather than being set afterwards
            !! because `spatial_clear_worker` runs first and resets it, and because the grid
            !! choice and the bucketing key both have to know before they run.
            integer, intent(in), optional :: backend
            integer(int64), intent(in), optional :: nside !! forced HEALPix resolution; disables tuning.
        end subroutine spatial_build_worker

        !> Rebuilds `self` from `x`, `y`, `z` unless they are element-for-element what it already
        !! holds, in which case it only widens the radius record.
        !> Builds a sky index: `(ra, dec)` in degrees onto unit vectors, angles onto chords.
        !!
        !! **The conversion is exact, not an approximation.** `chord = 2*sin(theta/2)` is strictly
        !! increasing in `theta` over [0, 180 degrees], so a Euclidean ball of that radius in
        !! unit-vector space selects exactly the points within `theta` on the sky -- with no pole
        !! special case and no wrap at 0h, because the sphere has neither.
        !> Fixes the HEALPix resolution for a query chord, coarsening to keep the counting path.
        !>
        !> The pixel counterpart of `spatial_set_grid`, and it answers the same question: what
        !> bucket size serves a query of this size, subject to the bucket count staying under
        !> `spatial_max_cells_per_point * npts` so the bucketing sort keeps `pf_argsort`'s counting
        !> fast path. Sets `nside_v`, `npix_v` and `n_cells`, and zeroes the 3D grid's own fields
        !> so that `%cell_size`, `%cell_sides` and `%grid` report a value no valid grid ever has.
        module subroutine spatial_set_nside(self, want, coarsened)
            type(pf_spatial_index), intent(inout) :: self !! the index whose resolution is being set.
            integer(int64), intent(in) :: want !! the resolution asked for; clamped by the cap.
            logical, intent(out), optional :: coarsened !! whether the cap bound before `want` did.
        end subroutine spatial_set_nside

        module subroutine spatial_build_sky_worker(self, ra, dec, radii_deg, cell, backend, nside)
            type(pf_spatial_index), intent(inout), target :: self !! the index to build.
            real(real64), intent(in) :: ra(:) !! right ascension of every point, in degrees.
            real(real64), intent(in) :: dec(:) !! declination of every point, in degrees.
            real(real64), intent(in) :: radii_deg(:) !! the angular radii later queries will use.
            real(real64), intent(in), optional :: cell !! forced cell side, in unit-vector space.
            integer, intent(in), optional :: backend !! PF_SKY_GRID3D (default) or PF_SKY_HEALPIX.
            integer(int64), intent(in), optional :: nside !! forced HEALPix resolution; disables tuning.
        end subroutine spatial_build_sky_worker

        module subroutine spatial_rebuild_worker(self, x, y, z, radii, rebuilt)
            type(pf_spatial_index), intent(inout), target :: self !! the index to validate.
            real(real64), intent(in), target :: x(:) !! x of every point.
            real(real64), intent(in), target :: y(:) !! y of every point.
            real(real64), intent(in), optional, target :: z(:) !! z; must match the built rank.
            real(real64), intent(in), optional :: radii(:) !! extra radii to fold into the record.
            logical, intent(out), optional :: rebuilt !! .true. when the data had changed.
        end subroutine spatial_rebuild_worker

        !> Re-tunes the cell size for `radii` over the points already stored, without the caller
        !! supplying them again.
        module subroutine spatial_rebuild_for_worker(self, radii, union)
            type(pf_spatial_index), intent(inout), target :: self !! the index to re-tune.
            real(real64), intent(in) :: radii(:) !! the radii to tune for; all must be > 0.
            logical, intent(in) :: union !! .true. adds to the recorded radii, .false. replaces them.
        end subroutine spatial_rebuild_for_worker

        !> Releases every array and returns `self` to its default, unbuilt state.
        module subroutine spatial_clear_worker(self)
            type(pf_spatial_index), intent(inout) :: self !! the index to empty.
        end subroutine spatial_clear_worker

        !> Fixes the grid dimensions for cell side `h`, coarsening it when the cell count would
        !! exceed what keeps the bucketing on `pf_argsort`'s counting path.
        module subroutine spatial_set_grid(self, h, coarsened)
            type(pf_spatial_index), intent(inout) :: self !! the index whose grid is being fixed.
            real(real64), intent(in) :: h !! the requested cell side.
            logical, intent(out), optional :: coarsened !! .true. when the clamp raised it.
        end subroutine spatial_set_grid

        !> Cells per axis, the per-axis cell side and its reciprocal, for one candidate cell size.
        !! Split out from `spatial_set_grid` so the probe can size a candidate without touching the
        !! index it is choosing for.
        module subroutine spatial_grid_dims(lo, hi, wrap, h, nc, cell, cell_inv)
            real(real64), intent(in) :: lo(3) !! grid origin.
            real(real64), intent(in) :: hi(3) !! opposite corner.
            real(real64), intent(in) :: wrap(3) !! box length on a periodic axis, 0 on a free one.
            real(real64), intent(in) :: h !! candidate cell side.
            integer(int64), intent(out) :: nc(3) !! cells along each axis, at least 1.
            real(real64), intent(out) :: cell(3) !! cell side per axis; equals `h` unless the axis wraps.
            real(real64), intent(out) :: cell_inv(3) !! 1/cell.
        end subroutine spatial_grid_dims

        !> Buckets the stored points into the current grid, filling `start` and `idx`.
        module subroutine spatial_bucket(self)
            type(pf_spatial_index), intent(inout), target :: self !! the index to bucket.
        end subroutine spatial_bucket

        !> Aims three local pointers at wherever the coordinates actually live.
        !!
        !! One pointer assignment per query, and nothing in the inner loop knows whether the index
        !! owns its coordinates or borrows them -- which is the whole reason `copy=` costs no branch
        !! in the hot loop.
        module subroutine spatial_storage(self, xs, ys, zs)
            type(pf_spatial_index), intent(in), target :: self !! the index holding or borrowing the arrays.
            real(real64), pointer, contiguous, intent(out) :: xs(:) !! x of stored point k.
            real(real64), pointer, contiguous, intent(out) :: ys(:) !! y of stored point k.
            real(real64), pointer, contiguous, intent(out) :: zs(:) !! z of stored point k.
        end subroutine spatial_storage

        !> Folds a radius list into the index's two accumulators, `sum(r^2)` and `sum(r^3)`.
        module subroutine spatial_fold_radii(self, radii, union)
            type(pf_spatial_index), intent(inout) :: self !! the index whose record is widened.
            real(real64), intent(in) :: radii(:) !! the radii to record.
            logical, intent(in) :: union !! .true. adds to what is recorded, .false. replaces it.
        end subroutine spatial_fold_radii

        !> Re-chooses the cell size for the recorded radii and re-buckets the stored points.
        module subroutine spatial_retune(self)
            type(pf_spatial_index), intent(inout), target :: self !! the index to re-tune in place.
        end subroutine spatial_retune

        !> The cell a point falls in, 1-based, for the index's current grid.
        module function spatial_cell_of(self, px, py, pz) result(c)
            type(pf_spatial_index), intent(in) :: self !! the index supplying the grid.
            real(real64), intent(in) :: px !! x of the point.
            real(real64), intent(in) :: py !! y of the point.
            real(real64), intent(in) :: pz !! z of the point.
            integer(int64) :: c !! the 1-based linear cell index.
        end function spatial_cell_of
    end interface

    ! ---- Cell-size tuning: the model, the density and the deterministic probe ----
    ! ---- (parquet_spatial_tune.f90)                                            ----

    interface
        !> Chooses the cell side for `radii` over the points `self` holds: cost model for a bracket,
        !! then a deterministic work-count probe to pick from it.
        module subroutine spatial_choose_cell(self, radii, h)
            type(pf_spatial_index), intent(inout), target :: self !! the index being built; its grid is scratch here.
            real(real64), intent(in) :: radii(:) !! the radii later queries will use.
            real(real64), intent(out) :: h !! the chosen cell side.
        end subroutine spatial_choose_cell

        !> Chooses the HEALPix resolution for `chords` over the points `self` holds.
        !>
        !> **The same deterministic work-count probe the cell tuner uses, over the same cost model
        !> and the same draw of query points** -- only the candidates differ, being powers of two
        !> rather than a continuous bracket. Reusing it is what keeps a backend comparison honest:
        !> both are ranked by `spatial_work_a * buckets + spatial_work_b * points`, so neither is
        !> tuned against a yardstick the other never saw.
        !>
        !> **A rule alone would have been wrong, which is why this measures.** Matching the pixel
        !> to the radius is right at small radii and demonstrably not at large ones -- at a five
        !> degree radius a resolution several steps FINER than radius-matched tests less than half
        !> as many candidates, because a coarse pixel then holds far more points than the disc can
        !> use. The candidate set therefore reaches from well below the radius-matched value up to
        !> the buckets-per-point cap.
        module subroutine spatial_choose_nside(self, chords, nside)
            type(pf_spatial_index), intent(inout), target :: self !! the index being built.
            real(real64), intent(in) :: chords(:) !! the query radii, as chords in unit-vector space.
            integer(int64), intent(out) :: nside !! the chosen resolution parameter.
        end subroutine spatial_choose_nside

        !> The cost model's cell size: `h = (kappa^4 * r_eff / ((4/3) pi rho))^(1/(1+ndim))`.
        !!
        !! Derived by minimising `c_cell*((2r+h)/h)^3 + c_point*(2r+h)^3*rho` over `h`. The exponent
        !! carries the dimension: 1/4 in 3D, 1/3 in 2D, 1/2 in 1D -- which falls out of the same
        !! formula once `rho` is a density in the cloud's own dimension rather than in the bounding
        !! box, so there is no 2D branch anywhere.
        module function spatial_model_cell(ndim, rho, r_eff) result(h)
            integer, intent(in) :: ndim !! dimensions the points actually occupy: 1, 2 or 3.
            real(real64), intent(in) :: rho !! points per unit ndim-volume, from the median occupied cell.
            real(real64), intent(in) :: r_eff !! the effective query radius.
            real(real64) :: h !! the model's cell side.
        end function spatial_model_cell

        !> Measures the local density from the median occupied cell of a provisional coarse grid,
        !! and reports how many dimensions the cloud actually occupies.
        !!
        !! **Never `n / bounding-box volume`**, which describes nowhere for clumped data and
        !! understates the density of a sparse geometry. The median over OCCUPIED cells is a
        !! statement about where the points are.
        module subroutine spatial_measure_density(self, ndim, rho)
            type(pf_spatial_index), intent(in), target :: self !! the index holding the points.
            integer, intent(out) :: ndim !! non-degenerate axes: 1, 2 or 3.
            real(real64), intent(out) :: rho !! points per unit ndim-volume.
        end subroutine spatial_measure_density

        !> The two numbers the probe ranks a candidate cell by, for any cell size, over the
        !! points an index already holds: how many cells a query would visit and how many points
        !! it would distance-test.
        !!
        !! **Test-only, and public because the A/B constant has to be re-derivable on a machine
        !! this project has not measured.** `spatial_work_a / spatial_work_b` was fitted on machine
        !! A (arm64, NEON) and is a ratio of a cache-miss-ish cost to an arithmetic-ish one -- the
        !! two things most likely to differ on AVX2 or AVX-512. Pairing these counts with a timed
        !! sweep is what lets `bench/benchmark_spatial.sh --mode=ab` re-fit it elsewhere without
        !! reimplementing the walk. No library code calls it.
        !!
        !! **It takes the radius LIST, not one radius, because the probe does.** A fit that counted
        !! work at one radius and timed queries at another would be fitting the constant against a
        !! workload the probe never faces -- which is exactly the mistake the first version of
        !! `--mode=ab` made.
        module subroutine parquet_debug_spatial_work(index, h, radius, cells, points)
            type(pf_spatial_index), intent(in), target :: index !! an index holding the points to probe.
            real(real64), intent(in) :: h !! the candidate cell side.
            real(real64), intent(in) :: radius(:) !! the radius LIST, exactly as `%build` was given it.
            integer(int64), intent(out) :: cells !! cells the probe queries would visit in total.
            integer(int64), intent(out) :: points !! points they would distance-test in total.
        end subroutine parquet_debug_spatial_work

        !> `r_eff = sum(r^3) / sum(r^2)`, the one radius a whole list collapses to.
        module function spatial_effective_radius(radii) result(r)
            real(real64), intent(in) :: radii(:) !! the radius list; must be non-empty and positive.
            real(real64) :: r !! the effective radius.
        end function spatial_effective_radius
    end interface

    ! ---- Ball search (parquet_spatial_query.f90) ----

    interface
        !> Walks the cells a ball of radius `r` about `p` can reach and reports what it finds.
        !!
        !! The one scan every query family goes through. `out32`/`out64`/`dist` are filled only as
        !! far as they reach; `m` is always the TRUE count, so a caller can size a buffer and retry.
        !!
        !! **`min_key`/`keys` are how a pair sweep emits each pair from exactly one of its two
        !! endpoints.** `keys` gives every point a distinct ORDER KEY, addressed by stored position
        !! so it is read with the same locality as the coordinates, and the walk reports only points
        !! whose key is strictly above `min_key`. The two are present together or not at all --
        !! `keys` is not consulted unless `min_key` is given. A single-radius sweep passes the
        !! caller's row index as the key; a per-point-radius sweep passes a rank that orders the
        !! points by DESCENDING radius, which is what makes the endpoint doing the searching always
        !! the one whose ball is large enough to reach the other. See `spatial_pairs_within_worker`.
        module subroutine spatial_scan(self, p, r, m, out32, out64, dist, min_key, keys, r_inner, sorted)
            type(pf_spatial_index), intent(in), target :: self !! the index to search.
            real(real64), intent(in) :: p(3) !! the query point; p(3) is ignored by a 2D index.
            real(real64), intent(in) :: r !! the search radius; must be >= 0.
            integer(int64), intent(out) :: m !! how many points are within `r`, whatever the buffer holds.
            integer(int32), intent(inout), optional :: out32(:) !! caller's row indices, int32 buffer.
            integer(int64), intent(inout), optional :: out64(:) !! caller's row indices, int64 buffer.
            real(real64), intent(inout), optional :: dist(:) !! distance to each reported point.
            integer(int64), intent(in), optional :: min_key !! accept only points whose key is above this.
            integer(int64), intent(in), optional :: keys(:) !! order key per STORED position; needs `min_key`.
            real(real64), intent(in), optional :: r_inner !! an inner radius; makes the ball an annulus.
            logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        end subroutine spatial_scan

        !> Walks the cells an axis-shaped region can reach and reports what it finds.
        !!
        !! **One walk and one accept test serve all three shapes**, because a cylinder is the cone
        !! with `r1 == r2` and a capsule is the cone's test with the axis parameter CLAMPED to
        !! [0, 1] instead of rejected outside it. Writing any of the three separately means writing
        !! the cone twice.
        !!
        !! **The walk is per-SLAB along the axis's dominant direction, not over the region's
        !! bounding box.** A long diagonal has an AABB equal to the whole grid, so a bounding-box
        !! walk degenerates to a full scan exactly when the shape is most selective. Each slab
        !! intersects the axis, pads the sub-segment by the largest radius over that slab's own
        !! parameter range, and derives the other two axes' cell ranges from that alone -- so every
        !! cell is visited at most once and there is nothing to de-duplicate.
        module subroutine spatial_scan_axis(self, p1, p2, r1, r2, clamp, what, m, out32, out64, dist, &
            axis_point, axis_t, sorted)
            type(pf_spatial_index), intent(in), target :: self !! the index to search.
            real(real64), intent(in) :: p1(3) !! one end of the axis; p1(3) is 0 on a 2D index.
            real(real64), intent(in) :: p2(3) !! the other end of the axis.
            real(real64), intent(in) :: r1 !! radius at `p1`; must be >= 0.
            real(real64), intent(in) :: r2 !! radius at `p2`; must be >= 0.
            logical, intent(in) :: clamp !! .true. gives round ends (a capsule), .false. flat ones.
            character(len=*), intent(in) :: what !! the calling procedure, for any message.
            integer(int64), intent(out) :: m !! how many points qualify, whatever the buffer holds.
            integer(int32), intent(inout), optional :: out32(:) !! caller's row indices, int32 buffer.
            integer(int64), intent(inout), optional :: out64(:) !! caller's row indices, int64 buffer.
            real(real64), intent(inout), optional :: dist(:) !! distance to the axis, or to the segment.
            real(real64), intent(inout), optional :: axis_point(:,:) !! `(ndim, m)`: the point `dist` was measured from.
            real(real64), intent(inout), optional :: axis_t(:) !! where on the axis that point sits, in [0, 1].
            logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        end subroutine spatial_scan_axis

        !> Orders a query's results by increasing distance, ties broken by ascending row index.
        !!
        !! **The tie-break is explicit and is a CONTRACT, not a detail.** Without it the order
        !! among equal distances is the order the walk produced, which is cell order -- and cell
        !! order depends on the tuned cell size, which depends on the machine. A caller who asks
        !! for a canonical order would then get a different one on a different machine.
        module subroutine spatial_order_by_dist(nfill, d, out32, out64, axis_point, axis_t)
            integer(int64), intent(in) :: nfill !! how many leading entries were actually written.
            real(real64), intent(inout) :: d(:) !! the distances: the sort key, permuted in place.
            integer(int32), intent(inout), optional :: out32(:) !! int32 row buffer, permuted with it.
            integer(int64), intent(inout), optional :: out64(:) !! int64 row buffer, permuted with it.
            real(real64), intent(inout), optional :: axis_point(:,:) !! axis feet, permuted with it.
            real(real64), intent(inout), optional :: axis_t(:) !! axis parameters, permuted with it.
        end subroutine spatial_order_by_dist

        !> The `kk` nearest points to `p`, by a ball that expands until it holds enough of them.
        !!
        !! **Exact, and the argument is worth keeping in front of whoever changes this.** Once a
        !! ball of radius `r` has returned `m >= kk` points, the `kk`-th smallest of their
        !! distances is at most `r`; every point closer than that is therefore inside `r` and was
        !! found; so the `kk` nearest are exactly the `kk` smallest of what the ball returned.
        !! **A walk that expanded in rings of CELLS and stopped at `kk` candidates would be
        !! wrong**, because a cell's far corner is further away than an unvisited cell's near
        !! face. The accept test here is an exact ball at a known radius, which is what makes the
        !! argument hold; do not replace it with a cell-count criterion.
        module subroutine spatial_shell_search(self, p, kk, r_seed, rows, dists, what)
            type(pf_spatial_index), intent(in), target :: self !! the index to search.
            real(real64), intent(in) :: p(3) !! the query point, already widened to three coordinates.
            integer(int64), intent(in) :: kk !! how many neighbours are wanted; must be 1 <= kk <= npts.
            real(real64), intent(inout) :: r_seed !! in: a starting radius, or <= 0 to derive one. out: what converged.
            integer(int64), allocatable, intent(inout) :: rows(:) !! scratch, grown as needed; `rows(1:kk)` on return.
            real(real64), allocatable, intent(inout) :: dists(:) !! scratch, grown as needed; `dists(1:kk)` on return.
            character(len=*), intent(in) :: what !! the calling procedure, for any message.
        end subroutine spatial_shell_search

        !> Counts the cells a ball would visit and the points it would distance-test, without
        !! testing any of them. The probe's whole measurement.
        module subroutine spatial_scan_work(nc, lo, cell_inv, start, wrap, p, r, cells, pts)
            integer(int64), intent(in) :: nc(3) !! cells along each axis.
            real(real64), intent(in) :: lo(3) !! grid origin.
            real(real64), intent(in) :: cell_inv(3) !! 1/cell per axis.
            integer(int64), intent(in) :: start(:) !! the candidate grid's dense prefix sums.
            real(real64), intent(in) :: wrap(3) !! box length on a periodic axis, 0 on a free one.
            real(real64), intent(in) :: p(3) !! the query point.
            real(real64), intent(in) :: r !! the search radius.
            integer(int64), intent(inout) :: cells !! accumulated cells visited.
            integer(int64), intent(inout) :: pts !! accumulated points that would be tested.
        end subroutine spatial_scan_work
    end interface

    ! ---- Bulk queries (parquet_spatial_bulk.f90) ----

    interface
        !> Every point's neighbours as CSR: row `i` occupies `neighbours(offsets(i):offsets(i+1)-1)`.
        module subroutine spatial_all_within_worker(self, radii, offsets, neighbours, expect_metric, &
            threads, radii_inner, sorted)
            type(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
            real(real64), intent(in) :: radii(:) !! one radius, or one per point, in the index's own units.
            integer(int64), allocatable, intent(out) :: offsets(:) !! length n+1, `offsets(1) == 1`.
            integer(int64), allocatable, intent(out) :: neighbours(:) !! the concatenated neighbour lists.
            integer, intent(in) :: expect_metric !! the metric this call's radii are stated in.
            integer, intent(in), optional :: threads !! team size; absent resolves automatically.
            real(real64), intent(in), optional :: radii_inner(:) !! inner radii, making each ball an annulus.
            logical, intent(in), optional :: sorted !! .true. orders each row by increasing distance.
        end subroutine spatial_all_within_worker

        !> Every neighbouring pair exactly once, with `i < j` in the caller's row numbering.
        module subroutine spatial_pairs_within_worker(self, radii, ii, jj, expect_metric, threads, r_inner)
            type(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
            real(real64), intent(in) :: radii(:) !! one radius, or one per point, in the index's own units.
            integer(int64), allocatable, intent(out) :: ii(:) !! the lower row index of each pair.
            integer(int64), allocatable, intent(out) :: jj(:) !! the higher row index of each pair.
            integer, intent(in) :: expect_metric !! the metric this call's radii are stated in.
            integer, intent(in), optional :: threads !! team size; absent resolves automatically.
            real(real64), intent(in), optional :: r_inner !! an inner radius; SCALAR only, see the worker.
        end subroutine spatial_pairs_within_worker

        !> How many neighbours each point has, in the caller's row order.
        module subroutine spatial_count_all_worker(self, radii, counts, expect_metric, threads, radii_inner)
            type(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
            real(real64), intent(in) :: radii(:) !! one radius, or one per point, in the index's own units.
            integer(int64), allocatable, intent(out) :: counts(:) !! length n, in the caller's row order.
            integer, intent(in) :: expect_metric !! the metric this call's radii are stated in.
            integer, intent(in), optional :: threads !! team size; absent resolves automatically.
            real(real64), intent(in), optional :: radii_inner(:) !! inner radii, making each ball an annulus.
        end subroutine spatial_count_all_worker

        !> The distance from every point to its `k`-th nearest OTHER point, in the caller's row order.
        module subroutine spatial_kth_worker(self, k, dist, expect_metric, threads)
            type(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
            integer(int64), intent(in) :: k !! which neighbour to report; 1 <= k <= npts-1.
            real(real64), allocatable, intent(out) :: dist(:) !! length n, in the index's own units.
            integer, intent(in) :: expect_metric !! the metric the caller stated `k` for.
            integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        end subroutine spatial_kth_worker

        !> Labels the connected components of an undirected graph given as an edge list.
        module subroutine spatial_components_worker(i, j, nvert, labels, ncomp, sizes, min_size)
            integer(int64), intent(in) :: i(:) !! one endpoint of each edge.
            integer(int64), intent(in) :: j(:) !! the other endpoint of each edge.
            integer(int64), intent(in) :: nvert !! how many vertices the graph has.
            integer(int64), allocatable, intent(out) :: labels(:) !! length nvert; 0 where nothing qualifies.
            integer(int64), intent(out), optional :: ncomp !! how many components earned a label.
            integer(int64), allocatable, intent(out), optional :: sizes(:) !! length ncomp, in label order.
            integer, intent(in), optional :: min_size !! smallest component that earns a label; default 1.
        end subroutine spatial_components_worker

        !> Rebuilds `self` when `radii` would choose a cell more than `spatial_rebuild_factor` from
        !! the one in use, warning once per index unless the warning is switched off.
        !!
        !! **Only a BULK entry point may call this**, and only before it opens a parallel region: a
        !! query that can mutate the index is not read-only, so two threads could rebuild at once
        !! and race on the same arrays. That is a design rule rather than a mechanism, which is why
        !! it is written here rather than left to be rediscovered.
        module subroutine spatial_maybe_rebuild(self, radii)
            type(pf_spatial_index), intent(inout), target :: self !! the index that may be re-tuned.
            real(real64), intent(in) :: radii(:) !! the radii this bulk query is about to use.
        end subroutine spatial_maybe_rebuild

        !> How many threads a bulk query should open, after every cap and the affinity clamp.
        module function spatial_threads(threads) result(n)
            integer, intent(in), optional :: threads !! an explicit request; absent resolves automatically.
            integer :: n !! the team size to open; at least 1.
        end function spatial_threads
    end interface

contains

    ! ---- %build ----

    !> `%build` with a single radius hint.
    subroutine bind_build_r0(self, x, y, z, radius, cell, box_lo, box_hi, copy)
        class(pf_spatial_index), intent(inout), target :: self !! the index to fill.
        real(real64), intent(in), target :: x(:) !! x of every point.
        real(real64), intent(in), target :: y(:) !! y of every point.
        real(real64), intent(in), optional, target :: z(:) !! z; absent builds a 2D index.
        real(real64), intent(in) :: radius !! the radius later queries will use; must be > 0.
        real(real64), intent(in), optional :: cell !! an explicit cell side; disables tuning entirely.
        real(real64), intent(in), optional :: box_lo(:) !! periodic box corner; with box_hi turns wrapping on.
        real(real64), intent(in), optional :: box_hi(:) !! the opposite periodic box corner.
        logical, intent(in), optional :: copy !! .false. points at the caller's arrays; default .true.

        call spatial_build_worker(self, x, y, z, [radius], cell, box_lo, box_hi, copy)
    end subroutine bind_build_r0

    !> `%build` with a list of radii; the list collapses to `r_eff = sum(r^3)/sum(r^2)`.
    subroutine bind_build_r1(self, x, y, z, radius, cell, box_lo, box_hi, copy)
        class(pf_spatial_index), intent(inout), target :: self !! the index to fill.
        real(real64), intent(in), target :: x(:) !! x of every point.
        real(real64), intent(in), target :: y(:) !! y of every point.
        real(real64), intent(in), optional, target :: z(:) !! z; absent builds a 2D index.
        real(real64), intent(in) :: radius(:) !! the radii later queries will use; all must be > 0.
        real(real64), intent(in), optional :: cell !! an explicit cell side; disables tuning entirely.
        real(real64), intent(in), optional :: box_lo(:) !! periodic box corner; with box_hi turns wrapping on.
        real(real64), intent(in), optional :: box_hi(:) !! the opposite periodic box corner.
        logical, intent(in), optional :: copy !! .false. points at the caller's arrays; default .true.

        call spatial_build_worker(self, x, y, z, radius, cell, box_lo, box_hi, copy)
    end subroutine bind_build_r1

    !> `%build_sky` with a single angular radius.
    !>
    !> **`(ra, dec)` in degrees; everything after that is Cartesian.** The points become unit
    !> vectors and the angular radius becomes a chord, after which this is an ordinary 3D index --
    !> the same tuner, the same bucketing, the same walk. That is the whole design: a sky index is
    !> a MODE of one index type, not a second type that would duplicate all of it to change two
    !> formulas.
    !>
    !> **There is no `copy=`, and that is not an omission.** The stored coordinates are unit
    !> vectors this routine computes; there is nothing of the caller's to borrow, so a
    !> `copy=.false.` that silently copied would be worse than not offering it.
    !>
    !> `cell=` and `%cell_size()` are both in unit-vector space rather than degrees, so they are a
    !> matched pair and a value read from one can be fed back into the other. `%effective_radius()`
    !> is the one that comes back in DEGREES, because it is a radius the caller gave in degrees.
    subroutine bind_build_sky_r0(self, ra, dec, radius_deg, cell, backend, nside)
        class(pf_spatial_index), intent(inout), target :: self !! the index to build.
        real(real64), intent(in) :: ra(:) !! right ascension of every point, in degrees.
        real(real64), intent(in) :: dec(:) !! declination of every point, in degrees; |dec| <= 90.
        real(real64), intent(in) :: radius_deg !! the angular radius later queries will use.
        real(real64), intent(in), optional :: cell !! forced cell side, in unit-vector space.
        integer, intent(in), optional :: backend !! PF_SKY_GRID3D (default) or PF_SKY_HEALPIX.
        integer(int64), intent(in), optional :: nside !! forced HEALPix resolution; disables tuning.

        call spatial_build_sky_worker(self, ra, dec, [radius_deg], cell, backend, nside)
    end subroutine bind_build_sky_r0

    !> `%build_sky` with a list of angular radii. See `bind_build_sky_r0`.
    subroutine bind_build_sky_r1(self, ra, dec, radius_deg, cell, backend, nside)
        class(pf_spatial_index), intent(inout), target :: self !! the index to build.
        real(real64), intent(in) :: ra(:) !! right ascension of every point, in degrees.
        real(real64), intent(in) :: dec(:) !! declination of every point, in degrees; |dec| <= 90.
        real(real64), intent(in) :: radius_deg(:) !! the angular radii later queries will use.
        real(real64), intent(in), optional :: cell !! forced cell side, in unit-vector space.
        integer, intent(in), optional :: backend !! PF_SKY_GRID3D (default) or PF_SKY_HEALPIX.
        integer(int64), intent(in), optional :: nside !! forced HEALPix resolution; disables tuning.

        call spatial_build_sky_worker(self, ra, dec, radius_deg, cell, backend, nside)
    end subroutine bind_build_sky_r1

    ! ---- %rebuild and %rebuild_for ----

    !> `%rebuild` with a single radius hint.
    subroutine bind_rebuild_r0(self, x, y, z, radius, rebuilt)
        class(pf_spatial_index), intent(inout), target :: self !! the index to validate.
        real(real64), intent(in), target :: x(:) !! x of every point.
        real(real64), intent(in), target :: y(:) !! y of every point.
        real(real64), intent(in), optional, target :: z(:) !! z; must match the built rank.
        real(real64), intent(in), optional :: radius !! a radius to fold into the record.
        logical, intent(out), optional :: rebuilt !! .true. when the data had changed.

        if (present(radius)) then
            call spatial_rebuild_worker(self, x, y, z, [radius], rebuilt)
        else
            call spatial_rebuild_worker(self, x, y, z, rebuilt=rebuilt)
        end if
    end subroutine bind_rebuild_r0

    !> `%rebuild` with a list of radii.
    subroutine bind_rebuild_r1(self, x, y, z, radius, rebuilt)
        class(pf_spatial_index), intent(inout), target :: self !! the index to validate.
        real(real64), intent(in), target :: x(:) !! x of every point.
        real(real64), intent(in), target :: y(:) !! y of every point.
        real(real64), intent(in), optional, target :: z(:) !! z; must match the built rank.
        real(real64), intent(in) :: radius(:) !! radii to fold into the record.
        logical, intent(out), optional :: rebuilt !! .true. when the data had changed.

        call spatial_rebuild_worker(self, x, y, z, radius, rebuilt)
    end subroutine bind_rebuild_r1

    !> `%rebuild_for` with a single radius: re-tunes over the points already stored.
    !>
    !> **On a SKY index the radius is in DEGREES**, exactly as `%build_sky`'s `radius_deg=` is, and
    !> subject to the same 90-degree ceiling. The conversion to the chord the index tunes on happens
    !> here rather than in the caller -- which is what makes "angles in, angles out" true of every
    !> sky entry point without exception. `%effective_radius()` answers in degrees too, so a value
    !> read from one may be handed straight back to the other.
    !>
    !> It re-tunes whichever backend the index has -- a cell side for `PF_SKY_GRID3D`, a HEALPix
    !> resolution for `PF_SKY_HEALPIX` -- and never changes which one that is. A caller wanting
    !> the other backend calls `%build_sky` again.
    subroutine bind_rebuild_for_r0(self, radius)
        class(pf_spatial_index), intent(inout), target :: self !! the index to re-tune.
        real(real64), intent(in) :: radius !! the radius to tune for; must be > 0. DEGREES on a sky index.

        ! The branch is on the METRIC and it is here rather than in the worker so that `sky_chords`
        ! -- which validates, refuses above 90 degrees and converts -- is reached directly instead
        ! of by host association from a submodule. An UNBUILT index has metric_id defaulting to
        ! PF_METRIC_EUCLIDEAN, so it takes the second arm and still aborts with "has not been
        ! built" rather than with a units message.
        if (self%metric_id == PF_METRIC_SKY) then
            call spatial_rebuild_for_worker(self, sky_chords([radius], "rebuild_for"), .false.)
        else
            call spatial_rebuild_for_worker(self, [radius], .false.)
        end if
    end subroutine bind_rebuild_for_r0

    !> `%rebuild_for` with a list of radii.
    subroutine bind_rebuild_for_r1(self, radius)
        class(pf_spatial_index), intent(inout), target :: self !! the index to re-tune.
        real(real64), intent(in) :: radius(:) !! the radii to tune for; all > 0. DEGREES on a sky index.

        ! Same metric branch as the scalar form; see `bind_rebuild_for_r0` for why it is here.
        if (self%metric_id == PF_METRIC_SKY) then
            call spatial_rebuild_for_worker(self, sky_chords(radius, "rebuild_for"), .false.)
        else
            call spatial_rebuild_for_worker(self, radius, .false.)
        end if
    end subroutine bind_rebuild_for_r1

    ! ---- Lifecycle and metadata ----

    !> Releases everything the index holds and returns it to unbuilt.
    subroutine bind_clear(self)
        class(pf_spatial_index), intent(inout) :: self !! the index to empty.

        call spatial_clear_worker(self)
    end subroutine bind_clear

    !> How many points the index holds.
    integer(int64) function bind_size(self) result(n)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        n = self%npts
    end function bind_size

    !> The scalar cell side this index was built with, ready to pass straight back as `cell=` on
    !> another build over comparable data.
    !>
    !> **On a periodic index the per-axis sides are adjusted UP from this**, to `L/nx` exactly, so
    !> that the cells tile the box; this reports the side that was asked for, which is what makes it
    !> reusable.
    real(real64) function bind_cell_size(self) result(h)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        h = self%cell_side
    end function bind_cell_size

    !> How many cells the grid holds, occupied or not.
    integer(int64) function bind_cells(self) result(n)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        n = self%n_cells
    end function bind_cells

    !> The cell side actually used on each axis.
    !>
    !> Equal to `%cell_size()` on all three axes UNLESS the index is periodic, where each axis's
    !> side is adjusted up to `L/n` so that the cells tile the box exactly -- a grid that does not
    !> tile leaves the seam cell a different width from every other, which no query result can
    !> reveal. This is the only way to observe that from outside the module, which is why it is
    !> public.
    subroutine bind_cell_sides(self, cx, cy, cz)
        class(pf_spatial_index), intent(in) :: self !! the index queried.
        real(real64), intent(out) :: cx !! cell side along x.
        real(real64), intent(out) :: cy !! cell side along y.
        real(real64), intent(out) :: cz !! cell side along z; the whole extent for a 2D index.

        cx = self%cell(1)
        cy = self%cell(2)
        cz = self%cell(3)
    end subroutine bind_cell_sides

    !> Cells along each axis. A 2D index always reports `nz == 1`.
    subroutine bind_grid(self, nx, ny, nz)
        class(pf_spatial_index), intent(in) :: self !! the index queried.
        integer(int64), intent(out) :: nx !! cells along x.
        integer(int64), intent(out) :: ny !! cells along y.
        integer(int64), intent(out) :: nz !! cells along z; 1 for a 2D index.

        nx = self%grid_n(1)
        ny = self%grid_n(2)
        nz = self%grid_n(3)
    end subroutine bind_grid

    !> `PF_METRIC_EUCLIDEAN` or `PF_METRIC_SKY`.
    integer function bind_metric(self) result(m)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        m = self%metric_id
    end function bind_metric

    !> `PF_SKY_GRID3D` or `PF_SKY_HEALPIX`: which structure narrows the candidate set.
    !>
    !> Always `PF_SKY_GRID3D` on a Euclidean index, which has no sphere to pixelate and takes no
    !> `backend=`. On a sky index it is whatever `%build_sky` was given, and it does not change
    !> under `%rebuild_for` -- a caller wanting the other backend calls `%build_sky` again.
    integer function bind_backend(self) result(b)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        b = self%backend_id
    end function bind_backend

    !> The HEALPix resolution parameter, or 0 when this index is not HEALPix-backed.
    !>
    !> **Zero is the documented answer for every other index**, rather than an abort, so that
    !> reporting code can print an index's shape without first asking what backs it -- the same
    !> reason `%cell_size` answers 0 on a HEALPix index.
    integer(int64) function bind_nside(self) result(n)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        n = self%nside_v
    end function bind_nside

    !> The HEALPix pixel count `12*nside**2`, or 0 when this index is not HEALPix-backed.
    !>
    !> This is also what `%cells` answers on a HEALPix index: a pixel IS its bucket, so the pixel
    !> count is the bucket count and is what the buckets-per-point cap governs on both backends.
    integer(int64) function bind_npix(self) result(n)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        n = self%npix_v
    end function bind_npix

    !> How many coordinates the caller supplied: 2 or 3.
    integer function bind_ndim(self) result(d)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        d = self%ncoord
    end function bind_ndim

    !> Whether `%build` has run.
    logical function bind_is_built(self) result(ok)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        ok = self%built_ok
    end function bind_is_built

    !> Whether the index wraps at the box faces.
    logical function bind_is_periodic(self) result(ok)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        ok = self%periodic_on
    end function bind_is_periodic

    !> The effective radius the cell size is currently tuned for: `sum(r^3)/sum(r^2)` over every
    !> radius this index has been built for or asked about.
    !>
    !> Reported as one number rather than as a list because that is all the index keeps -- two
    !> accumulators and a count -- which is also what makes folding a new radius in cost two
    !> additions rather than a growing array.
    real(real64) function bind_effective_radius(self) result(r)
        class(pf_spatial_index), intent(in) :: self !! the index queried.

        r = 0.0_real64
        if (self%r2sum > 0.0_real64) r = self%r3sum / self%r2sum
        ! A sky index accumulates chords, because that is what it tunes on -- but it was given
        ! degrees, so it answers in degrees. `%cell_size()` deliberately does NOT convert: it pairs
        ! with `cell=`, and both are in unit-vector space.
        if (self%metric_id == PF_METRIC_SKY) &
            r = 2.0_real64 * asin(min(0.5_real64 * r, 1.0_real64)) * spatial_rad2deg
    end function bind_effective_radius

    ! ---- Single queries ----

    !> `%within` into an `int32` buffer.
    !>
    !> **`r_inner=` makes the ball an annulus**, `r_inner <= d <= r`, with BOTH bounds inclusive.
    !> A point sitting exactly on the inner surface is therefore in the annulus and in the inner
    !> ball alike, so "annulus = outer ball minus inner ball" holds everywhere except on that
    !> surface itself.
    !>
    !> **`sorted=.true.` returns the rows by increasing distance**, ties broken by ascending row
    !> index so the order is the same on every machine. It needs the distances even when `dist=`
    !> is absent, so asking for it without wanting them still pays for them. With a buffer shorter
    !> than `m` the rows kept are the first `m` the walk found and `sorted=` orders *those* -- it
    !> does not make a short buffer hold the NEAREST rows. `%nearest` is the query that does that.
    integer(int64) function bind_within_i32(self, p, r, out, dist, r_inner, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p(:) !! the query point; 2 or 3 coordinates.
        real(real64), intent(in) :: r !! the search radius.
        integer(int32), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist(:) !! distance to each reported point.
        real(real64), intent(in), optional :: r_inner !! an inner radius; makes the ball an annulus.
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        real(real64) :: q(3) !! `p` widened to three coordinates; a named local, see `query_point`.

        q = query_point(self, p, "within")
        call spatial_scan(self, q, r, m, out32=out, dist=dist, r_inner=r_inner, sorted=sorted)
    end function bind_within_i32

    !> `%within` into an `int64` buffer. See `bind_within_i32` for `r_inner=` and `sorted=`.
    integer(int64) function bind_within_i64(self, p, r, out, dist, r_inner, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p(:) !! the query point; 2 or 3 coordinates.
        real(real64), intent(in) :: r !! the search radius.
        integer(int64), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist(:) !! distance to each reported point.
        real(real64), intent(in), optional :: r_inner !! an inner radius; makes the ball an annulus.
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        real(real64) :: q(3) !! `p` widened to three coordinates; a named local, see `query_point`.

        q = query_point(self, p, "within")
        call spatial_scan(self, q, r, m, out64=out, dist=dist, r_inner=r_inner, sorted=sorted)
    end function bind_within_i64

    !> How many points lie within `r` of `p`, without materialising them.
    !>
    !> `r_inner=` counts an annulus instead, on the same inclusive rule as `%within`.
    integer(int64) function bind_count_within(self, p, r, r_inner) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p(:) !! the query point; 2 or 3 coordinates.
        real(real64), intent(in) :: r !! the search radius.
        real(real64), intent(in), optional :: r_inner !! an inner radius; counts an annulus.
        real(real64) :: q(3) !! `p` widened to three coordinates; a named local, see `query_point`.

        q = query_point(self, p, "count_within")
        call spatial_scan(self, q, r, m, r_inner=r_inner)
    end function bind_count_within

    !> `%within_sky` into an `int32` buffer.
    !>
    !> **Angles in, angles out.** `rsky_deg` is an angular radius in degrees and `dist_deg` comes
    !> back in degrees; a caller never sees a chord. The conversion is exact in both directions --
    !> `chord = 2*sin(theta/2)` going in, `theta = 2*asin(chord/2)` coming out.
    !>
    !> Refused on a Euclidean index, and every Euclidean query is refused on a sky one. That guard
    !> is the entire reason the metric is a property of the INDEX rather than of the call: without
    !> it, `%within` on a sky index would quietly answer in chords to someone who asked in degrees.
    !>
    !> **`rsky_deg = 0` is a knife edge and is not the way to find one catalogue entry.** The query
    !> point is not compared as `(ra, dec)`: `sky_vector` derives a unit vector from it by the same
    !> expression `spatial_build_sky_worker` applied to the catalogue, and two evaluations of a
    !> transcendental expression need not agree to the last bit. On ifx 2026.1 they do not -- that
    !> expression in a bulk loop and as a scalar differ by 1-2 ulp under `-O0`, putting a catalogue
    !> point about 1e-14 degrees from its own `(ra, dec)`; at `-O2`, and under gfortran at either,
    !> they agree exactly. So a zero radius may find the point or may find nothing, depending on
    !> the compiler and the optimisation level. Ask for a radius above that error instead: 1e-9
    !> degrees is 3.6 microarcseconds. `%within` has no such caveat, because it compares the
    !> stored coordinates rather than re-deriving them.
    integer(int64) function bind_sky_i32(self, ra, dec, rsky_deg, out, dist_deg, r_inner_deg, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the sky index to search.
        real(real64), intent(in) :: ra !! right ascension of the query point, in degrees.
        real(real64), intent(in) :: dec !! declination of the query point, in degrees.
        real(real64), intent(in) :: rsky_deg !! the angular search radius, in degrees.
        integer(int32), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist_deg(:) !! angular separation, in degrees.
        real(real64), intent(in), optional :: r_inner_deg !! an inner angular radius; gives an annulus.
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing separation.

        call sky_scan(self, ra, dec, rsky_deg, m, "within_sky", out32=out, dist_deg=dist_deg, &
            r_inner_deg=r_inner_deg, sorted=sorted)
    end function bind_sky_i32

    !> `%within_sky` into an `int64` buffer. See `bind_sky_i32`.
    integer(int64) function bind_sky_i64(self, ra, dec, rsky_deg, out, dist_deg, r_inner_deg, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the sky index to search.
        real(real64), intent(in) :: ra !! right ascension of the query point, in degrees.
        real(real64), intent(in) :: dec !! declination of the query point, in degrees.
        real(real64), intent(in) :: rsky_deg !! the angular search radius, in degrees.
        integer(int64), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist_deg(:) !! angular separation, in degrees.
        real(real64), intent(in), optional :: r_inner_deg !! an inner angular radius; gives an annulus.
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing separation.

        call sky_scan(self, ra, dec, rsky_deg, m, "within_sky", out64=out, dist_deg=dist_deg, &
            r_inner_deg=r_inner_deg, sorted=sorted)
    end function bind_sky_i64

    !> `%count_within_sky`: how many points lie within `rsky_deg` degrees of `(ra, dec)`.
    !>
    !> The sky twin of `%count_within`, and the same trade: no buffer, so nothing is written and
    !> nothing can truncate -- useful for a local surface density at one position, or for sizing a
    !> buffer before asking for the rows. `%within_sky` returns the same number, so this exists for
    !> the caller who does not want the rows at all rather than to answer anything new.
    !>
    !> Every guard, the degrees-to-chord conversion and the annulus rule come from the shared
    !> `sky_scan` rather than from a second copy, which is what keeps this and `%within_sky` from
    !> ever disagreeing about what "within" means.
    integer(int64) function bind_count_sky(self, ra, dec, rsky_deg, r_inner_deg) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the sky index to search.
        real(real64), intent(in) :: ra !! right ascension of the query point, in degrees.
        real(real64), intent(in) :: dec !! declination of the query point, in degrees.
        real(real64), intent(in) :: rsky_deg !! the angular search radius, in degrees.
        real(real64), intent(in), optional :: r_inner_deg !! an inner angular radius; counts an annulus.

        call sky_scan(self, ra, dec, rsky_deg, m, "count_within_sky", r_inner_deg=r_inner_deg)
    end function bind_count_sky

    !> The shared body of both `%within_sky` forms: guard, convert, scan, convert back.
    !>
    !> **Sorting happens on CHORDS and needs no undoing**, because the chord is strictly
    !> increasing in the angle -- so ordering by chord and ordering by degrees are the same order.
    subroutine sky_scan(self, ra, dec, rsky_deg, m, what, out32, out64, dist_deg, r_inner_deg, sorted)
        type(pf_spatial_index), intent(in), target :: self !! the sky index to search.
        real(real64), intent(in) :: ra !! right ascension of the query point, in degrees.
        real(real64), intent(in) :: dec !! declination of the query point, in degrees.
        real(real64), intent(in) :: rsky_deg !! the angular search radius, in degrees.
        integer(int64), intent(out) :: m !! how many points qualify, whatever the buffer holds.
        character(len=*), intent(in) :: what !! the calling binding, so a message names it and not this helper.
        integer(int32), intent(inout), optional :: out32(:) !! int32 output buffer.
        integer(int64), intent(inout), optional :: out64(:) !! int64 output buffer.
        real(real64), intent(out), optional :: dist_deg(:) !! angular separation, in degrees.
        real(real64), intent(in), optional :: r_inner_deg !! an inner angular radius; gives an annulus.
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing separation.
        real(real64) :: p(3), half, chord_in
        integer(int64) :: k, nfill

        if (.not. self%built_ok) error stop "pf_spatial_index%" // what // &
            ": this index has not been built; call %build_sky first"
        if (self%metric_id /= PF_METRIC_SKY) error stop "pf_spatial_index%" // what // &
            ": this index was built with %build, not %build_sky; use %within"
        if (.not. (rsky_deg >= 0.0_real64)) error stop "pf_spatial_index%" // what // &
            ": the angular radius must be >= 0 and not NaN"
        if (rsky_deg > spatial_max_sky_deg) error stop "pf_spatial_index%" // what // &
            ": an angular radius above 90 degrees is not a neighbour " // &
            "search; the ball then covers most of the sky and the grid has nothing to prune"
        call spatial_check_finite([ra, dec], "pf_spatial_index%" // what, "query ra/dec")
        p = sky_vector(ra, dec)
        if (present(r_inner_deg)) then
            if (.not. (r_inner_deg >= 0.0_real64)) error stop "pf_spatial_index%" // what // &
                ": the inner angular radius must be >= 0 and not NaN"
            if (r_inner_deg > rsky_deg) error stop "pf_spatial_index%" // what // &
                ": the inner angular radius must not exceed the outer one"
            chord_in = sky_chord(r_inner_deg)
            call spatial_scan(self, p, sky_chord(rsky_deg), m, out32=out32, out64=out64, &
                dist=dist_deg, r_inner=chord_in, sorted=sorted)
        else
            call spatial_scan(self, p, sky_chord(rsky_deg), m, out32=out32, out64=out64, &
                dist=dist_deg, sorted=sorted)
        end if
        if (.not. present(dist_deg)) return
        ! Chords back to degrees, over the entries actually written -- which is the true count
        ! capped by the SHORTEST buffer, not by this one. `out` has to enter the minimum: the walk
        ! stops at the common cap, so a caller passing a short `out` beside a long `dist_deg`
        ! leaves the tail of `dist_deg` unwritten, and converting it would read an undefined value
        ! and hand back a plausible number. nagfor's `-nan` turns that into a visible NaN; every
        ! other compiler in the fleet returns garbage silently.
        nfill = m
        if (present(out32)) nfill = min(nfill, size(out32, kind=int64))
        if (present(out64)) nfill = min(nfill, size(out64, kind=int64))
        nfill = min(nfill, size(dist_deg, kind=int64))
        do k = 1_int64, nfill
            half = min(0.5_real64 * dist_deg(k), 1.0_real64)
            dist_deg(k) = 2.0_real64 * asin(half) * spatial_rad2deg
        end do
    end subroutine sky_scan

    !> `%within_segment` into an `int32` buffer.
    !>
    !> **A capsule**: every point whose distance to the SEGMENT `p1`-`p2` is at most `r`, so the
    !> ends are round. A point beyond an end is inside this shape and outside `%within_cylinder`,
    !> which is the whole difference between the two. `dist` reports the distance to the segment.
    !>
    !> `p1 == p2` degenerates to a ball of radius `r` about `p1` -- the right answer rather than a
    !> special case, and the same reduction `%within_cone` makes.
    !>
    !> **`axis_point=` is the point `dist` was measured FROM**, which for a capsule is the closest
    !> point on the SEGMENT and not the projection onto the infinite line: a point beyond `p2` has
    !> its distance measured from `p2` itself. `axis_t=` says where that point sits along the axis,
    !> normalised to `[0, 1]` -- 0 at `p1`, 1 at `p2`, never outside -- so
    !> `axis_point == p1 + axis_t*(p2 - p1)`. Multiply `axis_t` by `norm(p2 - p1)` for a length
    !> along the axis in the index's own units. See `spatial_scan_axis` for the full contract.
    integer(int64) function bind_seg_i32(self, p1, p2, r, out, dist, axis_point, axis_t, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p1(:) !! one end of the segment; 2 or 3 coordinates.
        real(real64), intent(in) :: p2(:) !! the other end; as many coordinates as `p1`.
        real(real64), intent(in) :: r !! the search radius about the segment.
        integer(int32), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist(:) !! distance to the segment, per reported point.
        real(real64), intent(out), optional :: axis_point(:,:) !! `(ndim, m)`: where `dist` was measured from.
        real(real64), intent(out), optional :: axis_t(:) !! where that point sits along the axis, in [0, 1].
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        real(real64) :: q1(3), q2(3) !! the two ends widened to three coordinates; named locals,
            !! see `query_point`.

        q1 = query_point(self, p1, "within_segment")
        q2 = query_point(self, p2, "within_segment")
        call spatial_scan_axis(self, q1, q2, r, r, .true., "within_segment", m, &
            out32=out, dist=dist, axis_point=axis_point, axis_t=axis_t, sorted=sorted)
    end function bind_seg_i32

    !> `%within_segment` into an `int64` buffer. See `bind_seg_i32` for the shape.
    integer(int64) function bind_seg_i64(self, p1, p2, r, out, dist, axis_point, axis_t, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p1(:) !! one end of the segment; 2 or 3 coordinates.
        real(real64), intent(in) :: p2(:) !! the other end; as many coordinates as `p1`.
        real(real64), intent(in) :: r !! the search radius about the segment.
        integer(int64), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist(:) !! distance to the segment, per reported point.
        real(real64), intent(out), optional :: axis_point(:,:) !! `(ndim, m)`: where `dist` was measured from.
        real(real64), intent(out), optional :: axis_t(:) !! where that point sits along the axis, in [0, 1].
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        real(real64) :: q1(3), q2(3) !! the two ends widened to three coordinates; named locals,
            !! see `query_point`.

        q1 = query_point(self, p1, "within_segment")
        q2 = query_point(self, p2, "within_segment")
        call spatial_scan_axis(self, q1, q2, r, r, .true., "within_segment", m, &
            out64=out, dist=dist, axis_point=axis_point, axis_t=axis_t, sorted=sorted)
    end function bind_seg_i64

    !> `%within_cylinder` into an `int32` buffer.
    !>
    !> **Flat ends**: a point qualifies when it lies between the two end planes AND within `r` of
    !> the axis. `dist` reports the perpendicular distance to the axis.
    !>
    !> `p1 == p2` degenerates to a ball of radius `r` about `p1`, exactly as `%within_segment`
    !> does -- with no axis there is no "between the ends" left to test.
    !>
    !> `axis_point=` and `axis_t=` behave as on `%within_segment`; here the clamp never bites,
    !> since a point whose axis parameter falls outside `[0, 1]` is rejected rather than clamped.
    integer(int64) function bind_cyl_i32(self, p1, p2, r, out, dist, axis_point, axis_t, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p1(:) !! one end of the axis; 2 or 3 coordinates.
        real(real64), intent(in) :: p2(:) !! the other end; as many coordinates as `p1`.
        real(real64), intent(in) :: r !! the cylinder radius.
        integer(int32), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist(:) !! distance to the axis, per reported point.
        real(real64), intent(out), optional :: axis_point(:,:) !! `(ndim, m)`: the foot of that distance.
        real(real64), intent(out), optional :: axis_t(:) !! where that foot sits along the axis, in [0, 1].
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        real(real64) :: q1(3), q2(3) !! the two ends widened to three coordinates; named locals,
            !! see `query_point`.

        q1 = query_point(self, p1, "within_cylinder")
        q2 = query_point(self, p2, "within_cylinder")
        call spatial_scan_axis(self, q1, q2, r, r, .false., "within_cylinder", m, &
            out32=out, dist=dist, axis_point=axis_point, axis_t=axis_t, sorted=sorted)
    end function bind_cyl_i32

    !> `%within_cylinder` into an `int64` buffer. See `bind_cyl_i32` for the shape.
    integer(int64) function bind_cyl_i64(self, p1, p2, r, out, dist, axis_point, axis_t, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p1(:) !! one end of the axis; 2 or 3 coordinates.
        real(real64), intent(in) :: p2(:) !! the other end; as many coordinates as `p1`.
        real(real64), intent(in) :: r !! the cylinder radius.
        integer(int64), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist(:) !! distance to the axis, per reported point.
        real(real64), intent(out), optional :: axis_point(:,:) !! `(ndim, m)`: the foot of that distance.
        real(real64), intent(out), optional :: axis_t(:) !! where that foot sits along the axis, in [0, 1].
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        real(real64) :: q1(3), q2(3) !! the two ends widened to three coordinates; named locals,
            !! see `query_point`.

        q1 = query_point(self, p1, "within_cylinder")
        q2 = query_point(self, p2, "within_cylinder")
        call spatial_scan_axis(self, q1, q2, r, r, .false., "within_cylinder", m, &
            out64=out, dist=dist, axis_point=axis_point, axis_t=axis_t, sorted=sorted)
    end function bind_cyl_i64

    !> `%within_cone` into an `int32` buffer.
    !>
    !> **A truncated cone (a frustum)**: flat ends like the cylinder, but the radius grows linearly
    !> along the axis, from `r1` at `p1` to `r2` at `p2`. `r1 == r2` reproduces
    !> `%within_cylinder` exactly, which is why this is the general routine the other two are
    !> written in terms of.
    !>
    !> **This is the shape a fixed angular aperture actually sweeps out**, since the transverse
    !> extent an aperture subtends grows linearly with distance -- so a selection outward from an
    !> observer is a cone rather than a cylinder.
    !>
    !> `dist` reports the perpendicular distance to the axis, not to the sloping surface.
    !> `p1 == p2` degenerates to a ball of radius `max(r1, r2)` about `p1`.
    !> `axis_point=` and `axis_t=` behave as on `%within_cylinder`.
    integer(int64) function bind_cone_i32(self, p1, p2, r1, r2, out, dist, axis_point, axis_t, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p1(:) !! one end of the axis; 2 or 3 coordinates.
        real(real64), intent(in) :: p2(:) !! the other end; as many coordinates as `p1`.
        real(real64), intent(in) :: r1 !! the radius at `p1`.
        real(real64), intent(in) :: r2 !! the radius at `p2`.
        integer(int32), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist(:) !! distance to the axis, per reported point.
        real(real64), intent(out), optional :: axis_point(:,:) !! `(ndim, m)`: the foot of that distance.
        real(real64), intent(out), optional :: axis_t(:) !! where that foot sits along the axis, in [0, 1].
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        real(real64) :: q1(3), q2(3) !! the two ends widened to three coordinates; named locals,
            !! see `query_point`.

        q1 = query_point(self, p1, "within_cone")
        q2 = query_point(self, p2, "within_cone")
        call spatial_scan_axis(self, q1, q2, r1, r2, .false., "within_cone", m, &
            out32=out, dist=dist, axis_point=axis_point, axis_t=axis_t, sorted=sorted)
    end function bind_cone_i32

    !> `%within_cone` into an `int64` buffer. See `bind_cone_i32` for the shape.
    integer(int64) function bind_cone_i64(self, p1, p2, r1, r2, out, dist, axis_point, axis_t, sorted) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p1(:) !! one end of the axis; 2 or 3 coordinates.
        real(real64), intent(in) :: p2(:) !! the other end; as many coordinates as `p1`.
        real(real64), intent(in) :: r1 !! the radius at `p1`.
        real(real64), intent(in) :: r2 !! the radius at `p2`.
        integer(int64), intent(out) :: out(:) !! caller's row indices of the points found.
        real(real64), intent(out), optional :: dist(:) !! distance to the axis, per reported point.
        real(real64), intent(out), optional :: axis_point(:,:) !! `(ndim, m)`: the foot of that distance.
        real(real64), intent(out), optional :: axis_t(:) !! where that foot sits along the axis, in [0, 1].
        logical, intent(in), optional :: sorted !! .true. orders the result by increasing distance.
        real(real64) :: q1(3), q2(3) !! the two ends widened to three coordinates; named locals,
            !! see `query_point`.

        q1 = query_point(self, p1, "within_cone")
        q2 = query_point(self, p2, "within_cone")
        call spatial_scan_axis(self, q1, q2, r1, r2, .false., "within_cone", m, &
            out64=out, dist=dist, axis_point=axis_point, axis_t=axis_t, sorted=sorted)
    end function bind_cone_i64

    ! ---- Bulk queries ----

    !> `%all_within` with one radius for every point.
    !>
    !> `r_inner=` makes each ball an annulus, on `%within`'s inclusive rule. `sorted=.true.`
    !> orders every row by increasing distance, ties by ascending row index.
    subroutine bind_all_within_r0(self, radius, offsets, neighbours, threads, r_inner, sorted)
        class(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
        real(real64), intent(in) :: radius !! the search radius, the same for every point.
        integer(int64), allocatable, intent(out) :: offsets(:) !! length n+1; `offsets(1) == 1`.
        integer(int64), allocatable, intent(out) :: neighbours(:) !! the concatenated neighbour lists.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner !! one inner radius for every point.
        logical, intent(in), optional :: sorted !! .true. orders each row by increasing distance.

        if (present(r_inner)) then
            call spatial_all_within_worker(self, [radius], offsets, neighbours, PF_METRIC_EUCLIDEAN, &
                threads, radii_inner=[r_inner], sorted=sorted)
        else
            call spatial_all_within_worker(self, [radius], offsets, neighbours, PF_METRIC_EUCLIDEAN, &
                threads, sorted=sorted)
        end if
    end subroutine bind_all_within_r0

    !> `%all_within` with an independent radius per point.
    !>
    !> **Directed**: row `i`'s neighbour list holds what lies within `radius(i)` of it, so `j` can
    !> appear in `i`'s list without `i` appearing in `j`'s. That is the meaning of the query, not a
    !> defect -- `%pairs_within` is the symmetric form.
    !> `r_inner=` takes one value or one per point, matching `radius`. `sorted=` is as on the
    !> single-radius form.
    subroutine bind_all_within_r1(self, radius, offsets, neighbours, threads, r_inner, sorted)
        class(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
        real(real64), intent(in) :: radius(:) !! one radius per point, in the caller's row order.
        integer(int64), allocatable, intent(out) :: offsets(:) !! length n+1; `offsets(1) == 1`.
        integer(int64), allocatable, intent(out) :: neighbours(:) !! the concatenated neighbour lists.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner(:) !! inner radii; one value or one per point.
        logical, intent(in), optional :: sorted !! .true. orders each row by increasing distance.

        call spatial_all_within_worker(self, radius, offsets, neighbours, PF_METRIC_EUCLIDEAN, &
            threads, radii_inner=r_inner, sorted=sorted)
    end subroutine bind_all_within_r1

    !> `%pairs_within` with one radius for every point.
    !>
    !> Every neighbouring pair appears exactly once, with `i < j`. With one radius the relation is
    !> symmetric by construction, so there is nothing to choose: either endpoint's ball reaches the
    !> other or neither does.
    subroutine bind_pairs_within_r0(self, radius, i, j, threads, r_inner)
        class(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
        real(real64), intent(in) :: radius !! the search radius, the same for every point.
        integer(int64), allocatable, intent(out) :: i(:) !! the lower row index of each pair.
        integer(int64), allocatable, intent(out) :: j(:) !! the higher row index of each pair.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner !! an inner radius; pairs closer than this are dropped.

        call spatial_pairs_within_worker(self, [radius], i, j, PF_METRIC_EUCLIDEAN, threads, r_inner)
    end subroutine bind_pairs_within_r0

    !> `%pairs_within` with an independent radius per point.
    !>
    !> **A pair qualifies when EITHER ball reaches the other: `d(i, j) <= max(radius(i),
    !> radius(j))`.** Which endpoint would have been doing the searching does not enter into it,
    !> so the edge list is a symmetric graph however wide the radii spread -- and it is still every
    !> pair exactly once, with `i < j`.
    !>
    !> Note this is genuinely more pairs than `%all_within` with the same vector reports, which is
    !> not a discrepancy: that query is DIRECTED, listing for each row only what lies within its
    !> own radius. `%count_all_within` counts the directed neighbours too, so it does not size this
    !> result.
    !>
    !> Two other conventions are common for a per-point radius and neither is what this gives:
    !> `d <= min(r_i, r_j)` (both must agree) and `d <= r_i + r_j` (the balls touch). Either can be
    !> had by filtering this result, since both are subsets of it.
    !>
    !> **`r_inner=` is SCALAR here even though `radius` is a vector, and that is a correctness
    !> constraint rather than a simplification.** With per-point radii a pair qualifies when it
    !> lies in *i*'s annulus OR in *j*'s, and the union of two different annuli is not an annulus
    !> -- so the descending-radius ranking that makes each pair be emitted from exactly one
    !> endpoint no longer covers it, and the edge list would be quietly incomplete. Passing a
    !> per-point `r_inner` here is refused.
    subroutine bind_pairs_within_r1(self, radius, i, j, threads, r_inner)
        class(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
        real(real64), intent(in) :: radius(:) !! one radius per point, in the caller's row order.
        integer(int64), allocatable, intent(out) :: i(:) !! the lower row index of each pair.
        integer(int64), allocatable, intent(out) :: j(:) !! the higher row index of each pair.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner !! one inner radius for every pair; scalar only.

        call spatial_pairs_within_worker(self, radius, i, j, PF_METRIC_EUCLIDEAN, threads, r_inner)
    end subroutine bind_pairs_within_r1

    !> `%count_all_within` with one radius for every point.
    subroutine bind_count_all_r0(self, radius, counts, threads, r_inner)
        class(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
        real(real64), intent(in) :: radius !! the search radius, the same for every point.
        integer(int64), allocatable, intent(out) :: counts(:) !! length n, in the caller's row order.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner !! one inner radius for every point.

        if (present(r_inner)) then
            call spatial_count_all_worker(self, [radius], counts, PF_METRIC_EUCLIDEAN, threads, [r_inner])
        else
            call spatial_count_all_worker(self, [radius], counts, PF_METRIC_EUCLIDEAN, threads)
        end if
    end subroutine bind_count_all_r0

    !> `%count_all_within` with an independent radius per point.
    subroutine bind_count_all_r1(self, radius, counts, threads, r_inner)
        class(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
        real(real64), intent(in) :: radius(:) !! one radius per point, in the caller's row order.
        integer(int64), allocatable, intent(out) :: counts(:) !! length n, in the caller's row order.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner(:) !! inner radii; one value or one per point.

        call spatial_count_all_worker(self, radius, counts, PF_METRIC_EUCLIDEAN, threads, r_inner)
    end subroutine bind_count_all_r1

    ! ---- Bulk queries on the sky ----
    !
    ! Each of these is its Euclidean twin with the radii converted from degrees to chords at the
    ! entry point, which is the only place that conversion may happen -- see `spatial_bulk_setup`.
    ! Nothing else differs: the sweep, the threading, the auto-rebuild and the directed/symmetric
    ! split are all shared, because a sky index IS a Euclidean index over unit vectors.

    !> `%all_within_sky` with one angular radius for every point.
    !>
    !> The catalogue self-match: for each row, every row within `radius_deg` degrees of it, as one
    !> CSR structure. DIRECTED under a per-point radius, exactly as `%all_within` is.
    subroutine bind_all_sky_r0(self, radius_deg, offsets, neighbours, threads, r_inner_deg, sorted)
        class(pf_spatial_index), intent(inout), target :: self !! the sky index to sweep.
        real(real64), intent(in) :: radius_deg !! the angular radius, in degrees.
        integer(int64), allocatable, intent(out) :: offsets(:) !! length n+1, `offsets(1) == 1`.
        integer(int64), allocatable, intent(out) :: neighbours(:) !! the concatenated neighbour lists.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner_deg !! an inner angular radius, in degrees.
        logical, intent(in), optional :: sorted !! .true. orders each row by increasing separation.

        if (present(r_inner_deg)) then
            call spatial_all_within_worker(self, sky_chords([radius_deg], "all_within_sky"), &
                offsets, neighbours, PF_METRIC_SKY, threads, &
                sky_chords([r_inner_deg], "all_within_sky"), sorted)
        else
            call spatial_all_within_worker(self, sky_chords([radius_deg], "all_within_sky"), &
                offsets, neighbours, PF_METRIC_SKY, threads, sorted=sorted)
        end if
    end subroutine bind_all_sky_r0

    !> `%all_within_sky` with an independent angular radius per point, in the caller's row order.
    subroutine bind_all_sky_r1(self, radius_deg, offsets, neighbours, threads, r_inner_deg, sorted)
        class(pf_spatial_index), intent(inout), target :: self !! the sky index to sweep.
        real(real64), intent(in) :: radius_deg(:) !! one angular radius per point, in degrees.
        integer(int64), allocatable, intent(out) :: offsets(:) !! length n+1, `offsets(1) == 1`.
        integer(int64), allocatable, intent(out) :: neighbours(:) !! the concatenated neighbour lists.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner_deg(:) !! inner angular radii, in degrees.
        logical, intent(in), optional :: sorted !! .true. orders each row by increasing separation.

        if (present(r_inner_deg)) then
            call spatial_all_within_worker(self, sky_chords(radius_deg, "all_within_sky"), &
                offsets, neighbours, PF_METRIC_SKY, threads, &
                sky_chords(r_inner_deg, "all_within_sky"), sorted)
        else
            call spatial_all_within_worker(self, sky_chords(radius_deg, "all_within_sky"), &
                offsets, neighbours, PF_METRIC_SKY, threads, sorted=sorted)
        end if
    end subroutine bind_all_sky_r1

    !> `%pairs_within_sky` with one angular radius for every point.
    !>
    !> Every close pair once, with `i < j` -- the shape a group finder or a duplicate-source search
    !> wants.
    subroutine bind_pairs_sky_r0(self, radius_deg, i, j, threads, r_inner_deg)
        class(pf_spatial_index), intent(inout), target :: self !! the sky index to sweep.
        real(real64), intent(in) :: radius_deg !! the angular radius, in degrees.
        integer(int64), allocatable, intent(out) :: i(:) !! the lower row index of each pair.
        integer(int64), allocatable, intent(out) :: j(:) !! the higher row index of each pair.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner_deg !! an inner angular radius, in degrees.
        real(real64), allocatable :: cin(:)

        if (present(r_inner_deg)) then
            cin = sky_chords([r_inner_deg], "pairs_within_sky")
            call spatial_pairs_within_worker(self, sky_chords([radius_deg], "pairs_within_sky"), &
                i, j, PF_METRIC_SKY, threads, cin(1))
        else
            call spatial_pairs_within_worker(self, sky_chords([radius_deg], "pairs_within_sky"), &
                i, j, PF_METRIC_SKY, threads)
        end if
    end subroutine bind_pairs_sky_r0

    !> `%pairs_within_sky` with an independent angular radius per point.
    !>
    !> **SYMMETRIC, and the degrees-to-chord conversion cannot disturb that.** A pair qualifies
    !> when `separation <= max(radius_deg(i), radius_deg(j))`, and because the chord is strictly
    !> increasing in the angle, the chord of the larger angle IS the larger chord -- so ranking the
    !> points by descending chord ranks them by descending angle, and the sweep needs no knowledge
    !> of which metric it is running under.
    !>
    !> **`r_inner_deg=` is SCALAR here for the same reason `%pairs_within`'s is**, and the chord
    !> being a monotone reparameterisation of the angle changes nothing about that argument: the
    !> union of two different annuli is still not an annulus.
    subroutine bind_pairs_sky_r1(self, radius_deg, i, j, threads, r_inner_deg)
        class(pf_spatial_index), intent(inout), target :: self !! the sky index to sweep.
        real(real64), intent(in) :: radius_deg(:) !! one angular radius per point, in degrees.
        integer(int64), allocatable, intent(out) :: i(:) !! the lower row index of each pair.
        integer(int64), allocatable, intent(out) :: j(:) !! the higher row index of each pair.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner_deg !! one inner angular radius; scalar only.
        real(real64), allocatable :: cin(:)

        if (present(r_inner_deg)) then
            cin = sky_chords([r_inner_deg], "pairs_within_sky")
            call spatial_pairs_within_worker(self, sky_chords(radius_deg, "pairs_within_sky"), &
                i, j, PF_METRIC_SKY, threads, cin(1))
        else
            call spatial_pairs_within_worker(self, sky_chords(radius_deg, "pairs_within_sky"), &
                i, j, PF_METRIC_SKY, threads)
        end if
    end subroutine bind_pairs_sky_r1

    !> `%count_all_within_sky` with one angular radius for every point.
    !>
    !> Counts without materialising the lists, which on a crowded field is the difference between
    !> a length-n array and one that does not fit in memory. DIRECTED, as `%all_within_sky` is.
    subroutine bind_count_all_sky_r0(self, radius_deg, counts, threads, r_inner_deg)
        class(pf_spatial_index), intent(inout), target :: self !! the sky index to sweep.
        real(real64), intent(in) :: radius_deg !! the angular radius, in degrees.
        integer(int64), allocatable, intent(out) :: counts(:) !! length n, in the caller's row order.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner_deg !! an inner angular radius, in degrees.

        if (present(r_inner_deg)) then
            call spatial_count_all_worker(self, sky_chords([radius_deg], "count_all_within_sky"), &
                counts, PF_METRIC_SKY, threads, sky_chords([r_inner_deg], "count_all_within_sky"))
        else
            call spatial_count_all_worker(self, sky_chords([radius_deg], "count_all_within_sky"), &
                counts, PF_METRIC_SKY, threads)
        end if
    end subroutine bind_count_all_sky_r0

    !> `%count_all_within_sky` with an independent angular radius per point.
    subroutine bind_count_all_sky_r1(self, radius_deg, counts, threads, r_inner_deg)
        class(pf_spatial_index), intent(inout), target :: self !! the sky index to sweep.
        real(real64), intent(in) :: radius_deg(:) !! one angular radius per point, in degrees.
        integer(int64), allocatable, intent(out) :: counts(:) !! length n, in the caller's row order.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        real(real64), intent(in), optional :: r_inner_deg(:) !! inner angular radii, in degrees.

        if (present(r_inner_deg)) then
            call spatial_count_all_worker(self, sky_chords(radius_deg, "count_all_within_sky"), &
                counts, PF_METRIC_SKY, threads, sky_chords(r_inner_deg, "count_all_within_sky"))
        else
            call spatial_count_all_worker(self, sky_chords(radius_deg, "count_all_within_sky"), &
                counts, PF_METRIC_SKY, threads)
        end if
    end subroutine bind_count_all_sky_r1

    ! ---- k nearest neighbours ----

    !> `%nearest` with an `int32` k into an `int32` buffer.
    !>
    !> **The `k` points nearest `p`, always ordered by increasing distance**, ties broken by
    !> ascending row index. The result is EXACT: a ball is grown until it holds at least `k`
    !> points, and the `k` nearest are then the `k` smallest of what that ball returned -- see
    !> `spatial_shell_search`, which carries the argument and the trap it avoids.
    !>
    !> `m` is `min(k, %size())`, so asking for more neighbours than the index holds returns them
    !> all rather than failing. A buffer shorter than `m` is filled as far as it reaches, and
    !> because the ordering happens before the copy those ARE the nearest ones -- unlike
    !> `%within(sorted=.true.)`, whose short buffer holds whatever the walk found first.
    !>
    !> The query point need not be one of the catalogue's own: a point coincident with a stored
    !> row simply finds it at distance zero.
    integer(int64) function bind_near_k32_i32(self, p, k, out, dist) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p(:) !! the query point; 2 or 3 coordinates.
        integer(int32), intent(in) :: k !! how many neighbours to report; must be >= 1.
        integer(int32), intent(out) :: out(:) !! caller's row indices, nearest first.
        real(real64), intent(out), optional :: dist(:) !! distance to each reported point.
        real(real64) :: q(3) !! `p` widened to three coordinates; a named local, see `query_point`.

        q = query_point(self, p, "nearest")
        call near_scan(self, q, int(k, kind=int64), m, out32=out, dist=dist)
    end function bind_near_k32_i32

    !> `%nearest` with an `int32` k into an `int64` buffer. See `bind_near_k32_i32`.
    integer(int64) function bind_near_k32_i64(self, p, k, out, dist) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p(:) !! the query point; 2 or 3 coordinates.
        integer(int32), intent(in) :: k !! how many neighbours to report; must be >= 1.
        integer(int64), intent(out) :: out(:) !! caller's row indices, nearest first.
        real(real64), intent(out), optional :: dist(:) !! distance to each reported point.
        real(real64) :: q(3) !! `p` widened to three coordinates; a named local, see `query_point`.

        q = query_point(self, p, "nearest")
        call near_scan(self, q, int(k, kind=int64), m, out64=out, dist=dist)
    end function bind_near_k32_i64

    !> `%nearest` with an `int64` k into an `int32` buffer. See `bind_near_k32_i32`.
    integer(int64) function bind_near_k64_i32(self, p, k, out, dist) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p(:) !! the query point; 2 or 3 coordinates.
        integer(int64), intent(in) :: k !! how many neighbours to report; must be >= 1.
        integer(int32), intent(out) :: out(:) !! caller's row indices, nearest first.
        real(real64), intent(out), optional :: dist(:) !! distance to each reported point.
        real(real64) :: q(3) !! `p` widened to three coordinates; a named local, see `query_point`.

        q = query_point(self, p, "nearest")
        call near_scan(self, q, k, m, out32=out, dist=dist)
    end function bind_near_k64_i32

    !> `%nearest` with an `int64` k into an `int64` buffer. See `bind_near_k32_i32`.
    integer(int64) function bind_near_k64_i64(self, p, k, out, dist) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p(:) !! the query point; 2 or 3 coordinates.
        integer(int64), intent(in) :: k !! how many neighbours to report; must be >= 1.
        integer(int64), intent(out) :: out(:) !! caller's row indices, nearest first.
        real(real64), intent(out), optional :: dist(:) !! distance to each reported point.
        real(real64) :: q(3) !! `p` widened to three coordinates; a named local, see `query_point`.

        q = query_point(self, p, "nearest")
        call near_scan(self, q, k, m, out64=out, dist=dist)
    end function bind_near_k64_i64

    !> The shared body of every `%nearest` form: guard, expanding ball, copy out.
    subroutine near_scan(self, p, k, m, out32, out64, dist)
        type(pf_spatial_index), intent(in), target :: self !! the index to search.
        real(real64), intent(in) :: p(3) !! the query point, already widened to three coordinates.
        integer(int64), intent(in) :: k !! how many neighbours to report.
        integer(int64), intent(out) :: m !! `min(k, %size())`, whatever the buffer holds.
        integer(int32), intent(out), optional :: out32(:) !! int32 output buffer.
        integer(int64), intent(out), optional :: out64(:) !! int64 output buffer.
        real(real64), intent(out), optional :: dist(:) !! distance to each reported point.
        integer(int64), allocatable :: rows(:)
        real(real64), allocatable :: ds(:)
        integer(int64) :: cap, t
        real(real64) :: rseed

        m = 0_int64
        if (k < 1_int64) error stop "pf_spatial_index%nearest: k must be >= 1"
        if (present(out32) .and. self%npts > int(huge(0_int32), kind=int64)) error stop &
            "pf_spatial_index%nearest: this index holds more rows than an int32 buffer can name; use an int64 one"
        ! The expanding ball reaches the same clamped grid walk `%within` does, so the point needs
        ! the same screen -- this is the third and last choke point a caller-supplied point enters
        ! through, beside spatial_scan and spatial_scan_axis.
        call spatial_check_finite(p, "pf_spatial_index%nearest", "query point coordinate")
        m = min(k, self%npts)
        if (m == 0_int64) return
        rseed = -1.0_real64
        call spatial_shell_search(self, p, m, rseed, rows, ds, "nearest")
        cap = huge(0_int64)
        if (present(out32)) cap = min(cap, size(out32, kind=int64))
        if (present(out64)) cap = min(cap, size(out64, kind=int64))
        if (present(dist)) cap = min(cap, size(dist, kind=int64))
        do t = 1_int64, min(m, cap)
            if (present(out32)) out32(t) = int(rows(t), kind=int32)
            if (present(out64)) out64(t) = rows(t)
            if (present(dist)) dist(t) = ds(t)
        end do
    end subroutine near_scan

    !> `%nearest_sky` with an `int32` k into an `int32` buffer.
    !>
    !> `%nearest` on the sky: the `k` rows nearest `(ra, dec)` in angular separation, ordered by
    !> increasing separation, with `dist_deg` in DEGREES.
    !>
    !> **The expanding ball caps at 180 degrees here, not at the 90 that `%within_sky` enforces.**
    !> That limit is a usefulness judgement about a radius the caller chose; this radius is derived
    !> rather than chosen, and a sparse catalogue may legitimately need a wide one to reach `k`.
    integer(int64) function bind_near_sky_k32_i32(self, ra, dec, k, out, dist_deg) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the sky index to search.
        real(real64), intent(in) :: ra !! right ascension of the query point, in degrees.
        real(real64), intent(in) :: dec !! declination of the query point, in degrees.
        integer(int32), intent(in) :: k !! how many neighbours to report; must be >= 1.
        integer(int32), intent(out) :: out(:) !! caller's row indices, nearest first.
        real(real64), intent(out), optional :: dist_deg(:) !! angular separation, in degrees.

        call near_sky_scan(self, ra, dec, int(k, kind=int64), m, out32=out, dist_deg=dist_deg)
    end function bind_near_sky_k32_i32

    !> `%nearest_sky` with an `int32` k into an `int64` buffer. See `bind_near_sky_k32_i32`.
    integer(int64) function bind_near_sky_k32_i64(self, ra, dec, k, out, dist_deg) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the sky index to search.
        real(real64), intent(in) :: ra !! right ascension of the query point, in degrees.
        real(real64), intent(in) :: dec !! declination of the query point, in degrees.
        integer(int32), intent(in) :: k !! how many neighbours to report; must be >= 1.
        integer(int64), intent(out) :: out(:) !! caller's row indices, nearest first.
        real(real64), intent(out), optional :: dist_deg(:) !! angular separation, in degrees.

        call near_sky_scan(self, ra, dec, int(k, kind=int64), m, out64=out, dist_deg=dist_deg)
    end function bind_near_sky_k32_i64

    !> `%nearest_sky` with an `int64` k into an `int32` buffer. See `bind_near_sky_k32_i32`.
    integer(int64) function bind_near_sky_k64_i32(self, ra, dec, k, out, dist_deg) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the sky index to search.
        real(real64), intent(in) :: ra !! right ascension of the query point, in degrees.
        real(real64), intent(in) :: dec !! declination of the query point, in degrees.
        integer(int64), intent(in) :: k !! how many neighbours to report; must be >= 1.
        integer(int32), intent(out) :: out(:) !! caller's row indices, nearest first.
        real(real64), intent(out), optional :: dist_deg(:) !! angular separation, in degrees.

        call near_sky_scan(self, ra, dec, k, m, out32=out, dist_deg=dist_deg)
    end function bind_near_sky_k64_i32

    !> `%nearest_sky` with an `int64` k into an `int64` buffer. See `bind_near_sky_k32_i32`.
    integer(int64) function bind_near_sky_k64_i64(self, ra, dec, k, out, dist_deg) result(m)
        class(pf_spatial_index), intent(in), target :: self !! the sky index to search.
        real(real64), intent(in) :: ra !! right ascension of the query point, in degrees.
        real(real64), intent(in) :: dec !! declination of the query point, in degrees.
        integer(int64), intent(in) :: k !! how many neighbours to report; must be >= 1.
        integer(int64), intent(out) :: out(:) !! caller's row indices, nearest first.
        real(real64), intent(out), optional :: dist_deg(:) !! angular separation, in degrees.

        call near_sky_scan(self, ra, dec, k, m, out64=out, dist_deg=dist_deg)
    end function bind_near_sky_k64_i64

    !> The shared body of every `%nearest_sky` form: guard, convert, search, convert back.
    subroutine near_sky_scan(self, ra, dec, k, m, out32, out64, dist_deg)
        type(pf_spatial_index), intent(in), target :: self !! the sky index to search.
        real(real64), intent(in) :: ra !! right ascension of the query point, in degrees.
        real(real64), intent(in) :: dec !! declination of the query point, in degrees.
        integer(int64), intent(in) :: k !! how many neighbours to report.
        integer(int64), intent(out) :: m !! `min(k, %size())`, whatever the buffer holds.
        integer(int32), intent(out), optional :: out32(:) !! int32 output buffer.
        integer(int64), intent(out), optional :: out64(:) !! int64 output buffer.
        real(real64), intent(out), optional :: dist_deg(:) !! angular separation, in degrees.
        integer(int64) :: t, nfill
        real(real64) :: half
        real(real64) :: v(3) !! the query direction as a unit vector; a named local, see `query_point`.

        m = 0_int64
        if (.not. self%built_ok) error stop &
            "pf_spatial_index%nearest_sky: this index has not been built; call %build_sky first"
        if (self%metric_id /= PF_METRIC_SKY) error stop &
            "pf_spatial_index%nearest_sky: this index was built with %build, not %build_sky; use %nearest"
        ! Screened before the conversion rather than after it: `cos(Inf)` raises IEEE_INVALID on
        ! the spot, so a check on the resulting vector would come too late under nagfor.
        call spatial_check_finite([ra, dec], "pf_spatial_index%nearest_sky", "query ra/dec")
        v = sky_vector(ra, dec)
        call near_scan(self, v, k, m, out32=out32, out64=out64, dist=dist_deg)
        if (.not. present(dist_deg)) return
        ! The same rule as `sky_scan`: only the entries `near_scan` filled, which is the true count
        ! capped by the shortest buffer of the three, `out` included.
        nfill = m
        if (present(out32)) nfill = min(nfill, size(out32, kind=int64))
        if (present(out64)) nfill = min(nfill, size(out64, kind=int64))
        nfill = min(nfill, size(dist_deg, kind=int64))
        do t = 1_int64, nfill
            half = min(0.5_real64 * dist_deg(t), 1.0_real64)
            dist_deg(t) = 2.0_real64 * asin(half) * spatial_rad2deg
        end do
    end subroutine near_sky_scan

    ! ---- The k-th neighbour distance, for every point at once ----

    !> `%kth_distance` with an `int32` k.
    !>
    !> **For every point, the distance to its `k`-th nearest OTHER point** -- self excluded, which
    !> is the whole reason the operation exists: an adaptive-kernel density estimator wants the
    !> `k`-th neighbour, and counting the point itself shifts every bandwidth by one rank.
    !>
    !> `dist` comes back length `%size()` in the CALLER's row order. `k <= %size() - 1` is a
    !> precondition and is checked up front, which is better than returning a sentinel for points
    !> that cannot answer.
    !>
    !> **Cheaper than a loop of `%nearest`**, because the sweep runs in the index's own stored
    !> order: consecutive points are spatially adjacent, so the radius that converged for one seeds
    !> the next. That changes only the starting radius, never an answer.
    subroutine bind_kth_k32(self, k, dist, threads)
        class(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
        integer(int32), intent(in) :: k !! which neighbour to report; 1 <= k <= %size()-1.
        real(real64), allocatable, intent(out) :: dist(:) !! length n, in the caller's row order.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.

        call spatial_kth_worker(self, int(k, kind=int64), dist, PF_METRIC_EUCLIDEAN, threads)
    end subroutine bind_kth_k32

    !> `%kth_distance` with an `int64` k. See `bind_kth_k32`.
    subroutine bind_kth_k64(self, k, dist, threads)
        class(pf_spatial_index), intent(inout), target :: self !! the index to sweep.
        integer(int64), intent(in) :: k !! which neighbour to report; 1 <= k <= %size()-1.
        real(real64), allocatable, intent(out) :: dist(:) !! length n, in the caller's row order.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.

        call spatial_kth_worker(self, k, dist, PF_METRIC_EUCLIDEAN, threads)
    end subroutine bind_kth_k64

    !> `%kth_distance_sky` with an `int32` k. Answers in DEGREES.
    subroutine bind_kth_sky_k32(self, k, dist_deg, threads)
        class(pf_spatial_index), intent(inout), target :: self !! the sky index to sweep.
        integer(int32), intent(in) :: k !! which neighbour to report; 1 <= k <= %size()-1.
        real(real64), allocatable, intent(out) :: dist_deg(:) !! length n, in degrees, caller's row order.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.

        call kth_sky(self, int(k, kind=int64), dist_deg, threads)
    end subroutine bind_kth_sky_k32

    !> `%kth_distance_sky` with an `int64` k. See `bind_kth_sky_k32`.
    subroutine bind_kth_sky_k64(self, k, dist_deg, threads)
        class(pf_spatial_index), intent(inout), target :: self !! the sky index to sweep.
        integer(int64), intent(in) :: k !! which neighbour to report; 1 <= k <= %size()-1.
        real(real64), allocatable, intent(out) :: dist_deg(:) !! length n, in degrees, caller's row order.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.

        call kth_sky(self, k, dist_deg, threads)
    end subroutine bind_kth_sky_k64

    !> The shared body of both `%kth_distance_sky` forms: sweep in chords, report in degrees.
    subroutine kth_sky(self, k, dist_deg, threads)
        type(pf_spatial_index), intent(inout), target :: self !! the sky index to sweep.
        integer(int64), intent(in) :: k !! which neighbour to report.
        real(real64), allocatable, intent(out) :: dist_deg(:) !! length n, in degrees.
        integer, intent(in), optional :: threads !! team size; absent resolves automatically.
        integer(int64) :: t
        real(real64) :: half

        call spatial_kth_worker(self, k, dist_deg, PF_METRIC_SKY, threads)
        do t = 1_int64, size(dist_deg, kind=int64)
            half = min(0.5_real64 * dist_deg(t), 1.0_real64)
            dist_deg(t) = 2.0_real64 * asin(half) * spatial_rad2deg
        end do
    end subroutine kth_sky

    ! ---- Connected components ----

    !> `pf_connected_components` with an `int32` vertex count.
    !>
    !> **`nvert` is required and must not be derived from the edge list.** An isolated vertex never
    !> appears in an edge list, so `max(maxval(i), maxval(j))` silently drops every trailing
    !> isolated point and returns a `labels` array shorter than the catalogue -- a wrong answer
    !> with no symptom.
    !>
    !> `labels` comes back length `nvert`: a vertex in a qualifying component gets a label in
    !> `1..ncomp`, and **everything else gets 0**. `min_size` (default 1) is the smallest component
    !> that earns one, so by default every vertex is labelled and `sum(sizes)` is `nvert` -- the
    !> textbook reading, in which a singleton *is* a connected component.
    !>
    !> **A group finder wants `min_size = 2`**, and should say so at the call: in a group catalogue
    !> a galaxy with no neighbours is not a group of one, it is a field galaxy. Then `labels > 0` is
    !> the mask that selects group members and `ncomp` is the number of groups anyone would quote.
    !> That threshold is a domain choice, so it is the caller's to make rather than this routine's
    !> to assume.
    !>
    !> **Labels are assigned by ascending vertex index of first appearance**, which is a contract
    !> rather than a detail: without it the numbering would fall out of the union-find's internal
    !> root choices and a group catalogue would differ between compilers. `sizes` follows the same
    !> order.
    !>
    !> `min_size` is a plain default `integer` and has no `_int64` form: a component-size threshold
    !> above two billion would select nothing any real catalogue contains.
    subroutine components_n32(i, j, nvert, labels, ncomp, sizes, min_size)
        integer(int64), intent(in) :: i(:) !! one endpoint of each edge, in `1..nvert`.
        integer(int64), intent(in) :: j(:) !! the other endpoint of each edge, in `1..nvert`.
        integer(int32), intent(in) :: nvert !! how many vertices the graph has; >= 0.
        integer(int64), allocatable, intent(out) :: labels(:) !! length nvert; 0 where nothing qualifies.
        integer(int64), intent(out), optional :: ncomp !! how many components earned a label.
        integer(int64), allocatable, intent(out), optional :: sizes(:) !! length ncomp, in label order.
        integer, intent(in), optional :: min_size !! smallest component that earns a label; default 1.

        call spatial_components_worker(i, j, int(nvert, kind=int64), labels, ncomp, sizes, min_size)
    end subroutine components_n32

    !> `pf_connected_components` with an `int64` vertex count. See `components_n32`.
    subroutine components_n64(i, j, nvert, labels, ncomp, sizes, min_size)
        integer(int64), intent(in) :: i(:) !! one endpoint of each edge, in `1..nvert`.
        integer(int64), intent(in) :: j(:) !! the other endpoint of each edge, in `1..nvert`.
        integer(int64), intent(in) :: nvert !! how many vertices the graph has; >= 0.
        integer(int64), allocatable, intent(out) :: labels(:) !! length nvert; 0 where nothing qualifies.
        integer(int64), intent(out), optional :: ncomp !! how many components earned a label.
        integer(int64), allocatable, intent(out), optional :: sizes(:) !! length ncomp, in label order.
        integer, intent(in), optional :: min_size !! smallest component that earns a label; default 1.

        call spatial_components_worker(i, j, nvert, labels, ncomp, sizes, min_size)
    end subroutine components_n64

    ! ---- Shared argument checking ----

    !> Widens a caller's 2- or 3-element query point to the internal 3-vector, checking it against
    !> the rank the index was built with.
    !>
    !> **Every caller assigns the result to a named local and passes THAT** -- never
    !> `call spatial_scan(self, query_point(self, p, ...), ...)` inline. The scan procedures take
    !> the point as an explicit-shape `p(3)`, so an inline function result is argument-associated
    !> through a compiler-created array temporary. The two forms cost the same, but only the named
    !> local is silent: under ifx's debug profile (`-check arg_temp_created`) the inline form emits
    !> `forrtl: warning (406)` with a full traceback on EVERY call, which at one warning per query
    !> buries the test output it is printed into. `sky_vector`'s result is passed the same way, for
    !> the same reason.
    function query_point(self, p, what) result(q)
        class(pf_spatial_index), intent(in) :: self !! the index being queried.
        real(real64), intent(in) :: p(:) !! the caller's query point.
        character(len=*), intent(in) :: what !! the calling procedure, for the message.
        real(real64) :: q(3) !! the point as three coordinates, z zeroed for a 2D index.

        if (.not. self%built_ok) error stop "pf_spatial_index%" // what // &
            ": this index has not been built; call %build first"
        ! The metric is a property of the INDEX, and this is what that buys: a Euclidean query on a
        ! sky index would otherwise answer in chords to a caller who is thinking in degrees, which
        ! is a wrong answer wearing the right units.
        if (self%metric_id /= PF_METRIC_EUCLIDEAN) error stop "pf_spatial_index%" // what // &
            ": this index was built with %build_sky; use %within_sky, which answers in degrees"
        if (size(p) /= self%ncoord) error stop "pf_spatial_index%" // what // &
            ": the query point must have as many coordinates as the index was built with"
        q = 0.0_real64
        q(1:self%ncoord) = p
    end function query_point

    !> The unit vector for one `(ra, dec)` given in degrees.
    !>
    !> The standard conversion, with right ascension measured about the pole. Nothing here needs a
    !> special case at the poles or at 0h -- that is the point of working in vectors.
    function sky_vector(ra, dec) result(v)
        real(real64), intent(in) :: ra !! right ascension, in degrees.
        real(real64), intent(in) :: dec !! declination, in degrees.
        real(real64) :: v(3) !! the corresponding unit vector.
        real(real64) :: cd

        cd = cos(dec * spatial_deg2rad)
        v(1) = cd * cos(ra * spatial_deg2rad)
        v(2) = cd * sin(ra * spatial_deg2rad)
        v(3) = sin(dec * spatial_deg2rad)
    end function sky_vector

    !> The chord across the unit sphere subtended by an angle given in degrees.
    !>
    !> Strictly increasing over [0, 180 degrees], which is what makes a Euclidean ball of this
    !> radius select exactly the points within that angle -- an identity, not an approximation.
    real(real64) function sky_chord(deg) result(c)
        real(real64), intent(in) :: deg !! the angle, in degrees.

        c = 2.0_real64 * sin(0.5_real64 * deg * spatial_deg2rad)
    end function sky_chord

    !> A whole radius list from degrees to chords, validated on the way.
    !>
    !> **The one place a bulk sky radius is converted.** Every `_sky` bulk binding goes through it,
    !> so the range checks are stated once and a new bulk form cannot quietly skip them.
    function sky_chords(deg, what) result(c)
        real(real64), intent(in) :: deg(:) !! the angular radii, in degrees.
        character(len=*), intent(in) :: what !! the calling procedure, for the message.
        real(real64), allocatable :: c(:) !! the corresponding chords.
        integer :: k

        allocate (c(size(deg)))
        do k = 1, size(deg)
            if (.not. (deg(k) >= 0.0_real64)) error stop "pf_spatial_index%" // what // &
                ": every angular radius must be >= 0 and not NaN"
            if (deg(k) > spatial_max_sky_deg) error stop "pf_spatial_index%" // what // &
                ": an angular radius above 90 degrees is not a neighbour search; the ball then " // &
                "covers most of the sky and the grid has nothing to prune"
            c(k) = sky_chord(deg(k))
        end do
    end function sky_chords

    ! ---- Test-only hooks ----

    !> Forces the cell side every later `%build` uses, bypassing the model and the probe.
    !>
    !> **Test-only, and public only because it has to be**: the state it forces is private to this
    !> module and there is no `bind(C)` boundary to hide the hook behind, which is why this is a
    !> Fortran procedure rather than a `parquet_debug_*` entry point in the C++ wrapper. It exists
    !> because a threshold no test-sized fixture can reach is a threshold no test exercises. No
    !> library code calls it.
    subroutine parquet_debug_set_spatial_cell(h)
        real(real64), intent(in) :: h !! the cell side to force; <= 0 restores automatic tuning.

        dbg_cell = h
    end subroutine parquet_debug_set_spatial_cell

    !> How many candidate cell sizes the most recent tuning run evaluated.
    !>
    !> **The negative control for the probe.** A probe that never fires passes every "the answers
    !> are correct" test ever written for it, because the answers do not depend on the cell size;
    !> this is what lets a test assert that it ran at all. Test-only, and public for the same reason
    !> as `parquet_debug_set_spatial_cell`.
    integer(int64) function parquet_debug_spatial_probe_count() result(n)

        n = dbg_probe_count
    end function parquet_debug_spatial_probe_count

    !> Forces the initial radius every later expanding-ball search starts from.
    !>
    !> **Test-only, and required rather than convenient.** A shell that never expands passes every
    !> correctness test ever written for it, because the answers do not depend on how many rounds
    !> it took -- so this is what lets a test-sized fixture exercise a many-round expansion at all,
    !> and lets a deliberately tiny start be checked against a deliberately generous one. Public
    !> for the same reason `parquet_debug_set_spatial_cell` is: the state it forces is private to
    !> this module and there is no `bind(C)` boundary to hide the hook behind. No library code
    !> calls it.
    subroutine parquet_debug_set_spatial_shell_start(r)
        real(real64), intent(in) :: r !! the initial radius to force; <= 0 restores the derived one.

        dbg_shell_start = r
    end subroutine parquet_debug_set_spatial_shell_start

    !> How many expansion rounds every shell search has taken since the counters were reset.
    !>
    !> **A running total, not the last call's count** -- which is what makes it usable for the bulk
    !> sweep, where the interesting quantity is whether the radius carried over from one point to
    !> the next actually saved rounds. Test-only, public for the same reason as the setter above.
    integer(int64) function parquet_debug_spatial_shell_rounds() result(n)

        n = dbg_shell_rounds
    end function parquet_debug_spatial_shell_rounds

    !> How many automatic rebuilds have happened since the counters were reset.
    integer(int64) function parquet_debug_spatial_rebuilds() result(n)

        n = dbg_rebuilds
    end function parquet_debug_spatial_rebuilds

    !> The team size the most recent bulk query resolved. 0 before any has run.
    integer function parquet_debug_spatial_threads_used() result(n)

        n = dbg_threads_used
    end function parquet_debug_spatial_threads_used

    !> Forces the HEALPix resolution every later `%build_sky` will use, bypassing the probe.
    !>
    !> **Test-only, and required rather than convenient**, for the same reason
    !> `parquet_debug_set_spatial_cell` is: a test-sized fixture never reaches the resolutions a
    !> real catalogue does, so without this every test runs at whatever the probe happens to pick
    !> and no test can pin a resolution or compare two of them. Public because the state it forces
    !> is private to this module and there is no `bind(C)` boundary to hide the hook behind. No
    !> library code calls it.
    !>
    !> The buckets-per-point cap still applies on top, exactly as it does to an explicit `nside=`:
    !> this forces the resolution ASKED FOR, not the one used.
    subroutine parquet_debug_set_spatial_nside(n)
        integer(int64), intent(in) :: n !! the resolution to force; <= 0 restores the probe.

        dbg_nside = n
    end subroutine parquet_debug_set_spatial_nside

    !> Narrows the HEALPix walk's stack run buffer, so a small fixture reaches its fallback.
    !>
    !> **Test-only, and required rather than convenient.** A disc arrives as a handful of
    !> contiguous pixel runs, so the walk keeps a 512-column stack buffer and allocates only when
    !> a disc outgrows it -- which needs more than 256 rings, and so a resolution the
    !> buckets-per-point cap grants only to catalogues of about a million points. The fallback is
    !> therefore unreachable at any size a test can build, while sitting on a path whose failure
    !> is a silently short neighbour list. Public for the same reason
    !> `parquet_debug_set_spatial_cell` is: no `bind(C)` boundary exists here to hide it behind.
    subroutine parquet_debug_set_spatial_run_buffer(n)
        integer(int64), intent(in) :: n !! columns the walk may use; <= 0 restores the whole buffer.

        dbg_run_buf = n
    end subroutine parquet_debug_set_spatial_run_buffer

    !> How many pixels the HEALPix candidate walk has covered since the counters were reset.
    !>
    !> **The negative control for the backend, and it works in both directions.** An A/B test that
    !> asserts the two backends return identical results passes just as happily when both arms ran
    !> the same code -- so a test must also show that the HEALPix arm really walked pixels and that
    !> the 3D arm really did not. This counter answers both: non-zero after a HEALPix query, still
    !> zero after a 3D one. Test-only, and public for the same reason as
    !> `parquet_debug_set_spatial_cell` -- the state is private to this module and there is no
    !> `bind(C)` boundary to hide the hook behind.
    integer(int64) function parquet_debug_spatial_pixels_visited() result(n)

        n = dbg_pixels_visited
    end function parquet_debug_spatial_pixels_visited

    !> Clears the probe, rebuild, pixel and thread counters, and the forced cell size.
    subroutine parquet_debug_reset_spatial_counters()

        dbg_probe_count = 0_int64
        dbg_rebuilds = 0_int64
        dbg_pixels_visited = 0_int64
        dbg_threads_used = 0
        dbg_cell = -1.0_real64
        dbg_nside = 0_int64
        dbg_run_buf = 0_int64
        dbg_shell_start = -1.0_real64
        dbg_shell_rounds = 0_int64
    end subroutine parquet_debug_reset_spatial_counters

end module parquet_spatial ! GCOVR_EXCL_LINE
