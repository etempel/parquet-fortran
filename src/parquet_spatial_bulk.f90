!> The bulk query families: every point's neighbours at once, threaded.
!!
!! **These are the only entry points that may rebuild the index**, and they do it before opening the
!! parallel region. A query that can mutate the index is not read-only, so two threads querying
!! concurrently could both decide to rebuild and race on the same arrays -- a corrupted heap rather
!! than a wrong number. The prevention is a design rule and not a mechanism, which is exactly why
!! "let single queries rebuild too" must not be added later without one.
!!
!! **Each ball family is two passes over the same walk**: count, prefix-sum, fill. That is what
!! allocates the result exactly once at exactly the right size, and it is why `%count_all_within`
!! is not a special case but simply the first pass on its own. The one exception is the
!! line-of-sight pair sweep, which is one pass into per-thread buffers concatenated afterwards
!! (`spatial_pairs_los_worker`): its list is small by construction, and the second pass was most
!! of its cost.
!!
!! The sweep runs in STORED order rather than the caller's row order and writes each answer to the
!! row it belongs to. Stored order means consecutive query points sit in the same or neighbouring
!! cells, so the cells a query walks are usually already in cache from the previous one.
submodule (parquet_spatial) parquet_spatial_bulk
    implicit none

    !> One thread's pair list for the line-of-sight sweep: grown by doubling, concatenated after
    !> the sweep. A plain type with allocatable components, so one slot per thread lives in a
    !> SHARED array allocated before the parallel region, never in a `block` inside it -- the
    !> recorded ifx segfault (`fortran-gotchas.md`).
    type :: pair_buf
        integer(int64), allocatable :: a(:) !! the lower row of each pair.
        integer(int64), allocatable :: b(:) !! the higher row.
        real(real64), allocatable :: dp(:) !! the transverse separation, when the caller asked for either.
        real(real64), allocatable :: dl(:) !! the parallel separation, likewise.
        integer(int64) :: n = 0_int64 !! pairs held.
    end type pair_buf

    !> Pairs a thread's buffer starts with; it doubles from there.
    integer(int64), parameter :: pair_buf_first = 2048_int64

contains

    !> How many threads a bulk query should open, after every cap and the affinity clamp.
    module procedure spatial_threads
#ifdef _OPENMP
        use omp_lib, only: omp_get_max_threads, omp_in_parallel
#endif

        n = 1
        if (present(threads)) then
            if (threads < 1) error stop "pf_spatial_index: threads= must be >= 1"
            n = threads
        else
#ifdef _OPENMP
            n = omp_get_max_threads()
            ! Inside someone else's region the default is serial: `omp_get_max_threads` reads an
            ! ICV rather than the current team size, so it answers 8 inside an 8-thread region and
            ! a missing check means 8x8 threads. An explicit `threads=` is still honoured, which is
            ! what makes this a DEFAULT rather than the kind of refusal that would fire across the
            ! whole test suite.
            if (omp_in_parallel()) n = 1
#endif
            if (cfg_spatial_threads > 0) n = min(n, cfg_spatial_threads)
        end if
        ! One clamp, in one place, for every subsystem: `omp_get_max_threads` is what the
        ! environment asked for and `omp_get_num_procs` is what the affinity mask allows, and a
        ! team opened on more threads than there are processors time-shares them.
        n = parquet_clamp_to_affinity(n, "spatial")
        if (n < 1) n = 1
    end procedure spatial_threads

    !> Rebuilds when the radii a bulk query is about to use would choose a very different cell.
    module procedure spatial_maybe_rebuild
        real(real64) :: r_cur, r_new, h_cur, h_new, s2, s3

        ! Defensive: the one caller is `spatial_bulk_setup`, whose own first statement is this
        ! same check, so an unbuilt index has already aborted by the time this runs.
        if (.not. self%built_ok) error stop & ! GCOVR_EXCL_LINE
            "pf_spatial_index: this index has not been built; call %build first" ! GCOVR_EXCL_LINE
        if (self%npts < 2_int64) return
        if (self%r2sum <= 0.0_real64) return
        if (dbg_cell > 0.0_real64) return
        r_cur = self%r3sum / self%r2sum
        s2 = self%r2sum + sum(radii * radii)
        s3 = self%r3sum + sum(radii * radii * radii)
        r_new = s3 / s2
        ! Model against model, not model against the cell in use: the cell in use came from the
        ! PROBE and may legitimately sit a factor from what the model would have said, so comparing
        ! the two would trigger a rebuild on a perfectly well-tuned index.
        h_cur = spatial_model_cell(self%dims_eff, self%rho, r_cur)
        h_new = spatial_model_cell(self%dims_eff, self%rho, r_new)
        if (h_new <= spatial_rebuild_factor * h_cur .and. &
            h_new * spatial_rebuild_factor >= h_cur) return
        call spatial_fold_radii(self, radii, .true.)
        call spatial_retune(self)
        dbg_rebuilds = dbg_rebuilds + 1_int64
        ! Once per index, not once per query: the message exists to tell the caller their radius=
        ! hint was wrong, and repeating it per query would bury that under itself.
        !
        ! The suppression test stays here even though `parquet_emit_advice` repeats it, for the same
        ! reason `parquet_clamp_to_affinity` keeps its own: `self%warned` is claimed inside this
        ! branch, so a silenced query must not spend the one message an unsilenced later query would
        ! have received.
        if (.not. self%warned .and. .not. parquet_output_is_suppressed()) then
            self%warned = .true.
            ! A sky index accumulates chords; the caller asked in degrees and must read degrees.
            if (self%metric_id == PF_METRIC_SKY) then
                r_cur = 2.0_real64 * asin(min(0.5_real64 * r_cur, 1.0_real64)) * spatial_rad2deg
                r_new = 2.0_real64 * asin(min(0.5_real64 * r_new, 1.0_real64)) * spatial_rad2deg
            end if
            call parquet_emit_advice("pf_spatial_index: rebuilt for a query radius far from the one " // &
                "it was built for (built " // num_text(r_cur) // ", now " // num_text(r_new) // &
                ", cell " // num_text(self%cell_side) // "). Pass a better radius= to %build, or " // &
                "parquet_set_verbosity('silent') to silence this and every other piece of advice.")
        end if
    end procedure spatial_maybe_rebuild

    !> Every point's neighbours as CSR.
    module procedure spatial_all_within_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64), allocatable :: counts(:), offsets(:)
        ! The neighbour list is the big array and is filled in the kind asked for; `offsets` is
        ! only n+1 long, so it is accumulated in int64 whatever was asked for and narrowed once.
        integer(int32), allocatable :: nb32(:)
        integer(int64), allocatable :: nb64(:)
        integer(int64) :: n, t, i, u, m, s0, e0, total
        real(real64) :: p(3), r, rin
        integer :: nt, nr, nri
        logical :: direct, want_sort, want32

        want32 = present(offsets32)
        call spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads, radii_inner, nri)
        want_sort = .false.
        if (present(sorted)) want_sort = sorted
        ! A neighbour is a row of this index, so `n` bounds every value the list can hold.
        if (want32) call spatial_check_rows_i32(n, "pf_spatial_index%" // what)
        allocate (offsets(n + 1_int64))
        allocate (counts(max(n, 1_int64)))
        if (n == 0_int64) then
            offsets(1) = 1_int64
            call csr_emit(offsets, nb32, nb64, offsets32, neighbours32, offsets64, neighbours64, want32, 0_int64)
            return
        end if
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) private(t, i, u, m, p, r, rin)
        do t = 1_int64, n
            i = self%idx(t)
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = radii(1)
            if (nr > 1) r = radii(i)
            rin = inner_at(radii_inner, nri, i)
            call spatial_scan(self, p, r, m, r_inner=rin)
            counts(i) = m
        end do
        !$omp end parallel do
        offsets(1) = 1_int64
        do i = 1_int64, n
            offsets(i + 1_int64) = offsets(i) + counts(i)
        end do
        total = offsets(n + 1_int64) - 1_int64
        ! **A DIFFERENT quantity from the row count checked above, and that is the whole point.**
        ! `offsets` accumulates to the length of the neighbour list, which passes huge(int32) at a
        ! few tens of millions of points long before any row index does (`feature_risks.md`).
        if (want32) call spatial_check_total_i32(total, "pf_spatial_index%" // what)
        if (want32) then
            allocate (nb32(total), nb64(0))
        else
            allocate (nb64(total), nb32(0))
        end if
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) &
        !$omp     private(t, i, u, m, p, r, rin, s0, e0)
        do t = 1_int64, n
            i = self%idx(t)
            s0 = offsets(i)
            e0 = offsets(i + 1_int64) - 1_int64
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = radii(1)
            if (nr > 1) r = radii(i)
            rin = inner_at(radii_inner, nri, i)
            ! Written out per kind: a slice of an ABSENT optional cannot be formed, and the branch
            ! is loop-invariant.
            if (want32) then
                call spatial_scan(self, p, r, m, out32=nb32(s0:e0), r_inner=rin, sorted=want_sort)
            else
                call spatial_scan(self, p, r, m, out64=nb64(s0:e0), r_inner=rin, sorted=want_sort)
            end if
        end do
        !$omp end parallel do
        call csr_emit(offsets, nb32, nb64, offsets32, neighbours32, offsets64, neighbours64, want32, total)
    end procedure spatial_all_within_worker

    !> Hands a CSR sweep's two arrays to whichever of the four arguments the caller passed.
    !!
    !! `offsets` is narrowed element-wise -- it is only n+1 long, and its ceiling was checked
    !! against the neighbour total before the fill. The neighbour list is moved, never copied.
    subroutine csr_emit(offsets, nb32, nb64, offsets32, neighbours32, offsets64, neighbours64, want32, total)
        integer(int64), allocatable, intent(inout) :: offsets(:) !! the accumulated offsets, int64.
        integer(int32), allocatable, intent(inout) :: nb32(:) !! the neighbour list, int32 arm.
        integer(int64), allocatable, intent(inout) :: nb64(:) !! the neighbour list, int64 arm.
        integer(int32), allocatable, intent(out), optional :: offsets32(:) !! the caller's int32 offsets.
        integer(int32), allocatable, intent(out), optional :: neighbours32(:) !! the caller's int32 lists.
        integer(int64), allocatable, intent(out), optional :: offsets64(:) !! the caller's int64 offsets.
        integer(int64), allocatable, intent(out), optional :: neighbours64(:) !! the caller's int64 lists.
        logical, intent(in) :: want32 !! .true. when the caller asked for the int32 arm.
        integer(int64), intent(in) :: total !! entries the neighbour list holds.

        if (want32) then
            allocate (offsets32(size(offsets, kind=int64)))
            offsets32 = int(offsets, kind=int32)
            if (.not. allocated(nb32)) allocate (nb32(total))
            call move_alloc(nb32, neighbours32)
        else
            call move_alloc(offsets, offsets64)
            if (.not. allocated(nb64)) allocate (nb64(total))
            call move_alloc(nb64, neighbours64)
        end if
    end subroutine csr_emit

    !> The inner radius that applies to row `i`, as a plain value.
    !>
    !> Absent is reported as zero rather than through a flag, because `d >= 0` is what every point
    !> already satisfies -- so the ordinary ball and the annulus run the same code and the walk
    !> needs no second path. See `spatial_scan`.
    real(real64) function inner_at(radii_inner, nri, i) result(rin)
        real(real64), intent(in), optional :: radii_inner(:) !! the caller's inner radii, if any.
        integer, intent(in) :: nri !! `size(radii_inner)`, or 0 when it was absent.
        integer(int64), intent(in) :: i !! the caller's row index.

        rin = 0.0_real64
        if (nri < 1) return
        if (nri == 1) then
            rin = radii_inner(1)
        else
            rin = radii_inner(i)
        end if
    end function inner_at

    !> How many neighbours each point has, in the caller's row order.
    module procedure spatial_count_all_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        ! Counted in int64 and narrowed once at the end: a neighbour count is bounded by `n`, so
        ! the row-count guard below is what decides whether the int32 answer can carry it, and the
        ! array is only n long -- there is nothing to be saved by narrowing inside the loop.
        integer(int64), allocatable :: counts(:)
        integer(int64) :: n, t, i, u, m
        real(real64) :: p(3), r, rin
        integer :: nt, nr, nri
        logical :: direct, want32

        want32 = present(counts32)
        call spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads, radii_inner, nri)
        if (want32) call spatial_check_rows_i32(n, "pf_spatial_index%" // what)
        allocate (counts(n))
        if (n > 0_int64) then
            call spatial_storage(self, xs, ys, zs)
            direct = self%owns
            !$omp parallel do num_threads(nt) schedule(guided) default(shared) private(t, i, u, m, p, r, rin)
            do t = 1_int64, n
                i = self%idx(t)
                u = t
                if (.not. direct) u = i
                p(1) = xs(u)
                p(2) = ys(u)
                p(3) = zs(u)
                r = radii(1)
                if (nr > 1) r = radii(i)
                rin = inner_at(radii_inner, nri, i)
                call spatial_scan(self, p, r, m, r_inner=rin)
                counts(i) = m
            end do
            !$omp end parallel do
        end if
        if (want32) then
            allocate (counts32(n))
            counts32 = int(counts, kind=int32)
        else
            call move_alloc(counts, counts64)
        end if
    end procedure spatial_count_all_worker

    !> The per-point walk radius and acceptance terms one `PF_LINK_*` rule needs.
    module procedure spatial_link_terms
        real(real64) :: c, w, s
        integer :: k, nk

        nk = size(radii)
        allocate (walk(nk))
        ! **The radii reaching here are not yet validated**, and that is safe only because no arm
        ! below can trap on a bad one. The Euclidean arms are multiplications, so a NaN or a
        ! negative simply travels into `walk` and is refused by `spatial_bulk_setup` with the
        ! message it deserves. The sky arms take a square root, and they are reached only through
        ! `sky_chords`, which has already refused a NaN and anything past 90 degrees -- so the
        ! radicand sits in [1/2, 1] and never goes negative. Do NOT clamp it with `min`/`max` to
        ! "make sure": those compile to instructions that raise IEEE_INVALID on a NaN, which is
        ! exactly the abort this ordering avoids.
        select case (combine)
        case (PF_LINK_MAX, PF_LINK_MIN)
            ! `B(r, r) = r` for both, and neither needs a per-candidate term at all: the bound a
            ! pair carries is one of its two radii, and the ranking already arranges for the
            ! endpoint holding that one to be the endpoint doing the searching.
            walk = radii
        case (PF_LINK_MEAN)
            allocate (u(nk), v(nk))
            if (metric == PF_METRIC_SKY) then
                do k = 1, nk
                    c = radii(k)
                    ! `cos(theta/2)` from the chord with no inverse sine: `c = 2*sin(theta/2)`.
                    w = sqrt(1.0_real64 - 0.25_real64 * c * c)
                    ! `s = 2*cos(theta/4)`. Reached through `sqrt(2*(1 + w))` rather than through
                    ! `sqrt((1 - w)/2)` for the sine: at a small radius `w` sits just below 1, so
                    ! the second form subtracts two nearly equal doubles and throws away half the
                    ! significant digits of the very quantity a small radius depends on.
                    s = sqrt(2.0_real64 * (1.0_real64 + w))
                    u(k) = c / s            ! 2*sin(theta/4), formed without that cancellation
                    v(k) = 0.5_real64 * s   ! cos(theta/4)
                end do
            else
                u = 0.5_real64 * radii
                v = 1.0_real64
            end if
            ! `2*u*v` is `c` on the sky and `r` in the plane: the mean of a radius with itself.
            walk = radii
        case (PF_LINK_SUM)
            allocate (u(nk), v(nk))
            if (metric == PF_METRIC_SKY) then
                do k = 1, nk
                    c = radii(k)
                    w = sqrt(1.0_real64 - 0.25_real64 * c * c)
                    u(k) = c                ! 2*sin(theta/2)
                    v(k) = w                ! cos(theta/2)
                    ! `chord(2*theta) = 2*sin(theta) = 2*c*cos(theta/2)`, which is `2*u*v` -- and
                    ! NOT `2*c`, the chord doubled. The two agree only in the small-angle limit,
                    ! and the doubled chord is the larger, so using it would merely walk wider
                    ! than necessary; the identity is used because it is the tight bound.
                    walk(k) = 2.0_real64 * c * w
                end do
            else
                u = radii
                v = 1.0_real64
                walk = 2.0_real64 * radii
            end if
        end select
    end procedure spatial_link_terms

    !> Every neighbouring pair exactly once, with `i < j` in the caller's row numbering.
    !> Allocates a pair sweep's two output buffers in the kind asked for, the other kind empty.
    !!
    !! **The unwanted kind is allocated at length zero rather than left unallocated**, so that the
    !! sweep can always form the slice it passes to `spatial_scan` without testing `allocated()`
    !! inside its loop. An empty allocation costs nothing.
    subroutine pairs_alloc(a32, b32, a64, b64, want32, total)
        integer(int32), allocatable, intent(out) :: a32(:) !! lower row of each pair, int32 arm.
        integer(int32), allocatable, intent(out) :: b32(:) !! higher row of each pair, int32 arm.
        integer(int64), allocatable, intent(out) :: a64(:) !! lower row of each pair, int64 arm.
        integer(int64), allocatable, intent(out) :: b64(:) !! higher row of each pair, int64 arm.
        logical, intent(in) :: want32 !! .true. when the caller asked for the int32 arm.
        integer(int64), intent(in) :: total !! pairs the sweep will emit.

        if (want32) then
            allocate (a32(total), b32(total))
            allocate (a64(0), b64(0))
        else
            allocate (a64(total), b64(total))
            allocate (a32(0), b32(0))
        end if
    end subroutine pairs_alloc

    !> Moves a pair sweep's filled buffers into whichever of the four arguments the caller passed.
    !!
    !! `move_alloc` rather than assignment: the answer is the largest array a pair sweep holds, and
    !! copying it would double the peak the int32 arm exists to halve.
    subroutine pairs_emit(a32, b32, a64, b64, ii32, jj32, ii64, jj64, want32)
        integer(int32), allocatable, intent(inout) :: a32(:) !! lower row of each pair, int32 arm.
        integer(int32), allocatable, intent(inout) :: b32(:) !! higher row of each pair, int32 arm.
        integer(int64), allocatable, intent(inout) :: a64(:) !! lower row of each pair, int64 arm.
        integer(int64), allocatable, intent(inout) :: b64(:) !! higher row of each pair, int64 arm.
        integer(int32), allocatable, intent(out), optional :: ii32(:) !! the caller's int32 lower rows.
        integer(int32), allocatable, intent(out), optional :: jj32(:) !! the caller's int32 higher rows.
        integer(int64), allocatable, intent(out), optional :: ii64(:) !! the caller's int64 lower rows.
        integer(int64), allocatable, intent(out), optional :: jj64(:) !! the caller's int64 higher rows.
        logical, intent(in) :: want32 !! .true. when the caller asked for the int32 arm.

        if (want32) then
            call move_alloc(a32, ii32)
            call move_alloc(b32, jj32)
        else
            call move_alloc(a64, ii64)
            call move_alloc(b64, jj64)
        end if
    end subroutine pairs_emit

    module procedure spatial_pairs_within_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64), allocatable :: counts(:), heads(:), keys(:)
        real(real64), allocatable :: walk(:), urow(:), vrow(:), us(:), vs(:)
        ! The answer is built in locals of ONE kind and moved into whichever pair the caller
        ! asked for. The unwanted pair is allocated empty rather than left unallocated, so that
        ! the slice below always has something to name; the branch picks the allocated one.
        integer(int32), allocatable :: a32(:), b32(:)
        integer(int64), allocatable :: a64(:), b64(:)
        integer(int64) :: n, t, i, u, m, s0, e0, total, k
        real(real64) :: p(3), r, rin, uself, vself
        integer :: nt, nr, rule
        logical :: direct, want_bound, want32

        want32 = present(ii32)

        rule = PF_LINK_MAX
        if (present(combine)) rule = combine
        if (rule /= PF_LINK_MAX .and. rule /= PF_LINK_MIN .and. rule /= PF_LINK_MEAN .and. &
            rule /= PF_LINK_SUM) error stop "pf_spatial_index%" // what // &
            ": combine= must be PF_LINK_MAX, PF_LINK_MIN, PF_LINK_MEAN or PF_LINK_SUM"
        ! **Every guard below sees the WALK radii, not the ones the caller passed**, and that is
        ! deliberate: under `PF_LINK_SUM` the sweep really does walk twice each radius, so the
        ! length check, the NaN screen, the periodic half-box guard, the rebuild decision and the
        ! index's recorded effective radius all have to be about the balls that are actually
        ! walked. A consequence worth knowing: after a sum-rule sweep `%effective_radius()` and
        ! the rebuild warning quote the doubled radius. For every other rule the two are equal.
        call spatial_link_terms(radii, rule, expect_metric, walk, urow, vrow)
        ! Scalar only, and validated against the OUTER radii through the same path every other
        ! bulk form uses -- which is what makes a per-point outer radius with a scalar inner one
        ! check against the smallest of them rather than against nothing. `nri` is not asked for:
        ! the value is already to hand, and this sweep applies it to every point alike.
        rin = 0.0_real64
        if (present(r_inner)) then
            rin = r_inner
            call spatial_bulk_setup(self, walk, expect_metric, n, nr, nt, threads, [rin])
        else
            call spatial_bulk_setup(self, walk, expect_metric, n, nr, nt, threads)
        end if
        ! The row indices this sweep reports are bounded by `n`, so one comparison before anything
        ! is allocated decides whether an int32 answer can carry them (`feature_risks.md`).
        if (want32) call spatial_check_rows_i32(n, "pf_spatial_index%" // what)
        if (n == 0_int64) then
            call pairs_alloc(a32, b32, a64, b64, want32, 0_int64)
            call pairs_emit(a32, b32, a64, b64, ii32, jj32, ii64, jj64, want32)
            return
        end if
        ! **One radius makes every rule the walk radius**, so the per-candidate bound is switched
        ! off rather than evaluated to a foregone conclusion: `PF_LINK_MEAN` of a radius with
        ! itself is that radius, and `PF_LINK_SUM`'s bound is its own already-doubled walk. This
        ! is not only an optimisation -- `bnd_*` are addressed by stored position and would have
        ! to be length `n` while `radii` is length 1.
        want_bound = allocated(urow) .and. nr > 1
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        ! `PF_LINK_MIN` ranks the other way up. Its bound is the SMALLER of the two radii, so the
        ! endpoint that can be relied on to reach its partner is the one with the smaller ball,
        ! and it is that endpoint which must do the searching. Every other rule's bound is at
        ! least the larger radius, so the larger ball searches.
        call pair_order_keys(self, walk, nr, n, nt, rule == PF_LINK_MIN, keys)
        if (want_bound) then
            ! Into STORED order, for the reason `pair_order_keys` gives about its own keys: the
            ! scan reads one of these per CANDIDATE, so it must be addressed the way a coordinate
            ! is, not gathered through the row permutation on the hottest loop in the module.
            allocate (us(n), vs(n))
            do t = 1_int64, n
                us(t) = urow(self%idx(t))
                vs(t) = vrow(self%idx(t))
            end do
        end if
        allocate (counts(n), heads(n + 1_int64))
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) &
        !$omp     private(t, i, u, m, p, r, uself, vself)
        do t = 1_int64, n
            i = self%idx(t)
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = walk(1)
            if (nr > 1) r = walk(i)
            ! `min_key` makes the walk report only points ranked above this one, so each pair is
            ! produced by exactly one of its two endpoints and there is nothing to de-duplicate.
            ! **The two calls are written out rather than folded behind unallocated actuals.**
            ! Passing an unallocated allocatable to an optional dummy does make it absent, and
            ! this project relies on that elsewhere -- but not across a parallel region, where ifx
            ! has a recorded segfault in the privatization scaffolding for exactly that shape.
            ! The branch is loop-invariant and costs nothing measurable.
            if (want_bound) then
                uself = us(t)
                vself = vs(t)
                call spatial_scan(self, p, r, m, min_key=keys(t), keys=keys, r_inner=rin, &
                    bnd_u_self=uself, bnd_v_self=vself, bnd_u=us, bnd_v=vs)
            else
                call spatial_scan(self, p, r, m, min_key=keys(t), keys=keys, r_inner=rin)
            end if
            counts(t) = m
        end do
        !$omp end parallel do
        ! Prefix over STORED order here, unlike the CSR forms: the output is a flat pair list with
        ! no per-row addressing, so the cheapest consistent order is the one the sweep runs in.
        heads(1) = 1_int64
        do t = 1_int64, n
            heads(t + 1_int64) = heads(t) + counts(t)
        end do
        total = heads(n + 1_int64) - 1_int64
        call pairs_alloc(a32, b32, a64, b64, want32, total)
        !$omp parallel do num_threads(nt) schedule(guided) default(shared) &
        !$omp     private(t, i, u, m, p, r, s0, e0, k, uself, vself)
        do t = 1_int64, n
            i = self%idx(t)
            s0 = heads(t)
            e0 = heads(t + 1_int64) - 1_int64
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            r = walk(1)
            if (nr > 1) r = walk(i)
            ! `i` is the endpoint that did the SEARCHING, which under a per-point radius is the one
            ! with the larger ball and so not necessarily the lower row. Each arm orders its pairs
            ! here, so both kinds carry the same contract: every unordered pair once, always with
            ! i < j. The kinds are written out rather than folded because a slice of an ABSENT
            ! optional cannot be formed at all, and the branch is loop-invariant.
            if (want32) then
                if (want_bound) then
                    uself = us(t)
                    vself = vs(t)
                    call spatial_scan(self, p, r, m, out32=b32(s0:e0), min_key=keys(t), keys=keys, &
                        r_inner=rin, bnd_u_self=uself, bnd_v_self=vself, bnd_u=us, bnd_v=vs)
                else
                    call spatial_scan(self, p, r, m, out32=b32(s0:e0), min_key=keys(t), keys=keys, r_inner=rin)
                end if
                do k = s0, e0
                    if (int(b32(k), kind=int64) > i) then
                        a32(k) = int(i, kind=int32)
                    else
                        a32(k) = b32(k)
                        b32(k) = int(i, kind=int32)
                    end if
                end do
            else
                if (want_bound) then
                    uself = us(t)
                    vself = vs(t)
                    call spatial_scan(self, p, r, m, out64=b64(s0:e0), min_key=keys(t), keys=keys, &
                        r_inner=rin, bnd_u_self=uself, bnd_v_self=vself, bnd_u=us, bnd_v=vs)
                else
                    call spatial_scan(self, p, r, m, out64=b64(s0:e0), min_key=keys(t), keys=keys, r_inner=rin)
                end if
                do k = s0, e0
                    if (b64(k) > i) then
                        a64(k) = i
                    else
                        a64(k) = b64(k)
                        b64(k) = i
                    end if
                end do
            end if
        end do
        !$omp end parallel do
        call pairs_emit(a32, b32, a64, b64, ii32, jj32, ii64, jj64, want32)
    end procedure spatial_pairs_within_worker

    !> Gives every point the ORDER KEY that decides which endpoint of a pair reports it.
    !>
    !> **A pair belongs in the output when EITHER ball reaches the other**, `d <= max(r_a, r_b)` --
    !> and only the endpoint with the larger radius is guaranteed to walk far enough to see its
    !> partner at all, so that is the one that has to do the reporting. Ranking the points by
    !> DESCENDING radius and emitting only upward through the rank therefore produces every
    !> qualifying pair exactly once, from the one endpoint whose own sweep can find it.
    !>
    !> Ties can fall either way and it does not matter: any permutation gives every point a
    !> distinct rank, which is the whole of what the emit-once rule needs.
    !>
    !> A single radius needs no sort — the balls are all the same size, so both endpoints see each
    !> other and any total order will do. The caller's row index is the one that keeps the output
    !> in the order it has always been in.
    !>
    !> **`ascending` inverts the rank for `PF_LINK_MIN` and for that rule only.** Its bound is the
    !> SMALLER of a pair's two radii, so the endpoint whose own ball is guaranteed to reach the
    !> other is the one with the smaller ball -- the mirror image of every other rule, and the one
    !> place where "rank by descending radius" would drop pairs instead of duplicating them.
    subroutine pair_order_keys(self, radii, nr, n, nt, ascending, keys)
        type(pf_spatial_index), intent(in) :: self !! the index about to be swept.
        real(real64), intent(in) :: radii(:) !! one walk radius, or one per point in row order.
        integer, intent(in) :: nr !! `size(radii)`.
        integer(int64), intent(in) :: n !! how many points the index holds.
        integer, intent(in) :: nt !! the team size, for the sort.
        logical, intent(in) :: ascending !! .true. ranks the SMALLEST radius first; `PF_LINK_MIN` only.
        integer(int64), allocatable, intent(out) :: keys(:) !! order key per STORED position.
        integer(int64), allocatable :: ord(:), rank_row(:)
        integer(int64) :: t, k

        allocate (keys(n))
        if (nr <= 1) then
            keys = self%idx
            return
        end if
        call pf_argsort(radii, ord, descending=.not. ascending, threads=nt)
        allocate (rank_row(n))
        ! A scatter through a permutation: every element is written once, so the threads never
        ! meet. Measured at a third of the line-of-sight sweep's whole call when serial, with the
        ! gather below (`feature_spatial_phase0.md`).
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(k)
        do k = 1_int64, n
            rank_row(ord(k)) = k
        end do
        !$omp end parallel do
        deallocate (ord)
        ! Into STORED order, so the sweep reads a key with the same locality as a coordinate
        ! instead of gathering one per candidate. Freeing `ord` first keeps the peak at two of
        ! these arrays rather than three.
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(t)
        do t = 1_int64, n
            keys(t) = rank_row(self%idx(t))
        end do
        !$omp end parallel do
    end subroutine pair_order_keys

    !> Validates a bulk call's radius list, rebuilds if it disagrees badly with the build, and
    !> resolves the team size. The three things every bulk family does identically, before any of
    !> them opens a parallel region.
    subroutine spatial_bulk_setup(self, radii, expect_metric, n, nr, nt, threads, radii_inner, nri)
        type(pf_spatial_index), intent(inout), target :: self !! the index about to be swept.
        real(real64), intent(in) :: radii(:) !! one radius, or one per point, in the index's own units.
        integer, intent(in) :: expect_metric !! the metric the caller's radii were stated in.
        integer(int64), intent(out) :: n !! how many points the index holds.
        integer, intent(out) :: nr !! `size(radii)`, so the sweep can pick per-row or scalar.
        integer, intent(out) :: nt !! the team size to open.
        integer, intent(in), optional :: threads !! an explicit request; absent resolves automatically.
        real(real64), intent(in), optional :: radii_inner(:) !! inner radii, when the call asked for an annulus.
        integer, intent(out), optional :: nri !! `size(radii_inner)`, or 0 when it was absent.
        integer :: nin
        logical :: bad

        if (.not. self%built_ok) error stop &
            "pf_spatial_index: this index has not been built; call %build first"
        ! **Both directions, from one place.** By the time a worker runs, its radii have already
        ! been converted into the index's own units, so nothing downstream can tell degrees from
        ! chords -- which is exactly the confusion this catches. Every bulk binding therefore
        ! declares which metric it converted FROM, and a mismatch aborts here rather than
        ! producing a plausible answer to a question nobody asked. A new bulk form comes through
        ! here too, converting an angular radius at its own entry point through `sky_chords`
        ! (scenarios `spatial_sky_bulk_refused`, `spatial_sky_bulk_on_euclidean`).
        if (self%metric_id /= expect_metric) then
            if (expect_metric == PF_METRIC_SKY) error stop &
                "pf_spatial_index: this is a Euclidean index; use the plain bulk forms, not the _sky ones"
            error stop &
                "pf_spatial_index: this index was built with %build_sky; use the _sky bulk forms, " // &
                "which take an angular radius in degrees"
        end if
        n = self%npts
        nr = size(radii)
        if (nr /= 1 .and. int(nr, kind=int64) /= n) error stop &
            "pf_spatial_index: radius must be one value or one per point"
        ! `.not. all(>= 0)` rather than `any(< 0)`: a NaN answers .false. to both comparisons, so
        ! the `any` form let a NaN radius through -- to `minval`/`maxval` below and to the tuner,
        ! where it traps under a compiler with the IEEE traps unmasked. Same fix as %build's.
        if (.not. all(radii >= 0.0_real64)) error stop "pf_spatial_index: every radius must be >= 0"
        if (present(nri)) nri = 0
        if (present(radii_inner)) then
            nin = size(radii_inner)
            ! `nri` is optional on its own: a sweep that applies one inner radius to every point
            ! has no use for the size, and asking for it would leave a dead store behind.
            if (present(nri)) nri = nin
            if (nin /= 1 .and. int(nin, kind=int64) /= n) error stop &
                "pf_spatial_index: the inner radius must be one value or one per point"
            if (.not. all(radii_inner >= 0.0_real64)) error stop &
                "pf_spatial_index: every inner radius must be >= 0"
            ! Compared against whichever shape the outer radii came in: a scalar inner radius has
            ! to clear the SMALLEST outer one, and a scalar outer radius has to be cleared by the
            ! largest inner one, or some row would be asked for an annulus turned inside out.
            if (nin == nr) then
                bad = any(radii_inner > radii)
            else if (nin == 1) then
                bad = radii_inner(1) > minval(radii)
            else
                bad = maxval(radii_inner) > radii(1)
            end if
            if (bad) error stop &
                "pf_spatial_index: every inner radius must not exceed the outer radius for that point"
        end if
        call spatial_maybe_rebuild(self, radii)
        nt = spatial_threads(threads)
        dbg_threads_used = nt
    end subroutine spatial_bulk_setup

    !> The guards both line-of-sight queries share.
    module procedure spatial_los_prepare

        if (.not. self%built_ok) error stop "pf_spatial_index%" // what // &
            ": this index has not been built; call %build first"
        if (self%metric_id /= PF_METRIC_EUCLIDEAN) error stop "pf_spatial_index%" // what // &
            ": this index was built with %build_sky; a line-of-sight cylinder needs Cartesian coordinates about an observer"
        if (self%ncoord /= 3) error stop "pf_spatial_index%" // what // &
            ": this index is two-dimensional; a line of sight needs three coordinates"
        if (self%periodic_on) error stop "pf_spatial_index%" // what // &
            ": this index is periodic; a line of sight has no meaning under the minimum image"
        ! Every route that leaves `radial` clear is refused above, so this is unreachable. ! GCOVR_EXCL_LINE
        if (.not. self%radial) error stop "pf_spatial_index%" // what // ": not a radial index" ! GCOVR_EXCL_LINE
        if (self%at_obs) error stop "pf_spatial_index%" // what // &
            ": a stored point coincides with the observer (distance 0), so it has no line of sight; " // &
            "move the observer= at %build or drop the point"
    end procedure spatial_los_prepare

    !> Every pair inside the line-of-sight cylinder the rule builds. The interface block's banner
    !> states the criterion; this is the sweep.
    !>
    !> **Each emitter walks its OWN cylinder, ranked by transverse length.** Rank descending by
    !> `b_perp` (ascending for `PF_LINK_MIN`) and emit upward through the rank: an emitter's
    !> partners then have the smaller transverse length, so `d_perp` under every rule is within its
    !> own `b_perp` (twice it under the sum), while `d_par` is within its own `b_par` under the
    !> union, the intersection and rule 0, and within the largest `b_par` ranked at or below it
    !> under the mean -- a suffix maximum along the rank -- and twice that under the sum. Under the
    !> union each endpoint walks exactly its own cylinder and a pair lying in both is emitted once
    !> by the tiebreak `spatial_scan` documents. So one rank serves all four rules and every pair
    !> is emitted exactly once, from the endpoint whose own walk is guaranteed to see it.
    !>
    !> **The walked cylinder is bounded in DISTANCE by what the parallel window can hold.** For an
    !> emitter with parallel envelope `W` (in `los`'s units) the partners' distances lie within
    !> `[min D, max D]` over the stored points within `W` of its own `los` -- read off the sorted
    !> tie groups `%build` kept, a range minimum and maximum per point through two segment trees
    !> built per call, and only for the emitters whose covering ball under the catalogue-wide bound
    !> does not already fit a cell -- or, without `los=` (where `los` IS the distance) and under the
    !> test-only global arm, within `D +- max(L*W, g)`. `los_walk_shape` pads that cylinder into one that
    !> contains every accepted partner, or picks the covering ball when that is the cheaper walk --
    !> the cylinder longer than the ball is wide, or the ball no wider than a cell -- and both
    !> routes count themselves (`parquet_debug_spatial_los_walk`).
    !>
    !> **The per-candidate lengths are built in STORED order and always passed**, so the walk has
    !> one shape; with a single pair of lengths every rule is the emitter's own cylinder (doubled
    !> for the sum), the arrays hold that, and rule 0 never reads them.
    !>
    !> **One pass, not two.** The emitters are swept in chunks of `spatial_sweep_chunk` in stored
    !> order, one chunk at a time per thread under dynamic scheduling, each thread appending the
    !> pairs it finds to a buffer of its own (`los_sweep_lean`, `pair_buf`); the chunks are then
    !> concatenated in chunk order, which is stored order, so the list is the one a serial sweep
    !> produces whatever the team size. The peak memory is the pair list once in the buffers (each
    !> buffer at most twice its share, from the doubling) and once in the output: up to three lists,
    !! against one plus 16 bytes per point for the count-then-fill shape the ball sweeps keep.
    !> Accepted because a line-of-sight list is small by construction -- the linking lengths are a
    !> fraction of the mean separation -- and the second pass was most of the sweep's cost
    !> (`feature_spatial_phase0.md`).
    module procedure spatial_pairs_los_worker
#ifdef _OPENMP
        use omp_lib, only: omp_get_thread_num
#endif
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:), ls(:)
        integer(int64), allocatable :: heads(:), keys(:), coff(:), ccnt(:), cthr(:)
        real(real64), allocatable :: walk(:), bps(:), bls(:), dp(:), dl(:), qlo(:), qhi(:), suf(:), tmin(:), tmax(:)
        real(real64), allocatable :: wpar(:)
        ! One pair buffer per thread, in a SHARED array allocated before the region; see the sweep.
        type(pair_buf), allocatable :: bufs(:)
        ! As the ball sweep: one kind is filled, the other allocated empty. See `pairs_alloc`.
        integer(int32), allocatable :: a32(:), b32(:)
        integer(int64), allocatable :: a64(:), b64(:)
        integer(int64) :: n, t, i, u, s0, e0, total, k, ng, a, b, ncyl, nbal, ntest, chunk, nchunk, c
        real(real64) :: p(3), q(3), r, qq, scale, hcell, rg
        integer :: nt, nr, rule, lrule, tid
        logical :: direct, want_sep, ball_all, local, need_local, want32, want_tie

        want32 = present(ii32)
        rule = PF_LINK_MAX
        if (present(combine)) rule = combine
        if (rule /= PF_LINK_MAX .and. rule /= PF_LINK_MIN .and. rule /= PF_LINK_MEAN .and. &
            rule /= PF_LINK_SUM) error stop "pf_spatial_index%" // what // &
            ": combine= must be PF_LINK_MAX, PF_LINK_MIN, PF_LINK_MEAN or PF_LINK_SUM"
        call spatial_los_prepare(self, what)
        n = self%npts
        nr = size(b_perp)
        if (size(b_par) /= nr) error stop "pf_spatial_index%" // what // &
            ": b_perp and b_par must be the same length: one value each, or one per point each"
        if (nr /= 1 .and. int(nr, kind=int64) /= n) error stop "pf_spatial_index%" // what // &
            ": b_perp and b_par must be one value each or one per point each"
        ! `.not. all(>= 0)` rather than `any(< 0)`, so a NaN is refused too: it answers .false. to
        ! both comparisons. The parallel lengths are multiplied by L below and the transverse ones
        ! squared, so both must be clean before either reaches arithmetic a trap could see.
        if (.not. all(b_perp >= 0.0_real64)) error stop "pf_spatial_index%" // what // &
            ": every b_perp and b_par must be >= 0 and not NaN"
        if (.not. all(b_par >= 0.0_real64)) error stop "pf_spatial_index%" // what // &
            ": every b_perp and b_par must be >= 0 and not NaN"
        scale = 1.0_real64
        if (rule == PF_LINK_SUM) scale = 2.0_real64
        ! The radii the sweep is ABOUT, which is what the rebuild decision, the recorded effective
        ! radius and the rank must see: the cross-section `scale * b_perp` for the cylinder walk,
        ! whose rank is `b_perp`'s; the covering ball `scale * sqrt(b_perp**2 + max(L*b_par, g)**2)`
        ! for the test-only ball walk, ranked as Stage 1's balls are. The setup may re-tune the
        ! index, which re-buckets it, so everything below that is addressed by stored position is
        ! built after this call and never before.
        ball_all = dbg_los_walk == 1
        allocate (walk(nr))
        do k = 1_int64, int(nr, kind=int64)
            if (ball_all) then
                qq = self%lip * b_par(k)
                if (self%tie_spread > qq) qq = self%tie_spread
                walk(k) = scale * sqrt(b_perp(k) * b_perp(k) + qq * qq)
            else
                walk(k) = scale * b_perp(k)
            end if
        end do
        call spatial_bulk_setup(self, walk, PF_METRIC_EUCLIDEAN, n, nr, nt, threads)
        ! One comparison before anything is allocated -- see the ball sweep for why it is `n`.
        if (want32) call spatial_check_rows_i32(n, "pf_spatial_index%" // what)
        if (n == 0_int64) then
            call pairs_alloc(a32, b32, a64, b64, want32, 0_int64)
            call pairs_emit(a32, b32, a64, b64, ii32, jj32, ii64, jj64, want32)
            if (present(dperp)) allocate (dperp(0))
            if (present(dpar)) allocate (dpar(0))
            return
        end if
        ! A parallel window spanning the catalogue's whole depth is what a `b_par` given in the
        ! coordinates' units against a redshift `los` looks like from inside. Said, not refused:
        ! the sweep is slow and exact, and the window can be meant.
        if (n > 1_int64) then
            if (self%lip * maxval(b_par) > self%d_hi - self%d_lo) then
                call parquet_emit_advice("pf_spatial_index%" // what // ": L x max(b_par) = " // &
                    num_text(self%lip * maxval(b_par)) // " exceeds the catalogue's range in distance from " // &
                    "the observer (" // num_text(self%d_hi - self%d_lo) // "), so the parallel window spans " // &
                    "the whole catalogue along every line of sight; is b_par in los='s units?")
            end if
        end if
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        ! The cell side the walk choice is keyed on: a covering ball no wider than this touches at
        ! most two cells per axis, which the cylinder cannot beat by enough to pay its own setup
        ! (`feature_fof_S2.md`, Stage 3's measurement). Zero switches the rule off, for the forced
        ! cylinder walk.
        hcell = min(self%cell(1), self%cell(2), self%cell(3))
        if (dbg_los_walk == 2) hcell = 0.0_real64
        call pair_order_keys(self, walk, nr, n, nt, rule == PF_LINK_MIN, keys)
        allocate (bps(n), bls(n))
        if (nr > 1) then
            !$omp parallel do num_threads(nt) schedule(static) default(shared) private(t)
            do t = 1_int64, n
                bps(t) = b_perp(self%idx(t))
                bls(t) = b_par(self%idx(t))
            end do
            !$omp end parallel do
            lrule = rule
        else
            bps = scale * b_perp(1)
            bls = scale * b_par(1)
            lrule = 0
        end if
        ! The parallel coordinate the accept test reads per candidate: the stored one, or the
        ! distance from the observer when none was stored -- the same array either way, so the
        ! scan needs no second path.
        if (self%has_los) then
            ls => self%los_s
        else
            ls => self%d_s
        end if
        want_sep = present(dperp) .or. present(dpar)
        ! The range of distances each emitter's partners can occupy, in stored order; empty under
        ! the ball walk, which has no use for it.
        if (ball_all) then
            allocate (qlo(0), qhi(0))
        else
            allocate (qlo(n), qhi(n))
            if (nr > 1 .and. (rule == PF_LINK_MEAN .or. rule == PF_LINK_SUM)) then
                ! The suffix maximum of the parallel length along the rank: rank k's value is the
                ! largest `b_par` among the points ranked k or below, which is where an emitter's
                ! partners come from under these two rules.
                allocate (suf(n))
                ! A scatter through a permutation: every key is written once, so the threads never
                ! meet. The suffix pass itself is a dependency chain and stays serial.
                !$omp parallel do num_threads(nt) schedule(static) default(shared) private(t)
                do t = 1_int64, n
                    suf(keys(t)) = bls(t)
                end do
                !$omp end parallel do
                do k = n - 1_int64, 1_int64, -1_int64
                    if (suf(k + 1_int64) > suf(k)) suf(k) = suf(k + 1_int64)
                end do
            end if
            ! The parallel envelope per emitter, in `los`'s units, read twice below.
            allocate (wpar(n))
            !$omp parallel do num_threads(nt) schedule(static) default(shared) private(t)
            do t = 1_int64, n
                wpar(t) = bls(t)
                if (nr > 1) then
                    if (rule == PF_LINK_MEAN) wpar(t) = suf(keys(t))
                    if (rule == PF_LINK_SUM) wpar(t) = 2.0_real64 * suf(keys(t))
                end if
            end do
            !$omp end parallel do
            local = self%has_los .and. .not. dbg_los_global
            if (local) local = allocated(self%los_grp)
            ! The catalogue-wide bound first, for every emitter: it needs no structure, and an
            ! emitter whose covering ball under it already fits a cell walks that ball whatever its
            ! own window would say -- the window can only shrink the ball -- so it needs no window at
            ! all. When no emitter needs one the trees are never built, which is what keeps a sweep
            ! that walks balls throughout as cheap as the ball sweep it replaces.
            need_local = .false.
            !$omp parallel do num_threads(nt) schedule(static) default(shared) private(t, qq, rg) &
            !$omp     reduction(.or.:need_local)
            do t = 1_int64, n
                qq = self%lip * wpar(t)
                if (self%tie_spread > qq) qq = self%tie_spread
                qlo(t) = self%d_s(t) - qq
                qhi(t) = self%d_s(t) + qq
                if (local) then
                    rg = walk(1)
                    if (nr > 1) rg = walk(self%idx(t))
                    rg = sqrt(rg * rg + qq * qq)
                    if (2.0_real64 * rg > hcell) need_local = .true.
                end if
            end do
            !$omp end parallel do
            if (local .and. need_local) then
                ng = size(self%los_grp, kind=int64)
                call seg_build(self%los_gmin, self%los_gmax, ng, tmin, tmax)
                !$omp parallel do num_threads(nt) schedule(static) default(shared) private(t, qq, rg, a, b)
                do t = 1_int64, n
                    qq = self%lip * wpar(t)
                    if (self%tie_spread > qq) qq = self%tie_spread
                    rg = walk(1)
                    if (nr > 1) rg = walk(self%idx(t))
                    rg = sqrt(rg * rg + qq * qq)
                    if (2.0_real64 * rg <= hcell) cycle
                    ! The emitter's own group lies inside its window, so the window is never empty.
                    call los_window(self%los_grp, ng, ls(t), wpar(t), a, b)
                    qlo(t) = seg_min(tmin, ng, a, b)
                    qhi(t) = seg_max(tmax, ng, a, b)
                end do
                !$omp end parallel do
            end if
        end if
        ! ---- The sweep: chunks of emitters in stored order, a pair list per thread ----
        chunk = spatial_sweep_chunk
        if (dbg_sweep_chunk > 0_int64) chunk = dbg_sweep_chunk
        nchunk = (n + chunk - 1_int64) / chunk
        allocate (coff(nchunk), ccnt(nchunk), cthr(nchunk), heads(nchunk + 1_int64))
        ! One slot per thread, allocated BEFORE the region and indexed by the thread number, each
        ! thread then growing its own slot and no other. The separations travel in the buffers
        ! whenever either is wanted; what the caller did not ask for is dropped after the copy.
        allocate (bufs(0:nt - 1))
        ! The union's emit-once tiebreak, exactly as the general scan switches it on: every walk
        ! but the forced covering ball passes it, and it means something under `PF_LINK_MAX` only.
        want_tie = lrule == PF_LINK_MAX .and. .not. ball_all
        ncyl = 0_int64
        nbal = 0_int64
        ntest = 0_int64
        !$omp parallel num_threads(nt) default(shared) private(tid, c, t, i, u, p, q, r) &
        !$omp     reduction(+:ncyl, nbal, ntest)
        tid = 0
#ifdef _OPENMP
        tid = omp_get_thread_num()
#endif
        call pair_buf_init(bufs(tid), want_sep)
        ! Dynamic over chunks: a survey's emitters are far from uniform in cost, and a chunk is
        ! small enough for the team to balance on. Each chunk records which thread swept it, where
        ! its pairs start in that thread's buffer and how many there are, which is all the
        ! concatenation below needs to put them back in stored order.
        !$omp do schedule(dynamic, 1)
        do c = 1_int64, nchunk
            coff(c) = bufs(tid)%n
            cthr(c) = tid
            do t = (c - 1_int64) * chunk + 1_int64, min(n, c * chunk)
                i = self%idx(t)
                u = t
                if (.not. direct) u = i
                p(1) = xs(u)
                p(2) = ys(u)
                p(3) = zs(u)
                q = p - self%obs
                r = walk(1)
                if (nr > 1) r = walk(i)
                call los_sweep_lean(self, xs, ys, zs, direct, t, i, p, q, r, hcell, keys, lrule, want_tie, ls, &
                    bps, bls, ball_all, qlo, qhi, want_sep, bufs(tid), ncyl, nbal, ntest)
            end do
            ccnt(c) = bufs(tid)%n - coff(c)
        end do
        !$omp end do
        !$omp end parallel
        heads(1) = 1_int64
        do c = 1_int64, nchunk
            heads(c + 1_int64) = heads(c) + ccnt(c)
        end do
        total = heads(nchunk + 1_int64) - 1_int64
        call pairs_alloc(a32, b32, a64, b64, want32, total)
        if (want_sep) allocate (dp(total), dl(total))
        ! The concatenation, chunk by chunk in stored order. Each pair already has its lower row
        ! first (`pair_buf_push`). The kinds are written out for the reason the ball sweep's are;
        ! both branches are loop-invariant.
        !$omp parallel do num_threads(nt) schedule(static) default(shared) private(c, k, s0, e0)
        do c = 1_int64, nchunk
            s0 = heads(c) - 1_int64
            e0 = coff(c)
            if (want32) then
                do k = 1_int64, ccnt(c)
                    a32(s0 + k) = int(bufs(cthr(c))%a(e0 + k), kind=int32)
                    b32(s0 + k) = int(bufs(cthr(c))%b(e0 + k), kind=int32)
                end do
            else
                do k = 1_int64, ccnt(c)
                    a64(s0 + k) = bufs(cthr(c))%a(e0 + k)
                    b64(s0 + k) = bufs(cthr(c))%b(e0 + k)
                end do
            end if
            if (want_sep) then
                do k = 1_int64, ccnt(c)
                    dp(s0 + k) = bufs(cthr(c))%dp(e0 + k)
                    dl(s0 + k) = bufs(cthr(c))%dl(e0 + k)
                end do
            end if
        end do
        !$omp end parallel do
        call pairs_emit(a32, b32, a64, b64, ii32, jj32, ii64, jj64, want32)
        dbg_los_cyl = dbg_los_cyl + ncyl
        dbg_los_balls = dbg_los_balls + nbal
        dbg_los_tested = dbg_los_tested + ntest
        if (present(dperp)) call move_alloc(dp, dperp)
        if (present(dpar)) call move_alloc(dl, dpar)
    end procedure spatial_pairs_los_worker

    !> Gives a thread's buffer its first capacity; called by the owning thread inside the region.
    subroutine pair_buf_init(buf, want_sep)
        type(pair_buf), intent(inout) :: buf !! the thread's buffer.
        logical, intent(in) :: want_sep !! whether the separations travel with the pairs.

        buf%n = 0_int64
        allocate (buf%a(pair_buf_first), buf%b(pair_buf_first))
        if (want_sep) allocate (buf%dp(pair_buf_first), buf%dl(pair_buf_first))
    end subroutine pair_buf_init

    !> Appends one pair, the lower row first, growing the buffer by doubling when it is full.
    subroutine pair_buf_push(buf, i, j, dpv, dlv, want_sep)
        type(pair_buf), intent(inout) :: buf !! the thread's buffer.
        integer(int64), intent(in) :: i !! the emitter's row.
        integer(int64), intent(in) :: j !! the partner's row.
        real(real64), intent(in) :: dpv !! their transverse separation.
        real(real64), intent(in) :: dlv !! their parallel separation.
        logical, intent(in) :: want_sep !! whether the separations are kept.
        integer(int64), allocatable :: ta(:), tb(:)
        real(real64), allocatable :: tp(:), tl(:)
        integer(int64) :: n, cap

        n = buf%n
        cap = size(buf%a, kind=int64)
        if (n == cap) then
            ! Doubling keeps the total copied under twice the final size, and leaves each buffer at
            ! most twice its share -- the bound the sweep's peak-memory statement rests on.
            allocate (ta(2_int64 * cap), tb(2_int64 * cap))
            ta(1:n) = buf%a(1:n)
            tb(1:n) = buf%b(1:n)
            call move_alloc(ta, buf%a)
            call move_alloc(tb, buf%b)
            if (want_sep) then
                allocate (tp(2_int64 * cap), tl(2_int64 * cap))
                tp(1:n) = buf%dp(1:n)
                tl(1:n) = buf%dl(1:n)
                call move_alloc(tp, buf%dp)
                call move_alloc(tl, buf%dl)
            end if
        end if
        n = n + 1_int64
        ! `i` is the endpoint that did the searching, not necessarily the lower row.
        if (j > i) then
            buf%a(n) = i
            buf%b(n) = j
        else
            buf%a(n) = j
            buf%b(n) = i
        end if
        if (want_sep) then
            buf%dp(n) = dpv
            buf%dl(n) = dlv
        end if
        buf%n = n
    end subroutine pair_buf_push

    !> One emitter's line-of-sight sweep, the lean form: the walk decided as `los_walk_shape`
    !> decides it -- the covering ball when the whole sweep is forced onto it (test-only), else the
    !> padded cylinder of its own line of sight, or the ball when that is the cheaper walk -- then
    !> the ball walk or the slab walk written out with the pre-filter inline, the accept through
    !> `los_accept`, and every accepted partner appended to the thread's buffer.
    !>
    !> It is `spatial_scan`'s free-box walk and `spatial_scan_axis`'s slab walk with everything the
    !> sweep never uses removed -- the optional-argument cascade, the buffer caps, the annulus, the
    !> per-pair bound, the sort -- which is where the per-emitter time went
    !> (`feature_spatial_phase0.md`, section 4). The cells are visited in the order those two walks
    !> visit them, and the direct/borrowed fork sits at the run as it does there, so the pairs come
    !> out in the order they would. Every route counts itself here, in its own body, so a test can
    !> tell which one ran.
    subroutine los_sweep_lean(self, xs, ys, zs, direct, t, irow, p, q, rwalk, hcell, keys, lrule, want_tie, ls, &
                              bps, bls, ball_all, qlo, qhi, want_sep, buf, ncyl, nbal, ntest)
        type(pf_spatial_index), intent(in), target :: self !! the index being swept.
        ! `contiguous` is load-bearing here for the reason `scan_healpix` gives: the distance loop
        ! below is the hottest one in the sweep.
        real(real64), intent(in), contiguous :: xs(:) !! stored x, in bucket order or the caller's.
        real(real64), intent(in), contiguous :: ys(:) !! stored y.
        real(real64), intent(in), contiguous :: zs(:) !! stored z.
        logical, intent(in) :: direct !! whether the coordinates are in bucket order.
        integer(int64), intent(in) :: t !! the emitter's stored position.
        integer(int64), intent(in) :: irow !! the emitter's row.
        real(real64), intent(in) :: p(3) !! the emitter's position.
        real(real64), intent(in) :: q(3) !! `p` relative to the observer.
        real(real64), intent(in) :: rwalk !! the emitter's cross-section, or its covering ball under `ball_all`.
        real(real64), intent(in) :: hcell !! the cell side a ball must exceed to be walked as a cylinder; 0 never.
        integer(int64), intent(in) :: keys(:) !! the rank per stored position.
        integer, intent(in) :: lrule !! the rule the accept applies; 0 for the emitter's own cylinder.
        logical, intent(in) :: want_tie !! whether the union's emit-once tiebreak is in force.
        real(real64), intent(in) :: ls(:) !! the parallel coordinate per stored position.
        real(real64), intent(in) :: bps(:) !! transverse length per stored position.
        real(real64), intent(in) :: bls(:) !! parallel length per stored position.
        logical, intent(in) :: ball_all !! .true. walks the covering ball for every emitter (test-only).
        real(real64), intent(in) :: qlo(:) !! the least distance a partner can have, per stored position; empty under `ball_all`.
        real(real64), intent(in) :: qhi(:) !! the greatest such distance; empty under `ball_all`.
        logical, intent(in) :: want_sep !! whether the separations travel with the pairs.
        type(pair_buf), intent(inout) :: buf !! the thread's pair list.
        integer(int64), intent(inout) :: ncyl !! incremented when the cylinder was walked.
        integer(int64), intent(inout) :: nbal !! incremented when a ball was walked.
        integer(int64), intent(inout) :: ntest !! incremented per candidate the accept test saw.
        integer(int64) :: nc(3), a(3), cnt(3), sa(3), sc(3), ilo, ihi, jj, kk, base, s0, e0, v, row, kself
        integer(int64) :: ia, alo, acnt
        real(real64) :: pa(3), pb(3), dv(3), c0(3), c1(3), rw, r, r2, dself, lself, bpself, blself
        real(real64) :: p1, p2, p3, q1, q2, q3, dx, dy, dz, d2, dpv, dlv
        real(real64) :: dd, ddinv, vx, vy, vz, b1, b2, b3, qx, qy, qz, tp, wx, wy, wz, sx, sy, sz, s2
        real(real64) :: w0, w1, ta, tb, tlo, thi, ctr, half
        logical :: use_ball, screen, ok
        integer :: d, da

        nc = self%grid_n
        dself = self%d_s(t)
        lself = ls(t)
        bpself = bps(t)
        blself = bls(t)
        kself = keys(t)
        ! Under the tiebreak the rank does not screen the candidates; it is read inside the accept.
        screen = .not. want_tie
        p1 = p(1)
        p2 = p(2)
        p3 = p(3)
        q1 = q(1)
        q2 = q(2)
        q3 = q(3)
        if (ball_all) then
            use_ball = .true.
            r = rwalk
        else
            call los_walk_shape(self%obs, q, dself, rwalk, hcell, qlo(t), qhi(t), use_ball, r, pa, pb, rw)
        end if

        ! ---- The ball: `spatial_scan`'s free-box walk ----
        if (use_ball) then
            nbal = nbal + 1_int64
            do d = 1, 3
                call axis_span_free(p(d), r, self%lo(d), self%cell_inv(d), nc(d), a(d), cnt(d))
                if (cnt(d) == 0_int64) return
            end do
            r2 = r * r
            ilo = a(1)
            ihi = a(1) + cnt(1) - 1_int64
            do kk = a(3), a(3) + cnt(3) - 1_int64
                do jj = a(2), a(2) + cnt(2) - 1_int64
                    base = 1_int64 + nc(1) * (jj + nc(2) * kk)
                    s0 = self%start(base + ilo)
                    e0 = self%start(base + ihi + 1_int64) - 1_int64
                    if (direct) then
                        do v = s0, e0
                            dx = xs(v) - p1
                            dy = ys(v) - p2
                            dz = zs(v) - p3
                            d2 = dx * dx + dy * dy + dz * dz
                            if (d2 <= r2) then
                                if (screen) then
                                    if (keys(v) <= kself) cycle
                                end if
                                ntest = ntest + 1_int64
                                call los_accept(dx, dy, dz, d2, q1, q2, q3, dself, self%d_s(v), lself, ls(v), lrule, &
                                    want_tie, keys(v), kself, bpself, blself, bps(v), bls(v), ok, dpv, dlv)
                                if (.not. ok) cycle
                                call pair_buf_push(buf, irow, self%idx(v), dpv, dlv, want_sep)
                            end if
                        end do
                    else
                        do v = s0, e0
                            row = self%idx(v)
                            dx = xs(row) - p1
                            dy = ys(row) - p2
                            dz = zs(row) - p3
                            d2 = dx * dx + dy * dy + dz * dz
                            if (d2 <= r2) then
                                if (screen) then
                                    if (keys(v) <= kself) cycle
                                end if
                                ntest = ntest + 1_int64
                                call los_accept(dx, dy, dz, d2, q1, q2, q3, dself, self%d_s(v), lself, ls(v), lrule, &
                                    want_tie, keys(v), kself, bpself, blself, bps(v), bls(v), ok, dpv, dlv)
                                if (.not. ok) cycle
                                call pair_buf_push(buf, irow, row, dpv, dlv, want_sep)
                            end if
                        end do
                    end if
                end do
            end do
            return
        end if

        ! ---- The padded cylinder: `spatial_scan_axis`'s slab walk, flat ends, one radius ----
        ncyl = ncyl + 1_int64
        dv = pb - pa
        dd = dv(1) * dv(1) + dv(2) * dv(2) + dv(3) * dv(3)
        ! Unreachable: `los_walk_shape` pads the axis by a positive slack, so it always has length. ! GCOVR_EXCL_LINE
        if (.not. (dd > 0.0_real64)) error stop & ! GCOVR_EXCL_LINE
            "pf_spatial_index%pairs_within_los: a line-of-sight axis must have positive length" ! GCOVR_EXCL_LINE
        ddinv = 1.0_real64 / dd
        vx = dv(1)
        vy = dv(2)
        vz = dv(3)
        b1 = pa(1)
        b2 = pa(2)
        b3 = pa(3)
        da = 1
        if (abs(dv(2)) > abs(dv(da))) da = 2
        if (abs(dv(3)) > abs(dv(da))) da = 3
        ctr = 0.5_real64 * (pa(da) + pb(da))
        half = 0.5_real64 * abs(dv(da)) + rw
        call axis_span_free(ctr, half, self%lo(da), self%cell_inv(da), nc(da), alo, acnt)
        if (acnt == 0_int64) return
        r2 = rw * rw
        do ia = alo, alo + acnt - 1_int64
            w0 = self%lo(da) + real(ia, kind=real64) * self%cell(da)
            w1 = w0 + self%cell(da)
            ta = (w0 - rw - pa(da)) / dv(da)
            tb = (w1 + rw - pa(da)) / dv(da)
            tlo = min(max(min(ta, tb), 0.0_real64), 1.0_real64)
            thi = min(max(max(ta, tb), 0.0_real64), 1.0_real64)
            c0 = pa + tlo * dv
            c1 = pa + thi * dv
            sa(da) = ia
            sc(da) = 1_int64
            do d = 1, 3
                if (d == da) cycle
                ctr = 0.5_real64 * (c0(d) + c1(d))
                half = 0.5_real64 * abs(c1(d) - c0(d)) + rw
                call axis_span_free(ctr, half, self%lo(d), self%cell_inv(d), nc(d), sa(d), sc(d))
            end do
            if (sc(1) == 0_int64 .or. sc(2) == 0_int64 .or. sc(3) == 0_int64) cycle
            ilo = sa(1)
            ihi = sa(1) + sc(1) - 1_int64
            do kk = sa(3), sa(3) + sc(3) - 1_int64
                do jj = sa(2), sa(2) + sc(2) - 1_int64
                    base = 1_int64 + nc(1) * (jj + nc(2) * kk)
                    s0 = self%start(base + ilo)
                    e0 = self%start(base + ihi + 1_int64) - 1_int64
                    if (direct) then
                        do v = s0, e0
                            qx = xs(v) - b1
                            qy = ys(v) - b2
                            qz = zs(v) - b3
                            tp = (qx * vx + qy * vy + qz * vz) * ddinv
                            if (tp < 0.0_real64 .or. tp > 1.0_real64) cycle
                            wx = qx - tp * vx
                            wy = qy - tp * vy
                            wz = qz - tp * vz
                            d2 = wx * wx + wy * wy + wz * wz
                            if (d2 <= r2) then
                                if (screen) then
                                    if (keys(v) <= kself) cycle
                                end if
                                ntest = ntest + 1_int64
                                ! Separations from the EMITTER, never from the axis's near end.
                                sx = xs(v) - p1
                                sy = ys(v) - p2
                                sz = zs(v) - p3
                                s2 = sx * sx + sy * sy + sz * sz
                                call los_accept(sx, sy, sz, s2, q1, q2, q3, dself, self%d_s(v), lself, ls(v), lrule, &
                                    want_tie, keys(v), kself, bpself, blself, bps(v), bls(v), ok, dpv, dlv)
                                if (.not. ok) cycle
                                call pair_buf_push(buf, irow, self%idx(v), dpv, dlv, want_sep)
                            end if
                        end do
                    else
                        do v = s0, e0
                            row = self%idx(v)
                            qx = xs(row) - b1
                            qy = ys(row) - b2
                            qz = zs(row) - b3
                            tp = (qx * vx + qy * vy + qz * vz) * ddinv
                            if (tp < 0.0_real64 .or. tp > 1.0_real64) cycle
                            wx = qx - tp * vx
                            wy = qy - tp * vy
                            wz = qz - tp * vz
                            d2 = wx * wx + wy * wy + wz * wz
                            if (d2 <= r2) then
                                if (screen) then
                                    if (keys(v) <= kself) cycle
                                end if
                                ntest = ntest + 1_int64
                                sx = xs(row) - p1
                                sy = ys(row) - p2
                                sz = zs(row) - p3
                                s2 = sx * sx + sy * sy + sz * sz
                                call los_accept(sx, sy, sz, s2, q1, q2, q3, dself, self%d_s(v), lself, ls(v), lrule, &
                                    want_tie, keys(v), kself, bpself, blself, bps(v), bls(v), ok, dpv, dlv)
                                if (.not. ok) cycle
                                call pair_buf_push(buf, irow, row, dpv, dlv, want_sep)
                            end if
                        end do
                    end if
                end do
            end do
        end do
    end subroutine los_sweep_lean

    !> The region one emitter walks, from the range of distances `[qlo, qhi]` its partners can
    !> occupy.
    !>
    !> **The padded cylinder contains every accepted partner, by two bounds computed per emitter.**
    !> With `P` the transverse bound, `D` the emitter's distance and `D_j` in `[qlo, qhi]`, an
    !> accepted partner has `sin(dtheta/2) <= P / (D + D_j)`, so its distance from the emitter's
    !> line of sight, `D_j sin dtheta <= 2 D_j sin(dtheta/2)`, is at most `2 P D_j / (D + D_j)`,
    !> which increases with `D_j`: the walked RADIUS is `2 P qhi / (D + qhi)`. Its projection on
    !> that line, `D_j cos dtheta = D_j - 2 D_j sin(dtheta/2)**2 >= D_j - 2 D_j P**2 / (D + D_j)**2`,
    !> and `2x / (D + x)**2` peaks at `x = D` with value `1 / (2D)`: the walked AXIS runs from
    !> `qlo - P**2 / (2D)` to `qhi`, flat ends. Both are widened by `spatial_los_slack` against
    !> rounding at the bound itself. Neither bound needs `D` to be large, so no emitter is excluded
    !> -- but the near-end pad grows as `1/D`, and an emitter close to the observer would walk a
    !> cylinder longer than its covering ball `sqrt(P**2 + dev**2)` is wide. When the axis exceeds
    !> that ball's diameter the ball is walked instead, which is the design's `2D <= Q` rule for
    !> a long cylinder and covers a short one too.
    !>
    !> **The ball is also walked when it is no wider than a cell.** It then touches at most two
    !> cells per axis, and the cylinder, thinner but paying a slab walk's setup per emitter, cannot
    !> visit enough fewer points to earn it back; where the ball spans several cells per axis its
    !> cell count grows with the cube of the ratio and the cylinder's with the first power, and the
    !> cylinder wins. The measurement behind the rule is in `feature_fof_S2.md`, Stage 3. Either
    !> choice is complete, since both regions contain the accepted set; only the cost differs.
    pure subroutine los_walk_shape(obs, q, dself, pw, hcell, qlo, qhi, use_ball, r, pa, pb, rw)
        real(real64), intent(in) :: obs(3) !! the observer.
        real(real64), intent(in) :: q(3) !! the emitter relative to the observer.
        real(real64), intent(in) :: dself !! the emitter's distance from the observer, `|q|` > 0.
        real(real64), intent(in) :: pw !! the transverse bound the emitter walks.
        real(real64), intent(in) :: hcell !! the cell side a covering ball must exceed for the cylinder to be walked; 0 never.
        real(real64), intent(in) :: qlo !! the least distance from the observer a partner can have.
        real(real64), intent(in) :: qhi !! the greatest such distance; `qlo <= qhi`.
        logical, intent(out) :: use_ball !! .true. when the ball is the shorter walk.
        real(real64), intent(out) :: r !! the covering ball's radius.
        real(real64), intent(out) :: pa(3) !! the padded axis's near end.
        real(real64), intent(out) :: pb(3) !! its far end.
        real(real64), intent(out) :: rw !! the padded cylinder's radius.
        real(real64) :: dev, slack, lo, hi

        ! The farthest a partner's distance can sit from the emitter's, whichever side.
        dev = dself - qlo
        if (qhi - dself > dev) dev = qhi - dself
        r = sqrt(pw * pw + dev * dev)
        slack = spatial_los_slack * max(abs(qlo), abs(qhi), dself)
        lo = qlo - 0.5_real64 * pw * pw / dself - slack
        hi = qhi + slack
        rw = 2.0_real64 * pw * qhi / (dself + qhi) + slack
        use_ball = hi - lo > 2.0_real64 * r .or. 2.0_real64 * r <= hcell
        pa = obs + q * (lo / dself)
        pb = obs + q * (hi / dself)
    end subroutine los_walk_shape

    !> The tie groups whose `los` lies within `w` of `lcen`: `a..b`, empty when `a > b`.
    pure subroutine los_window(grp, ng, lcen, w, a, b)
        real(real64), intent(in) :: grp(:) !! the distinct `los` values, ascending.
        integer(int64), intent(in) :: ng !! how many there are.
        real(real64), intent(in) :: lcen !! the window's centre.
        real(real64), intent(in) :: w !! its half-width; >= 0.
        integer(int64), intent(out) :: a !! the first group at or above `lcen - w`.
        integer(int64), intent(out) :: b !! the last group at or below `lcen + w`.
        integer(int64) :: lo, hi, mid
        real(real64) :: lb, ub

        lb = lcen - w
        ub = lcen + w
        lo = 1_int64
        hi = ng + 1_int64
        do while (lo < hi)
            mid = lo + (hi - lo) / 2_int64
            if (grp(mid) < lb) then
                lo = mid + 1_int64
            else
                hi = mid
            end if
        end do
        a = lo
        hi = ng + 1_int64
        do while (lo < hi)
            mid = lo + (hi - lo) / 2_int64
            if (grp(mid) <= ub) then
                lo = mid + 1_int64
            else
                hi = mid
            end if
        end do
        b = lo - 1_int64
    end subroutine los_window

    !> The least and greatest distance over the groups `a..b`, by a plain scan: what one query
    !> needs, where a tree would cost more to build than the window costs to read.
    pure subroutine los_window_range(gmin, gmax, a, b, qlo, qhi)
        real(real64), intent(in) :: gmin(:) !! each group's least distance.
        real(real64), intent(in) :: gmax(:) !! each group's greatest distance.
        integer(int64), intent(in) :: a !! the first group; `a <= b`.
        integer(int64), intent(in) :: b !! the last group.
        real(real64), intent(out) :: qlo !! the least distance over the window.
        real(real64), intent(out) :: qhi !! the greatest distance over the window.
        integer(int64) :: k

        qlo = gmin(a)
        qhi = gmax(a)
        do k = a + 1_int64, b
            if (gmin(k) < qlo) qlo = gmin(k)
            if (gmax(k) > qhi) qhi = gmax(k)
        end do
    end subroutine los_window_range

    !> Two bottom-up segment trees over the groups' least and greatest distances, for the range
    !> minimum and maximum every emitter's window needs, in `O(log n)` each after an `O(n)` build.
    !>
    !> Node `x` of the usual zero-based layout lives at `t(x + 1)`: element `k` at node `ng + k - 1`,
    !> node `x`'s children at `2x` and `2x + 1`, node 0 unused.
    pure subroutine seg_build(gmin, gmax, ng, tmin, tmax)
        real(real64), intent(in) :: gmin(:) !! each group's least distance.
        real(real64), intent(in) :: gmax(:) !! each group's greatest distance.
        integer(int64), intent(in) :: ng !! how many groups.
        real(real64), allocatable, intent(out) :: tmin(:) !! the minimum tree.
        real(real64), allocatable, intent(out) :: tmax(:) !! the maximum tree.
        integer(int64) :: x

        allocate (tmin(2_int64 * ng), tmax(2_int64 * ng))
        tmin(1) = huge(0.0_real64)
        tmax(1) = -huge(0.0_real64)
        do x = 1_int64, ng
            tmin(ng + x) = gmin(x)
            tmax(ng + x) = gmax(x)
        end do
        do x = ng - 1_int64, 1_int64, -1_int64
            tmin(x + 1_int64) = min(tmin(2_int64 * x + 1_int64), tmin(2_int64 * x + 2_int64))
            tmax(x + 1_int64) = max(tmax(2_int64 * x + 1_int64), tmax(2_int64 * x + 2_int64))
        end do
    end subroutine seg_build

    !> The least value over elements `a..b` of a tree `seg_build` made.
    pure function seg_min(tree, ng, a, b) result(v)
        real(real64), intent(in) :: tree(:) !! the minimum tree.
        integer(int64), intent(in) :: ng !! how many elements it holds.
        integer(int64), intent(in) :: a !! the first element; `a <= b`.
        integer(int64), intent(in) :: b !! the last element.
        real(real64) :: v !! the minimum.
        integer(int64) :: l, r

        v = huge(0.0_real64)
        l = a - 1_int64 + ng
        r = b + ng
        do while (l < r)
            if (iand(l, 1_int64) == 1_int64) then
                if (tree(l + 1_int64) < v) v = tree(l + 1_int64)
                l = l + 1_int64
            end if
            if (iand(r, 1_int64) == 1_int64) then
                r = r - 1_int64
                if (tree(r + 1_int64) < v) v = tree(r + 1_int64)
            end if
            l = l / 2_int64
            r = r / 2_int64
        end do
    end function seg_min

    !> The greatest value over elements `a..b` of a tree `seg_build` made.
    pure function seg_max(tree, ng, a, b) result(v)
        real(real64), intent(in) :: tree(:) !! the maximum tree.
        integer(int64), intent(in) :: ng !! how many elements it holds.
        integer(int64), intent(in) :: a !! the first element; `a <= b`.
        integer(int64), intent(in) :: b !! the last element.
        real(real64) :: v !! the maximum.
        integer(int64) :: l, r

        v = -huge(0.0_real64)
        l = a - 1_int64 + ng
        r = b + ng
        do while (l < r)
            if (iand(l, 1_int64) == 1_int64) then
                if (tree(l + 1_int64) > v) v = tree(l + 1_int64)
                l = l + 1_int64
            end if
            if (iand(r, 1_int64) == 1_int64) then
                r = r - 1_int64
                if (tree(r + 1_int64) > v) v = tree(r + 1_int64)
            end if
            l = l / 2_int64
            r = r / 2_int64
        end do
    end function seg_max

    !> The points inside the cylinder about `p` along `p`'s own line of sight: the query's own
    !> lengths and no combine rule, walked as the padded cylinder `los_walk_shape` gives -- the
    !> ball when that is the shorter walk, or under the test-only forced walk -- with its range of
    !> distances read off the tie groups within `b_par` of `los_p`. That window can be EMPTY for
    !> a query point, which is then an empty cylinder and nothing is walked.
    module procedure spatial_within_los_worker
        real(real64), pointer, contiguous :: ls(:)
        real(real64) :: pp(3), q(3), pa(3), pb(3), dpq, lp, qq, r, rw, qlo, qhi, hcell
        integer(int64) :: a, b, ng, ntest
        logical :: use_ball, local

        m = 0_int64
        call spatial_los_prepare(self, what)
        if (size(p) /= 3) error stop "pf_spatial_index%" // what // &
            ": the query point must have three coordinates"
        call spatial_check_finite(p, "pf_spatial_index%" // what, "query point coordinate")
        if (present(los_p) .neqv. self%has_los) then
            if (self%has_los) error stop "pf_spatial_index%" // what // ": this index carries a parallel " // &
                "coordinate (los= at %build), so los_p= is required: the query point's own value in the same units"
            error stop "pf_spatial_index%" // what // ": this index was built without los=, so los_p= has no " // &
                "meaning here; the parallel separation is the difference in distance from the observer"
        end if
        ! Strictly positive, unlike the pair sweep's lengths: `dist=` divides by both.
        if (.not. (b_perp > 0.0_real64 .and. b_par > 0.0_real64)) error stop "pf_spatial_index%" // what // &
            ": b_perp and b_par must both be > 0 and not NaN; the normalised distance divides by them"
        ! A named local, for the reason `query_point`'s callers give: the scan takes `p(3)`.
        pp = p
        q = pp - self%obs
        dpq = sqrt(q(1) * q(1) + q(2) * q(2) + q(3) * q(3))
        if (dpq == 0.0_real64) error stop "pf_spatial_index%" // what // &
            ": the query point coincides with the observer, so it has no line of sight"
        lp = dpq
        if (present(los_p)) then
            if (.not. (abs(los_p) <= huge(0.0_real64))) error stop "pf_spatial_index%" // what // &
                ": los_p must be a finite number (no NaN, no infinity)"
            lp = los_p
        end if
        if (self%has_los) then
            ls => self%los_s
        else
            ls => self%d_s
        end if
        ntest = 0_int64
        if (dbg_los_walk == 1) then
            qq = self%lip * b_par
            if (self%tie_spread > qq) qq = self%tie_spread
            r = sqrt(b_perp * b_perp + qq * qq)
            call spatial_scan(self, pp, r, m, out32=out32, out64=out64, dist=dist, sorted=sorted, los_rule=0, &
                los_q=q, los_d=dpq, los_l=lp, los_bp=b_perp, los_bl=b_par, los_ls=ls, dperp=dperp, dpar=dpar, &
                ntested=ntest)
            !$omp atomic
            dbg_los_balls = dbg_los_balls + 1_int64
        else
            local = self%has_los .and. .not. dbg_los_global
            if (local) local = allocated(self%los_grp)
            ! The same cell rule, and the same shortcut, as the sweep's (`spatial_pairs_los_worker`):
            ! the catalogue-wide bound first, and the query's own window only when the covering ball
            ! under that bound does not fit a cell.
            hcell = min(self%cell(1), self%cell(2), self%cell(3))
            if (dbg_los_walk == 2) hcell = 0.0_real64
            qq = self%lip * b_par
            if (self%tie_spread > qq) qq = self%tie_spread
            qlo = dpq - qq
            qhi = dpq + qq
            if (local) local = 2.0_real64 * sqrt(b_perp * b_perp + qq * qq) > hcell
            if (local) then
                ng = size(self%los_grp, kind=int64)
                call los_window(self%los_grp, ng, lp, b_par, a, b)
                if (a > b) then
                    ! No stored point is within `b_par` of `los_p`: the cylinder is empty.
                    !$omp atomic
                    dbg_los_cyl = dbg_los_cyl + 1_int64
                    return
                end if
                call los_window_range(self%los_gmin, self%los_gmax, a, b, qlo, qhi)
            end if
            call los_walk_shape(self%obs, q, dpq, b_perp, hcell, qlo, qhi, use_ball, r, pa, pb, rw)
            if (use_ball) then
                call spatial_scan(self, pp, r, m, out32=out32, out64=out64, dist=dist, sorted=sorted, los_rule=0, &
                    los_q=q, los_d=dpq, los_l=lp, los_bp=b_perp, los_bl=b_par, los_ls=ls, dperp=dperp, dpar=dpar, &
                    ntested=ntest)
                !$omp atomic
                dbg_los_balls = dbg_los_balls + 1_int64
            else
                call spatial_scan_axis(self, pa, pb, rw, rw, .false., what, m, out32=out32, out64=out64, dist=dist, &
                    sorted=sorted, los_rule=0, los_q=q, los_p=pp, los_d=dpq, los_l=lp, los_bp=b_perp, los_bl=b_par, &
                    los_ls=ls, dperp=dperp, dpar=dpar, ntested=ntest)
                !$omp atomic
                dbg_los_cyl = dbg_los_cyl + 1_int64
            end if
        end if
        !$omp atomic
        dbg_los_tested = dbg_los_tested + ntest
    end procedure spatial_within_los_worker

    !> The distance from every point to its `k`-th nearest OTHER point.
    module procedure spatial_kth_worker
        real(real64), pointer, contiguous :: xs(:), ys(:), zs(:)
        integer(int64), allocatable :: rows(:)
        real(real64), allocatable :: ds(:)
        integer(int64) :: n, t, i, u, q, pos, kk
        real(real64) :: p(3), rseed
        integer :: nt
        logical :: direct

        if (.not. self%built_ok) error stop &
            "pf_spatial_index%kth_distance: this index has not been built; call %build first"
        if (self%metric_id /= expect_metric) then
            if (expect_metric == PF_METRIC_SKY) error stop &
                "pf_spatial_index%kth_distance_sky: this is a Euclidean index; use %kth_distance"
            error stop "pf_spatial_index%kth_distance: this index was built with %build_sky; " // &
                "use %kth_distance_sky, which answers in degrees"
        end if
        n = self%npts
        if (k < 1_int64) error stop "pf_spatial_index%kth_distance: k must be >= 1"
        ! Checked up front rather than reported per point: `k` is a scalar and `n` is known, so a
        ! request that cannot be answered is a mistake in the call and not a property of one row.
        if (k > n - 1_int64) error stop &
            "pf_spatial_index%kth_distance: k must be at most %size()-1, since a point is not its own neighbour"
        nt = spatial_threads(threads)
        dbg_threads_used = nt
        allocate (dist(n))
        call spatial_storage(self, xs, ys, zs)
        direct = self%owns
        kk = k + 1_int64
        !$omp parallel num_threads(nt) default(shared) &
        !$omp     private(t, i, u, q, pos, p, rseed, rows, ds)
        ! Each thread carries its own converged radius from one query to the next. The sweep runs
        ! in STORED order, so consecutive points are spatially adjacent and the previous radius is
        ! a good guess -- which changes only how many rounds the expansion takes, never an answer.
        rseed = -1.0_real64
        !$omp do schedule(static)
        do t = 1_int64, n
            i = self%idx(t)
            u = t
            if (.not. direct) u = i
            p(1) = xs(u)
            p(2) = ys(u)
            p(3) = zs(u)
            call spatial_shell_search(self, p, kk, rseed, rows, ds, "kth_distance")
            ! Self is excluded by IDENTITY, not by distance: two coincident points are both at
            ! distance zero and only one of them is this row. Removing entry `pos` from a sorted
            ! list of `k+1` leaves the k-th other point at `k+1` when `pos` was inside the first
            ! `k`, and at `k` otherwise -- which also covers self not being in the list at all,
            ! possible only when more than `k+1` points share these coordinates and every
            ! candidate distance is therefore zero.
            pos = kk + 1_int64
            do q = 1_int64, kk
                if (rows(q) == i) then
                    pos = q
                    exit
                end if
            end do
            if (pos <= k) then
                dist(i) = ds(kk)
            else
                dist(i) = ds(k)
            end if
        end do
        !$omp end do
        !$omp end parallel
    end procedure spatial_kth_worker

    !> Labels the connected components of an undirected graph given as an edge list.
    !>
    !> **Serial, deliberately.** The edge pass is memory-bound and every concurrent union-find
    !> worth having is substantially subtler than this problem justifies; `%pairs_within`, which is
    !> what usually produces the edge list, is already threaded and dominates the runtime.
    module procedure spatial_components_worker
        integer(int64), allocatable :: parent(:), csize(:), lab(:)
        integer(int64) :: e, v, root, nc, nedge
        integer(int64) :: ms
        logical :: want32

        want32 = present(i32)
        ! The default is 1, the strict graph-theoretic reading in which every vertex belongs to
        ! some component. A group finder that wants singletons dropped says `min_size = 2` at the
        ! call, where the choice is visible; making that the default would have this routine answer
        ! a domain question a general graph utility has no business deciding.
        ms = 1_int64
        if (present(min_size)) ms = int(min_size, kind=int64)
        if (ms < 1_int64) error stop "pf_connected_components: min_size must be >= 1"
        if (want32) then
            nedge = size(i32, kind=int64)
            if (size(j32, kind=int64) /= nedge) error stop &
                "pf_connected_components: the two endpoint arrays must be the same length"
        else
            nedge = size(i64, kind=int64)
            if (size(j64, kind=int64) /= nedge) error stop &
                "pf_connected_components: the two endpoint arrays must be the same length"
        end if
        if (nvert < 0_int64) error stop "pf_connected_components: nvert must be >= 0"
        ! Every label, every component size and every edge endpoint is bounded by `nvert`, so one
        ! comparison before anything is allocated decides whether the int32 answer can carry them.
        ! Nested rather than `.and.`-ed: the ceiling is a function call, not a comparison.
        if (want32) then
            if (nvert > spatial_int32_ceiling()) error stop "pf_connected_components: " // &
                "nvert is larger than an int32 answer can name; take the labels as int64"
        end if
        if (nvert == 0_int64) then
            call comp_emit_empty(labels32, ncomp32, sizes32, labels64, ncomp64, sizes64, want32)
            return
        end if
        allocate (parent(nvert), csize(nvert), lab(nvert))
        do v = 1_int64, nvert
            parent(v) = v
            csize(v) = 1_int64
            lab(v) = 0_int64
        end do
        ! The union-find itself runs in int64 whatever kind the caller handed in: a vertex index is
        ! bounded by `nvert`, so nothing is lost, and the algorithm exists in ONE copy. Only the
        ! edge read differs, and the two loops are written out so that a slice of an ABSENT
        ! optional is never formed.
        if (want32) then
            do e = 1_int64, nedge
                call comp_link(parent, csize, int(i32(e), kind=int64), int(j32(e), kind=int64), nvert)
            end do
        else
            do e = 1_int64, nedge
                call comp_link(parent, csize, i64(e), j64(e), nvert)
            end do
        end if
        ! **Numbering by ascending vertex of first appearance, and that is a contract.** Left to
        ! the union-find's own roots the numbering would depend on union-by-size tie-breaking, so
        ! the same catalogue could come back with differently numbered groups on another compiler.
        nc = 0_int64
        do v = 1_int64, nvert
            root = uf_find(parent, v)
            if (csize(root) < ms) cycle
            if (lab(root) == 0_int64) then
                nc = nc + 1_int64
                lab(root) = nc
            end if
        end do
        ! Written straight into the kind asked for rather than narrowed afterwards: `labels` is
        ! length nvert and is the second-largest array a group finder holds.
        if (want32) then
            allocate (labels32(nvert))
            do v = 1_int64, nvert
                labels32(v) = int(lab(uf_find(parent, v)), kind=int32)
            end do
            if (present(ncomp32)) ncomp32 = int(nc, kind=int32)
            if (present(sizes32)) then
                allocate (sizes32(nc))
                do v = 1_int64, nvert
                    root = uf_find(parent, v)
                    if (lab(root) == 0_int64) cycle
                    sizes32(lab(root)) = int(csize(root), kind=int32)
                end do
            end if
        else
            allocate (labels64(nvert))
            do v = 1_int64, nvert
                labels64(v) = lab(uf_find(parent, v))
            end do
            if (present(ncomp64)) ncomp64 = nc
            if (present(sizes64)) then
                allocate (sizes64(nc))
                do v = 1_int64, nvert
                    root = uf_find(parent, v)
                    if (lab(root) == 0_int64) cycle
                    sizes64(lab(root)) = csize(root)
                end do
            end if
        end if
    end procedure spatial_components_worker

    !> One edge of `pf_connected_components`: range-check both endpoints and union their trees.
    !!
    !! **The one copy of the union step.** Both edge loops call it, so the two kinds cannot drift
    !! apart in what they accept or in how the forest is joined.
    subroutine comp_link(parent, csize, a, b, nvert)
        integer(int64), intent(inout) :: parent(:) !! the union-find forest.
        integer(int64), intent(inout) :: csize(:) !! the size of each root's tree.
        integer(int64), intent(in) :: a !! one endpoint, in 1..nvert.
        integer(int64), intent(in) :: b !! the other endpoint, in 1..nvert.
        integer(int64), intent(in) :: nvert !! how many vertices the graph has.
        integer(int64) :: ra, rb

        if (a < 1_int64 .or. a > nvert .or. b < 1_int64 .or. b > nvert) error stop &
            "pf_connected_components: every edge endpoint must be a vertex in 1..nvert"
        ra = uf_find(parent, a)
        rb = uf_find(parent, b)
        if (ra == rb) return
        ! Union by size, which is what keeps the trees shallow. Which root wins is an internal
        ! choice and deliberately does NOT decide the labels -- see the numbering pass above.
        if (csize(ra) < csize(rb)) then
            parent(ra) = rb
            csize(rb) = csize(rb) + csize(ra)
        else
            parent(rb) = ra
            csize(ra) = csize(ra) + csize(rb)
        end if
    end subroutine comp_link

    !> The answer for a graph with no vertices, in whichever kind was asked for.
    subroutine comp_emit_empty(labels32, ncomp32, sizes32, labels64, ncomp64, sizes64, want32)
        integer(int32), allocatable, intent(out), optional :: labels32(:) !! length 0, int32 arm.
        integer(int32), intent(out), optional :: ncomp32 !! 0, int32 arm.
        integer(int32), allocatable, intent(out), optional :: sizes32(:) !! length 0, int32 arm.
        integer(int64), allocatable, intent(out), optional :: labels64(:) !! length 0, int64 arm.
        integer(int64), intent(out), optional :: ncomp64 !! 0, int64 arm.
        integer(int64), allocatable, intent(out), optional :: sizes64(:) !! length 0, int64 arm.
        logical, intent(in) :: want32 !! .true. when the caller asked for the int32 arm.

        if (want32) then
            allocate (labels32(0))
            if (present(ncomp32)) ncomp32 = 0_int32
            if (present(sizes32)) allocate (sizes32(0))
        else
            allocate (labels64(0))
            if (present(ncomp64)) ncomp64 = 0_int64
            if (present(sizes64)) allocate (sizes64(0))
        end if
    end subroutine comp_emit_empty

    !> The union-find root of `v`, with full path compression.
    integer(int64) function uf_find(parent, v) result(r)
        integer(int64), intent(inout) :: parent(:) !! the forest; compressed in place.
        integer(int64), intent(in) :: v !! the vertex to look up.
        integer(int64) :: w, nxt

        r = v
        do while (parent(r) /= r)
            r = parent(r)
        end do
        w = v
        do while (parent(w) /= r)
            nxt = parent(w)
            parent(w) = r
            w = nxt
        end do
    end function uf_find

    !> A short decimal rendering of a real, for a message.
    function num_text(v) result(text)
        real(real64), intent(in) :: v !! the value to render.
        character(len=:), allocatable :: text !! the rendered value, trimmed.
        character(len=32) :: buf

        write (buf, '(g0.6)') v
        text = trim(adjustl(buf))
    end function num_text

end submodule parquet_spatial_bulk ! GCOVR_EXCL_LINE
