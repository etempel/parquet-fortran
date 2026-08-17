!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Sequential weighted draw WITHOUT replacement: which construction should back it?
!!
!! The user case this exists for: `M` items, each with a weight. Draw one with probability
!! proportional to weight; the caller may reject it; draw the next from what remains, again
!! proportional to weight among survivors. **How many draws will be taken is not known in advance**,
!! and drawing all `M` is the weighted shuffle. That process is exactly *successive sampling*
!! (Plackett-Luce), and it is NOT the same as inclusion-probability-proportional-to-size -- a
!! distinction measured at 18% for mild weights, so the two must not be confused.
!!
!! Three constructions are compared:
!!
!!   * **class-bucket** -- group items by DISTINCT weight value, pick a class with probability
!!     proportional to `n_c * v_c`, then pick uniformly within it and swap-remove. `O(M)` setup,
!!     `O(C)` per draw where `C` is the number of distinct weights, `O(1)` removal. Exact.
!!   * **segment tree** -- a complete binary tree of partial sums; descend by cumulative weight,
!!     zero the leaf, update ancestors. `O(M)` build, `O(log M)` per draw. Exact, and the general
!!     answer when the weights are continuous.
!!   * **exponential race** -- `key_i = -log(U_i) / w_i`, take the `k` smallest
!!     (Efraimidis-Spirakis). `O(M)` keys plus a selection, whatever `k` is. Parallel and
!!     coordinate-addressed, which the other two are not.
!!
!! **The race is deliberately FLATTERED here**: it is given `k` up front, which the real use case
!! cannot supply. If it loses even so, the conclusion is robust.
!!
!! `--mode=correct` is the gate and runs first: every arm is checked against exact enumeration of
!! the successive-sampling distribution before any timing is reported, because a fast sampler with
!! the wrong distribution is worth nothing.
!!
!! Not run by `fpm test`, by design (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark
!! tools"). Build and run with `--profile release`.
module pf_probe_weighted

    use iso_fortran_env, only: int32, int64, real64
    use parquet, only: pf_random_at, pf_random_int_at, pf_random_fill_streams, &
                       pf_partial_argsort, pf_argsort

    implicit none

    !> Largest distinct-weight count the class-bucket arm will accept before giving up.
    !! A real implementation falls back to the segment tree here rather than aborting.
    integer, parameter :: MAX_CLASS = 4096

contains

    ! ================================================================================
    ! Weight fixtures
    ! ================================================================================

    !> `m` weights drawn from `nclass` distinct values, assigned round-robin.
    !!
    !! Round-robin rather than in blocks so the bucketing pass sees an interleaved array, which is
    !! the cache-unfriendly case and therefore the honest one. Class 1 has weight 1, the last has
    !! weight 2.5 -- the ratio the user described.
    subroutine make_weights(m, nclass, w)
        integer(int64), intent(in) :: m             !! how many items
        integer, intent(in) :: nclass               !! how many distinct weights
        real(real64), intent(out) :: w(:)           !! the weights, length `m`
        integer(int64) :: i
        integer :: c
        do i = 1_int64, m
            c = int(mod(i - 1_int64, int(nclass, int64)), int32) + 1
            w(i) = 1.0_real64 + 1.5_real64 * real(c - 1, real64) / real(max(1, nclass - 1), real64)
        end do
    end subroutine make_weights

    ! ================================================================================
    ! Arm 1 -- class-bucket
    ! ================================================================================

    !> Discovers the distinct weights and buckets the item indices by class.
    !!
    !! Two passes: a linear scan against the classes found so far (cheap while `C` is small, which
    !! is the regime this arm is for), then a counting sort into one flat index array. The live
    !! items of class `c` are `idx(start(c) : start(c) + n(c) - 1)`.
    subroutine cb_build(w, cls_v, cls_start, cls_n, idx, nclass)
        real(real64), intent(in) :: w(:)            !! the weights
        real(real64), intent(out) :: cls_v(:)       !! distinct weight values, filled 1..nclass
        integer(int64), intent(out) :: cls_start(:) !! first slot of each class in `idx`
        integer(int64), intent(out) :: cls_n(:)     !! live count per class
        integer(int64), intent(out) :: idx(:)       !! item indices grouped by class
        integer, intent(out) :: nclass              !! how many distinct weights were found
        integer(int64) :: i, m, fill(MAX_CLASS)
        integer :: c, cof
        m = size(w, kind=int64)
        nclass = 0
        do i = 1_int64, m
            cof = 0
            do c = 1, nclass
                if (cls_v(c) == w(i)) then
                    cof = c
                    exit
                end if
            end do
            if (cof == 0) then
                nclass = nclass + 1
                if (nclass > MAX_CLASS) error stop &
                    "cb_build: more distinct weights than MAX_CLASS; use the segment tree instead"
                cls_v(nclass) = w(i)
                cls_n(nclass) = 0_int64
                cof = nclass
            end if
            cls_n(cof) = cls_n(cof) + 1_int64
        end do
        cls_start(1) = 1_int64
        do c = 2, nclass
            cls_start(c) = cls_start(c - 1) + cls_n(c - 1)
        end do
        fill(1:nclass) = cls_start(1:nclass)
        do i = 1_int64, m
            do c = 1, nclass
                if (cls_v(c) == w(i)) exit
            end do
            idx(fill(c)) = i
            fill(c) = fill(c) + 1_int64
        end do
    end subroutine cb_build

    !> One draw. `O(C)` for the class pick, `O(1)` for the removal.
    !!
    !! The class total is recomputed as `sum(n_c * v_c)` from the live counts every time rather
    !! than maintained by subtraction, so no floating-point error can accumulate across draws --
    !! which is the segment tree's one weakness (see `st_next`).
    !!
    !! **The two draws come from two different STREAMS, never from two draw indices of one
    !! stream.** Streams `2*stream-1` and `2*stream` cannot collide with each other or with a
    !! neighbouring caller's pair, whatever the library's draw grid does.
    !!
    !! **This probe is where `pf_random_algorithm` `/v1`'s stride defect was found**, and the story
    !! is worth keeping because the failure mode is the interesting part. Under `/v1`,
    !! `pf_random_at` and `pf_random_int_at` mapped `draw` onto Philox blocks differently -- the
    !! first read block `(draw-1)/2` (words `w0,w1` on an odd draw), the second block `draw-1`
    !! (always `w0,w1`). Interleaving them on one stream as draws `2k-1` and `2k` therefore made the
    !! class pick at `k=2` read exactly the block and words the index pick at `k=1` had already
    !! consumed, so the two were identical instead of independent. That version passed every
    !! distinctness check and failed the distribution gate at 0.029 against a 0.0008 standard error
    !! -- only a Monte Carlo comparison could see it.
    !!
    !! `/v2` gave `pf_random_int_at` stride 2, so the interleaved-draws version would now be correct.
    !! **The two-stream shape is kept anyway**, and deliberately: it is immune to the draw grid
    !! entirely, so it cannot be re-broken by a future contract change, and it stays correct if a
    !! `pf_random32_at` draw is ever mixed in -- that generic still walks its own finer grid. See
    !! `feature_risks.md` Risk-113.
    subroutine cb_next(seed, stream, kdraw, cls_v, cls_start, cls_n, idx, nclass, item, lg_c, lg_j)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: kdraw         !! 1-based draw number
        real(real64), intent(in) :: cls_v(:)        !! class weights
        integer(int64), intent(in) :: cls_start(:)  !! class offsets in `idx`
        integer(int64), intent(inout) :: cls_n(:)   !! live counts, decremented here
        integer(int64), intent(inout) :: idx(:)     !! index store, swap-removed here
        integer, intent(in) :: nclass               !! how many classes
        integer(int64), intent(out) :: item         !! the drawn item, or 0 when exhausted
        integer, intent(out) :: lg_c                !! undo log: which class
        integer(int64), intent(out) :: lg_j         !! undo log: which slot within the class
        real(real64) :: tot, acc, u
        integer(int64) :: j, pos, last
        integer :: c, pick
        tot = 0.0_real64
        do c = 1, nclass
            tot = tot + real(cls_n(c), real64) * cls_v(c)
        end do
        if (tot <= 0.0_real64) then
            item = 0_int64
            lg_c = 0
            lg_j = 0_int64
            return
        end if
        u = pf_random_at(seed, 2_int64 * stream - 1_int64, kdraw) * tot
        acc = 0.0_real64
        pick = nclass
        do c = 1, nclass
            acc = acc + real(cls_n(c), real64) * cls_v(c)
            if (u < acc) then
                pick = c
                exit
            end if
        end do
        ! A zero-weight or empty class can be landed on only through rounding; step off it.
        do while (cls_n(pick) == 0_int64 .or. cls_v(pick) <= 0.0_real64)
            pick = pick - 1
            if (pick < 1) then
                item = 0_int64
                lg_c = 0
                lg_j = 0_int64
                return
            end if
        end do
        j = pf_random_int_at(seed, 2_int64 * stream, 1_int64, cls_n(pick), kdraw)
        pos = cls_start(pick) + j - 1_int64
        last = cls_start(pick) + cls_n(pick) - 1_int64
        item = idx(pos)
        idx(pos) = idx(last)
        idx(last) = item                            ! parked at `last` so the undo is a plain swap
        cls_n(pick) = cls_n(pick) - 1_int64
        lg_c = pick
        lg_j = j
    end subroutine cb_next

    !> Undoes `ndrawn` draws in reverse, restoring the sampler to its just-built state. `O(k)`.
    subroutine cb_restore(ndrawn, lg_c, lg_j, cls_start, cls_n, idx)
        integer(int64), intent(in) :: ndrawn        !! how many draws to undo
        integer, intent(in) :: lg_c(:)              !! undo log: classes
        integer(int64), intent(in) :: lg_j(:)       !! undo log: slots
        integer(int64), intent(in) :: cls_start(:)  !! class offsets
        integer(int64), intent(inout) :: cls_n(:)   !! live counts, restored here
        integer(int64), intent(inout) :: idx(:)     !! index store, restored here
        integer(int64) :: t, p1, p2, tmp
        integer :: c
        do t = ndrawn, 1_int64, -1_int64
            c = lg_c(t)
            if (c == 0) cycle
            cls_n(c) = cls_n(c) + 1_int64
            p1 = cls_start(c) + lg_j(t) - 1_int64
            p2 = cls_start(c) + cls_n(c) - 1_int64
            tmp = idx(p1)
            idx(p1) = idx(p2)
            idx(p2) = tmp
        end do
    end subroutine cb_restore

    ! ================================================================================
    ! Arm 2 -- segment tree of partial sums
    ! ================================================================================

    !> Smallest power of two at or above `m`.
    pure function pow2_ceil(m) result(s)
        integer(int64), intent(in) :: m             !! how many leaves are needed
        integer(int64) :: s                         !! the padded leaf count
        s = 1_int64
        do while (s < m)
            s = s * 2_int64
        end do
    end function pow2_ceil

    !> Builds the tree: leaves hold the weights, every internal node its subtree's sum.
    subroutine st_build(w, st, sz)
        real(real64), intent(in) :: w(:)            !! the weights
        real(real64), intent(out) :: st(:)          !! the tree, length `2*sz`
        integer(int64), intent(in) :: sz            !! padded leaf count
        integer(int64) :: i, v, m
        m = size(w, kind=int64)
        st = 0.0_real64
        do i = 1_int64, m
            st(sz + i - 1_int64) = w(i)
        end do
        do v = sz - 1_int64, 1_int64, -1_int64
            st(v) = st(2_int64 * v) + st(2_int64 * v + 1_int64)
        end do
    end subroutine st_build

    !> One draw: descend by cumulative weight, zero the leaf, subtract from every ancestor.
    !!
    !! The ancestor update is a SUBTRACTION, so error accumulates across draws in a way the
    !! class-bucket arm's recompute-from-counts cannot. `--mode=correct` reports the residual at
    !! the root after every item has been drawn, which is that error made visible.
    subroutine st_next(seed, stream, kdraw, st, sz, item, lg_w)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: kdraw         !! 1-based draw number
        real(real64), intent(inout) :: st(:)        !! the tree, mutated here
        integer(int64), intent(in) :: sz            !! padded leaf count
        integer(int64), intent(out) :: item         !! the drawn item, or 0 when exhausted
        real(real64), intent(out) :: lg_w           !! undo log: the weight removed
        real(real64) :: u, wv
        integer(int64) :: v, p
        if (st(1) <= 0.0_real64) then
            item = 0_int64
            lg_w = 0.0_real64
            return
        end if
        u = pf_random_at(seed, stream, kdraw) * st(1)
        v = 1_int64
        do while (v < sz)
            v = 2_int64 * v
            if (u >= st(v)) then
                u = u - st(v)
                v = v + 1_int64
            end if
        end do
        item = v - sz + 1_int64
        wv = st(v)
        lg_w = wv
        st(v) = 0.0_real64
        p = v
        do while (p > 1_int64)
            p = p / 2_int64
            st(p) = st(p) - wv
        end do
    end subroutine st_next

    !> Undoes `ndrawn` draws in reverse: put each weight back on its leaf and its ancestors.
    !!
    !! `O(k log M)`, the segment tree's counterpart to `cb_restore`. Note the restore is itself a
    !! run of ADDITIONS on sums that were maintained by subtraction, so it does not recover the
    !! exact original tree -- see `st_next` on the residual this leaves.
    subroutine st_restore(ndrawn, lg_item, lg_w, st, sz)
        integer(int64), intent(in) :: ndrawn        !! how many draws to undo
        integer(int64), intent(in) :: lg_item(:)    !! undo log: which item
        real(real64), intent(in) :: lg_w(:)         !! undo log: its weight
        real(real64), intent(inout) :: st(:)        !! the tree, restored here
        integer(int64), intent(in) :: sz            !! padded leaf count
        integer(int64) :: t, v, p
        do t = ndrawn, 1_int64, -1_int64
            if (lg_item(t) == 0_int64) cycle
            v = sz + lg_item(t) - 1_int64
            st(v) = lg_w(t)
            p = v
            do while (p > 1_int64)
                p = p / 2_int64
                st(p) = st(p) + lg_w(t)
            end do
        end do
    end subroutine st_restore

    ! ================================================================================
    ! Arm 3 -- exponential race (Efraimidis-Spirakis)
    ! ================================================================================

    !> `key_i = -log(U_i) / w_i`, then the `k` smallest. Flattered: `k` is given up front.
    !!
    !! Two guards that are easy to omit and fail silently. `pf_random_at` returns `[0, 1)`, so
    !! `1 - u` lands in `(0, 1]` and `E = 0` exactly when `u = 0` -- an `E` of zero is the SMALLEST
    !! possible key, i.e. that item is always selected, with probability `2**-53` per draw. And a
    !! zero weight must give `+huge`, never a division trap.
    subroutine race_select(seed, stream, w, kdraw, out)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: stream        !! stream index
        real(real64), intent(in) :: w(:)            !! the weights
        integer(int64), intent(in) :: kdraw         !! how many to select
        integer(int64), intent(out) :: out(:)       !! the selected items, first `kdraw` used
        real(real64), allocatable :: keys(:)
        integer(int64), allocatable :: perm(:)
        real(real64) :: u, uu, e
        integer(int64) :: i, m
        m = size(w, kind=int64)
        allocate(keys(m))
        call pf_random_fill_streams(seed, stream, keys)
        do i = 1_int64, m
            u = keys(i)
            uu = 1.0_real64 - u                     ! (0, 1]
            if (uu >= 1.0_real64) uu = 1.0_real64 - epsilon(1.0_real64)
            e = -log(uu)
            if (w(i) > 0.0_real64) then
                keys(i) = e / w(i)
            else
                keys(i) = huge(1.0_real64)
            end if
        end do
        ! Same fairness switch `probe_random_subset` uses: a partial argsort degrades past a full
        ! sort as `k` approaches `m`, and a real implementation would switch, so this one does.
        if (4_int64 * kdraw >= m) then
            call pf_argsort(keys, perm)
        else
            call pf_partial_argsort(keys, perm, int(kdraw, int32))
        end if
        out(1:kdraw) = perm(1:kdraw)
        deallocate(keys, perm)
    end subroutine race_select

end module pf_probe_weighted


!> Drives the correctness gate and the three cost measurements.
program probe_random_weighted

    use iso_fortran_env, only: int32, int64, real64, output_unit
    use pf_probe_weighted
    use parquet, only: parquet_set_sort_threads
#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime
#endif

    implicit none

    integer(int64), parameter :: SEED = 20260817_int64
    character(len=32) :: mode
    integer(int64) :: g_m
    integer :: g_c, g_rounds

    call get_mode(mode)
    g_m = read_arg_int('--m=', 1000000_int64)
    g_c = int(read_arg_int('--c=', 2_int64), int32)
    g_rounds = int(read_arg_int('--rounds=', 5_int64), int32)

    call parquet_set_sort_threads(1)                ! single core throughout, as the case requires

    write(output_unit, '(a)') 'probe_random_weighted: sequential weighted draw without replacement'
    write(output_unit, '(a,i0,a,i0,a,i0)') 'defaults: m = ', g_m, ', classes = ', g_c, &
        ', best of ', g_rounds
    write(output_unit, '(a)') 'sorting forced to 1 thread'
    write(output_unit, '(a)') ''
    flush(output_unit)

    if (mode == 'correct' .or. mode == 'all') call run_correct()
    if (mode == 'setup' .or. mode == 'all') call run_setup()
    if (mode == 'cost' .or. mode == 'all') call run_cost()
    if (mode == 'repeat' .or. mode == 'all') call run_repeat()

contains

    subroutine get_mode(md)
        character(len=*), intent(out) :: md
        integer :: k
        character(len=64) :: buf
        md = 'all'
        do k = 1, command_argument_count()
            call get_command_argument(k, buf)
            if (index(buf, '--mode=') == 1) md = trim(buf(8:))
        end do
    end subroutine get_mode

    function read_arg_int(flag, dflt) result(v)
        character(len=*), intent(in) :: flag
        integer(int64), intent(in) :: dflt
        integer(int64) :: v
        integer :: k
        character(len=64) :: buf
        v = dflt
        do k = 1, command_argument_count()
            call get_command_argument(k, buf)
            if (index(buf, flag) == 1) read(buf(len(flag) + 1:), *) v
        end do
    end function read_arg_int

    real(real64) function wall()
#ifdef _OPENMP
        wall = omp_get_wtime()
#else
        block
            integer(int64) :: t, rate
            call system_clock(t, rate)
            wall = real(t, real64) / real(rate, real64)
        end block
#endif
    end function wall

    ! ================================================================================
    ! The gate -- every arm against exact enumeration, before any timing
    ! ================================================================================

    !> Compares each arm's position-by-position draw distribution with exact successive sampling.
    !!
    !! `m` and `k` are tiny so the reference can be ENUMERATED rather than derived: every ordered
    !! triple of distinct items, its exact probability under the sequential renormalised scheme,
    !! attributed to the three positions. An arm passes when its worst deviation over all
    !! `3 * m` cells sits inside a few Monte Carlo standard errors.
    subroutine run_correct()
        integer(int64), parameter :: MC = 10_int64  ! 10 items
        integer, parameter :: KD = 3                ! draw 3 of them
        integer, parameter :: NTRIAL = 200000
        real(real64) :: w(MC), exact(KD, MC), emp(KD, MC)
        real(real64) :: cls_v(MAX_CLASS)
        integer(int64) :: cls_start(MAX_CLASS), cls_n(MAX_CLASS), idx(MC)
        integer(int64) :: lg_j(KD), item, sz, drawn(KD), tot_items
        integer :: lg_c(KD), nclass, arm, t, p, i
        real(real64), allocatable :: st(:)
        real(real64) :: lg_w(KD), se, worst, resid
        logical :: ok

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'GATE -- distribution of each arm vs exact successive sampling'
        write(output_unit, '(a,i0,a,i0,a,i0,a)') 'm = ', MC, ', draws = ', KD, ', trials = ', &
            NTRIAL, ' per arm'
        write(output_unit, '(a)') ''
        flush(output_unit)

        do i = 1, int(MC)
            w(i) = 1.0_real64
        end do
        do i = 7, int(MC)
            w(i) = 2.5_real64                       ! six at 1.0, four at 2.5
        end do
        call exact_positions(w, exact)

        se = sqrt(0.15_real64 * 0.85_real64 / real(NTRIAL, real64))
        write(output_unit, '(a,f8.5)') '   Monte Carlo standard error (worst cell) ~ ', se
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '   arm             worst |exact - empirical|    verdict'
        flush(output_unit)

        do arm = 1, 3
            emp = 0.0_real64
            sz = pow2_ceil(MC)
            if (allocated(st)) deallocate(st)
            allocate(st(2 * sz))
            do t = 1, NTRIAL
                select case (arm)
                case (1)
                    call cb_build(w, cls_v, cls_start, cls_n, idx, nclass)
                    do p = 1, KD
                        call cb_next(SEED + int(t, int64) * 7919_int64, 1_int64, int(p, int64), &
                                     cls_v, cls_start, cls_n, idx, nclass, item, lg_c(p), lg_j(p))
                        drawn(p) = item
                    end do
                case (2)
                    call st_build(w, st, sz)
                    do p = 1, KD
                        call st_next(SEED + int(t, int64) * 7919_int64, 1_int64, int(p, int64), &
                                     st, sz, item, lg_w(p))
                        drawn(p) = item
                    end do
                case default
                    call race_select(SEED + int(t, int64) * 7919_int64, 1_int64, w, &
                                     int(KD, int64), drawn)
                end select
                call check_distinct(drawn, MC, arm)
                do p = 1, KD
                    emp(p, drawn(p)) = emp(p, drawn(p)) + 1.0_real64
                end do
            end do
            emp = emp / real(NTRIAL, real64)
            worst = maxval(abs(exact - emp))
            ok = worst < 5.0_real64 * se
            write(output_unit, '(a,a,f16.5,a,a)') '   ', arm_name(arm), worst, '        ', &
                merge('PASS', 'FAIL', ok)
            flush(output_unit)
            if (.not. ok) error stop "run_correct: an arm does not reproduce successive sampling"
        end do

        ! Drain the segment tree completely and report the root residual: the accumulated
        ! floating-point error of maintaining sums by subtraction.
        sz = pow2_ceil(g_m)
        if (allocated(st)) deallocate(st)
        block
            real(real64), allocatable :: wbig(:)
            real(real64) :: junk
            allocate(wbig(g_m), st(2 * sz))
            call make_weights(g_m, g_c, wbig)
            call st_build(wbig, st, sz)
            tot_items = 0_int64
            do
                call st_next(SEED, 2_int64, tot_items + 1_int64, st, sz, item, junk)
                if (item == 0_int64) exit
                tot_items = tot_items + 1_int64
                if (tot_items > g_m) exit
            end do
            resid = st(1)
            write(output_unit, '(a)') ''
            write(output_unit, '(a,i0,a,i0)') '   segment tree drained: items drawn = ', &
                tot_items, ' of ', g_m
            write(output_unit, '(a,es12.4,a)') '   root residual after full drain = ', resid, &
                '   (exact arithmetic would give 0)'
            write(output_unit, '(a)') '   the class-bucket arm recomputes from counts, so its' // &
                ' equivalent residual is exactly 0'
            deallocate(wbig)
        end block
        write(output_unit, '(a)') ''
        flush(output_unit)
    end subroutine run_correct

    !> Name of an arm, padded for the report tables.
    pure function arm_name(a) result(nm)
        integer, intent(in) :: a                    !! 1 class-bucket, 2 segment tree, 3 race
        character(len=14) :: nm                     !! padded name
        select case (a)
        case (1);      nm = 'class-bucket  '
        case (2);      nm = 'segment-tree  '
        case default;  nm = 'exp-race      '
        end select
    end function arm_name

    !> Exact position-by-position probabilities under successive sampling, by enumeration.
    subroutine exact_positions(w, p)
        real(real64), intent(in) :: w(:)            !! the weights
        real(real64), intent(out) :: p(:, :)        !! `p(pos, item)`, shape (3, m)
        real(real64) :: tot, pi_, pj, pk
        integer :: i, j, k, m
        m = size(w)
        tot = sum(w)
        p = 0.0_real64
        do i = 1, m
            pi_ = w(i) / tot
            do j = 1, m
                if (j == i) cycle
                pj = w(j) / (tot - w(i))
                do k = 1, m
                    if (k == i .or. k == j) cycle
                    pk = w(k) / (tot - w(i) - w(j))
                    p(1, i) = p(1, i) + pi_ * pj * pk
                    p(2, j) = p(2, j) + pi_ * pj * pk
                    p(3, k) = p(3, k) + pi_ * pj * pk
                end do
            end do
        end do
    end subroutine exact_positions

    !> A drawn sequence must hold distinct, in-range items -- checked on every trial.
    subroutine check_distinct(d, m, arm)
        integer(int64), intent(in) :: d(:)          !! the drawn items
        integer(int64), intent(in) :: m             !! population size
        integer, intent(in) :: arm                  !! which arm produced it
        integer :: a, b
        do a = 1, size(d)
            if (d(a) < 1_int64 .or. d(a) > m) then
                write(output_unit, '(a,a,a,i0)') 'FATAL: ', arm_name(arm), ' out of range: ', d(a)
                error stop 1
            end if
            do b = a + 1, size(d)
                if (d(a) == d(b)) then
                    write(output_unit, '(a,a,a,i0)') 'FATAL: ', arm_name(arm), &
                        ' repeated an item: ', d(a)
                    error stop 1
                end if
            end do
        end do
    end subroutine check_distinct

    ! ================================================================================
    ! Measurement 3 -- setup cost against m
    ! ================================================================================

    subroutine run_setup()
        ! Capped at 1e7: the distinct-weight discovery is O(M*C) as written, so 1e8 x 64
        ! classes is minutes per build. The trend over four decades extrapolates; a production
        ! implementation would use a hash there and should be re-measured if it does.
        integer(int64) :: sizes(4) = [10000_int64, 100000_int64, 1000000_int64, 10000000_int64]
        integer :: clist(3) = [2, 8, 64]
        integer(int64) :: m, sz
        integer :: si, ci, rd, nclass
        real(real64) :: t0, bcb, bst, brace
        real(real64), allocatable :: w(:), cls_v(:), st(:)
        integer(int64), allocatable :: cls_start(:), cls_n(:), idx(:), out(:)

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'SETUP COST -- one-off build, ms (measurement 3)'
        write(output_unit, '(a)') 'race column is the FULL cost of one k=1 draw: it has no' // &
            ' reusable setup'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '           m    C   bucket(ms)    segtree(ms)     race k=1(ms)'
        flush(output_unit)

        do si = 1, size(sizes)
            m = sizes(si)
            allocate(w(m), cls_v(MAX_CLASS), cls_start(MAX_CLASS), cls_n(MAX_CLASS), idx(m))
            sz = pow2_ceil(m)
            allocate(st(2 * sz), out(max(1_int64, m)))
            do ci = 1, size(clist)
                call make_weights(m, clist(ci), w)
                bcb = huge(1.0_real64)
                bst = huge(1.0_real64)
                brace = huge(1.0_real64)
                do rd = 1, g_rounds
                    t0 = wall()
                    call cb_build(w, cls_v, cls_start, cls_n, idx, nclass)
                    bcb = min(bcb, wall() - t0)
                    t0 = wall()
                    call st_build(w, st, sz)
                    bst = min(bst, wall() - t0)
                    if (m <= 10000000_int64) then
                        t0 = wall()
                        call race_select(SEED, 1_int64, w, 1_int64, out)
                        brace = min(brace, wall() - t0)
                    end if
                end do
                if (brace < huge(1.0_real64)) then
                    write(output_unit, '(i12,i5,3f15.3)') m, clist(ci), bcb * 1000.0_real64, &
                        bst * 1000.0_real64, brace * 1000.0_real64
                else
                    write(output_unit, '(i12,i5,2f15.3,a)') m, clist(ci), bcb * 1000.0_real64, &
                        bst * 1000.0_real64, '        skipped'
                end if
                flush(output_unit)
            end do
            deallocate(w, cls_v, cls_start, cls_n, idx, st, out)
        end do
        write(output_unit, '(a)') ''
        flush(output_unit)
    end subroutine run_setup

    ! ================================================================================
    ! Measurements 1 and 2 -- total cost against k, and against C
    ! ================================================================================

    subroutine run_cost()
        integer(int64) :: klist(7) = [1_int64, 10_int64, 100_int64, 1000_int64, 10000_int64, &
                                      100000_int64, 1000000_int64]
        integer :: ci, ki, rd, nclass, p
        integer(int64) :: m, sz, k, item
        real(real64) :: t0, bcb, bst, brace, junk
        real(real64), allocatable :: w(:), cls_v(:), st(:), st0(:)
        integer(int64), allocatable :: cls_start(:), cls_n(:), idx(:), out(:), lg_j(:)
        integer, allocatable :: lg_c(:)
        integer :: clist(3) = [2, 8, 64]

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'TOTAL COST -- build + k draws, ms (measurements 1 and 2)'
        write(output_unit, '(a,i0)') 'm = ', g_m
        write(output_unit, '(a)') 'race is FLATTERED: given k up front, which the real case' // &
            ' cannot supply'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '   C          k    bucket(ms)   segtree(ms)      race(ms)' // &
            '   race/bucket'
        flush(output_unit)

        m = g_m
        sz = pow2_ceil(m)
        allocate(w(m), cls_v(MAX_CLASS), cls_start(MAX_CLASS), cls_n(MAX_CLASS), idx(m))
        allocate(st(2 * sz), st0(2 * sz), out(m), lg_j(m), lg_c(m))

        do ci = 1, size(clist)
            call make_weights(m, clist(ci), w)
            call st_build(w, st0, sz)
            do ki = 1, size(klist)
                k = klist(ki)
                if (k > m) cycle
                bcb = huge(1.0_real64)
                bst = huge(1.0_real64)
                brace = huge(1.0_real64)
                do rd = 1, g_rounds
                    t0 = wall()
                    call cb_build(w, cls_v, cls_start, cls_n, idx, nclass)
                    do p = 1, int(k)
                        call cb_next(SEED, 1_int64, int(p, int64), cls_v, cls_start, cls_n, &
                                     idx, nclass, item, lg_c(p), lg_j(p))
                    end do
                    bcb = min(bcb, wall() - t0)

                    ! A FULL build, not a copy of `st0`: the bucket arm above pays a full
                    ! `cb_build`, so anything less here would be cold-start against warm-restart.
                    ! Warm reuse is measured on its own terms in `--mode=repeat`.
                    t0 = wall()
                    call st_build(w, st, sz)
                    do p = 1, int(k)
                        call st_next(SEED, 1_int64, int(p, int64), st, sz, item, junk)
                    end do
                    bst = min(bst, wall() - t0)

                    t0 = wall()
                    call race_select(SEED, 1_int64, w, k, out)
                    brace = min(brace, wall() - t0)
                end do
                write(output_unit, '(i4,i11,3f14.3,f14.2)') clist(ci), k, bcb * 1000.0_real64, &
                    bst * 1000.0_real64, brace * 1000.0_real64, brace / bcb
                flush(output_unit)
            end do
            write(output_unit, '(a)') ''
        end do
        deallocate(w, cls_v, cls_start, cls_n, idx, st, st0, out, lg_j, lg_c)
        flush(output_unit)
    end subroutine run_cost

    ! ================================================================================
    ! Reuse -- many sequences from the SAME weights
    ! ================================================================================

    subroutine run_repeat()
        integer, parameter :: NSEQ = 200
        integer(int64) :: klist(3) = [10_int64, 100_int64, 1000_int64]
        integer :: ki, s, p, nclass, rd
        integer(int64) :: m, sz, k, item
        real(real64) :: t0, brestore, brebuild, brace, bstres
        real(real64), allocatable :: w(:), cls_v(:), st(:), stlg_w(:)
        integer(int64), allocatable :: cls_start(:), cls_n(:), idx(:), out(:), lg_j(:), stlg_i(:)
        integer, allocatable :: lg_c(:)

        m = g_m
        sz = pow2_ceil(m)
        allocate(w(m), cls_v(MAX_CLASS), cls_start(MAX_CLASS), cls_n(MAX_CLASS), idx(m))
        allocate(st(2 * sz), out(m), lg_j(m), lg_c(m), stlg_i(m), stlg_w(m))
        call make_weights(m, g_c, w)

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a,i0,a)') 'REUSE -- ', NSEQ, ' independent sequences, same weights'
        write(output_unit, '(a,i0,a,i0)') 'm = ', m, ', C = ', g_c
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '          k   bucket+restore   segtree+restore' // &
            '   bucket+rebuild        race       best'
        flush(output_unit)

        do ki = 1, size(klist)
            k = klist(ki)
            brestore = huge(1.0_real64)
            brebuild = huge(1.0_real64)
            brace = huge(1.0_real64)
            bstres = huge(1.0_real64)
            do rd = 1, g_rounds
                ! build once, restore between sequences
                t0 = wall()
                call cb_build(w, cls_v, cls_start, cls_n, idx, nclass)
                do s = 1, NSEQ
                    do p = 1, int(k)
                        call cb_next(SEED + int(s, int64), 1_int64, int(p, int64), cls_v, &
                                     cls_start, cls_n, idx, nclass, item, lg_c(p), lg_j(p))
                    end do
                    call cb_restore(k, lg_c, lg_j, cls_start, cls_n, idx)
                end do
                brestore = min(brestore, wall() - t0)

                ! segment tree: build once, O(k log M) restore between sequences
                t0 = wall()
                call st_build(w, st, sz)
                do s = 1, NSEQ
                    do p = 1, int(k)
                        call st_next(SEED + int(s, int64), 1_int64, int(p, int64), st, sz, &
                                     item, stlg_w(p))
                        stlg_i(p) = item
                    end do
                    call st_restore(k, stlg_i, stlg_w, st, sz)
                end do
                bstres = min(bstres, wall() - t0)

                ! rebuild from scratch each sequence
                t0 = wall()
                do s = 1, NSEQ
                    call cb_build(w, cls_v, cls_start, cls_n, idx, nclass)
                    do p = 1, int(k)
                        call cb_next(SEED + int(s, int64), 1_int64, int(p, int64), cls_v, &
                                     cls_start, cls_n, idx, nclass, item, lg_c(p), lg_j(p))
                    end do
                end do
                brebuild = min(brebuild, wall() - t0)

                ! the race has no reusable state at all
                t0 = wall()
                do s = 1, NSEQ
                    call race_select(SEED + int(s, int64), 1_int64, w, k, out)
                end do
                brace = min(brace, wall() - t0)
            end do
            write(output_unit, '(i11,4f17.3,a)') k, brestore * 1000.0_real64, &
                bstres * 1000.0_real64, brebuild * 1000.0_real64, brace * 1000.0_real64, &
                merge('   segtree', '   bucket ', bstres < brestore)
            ! Does the segment tree survive being restored NSEQ times? Every restore adds back
            ! onto sums that were maintained by subtraction, so error can compound across cycles.
            ! This is the one correctness risk in the arm the cost table favours, so measure it.
            write(output_unit, '(a,es11.3,a,es11.3)') '              tree drift after ' // &
                'restores: root - true total = ', st(1) - sum(w), '   relative ', &
                (st(1) - sum(w)) / sum(w)
            flush(output_unit)
        end do
        deallocate(w, cls_v, cls_start, cls_n, idx, st, out, lg_j, lg_c, stlg_i, stlg_w)
        write(output_unit, '(a)') ''
        flush(output_unit)
    end subroutine run_repeat

end program probe_random_weighted
