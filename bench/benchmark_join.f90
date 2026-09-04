!> Benchmarks `parquet_table%join`: what the engine costs, what the column rewrite costs, and
!> whether the WAVES-shaped lookup join is paying for a sort it does not need.
!!
!! Driven by `bench/benchmark_join.sh`, which is where the environment variables and the
!! `--profile release` assertion live. Modes:
!!
!! * `shape`   -- THE MODE S6 P7 EXISTS FOR: a large left table against a small unique-keyed
!!                lookup table, `how="left"`, `require="m:1"`. Beside the join it times the SAME
!!                join against a right table holding nothing but the key (whose difference is the
!!                whole column rewrite), the two engine phases separately, and -- as the
!!                alternative -- a `pf_index_map` built over the right key and probed once per
!!                left row. That last arm is the prize the hash path would claim; if it is not
!!                well under the SORT it would replace, and that sort not a large share of the
!!                join, the hash path is not worth a second engine. It also prints the shares,
!!                including the residue that is neither the sort nor the rewrite -- which is
!!                where this join's time actually goes on a high-cardinality key.
!! * `size`    -- a symmetric inner join at four sizes, to see how the whole operation scales.
!! * `how`     -- all six `how` values on one fixture, per OUTPUT row, since they emit different
!!                numbers of them.
!! * `payload` -- an inner join carrying 1, 2, 4 and 8 columns. The SLOPE is the per-column
!!                rewrite cost, which is what a fused copy-gather-nullfill would attack; the
!!                intercept is everything else.
!!
!! **The decomposition is measured through the library's own API, not replicated here.** The
!! `key build` and `sort` arms call `%deep_copy`/`%append` and `pf_argsort` exactly as
!! `join_build_keys` and `table_join_pairs` do, so they are the engine's own phases rather than a
!! model of them -- and their sum must come in below the full join they decompose, which is the
!! check CLAUDE.md asks any harness that reproduces library code to pass before it is believed.
!! `payload`'s own control is the printed column count of the joined table: a `columns=` that
!! silently carried one column would give four identical figures and a confident wrong verdict.
!!
!! **Rules this program follows, from CLAUDE.md's benchmarking section.** Every fixture is built
!! and written before any timed loop; `%join` MUTATES its left table, so each round joins a fresh
!! `%clone` taken OUTSIDE the timer, which is also what keeps the fixture constant across rounds;
!! each figure is the best of `ROUNDS` rounds, the run least disturbed by everything else on the
!! machine; the row walks use wrapping counters rather than `mod` with a runtime divisor; and a
!! checksum is accumulated outside every timed region and printed, so no arm can be optimised
!! away.
!!
!! **Nothing here opens a file.** Every table is built in memory, so no figure reaches
!! `src/parquet_wrapper.cpp` and the wrapper deliberately asserts nothing about `FPM_CXXFLAGS`.
program benchmark_join
    use parquet
    use iso_fortran_env, only: int64, real64, output_unit
#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime, omp_get_max_threads
#endif
    implicit none

    integer(int64) :: nleft, nright, nsym
    integer :: rounds, threads_req, ncols_max
    character(len=:), allocatable :: mode
    integer(int64) :: checksum

    checksum = 0_int64
    call read_arguments()
    write (output_unit, "(a)") "# benchmark_join -- parquet_table%join"
    write (output_unit, "(a,i0,a,i0,a,i0)") "# nleft=", nleft, " nright=", nright, " nsym=", nsym
    write (output_unit, "(a,i0,a,i0,a,i0)") "# rounds=", rounds, " threads=", threads_req, &
        " ncols_max=", ncols_max
#ifdef _OPENMP
    write (output_unit, "(a,i0)") "# omp_get_max_threads=", omp_get_max_threads()
#else
    write (output_unit, "(a)") "# built without OpenMP: every figure is serial"
#endif
    write (output_unit, "(a)") ""

    select case (mode)
    case ("shape")
        call mode_shape()
    case ("size")
        call mode_size()
    case ("how")
        call mode_how()
    case ("payload")
        call mode_payload()
    case ("all")
        call mode_shape()
        call mode_size()
        call mode_how()
        call mode_payload()
    case default
        write (output_unit, "(a)") "unknown mode: " // mode
        stop 2
    end select
    write (output_unit, "(a)") ""
    write (output_unit, "(a,i0)") "# checksum ", checksum

contains

    !> Reads the `--key=value` flags the shell wrapper forwards.
    subroutine read_arguments()
        character(len=256) :: arg
        integer :: i, n

        nleft = 4000000_int64
        nright = 4000_int64
        nsym = 1000000_int64
        rounds = 3
        threads_req = 0
        ncols_max = 8
        mode = "all"
        n = command_argument_count()
        do i = 1, n
            call get_command_argument(i, arg)
            if (index(arg, "--nleft=") == 1) then
                read (arg(9:), *) nleft
            else if (index(arg, "--nright=") == 1) then
                read (arg(10:), *) nright
            else if (index(arg, "--nsym=") == 1) then
                read (arg(8:), *) nsym
            else if (index(arg, "--rounds=") == 1) then
                read (arg(10:), *) rounds
            else if (index(arg, "--threads=") == 1) then
                read (arg(11:), *) threads_req
            else if (index(arg, "--ncols=") == 1) then
                read (arg(9:), *) ncols_max
            else if (index(arg, "--mode=") == 1) then
                mode = trim(arg(8:))
            end if
        end do
    end subroutine read_arguments

    !> Seconds now, from the best clock available.
    function now() result(t)
        real(real64) :: t !! seconds, on an arbitrary origin.
#ifdef _OPENMP
        t = omp_get_wtime()
#else
        integer(int64) :: c, r
        call system_clock(c, r)
        t = real(c, real64) / real(r, real64)
#endif
    end function now

    !> Prints one figure as nanoseconds per unit, and the whole elapsed time beside it.
    subroutine report(label, best, units)
        character(len=*), intent(in) :: label !! what was measured.
        real(real64), intent(in) :: best      !! best elapsed time over the rounds, seconds.
        integer(int64), intent(in) :: units   !! rows the figure is divided by.

        write (output_unit, "(a,t50,f12.3,a,f11.3,a)") label, &
            best * 1.0e9_real64 / real(max(units, 1_int64), real64), " ns/row  ", &
            best * 1.0e3_real64, " ms"
        flush (output_unit)
    end subroutine report

    !> A reproducible scattered integer key. `hi` bounds the value range, so a small `hi` is how a
    !! fixture is given duplicates and a large one is how it is made unique-ish.
    subroutine fill_keys(seed, n, hi, keys)
        integer(int64), intent(in) :: seed  !! stream family.
        integer(int64), intent(in) :: n     !! how many.
        integer(int64), intent(in) :: hi    !! top of the closed range; the bottom is 1.
        integer(int64), allocatable, intent(out) :: keys(:) !! the keys.
        integer(int64) :: i

        allocate(keys(n))
        do i = 1_int64, n
            keys(i) = pf_random_int_at(seed, i, 1_int64, hi)
        end do
    end subroutine fill_keys

    !> A left table: one key column and one payload column.
    subroutine build_left(t, keys)
        type(parquet_table), intent(out) :: t  !! the table.
        integer(int64), intent(in) :: keys(:)  !! its key column.
        integer(int64), allocatable :: v(:)
        integer(int64) :: i

        allocate(v(size(keys, kind=int64)))
        do i = 1_int64, size(keys, kind=int64)
            v(i) = i
        end do
        call parquet_new_table(t)
        call t%add_column("id", keys)
        call t%add_column("lv", v)
    end subroutine build_left

    !> A right table with `ncols` payload columns named `v1` .. `vN`.
    subroutine build_right(t, keys, ncols)
        type(parquet_table), intent(out) :: t  !! the table.
        integer(int64), intent(in) :: keys(:)  !! its key column.
        integer, intent(in) :: ncols           !! payload columns to give it.
        integer(int64), allocatable :: v(:)
        character(len=8) :: nm
        integer(int64) :: i
        integer :: c

        allocate(v(size(keys, kind=int64)))
        call parquet_new_table(t)
        call t%add_column("id", keys)
        do c = 1, ncols
            do i = 1_int64, size(keys, kind=int64)
                v(i) = i * int(c, int64)
            end do
            write (nm, "(a,i0)") "v", c
            call t%add_column(trim(nm), v)
        end do
    end subroutine build_right

    !> Best-of-`rounds` time for one `%join`, each round on a fresh clone of `base`.
    !!
    !! The clone is outside the timer for two reasons and both matter: `%join` mutates, so
    !! without it round 2 would join an already-joined table, and its allocation is not part of
    !! what a caller pays for a join.
    subroutine time_join(base, other, on, how, columns, require, best, n_out, ncols_out)
        type(parquet_table), intent(in) :: base   !! the left table, never mutated.
        type(parquet_table), intent(in) :: other  !! the right table.
        character(len=*), intent(in) :: on        !! the key, as a separated string.
        character(len=*), intent(in) :: how       !! join kind.
        character(len=*), intent(in), optional :: columns !! payload list; absent carries all.
        character(len=*), intent(in), optional :: require !! cardinality assertion.
        real(real64), intent(out) :: best         !! best elapsed seconds.
        integer(int64), intent(out) :: n_out      !! rows the join produced.
        integer, intent(out), optional :: ncols_out !! columns it produced; the payload control.
        type(parquet_table) :: w
        real(real64) :: t0, t1
        integer :: r

        best = huge(1.0_real64)
        n_out = 0_int64
        do r = 1, rounds
            call base%clone(w)
            t0 = now()
            if (threads_req > 0) then
                call w%join(other, on, how=how, columns=columns, require=require, &
                    threads=threads_req)
            else
                call w%join(other, on, how=how, columns=columns, require=require)
            end if
            t1 = now()
            if (t1 - t0 < best) best = t1 - t0
            n_out = w%nrows()
            if (present(ncols_out)) ncols_out = w%ncols()
            checksum = checksum + n_out
        end do
    end subroutine time_join

    !> The WAVES shape: a large catalogue against a small unique-keyed lookup table.
    subroutine mode_shape()
        type(parquet_table) :: a, b, bkey
        type(parquet_column) :: kl, kr, kc
        type(pf_sort_keys) :: sk
        type(pf_index_map) :: map
        integer(int64), allocatable :: lk(:), rk(:), perm(:), go(:), hits(:)
        real(real64) :: t0, t1, best_join, best_key, best_sort, best_hash, best_nopay
        integer(int64) :: n_out, i
        integer :: r

        write (output_unit, "(a)") "## shape -- a large left table against a small lookup table"
        write (output_unit, "(a,i0,a,i0,a)") "   (", nleft, " left rows, ", nright, &
            " right rows, unique right key, how=left require=m:1)"
        write (output_unit, "(a)") ""
        ! A unique right key drawn from a range far wider than the table, so the m:1 assertion
        ! holds; the left key is drawn from that same range, so most left rows find nothing --
        ! which is the honest lookup-table shape and NOT the easy case for the engine.
        allocate(rk(nright))
        do i = 1_int64, nright
            rk(i) = i * 7919_int64
        end do
        call fill_keys(20260905_int64, nleft, nright * 7919_int64, lk)
        call build_left(a, lk)
        call build_right(b, rk, 1)

        call time_join(a, b, "id", "left", require="m:1", best=best_join, n_out=n_out)
        call report("%join(left, require=m:1)", best_join, nleft)

        ! The same join against a right table holding nothing but the key, so no payload column
        ! is gathered at all. The difference is the whole column rewrite -- the phase a fused
        ! copy-gather-nullfill would attack -- measured without a debug hook.
        call build_right(bkey, rk, 0)
        call time_join(a, bkey, "id", "left", require="m:1", best=best_nopay, n_out=n_out)
        call report("  the same join carrying NO payload column", best_nopay, nleft)

        ! Engine phase 1: the concatenated key column each join key needs. These are the two
        ! calls `join_build_keys` makes, on the same public bindings, so this is the engine's own
        ! phase rather than a model of it.
        call kl%init(PK_INT64, nleft)
        call kl%set_all(lk)
        call kr%init(PK_INT64, nright)
        call kr%set_all(rk)
        best_key = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call kl%deep_copy(kc)
            call kc%append(kr)
            t1 = now()
            if (t1 - t0 < best_key) best_key = t1 - t0
            checksum = checksum + kc%length()
        end do
        call report("  engine: %deep_copy + %append the key", best_key, nleft)

        ! Engine phase 2: the sort that answers every question the join asks, asked for with the
        ! run boundaries the engine reads its matches off -- which costs an extra copy of a key,
        ! so a plain pf_argsort here would understate it.
        best_sort = huge(1.0_real64)
        do r = 1, rounds
            call sk%clear()
            call sk%add(kc)
            t0 = now()
            if (threads_req > 0) then
                call pf_argsort(sk, perm, group_offsets=go, threads=threads_req)
            else
                call pf_argsort(sk, perm, group_offsets=go)
            end if
            t1 = now()
            if (t1 - t0 < best_sort) best_sort = t1 - t0
            checksum = checksum + size(go, kind=int64)
        end do
        call report("  engine: pf_argsort over nleft+nright", best_sort, nleft)

        ! The alternative: a map over the SMALL side, probed once per left row. This is what the
        ! hash path would do instead of sorting nleft+nright, and the whole of P7's question is
        ! whether the gap is worth a second engine.
        allocate(hits(nleft))
        best_hash = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call map%build(rk)
            call map%get_many(lk, hits)
            t1 = now()
            if (t1 - t0 < best_hash) best_hash = t1 - t0
            checksum = checksum + count(hits /= 0_int64, kind=int64)
            call map%clear()
        end do
        call report("  ALTERNATIVE: pf_index_map build(right)+get_many(left)", best_hash, nleft)
        write (output_unit, "(a)") ""
        write (output_unit, "(a,f8.3)") "   engine phases as a share of the join: ", &
            (best_key + best_sort) / best_join
        write (output_unit, "(a,f8.3)") "   the map alternative as a share of the sort: ", &
            best_hash / best_sort
        write (output_unit, "(a,f8.3)") "   the payload rewrite as a share of the join:  ", &
            (best_join - best_nopay) / best_join
        write (output_unit, "(a,f8.3)") "   everything that is NEITHER, as a share:      ", &
            (best_nopay - best_key - best_sort) / best_join
        write (output_unit, "(a,i0)") "   rows the join produced: ", n_out
        write (output_unit, "(a)") ""
    end subroutine mode_shape

    !> A symmetric inner join at four sizes.
    subroutine mode_size()
        type(parquet_table) :: a, b
        integer(int64), allocatable :: lk(:), rk(:)
        real(real64) :: best
        integer(int64) :: n, n_out
        integer :: s
        character(len=64) :: label

        write (output_unit, "(a)") "## size -- a symmetric inner join, per OUTPUT row"
        write (output_unit, "(a)") ""
        do s = 3, 0, -1
            n = max(nsym / (4_int64 ** s), 1000_int64)
            call fill_keys(20260906_int64, n, n, lk)
            call fill_keys(20260907_int64, n, n, rk)
            call build_left(a, lk)
            call build_right(b, rk, 1)
            call time_join(a, b, "id", "inner", best=best, n_out=n_out)
            write (label, "(a,i0,a,i0,a)") "%join(inner) n=", n, "  (", n_out, " out)"
            call report(trim(label), best, n_out)
        end do
        write (output_unit, "(a)") ""
    end subroutine mode_size

    !> All six `how` values on one fixture.
    subroutine mode_how()
        type(parquet_table) :: a, b
        integer(int64), allocatable :: lk(:), rk(:)
        real(real64) :: best
        integer(int64) :: n_out
        integer :: h
        character(len=16) :: hows(6)
        character(len=64) :: label

        hows = [character(len=16) :: "inner", "left", "right", "outer", "semi", "anti"]
        write (output_unit, "(a)") "## how -- every join kind on one fixture, per OUTPUT row"
        write (output_unit, "(a)") "   (semi and anti do work proportional to the INPUT and " // &
            "emit a fraction of it, so read"
        write (output_unit, "(a)") "    their ms column, not their ns/row one)"
        write (output_unit, "(a,i0,a)") "   (", nsym, " rows each side)"
        write (output_unit, "(a)") ""
        call fill_keys(20260906_int64, nsym, nsym, lk)
        call fill_keys(20260907_int64, nsym, nsym, rk)
        call build_left(a, lk)
        call build_right(b, rk, 1)
        do h = 1, 6
            call time_join(a, b, "id", trim(hows(h)), best=best, n_out=n_out)
            write (label, "(a,a,a,i0,a)") "%join(", trim(hows(h)), ")  (", n_out, " out)"
            call report(trim(label), best, n_out)
        end do
        write (output_unit, "(a)") ""
    end subroutine mode_how

    !> An inner join carrying 1, 2, 4 and 8 columns: the slope is the per-column rewrite cost.
    subroutine mode_payload()
        type(parquet_table) :: a, b
        integer(int64), allocatable :: lk(:), rk(:)
        real(real64) :: best, first
        integer(int64) :: n_out
        integer :: w, c, nc
        character(len=256) :: cols, more
        character(len=64) :: label

        write (output_unit, "(a)") "## payload -- an inner join carrying W columns, per OUTPUT row"
        write (output_unit, "(a,i0,a)") "   (", nsym, " rows each side; the SLOPE in W is the " // &
            "per-column rewrite cost)"
        write (output_unit, "(a)") ""
        call fill_keys(20260906_int64, nsym, nsym, lk)
        call fill_keys(20260907_int64, nsym, nsym, rk)
        call build_left(a, lk)
        call build_right(b, rk, ncols_max)
        first = 0.0_real64
        w = 1
        do while (w <= ncols_max)
            ! Built through a second buffer: `write (cols, ...) trim(cols), ...` would use one
            ! internal file as both the record and an output-list item, which the standard
            ! forbids and no compiler here diagnoses. It silently produced four identical arms
            ! the first time this mode was run, which is exactly the shape of a benchmark that
            ! measures nothing and says so in three significant figures.
            cols = "v1"
            do c = 2, w
                write (more, "(a,i0)") ",v", c
                cols = trim(cols) // trim(more)
            end do
            call time_join(a, b, "id", "inner", columns=trim(cols), best=best, n_out=n_out, &
                ncols_out=nc)
            ! The result's column count is this mode's negative control: without it a `columns=`
            ! that silently carried one column would give four identical figures and a confident
            ! "the rewrite is free" verdict. It must read w + 2 (this table's key and payload).
            write (label, "(a,i0,a,i0,a)") "%join(inner) carrying ", w, " column(s) -> ", nc, &
                " cols"
            call report(trim(label), best, n_out)
            if (w == 1) first = best
            w = w * 2
        end do
        if (first > 0.0_real64) then
            write (output_unit, "(a,f8.3)") "   cost of the last doubling, relative to W=1: ", &
                best / first
        end if
        write (output_unit, "(a)") ""
    end subroutine mode_payload

end program benchmark_join
