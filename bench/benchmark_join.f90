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
!! * `nullfill` -- THE MODE THAT HOLDS A COMPLEXITY CLASS: a left join null-filling two 16-byte
!!                string payload columns over a sweep of left row counts, beside the same join
!!                without them, then `%fillna` and `%ffill` of a 16-byte string column with 75%
!!                nulls beside the same verb on a float64 column. Two facts per size: the string
!!                to numeric RATIO, and how the time GREW from the previous size -- about 2 per
!!                doubling is linear, about 4 is quadratic. A per-element null-fill of a string
!!                column compacts the payload once per element, which is exactly the quadratic
!!                shape this mode exists to keep out; no unit test can assert it.
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
    integer(int64), allocatable :: nullfill_rows(:)
    integer(int64) :: checksum

    checksum = 0_int64
    call read_arguments()
    write (output_unit, "(a)") "# benchmark_join -- parquet_table%join"
    write (output_unit, "(a,i0,a,i0,a,i0)") "# nleft=", nleft, " nright=", nright, " nsym=", nsym
    write (output_unit, "(a,i0,a,i0,a,i0)") "# rounds=", rounds, " threads=", threads_req, &
        " ncols_max=", ncols_max
    write (output_unit, "(a,*(i0,:,','))") "# nullfill_rows=", nullfill_rows
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
    case ("nullfill")
        call mode_nullfill()
    case ("all")
        call mode_shape()
        call mode_size()
        call mode_how()
        call mode_payload()
        call mode_nullfill()
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
        nullfill_rows = [100000_int64, 200000_int64, 400000_int64, 800000_int64]
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
            else if (index(arg, "--nullfill-rows=") == 1) then
                call parse_row_list(arg(17:), nullfill_rows)
            end if
        end do
    end subroutine read_arguments

    !> Reads a comma-separated list of row counts, in the order given.
    subroutine parse_row_list(text, rows)
        character(len=*), intent(in) :: text                 !! "100000,200000,...".
        integer(int64), allocatable, intent(out) :: rows(:)  !! the counts.
        integer :: i, start, n

        n = 0
        do i = 1, len_trim(text)
            if (text(i:i) == ",") n = n + 1
        end do
        allocate(rows(n + 1))
        n = 0
        start = 1
        do i = 1, len_trim(text) + 1
            if (i > len_trim(text)) then
                n = n + 1
                read (text(start:i - 1), *) rows(n)
            else if (text(i:i) == ",") then
                n = n + 1
                read (text(start:i - 1), *) rows(n)
                start = i + 1
            end if
        end do
    end subroutine parse_row_list

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

    !> Best-of-`rounds` time for one fill verb on one column, each round on a fresh clone.
    subroutine time_fill(base, name, verb, is_string, best)
        type(parquet_table), intent(in) :: base    !! the table, never mutated.
        character(len=*), intent(in) :: name       !! the column to fill.
        character(len=*), intent(in) :: verb       !! "fillna" or "ffill".
        logical, intent(in) :: is_string           !! chooses the fill value's type for %fillna.
        real(real64), intent(out) :: best          !! best elapsed seconds.
        type(parquet_table) :: w
        real(real64) :: t0, t1
        integer :: r

        best = huge(1.0_real64)
        do r = 1, rounds
            call base%clone(w)
            t0 = now()
            if (verb == "fillna") then
                if (is_string) then
                    call w%fillna(name, "n/a")
                else
                    call w%fillna(name, -1.0_real64)
                end if
            else
                call w%ffill(name)
            end if
            t1 = now()
            if (t1 - t0 < best) best = t1 - t0
            ! The fill's own effect, so a verb optimised into a no-op cannot report a figure.
            if (.not. w%has_nulls(name)) checksum = checksum + 1_int64
        end do
    end subroutine time_fill

    !> One row of --mode=nullfill: the string arm, its numeric control, their ratio, and each
    !! arm's growth from the previous size (0 on the first size).
    subroutine report_pair(label, n, t_str, t_num, prev_str, prev_num)
        character(len=*), intent(in) :: label      !! what was measured.
        integer(int64), intent(in) :: n            !! rows at this size.
        real(real64), intent(in) :: t_str, t_num   !! best seconds, string arm and numeric control.
        real(real64), intent(inout) :: prev_str, prev_num !! the previous size's times; updated.
        real(real64) :: g_str, g_num

        g_str = 0.0_real64
        g_num = 0.0_real64
        if (prev_str > 0.0_real64) g_str = t_str / prev_str
        if (prev_num > 0.0_real64) g_num = t_num / prev_num
        write (output_unit, "(a,t26,i9,a,f11.3,a,f11.3,a,f8.2,a,f6.2,a,f6.2)") label, n, " rows:", &
            t_str * 1.0e3_real64, " ms strings,", t_num * 1.0e3_real64, " ms numeric, ratio", &
            t_str / max(t_num, 1.0e-9_real64), "  growth", g_str, " /", g_num
        flush (output_unit)
        prev_str = t_str
        prev_num = t_num
    end subroutine report_pair

    !> The null-fill sweep: a left join carrying two 16-byte string columns beside the same join
    !! carrying two int64 ones, then `%fillna` and `%ffill` on a 16-byte string column beside a
    !! float64 one, at every size of `nullfill_rows`.
    subroutine mode_nullfill()
        type(parquet_table) :: a, b_str, b_num, t_str, t_num
        type(parquet_string_column) :: sc
        integer(int64), allocatable :: lk(:), rk(:), rv(:)
        character(len=16), allocatable :: rs(:)
        real(real64), allocatable :: tx(:)
        logical, allocatable :: tvalid(:)
        real(real64) :: t_join_s, t_join_n, t_fill_s, t_fill_n, t_ff_s, t_ff_n
        real(real64) :: p_join_s, p_join_n, p_fill_s, p_fill_n, p_ff_s, p_ff_n
        integer(int64) :: n, n_out, nlook, i, r
        character(len=16) :: one
        integer :: s

        nlook = 10000_int64
        write (output_unit, "(a)") "## nullfill -- a left join null-filling two 16-byte string columns, " // &
            "and the fill verbs"
        write (output_unit, "(a,i0,a)") "   (", nlook, " right rows with a unique key; one left row in four " // &
            "matches, three are null-filled;"
        write (output_unit, "(a)") "    the fill verbs see a column three-quarters null; growth = this " // &
            "size's time over the previous"
        write (output_unit, "(a)") "    size's: about 2 per doubling is linear, about 4 is quadratic)"
        write (output_unit, "(a)") ""
        ! The lookup table: keys i*7919 (unique), two 16-byte string columns, and its numeric twin.
        allocate(rk(nlook), rv(nlook), rs(nlook))
        do i = 1_int64, nlook
            rk(i) = i * 7919_int64
            rv(i) = i
            write (rs(i), "(a,i11.11)") "str_v", i
        end do
        call parquet_new_table(b_str)
        call b_str%add_column("id", rk)
        call b_str%add_column("s1", rs)
        call b_str%add_column("s2", rs)
        call parquet_new_table(b_num)
        call b_num%add_column("id", rk)
        call b_num%add_column("v1", rv)
        call b_num%add_column("v2", rv)
        p_join_s = 0.0_real64
        p_join_n = 0.0_real64
        p_fill_s = 0.0_real64
        p_fill_n = 0.0_real64
        p_ff_s = 0.0_real64
        p_ff_n = 0.0_real64
        do s = 1, size(nullfill_rows)
            n = nullfill_rows(s)
            ! Left keys: every fourth row takes a right key, the rest a value no right key holds.
            allocate(lk(n))
            do i = 1_int64, n
                r = pf_random_int_at(20260907_int64, i, 1_int64, nlook)
                if (mod(i, 4_int64) == 0_int64) then
                    lk(i) = r * 7919_int64
                else
                    lk(i) = r * 7919_int64 + 1_int64
                end if
            end do
            call build_left(a, lk)
            call time_join(a, b_str, "id", "left", require="m:1", best=t_join_s, n_out=n_out)
            call time_join(a, b_num, "id", "left", require="m:1", best=t_join_n, n_out=n_out)
            call report_pair("join left m:1", n, t_join_s, t_join_n, p_join_s, p_join_n)
            ! The fill verbs' fixtures: a 16-byte string column and a float64 column, both with a
            ! value in every fourth row and a null elsewhere. The string one is built as a store
            ! so that its nulls are appended rather than written afterwards.
            allocate(tx(n), tvalid(n))
            call sc%clear()
            call sc%reserve(n, n * 16_int64)
            do i = 1_int64, n
                tx(i) = real(i, real64)
                tvalid(i) = mod(i, 4_int64) == 0_int64
                if (tvalid(i)) then
                    write (one, "(a,i11.11)") "fill_", i
                    call sc%append_string(one)
                else
                    call sc%append_null()
                end if
            end do
            call parquet_new_table(t_str)
            call t_str%add_column("s", sc)
            call parquet_new_table(t_num)
            call t_num%add_column("x", tx)
            call t_num%set("x", tx, is_valid=tvalid)
            call time_fill(t_str, "s", "fillna", .true., t_fill_s)
            call time_fill(t_num, "x", "fillna", .false., t_fill_n)
            call report_pair("%fillna", n, t_fill_s, t_fill_n, p_fill_s, p_fill_n)
            call time_fill(t_str, "s", "ffill", .true., t_ff_s)
            call time_fill(t_num, "x", "ffill", .false., t_ff_n)
            call report_pair("%ffill", n, t_ff_s, t_ff_n, p_ff_s, p_ff_n)
            checksum = checksum + n_out
            deallocate(lk, tx, tvalid)
        end do
        write (output_unit, "(a)") ""
    end subroutine mode_nullfill

end program benchmark_join
