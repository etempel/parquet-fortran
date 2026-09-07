!> The parquet-fortran arm of the cross-library join comparison: reads two parquet files, runs one
!> `parquet_table%join`, and writes the result back out so another library can check it row for row.
!!
!! Driven by `bench/benchmark_join_crosslib.sh`, which builds it `--profile release`, generates the
!! fixtures, runs the pandas / astropy / STILTS arms over the SAME files, and does the comparing.
!! `bench/benchmark_join.f90` is the different question of where the time goes INSIDE the join;
!! this program treats `%join` as a black box and exists to be compared against other software.
!!
!! **The three phases are timed separately and every arm of the comparison splits them the same
!! way**, because the join is a small share of a pipeline that reads and writes parquet, and a
!! single end-to-end figure would mostly measure Arrow against pyarrow against parquet-mr:
!!
!!   * `read`  -- `parquet_open_table` + `%materialize_all` on both inputs. Every column is brought
!!                into memory first, which is what makes the join figure comparable to a pandas or
!!                astropy join over frames that are already resident. It also deliberately gives up
!!                this library's laziness, which `--mode=lazy` measures instead.
!!   * `join`  -- `%join` alone, best of `--rounds`. Each round joins a fresh `%clone` taken
!!                OUTSIDE the timer, because `%join` mutates its left table.
!!   * `write` -- `parquet_write_table` of the result.
!!
!! `--mode=lazy` is the arm no other library has: it opens both tables, materializes only what the
!! join needs on the right, and lets the m:1 left join carry the left table's unread columns across
!! without reading them. Compared against `--mode=eager` on the same files it is what a lookup-table
!! enrichment actually costs.
!!
!! **Rules from CLAUDE.md's benchmarking section that this program follows.** Best of N rounds, the
!! run least disturbed by everything else on the machine; the fixture is constant across rounds
!! because each round clones the same materialized table; a checksum over every result is printed so
!! no arm can be optimised away; and the output is written once, after the timed rounds, so no
!! figure includes it twice.
!!
!! `--engine=auto|sort|hash` forces the join's pair-list engine through the test-only hook
!! `parquet_debug_set_join_engine` (default `auto`, the library's own choice), and the
!! `engine_used` fact reports which engine the timed join actually ran on -- what lets the
!! accuracy harness rerun its cases under each engine and check the forced one took the call.
!!
!! Output is one `key=value` line per fact on stdout, prefixed `RESULT`, for the wrapper to parse.
program benchmark_join_crosslib
    use parquet
    use iso_fortran_env, only: int64, real64, output_unit
#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime, omp_get_max_threads
#endif
    implicit none

    character(len=:), allocatable :: left_file, right_file, out_file
    character(len=:), allocatable :: on_key, other_on_key, how, columns, require_tok
    character(len=:), allocatable :: order_tok, suffix_tok, mode, left_keep, engine
    integer :: rounds, threads_req
    integer(int64), allocatable :: max_rows_req
    logical :: have_other_on, have_columns, have_require, have_order, have_suffix, have_keep
    logical :: have_max_rows

    call read_arguments()
    call apply_engine()

    select case (mode)
    case ("eager")
        call run_eager()
    case ("lazy")
        call run_lazy()
    case default
        write (output_unit, "(a)") "benchmark_join_crosslib: unknown --mode=" // mode
        stop 2
    end select

contains

    !> Reads the `--key=value` flags the shell wrapper forwards.
    subroutine read_arguments()
        character(len=1024) :: arg
        integer :: i, n

        left_file = ""
        right_file = ""
        out_file = ""
        on_key = "id"
        other_on_key = ""
        how = "inner"
        columns = ""
        require_tok = ""
        order_tok = ""
        suffix_tok = ""
        left_keep = ""
        mode = "eager"
        engine = "auto"
        rounds = 3
        threads_req = 0
        have_max_rows = .false.
        have_other_on = .false.
        have_columns = .false.
        have_require = .false.
        have_order = .false.
        have_suffix = .false.
        have_keep = .false.

        n = command_argument_count()
        do i = 1, n
            call get_command_argument(i, arg)
            if (index(arg, "--left=") == 1) then
                left_file = trim(arg(8:))
            else if (index(arg, "--right=") == 1) then
                right_file = trim(arg(9:))
            else if (index(arg, "--out=") == 1) then
                out_file = trim(arg(7:))
            else if (index(arg, "--on=") == 1) then
                on_key = trim(arg(6:))
            else if (index(arg, "--other-on=") == 1) then
                other_on_key = trim(arg(12:))
                have_other_on = len_trim(other_on_key) > 0
            else if (index(arg, "--how=") == 1) then
                how = trim(arg(7:))
            else if (index(arg, "--columns=") == 1) then
                columns = trim(arg(11:))
                have_columns = len_trim(columns) > 0
            else if (index(arg, "--require=") == 1) then
                require_tok = trim(arg(11:))
                have_require = len_trim(require_tok) > 0
            else if (index(arg, "--order=") == 1) then
                order_tok = trim(arg(9:))
                have_order = len_trim(order_tok) > 0
            else if (index(arg, "--other-suffix=") == 1) then
                suffix_tok = trim(arg(16:))
                have_suffix = len_trim(suffix_tok) > 0
            else if (index(arg, "--left-keep=") == 1) then
                left_keep = trim(arg(13:))
                have_keep = len_trim(left_keep) > 0
            else if (index(arg, "--mode=") == 1) then
                mode = trim(arg(8:))
            else if (index(arg, "--engine=") == 1) then
                engine = trim(arg(10:))
            else if (index(arg, "--rounds=") == 1) then
                read (arg(10:), *) rounds
            else if (index(arg, "--threads=") == 1) then
                read (arg(11:), *) threads_req
            else if (index(arg, "--max-rows=") == 1) then
                allocate(max_rows_req)
                read (arg(12:), *) max_rows_req
                have_max_rows = .true.
            else
                write (output_unit, "(a)") "benchmark_join_crosslib: unknown argument " // trim(arg)
                stop 2
            end if
        end do
        if (len_trim(left_file) == 0 .or. len_trim(right_file) == 0) then
            write (output_unit, "(a)") "benchmark_join_crosslib: --left= and --right= are required"
            stop 2
        end if
    end subroutine read_arguments

    !> Forces the join's pair-list engine through the test-only hook, per `--engine=`.
    !!
    !! A local `bind(C)` interface, as every `parquet_debug_*` hook is reached: the hook stays
    !! out of `src/parquet_bindings.f90` and out of the library's own interface.
    subroutine apply_engine()
        use iso_c_binding, only : c_int64_t
        interface
            subroutine set_join_engine(mode) bind(C, name="parquet_debug_set_join_engine")
                import :: c_int64_t
                integer(c_int64_t), value :: mode !! 0 automatic, 1 the sort engine, 2 the hash engine.
            end subroutine set_join_engine
        end interface

        select case (engine)
        case ("auto")
            call set_join_engine(0_c_int64_t)
        case ("sort")
            call set_join_engine(1_c_int64_t)
        case ("hash")
            call set_join_engine(2_c_int64_t)
        case default
            write (output_unit, "(a)") "benchmark_join_crosslib: unknown --engine=" // engine
            stop 2
        end select
    end subroutine apply_engine

    !> The engine the last join ran on (1 the sort engine, 2 the hash engine), from the hook's
    !! observable.
    function engine_used() result(e)
        use iso_c_binding, only : c_int64_t
        integer(int64) :: e !! 1 or 2.
        interface
            function join_engine_used() result(res) bind(C, name="parquet_debug_join_engine_used")
                import :: c_int64_t
                integer(c_int64_t) :: res !! the engine token.
            end function join_engine_used
        end interface

        e = int(join_engine_used(), int64)
    end function engine_used

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

    !> Prints one `RESULT key=value` line for the wrapper to parse.
    subroutine emit(key, value)
        character(len=*), intent(in) :: key   !! the fact's name.
        character(len=*), intent(in) :: value !! its value, already rendered.

        write (output_unit, "(a)") "RESULT " // key // "=" // value
        flush (output_unit)
    end subroutine emit

    !> Prints a seconds figure to nanosecond resolution.
    subroutine emit_time(key, seconds)
        character(len=*), intent(in) :: key   !! the fact's name.
        real(real64), intent(in) :: seconds   !! elapsed seconds.
        character(len=32) :: buf

        write (buf, "(f20.9)") seconds
        call emit(key, trim(adjustl(buf)))
    end subroutine emit_time

    !> Prints an integer figure.
    subroutine emit_int(key, value)
        character(len=*), intent(in) :: key   !! the fact's name.
        integer(int64), intent(in) :: value   !! the count.
        character(len=32) :: buf

        write (buf, "(i0)") value
        call emit(key, trim(buf))
    end subroutine emit_int

    !> Runs `%join` once on `w`, forwarding only the optional arguments the caller actually gave.
    !!
    !! Fortran has no way to forward an absent optional through a variable, and `%join` distinguishes
    !! absent from empty on every one of these, so the presence flags are branched on explicitly
    !! rather than passed as empty strings.
    subroutine do_join(w, other, matched, pairs, other_pairs)
        type(parquet_table), intent(inout) :: w      !! the left table, joined in place.
        type(parquet_table), intent(in) :: other     !! the right table.
        logical, allocatable, intent(out), optional :: matched(:) !! per left row, as it was on entry.
        integer(int64), allocatable, intent(out), optional :: pairs(:)       !! per output row.
        integer(int64), allocatable, intent(out), optional :: other_pairs(:) !! per output row.

        if (have_other_on) then
            if (have_columns) then
                call join_full(w, other, other_on_key, columns, matched, pairs, other_pairs)
            else
                call join_nocols(w, other, other_on_key, matched, pairs, other_pairs)
            end if
        else
            if (have_columns) then
                call join_cols_only(w, other, columns, matched, pairs, other_pairs)
            else
                call join_plain(w, other, matched, pairs, other_pairs)
            end if
        end if
    end subroutine do_join

    !> `%join` with both `other_on=` and `columns=`.
    subroutine join_full(w, other, oon, cols, matched, pairs, other_pairs)
        type(parquet_table), intent(inout) :: w  !! the left table.
        type(parquet_table), intent(in) :: other !! the right table.
        character(len=*), intent(in) :: oon      !! the right table's key names.
        character(len=*), intent(in) :: cols     !! the payload list.
        logical, allocatable, intent(out), optional :: matched(:) !! per left row on entry.
        integer(int64), allocatable, intent(out), optional :: pairs(:)       !! per output row.
        integer(int64), allocatable, intent(out), optional :: other_pairs(:) !! per output row.

        if (have_require .and. have_order .and. have_suffix) then
            call w%join(other, on=on_key, other_on=oon, how=how, columns=cols, &
                require=require_tok, order=order_tok, other_suffix=suffix_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_require .and. have_order) then
            call w%join(other, on=on_key, other_on=oon, how=how, columns=cols, &
                require=require_tok, order=order_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_require) then
            call w%join(other, on=on_key, other_on=oon, how=how, columns=cols, &
                require=require_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_order) then
            call w%join(other, on=on_key, other_on=oon, how=how, columns=cols, &
                order=order_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_suffix) then
            call w%join(other, on=on_key, other_on=oon, how=how, columns=cols, &
                other_suffix=suffix_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else
            call w%join(other, on=on_key, other_on=oon, how=how, columns=cols, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        end if
    end subroutine join_full

    !> `%join` with `other_on=` and no `columns=`.
    subroutine join_nocols(w, other, oon, matched, pairs, other_pairs)
        type(parquet_table), intent(inout) :: w  !! the left table.
        type(parquet_table), intent(in) :: other !! the right table.
        character(len=*), intent(in) :: oon      !! the right table's key names.
        logical, allocatable, intent(out), optional :: matched(:) !! per left row on entry.
        integer(int64), allocatable, intent(out), optional :: pairs(:)       !! per output row.
        integer(int64), allocatable, intent(out), optional :: other_pairs(:) !! per output row.

        if (have_require .and. have_order) then
            call w%join(other, on=on_key, other_on=oon, how=how, require=require_tok, &
                order=order_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_require) then
            call w%join(other, on=on_key, other_on=oon, how=how, require=require_tok, &
                matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_order) then
            call w%join(other, on=on_key, other_on=oon, how=how, order=order_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_suffix) then
            call w%join(other, on=on_key, other_on=oon, how=how, other_suffix=suffix_tok, &
                matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else
            call w%join(other, on=on_key, other_on=oon, how=how, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        end if
    end subroutine join_nocols

    !> `%join` with `columns=` and a shared key name.
    subroutine join_cols_only(w, other, cols, matched, pairs, other_pairs)
        type(parquet_table), intent(inout) :: w  !! the left table.
        type(parquet_table), intent(in) :: other !! the right table.
        character(len=*), intent(in) :: cols     !! the payload list.
        logical, allocatable, intent(out), optional :: matched(:) !! per left row on entry.
        integer(int64), allocatable, intent(out), optional :: pairs(:)       !! per output row.
        integer(int64), allocatable, intent(out), optional :: other_pairs(:) !! per output row.

        if (have_require .and. have_order) then
            call w%join(other, on=on_key, how=how, columns=cols, require=require_tok, &
                order=order_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_require) then
            call w%join(other, on=on_key, how=how, columns=cols, require=require_tok, &
                matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_order) then
            call w%join(other, on=on_key, how=how, columns=cols, order=order_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_suffix) then
            call w%join(other, on=on_key, how=how, columns=cols, other_suffix=suffix_tok, &
                matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else
            call w%join(other, on=on_key, how=how, columns=cols, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        end if
    end subroutine join_cols_only

    !> `%join` with a shared key name and no `columns=`.
    subroutine join_plain(w, other, matched, pairs, other_pairs)
        type(parquet_table), intent(inout) :: w  !! the left table.
        type(parquet_table), intent(in) :: other !! the right table.
        logical, allocatable, intent(out), optional :: matched(:) !! per left row on entry.
        integer(int64), allocatable, intent(out), optional :: pairs(:)       !! per output row.
        integer(int64), allocatable, intent(out), optional :: other_pairs(:) !! per output row.

        if (have_require .and. have_order) then
            call w%join(other, on=on_key, how=how, require=require_tok, order=order_tok, &
                matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_require) then
            call w%join(other, on=on_key, how=how, require=require_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_order) then
            call w%join(other, on=on_key, how=how, order=order_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else if (have_suffix) then
            call w%join(other, on=on_key, how=how, other_suffix=suffix_tok, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        else
            call w%join(other, on=on_key, how=how, matched=matched, &
                pairs=pairs, other_pairs=other_pairs, max_rows=max_rows_req)
        end if
    end subroutine join_plain

    !> Both tables fully in memory before the join: the arm comparable to pandas and astropy.
    subroutine run_eager()
        type(parquet_table) :: a0, a, b
        logical, allocatable :: matched(:)
        integer(int64), allocatable :: pairs(:), other_pairs(:)
        real(real64) :: t0, t1, t_read, t_join, t_write, t_clone
        integer :: r

        t0 = now()
        call parquet_open_table(a0, left_file)
        call a0%materialize_all()
        call parquet_open_table(b, right_file)
        call b%materialize_all()
        t1 = now()
        t_read = t1 - t0

        call emit_int("left_rows", a0%nrows())
        call emit_int("left_cols", int(a0%ncols(), int64))
        call emit_int("right_rows", b%nrows())
        call emit_int("right_cols", int(b%ncols(), int64))

        t_join = huge(1.0_real64)
        t_clone = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call a0%clone(a)
            t1 = now()
            t_clone = min(t_clone, t1 - t0)
            t0 = now()
            call do_join(a, b, matched, pairs, other_pairs)
            t1 = now()
            t_join = min(t_join, t1 - t0)
        end do
        ! The pair arrays are the join's own account of what it matched, in the coordinate system
        ! of the rows each table had ON ENTRY. Reporting their length and their zero counts is what
        ! lets the claims stage check the documented rules -- pairs is never 0 under inner/left,
        ! other_pairs never 0 under inner/right, and other_pairs all 0 under semi/anti.
        call emit_int("pairs_len", size(pairs, kind=int64))
        call emit_int("pairs_zero", count(pairs == 0_int64, kind=int64))
        call emit_int("other_pairs_len", size(other_pairs, kind=int64))
        call emit_int("other_pairs_zero", count(other_pairs == 0_int64, kind=int64))
        call emit_int("pairs_max", maxval(pairs))
        call emit_int("other_pairs_max", maxval(other_pairs))

        call emit_int("out_rows", a%nrows())
        call emit_int("out_cols", int(a%ncols(), int64))
        call emit_int("matched", count(matched, kind=int64))
        call emit_int("matched_of", size(matched, kind=int64))
        if (a%is_detached()) then
            call emit("detached", "1")
        else
            call emit("detached", "0")
        end if
        call emit("engine", engine)
        call emit_int("engine_used", engine_used())

        t_write = 0.0_real64
        if (len_trim(out_file) > 0) then
            t0 = now()
            call parquet_write_table(a, out_file, overwrite=.true.)
            t1 = now()
            t_write = t1 - t0
        end if

        call emit_time("read_s", t_read)
        call emit_time("clone_s", t_clone)
        call emit_time("join_s", t_join)
        call emit_time("write_s", t_write)
        call emit_time("total_s", t_read + t_join + t_write)
        call emit_int("rounds", int(rounds, int64))
    end subroutine run_eager

    !> The lazy arm no other library has: the left table reads only what it is told to.
    !!
    !! Only `--left-keep=` (plus the key) is materialized on this side, and on an m:1 join every
    !! column the left table never read stays readable afterwards -- so `write` here is what
    !! actually pays for those columns, and `read` is a fraction of the eager arm's.
    subroutine run_lazy()
        type(parquet_table) :: a, b
        logical, allocatable :: matched(:)
        real(real64) :: t0, t1, t_read, t_join, t_write

        t0 = now()
        call parquet_open_table(a, left_file)
        if (have_keep) call a%materialize(left_keep)
        call parquet_open_table(b, right_file)
        call b%materialize_all()
        t1 = now()
        t_read = t1 - t0

        call emit_int("left_rows", a%nrows())
        call emit_int("left_cols", int(a%ncols(), int64))
        call emit_int("right_rows", b%nrows())
        call emit_int("right_cols", int(b%ncols(), int64))

        t0 = now()
        call do_join(a, b, matched)
        t1 = now()
        t_join = t1 - t0

        call emit_int("out_rows", a%nrows())
        call emit_int("out_cols", int(a%ncols(), int64))
        call emit_int("matched", count(matched, kind=int64))
        call emit_int("matched_of", size(matched, kind=int64))
        if (a%is_detached()) then
            call emit("detached", "1")
        else
            call emit("detached", "0")
        end if
        call emit("engine", engine)
        call emit_int("engine_used", engine_used())

        t_write = 0.0_real64
        if (len_trim(out_file) > 0) then
            t0 = now()
            call parquet_write_table(a, out_file, overwrite=.true.)
            t1 = now()
            t_write = t1 - t0
        end if

        call emit_time("read_s", t_read)
        call emit_time("clone_s", 0.0_real64)
        call emit_time("join_s", t_join)
        call emit_time("write_s", t_write)
        call emit_time("total_s", t_read + t_join + t_write)
        call emit_int("rounds", 1_int64)
    end subroutine run_lazy

end program benchmark_join_crosslib
