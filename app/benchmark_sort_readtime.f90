!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Where a READ-TIME sort spends its time: `parquet_open_reader(..., sort_by=)`, phase by phase.
!!
!! Written for `feature_sort.md`'s **P13**, whose first step is a measurement rather than a design.
!! After R1 routed the permutation build to the Fortran engine, the phase shares that item quotes
!! were *derived* -- taken before R1 and rescaled by the measured engine ratio -- and the whole
!! point of this program is to replace them with measured ones.
!!
!! **The phases, and where each is counted.** Five come from C++ counters
!! (`parquet_debug_get_sort_*_nanos`, declared locally below rather than in
!! `src/parquet_bindings.f90`, which is this project's convention for a maintainer-only hook):
!!
!!   * `info` -- `parquet_reader_sort_key_info` binding the key to report its size;
!!   * `bind` -- `parquet_reader_sort_key_fetch` binding **the same key again**;
!!   * `copy` -- handing that reduction to Fortran-owned buffers;
!!   * `perm` -- building the `arrow::Int64Array` from the permutation;
!!   * `take` -- `arrow::compute::Take` over every cached column. This is P13's subject.
!!
!! The sixth, `engine`, is what is left of the wall-clock open once those five are subtracted: the
!! Fortran `pf_argsort` plus the open itself. It is DERIVED, and is labelled as such in the output,
!! because timing it directly would mean a Fortran-side hook on a public module.
!!
!! **Read `take` against the column count in each heading, and note what bounds it.** The Take loop
!! runs once per column already in the reader's cache. A sort may only be installed BEFORE any
!! column is read -- `parquet_reader_set_sort` aborts otherwise, and `parquet_open_reader(sort_by=)`
!! installs during the open -- so the cache at that moment holds the sort's own key columns and
!! nothing else. **The Take loop therefore scales with the number of KEYS, never with how many
!! columns the caller goes on to read.** `--keys=2` is the arm that shows it; there is deliberately
!! no prefetch arm, because prefetch-then-sort is not a reachable shape.
!!
!! Usage (via `tools/benchmark_sort_readtime.sh`, which sets `--profile release`):
!!
!!   fpm run benchmark_sort_readtime --profile release -- --n=1000000 --reps=5
!!   fpm run benchmark_sort_readtime --profile release -- --n=1000000 --key=string --keys=2
program benchmark_sort_readtime
    use parquet
    use iso_fortran_env, only : int32, int64, real64, output_unit
    use iso_c_binding, only : c_long_long
    implicit none

    ! ---- The maintainer-only phase counters, declared here and nowhere else ----
    interface
        subroutine parquet_debug_reset_sort_phase_nanos() bind(C, name="parquet_debug_reset_sort_phase_nanos")
        end subroutine parquet_debug_reset_sort_phase_nanos
        function parquet_debug_get_sort_info_nanos() result(n) bind(C, name="parquet_debug_get_sort_info_nanos")
            import :: c_long_long
            integer(c_long_long) :: n
        end function parquet_debug_get_sort_info_nanos
        function parquet_debug_get_sort_bind_nanos() result(n) bind(C, name="parquet_debug_get_sort_bind_nanos")
            import :: c_long_long
            integer(c_long_long) :: n
        end function parquet_debug_get_sort_bind_nanos
        function parquet_debug_get_sort_copy_nanos() result(n) bind(C, name="parquet_debug_get_sort_copy_nanos")
            import :: c_long_long
            integer(c_long_long) :: n
        end function parquet_debug_get_sort_copy_nanos
        function parquet_debug_get_sort_perm_nanos() result(n) bind(C, name="parquet_debug_get_sort_perm_nanos")
            import :: c_long_long
            integer(c_long_long) :: n
        end function parquet_debug_get_sort_perm_nanos
        function parquet_debug_get_sort_take_nanos() result(n) bind(C, name="parquet_debug_get_sort_take_nanos")
            import :: c_long_long
            integer(c_long_long) :: n
        end function parquet_debug_get_sort_take_nanos
        function parquet_debug_get_sort_take_columns() result(n) bind(C, name="parquet_debug_get_sort_take_columns")
            import :: c_long_long
            integer(c_long_long) :: n
        end function parquet_debug_get_sort_take_columns
    end interface

    integer(int64) :: n
    integer :: reps, nkeys
    character(len=32) :: key_mode
    character(len=256) :: file_name
    logical :: keep

    call read_args(n, reps, key_mode, nkeys, file_name, keep)
    call banner(n, reps, key_mode, nkeys, file_name)
    call write_fixture(trim(file_name), n)
    if (key_mode == "int64" .or. key_mode == "both") call measure(trim(file_name), "id", reps, nkeys, n)
    if (key_mode == "string" .or. key_mode == "both") call measure(trim(file_name), "name", reps, nkeys, n)
    if (.not. keep) call delete_file(trim(file_name))

contains

    !> Times `reps` opens with `sort_by=<key>` and reports the best one's phase split.
    !!
    !! **Best-of-N on the TOTAL, with that run's own phases**, never a per-phase minimum: phase
    !! minima taken from different runs do not sum to any run that happened, so their shares would
    !! be of a total nothing measured.
    subroutine measure(file_name, key, reps, nkeys, n)
        character(len=*), intent(in) :: file_name !! the fixture to read.
        character(len=*), intent(in) :: key       !! leading column to sort by.
        integer, intent(in) :: reps               !! timed repetitions; the best total is kept.
        integer, intent(in) :: nkeys              !! 1, or 2 to add a second key ("x1 asc").
        integer(int64), intent(in) :: n           !! rows, for the per-element figures.
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        real(real64) :: t0, t1, best
        integer(c_long_long) :: info, bind_, copy, perm, take, cols
        integer(c_long_long) :: b_info, b_bind, b_copy, b_perm, b_take, b_cols
        real(real64) :: derived
        integer :: r
        !
        best = huge(1.0_real64)
        b_info = 0; b_bind = 0; b_copy = 0; b_perm = 0; b_take = 0; b_cols = 0
        ! Built once: `parquet_sortkey` has `%add` and no `%clear`, and the key does not change
        ! between repetitions anyway.
        call srt%add(key // " asc")
        ! The second key is a payload column, so it is decoded ONLY because the sort names it --
        ! which is the whole point: it doubles the Take loop's work without the caller reading it.
        if (nkeys >= 2) call srt%add("x1 asc")
        do r = 1, reps
            call parquet_debug_reset_sort_phase_nanos()
            call cpu_wall(t0)
            call parquet_open_reader(reader, file_name, sort_by=srt)
            call cpu_wall(t1)
            info = parquet_debug_get_sort_info_nanos()
            bind_ = parquet_debug_get_sort_bind_nanos()
            copy = parquet_debug_get_sort_copy_nanos()
            perm = parquet_debug_get_sort_perm_nanos()
            take = parquet_debug_get_sort_take_nanos()
            cols = parquet_debug_get_sort_take_columns()
            call parquet_close_reader(reader)
            if (t1 - t0 < best) then
                best = t1 - t0
                b_info = info; b_bind = bind_; b_copy = copy; b_perm = perm; b_take = take; b_cols = cols
            end if
        end do
        !
        ! Everything the C++ counters did not claim: pf_argsort, the open itself, and the buffer
        ! allocation between them. Named "engine+open" rather than "engine" so nobody quotes it as
        ! a sort-engine figure -- `tools/benchmark_sort_engine.sh` is what measures that.
        derived = best * 1.0e9_real64 - real(b_info + b_bind + b_copy + b_perm + b_take, real64)
        write (output_unit, '(a)') ""
        write (output_unit, '(a,a,a,i0,a)') "## sort_by=""", key, " asc""   (", b_cols, " column(s) taken)"
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "  phase          ms      ns/row   share    what it is"
        call row("info", real(b_info, real64), best, n, "sort_key_info: bind the key to size it")
        call row("bind", real(b_bind, real64), best, n, "sort_key_fetch: bind the SAME key again")
        call row("copy", real(b_copy, real64), best, n, "hand the reduction to Fortran buffers")
        call row("engine+open", derived, best, n, "DERIVED: pf_argsort + the open itself")
        call row("perm", real(b_perm, real64), best, n, "build the arrow::Int64Array")
        call row("take", real(b_take, real64), best, n, "arrow::compute::Take over the cache")
        write (output_unit, '(a)') "  -----------------------------------------------------------------------"
        call row("TOTAL", best * 1.0e9_real64, best, n, "wall clock for the whole open")
    end subroutine measure

    !> One line of the phase table.
    subroutine row(label, nanos, total_s, n, what)
        character(len=*), intent(in) :: label   !! phase name.
        real(real64), intent(in) :: nanos       !! the phase's cost, in nanoseconds.
        real(real64), intent(in) :: total_s     !! the whole open, in seconds.
        integer(int64), intent(in) :: n         !! rows.
        character(len=*), intent(in) :: what    !! one-line description.
        real(real64) :: share
        !
        share = 0.0_real64
        if (total_s > 0.0_real64) share = 100.0_real64 * nanos / (total_s * 1.0e9_real64)
        write (output_unit, '(a,a12,f10.2,f12.2,f8.1,a,a)') "  ", label, nanos * 1.0e-6_real64, &
            nanos / real(max(n, 1_int64), real64), share, "%   ", what
    end subroutine row

    !> Writes the fixture: an int64 key, a string key and four `real64` payload columns.
    !!
    !! Values are deliberately scattered rather than ordered -- an already-sorted key would let the
    !! engine's own paths finish early and would measure a case nobody has.
    subroutine write_fixture(file_name, n)
        character(len=*), intent(in) :: file_name !! where to write.
        integer(int64), intent(in) :: n           !! rows.
        type(parquet_writer) :: w
        type(parquet_schema) :: s
        integer(int64), allocatable :: id(:)
        real(real64), allocatable :: x(:)
        character(len=16), allocatable :: nm(:)
        character(len=8) :: cname
        integer(int64) :: i
        integer :: k
        logical :: there
        !
        inquire (file=file_name, exist=there)
        if (there) then
            write (output_unit, '(a)') "  fixture     : reusing the existing file"
            return
        end if
        allocate(id(n), x(n), nm(n))
        do i = 1_int64, n
            id(i) = mod(i * 2654435761_int64, 1000000007_int64)
            x(i) = real(id(i), real64) * 1.0e-9_real64
            write (nm(i), '(a,i12.12)') "s_", mod(i * 7919_int64, 999983_int64)
        end do
        call s%init("bench_sort_readtime")
        call s%add_field("id", "int64")
        ! `array_size` is the declared width; without it the write refuses a 14-character value.
        call s%add_field("name", "string", array_size=16)
        do k = 1, 4
            write (cname, '(a,i0)') "x", k
            call s%add_field(trim(cname), "float64")
        end do
        call parquet_parse_maml(s)
        call parquet_open_writer(w, file_name, s)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "name", nm)
        do k = 1, 4
            write (cname, '(a,i0)') "x", k
            call parquet_write_column(w, trim(cname), x)
        end do
        call parquet_close_writer(w)
        write (output_unit, '(a)') "  fixture     : written"
    end subroutine write_fixture

    !> Wall clock, in seconds. `cpu_time` measures CPU rather than elapsed time and would report a
    !! threaded phase as the SUM over its threads, which is the wrong quantity for every share here.
    subroutine cpu_wall(t)
        real(real64), intent(out) :: t !! seconds, from an arbitrary origin.
        integer(int64) :: c, r
        !
        call system_clock(count=c, count_rate=r)
        t = real(c, real64) / real(r, real64)
    end subroutine cpu_wall

    !> Removes the fixture unless `--keep` was given.
    subroutine delete_file(file_name)
        character(len=*), intent(in) :: file_name !! the file to remove.
        integer :: u
        logical :: there
        !
        inquire (file=file_name, exist=there)
        if (.not. there) return
        open (newunit=u, file=file_name, status="old")
        close (u, status="delete")
    end subroutine delete_file

    !> Prints what this run is, before it costs anything -- provenance first, per CLAUDE.md's
    !! benchmarking rules.
    subroutine banner(n, reps, key_mode, nkeys, file_name)
        integer(int64), intent(in) :: n            !! rows.
        integer, intent(in) :: reps                !! repetitions.
        character(len=*), intent(in) :: key_mode   !! which keys are measured.
        integer, intent(in) :: nkeys               !! sort keys per run.
        character(len=*), intent(in) :: file_name  !! the fixture.
        !
        write (output_unit, '(a)') "=============================================================="
        write (output_unit, '(a)') "benchmark_sort_readtime -- where a read-time sort spends time"
        write (output_unit, '(a)') "=============================================================="
        write (output_unit, '(a,i0,a,i0)') "  n = ", n, "   reps = ", reps
        write (output_unit, '(a,a,a,i0)') "  key type    : ", trim(key_mode), "   sort keys per run: ", nkeys
        write (output_unit, '(a,a)') "  fixture     : ", trim(file_name)
        write (output_unit, '(a)') "  'engine+open' is DERIVED (total minus the five C++ phases)."
    end subroutine banner

    !> `--key=` and friends. Every setting has a default, so a bare run is meaningful.
    subroutine read_args(n, reps, key_mode, nkeys, file_name, keep)
        integer(int64), intent(out) :: n             !! rows.
        integer, intent(out) :: reps                 !! repetitions.
        character(len=*), intent(out) :: key_mode    !! int64 / string / both.
        integer, intent(out) :: nkeys                !! sort keys per run: 1 or 2.
        character(len=*), intent(out) :: file_name   !! the fixture path.
        logical, intent(out) :: keep                 !! .true. leaves the fixture behind.
        character(len=256) :: a
        integer :: i
        !
        n = 1000000_int64
        reps = 5
        key_mode = "both"
        nkeys = 1
        file_name = "test_run/bench_sort_readtime.parquet"
        keep = .false.
        do i = 1, command_argument_count()
            call get_command_argument(i, a)
            if (a(1:4) == "--n=") then
                read (a(5:), *) n
            else if (a(1:7) == "--reps=") then
                read (a(8:), *) reps
            else if (a(1:6) == "--key=") then
                key_mode = a(7:)
            else if (a(1:7) == "--keys=") then
                read (a(8:), *) nkeys
            else if (a(1:7) == "--file=") then
                file_name = a(8:)
            else if (a(1:6) == "--keep") then
                keep = .true.
            end if
        end do
    end subroutine read_args

end program benchmark_sort_readtime
