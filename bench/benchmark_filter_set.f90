!> Benchmarks the set-valued filter clause `id in @set`: what the leaf's pre-evaluation costs
!> against the row-group decode it precedes, on a file sorted by the key (most row groups prune)
!> and on one shuffled (none prune), through the reader and through the whole-table and
!> `bounded=.true.` table reads, for a dense and a sparse set; and, in isolation, the map's three
!> backends over the same sets.
!!
!! Driven by `bench/benchmark_filter_set.sh`, which is where the environment variables and the
!! `--profile release` assertions live. Modes:
!!
!! * `leaf` -- per (file, set): the bind (the set's dedup through `%get_or_add_many`); the filter
!!             install (one `parquet_open_reader` with the filter, which runs the leaf's
!!             pre-evaluation -- the key column one row group at a time, the map's `%get_many`
!!             per row group, the verdicts, the screen); the decode of one payload column through
!!             that reader, pruned row groups skipped; the same open and decode with no filter,
!!             which is the baseline the leaf's cost is read against; and the two table reads,
!!             whole and `bounded=.true.`, open plus one payload column. The pruned row-group
!!             count is printed beside each install.
!! * `map`  -- the same sets as `pf_index_map`s, each backend forced, built serial and on the
!!             team, and probed with every row's id (`%get_many`, serial and on the team): the
!!             part of the leaf that is `parquet_index`'s, in isolation.
!!
!! The fixture is written by this program -- the `id` column and four `float64` payload columns,
!! `CHUNK` rows per row group -- as two files, one with `id = 1 .. NROWS` in order and one with
!! the same ids in a fixed random order (`pf_random_permutation` under a fixed seed), and both are
!! removed at the end unless `KEEP=1`.
!! The dense set is `1 .. NSET`, which the sorted file holds in its first row groups and the map's
!! automatic choice takes to the direct backend; the sparse set is every `NROWS / NSET`-th id,
!! which every row group of either file holds some of and which takes the hash backend.
!!
!! **Rules this program follows, from CLAUDE.md's benchmarking section.** Every array is written
!! once before any timed loop; each figure is the best of `ROUNDS` rounds; the fixture files are
!! written before anything is timed and each filtered read is preceded by an unfiltered one of the
!! same file, so the page cache holds both files throughout; and a checksum is accumulated and
!! printed so no arm can be optimised away.
program benchmark_filter_set
    use parquet
    use parquet_index, only: pf_index_map, pf_index_threads
    use parquet_sampling, only: pf_random_permutation
    use iso_fortran_env, only: int32, int64, real64, output_unit
    use iso_c_binding, only: c_long_long
#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime, omp_get_max_threads
#endif
    implicit none

    interface
        !> How many row groups the most recent screen in this process ruled out: the C++ debug
        !! hook the filter tests read, declared locally as every test does.
        function parquet_debug_get_row_groups_pruned() result(res) &
            bind(C, name="parquet_debug_get_row_groups_pruned")
            import :: c_long_long
            integer(c_long_long) :: res !! pruned row-group count of the last screen.
        end function parquet_debug_get_row_groups_pruned
    end interface

    integer(int64) :: nrows, nset, chunk
    integer :: rounds, threads_req
    character(len=:), allocatable :: mode, file_base, file_sorted, file_shuffled
    logical :: keep
    integer(int64) :: checksum

    checksum = 0_int64
    call read_arguments()
    file_sorted = file_base // "_sorted.parquet"
    file_shuffled = file_base // "_shuffled.parquet"
    write (output_unit, "(a)") "# benchmark_filter_set -- the set-valued filter clause"
    write (output_unit, "(a,i0,a,i0,a,i0,a,i0)") "# nrows=", nrows, " nset=", nset, " chunk=", chunk, &
        " rounds=", rounds
    write (output_unit, "(a,i0,a,i0)") "# threads=", threads_req, " pf_index_threads(nrows)=", pf_index_threads(nrows)
#ifdef _OPENMP
    write (output_unit, "(a,i0)") "# omp_get_max_threads=", omp_get_max_threads()
#else
    write (output_unit, "(a)") "# built without OpenMP: every figure is serial"
#endif
    write (output_unit, "(a)") ""
    call write_fixture(file_sorted, .false.)
    call write_fixture(file_shuffled, .true.)
    select case (mode)
    case ("leaf")
        call mode_leaf()
    case ("map")
        call mode_map()
    case ("all")
        call mode_leaf()
        call mode_map()
    case default
        write (output_unit, "(a)") "unknown mode: " // mode
        stop 2
    end select
    if (.not. keep) then
        call remove_file(file_sorted)
        call remove_file(file_shuffled)
    end if
    write (output_unit, "(a)") ""
    write (output_unit, "(a,i0)") "# checksum ", checksum

contains

    !> Reads the `--key=value` flags the shell wrapper forwards.
    subroutine read_arguments()
        character(len=512) :: arg
        integer :: i, n

        nrows = 10000000_int64
        nset = 1000000_int64
        chunk = 500000_int64
        rounds = 3
        threads_req = 0
        mode = "all"
        file_base = "test_run/benchmark_filter_set"
        keep = .false.
        n = command_argument_count()
        do i = 1, n
            call get_command_argument(i, arg)
            if (index(arg, "--nrows=") == 1) then
                read (arg(9:), *) nrows
            else if (index(arg, "--nset=") == 1) then
                read (arg(8:), *) nset
            else if (index(arg, "--chunk=") == 1) then
                read (arg(9:), *) chunk
            else if (index(arg, "--rounds=") == 1) then
                read (arg(10:), *) rounds
            else if (index(arg, "--threads=") == 1) then
                read (arg(11:), *) threads_req
            else if (index(arg, "--mode=") == 1) then
                mode = trim(arg(8:))
            else if (index(arg, "--file=") == 1) then
                file_base = trim(arg(8:))
            else if (index(arg, "--keep=") == 1) then
                keep = trim(arg(8:)) == "1"
            end if
        end do
        if (nset > nrows) nset = nrows
        if (nset < 1_int64) nset = 1_int64
        if (chunk < 1_int64) chunk = nrows
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

    !> One figure: milliseconds, and nanoseconds per unit of work.
    subroutine report(label, best, ops)
        character(len=*), intent(in) :: label !! what was timed.
        real(real64), intent(in) :: best      !! best round, seconds.
        integer(int64), intent(in) :: ops     !! units of work the figure is per.

        write (output_unit, "(a,t58,f11.3,a,f10.3,a)") label, best * 1.0e3_real64, " ms", &
            best * 1.0e9_real64 / real(max(ops, 1_int64), real64), " ns/row"
    end subroutine report

    !> Deletes a file, quietly.
    subroutine remove_file(path)
        character(len=*), intent(in) :: path !! the file.
        integer :: u, ios

        open (newunit=u, file=path, status="old", iostat=ios)
        if (ios == 0) close (u, status="delete")
    end subroutine remove_file

    !> Writes one fixture: `id` and four float64 payload columns, `chunk` rows per row group.
    subroutine write_fixture(path, shuffled)
        character(len=*), intent(in) :: path !! the file to write.
        logical, intent(in) :: shuffled      !! whether the ids are in the fixed random order.
        type(parquet_writer) :: w
        integer(int64), allocatable :: id(:)
        real(real64), allocatable :: v(:)
        integer(int64) :: i
        integer :: c
        character(len=8) :: nm
        real(real64) :: t0

        t0 = now()
        allocate(id(nrows), v(nrows))
        if (shuffled) then
            call pf_random_permutation(id, 42_int64)
        else
            do i = 1_int64, nrows
                id(i) = i
            end do
        end if
        call parquet_open_writer(w, path, chunk_size=int(chunk, int32))
        call parquet_write_column(w, "id", id)
        do c = 1, 4
            do i = 1_int64, nrows
                v(i) = real(i, real64) * real(c, real64)
            end do
            write (nm, "(a,i0)") "p", c
            call parquet_write_column(w, trim(nm), v)
        end do
        call parquet_close_writer(w)
        write (output_unit, "(a,a,a,f8.2,a)") "# wrote ", path, " in ", now() - t0, " s"
    end subroutine write_fixture

    !> The set: `1 .. nset` (dense), or every `nrows / nset`-th id (sparse).
    subroutine make_set(sparse, ids)
        logical, intent(in) :: sparse                       !! which set.
        integer(int64), allocatable, intent(out) :: ids(:)  !! receives the members.
        integer(int64) :: j, step

        allocate(ids(nset))
        step = max(1_int64, nrows / nset)
        do j = 1_int64, nset
            if (sparse) then
                ids(j) = 1_int64 + (j - 1_int64) * step
            else
                ids(j) = j
            end if
        end do
    end subroutine make_set

    ! ---- leaf ----

    !> The end-to-end figures of the clause, per file and per set.
    subroutine mode_leaf()
        integer(int64), allocatable :: ids(:), idbuf(:)
        real(real64), allocatable :: p1(:)
        character(len=:), allocatable :: path
        character(len=8) :: fname(2), sname(2)
        integer(int64) :: nkept, nrg, pruned, rg, rows
        integer :: f, k, r
        real(real64) :: t0, best_bind, best_open, best_read, best_open0, best_read0, best_tbl, best_tblb
        real(real64) :: best_chunk

        write (output_unit, "(a)") "## leaf"
        fname = ["sorted  ", "shuffled"]
        sname = ["dense   ", "sparse  "]
        allocate(p1(nrows), idbuf(nrows))
        p1 = 0.0_real64
        idbuf = 0_int64
        do f = 1, 2
            if (f == 1) then
                path = file_sorted
            else
                path = file_shuffled
            end if
            do k = 1, 2
                call make_set(k == 2, ids)
                write (output_unit, "(a)") "### " // trim(fname(f)) // " file, " // trim(sname(k)) // " set"
                best_bind = huge(1.0_real64)
                best_open = huge(1.0_real64)
                best_read = huge(1.0_real64)
                best_open0 = huge(1.0_real64)
                best_read0 = huge(1.0_real64)
                best_tbl = huge(1.0_real64)
                best_tblb = huge(1.0_real64)
                best_chunk = huge(1.0_real64)
                nkept = 0_int64
                nrg = 0_int64
                pruned = 0_int64
                do r = 1, rounds
                    block
                        type(parquet_filter) :: filt
                        type(parquet_reader) :: rdr
                        type(parquet_table) :: tbl
                        real(real64), allocatable :: pv(:)

                        ! No filter: the open and the decode the leaf's figures are read against.
                        t0 = now()
                        call parquet_open_reader(rdr, path)
                        best_open0 = min(best_open0, now() - t0)
                        t0 = now()
                        call parquet_read_column(rdr, "p1", p1)
                        best_read0 = min(best_read0, now() - t0)
                        checksum = checksum + int(p1(nrows), int64)
                        ! The key column one row group at a time, with no filter: the read
                        ! pattern the leaf's pre-evaluation uses, without its probe.
                        t0 = now()
                        call parquet_get_num_row_groups(rdr, nrg)
                        do rg = 1_int64, nrg
                            call parquet_get_chunk_size(rdr, rows, rg)
                            if (rows > 0_int64) call parquet_read_column_chunk(rdr, "id", rg, idbuf(1:rows))
                        end do
                        best_chunk = min(best_chunk, now() - t0)
                        checksum = checksum + idbuf(1)
                        call parquet_close_reader(rdr)
                        ! The bind: the set's dedup.
                        t0 = now()
                        call filt%bind("ids", ids)
                        best_bind = min(best_bind, now() - t0)
                        call filt%add("id in @ids")
                        ! The install: the leaf's pre-evaluation and the screen.
                        t0 = now()
                        call parquet_open_reader(rdr, path, filter=filt)
                        best_open = min(best_open, now() - t0)
                        call parquet_get_nrows(rdr, nkept)
                        call parquet_get_num_row_groups(rdr, nrg)
                        pruned = int(parquet_debug_get_row_groups_pruned(), int64)
                        t0 = now()
                        call parquet_read_column(rdr, "p1", p1(1:nkept))
                        best_read = min(best_read, now() - t0)
                        if (nkept > 0_int64) checksum = checksum + int(p1(nkept), int64)
                        call parquet_close_reader(rdr)
                        ! The table, whole and bounded: open plus one payload column.
                        t0 = now()
                        call parquet_open_table(tbl, path, filter=filt)
                        call tbl%get("p1", pv)
                        best_tbl = min(best_tbl, now() - t0)
                        checksum = checksum + size(pv, kind=int64)
                        t0 = now()
                        call parquet_open_table(tbl, path, filter=filt, bounded=.true.)
                        call tbl%get("p1", pv)
                        best_tblb = min(best_tblb, now() - t0)
                        checksum = checksum + size(pv, kind=int64)
                    end block
                end do
                write (output_unit, "(a,i0,a,i0,a,i0,a,i0)") "# rows kept=", nkept, " of ", nrows, &
                    "; row groups pruned=", pruned, " of ", nrg
                call report("open, no filter", best_open0, nrows)
                call report("decode p1, no filter", best_read0, nrows)
                call report("decode id one row group at a time, no filter", best_chunk, nrows)
                call report("bind: dedup of the set", best_bind, nset)
                call report("open with the filter (the leaf's pre-evaluation)", best_open, nrows)
                call report("decode p1 through the filter", best_read, nrows)
                call report("table read, whole: open + p1", best_tbl, nrows)
                call report("table read, bounded: open + p1", best_tblb, nrows)
                write (output_unit, "(a)") ""
            end do
        end do
    end subroutine mode_leaf

    ! ---- map ----

    !> The sets as maps, each backend forced, built and probed serial and on the team.
    subroutine mode_map()
        type(pf_index_map) :: m
        integer(int64), allocatable :: ids(:), probes(:), answers(:)
        integer :: k, b, r, nt
        real(real64) :: t0, best
        character(len=8) :: sname(2)
        character(len=6) :: backends(3)
        character(len=64) :: tag

        write (output_unit, "(a)") "## map"
        sname = ["dense   ", "sparse  "]
        backends = ["direct", "hash  ", "sorted"]
        nt = threads_req
        if (nt < 1) nt = pf_index_threads(nrows)
        allocate(probes(nrows), answers(nrows))
        call pf_random_permutation(probes, 42_int64)
        answers = 0_int64
        do k = 1, 2
            call make_set(k == 2, ids)
            write (output_unit, "(a)") "### " // trim(sname(k)) // " set"
            do b = 1, 3
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call m%build(ids, method=trim(backends(b)), threads=1)
                    best = min(best, now() - t0)
                    checksum = checksum + m%nkeys()
                end do
                call report(trim(backends(b)) // ": build, threads=1", best, nset)
                if (nt > 1) then
                    best = huge(1.0_real64)
                    do r = 1, rounds
                        t0 = now()
                        call m%build(ids, method=trim(backends(b)), threads=nt)
                        best = min(best, now() - t0)
                        checksum = checksum + m%nkeys()
                    end do
                    write (tag, "(a,i0)") trim(backends(b)) // ": build, threads=", nt
                    call report(trim(tag), best, nset)
                end if
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call m%get_many(probes, answers, threads=1)
                    best = min(best, now() - t0)
                    checksum = checksum + answers(1) + answers(nrows)
                end do
                call report(trim(backends(b)) // ": get_many over every row, threads=1", best, nrows)
                if (nt > 1) then
                    best = huge(1.0_real64)
                    do r = 1, rounds
                        t0 = now()
                        call m%get_many(probes, answers, threads=nt)
                        best = min(best, now() - t0)
                        checksum = checksum + answers(1) + answers(nrows)
                    end do
                    write (tag, "(a,i0)") trim(backends(b)) // ": get_many over every row, threads=", nt
                    call report(trim(tag), best, nrows)
                end if
                write (output_unit, "(a,a,i0,a)") "# ", trim(backends(b)) // " memory: ", m%memory_bytes(), " bytes"
            end do
            write (output_unit, "(a)") ""
        end do
    end subroutine mode_map

end program benchmark_filter_set
