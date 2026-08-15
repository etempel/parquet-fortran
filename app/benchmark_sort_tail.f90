!> Phase breakdown of a single-key `pf_argsort`, so the SERIAL TAIL can be sized separately from the
!! threaded engine.
!!
!! `app/benchmark_sort_engine.f90`'s single-key arms are end-to-end (extraction + engine + narrowing)
!! and its multi-key arms build their keys outside the timer, so neither one can say how much of a
!! single-key sort is tail. This one splits it three ways at the same thread count:
!!
!!   extract   `keys%add(values)`      -- builds the sort_key_buf from the caller's array
!!   engine    `pf_argsort(keys, ...)` -- keys already built, so this is the threaded engine alone
!!   narrow    int64 -> int32 permutation
!!
!! Run it before and after any change to the tail: a phase that does not move was not the one you
!! fixed. Not picked up by `fpm test` -- it is an `auto-executables` target.
program benchmark_sort_tail
    use parquet
    use omp_lib
    use iso_fortran_env, only : int32, int64, real64, output_unit
    implicit none
    !
    integer(int64) :: n
    integer :: reps, maxthreads
    real(real64), allocatable :: v(:)
    integer(int64), allocatable :: vi(:)
    character(len=16) :: family !! which key type to time: f64, i64 or i64lo.
    integer(int64) :: vrange    !! integer key values are folded onto 0..vrange-1; 0 = full range.
    logical :: counting         !! .false. forces the radix path so the two can be compared.
    logical :: serial_policy    !! .true. caps the AUTOMATIC thread count to 1.
    integer(int64), allocatable :: perm64(:)
    integer(int32), allocatable :: perm32(:)
    type(pf_sort_keys) :: keys
    integer(int64) :: s, i
    integer :: tl(8), it, nt, rep
    logical :: use_fortran !! .true. selects the pure-Fortran engine; .false. the shipped C++ one.
    real(real64) :: t0, t1, t_ex, t_en, t_na, t_e2e, b_ex, b_en, b_na, b_e2e
    !
    call read_args(n, reps, maxthreads, use_fortran, family, serial_policy, vrange, counting)
    ! Without this the shipped C++ comparison engine answers, which is a different measurement
    ! entirely -- it costs 313 ns/element serially where the Fortran radix costs 22.
    call parquet_debug_use_fortran_sort_engine(use_fortran)
    ! **`pf_sort_keys%add` has no thread argument**, so it always takes the automatic count -- which
    ! means the `thr = 1` row extracts on every core unless the automatic count itself is capped.
    ! Without this the extract column cannot show the serial arm at all, and a regression that only
    ! exists there reads as innocent. Same reason the table layer's own parallel paths see it: inside
    ! a parallel region `pf_sort_threads()` answers 1 by design.
    if (serial_policy) call parquet_set_sort_threads(1)
    ! The counting path is SERIAL by design, so which of the two paths wins is a function of the team
    ! size as well as of the value range -- that is the whole point of sweeping both here.
    call parquet_set_sort_counting_path(counting)
    allocate(v(n), vi(n))
    s = 88172645463325252_int64
    do i = 1_int64, n
        s = ieor(s, ishft(s, 13)); s = ieor(s, ishft(s, -7)); s = ieor(s, ishft(s, 17))
        v(i) = real(s, real64) * 1.0e-9_real64
        ! `i64lo` folds onto 10 distinct values with a modulus, matching
        ! `benchmark_sort_engine`'s own low-cardinality fixture -- that is what puts the engine on
        ! its counting fast path, which is serial by design and so is the control arm here.
        if (vrange > 0_int64) then
            vi(i) = iand(s, huge(1_int64)) - iand(s, huge(1_int64)) / vrange * vrange
        else if (family == "i64lo") then
            vi(i) = iand(s, huge(1_int64)) - iand(s, huge(1_int64)) / 10_int64 * 10_int64
        else
            vi(i) = s
        end if
    end do
    !
    write (output_unit, '(a)') "=============================================================="
    write (output_unit, '(a)') "benchmark_sort_tail -- where a single-key pf_argsort spends time"
    write (output_unit, '(a)') "=============================================================="
    write (output_unit, '(a,i0,a,i0,a,i0)') "  n = ", n, "   reps = ", reps, &
        "   omp_get_num_procs() = ", omp_get_num_procs()
    write (output_unit, '(a,l1,a,a,a,l1)') "  fortran engine = ", use_fortran, &
        "   family = ", trim(family), "   serial auto-policy = ", serial_policy
    write (output_unit, '(a,i0,a,l1)') "  value range = ", vrange, "   counting path = ", counting
    write (output_unit, '(a)') "  all figures ns/element, best of reps"
    write (output_unit, '(a)') ""
    write (output_unit, '(a)') "  thr    extract     engine     narrow   sum(tail+eng)   end-to-end  design buckets"
    !
    tl = [1, 2, 4, 8, 16, 32, 64, 128]
    do it = 1, size(tl)
        nt = tl(it)
        if (nt > maxthreads) cycle
        b_ex = huge(1.0_real64); b_en = huge(1.0_real64)
        b_na = huge(1.0_real64); b_e2e = huge(1.0_real64)
        do rep = 0, reps
            ! --- extraction alone -------------------------------------------------------------
            call keys%clear()
            t0 = omp_get_wtime()
            if (family == "f64") then
                call keys%add(v)
            else
                call keys%add(vi)
            end if
            t1 = omp_get_wtime()
            t_ex = t1 - t0
            ! --- engine alone, keys already built ---------------------------------------------
            t0 = omp_get_wtime()
            call pf_argsort(keys, perm64, threads=nt)
            t1 = omp_get_wtime()
            t_en = t1 - t0
            ! --- narrowing alone --------------------------------------------------------------
            if (allocated(perm32)) deallocate(perm32)
            t0 = omp_get_wtime()
            allocate(perm32(n))
            perm32 = int(perm64, int32)
            t1 = omp_get_wtime()
            t_na = t1 - t0
            ! --- the whole thing, as a user writes it -----------------------------------------
            t0 = omp_get_wtime()
            if (family == "f64") then
                call pf_argsort(v, perm32, threads=nt)
            else
                call pf_argsort(vi, perm32, threads=nt)
            end if
            t1 = omp_get_wtime()
            t_e2e = t1 - t0
            if (rep > 0) then
                b_ex = min(b_ex, t_ex); b_en = min(b_en, t_en)
                b_na = min(b_na, t_na); b_e2e = min(b_e2e, t_e2e)
            end if
        end do
        write (output_unit, '(i5,4f11.3,f13.3,i8,i8)') nt, ns(b_ex), ns(b_en), ns(b_na), &
            ns(b_ex) + ns(b_en) + ns(b_na), ns(b_e2e), &
            int(parquet_debug_sort_design()), int(parquet_debug_sort_split_buckets())
    end do
    !
contains
    !
    !> Seconds to nanoseconds per element.
    function ns(t) result(r)
        real(real64), intent(in) :: t !! elapsed seconds.
        real(real64) :: r             !! nanoseconds per element.
        r = t * 1.0e9_real64 / real(n, real64)
    end function ns
    !
    subroutine read_args(n, reps, maxthreads, use_fortran, family, serial_policy, vrange, counting)
        integer(int64), intent(out) :: n          !! rows.
        integer, intent(out) :: reps              !! timed repetitions; the best is kept.
        integer, intent(out) :: maxthreads        !! highest thread count on the ladder.
        logical, intent(out) :: use_fortran       !! .false. selects the shipped C++ engine.
        character(len=*), intent(out) :: family   !! f64, i64 or i64lo.
        logical, intent(out) :: serial_policy     !! .true. caps the automatic thread count to 1.
        integer(int64), intent(out) :: vrange     !! fold integer keys onto 0..vrange-1; 0 = off.
        logical, intent(out) :: counting          !! .false. forces the radix path.
        character(len=64) :: a
        integer :: i
        !
        n = 5000000_int64
        reps = 3
        maxthreads = 64
        use_fortran = .true.
        family = "f64"
        serial_policy = .false.
        vrange = 0_int64
        counting = .true.
        do i = 1, command_argument_count()
            call get_command_argument(i, a)
            if (a(1:4) == "--n=") then
                read (a(5:), *) n
            else if (a(1:7) == "--reps=") then
                read (a(8:), *) reps
            else if (a(1:10) == "--threads=") then
                read (a(11:), *) maxthreads
            else if (a(1:6) == "--cpp") then
                use_fortran = .false.
            else if (a(1:9) == "--family=") then
                family = a(10:)
            else if (a(1:9) == "--serial") then
                serial_policy = .true.
            else if (a(1:8) == "--range=") then
                read (a(9:), *) vrange
            else if (a(1:14) == "--no-counting") then
                counting = .false.
            end if
        end do
    end subroutine read_args
    !
end program benchmark_sort_tail
