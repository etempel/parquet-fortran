! ============================================================================================
! The per-key ordering decision -- ONE source of truth, textually included into every comparator
! that needs it. Not a standalone compilation unit: this file is `include`d into the CONTAINS
! section of each host procedure in src/parquet_sorting_engine.f90.
! ============================================================================================
!
! WHY AN INCLUDE FILE RATHER THAN A SHARED PROCEDURE. The obvious shape -- one module procedure
! that everything calls -- is what this replaced, and it cost 1.3x-2.2x on Linux. Under ELF with
! `-fPIC` (which fpm's release profile passes to every file), gfortran must assume any GLOBAL
! symbol could be replaced at link time by a shared library, so it refuses to inline it. GCC says
! so itself under `-fopt-info-inline-all`:
!
!     missed: not inlinable: sort_row_less -> sort_compare_key,
!             function body can be overwritten at link time
!
! Measured on machine B (2026-08-14, feature_benchmark_comparator_B.md): a single-key comparison
! paid FOUR out-of-line calls where the equivalent C++ -- whose helpers are `static inline`, i.e.
! internal linkage and immune -- paid none.
!
! Fortran offers no `static`, and the linkage a procedure gets is decided by WHERE it is declared:
!
!     module procedure                      -> GLOBAL   (interposable, not inlinable under -fPIC)
!     private procedure in a SUBMODULE      -> GLOBAL   (this is what caught the old compare_bytes)
!     private procedure in a MODULE         -> local, but unreachable from a submodule
!     INTERNAL procedure (inside a procedure) -> LOCAL   <-- the only usable one
!
! So the hot helpers must be INTERNAL procedures of whatever calls them, and this file is how they
! stay a single source of truth while being compiled into several hosts. Verified: with `-O3` the
! copies vanish entirely (inlined); with `-fno-inline` they appear as `t cmp_key.1.isra.0`, i.e.
! local, which is exactly what makes them immune.
!
! THIS PRESERVES feature_risks.md Risk-34 BY A DIFFERENT MECHANISM, and the difference matters.
! The old guarantee was "both comparators call one shared body". The new one is "both comparators
! include one shared source". There is still exactly one place to edit, so the two cannot drift --
! but only as long as nobody reimplements the logic in a host instead of including this file.
!
! RULES FOR EDITING THIS FILE:
!   * Every procedure here takes everything it needs as an ARGUMENT and reads no host variable.
!     That is what lets it be included anywhere, and it also means gfortran passes no static chain
!     pointer -- a helper that closed over a host variable would reintroduce a hidden cost.
!   * It is compiled as part of each host, so the project's 132-column limit applies normally.
!   * It is NOT scanned by most of tools/check_source_conventions.py, whose checks glob `src/*.f90`
!     (`check_include_files_are_used` is the one that does look here). Do not put anything in this
!     file that relies on one of those checks to police it.
!   * cpp does NOT see this file. fpm.toml declares `[preprocess.cpp]`, which runs over the .f90;
!     the Fortran `include` is expanded later, by the compiler. A `#ifdef` here would silently do
!     nothing.

        !> Output tier of row `i` under one key: 0 sorts first, 2 last.
        !!
        !! Absolute -- `descending` never reaches this, which is Arrow's own rule: a descending sort
        !! still puts nulls last by default rather than flipping them to the front. Under the default
        !! placement the tiers are values(0), NaNs(1), nulls(2); under `nulls_first` they are
        !! nulls(0), NaNs(1), values(2). NaN is tier 1 either way, and only a real key can be in it.
        integer function tier_of(key, i)
            type(sort_key_buf), intent(in) :: key !! the bound key.
            integer(int64), intent(in) :: i       !! row, 1-based.
            logical :: is_null                    !! row `i` is null under this key.
            logical :: is_nan                     !! row `i` is a NaN under this key.
            !
            ! An UNALLOCATED `valid` means "this key has no nulls at all" -- the fast path, and the
            ! first thing a port of the C++ side gets wrong, because there the same state is an
            ! empty vector.
            is_null = .false.
            if (allocated(key%valid)) is_null = (key%valid(i) == 0_c_int8_t)
            !
            is_nan = .false.
            if (.not. is_null .and. key%family == SK_REAL) is_nan = ieee_is_nan(key%reals(i))
            !
            if (key%nulls_first) then
                if (is_null) then
                    tier_of = 0
                else if (is_nan) then
                    tier_of = 1
                else
                    tier_of = 2
                end if
            else
                if (is_null) then
                    tier_of = 2
                else if (is_nan) then
                    tier_of = 1
                else
                    tier_of = 0
                end if
            end if
        end function tier_of

        !> -1/0/+1 for rows `a` and `b` under ONE key, with its order and null placement applied.
        !!
        !! Two rows in the same non-value tier (both null, or both NaN) compare EQUAL, so the
        !! caller's index tiebreaker keeps them in file order. `descending` negates the answer within
        !! the value tier only -- every early return below skips it.
        integer function cmp_key(key, a, b)
            type(sort_key_buf), intent(in) :: key !! the bound key.
            integer(int64), intent(in) :: a       !! first row, 1-based.
            integer(int64), intent(in) :: b       !! second row, 1-based.
            integer :: ta, tb                     !! tiers of `a` and `b`.
            integer :: value_tier                 !! which tier number the VALUES occupy here.
            integer(int64) :: ia, ib              !! integer key values.
            real(real64) :: ra, rb                !! real key values.
            !
            ta = tier_of(key, a)
            tb = tier_of(key, b)
            if (ta /= tb) then
                cmp_key = -1
                if (ta > tb) cmp_key = 1
                return
            end if
            !
            ! Same tier. If it is not the VALUE tier then both rows are null, or both are NaN, and
            ! the answer is EQUAL -- which is what leaves them in file order once the caller's index
            ! tiebreaker runs. Note the value tier is 2 under `nulls_first` and 0 otherwise.
            value_tier = 0
            if (key%nulls_first) value_tier = 2
            if (ta /= value_tier) then
                cmp_key = 0
                return
            end if
            !
            cmp_key = 0
            select case (key%family)
            case (SK_INT)
                ia = key%ints(a)
                ib = key%ints(b)
                if (ia < ib) then
                    cmp_key = -1
                else if (ia > ib) then
                    cmp_key = 1
                end if
            case (SK_REAL)
                ra = key%reals(a)
                rb = key%reals(b)
                if (ra < rb) then
                    cmp_key = -1
                else if (ra > rb) then
                    cmp_key = 1
                end if
            case default
                cmp_key = compare_bytes(key, a, b)
            end select
            !
            if (key%descending) cmp_key = -cmp_key
        end function cmp_key
