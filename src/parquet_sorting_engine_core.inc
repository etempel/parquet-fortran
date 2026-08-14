        ! ---- The comparator core, as INTERNAL procedures --------------------------------------
        !
        ! NOT a standalone program unit, and deliberately not a `.f90` file: this is `#include`d
        ! into the `contains` block of every procedure in src/parquet_sorting_engine.f90 that
        ! drives a comparison loop. fpm compiles `.f90` only, so it never tries to build this
        ! file on its own -- but it MUST ship, so it must never be added to
        ! tools/prep_fpm_publish.sh's REMOVE_PATHS.
        !
        ! **Why these are internal procedures rather than module procedures.** gfortran gives a
        ! module-level procedure a GLOBAL symbol, and fpm's release profile compiles with
        ! `-fPIC`. Under `-fPIC` GCC must assume a global symbol could be replaced at load time,
        ! so it refuses to inline it -- `-fopt-info-inline-all` reports "not inlinable: ...
        ! function body can be overwritten at link time" on every edge. The shipped comparator
        ! therefore cost FOUR out-of-line calls per single-key comparison, at ~2 ns each, while
        ! the C++ engine it is measured against writes its helpers `static inline` (internal
        ! linkage, which interposition cannot touch) and inlines to one flat function.
        !
        ! An internal procedure -- one contained inside another procedure -- emits NO symbol at
        ! all, so it is always inlinable, `-fPIC` or not. That is the whole reason for this file.
        ! Being `private` does NOT achieve this: a private module-level procedure still gets a
        ! global symbol. Raising the inliner budget does not either (byte-identical object).
        !
        ! **Keep these in step with the module-level twins** `sort_tier_of` / `sort_compare_key` /
        ! `sort_row_less` / `sort_keys_compare` in src/parquet_sorting_engine.f90, which still
        ! exist because src/parquet_sorting.f90 declares their interfaces. Those are a second copy
        ! of this logic and can drift from it -- feature_risks.md Risk-34 applies to this file too.
        !
        ! **Every dummy here is a plain `type(sort_key_buf)`, never `class`** -- see the submodule
        ! header for the ifx runtime-descriptor cost that rule exists to avoid.
        !
        ! After editing this file, run `fpm clean --skip` before measuring: fpm does not know the
        ! `.f90` depends on it, so an include-only edit may not trigger a rebuild.

        !> Internal twin of `sort_tier_of`: output tier of row `i` under one key.
        function inl_tier_of(key, i) result(tier)
            type(sort_key_buf), intent(in) :: key !! the bound key.
            integer(int64), intent(in) :: i       !! row, 1-based.
            integer :: tier                       !! 0, 1 or 2.
            !
            logical :: is_null !! row `i` is null under this key.
            logical :: is_nan  !! row `i` is a NaN under this key (real keys only).
            !
            ! An UNALLOCATED `valid` means "this key has no nulls at all" -- the fast path.
            is_null = .false.
            if (allocated(key%valid)) is_null = (key%valid(i) == 0_c_int8_t)
            !
            is_nan = .false.
            ! `x /= x` rather than `ieee_is_nan` -- see this submodule's header.
            if (.not. is_null .and. key%family == SK_REAL) is_nan = (key%reals(i) /= key%reals(i))
            !
            ! `descending` deliberately does not appear here.
            if (key%nulls_first) then
                if (is_null) then
                    tier = 0
                else if (is_nan) then
                    tier = 1
                else
                    tier = 2
                end if
            else
                if (is_null) then
                    tier = 2
                else if (is_nan) then
                    tier = 1
                else
                    tier = 0
                end if
            end if
        end function inl_tier_of

        !> Internal twin of `sort_compare_key`: -1/0/+1 for rows `a` and `b` under ONE key.
        !!
        !! The string arm deliberately calls the module-level `compare_bytes` rather than
        !! inlining it: it is the cold path for numeric keys, its cost is dominated by its own
        !! byte loop, and inlining it fattens every instantiation of this file -- which is what
        !! made a partial conversion measurably SLOWER on the integer arm.
        function inl_compare_key(key, a, b) result(c)
            type(sort_key_buf), intent(in) :: key !! the bound key.
            integer(int64), intent(in) :: a       !! first row, 1-based.
            integer(int64), intent(in) :: b       !! second row, 1-based.
            integer :: c                          !! -1, 0 or +1.
            !
            integer :: ta, tb        !! tiers of `a` and `b`.
            integer :: value_tier    !! which tier number the VALUES occupy under this key.
            integer(int64) :: ia, ib !! integer key values.
            real(real64) :: ra, rb   !! real key values.
            !
            ta = inl_tier_of(key, a)
            tb = inl_tier_of(key, b)
            if (ta /= tb) then
                c = -1
                if (ta > tb) c = 1
                return
            end if
            !
            ! Same tier. If it is not the VALUE tier then both rows are null, or both are NaN,
            ! and the answer is EQUAL. The value tier is 2 under `nulls_first` and 0 otherwise.
            value_tier = 0
            if (key%nulls_first) value_tier = 2
            if (ta /= value_tier) then
                c = 0
                return
            end if
            !
            c = 0
            select case (key%family)
            case (SK_INT)
                ia = key%ints(a)
                ib = key%ints(b)
                if (ia < ib) then
                    c = -1
                else if (ia > ib) then
                    c = 1
                end if
            case (SK_REAL)
                ra = key%reals(a)
                rb = key%reals(b)
                if (ra < rb) then
                    c = -1
                else if (ra > rb) then
                    c = 1
                end if
            case default
                c = compare_bytes(key, a, b)
            end select
            !
            ! `descending` reverses the VALUE tier only -- every early return above skipped it.
            if (key%descending) c = -c
        end function inl_compare_key

        !> Internal twin of `sort_row_less`: every key in precedence order, then the row index.
        function inl_row_less(keys, a, b) result(less)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: a           !! first row, 1-based.
            integer(int64), intent(in) :: b           !! second row, 1-based.
            logical :: less                           !! .true. when `a` sorts before `b`.
            !
            integer :: k !! key index.
            integer :: c !! this key's three-way answer.
            !
            do k = 1, size(keys)
                c = inl_compare_key(keys(k), a, b)
                if (c /= 0) then
                    less = (c < 0)
                    return
                end if
            end do
            !
            ! THE tiebreaker -- what makes this a total order. See `sort_row_less`.
            less = (a < b)
        end function inl_row_less

        !> Internal twin of `sort_keys_compare`: three-way, WITHOUT the index tiebreaker.
        function inl_keys_compare(keys, a, b, nkeys) result(c)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: a           !! first row, 1-based.
            integer(int64), intent(in) :: b           !! second row, 1-based.
            integer, intent(in) :: nkeys              !! leading keys taking part.
            integer :: c                              !! -1, 0 or +1.
            !
            integer :: k  !! key index.
            integer :: nk !! `nkeys`, clamped.
            !
            ! Clamped rather than validated -- a prefix longer than the key list means "all".
            nk = nkeys
            if (nk > size(keys)) nk = size(keys)
            !
            c = 0
            do k = 1, nk
                c = inl_compare_key(keys(k), a, b)
                if (c /= 0) return
            end do
        end function inl_keys_compare
