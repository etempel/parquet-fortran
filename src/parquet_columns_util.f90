!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Shared internal helpers for `parquet_column`: argument guards, the validity-bitmap
!! primitives, and the two kind-category predicates.
!!
!! These live in a submodule of their own — rather than contained in `parquet_columns` itself —
!! for one concrete reason: a procedure contained in a submodule is invisible to that
!! submodule's siblings, while one contained in the *module* is reported as
!! `-Wunused-function` when the module is compiled as its own translation unit (its only
!! callers being in other files). Declaring the interfaces in the module and implementing them
!! here gives every submodule access with no warnings, and matches how `parquet_core.f90` already
!! shares its private helpers.
!!
!! **The bitmap.** A hand-rolled `integer(int64)` block array with `1 = null`, `0 = valid`, so
!! an all-zero map means "no nulls" and the whole map can be dropped in O(1) rather than
!! scanned. Bit `b` (1-based) lives in block `(b-1)/64 + 1` at position `mod(b-1, 64)`. It is
!! indexed per ELEMENT, not per row, so a vector column needs `nrows*width` bits (RF8).
!! Measured against stdlib's `bitset_large`, this is 1.6-2x faster on random set/test and on the
!! reindex rebuild at identical memory, and it has no int32 bit-index ceiling — which is why this
!! library takes no stdlib dependency for it.
submodule (parquet_columns) parquet_columns_util
    implicit none
    !> Every bit of a validity word set. Written as `not(0)` rather than `-1` because gfortran
    !! range-checks a negative literal of this kind, and because "all bits" is what it means here.
    integer(int64), parameter :: ALL_BITS = not(0_int64)
contains
    !
    !> Aborts unless the column's active kind is exactly `expected`.
    !!
    !! Kind checking is exact everywhere, with no widening (DD2): a `PK_INT32` column is not
    !! readable through an int64 accessor even though the value would fit. Widening is a
    !! decision the table layer makes once, when it materializes a column from a file.
    !!
    !! **This is the implementation; `check_kind` below forwards to it.** Every guard in this
    !! file comes in that pair, because a typed per-cell accessor calling a `class`-dummy guard
    !! would reintroduce the descriptor block the typed tier exists to remove; the guards are part
    !! of that problem for exactly this reason.
    module procedure parquet_column_check_kind
        if (col%kind /= expected) then
            error stop EP//proc//": column kind is "//trim(kind_text(col%kind))// &
                ", but this call requires "//trim(kind_text(expected))
        end if
    end procedure parquet_column_check_kind
    !
    !> Aborts unless the column's active kind is exactly `expected` (polymorphic form).
    module procedure check_kind
        call parquet_column_check_kind(self, expected, proc)
    end procedure check_kind
    !
    !> Aborts unless `i` is a valid 1-based row index for this column.
    module procedure parquet_column_check_index
        if (i < 1_int64 .or. i > col%nrows) then
            error stop EP//proc//": row index out of range"
        end if
    end procedure parquet_column_check_index
    !
    !> Aborts unless `i` is a valid 1-based row index (polymorphic form).
    !!
    !! Currently has no caller. It is one of five polymorphic guards (`check_kind`, `check_index`,
    !! `check_element`, `check_nrows`, `check_width`), and the other three are still reached from
    !! the bulk/whole-column paths that legitimately keep a `class` dummy -- see CLAUDE.md's
    !! "`parquet_column`'s TYPED accessor tier", whose scope deliberately stops at per-cell. This
    !! one lost its callers when the per-cell paths moved to `parquet_column_check_index`, and is
    !! kept so the set stays complete for the next polymorphic bulk path rather than being deleted
    !! and re-added. Excluded from coverage because it is private to `parquet_columns`, so nothing
    !! -- test included -- can reach it while it has no caller.
    ! GCOVR_EXCL_START
    module procedure check_index
        call parquet_column_check_index(self, i, proc)
    end procedure check_index
    ! GCOVR_EXCL_STOP
    !
    !> Aborts unless `e` is a valid 1-based element index within a row (`1 <= e <= width`).
    !!
    !! Deliberately a different message from `check_index`'s, because the commonest way to get
    !! this wrong is to pass a FLAT element position where a row index and an element index were
    !! wanted -- naming the element axis is what makes that visible instead of looking like an
    !! ordinary out-of-range row.
    module procedure parquet_column_check_element
        if (e < 1_int64 .or. e > int(col%width, int64)) then
            error stop EP//proc//": element index out of range (must be 1 <= e <= the column's width)"
        end if
    end procedure parquet_column_check_element
    !
    !> Aborts unless `e` is a valid 1-based element index within a row (polymorphic form).
    !!
    !! Has never had a caller -- the element-indexed API was typed from the start. Kept and
    !! excluded for the same reason as `check_index` above; see its note.
    ! GCOVR_EXCL_START
    module procedure check_element
        call parquet_column_check_element(self, e, proc)
    end procedure check_element
    ! GCOVR_EXCL_STOP
    !
    !> Aborts unless `n` matches the column's own row count.
    module procedure check_nrows
        if (n /= self%nrows) then
            error stop EP//proc//": value count does not match the column's row count"
        end if
    end procedure check_nrows
    !
    !> Aborts unless `n` matches the column's own vector width.
    module procedure parquet_column_check_width
        if (n /= int(col%width, int64)) then
            error stop EP//proc//": value count per row does not match the column width"
        end if
    end procedure parquet_column_check_width
    !
    !> Aborts unless `n` matches the column's own vector width (polymorphic form).
    module procedure check_width
        call parquet_column_check_width(self, n, proc)
    end procedure check_width
    !
    !> Number of validity bits the column needs: one per element.
    module procedure bits_needed
        res = col%nrows*int(col%width, int64)
    end procedure bits_needed
    !
    !> Number of int64 blocks needed to hold `nbits` bits.
    module procedure blocks_for
        res = (nbits + BITS_PER_BLOCK - 1_int64)/BITS_PER_BLOCK
    end procedure blocks_for
    !
    !> Whether bit `b` is set. An unallocated bitmap has no set bits, which is what makes the
    !! sparse representation transparent to every caller: a null-free column simply has no map.
    module procedure bit_test
        integer(int64) :: k
        ! Defensive only, and therefore never executed by the test suite: every caller already
        ! establishes that the bitmap exists (is_null returns early unless has_nulls, and
        ! gather_validity checks `allocated` itself). Kept so a future caller cannot turn a
        ! missing bitmap into an out-of-bounds read.
        ! gcov attribution artifact: the condition below is evaluated on every call, so the `if`
        ! line registers hits even though the branch is never taken -- confirmed by its own body
        ! (the two lines inside) reliably showing zero.
        if (.not. allocated(map)) then      ! GCOVR_EXCL_START
            res = .false.
            return
        end if                              ! GCOVR_EXCL_STOP
        k = b - 1_int64
        res = btest(map(k/BITS_PER_BLOCK + 1_int64), int(mod(k, BITS_PER_BLOCK)))
    end procedure bit_test
    !
    !> Sets bit `b` of an allocated bitmap (marks the element null).
    !!
    !! **The update is `!$omp atomic`, and both halves of how it is written are required.**
    !! Validity is packed `BITS_PER_BLOCK` elements to one `integer(int64)`, so "different rows"
    !! is not "different memory": two threads writing rows that share a block both read, modify
    !! and write that block, and one update is lost. Nothing announces it -- the column still
    !! validates, the row count is still right, and some row's null flag is simply wrong. This is
    !! reachable straight from the public API (`%set_null` in a parallel loop, which
    !! `doc/pages/operating/thread-safety.md` documents as safe) and from any concurrent value
    !! write to a null-carrying column, since a setter clears the element's bit. See
    !! feature_risks.md Risk-135.
    !!
    !! The two halves: the mask is built *before* the directive, because `!$omp atomic update`
    !! accepts only `x = x op expr` or `x = intrinsic(x, expr)` with `intrinsic` one of
    !! `max`/`min`/`iand`/`ior`/`ieor` -- `ibset` is not in that list, which is why this reads
    !! `ior(map(blk), msk)` rather than the more obvious `ibset(map(blk), pos)`. And the procedure
    !! is not `pure`, because gfortran rejects any OpenMP directive in a `pure` procedure.
    module procedure bit_set
        integer(int64) :: k, blk, msk
        k = b - 1_int64
        blk = k/BITS_PER_BLOCK + 1_int64
        msk = ibset(0_int64, int(mod(k, BITS_PER_BLOCK)))
        !$omp atomic update
        map(blk) = ior(map(blk), msk)
    end procedure bit_set
    !
    !> Clears bit `b` of an allocated bitmap (marks the element valid). Atomic and non-`pure` for
    !! the reasons `bit_set` records; `iand` with the complement is the `ibclr` this construct
    !! will accept.
    module procedure bit_clear
        integer(int64) :: k, blk, msk
        k = b - 1_int64
        blk = k/BITS_PER_BLOCK + 1_int64
        msk = ibset(0_int64, int(mod(k, BITS_PER_BLOCK)))
        !$omp atomic update
        map(blk) = iand(map(blk), not(msk))
    end procedure bit_clear
    !
    module procedure bits_set_range
        integer(int64) :: k0, k1, w0, w1, b0, b1, w
        !
        if (hi < lo) return
        k0 = lo - 1_int64                       ! 0-based first bit
        k1 = hi - 1_int64                       ! 0-based last bit
        w0 = k0/BITS_PER_BLOCK + 1_int64
        w1 = k1/BITS_PER_BLOCK + 1_int64
        b0 = mod(k0, BITS_PER_BLOCK)
        b1 = mod(k1, BITS_PER_BLOCK)
        if (w0 == w1) then
            ! Wholly inside one word: set bits b0..b1 of it and nothing else.
            map(w0) = ior(map(w0), range_mask(b0, b1))
            return
        end if
        map(w0) = ior(map(w0), range_mask(b0, BITS_PER_BLOCK - 1_int64))
        do w = w0 + 1_int64, w1 - 1_int64
            map(w) = ALL_BITS                   ! whole interior words, no masking at all
        end do
        map(w1) = ior(map(w1), range_mask(0_int64, b1))
    end procedure bits_set_range
    !
    module procedure bits_clear_range
        integer(int64) :: k0, k1, w0, w1, b0, b1, w
        !
        if (hi < lo) return
        k0 = lo - 1_int64
        k1 = hi - 1_int64
        w0 = k0/BITS_PER_BLOCK + 1_int64
        w1 = k1/BITS_PER_BLOCK + 1_int64
        b0 = mod(k0, BITS_PER_BLOCK)
        b1 = mod(k1, BITS_PER_BLOCK)
        if (w0 == w1) then
            map(w0) = iand(map(w0), not(range_mask(b0, b1)))
            return
        end if
        map(w0) = iand(map(w0), not(range_mask(b0, BITS_PER_BLOCK - 1_int64)))
        do w = w0 + 1_int64, w1 - 1_int64
            map(w) = 0_int64
        end do
        map(w1) = iand(map(w1), not(range_mask(0_int64, b1)))
    end procedure bits_clear_range
    !
    module procedure bits_copy_range
        integer(int64) :: i, n, dk, sk, dw, db, taken, chunk, sw, sb, piece, mask
        !
        if (nbits <= 0_int64) return
        n = 0_int64
        do while (n < nbits)
            dk = dst_lo - 1_int64 + n
            dw = dk/BITS_PER_BLOCK + 1_int64
            db = mod(dk, BITS_PER_BLOCK)
            ! How much of this destination word this pass can fill.
            chunk = min(BITS_PER_BLOCK - db, nbits - n)
            ! Gather `chunk` source bits starting at src_lo+n into the low bits of `piece`. The
            ! run can straddle two source words, so it is taken in at most two bites rather than
            ! assuming any alignment between the two bitmaps.
            piece = 0_int64
            taken = 0_int64
            do while (taken < chunk)
                sk = src_lo - 1_int64 + n + taken
                sw = sk/BITS_PER_BLOCK + 1_int64
                sb = mod(sk, BITS_PER_BLOCK)
                i = min(BITS_PER_BLOCK - sb, chunk - taken)
                piece = ior(piece, ishft(low_bits(ishft(src(sw), -sb), i), taken))
                taken = taken + i
            end do
            mask = ishft(low_bits(ALL_BITS, chunk), db)
            if (merge_only_set) then
                dst(dw) = ior(dst(dw), iand(ishft(piece, db), mask))
            else
                dst(dw) = ior(iand(dst(dw), not(mask)), iand(ishft(piece, db), mask))
            end if
            n = n + chunk
        end do
    end procedure bits_copy_range
    !
    !> The low `n` bits of `v`; `n >= 64` returns `v` unchanged. Written out because
    !! `ishft(x, -64)` is undefined for a 64-bit integer, so the obvious mask expression
    !! `not(ishft(-1, n))` cannot be used at the full width.
    pure function low_bits(v, n) result(res)
        integer(int64), intent(in) :: v !! the value.
        integer(int64), intent(in) :: n !! how many low bits to keep.
        integer(int64) :: res           !! `v` with every bit at or above `n` cleared.
        if (n >= BITS_PER_BLOCK) then
            res = v
        else if (n <= 0_int64) then
            ! Defensive. No caller can ask for zero bits: `bits_copy_range`'s two `min` expressions
            ! are both bounded below by 1 (its loops only run while `taken < chunk` and `n < nbits`,
            ! and a bit offset is at most 63), and `range_mask` passes `b1 - b0 + 1` with b1 >= b0.
            res = 0_int64 ! GCOVR_EXCL_LINE
        else
            res = iand(v, not(ishft(ALL_BITS, int(n))))
        end if
    end function low_bits
    !
    !> A word with bits `b0 .. b1` (0-based, inclusive) set and every other bit clear.
    pure function range_mask(b0, b1) result(res)
        integer(int64), intent(in) :: b0 !! first bit position.
        integer(int64), intent(in) :: b1 !! last bit position.
        integer(int64) :: res            !! the mask.
        res = ishft(low_bits(ALL_BITS, b1 - b0 + 1_int64), int(b0))
    end function range_mask
    !
    !> Ensures the bitmap exists and covers every element of the column, zero-filling any new
    !! blocks so that added rows start out valid.
    !!
    !! This is the lazy allocation R2 requires: a column that never sees a null never calls this
    !! and never pays for a map.
    !> Aliases the embedded container column, so `parquet_tables` can reach it.
    !!
    !! Refuses a non-container kind by name rather than handing back a null pointer: every caller
    !! is about to dereference this, and a null pointer would fault somewhere else entirely.
    module procedure parquet_column_container
        if (.not. parquet_kind_is_container(col%kind)) then
            error stop EP//"container: column kind is "//trim(kind_text(col%kind))// &
                ", but this call requires a container kind"
        end if
        ! Not reachable through the public API -- adopt_container is the only writer of a
        ! container kind and it refuses an unallocated argument, so a PK_LIST column without a
        ! container cannot be constructed. Kept because the alternative to a guard here is a null
        ! pointer returned to a caller that will dereference it.
        if (.not. allocated(col%container)) then ! GCOVR_EXCL_START
            error stop EP//"container: container storage is not allocated"
        end if ! GCOVR_EXCL_STOP
        p => col%container
    end procedure parquet_column_container
    !
    !> Aliases the embedded container column (polymorphic form).
    module procedure container_ptr
        call parquet_column_container(self, p)
    end procedure container_ptr
    !
    module procedure has_validity_storage
        ! The three validity mechanisms, in the order the three dispatch classes list them. Only the
        ! first two allocate anything, so only they can be raced on.
        if (is_temporal_kind(self%kind)) then
            res = .true.
        else if (is_string_kind(self%kind)) then
            res = .false.
            if (allocated(self%str)) res = parquet_string_column_has_validity(self%str)
        else if (parquet_kind_is_container(self%kind)) then
            ! A FOURTH dispatch class: row nullness lives inside the container. This column's own
            ! bitmap is never allocated for a container kind, so answering from `self%validity`
            ! would say "nulling would still have to allocate" forever, whatever the container has
            ! already done.
            res = .false.
            if (allocated(self%container)) res = .true.
        else
            res = allocated(self%validity)
        end if
    end procedure has_validity_storage
    !
    module procedure ensure_validity
        if (is_temporal_kind(self%kind)) return
        ! The concurrency escape hatch must reach a container too, and must NOT be refused for one:
        ! a table filled from several threads calls this before the region precisely so that the
        ! first null does not race with a lazy allocation, and a container allocates lazily for the
        ! same reasons everything else here does.
        if (parquet_kind_is_container(self%kind)) then
            if (allocated(self%container)) call self%container%ensure_validity()
            return
        end if
        if (is_string_kind(self%kind)) then
            if (allocated(self%str)) call parquet_string_column_reserve_validity(self%str)
            return
        end if
        ! PK_NONE has no storage to give validity to, and ensure_bitmap would size a bitmap from
        ! a zero-width column. A column with no kind cannot be nulled either, so there is nothing
        ! to pre-empt.
        if (self%kind == PK_NONE) return
        call ensure_bitmap(self)
    end procedure ensure_validity
    !
    module procedure ensure_bitmap
        integer(int64) :: need, have, want_bits
        integer(int64), allocatable :: tmp(:)
        ! Sized from the storage CAPACITY, not from the row count. Sizing it from `nrows` would
        ! reallocate the bitmap on every single append even though the storage itself only
        ! reallocates geometrically -- reintroducing exactly the O(n^2) behaviour `cap` exists to
        ! remove, on a column that happens to carry a null, with every test still passing. The
        ! extra blocks are zero-filled below, so rows in the spare capacity read as valid if a
        ! later append ever brings them into range.
        want_bits = max(col%cap, col%nrows)*int(col%width, int64)
        need = max(blocks_for(want_bits), 1_int64)
        if (.not. allocated(col%validity)) then
            allocate(col%validity(need))
            col%validity = 0_int64
        else
            have = size(col%validity, kind=int64)
            if (have < need) then
                allocate(tmp(need))
                tmp = 0_int64
                tmp(1:have) = col%validity(1:have)
                call move_alloc(tmp, col%validity)
            end if
        end if
        col%has_nulls = .true.
    end procedure ensure_bitmap
    !
    !> Releases the bitmap: every row becomes valid again and the column costs one scalar.
    !!
    !! O(1) and deliberately unconditional — the callers that must not lose nulls (`compact_validity`)
    !! check first, while a whole-column `set_all` can drop the map outright because it has just
    !! overwritten every value (RF9).
    module procedure drop_bitmap
        if (allocated(self%validity)) deallocate(self%validity)
        self%has_nulls = .false.
    end procedure drop_bitmap
    !
    !> Whether a kind keeps its null state inside each element rather than in the column bitmap.
    module procedure is_temporal_kind
        select case (kind)
        case (PK_DATE, PK_TIME, PK_TIMESTAMP, PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC)
            res = .true.
        case default
            res = .false.
        end select
    end procedure is_temporal_kind
    !
    !> Whether a kind delegates storage and validity to an embedded `parquet_string_column`.
    module procedure is_string_kind
        res = (kind == PK_STRING .or. kind == PK_STRING_VEC)
    end procedure is_string_kind
    !
    !> The kind's name as fixed-length text, for error messages.
    !!
    !! Lives here rather than contained in `parquet_columns` for the same reason as the helpers
    !! above, and for a sharper one: gfortran does not emit a private *contained* module
    !! procedure whose only callers are in submodules, so that arrangement compiles cleanly and
    !! then fails at LINK time with an undefined symbol.
    module procedure kind_text
        select case (kind)
        case (PK_NONE)
            res = "PK_NONE"
        case (PK_INT32)
            res = "PK_INT32"
        case (PK_INT64)
            res = "PK_INT64"
        case (PK_FLOAT32)
            res = "PK_FLOAT32"
        case (PK_FLOAT64)
            res = "PK_FLOAT64"
        case (PK_LOGICAL)
            res = "PK_LOGICAL"
        case (PK_STRING)
            res = "PK_STRING"
        case (PK_DATE)
            res = "PK_DATE"
        case (PK_TIME)
            res = "PK_TIME"
        case (PK_TIMESTAMP)
            res = "PK_TIMESTAMP"
        case (PK_INT32_VEC)
            res = "PK_INT32_VEC"
        case (PK_INT64_VEC)
            res = "PK_INT64_VEC"
        case (PK_FLOAT32_VEC)
            res = "PK_FLOAT32_VEC"
        case (PK_FLOAT64_VEC)
            res = "PK_FLOAT64_VEC"
        case (PK_LOGICAL_VEC)
            res = "PK_LOGICAL_VEC"
        case (PK_STRING_VEC)
            res = "PK_STRING_VEC"
        case (PK_DATE_VEC)
            res = "PK_DATE_VEC"
        case (PK_TIME_VEC)
            res = "PK_TIME_VEC"
        case (PK_TIMESTAMP_VEC)
            res = "PK_TIMESTAMP_VEC"
        case (PK_LIST)
            res = "PK_LIST"
        case (PK_MAP)
            res = "PK_MAP"
        case (PK_STRUCT)
            res = "PK_STRUCT"
        case default
            res = "PK_UNKNOWN"
        end select
    end procedure kind_text
    !
end submodule parquet_columns_util
