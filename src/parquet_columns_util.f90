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
!! here gives every submodule access with no warnings, and matches how `parquet.f90` already
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
contains
    !
    !> Aborts unless the column's active kind is exactly `expected`.
    !!
    !! Kind checking is exact everywhere, with no widening (DD2): a `PK_INT32` column is not
    !! readable through an int64 accessor even though the value would fit. Widening is a
    !! decision the table layer makes once, when it materializes a column from a file.
    module procedure check_kind
        if (self%kind /= expected) then
            error stop EP//proc//": column kind is "//trim(kind_text(self%kind))// &
                ", but this call requires "//trim(kind_text(expected))
        end if
    end procedure check_kind
    !
    !> Aborts unless `i` is a valid 1-based row index for this column.
    module procedure check_index
        if (i < 1_int64 .or. i > self%nrows) then
            error stop EP//proc//": row index out of range"
        end if
    end procedure check_index
    !
    !> Aborts unless `n` matches the column's own row count.
    module procedure check_nrows
        if (n /= self%nrows) then
            error stop EP//proc//": value count does not match the column's row count"
        end if
    end procedure check_nrows
    !
    !> Aborts unless `n` matches the column's own vector width.
    module procedure check_width
        if (n /= int(self%width, int64)) then
            error stop EP//proc//": value count per row does not match the column width"
        end if
    end procedure check_width
    !
    !> Number of validity bits the column needs: one per element.
    module procedure bits_needed
        res = self%nrows*int(self%width, int64)
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
    module procedure bit_set
        integer(int64) :: k, blk
        k = b - 1_int64
        blk = k/BITS_PER_BLOCK + 1_int64
        map(blk) = ibset(map(blk), int(mod(k, BITS_PER_BLOCK)))
    end procedure bit_set
    !
    !> Clears bit `b` of an allocated bitmap (marks the element valid).
    module procedure bit_clear
        integer(int64) :: k, blk
        k = b - 1_int64
        blk = k/BITS_PER_BLOCK + 1_int64
        map(blk) = ibclr(map(blk), int(mod(k, BITS_PER_BLOCK)))
    end procedure bit_clear
    !
    !> Ensures the bitmap exists and covers every element of the column, zero-filling any new
    !! blocks so that added rows start out valid.
    !!
    !! This is the lazy allocation R2 requires: a column that never sees a null never calls this
    !! and never pays for a map.
    module procedure ensure_bitmap
        integer(int64) :: need, have
        integer(int64), allocatable :: tmp(:)
        need = max(blocks_for(bits_needed(self)), 1_int64)
        if (.not. allocated(self%validity)) then
            allocate(self%validity(need))
            self%validity = 0_int64
        else
            have = size(self%validity, kind=int64)
            if (have < need) then
                allocate(tmp(need))
                tmp = 0_int64
                tmp(1:have) = self%validity(1:have)
                call move_alloc(tmp, self%validity)
            end if
        end if
        self%has_nulls = .true.
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
