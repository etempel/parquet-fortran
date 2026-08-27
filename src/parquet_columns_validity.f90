!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Kind-dispatched validity for `parquet_column` (RF8).
!!
!! Validity is deliberately NOT uniform across kinds, and this submodule is the single place
!! that knows the difference:
!!
!! ```
!! kind                          null state lives in            any_null() answers via
!! int32/int64/f32/f64/logical   the column's own bitmap        bitmap scan (O(blocks))
!!   and their *_VEC forms       one bit per ELEMENT            (nrows*width bits)
!! string (+ PK_STRING_VEC)      the embedded string column     its own null_count() > 0
!! date/time/timestamp (+ _VEC)  inside each element            cached flag, rescanned when dirty
!! list/map/struct               the container column           per-row is_null_row, early exit
!! ```
!!
!! **Every one of the five queries below has to carry all four rows of that table, and the
!! container row was once carried by only two of them.** `is_null_row`, `set_null_row` and
!! `clear_null_row` delegated from the day container columns landed, while `any_null`,
!! `row_validity`, `element_validity` and the element form of `is_null` fell through to a bitmap
!! that is deliberately never allocated for a container kind -- so `%is_null(i)` answered `.true.`
!! for a null row while `%any_null()` answered `.false.` about the same column, with nothing to
!! announce the disagreement. The writers were delegated and the readers were not, and nothing in
!! the language connects the two sets: **a query added here must be checked against all four rows,
!! not against the bitmap it happens to be written around** (feature_risks.md Risk-159).
!!
!! Hoisting everything into the column bitmap was rejected: it would mean two sources of truth
!! kept in sync by every mutation path, and for the temporal kinds it would fight
!! `parquet_temporal`'s deliberate decision that those three types carry their own null state.
!!
!! The bitmap is **sparse** (R2): a null-free column allocates nothing at all. It appears on the
!! first `set_null`/`append_nulls` and is dropped again by a whole-column `set_all` or an
!! explicit `compact_validity`.
submodule (parquet_columns) parquet_columns_validity
    implicit none
contains
    !
    !> Whether any element of the column is null.
    !!
    !! Bitmap kinds scan the blocks (cheap: one machine word per 64 elements, and the common
    !! null-free case is a single `allocated()` test). String kinds ask the embedded column.
    !! Temporal kinds would need an O(n) element scan, so the answer is cached and only
    !! recomputed after a mutation has marked it dirty -- otherwise `any_null` would be an O(n)
    !! query on types whose whole point is cheap access.
    !!
    !! **This is NOT a one-line forwarder onto `parquet_column_any_null`, and the duplication is
    !! forced rather than chosen**: this form is `intent(inout)` and so may refresh a temporal
    !! column's null cache, while the typed form is `intent(in)` and must scan instead (its own
    !! doc-comment explains why that matters under concurrency). The two ladders therefore have to
    !! be kept in step BY HAND, and they were not -- both were missing the container arm below at
    !! the same time. A dispatch class added to either one belongs in both.
    module procedure any_null
        integer(int64) :: k, nbits, nblk
        res = .false.
        if (self%nrows <= 0_int64) return
        if (is_string_kind(self%kind)) then
            if (allocated(self%str)) res = parquet_string_column_null_count(self%str) > 0_int64
            return
        end if
        if (is_temporal_kind(self%kind)) then
            if (self%nulls_dirty) then
                call rescan_temporal_nulls(self)
            end if
            res = self%nulls_cached
            return
        end if
        ! The container class: row nullness lives inside the container, so `has_nulls` stays
        ! .false. and the test below would answer "no nulls" for a column that has them. See the
        ! twin arm in parquet_column_any_null, and the file header's dispatch table.
        if (parquet_kind_is_container(self%kind)) then
            if (.not. allocated(self%container)) return
            do k = 1_int64, self%nrows
                if (self%container%is_null_row(k)) then
                    res = .true.
                    return
                end if
            end do
            return
        end if
        if (.not. self%has_nulls) return
        if (.not. allocated(self%validity)) return
        nbits = bits_needed(self)
        nblk = min(blocks_for(nbits), size(self%validity, kind=int64))
        do k = 1_int64, nblk
            if (self%validity(k) /= 0_int64) then
                res = .true.
                return
            end if
        end do
    end procedure any_null
    !
    !> Whether row `i` is null -- meaning, on a vector kind, that ANY element of it is null.
    !!
    !! The row forms of the validity API are asymmetric on purpose: a row QUERY answers about the
    !! row as a whole ("is anything here missing?"), while a whole-row MUTATION (`set_null(i)`,
    !! `clear_null(i)`, `append_nulls`) acts on every element of it. Use `is_null(i, e)` when a
    !! single element is what is meant.
    !!
    !! Costs O(width) with an early exit on the first null, against O(1) for the first-element
    !! convention this replaced. That is the narrow, deliberate price of the correctness: a row
    !! whose third element was null used to answer `.false.` here. The bulk paths do not pay it --
    !! `row_validity` walks the bitmap by machine word instead of calling this per row.
    !! **This is the implementation; the `is_null_row` binding below forwards to it.** See
    !! feature_ifx.md and the typed-tier banner in `parquet_columns.f90` for why the body lives
    !! at the `type` end -- and note that a typed body must call the TYPED guards, or the whole
    !! descriptor block it exists to remove comes straight back.
    module procedure parquet_column_is_null_row
        integer(int64) :: e, base, w
        call parquet_column_check_index(col, i, "is_null")
        res = .false.
        w = int(col%width, int64)
        select case (col%kind)
        case (PK_STRING, PK_STRING_VEC)
            if (.not. allocated(col%str)) return
            if (col%kind == PK_STRING) then
                res = parquet_string_column_is_null(col%str, i)
            else
                base = (i - 1_int64)*w
                do e = 1_int64, w
                    if (parquet_string_column_is_null(col%str, base + e)) then
                        res = .true.
                        return
                    end if
                end do
            end if
        case (PK_DATE)
            res = col%dt(i)%is_null()
        case (PK_TIME)
            res = col%tm(i)%is_null()
        case (PK_TIMESTAMP)
            res = col%ts(i)%is_null()
        case (PK_DATE_VEC)
            do e = 1_int64, w
                if (col%dtv(e, i)%is_null()) then
                    res = .true.
                    return
                end if
            end do
        case (PK_TIME_VEC)
            do e = 1_int64, w
                if (col%tmv(e, i)%is_null()) then
                    res = .true.
                    return
                end if
            end do
        case (PK_TIMESTAMP_VEC)
            do e = 1_int64, w
                if (col%tsv(e, i)%is_null()) then
                    res = .true.
                    return
                end if
            end do
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! Asked of the container: this column's own bitmap is never allocated for a container
            ! kind, so falling through to the default arm below would answer .false. for a row
            ! that really is null -- a silent wrong answer rather than a missing feature.
            res = col%container%is_null_row(i)
        case (PK_NONE)
            ! Unreachable through the public API: check_index above rejects every index on a
            ! kindless column (its row count is 0), so this arm is defensive only.
            error stop EP//"is_null: column has no kind assigned" ! GCOVR_EXCL_LINE
        case default
            if (.not. col%has_nulls) return
            base = (i - 1_int64)*w
            do e = 1_int64, w
                if (bit_test(col%validity, base + e)) then
                    res = .true.
                    return
                end if
            end do
        end select
    end procedure parquet_column_is_null_row
    !
    !> Whether row `i` is null -- meaning, on a vector kind, that ANY element of it is null
    !! (polymorphic form).
    module procedure is_null_row
        res = parquet_column_is_null_row(self, i)
    end procedure is_null_row
    !
    !> Whether element `e` of row `i` is null.
    !!
    !! Defined for every kind, scalar included: a scalar column has `width == 1`, so `e` can only
    !! be 1 and the answer equals the row form. Keeping it defined there rather than an error is
    !! what lets generic code (the generated table accessors, a caller's own loop over elements)
    !! use one shape for both without branching on the kind.
    module procedure parquet_column_is_null_elem
        integer(int64) :: w
        call parquet_column_check_index(col, i, "is_null")
        w = int(col%width, int64)
        call parquet_column_check_element(col, e, "is_null")
        res = .false.
        select case (col%kind)
        case (PK_STRING, PK_STRING_VEC)
            if (.not. allocated(col%str)) return
            res = parquet_string_column_is_null(col%str, (i - 1_int64)*w + e)
        case (PK_DATE)
            res = col%dt(i)%is_null()
        case (PK_TIME)
            res = col%tm(i)%is_null()
        case (PK_TIMESTAMP)
            res = col%ts(i)%is_null()
        case (PK_DATE_VEC)
            res = col%dtv(e, i)%is_null()
        case (PK_TIME_VEC)
            res = col%tmv(e, i)%is_null()
        case (PK_TIMESTAMP_VEC)
            res = col%tsv(e, i)%is_null()
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! A container column's `width` is 1 (adopt_container fixes it there and says why), so
            ! check_element above has already restricted `e` to 1 and there is exactly one element
            ! per row -- which makes this the same question the row form answers, and answering it
            ! keeps the width-1 shape this procedure's own doc-comment promises for every other
            ! width-1 kind. Falling through to the default arm instead read an unallocated bitmap
            ! and answered .false. for a genuinely null row.
            !
            ! The TABLE layer refuses %is_null(i, e) on a container column rather than forwarding
            ! here, and the asymmetry is deliberate: there `e` is the caller asking about the e-th
            ! element INSIDE the list, which is a question about the container's contents that the
            ! table cannot answer and must not guess at. See feature_container_phase6.md, Q3.
            res = col%container%is_null_row(i)
        case (PK_NONE)
            ! Unreachable through the public API, exactly as in is_null_row above.
            error stop EP//"is_null: column has no kind assigned" ! GCOVR_EXCL_LINE
        case default
            if (.not. col%has_nulls) return
            res = bit_test(col%validity, (i - 1_int64)*w + e)
        end select
    end procedure parquet_column_is_null_elem
    !
    !> Whether element `e` of row `i` is null (polymorphic form).
    module procedure is_null_elem
        res = parquet_column_is_null_elem(self, i, e)
    end procedure is_null_elem
    !
    !> Builds the whole per-row validity mask in one pass.
    !!
    !! Three cases, in decreasing order of how well they can be done:
    !!
    !!  1. **No nulls at all** -- `valid` is left unallocated and nothing is scanned. This is the
    !!     case worth optimising for: it is what every column of an ordinary file looks like, and
    !!     an unallocated result passed to an `optional` dummy is an absent argument, so the
    !!     caller's writer never has to build a null bitmap either.
    !!  2. **A bitmap kind with nulls** -- the bitmap is walked one 64-bit block at a time, and a
    !!     block that is entirely zero (64 valid elements, the overwhelmingly common block even in
    !!     a column that does have nulls) is skipped without touching a single row. Only a nonzero
    !!     block costs per-row work.
    !!  3. **A string or temporal kind** -- their null state does not live in this bitmap at all
    !!     (see the table in the module doc), so there is nothing to walk and the per-row `is_null`
    !!     loop is the honest implementation.
    module procedure row_validity
        integer(int64) :: i, e, n, nblk, blk, base, lo, hi, w, nbits
        integer(int64) :: word
        integer :: p
        !
        n = self%nrows
        if (n <= 0_int64) return
        ! Case 1. O(1) for a column that never had a null set, so it costs nothing on the common
        ! path. Goes through parquet_column_any_null rather than the type-bound any_null because
        ! this procedure is intent(in): see the interface's own note in parquet_columns.f90 for
        ! why that matters more than the cache refresh it gives up.
        if (.not. parquet_column_any_null(self)) return
        allocate(valid(n))
        valid = .true.
        w = int(self%width, int64)
        ! Case 3, specialised on the kind ONCE rather than per row. The generic `%is_null(i)` it
        ! replaced re-validated the index, re-read the width and re-dispatched the kind for every
        ! single row, and for a vector kind did all of that around an inner loop as well. The
        ! temporal `%is_null` are ELEMENTAL (parquet_temporal.f90), so the whole column becomes one
        ! array expression here and the per-row call disappears entirely; `any(..., dim=1)` is the
        ! vector form of "the row is null if ANY element is". Measured on 2 M rows, best of three
        ! runs, via `element_validity` (whose case 3 has the same shape): date 10.75 -> 3.88 ms and
        ! timestamp 10.13 -> 3.94 ms, i.e. 2.6-2.8x. What remains is the 1.4x gap to the bitmap
        ! walk's 2.69 ms, and that gap is structural rather than more work to do: the bitmap path
        ! skips a whole 64-element word whenever it is zero, while these kinds have to look at
        ! every element because each one carries its own null flag.
        select case (self%kind)
        case (PK_DATE)
            valid(1:n) = .not. self%dt(1:n)%is_null()
            return
        case (PK_TIME)
            valid(1:n) = .not. self%tm(1:n)%is_null()
            return
        case (PK_TIMESTAMP)
            valid(1:n) = .not. self%ts(1:n)%is_null()
            return
        case (PK_DATE_VEC)
            valid(1:n) = .not. any(self%dtv(1:w, 1:n)%is_null(), dim=1)
            return
        case (PK_TIME_VEC)
            valid(1:n) = .not. any(self%tmv(1:w, 1:n)%is_null(), dim=1)
            return
        case (PK_TIMESTAMP_VEC)
            valid(1:n) = .not. any(self%tsv(1:w, 1:n)%is_null(), dim=1)
            return
        case (PK_STRING, PK_STRING_VEC)
            ! A string column's nulls live in parquet_string_column's own bitmap, reached by a
            ! flat element index; there is no elemental form, so this stays a loop -- but one
            ! without the two index checks and the kind dispatch per element. The early exit
            ! keeps a row with a null in its first element from scanning the rest.
            if (.not. allocated(self%str)) return
            do i = 1_int64, n
                base = (i - 1_int64)*w
                do e = 1_int64, w
                    if (parquet_string_column_is_null(self%str, base + e)) then
                        valid(i) = .false.
                        exit
                    end if
                end do
            end do
            return
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! Row nullness lives inside the container, so there is no bitmap here to walk and the
            ! block loop below would read an unallocated one. A per-row call is the only shape
            ! available -- the containers expose no bulk validity view -- and it is bounded by
            ! nrows rather than by width*nrows, since a container column's width is 1.
            if (.not. allocated(self%container)) return
            do i = 1_int64, n
                valid(i) = .not. self%container%is_null_row(i)
            end do
            return
        end select
        ! Case 2. any_null already established there is a bitmap; the guard keeps a future caller
        ! from turning a missing one into an out-of-bounds read.
        if (.not. allocated(self%validity)) return
        nbits = bits_needed(self)
        nblk = min(blocks_for(nbits), size(self%validity, kind=int64))
        ! One loop for both scalar and vector kinds, because "any element null" makes them the
        ! same walk: every SET bit names a null element, and that element's row is null. A zero
        ! word is 64 valid elements and is skipped whole -- which is the property worth protecting
        ! here, and it survives unchanged from the first-element convention this replaced. The
        ! remaining cost is proportional to the NUMBER OF NULLS rather than to nrows, so on the
        ! sparse-null case this is strictly less work than testing one bit per row was. Marking a
        ! row twice (two null elements in one row) is harmless.
        do blk = 1_int64, nblk
            word = self%validity(blk)
            if (word == 0_int64) cycle
            base = (blk - 1_int64)*BITS_PER_BLOCK
            ! Walk the SET bits only, via trailz + ibclr, rather than testing all 64 positions.
            ! That is what keeps the cost proportional to the number of nulls: scanning every
            ! position instead makes a densely-null column cost `width` times more than the
            ! old one-bit-test-per-row form did (measured at 2.7x on a width-16 column that is
            ! half null -- this loop shape brings it back to parity).
            do while (word /= 0_int64)
                p = trailz(word)
                i = (base + int(p, int64))/w + 1_int64
                if (i <= n) valid(i) = .false.
                word = ibclr(word, p)
            end do
        end do
    end procedure row_validity
    !
    !> Builds the whole per-ELEMENT validity mask in one pass, shaped `(width, nrows)`.
    !!
    !! The same three cases as `row_validity`, and the same contract: a null-free column leaves
    !! `valid` UNALLOCATED, so it reaches an `optional` dummy as an absent argument and costs
    !! nothing. The difference is only that no row summary is applied -- this is the column's
    !! actual state, and it is the shape `parquet_write_column` takes for a vector column, so a
    !! table write can hand it straight on without a conversion pass.
    module procedure element_validity
        integer(int64) :: i, e, n, nblk, blk, base, w, nbits, flat
        integer(int64) :: word
        integer :: p
        !
        n = self%nrows
        if (n <= 0_int64) return
        if (.not. parquet_column_any_null(self)) return
        w = int(self%width, int64)
        allocate(valid(w, n))
        valid = .true.
        ! String and temporal kinds keep their null state outside the bitmap, so there is none to
        ! walk -- but the per-element `%is_null(i, e)` this replaced was the expensive part, not
        ! the absence of a bitmap: two index checks, a width read and a kind dispatch for every
        ! element. Specialising the kind once turns the temporal cases into a single elemental
        ! array expression. See row_validity's case 3 for the full reasoning and the measurement.
        select case (self%kind)
        case (PK_DATE)
            valid(1, 1:n) = .not. self%dt(1:n)%is_null()
            return
        case (PK_TIME)
            valid(1, 1:n) = .not. self%tm(1:n)%is_null()
            return
        case (PK_TIMESTAMP)
            valid(1, 1:n) = .not. self%ts(1:n)%is_null()
            return
        case (PK_DATE_VEC)
            valid(1:w, 1:n) = .not. self%dtv(1:w, 1:n)%is_null()
            return
        case (PK_TIME_VEC)
            valid(1:w, 1:n) = .not. self%tmv(1:w, 1:n)%is_null()
            return
        case (PK_TIMESTAMP_VEC)
            valid(1:w, 1:n) = .not. self%tsv(1:w, 1:n)%is_null()
            return
        case (PK_STRING, PK_STRING_VEC)
            ! No elemental form (see row_validity), and no early exit either: unlike the row
            ! summary, every element's own answer is wanted here.
            if (.not. allocated(self%str)) return
            do i = 1_int64, n
                base = (i - 1_int64)*w
                do e = 1_int64, w
                    valid(e, i) = .not. parquet_string_column_is_null(self%str, base + e)
                end do
            end do
            return
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! width is 1 for a container column, so the mask is (1, nrows) and this is the row
            ! arm reshaped -- which is the same answer parquet_column_is_null_elem gives for the
            ! only element index that exists here. See row_validity's own container arm.
            if (.not. allocated(self%container)) return
            do i = 1_int64, n
                valid(1, i) = .not. self%container%is_null_row(i)
            end do
            return
        end select
        if (.not. allocated(self%validity)) return
        nbits = bits_needed(self)
        nblk = min(blocks_for(nbits), size(self%validity, kind=int64))
        do blk = 1_int64, nblk
            word = self%validity(blk)
            if (word == 0_int64) cycle
            base = (blk - 1_int64)*BITS_PER_BLOCK
            ! Set bits only (trailz + ibclr), for the reason row_validity's own loop explains.
            do while (word /= 0_int64)
                p = trailz(word)
                flat = base + int(p, int64)          ! 0-based flat element position
                i = flat/w + 1_int64
                e = mod(flat, w) + 1_int64
                if (i <= n) valid(e, i) = .false.
                word = ibclr(word, p)
            end do
        end do
    end procedure element_validity
    !
    !> Writes a whole per-element validity mask in one pass: `.false.` marks that element null.
    !!
    !! The bulk counterpart of `set_null(i, e)`, and what lets the read path stop widening without
    !! paying for it: replaying a mask element by element would cost `width*nrows` type-bound
    !! calls, where this touches the bitmap directly and allocates it at most once.
    !!
    !! Only ever ADDS nulls. An element whose entry is `.true.` is left exactly as it is, so this
    !! composes with a mask describing only part of what the caller knows and never resurrects a
    !! value that was already null.
    module procedure set_validity_elems
        integer(int64) :: i, e, n, w, k, blk, cur, word
        logical :: any_false
        !
        n = self%nrows
        w = int(self%width, int64)
        if (size(valid, 1, kind=int64) /= w .or. size(valid, 2, kind=int64) /= n) then
            error stop EP//"set_validity: mask shape does not match the column (width, nrows)"
        end if
        if (n <= 0_int64) return
        ! Nothing to record, and in particular no bitmap to allocate -- the common case for a
        ! column whose file reported nulls that the rows actually read do not contain.
        any_false = .not. all(valid)
        if (.not. any_false) return
        select case (self%kind)
        case (PK_NONE)
            ! Unreachable through the public API: `init` rejects PK_NONE outright, so a kindless
            ! column always has nrows == 0 and the `n <= 0` return above fires first. Defensive only.
            error stop EP//"set_validity: column has no kind assigned" ! GCOVR_EXCL_LINE
        case (PK_DATE, PK_TIME, PK_TIMESTAMP, PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC, &
              PK_STRING, PK_STRING_VEC)
            ! No bitmap to write: these carry their null state in the element itself, so a
            ! per-element setter really is the only route. What is avoidable is reaching it
            ! through the generic `%set_null(i, e)`, which re-validates both indices, re-reads
            ! the width and re-dispatches the kind for every null -- so the kind is resolved
            ! once here instead, exactly as row_validity/element_validity now do on the read
            ! side. There is no elemental shortcut in this direction: the setters ARE elemental,
            ! but only a subset of elements is being nulled and Fortran has no masked elemental
            ! call (`where` governs assignment, not a procedure reference).
            select case (self%kind)
            case (PK_DATE)
                do i = 1_int64, n
                    if (.not. valid(1, i)) call self%dt(i)%set_null()
                end do
            case (PK_TIME)
                do i = 1_int64, n
                    if (.not. valid(1, i)) call self%tm(i)%set_null()
                end do
            case (PK_TIMESTAMP)
                do i = 1_int64, n
                    if (.not. valid(1, i)) call self%ts(i)%set_null()
                end do
            case (PK_DATE_VEC)
                do i = 1_int64, n
                    do e = 1_int64, w
                        if (.not. valid(e, i)) call self%dtv(e, i)%set_null()
                    end do
                end do
            case (PK_TIME_VEC)
                do i = 1_int64, n
                    do e = 1_int64, w
                        if (.not. valid(e, i)) call self%tmv(e, i)%set_null()
                    end do
                end do
            case (PK_TIMESTAMP_VEC)
                do i = 1_int64, n
                    do e = 1_int64, w
                        if (.not. valid(e, i)) call self%tsv(e, i)%set_null()
                    end do
                end do
            case default
                ! The string kinds: their nulls live in parquet_string_column's own bitmap,
                ! reached by a flat element index, so this keeps the existing per-element route
                ! minus the dispatch.
                do i = 1_int64, n
                    do e = 1_int64, w
                        if (.not. valid(e, i)) call self%set_null(i, e)
                    end do
                end do
            end select
            ! Resolving the kind once above is what makes this necessary: the six temporal arms
            ! write the element's own null flag directly instead of going through `%set_null`, so
            ! nothing else invalidates the cached "does this column hold a null?" answer. Leaving
            ! it stale makes `%any_null` report a column null-free just after nulls were written
            ! into it, and every bulk validity path short-circuits on that -- so the mask comes
            ! back UNALLOCATED, reaches an optional dummy as absent, and a write drops the nulls
            ! with nothing to report. Kept kind-agnostic rather than repeated per arm so a future
            ! temporal kind cannot be added without it. (The string kinds keep their nulls in the
            ! embedded store and have no cache, so this is a no-op for them.)
            if (is_temporal_kind(self%kind)) self%nulls_dirty = .true.
        case default
            call ensure_bitmap(self)
            ! One store per 64 elements instead of a call per null. The destination bit index runs
            ! monotonically with (i, e), so the word being built is written out only when the walk
            ! crosses into the next one. ORed in, never assigned, because this only ever ADDS
            ! nulls -- see the interface's own note.
            word = 0_int64
            cur = 1_int64
            do i = 1_int64, n
                do e = 1_int64, w
                    k = (i - 1_int64)*w + e - 1_int64
                    blk = k/BITS_PER_BLOCK + 1_int64
                    if (blk /= cur) then
                        if (word /= 0_int64) self%validity(cur) = ior(self%validity(cur), word)
                        word = 0_int64
                        cur = blk
                    end if
                    if (.not. valid(e, i)) word = ibset(word, int(mod(k, BITS_PER_BLOCK)))
                end do
            end do
            if (word /= 0_int64) self%validity(cur) = ior(self%validity(cur), word)
        end select
    end procedure set_validity_elems
    !
    module procedure set_validity_rows
        integer(int64) :: n, w, i, k, blk, cur, word
        character(len=32) :: got, want
        logical :: any_false
        !
        n = self%nrows
        w = int(self%width, int64)
        if (size(valid, kind=int64) /= n) then
            write(got, "(I0)") size(valid, kind=int64)
            write(want, "(I0)") n
            error stop EP//"set_validity: mask has "//trim(got)//" entries but the column has "// &
                trim(want)//" rows"
        end if
        if (n <= 0_int64) return
        any_false = .not. all(valid)
        if (.not. any_false) return
        select case (self%kind)
        case (PK_NONE)
            ! Unreachable, exactly as in set_validity_elems above: a kindless column has no rows.
            error stop EP//"set_validity: column has no kind assigned" ! GCOVR_EXCL_LINE
        case (PK_STRING, PK_STRING_VEC, PK_DATE, PK_TIME, PK_TIMESTAMP, &
              PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC)
            ! No bitmap: these carry their null state in the element itself, so the per-row setter
            ! is the only route -- and it is already the whole-row one, so nothing is lost.
            do i = 1_int64, n
                if (.not. valid(i)) call self%set_null(i)
            end do
        case default
            call ensure_bitmap(self)
            ! A false row marks all w of its elements, which for w > 1 is a short contiguous run;
            ! the word accumulator handles that and the usual w == 1 without a special case.
            word = 0_int64
            cur = 1_int64
            do i = 1_int64, n
                if (.not. valid(i)) then
                    do k = (i - 1_int64)*w, i*w - 1_int64
                        blk = k/BITS_PER_BLOCK + 1_int64
                        if (blk /= cur) then
                            if (word /= 0_int64) self%validity(cur) = ior(self%validity(cur), word)
                            word = 0_int64
                            cur = blk
                        end if
                        word = ibset(word, int(mod(k, BITS_PER_BLOCK)))
                    end do
                end if
            end do
            if (word /= 0_int64) self%validity(cur) = ior(self%validity(cur), word)
        end select
    end procedure set_validity_rows
    !
    !> Marks EVERY element of row `i` null.
    !!
    !! For a bitmap kind this is where the bitmap is lazily allocated (R2 ii) -- the first null
    !! in a column is what makes it exist at all. For a vector kind every element of the row is
    !! marked. Temporal kinds write the element's own null state; string kinds delegate.
    !!
    !! Deliberately whole-row, unlike the row QUERY `is_null(i)` which answers "any element": a
    !! caller naming only a row is saying the row is missing, while a caller asking about a row
    !! wants to know whether anything in it is. `set_null(i, e)` is the way to null one element.
    module procedure parquet_column_set_null_row
        integer(int64) :: e, base, w
        call parquet_column_check_index(col, i, "set_null")
        w = int(col%width, int64)
        select case (col%kind)
        case (PK_STRING)
            call parquet_string_column_set_null(col%str, i)
        case (PK_STRING_VEC)
            base = (i - 1_int64)*w
            do e = 1_int64, w
                call parquet_string_column_set_null(col%str, base + e)
            end do
        case (PK_DATE)
            call col%dt(i)%set_null()
            col%nulls_dirty = .true.
        case (PK_TIME)
            call col%tm(i)%set_null()
            col%nulls_dirty = .true.
        case (PK_TIMESTAMP)
            call col%ts(i)%set_null()
            col%nulls_dirty = .true.
        case (PK_DATE_VEC)
            do e = 1_int64, w
                call col%dtv(e, i)%set_null()
            end do
            col%nulls_dirty = .true.
        case (PK_TIME_VEC)
            do e = 1_int64, w
                call col%tmv(e, i)%set_null()
            end do
            col%nulls_dirty = .true.
        case (PK_TIMESTAMP_VEC)
            do e = 1_int64, w
                call col%tsv(e, i)%set_null()
            end do
            col%nulls_dirty = .true.
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! Delegated for the same reason as `is_null` above: row nullness lives inside the
            ! container, so writing a bit into this column's own bitmap would leave %is_null
            ! still answering .false. -- a mutation that appears to succeed and changes nothing.
            call col%container%set_null_row(i)
        case (PK_NONE)
            ! Unreachable through the public API: check_index above rejects every index on a
            ! kindless column (its row count is 0), so this arm is defensive only.
            error stop EP//"set_null: column has no kind assigned" ! GCOVR_EXCL_LINE
        case default
            call ensure_bitmap(col)
            base = (i - 1_int64)*w
            do e = 1_int64, w
                call bit_set(col%validity, base + e)
            end do
        end select
    end procedure parquet_column_set_null_row
    !
    !> Marks EVERY element of row `i` null (polymorphic form).
    module procedure set_null_row
        call parquet_column_set_null_row(self, i)
    end procedure set_null_row
    !
    !> Marks element `e` of row `i` null, leaving the row's other elements alone.
    !!
    !! Defined on a scalar column too (`width == 1`, so `e` can only be 1), where it is exactly
    !! the row form -- see `is_null_elem` for why that is deliberate rather than an oversight.
    module procedure parquet_column_set_null_elem
        integer(int64) :: w, flat
        call parquet_column_check_index(col, i, "set_null")
        w = int(col%width, int64)
        call parquet_column_check_element(col, e, "set_null")
        flat = (i - 1_int64)*w + e
        select case (col%kind)
        case (PK_STRING, PK_STRING_VEC)
            call parquet_string_column_set_null(col%str, flat)
        case (PK_DATE)
            call col%dt(i)%set_null()
            col%nulls_dirty = .true.
        case (PK_TIME)
            call col%tm(i)%set_null()
            col%nulls_dirty = .true.
        case (PK_TIMESTAMP)
            call col%ts(i)%set_null()
            col%nulls_dirty = .true.
        case (PK_DATE_VEC)
            call col%dtv(e, i)%set_null()
            col%nulls_dirty = .true.
        case (PK_TIME_VEC)
            call col%tmv(e, i)%set_null()
            col%nulls_dirty = .true.
        case (PK_TIMESTAMP_VEC)
            call col%tsv(e, i)%set_null()
            col%nulls_dirty = .true.
        case (PK_NONE)
            ! Unreachable through the public API, exactly as in set_null_row above.
            error stop EP//"set_null: column has no kind assigned" ! GCOVR_EXCL_LINE
        case default
            call ensure_bitmap(col)
            call bit_set(col%validity, flat)
        end select
    end procedure parquet_column_set_null_elem
    !
    !> Marks element `e` of row `i` null, leaving the row's other elements alone
    !! (polymorphic form).
    module procedure set_null_elem
        call parquet_column_set_null_elem(self, i, e)
    end procedure set_null_elem
    !
    !> Marks EVERY element of row `i` valid without writing a value.
    !!
    !! The value behind a previously-null row is unspecified until it is written, so this is
    !! normally used by the value-writing paths rather than called directly. It never drops the
    !! bitmap (a single-cell edit does not pay for a scan, R2 iii) -- use `compact_validity` for
    !! that. On a column that has no bitmap and no nulls it is a no-op.
    !!
    !! Whole-row, mirroring `set_null(i)`; `clear_null(i, e)` clears one element.
    module procedure parquet_column_clear_null_row
        integer(int64) :: e, base, w
        call parquet_column_check_index(col, i, "clear_null")
        w = int(col%width, int64)
        select case (col%kind)
        case (PK_STRING)
            call parquet_string_column_set(col%str, i, "")
        case (PK_STRING_VEC)
            base = (i - 1_int64)*w
            do e = 1_int64, w
                call parquet_string_column_set(col%str, base + e, "")
            end do
        case (PK_DATE, PK_TIME, PK_TIMESTAMP, PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC)
            error stop EP//"clear_null: a temporal element becomes valid by writing a value to it"
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! Unlike a temporal element, a container row CAN be made present again without writing
            ! a value: the row comes back with whatever elements its offsets still describe. See
            ! parquet_list_column%clear_null, which says what that does and does not restore.
            call col%container%clear_null_row(i)
        case (PK_NONE)
            ! Unreachable through the public API: check_index above rejects every index on a
            ! kindless column (its row count is 0), so this arm is defensive only.
            error stop EP//"clear_null: column has no kind assigned" ! GCOVR_EXCL_LINE
        case default
            if (.not. col%has_nulls) return
            base = (i - 1_int64)*w
            do e = 1_int64, w
                call bit_clear(col%validity, base + e)
            end do
        end select
    end procedure parquet_column_clear_null_row
    !
    !> Marks EVERY element of row `i` valid without writing a value (polymorphic form).
    module procedure clear_null_row
        call parquet_column_clear_null_row(self, i)
    end procedure clear_null_row
    !
    !> Marks element `e` of row `i` valid, leaving the row's other elements alone.
    !!
    !! Same rules as the row form: the value behind it is unspecified until written, the bitmap is
    !! never dropped here, and the temporal kinds reject it because a temporal element becomes
    !! valid only by having a value written to it.
    module procedure parquet_column_clear_null_elem
        integer(int64) :: w, flat
        call parquet_column_check_index(col, i, "clear_null")
        w = int(col%width, int64)
        call parquet_column_check_element(col, e, "clear_null")
        flat = (i - 1_int64)*w + e
        select case (col%kind)
        case (PK_STRING, PK_STRING_VEC)
            call parquet_string_column_set(col%str, flat, "")
        case (PK_DATE, PK_TIME, PK_TIMESTAMP, PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC)
            error stop EP//"clear_null: a temporal element becomes valid by writing a value to it"
        case (PK_NONE)
            ! Unreachable through the public API, exactly as in clear_null_row above.
            error stop EP//"clear_null: column has no kind assigned" ! GCOVR_EXCL_LINE
        case default
            if (.not. col%has_nulls) return
            call bit_clear(col%validity, flat)
        end select
    end procedure parquet_column_clear_null_elem
    !
    !> Marks element `e` of row `i` valid, leaving the row's other elements alone
    !! (polymorphic form).
    module procedure clear_null_elem
        call parquet_column_clear_null_elem(self, i, e)
    end procedure clear_null_elem
    !
    !> Scans for remaining nulls and releases the bitmap when there are none (R2 iv).
    !!
    !! This is the explicit, public counterpart to the automatic O(1) drop a whole-column
    !! `set_all` performs: single-cell edits deliberately skip the scan, so a column that has had
    !! its last null overwritten one cell at a time keeps its bitmap until this is called.
    !! Harmless (and cheap) on kinds that carry no bitmap.
    module procedure compact_validity
        if (is_string_kind(self%kind)) return
        if (is_temporal_kind(self%kind)) then
            call rescan_temporal_nulls(self)
            return
        end if
        if (.not. self%has_nulls) return
        if (self%any_null()) return
        call drop_bitmap(self)
    end procedure compact_validity
    !
    !> Whether the column holds at least one null, without writing to it. The interface in
    !! `parquet_columns.f90` carries the contract; two implementation notes belong here.
    !!
    !! The temporal arm reads `nulls_dirty` and `nulls_cached` without synchronisation, which is
    !! sound in a way the type-bound `any_null` is not. Both are plain scalars written as whole
    !! words, so a concurrent reader sees the value before or after another thread's write and
    !! never a mixture -- and BOTH are legitimate snapshots of a column something is mutating at
    !! that moment. What made `any_null` unsafe was never the reads; it was the write that
    !! discarded another thread's `nulls_dirty = .true.` (feature_risks.md Risk-136).
    !!
    !! It calls `parquet_column_is_null`, not `col%is_null`, because a type-bound call on a
    !! non-polymorphic dummy is the conversion the typed tier exists to remove -- enforced by
    !! `check_no_type_bound_column_access` for every `parquet_column_*` procedure.
    module procedure parquet_column_any_null
        integer(int64) :: k, nbits, nblk
        res = .false.
        if (col%nrows <= 0_int64) return
        if (is_string_kind(col%kind)) then
            if (allocated(col%str)) res = parquet_string_column_null_count(col%str) > 0_int64
            return
        end if
        if (is_temporal_kind(col%kind)) then
            if (.not. col%nulls_dirty) then
                res = col%nulls_cached
                return
            end if
            do k = 1_int64, col%nrows
                if (parquet_column_is_null(col, k)) then
                    res = .true.
                    return
                end if
            end do
            return
        end if
        ! The FOURTH dispatch class, and the one this procedure was missing. Row nullness lives
        ! inside the container, so `has_nulls` is deliberately .false. for a container column
        ! (adopt_container says why) -- and falling through to the test below therefore answered
        ! "no nulls" for a column whose own %is_null(i) answers .true., which is two public
        ! queries disagreeing about the same row with nothing to announce it. Scanned rather than
        ! cached because there is no bitmap to skip whole words in, exactly as the temporal arm
        ! above scans; the early exit is what keeps the common case cheap.
        if (parquet_kind_is_container(col%kind)) then
            if (.not. allocated(col%container)) return
            do k = 1_int64, col%nrows
                if (col%container%is_null_row(k)) then
                    res = .true.
                    return
                end if
            end do
            return
        end if
        if (.not. col%has_nulls) return
        if (.not. allocated(col%validity)) return
        nbits = bits_needed(col)
        nblk = min(blocks_for(nbits), size(col%validity, kind=int64))
        do k = 1_int64, nblk
            if (col%validity(k) /= 0_int64) then
                res = .true.
                return
            end if
        end do
    end procedure parquet_column_any_null
    !
    !> Recomputes a temporal column's cached "has at least one null" flag (the O(n) scan the
    !! cache exists to avoid repeating).
    !!
    !! **Writes to the column, so only `any_null` and `compact_validity` may reach it** -- both
    !! are declared `intent(inout)` and neither is on a read path. Clearing `nulls_dirty` at the
    !! end is what discards a concurrent `set_null`'s dirty flag; see feature_risks.md Risk-136.
    subroutine rescan_temporal_nulls(self)
        class(parquet_column), intent(inout) :: self !! the temporal column.
        integer(int64) :: k
        self%nulls_cached = .false.
        do k = 1_int64, self%nrows
            if (self%is_null(k)) then
                self%nulls_cached = .true.
                exit
            end if
        end do
        self%nulls_dirty = .false.
    end subroutine rescan_temporal_nulls
    !
end submodule parquet_columns_validity
