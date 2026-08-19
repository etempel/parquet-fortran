!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Turning a parquet file's columns into `parquet_column` stores: deciding each column's kind
!! from the file schema, driving the per-kind readers, and freeing the Arrow buffers as it goes.
!!
!! The work is split in two on purpose. **Classification** (`table_classify`) settles a column's
!! kind, width and readability from the file SCHEMA alone and runs for every column at open time;
!! it reads no data, which is what lets `%kind`/`%width` answer about a column nobody has touched.
!! **Materialization** (`table_materialize`) reads the values, and happens on first touch.
!!
!! The memory rule this file exists to enforce: the table owns the sole Fortran-side copy of each
!! column, and the reader's decoded Arrow array is released the moment that copy exists. Without
!! that release a materialized table would hold the whole file twice for the reader's entire
!! lifetime. A struct is cached by the reader as ONE array covering all its leaves, which is why
!! there are two release policies rather than one -- see `table_materialize_all`.
submodule (parquet_tables) parquet_tables_read
    implicit none
    !
    !> Below this much work in ONE column, its read stays a single whole-column call rather than
    !! being split across its row groups (`parallel_colread_ok`).
    !!
    !! Measured in ELEMENTS (`rows * width`), matching `colwork_min_elements`
    !! (`src/parquet_tables_parallel.f90`) and for the same reason: `parquet_column` exposes no byte
    !! size, and deriving one here would duplicate the kind-to-size table that lives in the
    !! generator. The two floors are deliberately the same number -- both guard "is one column's
    !! worth of work big enough to be worth a thread team" -- but they are separate constants
    !! because they gate different operations and a future measurement could reasonably move one
    !! without the other.
    !!
    !! **This one is charged with more than a team spawn**, which is why it is not smaller: each
    !! thread also opens its own `parquet_reader`, and that parses the file footer. Below roughly a
    !! megabyte of column that is the dominant cost and the split loses.
    !!
    !! **Not a public setting** -- `parquet_set_prefetch_threads` is already the user-facing control
    !! over this read. It IS overridable through a test-only hook; see `colread_gate_limit`.
    integer(int64), parameter :: colread_min_elements = 131072_int64

    !
contains
    !
    module procedure table_kind_from_type
        ! parquet_get_column_type reports the ELEMENT type of a vector column, so col_size is
        ! what decides between the scalar and *_VEC form of each kind.
        logical :: vec
        !
        ok = .true.
        vec = col_size > 1
        select case (trim(type_name))
        case ("int32")
            kind = merge(PK_INT32_VEC, PK_INT32, vec)
        case ("int64")
            kind = merge(PK_INT64_VEC, PK_INT64, vec)
        case ("float32")
            kind = merge(PK_FLOAT32_VEC, PK_FLOAT32, vec)
        case ("float64")
            kind = merge(PK_FLOAT64_VEC, PK_FLOAT64, vec)
        case ("boolean")
            kind = merge(PK_LOGICAL_VEC, PK_LOGICAL, vec)
        case ("string")
            kind = merge(PK_STRING_VEC, PK_STRING, vec)
        case ("date")
            kind = merge(PK_DATE_VEC, PK_DATE, vec)
        case ("time")
            kind = merge(PK_TIME_VEC, PK_TIME, vec)
        case ("timestamp")
            kind = merge(PK_TIMESTAMP_VEC, PK_TIMESTAMP, vec)
        case default
            ! Both call sites (table_classify, table_resolve_width) only ever pass a type_name
            ! that already came back from parquet_get_column_type, which itself either returns
            ! one of exactly these 9 canonical tokens or error stops -- see
            ! "parquet_get_column_type on a column outside the 9 canonical types aborts" in
            ! test_errors.f90. There is no path that reaches this select with any other token.
            kind = PK_NONE ! GCOVR_EXCL_LINE
            ok = .false. ! GCOVR_EXCL_LINE -- gcov attribution artifact: shows a large positive
                ! hit count under -O0 despite gcov's own unexecuted_block flag agreeing with
                ! kind = PK_NONE above (0 hits) that this branch is never actually entered.
        end select
    end procedure table_kind_from_type
    !
    module procedure table_classify
        character(len=:), allocatable :: type_name
        integer :: kind, col_size
        logical :: ok
        !
        associate (slot => cache%cols(idx))
            ! Ask whether the type is readable BEFORE asking what it is: parquet_get_column_type
            ! error stops on a type outside its nine canonical tokens, so probing with
            ! parquet_column_exists(types=...) first is what keeps one exotic column from making
            ! the whole file unopenable.
            if (.not. parquet_column_exists(cache%reader, slot%file_name, &
                    types="int32,int64,float32,float64,string,boolean,date,time,timestamp")) then
                slot%supported = .false.
                slot%declared_kind = PK_NONE
                slot%width = 1
                slot%residency = RES_EMPTY
                return
            end if
            call parquet_get_column_type(cache%reader, slot%file_name, type_name)
            ! Supportedness comes from the element type alone -- table_kind_from_type only uses
            ! col_size to pick between a kind and its *_VEC form -- so it can be settled here even
            ! for a column whose width is not knowable yet.
            call table_kind_from_type(type_name, 1, kind, ok)
            if (.not. ok) then
                ! Not reachable: the parquet_column_exists(types=...) probe above already
                ! restricted type_name to the same 9 tokens table_kind_from_type recognizes, so
                ! ok is always .true. here. Kept as a second line of defense rather than an
                ! assertion, matching this procedure's own early-return shape above.
                slot%supported = .false. ! GCOVR_EXCL_LINE
                slot%declared_kind = PK_NONE ! GCOVR_EXCL_LINE
                slot%width = 1 ! GCOVR_EXCL_LINE
                slot%residency = RES_EMPTY ! GCOVR_EXCL_LINE
                return ! GCOVR_EXCL_LINE
            end if
            slot%supported = .true.
            slot%residency = RES_EMPTY
            ! A plain LIST/LARGE_LIST is the one type whose width lives in the data rather than the
            ! schema, so it is DEFERRED rather than measured here: classifying it now would mean
            ! decoding the column at open, which is exactly what makes a lazy open worthless. Its
            ! kind and width are resolved by table_resolve_width on first use.
            if (parquet_column_width_needs_data(cache%reader, slot%file_name)) then
                slot%declared_kind = PK_NONE
                slot%width = 0
                slot%width_pending = .true.
                return
            end if
            ! Every remaining type -- scalar or FIXED_SIZE_LIST -- answers from the schema, free.
            call parquet_get_col_size(cache%reader, slot%file_name, col_size)
            call table_kind_from_type(type_name, col_size, kind, ok)
            slot%declared_kind = kind
            slot%width = max(col_size, 1)
            call record_temporal_unit(cache, slot, type_name)
        end associate
    end procedure table_classify
    !
    !> Records a TIME/TIMESTAMP column's stored resolution (and, for a timestamp, its timezone
    !! flag) on the descriptor, while the reader is still there to answer.
    !!
    !! This has to happen at classification time and nowhere later, because nothing else in the
    !! table remembers it: a `parquet_timestamp` stores seconds+nanoseconds and carries no unit,
    !! and the descriptor's own `unit` is the physical unit (`"Msun"`), not the temporal
    !! resolution. A schema-less write reads it back to emit the matching `timestamp[ns]`-style
    !! data_type token; without it the writer would default to microseconds and a nanosecond
    !! column would fail the write.
    subroutine record_temporal_unit(cache, slot, type_name)
        type(parquet_table_cache), intent(inout) :: cache      !! the column store (for the reader).
        type(parquet_table_column), intent(inout) :: slot      !! descriptor being classified.
        character(len=*), intent(in) :: type_name              !! canonical type token from the file.
        character(len=:), allocatable :: tz
        !
        ! DATE has no unit (it is a day count) and no timezone, so it is deliberately not asked
        ! about -- parquet_get_column_time_info aborts on a non-time/timestamp column.
        if (trim(type_name) /= "time" .and. trim(type_name) /= "timestamp") return
        call parquet_get_column_time_info(cache%reader, slot%file_name, slot%time_unit, tz)
        slot%time_utc = len_trim(tz) > 0
    end subroutine record_temporal_unit
    !
    module procedure table_materialize
        associate (slot => cache%cols(idx))
            ! A parquet file records no unit for a column, so the only source is the read-in
            ! MAML's `unit:` key, captured onto the descriptor at open. It is applied AFTER the
            ! read rather than passed into it, because materialize_slice assembles the column from
            ! several row-group pieces and would have to thread it through each of them.
            ! The two arms differ only in which reader they drive. Written out rather than
            ! aliased with a pointer because `cache` is a plain dummy here, so a pointer to its
            ! allocatable reader component is not permitted.
            if (sc%regime == REGIME_SLICE) then
                if (present(rdr)) then
                    call materialize_slice(cache, sc, idx, rdr)
                else
                    call materialize_slice(cache, sc, idx)
                end if
            else if (present(rdr)) then
                ! `rdr` present means this call is ALREADY inside the parallel-over-columns region,
                ! so the row-group split below must not even be considered -- that is what makes
                ! the two parallel read paths mutually exclusive by construction rather than by a
                ! flag someone has to remember to pass.
                call table_materialize_kind(slot%declared_kind, rdr, slot%file_name, &
                    slot%values, sc%nrows, int(slot%width, int32), "")
            else if (.not. materialize_column_parallel(cache, sc, idx)) then
                call table_materialize_kind(slot%declared_kind, cache%reader, slot%file_name, &
                    slot%values, sc%nrows, int(slot%width, int32), "")
            end if
            if (allocated(slot%unit)) call slot%values%set_unit(slot%unit)
            slot%residency = RES_FULL
            ! A deferred %cast is carried out by this read and by nothing else: `declared_kind`
            ! was rewritten when it was asked for, and the per-kind readers above have just
            ! decoded the file column straight into that kind. Clearing the flag here is what
            ! makes the deferral invisible -- the slot is now in exactly the state an eager cast
            ! would have left it in.
            slot%cast_pending = .false.
        end associate
        cache%reads_started = .true.
    end procedure table_materialize
    !
    !> Reads ONE column by splitting its row groups across threads, each with its own reader.
    !! Answers `.false.` without touching anything when the gate declines, so the caller falls
    !! through to the ordinary whole-column read.
    !!
    !! **The full regime only.** A slice already assembles its column from row-group pieces
    !! (`materialize_slice`), and giving that a second, parallel shape would double the surface where
    !! the slice's own trimming arithmetic has to be right. `materialize_slice`'s pieces are already
    !! a natural place to parallelise later; it is deliberately not done here.
    !!
    !! **The bounds are built locally, not cached.** `cache%rg_bounds` is a slice-regime object --
    !! `%row_group_bounds` branches on whether it is allocated -- so filling it in for a full table
    !! would change a public answer as a side effect of an internal optimisation. Building them here
    !! is a footer walk (`parquet_get_num_row_groups` plus one `parquet_get_chunk_size` per group)
    !! against a read this gate has already judged large enough to be worth splitting.
    !!
    !! **The column is sized ONCE and every row group pastes into its own disjoint range.** That is
    !! what makes the region safe with no lock and no per-thread staging: `grow_storage` reallocates
    !! exact-fit, so growing per row group would be quadratic *and* would make the threads contend
    !! over one allocation. Sizing up front removes both problems at once.
    logical function materialize_column_parallel(cache, sc, idx) result(did)
#ifdef _OPENMP
        use omp_lib, only : omp_get_thread_num
#endif
        type(parquet_table_cache), intent(inout) :: cache !! the column store.
        type(table_scope), intent(in) :: sc               !! rows this table covers.
        integer, intent(in) :: idx                        !! slot to fill.
        integer(int64) :: nrg
#ifdef _OPENMP
        integer(int64), allocatable :: bounds(:,:)
        integer :: nslots, t
        integer(int64) :: rg
        logical :: needs_bitmap
        type(parquet_reader), allocatable :: readers(:)
        type(parquet_column), allocatable :: chunks(:)
        logical, allocatable :: reader_open(:)
#endif
        !
        did = .false.
#ifdef _OPENMP
        ! Cheapest first: the row-group count is one footer field, and every other clause is free
        ! once it is known. Asking it before per_thread_readers_ok would invert that.
        if (.not. per_thread_readers_ok(cache, sc)) return
        call parquet_get_num_row_groups(cache%reader, nrg)
        if (.not. parallel_colread_ok(cache, sc, idx, nrg)) return
        call reader_row_group_bounds(cache%reader, bounds)
        !
        nslots = prefetch_thread_count()
        ! One slot per thread the region could possibly use, allocated up front so the region only
        ! ever indexes an existing element. `chunks` is here for the same reason `readers` is: a
        ! derived type with ALLOCATABLE COMPONENTS declared in a block lexically inside a parallel
        ! region makes ifx emit privatization scaffolding (`mold_ctor` -> `for_alloc_private` ->
        ! `do_alloc_copy`) that segfaults at -O1+ on every thread entering the region --
        ! feature_risks.md Risk-45. Reusing one chunk per thread across the row groups the
        ! scheduler hands it is also what `materialize_slice` does serially, so the chunk's own
        ! contract (each `table_materialize_chunk_kind` call resizes it) is already relied on.
        allocate(readers(nslots))
        allocate(chunks(nslots))
        allocate(reader_open(nslots))
        reader_open = .false.
        call parquet_debug_note_colread_threads(int(min(int(nslots, int64), nrg), int64))
        associate (slot => cache%cols(idx))
            call slot%values%init(slot%declared_kind, sc%nrows, int(slot%width, int32), "")
            ! VALIDITY IS ALLOCATED LAZILY, SO THE FIRST NULL WOULD RACE. `%paste` calls
            ! `ensure_bitmap` whenever its source chunk carries a null, and that allocates on first
            ! use -- so two threads pasting null-carrying row groups would both find the bitmap
            ! unallocated and both allocate it. This is the hazard `%ensure_validity` exists for,
            ! reached from inside the library rather than left to the caller, who never asked for
            ! this parallelism and cannot see it. No test can catch its absence -- the window is a
            ! few instructions wide and the mutation passes every time here -- so see
            ! feature_risks.md Risk-57 before removing it.
            !
            ! Asked of the FOOTER first (`parquet_column_has_nulls`, the same query the ordinary
            ! whole-column read uses to skip building a mask at all), so a null-free column -- the
            ! common case, and the one this path is fastest on -- still allocates nothing. "Might
            ! have nulls" is that query's uncertain answer, which is the safe direction here too:
            ! a bitmap nobody needed costs one bit per row.
            needs_bitmap = parquet_column_has_nulls(cache%reader, slot%file_name, 0_int64, 0_int64)
            if (needs_bitmap) call slot%values%ensure_validity()
        end associate
        ! THE ASSOCIATE ABOVE MUST END BEFORE THE REGION, AND THE ONE BELOW IS ITS REPLACEMENT --
        ! do not merge them back into a single associate spanning the `!$omp parallel do`. NAG 7.2
        ! (Build 7244) generates invalid intermediate C for an ASSOCIATE name referenced inside an
        ! OpenMP construct when the association was established outside it: the association's
        ! temporary is used in the outlined region but never declared there, so the host C compiler
        ! rejects nagfor's OWN output ("error: use of undeclared identifier 'slot_'"), which reads
        ! like a defect in this file and is not. nagfor warns first -- "ASSOCIATE name SLOT used in
        ! nested OpenMP construct" -- and that warning is the only tell. gfortran, ifx and flang all
        ! accept the spanning form, so this split is a NAG workaround rather than a correctness
        ! requirement; it costs nothing (identical indentation, one extra association per iteration
        ! that every compiler folds away) and a pointer would work equally well. See
        ! feature_nag_associate.md for the minimal reproducer and the NAG support report.
        !$omp parallel do default(shared) private(rg, t) schedule(dynamic) num_threads(nslots)
        do rg = 1_int64, nrg
            block
                ! Plain locals ONLY -- a derived type with allocatable components declared in a
                ! block lexically inside a parallel region segfaults ifx at -O1+
                ! (feature_risks.md Risk-45), which is why `readers` and `chunks` are arrays
                ! allocated before the region instead.
                integer(int64) :: rows_rg
                !
                rows_rg = bounds(2, rg) - bounds(1, rg) + 1_int64
                ! A row group a filter or sample emptied contributes nothing and must be
                ! stepped over -- pasting a zero-row chunk is not merely wasteful, it would
                ! ask %paste for an empty range at a valid position.
                if (rows_rg > 0_int64) then
                    associate (slot => cache%cols(idx))
                        t = omp_get_thread_num() + 1
                        if (.not. reader_open(t)) then
                            ! Through the same helper parquet_open_table uses, so this thread's
                            ! reader carries the table's qc/sample exactly as the table's own does.
                            call table_open_reader_with_transform(cache, cache%source_file, readers(t))
                            reader_open(t) = .true.
                        end if
                        call table_materialize_chunk_kind(slot%declared_kind, readers(t), &
                            slot%file_name, rg, chunks(t), rows_rg, int(slot%width, int32), "")
                        ! The ROWS are disjoint by construction -- row group rg owns table rows
                        ! bounds(1,rg)..bounds(2,rg) and no other row group owns any of them -- but
                        ! disjoint rows are NOT disjoint validity bits, which is what
                        ! paste_row_group_safely exists for. See its own comment.
                        call paste_row_group_safely(slot%values, chunks(t), bounds(1, rg), &
                            bounds(2, rg), int(slot%width, int64), needs_bitmap)
                        call chunks(t)%clear()
                    end associate
                end if
            end block
        end do
        !$omp end parallel do
        do t = 1, nslots
            if (reader_open(t)) call parquet_close_reader(readers(t))
        end do
        did = .true.
#endif
    end function materialize_column_parallel
    !
    !> The sub-range of rows `lo..hi` whose validity bits occupy **whole** bitmap blocks, so that
    !! pasting it cannot touch a block any neighbouring row range also writes. `mid_lo > mid_hi`
    !! means no such sub-range exists and the caller must serialise the whole paste.
    !!
    !! **This is the arithmetic behind a silent wrong answer, so it is a separate, pure procedure
    !! that can be tested on its own** — the race it prevents is a few instructions wide and an
    !! end-to-end test cannot be relied on to see it (`feature_risks.md` Risk-61 records the same
    !! division of labour for `parquet_string_column`'s own byte-aligned split).
    !!
    !! `parquet_column` packs validity as a bitmap indexed by ELEMENT, `parquet_validity_block_bits`
    !! of them to a block. **That constant is imported, never copied** — a second definition of it
    !! here could drift from the real one with nothing to report it, and the failure would be a
    !! silent wrong answer rather than a build error. `parquet_columns` publishes it for this caller
    !! specifically.
    !!
    !! Row boundary `r` sits on a block boundary exactly when `(r-1)*width` is a multiple of the
    !! block, so aligned boundaries repeat every `block/gcd(width, block)` rows — every 64 rows for a
    !! scalar column, and as often as every row for a width that is itself a multiple of the block.
    pure subroutine bitmap_whole_block_rows(lo, hi, width, mid_lo, mid_hi)
        integer(int64), intent(in) :: lo     !! first row of the range.
        integer(int64), intent(in) :: hi     !! last row of the range.
        integer(int64), intent(in) :: width  !! elements per row.
        integer(int64), intent(out) :: mid_lo !! first row of the whole-block sub-range.
        integer(int64), intent(out) :: mid_hi !! last row of it; < mid_lo when there is none.
        integer(int64) :: period, a, b
        mid_lo = 1_int64
        mid_hi = 0_int64
        if (hi < lo) return
        if (width <= 0_int64) return
        period = parquet_validity_block_bits/gcd_int64(width, parquet_validity_block_bits)
        ! Round `lo` up and `hi` down onto that period.
        a = lo + modulo(-(lo - 1_int64), period)
        b = hi - modulo(hi, period)
        if (a > b) return
        mid_lo = a
        mid_hi = b
    end subroutine bitmap_whole_block_rows
    !
    !> Test-only view of `bitmap_whole_block_rows`; see the interface in `parquet_tables.f90`.
    module procedure parquet_debug_colread_block_rows
        call bitmap_whole_block_rows(lo, hi, width, mid_lo, mid_hi)
    end procedure parquet_debug_colread_block_rows
    !
    !> Greatest common divisor; Fortran has no intrinsic for it.
    pure integer(int64) function gcd_int64(a, b) result(g)
        integer(int64), intent(in) :: a !! first value (> 0).
        integer(int64), intent(in) :: b !! second value (> 0).
        integer(int64) :: x, y, r
        x = abs(a)
        y = abs(b)
        do while (y /= 0_int64)
            r = modulo(x, y)
            x = y
            y = r
        end do
        g = max(x, 1_int64)
    end function gcd_int64
    !
    !> Pastes one row group's chunk into its place in a column being filled by several threads.
    !!
    !! **Disjoint ROWS are not disjoint validity BITS, and that difference is a silent wrong
    !! answer.** `parquet_column`'s validity is a bit-packed `integer(int64)` map, so
    !! many elements share one block and `%paste` updates a block with a read-modify-write. A
    !! row-group boundary almost never lands on a block boundary — for a
    !! 25,000-row group it never does — so the thread finishing row group `g` and the thread
    !! starting `g+1` both read, modify and write the *same* block, and one update is lost. The
    !! column still validates, the row count is right, and some row's null flag is simply wrong.
    !! This was observed once in 25 full test runs before it was diagnosed.
    !!
    !! The fix keeps the parallelism: the middle of the range occupies whole blocks and is pasted
    !! freely, while only the ragged ends — under one bitmap block each — go through a
    !! critical section shared by every thread. Serialising the *whole* paste would also be correct
    !! and was measured at roughly **5x slower** on a null-carrying column, because the validity
    !! write is about two thirds of this operation's cost.
    !!
    !! **`no_bitmap` is not an optimisation, it is the common case**: a column the footer says has no
    !! nulls never allocates a bitmap, so `%paste` writes no validity at all and there is nothing to
    !! serialise.
    subroutine paste_row_group_safely(dst, chunk, lo, hi, width, has_bitmap)
        type(parquet_column), intent(inout) :: dst !! the column being filled.
        type(parquet_column), intent(in) :: chunk  !! this row group's decoded rows.
        integer(int64), intent(in) :: lo           !! first destination row this chunk owns.
        integer(int64), intent(in) :: hi           !! last destination row this chunk owns.
        integer(int64), intent(in) :: width        !! elements per row.
        logical, intent(in) :: has_bitmap          !! whether a validity bitmap exists to race on.
        integer(int64) :: mid_lo, mid_hi
        if (.not. has_bitmap) then
            call dst%paste(chunk, lo)
            return
        end if
        call bitmap_whole_block_rows(lo, hi, width, mid_lo, mid_hi)
        if (mid_lo > mid_hi) then
            ! No whole block anywhere in this range -- a very short row group. Serialise all of it.
            !$omp critical (colread_bitmap)
            call dst%paste(chunk, lo)
            !$omp end critical (colread_bitmap)
            return
        end if
        if (mid_lo > lo) then
            !$omp critical (colread_bitmap)
            call dst%paste(chunk, lo, 1_int64, mid_lo - lo)
            !$omp end critical (colread_bitmap)
        end if
        call dst%paste(chunk, mid_lo, mid_lo - lo + 1_int64, mid_hi - mid_lo + 1_int64)
        if (mid_hi < hi) then
            !$omp critical (colread_bitmap)
            call dst%paste(chunk, mid_hi + 1_int64, mid_hi - lo + 2_int64, hi - mid_hi)
            !$omp end critical (colread_bitmap)
        end if
    end subroutine paste_row_group_safely
    !
    !> Assembles one column from just the row groups covering the table's slice.
    !!
    !! The full regime reads a column in one call; a slice cannot, because a whole-column read
    !! would decode every row group in the file -- the very cost the slice regime exists to
    !! avoid. So each covering row group is read on its own and placed into the slice's column.
    !!
    !! The column is sized ONCE, up front, and each row group is pasted into its place: the row
    !! count is `sc%nrows`, which is known before any data is read. Growing it with `append`
    !! instead is quadratic -- every append reallocates the column exact-fit and copies
    !! everything already in it (`grow_storage`), so assembling k row groups copies k*(k-1)/2
    !! chunks' worth of data and allocates far more memory than the finished column occupies. On
    !! a 16-column, quarter-of-8-GB slice spanning 8 row groups that cost as much as reading the
    !! whole file. Trimming the first and last row group to the slice bounds is likewise just the
    !! sub-range `paste` is asked for, rather than a `keep` mask plus `delete_by_mask`.
    !!
    !! The string kinds cannot be pasted (a variable-length store has no fixed row slots), so
    !! they keep the grow-and-append shape -- which costs them nothing, because
    !! `parquet_string_column` grows its buffers geometrically rather than exact-fit.
    subroutine materialize_slice(cache, sc, idx, rdr)
        type(parquet_table_cache), intent(inout) :: cache !! the column store.
        type(table_scope), intent(in) :: sc               !! rows this table covers.
        integer, intent(in) :: idx                        !! slot to fill.
        type(parquet_column) :: chunk
        type(parquet_reader), intent(inout), optional :: rdr !! reader override; see table_materialize.
        logical, allocatable :: keep(:)
        character(len=:), allocatable :: sfx
        integer(int64) :: rg, rg_lo, rg_hi, lo, hi, rows_rg, cursor, take
        logical :: in_place
        !
        associate (slot => cache%cols(idx), bounds => cache%rg_bounds)
            in_place = .not. (slot%declared_kind == PK_STRING .or. slot%declared_kind == PK_STRING_VEC)
            if (in_place) then
                call slot%values%init(slot%declared_kind, sc%nrows, int(slot%width, int32), "")
            else
                call slot%values%init(slot%declared_kind, 0_int64, int(slot%width, int32), "")
            end if
            cursor = 1_int64
            do rg = 1_int64, size(bounds, 2, kind=int64)
                rg_lo = bounds(1, rg)
                rg_hi = bounds(2, rg)
                if (rg_hi < sc%row_lo .or. rg_lo > sc%row_hi) cycle
                rows_rg = rg_hi - rg_lo + 1_int64
                if (present(rdr)) then
                    call table_materialize_chunk_kind(slot%declared_kind, rdr, &
                        slot%file_name, rg, chunk, rows_rg, int(slot%width, int32), "")
                else
                    call table_materialize_chunk_kind(slot%declared_kind, cache%reader, &
                        slot%file_name, rg, chunk, rows_rg, int(slot%width, int32), "")
                end if
                lo = max(sc%row_lo, rg_lo)
                hi = min(sc%row_hi, rg_hi)
                take = hi - lo + 1_int64
                if (in_place) then
                    call slot%values%paste(chunk, cursor, lo - rg_lo + 1_int64, take)
                else
                    if (lo > rg_lo .or. hi < rg_hi) then
                        allocate(keep(rows_rg))
                        keep = .false.
                        keep(lo - rg_lo + 1_int64:hi - rg_lo + 1_int64) = .true.
                        call chunk%delete_by_mask(keep)
                        deallocate(keep)
                    end if
                    call slot%values%append(chunk)
                end if
                cursor = cursor + take
                call chunk%clear()
            end do
            ! A preallocated column that the row groups did not completely fill would hand back
            ! uninitialized values as if they had been read, so say so instead. Not reachable
            ! through any public path: cache%rg_bounds is built from the file footer and the scope
            ! was validated against it at open time, so the covering row groups always tile the
            ! slice exactly. Kept as an assertion because the failure it guards is silent.
            if (cursor - 1_int64 /= sc%nrows) then
                call table_context_suffix(cache, slot%name, sfx) ! GCOVR_EXCL_LINE
                error stop EP // "materialize: the row groups covering this slice do not " // & ! GCOVR_EXCL_LINE
                    "cover every one of its rows" // sfx ! GCOVR_EXCL_LINE
            end if
        end associate
    end subroutine materialize_slice
    !
    module procedure table_release_one
        character(len=:), allocatable :: top
        !
        call top_level_of(name, top)
        if (present(rdr)) then
            call parquet_release_column(rdr, top)
        else
            call parquet_release_column(cache%reader, top)
        end if
    end procedure table_release_one
    !
    module procedure record_open_thread
#ifdef _OPENMP
        use omp_lib, only : omp_in_parallel, omp_get_thread_num
        cache%opened_in_parallel = omp_in_parallel()
        if (cache%opened_in_parallel) then
            cache%owner_thread = omp_get_thread_num()
        else
            cache%owner_thread = -1
        end if
#else
        cache%opened_in_parallel = .false.
        cache%owner_thread = -1
#endif
    end procedure record_open_thread
    !
    module procedure unsafe_first_touch
#ifdef _OPENMP
        use omp_lib, only : omp_in_parallel, omp_get_thread_num
        unsafe = .false.
        if (.not. omp_in_parallel()) return
        ! A table this very thread opened inside the region cannot be shared with another thread
        ! -- that is the shape of a parallel per-slice program (each thread opens its own table
        ! over its own row range), and refusing it would make the slice regime unusable exactly
        ! where it is most useful. Anything else may be shared, and a first touch on a shared
        ! store is what RF4 forbids.
        unsafe = .not. (cache%opened_in_parallel .and. cache%owner_thread == omp_get_thread_num())
#else
        unsafe = .false.
#endif
    end procedure unsafe_first_touch
    !
    module procedure table_resolve_width
        character(len=:), allocatable :: type_name, sfx
        integer(int64) :: rg_lo, rg_hi
        integer :: kind, w
        logical :: ok
        !
        associate (slot => cache%cols(idx))
            if (.not. slot%width_pending) return
            ! Resolving reads data, so a detached table cannot do it at all -- and this guard has
            ! to be here as well as in table_touch, because %kind and %width reach this helper
            ! directly without going through it.
            call table_check_not_detached(cache, sc, slot%name, proc)
            ! Resolving reads data, so it is a first touch as far as RF4 is concerned even when it
            ! does not materialize anything -- a shared store must not have it happen concurrently.
            if (unsafe_first_touch(cache)) then
                call table_context_suffix(cache, slot%name, sfx)
                error stop EP // trim(proc) // ": this column's width is not known yet, and " // &
                    "finding it means reading the column; do it before the parallel region " // &
                    "(%kind, %width, %prefetch or %materialize_all)" // sfx
            end if
            call resolve_width_row_groups(cache, sc, rg_lo, rg_hi)
            call parquet_measure_list_width(cache%reader, slot%file_name, rg_lo, rg_hi, proven, w)
            call parquet_get_column_type(cache%reader, slot%file_name, type_name)
            call table_kind_from_type(type_name, w, kind, ok)
            slot%declared_kind = kind
            ! An empty column measures as 0; a descriptor width is always at least 1, exactly as
            ! table_classify's own max(col_size, 1) does for the schema-known kinds.
            slot%width = max(w, 1)
            slot%width_pending = .false.
        end associate
    end procedure table_resolve_width
    !
    !> The 1-based inclusive row-group range a scope covers, as table_resolve_width's measurement
    !> bounds. 0/0 means "every row group" -- which is both what the whole-file regime wants and
    !> what the C++ side reads as "no range given".
    subroutine resolve_width_row_groups(cache, sc, rg_lo, rg_hi)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        type(table_scope), intent(in) :: sc            !! rows this table covers.
        integer(int64), intent(out) :: rg_lo           !! first row group, or 0 for all.
        integer(int64), intent(out) :: rg_hi           !! last row group, or 0 for all.
        !
        rg_lo = 0_int64
        rg_hi = 0_int64
        if (sc%regime /= REGIME_SLICE) return
        if (.not. allocated(cache%rg_bounds)) return
        ! Both sides of this comparison are in the table's own coordinates, on a masked slice as
        ! much as an unmasked one -- which is what lets the same scan serve both.
        call rg_covering_range(cache%rg_bounds, sc%row_lo, sc%row_hi, rg_lo, rg_hi)
    end subroutine resolve_width_row_groups
    !
    module procedure table_touch
        character(len=:), allocatable :: sfx
        !
        if (cache%cols(idx)%residency == RES_FULL) return
        ! Already-resident reads never get here, which is the point: they take no lock and run
        ! fully parallel. A FIRST touch is different -- it allocates and publishes shared state
        ! with no ordering guarantee behind it, so another thread could see RES_FULL before the
        ! values it advertises are visible. Rather than build double-checked locking around a
        ! read path that must stay free, v1 forbids the situation outright and says how to avoid
        ! it (RF4). A table the touching thread opened inside the region is exempt -- see
        ! unsafe_first_touch.
        if (unsafe_first_touch(cache)) then
            call table_context_suffix(cache, cache%cols(idx)%name, sfx)
            error stop EP // trim(proc) // ": this column was not read before the parallel " // &
                "region; call table%prefetch(...) or table%materialize_all() before it. Note " // &
                "%kind and %width count as a read for a variable-length LIST column, whose " // &
                "width can only be found by reading it" // sfx
        end if
        associate (slot => cache%cols(idx))
            ! Not reachable yet: every current way to create an in-memory slot (table_new_slot,
            ! called from %add_column/%copy_column) populates it and sets residency = RES_FULL
            ! in the same procedure call, with no intervening point where table_touch could see
            ! it still RES_EMPTY. This guard exists for the predefined/generated-table-type
            ! columns described elsewhere in this file (parquet_tables_mutate.f90's own
            ! `predefined` guards on %drop_column/%rename_column) -- a later milestone's
            ! generated accessor could declare a column before it is ever populated or read from
            ! a file, and that is exactly the state this checks for.
            ! gcov attribution artifact: the `if` line itself is evaluated on every call and so
            ! shows hits, while the body below never runs -- see CLAUDE.md's "Fortran gcov
            ! attribution artifacts", the guard-clause shape.
            if (.not. slot%file_source) then ! GCOVR_EXCL_START
                call table_context_suffix(cache, slot%name, sfx)
                error stop EP // trim(proc) // ": this column holds no values and has no file " // &
                    "column to read them from" // sfx
            end if
            ! GCOVR_EXCL_STOP
            ! D6: once a row-structural mutation has changed the row set, a column read from the
            ! file would no longer line up with the columns already in memory. This has to come
            ! BEFORE table_resolve_width, which reads data itself for a plain-LIST column and
            ! would do so through a reader detaching has already released.
            call table_check_not_detached(cache, sc, slot%name, proc)
            ! A deferred-width column has to be classified before it can be materialized, since the
            ! materialize dispatches on the kind. The footer screen's unproven candidate is enough
            ! here on purpose: the read below checks every row's length against the width it was
            ! given (get_uniform_list_values, parquet_wrapper.cpp) and aborts on a mismatch, so a
            ! wrong candidate fails loudly and the read doubles as the proof -- which is what keeps
            ! this to ONE pass over the data instead of measuring first and then reading.
            call table_resolve_width(cache, sc, idx, .false., proc)
        end associate
        call table_materialize(cache, sc, idx)
        ! Release policy for a SINGLE first touch: free the Arrow buffers straight away. For a
        ! struct leaf that means the struct's array is decoded again when a sibling leaf is first
        ! touched -- the deliberate trade, since holding it would keep the whole struct alive for
        ! a table that may never read the other leaves at all. %prefetch of several leaves at
        ! once avoids the re-decode (see table_materialize_all).
        call table_release_one(cache, cache%cols(idx)%file_name)
    end procedure table_touch
    !
    module procedure table_materialize_all
        logical, allocatable :: want(:)
        !
        allocate(want(cache%ncols))
        want = .true.
        call materialize_marked(cache, sc, want)
    end procedure table_materialize_all
    !
    !> Reads every marked slot in ONE pass, with the batch release policy.
    !!
    !! The reader caches a struct as ONE array shared by all its leaves, so releasing after every
    !! leaf would re-read the struct once per leaf. Slots are in file schema order, which puts a
    !! struct's leaves next to each other, so releasing the PREVIOUS top-level name as soon as the
    !! top-level changes frees each array exactly once, at the earliest point it is safe to. A
    !! single first touch cannot use this (there is no following column to compare against) and
    !! releases immediately instead -- see `table_touch`. That difference is the whole reason
    !! `%prefetch` of a struct's leaves is cheaper than touching them one at a time.
    subroutine materialize_marked(cache, sc, want)
        type(parquet_table_cache), intent(inout) :: cache !! the column store.
        type(table_scope), intent(in) :: sc               !! rows this table covers.
        logical, intent(in) :: want(:)                    !! .true. for each slot to read.
        integer :: i
        character(len=:), allocatable :: top, prev_top
        !
        ! The other long read window besides a lazy first touch (see table_resolve): %prefetch and
        ! %materialize_all both land here, and both read from the file for as long as it takes.
        ! Registered so a concurrent %append refuses rather than reallocating storage this loop is
        ! writing into.
        call table_read_enter(cache, "prefetch")
        ! Every marked column is validated on this thread, before any of them is read, so the
        ! parallel path below cannot abort from inside a region -- and so the serial and parallel
        ! paths refuse exactly the same tables at exactly the same point.
        do i = 1, cache%ncols
            if (.not. materialize_wanted(cache, want, i)) cycle
            ! Checked per column rather than once up front, so a detached table whose columns are
            ! all resident is still a quiet no-op -- which is what %materialize_all means there.
            call table_check_not_detached(cache, sc, cache%cols(i)%name, "materialize_all")
        end do
        ! Guarded so that a build WITHOUT OpenMP compiles: `parallel_prefetch_ok` already answers
        ! .false. there, so this branch is dead either way -- but the reference to
        ! `materialize_marked_parallel` still has to resolve, and that procedure needs `omp_lib`.
        ! The serial loop below is the fallback, and is what such a build runs unconditionally.
#ifdef _OPENMP
        if (parallel_prefetch_ok(cache, sc, want)) then
            call materialize_marked_parallel(cache, sc, want)
            call table_read_exit(cache)
            return
        end if
#endif
        prev_top = ""
        do i = 1, cache%ncols
            if (.not. materialize_wanted(cache, want, i)) cycle
            call table_materialize(cache, sc, i)
            call top_level_of(cache%cols(i)%file_name, top)
            if (len(prev_top) > 0 .and. prev_top /= top) then
                call parquet_release_column(cache%reader, prev_top)
            end if
            prev_top = top
        end do
        if (len(prev_top) > 0) call parquet_release_column(cache%reader, prev_top)
        call table_read_exit(cache)
    end subroutine materialize_marked
    !
    !> Whether slot `i` is one this pass has to read: marked, readable, file-backed, not already
    !! resident. Factored out because the serial loop, the validation pass and the parallel
    !! partitioner must agree on it exactly -- a fourth copy of this test is how the two paths
    !! would come to read different sets of columns.
    logical function materialize_wanted(cache, want, i)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        logical, intent(in) :: want(:)                 !! .true. for each slot to read.
        integer, intent(in) :: i                       !! slot to test.
        !
        materialize_wanted = .false.
        if (.not. want(i)) return
        if (.not. cache%cols(i)%supported) return
        if (.not. cache%cols(i)%file_source) return
        if (cache%cols(i)%residency == RES_FULL) return
        materialize_wanted = .true.
    end function materialize_wanted
    !
    !> Whether this prefetch may read its columns on several threads at once.
    !!
    !! **Deliberately conservative, and every clause below is a correctness or cost rule rather
    !! than a tuning knob.** The parallel path gives each thread its own `parquet_reader` on the
    !! same file, because a shared one entered concurrently aborts (the C++ `ConcurrencyGuard`).
    !! That is only sound when a freshly opened reader would see *exactly* what the table's own
    !! reader sees, and only worth doing when opening those readers does not duplicate real work:
    !!
    !!   * **A `sample_fraction=` and a `qc=` are CARRIED by every per-thread reader**, because
    !!     `table_open_reader_with_transform` opens them rather than a bare `parquet_open_reader`.
    !!     The sample is reproduced from `cache%read_sample_seed`, which `parquet_open_table`
    !!     settles before any reader exists (feature_risks.md Risk-55) -- so there is deliberately
    !!     **no** seed test here, and adding one back would read as though that invariant were in
    !!     doubt. A qc schema installs its rules per reader but runs its checks per COLUMN, and
    !!     each column is read by exactly one thread, so nothing is checked or warned twice.
    !!     Measured on a 16-column x 2 M-row file, 8 threads, best of 5, four rounds: **3.5x** for
    !!     the sample and **3.3x** for qc, against 4.5x with no transform at all.
    !!   * **A `sort=` and a `filter=` still fall back to the serial reader -- both on COST, and
    !!     both as a DEFERRAL rather than a property of the problem.** Each per-thread reader would
    !!     redo the transform's own derived state, which is the "does not duplicate real work"
    !!     clause of the principle above:
    !!       - a **sort** rebuilds the whole permutation, serially (`pf_sort_threads` stands down
    !!         inside a region) -- 5.88 s at 20.8 M rows on one measured machine, against a
    !!         prefetch saving of a couple of seconds on the same file;
    !!       - a **filter** re-decodes its key columns, re-runs the statistics screen and rebuilds
    !!         the row mask -- a cost that grows with the thread count while the read saving decays
    !!         with it, so the curve peaks early and then goes negative. Measured on the same file,
    !!         three rounds, speedup against a forced-serial read by thread cap: **2 -> 1.06x,
    !!         3 -> 1.22x, 4 -> 1.19x, 5 -> 1.21x, 6 -> 1.04x, 7 -> 0.94x, 8 -> 0.94x**, against
    !!         4.0-4.7x for the same file with no filter. So it is refused **at the automatic
    !!         thread count, which is what a caller actually gets** -- the peak is real but is worth
    !!         a fifth of what the untransformed case wins, and capturing it would mean inventing a
    !!         thread-count heuristic from one machine's numbers. Correctness is not the issue at
    !!         all (a filter is a pure function of the file, so every reader computes the same mask,
    !!         and that path is implemented and tested); only the cost is.
    !!
    !!         **More filter columns makes this monotonically worse, not better** -- the natural
    !!         next question, and the answer is the opposite of the intuition. The per-reader cost
    !!         is proportional to how many key columns the filter decodes, so the optimum thread
    !!         count falls as sqrt(columns read / columns filtered on). Same file, best speedup over
    !!         every thread count tried: **1 key column -> 1.29x, 2 -> 1.02x, 4 -> 0.77x,
    !!         8 -> 0.65x.** From two key columns on there is NO thread count at which this wins.
    !!     **Both are fixable the same way, and by the same milestone.** The permutation is an
    !!     immutable `shared_ptr<arrow::Array>` and the mask is likewise derived once and never
    !!     mutated, so each could be *shared* with a per-thread reader for a refcount increment
    !!     instead of rebuilt. That is milestone P9, which these two clauses both wait on. Do not
    !!     read either line as "this cannot be parallelised".
    !!   * **Not detached, and file-backed**, or there is no file to open a second reader on.
    !!   * **At least two top-level names to read.** One column cannot be split, and the release
    !!     policy groups a struct's leaves under their top-level name (see `materialize_marked`),
    !!     so that is the unit of work.
    !!   * **Not already inside a parallel region.** Nested regions are the caller's business, and
    !!     a table reached from inside one is exactly the shared-store case the first-touch guard
    !!     refuses anyway.
    logical function parallel_prefetch_ok(cache, sc, want)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        type(table_scope), intent(in) :: sc            !! rows this table covers.
        logical, intent(in) :: want(:)                 !! .true. for each slot to read.
        integer :: ngroups
        !
        parallel_prefetch_ok = .false.
#ifdef _OPENMP
        if (.not. per_thread_readers_ok(cache, sc)) return
        call count_top_level_groups(cache, want, ngroups)
        if (ngroups < 2) return
        parallel_prefetch_ok = .true.
#endif
    end function parallel_prefetch_ok
    !
    !> Whether this table may be read through SEVERAL readers of its own at once, at all -- the
    !! clauses both parallel read paths share, asked in one place so the two cannot drift.
    !!
    !! Everything specific to a path stays in that path's own gate: how many top-level names there
    !! are to read (`parallel_prefetch_ok`), or how many row groups and how much work one column is
    !! (`parallel_colread_ok`). What is shared is the question *may a second reader exist and would
    !! it see the same rows*, and the answer is the same for both by construction, because both open
    !! their readers through `table_open_reader_with_transform`.
    !!
    !! Splitting this out is not tidiness: the two gates disagreeing would not fail anything. The
    !! looser one would simply parallelise a case the stricter one had judged unsafe, silently.
    logical function per_thread_readers_ok(cache, sc)
#ifdef _OPENMP
        use omp_lib, only : omp_in_parallel
#endif
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        type(table_scope), intent(in) :: sc            !! rows this table covers.
        !
        per_thread_readers_ok = .false.
#ifdef _OPENMP
        if (prefetch_thread_count() <= 1) return
        if (omp_in_parallel()) return
        if (sc%detached) return
        if (.not. cache%file_backed) return
        if (.not. allocated(cache%reader)) return
        ! NO CLAUSE TESTS THE READ-TIME TRANSFORM, and that is the point rather than an omission.
        ! Every additional reader adopts the table's own filter mask and sort permutation instead of
        ! rebuilding them (`table_open_reader_with_transform`), so no transform costs a second reader
        ! anything to reproduce and none of them can produce a different row set. A clause added back
        ! here "to be safe" would refuse a case that is now both correct and fast.
        per_thread_readers_ok = .true.
#endif
    end function per_thread_readers_ok
    !
    !> Whether ONE column's whole-column read may be spread across its row groups.
    !!
    !! The complement of `parallel_prefetch_ok`: that one splits a read by column and needs at least
    !! two of them, so a single column's first touch -- a `%get` of one name, a `%prefetch` of one
    !! name, a `%reload` -- has always been serial no matter how large the column was. This splits
    !! the same read by ROW GROUP instead, so the two together cover both shapes.
    !!
    !! **Arrow does not already do this, which had to be measured rather than assumed.** Reading one
    !! whole column with Arrow's own `use_threads` on and off came out within noise of itself
    !! (0.96x-1.02x over five runs at two column sizes, reproduced independently on three machines),
    !! so the decode of a single column really is serial inside Arrow and there is something here to
    !! win. Expect 2-3x rather than the thread count: a column decode is memory-bandwidth work, and
    !! every other parallel path in this library hit that ceiling well short of its team size.
    !!
    !! Its own clauses, on top of `per_thread_readers_ok`'s shared ones:
    !!
    !!   * **At least two row groups.** One row group cannot be split, and a file written in one
    !!     chunk is common.
    !!   * **Enough work to pay for the readers.** Each thread opens its own `parquet_reader`, which
    !!     parses the footer; below `colread_min_elements` (rows x width) that costs more than the
    !!     split saves. The floor is in ELEMENTS, not rows, so a narrow long column and a wide short
    !!     one are judged by the same measure -- and it is overridable, because no fixture a test can
    !!     afford reaches the real one (`feature_risks.md` Risk-49's lesson).
    !!   * **Not a string column.** `%paste` is what puts each row group's chunk into its place
    !!     without reallocating, and a `parquet_string_column` is a packed variable-length store with
    !!     no fixed row slots, so it cannot be overwritten in place. The serial grow-and-append shape
    !!     stays, and costs those two kinds nothing, because their buffers grow geometrically.
    logical function parallel_colread_ok(cache, sc, idx, nrg)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        type(table_scope), intent(in) :: sc            !! rows this table covers.
        integer, intent(in) :: idx                     !! slot about to be read.
        integer(int64), intent(in) :: nrg              !! row groups this read would span.
        integer(int64) :: floor_elems
        !
        parallel_colread_ok = .false.
#ifdef _OPENMP
        if (.not. per_thread_readers_ok(cache, sc)) return
        if (nrg < 2_int64) return
        if (cache%cols(idx)%declared_kind == PK_STRING) return
        if (cache%cols(idx)%declared_kind == PK_STRING_VEC) return
        call colread_gate_limit(floor_elems)
        if (sc%nrows * int(max(cache%cols(idx)%width, 1), int64) < floor_elems) return
        parallel_colread_ok = .true.
#endif
    end function parallel_colread_ok
    !
    !> The work floor `parallel_colread_ok` gates on, with its test-only override applied.
    !!
    !! Same shape and same reason as `colwork_gate_limits` (`src/parquet_tables_parallel.f90`): the
    !! real constant sits far above anything a test fixture can reach, so the only way to exercise
    !! both sides of the gate is to move the floor rather than the input.
    subroutine colread_gate_limit(floor_elems)
        use iso_c_binding, only : c_int64_t
        integer(int64), intent(out) :: floor_elems !! elements below which the read stays serial.
        interface
            function get_elems() result(res) bind(C, name="parquet_debug_get_colread_min_elements")
                import :: c_int64_t
                integer(c_int64_t) :: res
            end function get_elems
        end interface
        integer(int64) :: v
        !
        floor_elems = colread_min_elements
        v = int(get_elems(), int64)
        if (v > 0_int64) floor_elems = v
    end subroutine colread_gate_limit
    !
    !> Records, for the test suite only, how many threads the last single-column row-group-parallel
    !! read was given. Zero means that read took the ordinary whole-column path.
    !!
    !! A separate counter from `parquet_debug_note_prefetch_threads` because the two paths are
    !! alternatives: one counter could report that *a* parallel read happened but never which, and
    !! "the row-group path ran when the column path should have" is exactly the confusion worth being
    !! able to detect.
    subroutine parquet_debug_note_colread_threads(n)
        use iso_c_binding, only : c_int64_t
        integer(int64), intent(in) :: n !! threads the region was given.
        interface
            subroutine set_colread_used(k) bind(C, name="parquet_debug_set_colread_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: k
            end subroutine set_colread_used
        end interface
        !
        call set_colread_used(int(n, c_int64_t))
    end subroutine parquet_debug_note_colread_threads
    !
    !> How many threads a parallel prefetch may use: as many as OpenMP offers, capped by
    !> parquet_set_prefetch_threads when that was set. A CAP only -- the setting can never ask for
    !> more threads than OpenMP has been given, so setting it above OMP_NUM_THREADS changes nothing.
    !>
    !> `1` needs no special case anywhere: it makes parallel_prefetch_ok decline through the same
    !> test that already handles a single-threaded OpenMP environment, and the serial path takes
    !> over. The one thing this must stay is the SINGLE source of that number -- it sizes the
    !> per-thread reader array AND limits the team, and those two disagreeing is an out-of-bounds
    !> index rather than a slowdown.
    integer function prefetch_thread_count() result(n)
        use parquet_settings, only : parquet_get_prefetch_threads
#ifdef _OPENMP
        use omp_lib, only : omp_get_max_threads
#endif
        integer :: cap
        !
        n = 1
#ifdef _OPENMP
        n = omp_get_max_threads()
#endif
        cap = parquet_get_prefetch_threads()
        if (cap > 0 .and. cap < n) n = cap
    end function prefetch_thread_count

    !> Records, for the test suite only, how many threads the last parallel prefetch was given.
    !>
    !> The count is Fortran-side state with no other way out: parquet_table's components are private
    !> and the number is a local of the region below, so nothing outside could observe whether
    !> parquet_set_prefetch_threads had any effect -- and a knob that is stored but never acted on
    !> passes every set/get test ever written for it (feature_risks.md Risk-41). Pushing it to a C++
    !> global keeps the hook out of the library's own Fortran interface, which CLAUDE.md's
    !> "A Fortran-side debug hook has to be PUBLIC, so prefer a C++ one" asks for.
    !>
    !> Called once per prefetch, on a path that has just opened parquet readers and is about to
    !> decode whole columns, so the cost is unmeasurable. **That ratio is the rule**: a debug hook
    !> may sit on a coarse operation like this one, never on a per-row or per-element path.
    subroutine parquet_debug_note_prefetch_threads(n)
        use iso_c_binding, only : c_int64_t
        integer(int64), intent(in) :: n !! threads the region was given.
        interface
            subroutine set_prefetch_used(k) bind(C, name="parquet_debug_set_prefetch_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: k
            end subroutine set_prefetch_used
        end interface
        !
        call set_prefetch_used(int(n, c_int64_t))
    end subroutine parquet_debug_note_prefetch_threads

    !> Counts the distinct top-level names this pass will read. Slots are in file schema order, so
    !! a struct's leaves are adjacent and a change of top-level name starts a new group.
    subroutine count_top_level_groups(cache, want, ngroups)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        logical, intent(in) :: want(:)                 !! .true. for each slot to read.
        integer, intent(out) :: ngroups                !! number of distinct top-level names.
        integer :: i
        character(len=:), allocatable :: top, prev_top
        !
        ngroups = 0
        prev_top = ""
        do i = 1, cache%ncols
            if (.not. materialize_wanted(cache, want, i)) cycle
            call top_level_of(cache%cols(i)%file_name, top)
            if (len(prev_top) == 0 .or. prev_top /= top) ngroups = ngroups + 1
            prev_top = top
        end do
    end subroutine count_top_level_groups
    !
    ! The whole procedure is inside `#ifdef _OPENMP`, not just its `use omp_lib`, because its body
    ! calls omp_get_thread_num/omp_get_max_threads throughout and drives an `!$omp parallel` region
    ! -- so there is nothing left of it to compile once OpenMP is out. Its only caller is guarded to
    ! match, and falls through to the serial loop. The guard opens BEFORE the doc-comment block so
    ! that the `!>` stays adjacent to the procedure it documents in the preprocessed source; a
    ! doc-comment left behind by a removed procedure would attach itself to whatever followed.
#ifdef _OPENMP
    !> Reads the marked columns on several threads, one top-level name at a time.
    !!
    !! Each thread drives its OWN reader, opened on the same file: a `parquet_reader` is not safe
    !! to enter from two threads at once and says so by aborting, and the whole point here is that
    !! the caller never has to know that. The work unit is a top-level name rather than a column,
    !! so a struct's leaves stay together on one thread and each thread's release policy is
    !! exactly the serial one, applied to its own reader.
    !!
    !! Writing into `cache%cols(i)` from several threads is safe because the slots are distinct
    !! allocations and each is written by exactly one thread; `cache%reads_started` is the one
    !! cache-level scalar the materialize path sets, and it is set here, once, before the region.
    !! `parallel_prefetch_ok` has already established that a freshly opened reader sees the same
    !! rows as the table's own -- do not relax that without re-reading its own comment.
    !!
    !! **A `parquet_reader` may be neither `private()`d nor declared in this region's lexical scope
    !! -- every thread's own lives in a SHARED array, indexed by thread number and allocated BEFORE
    !! the region.** The two supported compilers each forbid one of the two obvious shapes, and
    !! they forbid opposite ones, which is why this third shape is the only one available:
    !!
    !!   * **gfortran breaks `private()`**: it does not reliably default-initialize a private copy
    !!     of a finalizable type, so the first finalization frees an undefined pointer (CLAUDE.md,
    !!     "Never give a FINALIZABLE derived type to OpenMP's `private()`"). Its documented
    !!     workaround is to declare the variable in a `block` inside the loop body instead.
    !!   * **ifx breaks that very workaround, while `private()` works fine there.** A finalizable
    !!     type *with allocatable components* declared inside a `block` lexically nested in a
    !!     parallel region makes ifx emit privatization scaffolding for it
    !!     (`<TYPE>.omp.mold_ctor` -> `for_alloc_private` -> `do_alloc_copy` ->
    !!     `copy_src_xdesc_to_dest_xdesc`) that segfaults on every thread entering the region --
    !!     100% reproducible with as few as 2 threads and independent of team size, so not a race.
    !!     Confirmed on ifx 2026.1 by gdb backtrace and by a standalone bisected probe.
    !!
    !! Two conditions narrow that second one, and both are met here, so neither is a way out:
    !! it needs **`-O1`+** (at `-O0` it runs clean, so a `--profile debug` run cannot see it and a
    !! green debug build proves nothing), and it needs the type to come from a **separately
    !! compiled module** -- the identical type defined in the same file as its user does not crash,
    !! which is why a quick single-file reproducer will wrongly exonerate the shape.
    !!
    !! Indexing a pre-allocated shared array satisfies both compilers at once: no derived-type
    !! instance is constructed inside the parallel construct at all, only a reference to an
    !! already-existing element -- the same "distinct slot per thread" shape `cache%cols(i)` relies
    !! on above. Verified rather than assumed: with this shape ifx emits **no** privatization
    !! scaffolding for `parquet_reader` (`nm` on this file's object finds no `for_alloc_private`
    !! and no `mold_ctor`), and the region runs correctly at 2, 8 and 16 threads. Passing an
    !! element of that array on to an `optional, intent(inout)` dummy -- exactly how
    !! `table_materialize`/`table_release_one` receive `rdr` -- does not reintroduce it either.
    !!
    !! So the `block` below is deliberately kept free of anything but plain integers: a
    !! `parquet_reader`, `parquet_writer` or `parquet_schema` declared there brings the ifx crash
    !! straight back, at `-O2` only, with a backtrace naming no library code. A `parquet_table` is
    !! the one finalizable type this library exposes that would be safe, because it deliberately
    !! has no allocatable components at all -- do not read that as permission, since the next
    !! component added to it would silently make this region crash too. See `feature_risks.md`
    !! Risk-45.
    subroutine materialize_marked_parallel(cache, sc, want)
        use omp_lib, only : omp_get_thread_num, omp_get_max_threads
        type(parquet_table_cache), intent(inout) :: cache !! the column store.
        type(table_scope), intent(in) :: sc               !! rows this table covers.
        logical, intent(in) :: want(:)                    !! .true. for each slot to read.
        integer :: i, g, ngroups, t, nslots
        integer, allocatable :: g_lo(:), g_hi(:)
        character(len=:), allocatable :: top, prev_top
        type(parquet_reader), allocatable :: readers(:)
        logical, allocatable :: reader_open(:)
        !
        call count_top_level_groups(cache, want, ngroups)
        allocate(g_lo(ngroups), g_hi(ngroups))
        g = 0
        prev_top = ""
        do i = 1, cache%ncols
            if (.not. materialize_wanted(cache, want, i)) cycle
            call top_level_of(cache%cols(i)%file_name, top)
            if (len(prev_top) == 0 .or. prev_top /= top) then
                g = g + 1
                g_lo(g) = i
            end if
            g_hi(g) = i
            prev_top = top
        end do
        cache%reads_started = .true.
        ! One slot per thread the team could possibly use, allocated up front so the region below
        ! only ever indexes an existing element -- see this subroutine's own doc-comment. This must
        ! be the SAME number the region is limited to below, or a thread indexes past the end.
        nslots = prefetch_thread_count()
        allocate(readers(nslots))
        allocate(reader_open(nslots))
        reader_open = .false.
        call parquet_debug_note_prefetch_threads(int(min(nslots, ngroups), int64))
        !$omp parallel do default(shared) private(g, t) schedule(dynamic) num_threads(nslots)
        do g = 1, ngroups
            block
                ! Plain integers ONLY. A finalizable derived type declared here segfaults ifx at
                ! -O1+ -- see this subroutine's own doc-comment and feature_risks.md Risk-45.
                integer :: k
                !
                t = omp_get_thread_num() + 1
                ! Opened once per thread, on that thread's own slot, and reused across every group
                ! the scheduler hands to this thread -- reopening per group would pay Arrow's
                ! reader-construction cost once per top-level name instead of once per thread.
                if (.not. reader_open(t)) then
                    ! Arrow's own per-column threading is deliberately left ENABLED here rather
                    ! than disabled to avoid oversubscription: measured both ways on a
                    ! 24-column x 900k-row file, 8 OpenMP threads -- 0.037-0.040 s with Arrow
                    ! threading on, 0.046-0.047 s with use_threads=.false. Nesting the two is
                    ! faster, not slower, so the obvious "one level of parallelism only" instinct
                    ! is wrong here. Re-measure before changing it.
                    !
                    ! THROUGH THE SAME HELPER `parquet_open_table` USES, never a bare
                    ! parquet_open_reader: that is what makes this thread's reader carry the
                    ! table's filter/qc/sample -- and carry them identically, by construction
                    ! rather than by a second copy of the plumbing that has to be kept in step.
                    ! Passing `readers(t)` is also what keeps `cache%rg_bounds` unwritten here;
                    ! see the helper's own doc-comment.
                    call table_open_reader_with_transform(cache, cache%source_file, readers(t))
                    reader_open(t) = .true.
                end if
                do k = g_lo(g), g_hi(g)
                    if (.not. materialize_wanted(cache, want, k)) cycle
                    call table_materialize(cache, sc, k, readers(t))
                end do
                ! One release per group, on this thread's own reader, exactly as the serial loop
                ! releases one top-level name once.
                call table_release_one(cache, cache%cols(g_lo(g))%file_name, readers(t))
            end block
        end do
        !$omp end parallel do
        do t = 1, nslots
            if (reader_open(t)) call parquet_close_reader(readers(t))
        end do
    end subroutine materialize_marked_parallel
#endif
    !
    module procedure table_materialize_every
        call table_check_open(self, "materialize_all")
        call table_materialize_all(self%cache, table_scope_of(self))
    end procedure table_materialize_every
    !
    module procedure prefetch_string
        character(len=:), allocatable :: toks(:)
        logical, allocatable :: want(:)
        integer :: i
        logical :: ok, all_found
        !
        call table_check_open(self, "prefetch")
        ! One tokenizer for the whole library (parquet_core), so this and the reader-level
        ! parquet_prefetch_columns cannot disagree about punctuation. A single name with no
        ! separator comes back as one token, which is the overwhelmingly common case and takes
        ! exactly the same path it always did.
        call parquet_split_name_list(names, toks)
        ! `want` is left unallocated here on purpose: grow_want_mask is the mask's single owner, so
        ! it creates it on first use rather than each caller repeating the allocate-and-blank pair
        ! that has to agree with it.
        all_found = .true.
        do i = 1, size(toks)
            call mark_one_name(self, trim(toks(i)), present(found), want, ok)
            ! One miss does not abandon the rest, matching the array form: the caller asked for a
            ! set, and the ones that do exist are still worth reading. `found` reports whether ALL
            ! of them were found.
            if (.not. ok) all_found = .false.
        end do
        if (present(found)) found = all_found
        ! materialize_marked walks every slot, so the mask has to cover the column count as it is
        ! after resolution -- which may have grown, since asking for parquet_row_index creates it.
        call grow_want_mask(want, self%cache%ncols)
        ! ONE pass over the marked slots, however many names were given -- which is the whole
        ! reason the array form exists, and the string form now shares it. A zero-token string
        ! marks nothing and reads nothing, leaving `found` .true.
        call materialize_marked(self%cache, table_scope_of(self), want)
    end procedure prefetch_string
    !
    !> Marks the slot(s) one %prefetch name asks for: the column of that exact name, or -- when
    !! there is none -- every leaf under `name.`.
    !!
    !! A real column of that exact name wins, always. Only when there is none does the name get
    !! read as a struct prefix, so a file with a column literally called "main" is reached by its
    !! own name even if it also has "main.a" leaves.
    !!
    !! `report_miss` is the caller's `present(found)`: with it, a missing name marks nothing and
    !! reports `ok = .false.`; without it, the resolve aborts, which is what a caller who asked
    !! for no `found=` expects. It cannot simply be an optional forwarded straight through,
    !! because each name's result has to be folded into one answer rather than overwriting it.
    subroutine mark_one_name(self, name, report_miss, want, ok)
        class(parquet_table), intent(in) :: self     !! the table.
        character(len=*), intent(in) :: name         !! one column name, already trimmed.
        logical, intent(in) :: report_miss           !! .true.: report a miss instead of aborting.
        logical, allocatable, intent(inout) :: want(:) !! marks accumulated across every name.
        logical, intent(out) :: ok                   !! .false. if this name matched nothing.
        integer :: idx, n
        logical, allocatable :: leaves(:)
        logical :: got
        !
        ok = .true.
        idx = table_find(self, name)
        if (idx > 0) then
            if (report_miss) then
                call table_prefetch_resolve(self, name, "prefetch", idx, got)
                if (.not. got) ok = .false.
            else
                call table_prefetch_resolve(self, name, "prefetch", idx)
            end if
            ! No `idx == 0` check here, deliberately: the name was just found, and the resolve
            ! looks it up again against a cache nothing has mutated in between, so it cannot come
            ! back a miss. A guard would be unreachable AND wrong-headed -- it would turn a slot
            ! index that somehow went bad into a quiet "not found" instead of the out-of-bounds
            ! write `--profile debug` reports.
            !
            ! Resolving can ADD a column: asking for parquet_row_index materializes it, which is a
            ! new slot. So the mask has to be re-sized against the column count as it is NOW, not
            ! as it was when the loop started -- otherwise the very next line writes past its end.
            call grow_want_mask(want, self%cache%ncols)
            want(idx) = .true.
            return
        end if
        ! Every leaf under "<name>." in ONE pass, which is the point: the reader decodes a struct
        ! as one array shared by all its leaves, so reading them separately decodes it once per
        ! leaf.
        call mark_struct_leaves(self%cache, name, leaves, n)
        if (n == 0) then
            ! Nothing of that name and no leaves under it: an ordinary missing column, reported
            ! the ordinary way. A prefix that matches nothing is a mistake, not a quiet no-op.
            if (report_miss) then
                call table_prefetch_resolve(self, name, "prefetch", idx, got)
                ! `got`, not .false.: the reserved row-index name resolves HERE (there is no slot
                ! to find until this call makes one), so hardcoding a miss would report .false.
                ! for a name that was found and materialized.
                ok = got
            else
                call table_prefetch_resolve(self, name, "prefetch", idx)
            end if
            return
        end if
        call grow_want_mask(want, size(leaves))
        want = want .or. leaves
    end subroutine mark_one_name
    !
    !> Grows a %prefetch mark mask to `n` entries, keeping what is already marked -- creating it
    !! blank when it does not exist yet.
    !!
    !! Needed because resolving a name can create a column: `parquet_row_index` is materialized on
    !! demand, through `table_new_slot`, so a mask sized before the loop is one entry short from
    !! that point on. A plain `fpm test` runs straight past the overrun; `--profile debug` is what
    !! catches it.
    !!
    !! **This is the mask's only constructor.** Its callers deliberately leave `want` unallocated
    !! and let the first call here size it: a caller that allocated its own would be a second place
    !! the "blank, one entry per column" rule is written down, free to drift from this one.
    subroutine grow_want_mask(want, n)
        logical, allocatable, intent(inout) :: want(:) !! the mask to grow.
        integer, intent(in) :: n                       !! entries it must have.
        logical, allocatable :: bigger(:)
        !
        if (.not. allocated(want)) then
            allocate(want(n))
            want = .false.
            return
        end if
        if (size(want) >= n) return
        allocate(bigger(n))
        bigger = .false.
        bigger(1:size(want)) = want
        call move_alloc(bigger, want)
    end subroutine grow_want_mask
    !
    !> Marks every slot whose name begins with `prefix // "."`, reporting how many.
    !!
    !! Unsupported leaves under the prefix are marked like any other and skipped by
    !! `materialize_marked` itself -- an exotic leaf should not make its struct unprefetchable,
    !! which is the same rule that keeps one exotic column from making a file unopenable.
    subroutine mark_struct_leaves(cache, prefix, want, n)
        type(parquet_table_cache), intent(in) :: cache      !! the column store.
        character(len=*), intent(in) :: prefix              !! the struct name, without its dot.
        logical, allocatable, intent(out) :: want(:)        !! .true. for each matching slot.
        integer, intent(out) :: n                           !! how many matched.
        character(len=:), allocatable :: pfx
        integer :: i, w
        !
        allocate(want(cache%ncols))
        want = .false.
        n = 0
        pfx = trim(prefix) // "."
        w = len(pfx)
        do i = 1, cache%ncols
            if (len(cache%cols(i)%name) <= w) cycle
            if (cache%cols(i)%name(1:w) /= pfx) cycle
            want(i) = .true.
            n = n + 1
        end do
    end subroutine mark_struct_leaves
    !
    module procedure prefetch_array
        logical, allocatable :: want(:)
        integer :: i, idx
        logical :: got
        !
        call table_check_open(self, "prefetch")
        ! Unallocated on purpose; grow_want_mask owns the mask. See prefetch_string.
        if (present(found)) found = .true.
        do i = 1, size(names)
            ! `found` has to be forwarded conditionally, not just passed along: handing the
            ! callee a present dummy would turn every missing name into a quiet miss, including
            ! for a caller who asked for no `found=` and therefore expects an abort.
            if (present(found)) then
                call table_prefetch_resolve(self, trim(names(i)), "prefetch", idx, got)
                if (.not. got) then
                    ! One miss does not abandon the rest: the caller asked for a set, and the
                    ! ones that do exist are still worth reading. `found` reports whether ALL
                    ! of them were found.
                    found = .false.
                    cycle
                end if
            else
                call table_prefetch_resolve(self, trim(names(i)), "prefetch", idx)
            end if
            ! Resolving can ADD a column -- asking for parquet_row_index materializes it -- so the
            ! mask is re-sized against the column count as it is now. See grow_want_mask.
            call grow_want_mask(want, self%cache%ncols)
            want(idx) = .true.
        end do
        call grow_want_mask(want, self%cache%ncols)
        call materialize_marked(self%cache, table_scope_of(self), want)
    end procedure prefetch_array
    !
    !> Resolves a name for %prefetch: a miss obeys `found=`, and an unsupported column is an
    !! error either way -- asking to read a column this library cannot read is a mistake, not a
    !! quiet no-op, even though it is harmless.
    subroutine table_prefetch_resolve(self, name, proc, idx, found)
        class(parquet_table), intent(in) :: self  !! the table.
        character(len=*), intent(in) :: name      !! column name.
        character(len=*), intent(in) :: proc      !! calling procedure, for the message.
        integer, intent(out) :: idx               !! slot index, or 0 on a reported miss.
        logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, proc)
        ! The reserved name has to resolve here too, exactly as it does in table_resolve for the
        ! value accessors -- otherwise %prefetch is the one documented way to ask for a column
        ! ahead of time that cannot ask for this one, and reports "no column of this name" for a
        ! name %has_column answers .true. for. Materializing it is what %prefetch means, so it
        ! becomes an ordinary listed column here just as it does after a %get.
        !
        ! %materialize_all deliberately does NOT go through this: it marks the slots that exist,
        ! so a table that never asked for the row index does not acquire one from a bulk read.
        !
        ! Look up first and compare only on a miss, for the reason `table_resolve` spells out (a
        ! character comparison is a library call under gfortran). This one is per-column rather
        ! than per-cell, so it costs nothing measurable -- it is written this way so the file
        ! carries one shape, not two.
        idx = table_find(self, name)
        if (idx == 0) then
            if (name == PARQUET_ROW_INDEX) then
                if (allocated(self%cache%meta_keys)) then
                    call table_make_row_index(self)
                    idx = table_find(self, name)
                end if
            end if
        end if
        if (idx == 0) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": no column of this name" // sfx
        end if
        if (.not. self%cache%cols(idx)%supported) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": this column's type is not supported by " // &
                "parquet_table, so it cannot be read" // sfx
        end if
        if (present(found)) found = .true.
    end subroutine table_prefetch_resolve
    !
    module procedure table_make_row_index
        integer(int64), allocatable :: rows(:)
        integer(int64) :: i
        integer :: idx
        type(parquet_column) :: col
        character(len=:), allocatable :: sfx
        !
        if (self%cache%row_index_live) return
        if (.not. self%cache%file_backed .or. self%detached) then
            call table_context_suffix(self%cache, PARQUET_ROW_INDEX, sfx)
            error stop EP // "the " // PARQUET_ROW_INDEX // " column says which row of the " // &
                "source file each row came from, so it is only available while the table still " // &
                "has that file; materialize it BEFORE the mutation that detaches" // sfx
        end if
        allocate(rows(self%row_count))
        if (self%regime == REGIME_SLICE .and. .not. table_transform_narrows(self%cache) .and. &
                .not. allocated(self%cache%read_sort)) then
            ! An unfiltered slice is pure arithmetic: its rows are a contiguous run of file rows.
            do i = 1_int64, self%row_count
                rows(i) = self%row_lo + i - 1_int64
            end do
        else if (self%regime == REGIME_FULL .and. .not. table_transform_narrows(self%cache) .and. &
                .not. allocated(self%cache%read_sort)) then
            do i = 1_int64, self%row_count
                rows(i) = i
            end do
        else
            ! Filtered, sampled or sorted: which file rows survived, and in what order, lives in
            ! the reader's own mask and permutation. A masked SLICE is covered by the same call,
            ! since its reader is scoped to the slice's rows already.
            call parquet_get_physical_row_indices(self%cache%reader, rows)
        end if
        ! The slot is made directly rather than through %add_column, which would call
        ! table_fix_nrows and mark the column user_populated: nobody wrote these values, the table
        ! derived them, and %evict_column, %reload, %print_stat and %clone all care about the
        ! difference -- marking it would refuse an eviction of a column the table can rebuild
        ! from nothing, and report it as edited.
        call table_new_slot(self, PARQUET_ROW_INDEX, .false., idx)
        call col%adopt(rows)
        call self%cache%cols(idx)%values%move_from(col)
        self%cache%cols(idx)%declared_kind = PK_INT64
        self%cache%cols(idx)%width = 1
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .false.
        self%cache%row_index_live = .true.
    end procedure table_make_row_index
    !
    module procedure table_validate_qc
        character(len=:), allocatable :: qc_cols(:)
        logical, allocatable :: want(:), was_resident(:)
        integer :: i, j, n
        !
        call table_check_open(self, "validate_qc")
        ! Nothing declared, or nothing to check it against: a no-op rather than an error, so a
        ! caller can make this call unconditionally on any table.
        if (.not. allocated(self%cache%read_qc_schema)) return
        if (.not. self%cache%file_backed .or. self%detached) return
        call parquet_get_qc_columns(self%cache%read_qc_schema, qc_cols)
        if (size(qc_cols) == 0) return
        !
        ! Recorded BEFORE anything is read, so that what gets released afterwards is exactly what
        ! this call created -- a column the program had already read stays resident.
        allocate(was_resident(self%cache%ncols))
        allocate(want(self%cache%ncols))
        want = .false.
        do i = 1, self%cache%ncols
            was_resident(i) = self%cache%cols(i)%residency == RES_FULL
        end do
        ! The qc schema names the FILE's columns, as every read-time transform does, so the match
        ! is against file_name -- a remapped column is checked under the name the file calls it.
        n = 0
        do j = 1, size(qc_cols)
            do i = 1, self%cache%ncols
                if (self%cache%cols(i)%file_name /= trim(qc_cols(j))) cycle
                if (.not. self%cache%cols(i)%supported) cycle
                want(i) = .true.
                n = n + 1
                exit
            end do
        end do
        if (n == 0) return
        ! One pass, so a struct whose leaves all declare qc is decoded once. The reader applies
        ! the qc as each column is read; a violation aborts here (or warns, under qc_soft=).
        call materialize_marked(self%cache, table_scope_of(self), want)
        do i = 1, self%cache%ncols
            if (was_resident(i)) cycle
            if (self%cache%cols(i)%residency /= RES_FULL) cycle
            call self%evict_column(self%cache%cols(i)%name)
        end do
    end procedure table_validate_qc
    !
    module procedure table_evict_column
        integer :: idx
        logical :: forced
        character(len=:), allocatable :: sfx
        !
        call table_check_not_shared(self, "evict_column")
        ! Deliberately table_lookup_or_fail, not table_resolve: reading a column in order to
        ! throw it away would be exactly backwards.
        call table_lookup_or_fail(self, name, "evict_column", idx, found)
        if (idx == 0) return
        ! Nothing held, nothing to release. Said before the checks below so that evicting an
        ! already-evicted column stays idempotent whatever else is true of the table.
        if (self%cache%cols(idx)%residency /= RES_FULL) return
        forced = .false.
        if (present(force)) forced = force
        if (.not. self%cache%cols(idx)%file_source) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "evict_column: this column was not read from a file, so its values " // &
                "are the only copy there is; use %drop_column if you mean to discard them" // sfx
        end if
        if (self%detached .or. .not. self%cache%file_backed) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "evict_column: this table has been detached from its file, so an " // &
                "evicted column could never be read back; use %drop_column if you mean to " // &
                "discard it" // sfx
        end if
        ! LAST of the three, and the order is load-bearing rather than incidental. The two checks
        ! above have no force= escape and must not acquire one: when there is no file to read back
        ! from, the values really are unrecoverable, so the answer is %drop_column and not a
        ! keyword. Only once a re-read is known to be possible does the question "would that lose
        ! anything?" arise at all -- which is this check, and the one force= may answer.
        !
        ! Testing it before them would also give an %add_column column the wrong message, since
        ! %add_column claims its column: the caller would be told to pass force=.true., and doing
        ! so would then hit the file_source abort anyway.
        !
        ! Only what the value-setting API wrote is seen here; a write through a %col/%ref pointer
        ! marks nothing, which is why %set_user_populated exists (see its own doc-comment).
        if (self%cache%cols(idx)%user_populated .and. .not. forced) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "evict_column: this column holds values written into the table, " // &
                "which are the only copy there is -- the file's own values would come back on " // &
                "the next read; pass force=.true. if you really mean to discard them" // sfx
        end if
        ! Values only: the descriptor stays exactly as it is, so %column_names, %kind, %width and
        ! %unit keep answering and the next touch reads the column again.
        call self%cache%cols(idx)%values%clear()
        self%cache%cols(idx)%residency = RES_EMPTY
        self%cache%cols(idx)%user_populated = .false.
        ! Structural for a pointer's purposes: whatever %col handed out for this column now points
        ! at freed storage, which is exactly what the generation counter is there to report.
        self%cache%generation = self%cache%generation + 1_int64
    end procedure table_evict_column
    !
    module procedure table_reload
        integer :: idx
        logical :: forced
        character(len=:), allocatable :: sfx
        !
        call table_check_not_shared(self, "reload")
        call table_prefetch_resolve(self, name, "reload", idx, found)
        if (idx == 0) return
        forced = .false.
        if (present(force)) forced = force
        if (.not. self%cache%cols(idx)%file_source) then
            ! A column added in memory has no file behind it, so there is nothing to reload
            ! FROM -- and silently keeping the current values would make %reload look like it
            ! worked when it did nothing.
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "reload: this column was not read from a file, so there is " // &
                "nothing to reload it from" // sfx
        end if
        if (self%detached) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "reload: this table has been detached from its file by a " // &
                "row-structural change, so a re-read would no longer line up" // sfx
        end if
        ! LAST of the three, for the reason table_evict_column's own comment gives at length: the
        ! two checks above have no force= escape and must not gain one, and testing this first
        ! would tell the caller of an %add_column column to pass force=.true. -- advice that then
        ! hits the abort above anyway, since %add_column claims the column it creates.
        !
        ! Discarding the caller's edits is what %reload is FOR, but it is a strong enough action
        ! to be worth saying rather than assuming -- and it puts %reload under the same rule as
        ! %evict_column instead of a second one. A caller reloading a column they never wrote to
        ! loses nothing by the guard, because a column nobody wrote to is never marked.
        if (self%cache%cols(idx)%user_populated .and. .not. forced) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "reload: this column holds values written into the table, and " // &
                "reloading would replace them with the file's own; pass force=.true. if that " // &
                "is what you mean" // sfx
        end if
        ! Drop what is there and take the first-touch path again, so a reload and a first read
        ! cannot drift apart.
        call self%cache%cols(idx)%values%clear()
        self%cache%cols(idx)%residency = RES_EMPTY
        ! Whatever was here is being replaced by the file's own values, so the claim goes with it.
        ! Load-bearing, not tidiness: leaving it set would make the NEXT %evict_column refuse a
        ! column that now holds exactly what the file holds, and would let a non-resident column
        ! be marked -- which the rest of this file (and %print_stat) takes to be impossible.
        self%cache%cols(idx)%user_populated = .false.
        ! A re-read replaces the column's storage outright, so any pointer into it is stale --
        ! which is what the generation counter is for (%generation).
        self%cache%generation = self%cache%generation + 1_int64
        call table_touch(self%cache, table_scope_of(self), idx, "reload")
    end procedure table_reload
    !
    module procedure table_set_user_populated
        integer :: idx
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, "set_user_populated")
        ! A descriptor mutation on a table another thread may be reading, so it belongs outside a
        ! parallel region like every other one -- exactly as %ensure_validity is guarded.
        call table_check_not_shared(self, "set_user_populated")
        ! table_lookup_or_fail, not table_resolve: marking a column must not READ it, for the same
        ! reason %evict_column gives for its own lookup.
        call table_lookup_or_fail(self, name, "set_user_populated", idx, found)
        if (idx == 0) return
        ! Nothing in an empty slot for the caller to be claiming, and the mark would outlive the
        ! read that eventually fills it -- so %reload would then refuse a column holding exactly
        ! what the file holds. Clearing stays unconditional: a slot with no values has no claim on
        ! it either way, so `.false.` is always already true and saying so again costs nothing.
        if (flag .and. self%cache%cols(idx)%residency /= RES_FULL) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "set_user_populated: this column holds no values to claim -- it " // &
                "has not been read, or was evicted; read it first (%prefetch/%get), or pass " // &
                ".false." // sfx
        end if
        ! No guard against being called twice, and no %generation() bump. The first because
        ! setting a boolean to the same value twice is provably idempotent, which is the stated
        ! exemption from the check-before-mutate default. The second because nothing here
        ! reallocates: no %col pointer and no row or column handle is invalidated, and bumping
        ! would force every outstanding handle to be re-fetched for a change that cannot have
        ! affected any of them -- including the handle whose %ref write is the reason this
        ! procedure exists, which could then never call it.
        self%cache%cols(idx)%user_populated = flag
    end procedure table_set_user_populated
    !
    module procedure table_is_user_populated
        integer :: idx
        !
        ok = .false.
        call table_check_open(self, "is_user_populated")
        ! A query: no shared-table guard (it mutates nothing) and no read (table_lookup_or_fail).
        call table_lookup_or_fail(self, name, "is_user_populated", idx, found)
        if (idx == 0) return
        ok = self%cache%cols(idx)%user_populated
    end procedure table_is_user_populated
    !
    !> The part of a (possibly dotted) column path before its first "." -- i.e. the name the
    !! reader caches the decoded array under. A name with no dot is its own top level.
    !!
    !! A subroutine, not a `character(len=:), allocatable` function, per CLAUDE.md's "no
    !! character-returning function" rule -- confirmed via ThreadSanitizer that the earlier
    !! function form raced two OpenMP threads on gfortran's hidden, non-thread-local
    !! length-tracking temporary (GCC PR113797), corrupting memory that only surfaced later,
    !! in unrelated code.
    subroutine top_level_of(path, top)
        character(len=*), intent(in) :: path                  !! column path, dotted or not.
        character(len=:), allocatable, intent(out) :: top      !! the top-level field name.
        integer :: dot
        !
        dot = index(path, ".")
        if (dot == 0) then
            top = trim(path)
        else
            top = path(1:dot - 1)
        end if
    end subroutine top_level_of
    !
end submodule parquet_tables_read
