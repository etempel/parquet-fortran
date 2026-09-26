!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Independent, self-contained module for Parquet BYTE_ARRAY/STRING column storage.
!!
!! Provides two public types:
!!
!! * `parquet_string_column` -- an owning, contiguous Arrow-`LargeUtf8`-compatible string
!!   column (int64 offsets + a packed byte payload + a lazily allocated, bit-packed validity
!!   bitmap). Designed for row-group-chunked appends, tens-to-hundreds of millions of rows and
!!   multi-gigabyte payloads with a minimal allocation count and good cache locality.
!! * `parquet_string` -- a lightweight, non-owning handle referring to one element of a column
!!   (a column pointer + a 1-based index), resolved lazily on access so it survives appends to
!!   the column. It owns nothing and frees nothing.
!!
!! This module depends only on `iso_fortran_env` and `iso_c_binding`; it has no dependency on
!! any other module in this library (the read/write integration layer depends on it, never the
!! reverse). See `raw_buffers`/`append_buffers` for the buffer-level interop hooks the future
!! Parquet read/write path consumes.
module parquet_strings
    use, intrinsic :: iso_fortran_env, only : int8, int32, int64
    ! The ONLY reason this otherwise self-contained module imports anything of the
    ! library's: its two print procedures are solicited output, and verbosity="silent"
    ! governs those exactly as it governs %print_stat -- and, since threading arrived, the
    ! string-column thread cap, which is a setting for the same reason every other thread cap is.
    !
    ! It imports parquet_settings_BASE rather than parquet_settings, and that is load-bearing
    ! rather than tidiness: parquet_settings imports parquet_bindings to mirror the C++-side knobs,
    ! so importing it here would make a program whose only import is `use parquet_strings` fail to
    ! link without the whole Arrow/Parquet C++ stack -- which is exactly the independence this
    ! module exists to offer. The base module is a leaf. Enforced by
    ! check_parquet_strings_stays_leaf (tools/check_source_conventions.py).
    use parquet_settings_base
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_in_parallel
#endif
    use, intrinsic :: iso_c_binding, only : c_ptr, c_loc, c_f_pointer, c_null_ptr, c_associated
    !
    implicit none
    private
    !
    public :: parquet_string_column
    public :: parquet_string
    public :: parquet_string_threads
    !
    ! ---- Re-exported from parquet_settings_base ----
    !
    ! **A module re-exports, get and set, every knob its own code reads.** A program that imports
    ! this module for its capability must be able to configure that capability from the same import;
    ! otherwise the only route is `use parquet_settings`, which reaches `parquet_bindings` and drags
    ! the whole Arrow stack back into a build this module exists to keep clear of it. The output
    ! pair comes too wherever the module can emit or suppress output.
    public :: parquet_set_string_threads, parquet_get_string_threads
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream
    !
    public :: parquet_debug_set_string_min_bytes
    public :: parquet_debug_set_string_max_auto_threads
    public :: parquet_debug_string_row_ranges
    public :: parquet_debug_string_bulk_threads
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_strings: "
    !
    !> Per-bit masks for the LSB-first, 1=valid validity bitmap (bit 7 = 10000000b = -128 as int8).
    !! ibset(0_int8, 7) yields the bit-7 value without an out-of-range int8 literal.
    integer(int8), parameter :: BIT_MASK(0:7) = [1_int8, 2_int8, 4_int8, 8_int8, &
        16_int8, 32_int8, 64_int8, ibset(0_int8, 7)]
    !
    !> One validity byte with all eight rows valid -- the value `ensure_validity_cap` initialises
    !! fresh bytes to, named here so a bulk fill and that initialisation cannot drift apart.
    integer(int8), parameter :: ALL_VALID_BYTE = -1_int8
    !
    !> Minimum initial row / character capacity for the first allocation from empty.
    integer(int64), parameter :: MIN_ROW_CAP = 16_int64
    integer(int64), parameter :: MIN_CHAR_CAP = 64_int64
    !
    !> Payload below which a bulk operation runs serially however many threads are available.
    !!
    !! **Measured in BYTES, not rows**, because that is what the work actually scales with: a column
    !! of 10 M single-character elements and one of 10 k ten-kilobyte elements have wildly different
    !! row counts and nearly the same payload, and it is the payload that has to be copied. The
    !! value is one order of magnitude above the point where thread startup stops dominating a
    !! memcpy of that size on the machines this was measured on.
    integer(int64), parameter :: STRING_MIN_BYTES = 262144_int64
    !
    !> Fewest threads worth splitting a bulk rebuild across. Below this the operation runs serial.
    !!
    !! **This is not a tuning preference, it is the break-even of a structural cost.** Making a
    !! rebuild splittable requires a three-phase form (lengths, scan, copy) that is measurably slower
    !! than the single pass it replaces -- 0.0266 s against 0.0162 s on 4 M elements / 70 MB, because
    !! a permuted `offsets(perm(k))` read misses cache and the split pays that miss twice where one
    !! pass pays it once. So the parallelism has to beat roughly 1.7x before it breaks even at all,
    !! and measurement agrees: 2 threads is a net LOSS (0.0177 s), 4 is the first real gain
    !! (0.0136 s). Measured on an 8-core M1 Pro; a machine with more memory bandwidth would break
    !! even sooner, so this is a conservative floor rather than a universal one.
    integer, parameter :: STRING_MIN_THREADS = 4
    !
    !> Most threads the AUTOMATIC answer will ever ask for. An explicit `parquet_set_string_threads`
    !! is honoured above this; only the unconfigured default is bounded by it.
    !!
    !! **Measured, and the reason is that more threads eventually make this SLOWER, not merely no
    !! faster.** On a 2-socket, 192-physical-core (384-logical) EPYC 9654 running `ifx`, the same
    !! `%reindex_trusted` sweep peaks in the 64-128 band and then loses ground as the thread count
    !! rises into the SMT siblings:
    !!
    !! | threads | 4 M rows | 40 M rows | 4 M with nulls |
    !! |---|---|---|---|
    !! | 64 | 3.18x | **4.51x** | **3.54x** |
    !! | 128 | **3.42x** | 4.40x | 3.44x |
    !! | 256 | 2.73x | 3.40x | 3.36x |
    !! | 384 | 2.43x | 2.70x | 2.89x |
    !!
    !! Taking `omp_get_max_threads()` there means 384 -- close to the *worst* threaded point of every
    !! sweep, giving up 18-40 % of the speedup that was available, for a caller who did nothing wrong.
    !! 64 is chosen over 128 because it is optimal on the two larger/realistic runs, costs 7 % on the
    !! smallest, and is the less aggressive number for a library to claim from an application that may
    !! have its own plans for the machine.
    !!
    !! This is a ceiling on a DEFAULT, not a limit on the operation: `parquet_set_string_threads(128)`
    !! gets 128. Re-measure before changing it; the useful band was found, not merely approached.
    integer, parameter :: STRING_MAX_AUTO_THREADS = 64
    !
    !> Test-only override of `STRING_MIN_BYTES`; `<= 0` restores the real constant.
    !!
    !! **Exists because no fixture a test suite can afford reaches the real floor.** Every column
    !! small enough to build in a unit test sits far below 256 KiB, so without this every test would
    !! silently exercise the serial path and a threaded operation would be covered by nothing --
    !! exactly the failure where a dense sweep over small arrays never reached the code it was
    !! written for.
    integer(int64), save :: dbg_string_min_bytes = 0_int64
    !
    !> Test-only override of `STRING_MAX_AUTO_THREADS`; `<= 0` restores the real constant.
    !!
    !! **Without this the ceiling is untestable on any ordinary development machine**, because it only
    !! binds when OpenMP offers more threads than the ceiling allows -- 64 on a machine with 8 cores
    !! is never reached, so a change that dropped the ceiling entirely would pass every test written
    !! for it and only show up on a 192-core node. Lowering it is how a small machine exercises the
    !! same branch. Same reasoning as `dbg_string_min_bytes`, and the same failure mode.
    integer, save :: dbg_string_max_auto = 0
    !
    !> An owning, Arrow-LargeUtf8-compatible variable-length string column.
    !!
    !! Storage is Arrow-like and contiguous: `offsets` (int64, 1-based Fortran array with
    !! `offsets(1)=0`, logical length `nrows+1`), a packed `data` byte payload, and a lazily
    !! allocated bit-packed `validity` bitmap (1=valid). Element `i` (1-based) occupies
    !! `data(offsets(i)+1 : offsets(i+1))`. All components are private; access is via the
    !! type-bound procedures. Row/character indexing is `integer(int64)` throughout.
    type :: parquet_string_column
        private
        integer(int64), allocatable :: offsets(:)     !! int64 offsets; offsets(1)=0, length nrows+1.
        character(len=1), allocatable :: data(:)       !! packed byte payload; used bytes = nchars.
        integer(int8), allocatable :: validity(:)      !! bit-packed bitmap (1=valid); lazy.
        integer(int64) :: nrows = 0                    !! number of elements stored.
        integer(int64) :: nchars = 0                   !! total characters stored (= offsets(nrows+1)).
        integer(int64) :: n_null = 0                   !! cached number of null elements.
        logical :: has_nulls = .false.                 !! .true. once the validity bitmap is materialized.
    contains
        ! --- construction / memory management ---
        procedure :: clear                             !! Reset to empty and release all owned buffers.
        procedure, private :: reserve_i32              !! int32 specific of reserve.
        procedure, private :: reserve_i64              !! int64 specific of reserve.
        generic :: reserve => reserve_i32, reserve_i64 !! Grow capacity for at least n_rows/n_characters.
        procedure :: shrink_to_fit                     !! Reallocate buffers down to the current size.
        procedure :: capacity                          !! Current row capacity.
        procedure :: character_capacity                !! Current character-buffer capacity (bytes).
        procedure :: size => col_size                  !! Number of elements stored.
        procedure :: character_size                    !! Total characters stored.
        procedure :: empty => col_empty                !! .true. when no rows are stored.
        procedure :: null_count                        !! Number of null elements.
        procedure :: has_validity                      !! Whether the validity bitmap exists yet.
        procedure :: reserve_validity                  !! Materialize the validity bitmap up front.
        procedure :: memory_usage                      !! Total bytes of allocated buffers.
        procedure :: validate                          !! Verify class invariants.
        ! --- access ---
        procedure, private :: length_i32               !! int32 specific of length.
        procedure, private :: length_i64               !! int64 specific of length.
        generic :: length => length_i32, length_i64    !! Length of element i (no allocation).
        procedure, private :: get_i32                  !! int32 specific of get.
        procedure, private :: get_i64                  !! int64 specific of get.
        generic :: get => get_i32, get_i64             !! Writes element i into an allocatable string.
        procedure, private :: copy_to_i32              !! int32 specific of copy_to.
        procedure, private :: copy_to_i64              !! int64 specific of copy_to.
        generic :: copy_to => copy_to_i32, copy_to_i64 !! Copies element i into a fixed-length slot.
        procedure, private :: view_i32                 !! int32 specific of view.
        procedure, private :: view_i64                 !! int64 specific of view.
        generic :: view => view_i32, view_i64          !! Zero-copy handle to element i.
        procedure :: view_all                          !! One handle per element, in order.
        procedure, private :: view_slice_i32           !! int32 specific of view_slice.
        procedure, private :: view_slice_i64           !! int64 specific of view_slice.
        generic :: view_slice => view_slice_i32, view_slice_i64 !! One handle per row of [first, last].
        procedure, private :: is_null_i32              !! int32 specific of is_null.
        procedure, private :: is_null_i64              !! int64 specific of is_null.
        generic :: is_null => is_null_i32, is_null_i64 !! Whether element i is null.
        procedure, private :: is_empty_i32             !! int32 specific of is_empty.
        procedure, private :: is_empty_i64             !! int64 specific of is_empty.
        generic :: is_empty => is_empty_i32, is_empty_i64 !! Whether element i has zero length.
        ! --- modification ---
        procedure :: append_string                     !! Append a string to the end.
        procedure :: append_null                       !! Append a null element to the end.
        procedure :: append_column                     !! Append all elements from another column.
        procedure :: append_values                     !! Bulk-append a character array, trimming each element.
        procedure, private :: append_from_i32          !! int32 specific of append_from.
        procedure, private :: append_from_i64          !! int64 specific of append_from.
        generic :: append_from => append_from_i32, append_from_i64 !! Append one element of another column.
        procedure, private :: build_from_handles       !! handles specific of build_from.
        procedure, private :: build_from_character     !! character-array specific of build_from.
        generic :: build_from => build_from_handles, build_from_character
        !! Clears self, then fills it -- from an array of handles, or from a character array
        !! (trailing blanks trimmed, one optional null mask). Both replace the whole column.
        procedure, private :: set_i32                  !! int32 specific of set.
        procedure, private :: set_i64                  !! int64 specific of set.
        generic :: set => set_i32, set_i64             !! Replace the content of element i.
        procedure, private :: set_null_i32             !! int32 specific of set_null.
        procedure, private :: set_null_i64             !! int64 specific of set_null.
        generic :: set_null => set_null_i32, set_null_i64 !! Sets element i to null (discards any content).
        procedure, private :: erase_i32                !! int32 specific of erase.
        procedure, private :: erase_i64                !! int64 specific of erase.
        generic :: erase => erase_i32, erase_i64       !! Remove element i (shifts later elements down).
        procedure, private :: reindex_i32              !! int32 specific of reindex.
        procedure, private :: reindex_i64              !! int64 specific of reindex.
        generic :: reindex => reindex_i32, reindex_i64 !! Reorder every element by a permutation.
        procedure, private :: reindex_trusted_i32      !! int32 specific of reindex_trusted.
        procedure, private :: reindex_trusted_i64      !! int64 specific of reindex_trusted.
        !> INTERNAL -- reindex without the duplicate/range scan. Public only because Fortran offers
        !! no narrower visibility -- see reindex_trusted_i64.
        generic :: reindex_trusted => reindex_trusted_i32, reindex_trusted_i64
        procedure :: delete_by_mask                    !! Keep only the elements whose mask entry is .true.
        procedure :: set_validity                      !! Null every element whose mask entry is .false., in one pass.
        procedure :: set_where                         !! Write one value into every element whose mask entry is .true.
        procedure, private :: gather_i32               !! int32 specific of gather.
        procedure, private :: gather_i64               !! int64 specific of gather.
        generic :: gather => gather_i32, gather_i64    !! Keep the listed elements, in the listed order.
        procedure, private :: gather_from_i32          !! int32 specific of gather_from.
        procedure, private :: gather_from_i64          !! int64 specific of gather_from.
        !> Build this column from another's listed elements in one pass, with an optional null
        !! mask -- clone + gather + set_validity as one operation.
        generic :: gather_from => gather_from_i32, gather_from_i64
        procedure, private :: append_nulls_i32         !! int32 specific of append_nulls.
        procedure, private :: append_nulls_i64         !! int64 specific of append_nulls.
        generic :: append_nulls => append_nulls_i32, append_nulls_i64 !! Append n null elements in bulk.
        procedure :: strip_all                         !! Strip both ends of every non-null element.
        procedure :: trim_all                          !! Trailing-trim every non-null element.
        ! --- searching / comparison ---
        procedure :: find                              !! Index of first/last element equal to str.
        procedure, private :: contains_i32             !! int32 specific of contains.
        procedure, private :: contains_i64             !! int64 specific of contains.
        generic :: contains => contains_i32, contains_i64 !! Whether element i contains a substring.
        procedure, private :: startswith_i32           !! int32 specific of startswith.
        procedure, private :: startswith_i64           !! int64 specific of startswith.
        generic :: startswith => startswith_i32, startswith_i64 !! Whether element i begins with prefix.
        procedure, private :: endswith_i32             !! int32 specific of endswith.
        procedure, private :: endswith_i64             !! int64 specific of endswith.
        generic :: endswith => endswith_i32, endswith_i64 !! Whether element i ends with suffix.
        procedure, private :: equals_i32               !! int32 specific of equals.
        procedure, private :: equals_i64               !! int64 specific of equals.
        generic :: equals => equals_i32, equals_i64    !! Whether element i equals str.
        procedure :: compare                           !! Orders element i against element j.
        procedure :: argminmax                         !! Indices of the smallest and largest elements.
        ! --- conversion / ownership ---
        procedure :: to_character                      !! Materialize the whole column as a char array.
        procedure :: clone                             !! Independent deep copy.
        procedure, private :: slice_i32                !! int32 specific of slice.
        procedure, private :: slice_i64                !! int64 specific of slice.
        generic :: slice => slice_i32, slice_i64       !! Independent, owning copy of rows [first, last].
        procedure :: move_from                         !! Transfer all buffers from another column.
        procedure :: swap                              !! Exchange contents with another column.
        ! --- diagnostics ---
        procedure :: print => col_print                !! Human-readable representation.
        procedure :: summary                           !! Writes a compact one-line overview string.
        procedure :: statistics                        !! Detailed metrics (optional out-args).
        ! --- interop hooks (advanced; for the Parquet read/write integration layer) ---
        procedure :: raw_buffers                       !! Export c_loc pointers to the internal buffers.
        procedure :: copy_buffers                      !! Copy the offsets and packed payload into caller arrays.
        procedure :: append_buffers                    !! Bulk-append one row group from C buffers.
        !
        ! --- NO `final` PROCEDURE, deliberately ---
        !
        ! This type owns three ALLOCATABLE components (offsets, data, validity) and nothing else --
        ! no pointer, no handle, no external resource. Fortran deallocates allocatable components
        ! automatically whenever the object ceases to exist, is deallocated, or is entered as an
        ! `intent(out)` dummy (F2018 9.7.3.2), so the `final :: finalize_column` this type used to
        ! carry -- whose entire body was three `if (allocated(x)) deallocate(x)` lines -- freed
        ! nothing the language was not already freeing. Contrast parquet_writer/parquet_reader/
        ! parquet_table, whose finalizers release C++ handles and OpenMP locks the language knows
        ! nothing about: those are real and must stay.
        !
        ! Do not add one back. Three things now depend on the absence:
        !   * nagfor 7.2 emits INVALID C under -C=undefined when finalizing an ARRAY whose element
        !     type has a finalizable COMPONENT. parquet_column embeds one of these, and
        !     parquet_table_column embeds a parquet_column, so `cache%cols(:)` reached it
        !     transitively and src/parquet_tables_lifecycle.f90 would not compile.
        !   * a finalizable type may not go in an OpenMP `private()` clause under gfortran.
        !   * intrinsic assignment to or from a finalizable type runs the finalizer twice per
        !     iteration, which a loop assigning columns would pay on every element.
    end type parquet_string_column
    !
    !> A lightweight, non-owning handle to one element of a parquet_string_column.
    !!
    !! Holds a column pointer plus a 1-based index and resolves lazily on access, so it stays
    !! valid across appends/reserve/shrink of the referenced column. It is invalidated by
    !! `erase` (index shift), `clear`, `move_from`/`swap`, or the column going out of scope. The
    !! referenced column must be declared with the `target` attribute and must outlive the handle.
    type :: parquet_string
        private
        class(parquet_string_column), pointer :: col => null() !! referenced column (borrowed).
        integer(int64) :: idx = 0                              !! 1-based element index.
    contains
        procedure :: length => psv_length          !! Length of the referenced string.
        procedure :: is_empty => psv_is_empty       !! Whether the referenced string has zero length.
        procedure :: is_null => psv_is_null         !! Whether the referenced element is null.
        procedure :: set_null => psv_set_null       !! Sets the referenced column element to null.
        procedure :: to_string => psv_to_string     !! Writes the referenced string into an argument.
        procedure :: equals => psv_equals           !! Exact comparison against str.
        procedure :: contains => psv_contains       !! Substring search for str.
        procedure :: startswith => psv_startswith   !! Prefix test.
        procedure :: endswith => psv_endswith       !! Suffix test.
        procedure :: print => psv_print             !! Human-readable representation.
        !
        ! THIS TYPE DELIBERATELY HAS NO `final` PROCEDURE, and one must not be added for symmetry
        ! with parquet_string_column above -- that one owns buffers and genuinely needs to free
        ! them, whereas this is a NON-OWNING handle: a borrowed pointer and an index. It once
        ! carried `final :: finalize_handle`, whose entire body was `self%col => null()`; that is
        ! unobservable on every path finalization can occur (the object is either ceasing to exist
        ! or being wholly overwritten, and Fortran never auto-deallocates a pointer component, so
        ! there was nothing to release and no dangling reference the caller could still reach).
        !
        ! Three things depend on the absence, so weigh them before reintroducing one:
        !   * nagfor 7.2 emits INVALID C under -C=undefined when finalizing an ARRAY whose element
        !     type has a finalizable COMPONENT -- so `type(t_row) :: rows(3)` holding one of these
        !     would not compile. See fpm.toml's nagdeb comment for the reproducer.
        !   * a finalizable type may not go in an OpenMP `private()` clause under gfortran (CLAUDE.md
        !     has the rule); handles staying non-finalizable is what keeps them usable there, which
        !     is the same reason parquet_table_row and parquet_table_col have no finalizer either.
        !   * intrinsic assignment to or from a finalizable type runs the finalizer twice per
        !     iteration, so `h = col%view(i)` in a user loop would pay for it on every element.
    end type parquet_string
    !
    ! ---- The TYPED (non-polymorphic) tier: `parquet_string_column_*` ---------------------
    !
    ! Every name below is the `type(parquet_string_column)`-dummy IMPLEMENTATION of the like-named
    ! binding above, which is now a one-line forwarder onto it. `parquet_columns` calls these and
    ! never `%str%<binding>(...)`: passing a `type(parquet_string_column)` actual (the `str`
    ! component of a `parquet_column`) to a `class` dummy makes ifx build a runtime type descriptor
    ! -- one record per allocatable component, 24 stores -- in the CALLER's prologue,
    ! unconditionally, ahead of any branch. Those stores land in STATIC storage, so every thread
    ! writes the same cache lines and a per-element scan collapses under OpenMP: measured at
    ! 38.9 s against 0.26 s for one 200000-row null scan on 384 threads.
    !
    ! The generics are public so that `parquet_columns` can reach them; the specifics beneath them
    ! stay private, and `src/parquet.f90` privatises every generic again, so the surface a
    ! `use parquet` program sees is unchanged. `check_no_type_bound_string_column_access`
    ! (tools/check_source_conventions.py) is the enforcement.
    !
    public :: parquet_string_column_clear
    public :: parquet_string_column_shrink_to_fit
    public :: parquet_string_column_capacity
    public :: parquet_string_column_character_capacity
    public :: parquet_string_column_size
    public :: parquet_string_column_character_size
    public :: parquet_string_column_null_count
    public :: parquet_string_column_has_validity
    public :: parquet_string_column_reserve_validity
    public :: parquet_string_column_append_column
    public :: parquet_string_column_append_null
    public :: parquet_string_column_append_values
    public :: parquet_string_column_delete_by_mask
    public :: parquet_string_column_set_validity
    public :: parquet_string_column_set_where
    public :: parquet_string_column_reserve
    public :: parquet_string_column_get
    public :: parquet_string_column_copy_to
    public :: parquet_string_column_is_null
    public :: parquet_string_column_append_from
    public :: parquet_string_column_set
    public :: parquet_string_column_set_null
    public :: parquet_string_column_reindex
    public :: parquet_string_column_reindex_trusted
    public :: parquet_string_column_gather
    public :: parquet_string_column_gather_from
    public :: parquet_string_column_append_nulls
    public :: parquet_string_column_move_from
    public :: parquet_string_column_raw_buffers
    public :: parquet_string_column_argminmax
    !
    !> Typed form of `%reserve`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_reserve
        module procedure parquet_string_column_reserve_i32
        module procedure parquet_string_column_reserve_i64
    end interface parquet_string_column_reserve
    !> Typed form of `%get`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_get
        module procedure parquet_string_column_get_i32
        module procedure parquet_string_column_get_i64
    end interface parquet_string_column_get
    !> Typed form of `%copy_to`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_copy_to
        module procedure parquet_string_column_copy_to_i32
        module procedure parquet_string_column_copy_to_i64
    end interface parquet_string_column_copy_to
    !> Typed form of `%is_null`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_is_null
        module procedure parquet_string_column_is_null_i32
        module procedure parquet_string_column_is_null_i64
    end interface parquet_string_column_is_null
    !> Typed form of `%append_from`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_append_from
        module procedure parquet_string_column_append_from_i32
        module procedure parquet_string_column_append_from_i64
    end interface parquet_string_column_append_from
    !> Typed form of `%set`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_set
        module procedure parquet_string_column_set_i32
        module procedure parquet_string_column_set_i64
    end interface parquet_string_column_set
    !> Typed form of `%set_null`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_set_null
        module procedure parquet_string_column_set_null_i32
        module procedure parquet_string_column_set_null_i64
    end interface parquet_string_column_set_null
    !> Typed form of `%reindex`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_reindex
        module procedure parquet_string_column_reindex_i32
        module procedure parquet_string_column_reindex_i64
    end interface parquet_string_column_reindex
    !> Typed form of `%reindex_trusted`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_reindex_trusted
        module procedure parquet_string_column_reindex_trusted_i32
        module procedure parquet_string_column_reindex_trusted_i64
    end interface parquet_string_column_reindex_trusted
    !> Typed form of `%gather`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_gather
        module procedure parquet_string_column_gather_i32
        module procedure parquet_string_column_gather_i64
    end interface parquet_string_column_gather
    !> Typed form of `%gather_from`, generic over an int32 or int64 index argument.
    interface parquet_string_column_gather_from
        module procedure parquet_string_column_gather_from_i32
        module procedure parquet_string_column_gather_from_i64
    end interface parquet_string_column_gather_from
    !> Typed form of `%append_nulls`, generic over an int32 or int64 index/count argument.
    interface parquet_string_column_append_nulls
        module procedure parquet_string_column_append_nulls_i32
        module procedure parquet_string_column_append_nulls_i64
    end interface parquet_string_column_append_nulls
    !
contains
    !
    ! ==================================================================================
    ! Internal helpers (private module procedures)
    ! ==================================================================================
    !
    !> How many threads one `parquet_string_column` bulk operation would use here, right now.
    !!
    !! **Public for the same reason `pf_sort_threads` is**: this is the ONE place the cap and the
    !! OpenMP environment are combined, so a caller asking "what will this do?" and the operation
    !! itself can never give different answers. A second reader is how the two would come to
    !! disagree.
    !!
    !! Three rules, in this order:
    !!
    !! * **Serial inside an OpenMP parallel region**, deliberately, and this is not a refusal -- it
    !!   picks a DEFAULT, exactly as `pf_sort_threads` and `parallel_prefetch_ok` do. `T` threads
    !!   each asking for `T` more is slower than not threading at all, and nesting is the caller's
    !!   business. Note `omp_get_max_threads()` reads an ICV rather than the current team size, so
    !!   inside an 8-thread region it answers 8 and a missing check means 8x8.
    !! * **Otherwise, an explicit `parquet_set_string_threads` is HONOURED**, bounded by what OpenMP
    !!   offers and by the CPU affinity this process actually has. A caller who names a number has
    !!   said what they want -- but a number the affinity mask cannot run is not something they can
    !!   have, and opening it would time-share the mask's processors rather than use more of them.
    !!   That last bound is `parquet_clamp_to_affinity` (src/parquet_settings_base.f90), shared with
    !!   sorting, table prefetching and the bulk random draws, and it warns once per process when it
    !!   bites.
    !! * **With no explicit setting, the automatic answer is capped at
    !!   `STRING_MAX_AUTO_THREADS`**, not taken as `omp_get_max_threads()`. See that constant for the
    !!   measurement; in short, a very large machine's full thread count is past the point where
    !!   this work stops scaling and is measurably *worse* than a fraction of it.
    !!
    !! **The second rule differs from `pf_sort_threads`, deliberately**, where a setting can only
    !! ever lower the automatic answer. Sorting has no measured ceiling of its own, so there is
    !! nothing for an explicit request to reach past; here there is, and refusing to honour it would
    !! leave a caller on a 192-core machine unable to ask for the 128 threads that machine's own
    !! measurement prefers.
    integer function parquet_string_threads() result(n)
        integer :: cap, avail
        n = 1
        avail = 1
#ifdef _OPENMP
        if (omp_in_parallel()) return
        avail = omp_get_max_threads()
#endif
        cap = parquet_get_string_threads()
        if (cap > 0) then
            n = min(cap, avail)
        else
            n = min(string_max_auto(), avail)
        end if
        if (n < 1) n = 1
        n = parquet_clamp_to_affinity(n, "string operations")
    end function parquet_string_threads
    !
    !> Overrides the payload floor below which a bulk operation stays serial. **Test-only**; `<= 0`
    !! restores the real `STRING_MIN_BYTES`.
    !!
    !! **Public only because Fortran has no narrower visibility, and deliberately accepted** --
    !! `parquet_debug_table_set_inflight` is the existing precedent and the reasoning is the same
    !! (`.claude/rules/testing.md`, "Debug hooks"). The C++ route
    !! is not available here: `parquet_strings` is standalone by design and reaches no `bind(C)`
    !! surface at all, so routing this through `parquet_wrapper.cpp` would cost the module's
    !! independence to save one public name. No library code calls this.
    subroutine parquet_debug_set_string_min_bytes(n)
        integer(int64), intent(in) :: n !! new floor in bytes, or <= 0 to restore the real one.
        dbg_string_min_bytes = n
    end subroutine parquet_debug_set_string_min_bytes
    !
    !> Overrides the ceiling on the AUTOMATIC thread count. **Test-only**; `<= 0` restores the real
    !! `STRING_MAX_AUTO_THREADS`. Public for the same reason its sibling above is.
    subroutine parquet_debug_set_string_max_auto_threads(n)
        integer, intent(in) :: n !! new ceiling, or <= 0 to restore the real one.
        dbg_string_max_auto = n
    end subroutine parquet_debug_set_string_max_auto_threads
    !
    !> What a bulk operation over `col` would actually resolve to, floor and all. **Test-only**.
    !!
    !! Returns the REAL decision rather than letting a test rebuild it: a test that reimplements
    !! `bulk_threads` asserts against its own copy, which drifts the moment the rule changes and
    !! then agrees with itself forever. This is the subject a knob's observed-effect test measures.
    integer function parquet_debug_string_bulk_threads(col, threads) result(n)
        type(parquet_string_column), intent(in) :: col !! the column an operation would walk.
        !> an explicit request, as `%gather_from(..., threads=)` takes; absent asks the automatic
        !! rule. The explicit answer is `bulk_threads_explicit`'s for a selection the size of `col`.
        integer, intent(in), optional :: threads
        if (present(threads)) then
            n = bulk_threads_explicit(threads, col%nrows, col%nchars)
        else
            n = bulk_threads(col%nrows, col%nchars)
        end if
    end function parquet_debug_string_bulk_threads
    !
    !> Exposes `thread_row_ranges` for testing. **Test-only**; no library code calls it.
    !!
    !! Public for the same reason `parquet_debug_set_string_min_bytes` is, and with more at stake:
    !! the byte-alignment rule this returns is the one property of the threading layer whose failure
    !! is a **silent wrong answer** (two threads sharing a validity byte lose one another's writes,
    !! and the column still validates). It has no consumer inside the module until S5, so without
    !! this it would ship untested — which is precisely how a rule everyone agrees with comes to be
    !! implemented wrongly.
    subroutine parquet_debug_string_row_ranges(n, nt, lo, hi)
        integer(int64), intent(in) :: n                   !! total rows.
        integer, intent(in) :: nt                         !! number of ranges.
        integer(int64), allocatable, intent(out) :: lo(:) !! first row of each range.
        integer(int64), allocatable, intent(out) :: hi(:) !! last row of each range.
        call thread_row_ranges(n, nt, lo, hi)
    end subroutine parquet_debug_string_row_ranges
    !
    !> The payload floor actually in force: the test override where one is set, otherwise the real
    !! constant. One reader, so the two cannot drift.
    integer(int64) function string_floor_bytes() result(n)
        n = STRING_MIN_BYTES
        if (dbg_string_min_bytes > 0_int64) n = dbg_string_min_bytes
    end function string_floor_bytes
    !
    !> The automatic thread ceiling actually in force: the test override where one is set, otherwise
    !! the real constant. One reader, so the two cannot drift.
    integer function string_max_auto() result(n)
        n = STRING_MAX_AUTO_THREADS
        if (dbg_string_max_auto > 0) n = dbg_string_max_auto
    end function string_max_auto
    !
    !> Threads a bulk operation over `payload` bytes should actually use: `parquet_string_threads()`
    !! narrowed by the work floor and by having at least one whole validity byte (8 rows) per thread.
    !!
    !! **The 8-rows-per-thread clamp is not a tuning choice.** `thread_row_ranges` can only produce
    !! byte-aligned boundaries, so asking for more threads than there are validity bytes yields empty
    !! ranges; clamping here keeps that impossible rather than merely unlikely.
    integer function bulk_threads(nrows, payload) result(n)
        integer(int64), intent(in) :: nrows   !! rows the operation will walk.
        integer(int64), intent(in) :: payload !! bytes the operation will move.
        n = 1
        if (nrows <= 0_int64) return
        if (payload < string_floor_bytes()) return
        n = parquet_string_threads()
        if (int(n, int64) > (nrows + 7_int64)/8_int64) n = int((nrows + 7_int64)/8_int64)
        ! Below the break-even the split costs more than it saves, so decline outright rather than
        ! run a slower shape on two threads. See STRING_MIN_THREADS.
        if (n < STRING_MIN_THREADS) n = 1
        if (n < 1) n = 1
    end function bulk_threads
    !
    !> Threads a bulk operation over `payload` bytes should use when the caller NAMED a count: the
    !! count itself, bounded as an explicit `parquet_set_string_threads` is (`parquet_string_threads`'
    !! second rule: the affinity mask), by the validity-byte rule and the break-even `bulk_threads`
    !! applies, and by the payload floor -- but NOT stood down inside a parallel region. A caller
    !! that names a count has decided where the team goes: `parquet_table`'s per-column rewrite
    !! hands its whole team to one column at a time (`%gather_from(..., threads=)`), and an
    !! explicit `threads=` on a sort is honoured the same way.
    integer function bulk_threads_explicit(threads, nrows, payload) result(n)
        integer, intent(in) :: threads        !! the count asked for.
        integer(int64), intent(in) :: nrows   !! rows the operation will walk.
        integer(int64), intent(in) :: payload !! bytes the operation will move.
        n = 1
        if (nrows <= 0_int64) return
        if (payload < string_floor_bytes()) return
        n = max(threads, 1)
        if (n > 1) n = parquet_clamp_to_affinity(n, "string operations")
        if (int(n, int64) > (nrows + 7_int64)/8_int64) n = int((nrows + 7_int64)/8_int64)
        if (n < STRING_MIN_THREADS) n = 1
        if (n < 1) n = 1
    end function bulk_threads_explicit
    !
    !> Splits rows `1..n` into `nt` contiguous ranges whose boundaries fall on **validity BYTE**
    !! boundaries -- every `lo` is `1 mod 8` and every `hi` is `0 mod 8` except the last.
    !!
    !! **This is the correctness rule of the whole threading layer, not a convenience.** The validity
    !! bitmap packs 8 rows per byte, so two threads writing rows in the same byte race on that byte:
    !! a read-modify-write each, one of which is lost. Nothing aborts, the column still validates,
    !! and the nulls are simply wrong. Splitting on
    !! arbitrary row counts is what makes that possible; splitting on byte boundaries makes it
    !! impossible, because no two threads ever touch the same byte. Pinned by the test
    !! `thread row ranges cover every row and never share a validity byte` (`test/test_parquet_string.f90`).
    !!
    !! Ranges are returned even when the split is uneven, and an empty range (`lo > hi`) is a valid
    !! answer for a trailing thread -- callers must tolerate it rather than assume every thread gets
    !! work.
    subroutine thread_row_ranges(n, nt, lo, hi)
        integer(int64), intent(in) :: n                  !! total rows (>= 0).
        integer, intent(in) :: nt                        !! number of ranges (>= 1).
        integer(int64), allocatable, intent(out) :: lo(:) !! first row of each range.
        integer(int64), allocatable, intent(out) :: hi(:) !! last row of each range; hi < lo when empty.
        integer(int64) :: nbytes, per, extra, cur, take
        integer :: k
        allocate(lo(nt), hi(nt))
        if (n <= 0_int64) then
            lo = 1_int64
            hi = 0_int64
            return
        end if
        ! Divide the BYTES, then convert back to rows -- which is what guarantees the alignment,
        ! rather than dividing rows and rounding afterwards.
        nbytes = (n + 7_int64)/8_int64
        per = nbytes/int(nt, int64)
        extra = mod(nbytes, int(nt, int64))
        cur = 1_int64
        do k = 1, nt
            take = per
            if (int(k, int64) <= extra) take = take + 1_int64
            lo(k) = cur
            hi(k) = min(cur + take*8_int64 - 1_int64, n)
            if (take == 0_int64) hi(k) = cur - 1_int64
            cur = hi(k) + 1_int64
        end do
        ! The last range always runs to n: integer division cannot leave a tail, but saying so here
        ! means a future change to the split above cannot silently drop rows.
        if (hi(nt) < n) hi(nt) = n
    end subroutine thread_row_ranges
    !
    !> Aborts with a bounds-violation message; called by every index-checked accessor.
    subroutine check_index(c, i, proc)
        type(parquet_string_column), intent(in) :: c !! the column.
        integer(int64), intent(in) :: i              !! the offending 1-based index.
        character(len=*), intent(in) :: proc         !! calling procedure name (for the message).
        if (i < 1 .or. i > c%nrows) then
            error stop EP//"index out of range in "//proc
        end if
    end subroutine check_index
    !
    !> Aborts unless [first,last] is a valid, non-empty 1-based inclusive row range within c%nrows.
    subroutine check_range(c, first, last, proc)
        type(parquet_string_column), intent(in) :: c !! the column.
        integer(int64), intent(in) :: first           !! first row of the range (1-based, inclusive).
        integer(int64), intent(in) :: last            !! last row of the range (1-based, inclusive).
        character(len=*), intent(in) :: proc          !! calling procedure name (for the message).
        if (first < 1_int64 .or. last > c%nrows .or. first > last) then
            error stop EP//"invalid row range in "//proc
        end if
    end subroutine check_range
    !
    !> Aborts because a null element was accessed where a non-null was required.
    subroutine fail_null(proc)
        character(len=*), intent(in) :: proc !! calling procedure name (for the message).
        error stop EP//"null element accessed in "//proc//" (guard with is_null, or pass a null option)"
    end subroutine fail_null
    !
    !> Ensures `offsets` is allocated with room for at least `need_rows` rows (length need_rows+1),
    !! initialising `offsets(1)=0` on first allocation and preserving existing entries on growth.
    subroutine ensure_offsets_cap(c, need_rows)
        type(parquet_string_column), intent(inout) :: c !! the column.
        integer(int64), intent(in) :: need_rows         !! required row capacity.
        integer(int64) :: need, newcap
        integer(int64), allocatable :: tmp(:)
        if (need_rows < 0) error stop EP//"row capacity overflow"
        need = need_rows + 1
        if (.not. allocated(c%offsets)) then
            newcap = max(need, MIN_ROW_CAP)
            allocate(c%offsets(newcap))
            c%offsets(1) = 0_int64
        else if (size(c%offsets, kind=int64) < need) then
            newcap = size(c%offsets, kind=int64)
            newcap = max(need, newcap + newcap/2_int64)
            allocate(tmp(newcap))
            tmp(1:c%nrows+1) = c%offsets(1:c%nrows+1)
            call move_alloc(tmp, c%offsets)
        end if
    end subroutine ensure_offsets_cap
    !
    !> Ensures `data` is allocated with room for at least `need_chars` bytes, preserving the
    !! already-used bytes on growth.
    subroutine ensure_data_cap(c, need_chars)
        type(parquet_string_column), intent(inout) :: c !! the column.
        integer(int64), intent(in) :: need_chars        !! required character capacity (bytes).
        integer(int64) :: newcap
        character(len=1), allocatable :: tmp(:)
        if (need_chars < 0) error stop EP//"character capacity overflow"
        if (.not. allocated(c%data)) then
            if (need_chars > 0) allocate(c%data(max(need_chars, MIN_CHAR_CAP)))
        else if (size(c%data, kind=int64) < need_chars) then
            newcap = size(c%data, kind=int64)
            newcap = max(need_chars, newcap + newcap/2_int64)
            allocate(tmp(newcap))
            if (c%nchars > 0) tmp(1:c%nchars) = c%data(1:c%nchars)
            call move_alloc(tmp, c%data)
        end if
    end subroutine ensure_data_cap
    !
    !> Ensures the validity bitmap is allocated to cover at least `need_rows` rows, initialising
    !! new bytes to all-ones (every row valid) and preserving existing bits on growth.
    subroutine ensure_validity_cap(c, need_rows)
        type(parquet_string_column), intent(inout) :: c !! the column.
        integer(int64), intent(in) :: need_rows         !! required row capacity.
        integer(int64) :: need_bytes, newcap, oldsz
        integer(int8), allocatable :: tmp(:)
        need_bytes = (need_rows + 7_int64)/8_int64
        if (.not. allocated(c%validity)) then
            allocate(c%validity(max(need_bytes, 1_int64)))
            c%validity = -1_int8
        else if (size(c%validity, kind=int64) < need_bytes) then
            oldsz = size(c%validity, kind=int64)
            newcap = max(need_bytes, oldsz + oldsz/2_int64)
            allocate(tmp(newcap))
            tmp = -1_int8
            tmp(1:oldsz) = c%validity(1:oldsz)
            call move_alloc(tmp, c%validity)
        end if
    end subroutine ensure_validity_cap
    !
    !> Returns whether element `i` (1-based) is valid (non-null). All rows are valid until the
    !! validity bitmap is materialized.
    logical function bit_valid(c, i)
        type(parquet_string_column), intent(in) :: c !! the column.
        integer(int64), intent(in) :: i              !! 1-based element index.
        integer(int64) :: k
        if (.not. c%has_nulls) then
            bit_valid = .true.
            return
        end if
        k = i - 1_int64
        bit_valid = iand(c%validity(k/8_int64 + 1_int64), BIT_MASK(int(mod(k, 8_int64)))) /= 0_int8
    end function bit_valid
    !
    !> Marks element `i` (1-based) valid in the validity bitmap (bitmap assumed allocated).
    subroutine set_bit_valid(c, i)
        type(parquet_string_column), intent(inout) :: c !! the column.
        integer(int64), intent(in) :: i                 !! 1-based element index.
        integer(int64) :: k, b
        k = i - 1_int64
        b = k/8_int64 + 1_int64
        c%validity(b) = ior(c%validity(b), BIT_MASK(int(mod(k, 8_int64))))
    end subroutine set_bit_valid
    !
    !> Marks element `i` (1-based) null in the validity bitmap (bitmap assumed allocated).
    subroutine set_bit_null(c, i)
        type(parquet_string_column), intent(inout) :: c !! the column.
        integer(int64), intent(in) :: i                 !! 1-based element index.
        integer(int64) :: k, b
        k = i - 1_int64
        b = k/8_int64 + 1_int64
        c%validity(b) = iand(c%validity(b), not(BIT_MASK(int(mod(k, 8_int64)))))
    end subroutine set_bit_null
    !
    !> Rebuilds `c`'s validity bitmap for a MASK-COMPACTED column: destination row `j` is the `j`-th
    !! source row whose `keep` entry is `.true.`, and its null state is `old_null` at that source row.
    !!
    !! **Accumulates a whole byte in a register and stores it once per eight rows**, rather than
    !! calling `set_bit_valid`/`set_bit_null` per row — each of those is a read-modify-write on the
    !! bitmap. Same insight as `copy_validity_run`, in the one case where that helper cannot be used:
    !! a compaction has no contiguous run to copy, because which source rows survive is arbitrary.
    !!
    !! Serial by necessity, not by omission. A destination row index is a rank among survivors, so a
    !! thread's first output row is not a multiple of 8 and the byte-aligned split every other
    !! threaded validity phase in this module relies on cannot be constructed.
    subroutine rebuild_validity_compacted(c, keep, old_null, n, nn)
        type(parquet_string_column), intent(inout) :: c !! the column, already compacted and sized.
        logical, intent(in) :: keep(:)                  !! .true. for every source row retained.
        logical, intent(in) :: old_null(:)              !! per SOURCE row: was it null.
        integer(int64), intent(in) :: n                 !! source row count.
        integer(int64), intent(out) :: nn               !! nulls among the survivors.
        integer(int64) :: k, done, bidx
        integer(int8) :: acc
        integer :: bit
        nn = 0_int64
        done = 0_int64
        bidx = 1_int64
        ! Unused high bits of the final byte are left VALID, which is the value `ensure_validity_cap`
        ! gives a fresh byte -- so a partly-filled trailing byte is indistinguishable from one this
        ! column never wrote.
        acc = ALL_VALID_BYTE
        do k = 1_int64, n
            if (.not. keep(k)) cycle
            bit = int(mod(done, 8_int64))
            if (old_null(k)) then
                acc = iand(acc, not(BIT_MASK(bit)))
                nn = nn + 1_int64
            end if
            done = done + 1_int64
            if (bit == 7) then
                c%validity(bidx) = acc
                bidx = bidx + 1_int64
                acc = ALL_VALID_BYTE
            end if
        end do
        if (mod(done, 8_int64) /= 0_int64) c%validity(bidx) = acc
    end subroutine rebuild_validity_compacted
    !
    !> Copies validity bits `k0..k1` (0-based offsets from `s_first`/`d_first`) one at a time,
    !! **adding** the nulls it copied to `nn`. The slow shape, used for the ragged ends of a run and
    !! for a run whose two sides do not share a bit phase.
    subroutine copy_validity_bits(src_map, s_bit0, dst_map, d_bit0, k0, k1, nn)
        integer(int8), intent(in) :: src_map(:)     !! source bitmap (1 = valid, as Arrow packs it).
        integer(int64), intent(in) :: s_bit0        !! 0-based bit index of the run's first source element.
        integer(int8), intent(inout) :: dst_map(:)  !! destination bitmap.
        integer(int64), intent(in) :: d_bit0        !! 0-based bit index of the run's first destination element.
        integer(int64), intent(in) :: k0            !! first 0-based offset within the run.
        integer(int64), intent(in) :: k1            !! last 0-based offset within the run.
        integer(int64), intent(inout) :: nn         !! accumulates the nulls copied.
        integer(int64) :: k, sb, db, dbyte
        do k = k0, k1
            sb = s_bit0 + k
            db = d_bit0 + k
            dbyte = db/8_int64 + 1_int64
            if (iand(src_map(sb/8_int64 + 1_int64), BIT_MASK(int(mod(sb, 8_int64)))) /= 0_int8) then
                dst_map(dbyte) = ior(dst_map(dbyte), BIT_MASK(int(mod(db, 8_int64))))
            else
                dst_map(dbyte) = iand(dst_map(dbyte), not(BIT_MASK(int(mod(db, 8_int64)))))
                nn = nn + 1_int64
            end if
        end do
    end subroutine copy_validity_bits
    !
    !> Copies `m` validity bits from `src` (starting at 1-based element `s_first`) to `dst`
    !! (starting at `d_first`), reporting how many of them were null.
    !!
    !! **The whole middle of the run moves as BYTES, not as eight bits each.** A validity bitmap
    !! packs 8 rows per byte, so a run whose two sides share a bit phase -- which is what
    !! `append_column` onto an 8-aligned column and `slice` from an 8-aligned row both give -- needs
    !! one byte copy and one `popcnt` per eight rows instead of eight read-modify-writes and eight
    !! branches. Measured at **6.6x** on `append_column` and **3.3x** on `slice` over 4 M rows, which
    !! is why this is an algorithm change rather than the threading S5 originally scheduled: there is
    !! no race surface left to get wrong, and it helps the single-threaded caller too.
    !!
    !! **The byte path requires BOTH sides to start on a byte boundary, not merely to agree on a bit
    !! phase.** Two runs sharing a non-zero phase could in principle be copied byte-wise after a
    !! ragged head, but neither caller can produce that -- `slice` always writes a destination from
    !! bit 0, `append_column` always reads a source from bit 0 -- so the head would be a branch no
    !! test could reach. The narrower condition costs those hypothetical callers nothing but a
    !! fallback that is already correct, and it leaves no untestable code behind.
    !!
    !! The ragged TAIL and a misaligned run fall back to `copy_validity_bits`, and neither is a rare
    !! corner: `slice(3, 900)` takes the fallback whole, and any row count not a multiple of 8 has a
    !! tail. Both must stay correct rather than merely present.
    subroutine copy_validity_run(src_map, s_bit0, dst_map, d_bit0, m, nn)
        integer(int8), intent(in) :: src_map(:)     !! source bitmap (1 = valid).
        integer(int64), intent(in) :: s_bit0        !! 0-based bit index of the first source element.
        integer(int8), intent(inout) :: dst_map(:)  !! destination bitmap.
        integer(int64), intent(in) :: d_bit0        !! 0-based bit index of the first destination element.
        integer(int64), intent(in) :: m             !! elements to copy.
        integer(int64), intent(out) :: nn           !! nulls among the copied elements.
        integer(int64) :: k, nbytes, sbyte, dbyte
        nn = 0_int64
        if (m <= 0_int64) return
        if (mod(s_bit0, 8_int64) /= 0_int64 .or. mod(d_bit0, 8_int64) /= 0_int64) then
            call copy_validity_bits(src_map, s_bit0, dst_map, d_bit0, 0_int64, m - 1_int64, nn)
            return
        end if
        nbytes = m/8_int64
        sbyte = s_bit0/8_int64 + 1_int64
        dbyte = d_bit0/8_int64 + 1_int64
        do k = 0_int64, nbytes - 1_int64
            dst_map(dbyte + k) = src_map(sbyte + k)
            ! A set bit is a VALID row, so the nulls in this byte are its zeros. Masked to 8 bits
            ! because int8 is signed and `int()` would sign-extend the high bit into 24 more ones.
            nn = nn + int(8 - popcnt(iand(int(src_map(sbyte + k), int32), 255)), int64)
        end do
        if (nbytes*8_int64 < m) then
            call copy_validity_bits(src_map, s_bit0, dst_map, d_bit0, nbytes*8_int64, m - 1_int64, nn)
        end if
    end subroutine copy_validity_run
    !
    !> `copy_validity_run` between two columns, both of which must already have a bitmap. A thin
    !! wrapper so the byte-wise core has exactly one implementation, shared with the read path's
    !! `append_buffers` -- whose source is a raw Arrow bitmap behind a C pointer and so cannot be
    !! expressed as a column at all.
    subroutine copy_validity_run_cols(src, s_first, dst, d_first, m, nn)
        type(parquet_string_column), intent(in) :: src    !! source column (bitmap allocated).
        integer(int64), intent(in) :: s_first             !! 1-based first source element.
        type(parquet_string_column), intent(inout) :: dst !! destination column (bitmap allocated).
        integer(int64), intent(in) :: d_first             !! 1-based first destination element.
        integer(int64), intent(in) :: m                   !! elements to copy.
        integer(int64), intent(out) :: nn                 !! nulls among the copied elements.
        call copy_validity_run(src%validity, s_first - 1_int64, dst%validity, d_first - 1_int64, m, nn)
    end subroutine copy_validity_run_cols
    !
    !> Marks `m` elements from `d_first` valid, whole bytes at a time. The counterpart of
    !! `copy_validity_run` for a source that has no nulls at all: the destination still needs its
    !! bits set, since its bitmap can carry stale nulls from rows that have since been removed.
    !!
    !! Same byte-alignment condition, and for the same reason -- see `copy_validity_run`.
    subroutine fill_validity_valid(dst, d_first, m)
        type(parquet_string_column), intent(inout) :: dst !! destination column (bitmap allocated).
        integer(int64), intent(in) :: d_first             !! 1-based first destination element.
        integer(int64), intent(in) :: m                   !! elements to mark valid.
        integer(int64) :: k, nbytes, db, dbyte
        if (m <= 0_int64) return
        db = d_first - 1_int64
        if (mod(db, 8_int64) /= 0_int64) then
            do k = 0_int64, m - 1_int64
                call set_bit_valid(dst, d_first + k)
            end do
            return
        end if
        nbytes = m/8_int64
        dbyte = db/8_int64 + 1_int64
        do k = 0_int64, nbytes - 1_int64
            dst%validity(dbyte + k) = ALL_VALID_BYTE
        end do
        do k = nbytes*8_int64, m - 1_int64
            call set_bit_valid(dst, d_first + k)
        end do
    end subroutine fill_validity_valid
    !
    !> Returns the 1-based payload bounds `a:b` of element `i` (b < a for a zero-length element).
    subroutine elem_bounds(c, i, a, b)
        type(parquet_string_column), intent(in) :: c !! the column.
        integer(int64), intent(in) :: i              !! 1-based element index.
        integer(int64), intent(out) :: a             !! first payload byte (offsets(i)+1).
        integer(int64), intent(out) :: b             !! last payload byte (offsets(i+1)).
        a = c%offsets(i) + 1_int64
        b = c%offsets(i+1)
    end subroutine elem_bounds
    !
    !> Returns the trailing-blank-trimmed length of the payload bytes `data(a:b)`.
    integer(int64) function elem_trim_len(c, a, b)
        type(parquet_string_column), intent(in) :: c !! the column.
        integer(int64), intent(in) :: a              !! first payload byte.
        integer(int64), intent(in) :: b              !! last payload byte.
        integer(int64) :: j
        j = b
        do while (j >= a)
            if (c%data(j) /= ' ') exit
            j = j - 1_int64
        end do
        elem_trim_len = j - a + 1_int64
    end function elem_trim_len
    !
    !> Compares (non-null) element `i` against `str`; exact = byte-exact, else trailing-trim both.
    logical function elem_equals(c, i, str, exact) result(res)
        type(parquet_string_column), intent(in) :: c !! the column.
        integer(int64), intent(in) :: i              !! 1-based element index.
        character(len=*), intent(in) :: str          !! query string.
        logical, intent(in) :: exact                 !! .true. => byte-exact comparison.
        integer(int64) :: a, b, elen, el, k
        integer :: sl
        call elem_bounds(c, i, a, b)
        elen = b - a + 1_int64
        if (exact) then
            if (elen /= int(len(str), int64)) then
                res = .false.
                return
            end if
            do k = 1_int64, elen
                if (c%data(a+k-1_int64) /= str(k:k)) then
                    res = .false.
                    return
                end if
            end do
            res = .true.
        else
            el = elem_trim_len(c, a, b)
            sl = len_trim(str)
            if (el /= int(sl, int64)) then
                res = .false.
                return
            end if
            do k = 1_int64, el
                if (c%data(a+k-1_int64) /= str(k:k)) then
                    res = .false.
                    return
                end if
            end do
            res = .true.
        end if
    end function elem_equals
    !
    !> Computes the stored substring bounds `str(lo:hi)` after applying strip/trim (hi < lo = empty).
    subroutine process_bounds(str, do_strip, do_trim, lo, hi)
        character(len=*), intent(in) :: str !! the raw string.
        logical, intent(in) :: do_strip     !! strip leading and trailing blanks.
        logical, intent(in) :: do_trim       !! trailing-trim (ignored when do_strip is .true.).
        integer, intent(out) :: lo           !! first stored character index.
        integer, intent(out) :: hi           !! last stored character index.
        integer :: n
        n = len(str)
        lo = 1
        hi = n
        if (do_strip) then
            do while (lo <= n)
                if (str(lo:lo) /= ' ') exit
                lo = lo + 1
            end do
            do while (hi >= lo)
                if (str(hi:hi) /= ' ') exit
                hi = hi - 1
            end do
        else if (do_trim) then
            do while (hi >= 1)
                if (str(hi:hi) /= ' ') exit
                hi = hi - 1
            end do
        end if
    end subroutine process_bounds
    !
    !> Swaps every component of two columns in O(1) (self-safe: a=b leaves the object unchanged).
    subroutine swap_impl(a, b)
        type(parquet_string_column), intent(inout) :: a !! first column.
        type(parquet_string_column), intent(inout) :: b !! second column.
        integer(int64), allocatable :: to(:)
        character(len=1), allocatable :: td(:)
        integer(int8), allocatable :: tv(:)
        integer(int64) :: t_nrows, t_nchars, t_nnull
        logical :: t_has
        call move_alloc(a%offsets, to);   call move_alloc(b%offsets, a%offsets);   call move_alloc(to, b%offsets)
        call move_alloc(a%data, td);      call move_alloc(b%data, a%data);         call move_alloc(td, b%data)
        call move_alloc(a%validity, tv);  call move_alloc(b%validity, a%validity); call move_alloc(tv, b%validity)
        t_nrows = a%nrows;  a%nrows = b%nrows;   b%nrows = t_nrows
        t_nchars = a%nchars; a%nchars = b%nchars; b%nchars = t_nchars
        t_nnull = a%n_null; a%n_null = b%n_null;  b%n_null = t_nnull
        t_has = a%has_nulls; a%has_nulls = b%has_nulls; b%has_nulls = t_has
    end subroutine swap_impl
    !
    ! ==================================================================================
    ! Construction / memory management
    ! ==================================================================================
    !
    !> Resets the column to an empty state and releases all owned memory (capacity becomes 0).
    !! Invalidates every outstanding handle into this column.
    subroutine parquet_string_column_clear(self)
        type(parquet_string_column), intent(inout) :: self !! the column.
        if (allocated(self%offsets)) deallocate(self%offsets)
        if (allocated(self%data)) deallocate(self%data)
        if (allocated(self%validity)) deallocate(self%validity)
        self%nrows = 0
        self%nchars = 0
        self%n_null = 0
        self%has_nulls = .false.
    end subroutine parquet_string_column_clear
    !
    !> Binding form of `parquet_string_column_clear`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine clear(self)
        class(parquet_string_column), intent(inout) :: self !! the column.
        call parquet_string_column_clear(self)
    end subroutine clear
    !
    !> int32 specific of reserve; see the reserve generic.
    subroutine parquet_string_column_reserve_i32(self, n_rows, n_characters)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: n_rows                !! required row capacity.
        integer(int32), intent(in) :: n_characters          !! required character capacity (bytes).
        call parquet_string_column_reserve_i64(self, int(n_rows, int64), int(n_characters, int64))
    end subroutine parquet_string_column_reserve_i32
    !
    !> Binding form of `parquet_string_column_reserve_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine reserve_i32(self, n_rows, n_characters)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: n_rows                !! required row capacity.
        integer(int32), intent(in) :: n_characters          !! required character capacity (bytes).
        call parquet_string_column_reserve_i32(self, n_rows, n_characters)
    end subroutine reserve_i32
    !
    !> int64 specific of reserve: grows capacity to hold at least `n_rows`/`n_characters`
    !! (never shrinks; pass 0 for "no requirement on this dimension").
    subroutine parquet_string_column_reserve_i64(self, n_rows, n_characters)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n_rows                !! required row capacity.
        integer(int64), intent(in) :: n_characters          !! required character capacity (bytes).
        if (n_rows > 0) call ensure_offsets_cap(self, max(n_rows, self%nrows))
        if (n_characters > 0) call ensure_data_cap(self, max(n_characters, self%nchars))
        if (self%has_nulls .and. n_rows > 0) call ensure_validity_cap(self, max(n_rows, self%nrows))
    end subroutine parquet_string_column_reserve_i64
    !
    !> Binding form of `parquet_string_column_reserve_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine reserve_i64(self, n_rows, n_characters)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n_rows                !! required row capacity.
        integer(int64), intent(in) :: n_characters          !! required character capacity (bytes).
        call parquet_string_column_reserve_i64(self, n_rows, n_characters)
    end subroutine reserve_i64
    !
    !> Reallocates the buffers down to exactly the current size (frees unused capacity).
    !! Invalidates every outstanding handle into this column.
    subroutine parquet_string_column_shrink_to_fit(self)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), allocatable :: to(:)
        character(len=1), allocatable :: td(:)
        integer(int8), allocatable :: tv(:)
        integer(int64) :: need_bytes
        if (allocated(self%offsets)) then
            if (size(self%offsets, kind=int64) > self%nrows+1) then
                allocate(to(self%nrows+1))
                to(:) = self%offsets(1:self%nrows+1)
                call move_alloc(to, self%offsets)
            end if
        end if
        if (allocated(self%data)) then
            if (self%nchars == 0) then
                deallocate(self%data)
            else if (size(self%data, kind=int64) > self%nchars) then
                allocate(td(self%nchars))
                td(:) = self%data(1:self%nchars)
                call move_alloc(td, self%data)
            end if
        end if
        if (self%has_nulls .and. allocated(self%validity)) then
            need_bytes = (self%nrows + 7_int64)/8_int64
            if (size(self%validity, kind=int64) > need_bytes .and. need_bytes >= 1) then
                allocate(tv(need_bytes))
                tv(:) = self%validity(1:need_bytes)
                call move_alloc(tv, self%validity)
            end if
        end if
    end subroutine parquet_string_column_shrink_to_fit
    !
    !> Binding form of `parquet_string_column_shrink_to_fit`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine shrink_to_fit(self)
        class(parquet_string_column), intent(inout) :: self !! the column.
        call parquet_string_column_shrink_to_fit(self)
    end subroutine shrink_to_fit
    !
    !> Returns the current row capacity.
    integer(int64) function parquet_string_column_capacity(self)
        type(parquet_string_column), intent(in) :: self !! the column.
        if (allocated(self%offsets)) then
            parquet_string_column_capacity = size(self%offsets, kind=int64) - 1_int64
        else
            parquet_string_column_capacity = 0_int64
        end if
    end function parquet_string_column_capacity
    !
    !> Binding form of `parquet_string_column_capacity`; forwards to it,
    !! keeping the implementation at the `type` end.
    integer(int64) function capacity(self)
        class(parquet_string_column), intent(in) :: self !! the column.
        capacity = parquet_string_column_capacity(self)
    end function capacity
    !
    !> Returns the current character-buffer capacity in bytes.
    integer(int64) function parquet_string_column_character_capacity(self)
        type(parquet_string_column), intent(in) :: self !! the column.
        if (allocated(self%data)) then
            parquet_string_column_character_capacity = size(self%data, kind=int64)
        else
            parquet_string_column_character_capacity = 0_int64
        end if
    end function parquet_string_column_character_capacity
    !
    !> Binding form of `parquet_string_column_character_capacity`; forwards to it,
    !! keeping the implementation at the `type` end.
    integer(int64) function character_capacity(self)
        class(parquet_string_column), intent(in) :: self !! the column.
        character_capacity = parquet_string_column_character_capacity(self)
    end function character_capacity
    !
    !> Returns the number of elements stored.
    integer(int64) function parquet_string_column_size(self)
        type(parquet_string_column), intent(in) :: self !! the column.
        parquet_string_column_size = self%nrows
    end function parquet_string_column_size
    !
    !> Binding form of `parquet_string_column_size`; forwards to it,
    !! keeping the implementation at the `type` end.
    integer(int64) function col_size(self)
        class(parquet_string_column), intent(in) :: self !! the column.
        col_size = parquet_string_column_size(self)
    end function col_size
    !
    !> Returns the total number of characters stored across all elements.
    integer(int64) function parquet_string_column_character_size(self)
        type(parquet_string_column), intent(in) :: self !! the column.
        parquet_string_column_character_size = self%nchars
    end function parquet_string_column_character_size
    !
    !> Binding form of `parquet_string_column_character_size`; forwards to it,
    !! keeping the implementation at the `type` end.
    integer(int64) function character_size(self)
        class(parquet_string_column), intent(in) :: self !! the column.
        character_size = parquet_string_column_character_size(self)
    end function character_size
    !
    !> Returns .true. when the column holds no rows.
    logical function col_empty(self)
        class(parquet_string_column), intent(in) :: self !! the column.
        col_empty = self%nrows == 0
    end function col_empty
    !
    !> Returns the number of null elements.
    integer(int64) function parquet_string_column_null_count(self)
        type(parquet_string_column), intent(in) :: self !! the column.
        parquet_string_column_null_count = self%n_null
    end function parquet_string_column_null_count
    !
    !> Binding form of `parquet_string_column_null_count`; forwards to it,
    !! keeping the implementation at the `type` end.
    integer(int64) function null_count(self)
        class(parquet_string_column), intent(in) :: self !! the column.
        null_count = parquet_string_column_null_count(self)
    end function null_count
    !
    !> Whether the validity bitmap has been materialized yet.
    !!
    !! A column with no bitmap has every element valid; the bitmap is allocated lazily, the first
    !! time an element is actually nulled. So this answers "would nulling an element allocate?",
    !! which is what a caller about to null elements from several threads needs to know -- see
    !! `reserve_validity`.
    logical function parquet_string_column_has_validity(self)
        type(parquet_string_column), intent(in) :: self !! the column.
        parquet_string_column_has_validity = allocated(self%validity)
    end function parquet_string_column_has_validity
    !
    !> Binding form of `parquet_string_column_has_validity`; forwards to it,
    !! keeping the implementation at the `type` end.
    logical function has_validity(self)
        class(parquet_string_column), intent(in) :: self !! the column.
        has_validity = parquet_string_column_has_validity(self)
    end function has_validity
    !
    !> Materializes the validity bitmap now, with every element still valid.
    !!
    !! Nothing about the column's contents changes -- the bitmap is initialised to all-ones -- so
    !! this is purely about *when* the allocation happens. Its reason to exist is concurrency:
    !! `append_null`/`set_null` allocate the bitmap lazily, so two threads nulling elements of the
    !! same previously null-free column would race on that allocation with no diagnostic. Calling
    !! this first leaves them nothing to allocate.
    !!
    !! Idempotent: a column that already has a bitmap is untouched, and a zero-row column still
    !! gets the minimum allocation, so a later append has nothing to grow from scratch either.
    subroutine parquet_string_column_reserve_validity(self)
        type(parquet_string_column), intent(inout) :: self !! the column.
        call ensure_validity_cap(self, max(self%nrows, 1_int64))
        self%has_nulls = .true.
    end subroutine parquet_string_column_reserve_validity
    !
    !> Binding form of `parquet_string_column_reserve_validity`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine reserve_validity(self)
        class(parquet_string_column), intent(inout) :: self !! the column.
        call parquet_string_column_reserve_validity(self)
    end subroutine reserve_validity
    !
    !> Returns the total bytes of allocated buffers (offsets + data + validity + object overhead).
    integer(int64) function memory_usage(self)
        class(parquet_string_column), intent(in) :: self !! the column.
        memory_usage = int(storage_size(self)/8, int64)
        if (allocated(self%offsets)) memory_usage = memory_usage + size(self%offsets, kind=int64)*8_int64
        if (allocated(self%data)) memory_usage = memory_usage + size(self%data, kind=int64)
        if (allocated(self%validity)) memory_usage = memory_usage + size(self%validity, kind=int64)
    end function memory_usage
    !
    !> Verifies all class invariants and internal consistency; returns .true. when they hold.
    logical function validate(self, message)
        class(parquet_string_column), intent(in) :: self       !! the column.
        character(len=:), allocatable, intent(out), optional :: message !! diagnostic on failure.
        integer(int64) :: i, cnt
        validate = .false.
        if (present(message)) message = ""
        if (self%nrows < 0 .or. self%nchars < 0) then ! GCOVR_EXCL_START -- gcov attribution artifact
            if (present(message)) message = "negative nrows/nchars"
            return
        end if ! GCOVR_EXCL_STOP
        if (self%n_null < 0 .or. self%n_null > self%nrows) then ! GCOVR_EXCL_START -- gcov attribution artifact
            if (present(message)) message = "n_null out of range"
            return
        end if ! GCOVR_EXCL_STOP
        if (self%nrows > 0) then
            if (.not. allocated(self%offsets)) then ! GCOVR_EXCL_START -- gcov attribution artifact
                if (present(message)) message = "offsets not allocated"
                return
            end if ! GCOVR_EXCL_STOP
            if (size(self%offsets, kind=int64) < self%nrows+1) then ! GCOVR_EXCL_START -- gcov attribution artifact
                if (present(message)) message = "offsets too small"
                return
            end if ! GCOVR_EXCL_STOP
            if (self%offsets(1) /= 0_int64) then ! GCOVR_EXCL_START -- gcov attribution artifact
                if (present(message)) message = "offsets(1) /= 0"
                return
            end if ! GCOVR_EXCL_STOP
            do i = 1_int64, self%nrows
                if (self%offsets(i+1) < self%offsets(i)) then ! GCOVR_EXCL_START -- gcov attribution artifact
                    if (present(message)) message = "offsets not monotonic"
                    return
                end if ! GCOVR_EXCL_STOP
            end do
            if (self%offsets(self%nrows+1) /= self%nchars) then ! GCOVR_EXCL_START -- gcov attribution artifact
                if (present(message)) message = "offsets(nrows+1) /= nchars"
                return
            end if ! GCOVR_EXCL_STOP
        end if
        if (self%nchars > 0) then
            if (.not. allocated(self%data)) then ! GCOVR_EXCL_START -- gcov attribution artifact
                if (present(message)) message = "data not allocated"
                return
            end if ! GCOVR_EXCL_STOP
            if (size(self%data, kind=int64) < self%nchars) then ! GCOVR_EXCL_START -- gcov attribution artifact
                if (present(message)) message = "data too small"
                return
            end if ! GCOVR_EXCL_STOP
        end if
        if (self%has_nulls) then
            cnt = 0
            do i = 1_int64, self%nrows
                if (.not. bit_valid(self, i)) cnt = cnt + 1_int64
            end do
            if (cnt /= self%n_null) then ! GCOVR_EXCL_START -- gcov attribution artifact
                if (present(message)) message = "n_null disagrees with validity bitmap"
                return
            end if ! GCOVR_EXCL_STOP
        else if (self%n_null /= 0) then
            if (present(message)) message = "n_null /= 0 without a validity bitmap" ! GCOVR_EXCL_LINE
            return ! GCOVR_EXCL_LINE
        end if
        validate = .true.
    end function validate
    !
    ! ==================================================================================
    ! Access
    ! ==================================================================================
    !
    !> int32 specific of length; see the length generic.
    integer(int64) function length_i32(self, i, check_null) result(n)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        n = self%length_i64(int(i, int64), check_null)
    end function length_i32
    !
    !> int64 specific of length: length of element i without allocating. A null element returns 0
    !! by default, or error stops when `check_null` is .true.
    integer(int64) function length_i64(self, i, check_null) result(n)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        call check_index(self, i, "length")
        if (.not. bit_valid(self, i)) then
            if (present(check_null)) then
                if (check_null) call fail_null("length")
            end if
            n = 0_int64
            return
        end if
        n = self%offsets(i+1) - self%offsets(i)
    end function length_i64
    !
    !> int32 specific of get; see the get generic.
    !! (Coverage note: this header line never registers as "hit" in gcov although the one-line body
    !! below it does -- which is what proves the specific runs. The same attribution artifact is
    !! documented at length above `date_parse` in `src/parquet_temporal.f90`. Excluded as an
    !! artifact, not as a gap.)
    subroutine parquet_string_column_get_i32(self, i, res, null_value, allow_null) ! GCOVR_EXCL_LINE -- gcov attribution artifact
        type(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        character(len=:), allocatable, intent(out) :: res !! element i (unallocated if null and allowed).
        character(len=*), intent(in), optional :: null_value !! substitute returned for a null element.
        logical, intent(in), optional :: allow_null           !! .true. => suppress abort, return empty string for a null.
        call parquet_string_column_get_i64(self, int(i, int64), res, null_value, allow_null)
    end subroutine parquet_string_column_get_i32
    !
    !> Binding form of `parquet_string_column_get_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine get_i32(self, i, res, null_value, allow_null)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        character(len=:), allocatable, intent(out) :: res !! element i (unallocated if null and allowed).
        character(len=*), intent(in), optional :: null_value !! substitute returned for a null element.
        logical, intent(in), optional :: allow_null           !! .true. => suppress abort, return empty string for a null.
        call parquet_string_column_get_i32(self, i, res, null_value, allow_null)
    end subroutine get_i32
    !
    !> int64 specific of get: writes element i into `res`. A null element error stops by
    !! default; pass `null_value` to substitute a string, or `allow_null=.true.` to suppress the
    !! abort and return an empty string (detect null via is_null). When both are given,
    !! `null_value` takes precedence. A subroutine (not a function) so this never returns
    !! `character(len=:), allocatable` as a function result -- see "Build and compiler notes" in
    !! CLAUDE.md for why.
    subroutine parquet_string_column_get_i64(self, i, res, null_value, allow_null)
        type(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        character(len=:), allocatable, intent(out) :: res !! element i (unallocated if null and allowed).
        character(len=*), intent(in), optional :: null_value !! substitute returned for a null element.
        logical, intent(in), optional :: allow_null           !! .true. => suppress abort, return empty string for a null.
        integer(int64) :: a, b, elen
        logical :: allow
        call check_index(self, i, "get")
        if (.not. bit_valid(self, i)) then
            if (present(null_value)) then
                res = null_value
                return
            end if
            allow = .false.
            if (present(allow_null)) allow = allow_null
            if (allow) then
                res = ""
                return
            end if
            call fail_null("get")
        end if
        call elem_bounds(self, i, a, b)
        elen = b - a + 1_int64
        allocate(character(len=elen) :: res)
        if (elen > 0) res = transfer(self%data(a:b), res)
    end subroutine parquet_string_column_get_i64
    !
    !> Binding form of `parquet_string_column_get_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine get_i64(self, i, res, null_value, allow_null)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        character(len=:), allocatable, intent(out) :: res !! element i (unallocated if null and allowed).
        character(len=*), intent(in), optional :: null_value !! substitute returned for a null element.
        logical, intent(in), optional :: allow_null           !! .true. => suppress abort, return empty string for a null.
        call parquet_string_column_get_i64(self, i, res, null_value, allow_null)
    end subroutine get_i64
    !
    !> int32 specific of copy_to; see the copy_to generic.
    subroutine parquet_string_column_copy_to_i32(self, i, dest, allow_null)
        type(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(out) :: dest            !! receives the element, blank-padded.
        logical, intent(in), optional :: allow_null      !! .true. => a null yields blanks.
        call parquet_string_column_copy_to_i64(self, int(i, int64), dest, allow_null)
    end subroutine parquet_string_column_copy_to_i32
    !
    !> Binding form of `parquet_string_column_copy_to_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine copy_to_i32(self, i, dest, allow_null)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(out) :: dest            !! receives the element, blank-padded.
        logical, intent(in), optional :: allow_null      !! .true. => a null yields blanks.
        call parquet_string_column_copy_to_i32(self, i, dest, allow_null)
    end subroutine copy_to_i32
    !
    !> int64 specific of copy_to: copies element `i`'s bytes into `dest`, blank-padding the rest.
    !!
    !! **The allocation-free counterpart of `%get`**, for the very common shape where the caller
    !! already has somewhere fixed-width to put the value — a `character(len=N)` array being filled
    !! row by row, or a scratch buffer reused across a loop. `%get` must allocate, because it
    !! returns a string sized to the element; this cannot and does not.
    !!
    !! **It follows Fortran's own assignment semantics exactly**, so it is a drop-in for
    !! `call c%get(i, s); dest = s`: shorter values are blank-padded, and a value longer than `dest`
    !! is truncated rather than aborting — which is what `dest = s` would have done. A caller that
    !! must not truncate sizes `dest` from `%length(i)` or `%max_length()` first, both of which
    !! allocate nothing either.
    subroutine parquet_string_column_copy_to_i64(self, i, dest, allow_null)
        type(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(out) :: dest            !! receives the element, blank-padded.
        logical, intent(in), optional :: allow_null      !! .true. => a null yields blanks.
        integer(int64) :: a, b, elen, n
        logical :: ok_null
        call check_index(self, i, "copy_to")
        ok_null = .false.
        if (present(allow_null)) ok_null = allow_null
        if (.not. bit_valid(self, i)) then
            if (.not. ok_null) call fail_null("copy_to")
            dest = ""
            return
        end if
        call elem_bounds(self, i, a, b)
        elen = b - a + 1_int64
        n = min(elen, int(len(dest), int64))
        if (n > 0_int64) dest(1:n) = transfer(self%data(a:a+n-1_int64), dest(1:n))
        if (n < int(len(dest), int64)) dest(n+1_int64:) = ""
    end subroutine parquet_string_column_copy_to_i64
    !
    !> Binding form of `parquet_string_column_copy_to_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine copy_to_i64(self, i, dest, allow_null)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(out) :: dest            !! receives the element, blank-padded.
        logical, intent(in), optional :: allow_null      !! .true. => a null yields blanks.
        call parquet_string_column_copy_to_i64(self, i, dest, allow_null)
    end subroutine copy_to_i64
    !
    !> int32 specific of view; see the view generic.
    function view_i32(self, i) result(h)
        class(parquet_string_column), intent(in), target :: self !! the column (must be a target).
        integer(int32), intent(in) :: i                          !! 1-based element index.
        type(parquet_string) :: h                                !! handle to element i.
        h = self%view_i64(int(i, int64))
    end function view_i32
    !
    !> int64 specific of view: a zero-copy handle to element i. The column must be declared with
    !! the `target` attribute and must outlive the handle.
    function view_i64(self, i) result(h)
        class(parquet_string_column), intent(in), target :: self !! the column (must be a target).
        integer(int64), intent(in) :: i                          !! 1-based element index.
        type(parquet_string) :: h                                !! handle to element i.
        call check_index(self, i, "view")
        h%col => self
        h%idx = i
    end function view_i64
    !
    !> Fills data_string with one handle per element of self, in order (view(1), view(2), ...).
    !! Aborts if size(data_string) does not match self%size() (rather than silently clipping to
    !! the shorter length, which would hide a caller bug behind a partially-populated result).
    subroutine view_all(self, data_string)
        class(parquet_string_column), intent(in), target :: self !! the column (must be a target).
        type(parquet_string), dimension(:), intent(out) :: data_string !! receives one handle per element.
        integer(int64) :: i
        if (size(data_string, kind=int64) /= self%nrows) then
            error stop EP//"view_all: size(data_string) does not match self%size()"
        end if
        ! **The components are written directly, not via `%view(i)`, and that is worth 2.5x.**
        ! The 2.5x was measured while `parquet_string` still had a finalizer, which intrinsic
        ! assignment ran twice per element -- once on the destination, which it finalizes before
        ! overwriting, and once on the function result afterwards -- to set a pointer and an
        ! integer. That finalizer is gone (the type body says why, at length, and why it must not
        ! come back), so what the direct writes still save is one call plus one bounds check per
        ! element: `%view`'s check cannot fail here, since `i` runs over exactly 1..nrows.
        do i = 1_int64, self%nrows
            data_string(i)%col => self
            data_string(i)%idx = i
        end do
    end subroutine view_all
    !
    !> int32 specific of view_slice; see the view_slice generic.
    subroutine view_slice_i32(self, first, last, data_string)
        class(parquet_string_column), intent(in), target :: self !! the column (must be a target).
        integer(int32), intent(in) :: first                       !! first row of the range (1-based, inclusive).
        integer(int32), intent(in) :: last                        !! last row of the range (1-based, inclusive).
        type(parquet_string), dimension(:), intent(out) :: data_string !! one handle per row in [first, last].
        call self%view_slice_i64(int(first, int64), int(last, int64), data_string)
    end subroutine view_slice_i32
    !
    !> int64 specific of view_slice: fills data_string with one zero-copy handle per row of
    !! [first, last] (1-based, inclusive), in order (data_string(1) = view(first), ...) --
    !! view_all restricted to a range. Aborts if size(data_string) /= last-first+1, same
    !! convention as view_all.
    subroutine view_slice_i64(self, first, last, data_string)
        class(parquet_string_column), intent(in), target :: self !! the column (must be a target).
        integer(int64), intent(in) :: first                       !! first row of the range (1-based, inclusive).
        integer(int64), intent(in) :: last                        !! last row of the range (1-based, inclusive).
        type(parquet_string), dimension(:), intent(out) :: data_string !! one handle per row in [first, last].
        integer(int64) :: i, n
        call check_range(self, first, last, "view_slice")
        n = last - first + 1_int64
        if (size(data_string, kind=int64) /= n) then
            error stop EP//"view_slice: size(data_string) does not match last-first+1"
        end if
        ! Direct component writes, for the reason `view_all` gives at length: a call and a bounds
        ! check per element, neither of which this loop needs.
        do i = 1_int64, n
            data_string(i)%col => self
            data_string(i)%idx = first + i - 1_int64
        end do
    end subroutine view_slice_i64
    !
    !> int32 specific of is_null; see the is_null generic.
    logical function parquet_string_column_is_null_i32(self, i) result(res)
        type(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        res = parquet_string_column_is_null_i64(self, int(i, int64))
    end function parquet_string_column_is_null_i32
    !
    !> Binding form of `parquet_string_column_is_null_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    logical function is_null_i32(self, i) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        res = parquet_string_column_is_null_i32(self, i)
    end function is_null_i32
    !
    !> int64 specific of is_null: whether element i is null (always safe -- the primary null guard).
    logical function parquet_string_column_is_null_i64(self, i) result(res)
        type(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        call check_index(self, i, "is_null")
        res = .not. bit_valid(self, i)
    end function parquet_string_column_is_null_i64
    !
    !> Binding form of `parquet_string_column_is_null_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    logical function is_null_i64(self, i) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        res = parquet_string_column_is_null_i64(self, i)
    end function is_null_i64
    !
    !> int32 specific of is_empty; see the is_empty generic.
    logical function is_empty_i32(self, i, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        res = self%is_empty_i64(int(i, int64), check_null)
    end function is_empty_i32
    !
    !> int64 specific of is_empty: whether element i has zero length. A null element returns
    !! .true. by default, or error stops when `check_null` is .true.
    logical function is_empty_i64(self, i, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        call check_index(self, i, "is_empty")
        if (.not. bit_valid(self, i)) then
            if (present(check_null)) then
                if (check_null) call fail_null("is_empty")
            end if
            res = .true.
            return
        end if
        res = (self%offsets(i+1) - self%offsets(i)) == 0_int64
    end function is_empty_i64
    !
    ! ==================================================================================
    ! Modification
    ! ==================================================================================
    !
    !> Appends a string to the end of the column. By default the string is stored verbatim; pass
    !! `strip=.true.` to remove leading and trailing blanks, or `trim=.true.` to remove trailing
    !! blanks only (`trim` is ignored when `strip` is .true.). Automatically grows capacity.
    subroutine append_string(self, str, strip, trim)
        class(parquet_string_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: str                 !! the string to append.
        logical, intent(in), optional :: strip               !! remove leading and trailing blanks.
        logical, intent(in), optional :: trim                !! remove trailing blanks only.
        logical :: do_strip, do_trim
        integer :: lo, hi
        integer(int64) :: slen
        do_strip = .false.
        if (present(strip)) do_strip = strip
        do_trim = .false.
        if (present(trim)) do_trim = trim
        call process_bounds(str, do_strip, do_trim, lo, hi)
        slen = int(max(0, hi - lo + 1), int64)
        call ensure_offsets_cap(self, self%nrows + 1_int64)
        if (slen > 0) then
            call ensure_data_cap(self, self%nchars + slen)
            self%data(self%nchars+1 : self%nchars+slen) = transfer(str(lo:hi), self%data, int(slen))
        end if
        self%offsets(self%nrows+2) = self%nchars + slen
        if (self%has_nulls) then
            call ensure_validity_cap(self, self%nrows + 1_int64)
            call set_bit_valid(self, self%nrows + 1_int64)
        end if
        self%nrows = self%nrows + 1_int64
        self%nchars = self%nchars + slen
    end subroutine append_string
    !
    !> Appends a null element to the end of the column (a zero-width, invalid slot).
    subroutine parquet_string_column_append_null(self)
        type(parquet_string_column), intent(inout) :: self !! the column.
        call ensure_offsets_cap(self, self%nrows + 1_int64)
        self%offsets(self%nrows+2) = self%nchars
        self%has_nulls = .true.
        call ensure_validity_cap(self, self%nrows + 1_int64)
        call set_bit_null(self, self%nrows + 1_int64)
        self%n_null = self%n_null + 1_int64
        self%nrows = self%nrows + 1_int64
    end subroutine parquet_string_column_append_null
    !
    !> Binding form of `parquet_string_column_append_null`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine append_null(self)
        class(parquet_string_column), intent(inout) :: self !! the column.
        call parquet_string_column_append_null(self)
    end subroutine append_null
    !
    !> Appends all elements (payload and nulls) from another column. Bulk-copies the payload and
    !! offsets; never re-trims. `other` is left unchanged.
    subroutine parquet_string_column_append_column(self, other)
        type(parquet_string_column), intent(inout) :: self !! the destination column.
        type(parquet_string_column), intent(in) :: other    !! the source column.
        integer(int64) :: base, k, nn
        if (other%nrows == 0) return
        call ensure_offsets_cap(self, self%nrows + other%nrows)
        if (other%nchars > 0) then
            call ensure_data_cap(self, self%nchars + other%nchars)
            self%data(self%nchars+1 : self%nchars+other%nchars) = other%data(1:other%nchars)
        end if
        base = self%nchars
        do k = 1_int64, other%nrows
            self%offsets(self%nrows+1+k) = base + other%offsets(k+1)
        end do
        ! Both arms move the bitmap in whole bytes wherever the two sides share a bit phase, which
        ! appending to an 8-aligned (or empty) destination always does. See `copy_validity_run`.
        if (other%has_nulls) then
            self%has_nulls = .true.
            call ensure_validity_cap(self, self%nrows + other%nrows)
            call copy_validity_run_cols(other, 1_int64, self, self%nrows + 1_int64, other%nrows, nn)
            self%n_null = self%n_null + nn
        else if (self%has_nulls) then
            call ensure_validity_cap(self, self%nrows + other%nrows)
            call fill_validity_valid(self, self%nrows + 1_int64, other%nrows)
        end if
        self%nrows = self%nrows + other%nrows
        self%nchars = self%nchars + other%nchars
    end subroutine parquet_string_column_append_column
    !
    !> Binding form of `parquet_string_column_append_column`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine append_column(self, other)
        class(parquet_string_column), intent(inout) :: self !! the destination column.
        type(parquet_string_column), intent(in) :: other    !! the source column.
        call parquet_string_column_append_column(self, other)
    end subroutine append_column
    !
    !> int32 specific of append_from; see the append_from generic.
    subroutine parquet_string_column_append_from_i32(self, src, i)
        type(parquet_string_column), intent(inout) :: self !! the destination column.
        type(parquet_string_column), intent(in) :: src      !! the source column.
        integer(int32), intent(in) :: i                     !! 1-based source element index.
        call parquet_string_column_append_from_i64(self, src, int(i, int64))
    end subroutine parquet_string_column_append_from_i32
    !
    !> Binding form of `parquet_string_column_append_from_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine append_from_i32(self, src, i)
        class(parquet_string_column), intent(inout) :: self !! the destination column.
        type(parquet_string_column), intent(in) :: src      !! the source column.
        integer(int32), intent(in) :: i                     !! 1-based source element index.
        call parquet_string_column_append_from_i32(self, src, i)
    end subroutine append_from_i32
    !
    !> int64 specific of append_from: appends element `i` of `src` to the end of self, **null state
    !! included**.
    !!
    !! **The allocation-free counterpart of `call src%get(i, s); call dst%append_string(s)`**, which
    !! is the shape every "copy the rows I want into a new column" loop reaches for and which costs
    !! one heap round trip per row for bytes that are already contiguous in `src`.
    !!
    !! It never trims, matching `%append_string`'s own default: the bytes are copied verbatim.
    !!
    !! **`src` must not be the same object as self.** Fortran forbids argument-associating one object
    !! with both an `intent(inout)` and an `intent(in)` dummy of the same call once either is defined
    !! (F2018 15.5.2.13), and neither gfortran nor ifx diagnoses it. Appending a column to itself is
    !! `%append_column(other)`'s job, on a copy.
    subroutine parquet_string_column_append_from_i64(self, src, i)
        type(parquet_string_column), intent(inout) :: self !! the destination column.
        type(parquet_string_column), intent(in) :: src      !! the source column.
        integer(int64), intent(in) :: i                     !! 1-based source element index.
        integer(int64) :: a, b, elen
        call check_index(src, i, "append_from")
        if (.not. bit_valid(src, i)) then
            call parquet_string_column_append_null(self)
            return
        end if
        call elem_bounds(src, i, a, b)
        elen = b - a + 1_int64
        call ensure_offsets_cap(self, self%nrows + 1_int64)
        call ensure_data_cap(self, self%nchars + elen)
        if (elen > 0_int64) self%data(self%nchars+1_int64:self%nchars+elen) = src%data(a:b)
        self%offsets(self%nrows+2_int64) = self%nchars + elen
        if (self%has_nulls) then
            call ensure_validity_cap(self, self%nrows + 1_int64)
            call set_bit_valid(self, self%nrows + 1_int64)
        end if
        self%nrows = self%nrows + 1_int64
        self%nchars = self%nchars + elen
    end subroutine parquet_string_column_append_from_i64
    !
    !> Binding form of `parquet_string_column_append_from_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine append_from_i64(self, src, i)
        class(parquet_string_column), intent(inout) :: self !! the destination column.
        type(parquet_string_column), intent(in) :: src      !! the source column.
        integer(int64), intent(in) :: i                     !! 1-based source element index.
        call parquet_string_column_append_from_i64(self, src, i)
    end subroutine append_from_i64
    !
    !> Clears self, then gathers an array of independently-obtained parquet_string handles into it
    !! (each handle's referenced element becomes one row, in array order; a null handle becomes
    !! %append_null()). Every handle is validated before self is cleared or otherwise touched:
    !! aborts if any handle aliases self (self%clear() would corrupt it before it could be read),
    !! is unassociated, or refers to a stale/out-of-range index. The first handle (in array order)
    !! that trips any of these three checks (in that priority order) determines the abort message
    !! -- other handles are not scanned once one is found.
    subroutine build_from_handles(self, handles)
        class(parquet_string_column), intent(inout), target :: self !! cleared, then filled from handles.
        type(parquet_string), intent(in) :: handles(:)               !! source handles, in order.
        integer(int64) :: k, m, want, a, elen, idx, nnull
        integer(int64), allocatable :: lo(:), hi(:)
        integer :: nt, tix
        logical :: any_null
        m = size(handles, kind=int64)
        ! The validation pass SIZES the result as it goes. Both halves have to happen before `self`
        ! is touched anyway -- a handle aliasing `self` would be corrupted by the %clear() below
        ! before it could be read -- so summing the lengths here is free, and it is what lets the
        ! fill loop allocate once rather than growing the destination one element at a time.
        want = 0_int64
        any_null = .false.
        do k = 1_int64, m
            if (associated(handles(k)%col, self)) then
                error stop EP//"build_from: handle aliases the destination column self"
            end if
            if (.not. associated(handles(k)%col)) then
                error stop EP//"build_from: unassociated handle in input array"
            end if
            if (handles(k)%idx < 1_int64 .or. handles(k)%idx > handles(k)%col%nrows) then
                error stop EP//"build_from: stale or out-of-range handle in input array"
            end if
            if (handles(k)%is_null()) then
                any_null = .true.
            else
                idx = handles(k)%idx
                want = want + (handles(k)%col%offsets(idx+1_int64) - handles(k)%col%offsets(idx))
            end if
        end do
        call self%clear()
        ! Returning here rather than reserving keeps an empty result byte-for-byte what %clear()
        ! leaves behind -- capacity 0, nothing allocated -- which is what this used to produce when
        ! the append loop simply never ran.
        if (m == 0_int64) return
        call ensure_offsets_cap(self, m)
        call ensure_data_cap(self, want)
        if (any_null) then
            self%has_nulls = .true.
            ! Freshly allocated validity bytes are all-ones, i.e. every row valid, so only the
            ! null rows below need writing.
            call ensure_validity_cap(self, m)
        end if
        nt = bulk_threads(m, want)
        ! The serial fill is the ORIGINAL single-pass loop and is kept for the same reason
        ! `reindex_apply_serial` is: the splittable shape below reads each handle's source column
        ! twice (once for its length, once for its bytes), and those reads are scattered across
        ! however many columns the handles came from. Running it on one thread would hand every
        ! below-the-floor caller -- and every caller already inside a parallel region -- a slower
        ! loop for nothing. The two must produce byte-identical columns.
        if (nt <= 1) then
            call build_from_fill_serial(self, handles, want)
            return
        end if
        call thread_row_ranges(m, nt, lo, hi)
        ! Three phases, for the reason `reindex_apply` documents at length: the single loop carries
        ! a sequential dependence through the write cursor `pos`, and the prefix sum that removes it
        ! IS `offsets`, so it costs no extra memory.
        !
        ! Phase 1: each element's LENGTH into its own slot, plus its validity bit and the null count.
        ! **This is the phase the byte-aligned ranges exist for** -- the bitmap packs 8 rows per
        ! byte, so two threads meeting inside one would lose each other's writes and leave a column
        ! that still validates with the wrong rows null.
        self%offsets(1) = 0_int64
        nnull = 0_int64
        !$omp parallel do default(shared) private(tix, k, idx) reduction(+:nnull) &
        !$omp     schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do k = lo(tix), hi(tix)
                if (handles(k)%is_null()) then
                    ! A null occupies a zero-width slot at the current position, exactly as
                    ! %append_null gave it.
                    self%offsets(k+1_int64) = 0_int64
                    call set_bit_null(self, k)
                    nnull = nnull + 1_int64
                else
                    idx = handles(k)%idx
                    self%offsets(k+1_int64) = handles(k)%col%offsets(idx+1_int64) - &
                        handles(k)%col%offsets(idx)
                end if
            end do
        end do
        !$omp end parallel do
        self%n_null = nnull
        ! Phase 2: the scan. Serial deliberately -- O(m) over int64 and bandwidth-light next to the
        ! payload; measure before parallelising it.
        do k = 1_int64, m
            self%offsets(k+1_int64) = self%offsets(k+1_int64) + self%offsets(k)
        end do
        ! `data` was sized ONCE, from the validation pass, so a disagreement between that pass and
        ! this one would write past its end -- silently, since nothing here grows it any more. The
        ! serial fill has to check this per element because it discovers the overflow as it writes;
        ! here the whole sum is known before a single byte moves, so one compare covers it, and it
        ! catches an undersized sum too. Defensive by construction: no fixture this repository can
        ! build makes the two passes disagree.
        if (self%offsets(m+1_int64) /= want) then
            error stop EP//"build_from: internal error, payload sum disagrees with the fill" ! GCOVR_EXCL_LINE
        end if
        ! Phase 3: the payload copy -- the expensive one, and the one that divides cleanly. Each
        ! element's bytes are copied straight out of its OWN column's payload, since the handles may
        ! reference several different columns, so this is a contiguous copy per handle rather than
        ! one bulk move.
        !$omp parallel do default(shared) private(tix, k, a, elen, idx) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do k = lo(tix), hi(tix)
                elen = self%offsets(k+1_int64) - self%offsets(k)
                if (elen > 0_int64) then
                    idx = handles(k)%idx
                    a = handles(k)%col%offsets(idx) + 1_int64
                    self%data(self%offsets(k)+1_int64:self%offsets(k)+elen) = &
                        handles(k)%col%data(a:a+elen-1_int64)
                end if
            end do
        end do
        !$omp end parallel do
        self%nrows = m
        self%nchars = self%offsets(m+1_int64)
    end subroutine build_from_handles
    !
    !> `build_from`'s original single-pass fill, kept for the below-the-floor and in-a-parallel-
    !! region cases. See `build_from` for why both shapes exist and what binds them together.
    !!
    !! Offsets, validity and the payload in one loop. What it avoids -- and what any rewrite here
    !! must keep avoiding -- is the pair `%to_string` + `%append_string` per element: a
    !! deferred-length allocation and free, plus a capacity check on a destination that grew
    !! incrementally.
    subroutine build_from_fill_serial(self, handles, want)
        type(parquet_string_column), intent(inout) :: self !! sized and cleared; receives the fill.
        type(parquet_string), intent(in) :: handles(:)      !! source handles, already validated.
        integer(int64), intent(in) :: want                  !! payload bytes the validation pass summed.
        integer(int64) :: k, m, pos, a, b, elen, idx
        m = size(handles, kind=int64)
        pos = 0_int64
        do k = 1_int64, m
            if (handles(k)%is_null()) then
                call set_bit_null(self, k)
                self%n_null = self%n_null + 1_int64
            else
                idx = handles(k)%idx
                a = handles(k)%col%offsets(idx) + 1_int64
                b = handles(k)%col%offsets(idx+1_int64)
                elen = b - a + 1_int64
                ! The destination was sized ONCE, from the validation pass, so this loop is the one
                ! place where a disagreement between the two would write past the end of `data`
                ! -- silently, since nothing here grows it any more. One integer compare converts
                ! that into a clean abort. It cannot fire while both loops agree, and no fixture
                ! this repository can build makes them disagree (a small column is covered by
                ! MIN_CHAR_CAP whatever the sum says), so it is defensive by construction.
                if (pos + elen > want) then
                    error stop EP//"build_from: internal error, payload sum disagrees with the fill" ! GCOVR_EXCL_LINE
                end if
                if (elen > 0_int64) self%data(pos+1_int64:pos+elen) = handles(k)%col%data(a:b)
                pos = pos + elen
            end if
            ! Written on both arms: a null occupies a zero-width slot at the current position,
            ! exactly as %append_null gave it.
            self%offsets(k+1_int64) = pos
        end do
        self%nrows = m
        self%nchars = pos
    end subroutine build_from_fill_serial
    !
    !> Character-array specific of `build_from`: clears the column and rebuilds it from `values`,
    !! trimming each element's trailing blanks.
    !!
    !! **Why this exists rather than a loop of `%append_string`.** Every element of a
    !! `character(len=*)` array shares one declared length, so filling a column this way is the
    !! single commonest thing anyone does with one -- and doing it per element costs a call that
    !! re-derives the trim, re-checks two capacities that are already known to be sufficient, and
    !! copies the payload with `transfer(str(lo:hi), self%data, slen)`, which builds a temporary per
    !! element because the payload is `character(len=1), allocatable`. Measured on 1M x
    !! `character(len=24)`: **55.5 ms of per-element calls against 9.5 ms here**, taking
    !! `parquet_column%set_all` from 65.2 ms to 9.5 ms overall.
    !!
    !! **The trick that removes the per-element `transfer`.** `build_from_elements` takes `values`
    !! as an assumed-size array of `character(len=w)`, so its bytes are one `w*n` block;
    !! `pack_character_bytes` below re-sees exactly that block through a `character(len=1) ::
    !! src(*)` dummy by sequence association, after which each element's payload copy is an
    !! ordinary section-to-section assignment between two `character(len=1)` arrays -- the same
    !! shape `build_from_handles`' phase 3 uses, and no temporary.
    !!
    !! **`values` is deliberately NOT declared `contiguous`.** Sequence association needs contiguous
    !! storage, but a `contiguous` dummy has the COMPILER make it: gfortran copies an assumed-shape
    !! actual into a temporary on every call, contiguous or not, and ifx copies a strided one onto
    !! the stack, where a large array overflows it. A contiguous `values` instead reaches the
    !! assumed-size worker with no copy under either compiler, and a strided one is copied into an
    !! allocatable here.
    !!
    !! **`len_trim` stays on the ELEMENT view and must not be hand-rolled.** Replacing it with a
    !! trailing-blank scan over the byte view -- which looks like the natural thing to do once the
    !! byte view exists -- measured **3x slower** (32.5 ms against 9.5): the intrinsic is far better
    !! than a per-byte loop. `len_trim` is then the floor here, at ~7.5 ns/element.
    !!
    !! `is_null`, when present, marks those elements null: they occupy a zero-width slot, exactly
    !! as `%append_null` would leave them, and their `values` entry is ignored.
    subroutine build_from_character(self, values, is_null)
        class(parquet_string_column), intent(inout) :: self       !! cleared, then filled from `values`.
        character(len=*), intent(in) :: values(:)                 !! source elements; trailing blanks trimmed.
        logical, intent(in), optional :: is_null(:)               !! .true. => store that element as null.
        character(len=:), allocatable :: values_c(:)              !! `values`, copied when it is strided.
        if (is_contiguous(values)) then
            call build_from_elements(self, values, size(values, kind=int64), is_null)
        else
            values_c = values
            call build_from_elements(self, values_c, size(values_c, kind=int64), is_null)
        end if
    end subroutine build_from_character
    !
    !> `build_from_character`'s body, over `n` contiguous elements; see it for the shape.
    subroutine build_from_elements(self, values, n, is_null)
        class(parquet_string_column), intent(inout) :: self       !! cleared, then filled from `values`.
        character(len=*), intent(in) :: values(*)                 !! `n` source elements; trailing blanks trimmed.
        integer(int64), intent(in) :: n                           !! how many elements `values` holds.
        logical, intent(in), optional :: is_null(:)               !! .true. => store that element as null.
        integer(int64) :: k, nchars, nnull
        integer, allocatable :: lens(:)
        ! Stands in for `self%data` when the column holds no bytes at all. `ensure_data_cap`
        ! deliberately allocates nothing for a zero-byte payload, and `pack_character_bytes` takes
        ! `dst` as an ordinary (non-allocatable) dummy -- so passing `self%data` there unallocated
        ! is not conforming, and nagfor's -C=array aborts on it ("ALLOCATABLE SELF%DATA is not
        ! currently allocated") where gfortran runs on silently. Reached whenever every element is
        ! blank or null.
        ! Handing the same routine a real one-byte array instead keeps ONE implementation of the
        ! offset walk: with every `lens(k)` zero it writes offsets and never touches `dst`.
        character(len=1) :: no_bytes(1)
        if (present(is_null)) then
            if (size(is_null, kind=int64) /= n) then
                error stop EP//"build_from: is_null must have the same length as values"
            end if
        end if
        call self%clear()
        if (n <= 0_int64) return
        allocate(lens(n))
        ! Pass 1: each element's trimmed length, and the null count. Separate from the pack below
        ! because the payload has to be allocated at its exact final size before a byte moves --
        ! the same reason build_from_handles sizes in its validation pass.
        nchars = 0_int64
        nnull = 0_int64
        if (present(is_null)) then
            do k = 1_int64, n
                if (is_null(k)) then
                    lens(k) = 0
                    nnull = nnull + 1_int64
                else
                    lens(k) = len_trim(values(k))
                    nchars = nchars + int(lens(k), int64)
                end if
            end do
        else
            do k = 1_int64, n
                lens(k) = len_trim(values(k))
                nchars = nchars + int(lens(k), int64)
            end do
        end if
        call self%reserve(n, nchars)
        ! Pass 2: the prefix sum and the payload, in one walk, continuing from offsets(1) = 0.
        self%offsets(1) = 0_int64
        if (nchars > 0_int64) then
            call pack_character_bytes(values, self%data, self%offsets, lens, n, len(values), 0_int64)
        else
            call pack_character_bytes(values, no_bytes, self%offsets, lens, n, len(values), 0_int64)
        end if
        self%nrows = n
        self%nchars = nchars
        if (nnull > 0_int64) then
            self%has_nulls = .true.
            ! Freshly allocated validity bytes are all-ones (every row valid), so only the null
            ! rows need writing -- build_from_handles relies on the same property.
            call ensure_validity_cap(self, n)
            do k = 1_int64, n
                if (is_null(k)) call set_bit_null(self, k)
            end do
            self%n_null = nnull
        end if
    end subroutine build_from_elements
    !
    !> The packing walk of `build_from_elements` and `append_elements`. `src` is declared
    !! `character(len=1) :: src(*)` so that sequence association hands it the caller's whole `w*n`
    !! byte block; that is what makes each element's copy a section-to-section assignment rather
    !! than a `transfer` with a temporary. See `build_from_character` for the measurement that
    !! justifies the shape.
    subroutine pack_character_bytes(src, dst, offsets, lens, n, w, row_base)
        character(len=1), intent(in) :: src(*)      !! the caller's char(w) array, re-seen as bytes.
        character(len=1), intent(inout) :: dst(:)   !! the payload buffer, already sized.
        integer(int64), intent(inout) :: offsets(:) !! writes offsets(row_base+2 : row_base+n+1).
        integer, intent(in) :: lens(:)              !! each element's trimmed length, from pass 1.
        integer(int64), intent(in) :: n             !! element count.
        integer, intent(in) :: w                    !! declared length of one source element.
        integer(int64), intent(in) :: row_base      !! rows already in the column; 0 for a rebuild.
        integer(int64) :: k, base, at
        ! `offsets(row_base+1)` is already the running byte total -- 0 for a rebuild, `self%nchars`
        ! for an append -- so the scan simply continues from it and both callers share this loop.
        do k = 1_int64, n
            base = (k - 1_int64)*int(w, int64)
            at = offsets(row_base + k)
            offsets(row_base + k + 1_int64) = at + int(lens(k), int64)
            if (lens(k) > 0) then
                dst(at+1_int64 : at + int(lens(k), int64)) = src(base+1_int64 : base + int(lens(k), int64))
            end if
        end do
    end subroutine pack_character_bytes
    !
    !> Bulk-appends a character array, trimming each element's trailing blanks -- the appending
    !! counterpart of `build_from`'s character form, sharing its packing walk and existing for the
    !! same reason: `parquet_column%append_values` on a string column used one `%append_string` call
    !! per element. See `build_from_character` for why the byte-view copy is what makes this fast
    !! and why `len_trim` must stay on the element view.
    !!
    !! `is_null`, when present, appends those elements as zero-width nulls and ignores their
    !! `values` entry.
    !!
    !! `values` is not declared `contiguous`, for the reason `build_from_character` gives: a strided
    !! one is copied into an allocatable here, and a contiguous one reaches `append_elements` as it is.
    subroutine parquet_string_column_append_values(self, values, is_null)
        type(parquet_string_column), intent(inout) :: self   !! the column, appended to.
        character(len=*), intent(in) :: values(:)             !! elements to append; blanks trimmed.
        logical, intent(in), optional :: is_null(:)           !! .true. => append that element as null.
        character(len=:), allocatable :: values_c(:)          !! `values`, copied when it is strided.
        if (is_contiguous(values)) then
            call append_elements(self, values, size(values, kind=int64), is_null)
        else
            values_c = values
            call append_elements(self, values_c, size(values_c, kind=int64), is_null)
        end if
    end subroutine parquet_string_column_append_values
    !
    !> `parquet_string_column_append_values`' body, over `n` contiguous elements.
    subroutine append_elements(self, values, n, is_null)
        type(parquet_string_column), intent(inout) :: self   !! the column, appended to.
        character(len=*), intent(in) :: values(*)             !! `n` elements to append; blanks trimmed.
        integer(int64), intent(in) :: n                       !! how many elements `values` holds.
        logical, intent(in), optional :: is_null(:)           !! .true. => append that element as null.
        integer(int64) :: k, nchars, nnull, base_rows
        integer, allocatable :: lens(:)
        ! Stands in for `self%data` when the column holds no bytes at all. `ensure_data_cap`
        ! deliberately allocates nothing for a zero-byte payload, and `pack_character_bytes` takes
        ! `dst` as an ordinary (non-allocatable) dummy -- so passing `self%data` there unallocated
        ! is not conforming, and nagfor's -C=array aborts on it ("ALLOCATABLE SELF%DATA is not
        ! currently allocated") where gfortran runs on silently. Reached whenever every element is
        ! blank or null. Here it needs the column to be empty too, since an
        ! earlier append will already have allocated the payload.
        ! Handing the same routine a real one-byte array instead keeps ONE implementation of the
        ! offset walk: with every `lens(k)` zero it writes offsets and never touches `dst`.
        character(len=1) :: no_bytes(1)
        if (present(is_null)) then
            if (size(is_null, kind=int64) /= n) then
                error stop EP//"append_values: is_null must have the same length as values"
            end if
        end if
        if (n <= 0_int64) return
        allocate(lens(n))
        nchars = 0_int64
        nnull = 0_int64
        if (present(is_null)) then
            do k = 1_int64, n
                if (is_null(k)) then
                    lens(k) = 0
                    nnull = nnull + 1_int64
                else
                    lens(k) = len_trim(values(k))
                    nchars = nchars + int(lens(k), int64)
                end if
            end do
        else
            do k = 1_int64, n
                lens(k) = len_trim(values(k))
                nchars = nchars + int(lens(k), int64)
            end do
        end if
        base_rows = self%nrows
        call ensure_offsets_cap(self, base_rows + n)
        if (nchars > 0_int64) call ensure_data_cap(self, self%nchars + nchars)
        if (allocated(self%data)) then
            call pack_character_bytes(values, self%data, self%offsets, lens, n, len(values), base_rows)
        else
            call pack_character_bytes(values, no_bytes, self%offsets, lens, n, len(values), base_rows)
        end if
        if (nnull > 0_int64) then
            self%has_nulls = .true.
            call ensure_validity_cap(self, base_rows + n)
            do k = 1_int64, n
                if (is_null(k)) call set_bit_null(self, base_rows + k)
            end do
            self%n_null = self%n_null + nnull
        else if (self%has_nulls) then
            ! The column already tracks validity, so the appended rows need their bits written
            ! valid rather than left at whatever the grown buffer holds -- the same step
            ! `append_buffers` takes for exactly this case.
            call ensure_validity_cap(self, base_rows + n)
            call fill_validity_valid(self, base_rows + 1_int64, n)
        end if
        self%nrows = base_rows + n
        self%nchars = self%nchars + nchars
    end subroutine append_elements
    !
    !> Binding form of `parquet_string_column_append_values`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine append_values(self, values, is_null)
        class(parquet_string_column), intent(inout) :: self   !! the column, appended to.
        character(len=*), intent(in) :: values(:)             !! elements to append; blanks trimmed.
        logical, intent(in), optional :: is_null(:)           !! .true. => append that element as null.
        call parquet_string_column_append_values(self, values, is_null)
    end subroutine append_values
    !
    !> int32 specific of set; see the set generic.
    subroutine parquet_string_column_set_i32(self, i, str, strip, trim)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based element index.
        character(len=*), intent(in) :: str                 !! the replacement string.
        logical, intent(in), optional :: strip               !! remove leading and trailing blanks.
        logical, intent(in), optional :: trim                !! remove trailing blanks only.
        call parquet_string_column_set_i64(self, int(i, int64), str, strip, trim)
    end subroutine parquet_string_column_set_i32
    !
    !> Binding form of `parquet_string_column_set_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine set_i32(self, i, str, strip, trim)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based element index.
        character(len=*), intent(in) :: str                 !! the replacement string.
        logical, intent(in), optional :: strip               !! remove leading and trailing blanks.
        logical, intent(in), optional :: trim                !! remove trailing blanks only.
        call parquet_string_column_set_i32(self, i, str, strip, trim)
    end subroutine set_i32
    !
    !> int64 specific of set: replaces the content of element i (clearing its null status).
    !! Same-length replacement is O(length); a different length shifts the payload tail (O(N)).
    !! Same strip/trim options as append_string.
    subroutine parquet_string_column_set_i64(self, i, str, strip, trim)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based element index.
        character(len=*), intent(in) :: str                 !! the replacement string.
        logical, intent(in), optional :: strip               !! remove leading and trailing blanks.
        logical, intent(in), optional :: trim                !! remove trailing blanks only.
        logical :: do_strip, do_trim
        integer :: lo, hi
        integer(int64) :: a, b, old_len, new_len, delta, j
        call check_index(self, i, "set")
        do_strip = .false.
        if (present(strip)) do_strip = strip
        do_trim = .false.
        if (present(trim)) do_trim = trim
        call process_bounds(str, do_strip, do_trim, lo, hi)
        new_len = int(max(0, hi - lo + 1), int64)
        call elem_bounds(self, i, a, b)
        old_len = b - a + 1_int64
        delta = new_len - old_len
        if (delta /= 0_int64) then
            if (delta > 0_int64) call ensure_data_cap(self, self%nchars + delta)
            call elem_bounds(self, i, a, b)   ! re-read: data may have moved on grow
            ! shift the tail data(b+1 : nchars) by delta, directionally (no temp buffer)
            if (delta > 0_int64) then
                do j = self%nchars, b+1_int64, -1_int64
                    self%data(j+delta) = self%data(j)
                end do
            else
                do j = b+1_int64, self%nchars
                    self%data(j+delta) = self%data(j)
                end do
            end if
            do j = i+1_int64, self%nrows+1_int64
                self%offsets(j) = self%offsets(j) + delta
            end do
            self%nchars = self%nchars + delta
        end if
        if (new_len > 0) self%data(a:a+new_len-1_int64) = transfer(str(lo:hi), self%data, int(new_len))
        if (.not. bit_valid(self, i)) then
            call set_bit_valid(self, i)
            self%n_null = self%n_null - 1_int64
        end if
    end subroutine parquet_string_column_set_i64
    !
    !> Binding form of `parquet_string_column_set_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine set_i64(self, i, str, strip, trim)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based element index.
        character(len=*), intent(in) :: str                 !! the replacement string.
        logical, intent(in), optional :: strip               !! remove leading and trailing blanks.
        logical, intent(in), optional :: trim                !! remove trailing blanks only.
        call parquet_string_column_set_i64(self, i, str, strip, trim)
    end subroutine set_i64
    !
    !> int32 specific of set_null; see the set_null generic.
    subroutine parquet_string_column_set_null_i32(self, i)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based element index.
        call parquet_string_column_set_null_i64(self, int(i, int64))
    end subroutine parquet_string_column_set_null_i32
    !
    !> Binding form of `parquet_string_column_set_null_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine set_null_i32(self, i)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based element index.
        call parquet_string_column_set_null_i32(self, i)
    end subroutine set_null_i32
    !
    !> int64 specific of set_null: sets element i to null, discarding any existing content (shrinks
    !! its payload span to zero width, shifting the tail left by the same amount set_i64 would for
    !! a same-index replacement with a shorter string). Idempotent: calling this on an already-null
    !! element leaves it null without double-counting null_count().
    subroutine parquet_string_column_set_null_i64(self, i)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based element index.
        integer(int64) :: a, b, old_len, j
        logical :: was_null
        call check_index(self, i, "set_null")
        was_null = .not. bit_valid(self, i)
        call elem_bounds(self, i, a, b)
        old_len = b - a + 1_int64
        if (old_len > 0_int64) then
            do j = b+1_int64, self%nchars
                self%data(j-old_len) = self%data(j)
            end do
            do j = i+1_int64, self%nrows+1_int64
                self%offsets(j) = self%offsets(j) - old_len
            end do
            self%nchars = self%nchars - old_len
        end if
        self%has_nulls = .true.
        call ensure_validity_cap(self, self%nrows)
        call set_bit_null(self, i)
        if (.not. was_null) self%n_null = self%n_null + 1_int64
    end subroutine parquet_string_column_set_null_i64
    !
    !> Binding form of `parquet_string_column_set_null_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine set_null_i64(self, i)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based element index.
        call parquet_string_column_set_null_i64(self, i)
    end subroutine set_null_i64
    !
    !> int32 specific of erase; see the erase generic.
    subroutine erase_i32(self, i)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based element index.
        call self%erase_i64(int(i, int64))
    end subroutine erase_i32
    !
    !> int64 specific of erase: removes element i, shifting all later elements down by one
    !! (immediate compaction, order-preserving, O(N)). Invalidates handles at index >= i.
    subroutine erase_i64(self, i)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based element index.
        integer(int64) :: a, b, old_len, j
        logical :: was_null
        call check_index(self, i, "erase")
        was_null = .not. bit_valid(self, i)
        call elem_bounds(self, i, a, b)
        old_len = b - a + 1_int64
        ! compact payload: shift data(b+1 : nchars) left by old_len
        if (old_len > 0_int64) then
            do j = b+1_int64, self%nchars
                self%data(j-old_len) = self%data(j)
            end do
        end if
        ! rebuild offsets: drop offsets(i+1), shifting the rest down and subtracting old_len
        do j = i+1_int64, self%nrows
            self%offsets(j) = self%offsets(j+1) - old_len
        end do
        ! shift validity bits down by one (only meaningful when nulls exist)
        if (self%has_nulls) then
            do j = i, self%nrows-1_int64
                if (bit_valid(self, j+1_int64)) then
                    call set_bit_valid(self, j)
                else
                    call set_bit_null(self, j)
                end if
            end do
        end if
        if (was_null) self%n_null = self%n_null - 1_int64
        self%nrows = self%nrows - 1_int64
        self%nchars = self%nchars - old_len
    end subroutine erase_i64
    !
    !> int32 specific of reindex; see the reindex generic.
    subroutine parquet_string_column_reindex_i32(self, perm)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        call parquet_string_column_reindex_i64(self, int(perm, int64))
    end subroutine parquet_string_column_reindex_i32
    !
    !> Binding form of `parquet_string_column_reindex_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine reindex_i32(self, perm)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        call parquet_string_column_reindex_i32(self, perm)
    end subroutine reindex_i32
    !
    !> int64 specific of reindex: reorders every element so that element `k` of the result is the
    !! element that was at `perm(k)` before the call, rebuilding the payload, offsets and validity
    !! in one O(nchars) pass. `perm` must be a true permutation of `1..size()`; it is fully
    !! validated first (length, range, no duplicates), so a bad permutation aborts before any
    !! buffer is touched and the column is left unchanged. The null count is preserved by
    !! construction. Invalidates every outstanding handle into the column.
    subroutine parquet_string_column_reindex_i64(self, perm)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        integer(int64) :: n, k, p, word
        integer(int8), allocatable :: seen(:)
        n = self%nrows
        if (size(perm, kind=int64) /= n) then
            error stop EP//"reindex: permutation length does not match the row count"
        end if
        if (n == 0_int64) return
        ! Validate first: a bad permutation must not leave the column half-rebuilt. The seen-set is
        ! BIT-PACKED rather than a `logical` array, which on gfortran costs 4 bytes per element to
        ! record one bit -- and this walk is per ELEMENT, so a vector string column pays it over
        ! width*nrows. Same shape as parquet_column%reindex and check_permutation; keep them in step.
        allocate(seen((n + 7_int64)/8_int64))
        seen = 0_int8
        do k = 1_int64, n
            p = perm(k)
            if (p < 1_int64 .or. p > n) error stop EP//"reindex: permutation entry out of range"
            word = (p - 1_int64)/8_int64 + 1_int64
            if (btest(seen(word), int(mod(p - 1_int64, 8_int64)))) then
                error stop EP//"reindex: permutation contains a duplicate index"
            end if
            seen(word) = ibset(seen(word), int(mod(p - 1_int64, 8_int64)))
        end do
        deallocate(seen)
        call reindex_apply(self, perm)
    end subroutine parquet_string_column_reindex_i64
    !
    !> Binding form of `parquet_string_column_reindex_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine reindex_i64(self, perm)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        call parquet_string_column_reindex_i64(self, perm)
    end subroutine reindex_i64
    !
    !> int32 specific of reindex_trusted; see the reindex_trusted generic.
    subroutine parquet_string_column_reindex_trusted_i32(self, perm)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        call parquet_string_column_reindex_trusted_i64(self, int(perm, int64))
    end subroutine parquet_string_column_reindex_trusted_i32
    !
    !> Binding form of `parquet_string_column_reindex_trusted_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine reindex_trusted_i32(self, perm)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        call parquet_string_column_reindex_trusted_i32(self, perm)
    end subroutine reindex_trusted_i32
    !
    !> int64 specific of reindex_trusted: `reindex` without the O(n) range/duplicate scan, for a
    !! permutation the caller has already established is one. The O(1) length check still runs.
    !!
    !! **Public only because Fortran has no narrower visibility**, and reached from exactly two
    !! places: `parquet_column%reindex_trusted` (which is how `parquet_table%sort_by` avoids
    !! re-validating one permutation once per column) and `pf_permute(..., assume_valid=.true.)`.
    !! A caller who passes a non-permutation gets silently duplicated and dropped elements, so this
    !! is internal plumbing rather than an alternative to `%reindex`.
    subroutine parquet_string_column_reindex_trusted_i64(self, perm)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        if (size(perm, kind=int64) /= self%nrows) then
            error stop EP//"reindex_trusted: permutation length does not match the row count"
        end if
        if (self%nrows == 0_int64) return
        call reindex_apply(self, perm)
    end subroutine parquet_string_column_reindex_trusted_i64
    !
    !> Binding form of `parquet_string_column_reindex_trusted_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine reindex_trusted_i64(self, perm)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        call parquet_string_column_reindex_trusted_i64(self, perm)
    end subroutine reindex_trusted_i64
    !
    !> Rebuilds payload, offsets and validity in the order `perm` gives, for a permutation that has
    !! already been checked (or trusted). Split out so the two entry points differ only in whether
    !! they scan, rather than carrying two copies of the rebuild.
    !> `reindex_apply`'s single-pass form: the whole rebuild in one loop over the permutation.
    !!
    !! Faster than the parallel form's three phases whenever there is one thread to run it on, for
    !! the reason given at that procedure's own branch: one pass pays one cache miss per element on
    !! the scattered `offsets(perm(k))` read, and any splittable form pays it twice.
    subroutine reindex_apply_serial(self, perm)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        integer(int64) :: n, k, a, b, elen, pos
        integer(int64), allocatable :: new_off(:)
        character(len=1), allocatable :: new_data(:)
        logical, allocatable :: old_null(:)
        n = self%nrows
        if (self%has_nulls) then
            allocate(old_null(n))
            do k = 1_int64, n
                old_null(k) = .not. bit_valid(self, k)
            end do
        end if
        allocate(new_off(n+1_int64))
        new_off(1) = 0_int64
        allocate(new_data(max(self%nchars, 1_int64)))
        pos = 0_int64
        do k = 1_int64, n
            call elem_bounds(self, perm(k), a, b)
            elen = b - a + 1_int64
            if (elen > 0_int64) new_data(pos+1_int64:pos+elen) = self%data(a:b)
            pos = pos + elen
            new_off(k+1_int64) = pos
        end do
        call move_alloc(new_off, self%offsets)
        call move_alloc(new_data, self%data)
        if (self%has_nulls) then
            call ensure_validity_cap(self, n)
            do k = 1_int64, n
                if (old_null(perm(k))) then
                    call set_bit_null(self, k)
                else
                    call set_bit_valid(self, k)
                end if
            end do
        end if
    end subroutine reindex_apply_serial
    !
    subroutine reindex_apply(self, perm)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: perm(:)               !! 1-based permutation of 1..size().
        integer(int64) :: n, k, a, elen
        integer(int64), allocatable :: new_off(:)
        character(len=1), allocatable :: new_data(:)
        logical, allocatable :: old_null(:)
        integer(int64), allocatable :: lo(:), hi(:)
        integer :: nt, tix
        n = self%nrows
        nt = bulk_threads(n, self%nchars)
        ! **The serial path is the ORIGINAL single-pass loop, kept deliberately.** Making this
        ! splittable costs a restructure -- lengths, a scan, then the copy -- and that restructure is
        ! measurably slower than one pass when there is only one thread to run it on: 0.0154 s
        ! against 0.0266 s on 4 M elements / 70 MB. The reason is cache misses, not instruction
        ! count. `perm` is a permutation, so every `offsets(perm(k))` read misses in a 32 MB array,
        ! and any two-pass form pays that miss twice where one pass pays it once.
        !
        ! So collapsing these two into "just run the parallel version with nt = 1" would hand a 1.7x
        ! slowdown to every caller that does not thread -- a column below the work floor, and any
        ! caller already inside an OpenMP parallel region, which is where %sort_by reaches this from.
        ! The two paths must produce byte-identical columns; `test_parquet_string`'s equality test
        ! over both is what holds them to that.
        if (nt <= 1) then
            call reindex_apply_serial(self, perm)
            return
        end if
        call thread_row_ranges(n, nt, lo, hi)
        ! Capture the old null flags before any buffer is replaced. Writes are to disjoint elements
        ! of a fresh array, so ANY split is safe here -- the byte-aligned one is reused only because
        ! it is already computed.
        if (self%has_nulls) then
            allocate(old_null(n))
            !$omp parallel do default(shared) private(tix, k) schedule(static) num_threads(nt) if (nt > 1)
            do tix = 1, nt
                do k = lo(tix), hi(tix)
                    old_null(k) = .not. bit_valid(self, k)
                end do
            end do
            !$omp end parallel do
        end if
        allocate(new_off(n+1_int64))
        allocate(new_data(max(self%nchars, 1_int64)))
        ! Three phases, because the obvious single loop carries a sequential dependence through the
        ! write cursor and cannot be split at all. **The prefix sum IS `new_off`**, which is what
        ! makes this cost no extra memory: phase 1 writes each element's LENGTH into its own slot,
        ! phase 2 turns those lengths into offsets in place, and phase 3 then knows every element's
        ! destination without reference to any other element -- so the destination ranges are
        ! disjoint by construction and the copy is embarrassingly parallel.
        !
        ! Phase 1: lengths, one slot per element, no dependence.
        !$omp parallel do default(shared) private(tix, k) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do k = lo(tix), hi(tix)
                new_off(k+1_int64) = self%offsets(perm(k)+1_int64) - self%offsets(perm(k))
            end do
        end do
        !$omp end parallel do
        ! Phase 2: the scan. Serial deliberately -- it is O(n) over int64 and bandwidth-light next
        ! to the payload, and a parallel scan is more code and more risk than it is worth here.
        ! Measure before changing that.
        new_off(1) = 0_int64
        do k = 1_int64, n
            new_off(k+1_int64) = new_off(k+1_int64) + new_off(k)
        end do
        ! Phase 3: the payload copy -- the expensive one, and the one that divides cleanly.
        !$omp parallel do default(shared) private(tix, k, a, elen) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do k = lo(tix), hi(tix)
                ! The LENGTH comes from `new_off`, which phase 2 just made sequential -- not from a
                ! second look at `offsets(perm(k)+1)`. That matters more than it looks: `perm` is a
                ! permutation, so every `offsets(perm(k))` read is a cache miss into a 32 MB array,
                ! and taking the length from here rather than from the source halves the misses this
                ! phase pays. Only the source START still has to be looked up.
                elen = new_off(k+1_int64) - new_off(k)
                if (elen > 0_int64) then
                    a = self%offsets(perm(k)) + 1_int64
                    new_data(new_off(k)+1_int64:new_off(k)+elen) = self%data(a:a+elen-1_int64)
                end if
            end do
        end do
        !$omp end parallel do
        call move_alloc(new_off, self%offsets)
        call move_alloc(new_data, self%data)
        ! Validity, in the new order (n_null is unchanged by a permutation). **This is the phase the
        ! byte-aligned ranges exist for**: the bitmap packs 8 rows per byte, so two threads meeting
        ! inside a byte would race on it -- a read-modify-write each, one lost, with nothing to
        ! notice afterwards. `thread_row_ranges` makes that impossible rather than unlikely.
        if (self%has_nulls) then
            call ensure_validity_cap(self, n)
            !$omp parallel do default(shared) private(tix, k) schedule(static) num_threads(nt) if (nt > 1)
            do tix = 1, nt
                do k = lo(tix), hi(tix)
                    if (old_null(perm(k))) then
                        call set_bit_null(self, k)
                    else
                        call set_bit_valid(self, k)
                    end if
                end do
            end do
            !$omp end parallel do
        end if
    end subroutine reindex_apply
    !
    !> Keeps only the elements whose `keep` entry is .true., in order, compacting payload, offsets
    !! and validity in one O(nchars) in-place pass. `keep` must have exactly `size()` entries.
    !! Dropping every element leaves a valid empty column. Invalidates every outstanding handle
    !! into the column. Note this is the bulk counterpart of `erase`: deleting m elements one at a
    !! time costs O(m*nchars), this costs O(nchars) once.
    subroutine parquet_string_column_delete_by_mask(self, keep)
        type(parquet_string_column), intent(inout) :: self !! the column.
        logical, intent(in) :: keep(:)                      !! .true. for every element to retain.
        integer(int64) :: n
        integer :: nt
        n = self%nrows
        if (size(keep, kind=int64) /= n) then
            error stop EP//"delete_by_mask: mask length does not match the row count"
        end if
        if (n == 0_int64) return
        nt = bulk_threads(n, self%nchars)
        if (nt <= 1) then
            call delete_by_mask_serial(self, keep)
        else
            call delete_by_mask_parallel(self, keep, nt)
        end if
    end subroutine parquet_string_column_delete_by_mask
    !
    !> Binding form of `parquet_string_column_delete_by_mask`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine delete_by_mask(self, keep)
        class(parquet_string_column), intent(inout) :: self !! the column.
        logical, intent(in) :: keep(:)                      !! .true. for every element to retain.
        call parquet_string_column_delete_by_mask(self, keep)
    end subroutine delete_by_mask
    !
    !> `delete_by_mask`'s threaded form. **The only rebuild in this module that carries TWO
    !! loop-carried cursors** -- a destination row index and a destination byte offset -- because it
    !! is the only one whose output row count differs from its input's.
    !!
    !! That is why it does not use the per-element prefix sum every other phased rebuild here uses:
    !! `offsets` is indexed by DESTINATION row, and the natural per-element array is indexed by
    !! SOURCE row, so the usual shape would need a second `n`-sized temporary to carry the mapping.
    !! Instead each thread counts its own range first (rows and bytes), an `nt`-sized exclusive scan
    !! turns those counts into per-thread bases, and each thread then walks its range sequentially
    !! from its own base. **Auxiliary memory is O(threads), not O(rows).**
    !!
    !! Like `compact_all`, it rebuilds into fresh buffers rather than compacting in place -- see that
    !! procedure for why in-place and parallel are incompatible here, and why the fresh destination
    !! is also what lets the copy be a `memcpy` rather than a scalar byte loop.
    subroutine delete_by_mask_parallel(self, keep, nt)
        type(parquet_string_column), intent(inout) :: self !! the column.
        logical, intent(in) :: keep(:)                      !! .true. for every element to retain.
        integer, intent(in) :: nt                           !! threads to use (> 1).
        integer(int64) :: n, k, a, b, elen, kept, total, nn, rpos, bpos, cnt, bytes
        integer(int64), allocatable :: new_off(:), rlo(:), rhi(:), row_base(:), byte_base(:)
        character(len=1), allocatable :: new_data(:)
        logical, allocatable :: old_null(:)
        integer :: tix
        n = self%nrows
        call thread_row_ranges(n, nt, rlo, rhi)
        allocate(row_base(nt + 1), byte_base(nt + 1))
        if (self%has_nulls) then
            allocate(old_null(n))
            !$omp parallel do default(shared) private(tix, k) schedule(static) num_threads(nt) if (nt > 1)
            do tix = 1, nt
                do k = rlo(tix), rhi(tix)
                    old_null(k) = .not. bit_valid(self, k)
                end do
            end do
            !$omp end parallel do
        end if
        ! Phase 1: how many rows and bytes each thread's range contributes. Reads `keep` and
        ! `offsets` only -- never the payload -- so it is cheap next to the copy it is planning.
        !$omp parallel do default(shared) private(tix, k, cnt, bytes) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            cnt = 0_int64
            bytes = 0_int64
            do k = rlo(tix), rhi(tix)
                if (keep(k)) then
                    cnt = cnt + 1_int64
                    bytes = bytes + (self%offsets(k+1_int64) - self%offsets(k))
                end if
            end do
            row_base(tix+1) = cnt
            byte_base(tix+1) = bytes
        end do
        !$omp end parallel do
        ! Phase 2: the scan -- over `nt` entries rather than `n`, which is the whole point.
        row_base(1) = 0_int64
        byte_base(1) = 0_int64
        do tix = 1, nt
            row_base(tix+1) = row_base(tix+1) + row_base(tix)
            byte_base(tix+1) = byte_base(tix+1) + byte_base(tix)
        end do
        kept = row_base(nt+1)
        total = byte_base(nt+1)
        allocate(new_off(kept + 1_int64))
        new_off(1) = 0_int64
        allocate(new_data(max(total, 1_int64)))
        ! Phase 3: each thread compacts its own range into its own disjoint slice of both outputs.
        !$omp parallel do default(shared) private(tix, k, a, b, elen, rpos, bpos) &
        !$omp     schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            rpos = row_base(tix)
            bpos = byte_base(tix)
            do k = rlo(tix), rhi(tix)
                if (.not. keep(k)) cycle
                call elem_bounds(self, k, a, b)
                elen = b - a + 1_int64
                if (elen > 0_int64) new_data(bpos+1_int64:bpos+elen) = self%data(a:b)
                bpos = bpos + elen
                rpos = rpos + 1_int64
                new_off(rpos+1_int64) = bpos
            end do
        end do
        !$omp end parallel do
        call move_alloc(new_off, self%offsets)
        call move_alloc(new_data, self%data)
        self%nrows = kept
        self%nchars = total
        ! Phase 4: validity, and it is **deliberately serial**. A destination row index is a rank
        ! among survivors, so thread `t`'s first output row is `row_base(t)+1`, which is not a
        ! multiple of 8 -- the byte-aligned split that makes every other threaded validity phase in
        ! this module safe cannot be constructed here. Writing it serially is correct by
        ! construction; splitting it on `row_base` would reintroduce the unaligned
        ! read-modify-write.
        if (self%has_nulls) then
            call ensure_validity_cap(self, max(kept, 1_int64))
            call rebuild_validity_compacted(self, keep, old_null, n, nn)
            self%n_null = nn
        end if
    end subroutine delete_by_mask_parallel
    !
    !> `delete_by_mask`'s serial, in-place form. See `delete_by_mask_parallel` for why the threaded
    !! one cannot be in place and why this one's copy must stay a scalar loop.
    subroutine delete_by_mask_serial(self, keep)
        type(parquet_string_column), intent(inout) :: self !! the column.
        logical, intent(in) :: keep(:)                      !! .true. for every element to retain.
        integer(int64) :: n, k, j, a, b, elen, pos, kept, nn
        logical, allocatable :: old_null(:)
        n = self%nrows
        if (self%has_nulls) then
            allocate(old_null(n))
            do k = 1_int64, n
                old_null(k) = .not. bit_valid(self, k)
            end do
        end if
        ! compact payload and offsets in place; the write cursor never overtakes the read cursor
        pos = 0_int64
        kept = 0_int64
        do k = 1_int64, n
            if (.not. keep(k)) cycle
            call elem_bounds(self, k, a, b)
            elen = b - a + 1_int64
            ! A scalar byte loop, deliberately -- see `compact_all` for why a section assignment
            ! between two ranges of the SAME array costs a heap temporary per element (measured at
            ! 7.5x slower here) and must not be substituted for this.
            do j = 0_int64, elen - 1_int64
                self%data(pos+1_int64+j) = self%data(a+j)
            end do
            pos = pos + elen
            kept = kept + 1_int64
            self%offsets(kept+1_int64) = pos
        end do
        self%nrows = kept
        self%nchars = pos
        ! rebuild validity for the surviving elements and recount the nulls
        if (self%has_nulls) then
            call rebuild_validity_compacted(self, keep, old_null, n, nn)
            self%n_null = nn
        end if
    end subroutine delete_by_mask_serial
    !
    !> Nulls every element whose `valid` entry is `.false.`, in ONE rebuild of the payload.
    !!
    !! `parquet_column%set_validity`'s contract one tier down: a `.false.` entry nulls that
    !! element, discarding its bytes exactly as `set_null` would; a `.true.` entry changes NOTHING,
    !! so an element that is already null stays null and the call only ever ADDS nulls. An all-true
    !! mask returns before a buffer is touched. `valid` must have exactly `size()` entries.
    !!
    !! **This exists because `set_null` compacts the payload on every call.** Nulling m elements
    !! one at a time costs O(m * (nchars + nrows)) -- quadratic in the nulls -- where this costs
    !! one pass over the payload whatever m is. That is the difference between a `%join` that
    !! null-filled two 16-byte string columns over 800k rows in 208 s and one that does it in
    !! well under a second; `bench/benchmark_join.sh --mode=nullfill` holds that ratio, since no
    !! unit test can assert a complexity class. Same trade-off as `delete_by_mask` against
    !! `erase`, and the same rebuild: `rebuild_selected` is shared with `set_where`.
    subroutine parquet_string_column_set_validity(self, valid)
        type(parquet_string_column), intent(inout) :: self !! the column.
        logical, intent(in) :: valid(:)                     !! one entry per element; .false. nulls it.
        character(len=1) :: no_bytes(1)
        integer(int64) :: n
        integer :: nt
        n = self%nrows
        if (size(valid, kind=int64) /= n) call fail_mask_length(size(valid, kind=int64), n, "set_validity")
        if (n == 0_int64) return
        if (all(valid)) return
        nt = bulk_threads(n, self%nchars)
        no_bytes = " "
        call rebuild_selected(self, .not. valid, 0_int64, no_bytes, .true., nt)
    end subroutine parquet_string_column_set_validity
    !
    !> Binding form of `parquet_string_column_set_validity`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine set_validity(self, valid)
        class(parquet_string_column), intent(inout) :: self !! the column.
        logical, intent(in) :: valid(:)                     !! one entry per element; .false. nulls it.
        call parquet_string_column_set_validity(self, valid)
    end subroutine set_validity
    !
    !> Writes `value` into every element whose `mask` entry is `.true.`, in ONE rebuild of the
    !! payload, and marks each of them valid.
    !!
    !! The bulk counterpart of `set(i, value)` over a selection, and the same rebuild as
    !! `set_validity` with a value in place of the zero-width slot. The mask, not the null state,
    !! selects: an element that already held a value takes `value` too when its entry is `.true.`,
    !! and a null element whose entry is `.false.` stays null. `value` is stored verbatim, blanks
    !! included -- a scalar is never trimmed, which is `set`'s rule without its `strip=`/`trim=`
    !! options. An all-false mask returns before a buffer is touched. `mask` must have exactly
    !! `size()` entries.
    !!
    !! Exists for `parquet_table%fillna`, whose per-element `set` shifted the payload tail once
    !! per null -- O(nulls * nchars) -- where this costs one pass; see `set_validity` for the
    !! measurement and the benchmark that holds it.
    subroutine parquet_string_column_set_where(self, mask, value)
        type(parquet_string_column), intent(inout) :: self !! the column.
        logical, intent(in) :: mask(:)                      !! one entry per element; .true. takes `value`.
        character(len=*), intent(in) :: value               !! the replacement, stored verbatim.
        character(len=1), allocatable :: vbytes(:)
        integer(int64) :: n, vlen, nsel
        integer :: nt
        n = self%nrows
        if (size(mask, kind=int64) /= n) call fail_mask_length(size(mask, kind=int64), n, "set_where")
        if (n == 0_int64) return
        nsel = count(mask, kind=int64)
        if (nsel == 0_int64) return
        vlen = len(value, kind=int64)
        ! Re-seen as bytes ONCE, here, rather than by a `transfer` per selected element -- which
        ! would allocate a temporary each time (fortran-gotchas.md). Sized at least 1 so that a
        ! zero-length value still hands the rebuild an allocated array.
        allocate(vbytes(max(vlen, 1_int64)))
        vbytes = " "
        if (vlen > 0_int64) vbytes(1:vlen) = transfer(value, vbytes(1:vlen))
        ! The bytes this call will MOVE: the kept payload plus one copy of the value per selected
        ! element, which is what the threading floor has to be measured against.
        nt = bulk_threads(n, self%nchars + nsel*vlen)
        call rebuild_selected(self, mask, vlen, vbytes, .false., nt)
    end subroutine parquet_string_column_set_where
    !
    !> Binding form of `parquet_string_column_set_where`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine set_where(self, mask, value)
        class(parquet_string_column), intent(inout) :: self !! the column.
        logical, intent(in) :: mask(:)                      !! one entry per element; .true. takes `value`.
        character(len=*), intent(in) :: value               !! the replacement, stored verbatim.
        call parquet_string_column_set_where(self, mask, value)
    end subroutine set_where
    !
    !> The mask-length abort `set_validity` and `set_where` share, naming both counts.
    subroutine fail_mask_length(got, want, proc)
        integer(int64), intent(in) :: got   !! entries the caller's mask has.
        integer(int64), intent(in) :: want  !! elements the column has.
        character(len=*), intent(in) :: proc !! the calling procedure, for the message.
        character(len=32) :: tg, tw
        write(tg, "(I0)") got
        write(tw, "(I0)") want
        error stop EP//proc//": mask has "//trim(tg)//" entries but the column has "//trim(tw)// &
            " elements"
    end subroutine fail_mask_length
    !
    !> The one rebuild behind `set_validity` and `set_where`: every element `sel` marks is
    !! REPLACED by the `rlen` bytes of `rbytes` -- none of them for a null -- and every other
    !! element keeps its bytes; then the selected elements' validity bits are turned toward
    !! `to_null`, and `n_null` moves by exactly the number of bits that changed, so a selected
    !! element already in the requested state costs nothing and counts nothing.
    !!
    !! **The row count does not change, so the destination row IS the source row** and only the
    !! byte cursor is loop-carried. That is what makes this simpler than `delete_by_mask_parallel`
    !! (two cursors) and lets the same plan serve one thread or many: each range of
    !! `thread_row_ranges` counts the bytes it will emit, an `nt`-sized exclusive scan turns the
    !! counts into per-range byte bases, and each range then writes its own disjoint slice of the
    !! fresh payload AND its own offsets entries. Auxiliary memory is O(threads), not O(rows).
    !!
    !! **Fresh buffers even on one thread**, unlike `delete_by_mask_serial`: a `set_where` value
    !! can be longer than the element it replaces, so an in-place walk could overrun bytes it has
    !! not read yet, and one shape for both callers is worth one transient copy of the payload --
    !! which the join that motivated this already pays for its gather.
    !!
    !! The validity phase is byte-aligned by construction (`thread_row_ranges`), so no two threads
    !! ever read-modify-write the same validity byte, which is the condition it must meet, and
    !! the reason the row ranges are reused for it rather than any even split.
    subroutine rebuild_selected(self, sel, rlen, rbytes, to_null, nt)
        type(parquet_string_column), intent(inout) :: self !! the column.
        logical, intent(in) :: sel(:)                       !! .true. for every element to replace.
        integer(int64), intent(in) :: rlen                  !! bytes each replaced element takes.
        character(len=1), intent(in) :: rbytes(:)           !! those bytes (at least one entry).
        logical, intent(in) :: to_null                      !! .true.: the replaced elements become null.
        integer, intent(in) :: nt                           !! threads to use (>= 1).
        integer(int64) :: n, k, a, b, elen, total, nn, bpos, bytes
        integer(int64), allocatable :: new_off(:), rlo(:), rhi(:), byte_base(:)
        character(len=1), allocatable :: new_data(:)
        integer :: tix
        n = self%nrows
        call thread_row_ranges(n, nt, rlo, rhi)
        allocate(byte_base(nt + 1))
        ! Phase 1: the bytes each range will emit. Reads `sel` and `offsets` only, never the
        ! payload, so it is cheap next to the copy it is planning.
        !$omp parallel do default(shared) private(tix, k, bytes) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            bytes = 0_int64
            do k = rlo(tix), rhi(tix)
                if (sel(k)) then
                    bytes = bytes + rlen
                else
                    bytes = bytes + (self%offsets(k+1_int64) - self%offsets(k))
                end if
            end do
            byte_base(tix+1) = bytes
        end do
        !$omp end parallel do
        ! Phase 2: the scan, over `nt` entries rather than `n`.
        byte_base(1) = 0_int64
        do tix = 1, nt
            byte_base(tix+1) = byte_base(tix+1) + byte_base(tix)
        end do
        total = byte_base(nt+1)
        allocate(new_off(n + 1_int64))
        new_off(1) = 0_int64
        allocate(new_data(max(total, 1_int64)))
        ! Phase 3: each range fills its own disjoint slice of both outputs from its own base.
        !$omp parallel do default(shared) private(tix, k, a, b, elen, bpos) &
        !$omp     schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            bpos = byte_base(tix)
            do k = rlo(tix), rhi(tix)
                if (sel(k)) then
                    if (rlen > 0_int64) new_data(bpos+1_int64:bpos+rlen) = rbytes(1:rlen)
                    bpos = bpos + rlen
                else
                    call elem_bounds(self, k, a, b)
                    elen = b - a + 1_int64
                    if (elen > 0_int64) new_data(bpos+1_int64:bpos+elen) = self%data(a:b)
                    bpos = bpos + elen
                end if
                new_off(k+1_int64) = bpos
            end do
        end do
        !$omp end parallel do
        call move_alloc(new_off, self%offsets)
        call move_alloc(new_data, self%data)
        self%nchars = total
        ! Phase 4: validity. Nulling needs the bitmap whether or not one exists yet; clearing
        ! nulls on a column that has none is a no-op, and `bit_valid` already answers .true. for
        ! every element of such a column, so the walk would change nothing -- skip it outright.
        if (to_null) then
            self%has_nulls = .true.
            call ensure_validity_cap(self, n)
        else if (.not. self%has_nulls) then
            return
        end if
        nn = 0_int64
        !$omp parallel do default(shared) private(tix, k) reduction(+:nn) &
        !$omp     schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do k = rlo(tix), rhi(tix)
                if (.not. sel(k)) cycle
                if (to_null) then
                    if (bit_valid(self, k)) then
                        call set_bit_null(self, k)
                        nn = nn + 1_int64
                    end if
                else
                    if (.not. bit_valid(self, k)) then
                        call set_bit_valid(self, k)
                        nn = nn + 1_int64
                    end if
                end if
            end do
        end do
        !$omp end parallel do
        if (to_null) then
            self%n_null = self%n_null + nn
        else
            self%n_null = self%n_null - nn
        end if
    end subroutine rebuild_selected
    !
    !> int32 specific of gather; see the gather generic.
    subroutine parquet_string_column_gather_i32(self, idx)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: idx(:)                !! 1-based source index per destination element.
        call parquet_string_column_gather_i64(self, int(idx, int64))
    end subroutine parquet_string_column_gather_i32
    !
    !> Binding form of `parquet_string_column_gather_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine gather_i32(self, idx)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: idx(:)                !! 1-based source index per destination element.
        call parquet_string_column_gather_i32(self, idx)
    end subroutine gather_i32
    !
    !> int64 specific of gather: rebuilds the column so that element `k` is the element that was at
    !! `idx(k)`, for an index list of ANY length.
    !!
    !! This is the subset-and-reorder primitive `reindex` and `delete_by_mask` do not provide between
    !! them: `reindex` demands a permutation of the whole column, and `delete_by_mask` keeps the
    !! existing order. `idx` may name any element in 1..size(), in any order, and **may name one more
    !! than once** -- it is a gather, not a permutation, so the result may be shorter than, as long as,
    !! or longer than the column it replaces.
    !!
    !! **Only the range is checked.** Refusing repeats would need a seen-set sized by the SOURCE
    !! element count on every call, which is exactly the cost this primitive exists to avoid; a caller
    !! that needs distinctness (parquet_table%top_n does) checks it once for itself.
    !!
    !! **The rebuild is `gather_from`'s** (`gather_build` below), run from this column into a fresh
    !! one that `move_from` then hands back in O(1): one implementation of the two shapes -- the
    !! single-cursor serial twin and the four-phase threaded form -- for both entry points. Fresh
    !! buffers rather than an in-place compaction were always the rule here, because a reordering
    !! write cursor can overtake its own read cursor (which is why `delete_by_mask`, whose output
    !! order is its input order, may compact in place and this may not), so the hand-over costs
    !! nothing the old form did not already pay.
    subroutine parquet_string_column_gather_i64(self, idx)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: idx(:)                !! 1-based source index per destination element.
        type(parquet_string_column) :: fresh
        integer(int64) :: n, m, k
        n = self%nrows
        m = size(idx, kind=int64)
        ! Serial deliberately: which index is out of range must not depend on which thread noticed.
        do k = 1_int64, m
            if (idx(k) < 1_int64 .or. idx(k) > n) then
                error stop EP//"gather: index out of range"
            end if
        end do
        call gather_build(fresh, self, idx)
        call parquet_string_column_move_from(self, fresh)
    end subroutine parquet_string_column_gather_i64
    !
    !> Binding form of `parquet_string_column_gather_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine gather_i64(self, idx)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: idx(:)                !! 1-based source index per destination element.
        call parquet_string_column_gather_i64(self, idx)
    end subroutine gather_i64
    !
    !> int32 specific of gather_from; see the int64 form below.
    subroutine parquet_string_column_gather_from_i32(self, src, idx, valid, threads)
        type(parquet_string_column), intent(inout) :: self !! the destination; cleared first.
        type(parquet_string_column), intent(in) :: src     !! the source column, never written.
        integer(int32), intent(in) :: idx(:)                !! 1-based source element per destination element.
        logical, intent(in), optional :: valid(:)           !! per destination element; .false. nulls it.
        integer, intent(in), optional :: threads            !! explicit team; absent resolves as every bulk operation does.
        call parquet_string_column_gather_from_i64(self, src, int(idx, int64), valid=valid, threads=threads)
    end subroutine parquet_string_column_gather_from_i32
    !
    !> Binding form of `parquet_string_column_gather_from_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine gather_from_i32(self, src, idx, valid, threads)
        class(parquet_string_column), intent(inout) :: self !! the destination; cleared first.
        type(parquet_string_column), intent(in) :: src      !! the source column, never written.
        integer(int32), intent(in) :: idx(:)                !! 1-based source element per destination element.
        logical, intent(in), optional :: valid(:)           !! per destination element; .false. nulls it.
        integer, intent(in), optional :: threads            !! explicit team; absent resolves as every bulk operation does.
        call parquet_string_column_gather_from_i32(self, src, idx, valid=valid, threads=threads)
    end subroutine gather_from_i32
    !
    !> int64 specific of gather_from: builds this column from the elements of `src` that `idx` lists,
    !! in the order it lists them, in ONE pass -- `clone` + `gather` + `set_validity` as one
    !! operation. Element `k` of the result is `src`'s element `idx(k)`, with `src`'s own nulls
    !! carried across, and -- when `valid` is present -- element `k` null as well wherever
    !! `valid(k)` is `.false.`: a masked element takes a ZERO-WIDTH slot rather than a copy of bytes
    !! nothing may read, and the source is never touched. The destination is cleared first and its
    !! payload is sized to the selected characters exactly.
    !!
    !! `idx` is a gather on `gather`'s terms (any element, any order, repeats allowed, any length,
    !! only the range checked) and `valid` must have exactly `size(idx)` entries. The mask only ever
    !! ADDS nulls -- `parquet_column%gather_from` rests on that, one tier up -- and the null COUNT
    !! is recounted, since a selection may drop or repeat a null.
    !!
    !! **Threads.** Absent `threads`, the team is what every bulk operation here resolves to
    !! (`bulk_threads`: the cap, the payload floor, serial inside a parallel region). An explicit
    !! `threads` is honoured on the terms `bulk_threads_explicit` gives: bounded by the affinity
    !! mask, the validity-byte rule, the break-even and the payload floor, but not stood down inside
    !! a parallel region -- it exists so that `parquet_table`'s per-column rewrite, which has already
    !! decided to spend its team inside one column, can hand that team down. The serial
    !! single-cursor twin is kept for the measured reason `gather`'s always was: the phased form is
    !! 1.5x slower on one thread.
    subroutine parquet_string_column_gather_from_i64(self, src, idx, valid, threads)
        type(parquet_string_column), intent(inout) :: self !! the destination; cleared first.
        type(parquet_string_column), intent(in) :: src     !! the source column, never written.
        integer(int64), intent(in) :: idx(:)                !! 1-based source element per destination element.
        logical, intent(in), optional :: valid(:)           !! per destination element; .false. nulls it.
        integer, intent(in), optional :: threads            !! explicit team; absent resolves as every bulk operation does.
        integer(int64) :: n, m, k
        character(len=32) :: tg, tw
        n = src%nrows
        m = size(idx, kind=int64)
        if (present(valid)) then
            if (size(valid, kind=int64) /= m) then
                write(tg, "(I0)") size(valid, kind=int64)
                write(tw, "(I0)") m
                error stop EP//"gather_from: mask has "//trim(tg)//" entries but the index list names "// &
                    trim(tw)//" elements"
            end if
        end if
        ! Serial deliberately: which index is out of range must not depend on which thread noticed.
        do k = 1_int64, m
            if (idx(k) < 1_int64 .or. idx(k) > n) then
                error stop EP//"gather_from: index out of range"
            end if
        end do
        call gather_build(self, src, idx, valid, threads)
    end subroutine parquet_string_column_gather_from_i64
    !
    !> Binding form of `parquet_string_column_gather_from_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine gather_from_i64(self, src, idx, valid, threads)
        class(parquet_string_column), intent(inout) :: self !! the destination; cleared first.
        type(parquet_string_column), intent(in) :: src      !! the source column, never written.
        integer(int64), intent(in) :: idx(:)                !! 1-based source element per destination element.
        logical, intent(in), optional :: valid(:)           !! per destination element; .false. nulls it.
        integer, intent(in), optional :: threads            !! explicit team; absent resolves as every bulk operation does.
        call parquet_string_column_gather_from_i64(self, src, idx, valid=valid, threads=threads)
    end subroutine gather_from_i64
    !
    !> The one gather rebuild: `dst` becomes `src`'s elements `idx` lists, nulls carried, the mask's
    !! nulls added, on the team `threads` (explicit) or `bulk_threads` (automatic) resolves. Every
    !! index is in range and the mask, if present, has `size(idx)` entries -- the callers check.
    !!
    !! Four phases on the prefix-sum plan `reindex_apply` documents, and where the mask is read is
    !! the whole of its cost: phase 1 writes a masked element's length as 0 and its null flag as
    !! set, and nothing later has to know the mask exists. Phase 1: each selected element's LENGTH
    !! into its own slot, and its null flag -- disjoint slots of fresh arrays, any split safe. Phase
    !! 2: the scan, serial, which also sizes the payload to the SELECTED characters. Phase 3: the
    !! payload copy, disjoint destinations by construction. Phase 4: the validity bits, on the
    !! byte-aligned ranges (`thread_row_ranges`) that keep two threads out of one byte.
    subroutine gather_build(dst, src, idx, valid, threads)
        type(parquet_string_column), intent(inout) :: dst !! the destination; cleared first.
        type(parquet_string_column), intent(in) :: src    !! the source.
        integer(int64), intent(in) :: idx(:)               !! source element per destination element, in range.
        logical, intent(in), optional :: valid(:)          !! per destination element, when present.
        integer, intent(in), optional :: threads           !! explicit team, when present.
        integer(int64) :: n, m, k, a, elen, want, nn, est
        integer(int64), allocatable :: lo(:), hi(:)
        logical, allocatable :: sel_null(:)
        logical :: masked, nulls
        integer :: nt, tix
        n = src%nrows
        m = size(idx, kind=int64)
        call parquet_string_column_clear(dst)
        ! An all-true mask is no mask: it must cost no validity bytes and no per-element test.
        masked = .false.
        if (present(valid)) masked = .not. all(valid)
        nulls = src%has_nulls .or. masked
        ! The work measure has to be ESTIMATED, because the exact selected payload is only known
        ! after phase 1, which is itself one of the phases being split. Mean element length times
        ! the selection size is exact for a uniform column and cannot be far wrong for any column,
        ! and it is only ever used to answer "is this worth splitting" -- never as a size.
        est = 0_int64
        if (n > 0_int64) est = (src%nchars/n)*m
        if (present(threads)) then
            nt = bulk_threads_explicit(threads, m, est)
        else
            nt = bulk_threads(m, est)
        end if
        if (nt <= 1) then
            call gather_serial(dst, src, idx, valid, masked, nulls)
            return
        end if
        call thread_row_ranges(m, nt, lo, hi)
        allocate(dst%offsets(m + 1_int64))
        dst%offsets(1) = 0_int64
        if (nulls) allocate(sel_null(max(m, 1_int64)))
        !$omp parallel do default(shared) private(tix, k) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do k = lo(tix), hi(tix)
                if (masked) then
                    if (.not. valid(k)) then
                        dst%offsets(k+1_int64) = 0_int64
                        sel_null(k) = .true.
                        cycle
                    end if
                end if
                dst%offsets(k+1_int64) = src%offsets(idx(k)+1_int64) - src%offsets(idx(k))
                if (nulls) sel_null(k) = .not. bit_valid(src, idx(k))
            end do
        end do
        !$omp end parallel do
        do k = 1_int64, m
            dst%offsets(k+1_int64) = dst%offsets(k+1_int64) + dst%offsets(k)
        end do
        want = dst%offsets(m+1_int64)
        allocate(dst%data(max(want, 1_int64)))
        !$omp parallel do default(shared) private(tix, k, a, elen) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do k = lo(tix), hi(tix)
                elen = dst%offsets(k+1_int64) - dst%offsets(k)
                if (elen > 0_int64) then
                    a = src%offsets(idx(k)) + 1_int64
                    dst%data(dst%offsets(k)+1_int64:dst%offsets(k)+elen) = src%data(a:a+elen-1_int64)
                end if
            end do
        end do
        !$omp end parallel do
        dst%nrows = m
        dst%nchars = want
        if (nulls) then
            ! Fresh bytes start all-valid (ensure_validity_cap), so only the nulls are written --
            ! each range its own bytes, which is what the byte-aligned split is for.
            dst%has_nulls = .true.
            call ensure_validity_cap(dst, m)
            nn = 0_int64
            !$omp parallel do default(shared) private(tix, k) reduction(+:nn) &
            !$omp     schedule(static) num_threads(nt) if (nt > 1)
            do tix = 1, nt
                do k = lo(tix), hi(tix)
                    if (sel_null(k)) then
                        call set_bit_null(dst, k)
                        nn = nn + 1_int64
                    end if
                end do
            end do
            !$omp end parallel do
            dst%n_null = nn
        end if
    end subroutine gather_build
    !
    !> `gather_build`'s single-cursor serial twin, kept for the measured reason `gather`'s always
    !! was: the phased form pays a whole extra pass over the offsets that the single cursor never
    !! touches, and is 1.5x slower on one thread (0.0140 s against 0.0094 s on 4 M elements). The
    !! two shapes must produce byte-identical columns; `test_string_parallel` holds them to that.
    subroutine gather_serial(dst, src, idx, valid, masked, nulls)
        type(parquet_string_column), intent(inout) :: dst !! the destination, already cleared.
        type(parquet_string_column), intent(in) :: src    !! the source.
        integer(int64), intent(in) :: idx(:)               !! source element per destination element, in range.
        logical, intent(in), optional :: valid(:)          !! the mask; read only when `masked`.
        logical, intent(in) :: masked                      !! `valid` is present and holds a .false.
        logical, intent(in) :: nulls                       !! the result carries a validity bitmap.
        integer(int64) :: m, k, a, b, elen, pos, want, nn
        logical, allocatable :: sel_null(:)
        m = size(idx, kind=int64)
        ! Both preparation passes are O(m), not O(size()): the selected elements' null flags have to
        ! be read before any buffer is written, and the payload is sized to what is selected.
        if (nulls) then
            allocate(sel_null(max(m, 1_int64)))
            do k = 1_int64, m
                sel_null(k) = .not. bit_valid(src, idx(k))
            end do
        end if
        want = 0_int64
        do k = 1_int64, m
            if (masked) then
                if (.not. valid(k)) then
                    sel_null(k) = .true.
                    cycle
                end if
            end if
            call elem_bounds(src, idx(k), a, b)
            want = want + (b - a + 1_int64)
        end do
        allocate(dst%offsets(m + 1_int64))
        dst%offsets(1) = 0_int64
        allocate(dst%data(max(want, 1_int64)))
        pos = 0_int64
        do k = 1_int64, m
            if (masked) then
                if (.not. valid(k)) then
                    dst%offsets(k+1_int64) = pos
                    cycle
                end if
            end if
            call elem_bounds(src, idx(k), a, b)
            elen = b - a + 1_int64
            if (elen > 0_int64) dst%data(pos+1_int64:pos+elen) = src%data(a:b)
            pos = pos + elen
            dst%offsets(k+1_int64) = pos
        end do
        dst%nrows = m
        dst%nchars = pos
        ! The null COUNT can change here -- an element may be dropped, or taken twice -- so it is
        ! recounted, where a permutation lets reindex_apply carry it over unchanged.
        if (nulls) then
            dst%has_nulls = .true.
            call ensure_validity_cap(dst, m)
            nn = 0_int64
            do k = 1_int64, m
                if (sel_null(k)) then
                    call set_bit_null(dst, k)
                    nn = nn + 1_int64
                end if
            end do
            dst%n_null = nn
        end if
    end subroutine gather_serial
    !
    !> int32 specific of append_nulls; see the append_nulls generic.
    subroutine parquet_string_column_append_nulls_i32(self, n)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: n                     !! number of null elements to append.
        call parquet_string_column_append_nulls_i64(self, int(n, int64))
    end subroutine parquet_string_column_append_nulls_i32
    !
    !> Binding form of `parquet_string_column_append_nulls_i32`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine append_nulls_i32(self, n)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: n                     !! number of null elements to append.
        call parquet_string_column_append_nulls_i32(self, n)
    end subroutine append_nulls_i32
    !
    !> int64 specific of append_nulls: appends `n` null (zero-width, invalid) elements in one bulk
    !! operation -- the counterpart of calling `append_null` n times, but with a single capacity
    !! growth instead of one per element. `n == 0` is a no-op; a negative `n` aborts.
    subroutine parquet_string_column_append_nulls_i64(self, n)
        type(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                     !! number of null elements to append.
        integer(int64) :: k
        if (n < 0_int64) error stop EP//"append_nulls: negative element count"
        if (n == 0_int64) return
        call ensure_offsets_cap(self, self%nrows + n)
        self%has_nulls = .true.
        call ensure_validity_cap(self, self%nrows + n)
        do k = 1_int64, n
            self%offsets(self%nrows+1_int64+k) = self%nchars
            call set_bit_null(self, self%nrows + k)
        end do
        self%n_null = self%n_null + n
        self%nrows = self%nrows + n
    end subroutine parquet_string_column_append_nulls_i64
    !
    !> Binding form of `parquet_string_column_append_nulls_i64`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine append_nulls_i64(self, n)
        class(parquet_string_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                     !! number of null elements to append.
        call parquet_string_column_append_nulls_i64(self, n)
    end subroutine append_nulls_i64
    !
    !> Strips leading and trailing blanks from every non-null element, in place (O(nchars)).
    subroutine strip_all(self)
        class(parquet_string_column), intent(inout) :: self !! the column.
        call compact_all(self, .true.)
    end subroutine strip_all
    !
    !> Trailing-trims every non-null element, in place (O(nchars)).
    subroutine trim_all(self)
        class(parquet_string_column), intent(inout) :: self !! the column.
        call compact_all(self, .false.)
    end subroutine trim_all
    !
    !> Returns the strip/trim bounds `lo:hi` of the payload range `a:b` (hi < lo when nothing is
    !! left). Works on `data` directly rather than on a `character(len=*)` the way `process_bounds`
    !! does, so a bulk pass needs no per-element string to hand it.
    subroutine payload_bounds(c, a, b, do_strip, lo, hi)
        type(parquet_string_column), intent(in) :: c !! the column.
        integer(int64), intent(in) :: a              !! first payload byte of the element.
        integer(int64), intent(in) :: b              !! last payload byte of the element.
        logical, intent(in) :: do_strip              !! strip both ends when .true., else trailing only.
        integer(int64), intent(out) :: lo            !! first byte to keep.
        integer(int64), intent(out) :: hi            !! last byte to keep; hi < lo when empty.
        lo = a
        hi = b
        if (do_strip) then
            do while (lo <= b)
                if (c%data(lo) /= ' ') exit
                lo = lo + 1_int64
            end do
        end if
        do while (hi >= lo)
            if (c%data(hi) /= ' ') exit
            hi = hi - 1_int64
        end do
    end subroutine payload_bounds
    !
    !> Shared worker for strip_all/trim_all. Rewrites the payload compactly, serially **in place**
    !! (elements only shrink, so no reallocation is needed) or, past the work floor, into fresh
    !! buffers across threads. When `do_strip` is .true. both ends are trimmed; otherwise only
    !! trailing blanks are removed.
    !!
    !! **The threaded form cannot compact in place, and that is not a concession -- it is what makes
    !! it fast twice over.** Thread `t` writes its output starting at a byte position at or *before*
    !! its own input range, so its writes reach back into a range another thread is still reading:
    !! in-place and parallel are incompatible here. Writing into a fresh buffer removes the race
    !! **and** removes the aliasing, which lets the copy be a section assignment (one `memcpy`)
    !! instead of the scalar byte loop the in-place path is stuck with -- see the serial path's own
    !! comment for what that loop costs and why it may not be "tidied up".
    subroutine compact_all(c, do_strip)
        type(parquet_string_column), intent(inout) :: c !! the column.
        logical, intent(in) :: do_strip                 !! strip both ends when .true., else trailing only.
        integer(int64) :: i, orig_a, orig_b, lo, hi, elen, total
        integer(int64), allocatable :: new_off(:), rlo(:), rhi(:)
        character(len=1), allocatable :: new_data(:)
        integer :: nt, tix
        nt = bulk_threads(c%nrows, c%nchars)
        if (nt <= 1) then
            call compact_all_serial(c, do_strip)
            return
        end if
        call thread_row_ranges(c%nrows, nt, rlo, rhi)
        allocate(new_off(c%nrows + 1_int64))
        new_off(1) = 0_int64
        ! Phase 1: each element's KEPT length. Reads only the blank runs at the two ends, not the
        ! whole element, so it is far cheaper than the copy it is sizing.
        !$omp parallel do default(shared) private(tix, i, orig_a, orig_b, lo, hi) &
        !$omp     schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do i = rlo(tix), rhi(tix)
                new_off(i+1_int64) = 0_int64
                if (bit_valid(c, i)) then
                    orig_a = c%offsets(i) + 1_int64
                    orig_b = c%offsets(i+1_int64)
                    if (orig_b >= orig_a) then
                        call payload_bounds(c, orig_a, orig_b, do_strip, lo, hi)
                        if (hi >= lo) new_off(i+1_int64) = hi - lo + 1_int64
                    end if
                end if
            end do
        end do
        !$omp end parallel do
        ! Phase 2: the scan. Serial, as everywhere else in this module.
        do i = 1_int64, c%nrows
            new_off(i+1_int64) = new_off(i+1_int64) + new_off(i)
        end do
        total = new_off(c%nrows + 1_int64)
        allocate(new_data(max(total, 1_int64)))
        ! Phase 3: the copy. Only the LEADING blank run is rescanned (and only when stripping) --
        ! the length comes from the scan, so the trailing scan is not repeated. The rescan is
        ! guaranteed to terminate inside the element, because a non-blank byte exists whenever
        ! `elen > 0`.
        !$omp parallel do default(shared) private(tix, i, orig_a, lo, elen) &
        !$omp     schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do i = rlo(tix), rhi(tix)
                elen = new_off(i+1_int64) - new_off(i)
                if (elen > 0_int64) then
                    lo = c%offsets(i) + 1_int64
                    if (do_strip) then
                        do while (c%data(lo) == ' ')
                            lo = lo + 1_int64
                        end do
                    end if
                    new_data(new_off(i)+1_int64:new_off(i)+elen) = c%data(lo:lo+elen-1_int64)
                end if
            end do
        end do
        !$omp end parallel do
        call move_alloc(new_off, c%offsets)
        call move_alloc(new_data, c%data)
        c%nchars = total
        ! Validity is untouched: compaction only shortens elements, so no row changes null state and
        ! `n_null` is unchanged. This is the one bulk rebuild in the module with no validity phase,
        ! and so the one with no byte-alignment requirement.
    end subroutine compact_all
    !
    !> `compact_all`'s serial, in-place form. See `compact_all` for why the threaded one cannot be
    !! in place and why this one's copy must stay a scalar loop.
    subroutine compact_all_serial(c, do_strip)
        type(parquet_string_column), intent(inout) :: c !! the column.
        logical, intent(in) :: do_strip                 !! strip both ends when .true., else trailing only.
        integer(int64) :: i, orig_a, orig_b, prev_end, lo, hi, wpos, k
        wpos = 0_int64
        prev_end = 0_int64   ! original offsets(1) is always 0
        do i = 1_int64, c%nrows
            orig_a = prev_end + 1_int64        ! original start (offsets(i) already overwritten below)
            orig_b = c%offsets(i+1)            ! original end (not overwritten until end of this iteration)
            prev_end = orig_b                  ! save before the overwrite
            if (bit_valid(c, i) .and. orig_b >= orig_a) then
                ! The same helper the threaded path uses, so the two cannot drift on what "trim"
                ! means -- which the equality test between them would then be unable to detect.
                call payload_bounds(c, orig_a, orig_b, do_strip, lo, hi)
                ! **A scalar byte loop, deliberately, and it must stay one.** The obvious tidy-up --
                ! `c%data(wpos+1:wpos+n) = c%data(lo:hi)` -- is a section assignment whose two sides
                ! are the SAME array, so the compiler cannot prove they do not overlap and must
                ! materialise the right-hand side first: one heap temporary per element, which is
                ! exactly the shape to avoid. Measured at **5.5x SLOWER** (0.0261 s to 0.1439 s over
                ! 4 M elements). The threaded path can use a section assignment only because its
                ! destination is a different array.
                do k = lo, hi
                    wpos = wpos + 1_int64
                    c%data(wpos) = c%data(k)
                end do
            end if
            c%offsets(i+1) = wpos
        end do
        c%nchars = wpos
    end subroutine compact_all_serial
    !
    ! ==================================================================================
    ! Searching / comparison
    ! ==================================================================================
    !
    !> Returns the 1-based index of the first element equal to `str`, or 0 if none. By default
    !! the comparison is byte-exact; pass `exact=.false.` to trailing-trim both sides. Pass
    !! `reverse=.true.` to scan from the last row backward (returning the last match). Null
    !! elements never match.
    integer(int64) function find(self, str, exact, reverse) result(idx)
        class(parquet_string_column), intent(in) :: self !! the column.
        character(len=*), intent(in) :: str              !! the query string.
        logical, intent(in), optional :: exact            !! .false. => trailing-trim both sides.
        logical, intent(in), optional :: reverse          !! .true. => search from the back.
        logical :: do_exact, do_rev
        integer(int64) :: i
        do_exact = .true.
        if (present(exact)) do_exact = exact
        do_rev = .false.
        if (present(reverse)) do_rev = reverse
        idx = 0_int64
        if (do_rev) then
            do i = self%nrows, 1_int64, -1_int64
                if (bit_valid(self, i)) then
                    if (elem_equals(self, i, str, do_exact)) then
                        idx = i
                        return
                    end if
                end if
            end do
        else
            do i = 1_int64, self%nrows
                if (bit_valid(self, i)) then
                    if (elem_equals(self, i, str, do_exact)) then
                        idx = i
                        return
                    end if
                end if
            end do
        end if
    end function find
    !
    !> int32 specific of contains; see the contains generic.
    logical function contains_i32(self, i, str, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(in) :: str              !! substring to search for.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        res = self%contains_i64(int(i, int64), str, check_null)
    end function contains_i32
    !
    !> int64 specific of contains: whether element i contains `str` as a substring (exact bytes,
    !! empty substring matches). A null element returns .false. by default, or error stops when
    !! `check_null` is .true.
    logical function contains_i64(self, i, str, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(in) :: str              !! substring to search for.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        integer(int64) :: a, b, elen, start, k
        integer :: m
        logical :: match
        call check_index(self, i, "contains")
        if (guard_null_false(self, i, check_null, "contains")) then
            res = .false.
            return
        end if
        m = len(str)
        if (m == 0) then
            res = .true.
            return
        end if
        call elem_bounds(self, i, a, b)
        elen = b - a + 1_int64
        if (elen < int(m, int64)) then
            res = .false.
            return
        end if
        do start = 0_int64, elen - int(m, int64)
            match = .true.
            do k = 1_int64, int(m, int64)
                if (self%data(a+start+k-1_int64) /= str(k:k)) then
                    match = .false.
                    exit
                end if
            end do
            if (match) then
                res = .true.
                return
            end if
        end do
        res = .false.
    end function contains_i64
    !
    !> int32 specific of startswith; see the startswith generic.
    logical function startswith_i32(self, i, prefix, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(in) :: prefix           !! prefix to test.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        res = self%startswith_i64(int(i, int64), prefix, check_null)
    end function startswith_i32
    !
    !> int64 specific of startswith: whether element i begins with `prefix` (exact bytes, empty
    !! prefix matches). A null element returns .false. by default, or error stops when
    !! `check_null` is .true.
    logical function startswith_i64(self, i, prefix, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(in) :: prefix           !! prefix to test.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        integer(int64) :: a, b, elen, k
        integer :: p
        call check_index(self, i, "startswith")
        if (guard_null_false(self, i, check_null, "startswith")) then
            res = .false.
            return
        end if
        p = len(prefix)
        if (p == 0) then
            res = .true.
            return
        end if
        call elem_bounds(self, i, a, b)
        elen = b - a + 1_int64
        if (elen < int(p, int64)) then
            res = .false.
            return
        end if
        do k = 1_int64, int(p, int64)
            if (self%data(a+k-1_int64) /= prefix(k:k)) then
                res = .false.
                return
            end if
        end do
        res = .true.
    end function startswith_i64
    !
    !> int32 specific of endswith; see the endswith generic.
    logical function endswith_i32(self, i, suffix, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(in) :: suffix           !! suffix to test.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        res = self%endswith_i64(int(i, int64), suffix, check_null)
    end function endswith_i32
    !
    !> int64 specific of endswith: whether element i ends with `suffix` (exact bytes, empty suffix
    !! matches). A null element returns .false. by default, or error stops when `check_null` is .true.
    logical function endswith_i64(self, i, suffix, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(in) :: suffix           !! suffix to test.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        integer(int64) :: a, b, elen, k
        integer :: s
        call check_index(self, i, "endswith")
        if (guard_null_false(self, i, check_null, "endswith")) then
            res = .false.
            return
        end if
        s = len(suffix)
        if (s == 0) then
            res = .true.
            return
        end if
        call elem_bounds(self, i, a, b)
        elen = b - a + 1_int64
        if (elen < int(s, int64)) then
            res = .false.
            return
        end if
        do k = 1_int64, int(s, int64)
            if (self%data(b-int(s, int64)+k) /= suffix(k:k)) then
                res = .false.
                return
            end if
        end do
        res = .true.
    end function endswith_i64
    !
    !> int32 specific of equals; see the equals generic.
    logical function equals_i32(self, i, str, exact, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(in) :: str              !! query string.
        logical, intent(in), optional :: exact            !! .false. => trailing-trim both sides.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        res = self%equals_i64(int(i, int64), str, exact, check_null)
    end function equals_i32
    !
    !> int64 specific of equals: whether element i equals `str`. By default the comparison is
    !! byte-exact; pass `exact=.false.` to trailing-trim both sides. A null element returns .false.
    !! by default, or error stops when `check_null` is .true.
    logical function equals_i64(self, i, str, exact, check_null) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based element index.
        character(len=*), intent(in) :: str              !! query string.
        logical, intent(in), optional :: exact            !! .false. => trailing-trim both sides.
        logical, intent(in), optional :: check_null       !! .true. => error stop on a null element.
        logical :: do_exact
        call check_index(self, i, "equals")
        if (guard_null_false(self, i, check_null, "equals")) then
            res = .false.
            return
        end if
        do_exact = .true.
        if (present(exact)) do_exact = exact
        res = elem_equals(self, i, str, do_exact)
    end function equals_i64
    !
    !> Shared null guard for the logical comparison ops: returns .true. (meaning "the caller should
    !! return .false.") for a null element, honouring `check_null` (error stop when present & true).
    logical function guard_null_false(c, i, check_null, proc) result(is_null_row)
        type(parquet_string_column), intent(in) :: c !! the column.
        integer(int64), intent(in) :: i              !! 1-based element index.
        logical, intent(in), optional :: check_null   !! .true. => error stop on a null element.
        character(len=*), intent(in) :: proc          !! calling procedure name.
        is_null_row = .not. bit_valid(c, i)
        if (is_null_row .and. present(check_null)) then
            if (check_null) call fail_null(proc)
        end if
    end function guard_null_false
    !
    ! ==================================================================================
    ! Conversion / ownership
    ! ==================================================================================
    !
    !> Orders element `i` against element `j`: -1 when i sorts first, +1 when j does, 0 when equal.
    !!
    !! **Exactly Fortran's own `<` on the two values**, blanks and all: the shorter element is
    !! compared as though padded with blanks, so `"ab"` and `"ab  "` are equal and `"ab"` sorts before
    !! `"abc"`. That is deliberate rather than incidental — it means a caller can replace
    !! `call c%get(i, a); call c%get(j, b); if (a < b) ...` with this and get the same answer.
    !!
    !! **Why this exists: so that a min/max scan need not materialize every element.** Finding the
    !! smallest and largest value by fetching each one through `%get` costs a heap allocation per row
    !! — roughly 0.11 s per 4 M elements — where tracking the two winning *indices* through this and
    !! fetching only those two at the end costs none.
    !!
    !! **Null elements are not special-cased.** A null is zero-width, so it compares as an empty
    !! string and sorts first; a caller that needs nulls ordered differently must test `%is_null`
    !! itself, exactly as it would around `%get`.
    integer function compare(self, i, j) result(res)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based index of the first element.
        integer(int64), intent(in) :: j                  !! 1-based index of the second element.
        integer(int64) :: ai, bi, aj, bj
        call check_index(self, i, "compare")
        call check_index(self, j, "compare")
        ! No payload at all means every element is empty, and two empties are equal.
        res = 0
        if (.not. allocated(self%data)) return
        call elem_bounds(self, i, ai, bi)
        call elem_bounds(self, j, aj, bj)
        res = payload_compare(self%data, ai, bi, aj, bj)
    end function compare
    !
    !> Orders the payload bytes `data(ai:bi)` against `data(aj:bj)` exactly as `compare` orders
    !! two elements: -1, 0 or +1, the shorter operand compared as though blank-padded. This is
    !! `compare`'s body with the two bounds already in hand, so that a scan carrying its
    !! candidates' bounds across the loop (`parquet_string_column_argminmax`) pays one byte
    !! comparison per element and neither the index checks nor the bounds lookups.
    pure integer function payload_compare(data, ai, bi, aj, bj) result(res)
        character(len=1), intent(in) :: data(:) !! the packed payload.
        integer(int64), intent(in) :: ai        !! first operand's first byte.
        integer(int64), intent(in) :: bi        !! first operand's last byte (bi < ai when empty).
        integer(int64), intent(in) :: aj        !! second operand's first byte.
        integer(int64), intent(in) :: bj        !! second operand's last byte (bj < aj when empty).
        integer(int64) :: li, lj, k, common
        character(len=1) :: ci, cj
        li = bi - ai + 1_int64
        lj = bj - aj + 1_int64
        common = min(li, lj)
        res = 0
        do k = 1_int64, common
            ci = data(ai + k - 1_int64)
            cj = data(aj + k - 1_int64)
            if (ci /= cj) then
                if (ci < cj) then
                    res = -1
                else
                    res = 1
                end if
                return
            end if
        end do
        ! Equal over the common prefix: the longer element's remaining bytes are compared against
        ! blanks, which is what Fortran's own padding rule does.
        if (li > lj) then
            do k = common + 1_int64, li
                if (data(ai + k - 1_int64) /= " ") then
                    if (data(ai + k - 1_int64) < " ") then
                        res = -1
                    else
                        res = 1
                    end if
                    return
                end if
            end do
        else if (lj > li) then
            do k = common + 1_int64, lj
                if (data(aj + k - 1_int64) /= " ") then
                    if (" " < data(aj + k - 1_int64)) then
                        res = -1
                    else
                        res = 1
                    end if
                    return
                end if
            end do
        end if
    end function payload_compare
    !
    !> Typed form of `%argminmax`: the indices of the lexicographically smallest and largest
    !! non-null elements, or `0` for both when the column holds no non-null element.
    !!
    !! **The ordering is exactly `%compare`'s** -- Fortran's own `<` on the two values, the shorter
    !! one blank-padded -- and on a tie the EARLIER index is kept, so the answer is the one a scan
    !! keeping a running winner through `%compare` gives. What this buys over that scan is the
    !! per-element cost: the two candidates' payload bounds are carried across the loop, so an
    !! element costs one byte comparison against each and no index check, where the scan paid two
    !! `%compare` calls, each re-validating both indices and re-reading both offset pairs. A null
    !! element is skipped, as that scan skips it; nothing is materialized.
    subroutine parquet_string_column_argminmax(self, imin, imax)
        type(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(out) :: imin              !! index of the smallest element, or 0.
        integer(int64), intent(out) :: imax              !! index of the largest element, or 0.
        integer(int64) :: i, a, b, amin, bmin, amax, bmax
        imin = 0_int64
        imax = 0_int64
        amin = 0_int64
        bmin = -1_int64
        amax = 0_int64
        bmax = -1_int64
        do i = 1_int64, self%nrows
            if (.not. bit_valid(self, i)) cycle
            ! No payload at all means every element is empty, and the first non-null one wins
            ! both places outright: there is nothing to compare.
            if (.not. allocated(self%data)) then
                imin = i
                imax = i
                return
            end if
            a = self%offsets(i) + 1_int64
            b = self%offsets(i + 1_int64)
            if (imin == 0_int64) then
                imin = i
                imax = i
                amin = a
                bmin = b
                amax = a
                bmax = b
            else
                if (payload_compare(self%data, a, b, amin, bmin) < 0) then
                    imin = i
                    amin = a
                    bmin = b
                end if
                if (payload_compare(self%data, a, b, amax, bmax) > 0) then
                    imax = i
                    amax = a
                    bmax = b
                end if
            end if
        end do
    end subroutine parquet_string_column_argminmax
    !
    !> Binding form of `parquet_string_column_argminmax`; forwards to it,
    !! keeping the implementation at the `type` end.
    subroutine argminmax(self, imin, imax)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(out) :: imin              !! index of the smallest element, or 0.
        integer(int64), intent(out) :: imax              !! index of the largest element, or 0.
        call parquet_string_column_argminmax(self, imin, imax)
    end subroutine argminmax
    !
    !> Materializes the whole column into a conventional Fortran character array `out`, each
    !! element blank-padded to the longest element's length. A null element error stops by default;
    !! pass `null_value` to substitute a string for nulls.
    !!
    !! **The payload is copied straight out of `data`, never through `%get`.** `%get` allocates a
    !! deferred-length temporary per element, and a whole-column loop over it costs one heap
    !! allocation, one fill, one copy into `out(i)` and one free **per row** -- which measured as
    !! 71 % of this procedure, not the copying it was there to do. Do not reintroduce a
    !! `character(len=:), allocatable` intermediate here, or in any other bulk operation over this
    !! type.
    !!
    !! **The fill loop threads with no restructure**, unlike `reindex_apply`: `out(i)` is `maxlen`
    !! bytes at a fixed stride, so every element's destination is known from `i` alone and there is
    !! no write cursor to carry. There is no serial twin here for that reason -- with `nt == 1` this
    !! *is* the original loop, so nothing is slower for a caller that does not thread.
    subroutine to_character(self, out, null_value)
        class(parquet_string_column), intent(in) :: self          !! the column.
        character(len=:), allocatable, intent(out) :: out(:)      !! materialized, padded strings.
        character(len=*), intent(in), optional :: null_value      !! substitute for null elements.
        integer(int64) :: i, elen, maxlen, a, b
        integer(int64), allocatable :: lo(:), hi(:)
        integer :: nt, tix
        logical :: any_null
        maxlen = 0_int64
        if (present(null_value)) maxlen = int(len(null_value), int64)
        ! First pass sizes the result AND is where a null aborts -- both before `out` is allocated,
        ! so a column that cannot be materialized never allocates the array it would have gone into.
        !
        ! It reads 8 bytes per row against the fill loop's `maxlen`, so it is sized on its own terms.
        ! **The abort is deferred until after the region rather than raised inside it**: `error stop`
        ! from inside an OpenMP region is not somewhere to be adventurous, and it buys nothing here
        ! because `fail_null`'s message names no row -- whichever null a thread saw first, the
        ! message is identical, so there is no nondeterminism to protect against.
        nt = bulk_threads(self%nrows, self%nrows*8_int64)
        call thread_row_ranges(self%nrows, nt, lo, hi)
        any_null = .false.
        !$omp parallel do default(shared) private(tix, i, elen) reduction(max:maxlen) &
        !$omp     reduction(.or.:any_null) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do i = lo(tix), hi(tix)
                if (bit_valid(self, i)) then
                    elen = self%offsets(i+1) - self%offsets(i)
                    if (elen > maxlen) maxlen = elen
                else
                    any_null = .true.
                end if
            end do
        end do
        !$omp end parallel do
        if (any_null .and. .not. present(null_value)) call fail_null("to_character")
        allocate(character(len=maxlen) :: out(self%nrows))
        ! The work measure is what this loop WRITES (`nrows * maxlen`), not the packed payload: a
        ! column of short strings padded to a long `maxlen` moves several times its own
        ! `character_size()` here, and asking about the payload would decline to thread exactly the
        ! case that most needs it.
        nt = bulk_threads(self%nrows, self%nrows*maxlen)
        call thread_row_ranges(self%nrows, nt, lo, hi)   ! re-split: this loop's count may differ
        ! No validity is WRITTEN here -- `out` is a plain character array and the bitmap is only read
        ! -- so any split would be safe. The byte-aligned one is reused because it is already
        ! written, tested, and gives contiguous ranges, which is what keeps two threads off one cache
        ! line at the boundary.
        !$omp parallel do default(shared) private(tix, i, a, b, elen) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            do i = lo(tix), hi(tix)
                if (.not. bit_valid(self, i)) then
                    out(i) = null_value
                else
                    call elem_bounds(self, i, a, b)
                    elen = b - a + 1_int64
                    ! Left-justified and blank-padded to maxlen, exactly as the whole-element
                    ! assignment this replaced was: the payload bytes first, then the padding, which
                    ! together write each element's maxlen bytes exactly once.
                    if (elen > 0_int64) out(i)(1:elen) = transfer(self%data(a:b), out(i)(1:elen))
                    if (elen < maxlen) out(i)(elen+1_int64:) = ""
                end if
            end do
        end do
        !$omp end parallel do
    end subroutine to_character
    !
    !> Returns an independent deep copy of the column (shrunk to the current size). Mutating either
    !! object never affects the other.
    function clone(self) result(res)
        class(parquet_string_column), intent(in) :: self !! the source column.
        type(parquet_string_column) :: res               !! the deep copy.
        integer(int64) :: nb
        res%nrows = self%nrows
        res%nchars = self%nchars
        res%n_null = self%n_null
        res%has_nulls = self%has_nulls
        if (self%nrows > 0 .and. allocated(self%offsets)) then
            allocate(res%offsets(self%nrows+1))
            res%offsets(:) = self%offsets(1:self%nrows+1)
        end if
        if (self%nchars > 0 .and. allocated(self%data)) then
            allocate(res%data(self%nchars))
            res%data(:) = self%data(1:self%nchars)
        end if
        if (self%has_nulls .and. allocated(self%validity)) then
            nb = (self%nrows + 7_int64)/8_int64
            if (nb >= 1) then
                allocate(res%validity(nb))
                res%validity(:) = self%validity(1:nb)
            end if
        end if
    end function clone
    !
    !> int32 specific of slice; see the slice generic.
    subroutine slice_i32(self, first, last, dest)
        class(parquet_string_column), intent(in) :: self  !! the source column.
        integer(int32), intent(in) :: first                !! first row of the range (1-based, inclusive).
        integer(int32), intent(in) :: last                 !! last row of the range (1-based, inclusive).
        type(parquet_string_column), intent(inout) :: dest !! cleared, then filled with rows [first, last].
        call self%slice_i64(int(first, int64), int(last, int64), dest)
    end subroutine slice_i32
    !
    !> int64 specific of slice: materializes an independent, owning copy of rows [first, last]
    !! (1-based, inclusive) into dest -- one bulk data memcpy, one vectorized offset-rebase loop,
    !! and one validity-bitmap slice, same overall shape as append_column but scoped to the range.
    !! dest is cleared first; self is left unchanged.
    subroutine slice_i64(self, first, last, dest)
        class(parquet_string_column), intent(in) :: self  !! the source column.
        integer(int64), intent(in) :: first                !! first row of the range (1-based, inclusive).
        integer(int64), intent(in) :: last                 !! last row of the range (1-based, inclusive).
        type(parquet_string_column), intent(inout) :: dest !! cleared, then filled with rows [first, last].
        integer(int64) :: n, base, k, a, b
        call check_range(self, first, last, "slice")
        n = last - first + 1_int64
        call parquet_string_column_clear(dest)
        call ensure_offsets_cap(dest, n)
        base = self%offsets(first)
        a = base + 1_int64
        b = self%offsets(last+1_int64)
        if (b >= a) then
            call ensure_data_cap(dest, b - a + 1_int64)
            dest%data(1_int64 : b-a+1_int64) = self%data(a:b)
        end if
        do k = 1_int64, n
            dest%offsets(k+1_int64) = self%offsets(first+k) - base
        end do
        if (self%has_nulls) then
            dest%has_nulls = .true.
            call ensure_validity_cap(dest, n)
            ! `dest` was cleared, so its count starts at zero. The run moves in whole bytes when
            ! `first` is 8-aligned and bit by bit otherwise; see `copy_validity_run`.
            call copy_validity_run_cols(self, first, dest, 1_int64, n, dest%n_null)
        end if
        dest%nrows = n
        dest%nchars = b - base
    end subroutine slice_i64
    !
    !> Transfers all buffers from `other` into self, leaving `other` a valid empty column. Self's
    !! previous contents are released. Self-move (move_from with the same object) is a no-op.
    subroutine parquet_string_column_move_from(self, other)
        type(parquet_string_column), intent(inout) :: self  !! the destination column.
        type(parquet_string_column), intent(inout) :: other !! the source column (left empty).
        type(parquet_string_column) :: tmp
        call swap_impl(other, tmp)   ! other -> tmp, other emptied
        call swap_impl(self, tmp)    ! self <-> tmp: self gets other's data, tmp gets self's old
        ! tmp (self's former data) is discarded on return
    end subroutine parquet_string_column_move_from
    !
    !> Binding form of `parquet_string_column_move_from`; forwards to it, keeping the
    !! implementation at the `type` end.
    subroutine move_from(self, other)
        class(parquet_string_column), intent(inout) :: self !! the destination column.
        type(parquet_string_column), intent(inout) :: other !! the source column (left empty).
        call parquet_string_column_move_from(self, other)
    end subroutine move_from
    !
    !> Exchanges the contents of self and `other` in O(1). Symmetric: a%swap(b) == b%swap(a).
    subroutine swap(self, other)
        class(parquet_string_column), intent(inout) :: self !! the first column.
        type(parquet_string_column), intent(inout) :: other !! the second column.
        call swap_impl(self, other)
    end subroutine swap
    !
    ! ==================================================================================
    ! Diagnostics
    ! ==================================================================================
    !
    !> Writes a human-readable representation of the column, showing each element (or <null>) up
    !! to `max_rows` (default 20).
    !!
    !! Solicited output: `verbosity = "silent"` returns having written nothing. With no `unit=` the
    !! text goes wherever `message_stream` names, as everything this library writes does.
    subroutine col_print(self, unit, max_rows)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer, intent(in), optional :: unit            !! where to write; default the `message_stream` setting's unit.
        integer(int64), intent(in), optional :: max_rows  !! max elements to print (default 20).
        integer :: u
        integer(int64) :: i, lim, a, b, j
        if (parquet_output_is_suppressed()) return
        u = parquet_message_unit()
        if (present(unit)) u = unit
        lim = 20_int64
        if (present(max_rows)) lim = max_rows
        write(u, '(a,i0,a,i0,a,i0,a)') EP//"column with ", self%nrows, " rows, ", &
            self%nchars, " chars, ", self%n_null, " nulls"
        do i = 1_int64, min(self%nrows, lim)
            if (.not. bit_valid(self, i)) then
                write(u, '(2x,i0,a)') i, ": <null>"
            else
                ! Written straight from the payload slice. The saving is irrelevant here -- this
                ! loop is bounded by `lim` and dominated by the formatted write -- but it keeps
                ! the module free of the shape entirely, so the rule reads as absolute rather than
                ! "except where it does not matter", and a static check over this file needs no
                ! exemption.
                ! The payload goes out through an implied-do rather than as the section
                ! `self%data(a:b)`. Identical output list, identical output -- but ifx builds an
                ! array temporary for a SECTION of an allocatable component used as an I/O list
                ! item (its descriptor is not known contiguous at the write), and reports one
                ! `forrtl: warning (406)` per line printed under the debug profile's
                ! `-check arg_temp_created`. An implied-do passes each byte as a scalar and is
                ! silent. Measured both ways with ifx 2026.1.1.
                call elem_bounds(self, i, a, b)
                if (b >= a) then
                    ! The payload is one list item PER BYTE, so the format needs an unlimited
                    ! repeat rather than a fixed count of `a` descriptors: with `(2x,i0,a,a,a)`
                    ! anything longer than two characters exhausted the format, reverted to the
                    ! start of it, and hit a character against `i0` -- an I/O runtime error, on
                    ! every element of every column whose strings are longer than two bytes.
                    write(u, '(2x,i0,*(a))') i, ': "', (self%data(j), j = a, b), '"'
                else
                    ! A column whose elements are ALL zero-length never allocates `data` at all
                    ! (`ensure_data_cap` skips a zero request), so the slice above would reference
                    ! an unallocated array. gfortran happens to tolerate a zero-length section of
                    ! one; the standard does not, and CLAUDE.md records ifx following the standard
                    ! where gfortran hides it.
                    write(u, '(2x,i0,a)') i, ': ""'
                end if
            end if
        end do
        if (self%nrows > lim) write(u, '(2x,a,i0,a)') "... (", self%nrows - lim, " more)"
    end subroutine col_print
    !
    !> Writes a compact one-line overview string (row/char/null counts and capacities) into `res`.
    subroutine summary(self, res)
        class(parquet_string_column), intent(in) :: self !! the column.
        character(len=:), allocatable, intent(out) :: res !! the summary string.
        character(len=256) :: buf
        write(buf, '(a,i0,a,i0,a,i0,a,i0,a,i0,a)') "parquet_string_column(rows=", self%nrows, &
            ", chars=", self%nchars, ", nulls=", self%n_null, ", row_cap=", self%capacity(), &
            ", char_cap=", self%character_capacity(), ")"
        res = trim(buf)
    end subroutine summary
    !
    !> Returns detailed metrics via optional intent(out) arguments (kept basic; extensible later).
    subroutine statistics(self, nrows, nchars, n_null, min_len, max_len, row_capacity, &
            char_capacity, bytes)
        class(parquet_string_column), intent(in) :: self       !! the column.
        integer(int64), intent(out), optional :: nrows          !! number of elements.
        integer(int64), intent(out), optional :: nchars         !! total characters.
        integer(int64), intent(out), optional :: n_null         !! number of nulls.
        integer(int64), intent(out), optional :: min_len        !! shortest non-null element length.
        integer(int64), intent(out), optional :: max_len        !! longest non-null element length.
        integer(int64), intent(out), optional :: row_capacity   !! current row capacity.
        integer(int64), intent(out), optional :: char_capacity  !! current character capacity.
        integer(int64), intent(out), optional :: bytes          !! total allocated bytes.
        integer(int64) :: i, elen, lo, hi
        integer(int64), allocatable :: rlo(:), rhi(:)
        integer :: nt, tix
        if (present(nrows)) nrows = self%nrows
        if (present(nchars)) nchars = self%nchars
        if (present(n_null)) n_null = self%n_null
        if (present(row_capacity)) row_capacity = self%capacity()
        if (present(char_capacity)) char_capacity = self%character_capacity()
        if (present(bytes)) bytes = self%memory_usage()
        if (present(min_len) .or. present(max_len)) then
            ! `hi = -1` is the "no valid element seen" sentinel, which is what lets this be two
            ! plain reductions: a length is never negative, so a surviving -1 means the column had
            ! nothing to measure and both answers are 0 -- the same result the `any_valid` flag this
            ! replaced produced, but without a first-iteration special case a reduction cannot
            ! express.
            lo = huge(0_int64)
            hi = -1_int64
            nt = bulk_threads(self%nrows, self%nrows*8_int64)
            call thread_row_ranges(self%nrows, nt, rlo, rhi)
            !$omp parallel do default(shared) private(tix, i, elen) reduction(min:lo) &
            !$omp     reduction(max:hi) schedule(static) num_threads(nt) if (nt > 1)
            do tix = 1, nt
                do i = rlo(tix), rhi(tix)
                    if (bit_valid(self, i)) then
                        elen = self%offsets(i+1) - self%offsets(i)
                        if (elen < lo) lo = elen
                        if (elen > hi) hi = elen
                    end if
                end do
            end do
            !$omp end parallel do
            if (hi < 0_int64) then
                lo = 0_int64
                hi = 0_int64
            end if
            if (present(min_len)) min_len = lo
            if (present(max_len)) max_len = hi
        end if
    end subroutine statistics
    !
    ! ==================================================================================
    ! Interop hooks (advanced; consumed by the Parquet read/write integration layer)
    ! ==================================================================================
    !
!> Copies this column's offsets and packed payload into caller-provided arrays.
    !!
    !! The **safe, allocation-free counterpart to `raw_buffers`**: same two buffers, copied rather
    !! than pointed at. Use this when the destination has to own its bytes anyway — `raw_buffers`
    !! hands back `c_loc` pointers whose validity depends on the *actual* argument carrying the
    !! `TARGET` attribute all the way up the call chain, which is a precondition a caller several
    !! frames away cannot check.
    !!
    !! **This exists so that bulk consumers never walk the column element by element.** Materializing
    !! each element through `%get` costs one heap allocation, one fill, one copy and one free per row;
    !! at 4 M elements that measured as roughly 0.11 s per allocation, and as 19-33 % of an entire
    !! string sort in the consumer this was added for. The layout handed back is exactly the layout a
    !! packed consumer wants, so the copy is two `memcpy`s rather than a loop.
    !!
    !! `offsets` must hold at least `size()+1` entries and `data` at least `character_size()` bytes;
    !! both abort otherwise rather than truncating. Only those leading portions are written.
    !!
    !! **A null element is zero-width here, exactly as it is in the column** — `set_null` compacts the
    !! payload — so a consumer that ignores validity gets an empty string for a null, which is what
    !! `%get(..., allow_null=.true.)` would have given it.
    subroutine copy_buffers(self, offsets, data)
        class(parquet_string_column), intent(in) :: self !! the column.
        integer(int64), intent(out) :: offsets(:)        !! receives size()+1 offsets; offsets(1) = 0.
        character(len=1), intent(out) :: data(:)         !! receives character_size() payload bytes.
        if (size(offsets, kind=int64) < self%nrows + 1_int64) then
            error stop EP//"copy_buffers: the offsets array is shorter than size()+1"
        end if
        if (size(data, kind=int64) < self%nchars) then
            error stop EP//"copy_buffers: the data array is shorter than character_size()"
        end if
        if (self%nrows >= 0_int64 .and. allocated(self%offsets)) then
            offsets(1:self%nrows+1_int64) = self%offsets(1:self%nrows+1_int64)
        else
            ! An empty column may never have allocated its offsets at all.
            if (size(offsets, kind=int64) >= 1_int64) offsets(1) = 0_int64
        end if
        if (self%nchars > 0_int64) data(1:self%nchars) = self%data(1:self%nchars)
    end subroutine copy_buffers
    !
        !> Exports c_loc pointers to the internal offsets/data/validity buffers plus counts, for the
    !! Parquet writer to consume without materializing strings. The returned pointers are valid
    !! only until the next mutation of the column. `validity_ptr` is C_NULL_PTR when the column has
    !! no nulls; `data_ptr` is C_NULL_PTR when the payload is empty.
    !!
    !! (Coverage note: this header line never registers as "hit" in gcov although every line of the
    !! body does -- the same attribution artifact documented above `date_parse` in
    !! `src/parquet_temporal.f90`. The marker sits after the continuation ampersand because the
    !! statement plus a full marker comment would pass 132 columns. Excluded as an artifact, not
    !! as a gap.)
    subroutine parquet_string_column_raw_buffers(self, offsets_ptr, data_ptr, validity_ptr, nrows, nchars, & ! GCOVR_EXCL_LINE
        has_validity)
        type(parquet_string_column), intent(in), target :: self  !! the column (must be a target).
        type(c_ptr), intent(out) :: offsets_ptr                  !! -> int64 offsets(0:nrows).
        type(c_ptr), intent(out) :: data_ptr                     !! -> nchars payload bytes.
        type(c_ptr), intent(out) :: validity_ptr                 !! -> validity bitmap, or C_NULL_PTR.
        integer(int64), intent(out) :: nrows                     !! number of elements.
        integer(int64), intent(out) :: nchars                    !! total characters.
        logical, intent(out) :: has_validity                     !! whether a validity bitmap exists.
        nrows = self%nrows
        nchars = self%nchars
        has_validity = self%has_nulls
        offsets_ptr = c_null_ptr
        if (allocated(self%offsets)) then
            if (size(self%offsets, kind=int64) >= 1) offsets_ptr = c_loc(self%offsets)
        end if
        if (allocated(self%data) .and. self%nchars > 0) then
            data_ptr = c_loc(self%data)
        else
            data_ptr = c_null_ptr
        end if
        validity_ptr = c_null_ptr
        if (self%has_nulls .and. allocated(self%validity)) then
            if (size(self%validity, kind=int64) >= 1) validity_ptr = c_loc(self%validity)
        end if
    end subroutine parquet_string_column_raw_buffers
    !
    !> Binding form of `parquet_string_column_raw_buffers`; forwards to it, keeping the
    !! implementation at the `type` end.
    subroutine raw_buffers(self, offsets_ptr, data_ptr, validity_ptr, nrows, nchars, has_validity)
        class(parquet_string_column), intent(in), target :: self !! the column (must be a target).
        type(c_ptr), intent(out) :: offsets_ptr                  !! -> int64 offsets(0:nrows).
        type(c_ptr), intent(out) :: data_ptr                     !! -> nchars payload bytes.
        type(c_ptr), intent(out) :: validity_ptr                 !! -> validity bitmap, or C_NULL_PTR.
        integer(int64), intent(out) :: nrows                     !! number of elements.
        integer(int64), intent(out) :: nchars                    !! total characters.
        logical, intent(out) :: has_validity                     !! whether a validity bitmap exists.
        call parquet_string_column_raw_buffers(self, offsets_ptr, data_ptr, validity_ptr, nrows, nchars, &
            has_validity)
    end subroutine raw_buffers
    !
    !> Bulk-appends one row group straight from C buffers: `nrows_in` elements with a packed
    !! `nchars_in`-byte payload, an offsets buffer (int32 when `offsets_int32` is .true., else
    !! int64; length nrows_in+1, 0-based), and an optional Arrow validity bitmap (`validity` =
    !! C_NULL_PTR means all valid). Offsets are rebased onto the existing payload (int32 widened to
    !! int64 in the same pass); the validity bit region is merged (handling a non-byte-aligned join).
    !!
    !! **Precondition: `offsets` must already be rebased to this chunk, i.e. its first entry
    !! (`offsets(1)` in the Fortran 1-based view of the C array) must be exactly 0, and `data`
    !! must point at the first payload byte that first entry refers to.** A source sliced out of
    !! a larger buffer -- e.g. an Arrow array with a non-zero `offset()` -- is not rebased by
    !! construction and must be rebased by the caller (subtract the slice's own starting offset
    !! from every offsets entry, and advance `data` by that same amount) before calling this;
    !! passing an un-rebased `offsets` aborts immediately rather than silently misplacing every
    !! element's bytes. `validity`, when not C_NULL_PTR, is addressed starting from bit
    !! `validity_offset_bits` (default 0) rather than assumed to already start at bit 0 -- unlike
    !! `offsets`/`data`, Arrow never pre-rebases a validity bitmap for a sliced source array, so a
    !! nonzero source `offset()` (e.g. a struct-nested leaf resolved through a sliced child array)
    !! must be passed through here explicitly; that misalignment cannot be detected from a raw
    !! bitmap pointer alone; the caller is responsible for reporting the correct offset.
    subroutine append_buffers(self, nrows_in, nchars_in, offsets, data, validity, offsets_int32, validity_offset_bits)
        class(parquet_string_column), intent(inout) :: self !! the destination column.
        integer(int64), intent(in) :: nrows_in              !! number of incoming elements.
        integer(int64), intent(in) :: nchars_in             !! incoming payload byte count.
        type(c_ptr), intent(in) :: offsets                  !! -> int32/int64 offsets(0:nrows_in), offsets(0)=0.
        type(c_ptr), intent(in) :: data                     !! -> nchars_in payload bytes, at offsets(0).
        type(c_ptr), intent(in) :: validity                 !! -> Arrow bitmap, or C_NULL_PTR.
        logical, intent(in) :: offsets_int32                !! .true. => source offsets are int32.
        integer(int64), intent(in), optional :: validity_offset_bits !! bit index of element 1 in `validity` (default 0).
        integer(int64), pointer :: off64(:)
        integer(int32), pointer :: off32(:)
        character(len=1), pointer :: din(:)
        integer(int8), pointer :: vin(:)
        integer(int64) :: base, k, voff, nn
        if (nrows_in <= 0) return
        voff = 0_int64
        if (present(validity_offset_bits)) voff = validity_offset_bits
        if (offsets_int32) then
            call c_f_pointer(offsets, off32, [nrows_in+1])
            if (off32(1) /= 0_int32) then
                error stop EP//"append_buffers: source offsets(1) must be 0 -- rebase a sliced source before calling"
            end if
        else
            call c_f_pointer(offsets, off64, [nrows_in+1])
            if (off64(1) /= 0_int64) then
                error stop EP//"append_buffers: source offsets(1) must be 0 -- rebase a sliced source before calling"
            end if
        end if
        call ensure_offsets_cap(self, self%nrows + nrows_in)
        if (nchars_in > 0) then
            call ensure_data_cap(self, self%nchars + nchars_in)
            call c_f_pointer(data, din, [nchars_in])
            self%data(self%nchars+1 : self%nchars+nchars_in) = din(1:nchars_in)
        end if
        base = self%nchars
        if (offsets_int32) then
            do k = 1_int64, nrows_in
                self%offsets(self%nrows+1+k) = base + int(off32(k+1), int64)
            end do
        else
            do k = 1_int64, nrows_in
                self%offsets(self%nrows+1+k) = base + off64(k+1)
            end do
        end if
        if (c_associated(validity)) then
            self%has_nulls = .true.
            call ensure_validity_cap(self, self%nrows + nrows_in)
            call c_f_pointer(validity, vin, [(voff + nrows_in + 7_int64)/8_int64])
            ! The same byte-wise core `%slice`/`%append_column` use, reached with raw maps because
            ! the source here is an Arrow bitmap behind a C pointer rather than a column. Whole bytes
            ! move as bytes whenever both sides start on a byte boundary -- which the common case
            ! does, since Arrow hands back an unsliced chunk (`voff` 0) and a row group is appended
            ! onto a column whose row count is a multiple of 8 for every group but a ragged last one.
            call copy_validity_run(vin, voff, self%validity, self%nrows, nrows_in, nn)
            self%n_null = self%n_null + nn
        else if (self%has_nulls) then
            call ensure_validity_cap(self, self%nrows + nrows_in)
            call fill_validity_valid(self, self%nrows + 1_int64, nrows_in)
        end if
        self%nrows = self%nrows + nrows_in
        self%nchars = self%nchars + nchars_in
    end subroutine append_buffers
    !
    !> Finalizer -- deallocates all owned buffers.
    !
    ! ==================================================================================
    ! parquet_string (handle) type-bound procedures
    ! ==================================================================================
    !
    !> Aborts when the handle is unassociated or its index no longer refers to a valid element.
    subroutine check_handle(self, proc)
        class(parquet_string), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: proc       !! calling procedure name.
        if (.not. associated(self%col)) error stop EP//"unassociated string handle in "//proc
        if (self%idx < 1 .or. self%idx > self%col%nrows) then
            error stop EP//"string handle index out of range (column changed?) in "//proc
        end if
    end subroutine check_handle
    !
    !> Returns the length of the referenced string (0 for a null element).
    integer(int64) function psv_length(self) result(n)
        class(parquet_string), intent(in) :: self !! the handle.
        call check_handle(self, "length")
        n = self%col%length_i64(self%idx)
    end function psv_length
    !
    !> Returns whether the referenced string has zero length (.true. for a null element).
    logical function psv_is_empty(self, check_null) result(res)
        class(parquet_string), intent(in) :: self !! the handle.
        logical, intent(in), optional :: check_null !! .true. => error stop on a null element.
        call check_handle(self, "is_empty")
        res = self%col%is_empty_i64(self%idx, check_null)
    end function psv_is_empty
    !
    !> Returns whether the referenced element is null.
    logical function psv_is_null(self) result(res)
        class(parquet_string), intent(in) :: self !! the handle.
        call check_handle(self, "is_null")
        res = self%col%is_null_i64(self%idx)
    end function psv_is_null
    !
    !> Sets the referenced element to null (write-through: mutates the underlying column, not
    !! just this handle -- consistent with every other parquet_string accessor resolving live
    !! against the referenced column rather than holding independent state).
    subroutine psv_set_null(self)
        class(parquet_string), intent(in) :: self !! the handle.
        call check_handle(self, "set_null")
        call self%col%set_null_i64(self%idx)
    end subroutine psv_set_null
    !
    !> Writes the referenced string into `res`. A null element error stops by default; pass
    !! `null_value` to substitute a string, or `allow_null=.true.` to suppress the abort and
    !! return an empty string (detect null via is_null).
    subroutine psv_to_string(self, res, null_value, allow_null)
        class(parquet_string), intent(in) :: self             !! the handle.
        character(len=:), allocatable, intent(out) :: res     !! the referenced string.
        character(len=*), intent(in), optional :: null_value  !! substitute for a null element.
        logical, intent(in), optional :: allow_null            !! .true. => suppress abort, return empty string for null.
        call check_handle(self, "to_string")
        call self%col%get_i64(self%idx, res, null_value, allow_null)
    end subroutine psv_to_string
    !
    !> Exact comparison of the referenced string against `str` (see the column's equals).
    logical function psv_equals(self, str, exact, check_null) result(res)
        class(parquet_string), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: str        !! query string.
        logical, intent(in), optional :: exact      !! .false. => trailing-trim both sides.
        logical, intent(in), optional :: check_null !! .true. => error stop on a null element.
        call check_handle(self, "equals")
        res = self%col%equals_i64(self%idx, str, exact, check_null)
    end function psv_equals
    !
    !> Substring search within the referenced string (see the column's contains).
    logical function psv_contains(self, str, check_null) result(res)
        class(parquet_string), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: str        !! substring to search for.
        logical, intent(in), optional :: check_null !! .true. => error stop on a null element.
        call check_handle(self, "contains")
        res = self%col%contains_i64(self%idx, str, check_null)
    end function psv_contains
    !
    !> Prefix test on the referenced string (see the column's startswith).
    logical function psv_startswith(self, prefix, check_null) result(res)
        class(parquet_string), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: prefix     !! prefix to test.
        logical, intent(in), optional :: check_null !! .true. => error stop on a null element.
        call check_handle(self, "startswith")
        res = self%col%startswith_i64(self%idx, prefix, check_null)
    end function psv_startswith
    !
    !> Suffix test on the referenced string (see the column's endswith).
    logical function psv_endswith(self, suffix, check_null) result(res)
        class(parquet_string), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: suffix     !! suffix to test.
        logical, intent(in), optional :: check_null !! .true. => error stop on a null element.
        call check_handle(self, "endswith")
        res = self%col%endswith_i64(self%idx, suffix, check_null)
    end function psv_endswith
    !
    !> Writes a human-readable representation of the referenced string.
    !!
    !! Solicited output: `verbosity = "silent"` returns having written nothing. With no `unit=` the
    !! text goes wherever `message_stream` names.
    !!
    !! **The handle is checked BEFORE the verbosity is**, so a stale or unbound handle is reported
    !! whether or not anything would have been printed -- the ordering every other printer in this
    !! library uses, and the reason is the same: silently doing nothing for the wrong reason is
    !! worse than saying so.
    subroutine psv_print(self, unit)
        class(parquet_string), intent(in) :: self !! the handle.
        integer, intent(in), optional :: unit      !! where to write; default the `message_stream` setting's unit.
        integer :: u
        character(len=:), allocatable :: s
        call check_handle(self, "print")
        if (parquet_output_is_suppressed()) return
        u = parquet_message_unit()
        if (present(unit)) u = unit
        if (self%col%is_null_i64(self%idx)) then
            write(u, '(a)') "<null>"
        else
            call self%col%get_i64(self%idx, s)
            write(u, '(a,a,a)') '"', s, '"'
        end if
    end subroutine psv_print
    !
end module parquet_strings ! GCOVR_EXCL_LINE
