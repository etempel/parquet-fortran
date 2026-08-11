!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- everything outside the marked USER SECTIONs is regenerated.
! Regenerate with:  tools/generate_user_table_code.py
! source-maml: table_types/maml_example4.maml
! source-maml-sha256: 4655135a459f309e77c9ff12e4d36b1d0082d73f2a8df13d55b63baa41a3f2e3
!
!> Table type generated from the `test` MAML schema: `parquet_table_test`, with one accessor per predefined column.
!!
!! It extends `parquet_table`, so every inherited operation (%nrows, %get, %filter_rows, %clone, %append, %row, ...) applies
!! unchanged. Open it with %init, %init_slice or %init_empty -- a plain parquet_open_table will not compile for this type,
!! deliberately, since it would skip the predefined columns.
module parquet_table_example
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private
    public :: parquet_table_test
    ! >>>>> USER SECTION (uses) -- your own use/parameter declarations; preserved on regeneration
    ! >>>>> END USER SECTION (uses)

    !> Predefined-column table generated from the `test` schema.
    type, extends(parquet_table) :: parquet_table_test
        ! Components you declare below are copied by %clone and reset by %init automatically:
        ! the generator writes those statements into clone_extra/init_extra for you.
        ! >>>>> USER SECTION (components) -- your own table parameters; preserved on regeneration
        !> A worked example of a user-added table parameter, and the thing that makes this
        !! module's clone_extra/init_extra statements non-trivial: the generator writes
        !! `out%zeropoint = self%zeropoint` and `self%zeropoint = 0.0_real64` from this
        !! declaration, and test/test_table_codegen.f90 checks that both happen.
        real(real64) :: zeropoint = 0.0_real64
        ! >>>>> END USER SECTION (components)
    contains
        procedure :: init => parquet_table_test_init !! Opens a file and binds every predefined column.
        procedure, private :: parquet_table_test_init_slice_i32 !! %init_slice specific, int32 bounds.
        procedure, private :: parquet_table_test_init_slice_i64 !! %init_slice specific, int64 bounds.
        !> Opens one contiguous row range of a file, binding the same columns %init does. There is no `sort` argument: a sort
        !! reorders rows across the whole file, so a slice could not be cut along it.
        generic :: init_slice => parquet_table_test_init_slice_i32, parquet_table_test_init_slice_i64
        procedure, private :: parquet_table_test_init_empty_none !! %init_empty specific, no row count.
        procedure, private :: parquet_table_test_init_empty_i32 !! %init_empty specific, int32 row count.
        procedure, private :: parquet_table_test_init_empty_i64 !! %init_empty specific, int64 row count.
        !> Builds this table in memory, with no file behind it: every predefined column is created empty, or with `nrows` all-null
        !! rows. The output-catalogue shape.
        generic :: init_empty => parquet_table_test_init_empty_none, parquet_table_test_init_empty_i32, &
            parquet_table_test_init_empty_i64
        procedure :: init_extra => parquet_table_test_init_extra !! Resets/initializes YOUR components.
        !> Copies this type's own components on %clone/%clone_structure. Mostly written for you.
        procedure :: clone_extra => parquet_table_test_clone_extra
        !> Object ID field. (int64) `%uberid()` is the whole column, `%uberid(i)` row i, `%uberid(lo,hi)` a row range -- all zero
        !! copy. Take `%uberid()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_uberid_all
        procedure, private :: parquet_table_test_uberid_at_i32
        procedure, private :: parquet_table_test_uberid_at_i64
        procedure, private :: parquet_table_test_uberid_rng_i32
        procedure, private :: parquet_table_test_uberid_rng_i64
        generic :: uberid => parquet_table_test_uberid_all, parquet_table_test_uberid_at_i32, parquet_table_test_uberid_at_i64, &
            parquet_table_test_uberid_rng_i32, parquet_table_test_uberid_rng_i64
        !> A plain int32 scalar column. (int32) `%idx()` is the whole column, `%idx(i)` row i, `%idx(lo,hi)` a row range -- all zero
        !! copy. Take `%idx()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_idx_all
        procedure, private :: parquet_table_test_idx_at_i32
        procedure, private :: parquet_table_test_idx_at_i64
        procedure, private :: parquet_table_test_idx_rng_i32
        procedure, private :: parquet_table_test_idx_rng_i64
        generic :: idx => parquet_table_test_idx_all, parquet_table_test_idx_at_i32, parquet_table_test_idx_at_i64, &
            parquet_table_test_idx_rng_i32, parquet_table_test_idx_rng_i64
        !> A boolean scalar column. (boolean) `%flag()` is the whole column, `%flag(i)` row i, `%flag(lo,hi)` a row range -- all
        !! zero copy. Take `%flag()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_flag_all
        procedure, private :: parquet_table_test_flag_at_i32
        procedure, private :: parquet_table_test_flag_at_i64
        procedure, private :: parquet_table_test_flag_rng_i32
        procedure, private :: parquet_table_test_flag_rng_i64
        generic :: flag => parquet_table_test_flag_all, parquet_table_test_flag_at_i32, parquet_table_test_flag_at_i64, &
            parquet_table_test_flag_rng_i32, parquet_table_test_flag_rng_i64
        !> Name of the object. (string) `%name()` is the packed string store, `%name(i)` a handle to row i, `%name(lo,hi)` one
        !! handle per row of the range.
        procedure, private :: parquet_table_test_name_all
        procedure, private :: parquet_table_test_name_at_i32
        procedure, private :: parquet_table_test_name_at_i64
        procedure, private :: parquet_table_test_name_rng_i32
        procedure, private :: parquet_table_test_name_rng_i64
        generic :: name => parquet_table_test_name_all, parquet_table_test_name_at_i32, parquet_table_test_name_at_i64, &
            parquet_table_test_name_rng_i32, parquet_table_test_name_rng_i64
        !> Name of the object. (string) Copied out as a character array sized to the longest value present.
        procedure :: name_chr => parquet_table_test_name_chr
        !> Right ascension. Unit: deg. (float64) `%ra()` is the whole column, `%ra(i)` row i, `%ra(lo,hi)` a row range -- all zero
        !! copy. Take `%ra()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_ra_all
        procedure, private :: parquet_table_test_ra_at_i32
        procedure, private :: parquet_table_test_ra_at_i64
        procedure, private :: parquet_table_test_ra_rng_i32
        procedure, private :: parquet_table_test_ra_rng_i64
        generic :: ra => parquet_table_test_ra_all, parquet_table_test_ra_at_i32, parquet_table_test_ra_at_i64, &
            parquet_table_test_ra_rng_i32, parquet_table_test_ra_rng_i64
        !> Declination. Unit: deg. (float64) `%dec()` is the whole column, `%dec(i)` row i, `%dec(lo,hi)` a row range -- all zero
        !! copy. Take `%dec()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_dec_all
        procedure, private :: parquet_table_test_dec_at_i32
        procedure, private :: parquet_table_test_dec_at_i64
        procedure, private :: parquet_table_test_dec_rng_i32
        procedure, private :: parquet_table_test_dec_rng_i64
        generic :: dec => parquet_table_test_dec_all, parquet_table_test_dec_at_i32, parquet_table_test_dec_at_i64, &
            parquet_table_test_dec_rng_i32, parquet_table_test_dec_rng_i64
        !> Comoving coordinates. Unit: Mpc. (float32, width 3) `%crd()` is the whole column, `%crd(i)` row i, `%crd(lo,hi)` a row
        !! range -- all zero copy. Take `%crd()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_crd_all
        procedure, private :: parquet_table_test_crd_at_i32
        procedure, private :: parquet_table_test_crd_at_i64
        procedure, private :: parquet_table_test_crd_rng_i32
        procedure, private :: parquet_table_test_crd_rng_i64
        generic :: crd => parquet_table_test_crd_all, parquet_table_test_crd_at_i32, parquet_table_test_crd_at_i64, &
            parquet_table_test_crd_rng_i32, parquet_table_test_crd_rng_i64
        !> An int32 vector column. (int32, width 2) `%counts()` is the whole column, `%counts(i)` row i, `%counts(lo,hi)` a row
        !! range -- all zero copy. Take `%counts()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_counts_all
        procedure, private :: parquet_table_test_counts_at_i32
        procedure, private :: parquet_table_test_counts_at_i64
        procedure, private :: parquet_table_test_counts_rng_i32
        procedure, private :: parquet_table_test_counts_rng_i64
        generic :: counts => parquet_table_test_counts_all, parquet_table_test_counts_at_i32, parquet_table_test_counts_at_i64, &
            parquet_table_test_counts_rng_i32, parquet_table_test_counts_rng_i64
        !> A boolean vector column. (boolean, width 2) `%passed()` is the whole column, `%passed(i)` row i, `%passed(lo,hi)` a row
        !! range -- all zero copy. Take `%passed()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_passed_all
        procedure, private :: parquet_table_test_passed_at_i32
        procedure, private :: parquet_table_test_passed_at_i64
        procedure, private :: parquet_table_test_passed_rng_i32
        procedure, private :: parquet_table_test_passed_rng_i64
        generic :: passed => parquet_table_test_passed_all, parquet_table_test_passed_at_i32, parquet_table_test_passed_at_i64, &
            parquet_table_test_passed_rng_i32, parquet_table_test_passed_rng_i64
        !> A date column. (date) `%obsdate()` is the whole column, `%obsdate(i)` row i, `%obsdate(lo,hi)` a row range -- all zero
        !! copy. Take `%obsdate()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_obsdate_all
        procedure, private :: parquet_table_test_obsdate_at_i32
        procedure, private :: parquet_table_test_obsdate_at_i64
        procedure, private :: parquet_table_test_obsdate_rng_i32
        procedure, private :: parquet_table_test_obsdate_rng_i64
        generic :: obsdate => parquet_table_test_obsdate_all, parquet_table_test_obsdate_at_i32, &
            parquet_table_test_obsdate_at_i64, parquet_table_test_obsdate_rng_i32, parquet_table_test_obsdate_rng_i64
        !> A millisecond time column. (time) `%obstime()` is the whole column, `%obstime(i)` row i, `%obstime(lo,hi)` a row range --
        !! all zero copy. Take `%obstime()` once outside a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_obstime_all
        procedure, private :: parquet_table_test_obstime_at_i32
        procedure, private :: parquet_table_test_obstime_at_i64
        procedure, private :: parquet_table_test_obstime_rng_i32
        procedure, private :: parquet_table_test_obstime_rng_i64
        generic :: obstime => parquet_table_test_obstime_all, parquet_table_test_obstime_at_i32, &
            parquet_table_test_obstime_at_i64, parquet_table_test_obstime_rng_i32, parquet_table_test_obstime_rng_i64
        !> A UTC microsecond timestamp column. (timestamp) `%obsstamp()` is the whole column, `%obsstamp(i)` row i,
        !! `%obsstamp(lo,hi)` a row range -- all zero copy. Take `%obsstamp()` once outside a loop; the indexed forms repeat the
        !! column lookup.
        procedure, private :: parquet_table_test_obsstamp_all
        procedure, private :: parquet_table_test_obsstamp_at_i32
        procedure, private :: parquet_table_test_obsstamp_at_i64
        procedure, private :: parquet_table_test_obsstamp_rng_i32
        procedure, private :: parquet_table_test_obsstamp_rng_i64
        generic :: obsstamp => parquet_table_test_obsstamp_all, parquet_table_test_obsstamp_at_i32, &
            parquet_table_test_obsstamp_at_i64, parquet_table_test_obsstamp_rng_i32, parquet_table_test_obsstamp_rng_i64
        !> A string vector column, which has only the character copy-out form. (string, width 2) Copied out as a character array
        !! sized to the longest value present.
        procedure :: tags_chr => parquet_table_test_tags_chr
        !> Filled in by the program, never read from the file. Unit: Jy. (float64) Computed: no file column is read for it.
        !! `%flux()` is the whole column, `%flux(i)` row i, `%flux(lo,hi)` a row range -- all zero copy. Take `%flux()` once outside
        !! a loop; the indexed forms repeat the column lookup.
        procedure, private :: parquet_table_test_flux_all
        procedure, private :: parquet_table_test_flux_at_i32
        procedure, private :: parquet_table_test_flux_at_i64
        procedure, private :: parquet_table_test_flux_rng_i32
        procedure, private :: parquet_table_test_flux_rng_i64
        generic :: flux => parquet_table_test_flux_all, parquet_table_test_flux_at_i32, parquet_table_test_flux_at_i64, &
            parquet_table_test_flux_rng_i32, parquet_table_test_flux_rng_i64
        ! >>>>> USER SECTION (bindings) -- your own type-bound procedures; preserved on regeneration
        ! >>>>> END USER SECTION (bindings)
    end type parquet_table_test

contains

    !> Opens the file and binds every predefined column this schema declares: each is checked for presence, kind and width,
    !! converted to its declared kind where a conversion rule applies, and fully materialized before this returns. Every optional
    !! argument is forwarded to parquet_open_table unchanged, except `exact`, which belongs to the kind conversion.
    subroutine parquet_table_test_init(self, filename, maml, filter, sort, qc, qc_soft, use_threads, &
            sample_fraction, sample_seed, exact)
        class(parquet_table_test), intent(inout) :: self !! the table to fill.
        character(len=*), intent(in) :: filename !! parquet file to open.
        character(len=*), intent(in), optional :: maml !! read-in (Role-B) MAML describing the file.
        type(parquet_filter), intent(in), optional :: filter !! row filter, in INTERNAL column names.
        type(parquet_sortkey), intent(in), optional :: sort !! sort keys, in INTERNAL column names.
        type(parquet_read_qc), intent(in), optional :: qc !! read-time qc, in INTERNAL column names.
        logical, intent(in), optional :: qc_soft !! warn on a qc violation instead of aborting.
        logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.
        real(real64), intent(in), optional :: sample_fraction !! keep each row with this probability.
        integer(int32), intent(in), optional :: sample_seed !! seed for that draw.
        logical, intent(in), optional :: exact !! refuse a kind conversion that would lose a value;
        !! forwarded verbatim to %cast, whose own default applies when this is absent.
        !
        call parquet_open_table(self%parquet_table, filename, maml, filter, sort, qc, qc_soft, &
            use_threads, sample_fraction, sample_seed)
        call self%bind_predefined( &
            [character(len=8) :: "uberid", "idx", "flag", "name", "ra", "dec", "crd", "counts", "passed", "obsdate", "obstime", &
                "obsstamp", "tags", "flux"], &
            [PK_INT64, PK_INT32, PK_LOGICAL, PK_STRING, PK_FLOAT64, PK_FLOAT64, PK_FLOAT32_VEC, PK_INT32_VEC, PK_LOGICAL_VEC, &
                PK_DATE, PK_TIME, PK_TIMESTAMP, PK_STRING_VEC, PK_FLOAT64], &
            [1, 1, 1, 1, 1, 1, 3, 2, 2, 1, 1, 1, 2, 1], &
            [.true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .false.], &
            context="maml_example4.maml", exact=exact, &
            units=[character(len=3) :: "", "", "", "", "deg", "deg", "Mpc", "", "", "", "", "", "", "Jy"])
        call self%init_extra()
    end subroutine parquet_table_test_init
    !
    !> %init_slice specific taking i32 row bounds; see the generic binding.
    subroutine parquet_table_test_init_slice_i32(self, filename, row_lo, row_hi, maml, filter, qc, &
            qc_soft, use_threads, sample_fraction, sample_seed, exact)
        class(parquet_table_test), intent(inout) :: self !! the table to fill.
        character(len=*), intent(in) :: filename !! parquet file to open.
        integer(int32), intent(in) :: row_lo !! first file row to cover (1-based).
        integer(int32), intent(in) :: row_hi !! last file row to cover (inclusive).
        character(len=*), intent(in), optional :: maml !! read-in (Role-B) MAML describing the file.
        type(parquet_filter), intent(in), optional :: filter !! row filter, in INTERNAL column names.
        type(parquet_read_qc), intent(in), optional :: qc !! read-time qc, in INTERNAL column names.
        logical, intent(in), optional :: qc_soft !! warn on a qc violation instead of aborting.
        logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.
        real(real64), intent(in), optional :: sample_fraction !! keep each row with this probability.
        integer(int32), intent(in), optional :: sample_seed !! seed for that draw.
        logical, intent(in), optional :: exact !! refuse a kind conversion that would lose a value;
        !! forwarded verbatim to %cast, whose own default applies when this is absent.
        !
        call parquet_open_table(self%parquet_table, filename, row_lo, row_hi, maml, filter, qc, &
            qc_soft, use_threads, sample_fraction, sample_seed)
        call self%bind_predefined( &
            [character(len=8) :: "uberid", "idx", "flag", "name", "ra", "dec", "crd", "counts", "passed", "obsdate", "obstime", &
                "obsstamp", "tags", "flux"], &
            [PK_INT64, PK_INT32, PK_LOGICAL, PK_STRING, PK_FLOAT64, PK_FLOAT64, PK_FLOAT32_VEC, PK_INT32_VEC, PK_LOGICAL_VEC, &
                PK_DATE, PK_TIME, PK_TIMESTAMP, PK_STRING_VEC, PK_FLOAT64], &
            [1, 1, 1, 1, 1, 1, 3, 2, 2, 1, 1, 1, 2, 1], &
            [.true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .false.], &
            context="maml_example4.maml", exact=exact, &
            units=[character(len=3) :: "", "", "", "", "deg", "deg", "Mpc", "", "", "", "", "", "", "Jy"])
        call self%init_extra()
    end subroutine parquet_table_test_init_slice_i32
    !
    !> %init_slice specific taking i64 row bounds; see the generic binding.
    subroutine parquet_table_test_init_slice_i64(self, filename, row_lo, row_hi, maml, filter, qc, &
            qc_soft, use_threads, sample_fraction, sample_seed, exact)
        class(parquet_table_test), intent(inout) :: self !! the table to fill.
        character(len=*), intent(in) :: filename !! parquet file to open.
        integer(int64), intent(in) :: row_lo !! first file row to cover (1-based).
        integer(int64), intent(in) :: row_hi !! last file row to cover (inclusive).
        character(len=*), intent(in), optional :: maml !! read-in (Role-B) MAML describing the file.
        type(parquet_filter), intent(in), optional :: filter !! row filter, in INTERNAL column names.
        type(parquet_read_qc), intent(in), optional :: qc !! read-time qc, in INTERNAL column names.
        logical, intent(in), optional :: qc_soft !! warn on a qc violation instead of aborting.
        logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.
        real(real64), intent(in), optional :: sample_fraction !! keep each row with this probability.
        integer(int32), intent(in), optional :: sample_seed !! seed for that draw.
        logical, intent(in), optional :: exact !! refuse a kind conversion that would lose a value;
        !! forwarded verbatim to %cast, whose own default applies when this is absent.
        !
        call parquet_open_table(self%parquet_table, filename, row_lo, row_hi, maml, filter, qc, &
            qc_soft, use_threads, sample_fraction, sample_seed)
        call self%bind_predefined( &
            [character(len=8) :: "uberid", "idx", "flag", "name", "ra", "dec", "crd", "counts", "passed", "obsdate", "obstime", &
                "obsstamp", "tags", "flux"], &
            [PK_INT64, PK_INT32, PK_LOGICAL, PK_STRING, PK_FLOAT64, PK_FLOAT64, PK_FLOAT32_VEC, PK_INT32_VEC, PK_LOGICAL_VEC, &
                PK_DATE, PK_TIME, PK_TIMESTAMP, PK_STRING_VEC, PK_FLOAT64], &
            [1, 1, 1, 1, 1, 1, 3, 2, 2, 1, 1, 1, 2, 1], &
            [.true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .false.], &
            context="maml_example4.maml", exact=exact, &
            units=[character(len=3) :: "", "", "", "", "deg", "deg", "Mpc", "", "", "", "", "", "", "Jy"])
        call self%init_extra()
    end subroutine parquet_table_test_init_slice_i64
    !
    !> %init_empty specific taking no row count: every predefined column is created with zero rows, to be grown with %append.
    subroutine parquet_table_test_init_empty_none(self)
        class(parquet_table_test), intent(inout) :: self !! the table to build.
        !
        call parquet_new_table(self%parquet_table)
        call self%bind_predefined( &
            [character(len=8) :: "uberid", "idx", "flag", "name", "ra", "dec", "crd", "counts", "passed", "obsdate", "obstime", &
                "obsstamp", "tags", "flux"], &
            [PK_INT64, PK_INT32, PK_LOGICAL, PK_STRING, PK_FLOAT64, PK_FLOAT64, PK_FLOAT32_VEC, PK_INT32_VEC, PK_LOGICAL_VEC, &
                PK_DATE, PK_TIME, PK_TIMESTAMP, PK_STRING_VEC, PK_FLOAT64], &
            [1, 1, 1, 1, 1, 1, 3, 2, 2, 1, 1, 1, 2, 1], &
            [.false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                .false.], &
            context="maml_example4.maml", &
            units=[character(len=3) :: "", "", "", "", "deg", "deg", "Mpc", "", "", "", "", "", "", "Jy"])
        call self%init_extra()
    end subroutine parquet_table_test_init_empty_none
    !
    !> %init_empty specific taking an i32 row count: every predefined column is created with `nrows` all-null rows, ready to be
    !! filled in place.
    subroutine parquet_table_test_init_empty_i32(self, nrows)
        class(parquet_table_test), intent(inout) :: self !! the table to build.
        integer(int32), intent(in) :: nrows !! rows to create, all null.
        !
        call parquet_new_table(self%parquet_table)
        call self%bind_predefined( &
            [character(len=8) :: "uberid", "idx", "flag", "name", "ra", "dec", "crd", "counts", "passed", "obsdate", "obstime", &
                "obsstamp", "tags", "flux"], &
            [PK_INT64, PK_INT32, PK_LOGICAL, PK_STRING, PK_FLOAT64, PK_FLOAT64, PK_FLOAT32_VEC, PK_INT32_VEC, PK_LOGICAL_VEC, &
                PK_DATE, PK_TIME, PK_TIMESTAMP, PK_STRING_VEC, PK_FLOAT64], &
            [1, 1, 1, 1, 1, 1, 3, 2, 2, 1, 1, 1, 2, 1], &
            [.false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                .false.], &
            context="maml_example4.maml", &
            units=[character(len=3) :: "", "", "", "", "deg", "deg", "Mpc", "", "", "", "", "", "", "Jy"])
        if (nrows > 0) call self%append_null_rows(nrows)
        call self%init_extra()
    end subroutine parquet_table_test_init_empty_i32
    !
    !> %init_empty specific taking an i64 row count: every predefined column is created with `nrows` all-null rows, ready to be
    !! filled in place.
    subroutine parquet_table_test_init_empty_i64(self, nrows)
        class(parquet_table_test), intent(inout) :: self !! the table to build.
        integer(int64), intent(in) :: nrows !! rows to create, all null.
        !
        call parquet_new_table(self%parquet_table)
        call self%bind_predefined( &
            [character(len=8) :: "uberid", "idx", "flag", "name", "ra", "dec", "crd", "counts", "passed", "obsdate", "obstime", &
                "obsstamp", "tags", "flux"], &
            [PK_INT64, PK_INT32, PK_LOGICAL, PK_STRING, PK_FLOAT64, PK_FLOAT64, PK_FLOAT32_VEC, PK_INT32_VEC, PK_LOGICAL_VEC, &
                PK_DATE, PK_TIME, PK_TIMESTAMP, PK_STRING_VEC, PK_FLOAT64], &
            [1, 1, 1, 1, 1, 1, 3, 2, 2, 1, 1, 1, 2, 1], &
            [.false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                .false.], &
            context="maml_example4.maml", &
            units=[character(len=3) :: "", "", "", "", "deg", "deg", "Mpc", "", "", "", "", "", "", "Jy"])
        if (nrows > 0) call self%append_null_rows(nrows)
        call self%init_extra()
    end subroutine parquet_table_test_init_empty_i64
    !
    !> Resets and initializes this type's own components. Called by every constructor, so whatever you write here happens exactly
    !! once however the table was built -- and so your own parameters must be set AFTER %init returns, not before.
    subroutine parquet_table_test_init_extra(self)
        class(parquet_table_test), intent(inout) :: self !! the table just constructed.
        !
        self%zeropoint = 0.0_real64
        ! >>>>> USER SECTION (init_extra) -- your own initialization; preserved on regeneration
        ! >>>>> END USER SECTION (init_extra)
    end subroutine parquet_table_test_init_extra
    !
    !> Copies this type's own components when the table is cloned. `out` is guaranteed to have the same dynamic type as `self`.
    !! `structure_only` is .true. when called from %clone_structure, i.e. when `out` has the same columns but no rows.
    subroutine parquet_table_test_clone_extra(self, out, structure_only)
        class(parquet_table_test), intent(in) :: self !! the table being copied.
        class(parquet_table), intent(inout) :: out !! the copy, already holding the base state.
        logical, intent(in) :: structure_only !! .true. when called from %clone_structure.
        !
        select type (out)
        class is (parquet_table_test)
            out%zeropoint = self%zeropoint
            ! >>>>> USER SECTION (clone_extra) -- anything the generator could not copy for you
            ! >>>>> END USER SECTION (clone_extra)
        end select
    end subroutine parquet_table_test_clone_extra
    !
    !> Object ID field. (int64) The whole column. Points into the live storage: zero copy, writable, and invalidated by any
    !! row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_uberid_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), pointer :: p(:) !! alias to the column's storage.
        !
        call self%col("uberid", p)
    end function parquet_table_test_uberid_all
    !
    !> Object ID field. (int64) Row `i` alone. Repeats the column lookup on every call -- take `%uberid()` once instead when
    !! iterating.
    function parquet_table_test_uberid_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        integer(int64), pointer :: p !! alias to that row's storage.
        integer(int64), pointer :: a(:)
        !
        call self%col("uberid", a)
        call parquet_table_test_check_row(self, int(i, int64), "uberid")
        p => a(i)
    end function parquet_table_test_uberid_at_i32
    !
    !> Object ID field. (int64) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and invalidated
    !! by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_uberid_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        integer(int64), pointer :: p(:) !! alias to that range's storage.
        integer(int64), pointer :: a(:)
        !
        call self%col("uberid", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "uberid")
        p => a(lo:hi)
    end function parquet_table_test_uberid_rng_i32
    !
    !> Object ID field. (int64) Row `i` alone. Repeats the column lookup on every call -- take `%uberid()` once instead when
    !! iterating.
    function parquet_table_test_uberid_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        integer(int64), pointer :: p !! alias to that row's storage.
        integer(int64), pointer :: a(:)
        !
        call self%col("uberid", a)
        call parquet_table_test_check_row(self, i, "uberid")
        p => a(i)
    end function parquet_table_test_uberid_at_i64
    !
    !> Object ID field. (int64) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and invalidated
    !! by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_uberid_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        integer(int64), pointer :: p(:) !! alias to that range's storage.
        integer(int64), pointer :: a(:)
        !
        call self%col("uberid", a)
        call parquet_table_test_check_range(self, lo, hi, "uberid")
        p => a(lo:hi)
    end function parquet_table_test_uberid_rng_i64
    !
    !> A plain int32 scalar column. (int32) The whole column. Points into the live storage: zero copy, writable, and invalidated by
    !! any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_idx_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), pointer :: p(:) !! alias to the column's storage.
        !
        call self%col("idx", p)
    end function parquet_table_test_idx_all
    !
    !> A plain int32 scalar column. (int32) Row `i` alone. Repeats the column lookup on every call -- take `%idx()` once instead
    !! when iterating.
    function parquet_table_test_idx_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        integer(int32), pointer :: p !! alias to that row's storage.
        integer(int32), pointer :: a(:)
        !
        call self%col("idx", a)
        call parquet_table_test_check_row(self, int(i, int64), "idx")
        p => a(i)
    end function parquet_table_test_idx_at_i32
    !
    !> A plain int32 scalar column. (int32) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_idx_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        integer(int32), pointer :: p(:) !! alias to that range's storage.
        integer(int32), pointer :: a(:)
        !
        call self%col("idx", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "idx")
        p => a(lo:hi)
    end function parquet_table_test_idx_rng_i32
    !
    !> A plain int32 scalar column. (int32) Row `i` alone. Repeats the column lookup on every call -- take `%idx()` once instead
    !! when iterating.
    function parquet_table_test_idx_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        integer(int32), pointer :: p !! alias to that row's storage.
        integer(int32), pointer :: a(:)
        !
        call self%col("idx", a)
        call parquet_table_test_check_row(self, i, "idx")
        p => a(i)
    end function parquet_table_test_idx_at_i64
    !
    !> A plain int32 scalar column. (int32) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_idx_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        integer(int32), pointer :: p(:) !! alias to that range's storage.
        integer(int32), pointer :: a(:)
        !
        call self%col("idx", a)
        call parquet_table_test_check_range(self, lo, hi, "idx")
        p => a(lo:hi)
    end function parquet_table_test_idx_rng_i64
    !
    !> A boolean scalar column. (boolean) The whole column. Points into the live storage: zero copy, writable, and invalidated by
    !! any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_flag_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        logical, pointer :: p(:) !! alias to the column's storage.
        !
        call self%col("flag", p)
    end function parquet_table_test_flag_all
    !
    !> A boolean scalar column. (boolean) Row `i` alone. Repeats the column lookup on every call -- take `%flag()` once instead when
    !! iterating.
    function parquet_table_test_flag_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        logical, pointer :: p !! alias to that row's storage.
        logical, pointer :: a(:)
        !
        call self%col("flag", a)
        call parquet_table_test_check_row(self, int(i, int64), "flag")
        p => a(i)
    end function parquet_table_test_flag_at_i32
    !
    !> A boolean scalar column. (boolean) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_flag_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        logical, pointer :: p(:) !! alias to that range's storage.
        logical, pointer :: a(:)
        !
        call self%col("flag", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "flag")
        p => a(lo:hi)
    end function parquet_table_test_flag_rng_i32
    !
    !> A boolean scalar column. (boolean) Row `i` alone. Repeats the column lookup on every call -- take `%flag()` once instead when
    !! iterating.
    function parquet_table_test_flag_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        logical, pointer :: p !! alias to that row's storage.
        logical, pointer :: a(:)
        !
        call self%col("flag", a)
        call parquet_table_test_check_row(self, i, "flag")
        p => a(i)
    end function parquet_table_test_flag_at_i64
    !
    !> A boolean scalar column. (boolean) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_flag_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        logical, pointer :: p(:) !! alias to that range's storage.
        logical, pointer :: a(:)
        !
        call self%col("flag", a)
        call parquet_table_test_check_range(self, lo, hi, "flag")
        p => a(lo:hi)
    end function parquet_table_test_flag_rng_i64
    !
    !> Name of the object. (string) The packed string store itself, not a character array -- read it and edit its values in place,
    !! but do not change its length or element count through it.
    function parquet_table_test_name_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        type(parquet_string_column), pointer :: p !! alias to the packed store.
        !
        call self%col("name", p)
    end function parquet_table_test_name_all
    !
    !> Name of the object. (string) A zero-copy handle to row `i`. Repeats the column lookup on every call.
    function parquet_table_test_name_at_i32(self, i) result(h)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        type(parquet_string) :: h !! handle to that row's value.
        type(parquet_string_column), pointer :: p
        !
        call self%col("name", p)
        h = p%view(i)
    end function parquet_table_test_name_at_i32
    !
    !> Name of the object. (string) One zero-copy handle per row of `[lo, hi]`, in order.
    function parquet_table_test_name_rng_i32(self, lo, hi) result(h)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        type(parquet_string), allocatable :: h(:) !! one handle per row.
        type(parquet_string_column), pointer :: p
        !
        call self%col("name", p)
        allocate(h(max(hi - lo + 1, 0)))
        call p%view_slice(lo, hi, h)
    end function parquet_table_test_name_rng_i32
    !
    !> Name of the object. (string) A zero-copy handle to row `i`. Repeats the column lookup on every call.
    function parquet_table_test_name_at_i64(self, i) result(h)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        type(parquet_string) :: h !! handle to that row's value.
        type(parquet_string_column), pointer :: p
        !
        call self%col("name", p)
        h = p%view(i)
    end function parquet_table_test_name_at_i64
    !
    !> Name of the object. (string) One zero-copy handle per row of `[lo, hi]`, in order.
    function parquet_table_test_name_rng_i64(self, lo, hi) result(h)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        type(parquet_string), allocatable :: h(:) !! one handle per row.
        type(parquet_string_column), pointer :: p
        !
        call self%col("name", p)
        allocate(h(max(hi - lo + 1, 0)))
        call p%view_slice(lo, hi, h)
    end function parquet_table_test_name_rng_i64
    !
    !> Name of the object. (string) Copied out as a character array sized to the longest value present. A null element comes back
    !! blank -- gate on %is_null to tell a null from a genuinely empty string.
    subroutine parquet_table_test_name_chr(self, arr)
        class(parquet_table_test), intent(in) :: self !! the table.
        character(len=:), allocatable, intent(out) :: arr(:) !! one value per row.
        !
        call self%get("name", arr)
    end subroutine parquet_table_test_name_chr
    !
    !> Right ascension. Unit: deg. (float64) The whole column. Points into the live storage: zero copy, writable, and invalidated by
    !! any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_ra_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        real(real64), pointer :: p(:) !! alias to the column's storage.
        !
        call self%col("ra", p)
    end function parquet_table_test_ra_all
    !
    !> Right ascension. Unit: deg. (float64) Row `i` alone. Repeats the column lookup on every call -- take `%ra()` once instead
    !! when iterating.
    function parquet_table_test_ra_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        real(real64), pointer :: p !! alias to that row's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("ra", a)
        call parquet_table_test_check_row(self, int(i, int64), "ra")
        p => a(i)
    end function parquet_table_test_ra_at_i32
    !
    !> Right ascension. Unit: deg. (float64) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable,
    !! and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_ra_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        real(real64), pointer :: p(:) !! alias to that range's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("ra", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "ra")
        p => a(lo:hi)
    end function parquet_table_test_ra_rng_i32
    !
    !> Right ascension. Unit: deg. (float64) Row `i` alone. Repeats the column lookup on every call -- take `%ra()` once instead
    !! when iterating.
    function parquet_table_test_ra_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        real(real64), pointer :: p !! alias to that row's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("ra", a)
        call parquet_table_test_check_row(self, i, "ra")
        p => a(i)
    end function parquet_table_test_ra_at_i64
    !
    !> Right ascension. Unit: deg. (float64) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable,
    !! and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_ra_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        real(real64), pointer :: p(:) !! alias to that range's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("ra", a)
        call parquet_table_test_check_range(self, lo, hi, "ra")
        p => a(lo:hi)
    end function parquet_table_test_ra_rng_i64
    !
    !> Declination. Unit: deg. (float64) The whole column. Points into the live storage: zero copy, writable, and invalidated by any
    !! row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_dec_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        real(real64), pointer :: p(:) !! alias to the column's storage.
        !
        call self%col("dec", p)
    end function parquet_table_test_dec_all
    !
    !> Declination. Unit: deg. (float64) Row `i` alone. Repeats the column lookup on every call -- take `%dec()` once instead when
    !! iterating.
    function parquet_table_test_dec_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        real(real64), pointer :: p !! alias to that row's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("dec", a)
        call parquet_table_test_check_row(self, int(i, int64), "dec")
        p => a(i)
    end function parquet_table_test_dec_at_i32
    !
    !> Declination. Unit: deg. (float64) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_dec_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        real(real64), pointer :: p(:) !! alias to that range's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("dec", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "dec")
        p => a(lo:hi)
    end function parquet_table_test_dec_rng_i32
    !
    !> Declination. Unit: deg. (float64) Row `i` alone. Repeats the column lookup on every call -- take `%dec()` once instead when
    !! iterating.
    function parquet_table_test_dec_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        real(real64), pointer :: p !! alias to that row's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("dec", a)
        call parquet_table_test_check_row(self, i, "dec")
        p => a(i)
    end function parquet_table_test_dec_at_i64
    !
    !> Declination. Unit: deg. (float64) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_dec_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        real(real64), pointer :: p(:) !! alias to that range's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("dec", a)
        call parquet_table_test_check_range(self, lo, hi, "dec")
        p => a(lo:hi)
    end function parquet_table_test_dec_rng_i64
    !
    !> Comoving coordinates. Unit: Mpc. (float32, width 3) The whole column. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_crd_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        real(real32), pointer :: p(:,:) !! alias to the column's storage.
        !
        call self%col("crd", p)
    end function parquet_table_test_crd_all
    !
    !> Comoving coordinates. Unit: Mpc. (float32, width 3) Row `i`'s 3 values. Repeats the column lookup on every call -- take
    !! `%crd()` once instead when iterating.
    function parquet_table_test_crd_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        real(real32), pointer :: p(:) !! alias to that row's storage.
        real(real32), pointer :: a(:,:)
        !
        call self%col("crd", a)
        call parquet_table_test_check_row(self, int(i, int64), "crd")
        p => a(:, i)
    end function parquet_table_test_crd_at_i32
    !
    !> Comoving coordinates. Unit: Mpc. (float32, width 3) Rows `lo` through `hi` inclusive, full width. Points into the live
    !! storage: zero copy, writable, and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows,
    !! %truncate, %append).
    function parquet_table_test_crd_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        real(real32), pointer :: p(:,:) !! alias to that range's storage.
        real(real32), pointer :: a(:,:)
        !
        call self%col("crd", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "crd")
        p => a(:, lo:hi)
    end function parquet_table_test_crd_rng_i32
    !
    !> Comoving coordinates. Unit: Mpc. (float32, width 3) Row `i`'s 3 values. Repeats the column lookup on every call -- take
    !! `%crd()` once instead when iterating.
    function parquet_table_test_crd_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        real(real32), pointer :: p(:) !! alias to that row's storage.
        real(real32), pointer :: a(:,:)
        !
        call self%col("crd", a)
        call parquet_table_test_check_row(self, i, "crd")
        p => a(:, i)
    end function parquet_table_test_crd_at_i64
    !
    !> Comoving coordinates. Unit: Mpc. (float32, width 3) Rows `lo` through `hi` inclusive, full width. Points into the live
    !! storage: zero copy, writable, and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows,
    !! %truncate, %append).
    function parquet_table_test_crd_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        real(real32), pointer :: p(:,:) !! alias to that range's storage.
        real(real32), pointer :: a(:,:)
        !
        call self%col("crd", a)
        call parquet_table_test_check_range(self, lo, hi, "crd")
        p => a(:, lo:hi)
    end function parquet_table_test_crd_rng_i64
    !
    !> An int32 vector column. (int32, width 2) The whole column. Points into the live storage: zero copy, writable, and invalidated
    !! by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_counts_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), pointer :: p(:,:) !! alias to the column's storage.
        !
        call self%col("counts", p)
    end function parquet_table_test_counts_all
    !
    !> An int32 vector column. (int32, width 2) Row `i`'s 2 values. Repeats the column lookup on every call -- take `%counts()` once
    !! instead when iterating.
    function parquet_table_test_counts_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        integer(int32), pointer :: p(:) !! alias to that row's storage.
        integer(int32), pointer :: a(:,:)
        !
        call self%col("counts", a)
        call parquet_table_test_check_row(self, int(i, int64), "counts")
        p => a(:, i)
    end function parquet_table_test_counts_at_i32
    !
    !> An int32 vector column. (int32, width 2) Rows `lo` through `hi` inclusive, full width. Points into the live storage: zero
    !! copy, writable, and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate,
    !! %append).
    function parquet_table_test_counts_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        integer(int32), pointer :: p(:,:) !! alias to that range's storage.
        integer(int32), pointer :: a(:,:)
        !
        call self%col("counts", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "counts")
        p => a(:, lo:hi)
    end function parquet_table_test_counts_rng_i32
    !
    !> An int32 vector column. (int32, width 2) Row `i`'s 2 values. Repeats the column lookup on every call -- take `%counts()` once
    !! instead when iterating.
    function parquet_table_test_counts_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        integer(int32), pointer :: p(:) !! alias to that row's storage.
        integer(int32), pointer :: a(:,:)
        !
        call self%col("counts", a)
        call parquet_table_test_check_row(self, i, "counts")
        p => a(:, i)
    end function parquet_table_test_counts_at_i64
    !
    !> An int32 vector column. (int32, width 2) Rows `lo` through `hi` inclusive, full width. Points into the live storage: zero
    !! copy, writable, and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate,
    !! %append).
    function parquet_table_test_counts_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        integer(int32), pointer :: p(:,:) !! alias to that range's storage.
        integer(int32), pointer :: a(:,:)
        !
        call self%col("counts", a)
        call parquet_table_test_check_range(self, lo, hi, "counts")
        p => a(:, lo:hi)
    end function parquet_table_test_counts_rng_i64
    !
    !> A boolean vector column. (boolean, width 2) The whole column. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_passed_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        logical, pointer :: p(:,:) !! alias to the column's storage.
        !
        call self%col("passed", p)
    end function parquet_table_test_passed_all
    !
    !> A boolean vector column. (boolean, width 2) Row `i`'s 2 values. Repeats the column lookup on every call -- take `%passed()`
    !! once instead when iterating.
    function parquet_table_test_passed_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        logical, pointer :: p(:) !! alias to that row's storage.
        logical, pointer :: a(:,:)
        !
        call self%col("passed", a)
        call parquet_table_test_check_row(self, int(i, int64), "passed")
        p => a(:, i)
    end function parquet_table_test_passed_at_i32
    !
    !> A boolean vector column. (boolean, width 2) Rows `lo` through `hi` inclusive, full width. Points into the live storage: zero
    !! copy, writable, and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate,
    !! %append).
    function parquet_table_test_passed_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        logical, pointer :: p(:,:) !! alias to that range's storage.
        logical, pointer :: a(:,:)
        !
        call self%col("passed", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "passed")
        p => a(:, lo:hi)
    end function parquet_table_test_passed_rng_i32
    !
    !> A boolean vector column. (boolean, width 2) Row `i`'s 2 values. Repeats the column lookup on every call -- take `%passed()`
    !! once instead when iterating.
    function parquet_table_test_passed_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        logical, pointer :: p(:) !! alias to that row's storage.
        logical, pointer :: a(:,:)
        !
        call self%col("passed", a)
        call parquet_table_test_check_row(self, i, "passed")
        p => a(:, i)
    end function parquet_table_test_passed_at_i64
    !
    !> A boolean vector column. (boolean, width 2) Rows `lo` through `hi` inclusive, full width. Points into the live storage: zero
    !! copy, writable, and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate,
    !! %append).
    function parquet_table_test_passed_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        logical, pointer :: p(:,:) !! alias to that range's storage.
        logical, pointer :: a(:,:)
        !
        call self%col("passed", a)
        call parquet_table_test_check_range(self, lo, hi, "passed")
        p => a(:, lo:hi)
    end function parquet_table_test_passed_rng_i64
    !
    !> A date column. (date) The whole column. Points into the live storage: zero copy, writable, and invalidated by any
    !! row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_obsdate_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        type(parquet_date), pointer :: p(:) !! alias to the column's storage.
        !
        call self%col("obsdate", p)
    end function parquet_table_test_obsdate_all
    !
    !> A date column. (date) Row `i` alone. Repeats the column lookup on every call -- take `%obsdate()` once instead when
    !! iterating.
    function parquet_table_test_obsdate_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        type(parquet_date), pointer :: p !! alias to that row's storage.
        type(parquet_date), pointer :: a(:)
        !
        call self%col("obsdate", a)
        call parquet_table_test_check_row(self, int(i, int64), "obsdate")
        p => a(i)
    end function parquet_table_test_obsdate_at_i32
    !
    !> A date column. (date) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and invalidated by
    !! any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_obsdate_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        type(parquet_date), pointer :: p(:) !! alias to that range's storage.
        type(parquet_date), pointer :: a(:)
        !
        call self%col("obsdate", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "obsdate")
        p => a(lo:hi)
    end function parquet_table_test_obsdate_rng_i32
    !
    !> A date column. (date) Row `i` alone. Repeats the column lookup on every call -- take `%obsdate()` once instead when
    !! iterating.
    function parquet_table_test_obsdate_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        type(parquet_date), pointer :: p !! alias to that row's storage.
        type(parquet_date), pointer :: a(:)
        !
        call self%col("obsdate", a)
        call parquet_table_test_check_row(self, i, "obsdate")
        p => a(i)
    end function parquet_table_test_obsdate_at_i64
    !
    !> A date column. (date) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and invalidated by
    !! any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_obsdate_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        type(parquet_date), pointer :: p(:) !! alias to that range's storage.
        type(parquet_date), pointer :: a(:)
        !
        call self%col("obsdate", a)
        call parquet_table_test_check_range(self, lo, hi, "obsdate")
        p => a(lo:hi)
    end function parquet_table_test_obsdate_rng_i64
    !
    !> A millisecond time column. (time) The whole column. Points into the live storage: zero copy, writable, and invalidated by any
    !! row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_obstime_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        type(parquet_time), pointer :: p(:) !! alias to the column's storage.
        !
        call self%col("obstime", p)
    end function parquet_table_test_obstime_all
    !
    !> A millisecond time column. (time) Row `i` alone. Repeats the column lookup on every call -- take `%obstime()` once instead
    !! when iterating.
    function parquet_table_test_obstime_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        type(parquet_time), pointer :: p !! alias to that row's storage.
        type(parquet_time), pointer :: a(:)
        !
        call self%col("obstime", a)
        call parquet_table_test_check_row(self, int(i, int64), "obstime")
        p => a(i)
    end function parquet_table_test_obstime_at_i32
    !
    !> A millisecond time column. (time) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_obstime_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        type(parquet_time), pointer :: p(:) !! alias to that range's storage.
        type(parquet_time), pointer :: a(:)
        !
        call self%col("obstime", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "obstime")
        p => a(lo:hi)
    end function parquet_table_test_obstime_rng_i32
    !
    !> A millisecond time column. (time) Row `i` alone. Repeats the column lookup on every call -- take `%obstime()` once instead
    !! when iterating.
    function parquet_table_test_obstime_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        type(parquet_time), pointer :: p !! alias to that row's storage.
        type(parquet_time), pointer :: a(:)
        !
        call self%col("obstime", a)
        call parquet_table_test_check_row(self, i, "obstime")
        p => a(i)
    end function parquet_table_test_obstime_at_i64
    !
    !> A millisecond time column. (time) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_obstime_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        type(parquet_time), pointer :: p(:) !! alias to that range's storage.
        type(parquet_time), pointer :: a(:)
        !
        call self%col("obstime", a)
        call parquet_table_test_check_range(self, lo, hi, "obstime")
        p => a(lo:hi)
    end function parquet_table_test_obstime_rng_i64
    !
    !> A UTC microsecond timestamp column. (timestamp) The whole column. Points into the live storage: zero copy, writable, and
    !! invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_obsstamp_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        type(parquet_timestamp), pointer :: p(:) !! alias to the column's storage.
        !
        call self%col("obsstamp", p)
    end function parquet_table_test_obsstamp_all
    !
    !> A UTC microsecond timestamp column. (timestamp) Row `i` alone. Repeats the column lookup on every call -- take `%obsstamp()`
    !! once instead when iterating.
    function parquet_table_test_obsstamp_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        type(parquet_timestamp), pointer :: p !! alias to that row's storage.
        type(parquet_timestamp), pointer :: a(:)
        !
        call self%col("obsstamp", a)
        call parquet_table_test_check_row(self, int(i, int64), "obsstamp")
        p => a(i)
    end function parquet_table_test_obsstamp_at_i32
    !
    !> A UTC microsecond timestamp column. (timestamp) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy,
    !! writable, and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_obsstamp_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        type(parquet_timestamp), pointer :: p(:) !! alias to that range's storage.
        type(parquet_timestamp), pointer :: a(:)
        !
        call self%col("obsstamp", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "obsstamp")
        p => a(lo:hi)
    end function parquet_table_test_obsstamp_rng_i32
    !
    !> A UTC microsecond timestamp column. (timestamp) Row `i` alone. Repeats the column lookup on every call -- take `%obsstamp()`
    !! once instead when iterating.
    function parquet_table_test_obsstamp_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        type(parquet_timestamp), pointer :: p !! alias to that row's storage.
        type(parquet_timestamp), pointer :: a(:)
        !
        call self%col("obsstamp", a)
        call parquet_table_test_check_row(self, i, "obsstamp")
        p => a(i)
    end function parquet_table_test_obsstamp_at_i64
    !
    !> A UTC microsecond timestamp column. (timestamp) Rows `lo` through `hi` inclusive. Points into the live storage: zero copy,
    !! writable, and invalidated by any row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_obsstamp_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        type(parquet_timestamp), pointer :: p(:) !! alias to that range's storage.
        type(parquet_timestamp), pointer :: a(:)
        !
        call self%col("obsstamp", a)
        call parquet_table_test_check_range(self, lo, hi, "obsstamp")
        p => a(lo:hi)
    end function parquet_table_test_obsstamp_rng_i64
    !
    !> A string vector column, which has only the character copy-out form. (string, width 2) Copied out as a character array sized
    !! to the longest value present. A null element comes back blank -- gate on %is_null to tell a null from a genuinely empty
    !! string.
    subroutine parquet_table_test_tags_chr(self, arr)
        class(parquet_table_test), intent(in) :: self !! the table.
        character(len=:), allocatable, intent(out) :: arr(:,:) !! (element, row).
        !
        call self%get("tags", arr)
    end subroutine parquet_table_test_tags_chr
    !
    !> Filled in by the program, never read from the file. Unit: Jy. (float64) Computed: no file column is read for it. The whole
    !! column. Points into the live storage: zero copy, writable, and invalidated by any row-structural mutation (%filter_rows,
    !! %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_flux_all(self) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        real(real64), pointer :: p(:) !! alias to the column's storage.
        !
        call self%col("flux", p)
    end function parquet_table_test_flux_all
    !
    !> Filled in by the program, never read from the file. Unit: Jy. (float64) Computed: no file column is read for it. Row `i`
    !! alone. Repeats the column lookup on every call -- take `%flux()` once instead when iterating.
    function parquet_table_test_flux_at_i32(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: i !! 1-based row index within this table.
        real(real64), pointer :: p !! alias to that row's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("flux", a)
        call parquet_table_test_check_row(self, int(i, int64), "flux")
        p => a(i)
    end function parquet_table_test_flux_at_i32
    !
    !> Filled in by the program, never read from the file. Unit: Jy. (float64) Computed: no file column is read for it. Rows `lo`
    !! through `hi` inclusive. Points into the live storage: zero copy, writable, and invalidated by any row-structural mutation
    !! (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_flux_rng_i32(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int32), intent(in) :: lo !! first row of the range (1-based).
        integer(int32), intent(in) :: hi !! last row of the range (inclusive).
        real(real64), pointer :: p(:) !! alias to that range's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("flux", a)
        call parquet_table_test_check_range(self, int(lo, int64), int(hi, int64), "flux")
        p => a(lo:hi)
    end function parquet_table_test_flux_rng_i32
    !
    !> Filled in by the program, never read from the file. Unit: Jy. (float64) Computed: no file column is read for it. Row `i`
    !! alone. Repeats the column lookup on every call -- take `%flux()` once instead when iterating.
    function parquet_table_test_flux_at_i64(self, i) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: i !! 1-based row index within this table.
        real(real64), pointer :: p !! alias to that row's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("flux", a)
        call parquet_table_test_check_row(self, i, "flux")
        p => a(i)
    end function parquet_table_test_flux_at_i64
    !
    !> Filled in by the program, never read from the file. Unit: Jy. (float64) Computed: no file column is read for it. Rows `lo`
    !! through `hi` inclusive. Points into the live storage: zero copy, writable, and invalidated by any row-structural mutation
    !! (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, %append).
    function parquet_table_test_flux_rng_i64(self, lo, hi) result(p)
        class(parquet_table_test), intent(in), target :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range (1-based).
        integer(int64), intent(in) :: hi !! last row of the range (inclusive).
        real(real64), pointer :: p(:) !! alias to that range's storage.
        real(real64), pointer :: a(:)
        !
        call self%col("flux", a)
        call parquet_table_test_check_range(self, lo, hi, "flux")
        p => a(lo:hi)
    end function parquet_table_test_flux_rng_i64
    !
    !> error stops unless `i` is a row this table has. A pointer to an out-of-range element would be undefined behaviour rather than
    !! a wrong answer, so this is checked rather than trusted.
    subroutine parquet_table_test_check_row(self, i, name)
        class(parquet_table_test), intent(in) :: self !! the table.
        integer(int64), intent(in) :: i !! the 1-based row index to check.
        character(len=*), intent(in) :: name !! column name, for the message.
        !
        if (i < 1_int64 .or. i > self%nrows()) then
            error stop "parquet_table_test: row index out of range for column '" // trim(name) // "'"
        end if
    end subroutine parquet_table_test_check_row
    !
    !> error stops unless `[lo, hi]` is a row range this table has. An empty range (`hi < lo`) is accepted and yields a zero-length
    !! pointer, matching Fortran's own section rules.
    subroutine parquet_table_test_check_range(self, lo, hi, name)
        class(parquet_table_test), intent(in) :: self !! the table.
        integer(int64), intent(in) :: lo !! first row of the range.
        integer(int64), intent(in) :: hi !! last row of the range.
        character(len=*), intent(in) :: name !! column name, for the message.
        !
        if (hi < lo) return
        if (lo < 1_int64 .or. hi > self%nrows()) then
            error stop "parquet_table_test: row range out of range for column '" // trim(name) // "'"
        end if
    end subroutine parquet_table_test_check_range
    !
    ! >>>>> USER SECTION (procedures) -- your own procedure bodies; preserved on regeneration
    ! >>>>> END USER SECTION (procedures)

end module parquet_table_example ! GCOVR_EXCL_LINE -- gcov attribution artifact
