!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Core reader/writer implementation for Apache Parquet files: schema
!> construction/validation from MAML, writers/readers, row filtering,
!> quality-control (qc:) enforcement, and flat key-value table metadata.
!> Submodules parquet_write.f90/parquet_read.f90/parquet_metadata.f90 hold the
!> bodies of the module procedures declared in the interface block below.
!>
!> **This is an internal implementation module -- do not `use parquet_core`
!> directly.** Everything public here is re-exported by the `parquet` facade
!> module (parquet.f90), which is the one and only supported entry point and
!> the surface the library's semantic-versioning promise covers. `parquet_core`
!> has to stay accessible because the sibling modules (parquet_tables,
!> ...) use it, but its name and contents may change in any release.
module parquet_core
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use parquet_bindings
    ! Default accessibility here is `private`, so the names this brings in are NOT re-exported from
    ! parquet_core -- a user reaches them through the `parquet` facade's own `use parquet_settings`.
    ! What this import is for is the limit aliases a few lines below.
    use parquet_settings, only: parquet_max_filter_rule_len, parquet_max_filter_depth, parquet_max_filter_nodes, &
        parquet_max_sort_keys, parquet_max_sort_key_len
    use parquet_maml_base, only: parquet_maml_file
    use parquet_strings, only: parquet_string_column, parquet_string, parquet_string_column_raw_buffers
    ! parquet_list for the container type the LIST read specifics fill; parquet_columns for the
    ! payload column they build inside it, and the PK_* kinds they map a file's element family
    ! onto. Both are default-private here, so neither is re-exported from parquet_core -- a user
    ! reaches them through the `parquet` facade's own `use parquet_list`/`use parquet_columns`.
    ! The dependency runs THIS WAY ONLY: parquet_list must never import parquet_core, which is
    ! what keeps it (and every tier below it) clear of parquet_bindings and hence of Arrow.
    use parquet_list, only: parquet_list_column, parquet_list_column_offsets, parquet_list_column_payload, &
        parquet_list_column_row_validity
    use parquet_struct, only: parquet_struct_column, parquet_struct_column_field, parquet_struct_column_names, &
        parquet_struct_column_row_validity, parquet_struct_column_build
    use parquet_map, only: parquet_map_column, parquet_map_column_offsets, parquet_map_column_keys, &
        parquet_map_column_values, parquet_map_column_row_validity
    use parquet_columns, only: parquet_column, parquet_column_set_null, parquet_column_string_column, &
        parquet_column_data_ptr, parquet_column_is_null, parquet_kind_name, parquet_container_column, &
        parquet_kind_is_container, &
        PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP, &
        PK_LIST, PK_MAP, PK_STRUCT
    use parquet_temporal, only: parquet_date, parquet_time, parquet_timestamp, &
        parquet_unit_seconds, parquet_unit_millis, parquet_unit_micros, parquet_unit_nanos, &
        parquet_ns_per_sec, parquet_ns_per_day, parquet_ns_to_sec, parquet_ns_to_day
    implicit none
    private
    !
    !> Valid values for a field's data_type in a MAML file, checked by
    !> parquet_validate_maml (parquet_metadata_validate.f90) and schema%add_field
    !> (parquet_metadata.f90) alike -- add new supported types here as needed.
    !> Declared here (rather than in either submodule) so both can see it.
    character(len=7), parameter :: valid_maml_data_types(6) = [character(len=7) :: &
        "int32", "int64", "string", "boolean", "float32", "float64"]

    !> col_size/array_size sentinel for a MAML col_size: auto/array_size: auto declaration:
    !> resolved later, either by schema%set_col_size/%set_array_size before parquet_open_writer,
    !> or automatically from the actual data's own shape at the first
    !> parquet_write_column/parquet_write_column_chunk call able to supply it. col_size needs a
    !> matrix-form write (never the flat/1-D form, which needs col_size already known to interpret
    !> its own row count); array_size is resolved by either shape, from the caller's declared
    !> character length -- so a scalar string column's array_size: auto resolves on an ordinary
    !> 1-D write. See parquet_resolve_or_check_col_size/_array_size in parquet_write.f90.
    integer, parameter :: parquet_size_auto = -1
    !> col_size/array_size sentinel for a malformed MAML col_size:/array_size: value (non-numeric,
    !> or an explicit non-positive number) -- parquet_validate_maml_internal always rejects a
    !> schema carrying this value before it becomes usable, so a caller never actually observes it.
    integer, parameter :: size_invalid_sentinel = -2

    !> The short internal names for the filter and sort-key limits, reached by host association
    !> from parquet_read/parquet_read_filter/parquet_read_sort. Each is an alias *derived from* the
    !> public constant of the same meaning in parquet_settings -- where the value and the reasoning
    !> for it now live -- rather than a second copy of the number, so the two cannot drift. The
    !> short names are kept because the parser code reads better in its own vocabulary; see
    !> doc/pages/operating/settings.md for what a user sees.
    integer, parameter :: filter_max_rule_len = parquet_max_filter_rule_len
    integer, parameter :: filter_max_depth = parquet_max_filter_depth
    integer, parameter :: filter_max_nodes = parquet_max_filter_nodes
    integer, parameter :: sortkey_max_key_len = parquet_max_sort_key_len
    integer, parameter :: sortkey_max_keys = parquet_max_sort_keys

    !> The cap on one parquet_read_qc entry ("col, min, max, miss"), in the same spirit as the
    !> filter and sort caps above. Generous relative to the grammar -- a column name plus three
    !> short bounds -- since the point is to reject accidentally-huge input, not to bound a
    !> reasonable declaration.
    integer, parameter :: read_qc_max_entry_len = 1024

    !> The frozen contract identifier for `parquet_open_reader(..., sample_fraction=)`'s row draw.
    !!
    !! Reading `"sample:bernoulli-u<p/philox/v1"`, it names the mapping below and changes only when
    !! that mapping changes -- which is what lets a program record, alongside its results, the rule
    !! by which its rows were chosen, and lets a test fail loudly if the rule moves without anyone
    !! saying so. Same role as `pf_random_algorithm` plays for the generator itself.
    !!
    !! **The mapping in full**, for physical row `r` (1-based: the row's position in the file as
    !! written, unaffected by any filter, by row-group pruning, and by any row range):
    !!
    !! ```
    !! key     = pf_random_key(seed, parquet_sample_label)
    !! u(r)    = pf_random_at(key, 0_int64, r)
    !! keep(r) = u(r) < sample_fraction
    !! ```
    !!
    !! Two consequences worth knowing. `keep(r)` depends on nothing but the seed and the row, so one
    !! seed selects the same rows however the file is read -- filtered, pruned, chunked or not. And
    !! `sample_fraction = 0.0` keeps nothing by arithmetic rather than by a special case, `u` being
    !! in `[0, 1)`.
    character(len=*), parameter :: parquet_sample_algorithm = "sample:bernoulli-u<p/philox/v1"

    !> Label separating the row sample's stream from every other family drawn from the same seed.
    !!
    !! **Not decoration -- feature_risks.md Risk-123.** Without it, `sample_seed=42_int64` would
    !! draw the very words `pf_random_at(42_int64, 0_int64, ...)` hands a caller, so a program
    !! seeding both from one number would find its sample correlated with its own draws. Every
    !! marginal test still passes in that state; only a joint one sees it.
    !!
    !! The value is arbitrary beyond having to differ from every other label and from 0, and is
    !! public so a caller can reproduce the selection outside the library.
    integer(int64), parameter :: parquet_sample_label = 4994076164431785653_int64

    !> Canonical single data-type tokens parquet_column_exists/parquet_get_column_type recognize:
    !> valid_maml_data_types plus the three temporal base tokens ("date"/"time"/"timestamp").
    !> parquet_column_exists additionally accepts the group aliases "int" (int32/int64), "float"
    !> (float32/float64), and "temporal" (date/time/timestamp).
    character(len=9), parameter :: valid_query_data_types(9) = [character(len=9) :: &
        "int32", "int64", "string", "boolean", "float32", "float64", "date", "time", "timestamp"]

    !> One schema field's write-time metadata and QC bounds, parsed from a
    !> MAML fields: entry (or built via schema%add_field); one array element
    !> per field, held in parquet_column_info%col(:).
    type parquet_column_type
        logical :: is_set = .false. !! true if this column is currently enabled to be written; toggled by
        !! set_column_available/set_column_unavailable.
        logical :: is_deactivated = .false. !! true for columns merged in from a base MAML that the user's MAML
        !! excluded; protects is_set from being changed by set_column_available/set_column_unavailable (bulk or
        !! by name).
        logical :: is_protected = .false. !! true if this column's name is listed under extra: protected_cols: in
        !! whichever MAML built this cinfo; parquet_write_column error stops if an is_valid mask with any
        !! .false. entry is passed for such a column.
        logical :: has_qc_min = .false. !! true if a qc: min: bound was declared for this field.
        logical :: has_qc_max = .false. !! true if a qc: max: bound was declared for this field.
        character(len=2) :: qc_min_op = ">=" !! one of ">", ">="; default when qc: min: has no operator prefix.
        character(len=2) :: qc_max_op = "<=" !! one of "<", "<="; default when qc: max: has no operator prefix.
        character(len=:), allocatable :: qc_min_raw !! The qc: min: bound text, operator prefix already stripped/
        !! trimmed; numeric for int32/int64/float32/float64, literal for string.
        character(len=:), allocatable :: qc_max_raw !! qc: max: bound text, same convention as qc_min_raw.
        character(len=:), allocatable :: qc_miss_raw !! The qc: miss: value exactly as declared (unquoted, trimmed),
        !! kept ONLY so parquet_validate_field_rules can name the offending text when it is not one of the three
        !! legal forms; qc_allow_null below is what every enforcement path actually reads. Left unallocated when
        !! the field declares no qc: miss: at all, which is what distinguishes "not declared" from "declared
        !! empty" here -- qc_allow_null alone cannot, since both leave it .true./.false. by value only.
        logical :: qc_allow_null = .true. !! Whether Nulls are an expected part of this field's output, in which
        !! case parquet_write_column/parquet_open_reader's qc: enforcement never warns/errors about them.
        !! .true. for a declared qc: miss: Null/NA AND -- this is what the default carries -- for a field that
        !! declares no qc: miss: at all, since an undeclared miss: means the author said nothing about Nulls
        !! and the library does not invent a restriction. Only an EXPLICIT, EMPTY qc: miss: sets this .false.,
        !! which is how a schema asks for Null validation; finding a Null then triggers a qc: violation.
        !! **The .true. default is load-bearing and is the only inverted default in this type**: the "no miss:
        !! declared" case works by nothing ever assigning this, so flipping it back to .false. would silently
        !! turn Null validation on for every column of every schema. May be set on any data_type.
        character(len=:), allocatable :: name      !! The name of the field [required]; always the internal/
        !! canonical name, i.e. what parquet_write_column/set_column_available/etc. use -- never affected by a
        !! col_map: rename (see output_name).
        character(len=:), allocatable :: unit      !! The unit of measurement for the field.
        character(len=:), allocatable :: info      !! A short description of the field.
        character(len=:), allocatable :: ucd       !! Unified Content Descriptor for IVOA (can have many).
        character(len=:), allocatable :: data_type !! The data type of the field [required]. For a temporal
        !! column this is the base token only ("date"/"time"/"timestamp"); the unit/utc suffix is parsed out
        !! into time_unit/is_utc below.
        integer :: time_unit = 0 !! For a time/timestamp field, its stored unit as a parquet_unit_* selector
        !! (0 = not a temporal column, or unset). A bare time/timestamp token resolves to parquet_unit_micros.
        logical :: is_utc = .false. !! For a timestamp field, .true. if declared UTC-adjusted (timestamp[...,utc]).
        integer :: array_size = 1 !! Maximum length of character strings; parquet_size_auto if declared
        !! array_size: auto (not yet resolved -- see schema%set_array_size).
        integer :: col_size = 1   !! The number of elements in the vector column; parquet_size_auto if
        !! declared col_size: auto (not yet resolved -- see schema%set_col_size).
        character(len=:), allocatable :: output_name !! The name actually written to the parquet file/VOTable
        !! header. Equal to `name` unless a col_map: entry in the MAML that declared this field renamed it
        !! (col_map: maps internal_name -> output_name; `name` is then set to the internal_name and
        !! `output_name` keeps the field's own declared name from that MAML's fields: section).
    end type parquet_column_type

    !> Column-level schema state for one MAML: an array of parquet_column_type,
    !> one per declared field, plus lookups/toggles over it. Embedded in
    !> parquet_schema%cinfo; parquet_schema's own type-bound procedures are
    !> flat passthroughs to the ones here.
    type parquet_column_info
        type(parquet_column_type), allocatable :: col(:) !! One entry per declared field, in MAML source order.
    contains
        procedure :: get_column_index !! 1-based index of a column by name; error stops if not found.
        procedure :: is_column_set !! Whether a column is currently enabled to be written; error stops if not found.
        procedure :: get_num_fields !! Total number of declared fields.
        procedure :: get_field_name !! Field name at a given 1-based MAML source position.
        procedure :: get_field_by_name !! Full field definition by name; error stops if not found.
        procedure :: get_field_by_index !! Full field definition by 1-based MAML source position.
        generic :: get_field => get_field_by_name, get_field_by_index !! Reads back a field's full,
        !! add_field-equivalent definition, by name or by 1-based source position.
        ! Exposed as set_column_available/set_column_unavailable to match the
        ! same-named methods on parquet_schema (the primary public API); the
        ! backing module procedures keep the shorter set_available/set_unavailable
        ! names only to avoid colliding with parquet_schema's own impls.
        procedure :: set_column_unavailable => set_unavailable !! Disables a column, or every column if no name is given.
        procedure :: set_column_available => set_available !! Enables a column, or every column if no name is given.
        procedure :: set_col_size !! Resolves a column's col_size (only if currently "auto" unless force=.true.).
        procedure :: set_protected !! Marks/unmarks a column Null-protected (extra: protected_cols:).
        procedure :: set_array_size !! Resolves a string column's array_size (only if currently "auto" unless force=.true.).
    end type parquet_column_info

    !> One flat key-value table-metadata entry (a keyarray: item on the write
    !> side, or one row of parquet_reader%metadata on the read side).
    type parquet_metadata_entry
        character(len=:), allocatable :: key !! Metadata key.
        character(len=:), allocatable :: value !! Value, always stored as plain text (see add_metadata).
        character(len=:), allocatable :: description !! Optional free-text description; never populated on read.
        !> Type token the typed %add_metadata overload recorded for %value ("int32", "float64[]",
        !! ...); unallocated/blank for the string overload, for a MAML-declared keyarray: item (a
        !! MAML-declared value is a string by design) and for every entry built on the read side.
        !! Written to the file as a companion "<key>.datatype" key-value entry by
        !! build_file_metadata (parquet_wrapper.cpp) -- never as an entry of %items itself, so
        !! %add_metadata still appends exactly one item per call.
        character(len=:), allocatable :: datatype
    end type parquet_metadata_entry

    !> Flat key-value table metadata: one array of parquet_metadata_entry plus
    !> the add_metadata family of type-bound procedures that append to it (one
    !> specific per supported type/kind, dispatched via the add_metadata generic).
    type parquet_table_metadata
        type(parquet_metadata_entry), allocatable :: items(:) !! One entry per add_metadata call (or MAML keyarray: item).
        !> Verbatim source MAML lines, populated by parquet_read_maml; used by
        !> parquet_open_writer(..., write_maml=.true.) to save a sidecar .maml
        !> file next to the .parquet output. add_metadata calls made after
        !> parquet_read_maml append a new keyarray: entry to these lines (see
        !> parquet_append_keyarray_line), so the sidecar reflects them; it is
        !> NOT kept in sync with which columns end up enabled/written, though.
        character(len=:), allocatable :: source_maml_lines(:)
        !> Count of %items present immediately after the most recent parquet_parse_maml
        !> (table:/survey:/... header keys plus any real MAML keyarray: entries) -- the
        !> "base" metadata %clear_metadata preserves, discarding only entries appended by
        !> %add_metadata calls made after that parse. 0 for a never-parsed table_metadata.
        integer :: n_base_items = 0
    contains
        procedure :: clear_metadata => metadata_clear_metadata !! Discards %add_metadata entries added
        !! after the most recent parse, keeping the base (header keys + keyarray:) entries.
        procedure :: add_metadata_int32 !! int32 specific.
        procedure :: add_metadata_int64 !! int64 specific.
        procedure :: add_metadata_float32 !! float32 specific.
        procedure :: add_metadata_float64 !! float64 specific.
        procedure :: add_metadata_logical !! logical specific.
        procedure :: add_metadata_string !! string specific.
        procedure :: add_metadata_int32_array !! int32 array specific.
        procedure :: add_metadata_int64_array !! int64 array specific.
        procedure :: add_metadata_float32_array !! float32 array specific.
        procedure :: add_metadata_float64_array !! float64 array specific.
        procedure :: add_metadata_logical_array !! logical array specific.
        procedure :: add_metadata_string_array !! string array specific.
        !> Appends one flat key-value metadata entry; dispatched by value's type/kind/rank.
        generic :: add_metadata => add_metadata_int32, add_metadata_int64, add_metadata_float32, &
                                    add_metadata_float64, add_metadata_logical, add_metadata_string, &
                                    add_metadata_int32_array, add_metadata_int64_array, add_metadata_float32_array, &
                                    add_metadata_float64_array, add_metadata_logical_array, add_metadata_string_array
    end type parquet_table_metadata

    !> Bundles a parsed MAML source together with the column schema (cinfo)
    !> and table metadata (metadata) that parquet_parse_maml derives from it,
    !> so a single variable carries everything parquet_open_writer needs.
    !> %cinfo and %metadata are public: their fields (col(:), items(:)) and
    !> own type-bound procedures stay directly reachable, and the procedures
    !> below are flat convenience passthroughs (schema%set_column_available("id")
    !> instead of schema%cinfo%set_column_available("id")).
    !>
    !> On the read side a qc-maml is populated into %maml alone -- either by
    !> parquet_load_qc_maml_file or schema%add_col_qc -- and is never run
    !> through parquet_parse_maml (a qc-maml has no data_type and would fail
    !> the full schema validation), so %cinfo/%metadata stay empty;
    !> parquet_open_reader consumes only %maml.
    type parquet_schema
        type(parquet_maml_file)      :: maml     !! Raw MAML source plus (once parsed) missing-columns/col_map.
        type(parquet_column_info)    :: cinfo    !! Per-field schema/QC state, derived from %maml by parquet_parse_maml.
        type(parquet_table_metadata) :: metadata !! Flat key-value table metadata, derived from %maml's keyarray:.
        !> Set by %init: guards %add_field, which otherwise has no "fields:"
        !> header (or even a %maml at all) to append to. Only %init and
        !> %add_field are meaningful for a schema being built from scratch in
        !> memory -- a schema populated via parquet_parse_maml (from a real
        !> MAML file/object) never calls %init and never needs %add_field.
        logical, private :: is_initialized = .false.
    contains
        procedure :: init => schema_init !! Initializes a from-scratch schema (table: key + optional metadata).
        procedure :: is_init => schema_is_init !! Whether this schema is ready to use (via %init or a MAML parse).
        procedure :: is_parsed => schema_is_parsed !! Whether %cinfo has actually been populated by parquet_parse_maml.
        procedure :: clear => schema_clear !! Resets the entire schema back to its pristine,
        !! just-declared (never-initialized) state.
        procedure :: add_field => schema_add_field !! Appends one fields: entry to a from-scratch schema.
        procedure :: add_field_from => schema_add_field_from !! Copies one field's definition from another
        !! (already-parsed) schema and appends it here via %add_field.
        procedure :: set_column_available !! Enables a column, or every column if no name is given.
        procedure :: set_column_unavailable !! Disables a column, or every column if no name is given.
        procedure :: set_col_size => schema_set_col_size !! Resolves a column's col_size before parquet_open_writer.
        procedure :: set_protected => schema_set_protected !! Marks/unmarks a column Null-protected.
        procedure :: set_array_size => schema_set_array_size !! Resolves a string column's array_size before
        !! parquet_open_writer.
        procedure :: get_column_index => schema_get_column_index !! 1-based index of a column by
        !! name; error stops if not found.
        procedure :: is_column_set => schema_is_column_set !! Whether a column is currently enabled to be
        !! written; error stops if not found.
        procedure :: get_num_fields => schema_get_num_fields !! Total number of declared fields.
        procedure :: get_field_name => schema_get_field_name !! Field name at a given 1-based MAML source position.
        procedure :: get_field_by_name => schema_get_field_by_name !! Full field definition by name; error
        !! stops if not found.
        procedure :: get_field_by_index => schema_get_field_by_index !! Full field definition by 1-based
        !! MAML source position.
        generic :: get_field => get_field_by_name, get_field_by_index !! Reads back a field's full,
        !! add_field-equivalent definition, by name or by 1-based source position; forwards to %cinfo%get_field.
        procedure :: print_schema_info => schema_print_schema_info !! Writes a "Table name:" line plus an aligned
        !! name/unit/type/len/ucd/info listing of enabled (is_set) columns to a unit/file.
        ! add_col_qc/set_col_qc build a read-time qc-maml. Two intentional
        ! naming choices here: (1) "col_qc" is a deliberate domain abbreviation
        ! for "column quality-control" (the qc: block of a fields: entry) --
        ! kept short because it appears in every qc-building call. (2) set_col_qc
        ! is named set_ (not get_) precisely because it MUTATES the schema (it
        ! appends the entry, like add_col_qc): the set_ form exists so a caller
        ! can reuse one variable in place (call schema%set_col_qc(col) -- col holds
        ! the qc_input string on entry, the parsed column name on exit) instead of
        ! separately naming an input and an output variable the way add_col_qc's
        ! optional col_name argument requires. It is a builder that also returns
        ! the name, not a pure query -- and, being a subroutine, never returns
        ! character(len=:), allocatable as a function result (see "Build and
        ! compiler notes" in CLAUDE.md for why that matters).
        procedure :: add_col_qc => schema_add_col_qc !! Appends one qc: field entry from a compact string.
        procedure :: set_col_qc => schema_set_col_qc !! In-place form of %add_col_qc; parses the name into its argument.
        procedure :: schema_add_metadata_int32 !! int32 specific.
        procedure :: schema_add_metadata_int64 !! int64 specific.
        procedure :: schema_add_metadata_float32 !! float32 specific.
        procedure :: schema_add_metadata_float64 !! float64 specific.
        procedure :: schema_add_metadata_logical !! logical specific.
        procedure :: schema_add_metadata_string !! string specific.
        procedure :: schema_add_metadata_int32_array !! int32 array specific.
        procedure :: schema_add_metadata_int64_array !! int64 array specific.
        procedure :: schema_add_metadata_float32_array !! float32 array specific.
        procedure :: schema_add_metadata_float64_array !! float64 array specific.
        procedure :: schema_add_metadata_logical_array !! logical array specific.
        procedure :: schema_add_metadata_string_array !! string array specific.
        !> Appends one flat key-value metadata entry; dispatched by value's type/kind/rank.
        generic :: add_metadata => schema_add_metadata_int32, schema_add_metadata_int64, &
                                    schema_add_metadata_float32, schema_add_metadata_float64, &
                                    schema_add_metadata_logical, schema_add_metadata_string, &
                                    schema_add_metadata_int32_array, schema_add_metadata_int64_array, &
                                    schema_add_metadata_float32_array, schema_add_metadata_float64_array, &
                                    schema_add_metadata_logical_array, schema_add_metadata_string_array
        procedure :: clear_metadata => schema_clear_metadata !! Forwards to %metadata%clear_metadata.
    end type parquet_schema

    !> Overrides the default structure constructor so a schema can be built
    !> in one expression (my_maml = parquet_schema(table="my_table")) as an
    !> alternative to call my_maml%init(table="my_table"); both call
    !> parquet_schema_new/schema_init under the hood. table is the only
    !> required argument (the MAML table: key); survey, dataset, version,
    !> date, author, description, license, and maml_version are all optional
    !> and set the correspondingly-named MAML header key when given. Returns
    !> the newly initialized schema.
    interface parquet_schema
        module procedure parquet_schema_new
    end interface parquet_schema

    !> parquet_writer/parquet_reader own a handle to a C++-side Arrow/Parquet
    !> object with no automatic Fortran cleanup. Always prefer an explicit
    !> parquet_close_writer/parquet_close_reader call; the FINAL procedures
    !> below are only a safety net for a handle that's still open when its
    !> variable goes out of scope or is overwritten (e.g. an early RETURN
    !> between open and close), not a substitute for closing normally -- for a
    !> writer specifically, the safety net skips parquet_close_writer's
    !> completeness checks (so an incomplete write never crashes an implicit
    !> finalizer), meaning the resulting file is not guaranteed valid/complete
    !> unless parquet_close_writer was actually called.
    !>
    !> Do not copy a parquet_writer/parquet_reader (`w2 = w1`, passing one as
    !> a function result, etc.): the handle is a plain c_ptr, so a copy
    !> aliases the same underlying C++ object without any reference counting.
    !> Whichever copy is finalized/closed first frees it out from under the
    !> other, which would then double-free/use-after-free when it is itself
    !> later closed or finalized. Always use a single named writer/reader,
    !> passed by reference (as every procedure in this module already does).
    !> Fully opaque from outside this module: every component is an
    !> implementation detail (the raw C handle, schema bookkeeping, write
    !> tracking) that parquet_write_column/parquet_open_writer/etc. manage
    !> internally. Never referenced directly by any test or consumer -- only
    !> parquet_core.f90's own submodules (parquet_write, parquet_read,
    !> parquet_metadata) need access, which `private` here still allows.
    type parquet_writer
        private
        type(c_ptr) :: handle = c_null_ptr !! Opaque C++ Arrow/Parquet writer handle; c_null_ptr until opened.
        type(parquet_column_type), allocatable :: all_columns(:) !! Every column the schema declares, enabled or not.
        type(parquet_column_type), allocatable :: enabled_columns(:) !! Subset of all_columns currently enabled (is_set).
        integer, allocatable :: write_counts(:) !! Per-enabled-column count of parquet_write_column calls so far.
        !> Per-all_columns longest element (in characters) any parquet_string_column write has
        !> actually written to this column; 0 for a column no such write touched. Unallocated for a
        !> schema-less writer.
        !>
        !> The compact path neither reads nor enforces a declared array_size -- it stores each
        !> element's own bytes, and a reader takes each length from the data -- so a caller may
        !> legitimately write elements longer than the schema declares. What must NOT happen is the
        !> file then advertising the declaration: parquet_close_writer reconciles the two
        !> (parquet_reconcile_string_sizes) so the sidecar .maml and the file's own
        !> column.<name>.array_size describe what was written. Accepting loose input is a choice;
        !> emitting wrong metadata is not.
        integer, allocatable :: observed_string_len(:)
        logical :: is_schema_enforced = .false. !! true when opened with a schema (vs. a schema-less writer).
        logical :: qc = .false. !! defaults to present(schema) (i.e. on whenever a schema is given), overridable
        !! via parquet_open_writer(..., qc=); when true, parquet_write_column checks each column's qc: min/max
        !! (if declared) against its valid (is_valid) elements, and -- only for a column declaring an explicit,
        !! empty qc: miss: -- its Null elements, printing a WARNING (never an error) on violation.
        !! No-op without a schema.
        integer(c_long_long) :: expected_nrows = -1 !! set by the first parquet_write_column call; every later
        !! call must supply this same row count (see parquet_check_row_count), since
        !! Arrow/Parquet requires every column in a table to have equal length.
        character(len=256), allocatable :: written_names(:) !! Schema-less writer only (no cinfo, so
        !! enabled_columns/write_counts below don't exist): every name
        !! parquet_write_column has already written, so a repeat can still be
        !! caught -- see parquet_mark_column_written.
        character(len=:), allocatable :: maml_name !! Set from schema%maml%name by parquet_open_writer
        !! when a schema is given (unallocated for a schema-less writer, or if
        !! the schema's own %maml%name was never set); solely so
        !! parquet_write_column's "column not defined"/"type mismatch" errors
        !! can name which maml the schema came from -- see writer_maml_suffix.
        !> Set by parquet_open_writer from its own `filename` argument, solely
        !> so parquet_close_writer's missing-write error can name the output
        !> file; not used for anything else.
        character(len=:), allocatable :: filename
        !> true when parquet_open_writer(..., write_maml=.true.) was requested -- the sidecar
        !> .maml itself is written by parquet_close_writer (not parquet_open_writer), once every
        !> column's col_size/array_size is guaranteed resolved (no longer "auto"), so the sidecar
        !> always reflects what was actually written to the .parquet file.
        logical :: write_maml_requested = .false.
        !> Pruned (disabled fields removed) working copy of schema%metadata%source_maml_lines,
        !> saved by parquet_open_writer when write_maml_requested; parquet_close_writer rewrites
        !> its col_size:/array_size: values to their final resolved state and writes it out.
        character(len=:), allocatable :: sidecar_lines(:)
        !> True between parquet_new_row_group and its matching parquet_finish_row_group --
        !> mirrors the C++-side flag of the same purpose; kept here too so a
        !> parquet_write_column_chunk call can validate its own row count against
        !> current_row_group_nrows (below) without a round trip into C++.
        logical :: in_row_group = .false.
        !> The `nrows` most recently passed to parquet_new_row_group, valid only while
        !> in_row_group is .true. -- every parquet_write_column_chunk call for the open row
        !> group must supply exactly this many rows.
        integer(c_long_long) :: current_row_group_nrows = 0
        !> Row-filtering ("mask") state -- see parquet_write_row_mask/parquet_write_chunk_row_mask
        !> and doc/pages/io/writing.md's "Filtering rows with a mask". The two masking schemes are
        !> mutually exclusive per writer; only the fields relevant to whichever scheme (if any) is
        !> actually used are ever populated.
        logical, allocatable :: file_mask(:) !! Whole-file mask set by parquet_write_row_mask; unallocated if unused.
        integer(c_long_long) :: mask_cursor = 0 !! Cursor of file_mask positions already claimed by row groups
        !! (parquet_write_row_mask + row groups scheme only); see parquet_new_row_group_impl.
        logical :: mask_used_with_row_groups = .false. !! true once file_mask windowing has been applied by at
        !! least one parquet_new_row_group call -- drives the mask-fully-consumed check at
        !! parquet_close_writer (irrelevant/unused for a pure whole-column masked writer).
        logical, allocatable :: chunk_mask(:) !! Mask applicable to the currently-open row group: an identity
        !! mask by default, a file_mask window (shared-mask scheme), or an explicit
        !! parquet_write_chunk_row_mask call (per-row-group scheme) -- reset every parquet_new_row_group.
        logical :: chunk_mask_set_this_group = .false. !! true once parquet_write_chunk_row_mask has been called
        !! for the currently-open row group; reset by parquet_new_row_group.
        integer :: chunk_mask_scheme = 0 !! 0 = undecided (no row group's first chunk write yet), 1 = per-row-group
        !! masking (parquet_write_chunk_row_mask) used for every row group, 2 = never used -- fixed permanently
        !! at the first parquet_write_column_chunk call of the writer's first row group.
        logical :: row_group_first_write_done = .false. !! true once any parquet_write_column_chunk call has
        !! happened for the currently-open row group; reset by parquet_new_row_group. Rejects a
        !! parquet_write_chunk_row_mask call made too late (after that row group's first chunk write).
        logical :: cpp_row_group_open = .false. !! true once the underlying C++ row group has actually been
        !! opened (parquet_writer_new_row_group) for the currently-open Fortran-level row group; reset by
        !! parquet_new_row_group. The C++ open must be told the row group's post-mask *kept* row count, which
        !! isn't known at parquet_new_row_group time for the per-row-group mask scheme (its mask arrives in a
        !! separate, later call) -- so the open is deferred until the kept count is actually known: immediately
        !! if the shared whole-file mask windowing applies, else at whichever comes first of
        !! parquet_write_chunk_row_mask or this row group's first parquet_write_column_chunk call.
        logical :: row_group_is_empty = .false. !! true once the currently-open row group's post-mask kept
        !! count is known to be exactly 0 -- a zero-row row group has no underlying C++ row group at all (Arrow's
        !! own NewRowGroup requires a positive row count), so every parquet_write_column_chunk call for it must
        !! skip its C++ append entirely (nothing to append into) and parquet_finish_row_group must skip both the
        !! open and the finish. Reset by parquet_new_row_group.
        logical :: any_whole_column_write = .false. !! true once any parquet_write_column call has actually
        !! proceeded (not skipped as disabled) -- parquet_write_chunk_row_mask is unavailable once this is
        !! true (only the whole-file parquet_write_row_mask scheme can mask a writer with whole-column writes).
        logical :: write_started = .false. !! true once the writer's first parquet_write_column or
        !! parquet_new_row_group call happens -- parquet_write_row_mask must be called before this.
    contains
        final :: writer_finalize !! Safety-net close if the writer is still open when it goes out of scope.
    end type parquet_writer

    !> Scope-bound claim on one `parquet_writer`'s concurrency guard, held for a whole write entry
    !> point rather than only for the C++ call at the end of it.
    !>
    !> **Why this exists.** The guard lives in `parquet_wrapper.cpp` and used to be claimed only
    !> once a call crossed the `bind(C)` boundary -- which, on the write path, is the *last*
    !> statement of the entry point. Everything before it is Fortran mutating shared writer state,
    !> above all `parquet_check_and_mark_written_name` growing `writer%written_names` with
    !> allocate/copy/move_alloc. Two threads sharing one writer therefore corrupted the heap before
    !> either reached the guard, so the abort the README promises lost a race to a segfault. The
    !> read path never had the problem: its first act is already a guarded C++ call.
    !>
    !> **Why a FINAL rather than paired enter/leave calls.** These entry points have 64 early
    !> `return`s between them, and a missed release would silently weaken the guard rather than
    !> fail loudly. Fortran finalizes a nonpointer, nonallocatable local immediately before a
    !> `RETURN` or `END` (F2018 7.5.6.3), so declaring one of these as an ordinary local makes the
    !> release automatic on every path -- verified on both gfortran 15 and ifx 2026.1, including a
    !> return out of a loop. Using it costs two lines in an entry point and nothing at the exits:
    !>
    !> ```fortran
    !> type(writer_lock) :: lk   ! among the other local declarations
    !> call check_writer_open(writer)
    !> call lk%claim(writer)     ! released automatically, however this procedure exits
    !> ```
    !>
    !> **It deliberately has no allocatable components.** A finalizable type that has any is the
    !> shape ifx miscompiles when it is declared block-local inside an OpenMP parallel region (see
    !> CLAUDE.md's "Compiler & language gotchas"), and these locks sit in exactly the procedures a
    !> misusing caller invokes from inside one. One `type(c_ptr)` keeps it exempt; keep it that way.
    type writer_lock
        private
        !> Handle whose guard this lock currently holds, or `c_null_ptr` when it holds none --
        !> which is both the initial state and what `%claim` leaves behind for an unopened writer,
        !> so the finalizer can run unconditionally without tracking a separate flag.
        type(c_ptr) :: handle = c_null_ptr
    contains
        procedure :: claim => writer_lock_claim !! Claims `writer`'s guard for this thread until this lock dies.
        final :: writer_lock_release !! Releases the held guard, on every exit path including an early RETURN.
    end type writer_lock

    !> Opaque handle for an open parquet file being read; see parquet_writer's
    !> doc comment above for the shared handle-ownership/no-copy rules.
    type parquet_reader
        private
        type(c_ptr) :: handle = c_null_ptr !! Opaque C++ Arrow/Parquet reader handle; c_null_ptr until opened.
        !> Set by parquet_open_reader from its own `filename` argument, solely
        !> so parquet_get_nrows(..., check_positive=.true.) can name the file
        !> in its error message; not used for anything else.
        character(len=:), allocatable :: filename
        !> Flat key-value table metadata (whatever add_metadata wrote on the
        !> write side), copied once by parquet_open_reader from the underlying
        !> Arrow schema's KeyValueMetadata -- see parquet_reader_get_table_metadata_count
        !> and friends in parquet_bindings.f90. Every parquet_get_metadata call
        !> only scans this in-memory copy; it never re-reads the file. %description
        !> is never populated on this side (Arrow's flat KV store has no
        !> description field), only %key/%value.
        type(parquet_table_metadata) :: metadata
    contains
        final :: reader_finalize !! Safety-net close if the reader is still open when it goes out of scope.
    end type parquet_reader

    !> A row filter for parquet_open_reader/parquet_reader_set_filter: each %add
    !> call contributes one boolean expression over the file's columns, and
    !> several %add calls are AND-combined, i.e. (expr1) and (expr2). One
    !> expression is either a single clause, "<column> <op> [value]" (e.g.
    !> "ra > 180", "id is_not_null"), or clauses combined with and/or/not and
    !> parentheses, e.g. "(ra > 180 and dec <= 0) or id is_null". Keywords are
    !> case-insensitive and bind not > and > or. Null rows follow SQL's
    !> three-valued logic: a comparison against a Null is unknown, unknown never
    !> survives, and is_null/is_not_null are the only way to select on nullness.
    !> A NaN is a value, not a Null: it compares false against >, >=, <, <= and
    !> ==, so it survives /= and any negated comparison; is_nan/is_not_nan
    !> (floating-point columns only) select on it directly.
    !> Rules are unvalidated here -- the expression is parsed, and every clause
    !> validated (column exists, is a scalar column, value is well-formed for
    !> that column's type), once a reader actually applies the filter. See "Row
    !> filtering with parquet_filter" in doc/pages/io/filter-sort-sample.md for
    !> the grammar.
    !>
    !> %remap_column_names(from, to) rewrites the columns the rules refer to, in place, replacing
    !> from(k) with to(k) throughout every rule's expression. It exists for callers that build a
    !> filter in one column-name vocabulary and must apply it in another -- parquet_table does
    !> exactly this, translating a filter written in its own internal names into the file's
    !> physical names before handing it to a reader. It is a pure text operation with no schema
    !> access: a name in `from` that no rule mentions is a no-op, and a rule naming a column
    !> absent from `from` is left alone rather than rejected, since only the caller knows which
    !> names are supposed to exist. A rule that does not parse is also left untouched, so a
    !> malformed rule is still reported by the reader that applies it, with the file named.
    type parquet_filter
        !> Raw, unvalidated rule text, one entry per %add call. Deferred-length: every entry
        !! shares the length of the longest rule added so far (%add grows it as needed), so a
        !! parenthesised multi-clause expression is not constrained by a fixed component width.
        character(len=:), allocatable :: rules(:)
        integer :: n = 0 !! Number of rules actually in use.
    contains
        procedure :: add => parquet_filter_add !! Appends one AND-combined filter expression.
        !> Renames the columns every rule refers to, in place: `from(k)` becomes `to(k)`.
        procedure :: remap_column_names => parquet_filter_remap_column_names
    end type parquet_filter

    !> A read-time sort specification: an ordered list of sort KEYS, each naming one column and
    !> the direction to order it by. Passed as parquet_open_reader(..., sort_by=), or applied to
    !> an already-open reader with parquet_reader_set_sort. Every column read afterwards comes
    !> back in that order.
    !>
    !> One key per %add call, applied in the order added (first key is the primary one):
    !>
    !>     type(parquet_sortkey) :: srt
    !>     call srt%add("ra asc")
    !>     call srt%add("dec desc")
    !>     call srt%add("id", nulls_first=.true.)
    !>
    !> Key text is "<column> [asc|desc]" -- the direction is optional and defaults to ascending,
    !> is case-insensitive, and a leading "-" on the column name is shorthand for descending
    !> ("-dec" == "dec desc"). The column may be a dotted struct-leaf path, exactly as a filter
    !> clause's may.
    !>
    !> Nulls sort LAST by default, per key; pass nulls_first=.true. on %add to put that key's
    !> null rows first instead. NaN sits between real values and nulls (so, by default: values,
    !> then NaNs, then nulls) and, like nulls, its placement is absolute -- ordering a key
    !> descending reverses the values, not where nulls and NaNs go. This reproduces Arrow's own
    !> sort ordering exactly, so a result cross-checked against pyarrow matches row for row.
    !>
    !> Keys are unvalidated here -- the column must exist and be a sortable scalar column, which
    !> is checked once a reader actually applies the sort. See "Reading rows in sorted order with
    !> parquet_sortkey" in doc/pages/io/filter-sort-sample.md.
    !>
    !> The type is named for what it holds (the keys), not for the operation; a sorted read is
    !> requested through parquet_open_reader/parquet_reader_set_sort.
    !>
    !> %remap_column_names(from, to) rewrites the column each key orders by, in place, replacing
    !> from(k) with to(k); each key's direction and nulls_first setting are carried across
    !> unchanged. It is the sort twin of parquet_filter%remap_column_names and follows the same
    !> rules -- see that type's doc comment.
    type parquet_sortkey
        !> Raw, unvalidated key text, one entry per %add call. Deferred-length, the same way
        !! parquet_filter%rules is: every entry shares the length of the longest key added so far.
        character(len=:), allocatable :: keys(:)
        !> Per key: .true. to place that key's null rows before its values instead of after.
        !! Same extent as keys(1:n).
        logical, allocatable :: nulls_first(:)
        integer :: n = 0 !! Number of keys actually in use.
    contains
        procedure :: add => parquet_sortkey_add !! Appends one sort key, applied after those already added.
        !> Renames the column every key orders by, in place: `from(k)` becomes `to(k)`.
        procedure :: remap_column_names => parquet_sortkey_remap_column_names
    end type parquet_sortkey

    !> Read-time quality control declared in CODE rather than in a MAML file: one column per %add
    !> call, in exactly the compact "col, min, max, miss" string parquet_schema%add_col_qc already
    !> takes, so there is one read-time-QC grammar in this library rather than two. See
    !> "Building a qc-maml in code" in doc/pages/schema/quality-control.md for the field syntax -- the
    !> operator prefixes (>, >=, <, <=) and the Null/NA/empty miss: convention are inherited from
    !> %add_col_qc verbatim.
    !>
    !>     type(parquet_read_qc) :: qc
    !>     call qc%add("mass, >0, <=1000, Null")
    !>     call qc%add("flag, , , NA")
    !>
    !> Why this exists alongside %add_col_qc, which can already declare the same thing: %add_col_qc
    !> emits MAML text into a schema immediately, whereas a caller composing a read may need to
    !> hold its declarations UNRESOLVED -- so that the column names can be translated, and the
    !> declarations weighed against a MAML's own, only once the file is known. parquet_table is the
    !> motivating case; parquet_compose_read_qc is where the two sources are merged.
    !>
    !> Entries are unvalidated here -- exactly like parquet_filter%add and parquet_sortkey%add,
    !> every check happens when the entry is actually composed into a schema.
    type parquet_read_qc
        !> Raw, unvalidated entry text, one per %add call. Deferred-length: every entry shares the
        !! length of the longest added so far, the same way parquet_filter%rules does.
        character(len=:), allocatable :: entries(:)
        integer :: n = 0 !! Number of entries actually in use.
    contains
        procedure :: add => parquet_read_qc_add !! Appends one "col, min, max, miss" declaration.
        !> Renames the column each entry declares, in place: `from(k)` becomes `to(k)`.
        procedure :: remap_column_names => parquet_read_qc_remap_column_names
    end type parquet_read_qc

    !> Internal plumbing only (not part of the public API): one column's
    !> read-time QC declaration, parsed from a qc-maml's fields: entries by
    !> parquet_parse_qc_maml. min_text/max_text carry the qc: min:/max: value
    !> verbatim (operator prefix, if any, already stripped and captured in
    !> min_op/max_op by parquet_set_qc_bound) -- parsed against the actual
    !> Arrow column type only when a check runs, in parquet_wrapper.cpp's
    !> run_qc_range_check, never against a maml-declared data_type (this
    !> maml doesn't even require one). See "Row filtering" for the analogous
    !> convention parquet_filter already uses for its own rule values.
    type :: parquet_qc_rule
        character(len=64) :: name = "" !! Column this rule applies to.
        !> True only if this field had a qc: sub-block at all (even an empty
        !> one) -- a field with just a name: and no qc: gets no rule at all,
        !> the same as a field never mentioned in the maml (see
        !> parquet_parse_qc_maml); parquet_apply_qc filters these out before
        !> ever reaching parquet_reader_set_qc.
        logical :: has_qc_block = .false.
        logical :: has_min = .false. !! true if a qc: min: bound was declared.
        character(len=2) :: min_op = ">=" !! one of ">", ">=".
        character(len=256) :: min_text = "" !! qc: min: value, verbatim, operator prefix stripped.
        logical :: has_max = .false. !! true if a qc: max: bound was declared.
        character(len=2) :: max_op = "<=" !! one of "<", "<=".
        character(len=256) :: max_text = "" !! qc: max: value, verbatim, operator prefix stripped.
        logical :: null_values_allowed = .true. !! Whether Nulls are expected for this field, mirroring
        !! parquet_column_type%qc_allow_null: .true. for a qc: miss: Null/NA and for a field declaring no
        !! qc: miss: at all (the default -- an undeclared miss: says nothing about Nulls), .false. only for an
        !! explicit, empty qc: miss:, which is how a qc-maml asks for Null validation. The .true. default is
        !! load-bearing for the same reason it is on qc_allow_null; do not "normalize" it to .false.
    end type parquet_qc_rule

    !> Writes one column's values to an open parquet_writer (writer), under
    !> the given column name (name), dispatched by the actual/declared
    !> type/kind of `values` (scalar values(:), one value per row; or
    !> matrix/vector values(:,:), (element, row) values). is_valid (optional)
    !> marks per-element nulls (matching values' rank); a .false. entry for a
    !> protected column (see parquet_column_type%is_protected) error stops.
    !>
    !> A scalar "string" column can also be passed as a `type(parquet_string_column)` (see
    !> module parquet_strings, doc/pages/types/string-columns.md) instead of a padded
    !> character(len=...) array -- this specific carries its own per-element null status (see
    !> `%append_null`/`%is_null`), so `is_valid` is not accepted for it; every null already in the
    !> column is written as a Parquet Null directly. Scalar (1-D) columns only -- there is no
    !> vector/matrix parquet_string_column specific, unlike every other type family above.
    interface parquet_write_column
        module procedure parquet_write_int32_column
        module procedure parquet_write_int32_matrix_column
        module procedure parquet_write_int64_column
        module procedure parquet_write_int64_matrix_column
        module procedure parquet_write_float32_column
        module procedure parquet_write_float32_matrix_column
        module procedure parquet_write_float64_column
        module procedure parquet_write_float64_matrix_column
        module procedure parquet_write_logical_column
        module procedure parquet_write_logical_matrix_column
        module procedure parquet_write_string_column
        module procedure parquet_write_string_matrix_column
        module procedure parquet_write_string_column_compact
        module procedure parquet_write_list_column
        module procedure parquet_write_struct_column
        module procedure parquet_write_map_column
        module procedure parquet_write_date_column
        module procedure parquet_write_date_matrix_column
        module procedure parquet_write_time_column
        module procedure parquet_write_time_matrix_column
        module procedure parquet_write_timestamp_column
        module procedure parquet_write_timestamp_matrix_column
    end interface parquet_write_column

    !> Starts a new row group of `nrows` rows on `writer` -- see parquet_write_column_chunk
    !> below. Every column already known to `writer` (whether schema-declared or already
    !> chunk-written for an earlier row group) must then receive exactly one
    !> parquet_write_column_chunk call for this row group (or already be a whole column from an
    !> earlier parquet_write_column call) before parquet_finish_row_group. Error stops if a row
    !> group is already open (i.e. called again without an intervening parquet_finish_row_group)
    !> rather than silently abandoning the still-open one.
    interface parquet_new_row_group
        module procedure parquet_new_row_group_int32
        module procedure parquet_new_row_group_int64
    end interface parquet_new_row_group

    !> Writes one column's values for the currently-open row group (see parquet_new_row_group)
    !> to an open parquet_writer (writer), under the given column name (name) -- the streaming
    !> counterpart to parquet_write_column, for a large column you build and write one row group
    !> at a time instead of as one complete array. Dispatched by the actual/declared type/kind of
    !> `values`, same as parquet_write_column (scalar values(:) or matrix/vector values(:,:)),
    !> and every column's chunk for the currently-open row group must have exactly
    !> parquet_new_row_group's own `nrows` rows. A schema-enforced column's values are converted to
    !> its declared data_type exactly as parquet_write_column converts them (int32/int64/float32/
    !> float64 in any combination; a float-to-integer conversion error stops on a non-integral or
    !> out-of-range value, per chunk), so the same values are accepted whichever path writes them --
    !> see parquet_append_as_schema_chunk_int32. A kind the declared type is not compatible with at
    !> all (a logical chunk into an int32 column, say) is still an error stop. A column written
    !> once via parquet_write_column can never also be written via parquet_write_column_chunk
    !> (or vice versa), and every column must appear in the *first* row group written for this
    !> writer, since a Parquet file's schema is fixed from that point on.
    !>
    !> A scalar "string" column can also be passed as a `type(parquet_string_column)` holding
    !> exactly this row group's rows -- see parquet_write_column's own doc comment above for the
    !> shared parquet_string_column notes (no is_valid, scalar only).
    interface parquet_write_column_chunk
        module procedure parquet_write_int32_column_chunk
        module procedure parquet_write_int32_matrix_column_chunk
        module procedure parquet_write_int64_column_chunk
        module procedure parquet_write_int64_matrix_column_chunk
        module procedure parquet_write_float32_column_chunk
        module procedure parquet_write_float32_matrix_column_chunk
        module procedure parquet_write_float64_column_chunk
        module procedure parquet_write_float64_matrix_column_chunk
        module procedure parquet_write_logical_column_chunk
        module procedure parquet_write_logical_matrix_column_chunk
        module procedure parquet_write_string_column_chunk
        module procedure parquet_write_string_matrix_column_chunk
        module procedure parquet_write_string_column_chunk_compact
        module procedure parquet_write_list_column_chunk
        module procedure parquet_write_struct_column_chunk
        module procedure parquet_write_map_column_chunk
        module procedure parquet_write_date_column_chunk
        module procedure parquet_write_date_matrix_column_chunk
        module procedure parquet_write_time_column_chunk
        module procedure parquet_write_time_matrix_column_chunk
        module procedure parquet_write_timestamp_column_chunk
        module procedure parquet_write_timestamp_matrix_column_chunk
    end interface parquet_write_column_chunk

    !> Sets a whole-file row filter ("mask") on `writer`: `mask(i) = .false.` drops row `i`
    !> entirely from every column written from this point on (no trace at all -- no offset, no
    !> validity bit; this is not the same as writing a Null). Must be called after
    !> parquet_open_writer and before the writer's first parquet_write_column or
    !> parquet_new_row_group call, whichever comes first.
    !>
    !> For a writer using only parquet_write_column (no row groups): every subsequent
    !> parquet_write_column call's `values` (or the row dimension of `values(:,:)`) must have
    !> length `size(mask)` exactly, and the resulting column has `count(mask)` rows.
    !>
    !> For a writer using parquet_new_row_group/parquet_write_column_chunk: each
    !> parquet_new_row_group(writer, nrows) call automatically claims the next `nrows` positions
    !> of `mask` (in file order) as that row group's window, and every column's
    !> parquet_write_column_chunk call for that row group must supply `values` of length exactly
    !> `nrows`; the row group's actual written row count is `count()` of its window's mask slice
    !> (which may be anywhere from 0 to `nrows`). `mask` must be fully consumed by the writer's
    !> row groups by the time it closes (`error stop` at parquet_close_writer otherwise). See
    !> doc/pages/io/writing.md's "Filtering rows with a mask" for a worked example.
    !>
    !> Mutually exclusive with parquet_write_chunk_row_mask on the same writer -- once either has
    !> been used, calling the other is an `error stop`. Callable at most once per writer -- a
    !> second call is an `error stop` rather than silently replacing the first mask.
    interface parquet_write_row_mask
        module procedure parquet_write_row_mask_impl
    end interface parquet_write_row_mask

    !> Sets the row filter ("mask") for the currently-open row group only, for a writer using
    !> exclusively parquet_write_column_chunk (no parquet_write_column calls anywhere in its
    !> lifetime). Must be called once per row group, after that row group's parquet_new_row_group
    !> and before its first parquet_write_column_chunk call; `size(mask)` must equal that row
    !> group's own `nrows` (from parquet_new_row_group) exactly, and the row group's actual
    !> written row count is `count(mask)`. A second call for the same still-open row group is an
    !> `error stop` rather than silently replacing the first mask.
    !>
    !> If used for a writer's first row group, it must be used for **every** row group of that
    !> writer (all-or-nothing per writer, not per row group) -- `error stop` otherwise. It is
    !> unavailable (`error stop`) once any column has been written whole via parquet_write_column
    !> anywhere in the writer's lifetime; that combination can only use parquet_write_row_mask
    !> instead. Mutually exclusive with parquet_write_row_mask on the same writer.
    interface parquet_write_chunk_row_mask
        module procedure parquet_write_chunk_row_mask_impl
    end interface parquet_write_chunk_row_mask

    !> Returns `writer_or_reader`'s row-group size ("chunk_size", matching parquet_open_writer's
    !> own chunk_size argument name -- Arrow's own WriteTable convenience function uses this same
    !> term for the concept). For a parquet_writer: the caller's own explicit chunk_size, validated
    !> against every declared vector column, or -- if chunk_size was never set -- an estimate
    !> computed from the schema's declared types/col_size. Usable at any point after
    !> parquet_open_writer, including before any column has been written, and it does not change
    !> afterwards: parquet_new_row_group's own `nrows` is what sizes each streamed row group and
    !> never feeds back into this value, and a writer that only uses parquet_write_column has its
    !> row groups sized at parquet_close_writer from the finished table's real byte size, which a
    !> schema-only estimate here cannot anticipate. So on the write side this is a suggestion for a
    !> chunked-write loop, not a promise about the file's final layout. For a parquet_reader: the
    !> row group's size at `row_group` (1-based; omitted defaults to the first row group),
    !> reflecting the file's actual, already-written-and-fixed layout -- row groups are not
    !> guaranteed uniform, so it is only ever a suggestion for a chunked-read loop there too.
    interface parquet_get_chunk_size
        module procedure parquet_get_chunk_size_writer_int32
        module procedure parquet_get_chunk_size_writer_int64
        module procedure parquet_get_chunk_size_reader_int32
        module procedure parquet_get_chunk_size_reader_int64
    end interface parquet_get_chunk_size

    !> Returns `reader`'s row-group count in `num_row_groups`, dispatched by its
    !> integer(int32)/integer(int64) kind (the int32 specific also error stops if the actual
    !> count overflows int32 -- vanishingly unlikely in practice, but kept for consistency with
    !> parquet_get_nrows's own int32/int64 overload). Reflects the file's physical layout;
    !> unaffected by any filter= given to parquet_open_reader. See "Streaming/chunked reads" in
    !> doc/pages/io/reading.md for the chunked-read loop this and parquet_get_chunk_size/
    !> parquet_read_column_chunk are meant to be used together for.
    interface parquet_get_num_row_groups
        module procedure parquet_get_num_row_groups_int64
        module procedure parquet_get_num_row_groups_int32
    end interface parquet_get_num_row_groups

    !> Measures the uniform element-count-per-row (`width`) of a plain Parquet `LIST`/`LARGE_LIST`
    !> column, over the 1-based inclusive row-group range `row_group_lo`..`row_group_hi`, without
    !> ever materializing the whole column. Both bounds are accepted as `integer(int32)` or
    !> `integer(int64)`; `row_group_lo <= 0` means "every row group in the file".
    !>
    !> Such a column may hold a different number of elements in every row, so unlike a
    !> `FIXED_SIZE_LIST` (this library's own vector layout) its width is a property of the data
    !> rather than the schema -- see parquet_column_width_needs_data. `width` comes back as 1 when
    !> no single width above 1 covers every row, which is also the answer for a genuinely scalar
    !> column or a `FIXED_SIZE_LIST` of width 1, and as 0 for a column with no rows at all --
    !> matching what parquet_get_col_size reports for an empty list column.
    !>
    !> `proven` chooses how much work to do, and the difference matters:
    !>
    !> * `.false.` -- decide from the file footer alone, reading no column data at all. Per row
    !>   group, the mean elements per row is compared against its neighbours; a non-integral or
    !>   disagreeing mean proves no uniform width exists. A surviving answer is a CANDIDATE only:
    !>   rows of length 3, 1, 3, 1 average to exactly 2. Use this when a wrong answer is safe
    !>   because something downstream will reject it.
    !> * `.true.` -- screen as above, then confirm by reading the covered row groups one at a time,
    !>   stopping at the first row that disagrees. Peak memory stays at one row group, so this is
    !>   safe on a column far larger than memory, but it does read data.
    interface parquet_measure_list_width
        module procedure parquet_measure_list_width_int32
        module procedure parquet_measure_list_width_int64
    end interface parquet_measure_list_width

    !> Whether `name` contains any Null over the 1-based inclusive row-group range
    !> `row_group_lo`..`row_group_hi`, answered from the file's own statistics. Both bounds are
    !> accepted as `integer(int32)` or `integer(int64)`; `row_group_lo <= 0` means "every row group".
    !>
    !> Reads **no column data**: Parquet records a null count per column chunk in the footer. That is
    !> what makes this worth calling before a read -- a caller that knows a column is Null-free can
    !> skip building a validity mask for it entirely.
    !>
    !> Answers `.true.` when the column has at least one Null **or when the file cannot say**.
    !> Statistics are optional in the Parquet format, so a chunk without them (or without a null
    !> count) reads as "might have Nulls"; a dotted struct-field path is declined outright, because a
    !> struct leaf's validity is combined with every ancestor struct's on read and the leaf's own null
    !> count therefore does not describe the result. Treat `.false.` as a guarantee and `.true.` as
    !> "assume the worst" -- that is the direction which keeps a caller that skips a mask on the
    !> strength of this answer from ever meeting an unexpected Null.
    interface parquet_column_has_nulls
        module procedure parquet_column_has_nulls_int32
        module procedure parquet_column_has_nulls_int64
    end interface parquet_column_has_nulls

    !> Parses a MAML into a parquet_schema (its %maml, %cinfo and %metadata).
    !> The file form takes a `filename` (.maml file path) and loads it from
    !> disk first; the object form takes only `schema`, whose %maml has
    !> already been populated (e.g. built in memory). Both forms fill in
    !> `schema`'s %cinfo and %metadata in place.
    interface parquet_parse_maml
        module procedure parquet_parse_maml_from_file
        module procedure parquet_parse_maml_from_object
    end interface parquet_parse_maml

    !> Runs the full set of MAML validity checks (a table: key, at least one
    !> field, a valid data_type on every field, ...) against either an
    !> already-loaded MAML passed as `maml` (a parquet_maml_file, e.g. via
    !> parquet_load_maml_file or built in memory) or a MAML filename passed
    !> as `maml` (a character(len=*) .maml file path, loaded from disk first,
    !> then checked identically).
    interface parquet_validate_maml
        module procedure parquet_validate_maml_internal
        module procedure parquet_validate_maml_file
    end interface parquet_validate_maml

    !> Reads one column, named `name`, from an open parquet_reader (reader)
    !> into `values`, dispatched by its actual/declared type/kind and rank
    !> (scalar 1-D values(:), one value per row -- the column_1d specifics;
    !> or a full 2-D values(:,:), (element, row) values -- the array_full
    !> specifics, for a vector column). null_value (optional) fills missing
    !> entries; is_valid (optional) reports which elements were actually
    !> present (same rank as values).
    !>
    !> A scalar "string" column can also be read straight into a `type(parquet_string_column)`
    !> (see module parquet_strings) instead of a padded character(len=...) array -- `values` is
    !> cleared then filled with the whole column; every Null lands as `%append_null()`, so
    !> neither null_value nor is_valid is accepted for this specific -- check `%is_null(i)`
    !> afterward instead. Scalar (1-D) columns only.
    interface parquet_read_column
        module procedure parquet_read_int32_column_1d
        module procedure parquet_read_int64_column_1d
        module procedure parquet_read_float32_column_1d
        module procedure parquet_read_float64_column_1d
        module procedure parquet_read_logical_column_1d
        module procedure parquet_read_string_column_1d
        module procedure parquet_read_int32_array_full
        module procedure parquet_read_int64_array_full
        module procedure parquet_read_float32_array_full
        module procedure parquet_read_float64_array_full
        module procedure parquet_read_logical_array_full
        module procedure parquet_read_string_array_full
        module procedure parquet_read_string_column_compact
        module procedure parquet_read_date_column_1d
        module procedure parquet_read_date_array_full
        module procedure parquet_read_time_column_1d
        module procedure parquet_read_time_array_full
        module procedure parquet_read_timestamp_column_1d
        module procedure parquet_read_timestamp_array_full
        module procedure parquet_read_list_column
        module procedure parquet_read_struct_column
        module procedure parquet_read_map_column
    end interface parquet_read_column

    !> Reads one row of a vector (array) column named `name` from an open
    !> parquet_reader (reader): `values` receives that row's full element
    !> vector, selected by the 1-based `row_index`. Dispatched by `values`'
    !> actual/declared type/kind, and separately by `row_index`'s own kind
    !> (integer(int32) or integer(int64) -- the latter needed only to address
    !> a row beyond huge(1_int32), 2,147,483,647, in a file that large).
    !> null_value (optional) fills missing entries; is_valid (optional)
    !> reports which elements were actually present. See
    !> parquet_read_array_element_mode for the complementary "one element
    !> across all rows" access pattern. Reads only the one row group `row_index` falls in (not the
    !> whole column), unless a row filter is active (parquet_open_reader(..., filter=)/
    !> parquet_reader_set_filter), in which case `row_index` addresses the filtered result and the
    !> whole (filtered) column is read -- mapping a filtered row index back to its physical row
    !> group needs a per-row-group survivor count that this path does not yet use.
    interface parquet_read_array_row_mode
        module procedure parquet_read_int32_array_row_mode
        module procedure parquet_read_int32_array_row_mode_row_index_int64
        module procedure parquet_read_int64_array_row_mode
        module procedure parquet_read_int64_array_row_mode_row_index_int64
        module procedure parquet_read_float32_array_row_mode
        module procedure parquet_read_float32_array_row_mode_row_index_int64
        module procedure parquet_read_float64_array_row_mode
        module procedure parquet_read_float64_array_row_mode_row_index_int64
        module procedure parquet_read_logical_array_row_mode
        module procedure parquet_read_logical_array_row_mode_row_index_int64
        module procedure parquet_read_string_array_row_mode
        module procedure parquet_read_string_array_row_mode_row_index_int64
        module procedure parquet_read_date_array_row_mode
        module procedure parquet_read_date_array_row_mode_row_index_int64
        module procedure parquet_read_time_array_row_mode
        module procedure parquet_read_time_array_row_mode_row_index_int64
        module procedure parquet_read_timestamp_array_row_mode
        module procedure parquet_read_timestamp_array_row_mode_row_index_int64
    end interface parquet_read_array_row_mode

    !> Reads one element position of a vector (array) column named `name`
    !> from an open parquet_reader (reader) across all rows: `values`
    !> receives that element (selected by the 1-based `elem_index`) from
    !> every row. Dispatched by `values`' actual/declared type/kind.
    !> null_value (optional) fills missing entries; is_valid (optional)
    !> reports which rows were actually present. See
    !> parquet_read_array_row_mode for the complementary "one row" access pattern. Unlike
    !> parquet_read_array_row_mode (which only needs one row group), this access pattern
    !> inherently touches every row, so every row group contributes -- none can be skipped.
    !> Instead, unless a row filter is active, this streams the file row group by row group
    !> (never materializing the whole column's flattened element count in a single internal
    !> call), so a vector column whose total element count (rows times per-row width) would
    !> otherwise exceed 2,147,483,647 can still be read this way. If a row filter is active
    !> (parquet_open_reader(..., filter=)/parquet_reader_set_filter), the whole (filtered) column
    !> is read instead, for the same reason parquet_read_array_row_mode falls back: `elem_index`
    !> then addresses the filtered result.
    interface parquet_read_array_element_mode
        module procedure parquet_read_int32_array_element_mode
        module procedure parquet_read_int64_array_element_mode
        module procedure parquet_read_float32_array_element_mode
        module procedure parquet_read_float64_array_element_mode
        module procedure parquet_read_logical_array_element_mode
        module procedure parquet_read_string_array_element_mode
        module procedure parquet_read_date_array_element_mode
        module procedure parquet_read_time_array_element_mode
        module procedure parquet_read_timestamp_array_element_mode
    end interface parquet_read_array_element_mode

    !> Reads one row group's worth of one column, named `name`, from an open parquet_reader
    !> (reader) into `values` -- the row-group-chunked counterpart to parquet_read_column, for a
    !> large column you read one row group at a time instead of materializing the whole column.
    !> `row_group` (1-based) selects which row group; reads are stateless/random-access (unlike
    !> the write side's parquet_new_row_group/parquet_finish_row_group pairing, there is no
    !> "currently open" row group to track -- call with any row_group, in any order, as many
    !> times as you like). Use parquet_get_num_row_groups to learn how many row groups a file
    !> has, and parquet_get_chunk_size(reader, ..., row_group=) to learn a specific row group's
    !> own row count before allocating `values`. Dispatched by `values`' actual/declared
    !> type/kind and rank (scalar values(:) or vector values(:,:)) exactly like
    !> parquet_read_column, and separately by `row_group`'s own kind (integer(int32) or
    !> integer(int64) -- the latter needed only to address a row group beyond
    !> huge(1_int32) in a file that large). null_value (optional) fills missing entries;
    !> is_valid (optional) reports which elements were actually present.
    !>
    !> Two restrictions not shared with parquet_read_column: disallowed (error stop) on a reader
    !> with an active sort (a permutation destroys row-group locality, so there is no coherent
    !> "row group N of the sorted output" to serve -- see check_reader_no_sort; a filter= or
    !> sample_fraction= mask only ever REMOVES rows, so a chunked read works under one and hands
    !> back that row group's surviving rows); and, when the reader
    !> was opened with qc=.true., every chunk read runs the usual qc: min/max/miss checks scoped
    !> to just that one row group's own data (not the whole column) -- a hard-mode
    !> (qc_soft=.false.) violation error stops naming the offending row group; a soft-mode
    !> (qc_soft=.true.) violation warns at most once per column, same throttling as every other
    !> read path. See "Streaming/chunked reads" in doc/pages/io/reading.md.
    !>
    !> A scalar "string" column can also be read into a `type(parquet_string_column)` holding
    !> just row group `row_group`'s rows -- `values` is cleared then filled, same no-null_value/
    !> no-is_valid convention as parquet_read_column's own parquet_string_column specific above.
    interface parquet_read_column_chunk
        module procedure parquet_read_int32_column_chunk_rg32
        module procedure parquet_read_int32_column_chunk_rg64
        module procedure parquet_read_int32_array_column_chunk_rg32
        module procedure parquet_read_int32_array_column_chunk_rg64
        module procedure parquet_read_int64_column_chunk_rg32
        module procedure parquet_read_int64_column_chunk_rg64
        module procedure parquet_read_int64_array_column_chunk_rg32
        module procedure parquet_read_int64_array_column_chunk_rg64
        module procedure parquet_read_float32_column_chunk_rg32
        module procedure parquet_read_float32_column_chunk_rg64
        module procedure parquet_read_float32_array_column_chunk_rg32
        module procedure parquet_read_float32_array_column_chunk_rg64
        module procedure parquet_read_float64_column_chunk_rg32
        module procedure parquet_read_float64_column_chunk_rg64
        module procedure parquet_read_float64_array_column_chunk_rg32
        module procedure parquet_read_float64_array_column_chunk_rg64
        module procedure parquet_read_logical_column_chunk_rg32
        module procedure parquet_read_logical_column_chunk_rg64
        module procedure parquet_read_logical_array_column_chunk_rg32
        module procedure parquet_read_logical_array_column_chunk_rg64
        module procedure parquet_read_string_column_chunk_rg32
        module procedure parquet_read_string_column_chunk_rg64
        module procedure parquet_read_string_array_column_chunk_rg32
        module procedure parquet_read_string_array_column_chunk_rg64
        module procedure parquet_read_string_column_chunk_compact_rg32
        module procedure parquet_read_string_column_chunk_compact_rg64
        module procedure parquet_read_date_column_chunk_rg32
        module procedure parquet_read_date_column_chunk_rg64
        module procedure parquet_read_date_array_column_chunk_rg32
        module procedure parquet_read_date_array_column_chunk_rg64
        module procedure parquet_read_time_column_chunk_rg32
        module procedure parquet_read_time_column_chunk_rg64
        module procedure parquet_read_time_array_column_chunk_rg32
        module procedure parquet_read_time_array_column_chunk_rg64
        module procedure parquet_read_timestamp_column_chunk_rg32
        module procedure parquet_read_timestamp_column_chunk_rg64
        module procedure parquet_read_timestamp_array_column_chunk_rg32
        module procedure parquet_read_timestamp_array_column_chunk_rg64
        module procedure parquet_read_list_column_chunk_rg32
        module procedure parquet_read_struct_column_chunk_rg32
        module procedure parquet_read_struct_column_chunk_rg64
        module procedure parquet_read_map_column_chunk_rg32
        module procedure parquet_read_map_column_chunk_rg64
        module procedure parquet_read_list_column_chunk_rg64
    end interface parquet_read_column_chunk

    !> Returns `reader`'s post-filter row count in `nrows`, dispatched by
    !> its integer(int32)/integer(int64) kind (the int32 specific also error
    !> stops if the actual row count overflows int32). check_positive
    !> (optional, default .false.): if .true., error stops instead of
    !> returning 0 rows.
    interface parquet_get_nrows
        module procedure parquet_get_nrows_int64
        module procedure parquet_get_nrows_int32
    end interface parquet_get_nrows

    !> Opens `filename` for reading into `reader`. use_threads (optional):
    !> use Arrow's multi-threaded reader. filter (optional): a parquet_filter
    !> row filter to apply. sample_fraction (optional, real(real64)): keeps
    !> each row independently with probability sample_fraction (Bernoulli
    !> sampling, not an exact row count) -- omitted, or >= 1.0, reads every
    !> row (the current/default behavior); must not be negative or NaN
    !> (error stops); exactly 0.0 deterministically yields zero rows. Shares
    !> its underlying mask with filter= (see "Row filtering with
    !> parquet_filter" in doc/pages/io/filter-sort-sample.md): a filter, if
    !> also given, is applied
    !> on top of the downsample, and sample_fraction < 1.0 alone (even with
    !> no filter=) carries the same consequences filter= already has --
    !> chunked reads (parquet_read_column_chunk) are disallowed, and array
    !> row/element-mode reads fall back to a whole-column read. sample_seed
    !> (optional, integer(int32)): omitted or <= 0 draws a fresh seed from
    !> entropy (a different sample each call); > 0 makes the draw
    !> reproducible. The seed actually used (caller-supplied or
    !> entropy-drawn) is always reported by parquet_close_reader(...,
    !> print_stat=.true.), so a non-deterministic run's seed can be read
    !> back afterward and reused. schema (optional): a parquet_schema/qc-maml
    !> to validate columns against. qc (optional): enable qc: min/max/miss
    !> enforcement (needs schema). qc_soft (optional): qc violations warn
    !> instead of error-stopping. prefetch (optional, default .false.): when
    !> .true., every column in the file is read and cached right away, after
    !> any filter/sample has been applied, instead of each column being read
    !> lazily on first request -- equivalent to calling
    !> parquet_prefetch_columns for every column immediately after opening;
    !> materializes the whole file in memory up front, see
    !> doc/pages/operating/performance.md.
    !>
    !> nrows= is generic over integer(int32)/integer(int64)
    !> (parquet_open_reader_nrows_int32/_int64: fills `nrows` with the
    !> post-filter/post-sample row count, error-stopping if it overflows the
    !> requested kind), plus the original nrows-less form (parquet_open_reader_base)
    !> for when nrows isn't wanted at all. This mirrors parquet_get_nrows's
    !> own int32/int64 overload above; nrows is required (not optional) in
    !> the two typed specifics -- an optional dummy that may be absent can't
    !> be the sole thing distinguishing two specific procedures in a generic
    !> interface (a call omitting it would be ambiguous), so
    !> parquet_open_reader_base carries the nrows-absent case as a separate
    !> specific instead. sample_fraction/sample_seed are each single-kind
    !> (real64/int32) rather than dual-kind like nrows, specifically to avoid
    !> that same ambiguity: a *second* independently-optional dual-kind
    !> argument on this generic would force a 3x3 cross product of specifics
    !> (absent/real32/real64 sample_fraction times the three nrows shapes) --
    !> not worth it for a fraction/seed argument, which isn't the
    !> row-count/size/index category that convention exists for.
    interface parquet_open_reader
        module procedure parquet_open_reader_base
        module procedure parquet_open_reader_nrows_int64
        module procedure parquet_open_reader_nrows_int32
    end interface parquet_open_reader

    !> Reads back one key's value from the flat key-value table metadata a
    !> parquet_writer wrote via add_metadata (see parquet_reader%metadata,
    !> populated once by parquet_open_reader). `value`'s declared type/kind
    !> selects the specific procedure, so it also selects which stored
    !> representation is expected -- the stored string (always written by
    !> add_metadata as plain text, see parquet_metadata.f90) is parsed back
    !> into that type. Any key is allowed, including the writer's own
    !> reserved/internal keys (e.g. "DATE", "column.<name>.unit").
    !>
    !> If `key` is missing: returns `default` if given (printing a WARNING
    !> unless warn=.false.), else error stops. If `key` is present but its
    !> stored text cannot be converted to the requested type (including a
    !> stored integer that overflows a 32-bit target): always prints a
    !> WARNING, then falls back to `default` if given, else error stops.
    !> `default` must be the same type/kind as `value`; `warn` defaults to
    !> .true. and only affects the "key missing, default used" case.
    interface parquet_get_metadata
        module procedure parquet_get_metadata_int32
        module procedure parquet_get_metadata_int64
        module procedure parquet_get_metadata_float32
        module procedure parquet_get_metadata_float64
        module procedure parquet_get_metadata_logical
        module procedure parquet_get_metadata_string
        module procedure parquet_get_metadata_int32_array
        module procedure parquet_get_metadata_int64_array
        module procedure parquet_get_metadata_float32_array
        module procedure parquet_get_metadata_float64_array
        module procedure parquet_get_metadata_logical_array
        module procedure parquet_get_metadata_string_array
    end interface parquet_get_metadata

    !> Returns the total number of ELEMENTS column `name` holds across every row of an open
    !> parquet_reader (reader), in `total_elements`, dispatched by its
    !> integer(int32)/integer(int64) kind. `nrows` for a scalar column and `nrows * col_size` for
    !> a FIXED_SIZE_LIST one, both known from the file footer/schema, so no column data is read --
    !> which is what makes this safe on a column whose element count itself exceeds int32 (see
    !> CLAUDE.md's "Guarding a hard Arrow int32-only ceiling").
    !>
    !> For a plain LIST/LARGE_LIST column it is the SUM OF THE ROWS' OWN LENGTHS, which for a
    !> ragged column is not `nrows * col_size`: a 3-row column holding 2, 0 and 3 elements answers
    !> 5, while parquet_get_col_size answers 1, because a ragged column has no uniform width to
    !> report. The two queries ask different things of such a column and only col_size has a width
    !> to give. Measured from the rows' offsets one row group at a time -- data is read, but never
    !> more than one row group at once, and the footer cannot answer it (a null or empty list
    !> occupies a leaf slot, so the footer's own value count over-counts).
    interface parquet_get_column_total_elements
        module procedure parquet_get_column_total_elements_int64
        module procedure parquet_get_column_total_elements_int32
    end interface parquet_get_column_total_elements

    !> Reads and caches the named column(s) of an open parquet_reader
    !> (reader) right away rather than lazily on first request.
    !> parquet_prefetch_columns accepts `names` as either an array of column
    !> names (each element sharing one declared length -- pad shorter names
    !> with blanks) or a single scalar string listing the names separated by
    !> commas and/or semicolons ("ra;dec,mag"). The scalar form avoids the
    !> fixed-length array pitfall where a too-short declared length silently
    !> truncates a name.
    interface parquet_prefetch_columns
        module procedure parquet_prefetch_columns_array
        module procedure parquet_prefetch_columns_string
    end interface parquet_prefetch_columns

    public :: parquet_writer
    public :: parquet_reader
    !> Public only so the sibling parquet_tables module can reach them -- src/parquet.f90
    !! makes both private again, so neither is part of the `use parquet` surface.
    !! `parquet_split_name_list` is the library's one name-list tokenizer, and
    !! `parquet_parse_sort_key` its one sort-key direction grammar; the table layer's
    !! string key lists spell direction exactly as a read-time parquet_sortkey does
    !! because they run the same parser rather than a second copy of it.
    public :: parquet_split_name_list
    public :: parquet_parse_sort_key
    public :: parquet_filter
    public :: parquet_sortkey
    public :: parquet_read_qc
    public :: parquet_compose_read_qc
    public :: parquet_get_qc_columns
    public :: parquet_reader_set_sort
    public :: parquet_reader_adopt_transform
    public :: parquet_column_info
    public :: parquet_column_type
    public :: parquet_size_auto
    public :: parquet_sample_algorithm
    public :: parquet_sample_label
    public :: parquet_table_metadata
    public :: parquet_schema
    public :: parquet_maml_file
    public :: parquet_string_column
    public :: parquet_string
    ! Re-exported from parquet_temporal so users need only `use parquet` to get the date/time
    ! element types, the unit selectors used by their set_unix/to_unix and MAML units, and the
    ! nanosecond unit-conversion constants used by their difference/offset operators.
    public :: parquet_date, parquet_time, parquet_timestamp
    public :: parquet_unit_seconds, parquet_unit_millis, parquet_unit_micros, parquet_unit_nanos
    public :: parquet_ns_per_sec, parquet_ns_per_day, parquet_ns_to_sec, parquet_ns_to_day
    public :: parquet_get_column_time_info
    public :: parquet_open_writer
    public :: parquet_write_column
    public :: parquet_new_row_group
    public :: parquet_write_column_chunk
    public :: parquet_write_row_mask
    public :: parquet_write_chunk_row_mask
    public :: parquet_finish_row_group
    public :: parquet_get_chunk_size
    public :: parquet_close_writer
    public :: parquet_parse_maml
    public :: parquet_load_maml_file
    public :: parquet_load_qc_maml_file
    public :: parquet_validate_maml
    public :: parquet_validate_user_maml
    !> Applies a filter to an already-open reader (see the specifics' own doc-comments for the
    !> full contract). Two forms: whole-file, and scoped to an inclusive 1-based row-group range.
    !>
    !> The scoped form is the memory-bounded one, and the difference is in how the mask is BUILT,
    !> not merely in which rows survive. Whole-file, every filter column is read in one batched,
    !> thread-parallel pass and left decoded in the reader's cache -- fastest, and the right
    !> default. Scoped, the expression is evaluated one row group at a time and each chunk is
    !> released before the next is read, so peak memory is one row group's worth of the filter
    !> columns rather than the whole file; rows outside the range never match, and nothing is left
    !> cached, so a filter column read afterwards is read again. Use it when the file is larger
    !> than memory, or when only part of it is of interest -- typically alongside chunked reads
    !> over the same row groups.
    !>
    !> It is the PRESENCE of the row-group arguments that selects between those two engines, not
    !> their value: `row_group_lo = 0` means "all row groups" on the memory-bounded engine (and
    !> row_group_hi is then ignored), which is how a whole file larger than memory is filtered
    !> without first asking parquet_get_num_row_groups how many there are. A non-positive lower
    !> bound reads as "all row groups" here exactly as it already does in
    !> parquet_measure_list_width and parquet_column_has_nulls.
    !>
    !> A third form takes a physical ROW range as well (four numeric arguments rather than two):
    !> only rows row_lo..row_hi, 1-based and inclusive, may match. It exists because a row-group
    !> range can only ever start and end on a row-group boundary, so a caller working over an
    !> arbitrary row range -- a parquet_table slice, typically -- would otherwise get back the
    !> whole covering row groups' survivors and have no way to trim them, the mask being the only
    !> thing that knows which rows those are. With this form the filter answers for exactly the
    !> requested rows, and parquet_get_nrows afterwards is that range's own surviving count. The
    !> filter may hold no rules at all in this form, which installs the range by itself. The row
    !> range must lie inside the rows its row groups span, or the call fails with error stop rather
    !> than quietly handing back the intersection of the two -- which for a disjoint pair is empty,
    !> and an empty result is indistinguishable from a filter that matched nothing.
    interface parquet_reader_set_filter
        module procedure parquet_reader_set_filter_base
        module procedure parquet_reader_set_filter_scoped_int32
        module procedure parquet_reader_set_filter_scoped_int64
        module procedure parquet_reader_set_filter_rows_int32
        module procedure parquet_reader_set_filter_rows_int64
    end interface parquet_reader_set_filter

    public :: parquet_open_reader
    public :: parquet_reader_set_filter
    public :: parquet_close_reader
    public :: parquet_prefetch_columns
    public :: parquet_get_nrows
    public :: parquet_get_num_row_groups
    public :: parquet_get_col_size
    public :: parquet_measure_list_width, parquet_column_width_needs_data
    public :: parquet_column_has_nulls
    public :: parquet_get_column_total_elements
    public :: parquet_get_string_length
    public :: parquet_column_exists
    public :: parquet_get_column_type
    public :: parquet_get_column_shape
    public :: parquet_get_map_value_type
    public :: parquet_get_column_nullable
    public :: parquet_get_column_names
    public :: parquet_release_column
    public :: parquet_get_metadata
    public :: parquet_get_metadata_items
    public :: parquet_get_physical_row_indices
    public :: parquet_read_column
    public :: parquet_read_column_chunk
    public :: parquet_read_array_row_mode
    public :: parquet_read_array_element_mode

    ! ---- Schema / column-info / table-metadata plumbing ----
    interface
        !> Parses `raw` (a qc: min:/max: bound, operator already stripped) as
        !> a real64 number appropriate for `data_type`: for float32/float64,
        !> just requires a finite (non-NaN/non-Inf) parse; for int32/int64,
        !> additionally requires the parsed value to be an exact integer
        !> within that type's representable range. Returns .false. (value
        !> undefined) if parsing fails or any of these checks fail. Not
        !> meaningful for "string" (no numeric bound) or "boolean" (qc: is
        !> never enforced there) -- callers should not invoke this for those.
        module logical function parquet_qc_numeric_bound(raw, data_type, value)
            character(len=*), intent(in) :: raw !! qc: min:/max: text, operator prefix already stripped.
            character(len=*), intent(in) :: data_type !! field's declared data_type; selects the parsing rule.
            real(real64), intent(out) :: value !! parsed bound value; only meaningful when the function returns .true.
        end function parquet_qc_numeric_bound
        !> Parses `raw` (a qc: min:/max: bound, operator already stripped) as an
        !> EXACT int64, without ever going through real64. Whole-string and
        !> strict: an optional leading +/- then nothing but digits, and the
        !> value must fit in int64. Returns .false. (value undefined) for
        !> anything else -- a fractional bound ("1.5"), an exponent form
        !> ("1e3"), trailing junk, or a magnitude past int64 -- which is what
        !> lets a caller fall back to parquet_qc_numeric_bound's real64 route
        !> for the cases that legitimately need it.
        !>
        !> Why this exists at all: routing an integer bound through real64
        !> rounds it above 2**53, so a declared max: of 9007199254740993 became
        !> 9007199254740992 and a legitimate value equal to its own bound was
        !> reported as a violation; and the real64 range test rejected
        !> huge(int64) outright, since that value and huge(int64)+1 are the same
        !> real64. Both are wrong answers rather than approximations, and the
        !> read side (parse_int64_strict, parquet_wrapper.cpp) never had them
        !> because it parses the same text straight to int64. Do NOT use a bare
        !> list-directed read for this -- it accepts "5 6" as 5 (see CLAUDE.md).
        module logical function parquet_qc_bound_as_int64_text(raw, value)
            character(len=*), intent(in) :: raw !! qc: min:/max: text, operator prefix already stripped.
            integer(int64), intent(out) :: value !! parsed bound; only meaningful when the function returns .true.
        end function parquet_qc_bound_as_int64_text
        !> File form of parquet_parse_maml: loads `filename` from disk, then
        !> parses/validates it into `schema` (%maml, %cinfo, %metadata). Error
        !> stops if `schema` is already initialized (schema%is_init() ==
        !> .true., whether via %init or an earlier parse) -- call
        !> schema%clear() first to reuse the same variable for a different
        !> file, rather than silently discarding whatever it held before.
        module subroutine parquet_parse_maml_from_file(filename, schema)
            character(len=*), intent(in) :: filename !! .maml file path.
            type(parquet_schema), intent(inout) :: schema !! schema to populate (must not already be initialized).
        end subroutine parquet_parse_maml_from_file
        !> Loads `filename`'s raw lines from disk into a parquet_maml_file,
        !> running the full parquet_validate_maml checks (table:, at least
        !> one field, valid data_type, ...); not parsed into a schema yet
        !> (see parquet_parse_maml).
        module function parquet_load_maml_file(filename) result(maml)
            character(len=*), intent(in) :: filename !! .maml file path.
            type(parquet_maml_file) :: maml !! validated raw MAML.
        end function parquet_load_maml_file
        !> Loads a qc-maml's raw lines from disk WITHOUT running the full
        !> parquet_validate_maml checks parquet_load_maml_file always applies
        !> (table:, at least one field, valid data_type, ...) -- a qc-maml
        !> only needs name + qc: min:/max:/miss: per field, and doesn't need
        !> data_type at all. Its own (lighter) validation happens later, in
        !> parquet_parse_qc_maml, when parquet_open_reader(..., schema=) uses it.
        !> Returns a parquet_schema with only %maml populated (the raw qc-maml
        !> lines); %cinfo/%metadata stay empty, since a qc-maml is never parsed.
        module function parquet_load_qc_maml_file(filename) result(schema)
            character(len=*), intent(in) :: filename !! qc-maml file path.
            type(parquet_schema) :: schema !! schema with only %maml populated (raw qc-maml lines).
        end function parquet_load_qc_maml_file
        !> Validates and parses a qc-maml's fields: entries into `rules`, one
        !> entry per field that has at least a name (data_type/unit/ucd/etc.
        !> are irrelevant here and never required) -- error stops on a
        !> duplicate field name, an unknown top-level section/sub-key, or an
        !> unrecognized qc: miss: value (anything other than Null/NA,
        !> case-insensitive, or empty). Table-level metadata is ignored
        !> entirely. See parquet_qc_rule's own doc comment for min_text/max_text.
        !> The columns a qc-MAML declares an actual `qc:` block for, as a blank-padded array.
        !>
        !> Answers "what does this qc actually constrain?" without exposing the rule objects
        !> themselves: a field that merely NAMES a column, with no `qc:` key, declares nothing and
        !> is left out -- the same distinction parquet_apply_qc makes before it reaches the reader.
        !> `names` comes back zero-size when nothing is constrained. Sized to the longest name and
        !> blank-padded, so `trim(names(i))` is the name to pass on.
        module subroutine parquet_get_qc_columns(schema, names)
            type(parquet_schema), intent(in) :: schema !! a qc schema (composed or loaded).
            character(len=:), allocatable, intent(out) :: names(:) !! columns with a qc: block.
        end subroutine parquet_get_qc_columns
        module subroutine parquet_parse_qc_maml(maml, rules)
            type(parquet_maml_file), intent(in) :: maml !! raw qc-maml (e.g. from parquet_load_qc_maml_file).
            type(parquet_qc_rule), allocatable, intent(out) :: rules(:) !! one entry per field with at least a name.
        end subroutine parquet_parse_qc_maml
        !> Validates `user_maml` against `base_maml` (the full base schema):
        !> populates user_maml%missing_columns/%col_map, error stops on any
        !> field user_maml declares that base_maml doesn't.
        module subroutine parquet_validate_user_maml(base_maml, user_maml)
            type(parquet_maml_file), intent(in) :: base_maml !! the full base schema to validate against.
            type(parquet_maml_file), intent(inout) :: user_maml !! user-supplied MAML being validated.
        end subroutine parquet_validate_user_maml
        !> Runs the full parquet_validate_maml checks (table:, at least one
        !> field, valid data_type, ...) against an already-loaded MAML.
        module subroutine parquet_validate_maml_internal(maml)
            type(parquet_maml_file), intent(in) :: maml !! raw MAML to validate.
        end subroutine parquet_validate_maml_internal
        !> File form of parquet_validate_maml: loads `maml` from disk first,
        !> then runs the same checks as parquet_validate_maml_internal.
        module subroutine parquet_validate_maml_file(maml)
            character(len=*), intent(in) :: maml !! .maml file path.
        end subroutine parquet_validate_maml_file
        !> Object form of parquet_parse_maml: parses/validates a schema whose
        !> %maml has already been populated (e.g. built in memory), filling
        !> in %cinfo/%metadata in place.
        module subroutine parquet_parse_maml_from_object(schema)
            type(parquet_schema), intent(inout) :: schema !! schema with %maml already populated.
        end subroutine parquet_parse_maml_from_object
        !> Initializes a from-scratch parquet_schema: sets the (required)
        !> table: key and any of the optional scalar top-level MAML keys
        !> given, and marks this schema ready for %add_field. Error stops if
        !> this schema is already initialized (this%is_init() == .true., whether
        !> from an earlier %init call or a MAML parse -- e.g. calling %init on a
        !> schema already loaded via parquet_parse_maml), unless force=.true. is
        !> given (see below). List-shaped top-level sections (coauthors:,
        !> comments:, keywords:, DOIs:, depends:, keyarray:, extra:) are out of
        !> scope here -- keyarray: already has its own API (add_metadata); the
        !> others aren't supported by %init/%add_field at all yet.
        module subroutine schema_init(this, table, survey, dataset, version, date, author, description, license, &
                maml_version, force)
            class(parquet_schema), intent(inout) :: this !! schema being initialized (must not already be initialized,
            !! unless force=.true.).
            character(len=*), intent(in) :: table !! required table: key.
            character(len=*), intent(in), optional :: survey !! optional survey: key.
            character(len=*), intent(in), optional :: dataset !! optional dataset: key.
            character(len=*), intent(in), optional :: version !! optional version: key.
            character(len=*), intent(in), optional :: date !! optional date: key.
            character(len=*), intent(in), optional :: author !! optional author: key.
            character(len=*), intent(in), optional :: description !! optional description: key.
            character(len=*), intent(in), optional :: license !! optional license: key.
            character(len=*), intent(in), optional :: maml_version !! optional MAML_version: key.
            logical, intent(in), optional :: force !! if .true., bypass the "already initialized" error stop and
            !! fully reset this schema (discarding any fields/qc/metadata already added, and %cinfo/%metadata if
            !! %parquet_parse_maml had already run) before re-initializing, as if %init were being called for the
            !! first time (default .false.).
        end subroutine schema_init
        !> Whether this schema is ready to use -- .true. once either %init/parquet_schema(...) has
        !> completed (the in-code builder path) or parquet_parse_maml has populated %cinfo (a
        !> schema loaded from a .maml file/object, which never calls %init at all); .false. only
        !> for a just-declared parquet_schema that has had neither happen yet. Note %add_field's
        !> own "call schema%init(...) before adding fields" guard checks %init having been called
        !> specifically, not this broader readiness -- %add_field only makes sense on a from-scratch
        !> schema, so it is not satisfied merely by is_init() being .true. via a MAML parse.
        module logical function schema_is_init(this)
            class(parquet_schema), intent(in) :: this !! schema to query.
        end function schema_is_init
        !> Whether %cinfo has actually been populated by parquet_parse_maml -- .true. only once a
        !> parse has run (from a .maml file/object, or via %init/%add_field followed by
        !> parquet_parse_maml on the same schema), .false. otherwise, including for a from-scratch
        !> schema that has only had %init/%add_field called and never been parsed. This is the
        !> readiness check %print_schema_info itself requires (it error stops if %cinfo is
        !> unpopulated, unless allow_uninitialized=.true. is given) -- %is_init() is not equivalent
        !> here, since %is_init() is already .true. after %init alone, before any parse. Since
        !> %is_parsed() == .true. implies %cinfo%col is allocated, which itself already implies
        !> %is_init() == .true., %is_parsed() is always .false. whenever %is_init() is .false.
        module logical function schema_is_parsed(this)
            class(parquet_schema), intent(in) :: this !! schema to query.
        end function schema_is_parsed
        !> Resets this schema to exactly the state a freshly declared, never-initialized
        !> parquet_schema starts in: %maml/%cinfo/%metadata all back to their defaults (no
        !> lines, no fields, no metadata items) and is_init() == .false. again. Unlike
        !> %init(..., force=.true.) (which resets and immediately rebuilds with new header-key
        !> arguments), %clear leaves the schema uninitialized -- call %init again afterward to
        !> reuse the variable, or let it go out of scope. Always succeeds, even on an
        !> already-blank schema (a no-op in that case).
        module subroutine schema_clear(this)
            class(parquet_schema), intent(inout) :: this !! schema being reset to its pristine state.
        end subroutine schema_clear
        !> Structure-constructor form of %init: builds and returns an
        !> initialized parquet_schema in one expression instead of
        !> declaring the variable and calling %init separately. Same
        !> header-key arguments and error-stop conditions as schema_init;
        !> no force= here since `this` is always a brand-new result variable
        !> (never already initialized), so there is nothing to reset.
        module function parquet_schema_new(table, survey, dataset, version, date, author, description, license, &
                maml_version) result(this)
            character(len=*), intent(in) :: table !! required table: key.
            character(len=*), intent(in), optional :: survey !! optional survey: key.
            character(len=*), intent(in), optional :: dataset !! optional dataset: key.
            character(len=*), intent(in), optional :: version !! optional version: key.
            character(len=*), intent(in), optional :: date !! optional date: key.
            character(len=*), intent(in), optional :: author !! optional author: key.
            character(len=*), intent(in), optional :: description !! optional description: key.
            character(len=*), intent(in), optional :: license !! optional license: key.
            character(len=*), intent(in), optional :: maml_version !! optional MAML_version: key.
            type(parquet_schema) :: this !! newly initialized schema.
        end function parquet_schema_new
        !> Appends one fields: entry to a schema built from scratch (%init
        !> must be called first). name/data_type are required (data_type
        !> must be one of the supported types); unit/info/ucd/array_size/
        !> col_size and qc_min/qc_max/qc_miss are optional, the latter three
        !> forming an optional qc: sub-block (same min:/max: operator-direction
        !> and miss: Null/NA rules %add_col_qc enforces, checked independently
        !> here -- %add_field is a distinct API from %add_col_qc, not built on
        !> top of it: %add_col_qc is for read-time qc-mamls (name + qc: only,
        !> no data_type) and explicitly rejects a column already declared as a
        !> field, so it cannot be layered onto a field %add_field just added).
        !> Validates eagerly (name/data_type/duplicate/qc all checked here,
        !> not deferred to parquet_validate_maml).
        module subroutine schema_add_field(this, name, data_type, unit, info, ucd, array_size, col_size, &
                qc_min, qc_max, qc_miss)
            class(parquet_schema), intent(inout) :: this !! schema being built (%init must have been called first).
            character(len=*), intent(in) :: name !! field name.
            character(len=*), intent(in) :: data_type !! field's data type (must be one of the supported types).
            character(len=*), intent(in), optional :: unit !! unit of measurement.
            character(len=*), intent(in), optional :: info !! short description.
            character(len=*), intent(in), optional :: ucd !! IVOA Unified Content Descriptor.
            integer, intent(in), optional :: array_size !! maximum string length (string fields only).
            integer, intent(in), optional :: col_size !! vector-column element count.
            character(len=*), intent(in), optional :: qc_min !! qc: min: bound (operator prefix allowed).
            character(len=*), intent(in), optional :: qc_max !! qc: max: bound (operator prefix allowed).
            character(len=*), intent(in), optional :: qc_miss !! Sets qc: miss:. Absent means no miss: is declared,
            !! i.e. nothing is said about Nulls and none are checked. An explicit EMPTY string declares that
            !! Nulls are NOT expected and turns Null validation on. "Null"/"NA" (case-insensitive) declares that
            !! they are expected. Presence, not content, is what is tested -- see parquet_column_type%qc_allow_null.
        end subroutine schema_add_field
        !> Parses a (already lowercased) MAML data_type `token` into its temporal base type and
        !> unit/utc: recognizes `date`, `time[unit]`, `timestamp[unit(,utc)]` where unit is one
        !> of s/ms/us/ns (bare = microseconds). `is_temporal` reports whether the token's base is
        !> date/time/timestamp; `valid` whether it is well-formed (for a non-temporal token,
        !> is_temporal=.false. and valid=.true. -- validity of those is decided elsewhere). On a
        !> valid temporal token, `base` is "date"/"time"/"timestamp", `unit_sel` a parquet_unit_*
        !> selector (0 for date), and `is_utc` the UTC flag.
        !> Splits a `list[<elemtype>]` data_type token into its element base type plus, for a
        !> temporal element, that element's unit/UTC flag. `is_list` reports whether the token is
        !> a list token at all (so a caller can fall through to the scalar rules), and `valid`
        !> whether it is a WELL-FORMED one -- a malformed list token is `is_list=.true.` with
        !> `valid=.false.`, exactly as a malformed temporal token is for parquet_parse_temporal_type.
        !>
        !> The element type is REQUIRED: a bare `list` is malformed. A declared-but-unwritten list
        !> column has to be written with zero rows at close, and that cannot invent a payload kind.
        module subroutine parquet_parse_list_type(token, elem_base, unit_sel, is_utc, is_list, valid)
            character(len=*), intent(in) :: token !! the data_type token, e.g. "list[timestamp[ms,utc]]".
            character(len=:), allocatable, intent(out) :: elem_base !! element base type, e.g. "timestamp".
            integer, intent(out) :: unit_sel !! element's temporal unit selector, 0 for a non-temporal element.
            logical, intent(out) :: is_utc !! .true. for a UTC-adjusted timestamp element.
            logical, intent(out) :: is_list !! .true. if the token names a list column at all.
            logical, intent(out) :: valid !! .true. if the token is well-formed.
        end subroutine parquet_parse_list_type
        !> Splits a `map[<valuetype>]` data_type token into its value base type plus, for a
        !> temporal value, that value's unit/UTC flag. The map counterpart of
        !> parquet_parse_list_type above, with the identical contract: `is_map` reports whether
        !> the token is a map token at all (so a caller can fall through to the scalar rules), and
        !> `valid` whether it is a WELL-FORMED one.
        !>
        !> **The token carries no KEY type**, because v1 map keys are always strings -- `map[int32]`
        !> means `map<string,int32>`. A future version supporting other key types would spell that
        !> `map[<keytype>,<valuetype>]`, which is unambiguous against this form because it has a
        !> comma.
        !>
        !> The value type is REQUIRED: a bare `map` is malformed, for the same reason a bare `list`
        !> is -- a declared-but-unwritten map column has to be written with zero rows at close, and
        !> that cannot invent a value kind. (The bare `struct` token IS valid, and the difference is
        !> real: a struct's field layout comes from the column object at write time and cannot be
        !> expressed in MAML at all, whereas a map's value type is a single token the schema can
        !> perfectly well carry.)
        module subroutine parquet_parse_map_type(token, value_base, unit_sel, is_utc, is_map, valid)
            character(len=*), intent(in) :: token !! the data_type token, e.g. "map[timestamp[ms,utc]]".
            character(len=:), allocatable, intent(out) :: value_base !! the value base type token.
            integer, intent(out) :: unit_sel !! temporal unit selector, else 0.
            logical, intent(out) :: is_utc !! UTC flag for a temporal value type.
            logical, intent(out) :: is_map !! .true. if the token names a map column at all.
            logical, intent(out) :: valid !! .true. if it is a well-formed map token.
        end subroutine parquet_parse_map_type
        module subroutine parquet_parse_temporal_type(token, base, unit_sel, is_utc, is_temporal, valid)
            character(len=*), intent(in) :: token !! lowercased data_type token.
            character(len=:), allocatable, intent(out) :: base !! base type, or the token itself if non-temporal.
            integer, intent(out) :: unit_sel !! parquet_unit_* selector (0 for date/non-temporal).
            logical, intent(out) :: is_utc !! UTC-adjusted flag (timestamp only).
            logical, intent(out) :: is_temporal !! .true. if the base is date/time/timestamp.
            logical, intent(out) :: valid !! .true. if the token is well-formed.
        end subroutine parquet_parse_temporal_type
        !> Whether `token` (a MAML data_type, any case) is a valid data_type: one of the numeric/
        !> string/boolean base tokens, or a well-formed date/time/timestamp temporal token.
        module logical function parquet_data_type_token_valid(token)
            character(len=*), intent(in) :: token !! candidate data_type token.
        end function parquet_data_type_token_valid
        ! Flat convenience passthroughs on parquet_schema -- each forwards to
        ! the matching procedure on %cinfo, %metadata or %maml.
        !> Forwards to %cinfo%set_column_available; enables `name`, or every
        !> non-deactivated column if `name` is absent.
        module subroutine set_column_available(this, name)
            class(parquet_schema), intent(inout) :: this !! schema whose cinfo is updated.
            character(len=*), intent(in), optional :: name !! column to enable; every column if absent.
        end subroutine set_column_available
        !> Forwards to %cinfo%set_column_unavailable; disables `name`, or
        !> every non-deactivated column if `name` is absent.
        module subroutine set_column_unavailable(this, name)
            class(parquet_schema), intent(inout) :: this !! schema whose cinfo is updated.
            character(len=*), intent(in), optional :: name !! column to disable; every column if absent.
        end subroutine set_column_unavailable
        !> Forwards to %cinfo%set_protected.
        module subroutine schema_set_protected(this, name, protected)
            class(parquet_schema), intent(inout) :: this !! schema whose cinfo is updated.
            character(len=*), intent(in) :: name !! column to mark (internal name, as declared).
            logical, intent(in), optional :: protected !! .false. to unprotect; default .true.
        end subroutine schema_set_protected
        !> Forwards to %cinfo%set_col_size.
        module subroutine schema_set_col_size(this, name, col_size, force)
            class(parquet_schema), intent(inout) :: this !! schema whose cinfo is updated.
            character(len=*), intent(in) :: name !! column to resolve.
            integer, intent(in) :: col_size !! new col_size (must be a positive integer).
            logical, intent(in), optional :: force !! .true. allows overriding a col_size that isn't currently
            !! "auto" (default .false.: only an "auto" col_size may be resolved this way).
        end subroutine schema_set_col_size
        !> Forwards to %cinfo%set_array_size.
        module subroutine schema_set_array_size(this, name, array_size, force)
            class(parquet_schema), intent(inout) :: this !! schema whose cinfo is updated.
            character(len=*), intent(in) :: name !! string column to resolve.
            integer, intent(in) :: array_size !! new array_size (must be a positive integer).
            logical, intent(in), optional :: force !! .true. allows overriding an array_size that isn't currently
            !! "auto" (default .false.: only an "auto" array_size may be resolved this way).
        end subroutine schema_set_array_size
        !> Forwards to %cinfo%get_column_index.
        module integer function schema_get_column_index(this, name)
            class(parquet_schema), intent(in) :: this !! schema to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function schema_get_column_index
        !> Forwards to %cinfo%is_column_set.
        module logical function schema_is_column_set(this, name)
            class(parquet_schema), intent(in) :: this !! schema to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function schema_is_column_set
        !> Forwards to %cinfo%get_num_fields.
        module integer function schema_get_num_fields(this)
            class(parquet_schema), intent(in) :: this !! schema to query.
        end function schema_get_num_fields
        !> Forwards to %cinfo%get_field_name.
        module subroutine schema_get_field_name(this, index, name)
            class(parquet_schema), intent(in) :: this !! schema to query.
            integer, intent(in) :: index !! 1-based field position in MAML source order.
            character(len=:), allocatable, intent(out) :: name !! field name at that position.
        end subroutine schema_get_field_name
        !> Forwards to %cinfo%get_field_by_name -- see that procedure's own doc comment
        !> (parquet_column_info's spec, above) for the full contract, including the qc_min/
        !> qc_max/qc_miss round-trip caveats and the "schema must already be parsed" precondition.
        module subroutine schema_get_field_by_name(this, name, data_type, unit, info, ucd, array_size, col_size, &
                qc_min, qc_max, qc_miss)
            class(parquet_schema), intent(in) :: this !! schema to query.
            character(len=*), intent(in) :: name !! field name to look up.
            character(len=:), allocatable, intent(out), optional :: data_type !! field's data type.
            character(len=:), allocatable, intent(out), optional :: unit !! unit of measurement, if declared.
            character(len=:), allocatable, intent(out), optional :: info !! short description, if declared.
            character(len=:), allocatable, intent(out), optional :: ucd !! IVOA Unified Content Descriptor, if declared.
            integer, intent(out), optional :: array_size !! maximum string length (string fields only).
            integer, intent(out), optional :: col_size !! vector-column element count.
            character(len=:), allocatable, intent(out), optional :: qc_min !! Reconstructed qc: min: bound, operator-prefixed;
            !! empty if none was declared.
            character(len=:), allocatable, intent(out), optional :: qc_max !! Reconstructed qc: max: bound, operator-prefixed;
            !! empty if none was declared.
            character(len=:), allocatable, intent(out), optional :: qc_miss !! "Null" if this field allows Nulls
            !! (declared miss: Null/NA, or no miss: at all), else "" for the explicit empty miss: that asks
            !! for Null validation.
        end subroutine schema_get_field_by_name
        !> Forwards to %cinfo%get_field_by_index -- see get_field_by_name above for the full
        !> per-argument contract; the only difference is the lookup key (1-based MAML source
        !> position instead of name) and that `name` itself is returned (not optional, since the
        !> caller doesn't already know it).
        module subroutine schema_get_field_by_index(this, index, name, data_type, unit, info, ucd, array_size, &
                col_size, qc_min, qc_max, qc_miss)
            class(parquet_schema), intent(in) :: this !! schema to query.
            integer, intent(in) :: index !! 1-based field position in MAML source order.
            character(len=:), allocatable, intent(out) :: name !! field name at that position.
            character(len=:), allocatable, intent(out), optional :: data_type !! field's data type.
            character(len=:), allocatable, intent(out), optional :: unit !! unit of measurement, if declared.
            character(len=:), allocatable, intent(out), optional :: info !! short description, if declared.
            character(len=:), allocatable, intent(out), optional :: ucd !! IVOA Unified Content Descriptor, if declared.
            integer, intent(out), optional :: array_size !! maximum string length (string fields only).
            integer, intent(out), optional :: col_size !! vector-column element count.
            character(len=:), allocatable, intent(out), optional :: qc_min !! Reconstructed qc: min: bound, operator-prefixed;
            !! empty if none was declared.
            character(len=:), allocatable, intent(out), optional :: qc_max !! Reconstructed qc: max: bound, operator-prefixed;
            !! empty if none was declared.
            character(len=:), allocatable, intent(out), optional :: qc_miss !! "Null" if this field allows Nulls
            !! (declared miss: Null/NA, or no miss: at all), else "" for the explicit empty miss: that asks
            !! for Null validation.
        end subroutine schema_get_field_by_index
        !> Copies `name`'s full field definition from `source_schema` (via %get_field) and
        !> appends an equivalent field here via %add_field -- so two schemas can share a column
        !> definition (e.g. a handful of "identity" columns common to several output tables)
        !> without the caller re-typing its type/unit/info/qc by hand and risking drift between
        !> the copies. `source_schema` must already be parsed (see %get_field); `this` must
        !> already have %init called, exactly like a direct %add_field call would require.
        !> Subject to the same qc_min/qc_max/qc_miss round-trip caveats as %get_field: the copy
        !> is semantically equivalent to the source field, not necessarily a byte-identical MAML
        !> re-declaration.
        module subroutine schema_add_field_from(this, source_schema, name)
            class(parquet_schema), intent(inout) :: this !! schema gaining the copied field (%init already called).
            type(parquet_schema), intent(in) :: source_schema !! already-parsed schema to copy `name` from.
            character(len=*), intent(in) :: name !! name of the field to copy (looked up in source_schema).
        end subroutine schema_add_field_from
        !> Writes a fixed-width, aligned listing of this schema's enabled (is_set) columns, one
        !> per output line, in the order name/unit/data_type/col_size/ucd/info -- the last (info)
        !> column is left unpadded so no line carries trailing whitespace. Column widths are
        !> derived from the longest value actually present across the enabled columns (and, when
        !> header=.true., the header label itself), so each call produces its own self-contained,
        !> internally-aligned block; two calls for different schemas are not aligned with each
        !> other. Exactly one of unit/filename must identify the destination: unit (an
        !> already-open unit, e.g. opened once by the caller and reused across several schemas'
        !> worth of calls to build up one combined listing) or filename (opened here with
        !> position="append", written, and closed again before returning). Giving neither, or an
        !> unopened/read-only unit, or a unit+filename pair where filename does not match (exact,
        !> trimmed string equality against inquire(unit=unit, name=)) the file unit is already
        !> connected to, all error stop. Calling this on a schema that has not been parsed yet
        !> (schema%cinfo not populated -- neither parquet_parse_maml nor, for an in-code schema,
        !> %init/%add_field followed by parquet_parse_maml, has run) also error stops by default,
        !> with the message "schema is not initialized (not parsed)" -- pass
        !> allow_uninitialized=.true. to silently print nothing instead (a complete no-op: no file
        !> is opened/touched, even in filename= mode) rather than aborting. Use schema%is_parsed()
        !> to check readiness before calling, rather than schema%is_init(): a from-scratch schema
        !> that has only had %init/%add_field called (never parsed) has is_init() == .true. but
        !> is_parsed() == .false., and would still error stop here. Conversely, a schema loaded via
        !> parquet_parse_maml (from a file or an already-populated object) is fully valid here even
        !> though it never calls %init -- is_parsed() == .true. covers that case too.
        module subroutine schema_print_schema_info(this, unit, filename, prefix, header, table_name, &
                dash_before_header, dash_after_header, dash_after_fields, dash_char, allow_uninitialized)
            class(parquet_schema), intent(in) :: this !! schema whose enabled (is_set) columns are listed.
            integer, intent(in), optional :: unit !! already-open unit to write to (see filename for the alternative).
            character(len=*), intent(in), optional :: filename !! output path; opened with position="append" if unit absent.
            character(len=*), intent(in), optional :: prefix !! prepended to every emitted line (default: none).
            logical, intent(in), optional :: header !! print a "name unit type len ucd info" header row (default .true.).
            logical, intent(in), optional :: table_name !! print a "Table name: <table>" line, using this schema's
            !! required MAML table: key, positioned after dash_before_header and before the header row
            !! (default .true.).
            logical, intent(in), optional :: dash_before_header !! dashed separator line before the header (default .false.).
            logical, intent(in), optional :: dash_after_header !! dashed separator line after the header (default .true.).
            logical, intent(in), optional :: dash_after_fields !! dashed separator line after the last field row
            !! (default .false.).
            character(len=1), intent(in), optional :: dash_char !! character used to draw dashed lines (default "-").
            logical, intent(in), optional :: allow_uninitialized !! if .true., an unparsed schema (%cinfo not
            !! populated) is silently skipped (no output, no error) instead of error-stopping (default .false.).
        end subroutine schema_print_schema_info
        !> Subroutine form of %add_col_qc: forwards to %maml%add_col_qc. See
        !> parquet_maml_add_col_qc (parquet_maml_base_add_col_qc.f90) for the
        !> "col, min, max, miss" input syntax and validation rules.
        module subroutine schema_add_col_qc(this, qc_input, col_name)
            class(parquet_schema), intent(inout) :: this !! schema whose %maml gains one fields: entry.
            character(len=*), intent(in) :: qc_input !! compact "col, min, max, miss" string.
            character(len=:), allocatable, intent(out), optional :: col_name !! parsed column name.
        end subroutine schema_add_col_qc
        !> In-place form of %add_col_qc: forwards to %maml%set_col_qc. `col_name` is
        !> `intent(inout)`, not separate in/out arguments: on entry it holds the compact
        !> "col, min, max, miss" string, and on exit it holds just the parsed column name --
        !> so a caller reuses one variable (call schema%set_col_qc(col)) rather than
        !> assigning a function result back into it (a subroutine can't alias the same
        !> actual argument to separate intent(in)/intent(out) dummies).
        module subroutine schema_set_col_qc(this, col_name)
            class(parquet_schema), intent(inout) :: this !! schema whose %maml gains one fields: entry.
            character(len=:), allocatable, intent(inout) :: col_name !! compact "col, min, max, miss" string on
            !! entry; parsed column name on exit.
        end subroutine schema_set_col_qc
        !> int32 specific of %add_metadata; forwards to %metadata%add_metadata
        !> (see the parquet_get_metadata generic interface above for the
        !> read-side counterpart and its stored-representation semantics).
        !> Error stops if the schema has not been parsed yet (%cinfo not
        !> populated -- parquet_parse_maml must run before %add_metadata is
        !> called, not after), since an entry added before that point would
        !> otherwise be silently discarded when %cinfo%col/%metadata%items are
        !> (re)built by the parse that follows.
        module subroutine schema_add_metadata_int32(this, key, value, description, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int32), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_int32
        !> int64 specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_int64(this, key, value, description, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int64), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_int64
        !> float32 specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_float32(this, key, value, description, fmt, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real32), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_float32
        !> float64 specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_float64(this, key, value, description, fmt, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real64), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_float64
        !> logical specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_logical(this, key, value, description, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            logical, intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_logical
        !> string specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_string(this, key, value, description, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_string
        !> int32 array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_int32_array(this, key, value, description, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int32), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_int32_array
        !> int64 array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_int64_array(this, key, value, description, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int64), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_int64_array
        !> float32 array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_float32_array(this, key, value, description, fmt, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real32), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_float32_array
        !> float64 array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_float64_array(this, key, value, description, fmt, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real64), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_float64_array
        !> logical array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_logical_array(this, key, value, description, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            logical, intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_logical_array
        !> string array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_string_array(this, key, value, description, warn)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine schema_add_metadata_string_array
        !> Forwards to %metadata%clear_metadata: discards every %add_metadata entry added
        !> after the most recent parquet_parse_maml, keeping the base (header keys +
        !> keyarray:) entries from the schema's %init/%add_field build or MAML source.
        module subroutine schema_clear_metadata(this)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata is truncated.
        end subroutine schema_clear_metadata
        !> 1-based index of `name` in this%col; error stops if `name` is not found.
        module integer function get_column_index(this, name)
            class(parquet_column_info), intent(in) :: this !! column_info to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function get_column_index
        !> Whether `name` is currently enabled to be written (its col(:)%is_set); error stops if
        !> `name` is not found (via get_column_index).
        module logical function is_column_set(this, name)
            class(parquet_column_info), intent(in) :: this !! column_info to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function is_column_set
        !> Total number of fields defined in this column_info, in maml source
        !> order, with no filtering by is_set/is_deactivated -- i.e. every
        !> field that was ever declared (via a fields: entry or %add_field).
        module integer function get_num_fields(this)
            class(parquet_column_info), intent(in) :: this !! column_info to query.
        end function get_num_fields
        !> Name of the field at the given 1-based position in maml source
        !> order (same order get_num_fields counts). index must be between 1
        !> and get_num_fields(this); anything outside that range fails with
        !> error stop.
        module subroutine get_field_name(this, index, name)
            class(parquet_column_info), intent(in) :: this !! column_info to query.
            integer, intent(in) :: index !! 1-based field position in MAML source order.
            character(len=:), allocatable, intent(out) :: name !! field name at that position.
        end subroutine get_field_name
        !> Reads back `name`'s full field definition -- the same shape of values %add_field
        !> accepts, so a caller can inspect an already-parsed schema's columns or feed the result
        !> straight into another schema's %add_field (see %add_field_from, parquet_schema's own
        !> convenience wrapper built on top of this). Every output is optional/intent(out); pass
        !> only the ones you need. qc_min/qc_max are reconstructed as a single operator-prefixed
        !> string (e.g. ">= 5.0"), re-feedable into %add_field's own qc_min/qc_max arguments --
        !> note this always carries an explicit operator, even if the original %add_field call
        !> left it implicit (">=" for min, "<=" for max), which is semantically identical but not
        !> necessarily byte-identical to the original input. qc_miss comes back as "Null" or ""
        !> (never "NA", even if that's what was originally declared -- both are equivalent
        !> aliases and the distinction isn't preserved in storage). Error stops if `name` is not
        !> found. Requires this column_info to be populated: parquet_parse_maml for a schema loaded
        !> from a file, or -- for a from-scratch schema -- at least one %add_field, which parses
        !> what it appends and so needs no separate parse step (see schema_sync_appended_lines).
        module subroutine get_field_by_name(this, name, data_type, unit, info, ucd, array_size, col_size, &
                qc_min, qc_max, qc_miss)
            class(parquet_column_info), intent(in) :: this !! column_info to query.
            character(len=*), intent(in) :: name !! field name to look up.
            character(len=:), allocatable, intent(out), optional :: data_type !! field's data type.
            character(len=:), allocatable, intent(out), optional :: unit !! unit of measurement, if declared.
            character(len=:), allocatable, intent(out), optional :: info !! short description, if declared.
            character(len=:), allocatable, intent(out), optional :: ucd !! IVOA Unified Content Descriptor, if declared.
            integer, intent(out), optional :: array_size !! maximum string length (string fields only).
            integer, intent(out), optional :: col_size !! vector-column element count.
            character(len=:), allocatable, intent(out), optional :: qc_min !! Reconstructed qc: min: bound, operator-prefixed;
            !! empty if none was declared.
            character(len=:), allocatable, intent(out), optional :: qc_max !! Reconstructed qc: max: bound, operator-prefixed;
            !! empty if none was declared.
            character(len=:), allocatable, intent(out), optional :: qc_miss !! "Null" when this field allows Nulls
            !! (a declared qc: miss: Null/NA, or no miss: declared at all -- both mean the same thing), else ""
            !! for an explicit empty qc: miss:, which is the one form that asks for Null validation.
        end subroutine get_field_by_name
        !> Same as get_field_by_name, but looks the field up by its 1-based MAML source position
        !> (same order get_num_fields counts) instead of by name, additionally returning that
        !> field's name (always populated, not optional, since the caller doesn't already know
        !> it). index must be between 1 and get_num_fields(this); anything outside that range
        !> fails with error stop.
        module subroutine get_field_by_index(this, index, name, data_type, unit, info, ucd, array_size, col_size, &
                qc_min, qc_max, qc_miss)
            class(parquet_column_info), intent(in) :: this !! column_info to query.
            integer, intent(in) :: index !! 1-based field position in MAML source order.
            character(len=:), allocatable, intent(out) :: name !! field name at that position.
            character(len=:), allocatable, intent(out), optional :: data_type !! field's data type.
            character(len=:), allocatable, intent(out), optional :: unit !! unit of measurement, if declared.
            character(len=:), allocatable, intent(out), optional :: info !! short description, if declared.
            character(len=:), allocatable, intent(out), optional :: ucd !! IVOA Unified Content Descriptor, if declared.
            integer, intent(out), optional :: array_size !! maximum string length (string fields only).
            integer, intent(out), optional :: col_size !! vector-column element count.
            character(len=:), allocatable, intent(out), optional :: qc_min !! Reconstructed qc: min: bound, operator-prefixed;
            !! empty if none was declared.
            character(len=:), allocatable, intent(out), optional :: qc_max !! Reconstructed qc: max: bound, operator-prefixed;
            !! empty if none was declared.
            character(len=:), allocatable, intent(out), optional :: qc_miss !! "Null" when this field allows Nulls
            !! (a declared qc: miss: Null/NA, or no miss: declared at all -- both mean the same thing), else ""
            !! for an explicit empty qc: miss:, which is the one form that asks for Null validation.
        end subroutine get_field_by_index
        !> Backs parquet_schema%set_column_unavailable (see set_column_unavailable
        !> in the parquet_schema block above); disables `name`, or every
        !> non-deactivated column if `name` is absent. Error stops if `name`
        !> is a deactivated column.
        module subroutine set_unavailable(this, name)
            class(parquet_column_info), intent(inout) :: this !! column_info being updated.
            character(len=*), intent(in), optional :: name !! column to disable; every column if absent.
        end subroutine set_unavailable
        !> Backs parquet_schema%set_column_available; enables `name`, or every
        !> non-deactivated column if `name` is absent. Error stops if `name`
        !> is a deactivated column.
        module subroutine set_available(this, name)
            class(parquet_column_info), intent(inout) :: this !! column_info being updated.
            character(len=*), intent(in), optional :: name !! column to enable; every column if absent.
        end subroutine set_available
        !> Resolves `name`'s col_size to `col_size`. Error stops if col_size < 1, if `name` is not
        !> found (via get_column_index), or if `name`'s col_size is not currently "auto" and
        !> force is absent/.false. (pass force=.true. to override an already-resolved col_size too).
        module subroutine set_col_size(this, name, col_size, force)
            class(parquet_column_info), intent(inout) :: this !! column_info being updated.
            character(len=*), intent(in) :: name !! column to resolve.
            integer, intent(in) :: col_size !! new col_size (must be a positive integer).
            logical, intent(in), optional :: force !! .true. allows overriding a non-"auto" col_size (default .false.).
        end subroutine set_col_size
        !> Marks `name` Null-protected, or (protected=.false.) unmarks it -- the code-level
        !> equivalent of listing it under a MAML's extra: protected_cols:. A protected column may
        !> hold no Null at all: parquet_write_column error stops on an is_valid mask with any
        !> .false. entry, on a null date/time/timestamp element, and on an %append_null() in a
        !> parquet_string_column, and the column's Arrow field is written non-nullable.
        !>
        !> Unprotecting a column that is currently protected is allowed -- a program may have a good
        !> reason to relax its own schema -- but emits a WARNING naming the column, since it
        !> overrides a declaration someone made deliberately. Never an abort. The warning does not
        !> depend on where the protection came from: a MAML's extra: protected_cols: and an earlier
        !> set_protected call in code are indistinguishable here, and both warn.
        !>
        !> Error stops if `name` is not found. Call it before parquet_open_writer: the writer
        !> takes its copy of the schema at open time, so a later change has no effect on a writer
        !> that is already open.
        module subroutine set_protected(this, name, protected)
            class(parquet_column_info), intent(inout) :: this !! column_info being updated.
            character(len=*), intent(in) :: name !! column to mark (internal name, as declared).
            logical, intent(in), optional :: protected !! .false. to unprotect; default .true.
        end subroutine set_protected
        !> Resolves `name`'s array_size to `array_size`. Error stops if array_size < 1, if `name`
        !> is not found, if `name`'s data_type is not "string" (array_size only applies to string
        !> columns), or if `name`'s array_size is not currently "auto" and force is absent/.false.
        module subroutine set_array_size(this, name, array_size, force)
            class(parquet_column_info), intent(inout) :: this !! column_info being updated.
            character(len=*), intent(in) :: name !! string column to resolve.
            integer, intent(in) :: array_size !! new array_size (must be a positive integer).
            logical, intent(in), optional :: force !! .true. allows overriding a non-"auto" array_size (default .false.).
        end subroutine set_array_size
        !> int32 specific of parquet_table_metadata%add_metadata; stores
        !> `value` as plain text via parquet_metadata_append_entry.
        module subroutine add_metadata_int32(this, key, value, description, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int32), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_int32
        !> int64 specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_int64(this, key, value, description, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int64), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_int64
        !> float32 specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_float32(this, key, value, description, fmt, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real32), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_float32
        !> float64 specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_float64(this, key, value, description, fmt, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real64), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_float64
        !> logical specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_logical(this, key, value, description, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            logical, intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_logical
        !> string specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_string(this, key, value, description, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_string
        !> int32 array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_int32_array(this, key, value, description, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int32), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_int32_array
        !> int64 array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_int64_array(this, key, value, description, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int64), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_int64_array
        !> float32 array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_float32_array(this, key, value, description, fmt, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real32), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_float32_array
        !> float64 array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_float64_array(this, key, value, description, fmt, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real64), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_float64_array
        !> logical array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_logical_array(this, key, value, description, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            logical, intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_logical_array
        !> string array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_string_array(this, key, value, description, warn)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
        end subroutine add_metadata_string_array
        !> Truncates %items back to %n_base_items, discarding every entry appended by an
        !> %add_metadata call made since the most recent parquet_parse_maml -- a no-op if
        !> %items currently holds %n_base_items entries or fewer (nothing to discard).
        module subroutine metadata_clear_metadata(this)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata being truncated.
        end subroutine metadata_clear_metadata
    end interface

    ! ---- Writer concurrency guard (see the writer_lock type) ----
    interface
        !> Claims `writer`'s concurrency guard for the calling thread, aborting with the diagnostic
        !> in parquet_wrapper.cpp's ConcurrencyGuard if another thread already holds it. The claim
        !> lasts until `self` is finalized, which Fortran does on every exit path from the
        !> procedure `self` is a local of. Claiming an unopened writer is a no-op, so a caller does
        !> not need to order this against its own check_writer_open (though every caller does that
        !> check first anyway, for the better message).
        module subroutine writer_lock_claim(self, writer)
            class(writer_lock), intent(inout) :: self !! lock that will hold the guard until it dies.
            type(parquet_writer), intent(in) :: writer !! writer whose guard is being claimed.
        end subroutine writer_lock_claim
    end interface

    ! ---- Writer lifecycle & column management ----
    interface
        !> 1-based index of `name` in writer%enabled_columns (currently
        !> enabled/set columns only), or 0 if not found among them.
        module integer function parquet_get_enabled_column_index(writer, name)
            type(parquet_writer), intent(in) :: writer !! open writer to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function parquet_get_enabled_column_index
        !> 1-based index of `name` in writer%all_columns (every declared
        !> column, enabled or not), or 0 if not found among them. Assumes
        !> writer%all_columns is allocated -- every caller only calls this
        !> when writer%is_schema_enforced is true, which is set exactly when
        !> parquet_open_writer also allocates writer%all_columns.
        module integer function parquet_get_defined_column_index(writer, name)
            type(parquet_writer), intent(in) :: writer !! open writer to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function parquet_get_defined_column_index
        !> True if a parquet_write_column call declaring `expected_type`
        !> (the actual/declared Fortran type/kind of `values`) is compatible
        !> with the schema's own `actual_type` for that column -- exact match,
        !> or a numeric widening (e.g. int32/int64 values written into a
        !> float32/float64 schema column).
        module logical function parquet_is_type_compatible(actual_type, expected_type)
            character(len=*), intent(in) :: actual_type !! schema-declared data_type for the column.
            character(len=*), intent(in) :: expected_type !! data_type implied by the write call's own values.
        end function parquet_is_type_compatible
        !> Error stops if `name` is defined with a data_type incompatible with
        !> `expected_type` (see parquet_is_type_compatible). Assumes `name` is
        !> already known to be a defined column -- every caller checks that
        !> itself (and error stops on a not-defined column) before calling this.
        !> Used by BOTH write paths: parquet_write_column and
        !> parquet_write_column_chunk apply the same compatibility rule, so the
        !> same values are accepted for a given schema either way (see
        !> parquet_append_as_schema_int32 and its _chunk_ twin, which perform the
        !> conversion the rule permits).
        module subroutine parquet_assert_column_type(writer, name, expected_type, context)
            type(parquet_writer), intent(in) :: writer !! open writer to check against.
            character(len=*), intent(in) :: name !! column name being written.
            character(len=*), intent(in) :: expected_type !! data_type implied by the write call's own values.
            character(len=*), intent(in), optional :: context !! calling procedure named in the error
            !! message; defaults to "parquet_write_column", the chunked path passes its own name.
        end subroutine parquet_assert_column_type
        !> True if `name` is a currently enabled/set column of a schema-
        !> enforced writer (see parquet_column_type%is_set); always .true.
        !> for a schema-less writer once the column has been defined.
        module logical function parquet_is_column_enabled(writer, name)
            type(parquet_writer), intent(in) :: writer !! open writer to check.
            character(len=*), intent(in) :: name !! column name to look up.
        end function parquet_is_column_enabled
        !> The declared col_size (vector-column element count) for `name`,
        !> from the writer's schema; error stops if `name` is not defined, or if
        !> `name`'s col_size is still "auto" (unresolved -- the flat/1-D write
        !> form cannot resolve it itself; call schema%set_col_size before
        !> parquet_open_writer, or use the matrix write form instead).
        module integer function parquet_get_column_col_size(writer, name)
            type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer to check.
            character(len=*), intent(in) :: name !! column name to look up.
        end function parquet_get_column_col_size
        !> Creates `filename` and opens `writer` for writing. Without `schema`,
        !> the writer is schema-less: columns are inferred from the first
        !> parquet_write_column call for each name, with no field metadata/QC.
        !> With `schema`, every column/type/QC rule is fixed up front and
        !> enforced on every write. By default (`overwrite=.true.`) an existing
        !> file at `filename` is silently truncated; pass `overwrite=.false.`
        !> to instead fail immediately with `error stop` if `filename` already
        !> exists, rather than clobbering it.
        module subroutine parquet_open_writer(writer, filename, schema, write_maml, qc, &
                compression, compression_level, chunk_size, use_threads, overwrite)
            type(parquet_writer), intent(out) :: writer !! writer to open.
            character(len=*), intent(in) :: filename !! output .parquet path.
            type(parquet_schema), intent(in), optional :: schema !! schema to enforce; schema-less writer if absent.
            logical, intent(in), optional :: write_maml !! also save a sidecar .maml next to filename (needs schema).
            logical, intent(in), optional :: qc !! enable qc: min/max/miss WARNING checks on write; defaults to
            !! present(schema) (on whenever a schema is given), pass .false. to opt out; no-op without a schema.
            character(len=*), intent(in), optional :: compression !! Arrow compression codec name (e.g. "snappy", "zstd").
            integer, intent(in), optional :: compression_level !! codec-specific compression level.
            integer, intent(in), optional :: chunk_size !! Parquet row-group size.
            logical, intent(in), optional :: use_threads !! use Arrow's multi-threaded writer.
            logical, intent(in), optional :: overwrite !! allow truncating an existing file at filename; default .true.
        end subroutine parquet_open_writer
        !> Sole specific of parquet_write_row_mask -- see the generic interface above.
        module subroutine parquet_write_row_mask_impl(writer, mask)
            type(parquet_writer), intent(inout) :: writer !! open writer, before its first write/row group.
            logical, intent(in) :: mask(:) !! .false. drops that row entirely; kept rows preserve order.
        end subroutine parquet_write_row_mask_impl
        !> Sole specific of parquet_write_chunk_row_mask -- see the generic interface above.
        module subroutine parquet_write_chunk_row_mask_impl(writer, mask)
            type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
            logical, intent(in) :: mask(:) !! .false. drops that row entirely; must be this row group's own nrows long.
        end subroutine parquet_write_chunk_row_mask_impl
        !> int32 specific of parquet_new_row_group -- see the generic interface above.
        module subroutine parquet_new_row_group_int32(writer, nrows)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            integer(int32), intent(in) :: nrows !! row count for the new row group; must be positive.
        end subroutine parquet_new_row_group_int32
        !> int64 specific of parquet_new_row_group -- see the generic interface above.
        module subroutine parquet_new_row_group_int64(writer, nrows)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            integer(int64), intent(in) :: nrows !! row count for the new row group; must be positive.
        end subroutine parquet_new_row_group_int64
        !> Ends the currently-open row group -- see parquet_new_row_group above. Error stops if
        !> any column known to `writer` has no data for this row group (either a chunk just
        !> written, or an already-whole column with enough rows left to slice). On the very
        !> first call for `writer`, also locks the file's schema (from every column established
        !> by then) and opens it for writing -- no column can be introduced after this point.
        module subroutine parquet_finish_row_group(writer)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
        end subroutine parquet_finish_row_group
        !> Writer, int32 specific of parquet_get_chunk_size -- see the generic interface above.
        module subroutine parquet_get_chunk_size_writer_int32(writer, chunk_size)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            integer(int32), intent(out) :: chunk_size !! writer's resolved/authoritative row-group size.
        end subroutine parquet_get_chunk_size_writer_int32
        !> Writer, int64 specific of parquet_get_chunk_size -- see the generic interface above.
        module subroutine parquet_get_chunk_size_writer_int64(writer, chunk_size)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            integer(int64), intent(out) :: chunk_size !! writer's resolved/authoritative row-group size.
        end subroutine parquet_get_chunk_size_writer_int64
        !> Flushes and closes `writer`; error stops if SOME (but not all) of a schema-enforced
        !> writer's declared/enabled columns were written -- naming the column that was missed.
        !>
        !> A writer that had NOTHING written to it is not an error: every enabled column is written
        !> with zero rows and a WARNING says so, so an analysis that legitimately produced no rows
        !> still yields a valid file carrying the whole schema (see
        !> parquet_write_empty_columns_if_none_written). Writing a zero-length array to every column
        !> by hand produces the same file and no warning. Two cases are excluded and keep the abort:
        !> a row mask was set (rows were expected), and a row group was opened.
        module subroutine parquet_close_writer(writer)
            type(parquet_writer), intent(inout) :: writer !! writer to close.
        end subroutine parquet_close_writer
    end interface

    ! ---- Write column specifics (numeric / string / temporal) ----
    interface
        !> Writes a scalar int32 column; see the parquet_write_column generic
        !> interface above for the shared behavior of this whole family.
        module subroutine parquet_write_int32_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_int32_column
        !> Writes a vector (matrix) int32 column, one row per column of
        !> `values`; see parquet_write_column above.
        module subroutine parquet_write_int32_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_int32_matrix_column
        !> Writes a scalar int64 column; see parquet_write_column above.
        module subroutine parquet_write_int64_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_int64_column
        !> Writes a vector (matrix) int64 column; see parquet_write_column above.
        module subroutine parquet_write_int64_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_int64_matrix_column
        !> Writes a scalar float32 column; see parquet_write_column above.
        module subroutine parquet_write_float32_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            real(real32), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_float32_column
        !> Writes a vector (matrix) float32 column; see parquet_write_column above.
        module subroutine parquet_write_float32_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            real(real32), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_float32_matrix_column
        !> Writes a scalar float64 column; see parquet_write_column above.
        module subroutine parquet_write_float64_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            real(real64), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_float64_column
        !> Writes a vector (matrix) float64 column; see parquet_write_column above.
        module subroutine parquet_write_float64_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            real(real64), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_float64_matrix_column
        !> Writes a scalar logical (boolean) column; see parquet_write_column above.
        module subroutine parquet_write_logical_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            logical, intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_logical_column
        !> Writes a vector (matrix) logical (boolean) column; see parquet_write_column above.
        module subroutine parquet_write_logical_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            logical, intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_logical_matrix_column
        !> Writes a scalar string column; see parquet_write_column above.
        module subroutine parquet_write_string_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_string_column
        !> Writes a vector (matrix) string column; see parquet_write_column above.
        module subroutine parquet_write_string_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_string_matrix_column
        !> Compact (parquet_string_column) specific of parquet_write_column; see the generic
        !> interface's own doc comment above for the parquet_string_column notes.
        module subroutine parquet_write_string_column_compact(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_string_column), intent(in), target :: values !! one value (or Null) per row;
            !! target so raw_buffers can be called on it without copying.
        end subroutine parquet_write_string_column_compact
        !> Variable-length LIST specific of parquet_write_column: writes `values` as a genuine
        !> `LIST` column, with its per-row lengths, its null rows and its null elements intact.
        !>
        !> There is no is_valid argument, and none is needed: a parquet_list_column carries its own
        !> nullness at both levels, so a second channel for the same fact could only disagree with
        !> the first -- the same reasoning that keeps one off parquet_write_string_column_compact
        !> and off every temporal specific.
        !>
        !> On a schema-enforced writer the column must be declared `list[<elemtype>]` with an
        !> element type matching this column's payload kind exactly; there is no widening between
        !> element kinds the way a scalar numeric column has (see parquet_is_type_compatible).
        module subroutine parquet_write_list_column(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_list_column), intent(in), target :: values !! one list (or null list) per row;
            !! target so the write path can alias its offsets and payload rather than copying them.
        end subroutine parquet_write_list_column
        !> Struct (parquet_struct_column) specific of parquet_write_column.
        !>
        !> Writes a `STRUCT` column as ONE object: the file's field set, names and order come from
        !> `values` itself, not from the schema, which declares only the bare `struct` token. The
        !> struct's own per-row nullness and each field's own nullness are written independently,
        !> so a row whose every field is null is not written as a null row.
        !>
        !> A protected column (`extra: protected_cols:`) may contain no Null at EITHER level.
        module subroutine parquet_write_struct_column(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_struct_column), intent(in), target :: values !! the struct column to write.
        end subroutine parquet_write_struct_column
        !> Map (parquet_map_column) specific of parquet_write_column.
        !>
        !> Writes a genuine Parquet `MAP` column -- entries in the order they were appended,
        !> duplicate keys preserved as given, and both null levels (a null ROW, and a null VALUE)
        !> carried across. On a schema-enforced writer the column must be declared
        !> `map[<valuetype>]` and the value type must match exactly; there is no widening.
        !>
        !> **A map column's entry count is bounded by `huge(int32)` and there is no way around it.**
        !> Arrow addresses a map's entries with an int32 offsets buffer and provides no `large_map`
        !> to widen into, unlike a string column (`large_utf8`) or a list column (`large_list`), so
        !> a column that does not fit is refused rather than written in a wider form.
        module subroutine parquet_write_map_column(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_map_column), intent(in) :: values !! the map column to write.
        end subroutine parquet_write_map_column
        !> Scalar date specific of parquet_write_column. Null elements (see parquet_date%is_null)
        !> are written as genuine Parquet Nulls; there is no is_valid argument -- validity lives
        !> in the elements themselves.
        module subroutine parquet_write_date_column(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_date), intent(in) :: values(:) !! one date (or null element) per row.
        end subroutine parquet_write_date_column
        !> Vector (matrix) date specific of parquet_write_column, one row per column of `values`.
        module subroutine parquet_write_date_matrix_column(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_date), intent(in) :: values(:,:) !! (element, row) dates.
        end subroutine parquet_write_date_matrix_column
        !> Scalar time specific of parquet_write_column; null elements become Parquet Nulls.
        module subroutine parquet_write_time_column(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_time), intent(in) :: values(:) !! one time (or null element) per row.
        end subroutine parquet_write_time_column
        !> Vector (matrix) time specific of parquet_write_column.
        module subroutine parquet_write_time_matrix_column(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_time), intent(in) :: values(:,:) !! (element, row) times.
        end subroutine parquet_write_time_matrix_column
        !> Scalar timestamp specific of parquet_write_column; null elements become Parquet Nulls.
        module subroutine parquet_write_timestamp_column(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_timestamp), intent(in) :: values(:) !! one instant (or null element) per row.
        end subroutine parquet_write_timestamp_column
        !> Vector (matrix) timestamp specific of parquet_write_column.
        module subroutine parquet_write_timestamp_matrix_column(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_timestamp), intent(in) :: values(:,:) !! (element, row) instants.
        end subroutine parquet_write_timestamp_matrix_column
        !> Scalar date specific of parquet_write_column_chunk (one row group's worth); nulls come
        !> from the elements, so there is no is_valid argument.
        module subroutine parquet_write_date_column_chunk(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_date), intent(in) :: values(:) !! this row group's dates.
        end subroutine parquet_write_date_column_chunk
        !> Vector date specific of parquet_write_column_chunk.
        module subroutine parquet_write_date_matrix_column_chunk(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_date), intent(in) :: values(:,:) !! (element, row) dates for this row group.
        end subroutine parquet_write_date_matrix_column_chunk
        !> Scalar time specific of parquet_write_column_chunk.
        module subroutine parquet_write_time_column_chunk(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_time), intent(in) :: values(:) !! this row group's times.
        end subroutine parquet_write_time_column_chunk
        !> Vector time specific of parquet_write_column_chunk.
        module subroutine parquet_write_time_matrix_column_chunk(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_time), intent(in) :: values(:,:) !! (element, row) times for this row group.
        end subroutine parquet_write_time_matrix_column_chunk
        !> Scalar timestamp specific of parquet_write_column_chunk.
        module subroutine parquet_write_timestamp_column_chunk(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_timestamp), intent(in) :: values(:) !! this row group's instants.
        end subroutine parquet_write_timestamp_column_chunk
        !> Vector timestamp specific of parquet_write_column_chunk.
        module subroutine parquet_write_timestamp_matrix_column_chunk(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_timestamp), intent(in) :: values(:,:) !! (element, row) instants for this row group.
        end subroutine parquet_write_timestamp_matrix_column_chunk
        !> Writes a scalar int32 column's chunk for the currently-open row group; see
        !> parquet_write_column_chunk above for the shared behavior of this whole family.
        module subroutine parquet_write_int32_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: values(:) !! one value per row of the open row group.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_int32_column_chunk
        !> Writes a vector (matrix) int32 column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_int32_matrix_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: values(:,:) !! (element, row) values of the open row group.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_int32_matrix_column_chunk
        !> Writes a scalar int64 column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_int64_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: values(:) !! one value per row of the open row group.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_int64_column_chunk
        !> Writes a vector (matrix) int64 column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_int64_matrix_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: values(:,:) !! (element, row) values of the open row group.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_int64_matrix_column_chunk
        !> Writes a scalar float32 column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_float32_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            real(real32), intent(in) :: values(:) !! one value per row of the open row group.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_float32_column_chunk
        !> Writes a vector (matrix) float32 column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_float32_matrix_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            real(real32), intent(in) :: values(:,:) !! (element, row) values of the open row group.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_float32_matrix_column_chunk
        !> Writes a scalar float64 column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_float64_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            real(real64), intent(in) :: values(:) !! one value per row of the open row group.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_float64_column_chunk
        !> Writes a vector (matrix) float64 column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_float64_matrix_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            real(real64), intent(in) :: values(:,:) !! (element, row) values of the open row group.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_float64_matrix_column_chunk
        !> Writes a scalar logical (boolean) column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_logical_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            logical, intent(in) :: values(:) !! one value per row of the open row group.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_logical_column_chunk
        !> Writes a vector (matrix) logical (boolean) column's chunk; see
        !> parquet_write_column_chunk above.
        module subroutine parquet_write_logical_matrix_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            logical, intent(in) :: values(:,:) !! (element, row) values of the open row group.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_logical_matrix_column_chunk
        !> Writes a scalar string column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_string_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(in) :: values(:) !! one value per row of the open row group.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_string_column_chunk
        !> Writes a vector (matrix) string column's chunk; see parquet_write_column_chunk above.
        module subroutine parquet_write_string_matrix_column_chunk(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(in) :: values(:,:) !! (element, row) values of the open row group.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_string_matrix_column_chunk
        !> Compact (parquet_string_column) specific of parquet_write_column_chunk; see the generic
        !> interface's own doc comment above for the parquet_string_column notes.
        module subroutine parquet_write_string_column_chunk_compact(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_string_column), intent(in), target :: values !! one value (or Null) per row of the
            !! open row group; target so raw_buffers can be called on it without copying.
        end subroutine parquet_write_string_column_chunk_compact
        !> Variable-length LIST specific of parquet_write_column_chunk; see
        !> parquet_write_list_column above for the shared notes. `values` holds exactly this row
        !> group's rows.
        !>
        !> A streamed list column's field is nullable at BOTH levels unless the column is
        !> protected, because a parquet_list_column carries its null state inside itself and this
        !> row group therefore says nothing about what a later one will hold -- the same rule that
        !> already applies to a temporal column and to a parquet_string_column.
        module subroutine parquet_write_list_column_chunk(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer, with a row group open.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_list_column), intent(in), target :: values !! this row group's rows; target for
            !! the same reason as parquet_write_list_column's own values argument.
        end subroutine parquet_write_list_column_chunk
        !> Struct (parquet_struct_column) specific of parquet_write_column_chunk; writes one row
        !> group's rows. A streamed struct column is in the ALWAYS-NULLABLE class -- see
        !> parquet_write_struct_column, and src/parquet_wrapper.cpp's own section banner.
        module subroutine parquet_write_struct_column_chunk(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_struct_column), intent(in), target :: values !! this row group's rows.
        end subroutine parquet_write_struct_column_chunk
        !> Map (parquet_map_column) specific of parquet_write_column_chunk: writes `values` as one
        !> row group of an already-declared map column. See parquet_write_map_column, and
        !> src/parquet_wrapper.cpp's "MAP column writes" section banner.
        module subroutine parquet_write_map_column_chunk(writer, name, values)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_map_column), intent(in) :: values !! this row group's rows.
        end subroutine parquet_write_map_column_chunk
        !> Writes a declared-but-never-written `list[<elemtype>]` column with ZERO rows, at close.
        !>
        !> Takes the element BASE token (`"int32"`, `"timestamp"`, ...) rather than a `PK_*` kind,
        !> so that the caller -- `parquet_write_empty_columns_if_none_written`, in the intermediate
        !> `parquet_write` submodule -- needs no `parquet_columns` name of its own. That is a
        !> compiler constraint, not a preference: nagfor 7.2 cannot compile a sibling descendant of
        !> an intermediate submodule whose own code references a `PK_*` constant use-associated
        !> into `parquet_core` ("Bad module file format for PARQUET_WRITE, could not ref PK_INT32").
        module subroutine parquet_write_empty_list_column(writer, name, elem_base)
            type(parquet_writer), intent(inout) :: writer !! open writer being closed.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(in) :: elem_base !! element base token from parquet_parse_list_type.
        end subroutine parquet_write_empty_list_column
        !> Writes a declared-but-never-written `map[<valuetype>]` column with ZERO rows, at close.
        !>
        !> The map counterpart of parquet_write_empty_list_column above, and delegated to
        !> parquet_write_map for the same nagfor reason: parquet_write.f90 is an intermediate
        !> submodule and cannot reference a PK_* constant use-associated into parquet_core.
        module subroutine parquet_write_empty_map_column(writer, name, value_base)
            type(parquet_writer), intent(inout) :: writer !! open writer being closed.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(in) :: value_base !! value base token from parquet_parse_map_type.
        end subroutine parquet_write_empty_map_column
        !> Resolves the file unit (a parquet_unit_* selector) and UTC flag a temporal write uses:
        !> the schema-declared unit/utc when the column comes from a MAML/schema, else the fixed
        !> default (microseconds, timezone-naive).
        !>
        !> Reads a CONTAINER column at `path` and hands it to `col` as a container `parquet_column`.
        !>
        !> Declared here, and implemented in parquet_read_list, so that parquet_read_map can reuse
        !> it for a nested map VALUE -- the two need the identical three lines. It cannot live in
        !> the shared parquet_read parent: it references `PK_LIST` and friends, which parquet_core
        !> use-associates from parquet_columns, and an intermediate submodule referencing such a
        !> name makes nagfor 7.2 unable to compile ANY of its descendants ("Bad module file format
        !> for PARQUET_READ, could not ref PK_LIST"). Same constraint, same shape, and the same
        !> reason as `resolve_temporal_write_unit` and `struct_field_validity` below.
        module subroutine read_nested_payload(reader, path, rg, kind, col, context)
            type(parquet_reader), intent(in) :: reader  !! open reader.
            character(len=*), intent(in) :: path        !! the descent path of the child.
            integer(c_long_long), intent(in) :: rg      !! 1-based row group, or <= 0 for the whole column.
            integer, intent(in) :: kind                 !! PK_LIST, PK_MAP or PK_STRUCT.
            type(parquet_column), intent(inout) :: col  !! receives the container.
            character(len=*), intent(in) :: context     !! calling entry point, for error messages.
        end subroutine read_nested_payload
        !> Declared here, and implemented in parquet_write_temporal, so that parquet_write_list can
        !> apply the same rule to a temporal PAYLOAD. It cannot live in the shared parquet_write
        !> parent -- see the implementation's own comment for the nagfor constraint that rules that
        !> out.
        module subroutine resolve_temporal_write_unit(writer, idx, unit, is_utc)
            type(parquet_writer), intent(in) :: writer !! open writer.
            integer, intent(in) :: idx !! schema column index, or 0 for schema-less.
            integer, intent(out) :: unit !! resolved unit selector.
            integer(c_int32_t), intent(out) :: is_utc !! 1 if UTC-adjusted, else 0.
        end subroutine resolve_temporal_write_unit
        !> Builds one staged field's per-ROW validity mask, from the two places a null can live:
        !> the three temporal kinds carry it INSIDE the element (a default-initialized
        !> parquet_date IS null), every other kind keeps it in the column's own bitmap.
        !>
        !> Declared here, and implemented in parquet_write_struct, so that parquet_write_map can
        !> apply the same rule to a map's VALUES -- a map's entries are a two-field struct, so the
        !> two writes stage through the same registry and need the same mask. It cannot live in
        !> the shared parquet_write parent: it references `PK_DATE` and friends, which
        !> parquet_core use-associates from parquet_columns, and an intermediate submodule
        !> referencing such a name makes nagfor 7.2 unable to compile ANY of its descendants
        !> ("Bad module file format for PARQUET_WRITE, could not ref PK_INT32").
        module subroutine struct_field_validity(fcol, nrows, kind, is_valid, any_null)
            type(parquet_column), intent(in) :: fcol !! the field's (or the map's values') column.
            integer(int64), intent(in) :: nrows !! rows it holds.
            integer, intent(in) :: kind !! its PK_* kind.
            logical, allocatable, intent(out) :: is_valid(:) !! .true. where the value is present.
            logical, intent(out) :: any_null !! .true. if at least one value is null.
        end subroutine struct_field_validity
        !> Stages ONE field of a struct-shaped write, dispatched on its kind: nine families, each
        !> handing over that field's values and its per-row validity through the shared
        !> parquet_struct_field_* push entry points.
        !>
        !> Declared here for the same reason as struct_field_validity above: a map column's write
        !> stages its keys and its values through this same registry, so it calls this with
        !> `fname` "key" and "value" rather than carrying a second copy of the nine-way dispatch.
        !> `unit`/`is_utc` are absent for a STRUCT field, whose temporal values are always written
        !> as timezone-naive microseconds -- a struct declares no per-field layout in MAML, so
        !> there is nothing for a declared unit to come from. A MAP's `map[timestamp[ms,utc]]`
        !> token does carry one, so parquet_write_map supplies both.
        module subroutine push_struct_field(writer, fname, fcol, kind, nrows, val_ptr, unit, is_utc)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: fname !! the field's name, as written into the file.
            type(parquet_column), intent(in) :: fcol !! the field's column.
            integer, intent(in) :: kind !! the field's PK_* kind.
            integer(int64), intent(in) :: nrows !! rows in this write.
            type(c_ptr), intent(in) :: val_ptr !! per-row validity buffer, or c_null_ptr.
            integer, intent(in), optional :: unit !! temporal unit selector; default microseconds.
            integer(c_int32_t), intent(in), optional :: is_utc !! 1 if UTC-adjusted; default 0.
        end subroutine push_struct_field
    end interface

    ! ---- Reader lifecycle & queries ----
    interface
        !> Applies `filter` to an already-open `reader`, exactly as
        !> parquet_open_reader(..., filter=) would have: every column read from
        !> here on covers only the matching rows, and parquet_get_nrows reports
        !> the filtered count. Provided for callers that do not own the
        !> parquet_open_reader call (a higher-level type that opens its reader
        !> internally, say), and for building a filter from information only
        !> available once the file's schema/metadata can be inspected.
        !>
        !> Refuses, with error stop, in three states: when the reader already
        !> has an active filter (compose the clauses into one parquet_filter
        !> instead -- several %add calls are AND-combined); when any column has
        !> already been read on this reader, whether as a whole column or
        !> through the CHUNK api, since data already returned to the caller
        !> could not then be aligned with anything read afterwards; and when the
        !> reader already has an active SORT, because apply_row_transform masks
        !> first and permutes second, so a permutation's length is the
        !> post-filter row count -- filter first, then sort.
        !> A reader opened with sample_fraction= is fine: the filter combines
        !> with the sample draw, the same way passing both to
        !> parquet_open_reader does.
        !>
        !> **Whether the row-group arguments are PRESENT chooses the engine;
        !> their value chooses the row groups.** This two-argument form (and an
        !> open-time `filter=`) reads every filter column in one batched pass
        !> and leaves it decoded, so reading that column afterwards is free --
        !> the fast default, at one copy of each filter column in memory. Any
        !> form that names row groups evaluates a row group at a time and keeps
        !> only the mask, which is what makes a filtered read possible on a file
        !> larger than memory; pass `row_group_lo = 0` for that engine over the
        !> whole file, without having to call parquet_get_num_row_groups first.
        module subroutine parquet_reader_set_filter_base(reader, filter)
            type(parquet_reader), intent(inout) :: reader !! open, unfiltered reader with no column decoded yet.
            type(parquet_filter), intent(in) :: filter !! filter whose rules are parsed, validated, and applied.
        end subroutine parquet_reader_set_filter_base
        !> Row-group-scoped form, int32 bounds -- see the generic interface above.
        module subroutine parquet_reader_set_filter_scoped_int32(reader, filter, row_group_lo, row_group_hi)
            type(parquet_reader), intent(inout) :: reader !! open, unfiltered reader with no column decoded yet.
            type(parquet_filter), intent(in) :: filter !! filter whose rules are parsed, validated, and applied.
            integer(int32), intent(in) :: row_group_lo !! first row group to evaluate over (1-based), or 0
            !! for all of them, in which case row_group_hi is ignored.
            integer(int32), intent(in) :: row_group_hi !! last row group to evaluate over (inclusive).
        end subroutine parquet_reader_set_filter_scoped_int32
        !> Row-group-scoped form, int64 bounds -- see the generic interface above.
        module subroutine parquet_reader_set_filter_scoped_int64(reader, filter, row_group_lo, row_group_hi)
            type(parquet_reader), intent(inout) :: reader !! open, unfiltered reader with no column decoded yet.
            type(parquet_filter), intent(in) :: filter !! filter whose rules are parsed, validated, and applied.
            integer(int64), intent(in) :: row_group_lo !! first row group to evaluate over (1-based), or 0
            !! for all of them, in which case row_group_hi is ignored.
            integer(int64), intent(in) :: row_group_hi !! last row group to evaluate over (inclusive).
        end subroutine parquet_reader_set_filter_scoped_int64
        !> Row-BOUNDED form, int32 bounds -- see the generic interface above. Narrows the
        !> row-group-scoped form one step further: only physical rows row_lo..row_hi (1-based,
        !> inclusive) may match, so a range that starts or ends INSIDE a row group is expressed
        !> exactly rather than rounded out to whole row groups.
        module subroutine parquet_reader_set_filter_rows_int32(reader, filter, row_group_lo, row_group_hi, &
                row_lo, row_hi)
            type(parquet_reader), intent(inout) :: reader !! open, unfiltered reader with no column decoded yet.
            type(parquet_filter), intent(in) :: filter !! filter whose rules are parsed, validated, and applied;
            !! may hold no rules at all, in which case the row range alone decides which rows match.
            integer(int32), intent(in) :: row_group_lo !! first row group to evaluate over (1-based), or 0
            !! for all of them, in which case row_group_hi is ignored.
            integer(int32), intent(in) :: row_group_hi !! last row group to evaluate over (inclusive).
            integer(int32), intent(in) :: row_lo !! first physical row that may match (1-based).
            integer(int32), intent(in) :: row_hi !! last physical row that may match (inclusive).
        end subroutine parquet_reader_set_filter_rows_int32
        !> Row-BOUNDED form, int64 bounds -- see parquet_reader_set_filter_rows_int32.
        module subroutine parquet_reader_set_filter_rows_int64(reader, filter, row_group_lo, row_group_hi, &
                row_lo, row_hi)
            type(parquet_reader), intent(inout) :: reader !! open, unfiltered reader with no column decoded yet.
            type(parquet_filter), intent(in) :: filter !! filter whose rules are parsed, validated, and applied;
            !! may hold no rules at all, in which case the row range alone decides which rows match.
            integer(int64), intent(in) :: row_group_lo !! first row group to evaluate over (1-based), or 0
            !! for all of them, in which case row_group_hi is ignored.
            integer(int64), intent(in) :: row_group_hi !! last row group to evaluate over (inclusive).
            integer(int64), intent(in) :: row_lo !! first physical row that may match (1-based).
            integer(int64), intent(in) :: row_hi !! last physical row that may match (inclusive).
        end subroutine parquet_reader_set_filter_rows_int64
        !> Applies `sort_by` to an already-open `reader`, exactly as
        !> parquet_open_reader(..., sort_by=) would have: every column read
        !> from here on -- and every column already decoded -- comes back in
        !> key order. Provided for the same reasons parquet_reader_set_filter
        !> is: a caller that does not own the parquet_open_reader call, or one
        !> that can only choose its keys after inspecting the file's schema.
        !>
        !> Refuses, with error stop, in three states: when the reader already
        !> has a sort (add every key to one parquet_sortkey instead), when any
        !> column has already been decoded on this reader (data already handed
        !> back could not then be aligned with anything read afterwards), and
        !> when a chunked read has already been done (its rows were handed back
        !> in physical row-group order, which no permutation can reconcile).
        !> The last two are separate checks because a chunked read caches
        !> nothing, so the decoded-columns predicate cannot see it. A reader
        !> opened with filter= or sample_fraction= is fine: the sort orders the
        !> surviving rows -- that is the supported order, and the reverse
        !> (set_filter under an active sort) is refused by set_filter itself.
        module subroutine parquet_reader_set_sort(reader, sort_by)
            type(parquet_reader), intent(inout) :: reader !! open, unsorted reader with no column decoded yet.
            type(parquet_sortkey), intent(in) :: sort_by !! sort keys, parsed and validated here.
        end subroutine parquet_reader_set_sort
        !> Gives `reader` the read-time transform `source` has already worked out, instead of making
        !! it work the same thing out again from the same file.
        !!
        !! **What this is for.** The recommended way to read one file from several threads is to give
        !! each thread its own `parquet_reader` (see the Thread safety guide). When that file is read
        !! with a `filter=` or a `sort_by=`, every one of those readers would otherwise re-decode the
        !! filter's key columns and rebuild the whole sort permutation -- work that is identical in
        !! every reader and can cost more than the parallelism saves. This hands it over instead.
        !!
        !! **The cost is two atomic refcount increments**, not a data copy: a filter mask and a sort
        !! permutation are immutable Arrow arrays, so the readers share them. Everything else
        !! transferred is proportional to the file's row-group count.
        !!
        !! **`source` may be adopted from by several threads at once**, provided it is idle -- it is
        !! only read. What it must NOT be is in use by another thread at the same moment, which is
        !! refused rather than raced.
        !!
        !! Aborts unless: the two readers are open on files with the same row-group and row counts;
        !! `reader` has no filter, sample or sort of its own; and no column has been read on `reader`
        !! yet (one already read was read unmasked, and could not be lined up with an adopted mask).
        !! A `source` carrying no transform at all is a no-op, so a caller need not ask first.
        module subroutine parquet_reader_adopt_transform(reader, source)
            type(parquet_reader), intent(inout) :: reader !! open reader with no transform of its own yet.
            type(parquet_reader), intent(in) :: source !! open, idle reader whose transform is adopted.
        end subroutine parquet_reader_adopt_transform
        !> Reader, int32 specific of parquet_get_chunk_size -- see the generic interface above.
        module subroutine parquet_get_chunk_size_reader_int32(reader, chunk_size, row_group)
            type(parquet_reader), intent(in) :: reader !! open reader.
            integer(int32), intent(out) :: chunk_size !! row group's own physical row count.
            integer(int32), intent(in), optional :: row_group !! 1-based; omitted defaults to the first row group.
        end subroutine parquet_get_chunk_size_reader_int32
        !> Reader, int64 specific of parquet_get_chunk_size -- see the generic interface above.
        module subroutine parquet_get_chunk_size_reader_int64(reader, chunk_size, row_group)
            type(parquet_reader), intent(in) :: reader !! open reader.
            integer(int64), intent(out) :: chunk_size !! row group's own physical row count.
            integer(int64), intent(in), optional :: row_group !! 1-based; omitted defaults to the first row group.
        end subroutine parquet_get_chunk_size_reader_int64
        !> The nrows-less form of parquet_open_reader (see the generic
        !> interface above): opens as usual, filling in none of the
        !> post-filter row count. Called directly by the two nrows= forms
        !> below, which then call parquet_get_nrows themselves.
        !>
        !> prefetch (default .false.): when .true., every column in the file is
        !> read and cached right away, after any filter has been applied (see
        !> parquet_open_reader_base's implementation for why that ordering
        !> matters), instead of each column being read lazily on first request.
        !> Equivalent to calling parquet_prefetch_columns for every column in
        !> the file immediately after opening. Materializes the whole file in
        !> memory up front -- see MANUAL.md's "Performance and memory" section.
        module subroutine parquet_open_reader_base(reader, filename, use_threads, filter, sample_fraction, sample_seed, &
                schema, qc, qc_soft, prefetch, sort_by)
            type(parquet_reader), intent(out) :: reader !! reader to open.
            character(len=*), intent(in) :: filename !! .parquet file path.
            logical, intent(in), optional :: use_threads !! use Arrow's multi-threaded reader.
            type(parquet_filter), intent(in), optional :: filter !! row filter to apply.
            real(real64), intent(in), optional :: sample_fraction !! Bernoulli row-keep probability in [0.0, 1.0);
            !! omitted or >= 1.0 reads every row; must not be negative or NaN (error stops); exactly
            !! 0.0 yields zero rows deterministically.
            integer(int64), intent(in), optional :: sample_seed !! >0 for a reproducible sample draw;
            !! omitted or <= 0 settles a fresh seed. **`integer(int64)` only, deliberately**: one
            !! seed kind across the whole library, matching every `pf_random_*` seed. Do not add an
            !! `int32` specific -- a literal is written `42_int64`.
            type(parquet_schema), intent(in), optional :: schema !! schema/qc-maml to validate columns against.
            logical, intent(in), optional :: qc !! enable qc: min/max/miss enforcement; defaults to present(schema)
            !! (on whenever a schema is given), pass .false. to opt out; no-op without a schema.
            logical, intent(in), optional :: qc_soft !! qc violations warn instead of error-stopping.
            logical, intent(in), optional :: prefetch !! read and cache every column immediately.
            type(parquet_sortkey), intent(in), optional :: sort_by !! read-time sort keys; applied after any filter.
        end subroutine parquet_open_reader_base
        !> nrows (integer(int64)): filled in with the post-filter/post-sample row count
        !> via parquet_get_nrows(reader, nrows, check_positive=.true.) -- so,
        !> exactly like that check_positive path, a file (or filter/sample result)
        !> with zero rows fails immediately with error stop instead of
        !> silently returning nrows=0. Omit nrows entirely (dispatches to
        !> parquet_open_reader_base above) if you want to open a reader that
        !> may legitimately have zero matching rows -- call parquet_get_nrows
        !> yourself afterwards, without check_positive, to get 0 back instead
        !> of aborting.
        module subroutine parquet_open_reader_nrows_int64(reader, filename, use_threads, filter, sample_fraction, &
                sample_seed, schema, qc, qc_soft, nrows, prefetch, sort_by)
            type(parquet_reader), intent(out) :: reader !! reader to open.
            character(len=*), intent(in) :: filename !! .parquet file path.
            logical, intent(in), optional :: use_threads !! use Arrow's multi-threaded reader.
            type(parquet_filter), intent(in), optional :: filter !! row filter to apply.
            real(real64), intent(in), optional :: sample_fraction !! Bernoulli row-keep probability in [0.0, 1.0);
            !! omitted or >= 1.0 reads every row; must not be negative or NaN (error stops); exactly
            !! 0.0 yields zero rows deterministically.
            integer(int64), intent(in), optional :: sample_seed !! >0 for a reproducible sample draw;
            !! omitted or <= 0 settles a fresh seed. **`integer(int64)` only, deliberately**: one
            !! seed kind across the whole library, matching every `pf_random_*` seed. Do not add an
            !! `int32` specific -- a literal is written `42_int64`.
            type(parquet_schema), intent(in), optional :: schema !! schema/qc-maml to validate columns against.
            logical, intent(in), optional :: qc !! enable qc: min/max/miss enforcement; defaults to present(schema)
            !! (on whenever a schema is given), pass .false. to opt out; no-op without a schema.
            logical, intent(in), optional :: qc_soft !! qc violations warn instead of error-stopping.
            integer(int64), intent(out) :: nrows !! post-filter/post-sample row count; error stops if zero.
            logical, intent(in), optional :: prefetch !! read and cache every column immediately.
            type(parquet_sortkey), intent(in), optional :: sort_by !! read-time sort keys; applied after any filter.
        end subroutine parquet_open_reader_nrows_int64
        !> Same as parquet_open_reader_nrows_int64, but for a caller-supplied
        !> integer(int32) nrows -- also fails with error stop (via
        !> parquet_get_nrows_int32) if the actual row count overflows int32,
        !> exactly as a direct parquet_get_nrows(reader, nrows) call with an
        !> integer(int32) nrows would.
        module subroutine parquet_open_reader_nrows_int32(reader, filename, use_threads, filter, sample_fraction, &
                sample_seed, schema, qc, qc_soft, nrows, prefetch, sort_by)
            type(parquet_reader), intent(out) :: reader !! reader to open.
            character(len=*), intent(in) :: filename !! .parquet file path.
            logical, intent(in), optional :: use_threads !! use Arrow's multi-threaded reader.
            type(parquet_filter), intent(in), optional :: filter !! row filter to apply.
            real(real64), intent(in), optional :: sample_fraction !! Bernoulli row-keep probability in [0.0, 1.0);
            !! omitted or >= 1.0 reads every row; must not be negative or NaN (error stops); exactly
            !! 0.0 yields zero rows deterministically.
            integer(int64), intent(in), optional :: sample_seed !! >0 for a reproducible sample draw;
            !! omitted or <= 0 settles a fresh seed. **`integer(int64)` only, deliberately**: one
            !! seed kind across the whole library, matching every `pf_random_*` seed. Do not add an
            !! `int32` specific -- a literal is written `42_int64`.
            type(parquet_schema), intent(in), optional :: schema !! schema/qc-maml to validate columns against.
            logical, intent(in), optional :: qc !! enable qc: min/max/miss enforcement; defaults to present(schema)
            !! (on whenever a schema is given), pass .false. to opt out; no-op without a schema.
            logical, intent(in), optional :: qc_soft !! qc violations warn instead of error-stopping.
            integer(int32), intent(out) :: nrows !! post-filter/post-sample row count; error stops if zero or if it overflows int32.
            logical, intent(in), optional :: prefetch !! read and cache every column immediately.
            type(parquet_sortkey), intent(in), optional :: sort_by !! read-time sort keys; applied after any filter.
        end subroutine parquet_open_reader_nrows_int32
        !> Closes `reader`, freeing the underlying C++ handle. check_complete (optional,
        !> default .false.): verify every column read via parquet_read_column_chunk had every
        !> one of the file's row groups read by now (row-mode/whole-column reads are excluded --
        !> only chunk-read columns are tracked at all). check_hard (optional, default .true.
        !> when check_complete is .true.): an incomplete column error stops (naming the column
        !> and its missing row group(s)) when .true., or prints a WARNING and continues when
        !> .false. -- mirrors parquet_open_reader's qc/qc_soft pairing.
        module subroutine parquet_close_reader(reader, print_stat, check_complete, check_hard)
            type(parquet_reader), intent(inout) :: reader !! reader to close.
            logical, intent(in), optional :: print_stat !! print Arrow read-statistics to stdout on close.
            logical, intent(in), optional :: check_complete !! verify every chunk-read column's row groups were all read.
            logical, intent(in), optional :: check_hard !! error stop (.true., default) vs WARNING (.false.) on incompleteness.
        end subroutine parquet_close_reader
        !> Array specific of parquet_prefetch_columns: one name per element,
        !> fixed-length (pad shorter names with blanks).
        module subroutine parquet_prefetch_columns_array(reader, names)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: names(:) !! column names to prefetch.
        end subroutine parquet_prefetch_columns_array
        !> Scalar-string specific of parquet_prefetch_columns: `names` lists
        !> column names separated by commas and/or semicolons.
        module subroutine parquet_prefetch_columns_string(reader, names)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: names !! comma/semicolon-separated column names.
        end subroutine parquet_prefetch_columns_string
        !> int64 specific of parquet_get_nrows.
        module subroutine parquet_get_nrows_int64(reader, nrows, check_positive)
            type(parquet_reader), intent(in) :: reader !! open reader.
            integer(int64), intent(out) :: nrows !! post-filter row count.
            logical, intent(in), optional :: check_positive !! error stop instead of returning 0 rows.
        end subroutine parquet_get_nrows_int64
        !> int32 specific of parquet_get_nrows; also error stops if the
        !> actual row count overflows int32.
        module subroutine parquet_get_nrows_int32(reader, nrows, check_positive)
            type(parquet_reader), intent(in) :: reader !! open reader.
            integer(int32), intent(out) :: nrows !! post-filter row count.
            logical, intent(in), optional :: check_positive !! error stop instead of returning 0 rows.
        end subroutine parquet_get_nrows_int32
        !> int64 specific of parquet_get_num_row_groups.
        module subroutine parquet_get_num_row_groups_int64(reader, num_row_groups)
            type(parquet_reader), intent(in) :: reader !! open reader.
            integer(int64), intent(out) :: num_row_groups !! file's row-group count.
        end subroutine parquet_get_num_row_groups_int64
        !> int32 specific of parquet_get_num_row_groups; also error stops
        !> if the actual row-group count overflows int32.
        module subroutine parquet_get_num_row_groups_int32(reader, num_row_groups)
            type(parquet_reader), intent(in) :: reader !! open reader.
            integer(int32), intent(out) :: num_row_groups !! file's row-group count.
        end subroutine parquet_get_num_row_groups_int32
        !> Returns `name`'s declared col_size (vector-column element count;
        !> 1 for a scalar column) in `col_size`. Reads no column data for a scalar column or a
        !> FIXED_SIZE_LIST one (whose width is a schema-level constant), so this is safe even on a
        !> column whose total element count (nrows * col_size) itself exceeds int32. A plain
        !> LIST/LARGE_LIST column -- which this library never writes, but another producer may --
        !> has no schema-level width at all, so it is screened from the footer and then proven one
        !> row group at a time: that does read data, but never holds more than one row group.
        !> See parquet_get_column_total_elements, which answers via the same helper.
        module subroutine parquet_get_col_size(reader, name, col_size)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer, intent(out) :: col_size !! that column's declared element count.
        end subroutine parquet_get_col_size
        !> int32 specific of parquet_measure_list_width; see the generic's own doc-comment for
        !> what `row_group_lo`, `row_group_hi`, `proven` and `width` mean.
        module subroutine parquet_measure_list_width_int32(reader, name, row_group_lo, row_group_hi, proven, width)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group_lo !! first row group (1-based); <= 0 means all.
            integer(int32), intent(in) :: row_group_hi !! last row group (1-based, inclusive).
            logical, intent(in) :: proven !! .true.: prove it by reading; .false.: footer screen only.
            integer, intent(out) :: width !! uniform element count per row, or 1 if not uniform.
        end subroutine parquet_measure_list_width_int32
        !> int64 specific of parquet_measure_list_width; see the generic's own doc-comment.
        module subroutine parquet_measure_list_width_int64(reader, name, row_group_lo, row_group_hi, proven, width)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group_lo !! first row group (1-based); <= 0 means all.
            integer(int64), intent(in) :: row_group_hi !! last row group (1-based, inclusive).
            logical, intent(in) :: proven !! .true.: prove it by reading; .false.: footer screen only.
            integer, intent(out) :: width !! uniform element count per row, or 1 if not uniform.
        end subroutine parquet_measure_list_width_int64
        !> int32 specific of parquet_column_has_nulls; see the generic's own doc-comment.
        module function parquet_column_has_nulls_int32(reader, name, row_group_lo, row_group_hi) result(has_nulls)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group_lo !! first row group (1-based); <= 0 means all.
            integer(int32), intent(in) :: row_group_hi !! last row group (1-based, inclusive).
            logical :: has_nulls !! .true. if it has Nulls, or if the file cannot say.
        end function parquet_column_has_nulls_int32
        !> int64 specific of parquet_column_has_nulls; see the generic's own doc-comment.
        module function parquet_column_has_nulls_int64(reader, name, row_group_lo, row_group_hi) result(has_nulls)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group_lo !! first row group (1-based); <= 0 means all.
            integer(int64), intent(in) :: row_group_hi !! last row group (1-based, inclusive).
            logical :: has_nulls !! .true. if it has Nulls, or if the file cannot say.
        end function parquet_column_has_nulls_int64
        !> Whether `name`'s vector width can only be determined by reading its data.
        !>
        !> `.true.` for exactly one case: a plain Parquet `LIST`/`LARGE_LIST` column, whose rows may
        !> each hold a different number of elements, so no width exists in the schema to read.
        !> `.false.` for a scalar column (width 1 by construction) and for a `FIXED_SIZE_LIST` -- the
        !> layout this library always writes, and the one any Arrow-based writer preserves -- whose
        !> width is a schema constant. Answered from the schema; reads nothing.
        module function parquet_column_width_needs_data(reader, name) result(needs_data)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            logical :: needs_data !! .true. only for a plain LIST/LARGE_LIST column.
        end function parquet_column_width_needs_data
        !> int64 specific of parquet_get_column_total_elements.
        module subroutine parquet_get_column_total_elements_int64(reader, name, total_elements)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(out) :: total_elements !! total element count across every row.
        end subroutine parquet_get_column_total_elements_int64
        !> int32 specific of parquet_get_column_total_elements.
        module subroutine parquet_get_column_total_elements_int32(reader, name, total_elements)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(out) :: total_elements !! total element count across every row.
        end subroutine parquet_get_column_total_elements_int32
        !> Returns the longest actual string length among `name`'s values in
        !> `max_string_length`, so a caller can size a fixed-length
        !> character(len=...) buffer before reading a string column.
        module subroutine parquet_get_string_length(reader, name, max_string_length)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! string column name.
            integer, intent(out) :: max_string_length !! longest value length actually present in the column.
        end subroutine parquet_get_string_length
        !> Returns column `name`'s stored time unit and (for a timestamp) timezone. `unit`
        !> (optional) receives one of the parquet_unit_* selectors; `timezone` (optional,
        !> allocatable) receives the IANA timezone string ("" for a timezone-naive timestamp or
        !> a time column). Aborts if `name` is not a time/timestamp column. Distinct from
        !> parquet_get_metadata, which serves user-defined key/value metadata rather than this
        !> schema-level property.
        module subroutine parquet_get_column_time_info(reader, name, unit, timezone)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! time/timestamp column name.
            integer, intent(out), optional :: unit !! stored unit (a parquet_unit_* selector).
            character(len=:), allocatable, intent(out), optional :: timezone !! IANA tz, or "" if naive.
        end subroutine parquet_get_column_time_info
        !> Returns .true. if column `name` (a top-level or dotted struct-leaf path, same
        !> convention as every other column-name argument) exists in `reader`'s schema,
        !> optionally restricted to a set of allowed data types via `types`.
        !>
        !> **`types` asks "can I read this column as one of these?", not "is its physical type
        !> literally one of these?"** Each token is compared against the column's target -- the
        !> Fortran kind this library reads it into, which is the same narrowest-lossless mapping
        !> parquet_get_column_type reports (see its doc-comment below for the full table). So
        !> types="int32" matches an `int8` or `uint16` column, and types="int64" matches a `uint32`
        !> one, because those are the kinds those columns are read into.
        !>
        !> `types` is a comma-separated list of tokens: any of valid_query_data_types's nine single
        !> types ("int32"/"int64"/"float32"/"float64"/"string"/"boolean"/"date"/"time"/"timestamp"),
        !> and/or the group aliases "int" (any integer column), "float" (any column readable into a
        !> float -- which, since every numeric physical type converts to float64, means every
        !> numeric column, integers and decimals included), and "temporal" (date, time, or
        !> timestamp). Comparison is case-insensitive. Omit `types` to check existence regardless
        !> of type. error stops if `types` contains an unrecognized token (checked before the
        !> existence check, so a malformed filter is reported even for a column that doesn't
        !> exist).
        !>
        !> **An alias is therefore NOT the union of its member tokens, and that is deliberate.**
        !> types="float" matches an `int32` column (an integer is readable as a float) while
        !> types="float64" does not (that column's target kind is int32). The two ask different
        !> questions -- "can I read this as a float at all?" against "is float64 the right
        !> declaration?" -- and both are useful.
        !>
        !> A column this library cannot read at all (a MAP, say) has the target "unknown" and
        !> matches no token, but is still found by a plain (no `types`) existence check.
        module function parquet_column_exists(reader, name, types) result(exists)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name (dotted struct-leaf path allowed).
            character(len=*), intent(in), optional :: types !! comma-separated type tokens/group aliases.
            logical :: exists !! .true. if the column exists and (if types given) matches one of its tokens.
        end function parquet_column_exists
        !> Returns the Fortran type existing column `name` is READ INTO, in `type_name`: one of
        !> valid_query_data_types's nine tokens ("int32"/"int64"/"float32"/"float64"/"string"/
        !> "boolean"/"date"/"time"/"timestamp"), or "unknown" for a column this library cannot
        !> read at all. A vector (FIXED_SIZE_LIST) column reports its element type, e.g. an int32
        !> vector column reports "int32" (see parquet_get_col_size for its element count).
        !>
        !> The question it answers is "what do I declare?", so the answer is the **narrowest
        !> lossless** Fortran kind for the column's physical type, not the physical type's own
        !> name -- all four numeric targets accept the same 15 physical types, so which one a read
        !> actually uses is chosen by the caller's declaration rather than by the file:
        !>
        !>  - int8, int16, int32, uint8, uint16 -> "int32"
        !>  - int64, uint32 -> "int64"
        !>  - uint64 -> "int64", **lossy**: no Fortran kind covers its range, and a value above
        !>    huge(int64) aborts on read
        !>  - half_float, float -> "float32"; double -> "double" is "float64"
        !>  - decimal32/64/128/256 -> "float64", **lossy**, mapped on the type ID alone: no
        !>    precision/scale awareness, so decimal(9,0) answers "float64" like every other decimal
        !>  - bool -> "boolean"; string/large_string -> "string"
        !>  - date32/date64 -> "date"; time32/time64 -> "time"; timestamp -> "timestamp"
        !>  - anything else -> "unknown"
        !>
        !> The two lossy rows return the conventional target rather than "unknown" on purpose: a
        !> caller asking what to declare is better served by the kind the library will actually
        !> use than by being told a readable column is unreadable.
        !>
        !> error stops only if `name` doesn't exist -- that is a caller mistake, and
        !> parquet_column_exists is the query for it. An unreadable type is an answer ("unknown"),
        !> not an error.
        module subroutine parquet_get_column_type(reader, name, type_name)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! existing column name (dotted struct-leaf path allowed).
            character(len=:), allocatable, intent(out) :: type_name !! resolved canonical type token.
        end subroutine parquet_get_column_type
        !> Reports existing column `name`'s CONTAINER SHAPE in `shape`, as one of the tokens
        !> `"scalar"`, `"vector"`, `"list"`, `"map"`, `"struct"` or `"unknown"`. A schema-only
        !> query: it reads no column data at all.
        !>
        !> The question is "is this a container?", not "can I read this?", so a leaf whose ELEMENT
        !> type this library cannot read is still a `"scalar"` when it holds one value per row --
        !> that is the other query's business, and a decimal column is `("unknown", "scalar")`.
        !>
        !> An Arrow ENCODING WRAPPER -- an extension, dictionary or run-end-encoded type -- is
        !> read through, since none of them changes how many values a row holds: an
        !> `arrow.fixed_shape_tensor` over a four-wide fixed-size list answers `"vector"`, and
        !> parquet_get_col_size answers 4 for it.
        !>
        !> `"unknown"` is the answer for a shape that is neither, such as an Arrow union or a
        !> list-view. No Parquet file is currently known to produce one; it exists so an
        !> unrecognised shape is reported rather than called a `"scalar"`.
        !>
        !> Orthogonal to parquet_get_column_type, which reports the ELEMENT type and deliberately
        !> unwraps a list to it -- so a `list<double>` column answers `"float64"` there and
        !> `"list"` here, and a caller wanting a complete description asks both.
        !>
        !> `"vector"` means a fixed-size list, and only that. A variable-length `LIST` column
        !> answers `"list"` EVEN WHEN ITS ROWS HAPPEN TO BE UNIFORM and it would read perfectly
        !> well into a 2-D array, because whether they are uniform is a property of the data and
        !> answering it would mean reading the column. Ask parquet_get_col_size, which does look,
        !> if that is the question.
        !>
        !> error stops only if `name` doesn't exist, same as parquet_get_column_type: a shape this
        !> library cannot read is an answer, not an error.
        module subroutine parquet_get_column_shape(reader, name, shape)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! existing column name (dotted struct-leaf path allowed).
            character(len=:), allocatable, intent(out) :: shape !! resolved container-shape token.
        end subroutine parquet_get_column_shape
        !> Reports map column `name`'s VALUE type in `type_name`, as one of the same nine tokens
        !! parquet_get_column_type uses, or `"unknown"`. A schema-only query: it reads no column
        !! data at all.
        !!
        !! This is the one thing parquet_get_column_type cannot answer. That query deliberately
        !! reports `"unknown"` for a map, because a map cell is not one value and the question it
        !! answers is "what element type would I declare?". This one answers the narrower question
        !! a caller has once it already knows the column is a map: what kind will the values come
        !! back as?
        !!
        !! `"unknown"` means *this library cannot read the column*, and covers three cases that a
        !! caller does not need to tell apart: `name` is not a map at all, its keys are not strings
        !! (only string keys are supported), or its values are of a type outside the nine. Ask
        !! parquet_get_column_shape first if "is it a map?" is the question -- the pairing of the
        !! two is deliberate, and neither is derivable from the other.
        !!
        !! error stops only if `name` doesn't exist, same as parquet_get_column_type and
        !! parquet_get_column_shape: an unreadable column is an answer, not an error.
        module subroutine parquet_get_map_value_type(reader, name, type_name)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! existing column name (dotted struct-leaf path allowed).
            character(len=:), allocatable, intent(out) :: type_name !! resolved value-type token, or "unknown".
        end subroutine parquet_get_map_value_type
        !> Reports whether existing column `name` is declared NULLABLE in `reader`'s file schema,
        !> in `is_nullable`. A schema-only query -- it reads no column data, and says nothing about
        !> whether the column actually contains any Null (parquet_column_has_nulls answers that,
        !> from the footer's own null counts).
        !>
        !> The two are genuinely different questions. A nullable column may hold no Null at all,
        !> which is the normal state of a column written through the streaming API with an
        !> all-.true. is_valid mask; a non-nullable one cannot hold a Null even in principle,
        !> because the file's own schema forbids it.
        !>
        !> For a VECTOR column this reports the ELEMENT (child) field's flag, not the outer
        !> list field's -- the outer one is non-nullable by construction here, since a row's
        !> vector is never itself missing, so reporting it would answer a constant. A dotted
        !> struct path reports the leaf's own flag, the same rule parquet_get_column_type follows.
        !>
        !> error stops only if `name` doesn't exist. A column this library cannot READ still has
        !> a perfectly meaningful nullability flag, so an unreadable type is an answer here too,
        !> exactly as it is for parquet_get_column_type.
        module subroutine parquet_get_column_nullable(reader, name, is_nullable)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! existing column name (dotted struct-leaf path allowed).
            logical, intent(out) :: is_nullable !! .true. if the stored field is declared nullable.
        end subroutine parquet_get_column_nullable
        !> Returns every column `reader`'s file contains, in schema order, as the same
        !> (possibly dotted) names every other column-name argument in this module accepts:
        !> a nested STRUCT field contributes one entry per leaf beneath it ("addr.city"), never
        !> its own bare name, and every other field contributes one entry under its own name.
        !> `names` comes back allocated to exactly the column count, each element trimmed to the
        !> longest name present (blank-padded), so `trim(names(i))` is the name to pass on.
        !> A zero-column file yields a zero-size `names`.
        !>
        !> LIST/MAP columns (and any leaf beneath a struct that is itself a LIST/MAP) ARE listed,
        !> even though no read entry point supports them: the purpose here is to report what the
        !> file actually holds. Pass a listed name to parquet_column_exists (no `types`) or
        !> parquet_get_column_type to find out whether it can be read.
        module subroutine parquet_get_column_names(reader, names)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=:), allocatable, intent(out) :: names(:) !! one entry per column, schema order.
        end subroutine parquet_get_column_names
        !> The 1-based PHYSICAL file row index of each row this reader currently returns, in the
        !> order it returns them.
        !>
        !> Without a `filter=`/`sample_fraction=`/`sort_by=` this is simply `1, 2, 3, ...`, and
        !> with one it is the only way to find out which file rows survived and in what order --
        !> that lives in the reader's own mask and permutation and is not otherwise visible.
        !> `rows` comes back with one entry per row the reader reports (`parquet_get_nrows`).
        !>
        !> This is what `parquet_table`'s automatic `parquet_row_index` column is built on.
        module subroutine parquet_get_physical_row_indices(reader, rows)
            type(parquet_reader), intent(in) :: reader !! open reader.
            integer(int64), allocatable, intent(out) :: rows(:) !! one physical row index per returned row.
        end subroutine parquet_get_physical_row_indices
        !> Every key/value metadata entry the file carries, in the order it is stored.
        !>
        !> parquet_get_metadata answers for a key you already know; this reports what is there at
        !> all, which is what a caller copying metadata from one file to another needs (it is how
        !> parquet_table snapshots a file's metadata at open, so that %get_file_metadata keeps
        !> working after the table has detached from its file, and how parquet_write_table's
        !> copy_metadata= carries it to an output file).
        !>
        !> `keys` and `values` are index-aligned and each is allocated to its own longest entry,
        !> blank-padded, so `trim(keys(i))` is the key to pass on -- the same convention
        !> parquet_get_column_names uses. Both come back zero-size for a file with no metadata.
        !> Reads nothing: the answer comes from the copy parquet_open_reader already made of the
        !> file's footer metadata.
        module subroutine parquet_get_metadata_items(reader, keys, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=:), allocatable, intent(out) :: keys(:) !! one entry per metadata item, stored order.
            character(len=:), allocatable, intent(out) :: values(:) !! that item's value, same order.
        end subroutine parquet_get_metadata_items
        !> Frees column `name`'s decoded Arrow buffers inside `reader`, after the caller has
        !> copied the values it wanted into its own Fortran storage. Purely a memory/time trade:
        !> a later read of the same column transparently re-reads and re-decodes it, so this can
        !> never change a result. Composes with an active filter=/sample_fraction= (both are
        !> re-applied to every freshly decoded column).
        !>
        !> `name` may be a dotted struct-leaf path, but the reader caches a struct as ONE array,
        !> so releasing any leaf frees the whole struct -- when walking several leaves of one
        !> struct, release only after the last of them, or each will re-read the struct.
        !> A name that doesn't exist, or a column that was never read, is a silent no-op.
        module subroutine parquet_release_column(reader, name)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column to free (dotted struct-leaf path allowed).
        end subroutine parquet_release_column
    end interface

    ! ---- Sort key parsing (parquet_read_sort) ----
    interface
        !> Parses one parquet_sortkey%add key ("ra asc", "-dec", "main.inner.age") into its
        !> column name and direction. Purely syntactic: reports a parse failure via ok/errmsg --
        !> never aborts, so the caller can attach the reader's file context to the message -- and
        !> leaves every schema-dependent check (column exists, type is orderable) to the C++ side.
        !>
        !> Declared here rather than in parquet_read.f90 (and public) because parquet_tables
        !> parses the same grammar for its own string key lists -- t%sort_by("ra,-dec"). One
        !> parser, so the read-time and in-memory spellings of a direction cannot drift apart.
        module subroutine parquet_parse_sort_key(key, name, descending, ok, errmsg)
            character(len=*), intent(in) :: key !! raw key text from one %add call.
            character(len=:), allocatable, intent(out) :: name !! column name (possibly a dotted struct path).
            logical, intent(out) :: descending !! .true. for a descending key.
            logical, intent(out) :: ok !! .true. if the key parsed.
            character(len=:), allocatable, intent(out) :: errmsg !! parse-failure message; "" when ok.
        end subroutine parquet_parse_sort_key
    end interface

    ! ---- Filter/sort column-name remapping (parquet_read_filter / parquet_read_sort) ----
    interface
        !> Rewrites every column reference in this filter's rules, replacing name `from(k)` with
        !> `to(k)`. Backs parquet_filter%remap_column_names -- see that binding, and the type's own
        !> doc comment, for what it is for.
        module subroutine parquet_filter_remap_column_names(this, from, to)
            class(parquet_filter), intent(inout) :: this !! filter whose rules are rewritten in place.
            character(len=*), intent(in) :: from(:) !! names to replace; same size as `to`.
            character(len=*), intent(in) :: to(:) !! replacement for each entry of `from`.
        end subroutine parquet_filter_remap_column_names
        !> Rewrites every sort key's column, replacing name `from(k)` with `to(k)`. Backs
        !> parquet_sortkey%remap_column_names -- see that binding for what it is for. Each key's
        !> direction and its nulls_first setting are carried across unchanged.
        module subroutine parquet_sortkey_remap_column_names(this, from, to)
            class(parquet_sortkey), intent(inout) :: this !! sort spec whose keys are rewritten in place.
            character(len=*), intent(in) :: from(:) !! names to replace; same size as `to`.
            character(len=*), intent(in) :: to(:) !! replacement for each entry of `from`.
        end subroutine parquet_sortkey_remap_column_names
        !> Rewrites the column each qc entry declares, replacing name `from(k)` with `to(k)`. Backs
        !> parquet_read_qc%remap_column_names -- see that binding for what it is for. Only the
        !> entry's first (column-name) field is touched; its bounds are carried across verbatim.
        module subroutine parquet_read_qc_remap_column_names(this, from, to)
            class(parquet_read_qc), intent(inout) :: this !! qc declarations rewritten in place.
            character(len=*), intent(in) :: from(:) !! names to replace; same size as `to`.
            character(len=*), intent(in) :: to(:) !! replacement for each entry of `from`.
        end subroutine parquet_read_qc_remap_column_names
        !> Merges a MAML-declared and a code-declared read-time QC into the single schema
        !> parquet_open_reader(..., schema=) takes, applying the per-column override rule: for any
        !> column whose MAML `fields:` entry carries a `qc:` key AT ALL -- even an empty one, which
        !> already means "no Nulls here" -- the MAML's declaration wins in full and the code's entry
        !> for that column is dropped entirely, rather than merged bound by bound. A column the MAML
        !> says nothing about (or names without a `qc:` key) takes the code's entry instead.
        !>
        !> `composed` carries ONLY qc-bearing field entries: a MAML entry with no `qc:` key is not
        !> copied across, because nothing but qc is read from this schema (parquet_open_reader
        !> passes it to parquet_apply_qc and nowhere else). That is also what keeps the two sources
        !> from colliding over a column the MAML merely names.
        !>
        !> `ncolumns` is how many columns `composed` ends up declaring qc for. Pass `composed` to
        !> parquet_open_reader only when it is nonzero -- a schema with no rules still switches qc
        !> on C++-side for no benefit.
        module subroutine parquet_compose_read_qc(schema, qc, composed, ncolumns)
            type(parquet_schema), intent(in), optional :: schema !! the MAML's own schema, if there is one.
            type(parquet_read_qc), intent(in), optional :: qc !! code-declared qc, already in FILE column names.
            type(parquet_schema), intent(out) :: composed !! the merged qc schema.
            integer, intent(out) :: ncolumns !! columns `composed` declares qc for; 0 means "pass no schema".
        end subroutine parquet_compose_read_qc
    end interface

    ! ---- Read column specifics (by type x access mode) ----
    interface
        !> Scalar int32 specific of parquet_read_column; see the generic
        !> interface above for the shared behavior of this whole family.
        module subroutine parquet_read_int32_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(out) :: values(:) !! one value per row.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int32_column_1d
        !> Scalar int64 specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_int64_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(out) :: values(:) !! one value per row.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int64_column_1d
        !> Scalar float32 specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_float32_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            real(real32), intent(out) :: values(:) !! one value per row.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float32_column_1d
        !> Scalar float64 specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_float64_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            real(real64), intent(out) :: values(:) !! one value per row.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float64_column_1d
        !> Scalar logical (boolean) specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_logical_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            logical, intent(out) :: values(:) !! one value per row.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_logical_column_1d
        !> Scalar string specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_string_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(out) :: values(:) !! one value per row.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_string_column_1d
        !> Vector int32 specific of parquet_read_column: reads the whole
        !> 2-D (element, row) array at once; see parquet_read_column above.
        module subroutine parquet_read_int32_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(out) :: values(:, :) !! (element, row) values.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_int32_array_full
        !> Vector int64 specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_int64_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(out) :: values(:, :) !! (element, row) values.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_int64_array_full
        !> Vector float32 specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_float32_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real32), intent(out) :: values(:, :) !! (element, row) values.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_float32_array_full
        !> Vector float64 specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_float64_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real64), intent(out) :: values(:, :) !! (element, row) values.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_float64_array_full
        !> Vector logical (boolean) specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_logical_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            logical, intent(out) :: values(:, :) !! (element, row) values.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_logical_array_full
        !> Vector string specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_string_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            character(len=*), intent(out) :: values(:, :) !! (element, row) values.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_string_array_full
        !> Compact (parquet_string_column) specific of parquet_read_column; see the generic
        !> interface's own doc comment above for the parquet_string_column notes.
        module subroutine parquet_read_string_column_compact(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_string_column), intent(inout) :: values !! cleared, then filled with the whole column.
        end subroutine parquet_read_string_column_compact
        !> Variable-length list specific of parquet_read_column: reads a whole `LIST`/`LARGE_LIST`
        !> column into a `parquet_list_column` (see module parquet_list), rows of differing
        !> lengths and all. `values` is cleared and rebuilt, so the caller declares nothing about
        !> the column beforehand -- not even its payload type, which comes from the file.
        !>
        !> Both null levels survive: a Null list row reads back as `values%is_null(i)`, a Null
        !> ELEMENT inside a present row as a null element of the payload, and a present-but-empty
        !> row as `%length(i) == 0` with `%is_null(i)` .false. There is no null_value/is_valid
        !> argument, for the same reason parquet_read_string_column_compact has none -- the
        !> destination carries its own validity.
        !>
        !> A FIXED_SIZE_LIST (vector) column is accepted too, and yields rows that all happen to
        !> have the same length. The reverse also holds: a `LIST` column whose data is uniform
        !> still reads into a 2-D array through the vector specifics. The caller's chosen output
        !> type picks the interpretation, and there is no third rule about when each is allowed --
        !> only a 2-D read of genuinely ragged data is refused, because no 2-D array can hold it.
        !>
        !> Filtering, sampling and sorting compose with this read exactly as with any other.
        module subroutine parquet_read_list_column(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! list column name (dotted struct-leaf path allowed).
            type(parquet_list_column), intent(inout) :: values !! cleared, then filled with the whole column.
        end subroutine parquet_read_list_column
        !> Struct (parquet_struct_column) specific of parquet_read_column.
        !>
        !> Reads a `STRUCT` column as ONE object: `values` comes back with the file's declared
        !> field set, each field's values and its own nullness, and the struct's own per-row
        !> nullness -- which is a separate question from any field's, and the reason a row whose
        !> every field is null is not a null row. A field whose own type is a nested struct, list
        !> or map is refused, naming the field.
        !>
        !> Complementary to, and not a replacement for, addressing a struct's leaves by their
        !> dotted paths (`parquet_read_column(reader, "person.age", ...)`), which is unchanged and
        !> reads any depth of nesting as flat columns.
        module subroutine parquet_read_struct_column(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! struct column name.
            type(parquet_struct_column), intent(inout) :: values !! cleared, then filled with the whole column.
        end subroutine parquet_read_struct_column
        !> Map (parquet_map_column) specific of parquet_read_column.
        !>
        !> Reads a `MAP` column as ONE object: `values` comes back with every row's `key -> value`
        !> entries in stored order, duplicate keys included, plus the two null levels a map has --
        !> the row (this map is absent, which is different from a present but empty one) and each
        !> value. A KEY is never null: Arrow's MapType declares its key field non-nullable.
        !>
        !> V1 keys must be strings. A file whose map is keyed by anything else is refused, naming
        !> the actual key type, rather than having its keys rendered as text -- that would silently
        !> change the data, since `1`, `01` and `1.0` are different keys. A value whose own type is
        !> a nested list, map or struct is refused for the same reason a list's is.
        !>
        !> Filtering, sampling and sorting compose with this read exactly as with any other.
        module subroutine parquet_read_map_column(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! map column name.
            type(parquet_map_column), intent(inout) :: values !! cleared, then filled with the whole column.
        end subroutine parquet_read_map_column
        !> Scalar date specific of parquet_read_column. A Parquet Null in the column becomes a
        !> null `values` element (parquet_date%is_null); there is no null_value/is_valid argument
        !> -- validity lives in the elements themselves, so a null-containing date column reads
        !> without the error-on-Null that the numeric/string readers apply by default.
        module subroutine parquet_read_date_column_1d(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_date), intent(out) :: values(:) !! one date (null where the column had a Null) per row.
        end subroutine parquet_read_date_column_1d
        !> Vector date specific of parquet_read_column; reads the whole (element, row) array.
        module subroutine parquet_read_date_array_full(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_date), intent(out) :: values(:,:) !! (element, row) dates.
        end subroutine parquet_read_date_array_full
        !> Scalar time specific of parquet_read_column; see parquet_read_date_column_1d on nulls.
        module subroutine parquet_read_time_column_1d(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_time), intent(out) :: values(:) !! one time per row.
        end subroutine parquet_read_time_column_1d
        !> Vector time specific of parquet_read_column.
        module subroutine parquet_read_time_array_full(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_time), intent(out) :: values(:,:) !! (element, row) times.
        end subroutine parquet_read_time_array_full
        !> Scalar timestamp specific of parquet_read_column; see parquet_read_date_column_1d on nulls.
        module subroutine parquet_read_timestamp_column_1d(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            type(parquet_timestamp), intent(out) :: values(:) !! one instant per row.
        end subroutine parquet_read_timestamp_column_1d
        !> Vector timestamp specific of parquet_read_column.
        module subroutine parquet_read_timestamp_array_full(reader, name, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_timestamp), intent(out) :: values(:,:) !! (element, row) instants.
        end subroutine parquet_read_timestamp_array_full
        !> Scalar date, int32 row_group specific of parquet_read_column_chunk; a paired _rg64
        !> (int64 row_group) also exists. Nulls fill their elements (parquet_date%set_null).
        module subroutine parquet_read_date_column_chunk_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group.
            type(parquet_date), intent(out) :: values(:) !! that row group's dates.
        end subroutine parquet_read_date_column_chunk_rg32
        !> Scalar date, int64 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_date_column_chunk_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group.
            type(parquet_date), intent(out) :: values(:) !! that row group's dates.
        end subroutine parquet_read_date_column_chunk_rg64
        !> Vector date, int32 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_date_array_column_chunk_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(in) :: row_group !! 1-based row group.
            type(parquet_date), intent(out) :: values(:,:) !! (element, row) dates for that row group.
        end subroutine parquet_read_date_array_column_chunk_rg32
        !> Vector date, int64 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_date_array_column_chunk_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(in) :: row_group !! 1-based row group.
            type(parquet_date), intent(out) :: values(:,:) !! (element, row) dates for that row group.
        end subroutine parquet_read_date_array_column_chunk_rg64
        !> Scalar time, int32 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_time_column_chunk_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group.
            type(parquet_time), intent(out) :: values(:) !! that row group's times.
        end subroutine parquet_read_time_column_chunk_rg32
        !> Scalar time, int64 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_time_column_chunk_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group.
            type(parquet_time), intent(out) :: values(:) !! that row group's times.
        end subroutine parquet_read_time_column_chunk_rg64
        !> Vector time, int32 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_time_array_column_chunk_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(in) :: row_group !! 1-based row group.
            type(parquet_time), intent(out) :: values(:,:) !! (element, row) times for that row group.
        end subroutine parquet_read_time_array_column_chunk_rg32
        !> Vector time, int64 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_time_array_column_chunk_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(in) :: row_group !! 1-based row group.
            type(parquet_time), intent(out) :: values(:,:) !! (element, row) times for that row group.
        end subroutine parquet_read_time_array_column_chunk_rg64
        !> Scalar timestamp, int32 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_timestamp_column_chunk_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group.
            type(parquet_timestamp), intent(out) :: values(:) !! that row group's instants.
        end subroutine parquet_read_timestamp_column_chunk_rg32
        !> Scalar timestamp, int64 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_timestamp_column_chunk_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group.
            type(parquet_timestamp), intent(out) :: values(:) !! that row group's instants.
        end subroutine parquet_read_timestamp_column_chunk_rg64
        !> Vector timestamp, int32 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_timestamp_array_column_chunk_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(in) :: row_group !! 1-based row group.
            type(parquet_timestamp), intent(out) :: values(:,:) !! (element, row) instants for that row group.
        end subroutine parquet_read_timestamp_array_column_chunk_rg32
        !> Vector timestamp, int64 row_group specific of parquet_read_column_chunk.
        module subroutine parquet_read_timestamp_array_column_chunk_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(in) :: row_group !! 1-based row group.
            type(parquet_timestamp), intent(out) :: values(:,:) !! (element, row) instants for that row group.
        end subroutine parquet_read_timestamp_array_column_chunk_rg64
        !> Date row-mode read (one row's element vector), int32 row_index specific; a paired
        !> _row_index_int64 also exists. Nulls fill their elements (no is_valid argument).
        module subroutine parquet_read_date_array_row_mode(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_date), intent(out) :: values(:) !! that row's element vector.
            integer(int32), intent(in) :: row_index !! 1-based row to read.
        end subroutine parquet_read_date_array_row_mode
        !> Date row-mode read, int64 row_index specific.
        module subroutine parquet_read_date_array_row_mode_row_index_int64(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_date), intent(out) :: values(:) !! that row's element vector.
            integer(int64), intent(in) :: row_index !! 1-based row to read.
        end subroutine parquet_read_date_array_row_mode_row_index_int64
        !> Time row-mode read, int32 row_index specific.
        module subroutine parquet_read_time_array_row_mode(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_time), intent(out) :: values(:) !! that row's element vector.
            integer(int32), intent(in) :: row_index !! 1-based row to read.
        end subroutine parquet_read_time_array_row_mode
        !> Time row-mode read, int64 row_index specific.
        module subroutine parquet_read_time_array_row_mode_row_index_int64(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_time), intent(out) :: values(:) !! that row's element vector.
            integer(int64), intent(in) :: row_index !! 1-based row to read.
        end subroutine parquet_read_time_array_row_mode_row_index_int64
        !> Timestamp row-mode read, int32 row_index specific.
        module subroutine parquet_read_timestamp_array_row_mode(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_timestamp), intent(out) :: values(:) !! that row's element vector.
            integer(int32), intent(in) :: row_index !! 1-based row to read.
        end subroutine parquet_read_timestamp_array_row_mode
        !> Timestamp row-mode read, int64 row_index specific.
        module subroutine parquet_read_timestamp_array_row_mode_row_index_int64(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_timestamp), intent(out) :: values(:) !! that row's element vector.
            integer(int64), intent(in) :: row_index !! 1-based row to read.
        end subroutine parquet_read_timestamp_array_row_mode_row_index_int64
        !> Date element-mode read: element position `elem_index` from every row. Nulls fill their
        !> elements (no is_valid argument).
        module subroutine parquet_read_date_array_element_mode(reader, name, values, elem_index)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_date), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
        end subroutine parquet_read_date_array_element_mode
        !> Time element-mode read.
        module subroutine parquet_read_time_array_element_mode(reader, name, values, elem_index)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_time), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
        end subroutine parquet_read_time_array_element_mode
        !> Timestamp element-mode read.
        module subroutine parquet_read_timestamp_array_element_mode(reader, name, values, elem_index)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            type(parquet_timestamp), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
        end subroutine parquet_read_timestamp_array_element_mode
        !> Scalar int32, int32 row_group specific of parquet_read_column_chunk; see the generic
        !> interface above. A paired _rg64 specific (same value type, int64 row_group) also
        !> exists for a file with more than 2,147,483,647 row groups.
        module subroutine parquet_read_int32_column_chunk_rg32(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            integer(int32), intent(out) :: values(:) !! one value per row of the selected row group.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int32_column_chunk_rg32
        !> Scalar int32, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_int32_column_chunk_rg64(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            integer(int32), intent(out) :: values(:) !! one value per row of the selected row group.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int32_column_chunk_rg64
        !> Vector int32, int32 row_group specific of parquet_read_column_chunk; see the generic
        !> interface above.
        module subroutine parquet_read_int32_array_column_chunk_rg32(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            integer(int32), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_int32_array_column_chunk_rg32
        !> Vector int32, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_int32_array_column_chunk_rg64(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            integer(int32), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_int32_array_column_chunk_rg64
        !> Scalar int64, int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_int64_column_chunk_rg32(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            integer(int64), intent(out) :: values(:) !! one value per row of the selected row group.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int64_column_chunk_rg32
        !> Scalar int64, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_int64_column_chunk_rg64(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            integer(int64), intent(out) :: values(:) !! one value per row of the selected row group.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int64_column_chunk_rg64
        !> Vector int64, int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_int64_array_column_chunk_rg32(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            integer(int64), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_int64_array_column_chunk_rg32
        !> Vector int64, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_int64_array_column_chunk_rg64(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            integer(int64), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_int64_array_column_chunk_rg64
        !> Scalar float32, int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_float32_column_chunk_rg32(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            real(real32), intent(out) :: values(:) !! one value per row of the selected row group.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float32_column_chunk_rg32
        !> Scalar float32, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_float32_column_chunk_rg64(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            real(real32), intent(out) :: values(:) !! one value per row of the selected row group.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float32_column_chunk_rg64
        !> Vector float32, int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_float32_array_column_chunk_rg32(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            real(real32), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_float32_array_column_chunk_rg32
        !> Vector float32, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_float32_array_column_chunk_rg64(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            real(real32), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_float32_array_column_chunk_rg64
        !> Scalar float64, int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_float64_column_chunk_rg32(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            real(real64), intent(out) :: values(:) !! one value per row of the selected row group.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float64_column_chunk_rg32
        !> Scalar float64, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_float64_column_chunk_rg64(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            real(real64), intent(out) :: values(:) !! one value per row of the selected row group.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float64_column_chunk_rg64
        !> Vector float64, int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_float64_array_column_chunk_rg32(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            real(real64), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_float64_array_column_chunk_rg32
        !> Vector float64, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_float64_array_column_chunk_rg64(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            real(real64), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_float64_array_column_chunk_rg64
        !> Scalar logical (boolean), int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_logical_column_chunk_rg32(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            logical, intent(out) :: values(:) !! one value per row of the selected row group.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_logical_column_chunk_rg32
        !> Scalar logical (boolean), int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_logical_column_chunk_rg64(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            logical, intent(out) :: values(:) !! one value per row of the selected row group.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_logical_column_chunk_rg64
        !> Vector logical (boolean), int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_logical_array_column_chunk_rg32(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            logical, intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_logical_array_column_chunk_rg32
        !> Vector logical (boolean), int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_logical_array_column_chunk_rg64(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            logical, intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_logical_array_column_chunk_rg64
        !> Scalar string, int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_string_column_chunk_rg32(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            character(len=*), intent(out) :: values(:) !! one value per row of the selected row group.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_string_column_chunk_rg32
        !> Scalar string, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_column_chunk_rg32.
        module subroutine parquet_read_string_column_chunk_rg64(reader, name, row_group, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            character(len=*), intent(out) :: values(:) !! one value per row of the selected row group.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_string_column_chunk_rg64
        !> Vector string, int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_string_array_column_chunk_rg32(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            character(len=*), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_string_array_column_chunk_rg32
        !> Vector string, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_int32_array_column_chunk_rg32.
        module subroutine parquet_read_string_array_column_chunk_rg64(reader, name, row_group, values, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            character(len=*), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_string_array_column_chunk_rg64
        !> Compact (parquet_string_column), int32 row_group specific of parquet_read_column_chunk;
        !> see the generic interface's own doc comment above for the parquet_string_column notes.
        module subroutine parquet_read_string_column_chunk_compact_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            type(parquet_string_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        end subroutine parquet_read_string_column_chunk_compact_rg32
        !> Compact (parquet_string_column), int64 row_group specific of parquet_read_column_chunk;
        !> see parquet_read_string_column_chunk_compact_rg32.
        module subroutine parquet_read_string_column_chunk_compact_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            type(parquet_string_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        end subroutine parquet_read_string_column_chunk_compact_rg64
        !> Variable-length list, int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_list_column for what a list read yields. Reads exactly row group
        !> `row_group`'s rows, and -- like every other row-group-scoped operation -- refuses while
        !> a sort is installed, since a permutation destroys row-group locality.
        module subroutine parquet_read_list_column_chunk_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! list column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            type(parquet_list_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        end subroutine parquet_read_list_column_chunk_rg32
        !> Struct (parquet_struct_column), int32 row_group specific of parquet_read_column_chunk;
        !> see parquet_read_struct_column for what a struct read yields. Reads exactly row group
        !> `row_group`.
        module subroutine parquet_read_struct_column_chunk_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! struct column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            type(parquet_struct_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        end subroutine parquet_read_struct_column_chunk_rg32
        !> Struct (parquet_struct_column), int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_struct_column_chunk_rg32.
        module subroutine parquet_read_struct_column_chunk_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! struct column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            type(parquet_struct_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        end subroutine parquet_read_struct_column_chunk_rg64
        !> Map (parquet_map_column), int32 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_map_column for what a map read yields. Reads exactly row group
        !> `row_group`'s rows, and -- like every other row-group-scoped operation -- refuses while
        !> a sort is installed, since a permutation destroys row-group locality.
        module subroutine parquet_read_map_column_chunk_rg32(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! map column name.
            integer(int32), intent(in) :: row_group !! 1-based row group to read.
            type(parquet_map_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        end subroutine parquet_read_map_column_chunk_rg32
        !> Map (parquet_map_column), int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_map_column_chunk_rg32.
        module subroutine parquet_read_map_column_chunk_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! map column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            type(parquet_map_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        end subroutine parquet_read_map_column_chunk_rg64
        !> Variable-length list, int64 row_group specific of parquet_read_column_chunk; see
        !> parquet_read_list_column_chunk_rg32.
        module subroutine parquet_read_list_column_chunk_rg64(reader, name, row_group, values)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! list column name.
            integer(int64), intent(in) :: row_group !! 1-based row group to read.
            type(parquet_list_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        end subroutine parquet_read_list_column_chunk_rg64
        !> int32 value / int32 row_index specific of parquet_read_array_row_mode; see the generic
        !> interface above for the shared "one row of a vector column" behavior. A paired
        !> _row_index_int64 specific (same value type, int64 row_index) also exists for files with
        !> more than 2,147,483,647 rows -- see parquet_read_int32_array_row_mode_row_index_int64.
        module subroutine parquet_read_int32_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(out) :: values(:) !! that row's element vector.
            integer(int32), intent(in) :: row_index !! 1-based row to read.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_int32_array_row_mode
        !> int32 value / int64 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode. Only needed to address a row beyond
        !> huge(1_int32) (2,147,483,647) in a file that large.
        module subroutine parquet_read_int32_array_row_mode_row_index_int64(reader, name, values, row_index, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(out) :: values(:) !! that row's element vector.
            integer(int64), intent(in) :: row_index !! 1-based row to read.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_int32_array_row_mode_row_index_int64
        !> int64 value / int32 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode.
        module subroutine parquet_read_int64_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(out) :: values(:) !! that row's element vector.
            integer(int32), intent(in) :: row_index !! 1-based row to read.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_int64_array_row_mode
        !> int64 value / int64 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode_row_index_int64.
        module subroutine parquet_read_int64_array_row_mode_row_index_int64(reader, name, values, row_index, null_value, &
                is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(out) :: values(:) !! that row's element vector.
            integer(int64), intent(in) :: row_index !! 1-based row to read.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_int64_array_row_mode_row_index_int64
        !> float32 value / int32 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode.
        module subroutine parquet_read_float32_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real32), intent(out) :: values(:) !! that row's element vector.
            integer(int32), intent(in) :: row_index !! 1-based row to read.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_float32_array_row_mode
        !> float32 value / int64 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode_row_index_int64.
        module subroutine parquet_read_float32_array_row_mode_row_index_int64(reader, name, values, row_index, &
                null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real32), intent(out) :: values(:) !! that row's element vector.
            integer(int64), intent(in) :: row_index !! 1-based row to read.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_float32_array_row_mode_row_index_int64
        !> float64 value / int32 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode.
        module subroutine parquet_read_float64_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real64), intent(out) :: values(:) !! that row's element vector.
            integer(int32), intent(in) :: row_index !! 1-based row to read.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_float64_array_row_mode
        !> float64 value / int64 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode_row_index_int64.
        module subroutine parquet_read_float64_array_row_mode_row_index_int64(reader, name, values, row_index, &
                null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real64), intent(out) :: values(:) !! that row's element vector.
            integer(int64), intent(in) :: row_index !! 1-based row to read.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_float64_array_row_mode_row_index_int64
        !> logical (boolean) value / int32 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode.
        module subroutine parquet_read_logical_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            logical, intent(out) :: values(:) !! that row's element vector.
            integer(int32), intent(in) :: row_index !! 1-based row to read.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_logical_array_row_mode
        !> logical (boolean) value / int64 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode_row_index_int64.
        module subroutine parquet_read_logical_array_row_mode_row_index_int64(reader, name, values, row_index, &
                null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            logical, intent(out) :: values(:) !! that row's element vector.
            integer(int64), intent(in) :: row_index !! 1-based row to read.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_logical_array_row_mode_row_index_int64
        !> string value / int32 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode.
        module subroutine parquet_read_string_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            character(len=*), intent(out) :: values(:) !! that row's element vector.
            integer(int32), intent(in) :: row_index !! 1-based row to read.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_string_array_row_mode
        !> string value / int64 row_index specific of parquet_read_array_row_mode; see
        !> parquet_read_int32_array_row_mode_row_index_int64.
        module subroutine parquet_read_string_array_row_mode_row_index_int64(reader, name, values, row_index, &
                null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            character(len=*), intent(out) :: values(:) !! that row's element vector.
            integer(int64), intent(in) :: row_index !! 1-based row to read.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_string_array_row_mode_row_index_int64
        !> int32 specific of parquet_read_array_element_mode; see the generic
        !> interface above for the shared "one element across all rows" behavior.
        module subroutine parquet_read_int32_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int32_array_element_mode
        !> int64 specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_int64_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int64_array_element_mode
        !> float32 specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_float32_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real32), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float32_array_element_mode
        !> float64 specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_float64_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real64), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float64_array_element_mode
        !> logical (boolean) specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_logical_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            logical, intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_logical_array_element_mode
        !> string specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_string_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            character(len=*), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_string_array_element_mode
    end interface

    ! ---- get_metadata specifics ----
    interface
        !> int32 specific of parquet_get_metadata; see the generic interface
        !> above for the full key-missing/conversion-failure behavior.
        module subroutine parquet_get_metadata_int32(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            integer(int32), intent(out) :: value !! parsed value.
            integer(int32), intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_int32
        !> int64 specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_int64(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            integer(int64), intent(out) :: value !! parsed value.
            integer(int64), intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_int64
        !> float32 specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_float32(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            real(real32), intent(out) :: value !! parsed value.
            real(real32), intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_float32
        !> float64 specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_float64(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            real(real64), intent(out) :: value !! parsed value.
            real(real64), intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_float64
        !> logical specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_logical(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            logical, intent(out) :: value !! parsed value.
            logical, intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_logical
        !> string specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_string(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            character(len=:), allocatable, intent(out) :: value !! stored value, verbatim.
            character(len=*), intent(in), optional :: default !! fallback if key is missing.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_string
        !> int32 array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_int32_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            integer(int32), allocatable, intent(out) :: value(:) !! parsed values.
            integer(int32), intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_int32_array
        !> int64 array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_int64_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            integer(int64), allocatable, intent(out) :: value(:) !! parsed values.
            integer(int64), intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_int64_array
        !> float32 array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_float32_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            real(real32), allocatable, intent(out) :: value(:) !! parsed values.
            real(real32), intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_float32_array
        !> float64 array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_float64_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            real(real64), allocatable, intent(out) :: value(:) !! parsed values.
            real(real64), intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_float64_array
        !> logical array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_logical_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            logical, allocatable, intent(out) :: value(:) !! parsed values.
            logical, intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_logical_array
        !> string array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_string_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            character(len=:), allocatable, intent(out) :: value(:) !! stored values, verbatim.
            character(len=*), intent(in), optional :: default(:) !! fallback if key is missing.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_string_array
    end interface

    ! ---- Cross-subtree shared string/MAML helpers ----
    interface
        !> Parses a `protected_cols:` entry nested inside `extra:`, e.g.:
        !>   extra:
        !>     protected_cols: col1;col2; col3
        !> or, equivalently:
        !>   extra:
        !>     protected_cols:
        !>     - col1
        !>     - col2
        !>     - col3
        !> `names` is a zero-size array if there is no extra:/protected_cols:
        !> section. Names are trimmed and unquoted; empty tokens (e.g. a stray
        !> ";;" or trailing ";") are skipped. Matching against declared field
        !> names (by output_name) is done by the caller -- this subroutine only
        !> extracts the raw name list. Subroutine (not a function returning
        !> names) to sidestep a gfortran 15.2.0 ICE with a submodule-implemented
        !> module function returning a deferred-length character array result.
        module subroutine parquet_parse_protected_cols(lines, names)
            character(len=*), intent(in) :: lines(:) !! raw MAML source lines to scan.
            character(len=:), allocatable, intent(out) :: names(:) !! trimmed, unquoted protected column names.
        end subroutine parquet_parse_protected_cols
        !> Splits a MAML "key: value" `line` on its first colon, trimming
        !> and unquoting `value` (see parquet_unquote); `key` is trimmed but
        !> never unquoted.
        module subroutine parquet_split_key_value(line, key, value)
            character(len=*), intent(in) :: line !! raw MAML line, "key: value" form.
            character(len=:), allocatable, intent(out) :: key !! trimmed key text.
            character(len=:), allocatable, intent(out) :: value !! trimmed, unquoted value text.
        end subroutine parquet_split_key_value
        !> Strips one matching pair of surrounding single or double quotes
        !> from `s`, if present; returns `s` unchanged otherwise.
        module subroutine parquet_unquote(s, out)
            character(len=*), intent(in) :: s !! text to unquote.
            character(len=:), allocatable, intent(out) :: out !! s with surrounding quotes removed, if any.
        end subroutine parquet_unquote
        !> Case-insensitive lowercase of ASCII letters.
        module subroutine parquet_to_lower(s, out)
            character(len=*), intent(in) :: s !! input string.
            character(len=:), allocatable, intent(out) :: out !! s with every ASCII A-Z lowercased; other characters unchanged.
        end subroutine parquet_to_lower
        !> .true. when `line` is exactly the MAML block header `key` -- "fields:", "extra:",
        !> "keyarray:", "col_map:", "remap:", ... -- ignoring surrounding blanks and ASCII case.
        !>
        !> Every MAML KEY is case-insensitive, so every place that recognizes a block header must
        !> ask this rather than comparing the line against a lowercase literal. A section name and
        !> the keys nested inside it must agree on that rule: they did not once, and a MAML
        !> spelling its section `Extra:` then passed validation (which is case-insensitive) while
        !> its `protected_cols:`/`col_map:`/`remap:`/`filter:`/`sort:` were silently never found.
        !> See feature_risks.md Risk-91.
        !>
        !> Indentation is deliberately NOT considered: a caller that requires a top-level header
        !> keeps its own `line(1:1) /= " "` test, and a caller looking one level inside a block
        !> does not. VALUES are a separate question and stay case-SENSITIVE -- a column name in
        !> `col_map:`/`remap:`/`protected_cols:` is data, not a key.
        module logical function parquet_maml_key_matches(line, key)
            character(len=*), intent(in) :: line !! raw MAML source line.
            character(len=*), intent(in) :: key !! block-header name, written lowercase with its colon.
        end function parquet_maml_key_matches
    end interface

contains

    ! ---- FINAL targets (deliberately module-CONTAINED, not separate module procedures) ----
    !
    ! These three are bound as `final ::` on writer_lock/parquet_writer/parquet_reader, and their
    ! bodies live HERE rather than in parquet_write/parquet_read, which is the reverse of this
    ! file's usual arrangement. It is not a style choice: nagfor 7.2 panics with
    ! `Panic: Cannot find scope id 0` -- an internal compiler error naming no line -- when
    ! compiling ANY submodule of a separately compiled module that declares a FINAL binding whose
    ! target is a SEPARATE MODULE PROCEDURE, under -C=undefined. An empty submodule is enough.
    ! Keeping the target module-contained is the documented ingredient that avoids it, and it costs
    ! nothing: each body is four lines and needs only parquet_bindings, which this module already
    ! uses. See feature_nag_ice_scope_id.md for the 15-line reproducer and the ingredient table.
    ! Do not move these back into a submodule, and give any FUTURE finalizer the same treatment.

    !> FINAL procedure: releases the guard `self` holds, if any. Runs implicitly at every exit from
    !! the procedure holding the lock, so it must never validate anything or abort -- the same rule
    !! writer_finalize follows, and for the same reason (see CLAUDE.md's "Implicit finalizers must
    !! never route through a path that can throw/abort").
    subroutine writer_lock_release(self)
        type(writer_lock), intent(inout) :: self !! lock being finalized.
        if (.not. c_associated(self%handle)) return
        call parquet_writer_leave(self%handle)
        self%handle = c_null_ptr
    end subroutine writer_lock_release

    !> FINAL procedure: safety-net close for a writer whose variable goes out of scope (or is
    !! overwritten) still open; see parquet_writer's own doc comment for why this is not a
    !! substitute for %close.
    subroutine writer_finalize(this)
        type(parquet_writer), intent(inout) :: this !! writer being finalized.
        if (c_associated(this%handle)) then
            call abandon_parquet_writer(this%handle)
            this%handle = c_null_ptr
        end if
    end subroutine writer_finalize

    !> FINAL procedure: safety-net close for a reader whose variable goes out of scope (or is
    !! overwritten) still open; see parquet_writer's doc comment for why this is not a substitute
    !! for %close.
    subroutine reader_finalize(this)
        type(parquet_reader), intent(inout) :: this !! reader being finalized.
        if (c_associated(this%handle)) then
            call close_parquet_reader(this%handle)
            this%handle = c_null_ptr
        end if
    end subroutine reader_finalize

    !> Splits a scalar string of column names into the packed array every name-taking
    !> array form expects. Separators are commas and semicolons, interchangeably;
    !> each token is trimmed of surrounding blanks, and an empty token is dropped
    !> rather than being an error (so "a,,b" is two names and "" is none).
    !>
    !> This is the ONE tokenizer for the whole library. parquet_prefetch_columns'
    !> scalar form has accepted this spelling since 1.0.0 and its behaviour is what
    !> is reproduced here exactly; parquet_tables' own name- and key-list forms call
    !> the same procedure so that two spellings of one operation cannot disagree
    !> about punctuation. Public because a sibling module (parquet_tables) needs it
    !> and has no other route to it; src/parquet.f90 makes it private again, so it
    !> is not part of the `use parquet` surface.
    !>
    !> A SUBROUTINE with an allocatable intent(out) result, never a function
    !> returning character(len=:), allocatable -- see CLAUDE.md's project-wide ban
    !> on that shape (gfortran PR113797, a thread-unsafe hidden length temporary).
    subroutine parquet_split_name_list(text, names)
        character(len=*), intent(in) :: text !! names separated by commas and/or semicolons.
        character(len=:), allocatable, intent(out) :: names(:) !! one entry per non-empty token.
        character(len=:), allocatable :: tok
        integer :: i, start, ntok, maxlen, idx
        logical :: at_boundary

        ! Pass 1: count non-empty tokens and find the longest, so the packed array's
        ! element length covers every name exactly.
        ntok = 0
        maxlen = 0
        start = 1
        do i = 1, len(text) + 1
            at_boundary = (i > len(text))
            if (.not. at_boundary) at_boundary = (text(i:i) == ',' .or. text(i:i) == ';')
            if (at_boundary) then
                tok = trim(adjustl(text(start:i-1)))
                if (len(tok) > 0) then
                    ntok = ntok + 1
                    maxlen = max(maxlen, len(tok))
                end if
                start = i + 1
            end if
        end do

        ! len=1 rather than len=0 keeps a no-token result well-formed, matching what
        ! %column_names does for a zero-column table.
        allocate(character(len=max(maxlen, 1)) :: names(ntok))
        idx = 0
        start = 1
        do i = 1, len(text) + 1
            at_boundary = (i > len(text))
            if (.not. at_boundary) at_boundary = (text(i:i) == ',' .or. text(i:i) == ';')
            if (at_boundary) then
                tok = trim(adjustl(text(start:i-1)))
                if (len(tok) > 0) then
                    idx = idx + 1
                    names(idx) = tok
                end if
                start = i + 1
            end if
        end do
    end subroutine parquet_split_name_list

    !> Appends one AND-combined filter expression; see parquet_filter's own doc
    !> comment for the rule grammar. Unvalidated here -- the reader parses and
    !> validates every rule when it actually applies the filter. The stored
    !> text is deferred-length, so a rule only has to fit filter_max_rule_len
    !> (a sanity bound against unbounded input, not a design limit): adding a
    !> longer rule than any so far re-lengthens every stored entry.
    subroutine parquet_filter_add(this, rule)
        class(parquet_filter), intent(inout) :: this !! filter gaining one rule.
        character(len=*), intent(in) :: rule !! raw filter expression text (max filter_max_rule_len characters).
        character(len=:), allocatable :: tmp(:), preview
        character(len=32) :: cap_str

        if (len(rule) > filter_max_rule_len) then
            write(cap_str, '(i0)') filter_max_rule_len
            ! Only a short preview of the offending rule goes into the message, never the whole
            ! (unboundedly long) text: besides being unreadable on an overlong rule, ifx 2026.1.1's
            ! ERROR STOP runtime corrupts memory once the message reaches 8192 bytes -- confirmed
            ! empirically (a minimal repro with error stop on a plain character(len=8192) message
            ! crashes; 8191 does not), and a rule this long plus the surrounding text easily
            ! crosses that boundary.
            if (len(rule) > 100) then
                preview = rule(1:100) // "..."
            else
                ! Unreachable while filter_max_rule_len stays >= 100 (currently 8192): this
                ! branch needs len(rule) > filter_max_rule_len AND len(rule) <= 100 at once,
                ! which is a contradiction for any such value -- no fixture can build a rule
                ! that is simultaneously "too long" and "100 characters or fewer". Kept only
                ! as a defensive fallback in case filter_max_rule_len is ever lowered below
                ! 100. Revisit if that constant changes.
                preview = trim(rule) ! GCOVR_EXCL_LINE
            end if
            error stop "parquet_filter%add: rule exceeds the maximum supported length (" // trim(cap_str) // &
                " characters): " // preview
        end if

        if (.not. allocated(this%rules)) then
            allocate(character(len=len(rule)) :: tmp(1))
            tmp(1) = rule
            call move_alloc(tmp, this%rules)
            this%n = 1
            return
        end if

        allocate(character(len=max(len(this%rules), len(rule))) :: tmp(this%n + 1))
        tmp(1:this%n) = this%rules(1:this%n)
        tmp(this%n + 1) = rule
        call move_alloc(tmp, this%rules)
        this%n = this%n + 1
    end subroutine parquet_filter_add

    !> Appends one sort key; see parquet_sortkey's own doc comment for the key
    !> grammar and the null-placement rule. Unvalidated here -- the reader
    !> parses each key and checks the column when it actually applies the sort.
    !> The stored text is deferred-length, exactly as parquet_filter%add's is.
    subroutine parquet_sortkey_add(this, key, nulls_first)
        class(parquet_sortkey), intent(inout) :: this !! sort spec gaining one key.
        character(len=*), intent(in) :: key !! "<column> [asc|desc]" (max sortkey_max_key_len characters).
        logical, intent(in), optional :: nulls_first !! place this key's null rows first; defaults to .false. (nulls last).
        character(len=:), allocatable :: tmp(:)
        logical, allocatable :: tmp_nf(:)
        character(len=32) :: cap_str
        logical :: nf

        if (len(key) > sortkey_max_key_len) then
            write(cap_str, '(i0)') sortkey_max_key_len
            error stop "parquet_sortkey%add: key exceeds the maximum supported length (" // trim(cap_str) // &
                " characters): " // trim(key)
        end if
        if (this%n >= sortkey_max_keys) then
            write(cap_str, '(i0)') sortkey_max_keys
            error stop "parquet_sortkey%add: too many sort keys (maximum " // trim(cap_str) // ")"
        end if
        nf = .false.
        if (present(nulls_first)) nf = nulls_first

        if (.not. allocated(this%keys)) then
            allocate(character(len=len(key)) :: tmp(1))
            tmp(1) = key
            call move_alloc(tmp, this%keys)
            allocate(tmp_nf(1))
            tmp_nf(1) = nf
            call move_alloc(tmp_nf, this%nulls_first)
            this%n = 1
            return
        end if

        allocate(character(len=max(len(this%keys), len(key))) :: tmp(this%n + 1))
        tmp(1:this%n) = this%keys(1:this%n)
        tmp(this%n + 1) = key
        call move_alloc(tmp, this%keys)
        allocate(tmp_nf(this%n + 1))
        tmp_nf(1:this%n) = this%nulls_first(1:this%n)
        tmp_nf(this%n + 1) = nf
        call move_alloc(tmp_nf, this%nulls_first)
        this%n = this%n + 1
    end subroutine parquet_sortkey_add

    !> Appends one read-time QC declaration; see parquet_read_qc's own doc
    !> comment for the "col, min, max, miss" grammar, which is
    !> parquet_schema%add_col_qc's verbatim. Unvalidated here -- every check
    !> happens in parquet_compose_read_qc, where the entry becomes a real
    !> schema. The stored text is deferred-length, exactly as
    !> parquet_filter%add's is.
    subroutine parquet_read_qc_add(this, entry)
        class(parquet_read_qc), intent(inout) :: this !! qc declarations gaining one entry.
        character(len=*), intent(in) :: entry !! compact "col, min, max, miss" string.
        character(len=:), allocatable :: tmp(:)
        character(len=32) :: cap_str

        if (len(entry) > read_qc_max_entry_len) then
            write(cap_str, '(i0)') read_qc_max_entry_len
            ! Capped preview, never the whole entry -- see parquet_filter%add's own
            ! comment for the ifx ERROR STOP message-length hazard this avoids.
            error stop "parquet_read_qc%add: entry exceeds the maximum supported length (" // &
                trim(cap_str) // " characters): " // entry(1:100) // "..."
        end if

        if (.not. allocated(this%entries)) then
            allocate(character(len=len(entry)) :: tmp(1))
            tmp(1) = entry
            call move_alloc(tmp, this%entries)
            this%n = 1
            return
        end if

        allocate(character(len=max(len(this%entries), len(entry))) :: tmp(this%n + 1))
        tmp(1:this%n) = this%entries(1:this%n)
        tmp(this%n + 1) = entry
        call move_alloc(tmp, this%entries)
        this%n = this%n + 1
    end subroutine parquet_read_qc_add

end module
