!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> The public API of parquet-fortran, and the only module a user needs to name:
!> a single `use parquet` brings the whole library into scope.
!>
!> This module is a facade. It re-exports, unchanged, every public entity of the
!> modules that actually implement the library:
!>
!>   * `parquet_io`      -- readers, writers, schemas, MAML parsing/validation,
!>                          filters, sort keys, read-time qc, file metadata.
!>                          Itself a facade over the internal `parquet_core`.
!>   * `parquet_tables`  -- the `parquet_table` container: lazy columnar reads,
!>                          slices, row handles, mutation, table write-out.
!>   * `parquet_columns` -- the `parquet_column` foundation and the `PK_*` kind
!>                          constants.
!>   * `parquet_strings` -- `parquet_string_column`/`parquet_string`.
!>   * `parquet_temporal`-- `parquet_date`/`parquet_time`/`parquet_timestamp`
!>                          and the `parquet_unit_*`/`parquet_ns_*` constants.
!>   * `parquet_list`    -- `parquet_list_column`/`parquet_list_row`.
!>   * `parquet_struct`  -- `parquet_struct_column`/`parquet_struct_row`.
!>   * `parquet_map`     -- `parquet_map_column`/`parquet_map_row`.
!>   * `parquet_sorting` -- `pf_sort`/`pf_argsort`/`pf_permute`/`pf_is_sorted`
!>                          and the `pf_sort_keys` multi-key builder.
!>   * `parquet_stats`   -- array statistics over plain Fortran arrays:
!>                          the `pf_*` reduction family.
!>   * `parquet_random` -- counter-based random numbers: `pf_random_at` and
!>                          friends, reproducible under any OpenMP schedule.
!>   * `parquet_sampling`-- permutations, subsets, resampling and weighted draws.
!>   * `parquet_spatial` -- `pf_spatial_index`: neighbour and k-nearest search.
!>   * `parquet_index`   -- `pf_index_map` and `pf_index_pool`: fast key-to-index
!>                          lookup over single or composite integer keys, and a
!>                          recycling allocator for unique index values.
!>   * `parquet_healpix` -- the HEALPix sphere pixelisation.
!>   * `parquet_toml`   -- `pf_toml`: reading and writing TOML configuration
!>                          files on top of `toml-f`, with checked types, a
!>                          report for every key nobody read, and diagnostics
!>                          that point at the offending line.
!>   * `parquet_logging` -- `pf_logger` and the `pf_log_*` procedures: general
!>                          logging for the calling program. Nothing in this
!>                          library uses it; the library's own messages go
!>                          through `parquet_settings`' verbosity and stream.
!>   * `parquet_utils`   -- `pf_to_lower`/`pf_to_upper`, `pf_to_str` and the
!>                          POSIX path helpers (`pf_join_path`, `pf_dirname`,
!>                          `pf_basename`, `pf_path_ext`, `pf_path_stem`,
!>                          `pf_split_path`, `pf_path_add_suffix`). A leaf
!>                          below everything: nothing in it validates, aborts
!>                          or prints, and it imports only iso_fortran_env.
!>   * `parquet_integrate` -- `pf_integrate`: adaptive quadrature of a function
!>                          of one real64 variable over a finite range, with the
!>                          integrand as an object carrying its own parameters
!>                          or as a plain function.
!>   * `parquet_settings`-- process-global settings: thread caps, writer
!>                          defaults, terminal verbosity and message stream,
!>                          plus the read-only `parquet_max_*` limits.
!>   * `parquet_maml_base` -- `parquet_maml_file`, for embedded MAML schemas.
!>   * `parquet_version` -- `parquet_get_version`, this library's own version
!>                          string. This facade is the ONLY module that
!>                          re-exports it: an Arrow-free tier deliberately does
!>                          not, so a program importing one of those writes its
!>                          own `use parquet_version` line.
!>
!> Those modules remain individually usable (`use parquet_temporal` still works
!> and still costs less to compile against), but nothing requires it -- every
!> name they export is reachable through this one. The exception is
!> `parquet_bindings`, the raw C interop layer, which is deliberately NOT
!> re-exported: it is an implementation detail, not user API.
!>
!> This module holds no code of its own. Both version queries are re-exported
!> from elsewhere: `parquet_get_version` from the leaf `parquet_version`, and
!> `parquet_get_arrow_version` -- the linked Arrow/Parquet C++ versions, which
!> need the C++ boundary -- from `parquet_settings`.
module parquet
    use parquet_io
    use parquet_tables
    use parquet_columns
    use parquet_list
    use parquet_struct
    use parquet_map
    use parquet_strings
    use parquet_temporal
    use parquet_sorting
    use parquet_stats
    use parquet_random
    use parquet_sampling
    use parquet_spatial
    use parquet_index
    use parquet_healpix
    use parquet_logging
    use parquet_toml
    use parquet_utils
    use parquet_integrate
    ! Only the test hook: the transform itself and its contract check are internal.
    use parquet_expkey, only: parquet_debug_exp_key, parquet_debug_set_exp_key_contract
    use parquet_settings
    ! The library's own version string, which lives in a leaf module of its own so that a program
    ! built on an Arrow-free tier can report it without importing anything else. This bare `use` is
    ! the one re-export of it in the library -- see src/parquet_version.f90's header for why no
    ! sibling module carries it.
    use parquet_version
    ! parquet_maml_base is the one sibling imported with an `only:` list rather than in full. Its
    ! other public names (get_parquet_maml and the parquet_maml_maml_example* accessors) return
    ! THIS library's own embedded MAML test fixtures -- they were never part of the public surface,
    ! and a downstream project gets its own generated parquet_maml module from
    ! tools/generate_parquet_maml.sh instead. Only the three types below are user API.
    use parquet_maml_base, only: parquet_maml_file, parquet_maml_missing_column, parquet_maml_col_map_entry
    implicit none
    !
    ! Default accessibility is deliberately PUBLIC here, unlike every other module in this
    ! library: a bare `use <sibling>` with no `only:` list re-exports that module's whole
    ! public surface, which is exactly what a facade wants and what avoids maintaining a
    ! ~120-name `public ::` list that would go stale on every addition. The private
    ! statements below are what keep the siblings' cross-module plumbing out of the namespace a
    ! user gets from `use parquet`. The C interop layer needs no statement: parquet_bindings is
    ! simply never imported here.
    ! `parquet_split_name_list` and `parquet_parse_sort_key` need NO statement here: parquet_core
    ! makes both public so the sibling parquet_tables module can share the library's one name-list
    ! tokenizer and its one sort-key direction grammar rather than keeping second copies, and
    ! `parquet_io` -- the facade this one now re-exports in place of parquet_core -- already hides
    ! them. Adding a `private ::` for either here is an ERROR, not a redundancy: the name is not
    ! accessible in this scope at all, and nagfor reports it as an implicitly-typed local. The
    ! same holds for the seven key-conversion helpers parquet_core publishes for parquet_tables'
    ! %build_index (parquet_filter_real_key, parquet_index_real_key, parquet_date_key,
    ! parquet_time_key, parquet_timestamp_key, parquet_set_family_for_column,
    ! parquet_filter_column_tokens) and the FSET_* family codes they speak in: parquet_io hides
    ! them, so they need -- and may have -- no statement here.
    ! parquet_settings has to make these two public so the write path (a submodule of parquet_core,
    ! a different module) can reach them -- Fortran has no package scope. They are plumbing, not
    ! API, so the facade keeps them out of the namespace `use parquet` hands a user, exactly as the
    ! statements below do for the other cross-module plumbing.
    private :: parquet_valid_compressions, parquet_resolve_writer_compression
    ! The four output channels, the suppression query and the destination resolver are the same
    ! case: every module that emits or prints has to reach them, so parquet_settings makes them
    ! public, and the facade hides them.
    private :: parquet_emit_info, parquet_emit_advice, parquet_emit_warning
    private :: parquet_emit_error_context
    private :: parquet_output_is_suppressed
    private :: parquet_message_unit
    private :: parquet_clamp_to_affinity
    ! The Fortran->C++ mirror push, and the validity bitmap's block width. Both are public from
    ! their own module because a sibling needs them -- parquet_read/parquet_write/
    ! parquet_sorting_oracle call the push when a reader, writer or sort opens, and
    ! parquet_tables_read needs the block width to trim a threaded paste to whole blocks rather
    ! than copying the constant (`.claude/rules/columns-tables.md`, "Disjoint ROWS are not disjoint BITS"). Publishing to
    ! a SIBLING and publishing to every user of the library are different decisions, and only the
    ! first was ever taken. Row 30's code issues 2 and 3.
    private :: parquet_push_settings_to_cpp
    private :: parquet_validity_block_bits
    ! parquet_columns' typed per-cell accessor tier, hidden for the same reason again. These are
    ! how parquet_tables reaches a column's storage without a type-bound call -- which is what
    ! keeps ifx from building a runtime type descriptor in the caller's prologue on every access
    ! (feature_ifx.md). They duplicate no user-facing capability: the identical operations are
    ! already on parquet_column as %get_at/%set_at/%get_elem/%set_elem/%is_null/%set_null/
    ! %clear_null/%data_ptr/%string_column, which is what a user calls. Adding a typed accessor
    ! means adding a line here too.
    private :: parquet_column_get_at, parquet_column_set_at
    private :: parquet_column_get_elem, parquet_column_set_elem
    private :: parquet_column_data_ptr, parquet_column_string_column
    private :: parquet_column_is_null, parquet_column_set_null, parquet_column_clear_null
    ! Not part of that per-cell tier, hidden here for the same reason: it is the non-mutating
    ! `any_null` the table's read accessors must use, and a user already has `%any_null()`.
    private :: parquet_column_any_null
    ! Its ranged sibling, which %print_stat's statistics scan reads a column's nulls through a
    ! block at a time; a user already has `%row_validity_range`.
    private :: parquet_column_row_validity_range
    ! parquet_strings' typed tier, hidden for the same reason once more. These are how
    ! parquet_columns reaches a column's embedded parquet_string_column without a type-bound
    ! call, and they duplicate no user-facing capability: every one of them is already a binding
    ! on parquet_string_column (%is_null, %get, %set, ...), which is what a user calls. Adding a
    ! typed string accessor means adding a line here too.
    private :: parquet_string_column_clear
    private :: parquet_string_column_shrink_to_fit, parquet_string_column_capacity
    private :: parquet_string_column_character_capacity, parquet_string_column_size
    private :: parquet_string_column_character_size, parquet_string_column_null_count
    private :: parquet_string_column_has_validity, parquet_string_column_reserve_validity
    private :: parquet_string_column_append_column, parquet_string_column_append_null
    private :: parquet_string_column_append_values, parquet_string_column_delete_by_mask
    private :: parquet_string_column_set_validity, parquet_string_column_set_where
    private :: parquet_string_column_reserve, parquet_string_column_get, parquet_string_column_copy_to
    private :: parquet_string_column_is_null, parquet_string_column_append_from, parquet_string_column_set
    private :: parquet_string_column_set_null, parquet_string_column_reindex, parquet_string_column_reindex_trusted
    private :: parquet_string_column_gather, parquet_string_column_gather_from
    private :: parquet_string_column_append_nulls
    private :: parquet_string_column_raw_buffers
    private :: parquet_string_column_argminmax
    ! parquet_list's own two internal accessors, hidden for the same reason again: they are how
    ! the LIST write path reaches a list column's offsets and payload without a per-row allocation,
    ! and they duplicate no user-facing capability (%length/%view/%get already answer both
    ! questions one row at a time, which is what a user calls).
    private :: parquet_list_column_offsets, parquet_list_column_payload
    private :: parquet_list_column_row_validity
    ! parquet_struct's own four internal accessors, hidden for the same reason once more: they are
    ! how the STRUCT read and write paths reach a struct column's fields, names and row validity
    ! without a per-cell binding call, and they duplicate no user-facing capability
    ! (%field_count/%field_name/%view/%get_field already answer all of it one row at a time).
    private :: parquet_struct_column_field, parquet_struct_column_names
    private :: parquet_struct_column_row_validity, parquet_struct_column_build
    ! parquet_map's own four internal accessors, hidden for the same reason a third time: they are
    ! how src/parquet_write_map.f90 reaches a map column's offsets, its two flattened entry columns
    ! and its per-row validity without a per-row allocation, and they are of no use to a program
    ! that has the column object itself.
    private :: parquet_map_column_offsets, parquet_map_column_keys
    private :: parquet_map_column_values, parquet_map_column_row_validity
    ! parquet_healpix's run-emitting disc query, hidden for the same reason a fourth time: it is
    ! how `parquet_spatial`'s HEALPix sky backend turns one disc into contiguous slices of its
    ! bucketed point array, and a program holding a pixel list has `pf_query_disc` for that. A
    ! caller who genuinely wants runs writes `use parquet_healpix`.
    private :: pf_query_disc_runs
    !
    ! ---- The sorting tiers' internals need NO `private ::` here, and that is worth stating ----
    !
    ! `parquet_argsort` exports its engine, `sort_key_buf`, the six intrinsic extractors and the
    ! oracle plumbing so that `parquet_sorting` and `parquet_sorting_oracle` can share one copy of
    ! each rather than keeping second copies that could disagree. None of it reaches this facade:
    ! `parquet_sorting` imports the tier with a bare `use` under its own default `private`
    ! accessibility and names only the `pf_` surface, the settings and the debug hooks in its
    ! `public ::` lines, so everything else stops there. `parquet_sorting_oracle` is not imported
    ! here at all -- `parquet_debug_use_fortran_sort_engine` is reachable only by a program that
    ! names that module itself, which is what keeps the C++ engine out of every other build.
    !

end module parquet
