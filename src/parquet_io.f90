!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Reading and writing Parquet files, and nothing else: the minimal supported
!> entry point for a program that opens a file, moves columns in or out of it,
!> and closes it again.
!>
!> A single `use parquet_io` brings in the readers, writers, schemas, MAML
!> parsing and validation, row filters, sort keys, read-time quality control and
!> flat key-value file metadata -- together with the element types those calls
!> take and return (`parquet_string_column`/`parquet_string`, `parquet_date`/
!> `parquet_time`/`parquet_timestamp` and their unit selectors, and
!> `parquet_maml_file`) -- plus the process-global settings, so that a program
!> importing this module alone can still choose a writer's compression, cap the
!> library's threads, or silence what it prints.
!>
!> **What it deliberately leaves out** is everything that is not I/O: the
!> `parquet_table` container (`use parquet_tables`), the `parquet_column`
!> foundation (`use parquet_columns`), sorting (`use parquet_sorting` or
!> `use parquet_argsort`), random numbers (`use parquet_random`) and sampling
!> (`use parquet_sampling`). Each of those is its own entry module; `use parquet`
!> is the facade that carries all of them at once.
!>
!> That list is about the NAMESPACE, not about what fpm compiles, and
!> `parquet_random` is the one place the two differ: `pf_random_at` and its
!> siblings are not reachable from `use parquet_io`, but the reader draws
!> `sample_fraction=`'s rows with that generator, so its three-file leaf tier is
!> compiled for this import. The footprint table in
!> doc/pages/operating/choosing-a-module.md counts them.
!>
!> **This module does not make the library Arrow-free.** Reading and writing a
!> Parquet file *is* the Arrow-facing half of parquet-fortran, so this module's
!> graph reaches `parquet_bindings` and the C++ wrapper by design -- unlike
!> `parquet_argsort`, `parquet_sampling`, `parquet_sorting`, `parquet_strings`,
!> `parquet_temporal`, `parquet_columns` and `parquet_random`, whose Fortran
!> graphs do not. Note that no module makes the *package* Arrow-free: `link` is a
!> package-level key in `fpm.toml`, so `src/parquet_wrapper.cpp` is compiled and
!> `-larrow -larrow_compute -lparquet` linked whichever module you import. See
!> doc/pages/operating/choosing-a-module.md.
!>
!> Like the `parquet` facade, this is a re-export shell with no code of its own:
!> `parquet_core` and `parquet_settings` hold the implementations, and this
!> module exists so that neither of those names has to appear in user code.
!> `parquet_core` remains internal and may change in any release; `parquet_io`
!> is covered by the library's semantic-versioning promise. One consequence of
!> the facade shape is that FORD does not list the re-exported names as
!> belonging to this module, so its generated page is nearly empty -- the names
!> are in the site-wide procedure and type listings instead, exactly as
!> CLAUDE.md records for `module/parquet.html`.
module parquet_io
    use parquet_core
    use parquet_settings
    implicit none
    !
    ! Default accessibility is deliberately PUBLIC here, as in src/parquet.f90 and for the same
    ! reason: a bare `use <sibling>` with no `only:` list re-exports that module's whole public
    ! surface, which is what a facade wants and what avoids maintaining a name list that would go
    ! stale on every addition. The `private ::` statements below are the only thing keeping
    ! cross-module plumbing out of the namespace a user gets from `use parquet_io`. Each mirrors a
    ! statement in src/parquet.f90 -- the two facades must hide the same names, or `use parquet_io`
    ! would expose something `use parquet` does not.
    !
    ! parquet_core has to make these two public so the sibling parquet_tables module can reach
    ! them: parquet_split_name_list is the library's one name-list tokenizer and
    ! parquet_parse_sort_key its one sort-key direction grammar, shared so the table layer and the
    ! reader cannot disagree about punctuation. Plumbing, not API.
    private :: parquet_split_name_list
    private :: parquet_parse_sort_key
    ! parquet_settings has to make these public so the write path (a submodule of parquet_core, a
    ! different module) can resolve a compression token, and so every module that emits can reach
    ! the output channels. Same case again: public for want of package scope, hidden here.
    private :: parquet_valid_compressions, parquet_resolve_writer_compression
    private :: parquet_emit_info, parquet_emit_warning, parquet_emit_error_context
    private :: parquet_output_is_suppressed
end module parquet_io
