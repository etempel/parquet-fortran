!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_tables.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> Container-column access for `parquet_table`: the `%col`, `%get`, `%set`, `%add_column` and
!! `%ref` specifics for a `parquet_list_column`, a `parquet_map_column` and a
!! `parquet_struct_column`.
!!
!! **Five specifics per type, and deliberately no more.** They mirror exactly what
!! `parquet_string_column` gets, and decline the same three families for the same reason:
!! `%get_slice`/`%set_slice`, `%get_element`/`%set_element` and `%row%get`/`%set` all address a
!! fixed-width CELL, and a container row is a variable-length object with no such shape. A caller
!! that wants one row reaches the container through `%col` and asks it directly.
!!
!! **None of them takes an `is_valid=` mask**, matching the temporal kinds rather than the array
!! kinds: a container holds its own per-row nullness (`is_null_row`), and `adopt_container`
!! deliberately leaves the surrounding `parquet_column`'s own bitmap unallocated so that there is
!! exactly one answer to "is row i null?". A mask argument here would be the second one.
!! `%get_valid_mask` still answers for a container column, in its rank-1 form -- that is the
!! table-level way to ask, and it reads through to the container.
submodule (parquet_tables) parquet_tables_container
    implicit none
    !
contains
    !
    !> Downcasts an abstract container pointer to a `parquet_list_column`.
    !!
    !! Both callers run `table_require_kind`/`cache_require_kind` first, so by the time this is
    !! reached the column's declared kind is already PK_LIST and `adopt_container` -- the only writer
    !! of a container kind -- takes the kind FROM the container it is given. The two therefore
    !! cannot disagree, which is what makes the `class default` arm below unreachable rather than
    !! merely unlikely. It still aborts rather than leaving `p` null, because a null pointer that
    !! a caller then dereferences is a worse failure than an abort naming the cause.
    subroutine container_as_list(c, p)
        class(parquet_container_column), pointer, intent(in) :: c !! the abstract container.
        type(parquet_list_column), pointer, intent(out) :: p                  !! the same object, concretely typed.
        !
        nullify(p)
        select type (c)
        type is (parquet_list_column)
            p => c
        class default ! GCOVR_EXCL_START -- unreachable, see this procedure's own doc-comment.
            error stop EP // "internal: a PK_LIST column does not hold a parquet_list_column"
        end select ! GCOVR_EXCL_STOP
    end subroutine container_as_list
    !
    !> Downcasts an abstract container pointer to a `parquet_map_column`.
    !!
    !! Both callers run `table_require_kind`/`cache_require_kind` first, so by the time this is
    !! reached the column's declared kind is already PK_MAP and `adopt_container` -- the only writer
    !! of a container kind -- takes the kind FROM the container it is given. The two therefore
    !! cannot disagree, which is what makes the `class default` arm below unreachable rather than
    !! merely unlikely. It still aborts rather than leaving `p` null, because a null pointer that
    !! a caller then dereferences is a worse failure than an abort naming the cause.
    subroutine container_as_map(c, p)
        class(parquet_container_column), pointer, intent(in) :: c !! the abstract container.
        type(parquet_map_column), pointer, intent(out) :: p                  !! the same object, concretely typed.
        !
        nullify(p)
        select type (c)
        type is (parquet_map_column)
            p => c
        class default ! GCOVR_EXCL_START -- unreachable, see this procedure's own doc-comment.
            error stop EP // "internal: a PK_MAP column does not hold a parquet_map_column"
        end select ! GCOVR_EXCL_STOP
    end subroutine container_as_map
    !
    !> Downcasts an abstract container pointer to a `parquet_struct_column`.
    !!
    !! Both callers run `table_require_kind`/`cache_require_kind` first, so by the time this is
    !! reached the column's declared kind is already PK_STRUCT and `adopt_container` -- the only writer
    !! of a container kind -- takes the kind FROM the container it is given. The two therefore
    !! cannot disagree, which is what makes the `class default` arm below unreachable rather than
    !! merely unlikely. It still aborts rather than leaving `p` null, because a null pointer that
    !! a caller then dereferences is a worse failure than an abort naming the cause.
    subroutine container_as_struct(c, p)
        class(parquet_container_column), pointer, intent(in) :: c !! the abstract container.
        type(parquet_struct_column), pointer, intent(out) :: p                  !! the same object, concretely typed.
        !
        nullify(p)
        select type (c)
        type is (parquet_struct_column)
            p => c
        class default ! GCOVR_EXCL_START -- unreachable, see this procedure's own doc-comment.
            error stop EP // "internal: a PK_STRUCT column does not hold a parquet_struct_column"
        end select ! GCOVR_EXCL_STOP
    end subroutine container_as_struct
    !
    module procedure col_ptr_listcol
        integer :: idx
        class(parquet_container_column), pointer :: c
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LIST, "col")
        call parquet_column_container(self%cache%cols(idx)%values, c)
        call container_as_list(c, p)
    end procedure col_ptr_listcol
    !
    module procedure col_ptr_mapcol
        integer :: idx
        class(parquet_container_column), pointer :: c
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_MAP, "col")
        call parquet_column_container(self%cache%cols(idx)%values, c)
        call container_as_map(c, p)
    end procedure col_ptr_mapcol
    !
    module procedure col_ptr_structcol
        integer :: idx
        class(parquet_container_column), pointer :: c
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRUCT, "col")
        call parquet_column_container(self%cache%cols(idx)%values, c)
        call container_as_struct(c, p)
    end procedure col_ptr_structcol
    !
    module procedure get_arr_listcol
        integer :: idx
        class(parquet_container_column), pointer :: c
        type(parquet_list_column), pointer :: src
        class(parquet_container_column), allocatable :: copy
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LIST, "get")
        call parquet_column_container(self%cache%cols(idx)%values, c)
        call container_as_list(c, src)
        ! clone_into rather than a component-wise copy: it is the container's own deep copy, so a
        ! future component is carried across without this file having to learn about it.
        call src%clone_into(copy)
        select type (copy)
        type is (parquet_list_column)
            call arr%move_from(copy)
        end select
    end procedure get_arr_listcol
    !
    module procedure get_arr_mapcol
        integer :: idx
        class(parquet_container_column), pointer :: c
        type(parquet_map_column), pointer :: src
        class(parquet_container_column), allocatable :: copy
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_MAP, "get")
        call parquet_column_container(self%cache%cols(idx)%values, c)
        call container_as_map(c, src)
        ! clone_into rather than a component-wise copy: it is the container's own deep copy, so a
        ! future component is carried across without this file having to learn about it.
        call src%clone_into(copy)
        select type (copy)
        type is (parquet_map_column)
            call arr%move_from(copy)
        end select
    end procedure get_arr_mapcol
    !
    module procedure get_arr_structcol
        integer :: idx
        class(parquet_container_column), pointer :: c
        type(parquet_struct_column), pointer :: src
        class(parquet_container_column), allocatable :: copy
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRUCT, "get")
        call parquet_column_container(self%cache%cols(idx)%values, c)
        call container_as_struct(c, src)
        ! clone_into rather than a component-wise copy: it is the container's own deep copy, so a
        ! future component is carried across without this file having to learn about it.
        call src%clone_into(copy)
        select type (copy)
        type is (parquet_struct_column)
            call arr%move_from(copy)
        end select
    end procedure get_arr_structcol
    !
    module procedure set_arr_listcol
        integer :: idx
        class(parquet_container_column), allocatable :: copy
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LIST, "set")
        call table_require_length(self, idx, arr%nrows(), "set")
        ! An independent copy, then adopt_container MOVES it in -- so the caller's own container
        ! and the table's never share storage, and no second live copy of the payload survives the
        ! call. %set is a value replacement, not a way to hand ownership over.
        call arr%clone_into(copy)
        call self%cache%cols(idx)%values%adopt_container(copy)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_listcol
    !
    module procedure set_arr_mapcol
        integer :: idx
        class(parquet_container_column), allocatable :: copy
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_MAP, "set")
        call table_require_length(self, idx, arr%nrows(), "set")
        ! An independent copy, then adopt_container MOVES it in -- so the caller's own container
        ! and the table's never share storage, and no second live copy of the payload survives the
        ! call. %set is a value replacement, not a way to hand ownership over.
        call arr%clone_into(copy)
        call self%cache%cols(idx)%values%adopt_container(copy)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_mapcol
    !
    module procedure set_arr_structcol
        integer :: idx
        class(parquet_container_column), allocatable :: copy
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRUCT, "set")
        call table_require_length(self, idx, arr%nrows(), "set")
        ! An independent copy, then adopt_container MOVES it in -- so the caller's own container
        ! and the table's never share storage, and no second live copy of the payload survives the
        ! call. %set is a value replacement, not a way to hand ownership over.
        call arr%clone_into(copy)
        call self%cache%cols(idx)%values%adopt_container(copy)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_structcol
    !
    module procedure add_column_listcol
        integer :: idx
        class(parquet_container_column), allocatable :: copy
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, values%nrows())
        call table_new_slot(self, name, force, idx)
        call values%clone_into(copy)
        ! adopt_container is the ONLY writer of a container kind, and it settles the kind, the row
        ! count and the width from the container itself -- so unlike every other %add_column form
        ! there is no %init call here to keep in step with it.
        call self%cache%cols(idx)%values%adopt_container(copy)
        self%cache%cols(idx)%declared_kind = PK_LIST
        self%cache%cols(idx)%width = 1
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_listcol
    !
    module procedure add_column_mapcol
        integer :: idx
        class(parquet_container_column), allocatable :: copy
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, values%nrows())
        call table_new_slot(self, name, force, idx)
        call values%clone_into(copy)
        ! adopt_container is the ONLY writer of a container kind, and it settles the kind, the row
        ! count and the width from the container itself -- so unlike every other %add_column form
        ! there is no %init call here to keep in step with it.
        call self%cache%cols(idx)%values%adopt_container(copy)
        self%cache%cols(idx)%declared_kind = PK_MAP
        self%cache%cols(idx)%width = 1
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_mapcol
    !
    module procedure add_column_structcol
        integer :: idx
        class(parquet_container_column), allocatable :: copy
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, values%nrows())
        call table_new_slot(self, name, force, idx)
        call values%clone_into(copy)
        ! adopt_container is the ONLY writer of a container kind, and it settles the kind, the row
        ! count and the width from the container itself -- so unlike every other %add_column form
        ! there is no %init call here to keep in step with it.
        call self%cache%cols(idx)%values%adopt_container(copy)
        self%cache%cols(idx)%declared_kind = PK_STRUCT
        self%cache%cols(idx)%width = 1
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_structcol
    !
    module procedure col_ref_listcol
        class(parquet_container_column), pointer :: c
        !
        call col_resolve(self, "ref")
        nullify(p)
        call cache_require_kind(self%cache, self%slot, PK_LIST, "ref")
        call parquet_column_container(self%cache%cols(self%slot)%values, c)
        call container_as_list(c, p)
    end procedure col_ref_listcol
    !
    module procedure col_ref_mapcol
        class(parquet_container_column), pointer :: c
        !
        call col_resolve(self, "ref")
        nullify(p)
        call cache_require_kind(self%cache, self%slot, PK_MAP, "ref")
        call parquet_column_container(self%cache%cols(self%slot)%values, c)
        call container_as_map(c, p)
    end procedure col_ref_mapcol
    !
    module procedure col_ref_structcol
        class(parquet_container_column), pointer :: c
        !
        call col_resolve(self, "ref")
        nullify(p)
        call cache_require_kind(self%cache, self%slot, PK_STRUCT, "ref")
        call parquet_column_container(self%cache%cols(self%slot)%values, c)
        call container_as_struct(c, p)
    end procedure col_ref_structcol
    !
end submodule parquet_tables_container ! GCOVR_EXCL_LINE
