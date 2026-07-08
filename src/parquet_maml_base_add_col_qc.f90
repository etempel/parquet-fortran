!===========================================
! Hand-written submodule of parquet_maml_base (NOT generated). Implements the
! parquet_maml_file%add_col_qc type-bound procedure, whose deferred module-procedure
! interface is declared in the (generated) src/parquet_maml_base.f90.
!
! add_col_qc builds a read-time qc-maml incrementally from a compact one-line
! "col, min, max, miss" string, appending a fields: entry with a qc: block.
! It is deliberately self-contained (only intrinsic string handling): its
! parent module sits at the bottom of the module stack, so it cannot reuse the
! qc-maml validation helpers in parquet_metadata without creating a dependency
! cycle. The operator-direction and miss-value rules it enforces mirror those
! in parquet_metadata's parquet_parse_qc_maml / parquet_validate_maml_internal.
!===========================================
submodule (parquet_maml_base) parquet_maml_base_add_col_qc
    implicit none
contains

    module subroutine parquet_maml_add_col_qc(self, qc_input, col_name)
        class(parquet_maml_file), intent(inout) :: self
        character(len=*), intent(in) :: qc_input
        character(len=:), allocatable, intent(out) :: col_name
        !
        integer :: ntok, i
        character(len=:), allocatable :: name_str, min_str, max_str, miss_str
        character(len=:), allocatable :: miss_low
        logical :: has_qc

        ! An empty (or all-blank) qc_input is an explicit no-op: return an
        ! empty col_name and leave self%lines untouched. This is distinct from
        ! a leading comma (e.g. ", >0"), which has content -- an empty first
        ! field -- and is a genuine error (a missing column name).
        if (len_trim(qc_input) == 0) then
            col_name = ""
            return
        end if

        ! qc_input is at most four comma-separated fields:
        !   col_name , qc_min , qc_max , qc_miss
        ! (any of the last three may be empty). More than four is invalid.
        ntok = 1
        do i = 1, len(qc_input)
            if (qc_input(i:i) == ",") ntok = ntok + 1
        end do
        if (ntok > 4) then
            error stop "parquet_maml_file%add_col_qc: qc_input has more than 4 comma-separated fields: '" // &
                trim(qc_input) // "'"
        end if

        ! Positional split (empty token => that field omitted).
        name_str = nth_field(qc_input, 1)
        min_str  = nth_field(qc_input, 2)
        max_str  = nth_field(qc_input, 3)
        miss_str = nth_field(qc_input, 4)

        ! --- validate everything BEFORE mutating self%lines ---

        ! 1) column name (first field) must be non-empty.
        if (len_trim(name_str) == 0) then
            error stop "parquet_maml_file%add_col_qc: the first field (column name) must not be empty: '" // &
                trim(qc_input) // "'"
        end if
        col_name = trim(name_str)

        ! 2) the column must not already be declared in this MAML.
        if (field_name_exists(self, col_name)) then
            error stop "parquet_maml_file%add_col_qc: column '" // col_name // &
                "' is already declared in this qc-maml"
        end if

        ! 3) min:/max: operator direction and empty-value checks.
        call check_bound(min_str, .true., col_name)
        call check_bound(max_str, .false., col_name)

        ! 4) miss: must be empty, Null/null, or NA/na.
        if (len_trim(miss_str) > 0) then
            miss_low = to_lower(trim(adjustl(miss_str)))
            if (.not. (miss_low == "null" .or. miss_low == "na")) then
                error stop "parquet_maml_file%add_col_qc: invalid qc miss value '" // trim(adjustl(miss_str)) // &
                    "' for column '" // col_name // "' (expected Null/NA or empty)"
            end if
        end if

        ! --- emit lines ---

        ! Ensure a fields: header exists (add one on first use).
        if (.not. line_exists(self, "fields:")) call push_line(self, "fields:")

        call push_line(self, "- name: " // col_name)

        has_qc = (len_trim(min_str) > 0) .or. (len_trim(max_str) > 0) .or. (len_trim(miss_str) > 0)
        if (has_qc) then
            call push_line(self, "  qc:")
            ! min:/max: are single-quoted so a leading > or < is not mistaken
            ! for a YAML block-scalar indicator; the qc-maml parser unquotes.
            if (len_trim(min_str) > 0) call push_line(self, "    min: '" // trim(adjustl(min_str)) // "'")
            if (len_trim(max_str) > 0) call push_line(self, "    max: '" // trim(adjustl(max_str)) // "'")
            if (len_trim(miss_str) > 0) call push_line(self, "    miss: " // trim(adjustl(miss_str)))
        end if

    contains

        !> Returns field n (1-based) of a comma-separated string, trimmed of
        !> surrounding blanks; an empty string if n is beyond the last field.
        pure function nth_field(s, n) result(field)
            character(len=*), intent(in) :: s
            integer, intent(in) :: n
            character(len=:), allocatable :: field
            integer :: k, start, cur

            start = 1
            cur = 1
            field = ""
            do k = 1, len(s) + 1
                if (k > len(s)) then
                    if (cur == n) field = trim(adjustl(s(start:len(s))))
                    exit
                else if (s(k:k) == ",") then
                    if (cur == n) then
                        field = trim(adjustl(s(start:k-1)))
                        return
                    end if
                    cur = cur + 1
                    start = k + 1
                end if
            end do
        end function nth_field

        !> Validates one qc min:/max: bound. `raw` may be empty (omitted --
        !> nothing to check), a bare bound (no operator -- accepted as-is;
        !> numeric validity is deferred to read time, since a qc-maml carries
        !> no data_type), or an operator followed by a value. If an operator is
        !> present it must point the right way (min: >=/>, max: <=/<) and be
        !> followed by a non-empty value.
        subroutine check_bound(raw, is_min, colname)
            character(len=*), intent(in) :: raw
            logical, intent(in) :: is_min
            character(len=*), intent(in) :: colname
            character(len=:), allocatable :: t, rem
            character(len=2) :: op
            logical :: has_op

            t = trim(adjustl(raw))
            if (len_trim(t) == 0) return

            has_op = .true.
            if (index(t, ">=") == 1) then
                op = ">="; rem = trim(adjustl(t(3:)))
            else if (index(t, "<=") == 1) then
                op = "<="; rem = trim(adjustl(t(3:)))
            else if (index(t, ">") == 1) then
                op = "> "; rem = trim(adjustl(t(2:)))
            else if (index(t, "<") == 1) then
                op = "< "; rem = trim(adjustl(t(2:)))
            else
                has_op = .false.; rem = t
            end if

            if (has_op) then
                if (is_min .and. op(1:1) == "<") then
                    error stop "parquet_maml_file%add_col_qc: qc: min: for column '" // trim(colname) // &
                        "' uses a '" // trim(op) // "' operator; min: accepts only >= or > (use max: for an upper bound)"
                end if
                if (.not. is_min .and. op(1:1) == ">") then
                    error stop "parquet_maml_file%add_col_qc: qc: max: for column '" // trim(colname) // &
                        "' uses a '" // trim(op) // "' operator; max: accepts only <= or < (use min: for a lower bound)"
                end if
                if (len_trim(rem) == 0) then
                    if (is_min) then
                        error stop "parquet_maml_file%add_col_qc: bad qc min value provided for column '" // trim(colname) // "'"
                    else
                        error stop "parquet_maml_file%add_col_qc: bad qc max value provided for column '" // trim(colname) // "'"
                    end if
                end if
            end if
        end subroutine check_bound

    end subroutine parquet_maml_add_col_qc

    !> Case-insensitive lowercase of ASCII letters.
    pure function to_lower(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i, c
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) then
                out(i:i) = achar(c + 32)
            else
                out(i:i) = s(i:i)
            end if
        end do
    end function to_lower

    !> True if any line of self%lines equals `target` after trimming leading
    !> and trailing blanks (used for the fields: header check).
    pure function line_exists(self, target) result(found)
        type(parquet_maml_file), intent(in) :: self
        character(len=*), intent(in) :: target
        logical :: found
        integer :: i
        found = .false.
        if (.not. allocated(self%lines)) return
        do i = 1, size(self%lines)
            if (trim(adjustl(self%lines(i))) == trim(target)) then
                found = .true.
                return
            end if
        end do
    end function line_exists

    !> True if self%lines already declares a fields: entry named `name`, i.e.
    !> a "- name: <name>" line (matched the same way the qc-maml parser finds
    !> field names). Comparison of the name itself is case-sensitive, as
    !> parquet column names are.
    pure function field_name_exists(self, name) result(found)
        type(parquet_maml_file), intent(in) :: self
        character(len=*), intent(in) :: name
        logical :: found
        integer :: i, colon
        character(len=:), allocatable :: t, key, val
        found = .false.
        if (.not. allocated(self%lines)) return
        do i = 1, size(self%lines)
            t = trim(adjustl(self%lines(i)))
            if (len(t) == 0) cycle
            if (t(1:1) /= "-") cycle          ! only dash field items carry a name:
            t = trim(adjustl(t(2:)))
            colon = index(t, ":")
            if (colon <= 1) cycle
            key = trim(adjustl(t(1:colon-1)))
            if (to_lower(key) /= "name") cycle
            val = trim(adjustl(t(colon+1:)))
            ! strip a single pair of surrounding quotes, if any.
            if (len(val) >= 2) then
                if ((val(1:1) == '"' .and. val(len(val):len(val)) == '"') .or. &
                    (val(1:1) == "'" .and. val(len(val):len(val)) == "'")) then
                    val = val(2:len(val)-1)
                end if
            end if
            if (trim(val) == trim(name)) then
                found = .true.
                return
            end if
        end do
    end function field_name_exists

    !> Appends one line to self%lines, growing the (deferred-length) array and
    !> renormalizing its element length to fit. Only the significant (non-
    !> trailing-blank) part of `s` is stored, but any leading indentation is
    !> preserved.
    subroutine push_line(self, s)
        type(parquet_maml_file), intent(inout) :: self
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: tmp(:)
        integer :: n, newlen, i

        if (allocated(self%lines)) then
            n = size(self%lines)
        else
            n = 0
        end if
        newlen = len_trim(s)
        if (n > 0) newlen = max(newlen, len(self%lines))
        if (newlen < 1) newlen = 1

        allocate(character(len=newlen) :: tmp(n + 1))
        do i = 1, n
            tmp(i) = self%lines(i)
        end do
        tmp(n + 1) = s
        call move_alloc(tmp, self%lines)
    end subroutine push_line

end submodule parquet_maml_base_add_col_qc
