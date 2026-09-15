!> The workers both engines share: the abort helper, the argument validators, the evaluation
!> record and the plain-function wrapper.
!!
!! **Every validator takes the entry point's own name**, so an abort reads
!! `pf_minimize_simplex: step must not contain zero or NaN` rather than naming this file. The
!! generic a caller typed is the only name they can act on.
!!
!! **No validator compares a possibly-NaN value with an ordered operator.** `<`, `<=`, `>` and
!! `>=` signal IEEE invalid on a NaN operand, and Fortran does not guarantee that `.or.`
!! short-circuits, so `.not. ieee_is_finite(v) .or. v < 0` can raise the flag under nagfor's
!! `-ieee=stop` on exactly the input it exists to reject. Each one tests finiteness first and
!! compares only what survived.
submodule (parquet_optimize) parquet_optimize_support

    implicit none

contains

    module procedure optimize_abort

        character(len=:), allocatable :: msg

        msg = entry_point//": "//text
        if (present(context)) then
            if (len_trim(context) > CONTEXT_CAP) then
                msg = msg//" (context: "//context(1:CONTEXT_CAP)//"...)"
            else
                msg = msg//" (context: "//trim(context)//")"
            end if
        end if

        ! One thread aborts, not several: two threads reaching ERROR STOP at once leave the exit
        ! status nondeterministic under ifx. A local engine running under the multistart driver's
        ! threads is why this matters here (`api-conventions.md`).
        !$omp critical (parquet_optimize_abort)
        error stop msg
        !$omp end critical (parquet_optimize_abort)

    end procedure optimize_abort

    module procedure refuse_constrained

        ! pf_constrained_objective EXTENDS pf_objective, so the language accepts one at every
        ! entry point. An engine that cannot read its constraints would minimise it without them
        ! and return a confident answer from the wrong region; this is the only thing that stops
        ! it, and a new entry point without this call reopens the hole.
        select type (f)
        class is (pf_constrained_objective)
            call optimize_abort(entry_point, &
                "this engine does not honour nonlinear constraints; use pf_minimize_cobyla", &
                context)
        end select

    end procedure refuse_constrained

    module procedure validate_size

        if (n < 1) call optimize_abort(entry_point, "at least one variable is required", context)

    end procedure validate_size

    module procedure validate_tolerance

        logical :: bad

        bad = .not. ieee_is_finite(value)
        if (.not. bad) bad = (value < 0.0_real64)
        if (bad) call optimize_abort(entry_point, &
            name//" must be a finite, non-negative number", context)

    end procedure validate_tolerance

    module procedure validate_budget

        if (max_neval < 1) then
            call optimize_abort(entry_point, "max_neval must be positive", context)
        else if (max_neval > huge(1)/2) then
            call optimize_abort(entry_point, "max_neval must not exceed huge(1)/2", context)
        end if

    end procedure validate_budget

    module procedure history_append

        real(real64), allocatable :: grown_x(:,:), grown_f(:)
        integer :: cap, want, npar

        npar = size(x)
        cap = 0
        if (allocated(this%f)) cap = size(this%f)

        if (this%n >= cap) then
            ! Geometric growth, from a first block big enough that a short run never reallocates.
            ! The budget bounds the final length, so this terminates in O(log budget) copies.
            want = max(16, 2*cap)
            allocate(grown_x(npar, want))
            allocate(grown_f(want))
            if (this%n > 0) then
                grown_x(:, 1:this%n) = this%x(:, 1:this%n)
                grown_f(1:this%n) = this%f(1:this%n)
            end if
            call move_alloc(grown_x, this%x)
            call move_alloc(grown_f, this%f)
        end if

        this%n = this%n + 1
        this%x(:, this%n) = x
        this%f(this%n) = f

    end procedure history_append

    module procedure history_trim

        real(real64), allocatable :: cut_x(:,:), cut_f(:)
        integer :: npar, used

        ! An unasked-for or never-appended record comes back ALLOCATED and zero-length, never
        ! unallocated: `size()` on an unallocated result reads an unfilled descriptor, so a caller
        ! must never need `allocated(...)` to use one (`fortran/CLAUDE.md`).
        if (.not. allocated(this%f)) then
            allocate(this%x(0, 0))
            allocate(this%f(0))
            this%n = 0
            return
        end if

        used = this%n
        if (size(this%f) == used) return

        npar = size(this%x, 1)
        allocate(cut_x(npar, used))
        allocate(cut_f(used))
        if (used > 0) then
            cut_x(:, 1:used) = this%x(:, 1:used)
            cut_f(1:used) = this%f(1:used)
        end if
        call move_alloc(cut_x, this%x)
        call move_alloc(cut_f, this%f)

    end procedure history_trim

    module procedure func_objective_eval

        f = this%fun(x)

    end procedure func_objective_eval

end submodule parquet_optimize_support
