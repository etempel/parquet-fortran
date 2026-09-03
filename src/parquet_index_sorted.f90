!> The sorted backend: one `pf_argsort` call at build, a binary search at lookup.
!!
!! 16 bytes per key, exact-fit, with no slack of any kind -- the smallest footprint of the three
!! backends and the one the original specification asked for as "sorted index and binary search --
!! smaller memory footprint (optional, not default)". The price is `O(log n)` per lookup against
!! the other two backends' `O(1)`, which is why the automatic choice never selects it: it has to
!! be asked for with `method="sorted"`.
!!
!! **Frozen once built.** `%set`, `%get_or_add` and `%remove` all refuse a sorted map, because
!! keeping the array sorted through an insertion is `O(n)` per key -- and the exact fit that buys
!! the footprint is exactly what makes an insertion expensive. That refusal is what the footprint
!! is bought with, so it is a property rather than a limitation to be lifted.
!!
!! **Single-component keys only in v1.** A composite sorted backend needs a lexicographic order
!! over the tuple, which means either confirming `pf_argsort` is stable and chaining one pass per
!! component from the last to the first, or extending `parquet_argsort` with a tuple sort. Neither
!! is much work, but the memory argument for this backend is weakest exactly when the keys are
!! widest, so it was deferred; `%build` refuses `ncomp > 1` with a message saying so.
!!
!! The build is already threaded without anything here doing so: `pf_argsort` threads internally
!! on its own rules and reads `sort_threads`, so the `threads=` argument is forwarded to it
!! unchanged rather than reinterpreted.
submodule (parquet_index) parquet_index_sorted
    use parquet_argsort, only: pf_argsort
    implicit none

contains

    module procedure ix_sorted_find
        integer(int64) :: lo, hi, mid

        v = 0_int64
        if (self%nk <= 0_int64) return
        lo = 1_int64
        hi = self%nk
        do while (lo <= hi)
            ! `lo + (hi - lo) / 2` rather than `(lo + hi) / 2`: the sum can overflow for a large
            ! table where the difference cannot. The divisor is a literal, so this is a shift.
            mid = lo + (hi - lo) / 2_int64
            if (self%skeys(mid) < key) then
                lo = mid + 1_int64
            else if (self%skeys(mid) > key) then
                hi = mid - 1_int64
            else
                v = self%svals(mid)
                return
            end if
        end do
    end procedure ix_sorted_find

    module procedure ix_sorted_build
        integer(int64), allocatable :: perm(:)
        integer(int64) :: n, i
        character(len=32) :: t
        logical :: has_v

        n = size(keys, kind=int64)
        has_v = present(values)
        if (allocated(self%skeys)) deallocate(self%skeys)
        if (allocated(self%svals)) deallocate(self%svals)
        allocate(self%skeys(n))
        allocate(self%svals(n))
        self%nk = n
        if (n == 0_int64) return
        call pf_argsort(keys, perm, threads=threads)
        do i = 1_int64, n
            self%skeys(i) = keys(perm(i))
            ! With `values` absent the value for key `i` is `i`, so after the sort the value of the
            ! key now at position `i` is the position it came from -- which is `perm(i)` exactly.
            if (has_v) then
                self%svals(i) = values(perm(i))
            else
                self%svals(i) = perm(i)
            end if
        end do
        ! Duplicates are adjacent once sorted, so one pass finds them and names the offender.
        do i = 2_int64, n
            if (self%skeys(i) == self%skeys(i - 1_int64)) then
                write (t, "(i0)") self%skeys(i)
                error stop "pf_index_map%build: duplicate key " // trim(t) // &
                    " (every key must be unique)"
            end if
        end do
    end procedure ix_sorted_build

end submodule parquet_index_sorted
