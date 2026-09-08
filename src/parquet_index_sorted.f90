!> The sorted backend: one `pf_argsort` call at build, a binary search at lookup.
!!
!! 16 bytes per key, exact-fit, with no slack of any kind -- the smallest footprint of the three
!! backends and the one the original specification asked for as "sorted index and binary search --
!! smaller memory footprint (optional, not default)". A lookup is `O(log n)` inside a prefix
!! bucket against the other two backends' `O(1)` -- in practice level with the hash probe since
!! the prefix table below -- and the automatic choice still never selects it, because it is
!! frozen once built: it has to be asked for with `method="sorted"`.
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
!!
!! **The prefix table.** A binary search over ten million keys is 24 dependent loads, each a DRAM
!! miss on a table that size, and that is what made this backend ten times the hash backend's
!! lookup cost. The table cuts the key RANGE into buckets -- the bucket of a key is its top bits
!! above `spshift`, relative to the smallest key -- and records where each bucket starts in the
!! sorted array, so a lookup loads one table entry and searches the bucket alone: about eight
!! levels rather than 24 at ten million evenly spread keys, and the range test in front of it
!! answers a miss outside the keys with no search at all. Three rules shape it:
!!
!! - **Its size follows the key count, not a fixed 2**16.** The largest power of two with eight
!!   or more keys per bucket, capped at 2**16 buckets (512 KB, and a table that stays in cache);
!!   so it costs at most a sixteenth of the 16 bytes per key, and a map of fewer than sixteen
!!   keys gets none at all. A fixed 512 KB would be three times the keys of a ten-thousand-key
!!   map, on the backend whose reason to exist is memory.
!! - **The shift comes from the SPAN of the keys, not from the keys' width.** Taking the top 16
!!   bits of the raw key puts every key of `1 .. 10**7` in one bucket, which is the common case
!!   and the one that would gain nothing. Shifting so that the span covers the bucket count puts
!!   evenly spread keys evenly into buckets whatever their magnitude, and a clustered set degrades
!!   gracefully: the cluster's bucket is wide and searched as before, the rest of the table is
!!   empty and costs nothing.
!! - **Nothing here can overflow, by construction rather than by guard.** The span `kmax - kmin`
!!   is computed only after the nested test `ix_span_ok` uses (a span past `huge` takes a shift
!!   that leaves exactly the bucket count's bits, so it needs no span). A bucket is
!!   `shifta(key, s) - shifta(kmin, s)`, and `s` is at least 1 by construction -- the table has
!!   at most an eighth as many buckets as keys, and distinct keys span at least their count -- so
!!   both terms sit in `-2**62 .. 2**62 - 1` and their difference fits. And the search's midpoint
!!   is `lo + (hi - lo) / 2`, as it always was. `test_sorted_prefix_shapes` drives the extremes -- keys at `-huge` and `huge` in one
!!   map, a cluster beside one far outlier, a span of exactly the bucket count.
submodule (parquet_index) parquet_index_sorted
    use parquet_argsort, only: pf_argsort
    implicit none

    !> Keys per bucket the table aims for: the bucket search is then three levels deep on
    !! average, and the table one sixteenth of the keys' bytes.
    integer(int64), parameter :: IX_PFX_KEYS_PER_BUCKET = 8_int64
    !> The bucket-count cap, 2**16 -- 512 KB of table, section 6 item 5's figure, sized to stay
    !! resident in cache while a ten-million-key search walks 160 MB of keys.
    integer(int64), parameter :: IX_PFX_MAX_BUCKETS = 65536_int64

contains

    module procedure ix_sorted_find
        integer(int64) :: lo, hi, mid, b

        v = 0_int64
        if (self%nk <= 0_int64) return
        ! Outside the stored range is a miss with no search; inside it the bucket is known to
        ! exist, which is what lets `ix_sorted_bucket` skip every bounds test.
        if (key < self%skeys(1)) return
        if (key > self%skeys(self%nk)) return
        if (self%spnb > 0_int64) then
            b = ix_sorted_bucket(self, key)
            lo = self%spfx(b)
            hi = self%spfx(b + 1_int64) - 1_int64
        else
            lo = 1_int64
            hi = self%nk
        end if
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
        ! **The automatic path has a floor the sort itself does not**: below `IX_SORTED_MIN_THREADED`
        ! keys a threaded sort costs this build up to 1.9x what a serial one does, because the
        ! sort's tail passes open a team from 32768 rows while the permutation build stays serial
        ! far past that. The constant carries the ladder. An explicit `threads=` is forwarded
        ! whatever the key count, as everywhere else here.
        if (present(threads)) then
            call pf_argsort(keys, perm, threads=threads)
        else if (n >= IX_SORTED_MIN_THREADED) then
            call pf_argsort(keys, perm)
        else
            call pf_argsort(keys, perm, threads=1)
        end if
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
                call ix_abort("pf_index_map%build: duplicate key " // trim(t) // &
                    " (every key must be unique)")
            end if
        end do
        call ix_sorted_prefix(self)
    end procedure ix_sorted_build

    module procedure ix_sorted_probe_stats
        integer(int64) :: b, w, d, total

        max_probe = 0_int64
        mean_probe = 0.0_real64
        if (self%nk <= 0_int64) return
        if (self%spnb <= 0_int64) then
            ! No table: the binary search's worst depth over the whole array, the smallest d with
            ! 2**d > nk, which is also every key's depth for the purpose of a mean.
            max_probe = ix_search_depth(self%nk)
            mean_probe = real(max_probe, real64)
            return
        end if
        ! With the table: one load for the bucket, then the search inside it. The mean weights
        ! each bucket's depth by the keys it holds, so an empty bucket contributes nothing.
        total = 0_int64
        do b = 0_int64, self%spnb - 1_int64
            w = self%spfx(b + 1_int64) - self%spfx(b)
            if (w <= 0_int64) cycle
            d = 1_int64 + ix_search_depth(w)
            max_probe = max(max_probe, d)
            total = total + w * d
        end do
        mean_probe = real(total, real64) / real(self%nk, real64)
    end procedure ix_sorted_probe_stats

    !> The binary search's worst depth over `n` keys: the smallest `d` with `2**d > n`.
    pure function ix_search_depth(n) result(d)
        integer(int64), intent(in) :: n !! keys searched; 0 gives 0.
        integer(int64) :: d             !! levels the search can take.
        integer(int64) :: span

        d = 0_int64
        span = 1_int64
        do while (span <= n)
            span = span * 2_int64
            d = d + 1_int64
        end do
    end function ix_search_depth

    !> The prefix bucket of a key known to lie within `skeys(1) .. skeys(nk)` of a map that has
    !! a table. Two operations: an arithmetic shift by at least 1, then a subtraction of two
    !! values in `-2**62 .. 2**62 - 1`, which cannot overflow. Callers test the range first --
    !! there is no bounds test here, and a key outside the range would index outside the table.
    pure function ix_sorted_bucket(self, key) result(b)
        type(pf_index_map), intent(in) :: self !! a sorted map with `spnb > 0`.
        integer(int64), intent(in) :: key      !! a key within the stored range.
        integer(int64) :: b                    !! its bucket, `0 .. spnb - 1`.

        b = shifta(key, self%spshift) - self%spbase
    end function ix_sorted_bucket

    !> Builds the prefix table over `skeys(1 : nk)`, or leaves the map without one when the keys
    !! are too few for two buckets. One pass over the sorted keys: buckets are non-decreasing
    !! along the array, so each bucket's first position is the position of the first key whose
    !! bucket reaches it, and every bucket the keys skip starts where the next occupied one does.
    !!
    !! **The shift.** For a span that fits `int64`, the smallest `s` with `shiftr(span, s)` below
    !! the bucket count -- at least 1, because the count is at most an eighth of the keys and
    !! distinct keys span at least their count; the bucket count is then recomputed exactly from
    !! the shifted ends, since floors can leave it one above the target (the table is allocated
    !! for what it is, never for the target). For a span past `huge` -- keys at both ends of
    !! `int64` -- `s` is set so the shifted key keeps exactly the bucket count's bits, and no span
    !! is ever formed.
    subroutine ix_sorted_prefix(self)
        type(pf_index_map), intent(inout) :: self !! a sorted map whose `skeys(1:nk)` is final.
        integer(int64) :: n, nb, span, nbk, i, b, bprev, kmin, kmax
        integer :: lg, s
        logical :: wide

        self%spnb = 0_int64
        self%spshift = 0
        self%spbase = 0_int64
        if (allocated(self%spfx)) deallocate(self%spfx)
        n = self%nk
        if (n < 2_int64 * IX_PFX_KEYS_PER_BUCKET) return
        ! The largest power of two with IX_PFX_KEYS_PER_BUCKET or more keys per bucket, capped.
        nb = 1_int64
        lg = 0
        do while (nb * 2_int64 * IX_PFX_KEYS_PER_BUCKET <= n .and. nb < IX_PFX_MAX_BUCKETS)
            nb = nb * 2_int64
            lg = lg + 1
        end do
        kmin = self%skeys(1)
        kmax = self%skeys(n)
        ! NESTED, not `.and.`-ed: `huge + kmin` overflows for a positive `kmin` (the guard's own
        ! overflow, the class CLAUDE.md records under "`.and.` does not short-circuit").
        wide = .false.
        if (kmin < 0_int64) then
            if (kmax > huge(kmax) + kmin) wide = .true.
        end if
        if (wide) then
            ! The span is 2**63 or more: shifting by 64 - lg leaves lg bits, i.e. at most 2**lg
            ! buckets between the two shifted ends, and no span is needed to know it.
            s = 64 - lg
        else
            span = kmax - kmin
            s = 0
            do while (shiftr(span, s) >= nb)
                s = s + 1
            end do
        end if
        self%spshift = s
        self%spbase = shifta(kmin, s)
        nbk = shifta(kmax, s) - self%spbase + 1_int64
        allocate(self%spfx(0:nbk))
        bprev = -1_int64
        do i = 1_int64, n
            b = ix_sorted_bucket(self, self%skeys(i))
            do while (bprev < b)
                bprev = bprev + 1_int64
                self%spfx(bprev) = i
            end do
        end do
        do while (bprev < nbk)
            bprev = bprev + 1_int64
            self%spfx(bprev) = n + 1_int64
        end do
        self%spnb = nbk
    end subroutine ix_sorted_prefix

end submodule parquet_index_sorted
