!> The transforms behind `pf_dct`, `pf_idct`, `pf_dst` and `pf_idst`, the two length helpers, every
!> validation and the one `error stop`, and the radix-2 FFT they are all built on.
!!
!! The file is named for the engine rather than for one transform family, because both families
!! share it along with the validator and the abort.
!!
!! **The type-II transform through a complex FFT of the same length** (J. Makhoul, "A fast cosine
!! transform in one and multiple dimensions", IEEE Trans. ASSP 28(1), 1980). For `x` of length
!! `n`, indices from 0:
!!
!! 1. reorder: `v[j] = x[2j]` and `v[n-1-j] = x[2j+1]` -- the even-indexed values in order, then
!!    the odd-indexed ones reversed;
!! 2. transform: `V = FFT(v)`, `v` taken as complex;
!! 3. twiddle: `y[k] = 2 * Re(V[k] * exp(-i*pi*k/(2n)))`.
!!
!! **The inverse runs the same steps backwards.** For a real `v`, `V[n-k] = conj(V[k])`, which
!! makes `y[n-k] = -2*Im(V[k] * exp(-i*pi*k/(2n)))`; so `V[k] = exp(i*pi*k/(2n)) * (y[k] -
!! i*y[n-k]) / 2`, with `y[n]` read as zero, then `v` is the inverse FFT of `V` and `x` is `v` with
!! the reordering undone. The `/2` and the inverse FFT's `1/n` are folded into one exact `1/(2n)`.
!!
!! **Every twiddle factor is a direct `cos`/`sin` of index arithmetic, never a recurrence.** A
!! recurrence is faster and loses accuracy progressively across a pass, an error a round trip
!! partly cancels; `test_dct_matches_the_direct_definition` measures it at `n = 4096` instead.
!! Every angle is formed as `(i/m)*angle` with `m` a power of two, so `i/m` is exact and the
!! multiplication is the one rounding. A compiler rewriting that division as a multiplication by
!! the reciprocal (ifx's default `-fp-model=fast`) changes nothing: the reciprocal is exact too.
!!
!! **Nothing that can abort is `pure`.** The validator, the abort helper and `pf_next_pow2` are
!! impure deliberately: a `pure` guard-only subroutine's call is deleted by ifx at `-O0`
!! (`fortran-gotchas.md`).
!!
!! **The sine pair needs no engine of its own.** With `x'[j] = (-1)**j * x[j]`,
!!
!! ```
!! DST-II(x)[k] = DCT-II(x')[n-1-k]
!! ```
!!
!! -- alternate the signs, transform, reverse -- because
!! `cos(pi*(n-1-k)*(2j+1)/(2n)) = (-1)**j * sin(pi*(k+1)*(2j+1)/(2n))`, and the `(-1)**j` cancels
!! the alternation that was put in. `pf_idst` runs the three steps backwards. The reversal is what
!! carries the unpaired coefficient between the ends: `norm="ortho"` therefore needs no special
!! handling either, since the cosine pair's `sqrt(1/(4n))` at `k = 0` arrives at `k = n-1`, where
!! the sine pair's belongs. So there is one FFT, one validator and one convention here, and a
!! sine-transform error would have to be a cosine-transform error.
!!
!! **Each wrapper validates under its OWN name before delegating**, so a refused `pf_dst` call
!! says `pf_dst:` and not `pf_dct:`; the delegated call is then unconditionally valid and is given
!! no `context`. Four error scenarios pin that (`transform_dst_*`, `transform_idst_*`).
!!
!! **Performance is not a design constraint**: the one known consumer transforms once per bandwidth
!! selection, at `n = 1024`. So the FFT is the complex length-`n` one rather than the real-input
!! form of half the length, and nothing is cached between calls -- which is also what keeps the
!! module free of state. The sine pair adds one pass over the sequence to that.
submodule (parquet_transform) parquet_transform_core

    use iso_fortran_env, only : int64

    implicit none

    !> Twice pi: the angle of one full turn, for the FFT's twiddle factors `exp(-2*pi*i*q/n)`.
    real(real64), parameter :: TWO_PI = 6.283185307179586476925286766559005768_real64
    !> Half of pi: the DCT's twiddle angle `pi*k/(2n)` is formed as `(k/n)*HALF_PI`.
    real(real64), parameter :: HALF_PI = 1.570796326794896619231321691639751442_real64
    !> Longest `norm` token that is matched; a longer one folds to blanks and is refused.
    integer, parameter :: NORM_CAP = 8

contains

    ! ---- the separate module procedures, implemented ahead of every call to them -------------
    !
    ! nagfor rejects a separate module procedure whose body appears BELOW a call to it in the same
    ! submodule (`code-style.md`).

    module procedure pf_is_pow2

        ! Nested rather than `n > 0 .and. iand(n, n - 1) == 0`: `.and.` does not short-circuit,
        ! and `n - 1` overflows at the most negative integer (`fortran-gotchas.md`).
        is_pow2 = .false.
        if (n > 0) is_pow2 = (iand(n, n - 1) == 0)

    end procedure pf_is_pow2

    module procedure pf_next_pow2

        if (n < 1) call transform_abort("pf_next_pow2", "n must be positive")
        if (n > POW2_MAX) call transform_abort("pf_next_pow2", "n must not exceed " &
                                               //trim(int_text(int(POW2_MAX, int64))) &
                                               //", the largest power of two a default integer holds")
        p = int(ceil_pow2(int(n, int64)))

    end procedure pf_next_pow2

    module procedure dct_r64

        complex(real64), allocatable :: work(:) !! the reordered sequence, then its FFT
        real(real64)   :: theta                 !! the twiddle angle pi*k/(2n)
        real(real64)   :: scale                 !! the orthonormal factor for k > 0
        integer(int64) :: n, j, k               !! the length; indices from 0
        logical        :: ortho                 !! `norm` resolved to "ortho"

        n = size(x, kind=int64)
        call validate_call("pf_dct", n, size(y, kind=int64), norm, context, ortho)

        ! Step 1, walked over x's own index so that a length of one needs no special case: x[2i]
        ! goes to v[i] and x[2i+1] to v[n-1-i], with i = j/2 either way.
        allocate (work(0:n - 1))
        do j = 0, n - 1
            if (mod(j, 2_int64) == 0) then
                work(j/2) = cmplx(x(j + 1), 0.0_real64, kind=real64)
            else
                work(n - 1 - j/2) = cmplx(x(j + 1), 0.0_real64, kind=real64)
            end if
        end do

        ! Step 2.
        call fft_radix2(work, .false.)

        ! Step 3: 2*Re(V[k]*exp(-i*theta)) = 2*(Re(V[k])*cos(theta) + Im(V[k])*sin(theta)). At
        ! k = 0 the angle is zero and V[0] = sum(v) = sum(x), so y(1) = 2*sum(x): the convention's
        ! factor of two, in the one place it can be checked by eye.
        do k = 0, n - 1
            theta = HALF_PI*(real(k, real64)/real(n, real64))
            y(k + 1) = 2.0_real64*(real(work(k), real64)*cos(theta) + aimag(work(k))*sin(theta))
        end do

        ! scipy's norm='ortho': sqrt(1/(4n)) at k = 0 and sqrt(1/(2n)) elsewhere. The quotients
        ! are exact, so each factor is one correctly rounded square root.
        if (ortho) then
            y(1) = y(1)*sqrt(0.25_real64/real(n, real64))
            scale = sqrt(0.5_real64/real(n, real64))
            do k = 2, n
                y(k) = y(k)*scale
            end do
        end if

    end procedure dct_r64

    module procedure idct_r64

        complex(real64), allocatable :: work(:) !! the spectrum rebuilt from y, then its inverse FFT
        real(real64)   :: theta, c, s           !! the twiddle angle pi*k/(2n), its cosine and sine
        real(real64)   :: a, b                  !! the unnormalised y[k] and y[n-k]
        real(real64)   :: u0, uk                !! what undoes the orthonormal scaling, k = 0 and k > 0
        real(real64)   :: scale                 !! 1/(2n), exact
        integer(int64) :: n, j, k               !! the length; indices from 0
        logical        :: ortho                 !! `norm` resolved to "ortho"

        n = size(y, kind=int64)
        call validate_call("pf_idct", n, size(x, kind=int64), norm, context, ortho)

        u0 = 1.0_real64
        uk = 1.0_real64
        if (ortho) then
            u0 = sqrt(4.0_real64*real(n, real64))
            uk = sqrt(2.0_real64*real(n, real64))
        end if

        ! The spectrum, twice over: V[k] = exp(i*theta)*(y[k] - i*y[n-k])/2, the /2 left to the
        ! final scaling. At k = 0 the angle is zero and y[n] reads as zero.
        allocate (work(0:n - 1))
        work(0) = cmplx(u0*y(1), 0.0_real64, kind=real64)
        do k = 1, n - 1
            theta = HALF_PI*(real(k, real64)/real(n, real64))
            c = cos(theta)
            s = sin(theta)
            a = uk*y(k + 1)
            b = uk*y(n - k + 1)
            work(k) = cmplx(a*c + b*s, a*s - b*c, kind=real64)
        end do

        call fft_radix2(work, .true.)

        ! The reordering undone, with the /2 and the inverse FFT's 1/n as one exact factor.
        scale = 0.5_real64/real(n, real64)
        do j = 0, n - 1
            if (mod(j, 2_int64) == 0) then
                x(j + 1) = scale*real(work(j/2), real64)
            else
                x(j + 1) = scale*real(work(n - 1 - j/2), real64)
            end if
        end do

    end procedure idct_r64

    ! ---- the sine transforms, on the cosine pair above ------------------------------------------
    !
    ! Both bodies sit BELOW dct_r64 and idct_r64 because they call them: nagfor rejects a separate
    ! module procedure whose body appears below a call to it (`code-style.md`), and pf_dct here IS
    ! that call.

    module procedure dst_r64

        real(real64), allocatable :: work(:) !! the sign-alternated sequence
        real(real64)   :: swap               !! one end of a pair being reversed
        integer(int64) :: n, j               !! the length; indices from 1

        n = size(x, kind=int64)
        call validate_call("pf_dst", n, size(y, kind=int64), norm, context)

        ! x'[j] = (-1)**j * x[j] with j from 0, so the sign flips at every EVEN 1-based index.
        ! Written as a loop rather than work(2::2) = -work(2::2): a section assignment whose two
        ! sides are the same array costs a heap temporary (`fortran-gotchas.md`).
        allocate (work(n))
        do j = 1, n
            if (mod(j, 2_int64) == 0) then
                work(j) = -x(j)
            else
                work(j) = x(j)
            end if
        end do

        ! The cosine transform of x' carries the sine transform of x, backwards. This call has
        ! already been validated under pf_dst's own name, so it cannot abort and needs no context.
        call pf_dct(work, y, norm)

        ! Reverse in place: DCT-II coefficient n-1-k is DST-II coefficient k. Under "ortho" this
        ! also carries the exceptional scaling from k = 0, where the cosine transform puts it, to
        ! k = n-1, where the sine transform's belongs.
        do j = 1, n/2
            swap = y(j)
            y(j) = y(n + 1 - j)
            y(n + 1 - j) = swap
        end do

    end procedure dst_r64

    module procedure idst_r64

        integer(int64) :: n, j !! the length; indices from 1

        n = size(y, kind=int64)
        call validate_call("pf_idst", n, size(x, kind=int64), norm, context)

        ! Reverse, then invert. y(n:1:-1) is a section, not a copy: pf_idct's dummy is plain
        ! assumed-shape and carries no `contiguous` attribute, so the descriptor is passed as it
        ! stands. Already validated above, so this call cannot abort.
        call pf_idct(y(n:1:-1), x, norm)

        ! x[j] = (-1)**j * x'[j], the same alternation undone.
        do j = 2, n, 2
            x(j) = -x(j)
        end do

    end procedure idst_r64

    ! ---- validation and the one error stop ------------------------------------------------------

    !> Checks one transform call in the order of the guide page's table, and resolves `norm`.
    !!
    !! `n` is the length of the call's input (`x` for `pf_dct` and `pf_dst`, `y` for `pf_idct` and
    !! `pf_idst`) and `m` the size of its output. Impure deliberately (see the header).
    !!
    !! `ortho` is optional because the two sine transforms have no use for the resolved token: they
    !! forward the caller's `norm` to the cosine pair, which resolves it again. They still validate
    !! here, under their OWN name, so that a refused call names the procedure the caller wrote.
    subroutine validate_call(entry, n, m, norm, context, ortho)
        character(len=*), intent(in)           :: entry   !! the public procedure, for the message
        integer(int64), intent(in)             :: n       !! the input's length
        integer(int64), intent(in)             :: m       !! the output's size
        character(len=*), intent(in), optional :: norm    !! the caller's token, when given
        character(len=*), intent(in), optional :: context !! caller's call-site text
        logical, intent(out), optional         :: ortho   !! `norm` resolved to `"ortho"`, when wanted

        character(len=:), allocatable :: hint     !! the legal length to suggest, as text
        character(len=NORM_CAP)       :: token    !! `norm` folded to lower case
        integer(int64)                :: next     !! the smallest power of two at or above `n`
        logical                       :: is_ortho !! `norm` resolved, before handing it back

        if (n == 0) call transform_abort(entry, "the sequence must not be empty", context)

        next = ceil_pow2(n)
        if (next /= n) then
            if (next <= POW2_MAX) then
                hint = "; pf_next_pow2 gives "//trim(int_text(next))
            else
                ! Beyond pf_next_pow2's range: only a sequence of more than 2**30 values gets here,
                ! which no test fixture is.
                hint = "; the next power of two is "//trim(int_text(next)) ! GCOVR_EXCL_LINE
            end if
            call transform_abort(entry, "the sequence length must be a power of two (got " &
                                 //trim(int_text(n))//hint//")", context)
        end if

        if (m /= n) call transform_abort(entry, "x and y must have the same size", context)

        is_ortho = .false.
        if (present(norm)) then
            call fold_token(norm, token)
            select case (trim(token))
            case ("none")
                is_ortho = .false.
            case ("ortho")
                is_ortho = .true.
            case default
                call transform_abort(entry, 'norm must be "none" or "ortho"', context)
            end select
        end if
        if (present(ortho)) ortho = is_ortho

    end subroutine validate_call

    !> Aborts with `<entry>: <text>`, plus the caller's context when one was given.
    !!
    !! The single `error stop` of this module. Taken under a named `critical` so that one thread
    !! aborts when the call is inside a parallel region: two threads reaching `ERROR STOP` at once
    !! leave the exit status nondeterministic under ifx. Impure deliberately -- a `pure` guard-only
    !! procedure's call is deleted by ifx at `-O0`.
    subroutine transform_abort(entry, text, context)
        character(len=*), intent(in)           :: entry   !! the public procedure refusing the call
        character(len=*), intent(in)           :: text    !! what went wrong
        character(len=*), intent(in), optional :: context !! caller's call-site text

        character(len=:), allocatable :: msg !! the whole message

        msg = entry//": "//text
        if (present(context)) then
            if (len_trim(context) > CONTEXT_CAP) then
                msg = msg//" (context: "//context(1:CONTEXT_CAP)//"...)"
            else
                msg = msg//" (context: "//trim(context)//")"
            end if
        end if

        ! One thread aborts, not several: two threads reaching ERROR STOP at once leave the exit
        ! status nondeterministic under ifx (`api-conventions.md`).
        !$omp critical (parquet_transform_abort)
        error stop msg
        !$omp end critical (parquet_transform_abort)

    end subroutine transform_abort

    !> Folds a caller's `norm` token to lower case for matching.
    !!
    !! ASCII only. A token whose trimmed length exceeds `NORM_CAP` folds to blanks, which match no
    !! name, so it is refused whole: a token cut short could trim to a name it does not spell.
    pure subroutine fold_token(token, folded)
        character(len=*), intent(in)         :: token  !! the caller's token
        character(len=NORM_CAP), intent(out) :: folded !! its lower-case form, or blanks

        integer :: i, c !! a character's position and its code

        folded = ""
        if (len_trim(token) > NORM_CAP) return
        do i = 1, len_trim(token)
            c = iachar(token(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) c = c + 32
            folded(i:i) = achar(c)
        end do

    end subroutine fold_token

    !> An `int64` as text, left-justified in a 24-character buffer the caller trims.
    pure function int_text(n) result(text)
        integer(int64), intent(in) :: n    !! the integer
        character(len=24)          :: text !! its decimal digits, blank-padded

        write (text, '(i0)') n

    end function int_text

    !> The smallest power of two at or above `n`, for `1 <= n <= 2**62`.
    pure function ceil_pow2(n) result(p)
        integer(int64), intent(in) :: n !! the count to cover
        integer(int64)             :: p !! the smallest power of two that is at least `n`

        p = 1
        do while (p < n)
            p = 2*p
        end do

    end function ceil_pow2

    ! ---- the engine ---------------------------------------------------------------------------

    !> The complex FFT of `a`, in place, by radix-2 decimation in time: `A[k] = sum_j a[j]*w**(j*k)`
    !! with `w = exp(-2*pi*i/n)`, or with `w` conjugated when `inverse` is set; unscaled either way.
    !!
    !! `size(a)` is a power of two, which every caller has validated. The twiddle factors come from
    !! one table of `n/2` direct `cos`/`sin` values, the stage of length `span` reading every
    !! `n/span`-th entry.
    subroutine fft_radix2(a, inverse)
        complex(real64), intent(inout) :: a(0:)   !! the sequence, then its transform
        logical, intent(in)            :: inverse !! conjugate every twiddle factor: the inverse
                                                  !! transform, without its `1/n`

        complex(real64), allocatable :: tw(:) !! the twiddle factors exp(-2*pi*i*q/n), q = 0 .. n/2-1
        complex(real64) :: t, u, w                !! a butterfly's product, its other input, its twiddle
        real(real64)    :: theta                  !! a twiddle angle
        integer(int64)  :: n, i, j, bit          !! the length; bit reversal's index, partner and bit
        integer(int64)  :: q, span, half, stride, start !! a table index; a stage's length, its half,
                                                        !! its step through the table, a block's start

        n = size(a, kind=int64)

        ! Bit reversal: the element at i moves to the index whose binary digits are i's reversed. j
        ! counts up in reversed binary alongside i, and each pair is swapped once, from the lower i.
        j = 0
        do i = 0, n - 2
            if (i < j) then
                t = a(i)
                a(i) = a(j)
                a(j) = t
            end if
            bit = n/2
            do while (bit >= 1 .and. j >= bit)
                j = j - bit
                bit = bit/2
            end do
            j = j + bit
        end do

        ! tw(q) = exp(-2*pi*i*q/n), for q = 0 .. n/2-1: direct values, never a recurrence (see the
        ! header). q/n is exact, because n is a power of two.
        allocate (tw(0:n/2 - 1))
        do q = 0, n/2 - 1
            theta = TWO_PI*(real(q, real64)/real(n, real64))
            tw(q) = cmplx(cos(theta), -sin(theta), kind=real64)
        end do

        ! The butterflies, stage by stage: span 2, 4, ..., n.
        span = 2
        do while (span <= n)
            half = span/2
            stride = n/span
            do start = 0, n - 1, span
                do j = 0, half - 1
                    w = tw(j*stride)
                    if (inverse) w = conjg(w)
                    t = w*a(start + j + half)
                    u = a(start + j)
                    a(start + j) = u + t
                    a(start + j + half) = u - t
                end do
            end do
            span = 2*span
        end do

    end subroutine fft_radix2

end submodule parquet_transform_core
