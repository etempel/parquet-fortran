!> `parquet_skycoord`'s sexagesimal angles: the four procedures splitting an angle into its fields
!! and joining them, the three writing positions as text and the three reading them back.
!!
!! **The writers round once, in integers.** An angle is split into whole degrees and a fraction --
!! exactly, since `a - aint(a)` loses nothing -- the fraction is multiplied once by the number of
!! output units in a degree (seconds of time or arcseconds, times `10**q` for `q` decimals) and
!! rounded to the nearest unit, ties to even as C's and Python's formatting round them. Every field
!! after that is integer division, so the carry at 60 seconds, at 60 minutes and at 24 hours is
!! exact and no field can read `60`. No digit goes through the I/O library: each is written by hand
!! into a fixed buffer, so no processor-dependent leading zero or rounding mode reaches the text, and
!! the result is the call's one allocation.
!!
!! **The readers are strict**, because a lenient parse turns a malformed field into a plausible
!! wrong position: three fields in one separator style, each field's digits and range checked
!! before any arithmetic. The seconds convert exactly while they have at most `EXACT_DIGITS`
!! digits -- the digits as an integer divided once by an exact power of ten, which is the correctly
!! rounded quotient -- and through `pf_from_str` beyond that.
!!
!! **A NaN never reaches `int()`**, which traps under nagfor's default `-ieee=stop`: every entry
!! hands a NaN back, or writes `nan`, before anything converts it.
submodule (parquet_skycoord) parquet_skycoord_text
    use parquet_utils, only: pf_wrap_deg, pf_from_str, pf_to_str
    use, intrinsic :: iso_fortran_env, only: int64, real128
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    implicit none

    !> The writers' separator styles: colons, blanks, or the unit letters.
    integer, parameter :: STYLE_COLON = 1, STYLE_BLANK = 2, STYLE_LETTERS = 3
    !> The largest `precision` a writer takes: nine decimals of an arcsecond, ten of a second of time.
    integer, parameter :: MAX_PRECISION = 9
    !> `10**k` for every decimal count a writer uses.
    integer(int64), parameter :: POW10(0:10) = [1_int64, 10_int64, 100_int64, 1000_int64, 10000_int64, 100000_int64, &
        1000000_int64, 10000000_int64, 100000000_int64, 1000000000_int64, 10000000000_int64]
    !> `10.0**k` as doubles, for the seconds a reader converts exactly; every one is exact.
    real(real64), parameter :: POW10_R(0:15) = [1.0e0_real64, 1.0e1_real64, 1.0e2_real64, 1.0e3_real64, 1.0e4_real64, &
        1.0e5_real64, 1.0e6_real64, 1.0e7_real64, 1.0e8_real64, 1.0e9_real64, 1.0e10_real64, 1.0e11_real64, &
        1.0e12_real64, 1.0e13_real64, 1.0e14_real64, 1.0e15_real64]
    !> The most digits a seconds field converts exactly: as an integer it stays below `2**53`.
    integer, parameter :: EXACT_DIGITS = 15
    !> The magnitude, in degrees, from which a declination's whole degrees no default `integer` holds.
    real(real64), parameter :: DMS_LIMIT = real(huge(1), real64)
    !> Room for the longest pair a writer produces: a right ascension of 20 characters, a declination
    !! of at most 28 (`+2147483000d00m00.000000000s`) and the blank between them.
    integer, parameter :: TEXT_MAX = 64

contains

    ! ---- Fields ----

    module procedure pf_deg2hms
        real(real64) :: w, whole, x, t
        integer :: total

        if (deg /= deg) then
            h = 0
            m = 0
            s = deg
            return
        end if
        w = pf_wrap_deg(deg)          ! an infinity comes back NaN, raising IEEE_INVALID
        if (w /= w) then
            h = 0
            m = 0
            s = w
            return
        end if
        ! Seconds of time in the fraction of a degree, rounded once. `w - whole` is below 1 by at least
        ! an ulp of 1, and 240 times that still rounds below 240, so `t` is at most 239 and the whole
        ! seconds never reach the next degree.
        whole = aint(w)
        x = (w - whole) * 240.0_real64
        t = aint(x)
        total = int(whole) * 240 + int(t)
        h = total / 3600
        m = mod(total, 3600) / 60
        s = real(mod(total, 60), real64) + (x - t)
    end procedure pf_deg2hms

    module procedure pf_deg2dms
        real(real64) :: a, whole, frac, x, t
        integer :: total

        sgn = 1
        if (deg /= deg) then
            d = 0
            m = 0
            s = deg
            return
        end if
        if (deg < 0.0_real64) sgn = -1
        a = abs(deg)
        whole = aint(a)
        frac = a - whole              ! an infinity: Inf - Inf, a NaN, raising IEEE_INVALID
        if (frac /= frac) then
            d = 0
            m = 0
            s = frac
            return
        end if
        if (whole >= DMS_LIMIT) then
            d = 0
            m = 0
            s = ieee_value(0.0_real64, ieee_quiet_nan)
            return
        end if
        ! As in `pf_deg2hms`, `t` is at most 3599. The `min` changes no value that reaches it: it keeps
        ! the conversion harmless should an optimiser form it before the test above.
        x = frac * 3600.0_real64
        t = aint(x)
        total = int(t)
        d = int(min(whole, DMS_LIMIT))
        m = total / 60
        s = real(mod(total, 60), real64) + (x - t)
    end procedure pf_deg2dms

    module procedure pf_hms2deg
        deg = (real(h, real64) * 15.0_real64 + real(m, real64) * 0.25_real64) + s / 240.0_real64
    end procedure pf_hms2deg

    module procedure pf_dms2deg
        deg = real(d, real64) + (real(m, real64) * 60.0_real64 + s) / 3600.0_real64
        if (sgn < 0) deg = -deg
    end procedure pf_dms2deg

    ! ---- Writing text ----

    module procedure pf_ra2str
        character(len=TEXT_MAX) :: buf
        integer :: style, p, pos

        call skc_text_args("pf_ra2str", style, p, sep, precision)
        pos = 0
        call skc_put_ra(ra, style, p + 1, buf, pos)
        text = buf(1:pos)
    end procedure pf_ra2str

    module procedure pf_dec2str
        character(len=TEXT_MAX) :: buf
        integer :: style, p, pos

        call skc_text_args("pf_dec2str", style, p, sep, precision)
        pos = 0
        call skc_put_dec(dec, style, p, buf, pos)
        text = buf(1:pos)
    end procedure pf_dec2str

    module procedure pf_radec2str
        character(len=TEXT_MAX) :: buf
        integer :: style, p, pos

        call skc_text_args("pf_radec2str", style, p, sep, precision)
        pos = 0
        call skc_put_ra(ra, style, p + 1, buf, pos)
        call put_text(buf, pos, " ")
        call skc_put_dec(dec, style, p, buf, pos)
        text = buf(1:pos)
    end procedure pf_radec2str

    ! ---- Reading text ----

    module procedure pf_str2ra
        integer :: lo, hi, i
        real(real64) :: value

        call skc_span(text, lo, hi)
        ok = .false.
        if (hi < lo) return
        i = lo
        call skc_read_ra(text, i, hi, value, ok)
        if (ok) ok = i == hi + 1
        if (ok) ra = value
    end procedure pf_str2ra

    module procedure pf_str2dec
        integer :: lo, hi, i
        real(real64) :: value

        call skc_span(text, lo, hi)
        ok = .false.
        if (hi < lo) return
        i = lo
        call skc_read_dec(text, i, hi, value, ok)
        if (ok) ok = i == hi + 1
        if (ok) dec = value
    end procedure pf_str2dec

    module procedure pf_str2radec
        integer :: lo, hi, i, j
        real(real64) :: r, d
        logical :: good

        ok = .false.
        call skc_span(text, lo, hi)
        if (hi < lo) return
        i = lo
        call skc_read_ra(text, i, hi, r, good)
        if (.not. good) return
        ! Between the two angles: blanks, one comma, or both, and at least one of them.
        j = i
        call skip_blanks(text, j, hi)
        if (j <= hi) then
            if (text(j:j) == ",") then
                j = j + 1
                call skip_blanks(text, j, hi)
            end if
        end if
        if (j == i) return
        call skc_read_dec(text, j, hi, d, good)
        if (.not. good) return
        if (j /= hi + 1) return
        ra = r
        dec = d
        ok = .true.
    end procedure pf_str2radec

    ! ---- Helpers: the writers ----

    !> The separator style and the precision a writer was handed, the defaults applied. **Stops the
    !! program**, naming the writer the caller called, on a `sep` or `precision` outside their sets.
    pure subroutine skc_text_args(who, style, p, sep, precision)
        character(len=*), intent(in) :: who !! the writer the caller called, for the message.
        integer, intent(out) :: style !! `STYLE_COLON`, `STYLE_BLANK` or `STYLE_LETTERS`.
        integer, intent(out) :: p !! the precision, in `[0, MAX_PRECISION]`.
        character(len=*), intent(in), optional :: sep !! the caller's separator, if given.
        integer, intent(in), optional :: precision !! the caller's precision, if given.
        character(len=:), allocatable :: t

        style = STYLE_COLON
        if (present(sep)) then
            ! Trailing blanks carry no meaning in a blank-padded argument, so `": "` is a colon and a
            ! run of blanks is a blank; an empty `sep` is neither.
            if (len(sep) >= 1 .and. len_trim(sep) == 0) then
                style = STYLE_BLANK
            else if (sep == ":") then
                style = STYLE_COLON
            else if (sep == "hms") then
                style = STYLE_LETTERS
            else
                if (len(sep) > 100) then
                    t = sep(1:100) // "..."
                else
                    t = sep
                end if
                error stop who // ': sep must be ":", " " or "hms" (got "' // t // '")'
            end if
        end if
        p = 2
        if (present(precision)) then
            if (precision < 0 .or. precision > MAX_PRECISION) then
                call pf_to_str(precision, t)
                error stop who // ": precision must be in [0, 9] (got " // t // ")"
            end if
            p = precision
        end if
    end subroutine skc_text_args

    !> Writes a right ascension into `buf` after `pos`, `q` decimals of a second of time, and
    !! advances `pos`: `hh:mm:ss.sss`, or `nan`.
    pure subroutine skc_put_ra(ra, style, q, buf, pos)
        real(real64), intent(in) :: ra !! right ascension, degrees; any value.
        integer, intent(in) :: style !! the separator style.
        integer, intent(in) :: q !! the seconds' decimals, in `[1, 10]`.
        character(len=*), intent(inout) :: buf !! the text so far.
        integer, intent(inout) :: pos !! the last position written; advanced past the angle.
        real(real64) :: w, whole
        integer(int64) :: deg, units, total

        if (ra /= ra) then
            call put_text(buf, pos, "nan")
            return
        end if
        w = pf_wrap_deg(ra)          ! an infinity comes back NaN, raising IEEE_INVALID
        if (w /= w) then
            call put_text(buf, pos, "nan")
            return
        end if
        whole = aint(w)
        call skc_round_split(whole, w - whole, 240_int64 * POW10(q), deg, units)
        ! Whole seconds of time, the 24 hours a hair below 360 degrees can round to wrapped to 0.
        total = mod(deg * 240_int64 + units / POW10(q), 86400_int64)
        call put_int(buf, pos, total / 3600_int64, 2)
        call put_sep(buf, pos, style, "h")
        call put_int(buf, pos, mod(total, 3600_int64) / 60_int64, 2)
        call put_sep(buf, pos, style, "m")
        call put_int(buf, pos, mod(total, 60_int64), 2)
        call put_text(buf, pos, ".")
        call put_int(buf, pos, mod(units, POW10(q)), q)
        if (style == STYLE_LETTERS) call put_text(buf, pos, "s")
    end subroutine skc_put_ra

    !> Writes a declination into `buf` after `pos`, `q` decimals of an arcsecond, and advances `pos`:
    !! `+dd:mm:ss.ss`, or `nan`.
    pure subroutine skc_put_dec(dec, style, q, buf, pos)
        real(real64), intent(in) :: dec !! declination, degrees.
        integer, intent(in) :: style !! the separator style.
        integer, intent(in) :: q !! the arcseconds' decimals, in `[0, 9]`.
        character(len=*), intent(inout) :: buf !! the text so far.
        integer, intent(inout) :: pos !! the last position written; advanced past the angle.
        real(real64) :: a, whole, frac
        integer(int64) :: deg, units, secs

        if (dec /= dec) then
            call put_text(buf, pos, "nan")
            return
        end if
        a = abs(dec)
        whole = aint(a)
        frac = a - whole              ! an infinity: Inf - Inf, a NaN, raising IEEE_INVALID
        if (frac /= frac) then
            call put_text(buf, pos, "nan")
            return
        end if
        if (whole >= DMS_LIMIT) then
            call put_text(buf, pos, "nan")
            return
        end if
        if (dec < 0.0_real64) then
            call put_text(buf, pos, "-")
        else
            call put_text(buf, pos, "+")
        end if
        ! The `min` changes no value that reaches it; see `pf_deg2dms`.
        call skc_round_split(min(whole, DMS_LIMIT), frac, 3600_int64 * POW10(q), deg, units)
        secs = units / POW10(q)
        call put_int(buf, pos, deg, 2)
        call put_sep(buf, pos, style, "d")
        call put_int(buf, pos, secs / 60_int64, 2)
        call put_sep(buf, pos, style, "m")
        call put_int(buf, pos, mod(secs, 60_int64), 2)
        if (q > 0) then
            call put_text(buf, pos, ".")
            call put_int(buf, pos, mod(units, POW10(q)), q)
        end if
        if (style == STYLE_LETTERS) call put_text(buf, pos, "s")
    end subroutine skc_put_dec

    !> An angle's whole degrees and its fraction rounded to the nearest of `per_deg` units a degree,
    !! ties to even, the carry into the next degree taken.
    !!
    !! The one rounding a writer does: `frac * per_deg` is formed once, and the rest is integer
    !! arithmetic. `per_deg` stays below `2**53`, so it is exact as a double.
    !!
    !! **The rounding is decided from the EXACT product**, which the double `x` is not. Every whole
    !! and half unit in range is a representable double, so `x` can differ from the exact product
    !! across one of them only by landing exactly ON it -- the nearest double to a value within
    !! half an ulp of a representable number is that number -- and a tie is therefore the one place
    !! where `x` does not say which way to go. There the residual of a two-product does:
    !! `two_product_residual` gives `frac*per_deg - x` exactly, so its sign is the side the exact
    !! product falls on, and only a zero residual is a true tie for the ties-to-even rule.
    pure subroutine skc_round_split(whole, frac, per_deg, deg, units)
        real(real64), intent(in) :: whole !! whole degrees; an integer value below `2**53`.
        real(real64), intent(in) :: frac !! the fraction of a degree, in `[0, 1)`.
        integer(int64), intent(in) :: per_deg !! output units a degree: seconds a degree times `10**q`.
        integer(int64), intent(out) :: deg !! whole degrees, one more when the fraction rounds up to it.
        integer(int64), intent(out) :: units !! the rest, in `[0, per_deg)`.
        real(real64) :: x, t, e

        x = frac * real(per_deg, real64)
        t = aint(x)
        units = int(t, int64)
        if (x - t > 0.5_real64) then
            units = units + 1_int64
        else if (x - t == 0.5_real64) then
            e = two_product_residual(frac, real(per_deg, real64), x)
            if (e > 0.0_real64) then
                units = units + 1_int64
            else if (e == 0.0_real64) then
                units = units + mod(units, 2_int64)
            end if
        end if
        deg = int(whole, int64)
        if (units >= per_deg) then
            deg = deg + 1_int64
            units = units - per_deg
        end if
    end subroutine skc_round_split

    !> `a*b - p`, where `p` is the rounded `a*b`: the bits that one rounding dropped.
    !!
    !! **In `real128`, and NOT by a Dekker two-product**, which is the usual way to write this and
    !! is unsafe here. A two-product recovers each factor's halves through `c - (c - a)`, an
    !! identity whose value is algebraically `a`: ifx's default `-fp-model=fast` folds it out, and
    !! the function then returns zero for every argument -- silently sending every tie to the
    !! ties-to-even rule and leaving the writers exactly as wrong as they were (measured: seven
    !! golden text rows one unit out under ifx, none under gfortran). `volatile` is this
    !! repository's usual answer to that class of rewrite and is not available in a `pure`
    !! procedure.
    !!
    !! The product of two doubles needs 106 significant bits and `real128` holds 113, so one
    !! multiplication and one subtraction give the residual under any fp model, with no identity
    !! for an optimiser to fold. The conversion back to `real64` rounds it, which changes neither
    !! its sign nor whether it is zero -- all the caller reads. This is reached only on a tie,
    !! which is 0.03% of renderings.
    pure function two_product_residual(a, b, p) result(e)
        real(real64), intent(in) :: a !! the first factor.
        real(real64), intent(in) :: b !! the second factor.
        real(real64), intent(in) :: p !! their product as a double, `a*b` rounded once.
        real(real64) :: e !! `a*b - p`: zero only where the tie is a true one.

        e = real(real(a, real128) * real(b, real128) - real(p, real128), real64)
    end function two_product_residual

    !> Appends `s` to `buf` after `pos`, and advances `pos`.
    pure subroutine put_text(buf, pos, s)
        character(len=*), intent(inout) :: buf !! the text so far.
        integer, intent(inout) :: pos !! the last position written.
        character(len=*), intent(in) :: s !! what to append.

        buf(pos + 1:pos + len(s)) = s
        pos = pos + len(s)
    end subroutine put_text

    !> Appends the separator the style names, `letter` in the lettered style.
    pure subroutine put_sep(buf, pos, style, letter)
        character(len=*), intent(inout) :: buf !! the text so far.
        integer, intent(inout) :: pos !! the last position written.
        integer, intent(in) :: style !! the separator style.
        character(len=1), intent(in) :: letter !! the unit letter the field just written ends with.

        select case (style)
        case (STYLE_COLON)
            call put_text(buf, pos, ":")
        case (STYLE_BLANK)
            call put_text(buf, pos, " ")
        case default
            call put_text(buf, pos, letter)
        end select
    end subroutine put_sep

    !> Appends `value` in decimal, zero-padded to at least `width` digits, written digit by digit so
    !! no processor's formatting reaches it.
    pure subroutine put_int(buf, pos, value, width)
        character(len=*), intent(inout) :: buf !! the text so far.
        integer, intent(inout) :: pos !! the last position written.
        integer(int64), intent(in) :: value !! the value; at least 0.
        integer, intent(in) :: width !! the fewest digits to write.
        integer(int64) :: v
        integer :: nd, k

        nd = 1
        v = value
        do while (v >= 10_int64)
            v = v / 10_int64
            nd = nd + 1
        end do
        nd = max(nd, width)
        v = value
        do k = nd, 1, -1
            buf(pos + k:pos + k) = achar(iachar("0") + int(mod(v, 10_int64)))
            v = v / 10_int64
        end do
        pos = pos + nd
    end subroutine put_int

    ! ---- Helpers: the readers ----

    !> A right ascension read from `text(i:hi)` in degrees, `i` left just past it.
    pure subroutine skc_read_ra(text, i, hi, deg, ok)
        character(len=*), intent(in) :: text !! the text.
        integer, intent(inout) :: i !! where the angle starts; just past it when `ok`.
        integer, intent(in) :: hi !! the last position the angle may use.
        real(real64), intent(out) :: deg !! the right ascension, degrees; meaningful only when `ok`.
        logical, intent(out) :: ok !! whether a right ascension was read.
        real(real64) :: f1, f3
        integer :: f2
        logical :: neg

        ! Hours at most 24, and 24 only as the exact turn: `24:00:00` is the documented 360 degrees,
        ! while `25:00:00` and `24:00:00.001` are not readings of anything.
        call skc_read_fields(text, i, hi, .false., "h", 2, 24, neg, f1, f2, f3, ok)
        deg = 0.0_real64
        if (ok) deg = (f1 * 15.0_real64 + real(f2, real64) * 0.25_real64) + f3 / 240.0_real64
    end subroutine skc_read_ra

    !> A declination read from `text(i:hi)` in degrees, `i` left just past it.
    pure subroutine skc_read_dec(text, i, hi, deg, ok)
        character(len=*), intent(in) :: text !! the text.
        integer, intent(inout) :: i !! where the angle starts; just past it when `ok`.
        integer, intent(in) :: hi !! the last position the angle may use.
        real(real64), intent(out) :: deg !! the declination, degrees; meaningful only when `ok`.
        logical, intent(out) :: ok !! whether a declination was read.
        real(real64) :: f1, f3
        integer :: f2
        logical :: neg

        ! Degrees at most 90, and 90 only as the pole exactly: `+90:00:00` is what `pf_dec2str`
        ! writes for the north pole and reads back, while `+90:00:00.01` and `+91:00:00` name no
        ! direction on the sky.
        call skc_read_fields(text, i, hi, .true., "d", 3, 90, neg, f1, f2, f3, ok)
        deg = 0.0_real64
        if (ok) then
            deg = f1 + (real(f2, real64) * 60.0_real64 + f3) / 3600.0_real64
            if (neg) deg = -deg
        end if
    end subroutine skc_read_dec

    !> Three sexagesimal fields read from `text(i:hi)`, `i` left just past them.
    !!
    !! The separator after the leading field decides the style, and the second must match it: a
    !! colon, one or more blanks, or the unit letter -- `lead`, then `m`, and an `s` closing the
    !! seconds -- in either case with blanks allowed after it. The leading field has one to
    !! `max_lead` digits and a value of at most `max_value`, which it may reach only with zero
    !! minutes and zero seconds; the minutes one or two digits below 60; the seconds one or two
    !! digits below 60, then an optional point and any number of decimals. **Every range is decided
    !! from the digits**, so no rounding decides an acceptance.
    pure subroutine skc_read_fields(text, i, hi, signed, lead, max_lead, max_value, neg, f1, f2, f3, ok)
        character(len=*), intent(in) :: text !! the text.
        integer, intent(inout) :: i !! where the angle starts; just past it when `ok`.
        integer, intent(in) :: hi !! the last position the angle may use.
        logical, intent(in) :: signed !! whether one leading `+` or `-` is allowed.
        character(len=1), intent(in) :: lead !! the leading field's letter, lowercase: `h` or `d`.
        integer, intent(in) :: max_lead !! the most digits the leading field may have.
        integer, intent(in) :: max_value !! the largest value it may take, reached only exactly.
        logical, intent(out) :: neg !! whether the angle carried a `-`.
        real(real64), intent(out) :: f1 !! the leading field: hours or degrees.
        integer, intent(out) :: f2 !! the minutes.
        real(real64), intent(out) :: f3 !! the seconds.
        logical, intent(out) :: ok !! whether three well-formed fields were read.
        integer :: j, k, n1, style
        logical :: good

        ok = .false.
        neg = .false.
        f1 = 0.0_real64
        f2 = 0
        f3 = 0.0_real64
        j = i
        if (j > hi) return
        if (signed) then
            if (text(j:j) == "+" .or. text(j:j) == "-") then
                neg = text(j:j) == "-"
                j = j + 1
            end if
        end if
        ! The leading field.
        k = j
        call skip_digits(text, j, hi)
        if (j - k < 1 .or. j - k > max_lead) return
        n1 = digits_int(text(k:j - 1))
        if (n1 > max_value) return
        f1 = real(n1, real64)
        ! The first separator decides the style.
        if (j > hi) return
        if (text(j:j) == ":") then
            style = STYLE_COLON
            j = j + 1
        else if (text(j:j) == " ") then
            style = STYLE_BLANK
            call skip_blanks(text, j, hi)
        else if (is_letter(text(j:j), lead)) then
            style = STYLE_LETTERS
            j = j + 1
            call skip_blanks(text, j, hi)
        else
            return
        end if
        ! The minutes.
        k = j
        call skip_digits(text, j, hi)
        if (j - k < 1 .or. j - k > 2) return
        f2 = digits_int(text(k:j - 1))
        if (f2 >= 60) return
        ! The second separator, in the first one's style.
        if (j > hi) return
        select case (style)
        case (STYLE_COLON)
            if (text(j:j) /= ":") return
            j = j + 1
        case (STYLE_BLANK)
            if (text(j:j) /= " ") return
            call skip_blanks(text, j, hi)
        case default
            if (.not. is_letter(text(j:j), "m")) return
            j = j + 1
            call skip_blanks(text, j, hi)
        end select
        ! The seconds: the whole seconds are checked from their digits, so no rounding decides it.
        k = j
        call skip_digits(text, j, hi)
        if (j - k < 1 .or. j - k > 2) return
        if (digits_int(text(k:j - 1)) >= 60) return
        if (j <= hi) then
            if (text(j:j) == ".") then
                j = j + 1
                call skip_digits(text, j, hi)
            end if
        end if
        ! The bound is reached only exactly: a leading field AT `max_value` carries no minutes and
        ! no seconds, so the pole and the whole turn read and nothing past either does.
        if (n1 == max_value) then
            if (f2 /= 0) return
            if (.not. zero_field(text(k:j - 1))) return
        end if
        call seconds_value(text(k:j - 1), f3, good)
        if (.not. good) return
        if (style == STYLE_LETTERS) then
            if (j > hi) return
            if (.not. is_letter(text(j:j), "s")) return
            j = j + 1
        end if
        i = j
        ok = .true.
    end subroutine skc_read_fields

    !> The value of a seconds field -- one or two digits, then optionally a point and decimals.
    !!
    !! Up to `EXACT_DIGITS` digits, the digits as one integer divided once by the power of ten the
    !! decimals make: both are exact doubles, so the quotient is correctly rounded. Longer, it is
    !! read by `pf_from_str`.
    pure subroutine seconds_value(s, value, ok)
        character(len=*), intent(in) :: s !! the field, already checked for shape.
        real(real64), intent(out) :: value !! its value.
        logical, intent(out) :: ok !! whether it converted.
        integer(int64) :: n
        integer :: k, nd, ndec
        logical :: after

        nd = 0
        ndec = 0
        n = 0_int64
        after = .false.
        do k = 1, len(s)
            if (s(k:k) == ".") then
                after = .true.
                cycle
            end if
            nd = nd + 1
            if (after) ndec = ndec + 1
            if (nd <= EXACT_DIGITS) n = n * 10_int64 + int(iachar(s(k:k)) - iachar("0"), int64)
        end do
        if (nd <= EXACT_DIGITS) then
            value = real(n, real64) / POW10_R(ndec)
            ok = .true.
        else
            call pf_from_str(s, value, ok)
        end if
    end subroutine seconds_value

    !> Whether a seconds field is zero, from its characters alone: every digit in it is `0`.
    pure function zero_field(s) result(yes)
        character(len=*), intent(in) :: s !! the field, already checked for shape.
        logical :: yes !! whether the field is zero.
        integer :: k

        yes = .true.
        do k = 1, len(s)
            if (s(k:k) /= "0" .and. s(k:k) /= ".") yes = .false.
        end do
    end function zero_field

    !> The value of a run of at most three decimal digits.
    pure function digits_int(s) result(n)
        character(len=*), intent(in) :: s !! the digits.
        integer :: n !! their value.
        integer :: k

        n = 0
        do k = 1, len(s)
            n = n * 10 + (iachar(s(k:k)) - iachar("0"))
        end do
    end function digits_int

    !> Whether `c` is the letter `lower`, in either case.
    pure function is_letter(c, lower) result(yes)
        character(len=1), intent(in) :: c !! the character.
        character(len=1), intent(in) :: lower !! the letter, lowercase.
        logical :: yes !! whether `c` is it.

        yes = c == lower .or. iachar(c) == iachar(lower) - 32
    end function is_letter

    !> Advances `j` past a run of decimal digits, stopping at `hi + 1` at the latest.
    pure subroutine skip_digits(text, j, hi)
        character(len=*), intent(in) :: text !! the text.
        integer, intent(inout) :: j !! the position; left at the first non-digit.
        integer, intent(in) :: hi !! the last position to look at.

        do while (j <= hi)
            if (text(j:j) < "0" .or. text(j:j) > "9") exit
            j = j + 1
        end do
    end subroutine skip_digits

    !> Advances `j` past a run of blanks, stopping at `hi + 1` at the latest.
    pure subroutine skip_blanks(text, j, hi)
        character(len=*), intent(in) :: text !! the text.
        integer, intent(inout) :: j !! the position; left at the first non-blank.
        integer, intent(in) :: hi !! the last position to look at.

        do while (j <= hi)
            if (text(j:j) /= " ") exit
            j = j + 1
        end do
    end subroutine skip_blanks

    !> The bounds of `text` without its leading and trailing blanks; `hi < lo` when it is all blank.
    pure subroutine skc_span(text, lo, hi)
        character(len=*), intent(in) :: text !! the text.
        integer, intent(out) :: lo !! the first non-blank position.
        integer, intent(out) :: hi !! the last non-blank position.

        hi = len_trim(text)
        lo = 1
        call skip_blanks(text, lo, hi)
    end subroutine skc_span

end submodule parquet_skycoord_text
