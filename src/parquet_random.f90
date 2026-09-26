!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! ROUTE (e), and why this file opens with a preprocessor fork.
!
! Philox's round is built on two 32x32 -> 64 multiplies. Both operands are below 2**32, so the
! true product is below 2**64 -- which does not fit in a signed 64-bit integer, so writing the
! multiply directly is signed overflow. That is not a theoretical complaint: gfortran 14 and 15
! have both been observed MISCOMPILING the unprotected form at -O3, silently, with no warning
! under -Wall -Wextra, producing plausible-looking wrong values.
!
! Where the compiler has a 128-bit integer kind the product is formed in it instead. The
! operands still being below 2**32, the product is then provably in range and the undefined
! behaviour is removed outright rather than merely avoided; the optimiser narrows it straight
! back to a native multiply, so this is a statement that the product cannot overflow rather than
! a wider operation at run time.
!
! THREE arms, not two. Where there is no 128-bit kind there is a second way to remove the
! overflow, and nagfor takes it (PF_SAFE64): both Philox multipliers are ODD, so
! `M*c == 2*((M/2)*c) + c`, and `(M/2)*c` is bounded by (2**31-1)(2**32-1), below 2**63. The
! doubling is then done on halves so no intermediate reaches 2**63 either. Nothing on that arm can
! overflow at any input, which is what lets nagfor run this module under `-C=intovf` -- its
! `-ftrapv` -- where the wrapping arm aborts on the first draw. ifx keeps the wrapping product
! (PF_SAFE64 is not selected for it) because the overflow-free spelling is not free: measured on
! machine A, `pf_random_at` costs 8.82 ns wrapping against 21.49/12.87 = 1.67x under nagfor and
! 1.94x under a gfortran build forced onto this arm. That is a deliberate split -- correctness is
! identical on all three arms and verified bit-for-bit, so the only thing traded is speed against
! the ability to run a checking build.
!
! nagfor's arm is a CHOICE, not a necessity, and that is now measured rather than assumed: the
! wrapping kernel compiled by nagfor reproduces every golden vector at -O0, -O2, -O3 and -O4
! (`tools/check_random_kernels.sh`'s forced half, which reaches it by pre-expanding this file --
! nagfor has no `-U`). So safe64 buys `-C=intovf` and nothing else there. Worth contrasting with
! the compiler that does have a problem with it: gfortran's *wrapping* arm fails at `-O3 -flto`
! and `-Ofast -flto` (504 mismatches, Risk-101), which is the exposure the whole fork exists for.
!
! The arms are bit-exact, and that is asserted rather than assumed: `tools/check_random_kernels.sh`
! builds all three against the same golden vectors at every optimisation setting. Worth knowing
! what that found -- the wrapping arm FAILS there under `-O3 -flto` and `-Ofast -flto` (504
! mismatches, feature_risks.md Risk-101), and the safe64 arm passes at every setting. Removing the
! undefined behaviour removed a real miscompilation, not a theoretical one.
!
! "The compiler wraps this expression" is NOT the same claim as "this expression is safe", and
! conflating the two has already cost this module one silent wrong answer. ifx wraps the round
! multiply faithfully -- and was still caught using the undefinedness of a DIFFERENT overflowing
! expression to delete a branch that read the result's sign, hundreds of lines away (see
! `width_of`). Every remaining wrapping site therefore rests on the cross-implementation agreement
! sweep in `test_random`, not on a wrapping measurement.
!
! The fork is a compiler ALLOWLIST plus a build-breaking capability assertion. cpp cannot
! evaluate selected_int_kind(38), which is why a predefine is needed at all; fpm passes -cpp
! (or -fpp) automatically, so nothing is asked of a consumer. Deliberately absent: any
! consumer-supplied macro or escape hatch, which would reintroduce exactly the silent
! wrong-kernel hazard this exists to close. Every arm is keyed on a macro the COMPILER predefines
! -- `__GFORTRAN__`, `__flang__`/`__FLANG`, `__NAG_COMPILER_RELEASE`/`NAGFOR` -- so adding nagfor's
! arm widened the allowlist without opening that door. The remaining silent direction -- a capable
! compiler not named below quietly taking the wrapping path -- is closed by a test, not by cpp:
! `parquet_debug_random_uses_int128()` must agree with `selected_int_kind(38) > 0`, and
! `parquet_debug_random_uses_safe64()` names the third arm, which the first cannot distinguish
! from the wrapping one (both answer .false. there).

#if defined(__GFORTRAN__) || defined(__flang__) || defined(__FLANG)
#  define PF_INT128 1
#elif defined(__NAG_COMPILER_RELEASE) || defined(NAGFOR)
#  define PF_SAFE64 1
#endif

!> Counter-based random numbers: reproducible under any OpenMP schedule, at any thread count.
!!
!! An ordinary generator carries state, so which value a loop iteration receives depends on how
!! many draws came before it -- which, in a parallel loop, depends on the schedule, the thread
!! count and the machine's timing. This module removes the state instead of guarding it. Every
!! value is a pure function of three coordinates: a `seed`, a stream index `i`, and a 1-based
!! `draw` within that stream. Iteration 5000 gets the same number whether it ran first, last,
!! alone, or on a machine with 384 cores.
!!
!! ```fortran
!! !$omp parallel do schedule(dynamic)
!! do i = 1, n
!!     x(i) = pf_random_at(seed, i)          ! same value for this i, always
!! end do
!! ```
!!
!! The generator is **Philox4x32-10**, a counter-based cipher of established quality. Every value
!! this module can produce is frozen by the contract `pf_random_algorithm` names: a change to any
!! of it is a major version, and the identifier is how a program can tell.
!!
!! Not cryptographic. Do not use it for keys, tokens, or anything an adversary should not be able
!! to predict -- the seed is recoverable from a handful of outputs.
module parquet_random

    use iso_fortran_env, only: int32, int64, real32, real64
    use parquet_expkey, only: exp_key, ek_round => ek_rnd
    use parquet_ziggurat, only: zig_layers, zig_r, zig_w, zig_k, zig_f
    use ieee_arithmetic, only: ieee_value, ieee_positive_inf, ieee_negative_inf

    ! **This module depends on `iso_fortran_env`, `ieee_arithmetic` and ONE project module, and
    ! both halves of that matter.** A program that only draws random numbers links no settings, no
    ! sorting, and so no `parquet_bindings` and no Arrow. Everything that needed more than the bare
    ! generator (the permutation, the subset/resample forms, the weighted draw) lives in
    ! `parquet_sampling`, which is free to import whatever it needs precisely because this one is not.
    !
    ! Both imports are admissible because each is itself a leaf -- `iso_fortran_env` and nothing
    ! else -- so the standalone scripts can compile them alongside this file. The distributions
    ! need both: `parquet_expkey` because `-log(u)` from libm would make every `_portable` draw a
    ! per-libm value, which is exactly the property class this module exists to avoid, and
    ! `parquet_ziggurat` because 771 layer constants belong in a generated data module rather than
    ! in the middle of this one.
    !
    ! **Adding a `use` here is therefore a decision, not a detail.** The leaf property is what lets
    ! `tools/check_random_kernels.sh` and `tools/check_exp_key.sh` compile the kernel standalone,
    ! against several compilers and flag sets, without an Arrow install. The rule, enforced by
    ! `check_parquet_random_stays_leaf` in `tools/check_source_conventions.py`, has two clauses: an
    ! imported module must transitively reach nothing but COMPILER-SUPPLIED modules, and it must
    ! appear in BOTH standalone scripts' `SRC` lists. `parquet_settings_base` is admissible under
    ! the first clause for the case where a setting genuinely is needed; reach for that one, never
    ! `parquet_settings`.

    implicit none
    private

    public :: pf_random_algorithm
    public :: pf_exp_algorithm
    public :: pf_normal_algorithm
    public :: pf_gamma_algorithm
    public :: pf_poisson_algorithm
    public :: pf_normal_truncated_algorithm
    public :: pf_random_at
    public :: pf_random32_at
    public :: pf_random_bits_at
    public :: pf_random_int_at
    public :: pf_random_pair_spare_at
    public :: pf_random_fill_draws
    public :: pf_random_fill_streams
    public :: pf_random_exp_at
    public :: pf_random_exp_portable_at
    public :: pf_random_fill_exp
    public :: pf_random_fill_exp_portable
    public :: pf_random_normal_at
    public :: pf_random_normal_portable_at
    public :: pf_random_fill_normal
    public :: pf_random_fill_normal_portable
    public :: parquet_debug_normal_path
    public :: parquet_debug_gamma_path
    public :: parquet_debug_poisson_path
    public :: parquet_debug_normal_truncated_path
    public :: pf_random_seed
    public :: pf_random_key
    public :: parquet_debug_random_uses_int128
    public :: parquet_debug_random_uses_safe64
    public :: parquet_debug_random_block
    ! ---- Points on a sphere ----
    public :: pf_sphere_algorithm
    public :: pf_random_direction_at
    public :: pf_random_radec_at
    public :: pf_random_disc_at
    public :: pf_random_disc_radec_at
    public :: pf_random_ball_at
    public :: pf_random_vmf_at
    public :: pf_random_vmf_radec_at
    public :: pf_random_rotation_at
    public :: pf_random_fill_direction
    public :: pf_random_fill_radec

    !> Identifies the algorithm together with every mapping this module freezes -- the cipher, the
    !! key and counter layout, the word order, the integer rule and its retry key. Its value changes
    !! if and only if one of those changes, so a program that records it can tell whether a stored
    !! result is still reproducible. Identical on both sides of the route (e) fork, which changes
    !! how a product is formed and never what it equals.
    !!
    !! `/v2` differs from `/v1` in exactly one mapping: `pf_random_int_at`'s draw axis, which had
    !! stride 4 (a whole block per value, half of it unused) and now has stride 2, agreeing with
    !! `pf_random_at` and `pf_random_bits_at`. Draw 1 is unchanged; draws from 2 up moved. The
    !! cipher, the key and counter layout, the word order, the rejection rule and the retry key are
    !! all identical between the two.
    character(len=*), parameter :: pf_random_algorithm = "philox4x32-10/v3"

    ! ---- Frozen contract identifiers for the DISTRIBUTIONS ----
    !
    ! One identifier per distribution family, separate from `pf_random_algorithm` and from each
    ! other. The separation is the point: a program that records the uniform contract must not be
    ! told its uniform draws moved because a distribution's internals changed, and a program that
    ! records one distribution must not be told so by another's. `pf_random_algorithm` does not
    ! move during Phase 3; if it does, something has gone wrong.
    !
    ! Where a family has two realisations, ONE identifier covers both -- a program recording a
    ! single string wants to know whether either form moved, and two strings for one distribution
    ! invites recording only one.

    !> Identifies the exponential mapping, covering **both** realisations: the transform, the
    !! `1 - u` convention that keeps a draw finite, which logarithm each realisation uses, and the
    !! two-word cost they share.
    !!
    !! One identifier for the pair, per the rule above -- a program recording a single string wants
    !! to know whether either form moved. The two halves after the slash name the two logarithms:
    !! `libm` for `pf_random_exp_at` (fast, bit-identical for a given libm) and `expkey` for
    !! `pf_random_exp_portable_at` (`parquet_expkey`'s frozen transform, identical on every
    !! platform and compiler).
    character(len=*), parameter :: pf_exp_algorithm = "exp:-log(1-u)/libm+expkey/v1"

    !> Identifies the normal mapping, covering **both** realisations and every part of each that
    !! decides a value: the Ziggurat's layer count and the construction that solved for its
    !! tables, the polar variant, both rejection loops' draw order and word cost, and the
    !! sub-stream labels the coordinate-addressed forms derive.
    !!
    !! `ziggurat256` is `%normal`/`pf_random_normal_at` -- 256 equal-area layers from
    !! `parquet_ziggurat`, with libm `log`/`exp` in the tail and the wedge test, so bit-identical
    !! for a given libm. `polar` is `%normal_portable`/`pf_random_normal_portable_at` -- Marsaglia's
    !! polar method over `parquet_expkey`'s frozen logarithm and IEEE `sqrt`, identical on every
    !! platform.
    !!
    !! **It moves if `parquet_ziggurat` is regenerated with a different layer count**, because
    !! every value moves with it. It does NOT move when `pf_random_algorithm` does, and vice
    !! versa: a program recording one must not be told the other changed.
    character(len=*), parameter :: pf_normal_algorithm = "normal:ziggurat256+polar/libm+expkey/v1"

    !> Identifies the Gamma mapping: Marsaglia-Tsang's squeeze-and-log rejection, the
    !! `Gamma(a) = Gamma(a+1) * u**(1/a)` boost that extends it below `shape = 1`, the draw order
    !! within a candidate, and -- named explicitly, because it decides every value -- **which normal
    !! the inner loop consumes**.
    !!
    !! **The dependency has to be in the string.** Gamma draws a normal per candidate, so a change
    !! to the normal changes every Gamma value; a program recording only this identifier would
    !! otherwise miss it. `normal=ziggurat256` is that statement, and it makes
    !! `pf_normal_algorithm`'s first component load-bearing here too.
    !!
    !! **Bit-identical for a given libm only, and there is no portable form** (Q3-9). The boost
    !! needs `u**(1/a)` -- a `pow` this project has not frozen and does not intend to -- so a
    !! `%gamma_portable` could not keep the promise its name would imply, whatever the rest of the
    !! algorithm did. Do not add one for symmetry with `%normal`.
    character(len=*), parameter :: pf_gamma_algorithm = &
        "gamma:marsaglia-tsang+boost/normal=ziggurat256/libm/v1"

    !> Identifies the Poisson mapping: both algorithms, the crossover between them, and each one's
    !! draw order.
    !!
    !! **The crossover is contract, not a tuning knob**, which is why it is in the string: it
    !! decides which algorithm runs and therefore which value comes back. Below `lambda = 10` the
    !! draw is Knuth's product of uniforms, which is exact and terminates unconditionally; at or
    !! above it, Hoermann's transformed rejection with squeeze (PTRS), which is `O(1)` in `lambda`
    !! where Knuth is `O(lambda)`.
    !!
    !! Bit-identical for a given libm: PTRS needs `log` and `log_gamma`, and Knuth needs `exp`.
    character(len=*), parameter :: pf_poisson_algorithm = "poisson:knuth|10|ptrs/libm/v1"

    !> Identifies the truncated normal mapping: the three proposals, the rule choosing between
    !! them, both of that rule's thresholds, each proposal's draw order, and -- named explicitly,
    !! for the reason `pf_gamma_algorithm` names it -- **which normal the naive case consumes**.
    !!
    !! **The case rule is contract, not a tuning knob.** Which proposal runs decides which value
    !! comes back, so moving either threshold moves draws while every distributional test still
    !! passes (`feature_risks.md` Risk-248). It is in the string for the same reason the Poisson's
    !! crossover is in `pf_poisson_algorithm`.
    !!
    !! **Bit-identical for a given libm, and there is no portable form.** The uniform and tilted
    !! proposals both weigh an `exp` from libm, and `parquet_expkey` freezes a logarithm only, so a
    !! `%normal_truncated_portable` could not keep the promise its name would imply -- the same
    !! argument that keeps `%gamma_portable` from existing. Do not add one for symmetry with
    !! `%normal`.
    character(len=*), parameter :: pf_normal_truncated_algorithm = &
        "normal_trunc:robert95-3case/naive+uniform+exptilt/normal=ziggurat256/libm/v1"

    !> Identifies the points-on-a-sphere mappings: every part of every family that decides a value.
    !!
    !! One string covers all eight producers and both coordinate forms of each, on the rule the
    !! exponential's identifier gives: a program recording a single string wants to know whether any
    !! of them moved. It names, in order: `archimedes`, the direction's `z = 2*u1 - 1` and
    !! `phi = 2*pi*u2`; `frame`, the right-handed frame that takes `+z` onto a centre (the normalised
    !! centre crossed with the axis along which the caller's centre has its smallest component), and
    !! the cap's `1 - cos` offset formed from half-angle sines; `vmfinv`, the von Mises-Fisher
    !! inverse CDF on the `1 - u` convention, evaluated without cancellation at every `kappa`;
    !! `cbrt`, the ball's radius as a cube root of a mixture written on the ratio `r_inner/radius`;
    !! and `shoemake`, the quaternion construction and its expansion into a matrix. Behind all of them
    !! sit the family labels, the one-block-per-draw mapping, the standard frame and the pole rule of
    !! the RA/Dec forms.
    !!
    !! **Bit-identical for a given libm**: every family takes a sine and a cosine, and the vMF a
    !! logarithm, from libm. There are no `_portable` forms, for the reason `%gamma_portable` does
    !! not exist -- nothing here could keep the promise such a name would make.
    character(len=*), parameter :: pf_sphere_algorithm = &
        "sphere:archimedes+frame+vmfinv+cbrt+shoemake/libm/v1"

    !> Where the truncated normal's straddling arm switches from naive rejection to the uniform
    !! proposal. **Frozen contract; see `pf_normal_truncated_algorithm`.**
    !!
    !! Both proposals cover the same target integral over `[a, b]`, so their expected costs are in
    !! the ratio of their envelope masses: `b - a` for the uniform proposal against `sqrt(2*pi)`
    !! for the whole normal. They therefore cross exactly here. Not fitted, not measured, and not
    !! arbitrary -- unlike `poisson_crossover`, which is arbitrary within a region.
    real(real64), parameter :: trunc_straddle_crossover = 2.5066282746310002_real64

    !> Where Poisson switches from Knuth's product to PTRS. **Frozen contract; see
    !! `pf_poisson_algorithm`.** Knuth costs one uniform per unit of `lambda`, PTRS a constant two
    !! per candidate at about 99 % acceptance, and they cross in this region; the exact point is
    !! arbitrary within it and is fixed here so that a value never depends on a build.
    real(real64), parameter :: poisson_crossover = 10.0_real64

    !> Label separating the coordinate-addressed Ziggurat's sub-streams from every other family.
    !!
    !! **Not decoration -- `feature_risks.md` Risk-123.** Without it, `pf_random_normal_at` and
    !! `pf_random_normal_portable_at` would run their rejection loops over the SAME words at the
    !! same coordinate, so two draws a caller believes are independent would be two different
    !! functions of one uniform. Every marginal test still passes in that state; only a joint test
    !! sees it. The two values below need only differ from each other and from 0.
    integer(int64), parameter :: normal_zig_label = 4839268151750326891_int64
    !> Label separating the coordinate-addressed polar form's sub-streams. See `normal_zig_label`.
    integer(int64), parameter :: normal_polar_label = 1572035988640217453_int64

    ! ---- Points on a sphere: the family labels ----
    !
    ! **Not decoration -- `feature_risks.md` Risk-123**, for the reason `normal_zig_label` gives. A
    ! direction consumes two uniforms, so a family reading the raw words at its own coordinate would
    ! hand a caller who also draws `pf_random_at` there a weight that IS its direction's `z`. Each
    ! family therefore reads `(pf_random_key(seed, label), i)` at block `draw - 1`, and the values
    ! below need only differ from each other, from 0 and from every other label in the library;
    ! `tools/generate_random_golden_vectors.py --self-test` reads them back from this file and checks
    ! exactly that. The two coordinate forms of one family share its label on purpose: they are the
    ! same point, not two draws.

    !> Label of `pf_random_direction_at`, `pf_random_radec_at` and their stream and bulk forms.
    integer(int64), parameter :: sphere_direction_label = 2349958352582825620_int64
    !> Label of `pf_random_disc_at` and `pf_random_disc_radec_at`.
    integer(int64), parameter :: sphere_disc_label = 3756084276780366762_int64
    !> Label of `pf_random_ball_at`'s direction.
    integer(int64), parameter :: sphere_ball_label = 8225117764143958046_int64
    !> Label of `pf_random_ball_at`'s radius uniform, read at `(key, i, draw)` in `pf_random_at`'s grid.
    integer(int64), parameter :: sphere_ball_radius_label = 8082096235867769506_int64
    !> Label of `pf_random_vmf_at` and `pf_random_vmf_radec_at`.
    integer(int64), parameter :: sphere_vmf_label = 2338640209577774087_int64
    !> Label of `pf_random_rotation_at`'s first two uniforms.
    integer(int64), parameter :: sphere_rotation_label = 6988326662294119861_int64
    !> Label of `pf_random_rotation_at`'s third uniform, read at `(key, i, draw)` in `pf_random_at`'s grid.
    integer(int64), parameter :: sphere_rotation_angle_label = 8949935999960278870_int64

    !> `pi` in `real64`, private: this module imports no utility tier for a constant.
    real(real64), parameter :: sphere_pi = 3.14159265358979323846264338327950288_real64
    !> `2*pi`, the doubling of `sphere_pi` and so exact.
    real(real64), parameter :: sphere_two_pi = 2.0_real64 * sphere_pi
    !> Radians per degree, folded as a constant so every compiler rounds it the same way.
    real(real64), parameter :: sphere_deg2rad = sphere_pi / 180.0_real64
    !> Degrees per radian.
    real(real64), parameter :: sphere_rad2deg = 180.0_real64 / sphere_pi
    !> The exponent of the ball's cube root.
    real(real64), parameter :: sphere_third = 1.0_real64 / 3.0_real64
    !> The component magnitude above which `x*x + y*y` cannot go subnormal, so `hypot` is not needed.
    !!
    !! The squares underflow below about `1.5e-154`; this sits four decades clear of it.
    real(real64), parameter :: sphere_hypot_safe = 1.0e-150_real64
    !> The last draw a sphere family can address, `2**62`: one block per draw, and block `2**62` would
    !! set bit 62 of the block index and read another word space silently.
    integer(int64), parameter :: sphere_draw_max = 4611686018427387904_int64

    !> Smallest `u1**2 + u2**2` the polar form accepts, `2**-53`.
    !!
    !! The rejection loop already refuses `s >= 1`; this refuses the other end, and it is a domain
    !! guard rather than a distributional choice. `exp_key` is frozen and verified over
    !! `[2**-53, 1]` and no wider, so accepting a smaller `s` would call it outside the range
    !! `tools/check_exp_key.sh` sweeps. The probability of refusing a candidate for this reason is
    !! about `2**-53`, so the distortion is some twelve orders of magnitude below anything
    !! measurable -- and, being a comparison against a constant, it is exactly reproducible.
    real(real64), parameter :: polar_min_s = 2.0_real64**(-53)

    ! ---- Philox4x32-10 constants (verified against Random123) ----

    !> Low 32 bits set: masks a 64-bit register down to one Philox word.
    integer(int64), parameter :: M32 = 4294967295_int64
    !> The widest range the 32-bit candidate rule is allowed to serve. A block is four 32-bit words,
    !! so a narrow draw costs a quarter of an enciphering instead of a half; the price is that the
    !! rejection rate is `(2**32 mod s)/2**32`, which rises with `s` and reaches 33.3 % just above
    !! `2**32/3`. Capping the WIDTH at `2**24` bounds the worst case over every admitted `s` at
    !! 0.389 %, at `s = 16 711 936`. Note that the cap is on the width `hi - lo + 1`, never on
    !! `size(idx)`, which are the same number for a resample and different for everything else.
    integer(int64), parameter :: NARROW32_CAP = 16777216_int64
    !> The bits each half of a block keeps back from `to_real64`, which reads the top 53 of 64.
    integer, parameter :: SPARE_BITS = 11
    !> The mask selecting one half's spare bits, `2**SPARE_BITS - 1`.
    integer(int64), parameter :: SPARE_MASK = 2047_int64
    !> The largest range width the two halves' spare bits together can decide, `2**22`.
    integer(int64), parameter :: SPARE_CAP = 4194304_int64

    ! ---- Per-generic word spaces (domain tags) ----
    !
    ! One `(seed, stream)` pair names FOUR independent sequences of 32-bit words, not one, and each
    ! generic family reads its own. Two values taken at different `(generic, draw)` coordinates are
    ! therefore never functions of the same bits. Note the
    ! two DOCUMENTED identities that survive: `pf_random_at` is the top 53 bits of
    ! `pf_random_bits_at`, and `pf_random_exp_at` is `-log(1-u)` for that same `u`. Both live inside
    ! `DOM_REAL64` on purpose.
    !
    ! **The tag occupies bits 62-63 of the BLOCK INDEX, which no caller can reach.** `draw` is
    ! `integer(int64)` and every path bounds it at `huge(int64)`, so a stride-2 block index is at
    ! most `(huge-1)/2 = 0x3FFF...` and a stride-1 one at most `(huge-1)/4`. Both leave bits 62-63
    ! clear. The stream index and the key are user-controlled and could not carry a tag safely; the
    ! block index can, and it costs one `ior` on a value already in a register.
    !
    ! **The precondition, which is a property of this file rather than of the language:** no call
    ! site may hand `random_block` a block index above the one its own draw addresses. The two fills
    ! that read a block ahead (`blk + 1_int64`) do so only inside a `k + 4 <= m` guard, so the block
    ! ahead is always one the caller asked for. A future fill that prefetched unconditionally would
    ! set bit 62 and land in another domain, silently. See `feature_risks.md`.
    !
    ! `DOM_REAL64` is deliberately 0, so its blocks are byte-identical to those of every build
    ! before the spaces were split -- which is what keeps every uniform, every bit pattern and all
    ! four distributions at the values they have always returned.
    !
    ! Spelled with `ibset` rather than a shift: `ishft(3_int64, 62)` forms a value at or above
    ! `2**63` in a constant expression, which is the hazard `CLAUDE.md` records for `-128_int8`.

    !> Word space of `pf_random_at`, `pf_random_bits_at` and everything built on them.
    integer(int64), parameter :: DOM_REAL64 = 0_int64
    !> Word space of `pf_random32_at`.
    integer(int64), parameter :: DOM_REAL32 = ibset(0_int64, 62)
    !> Word space of `pf_random_int_at` over a range of `NARROW32_CAP` values or fewer.
    integer(int64), parameter :: DOM_INT_NARROW = ibset(0_int64, 63)
    !> Word space of `pf_random_int_at` over a wider range.
    integer(int64), parameter :: DOM_INT_WIDE = ibset(ibset(0_int64, 62), 63)
    !> `2**32`, the modulus of the 32-bit rejection threshold.
    integer(int64), parameter :: TWO32 = 4294967296_int64
    !> First round multiplier.
    integer(int64), parameter :: PHILOX_M0 = int(z'D2511F53', int64)
    !> Second round multiplier.
    integer(int64), parameter :: PHILOX_M1 = int(z'CD9E8D57', int64)
    !> Key bump for word 0 (the golden ratio).
    integer(int64), parameter :: PHILOX_W0 = int(z'9E3779B9', int64)
    !> Key bump for word 1 (sqrt(3) - 1).
    integer(int64), parameter :: PHILOX_W1 = int(z'BB67AE85', int64)
    !> Round count. Ten is the standard, well past the point where bias is measurable.
    integer, parameter :: PHILOX_ROUNDS = 10

    ! ---- SplitMix64 finaliser constants ----
    !
    ! Assembled from two 32-bit halves rather than written as one 64-bit BOZ literal: the halves
    ! are unambiguous constant expressions on every compiler, where a full-width BOZ whose top bit
    ! is set relies on a conversion rule that is easy to get subtly wrong.

    !> First multiplier of the SplitMix64 finaliser.
    integer(int64), parameter :: MIX_A = ior(ishft(int(z'BF58476D', int64), 32), int(z'1CE4E5B9', int64))
    !> Second multiplier of the SplitMix64 finaliser.
    integer(int64), parameter :: MIX_B = ior(ishft(int(z'94D049BB', int64), 32), int(z'133111EB', int64))

    !> Offset in label space that keeps a retried integer draw's key away from
    !! `pf_random_key(seed, n)`. Without it a retried draw would read the same block as the
    !! user's own derived-key family for label `n` -- 100 % of the time, against a documented
    !! idiom. Load-bearing, not decoration.
    integer(int64), parameter :: RETRY_TAG = ishft(int(z'5A170000', int64), 32)

    !> Low 16 bits set: one limb of the strict multiply.
    integer(int64), parameter :: M16 = 65535_int64

#ifdef PF_SAFE64
    !> Low 31 bits set: splits a value below `2**63` into the part that survives a doubling.
    integer(int64), parameter :: M31 = 2147483647_int64
    !> `PHILOX_M0 / 2`. Both Philox multipliers are ODD, which is what makes the halved-constant
    !! identity `M*c == 2*((M/2)*c) + c` hold with a bare `+ c` rather than a masked select.
    integer(int64), parameter :: PHILOX_H0 = ishft(PHILOX_M0, -1)
    !> `PHILOX_M1 / 2`; see `PHILOX_H0`.
    integer(int64), parameter :: PHILOX_H1 = ishft(PHILOX_M1, -1)
    !> Build-breaking assertion that both multipliers really are odd, since the `+ c` correction
    !! above is silently wrong for an even one. A zero divisor here is a compile error naming this
    !! line; the alternative is a wrong stream that only the golden vectors would catch.
    integer, parameter :: pf_safe64_assert = &
        1 / merge(1, 0, iand(PHILOX_M0, 1_int64) == 1_int64 .and. iand(PHILOX_M1, 1_int64) == 1_int64)
#endif

#ifdef PF_INT128
    !> The 128-bit integer kind route (e) forms Philox's multiplies in.
    integer, parameter :: k128 = selected_int_kind(38)
    !> Build-breaking capability assertion: if the allowlist above selected this fork on a target
    !! whose compiler has no 128-bit kind, `k128` is -1 and this is a division by zero in a
    !! constant expression -- a compile error naming this line, rather than a kind error somewhere
    !! confusing, or worse, silence.
    integer, parameter :: pf_int128_assert = 1 / merge(1, 0, k128 > 0)
    !> One Philox word's mask, widened.
    integer(k128), parameter :: M32_128 = int(M32, k128)
    !> 2**63, as the boundary at which a 64-bit pattern held in the wide kind is negative.
    integer(k128), parameter :: TWO63_128 = int(huge(1_int64), k128) + 1_k128
    !> 2**64.
    integer(k128), parameter :: TWO64_128 = 2_k128 * TWO63_128
    !> Low 64 bits set, widened.
    integer(k128), parameter :: MASK64_128 = TWO64_128 - 1_k128
#endif


    !> Process-wide call counter behind `pf_random_seed`.
    !!
    !! **This is the module's only mutable state**, and deliberately so: `pf_random_seed` is the one
    !! procedure here that is not a pure function, precisely because it has to differ between two
    !! calls the clock cannot separate. Everything else in `parquet_random` answers from its
    !! arguments alone, which is what makes the module reproducible and what
    !! `tools/check_random_kernels.sh` relies on when it compiles this file standalone.
    !!
    !! Updated by an `!$omp atomic capture` (see `pf_random_seed`), so a concurrent increment cannot
    !! be lost and two racing calls cannot be handed the same count. **Without OpenMP that atomic is
    !! a comment**, so the counter is then an ordinary read-modify-write -- which is sound here only
    !! because this library's threading model IS OpenMP; a caller that threads a non-OpenMP build by
    !! some other means must not treat `pf_random_seed` as thread-safe.
    !!
    !! nagfor's `-thread_safe` reports the increment ("Assignment to SEED_CALL_COUNTER ... in
    !! thread-safe procedure PF_RANDOM_SEED"). That diagnostic is a static "this procedure writes a
    !! variable from an outer scope" test with no notion of an atomic or a critical region, so it
    !! fires on every deliberate process-global in this library and cannot be satisfied while the
    !! counter exists. It is answered here rather than removed.
    integer(int64), save :: seed_call_counter = 0_int64


    !> One uniform `real64` in `[0, 1)`: value `draw` (default 1) of stream `i` under `seed`.
    !!
    !! `i` is `integer(int32)` or `integer(int64)`; the two give identical values, because an
    !! `int32` stream sign-extends before it reaches the counter. `seed` and `draw` are always
    !! `integer(int64)`, so an explicit draw literal is written `3_int64`.
    interface pf_random_at
        module procedure pf_random_at_i32
        module procedure pf_random_at_i64
    end interface pf_random_at

    ! ---- ONE STREAM IS ONE SEQUENCE OF WORDS, AND real32 WALKS A FINER GRID ----
    !
    ! This is the module's single most important cross-cutting fact and the one a caller is most
    ! likely to get wrong, so it is stated once here rather than a third of it on each generic.
    !
    ! A `(seed, i)` pair names one deterministic sequence of 32-bit Philox words. The four
    ! coordinate-addressed generics are VIEWS of that one sequence, and two strides exist:
    !
    !   generic                                    words read for draw d (1-based)   stride
    !   pf_random32_at(seed, i, d)                 d-1                               1
    !   pf_random_at / pf_random_bits_at(.., d)    2d-2, 2d-1                        2
    !   pf_random_int_at(seed, i, lo, hi, d)       2d-2, 2d-1                        2
    !
    ! Two rules follow, and together they are the whole story:
    !
    !   1. The three 64-bit generics AGREE on what draw `d` means. At one coordinate they are three
    !      presentations of the same 64 bits -- not independent draws -- and at different draws they
    !      are independent. So "walk the draw axis" IS a safe rule among them.
    !   2. `pf_random32_at` has its own finer grid, one word per value, and it deliberately is not a
    !      narrowing of `pf_random_at`. Its draws `2d-1` and `2d` are the two halves of 64-bit draw
    !      `d`, so mixing it with the others on ONE stream still aliases across draw indices:
    !
    !        pf_random32_at(seed, i, 2d-1)  ==  the LOW word of bits_at(seed, i, d)   500 of 500
    !
    ! The three constructions that cannot alias at all are: a separate STREAM index, a separate
    ! family from `pf_random_key`, or `pf_random_stream`, which tracks its own word cursor.
    !
    ! **`pf_random_int_at` had stride 4 until `pf_random_algorithm` reached `/v2`**, addressing the
    ! whole of block `d-1` and using its first pair -- which made it equal `pf_random_bits_at` at
    ! draw `2d-1`, so rule 1 above was false and an integer at draw 2 collided with a real at draw
    ! 3. Aligning the strides fixed that, halved the cost of a draw-axis integer fill, and left draw
    ! 1 bit-identical. See `feature_risks.md` Risk-113.
    !
    ! Domain-separating the generics -- folding a per-generic constant into the key -- would remove
    ! rule 2 as well, and would break the property `pf_random_stream` exists to provide: that a
    ! stream hands out exactly the values the coordinate-addressed calls give at the same positions
    ! (`test_stream_values`). That correspondence is possible only because every generic reads one
    ! word space. It was considered and declined for that reason.

    !> One uniform `real32` in `[0, 1)`: value `draw` (default 1) of stream `i` under `seed`.
    !!
    !! This enumerates its OWN sequence, one word per value, and is deliberately not a narrowing
    !! of `pf_random_at` -- the two share a stream but not a value. `i` is `integer(int32)` or
    !! `integer(int64)`; `seed` and `draw` are `integer(int64)`.
    interface pf_random32_at
        module procedure pf_random32_at_i32
        module procedure pf_random32_at_i64
    end interface pf_random32_at

    !> 64 raw bits: value `draw` (default 1) of stream `i` under `seed`, as `integer(int64)`.
    !!
    !! Reads the same two words as `pf_random_at`, so `pf_random_at` is exactly this pattern's top
    !! 53 bits scaled into `[0, 1)`. `i` is `integer(int32)` or `integer(int64)`; `seed` and `draw`
    !! are `integer(int64)`. Every one of the 2**64 patterns is possible.
    interface pf_random_bits_at
        module procedure pf_random_bits_at_i32
        module procedure pf_random_bits_at_i64
    end interface pf_random_bits_at

    !> A uniform integer in `[lo, hi]`, exactly unbiased: value `draw` (default 1) of stream `i`.
    !!
    !! `i`, `lo` and `hi` share one kind -- `integer(int32)` or `integer(int64)` -- and the result
    !! follows it; `seed` and `draw` are always `integer(int64)`. Swaps internally when `lo > hi`,
    !! so the function is total. Every width is exact, including the widest: no modulo bias, at any
    !! range, on either side of the fork.
    interface pf_random_int_at
        module procedure pf_random_int_at_i32
        module procedure pf_random_int_at_i64
    end interface pf_random_int_at

    !> Two uniforms of one enciphering, and an integer drawn from the bits they leave over.
    !!
    !! `call pf_random_pair_spare_at(seed, i, lo, hi, u1, u2, j, ok [, draw])`. One block carries
    !! 128 bits; `u1` and `u2` are its two halves converted exactly as `pf_random_at` converts them,
    !! which keeps the top 53 bits of each and **discards 22**. Those 22 are enough to choose from a
    !! list, so a composite draw -- a point somewhere, and which somewhere -- costs ONE enciphering
    !! rather than two.
    !!
    !! * `u1` and `u2` are exactly `pf_random_at(seed, i, 2*draw-1)` and `pf_random_at(seed, i, 2*draw)`,
    !!   whatever `lo`, `hi` and `ok` turn out to be. The integer cannot disturb the pair.
    !! * `ok` is `.true.` when `j` is an exactly uniform integer in `[lo, hi]`. It is `.false.`
    !!   when the 22 bits cannot decide the range exactly -- a width above `2**22`, or Lemire's
    !!   rejection firing, which for a width `w` happens with probability below `w/2**22`. **The
    !!   caller must then draw `j` itself**, by any exact means independent of these bits;
    !!   `pf_random_int_at` at the same coordinates on its own key is the obvious one. A mixture of
    !!   Lemire-on-acceptance and an independent uniform is uniform, so the result stays exact.
    !! * `lo > hi` is swapped rather than refused, as in `pf_random_int_at`. When `ok` is `.false.`
    !!   `j` is `min(lo, hi)`, a defined value rather than a wild one.
    !!
    !! `seed`, `i`, `lo`, `hi` and `draw` are `integer(int64)`; `draw` below 1 clamps to 1. `pure`,
    !! and a pure function of its coordinates like every other `_at` form.
    interface pf_random_pair_spare_at
        module procedure pf_random_pair_spare_at_i64
    end interface pf_random_pair_spare_at

    !> Fills `v` with consecutive values of one stream, starting at `draw` (default 1).
    !!
    !! `v` is a rank-1 `real(real64)` or `real(real32)` array, `intent(out)`; `i` is
    !! `integer(int32)` or `integer(int64)`; `seed` and `draw` are `integer(int64)`. The values are
    !! exactly what the matching scalar draws would give at those positions, so a prefix is a
    !! prefix: `v(1:3)` filled alone equals the first three of `v(1:6)`. A zero-sized `v` is a
    !! defined no-op.
    !!
    !! **Precondition on the draw axis: `draw + size(v) - 1` must not exceed `huge(int64)`.** The
    !! last element's position has to be representable, because there is no value at a position
    !! that cannot be named -- a fill that runs past the end is asking for draws that do not exist,
    !! and it silently receives wrapped ones. Everything up to and including the boundary is exact:
    !! a fill whose final position is `huge(int64)` itself is correct, and is tested. The scalar
    !! entry points have no such limit, since every representable `draw` is a valid one.
    !!
    !! **An INTEGER `v` takes two further required arguments, `lo` and `hi`**, which share `v`'s kind
    !! -- `call pf_random_fill_draws(seed, i, v, lo, hi [, draw])`. Element `k` is then exactly
    !! `pf_random_int_at(seed, i, lo, hi, draw+k-1)`, the same identity the real forms have with
    !! `pf_random_at`. The specifics are distinguishable on `v`'s type alone, so the generic resolves
    !! without ambiguity, and `lo > hi` is swapped rather than refused, exactly as in the scalar draw.
    !!
    !! **The integer form amortises much as `real64` does**: an integer draw has stride 2, so
    !! consecutive draws pair up two to a block and the fill enciphers once for each pair. Measured
    !! on machine B (gfortran 15.2.1, `-O3 -funroll-loops`, 4M values, best of three alternating
    !! rounds against the committed `/v1` build, with the `real64` fill flat at 8.66-8.69 ns as a
    !! cross-build control): **25.21-25.32 ns per value before, 15.74-15.76 after, 1.61x**. It was
    !! not always so -- see `pf_random_int_at`'s own note on the stride change that made it possible.
    interface pf_random_fill_draws
        module procedure pf_random_fill_draws_r64_i32
        module procedure pf_random_fill_draws_r64_i64
        module procedure pf_random_fill_draws_r32_i32
        module procedure pf_random_fill_draws_r32_i64
        module procedure pf_random_fill_draws_i32_i32
        module procedure pf_random_fill_draws_i32_i64
        module procedure pf_random_fill_draws_i64_i32
        module procedure pf_random_fill_draws_i64_i64
    end interface pf_random_fill_draws

    !> Fills `v` with ONE draw of each of `size(v)` consecutive streams, starting at stream `i0`.
    !!
    !! The other axis. Where `pf_random_fill_draws` fixes the stream and walks the draws, this fixes
    !! the draw and walks the streams -- so it is the bulk form of the loop this module's own
    !! documentation opens with, `x(i) = pf_random_at(seed, i)`. Element `k` is exactly
    !! `pf_random_at(seed, i0 + k - 1 [, draw])`, so the two forms are interchangeable and a prefix
    !! is a prefix.
    !!
    !! `v` is a rank-1 `real(real64)` or `real(real32)` array, `intent(out)`; `i0` is
    !! `integer(int32)` or `integer(int64)`; `seed` and `draw` are `integer(int64)`, and `draw`
    !! (default 1) is which draw of every one of those streams to take. A zero-sized `v` is a
    !! defined no-op.
    !!
    !! **Precondition, on the STREAM axis this time: `i0 + size(v) - 1` must not exceed
    !! `huge(int64)`.** Same reasoning as the draw-axis fill's own precondition -- the last element's
    !! stream has to be nameable. Note `i0` may be negative, and usually the whole range is nowhere
    !! near the boundary.
    !!
    !! **It cannot be as cheap per value as `pf_random_fill_draws`, and that is the contract rather
    !! than the implementation.** Each stream needs its own block, and one `real64` value consumes
    !! two of that block's four words -- the other two belong to draw 2 of the same stream, which
    !! this call is not asking for. A `real32` value consumes one of four. So where the draw-axis
    !! fill amortises one enciphering over two (or four) values, this one enciphers per value and
    !! wins only by removing the per-element call. Measured on machine B against the scalar loop it
    !! replaces: **1.28x on gfortran and 1.48x on ifx** for `real64`, **1.47x and 1.60x** for
    !! `real32`. When several values per stream
    !! are wanted, `pf_random_fill_draws` remains much the cheaper shape.
    !!
    !! **An INTEGER `v` takes two further required arguments, `lo` and `hi`**, which share `v`'s kind
    !! -- `call pf_random_fill_streams(seed, i0, v, lo, hi [, draw])`. Element `k` is exactly
    !! `pf_random_int_at(seed, i0+k-1, lo, hi [, draw])`. This axis was already one enciphering per
    !! value for the real kinds, so unlike the draw-axis integer form there is nothing given up here
    !! at all: it is the same work with the per-element call removed.
    interface pf_random_fill_streams
        module procedure pf_random_fill_streams_r64_i32
        module procedure pf_random_fill_streams_r64_i64
        module procedure pf_random_fill_streams_r32_i32
        module procedure pf_random_fill_streams_r32_i64
        module procedure pf_random_fill_streams_i32_i32
        module procedure pf_random_fill_streams_i32_i64
        module procedure pf_random_fill_streams_i64_i32
        module procedure pf_random_fill_streams_i64_i64
    end interface pf_random_fill_streams

    ! ================================================================================
    ! Tier 0 -- distributions
    ! ================================================================================
    !
    ! Every distribution here is addressed exactly as the uniforms are, by `(seed, i [, draw])`,
    ! and every one has its own frozen contract identifier. What differs between them is whether
    ! the tier-1 stream walk gives the SAME values at the same positions:
    !
    !   distribution   consumption   tier 0 == tier 1?
    !   exponential    fixed, 2 w    YES -- one draw of `pf_random_at` transformed
    !   normal         variable      no  -- see `pf_random_normal_at`
    !
    ! A rejection algorithm consumes a number of words that depends on the values it drew, so
    ! nobody -- not the library, not the caller -- can say where value `k` of a stream walk begins
    ! without having drawn the preceding `k-1`. The coordinate-addressed forms sidestep that by
    ! giving each value its own derived sub-stream; the stream walk cannot, because its whole
    ! purpose is to be a walk. The exponential is the one distribution where the question does not
    ! arise.

    !> One `Exp(1)` draw: value `draw` (default 1) of stream `i` under `seed`.
    !!
    !! `i` is `integer(int32)` or `integer(int64)`; `seed` and `draw` are `integer(int64)`. The
    !! result is in `[0, 36.7368]` -- `-log` of the smallest uniform this generator can produce.
    !!
    !! The mapping is `-log(1 - u)` where `u` is exactly `pf_random_at(seed, i, draw)`, so the
    !! draw is **bit-identical for a given libm**: the logarithm is the intrinsic one, and libm
    !! is not part of any contract this project controls. `pf_random_exp_portable_at` is the same
    !! value computed through a frozen logarithm, identical on every platform -- and about 3x the
    !! cost, which is why this is the default rather than that one.
    !!
    !! **`1 - u`, not `u`, and that is contract.** `pf_random_at` can return exactly 0 and can
    !! never return 1, so `-log(u)` would be infinite once in 2**53 draws while `-log(1 - u)` is
    !! finite always. The cost is that the draw can be exactly 0, which is correct and harmless.
    !!
    !! **Two words, the same two `pf_random_at` reads at that coordinate**, so tier 0, tier 1 and
    !! the bulk fill are the same value computed the same way -- which the normal's forms
    !! deliberately are not, its consumption being variable (`pf_random_normal_at` says why).
    !!
    !! **"The same way" is not quite "bit for bit", and the reason is the libm promise showing its
    !! teeth inside one program.** A compiler may serve `log` from a VECTOR libm inside the bulk
    !! fill's loop and a scalar one in this elemental call, and the two variants need not agree in
    !! the last bit. Measured on machine B: gfortran gives identical values on all three tiers,
    !! while ifx at its default `-fp-model=fast` differs on 21 of 64 by at most **2 ulp**.
    !! `pf_random_exp_portable_at` has no such exposure -- its logarithm is this library's own and
    !! there is no vector variant to substitute -- and its three tiers agree exactly everywhere.
    !!
    !! **What is exact on every tier and every compiler is CHUNK INVARIANCE**, which is the
    !! property that matters: a fill split at any boundary, in any order, on any number of threads
    !! gives the same values as one whole fill. That is asserted directly, and it holds because a
    !! given tier uses one code path for every element regardless of how many there are.
    interface pf_random_exp_at
        module procedure pf_random_exp_at_i32
        module procedure pf_random_exp_at_i64
    end interface pf_random_exp_at

    !> `pf_random_exp_at`'s value computed through a frozen logarithm: **identical on every
    !! platform, compiler and flag set**, not merely for a given libm.
    !!
    !! Same arguments, same range, same two words, same `1 - u` convention. The only difference is
    !! which logarithm: `parquet_expkey`'s transform, built from IEEE `+ - * /` with rounding
    !! barriers no compiler may reorder, rather than libm's. The two agree to about 2 ulp, which is
    !! eleven orders of magnitude below the Monte Carlo error of anything that could measure the
    !! difference -- so this is a choice about **reproducibility**, never about accuracy.
    !!
    !! **It costs about 3x**, measured on machine B (gfortran 15.2.1, `-O3 -funroll-loops`): the
    !! bulk fill 39.4 ns per value against 11.2, the scalar draw 81.3 against 27.0. The frozen
    !! transform is twelve barriered Horner steps, each a store and a reload, so it neither
    !! vectorises nor pipelines while a libm `log` does both. Reach for it when a stored result
    !! must reproduce across machines; take the default otherwise.
    !!
    !! **Not `pure`**, because the frozen transform's rounding barrier is a `volatile` local and a
    !! pure procedure may not have one. Elemental use still works, but a caller's own `pure`
    !! procedure and a `do concurrent` body cannot reach it -- `pf_random_exp_at` can, and
    !! `pf_random_fill_exp_portable` works outside the construct.
    interface pf_random_exp_portable_at
        module procedure pf_random_exp_portable_at_i32
        module procedure pf_random_exp_portable_at_i64
    end interface pf_random_exp_portable_at

    !> Fills `v` with consecutive `Exp(1)` draws of one stream, starting at `draw` (default 1).
    !!
    !! `v` is a rank-1 `real(real64)` array, `intent(out)`; `i` is `integer(int32)` or
    !! `integer(int64)`; `seed` and `draw` are `integer(int64)`. Element `k` is exactly
    !! `pf_random_exp_at(seed, i, draw+k-1)` -- to within the libm caveat that entry describes,
    !! and **exactly** as far as chunking is concerned: a prefix is a prefix and a fill split at
    !! any boundary agrees with a whole one, on every compiler. A zero-sized `v` is a defined
    !! no-op.
    !!
    !! **Precondition on the draw axis: `draw + size(v) - 1` must not exceed `huge(int64)`** --
    !! the same bound, for the same reason, as `pf_random_fill_draws`.
    !!
    !! **No `threads=`, deliberately.** Every element is a pure function of its coordinates, so the
    !! caller wraps their own `!$omp parallel do` around any chunking they like and gets the same
    !! answer at any thread count. An internal thread count would be less flexible and no faster.
    interface pf_random_fill_exp
        module procedure pf_random_fill_exp_i32
        module procedure pf_random_fill_exp_i64
    end interface pf_random_fill_exp

    !> `pf_random_fill_exp` through the frozen logarithm; element `k` is exactly
    !! `pf_random_exp_portable_at(seed, i, draw+k-1)`.
    !!
    !! Same arguments and same preconditions as `pf_random_fill_exp`, and the same reasons for
    !! taking no `threads=`. Not `pure`, for the reason `pf_random_exp_portable_at` gives.
    interface pf_random_fill_exp_portable
        module procedure pf_random_fill_exp_portable_i32
        module procedure pf_random_fill_exp_portable_i64
    end interface pf_random_fill_exp_portable

    !> One standard normal draw: value `draw` (default 1) of stream `i` under `seed`.
    !!
    !! `i` is `integer(int32)` or `integer(int64)`; `seed` and `draw` are `integer(int64)`. Mean 0,
    !! variance 1, and the whole real line is reachable up to what the tail algorithm can produce.
    !!
    !! **Ziggurat over 256 equal-area layers** (`parquet_ziggurat`), which accepts 98.5 % of draws
    !! after one 64-bit read and falls back to a wedge test or a tail walk otherwise. Bit-identical
    !! **for a given libm**: the wedge test needs `exp` and the tail needs `log`.
    !! `pf_random_normal_portable_at` is the same distribution through a frozen logarithm, identical
    !! on every platform.
    !!
    !! **This does NOT equal `%normal` at the same coordinate, and that is deliberate.** A rejection
    !! algorithm consumes a number of words that depends on the values it drew, so no caller can
    !! say where value `k` of a stream walk begins without having drawn the preceding `k-1` -- which
    !! would make a chunked or threaded fill impossible. The coordinate-addressed forms sidestep
    !! that by giving each `(i, draw)` its own derived sub-stream, so this value is a pure function
    !! of its coordinates and a bulk fill splits anywhere. The stream walk cannot do that, because
    !! being a walk is its purpose. See `pf_normal_algorithm`, and `pf_random_exp_at` for the one
    !! distribution where the two DO agree.
    !!
    !! **Independent of `pf_random_normal_portable_at` at the same coordinate**, by construction:
    !! the two derive their sub-streams through different labels, so they are two normals rather
    !! than two functions of one uniform. Same for either one against its own stream walk.
    interface pf_random_normal_at
        module procedure pf_random_normal_at_i32
        module procedure pf_random_normal_at_i64
    end interface pf_random_normal_at

    !> A standard normal that is **identical on every platform, compiler and flag set**, not merely
    !! for a given libm.
    !!
    !! Same arguments and same distribution as `pf_random_normal_at`; a different algorithm, and so
    !! a different value at the same coordinate. **Marsaglia's polar method**: draw a point in the
    !! square, reject it unless it lands in the unit disc, and map the survivor through
    !! `sqrt(-2*log(s)/s)`. Every step is IEEE `+ - * /` and `sqrt` -- both correctly rounded by the
    !! standard -- over `parquet_expkey`'s frozen logarithm, with rounding barriers on the two
    !! squares and on the argument of the `sqrt` so that no compiler may fuse or regroup them.
    !!
    !! **It costs more than the Ziggurat, in two ways.** It rejects 21.5 % of candidate pairs
    !! rather than 1.5 % of single draws, it discards the second variate the method produces (a
    !! stream that kept it would have hidden state, so `%rewind` would stop being exact), and its
    !! logarithm is about 3x libm's. Reach for it when a stored result must reproduce across
    !! machines; take `pf_random_normal_at` otherwise.
    !!
    !! **Not `pure`**, because the frozen transform's rounding barrier is a `volatile` local and a
    !! pure procedure may not have one. Elemental use still works, but a caller's own `pure`
    !! procedure and a `do concurrent` body cannot reach it -- `pf_random_normal_at` can.
    interface pf_random_normal_portable_at
        module procedure pf_random_normal_portable_at_i32
        module procedure pf_random_normal_portable_at_i64
    end interface pf_random_normal_portable_at

    !> Fills `v` with consecutive standard normal draws of one stream, starting at `draw`.
    !!
    !! `v` is a rank-1 `real(real64)` array, `intent(out)`; `i` is `integer(int32)` or
    !! `integer(int64)`; `seed` and `draw` are `integer(int64)`. Element `k` is exactly
    !! `pf_random_normal_at(seed, i, draw+k-1)`, so a prefix is a prefix and a chunked fill agrees
    !! with a whole one. A zero-sized `v` is a defined no-op.
    !!
    !! **This is why the coordinate-addressed realisation exists.** Each element runs its own
    !! rejection loop in its own sub-stream, so the fill splits at any boundary and gives the same
    !! answer at any thread count -- which a stream walk cannot, at any speed. It takes no
    !! `threads=` for exactly that reason: the caller wraps `!$omp parallel do` around whatever
    !! chunking they like.
    !!
    !! **It does not agree with a loop of `%normal`.** See `pf_random_normal_at`.
    interface pf_random_fill_normal
        module procedure pf_random_fill_normal_i32
        module procedure pf_random_fill_normal_i64
    end interface pf_random_fill_normal

    !> `pf_random_fill_normal` through the polar method and the frozen logarithm; element `k` is
    !! exactly `pf_random_normal_portable_at(seed, i, draw+k-1)`.
    !!
    !! Same arguments and same preconditions, and the same reasons for taking no `threads=`. Not
    !! `pure`, for the reason `pf_random_normal_portable_at` gives.
    interface pf_random_fill_normal_portable
        module procedure pf_random_fill_normal_portable_i32
        module procedure pf_random_fill_normal_portable_i64
    end interface pf_random_fill_normal_portable

    ! ================================================================================
    ! Tier 0 -- points on a sphere
    ! ================================================================================
    !
    ! Five families, each addressed by `(seed, i, [draw])` like every draw in this module, each with
    ! a vector form in radians and -- where a sky position means something -- an RA/Dec form in
    ! degrees. What they share, and what is contract for all of them (`pf_sphere_algorithm`):
    !
    !   * **One block per draw.** A family reads the two halves of block `draw - 1` of its own
    !     derived sub-stream `(pf_random_key(seed, label), i)`, plus one further uniform from a
    !     second label for the ball and the rotation. Nothing rejects, so the cost is fixed and the
    !     stream walk reaches the same value at the same draw.
    !   * **Independence.** Each family has its own label, so two families at one coordinate are
    !     two draws, and neither shares a bit with `pf_random_at` or any other generic there.
    !   * **The two coordinate forms of one family are the SAME point**, not two draws: the RA/Dec
    !     form is the vector form read in the standard frame. A program wanting two points calls one
    !     form at two draws.
    !   * **`draw` is at most `2**62`**, refused beyond it, since block `2**62` would set bit 62 of
    !     the block index and read another word space silently.
    !
    ! **Why the RA/Dec forms take no `frame=`.** `parquet_healpix` refuses a free RA/Dec procedure
    ! because two declination conventions are in live use and mixing them searches the wrong
    ! hemisphere. A sampler whose input and output are BOTH `(ra, dec)` is immune: the mirrored
    ! convention is a reflection in `z`, which maps the direction labelled `(ra, dec)` in one frame to
    ! the one labelled `(ra, dec)` in the other and preserves angles and solid angle, so the cap about
    ! `(ra0, dec0)` and its uniform measure are the same set of labels either way. The RA/Dec forms
    ! compute in the standard frame, `z = sin(dec)`, and the answer is the same under either.

    !> A direction uniformly distributed on the unit sphere: value `draw` (default 1) of stream `i`.
    !!
    !! `v = pf_random_direction_at(seed, i, [draw])`. `seed` and `draw` are `integer(int64)`; `i` is
    !! `integer(int32)` or `integer(int64)`, the two giving identical values; the result is a
    !! `real(real64)` vector of length 3 and length 1. `draw` below 1 clamps to 1 and above `2**62`
    !! aborts. **Archimedes' construction**: `z = 2*u1 - 1` and an azimuth `2*pi*u2` from the two
    !! halves of one block, so `z` is exactly uniform on `[-1, 1)` and `(0, 0, -1)` is reachable
    !! exactly. `pure`, not `elemental` -- a rank-1 result cannot be -- so a loop, or
    !! `pf_random_fill_direction`, draws many.
    interface pf_random_direction_at
        module procedure pf_random_direction_at_i32
        module procedure pf_random_direction_at_i64
    end interface pf_random_direction_at

    !> `pf_random_direction_at`'s point, as a right ascension and declination in degrees.
    !!
    !! `call pf_random_radec_at(seed, i, ra, dec, [draw])`: `ra` in `[0, 360)` and `dec` in
    !! `[-90, 90]`, both `real(real64)` and `intent(out)`; `seed`, `i` and `draw` as for
    !! `pf_random_direction_at`. **The same point, not a second draw**: read in the standard frame,
    !! `dec = atan2(z, hypot(x, y))`, and the right ascension of a pole is 0 by rule. `pure
    !! elemental`, so an index array fills a whole catalogue in one statement, one stream per row.
    interface pf_random_radec_at
        module procedure pf_random_radec_at_i32
        module procedure pf_random_radec_at_i64
    end interface pf_random_radec_at

    !> A direction uniformly distributed within an angular `radius` of `centre`, in radians.
    !!
    !! `v = pf_random_disc_at(seed, i, centre, radius, [draw], [r_inner])`. `centre` is a
    !! `real(real64)` vector of length 3, any nonzero finite length, normalised internally; `radius`
    !! is at least 0 and a value above `pi` is the whole sphere; `r_inner` in `[0, min(radius, pi)]`
    !! (default 0) excludes the directions closer than it, so a ring between two angles costs no
    !! extra name and `r_inner = radius` is the circle itself. The result is a unit vector in the
    !! caller's own frame. `radius = 0` returns the normalised centre exactly. The cosine of the
    !! angle from the centre is exactly uniform over the ring, and the azimuth about the centre is
    !! measured in a right-handed frame fixed by `pf_sphere_algorithm`. **An `r_inner` above `pi` is
    !! refused, not clamped**: clamping would leave a ring of zero width whose every draw is the
    !! antipode exactly. A NaN, negative or infinite radius, an `r_inner` outside `[0, radius]` or
    !! above `pi`, a centre that is zero, NaN or infinite, and a `draw` above `2**62` abort, naming
    !! this procedure. `seed`, `i` and `draw` as for `pf_random_direction_at`.
    interface pf_random_disc_at
        module procedure pf_random_disc_at_i32
        module procedure pf_random_disc_at_i64
    end interface pf_random_disc_at

    !> A sky position uniformly distributed within `radius_deg` of `(ra0, dec0)`, all in degrees.
    !!
    !! `call pf_random_disc_radec_at(seed, i, ra0, dec0, radius_deg, ra, dec, [draw], [r_inner_deg])`.
    !! **The same point as `pf_random_disc_at`** with the centre converted in the standard frame and
    !! both radii converted to radians. `ra0` is any finite value; `dec0` must lie in `[-90, 90]`, and
    !! is refused outside it rather than read as the direction it names, because a cap about a
    !! mirrored position is a plausible wrong answer nothing downstream could see; `dec0 = +/-90` is
    !! the pole exactly, whatever `ra0` says. `radius_deg` above 180 is the whole sky;
    !! `r_inner_deg` in `[0, min(radius_deg, 180)]` (default 0), an inner radius above 180 being
    !! refused rather than clamped. `ra` in `[0, 360)` and `dec` in `[-90, 90]`
    !! are `intent(out)`. `pure elemental`, so an index array draws a whole catalogue about one
    !! centre, or about a centre per row.
    interface pf_random_disc_radec_at
        module procedure pf_random_disc_radec_at_i32
        module procedure pf_random_disc_radec_at_i64
    end interface pf_random_disc_radec_at

    !> A point uniformly distributed inside the ball of `radius` about the origin, or in a shell.
    !!
    !! `p = pf_random_ball_at(seed, i, radius, [draw], [r_inner])`. `radius` is finite and at least
    !! 0; `r_inner` in `[0, radius]` (default 0) makes it the shell between the two. The result is a
    !! `real(real64)` vector of length 3; add a centre at the call site. A direction from the
    !! family's first label times `radius * (q**3 + u*(1 - q**3))**(1/3)` with `q = r_inner/radius`
    !! and `u` from its second label -- written on the ratio, so no `radius**3` can overflow.
    !! `radius = 0` is the origin. Refusals as for `pf_random_disc_at`.
    interface pf_random_ball_at
        module procedure pf_random_ball_at_i32
        module procedure pf_random_ball_at_i64
    end interface pf_random_ball_at

    !> A von Mises-Fisher direction about `mu` with concentration `kappa`: a Gaussian-like scatter.
    !!
    !! `v = pf_random_vmf_at(seed, i, mu, kappa, [draw])`. `mu` is any nonzero finite vector of length
    !! 3; `kappa` is finite and at least 0, where 0 is the uniform direction and a large `kappa` a
    !! tight scatter of per-axis width about `1/sqrt(kappa)` radians. The inverse of the distribution
    !! of the cosine, `w = 1 + log(u' + (1 - u')*exp(-2*kappa))/kappa` with `u' = 1 - u`, evaluated
    !! so that nothing cancels at any `kappa`: a tiny `kappa` still gives the uniform limit to the last
    !! ulp and a huge one the centre. Refusals as for `pf_random_disc_at`, with `kappa` in place of the
    !! radii.
    interface pf_random_vmf_at
        module procedure pf_random_vmf_at_i32
        module procedure pf_random_vmf_at_i64
    end interface pf_random_vmf_at

    !> A von Mises-Fisher sky position about `(ra0, dec0)` with a Gaussian width `sigma_deg`.
    !!
    !! `call pf_random_vmf_radec_at(seed, i, ra0, dec0, sigma_deg, ra, dec, [draw])`. **The same point
    !! as `pf_random_vmf_at` with `kappa = 1/sigma**2`, `sigma` in radians** -- the concentration whose
    !! small-angle limit is an isotropic Gaussian with that per-axis width, the number an error
    !! ellipse is quoted in. `sigma_deg` must be finite and strictly positive; the centre and the
    !! outputs as for `pf_random_disc_radec_at`.
    interface pf_random_vmf_radec_at
        module procedure pf_random_vmf_radec_at_i32
        module procedure pf_random_vmf_radec_at_i64
    end interface pf_random_vmf_radec_at

    !> A rotation matrix drawn uniformly from all rotations (the Haar measure).
    !!
    !! `r = pf_random_rotation_at(seed, i, [draw])`: a `real(real64)` array shaped `(3, 3)`, proper
    !! (determinant +1) and orthonormal to rounding. Applied as `matmul(r, v)`, so column `k` is the
    !! image of the `k`-th axis. Shoemake's construction: a uniform unit quaternion from three
    !! uniforms, two from the family's first label and one from its second, expanded into a matrix.
    interface pf_random_rotation_at
        module procedure pf_random_rotation_at_i32
        module procedure pf_random_rotation_at_i64
    end interface pf_random_rotation_at

    !> Fills the columns of `v` with consecutive directions of one stream, starting at `draw`.
    !!
    !! `call pf_random_fill_direction(seed, i, v, [draw])`: `v` is `real(real64)`, shaped `(3, n)`,
    !! `intent(out)`, and column `k` is `pf_random_direction_at(seed, i, draw+k-1)` to a couple of ulp
    !! -- a compiler may serve the sine and cosine from a vector libm in this loop and a scalar one
    !! in the scalar call -- and exactly as far as chunking goes: a fill split anywhere agrees with a
    !! whole one. `v` must have three rows even when it has no columns, and a zero-column fill is a
    !! no-op. `draw + n - 1` must be at most `2**62`. No `threads=`, for the reason
    !! `pf_random_fill_exp` gives. It amortises nothing -- a direction already uses a whole block --
    !! and exists for the shape a mock catalogue wants.
    interface pf_random_fill_direction
        module procedure pf_random_fill_direction_i32
        module procedure pf_random_fill_direction_i64
    end interface pf_random_fill_direction

    !> Fills `ra` and `dec` with consecutive sky positions of one stream, starting at `draw`.
    !!
    !! `call pf_random_fill_radec(seed, i, ra, dec, [draw])`: `ra` and `dec` are rank-1
    !! `real(real64)` arrays of one size, `intent(out)`, and element `k` is
    !! `pf_random_radec_at(seed, i, ra, dec, draw+k-1)` to a couple of ulp, exactly as far as
    !! chunking goes. Arrays of different sizes abort; the rest as for `pf_random_fill_direction`.
    interface pf_random_fill_radec
        module procedure pf_random_fill_radec_i32
        module procedure pf_random_fill_radec_i64
    end interface pf_random_fill_radec


    !> Derives an independent seed from a seed and a label, so one seed can fan out into families.
    !!
    !! `label` is `integer(int32)` or `integer(int64)`; `seed` and the result are `integer(int64)`.
    !! A derived key IS a seed, so derivations compose by nesting.
    interface pf_random_key
        module procedure pf_random_key_i32
        module procedure pf_random_key_i64
    end interface pf_random_key

    ! ================================================================================
    ! Tier 1 -- the stateful stream
    ! ================================================================================

    !> Highest 0-based word position a stream may hold. A stream addresses `2**63` words, which at
    !! the measured cost of a draw is some thousands of times the age of the universe -- so this
    !! bound exists to keep the position arithmetic provably free of signed overflow, not because a
    !! program will approach it. **That is a correctness requirement, not tidiness**: Risk-94 records
    !! this module being caught with a compiler using one overflowing expression's undefinedness to
    !! delete a branch hundreds of lines away, so a new unguarded overflow site on a hot path would
    !! be a regression against Risk-95's "every remaining wrapping site is `#else`-arm only".
    integer(int64), parameter :: stream_pos_max = huge(1_int64)

    !> A walk along one stream: the same values tier 0 addresses, reached in sequence.
    !!
    !! Tier 0 answers "what is the value at this coordinate?". Some programs cannot ask that,
    !! because how many values they need is data-dependent -- a rejection sampler, a random walk, a
    !! resample of unknown length. This type carries the position so the caller does not have to,
    !! and hands out consecutive values of one stream.
    !!
    !! **It changes no value.** A freshly seeded stream's `k`-th `%uniform` is exactly
    !! `pf_random_at(seed, stream, k)`, its `k`-th `%uniform32` exactly `pf_random32_at(seed,
    !! stream, k)`. The stream is a different way to reach the same grid, never a second generator.
    !!
    !! **Reproducibility is per iteration, and that is the discipline to follow**: seed at the top of
    !! each loop iteration from a run-invariant label, then draw as many values as that iteration
    !! needs.
    !!
    !! ```fortran
    !! type(pf_random_stream) :: rng
    !! !$omp parallel do schedule(dynamic) private(...)
    !! do i = 1, n
    !!     call rng%seed(seed, i)              ! O(1), no warm-up
    !!     do while (...)                      ! however many draws this iteration turns out to need
    !!         call rng%uniform(x)
    !!     end do
    !! end do
    !! ```
    !!
    !! What can never be reproducible is one long-lived stream consumed *across* the iterations of a
    !! dynamically scheduled loop -- the value an iteration receives then depends on how many draws
    !! ran before it, which depends on the schedule. That is a property of every stateful generator,
    !! not a limitation of this one; the answer to it is that `%seed` costs nothing.
    !!
    !! **Position is measured in 32-bit words, 1-based, and the word cost of each producer is
    !! contract** -- `%position` and `%rewind` are denominated in it: `%uniform` 2,
    !! `%uniform32` 1, `%bits` 2, `%int_range` 2, `%exp` 2, `%exp_portable` 2. `%int_range` and the
    !! two `%exp` forms additionally
    !! start on a word PAIR boundary, advancing one word first if a `%uniform32` has left the
    !! cursor odd. This is what keeps them equal to the `pf_random_int_at`/`pf_random_exp_at` at
    !! the same coordinate rather than re-reading a word a previous draw already used. `%int_range`
    !! cost 4 words plus up to 3 of alignment until `pf_random_algorithm` reached `/v2`, when the
    !! integer generic's stride became 2.
    !!
    !! **The points-on-a-sphere producers cost one whole BLOCK, 4 words, block-aligned**:
    !! `%direction`, `%radec`, `%disc`, `%disc_radec`, `%ball`, `%vmf`, `%vmf_radec` and `%rotation`
    !! each advance to the next multiple of four words (up to 3 of alignment) and then past one
    !! block, and the value is the coordinate-addressed one at draw `(position - 1)/4 + 1` of that
    !! block. They read their family's own derived sub-stream at that block's index rather than the
    !! raw words there, so a `%uniform` rewound onto the same block still sees those words fresh.
    !! `%address` returns the seed and stream index `%seed` was given, which is what a caller needs
    !! to compute a coordinate-addressed draw at a stream's own coordinates.
    !!
    !! **A producer whose cost is VARIABLE cannot be predicted, only observed.** Every producer
    !! listed above has a fixed cost, so a caller can compute where the stream will be after a
    !! known sequence of draws. `%normal` and `%normal_portable` cannot: each runs a rejection loop
    !! and consumes a number of words that depends on the values it drew -- 2 words in 98.5 % of
    !! `%normal`'s draws and more otherwise, a multiple of 4 for `%normal_portable`. `%position`
    !! stays exact for those, because it reports where the stream *is*; only predicting it in
    !! advance becomes impossible. Checkpoint and restart (`%position` then `%rewind`) therefore
    !! keep working for every producer, while arithmetic on positions stops.
    !!
    !! **A variable-cost producer's value is NOT the coordinate-addressed one at the same
    !! position**, and that is the price of a bulk fill being splittable at all -- see
    !! `pf_random_normal_at`, which explains why no implementation can have both.
    !!
    !! **The type is plain scalars: no allocatable components, no `FINAL`, deliberately and
    !! permanently.** gfortran does not reliably default-initialise an OpenMP `private()` copy of a
    !! finalizable type, and ifx segfaults on a block-local instance of a type with allocatable
    !! components inside a parallel region (`feature_risks.md` Risk-45). A type that is neither is
    !! safe in both shapes, which is what makes a per-thread instance usable at all. Adding either to
    !! this type would break every parallel use of it, on one compiler or the other.
    !!
    !! It holds the block it last enciphered, keyed by that block's index. One enciphering carries
    !! two `real64` values or four `real32`s, so keeping it is worth **1.70x (gfortran) / 1.78x (ifx)**
    !! on `real64` and **2.88x / 3.35x** on `real32` -- measured, and enough to take the stream from
    !! slower than a tier-0 loop to faster than one. Because the cache is *keyed* rather than
    !! consumed, it reaches nothing in the contract: `%position` still means a word index, and a
    !! stream saved and restored through `%position` alone is exact.
    !!
    !! **`pf_random_fill_draws` is still cheaper again** (1.87x / 1.67x against this type), so bulk
    !! work whose length is known in advance belongs there, not in a loop over a stream.
    !> One disc or ring, prepared once, so a loop over it pays the setup once instead of per draw.
    !!
    !! ```fortran
    !! type(pf_random_disc_cap) :: cap
    !! call cap%prepare([0.0_real64, 0.0_real64, 1.0_real64], 0.05_real64)
    !! do k = 1, n
    !!     v = cap%at(seed, 1_int64, k)          ! == pf_random_disc_at(seed, 1, centre, 0.05, k)
    !! end do
    !! ```
    !!
    !! **`%at` is `pf_random_disc_at` exactly**, to the bit, at the same coordinates: the scalar form
    !! is written as `%prepare` followed by `%at`, so the two cannot drift apart. What `%prepare`
    !! lifts out of the loop is the radius validation, the centre's normalisation, the right-handed
    !! frame and the two sines bounding the ring -- everything the scalar form redoes from unchanged
    !! arguments. A rejection walk over a fixed cap is where that matters, since it pays them per
    !! CANDIDATE.
    !!
    !! `%prepare` validates exactly as `pf_random_disc_at` does, aborting on the same arguments and
    !! naming itself. `%at` on an unprepared cap aborts. All three bindings are `pure`, and the
    !! object is read-only once prepared, so one cap built before a parallel region serves the team.
    type, public :: pf_random_disc_cap
        private
        real(real64) :: c(3) = 0.0_real64           !! the normalised centre
        real(real64) :: e1(3) = 0.0_real64          !! the frame's first vector, perpendicular to `c`
        real(real64) :: e2(3) = 0.0_real64          !! `c x e1`
        real(real64) :: h_in = 0.0_real64           !! `1 - cos(r_inner)`, where the ring starts
        real(real64) :: dh = 0.0_real64             !! the ring's width in `1 - cos`
        logical :: set = .false.                    !! whether `%prepare` has run
    contains
        procedure :: prepare => disc_cap_prepare    !! Validates and prepares the cap; may be re-run.
        procedure, private :: at_i32 => disc_cap_at_i32 !! `%at`, `int32` stream index
        procedure, private :: at_i64 => disc_cap_at_i64 !! `%at`, `int64` stream index
        !> The direction at `(seed, i, draw)`; one block. `i` takes either integer kind, as it does
        !> in every free `_at` form, and the two give identical values.
        generic :: at => at_i32, at_i64
        procedure :: is_set => disc_cap_is_set      !! Whether `%prepare` has run.
    end type pf_random_disc_cap

    type, public :: pf_random_stream
        private
        integer(int64) :: key = 0_int64             !! the stream family's seed
        integer(int64) :: stream = 0_int64          !! which stream of that family
        integer(int64) :: pos = 0_int64             !! 0-based word position; `%position` reports `pos+1`
        integer(int64) :: blk = -1_int64            !! block index held below, or -1 when none is
        integer(int64) :: c0 = 0_int64              !! held word 0
        integer(int64) :: c1 = 0_int64              !! held word 1
        integer(int64) :: c2 = 0_int64              !! held word 2
        integer(int64) :: c3 = 0_int64              !! held word 3
    contains
        procedure, private :: seed_base => stream_seed_base   !! `%seed` with no stream index
        procedure, private :: seed_i32 => stream_seed_i32     !! `%seed` with an `int32` stream index
        procedure, private :: seed_i64 => stream_seed_i64     !! `%seed` with an `int64` stream index
        !> (Re)seeds the stream to position 1. O(1), with no warm-up; `stream` defaults to 0.
        generic :: seed => seed_base, seed_i32, seed_i64
        procedure :: uniform => stream_uniform      !! Next `real64` in `[0, 1)`; costs 2 words.
        procedure :: uniform32 => stream_uniform32  !! Next `real32` in `[0, 1)`; costs 1 word.
        procedure :: bits => stream_bits            !! Next 64 raw bits; costs 2 words.
        procedure, private :: int_range_i32 => stream_int_range_i32  !! `%int_range`, `int32`
        procedure, private :: int_range_i64 => stream_int_range_i64  !! `%int_range`, `int64`
        !> Next integer in `[lo, hi]`, exactly unbiased; costs one word pair, taken pair-aligned.
        generic :: int_range => int_range_i32, int_range_i64
        procedure :: exp => stream_exp              !! Next `Exp(1)`; costs one word pair, pair-aligned.
        procedure :: exp_portable => stream_exp_portable  !! `%exp` through the frozen log; same cost.
        procedure :: normal => stream_normal        !! Next standard normal; VARIABLE cost, pair-aligned.
        procedure :: normal_portable => stream_normal_portable  !! `%normal` frozen; variable cost.
        procedure :: normal_truncated => stream_normal_truncated  !! Next normal restricted to an
                                                    !! interval; VARIABLE cost, pair-aligned.
        procedure :: gamma => stream_gamma          !! Next `Gamma(shape, 1)`; VARIABLE cost.
        procedure, private :: poisson_i32 => stream_poisson_i32 !! `%poisson`, `int32` result
        procedure, private :: poisson_i64 => stream_poisson_i64 !! `%poisson`, `int64` result
        !> Next `Poisson(lambda)` count, into an `int32` or `int64`; VARIABLE cost.
        generic :: poisson => poisson_i32, poisson_i64
        procedure, private :: fill_arr_r64 => stream_fill_r64        !! `%fill`, `real64`
        procedure, private :: fill_arr_r32 => stream_fill_r32        !! `%fill`, `real32`
        procedure, private :: fill_arr_i32 => stream_fill_i32        !! `%fill`, `int32`
        procedure, private :: fill_arr_i64 => stream_fill_i64        !! `%fill`, `int64`
        !> Fills `v` with the next `size(v)` values; an integer `v` also takes `lo` and `hi`.
        generic :: fill => fill_arr_r64, fill_arr_r32, fill_arr_i32, fill_arr_i64
        procedure, private :: rewind_base => stream_rewind_base      !! `%rewind` to position 1
        procedure, private :: rewind_i32 => stream_rewind_i32        !! `%rewind`, `int32`
        procedure, private :: rewind_i64 => stream_rewind_i64        !! `%rewind`, `int64`
        !> Sets the position; with no argument, back to 1. Accepts any value `%position` gave.
        generic :: rewind => rewind_base, rewind_i32, rewind_i64
        procedure :: position => stream_position    !! Current 1-based word position.
        procedure :: address => stream_address      !! The seed and stream index `%seed` was given.
        procedure :: direction => stream_direction  !! Next uniform unit vector; one block, block-aligned.
        procedure :: radec => stream_radec          !! `%direction`'s point as `(ra, dec)`, degrees; one block.
        procedure :: disc => stream_disc            !! Next direction within an angle of a centre; one block.
        procedure :: disc_radec => stream_disc_radec  !! `%disc` about `(ra0, dec0)`, degrees; one block.
        procedure :: ball => stream_ball            !! Next point in a ball or shell; one block.
        procedure :: vmf => stream_vmf              !! Next von Mises-Fisher direction; one block.
        procedure :: vmf_radec => stream_vmf_radec  !! `%vmf` about `(ra0, dec0)` by `sigma_deg`; one block.
        procedure :: rotation => stream_rotation    !! Next uniform rotation matrix; one block.
    end type pf_random_stream






contains

    ! ================================================================================
    ! Tier 0 -- stateless indexed draws
    ! ================================================================================

    !> `pf_random_at` for an `integer(int32)` stream index.
    pure elemental function pf_random_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! a uniform draw in `[0, 1)`
        r = to_real64(bits_of(seed, int(i, int64), draw_or_1(draw), DOM_REAL64))
    end function pf_random_at_i32

    !> `pf_random_at` for an `integer(int64)` stream index.
    pure elemental function pf_random_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid, negatives included
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! a uniform draw in `[0, 1)`
        r = to_real64(bits_of(seed, i, draw_or_1(draw), DOM_REAL64))
    end function pf_random_at_i64

    !> `pf_random32_at` for an `integer(int32)` stream index.
    pure elemental function pf_random32_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real32) :: r                           !! a uniform draw in `[0, 1)`
        r = to_real32(word_of(seed, int(i, int64), draw_or_1(draw) - 1_int64))
    end function pf_random32_at_i32

    !> `pf_random32_at` for an `integer(int64)` stream index.
    pure elemental function pf_random32_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real32) :: r                           !! a uniform draw in `[0, 1)`
        r = to_real32(word_of(seed, i, draw_or_1(draw) - 1_int64))
    end function pf_random32_at_i64

    !> `pf_random_bits_at` for an `integer(int32)` stream index.
    pure elemental function pf_random_bits_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        integer(int64) :: r                         !! 64 raw bits
        r = bits_of(seed, int(i, int64), draw_or_1(draw), DOM_REAL64)
    end function pf_random_bits_at_i32

    !> `pf_random_bits_at` for an `integer(int64)` stream index.
    pure elemental function pf_random_bits_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        integer(int64) :: r                         !! 64 raw bits
        r = bits_of(seed, i, draw_or_1(draw), DOM_REAL64)
    end function pf_random_bits_at_i64

    !> `pf_random_int_at` for `integer(int32)` stream index and bounds.
    !!
    !! The result is inside `[min(lo,hi), max(lo,hi)]` by construction, so narrowing the `int64`
    !! worker's answer back to `int32` is exact and cannot overflow.
    pure elemental function pf_random_int_at_i32(seed, i, lo, hi, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        integer(int32) :: r                         !! a uniform integer in `[min(lo,hi), max(lo,hi)]`
        r = int(int_at_impl(seed, int(i, int64), int(lo, int64), int(hi, int64), draw_or_1(draw)), int32)
    end function pf_random_int_at_i32

    !> `pf_random_int_at` for `integer(int64)` stream index and bounds.
    pure elemental function pf_random_int_at_i64(seed, i, lo, hi, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        integer(int64) :: r                         !! a uniform integer in `[min(lo,hi), max(lo,hi)]`
        r = int_at_impl(seed, i, lo, hi, draw_or_1(draw))
    end function pf_random_int_at_i64

    !> `pf_random_pair_spare_at` for an `integer(int64)` stream index. See the generic.
    pure subroutine pf_random_pair_spare_at_i64(seed, i, lo, hi, u1, u2, j, ok, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        real(real64), intent(out) :: u1             !! the block's first uniform, in `[0, 1)`
        real(real64), intent(out) :: u2             !! the block's second uniform, in `[0, 1)`
        integer(int64), intent(out) :: j            !! a uniform integer in the closed range, when `ok`
        logical, intent(out) :: ok                  !! whether the spare bits decided `j`
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        call pair_spare_draw(seed, i, lo, hi, draw_or_1(draw), u1, u2, j, ok)
    end subroutine pf_random_pair_spare_at_i64

    !> `pf_random_exp_at` for an `integer(int32)` stream index.
    pure elemental function pf_random_exp_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! an `Exp(1)` draw in `[0, 36.7368]`
        r = exp_of(seed, int(i, int64), draw_or_1(draw))
    end function pf_random_exp_at_i32

    !> `pf_random_exp_at` for an `integer(int64)` stream index.
    pure elemental function pf_random_exp_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! an `Exp(1)` draw in `[0, 36.7368]`
        r = exp_of(seed, i, draw_or_1(draw))
    end function pf_random_exp_at_i64

    !> `pf_random_exp_portable_at` for an `integer(int32)` stream index.
    !!
    !! `impure` because `exp_key` is: the frozen transform's rounding barrier is a `volatile`
    !! local, and a pure procedure may not have one. Elemental use is unaffected.
    impure elemental function pf_random_exp_portable_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! an `Exp(1)` draw in `[0, 36.7368]`
        r = exp_portable_of(seed, int(i, int64), draw_or_1(draw))
    end function pf_random_exp_portable_at_i32

    !> `pf_random_exp_portable_at` for an `integer(int64)` stream index.
    impure elemental function pf_random_exp_portable_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! an `Exp(1)` draw in `[0, 36.7368]`
        r = exp_portable_of(seed, i, draw_or_1(draw))
    end function pf_random_exp_portable_at_i64

    !> `pf_random_fill_exp` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_exp_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_exp(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_exp_i32

    !> `pf_random_fill_exp` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_exp_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_exp(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_exp_i64

    !> `pf_random_fill_exp_portable` from an `integer(int32)` stream index.
    subroutine pf_random_fill_exp_portable_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_exp_portable(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_exp_portable_i32

    !> `pf_random_fill_exp_portable` from an `integer(int64)` stream index.
    subroutine pf_random_fill_exp_portable_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_exp_portable(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_exp_portable_i64

    !> `pf_random_normal_at` for an `integer(int32)` stream index.
    pure elemental function pf_random_normal_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! a standard normal draw
        r = normal_at_impl(seed, int(i, int64), draw_or_1(draw))
    end function pf_random_normal_at_i32

    !> `pf_random_normal_at` for an `integer(int64)` stream index.
    pure elemental function pf_random_normal_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! a standard normal draw
        r = normal_at_impl(seed, i, draw_or_1(draw))
    end function pf_random_normal_at_i64

    !> `pf_random_normal_portable_at` for an `integer(int32)` stream index.
    !!
    !! `impure` because `exp_key` is: the frozen transform's rounding barrier is a `volatile`
    !! local, and a pure procedure may not have one. Elemental use is unaffected.
    impure elemental function pf_random_normal_portable_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! a standard normal draw
        r = normal_portable_at_impl(seed, int(i, int64), draw_or_1(draw))
    end function pf_random_normal_portable_at_i32

    !> `pf_random_normal_portable_at` for an `integer(int64)` stream index.
    impure elemental function pf_random_normal_portable_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! a standard normal draw
        r = normal_portable_at_impl(seed, i, draw_or_1(draw))
    end function pf_random_normal_portable_at_i64

    !> `pf_random_fill_normal` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_normal_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_normal(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_normal_i32

    !> `pf_random_fill_normal` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_normal_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_normal(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_normal_i64

    !> `pf_random_fill_normal_portable` from an `integer(int32)` stream index.
    subroutine pf_random_fill_normal_portable_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_normal_portable(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_normal_portable_i32

    !> `pf_random_fill_normal_portable` from an `integer(int64)` stream index.
    subroutine pf_random_fill_normal_portable_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_normal_portable(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_normal_portable_i64

    ! ================================================================================
    ! Tier 0 -- points on a sphere: the specifics
    ! ================================================================================

    !> `pf_random_direction_at` for an `integer(int32)` stream index.
    pure function pf_random_direction_at_i32(seed, i, draw) result(v)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64) :: v(3)                        !! a unit vector uniform on the sphere
        v = direction_draw(seed, int(i, int64), sph_draw("pf_random_direction_at", draw))
    end function pf_random_direction_at_i32

    !> `pf_random_direction_at` for an `integer(int64)` stream index.
    pure function pf_random_direction_at_i64(seed, i, draw) result(v)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64) :: v(3)                        !! a unit vector uniform on the sphere
        v = direction_draw(seed, i, sph_draw("pf_random_direction_at", draw))
    end function pf_random_direction_at_i64

    !> `pf_random_radec_at` for an `integer(int32)` stream index.
    pure elemental subroutine pf_random_radec_at_i32(seed, i, ra, dec, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: ra             !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec            !! declination, degrees, in `[-90, 90]`
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        ! Named rather than written inline at the call: `sph_radec`'s `v(3)` dummy is explicit-shape,
        ! so a function result -- a value with no address of its own -- is argument-associated
        ! through a temporary, which ifx reports as `warning (406)` on every call under
        ! --profile debug. See .claude/rules/fortran-gotchas.md.
        real(real64) :: v(3)
        v = direction_draw(seed, int(i, int64), sph_draw("pf_random_radec_at", draw))
        call sph_radec(v, ra, dec)
    end subroutine pf_random_radec_at_i32

    !> `pf_random_radec_at` for an `integer(int64)` stream index.
    pure elemental subroutine pf_random_radec_at_i64(seed, i, ra, dec, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: ra             !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec            !! declination, degrees, in `[-90, 90]`
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64) :: v(3)                        ! named for `sph_radec`'s explicit-shape dummy, as above
        v = direction_draw(seed, i, sph_draw("pf_random_radec_at", draw))
        call sph_radec(v, ra, dec)
    end subroutine pf_random_radec_at_i64

    !> `pf_random_disc_at` for an `integer(int32)` stream index.
    pure function pf_random_disc_at_i32(seed, i, centre, radius, draw, r_inner) result(v)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(in) :: centre(3)       !! the disc's centre; any nonzero finite length
        real(real64), intent(in) :: radius          !! angular radius, radians, at least 0; above `pi` is the sphere
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64), intent(in), optional :: r_inner !! inner angular radius in `[0, radius]`; absent means 0
        real(real64) :: v(3)                        !! a unit vector uniform in the disc or ring
        v = disc_draw("pf_random_disc_at", seed, int(i, int64), sph_draw("pf_random_disc_at", draw), &
                      centre, radius, r_inner)
    end function pf_random_disc_at_i32

    !> `pf_random_disc_at` for an `integer(int64)` stream index.
    pure function pf_random_disc_at_i64(seed, i, centre, radius, draw, r_inner) result(v)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(in) :: centre(3)       !! the disc's centre; any nonzero finite length
        real(real64), intent(in) :: radius          !! angular radius, radians, at least 0; above `pi` is the sphere
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64), intent(in), optional :: r_inner !! inner angular radius in `[0, radius]`; absent means 0
        real(real64) :: v(3)                        !! a unit vector uniform in the disc or ring
        v = disc_draw("pf_random_disc_at", seed, i, sph_draw("pf_random_disc_at", draw), &
                      centre, radius, r_inner)
    end function pf_random_disc_at_i64

    !> `pf_random_disc_radec_at` for an `integer(int32)` stream index.
    pure elemental subroutine pf_random_disc_radec_at_i32(seed, i, ra0, dec0, radius_deg, ra, dec, draw, r_inner_deg)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(in) :: ra0             !! right ascension of the centre, degrees; any finite value
        real(real64), intent(in) :: dec0            !! declination of the centre, degrees, in `[-90, 90]`
        real(real64), intent(in) :: radius_deg      !! angular radius, degrees, at least 0; above 180 is the sky
        real(real64), intent(out) :: ra             !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec            !! declination, degrees, in `[-90, 90]`
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64), intent(in), optional :: r_inner_deg !! inner radius, degrees, in `[0, radius_deg]`
        call disc_radec_draw("pf_random_disc_radec_at", seed, int(i, int64), sph_draw("pf_random_disc_radec_at", draw), &
                             ra0, dec0, radius_deg, ra, dec, r_inner_deg)
    end subroutine pf_random_disc_radec_at_i32

    !> `pf_random_disc_radec_at` for an `integer(int64)` stream index.
    pure elemental subroutine pf_random_disc_radec_at_i64(seed, i, ra0, dec0, radius_deg, ra, dec, draw, r_inner_deg)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(in) :: ra0             !! right ascension of the centre, degrees; any finite value
        real(real64), intent(in) :: dec0            !! declination of the centre, degrees, in `[-90, 90]`
        real(real64), intent(in) :: radius_deg      !! angular radius, degrees, at least 0; above 180 is the sky
        real(real64), intent(out) :: ra             !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec            !! declination, degrees, in `[-90, 90]`
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64), intent(in), optional :: r_inner_deg !! inner radius, degrees, in `[0, radius_deg]`
        call disc_radec_draw("pf_random_disc_radec_at", seed, i, sph_draw("pf_random_disc_radec_at", draw), &
                             ra0, dec0, radius_deg, ra, dec, r_inner_deg)
    end subroutine pf_random_disc_radec_at_i64

    !> `pf_random_ball_at` for an `integer(int32)` stream index.
    pure function pf_random_ball_at_i32(seed, i, radius, draw, r_inner) result(p)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(in) :: radius          !! the ball's radius; finite, at least 0
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64), intent(in), optional :: r_inner !! the shell's inner radius in `[0, radius]`; absent means 0
        real(real64) :: p(3)                        !! a point uniform in the ball or shell about the origin
        p = ball_draw("pf_random_ball_at", seed, int(i, int64), sph_draw("pf_random_ball_at", draw), radius, r_inner)
    end function pf_random_ball_at_i32

    !> `pf_random_ball_at` for an `integer(int64)` stream index.
    pure function pf_random_ball_at_i64(seed, i, radius, draw, r_inner) result(p)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(in) :: radius          !! the ball's radius; finite, at least 0
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64), intent(in), optional :: r_inner !! the shell's inner radius in `[0, radius]`; absent means 0
        real(real64) :: p(3)                        !! a point uniform in the ball or shell about the origin
        p = ball_draw("pf_random_ball_at", seed, i, sph_draw("pf_random_ball_at", draw), radius, r_inner)
    end function pf_random_ball_at_i64

    !> `pf_random_vmf_at` for an `integer(int32)` stream index.
    pure function pf_random_vmf_at_i32(seed, i, mu, kappa, draw) result(v)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(in) :: mu(3)           !! the mean direction; any nonzero finite length
        real(real64), intent(in) :: kappa           !! the concentration; finite, at least 0
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64) :: v(3)                        !! a unit vector from the von Mises-Fisher distribution
        v = vmf_draw("pf_random_vmf_at", seed, int(i, int64), sph_draw("pf_random_vmf_at", draw), mu, kappa)
    end function pf_random_vmf_at_i32

    !> `pf_random_vmf_at` for an `integer(int64)` stream index.
    pure function pf_random_vmf_at_i64(seed, i, mu, kappa, draw) result(v)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(in) :: mu(3)           !! the mean direction; any nonzero finite length
        real(real64), intent(in) :: kappa           !! the concentration; finite, at least 0
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64) :: v(3)                        !! a unit vector from the von Mises-Fisher distribution
        v = vmf_draw("pf_random_vmf_at", seed, i, sph_draw("pf_random_vmf_at", draw), mu, kappa)
    end function pf_random_vmf_at_i64

    !> `pf_random_vmf_radec_at` for an `integer(int32)` stream index.
    pure elemental subroutine pf_random_vmf_radec_at_i32(seed, i, ra0, dec0, sigma_deg, ra, dec, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(in) :: ra0             !! right ascension of the centre, degrees; any finite value
        real(real64), intent(in) :: dec0            !! declination of the centre, degrees, in `[-90, 90]`
        real(real64), intent(in) :: sigma_deg       !! per-axis Gaussian width, degrees; finite, above 0
        real(real64), intent(out) :: ra             !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec            !! declination, degrees, in `[-90, 90]`
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        call vmf_radec_draw("pf_random_vmf_radec_at", seed, int(i, int64), sph_draw("pf_random_vmf_radec_at", draw), &
                            ra0, dec0, sigma_deg, ra, dec)
    end subroutine pf_random_vmf_radec_at_i32

    !> `pf_random_vmf_radec_at` for an `integer(int64)` stream index.
    pure elemental subroutine pf_random_vmf_radec_at_i64(seed, i, ra0, dec0, sigma_deg, ra, dec, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(in) :: ra0             !! right ascension of the centre, degrees; any finite value
        real(real64), intent(in) :: dec0            !! declination of the centre, degrees, in `[-90, 90]`
        real(real64), intent(in) :: sigma_deg       !! per-axis Gaussian width, degrees; finite, above 0
        real(real64), intent(out) :: ra             !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec            !! declination, degrees, in `[-90, 90]`
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        call vmf_radec_draw("pf_random_vmf_radec_at", seed, i, sph_draw("pf_random_vmf_radec_at", draw), &
                            ra0, dec0, sigma_deg, ra, dec)
    end subroutine pf_random_vmf_radec_at_i64

    !> `pf_random_rotation_at` for an `integer(int32)` stream index.
    pure function pf_random_rotation_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64) :: r(3, 3)                     !! a proper rotation matrix, uniform over all rotations
        r = rotation_draw(seed, int(i, int64), sph_draw("pf_random_rotation_at", draw))
    end function pf_random_rotation_at_i32

    !> `pf_random_rotation_at` for an `integer(int64)` stream index.
    pure function pf_random_rotation_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1, at most `2**62`
        real(real64) :: r(3, 3)                     !! a proper rotation matrix, uniform over all rotations
        r = rotation_draw(seed, i, sph_draw("pf_random_rotation_at", draw))
    end function pf_random_rotation_at_i64

    !> `pf_random_fill_direction` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_direction_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:, :)        !! shaped `(3, n)`; column `k` is draw `draw+k-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_direction(seed, int(i, int64), v, draw)
    end subroutine pf_random_fill_direction_i32

    !> `pf_random_fill_direction` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_direction_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: v(:, :)        !! shaped `(3, n)`; column `k` is draw `draw+k-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_direction(seed, i, v, draw)
    end subroutine pf_random_fill_direction_i64

    !> `pf_random_fill_radec` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_radec_i32(seed, i, ra, dec, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: ra(:)          !! right ascensions, degrees; element `k` is draw `draw+k-1`
        real(real64), intent(out) :: dec(:)         !! declinations, degrees; the same size as `ra`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_radec(seed, int(i, int64), ra, dec, draw)
    end subroutine pf_random_fill_radec_i32

    !> `pf_random_fill_radec` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_radec_i64(seed, i, ra, dec, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: ra(:)          !! right ascensions, degrees; element `k` is draw `draw+k-1`
        real(real64), intent(out) :: dec(:)         !! declinations, degrees; the same size as `ra`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_radec(seed, i, ra, dec, draw)
    end subroutine pf_random_fill_radec_i64

    !> `pf_random_fill_draws` filling `real64` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_draws_r64_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r64(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_draws_r64_i32

    !> `pf_random_fill_draws` filling `real64` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_draws_r64_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r64(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_draws_r64_i64

    !> `pf_random_fill_draws` filling `real32` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_draws_r32_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real32), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r32(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_draws_r32_i32

    !> `pf_random_fill_draws` filling `real32` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_draws_r32_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real32), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r32(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_draws_r32_i64

    !> `pf_random_fill_streams` filling `real64` from an `integer(int32)` first stream index.
    pure subroutine pf_random_fill_streams_r64_i32(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i0            !! first stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_r64(seed, int(i0, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_streams_r64_i32

    !> `pf_random_fill_streams` filling `real64` from an `integer(int64)` first stream index.
    pure subroutine pf_random_fill_streams_r64_i64(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index; every value is valid
        real(real64), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_r64(seed, i0, v, draw_or_1(draw))
    end subroutine pf_random_fill_streams_r64_i64

    !> `pf_random_fill_streams` filling `real32` from an `integer(int32)` first stream index.
    pure subroutine pf_random_fill_streams_r32_i32(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i0            !! first stream index; sign-extends, so any value is valid
        real(real32), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_r32(seed, int(i0, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_streams_r32_i32

    !> `pf_random_fill_streams` filling `real32` from an `integer(int64)` first stream index.
    pure subroutine pf_random_fill_streams_r32_i64(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index; every value is valid
        real(real32), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_r32(seed, i0, v, draw_or_1(draw))
    end subroutine pf_random_fill_streams_r32_i64

    ! The eight integer specifics. The two-token suffix reads value-kind first and stream-index-kind
    ! second, exactly as `_r64_i32` does -- so `_i32_i64` fills an `integer(int32)` array from an
    ! `integer(int64)` stream index.

    !> `pf_random_fill_draws` filling `integer(int32)` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_draws_i32_i32(seed, i, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int32), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_draws_i32(seed, int(i, int64), v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_draws_i32_i32

    !> `pf_random_fill_draws` filling `integer(int32)` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_draws_i32_i64(seed, i, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int32), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_draws_i32(seed, i, v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_draws_i32_i64

    !> `pf_random_fill_draws` filling `integer(int64)` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_draws_i64_i32(seed, i, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_draws_i64(seed, int(i, int64), v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_draws_i64_i32

    !> `pf_random_fill_draws` filling `integer(int64)` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_draws_i64_i64(seed, i, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_draws_i64(seed, i, v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_draws_i64_i64

    !> `pf_random_fill_streams` filling `integer(int32)` from an `integer(int32)` first stream index.
    pure subroutine pf_random_fill_streams_i32_i32(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i0            !! first stream index; sign-extends, so any value is valid
        integer(int32), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_i32(seed, int(i0, int64), v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_streams_i32_i32

    !> `pf_random_fill_streams` filling `integer(int32)` from an `integer(int64)` first stream index.
    pure subroutine pf_random_fill_streams_i32_i64(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index; every value is valid
        integer(int32), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_i32(seed, i0, v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_streams_i32_i64

    !> `pf_random_fill_streams` filling `integer(int64)` from an `integer(int32)` first stream index.
    pure subroutine pf_random_fill_streams_i64_i32(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i0            !! first stream index; sign-extends, so any value is valid
        integer(int64), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_i64(seed, int(i0, int64), v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_streams_i64_i32

    !> `pf_random_fill_streams` filling `integer(int64)` from an `integer(int64)` first stream index.
    pure subroutine pf_random_fill_streams_i64_i64(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index; every value is valid
        integer(int64), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_i64(seed, i0, v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_streams_i64_i64

    ! ================================================================================
    ! Seeding and key derivation
    ! ================================================================================

    !> A fresh, nondeterministic seed, in `[1, huge(int64)]`.
    !!
    !! Folds the clock at its finest resolution together with a process-wide call counter, so two
    !! calls differ even when the clock has not ticked between them, and so do two calls racing on
    !! different threads -- the counter is incremented by an `!$omp atomic capture`, so no increment
    !! is lost and no two callers see the same count. (In a build without OpenMP that directive is a
    !! comment and the guarantee lapses; see the counter's declaration.) This is the
    !! one procedure in the module that is not a pure function, and the one that is deliberately
    !! not reproducible: a program that wants a reproducible run stores the value it got and passes
    !! that back next time.
    !!
    !! Two calls can only collide if their mixer outputs differ in nothing but the bit the range
    !! restriction clears -- one specific partner per call out of 2**64, which no run will meet.
    !!
    !! **Not cryptographic**, and not a substitute for one: the clock is guessable and the counter
    !! is small. For anything an adversary must not predict, take the seed from a CSPRNG instead.
    function pf_random_seed() result(s)
        integer(int64) :: s                         !! a nondeterministic seed in `[1, huge(int64)]`
        integer(int64) :: counted, ticks, rate, ceiling_
        ! Fetch-and-add, which is exactly what this needs -- `atomic capture` is the precise
        ! construct for it and is lock-free, where the named critical region this replaced
        ! serialised every caller through a lock for a single integer update. See the counter's
        ! own declaration for what holds when OpenMP is absent.
        !$omp atomic capture
        seed_call_counter = seed_call_counter + 1_int64
        counted = seed_call_counter
        !$omp end atomic
        call system_clock(count=ticks, count_rate=rate, count_max=ceiling_)
        s = mix64(ieor(mix64(ieor(ticks, rate)), counted))
        s = ibclr(s, 63)                            ! into [0, huge]; the mixer is otherwise a bijection
        if (s == 0_int64) s = 1_int64
    end function pf_random_seed

    !> `pf_random_key` for an `integer(int32)` label.
    pure elemental function pf_random_key_i32(seed, label) result(r)
        integer(int64), intent(in) :: seed          !! the seed to derive from
        integer(int32), intent(in) :: label         !! which derived family; sign-extends
        integer(int64) :: r                         !! an independent seed
        r = key_from(seed, int(label, int64))
    end function pf_random_key_i32

    !> `pf_random_key` for an `integer(int64)` label.
    pure elemental function pf_random_key_i64(seed, label) result(r)
        integer(int64), intent(in) :: seed          !! the seed to derive from
        integer(int64), intent(in) :: label         !! which derived family
        integer(int64) :: r                         !! an independent seed
        r = key_from(seed, label)
    end function pf_random_key_i64

    !> Reports which side of the route (e) fork was compiled. **Test-only.**
    !!
    !! Public only because it has to be: this module reaches no `bind(C)` surface, so the C++-side
    !! debug-hook convention the rest of the library uses is unavailable to it, and the fork is a
    !! compile-time source selection no runtime query could otherwise observe. It is excluded from
    !! README's API overview, no library code calls it, and it has no setter -- the fork is not a
    !! knob. Its whole job is to let one assertion close the allowlist's silent direction: this must
    !! agree with `selected_int_kind(38) > 0`, or a capable compiler is quietly shipping the
    !! wrapping kernel.
    pure function parquet_debug_random_uses_int128() result(r)
        logical :: r                                !! `.true.` if the 128-bit multiply was compiled
#ifdef PF_INT128
        r = .true.
#else
        r = .false.
#endif
    end function parquet_debug_random_uses_int128

    !> Reports whether the OVERFLOW-FREE 64-bit arm was compiled. **Test-only.**
    !!
    !! Public for the same reason as `parquet_debug_random_uses_int128`, and it exists because that
    !! one alone can no longer name the arm: the fork has three sides, and both `PF_SAFE64` and the
    !! wrapping arm answer `.false.` there. Without this, `tools/check_random_kernels.sh` would
    !! label a `PF_SAFE64` build "wrapping" and its vacuity guard would compare two arms it had
    !! mis-identified -- reporting that it had exercised both when it had exercised one twice.
    !!
    !! The two are mutually exclusive by construction (`#elif`), so `.true.` here implies `.false.`
    !! there; a build where both answer `.true.` is impossible and would mean the fork was edited
    !! into overlapping conditions.
    pure function parquet_debug_random_uses_safe64() result(r)
        logical :: r                                !! `.true.` if the overflow-free arm was compiled
#ifdef PF_SAFE64
        r = .true.
#else
        r = .false.
#endif
    end function parquet_debug_random_uses_safe64

    !> Runs the Philox block function directly, on raw counter and key words. **Test-only.**
    !!
    !! **This exists so the LIBRARY'S OWN kernel can be known-answer-checked against all three
    !! published Random123 vectors, rather than only the one the public API can reach.** The mapping
    !! puts the 0-based block index in counter words 0 and 1, and the block index comes from an
    !! `integer(int64)` draw — so it cannot exceed roughly `2**62`, while KAT 2 needs `0xffffffff`
    !! there and KAT 3 about `9.6e18`. Both are unreachable through `pf_random_at` and friends by
    !! construction, for any seed, stream or draw. Note the *key* is not restricted in the same way:
    !! it is derived from the seed by a bijection, so every 64-bit key is reachable; it is only the
    !! counter that is bounded.
    !!
    !! Without this, KAT 2 and KAT 3 could only be asserted against `test_random_reference.f90`, and
    !! the library kernel was tied to them transitively — directly known-answer-checked **once** and
    !! indirectly **twice**, through a reference that is itself checked three times. That chain is
    !! sound but it is a chain, and it leaves the one arithmetic that actually ships less directly
    !! evidenced than the reference written to check it. This hook makes all three direct.
    !!
    !! Public only because it has to be, exactly as `parquet_debug_random_uses_int128` is: this
    !! module reaches no `bind(C)` surface, so the C++-side debug-hook convention is unavailable to
    !! it. It is excluded from README's API overview, no library code calls it, and it is a pure
    !! observation with no setter — it cannot change what any draw returns.
    !!
    !! Arguments are the module's own coordinates, not Philox's four counter words: `key` splits
    !! into key words 0 and 1 (low half first), `stream` into counter words 2 and 3, and `index`
    !! into counter words 0 and 1. So KAT 2 — every counter and key word `0xffffffff` — is the call
    !! `parquet_debug_random_block(-1_int64, -1_int64, -1_int64, ...)`.
    !> Reports which branch a coordinate-addressed normal draw took, and what it cost. **Test-only.**
    !!
    !! Public only because it has to be: this module reaches no `bind(C)` surface, so the C++-side
    !! debug-hook convention the rest of the library uses is unavailable to it (see CLAUDE.md, "A
    !! Fortran-side debug hook has to be PUBLIC, so prefer a C++ one"). It is excluded from
    !! README.md's API overview and no library code calls it.
    !!
    !! **It is an OBSERVATION hook, not an override**, which is why it needs no process-global
    !! state at all: it re-runs the same construction the ordinary call runs and reports what
    !! happened. (It is not `pure` only because it can be asked for the polar form, which routes
    !! through `exp_key`.) A test asserts that all three Ziggurat branches occur over a fixture and
    !! the reported value equals the ordinary call's -- without which a rejection branch could be
    !! compiled and never entered while every test passed.
    !!
    !! `path` is 1 for the immediate rectangle acceptance, 2 for the wedge test and 3 for the tail;
    !! the polar form has one acceptance branch and always reports 1, its rejections showing up as
    !! `pairs > 2`. `pairs` counts 32-bit word PAIRS, so the word cost is twice it.
    !> Reports which branch a `%gamma` draw took. **Test-only**; see `parquet_debug_normal_path`
    !! for why a Fortran-side hook has to be public here.
    !!
    !! It advances `rng` exactly as `%gamma` does and returns the same value, so a test can walk a
    !! stream through it and census the branches. `path` is 1 for a squeeze acceptance and 2 for
    !! the full logarithmic test, plus 2 more when the `shape < 1` boost was applied -- so all four
    !! combinations are distinguishable.
    pure subroutine parquet_debug_gamma_path(rng, shape, r, path, squeeze_ok)
        type(pf_random_stream), intent(inout) :: rng    !! the stream to advance, as `%gamma` would
        real(real64), intent(in) :: shape               !! the shape parameter; must be > 0
        real(real64), intent(out) :: r                  !! the draw, equal to `%gamma`'s
        integer(int32), intent(out) :: path             !! 1/2 squeeze/log; +2 when boosted
        logical, intent(out), optional :: squeeze_ok    !! `.false.` iff the squeeze accepted a
                                                        !! candidate the full test would reject
        call gamma_draw(rng, shape, r, path, squeeze_ok)
    end subroutine parquet_debug_gamma_path

    !> Reports which algorithm and branch a `%poisson` draw took. **Test-only**; see
    !! `parquet_debug_normal_path`.
    !!
    !! `path` is 1 for Knuth's product, 2 for a PTRS candidate taken by the fast acceptance region
    !! and 3 for one taken by the full logarithmic test. Forcing the OTHER algorithm is not
    !! offered, and does not need to be: which one runs is decided by `lambda` alone, so a test
    !! reaches either by choosing a `lambda` on the side it wants -- and that is a better test,
    !! because it exercises the crossover as it actually ships.
    pure subroutine parquet_debug_poisson_path(rng, lambda, k, path, squeeze_ok)
        type(pf_random_stream), intent(inout) :: rng    !! the stream to advance, as `%poisson` would
        real(real64), intent(in) :: lambda              !! the mean; must be >= 0 and finite
        integer(int64), intent(out) :: k                !! the count, equal to `%poisson`'s
        integer(int32), intent(out) :: path             !! 1 Knuth, 2 PTRS fast, 3 PTRS log test
        logical, intent(out), optional :: squeeze_ok    !! `.false.` iff the fast region accepted a
                                                        !! candidate the full test would reject
        call poisson_draw(rng, lambda, k, path, squeeze_ok)
    end subroutine parquet_debug_poisson_path

    !> Reports which case a `%normal_truncated` draw took, and how many proposals it cost.
    !! **Test-only**; see `parquet_debug_normal_path`.
    !!
    !! `path` is 1 naive, 2 uniform proposal, 3 exponential tilting, plus 3 more when the interval
    !! was mirrored onto the non-negative side, so a test asserting the mirror is not lost has
    !! something to read. **The reachable set is `{1, 2, 3, 5, 6}`; `4` cannot occur** -- see
    !! `normal_truncated_draw`. Forcing a case is not offered and is not needed: the case is
    !! decided by the standardised bounds alone, so a test reaches any of the five by choosing an
    !! interval on the side it wants, which also exercises the two thresholds as they ship.
    pure subroutine parquet_debug_normal_truncated_path(rng, lo, hi, x, path, tries, mu, sigma)
        type(pf_random_stream), intent(inout) :: rng    !! the stream to advance, as `%normal_truncated` would
        real(real64), intent(in) :: lo              !! lower bound of the support, in `x`'s units
        real(real64), intent(in) :: hi              !! upper bound of the support, in `x`'s units
        real(real64), intent(out) :: x              !! the draw, equal to `%normal_truncated`'s
        integer(int32), intent(out) :: path         !! 1/2/3 naive/uniform/tilted; +3 when mirrored
        integer(int64), intent(out) :: tries        !! proposals the rejection loop consumed
        real(real64), intent(in), optional :: mu    !! untruncated mean; default 0
        real(real64), intent(in), optional :: sigma !! untruncated standard deviation; default 1
        call normal_truncated_draw(rng, lo, hi, x, path, tries, mu, sigma)
    end subroutine parquet_debug_normal_truncated_path

    subroutine parquet_debug_normal_path(seed, i, draw, portable, x, path, pairs)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index
        logical, intent(in) :: portable             !! `.true.` for the polar form, `.false.` Ziggurat
        real(real64), intent(out) :: x              !! the draw, equal to the ordinary call's
        integer(int32), intent(out) :: path         !! 1 rectangle, 2 wedge, 3 tail
        integer(int64), intent(out) :: pairs        !! word pairs the rejection loop consumed
        ! `draw_or_1` rather than `draw`, so a clamped draw reports what the ordinary call does.
        ! Without it, `draw = 0` derives label 0 here and label 1 there, and the hook silently
        ! describes a different construction than the one it is meant to observe.
        if (portable) then
            path = 1
            call polar_normal(pf_random_key(pf_random_key(seed, normal_polar_label), draw_or_1(draw)), &
                              i, 1_int64, x, pairs)
        else
            call zig_normal(pf_random_key(pf_random_key(seed, normal_zig_label), draw_or_1(draw)), &
                            i, 1_int64, x, pairs, path)
        end if
    end subroutine parquet_debug_normal_path

    pure subroutine parquet_debug_random_block(key, stream, index, w0, w1, w2, w3)
        integer(int64), intent(in) :: key           !! 64-bit key: key words 0 and 1, low half first
        integer(int64), intent(in) :: stream        !! splits into counter words 2 and 3
        integer(int64), intent(in) :: index         !! 0-based block index: counter words 0 and 1
        integer(int64), intent(out) :: w0           !! output word `c0`
        integer(int64), intent(out) :: w1           !! output word `c1`
        integer(int64), intent(out) :: w2           !! output word `c2`
        integer(int64), intent(out) :: w3           !! output word `c3`
        call random_block(key, stream, index, w0, w1, w2, w3)
    end subroutine parquet_debug_random_block

    ! ================================================================================
    ! The cipher
    ! ================================================================================

    !> Enciphers one Philox4x32-10 block: four 32-bit output words, each held in an `int64`.
    !!
    !! The state is four named scalars rather than a four-element array -- the array spelling
    !! measures 3.2x slower. The rounds are a loop rather than hand-unrolled, which ifx pays up to
    !! 2.37x for; that sign flips for the lane-blocked bulk kernels a later phase adds, which is
    !! why they will be separate procedures rather than a flag on this one.
    pure subroutine random_block(key, stream, index, w0, w1, w2, w3)
        integer(int64), intent(in) :: key           !! 64-bit key: a seed, or a retry key
        integer(int64), intent(in) :: stream        !! stream index; splits into counter words 2 and 3
        integer(int64), intent(in) :: index         !! 0-based block index; splits into counter words 0 and 1
        integer(int64), intent(out) :: w0           !! output word `c0`
        integer(int64), intent(out) :: w1           !! output word `c1`
        integer(int64), intent(out) :: w2           !! output word `c2`
        integer(int64), intent(out) :: w3           !! output word `c3`
        integer(int64) :: c0, c1, c2, c3, k0, k1, hi0, hi1, lo0, lo1
        integer :: r
#ifdef PF_INT128
        integer(k128) :: p0, p1
#else
        integer(int64) :: p0, p1
#endif
#ifdef PF_SAFE64
        integer(int64) :: s0, s1
#endif
        k0 = iand(key, M32)
        k1 = iand(ishft(key, -32), M32)
        c0 = iand(index, M32)
        c1 = iand(ishft(index, -32), M32)
        c2 = iand(stream, M32)
        c3 = iand(ishft(stream, -32), M32)
        do r = 1, PHILOX_ROUNDS
#ifdef PF_INT128
            ! Both operands are below 2**32, so each product is below 2**64 and fits the wide kind
            ! with 63 bits to spare. Each half is extracted while still wide and is below 2**32, so
            ! narrowing back is exact -- no value at or above 2**63 is ever formed in an int64.
            p0 = int(PHILOX_M0, k128) * int(c0, k128)
            p1 = int(PHILOX_M1, k128) * int(c2, k128)
            hi0 = int(ishft(p0, -32), int64)
            lo0 = int(iand(p0, M32_128), int64)
            hi1 = int(ishft(p1, -32), int64)
            lo1 = int(iand(p1, M32_128), int64)
#elif defined(PF_SAFE64)
            ! Overflow-free without a 128-bit kind, by the halved-constant identity. Both Philox
            ! multipliers are odd, so `M*c == 2*((M/2)*c) + c`; `(M/2)*c <= (2**31-1)(2**32-1)`,
            ! which is below 2**63, and the doubling is then done on the two halves so that no
            ! intermediate reaches 2**63 either -- `s` stays below 2**33. Nothing here can
            ! overflow at any input, so this arm needs no wrapping measurement standing behind it.
            p0 = PHILOX_H0 * c0
            s0 = ishft(iand(p0, M31), 1) + c0
            lo0 = iand(s0, M32)
            hi0 = ishft(p0, -31) + ishft(s0, -32)
            p1 = PHILOX_H1 * c2
            s1 = ishft(iand(p1, M31), 1) + c2
            lo1 = iand(s1, M32)
            hi1 = ishft(p1, -31) + ishft(s1, -32)
#else
            ! UB site 1 of 2: the true product can exceed huge(int64) and wraps. `ishft` is a
            ! LOGICAL shift, so it recovers the correct high half from the wrapped pattern. Shipped
            ! only where there is no 128-bit kind, on a compiler measured to wrap faithfully.
            p0 = PHILOX_M0 * c0
            p1 = PHILOX_M1 * c2
            hi0 = ishft(p0, -32)
            lo0 = iand(p0, M32)
            hi1 = ishft(p1, -32)
            lo1 = iand(p1, M32)
#endif
            c0 = ieor(ieor(hi1, c1), k0)
            c1 = lo1
            c2 = ieor(ieor(hi0, c3), k1)
            c3 = lo0
            ! The bump belongs between rounds, so round 10's is dead. Computing it anyway is
            ! cheaper than branching on the round number, and cannot change a value: nothing reads
            ! the key again. Both bumps are masked, so neither can overflow.
            k0 = iand(k0 + PHILOX_W0, M32)
            k1 = iand(k1 + PHILOX_W1, M32)
        end do
        w0 = c0
        w1 = c1
        w2 = c2
        w3 = c3
    end subroutine random_block

    !> Word `index` (0-based) of a stream: blocks 0, 1, 2, ... each giving `c0, c1, c2, c3`.
    !!
    !! **The four words are named scalars selected by `select case`, not a local array indexed at
    !! run time**, which is worth about 5-7 % of `pf_random32_at` on x86-64 -- 18.32 -> 17.37 ns on
    !! machine C (gfortran 15.2) and 22.33 -> 20.86 / 15.75 -> 14.67 on machine B (gfortran 14.2.1 /
    !! ifx). Machine A (arm64) measures no change, so this is positive-or-neutral rather than
    !! universal. Note it is the array indexing that pays and **not** the `/` and `modulo`:
    !! rewriting those as `ishft`/`iand` -- valid, since `index` is `draw - 1` with `draw` clamped
    !! to at least 1 -- was measured at nothing on both machines that tried it, so that spelling was
    !! deliberately NOT taken. It would trade a form correct for every input for one correct only
    !! for non-negative inputs, and buy zero.
    pure function word_of(seed, stream, index) result(w)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: index         !! 0-based word index within the stream
        integer(int64) :: w                         !! that word, in `[0, 2**32)`
        integer(int64) :: c0, c1, c2, c3
        call random_block(seed, stream, ior(DOM_REAL32, index / 4_int64), c0, c1, c2, c3)
        select case (int(modulo(index, 4_int64), int32))
        case (0)
            w = c0
        case (1)
            w = c1
        case (2)
            w = c2
        case default
            w = c3
        end select
    end function word_of

    !> The 64-bit pattern of `real64`/raw-bits value `draw` (1-based) of a stream.
    !!
    !! Value `d` occupies words `2d-2` and `2d-1`, so a block carries TWO values: draw 1 takes the
    !! pair `(c0, c1)` and draw 2 the pair `(c2, c3)`. The first word is the LOW half. A draw-axis
    !! bulk fill is faster precisely because it can use both pairs of a block where a scalar draw
    !! uses one. **`pf_random_int_at` reaches this too**, and has since `pf_random_algorithm` `/v2`
    !! -- the three 64-bit generics are one grid, so there is one function that decides which words
    !! a draw index names, and it is this one.
    !!
    !! **`ishft`/`iand` rather than `/` and `modulo`, and the spelling is measured, not assumed.**
    !! Machine B, gfortran 15.2.1, `-O3 -funroll-loops`, best of three rounds over 4M values, with
    !! the `real64` bulk fill as a cross-build control (8.66-8.70 ns in every build, a 0.5 % floor):
    !! a scalar `pf_random_int_at` costs **27.9 ns** with `/`+`modulo` and **25.8** with
    !! `ishft`+`iand`, and the integer draw-axis fill **16.3** against **15.8**. So this is worth
    !! about 2 ns on the integer path, which is the whole of what stride-2 addressing costs it.
    !!
    !! Note `word_of`'s own header records the same substitution measuring **nothing** for its
    !! `index/4` and `modulo(index,4)`, on two machines, and being declined there for that reason.
    !! Both notes are right: they are different functions on different paths, and the point is that
    !! the spelling is decided per site by measurement. Do not propagate either verdict to the other.
    !!
    !! The `max(draw, 1)` is what keeps the substitution safe, and it is **free** -- it measured
    !! inside the noise of the arms above. `ISHFT` is a LOGICAL shift, so `ishft(-1_int64, -1)` is
    !! `2**63 - 1` rather than 0: without the clamp, a negative `draw` would name an absurd block
    !! where `/` and `modulo` merely named the wrong pair. Every caller already clamps (`draw_or_1`
    !! at tier 0, `take_pair` at the stream, `draw + (k-1)` in the fills), so this changes no value
    !! any caller can obtain -- it makes the function total on its own rather than by their courtesy.
    pure function bits_of(seed, stream, draw, dom) result(b)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index; `< 1` clamps to 1
        integer(int64), intent(in) :: dom           !! word space: `DOM_REAL64` or `DOM_INT_WIDE`
        integer(int64) :: b                         !! the 64-bit pattern
        integer(int64) :: w0, w1, w2, w3, e
        e = max(draw, 1_int64) - 1_int64            ! 0-based value index; non-negative by the max
        call random_block(seed, stream, ior(dom, ishft(e, -1)), w0, w1, w2, w3)
        if (iand(e, 1_int64) == 0_int64) then
            b = ior(ishft(w1, 32), w0)
        else
            b = ior(ishft(w3, 32), w2)
        end if
    end function bits_of

    !> The top 53 bits of a 64-bit pattern, as a `real64` in `[0, 1)`.
    !!
    !! Exact: 53 bits fit a `real64` significand without rounding, and scaling by a power of two
    !! is exact, so this is the rational `(bits >> 11) / 2**53` and nothing else. 1.0 is
    !! unreachable by construction; 0.0 occurs with probability 2**-53.
    pure function to_real64(bits) result(r)
        integer(int64), intent(in) :: bits          !! any 64-bit pattern
        real(real64) :: r                           !! `[0, 1)`
        r = real(ishft(bits, -11), real64) * 2.0_real64**(-53)
    end function to_real64

    !> The top 24 bits of one 32-bit word, as a `real32` in `[0, 1)`.
    !!
    !! Exact for the same reason as `to_real64`, and it cannot return 1.0 -- which a narrowing of a
    !! `real64` draw could, by rounding up. That is why the `real32` sequence is its own.
    pure function to_real32(w) result(r)
        integer(int64), intent(in) :: w             !! one Philox word, in `[0, 2**32)`
        real(real32) :: r                           !! `[0, 1)`
        r = real(ishft(w, -8), real32) * 2.0_real32**(-24)
    end function to_real32

    !> Resolves an optional `draw` to a valid 1-based value index.
    !!
    !! Absent means 1. A non-positive `draw` is a documented precondition violation, absorbed
    !! rather than reported: every tier-0 procedure is `pure elemental` and so has no way to abort,
    !! and a caller that has computed a bad index deserves a defined answer over a silent one. It
    !! must be clamped HERE rather than left to flow into the block arithmetic, where truncating
    !! division would map `draw = 0` onto `draw = 1` by accident instead of by rule -- and would
    !! map `draw = -1` somewhere else again.
    pure function draw_or_1(draw) result(d)
        integer(int64), intent(in), optional :: draw !! the caller's `draw`, present or not
        integer(int64) :: d                         !! a value index of at least 1
        d = 1_int64
        if (present(draw)) d = draw
        if (d < 1_int64) d = 1_int64
    end function draw_or_1

    ! ================================================================================
    ! Distribution mappings
    ! ================================================================================

    !> The exponential mapping: the uniform at `(seed, stream, draw)`, transformed to `Exp(1)`.
    !!
    !! **The single place `-log(1 - u)` is written for the fast realisation**, so its three tiers
    !! cannot drift: the tier-0 specifics, the bulk fill's per-element step and `stream_exp` all
    !! reach the value through here or through the identical expression `fill_exp` inlines.
    !! `1 - u` rather than `u` is contract -- see `pf_random_exp_at`.
    pure function exp_of(seed, stream, draw) result(e)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! which stream of that family
        integer(int64), intent(in) :: draw          !! 1-based value index
        real(real64) :: e                           !! an `Exp(1)` draw in `[0, 36.7368]`
        e = -log(1.0_real64 - to_real64(bits_of(seed, stream, draw, DOM_REAL64)))
    end function exp_of

    !> `exp_of` through `parquet_expkey`'s frozen transform, for the `_portable` realisation.
    !!
    !! Not `pure`, because `exp_key` is not -- see `pf_random_exp_portable_at`. The uniform this
    !! transforms is bit-for-bit the one `exp_of` transforms at the same coordinate; only the
    !! logarithm differs, and the two agree to about 2 ulp.
    function exp_portable_of(seed, stream, draw) result(e)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! which stream of that family
        integer(int64), intent(in) :: draw          !! 1-based value index
        real(real64) :: e                           !! an `Exp(1)` draw in `[0, 36.7368]`
        e = exp_key(1.0_real64 - to_real64(bits_of(seed, stream, draw, DOM_REAL64)))
    end function exp_portable_of

    !> Fills `v` with consecutive `Exp(1)` draws, by filling uniforms in bulk and transforming.
    !!
    !! The uniform fill is where the amortisation is -- it enciphers once per PAIR of values,
    !! where a loop of `exp_of` would encipher once per value -- and the transform is then a
    !! separate pass over an array already in cache. Both passes together still beat the scalar
    !! loop, and the UNIFORMS are identical either way, since `fill_r64` is contractually equal to
    !! the matching `pf_random_at` calls -- only the logarithm can differ, and only because a
    !! compiler may vectorise this loop's `log` and not the scalar path's (see
    !! `pf_random_exp_at`). Measured on machine B (gfortran 15.2.1, `-O3
    !! -funroll-loops`, 4M values): 8.67 ns per value for the uniform fill alone, 11.16 with this
    !! transform on top -- so the exponential costs 29 % more than a uniform, against the 3.5x
    !! `fill_exp_portable` costs.
    pure subroutine fill_exp(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! which stream of that family
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index
        integer(int64) :: k, m
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        call fill_r64(seed, stream, v, draw)
        do k = 1_int64, m
            v(k) = -log(1.0_real64 - v(k))
        end do
    end subroutine fill_exp

    !> `fill_exp` through the frozen transform. Not `pure`, for `exp_key`'s reason.
    subroutine fill_exp_portable(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! which stream of that family
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index
        integer(int64) :: k, m
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        call fill_r64(seed, stream, v, draw)
        do k = 1_int64, m
            v(k) = exp_key(1.0_real64 - v(k))
        end do
    end subroutine fill_exp_portable

    !> The Ziggurat draw, reading word pairs `draw0, draw0+1, ...` of one `(key, stream)`.
    !!
    !! **One implementation, both tiers.** The coordinate-addressed forms call it with a derived
    !! sub-key and `draw0 = 1`; the stream walk calls it with the stream's own key and whatever
    !! pair the cursor is on, then advances by the pairs it reports. That is what keeps the two
    !! tiers from drifting apart algorithmically even though they deliberately produce different
    !! values -- the difference is entirely in the key, and is visible at the call sites.
    !!
    !! **The 64 bits are split three ways, disjointly.** Bits 0-7 pick the layer, bit 8 the sign,
    !! and bits 11-63 are the uniform (`to_real64` shifts right by 11). Reusing the same bits for
    !! the layer and the value -- as the original 1990s formulation does, for want of a 64-bit
    !! draw -- is the correlation Leong et al. found in it; here there is no reason to.
    !!
    !! `path` reports which branch produced the value: 1 the immediate rectangle acceptance
    !! (98.5 % of draws), 2 the wedge test, 3 the tail. It exists so a test can assert that all
    !! three were reached -- a rejection branch that is compiled and never entered would otherwise
    !! pass every test ever written for it. Nothing in the library reads it.
    pure subroutine zig_normal(key, stream, draw0, x, pairs, path)
        integer(int64), intent(in) :: key           !! the family this draw reads
        integer(int64), intent(in) :: stream        !! which stream of that family
        integer(int64), intent(in) :: draw0         !! 1-based pair index to start reading at
        real(real64), intent(out) :: x              !! a standard normal draw
        integer(int64), intent(out) :: pairs        !! word pairs consumed; at least 1
        integer(int32), intent(out) :: path         !! 1 rectangle, 2 wedge, 3 tail
        integer(int64) :: b, d
        integer(int32) :: i
        real(real64) :: u, xx, y, ta, tb

        d = draw0
        do
            b = bits_of(key, stream, d, DOM_REAL64)
            d = d + 1_int64
            i = int(iand(b, int(zig_layers - 1, int64)), int32)
            u = to_real64(b)
            if (u < zig_k(i)) then
                xx = u * zig_w(i)                   ! wholly inside the curve: no further work
                path = 1
                exit
            end if
            if (i == 0) then
                ! The tail, beyond `zig_r`. Marsaglia's exponential-rejection form: accept
                ! `zig_r + ta` when `2*tb >= ta*ta` for two independent Exp(1) draws, which is
                ! exactly the conditional density of a normal above `zig_r`.
                path = 3
                do
                    ta = -log(1.0_real64 - to_real64(bits_of(key, stream, d, DOM_REAL64))) / zig_r
                    tb = -log(1.0_real64 - to_real64(bits_of(key, stream, d + 1_int64, DOM_REAL64)))
                    d = d + 2_int64
                    if (tb + tb >= ta * ta) exit
                end do
                xx = zig_r + ta
                exit
            end if
            xx = u * zig_w(i)
            ! The wedge between the layer's inner and outer edges: accept if a uniform height in
            ! `[f(w(i)), f(w(i-1))]` falls below the curve at `xx`.
            y = zig_f(i) + to_real64(bits_of(key, stream, d, DOM_REAL64)) * (zig_f(i - 1) - zig_f(i))
            d = d + 1_int64
            if (y < exp(-0.5_real64 * xx * xx)) then
                path = 2
                exit
            end if
        end do
        ! The sign comes from bit 8 of whichever pattern produced the accepted magnitude, which
        ! on the tail path is the one that selected layer 0 -- it is untouched by the tail walk.
        if (btest(b, 8)) xx = -xx
        x = xx
        pairs = d - draw0
    end subroutine zig_normal

    !> The polar draw, reading word pairs `draw0, draw0+1, ...` of one `(key, stream)`.
    !!
    !! Two uniforms per candidate, mapped into the square `[-1,1)**2`, rejected unless they land
    !! in the unit disc -- so 21.5 % of candidate PAIRS are rejected, against the Ziggurat's 1.5 %
    !! of single draws. `pairs` is therefore always even and is 2 about 78.5 % of the time.
    !!
    !! **The second variate is discarded, deliberately.** The polar method produces two normals
    !! per accepted candidate and a generator that cached the mate would carry state beyond its
    !! word position, which would make `%rewind` inexact and a chunked fill wrong. Half the output
    !! is the price of the position being the whole state.
    !!
    !! **Three rounding barriers, and each one closes a specific rewrite** that `-ffast-math` (and
    !! ifx's default `-fp-model=fast`) is entitled to make. Everything else here is a single IEEE
    !! operation, and `sqrt` is correctly rounded by the standard. See `parquet_expkey`'s own note
    !! on the reassociation that broke that transform once already.
    !!
    !! **The two on `u1*u1` and `u2*u2` are MEASURED as load-bearing**, not argued: that sum is the
    !! exact shape a compiler contracts into an FMA, which rounds once where IEEE rounds twice.
    !! Removing them and rebuilding the golden vectors on machine B (gfortran 15.2.1) moves **12 of
    !! 144** rows under `-O3 -march=native` and under `-O3 -ffast-math -march=native`, and moves
    !! nothing under `-O2` or under a plain `-Ofast`. **`-march=native` is what decides it, not the
    !! optimisation level** -- baseline x86-64 has no FMA to contract into -- which is the same
    !! architecture gating that once hid this hazard in `exp_key`, and the reason a sweep that omits
    !! it proves nothing.
    !!
    !! **The one on `sqrt(q + q)` is insurance, and that is stated rather than implied.** Without
    !! it the argument is still an expression and a compiler is licensed to rewrite `sqrt(2*t/s)` as
    !! `sqrt(2*t)/sqrt(s)`, which rounds differently. Removing it moves **no** row under any of the
    !! four flag sets above, so gfortran does not currently make that rewrite. It costs one store
    !! and one reload per accepted draw and closes a rewrite the standard permits; the same
    !! reasoning kept `ek_rnd(small - logm)` in `exp_key`, which only ifx ever needed.
    !!
    !! Not `pure`, because `exp_key` is not.
    subroutine polar_normal(key, stream, draw0, x, pairs)
        integer(int64), intent(in) :: key           !! the family this draw reads
        integer(int64), intent(in) :: stream        !! which stream of that family
        integer(int64), intent(in) :: draw0         !! 1-based pair index to start reading at
        real(real64), intent(out) :: x              !! a standard normal draw
        integer(int64), intent(out) :: pairs        !! word pairs consumed; even, at least 2
        integer(int64) :: d
        real(real64) :: a1, a2, u1, u2, s, q

        d = draw0
        do
            a1 = to_real64(bits_of(key, stream, d, DOM_REAL64))
            a2 = to_real64(bits_of(key, stream, d + 1_int64, DOM_REAL64))
            d = d + 2_int64
            u1 = (a1 + a1) - 1.0_real64             ! `a + a` is exact, so this rounds once
            u2 = (a2 + a2) - 1.0_real64
            s = ek_round(u1 * u1) + ek_round(u2 * u2)
            if (s < 1.0_real64 .and. s >= polar_min_s) exit
        end do
        q = exp_key(s) / s                          ! `exp_key(s)` is `-log(s)`
        x = u1 * sqrt(ek_round(q + q))
        pairs = d - draw0
    end subroutine polar_normal

    !> `pf_random_normal_at`'s sub-stream construction, in one place so both tiers cannot drift.
    !!
    !! Value `(i, draw)` gets its own family: the label separates this realisation from every
    !! other, and nesting `draw` inside it gives each draw of each stream an independent infinite
    !! sub-stream for its rejection loop to walk. `pf_random_key` derivations compose by nesting,
    !! which is what makes that legal rather than merely plausible.
    pure function normal_at_impl(seed, stream, draw) result(x)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! which stream of that family
        integer(int64), intent(in) :: draw          !! 1-based value index
        real(real64) :: x                           !! a standard normal draw
        integer(int64) :: pairs
        integer(int32) :: path
        call zig_normal(pf_random_key(pf_random_key(seed, normal_zig_label), draw), &
                        stream, 1_int64, x, pairs, path)
    end function normal_at_impl

    !> `pf_random_normal_portable_at`'s sub-stream construction. See `normal_at_impl`.
    function normal_portable_at_impl(seed, stream, draw) result(x)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! which stream of that family
        integer(int64), intent(in) :: draw          !! 1-based value index
        real(real64) :: x                           !! a standard normal draw
        integer(int64) :: pairs
        call polar_normal(pf_random_key(pf_random_key(seed, normal_polar_label), draw), &
                          stream, 1_int64, x, pairs)
    end function normal_portable_at_impl

    !> Fills `v` with consecutive Ziggurat normals, hoisting the label derivation out of the loop.
    !!
    !! There is no block-level amortisation to be had here, unlike `fill_exp`: every element runs
    !! its own rejection loop in its own family, so the fill is exactly a loop of scalar draws with
    !! one `pf_random_key` chain lifted out of it. That is the cost of being splittable, and it is
    !! what lets a caller thread the fill themselves.
    pure subroutine fill_normal(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! which stream of that family
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index
        integer(int64) :: k, m, root, pairs
        integer(int32) :: path
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        root = pf_random_key(seed, normal_zig_label)
        do k = 1_int64, m
            call zig_normal(pf_random_key(root, draw + (k - 1_int64)), stream, 1_int64, v(k), pairs, path)
        end do
    end subroutine fill_normal

    !> `fill_normal` through the polar method. Not `pure`, for `exp_key`'s reason.
    subroutine fill_normal_portable(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! which stream of that family
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index
        integer(int64) :: k, m, root, pairs
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        root = pf_random_key(seed, normal_polar_label)
        do k = 1_int64, m
            call polar_normal(pf_random_key(root, draw + (k - 1_int64)), stream, 1_int64, v(k), pairs)
        end do
    end subroutine fill_normal_portable

    ! ================================================================================
    ! Points on a sphere: the mappings
    ! ================================================================================
    !
    ! **Every refusal lives in a `pure` procedure that also PRODUCES what the draw uses** -- the
    ! validated draw index, the normalised centre, the validated radii, the vMF offset, the
    ! concentration -- never in a check-only call: ifx deletes a guard-only `pure` call at `-O0`
    ! (`api-conventions.md`). A NaN is screened by `x /= x`, as its own statement, before any ordered
    ! comparison or `min`/`max` could raise `IEEE_INVALID` on it -- `x /= x` rather than
    ! `ieee_is_nan` because the screens run once per draw and `ieee_is_nan` is a runtime call under
    ! ifx and nagfor; the price is one accepted `-Wcompare-reals` note per screen.
    !
    ! **Every small quantity is formed directly rather than as a difference.** The offset of a point
    ! from its centre is carried as `h = 1 - cos(theta)`, never as `cos(theta)` subtracted from 1:
    ! a disc of `1e-9` radians holds its points to about an ulp this way and would lose its whole
    ! radius the other way, since `cos(1e-9)` and 1 are one double. The cap's two bounds come from
    ! half-angle sines for the same reason, and the vMF offset from `atanh` and `sinh` wherever
    ! `log(1 - x)` would cancel -- a `kappa` of `1e-30` otherwise puts every point at one pole.

    !> A `real64` as message text, in `es` form so no leading zero or exponent width varies by compiler.
    pure function sph_real_text(x) result(t)
        real(real64), intent(in) :: x               !! the value to render
        character(len=24) :: t                      !! the rendering, left-justified
        if (x /= x) then
            t = "NaN"
        else if (abs(x) >= 1.0e100_real64 .or. (abs(x) > 0.0_real64 .and. abs(x) < 1.0e-99_real64)) then
            write (t, '(es15.7e3)') x
        else
            write (t, '(es14.7)') x
        end if
        t = adjustl(t)
    end function sph_real_text

    !> The validated 1-based draw index of a sphere draw: absent means 1, below 1 clamps to 1, above
    !! `2**62` aborts naming `who`.
    pure function sph_draw(who, draw) result(d)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        integer(int64), intent(in), optional :: draw !! the caller's `draw`, present or not
        integer(int64) :: d                          !! a draw index in `[1, 2**62]`
        character(len=24) :: t
        d = draw_or_1(draw)
        if (d > sphere_draw_max) then
            write (t, '(i0)') d
            error stop who // ": draw must be at most 2**62 (got " // trim(t) // &
                "); the sphere family addresses one block per draw"
        end if
    end function sph_draw

    !> The validated first draw of a fill of `n > 0` values: as `sph_draw`, for the fill's LAST draw.
    pure function sph_fill_start(who, draw, n) result(d)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        integer(int64), intent(in), optional :: draw !! the caller's starting `draw`, present or not
        integer(int64), intent(in) :: n              !! how many values the fill draws; at least 1
        integer(int64) :: d                          !! the first draw index; `d + n - 1 <= 2**62`
        character(len=24) :: t, tn
        d = draw_or_1(draw)
        ! Compared as `d > max - (n - 1)`, which cannot overflow for any `n >= 1`, rather than by
        ! forming the last draw, which can.
        if (d > sphere_draw_max - (n - 1_int64)) then
            write (t, '(i0)') d
            write (tn, '(i0)') n
            error stop who // ": draw must be at most 2**62 (got " // trim(t) // " + " // trim(tn) // &
                " - 1); the sphere family addresses one block per draw"
        end if
    end function sph_fill_start

    !> `pf_random_pair_spare_at`'s body: one enciphering read as two uniforms AND an integer.
    !!
    !! `to_real64` keeps the top 53 bits of each 64-bit half, so each half discards 11 -- 22 bits
    !! across the block that cost nothing and are otherwise thrown away. They are enough to choose
    !! from a list, which is what lets a composite draw (a point in a pixel, and which pixel) take
    !! ONE enciphering instead of two.
    !!
    !! **`u1` and `u2` do not depend on whether the integer was decided**: they are the block's two
    !! uniforms unconditionally, exactly as `sph_pair` reads them. So the caller's point is
    !! unaffected by the choice and the two can be reasoned about separately.
    !!
    !! **`ok` is `.false.` when 22 bits cannot decide the range exactly** -- a width above `2**22`,
    !! or Lemire's rejection firing -- and the caller then draws the integer some other exact way.
    !! That keeps the result exactly uniform: conditional on acceptance Lemire is uniform, the
    !! caller's fallback is uniform, and a mixture of two uniforms is uniform. The rejection test
    !! reads only the spare bits, which live in a different field of the block from the uniforms.
    pure subroutine pair_spare_draw(seed, stream, lo, hi, d, u1, u2, j, ok)
        integer(int64), intent(in) :: seed          !! the family's derived key
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: lo            !! the range's low end
        integer(int64), intent(in) :: hi            !! the range's high end
        integer(int64), intent(in) :: d             !! the draw, at least 1
        real(real64), intent(out) :: u1             !! the block's first uniform, in `[0, 1)`
        real(real64), intent(out) :: u2             !! the block's second uniform, in `[0, 1)`
        integer(int64), intent(out) :: j            !! a uniform integer in `[lo, hi]`, when `ok`
        logical, intent(out) :: ok                  !! whether the spare bits decided it
        integer(int64) :: w0, w1, w2, w3, b1, b2, cand, a, s, m, hi22, lo22, t
        call random_block(seed, stream, ior(DOM_REAL64, d - 1_int64), w0, w1, w2, w3)
        b1 = ior(ishft(w1, 32), w0)
        b2 = ior(ishft(w3, 32), w2)
        u1 = to_real64(b1)
        u2 = to_real64(b2)
        a = min(lo, hi)
        j = a
        ok = .false.
        s = width_of(a, max(lo, hi))
        ! `s` is unsigned with 0 meaning the whole int64 range; a width above `2**22` reads as a
        ! large positive or as negative, and both are refused by this one test.
        if (s <= 0_int64 .or. s > SPARE_CAP) return
        cand = ior(ishft(iand(b1, SPARE_MASK), SPARE_BITS), iand(b2, SPARE_MASK))
        ! Lemire over a 22-bit candidate. `cand*s` is below `2**44`, so nothing overflows.
        m = cand * s
        hi22 = ishft(m, -(2 * SPARE_BITS))
        lo22 = iand(m, SPARE_CAP - 1_int64)
        ! Lemire's LAZY GUARD, as `int_reduce` uses it and for the same reason: the exact threshold
        ! is `mod(2**22, s)`, a division by a RUNTIME value, and paying it per draw costs more than
        ! the enciphering this routine exists to save. A low word at or above `s` cannot lie in the
        ! last partial block whatever the threshold is, so it is accepted without computing one;
        ! only the remaining `s/2**22` of draws divide. Both operands are below `2**22` and
        ! non-negative, so these are ordinary signed comparisons.
        if (lo22 >= s) then
            j = offset_by(a, hi22)
            ok = .true.
            return
        end if
        t = mod(SPARE_CAP, s)
        if (lo22 < t) return
        j = offset_by(a, hi22)
        ok = .true.
    end subroutine pair_spare_draw

    !> The two uniforms of one sphere draw: the halves of block `d - 1` of `(key, stream)`.
    !!
    !! They are exactly `pf_random_at(key, stream, 2*d - 1)` and `pf_random_at(key, stream, 2*d)`,
    !! read from one enciphering; the block index is formed directly because `2*d` overflows at the
    !! last admitted draw. `d` is already validated.
    pure subroutine sph_pair(key, stream, d, u1, u2)
        integer(int64), intent(in) :: key           !! the family's derived key
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: d             !! the draw, in `[1, 2**62]`
        real(real64), intent(out) :: u1             !! the block's first uniform, in `[0, 1)`
        real(real64), intent(out) :: u2             !! the block's second uniform, in `[0, 1)`
        integer(int64) :: w0, w1, w2, w3
        call random_block(key, stream, ior(DOM_REAL64, d - 1_int64), w0, w1, w2, w3)
        u1 = to_real64(ior(ishft(w1, 32), w0))
        u2 = to_real64(ior(ishft(w3, 32), w2))
    end subroutine sph_pair

    !> `centre` validated and normalised: scaled by its largest component first, so a vector like
    !! `[1e-300, 0, 1e-300]` is a direction rather than a zero, then divided by its length.
    pure function sph_unit(who, centre) result(c)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        real(real64), intent(in) :: centre(3)        !! any nonzero, finite vector
        real(real64) :: c(3)                         !! the unit vector along it
        real(real64) :: scale, length
        if (centre(1) /= centre(1) .or. centre(2) /= centre(2) .or. centre(3) /= centre(3)) then
            error stop who // ": the centre must be a nonzero, finite direction"
        end if
        scale = max(abs(centre(1)), abs(centre(2)), abs(centre(3)))
        if (.not. (scale > 0.0_real64 .and. scale <= huge(scale))) then
            error stop who // ": the centre must be a nonzero, finite direction"
        end if
        c = centre / scale
        length = sqrt(c(1) * c(1) + c(2) * c(2) + c(3) * c(3))
        c = c / length
    end function sph_unit

    !> The right-handed orthonormal frame `(e1, e2, c)` taking `+z` onto `c`. **Frozen contract.**
    !!
    !! `e1` is the normalised cross product of `c` with the coordinate axis along which the CALLER'S
    !! vector has its smallest component (the lowest such axis on a tie), and `e2 = c x e1`. That axis
    !! is never within 54.7 degrees of `c`, so the cross product never cancels. It is chosen from the
    !! caller's vector rather than from `c` because a positive scaling cannot reorder the components
    !! of the one while the rounding of the other can.
    pure subroutine sph_frame(centre, c, e1, e2)
        real(real64), intent(in) :: centre(3)       !! the caller's vector, which picks the axis
        real(real64), intent(in) :: c(3)            !! its normalisation
        real(real64), intent(out) :: e1(3)          !! the first frame vector, perpendicular to `c`
        real(real64), intent(out) :: e2(3)          !! `c x e1`
        real(real64) :: w(3), length
        integer :: k
        k = 1
        if (abs(centre(2)) < abs(centre(k))) k = 2
        if (abs(centre(3)) < abs(centre(k))) k = 3
        select case (k)
        case (1)
            w = [0.0_real64, c(3), -c(2)]
        case (2)
            w = [-c(3), 0.0_real64, c(1)]
        case default
            w = [c(2), -c(1), 0.0_real64]
        end select
        length = sqrt(w(1) * w(1) + w(2) * w(2) + w(3) * w(3))
        e1 = w / length
        e2 = [c(2) * e1(3) - c(3) * e1(2), c(3) * e1(1) - c(1) * e1(3), c(1) * e1(2) - c(2) * e1(1)]
    end subroutine sph_frame

    !> `(radius, r_inner)` validated: `radius` finite and at least 0, `r_inner` in `[0, radius]`.
    !!
    !! `suffix` is `""` or `"_deg"`, so a message names the arguments as the caller spelled them.
    !!
    !! **Where the radii are ANGLES, `half_turn` is present and `r_inner` above it is refused rather
    !! than clamped.** `radius` above the half turn is the whole sphere and clamping it loses
    !! nothing, but an inner radius clamped to `pi` leaves a ring of zero width whose every draw is
    !! the antipode exactly -- a point mass returned silently for what the caller wrote as an
    !! annulus. The ball's radii are DISTANCES, so it passes no `half_turn` and has no such bound.
    pure function sph_radii(who, suffix, radius, r_inner, half_turn) result(r)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        character(len=*), intent(in) :: suffix       !! `""` for radians, `"_deg"` for degrees
        real(real64), intent(in) :: radius           !! the outer radius
        real(real64), intent(in) :: r_inner          !! the inner radius
        real(real64), intent(in), optional :: half_turn !! `pi` or 180 for an angle; absent for a distance
        real(real64) :: r(2)                         !! `[radius, r_inner]`, unchanged
        if (radius /= radius) then
            error stop who // ": radius" // suffix // " must be finite and at least 0 (got NaN)"
        end if
        if (.not. (radius >= 0.0_real64 .and. radius <= huge(radius))) then
            error stop who // ": radius" // suffix // " must be finite and at least 0 (got " // &
                trim(sph_real_text(radius)) // ")"
        end if
        if (r_inner /= r_inner) then
            error stop who // ": r_inner" // suffix // " must lie in [0, radius" // suffix // &
                "] (got NaN against " // trim(sph_real_text(radius)) // ")"
        end if
        if (.not. (r_inner >= 0.0_real64 .and. r_inner <= radius)) then
            error stop who // ": r_inner" // suffix // " must lie in [0, radius" // suffix // "] (got " // &
                trim(sph_real_text(r_inner)) // " against " // trim(sph_real_text(radius)) // ")"
        end if
        if (present(half_turn)) then
            if (r_inner > half_turn) then
                error stop who // ": r_inner" // suffix // " must not exceed " // trim(sph_real_text(half_turn)) // &
                    " (got " // trim(sph_real_text(r_inner)) // "); a ring whose inner radius passes the half " // &
                    "turn is the antipode alone"
            end if
        end if
        r = [radius, r_inner]
    end function sph_radii

    !> The point at offset `h = 1 - cos(theta)` from `c` and azimuth `2*pi*u2` in the frame `(e1, e2)`.
    !!
    !! `h` is in `[0, 2]`, so `h*(2 - h)` -- the squared sine of `theta` -- is never negative.
    pure function sph_place(c, e1, e2, h, u2) result(v)
        real(real64), intent(in) :: c(3)            !! the centre, a unit vector
        real(real64), intent(in) :: e1(3)           !! the frame's first vector
        real(real64), intent(in) :: e2(3)           !! the frame's second vector
        real(real64), intent(in) :: h               !! `1 - cos` of the angle from `c`, in `[0, 2]`
        real(real64), intent(in) :: u2              !! the azimuth's uniform, in `[0, 1)`
        real(real64) :: v(3)                        !! the unit vector placed there
        real(real64) :: z, s, phi, cp, sp
        z = 1.0_real64 - h
        s = sqrt(h * (2.0_real64 - h))
        phi = sphere_two_pi * u2
        cp = cos(phi)
        sp = sin(phi)
        v = z * c + s * (cp * e1 + sp * e2)
    end function sph_place

    !> A sky position converted to a unit vector in the standard frame, `z = sin(dec)`, validated.
    !!
    !! **A declination of exactly +/-90 is the pole `(0, 0, +/-1)` by rule**, whatever `ra0` says:
    !! `cos(90 * pi/180)` is `6.1e-17` rather than 0, a representation limit no formulation removes,
    !! which is why it is stated as a rule (`pf_angdist_deg` makes the same one).
    pure function sph_centre_radec(who, ra0, dec0) result(c)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        real(real64), intent(in) :: ra0              !! right ascension, degrees; any finite value
        real(real64), intent(in) :: dec0             !! declination, degrees, in `[-90, 90]`
        real(real64) :: c(3)                         !! the unit vector
        real(real64) :: a, d, cd
        if (ra0 /= ra0 .or. dec0 /= dec0) then
            error stop who // ": the centre (ra0, dec0) must be finite with dec0 in [-90, 90] (got " // &
                trim(sph_real_text(ra0)) // ", " // trim(sph_real_text(dec0)) // ")"
        end if
        if (.not. (abs(ra0) <= huge(ra0) .and. dec0 >= -90.0_real64 .and. dec0 <= 90.0_real64)) then
            error stop who // ": the centre (ra0, dec0) must be finite with dec0 in [-90, 90] (got " // &
                trim(sph_real_text(ra0)) // ", " // trim(sph_real_text(dec0)) // ")"
        end if
        if (dec0 == 90.0_real64) then
            c = [0.0_real64, 0.0_real64, 1.0_real64]
        else if (dec0 == -90.0_real64) then
            c = [0.0_real64, 0.0_real64, -1.0_real64]
        else
            d = dec0 * sphere_deg2rad
            a = ra0 * sphere_deg2rad
            cd = cos(d)
            c = [cd * cos(a), cd * sin(a), sin(d)]
        end if
    end function sph_centre_radec

    !> A unit vector read as a sky position in the standard frame, in degrees.
    !!
    !! `dec = atan2(z, hypot(x, y))`, never `asin(z)`, which loses half its digits near a pole. **The
    !! right ascension of a pole is 0 by rule**: `atan2(0, 0)` is prohibited by the standard and
    !! nagfor answers it with a NaN and `IEEE_INVALID`. The `ra >= 360` fold is not redundant: a
    !! right ascension a hair below 0 comes back as exactly 360 once 360 is added.
    pure subroutine sph_radec(v, ra, dec)
        real(real64), intent(in) :: v(3)            !! a unit vector
        real(real64), intent(out) :: ra             !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec            !! declination, degrees, in `[-90, 90]`
        if (v(1) == 0.0_real64 .and. v(2) == 0.0_real64) then
            ra = 0.0_real64
        else
            ra = atan2(v(2), v(1)) * sphere_rad2deg
            if (ra < 0.0_real64) ra = ra + 360.0_real64
            if (ra >= 360.0_real64) ra = 0.0_real64
        end if
        ! `hypot` is scaled against an overflow a unit vector cannot reach and an underflow it can:
        ! within about 1e-162 radians of a pole the squares go subnormal. Above the guard the plain
        ! square root is the same value to a rounding and costs less than half as much; below it
        ! `hypot` still answers, so no direction gains an `IEEE_UNDERFLOW` it did not raise before.
        ! `v` is a unit vector by contract, so neither component can be NaN here.
        if (abs(v(1)) >= sphere_hypot_safe .or. abs(v(2)) >= sphere_hypot_safe) then
            dec = atan2(v(3), sqrt(v(1) * v(1) + v(2) * v(2))) * sphere_rad2deg
        else
            dec = atan2(v(3), hypot(v(1), v(2))) * sphere_rad2deg
        end if
    end subroutine sph_radec

    !> The direction of draw `d` of `(key, stream)`: Archimedes' construction. `key` is derived.
    !!
    !! `z = 2*u1 - 1` is exact, and so are `1 - z` and `1 + z`, so the squared sine is one rounding.
    pure function sph_direction_of_key(key, stream, d) result(v)
        integer(int64), intent(in) :: key           !! the family's derived key
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: d             !! the draw, validated
        real(real64) :: v(3)                        !! a unit vector
        real(real64) :: u1, u2, z, s, phi
        call sph_pair(key, stream, d, u1, u2)
        z = (u1 + u1) - 1.0_real64
        s = sqrt((1.0_real64 - z) * (1.0_real64 + z))
        phi = sphere_two_pi * u2
        v = [s * cos(phi), s * sin(phi), z]
    end function sph_direction_of_key

    !> `pf_random_direction_at`'s value at a validated draw: the one body its tiers share.
    pure function direction_draw(seed, stream, d) result(v)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: d             !! the draw, validated
        real(real64) :: v(3)                        !! a unit vector
        v = sph_direction_of_key(key_from(seed, sphere_direction_label), stream, d)
    end function direction_draw

    !> `pf_random_disc_at`'s value at a validated draw: the one body its tiers and its RA/Dec form share.
    !!
    !! `h = 1 - cos(theta)` is uniform between the ring's two bounds, `2*sin(r_inner/2)**2` and
    !! `2*sin(radius/2)**2`, their difference formed as a product of sines so a thin ring keeps its
    !! width; both radii are clamped to `pi` first.
    pure function disc_draw(who, seed, stream, d, centre, radius, r_inner) result(v)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: stream         !! the stream index
        integer(int64), intent(in) :: d              !! the draw, validated
        real(real64), intent(in) :: centre(3)        !! the disc's centre; any nonzero finite length
        real(real64), intent(in) :: radius           !! the angular radius, radians
        real(real64), intent(in), optional :: r_inner !! the inner angular radius, radians; absent means 0
        real(real64) :: v(3)                         !! a unit vector in the disc or ring
        type(pf_random_disc_cap) :: cap
        ! The prepared form IS this body's first half; going through it is what keeps the two equal
        ! to the bit, rather than two listings written to match (`fortran-gotchas.md`).
        call disc_prepare(cap, who, centre, radius, r_inner)
        v = disc_at_prepared(cap, seed, stream, d)
    end function disc_draw

    !> `pf_random_disc_cap%prepare`'s body: everything about a disc that no draw index can change.
    !!
    !! `intent(inout)` rather than `intent(out)`: a `pure` procedure may not take a polymorphic
    !! `intent(out)` dummy at all, and every component is assigned on every path that returns.
    !!
    !! **The half turn is the antipode BY RULE, at either end of the ring**, rather than by what
    !! `2*sin(r/2)**2` happens to evaluate to there. That versine form is written for accuracy near
    !! `r = 0`, and nothing requires it to give exactly 2 at `r = pi`: **gfortran from `-O2` packs
    !! the two `sin` calls below into one call to glibc's vector sine** (`_ZGVbN2v_sin`, an
    !! `-ftree-vectorize` transformation visible with `nm -u`), which is a few-ulp routine rather
    !! than a correctly rounded one and answers one ulp below 1 for `pi/2`. That leaves
    !! `h_in = 2 - 2**-51`.
    !!
    !! One ulp of `h` is 3e-8 of POSITION at the pole, not one ulp of it, because `sph_place` puts
    !! the point at a transverse offset of `sqrt(h*(2 - h))`: the ring this module's own refusal
    !! message calls "the antipode alone" came out as a circle of that radius, and a disc of radius
    !! `pi` left a hole of it at the antipode. So both ends are stated rather than computed, for
    !! the reason `sph_centre_radec` states one for a declination of +/-90. The product form still
    !! answers for every ring that does not touch the half turn, which is what it is there for.
    pure subroutine disc_prepare(cap, who, centre, radius, r_inner)
        class(pf_random_disc_cap), intent(inout) :: cap !! the cap to fill in
        character(len=*), intent(in) :: who          !! the entry point, for the message
        real(real64), intent(in) :: centre(3)        !! the disc's centre; any nonzero finite length
        real(real64), intent(in) :: radius           !! the angular radius, radians
        real(real64), intent(in), optional :: r_inner !! the inner angular radius, radians; absent means 0
        real(real64) :: rr(2), ro, ri, s
        rr(2) = 0.0_real64
        if (present(r_inner)) rr(2) = r_inner
        rr = sph_radii(who, "", radius, rr(2), sphere_pi)
        cap%c = sph_unit(who, centre)
        call sph_frame(centre, cap%c, cap%e1, cap%e2)
        ro = min(rr(1), sphere_pi)
        ri = min(rr(2), sphere_pi)
        s = sin(0.5_real64 * ri)
        cap%h_in = 2.0_real64 * s * s
        cap%dh = 2.0_real64 * sin(0.5_real64 * (ro + ri)) * sin(0.5_real64 * (ro - ri))
        ! Both radii are already clamped to the half turn, so each test is an equality in effect.
        if (ri >= sphere_pi) cap%h_in = 2.0_real64
        if (ro >= sphere_pi) cap%dh = 2.0_real64 - cap%h_in
        cap%set = .true.
    end subroutine disc_prepare

    !> `pf_random_disc_cap%at`'s body at a validated draw: the block and the placement, nothing else.
    pure function disc_at_prepared(cap, seed, stream, d) result(v)
        class(pf_random_disc_cap), intent(in) :: cap !! a prepared cap
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: stream         !! the stream index
        integer(int64), intent(in) :: d              !! the draw, validated
        real(real64) :: v(3)                         !! a unit vector in the disc or ring
        real(real64) :: u1, u2, h
        call sph_pair(key_from(seed, sphere_disc_label), stream, d, u1, u2)
        h = min(max(cap%h_in + u1 * cap%dh, 0.0_real64), 2.0_real64)
        v = sph_place(cap%c, cap%e1, cap%e2, h, u2)
    end function disc_at_prepared

    !> `pf_random_disc_cap%prepare`: validates and prepares, naming itself in any refusal.
    pure subroutine disc_cap_prepare(self, centre, radius, r_inner)
        class(pf_random_disc_cap), intent(inout) :: self !! the cap; any previous preparation is dropped
        real(real64), intent(in) :: centre(3)        !! the disc's centre; any nonzero finite length
        real(real64), intent(in) :: radius           !! the angular radius, radians; at least 0
        real(real64), intent(in), optional :: r_inner !! the inner angular radius, radians; absent means 0
        call disc_prepare(self, "pf_random_disc_cap%prepare", centre, radius, r_inner)
    end subroutine disc_cap_prepare

    !> `pf_random_disc_cap%at` for an `integer(int32)` stream index. See the generic.
    !!
    !! Sign-extends to `int64` and calls the wide specific, so the two kinds give identical values
    !! and both name `pf_random_disc_cap%at` in a refusal.
    pure function disc_cap_at_i32(self, seed, i, draw) result(v)
        class(pf_random_disc_cap), intent(in) :: self !! a prepared cap
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int32), intent(in) :: i              !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: v(3)                         !! a unit vector in the disc or ring
        v = disc_cap_at_i64(self, seed, int(i, int64), draw)
    end function disc_cap_at_i32

    !> `pf_random_disc_cap%at`: `pf_random_disc_at`'s value at the same coordinates, to the bit.
    pure function disc_cap_at_i64(self, seed, i, draw) result(v)
        class(pf_random_disc_cap), intent(in) :: self !! a prepared cap
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: i              !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: v(3)                         !! a unit vector in the disc or ring
        if (.not. self%set) error stop "pf_random_disc_cap%at: %prepare has not run"
        v = disc_at_prepared(self, seed, i, sph_draw("pf_random_disc_cap%at", draw))
    end function disc_cap_at_i64

    !> `pf_random_disc_cap%is_set`: whether `%prepare` has run.
    pure elemental function disc_cap_is_set(self) result(ok)
        class(pf_random_disc_cap), intent(in) :: self !! the cap
        logical :: ok                                !! `.true.` once prepared
        ok = self%set
    end function disc_cap_is_set

    !> `pf_random_disc_radec_at`'s value at a validated draw: `disc_draw` in degrees and the standard frame.
    pure subroutine disc_radec_draw(who, seed, stream, d, ra0, dec0, radius_deg, ra, dec, r_inner_deg)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: stream         !! the stream index
        integer(int64), intent(in) :: d              !! the draw, validated
        real(real64), intent(in) :: ra0              !! the centre's right ascension, degrees
        real(real64), intent(in) :: dec0             !! the centre's declination, degrees
        real(real64), intent(in) :: radius_deg       !! the angular radius, degrees
        real(real64), intent(out) :: ra              !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec             !! declination, degrees, in `[-90, 90]`
        real(real64), intent(in), optional :: r_inner_deg !! the inner radius, degrees; absent means 0
        real(real64) :: rr(2), c(3), v(3)           ! `c` and `v` named for the explicit-shape dummies they reach
        rr(2) = 0.0_real64
        if (present(r_inner_deg)) rr(2) = r_inner_deg
        rr = sph_radii(who, "_deg", radius_deg, rr(2), 180.0_real64)
        c = sph_centre_radec(who, ra0, dec0)
        v = disc_draw(who, seed, stream, d, c, rr(1) * sphere_deg2rad, rr(2) * sphere_deg2rad)
        call sph_radec(v, ra, dec)
    end subroutine disc_radec_draw

    !> `pf_random_ball_at`'s value at a validated draw.
    !!
    !! The radius is `radius * (q**3 + u*(1 - q**3))**(1/3)` with `q = r_inner/radius`, and
    !! `1 - q**3` is formed as `(1 - q)*(1 + q + q**2)` with `1 - q = (radius - r_inner)/radius`, so a
    !! thin shell keeps its thickness.
    pure function ball_draw(who, seed, stream, d, radius, r_inner) result(p)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: stream         !! the stream index
        integer(int64), intent(in) :: d              !! the draw, validated
        real(real64), intent(in) :: radius           !! the ball's radius
        real(real64), intent(in), optional :: r_inner !! the shell's inner radius; absent means 0
        real(real64) :: p(3)                         !! a point in the ball or shell
        real(real64) :: rr(2), dir(3), u, q, omq, r
        rr(2) = 0.0_real64
        if (present(r_inner)) rr(2) = r_inner
        rr = sph_radii(who, "", radius, rr(2))
        dir = sph_direction_of_key(key_from(seed, sphere_ball_label), stream, d)
        u = to_real64(bits_of(key_from(seed, sphere_ball_radius_label), stream, d, DOM_REAL64))
        if (rr(1) <= 0.0_real64) then               ! 0 exactly: `sph_radii` refused below it
            p = 0.0_real64
        else
            q = rr(2) / rr(1)
            omq = (rr(1) - rr(2)) / rr(1)
            r = rr(1) * ((q * q) * q + u * (omq * (1.0_real64 + q + q * q))) ** sphere_third
            p = r * dir
        end if
    end function ball_draw

    !> The vMF offset `h = 1 - w` from one uniform, validating `kappa`: the inverse CDF of the cosine.
    !!
    !! `h = -log(1 - x)/kappa` with `x = u*(1 - exp(-2*kappa))`, formed in whichever way cancels
    !! nothing. `1 - exp(-2*kappa)` is `2*sinh(kappa)*exp(-kappa)`, exact to rounding at any `kappa`,
    !! and exactly 1 from `kappa = 20` on, where `exp(-40)` is below half an ulp of 1. For `x <= 1/2`
    !! the logarithm is `log1p(-x)`, written `-2*atanh(x/(2 - x))` because Fortran has no `log1p`;
    !! above it, `1 - x` is the sum `(1 - u) + u*exp(-2*kappa)` of two non-negative terms. `kappa = 0`
    !! is the uniform limit `h = 2*u`, taken as its own arm.
    pure function sph_vmf_h(who, kappa, u) result(h)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        real(real64), intent(in) :: kappa            !! the concentration; finite, at least 0
        real(real64), intent(in) :: u                !! the uniform, in `[0, 1)`
        real(real64) :: h                            !! `1 - cos` of the angle from the mean, in `[0, 2]`
        real(real64) :: em, ex, x
        if (kappa /= kappa) then
            error stop who // ": kappa must be finite and at least 0 (got NaN)"
        end if
        if (.not. (kappa >= 0.0_real64 .and. kappa <= huge(kappa))) then
            error stop who // ": kappa must be finite and at least 0 (got " // trim(sph_real_text(kappa)) // ")"
        end if
        if (kappa <= 0.0_real64) then               ! 0 exactly: the validation above refused below it
            h = u + u
            return
        end if
        if (kappa >= 20.0_real64) then
            em = 1.0_real64
            ex = 0.0_real64
        else
            ex = exp(-2.0_real64 * kappa)
            em = 2.0_real64 * sinh(kappa) * exp(-kappa)
        end if
        x = u * em
        if (x <= 0.5_real64) then
            h = 2.0_real64 * atanh(x / (2.0_real64 - x)) / kappa
        else
            h = -log((1.0_real64 - u) + u * ex) / kappa
        end if
        h = min(max(h, 0.0_real64), 2.0_real64)
    end function sph_vmf_h

    !> `pf_random_vmf_at`'s value at a validated draw: the one body its tiers and its RA/Dec form share.
    pure function vmf_draw(who, seed, stream, d, mu, kappa) result(v)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: stream         !! the stream index
        integer(int64), intent(in) :: d              !! the draw, validated
        real(real64), intent(in) :: mu(3)            !! the mean direction; any nonzero finite length
        real(real64), intent(in) :: kappa            !! the concentration
        real(real64) :: v(3)                         !! a unit vector
        real(real64) :: c(3), e1(3), e2(3), u1, u2, h
        c = sph_unit(who, mu)
        call sph_frame(mu, c, e1, e2)
        call sph_pair(key_from(seed, sphere_vmf_label), stream, d, u1, u2)
        h = sph_vmf_h(who, kappa, u1)
        v = sph_place(c, e1, e2, h, u2)
    end function vmf_draw

    !> The concentration of a Gaussian width `sigma_deg`: `1/sigma**2` with `sigma` in radians, validated.
    !!
    !! A `sigma` so large that its square would overflow is the uniform limit, 0; one so small that its
    !! square would underflow is the centre, a `kappa` of `1e300`, whose offsets are below `1e-149`
    !! radians. Both are within an ulp of the draws they replace, and neither raises an IEEE flag:
    !! `huge` would, since the offset `h` it gives is subnormal.
    pure function sph_sigma_kappa(who, sigma_deg) result(kappa)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        real(real64), intent(in) :: sigma_deg        !! the width, degrees; finite, above 0
        real(real64) :: kappa                        !! the concentration, radians**-2
        real(real64) :: s
        if (sigma_deg /= sigma_deg) then
            error stop who // ": sigma_deg must be finite and strictly positive (got NaN)"
        end if
        if (.not. (sigma_deg > 0.0_real64 .and. sigma_deg <= huge(sigma_deg))) then
            error stop who // ": sigma_deg must be finite and strictly positive (got " // &
                trim(sph_real_text(sigma_deg)) // ")"
        end if
        s = sigma_deg * sphere_deg2rad
        if (s >= 1.0e150_real64) then
            kappa = 0.0_real64
        else if (s <= 1.0e-150_real64) then
            kappa = 1.0e300_real64
        else
            kappa = 1.0_real64 / (s * s)
        end if
    end function sph_sigma_kappa

    !> `pf_random_vmf_radec_at`'s value at a validated draw: `vmf_draw` by width, in the standard frame.
    pure subroutine vmf_radec_draw(who, seed, stream, d, ra0, dec0, sigma_deg, ra, dec)
        character(len=*), intent(in) :: who          !! the entry point, for the message
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: stream         !! the stream index
        integer(int64), intent(in) :: d              !! the draw, validated
        real(real64), intent(in) :: ra0              !! the centre's right ascension, degrees
        real(real64), intent(in) :: dec0             !! the centre's declination, degrees
        real(real64), intent(in) :: sigma_deg        !! the per-axis width, degrees
        real(real64), intent(out) :: ra              !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec             !! declination, degrees, in `[-90, 90]`
        real(real64) :: c(3), v(3)                   ! named for the explicit-shape dummies they reach
        c = sph_centre_radec(who, ra0, dec0)
        v = vmf_draw(who, seed, stream, d, c, sph_sigma_kappa(who, sigma_deg))
        call sph_radec(v, ra, dec)
    end subroutine vmf_radec_draw

    !> `pf_random_rotation_at`'s value at a validated draw: Shoemake's uniform quaternion, as a matrix.
    !!
    !! `(x, y, z, w) = (a*sin(t1), a*cos(t1), b*sin(t2), b*cos(t2))` with `a = sqrt(1 - u1)`,
    !! `b = sqrt(u1)`, `t1 = 2*pi*u2`, `t2 = 2*pi*u3`, and `w` the scalar part; `u1, u2` are the
    !! halves of block `d - 1` of the first label and `u3` the uniform at `(key, stream, d)` of the
    !! second. `r(3,3) = 2*u1 - 1` to rounding.
    pure function rotation_draw(seed, stream, d) result(r)
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: stream         !! the stream index
        integer(int64), intent(in) :: d              !! the draw, validated
        real(real64) :: r(3, 3)                      !! a proper rotation matrix
        real(real64) :: u1, u2, u3, a, b, t1, t2, qx, qy, qz, qw
        call sph_pair(key_from(seed, sphere_rotation_label), stream, d, u1, u2)
        u3 = to_real64(bits_of(key_from(seed, sphere_rotation_angle_label), stream, d, DOM_REAL64))
        a = sqrt(1.0_real64 - u1)
        b = sqrt(u1)
        t1 = sphere_two_pi * u2
        t2 = sphere_two_pi * u3
        qx = a * sin(t1)
        qy = a * cos(t1)
        qz = b * sin(t2)
        qw = b * cos(t2)
        r(1, 1) = 1.0_real64 - 2.0_real64 * (qy * qy + qz * qz)
        r(2, 1) = 2.0_real64 * (qx * qy + qz * qw)
        r(3, 1) = 2.0_real64 * (qx * qz - qy * qw)
        r(1, 2) = 2.0_real64 * (qx * qy - qz * qw)
        r(2, 2) = 1.0_real64 - 2.0_real64 * (qx * qx + qz * qz)
        r(3, 2) = 2.0_real64 * (qy * qz + qx * qw)
        r(1, 3) = 2.0_real64 * (qx * qz + qy * qw)
        r(2, 3) = 2.0_real64 * (qy * qz - qx * qw)
        r(3, 3) = 1.0_real64 - 2.0_real64 * (qx * qx + qy * qy)
    end function rotation_draw

    !> `pf_random_fill_direction`'s body: the derived key hoisted, then one scalar draw per column.
    pure subroutine fill_direction(seed, stream, v, draw)
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: stream         !! the stream index
        real(real64), intent(out) :: v(:, :)         !! shaped `(3, n)`
        integer(int64), intent(in), optional :: draw !! the caller's starting draw, present or not
        integer(int64) :: n, k, d0, key
        character(len=24) :: t
        if (size(v, 1, kind=int64) /= 3_int64) then
            write (t, '(i0)') size(v, 1, kind=int64)
            error stop "pf_random_fill_direction: v must be shaped (3, n) (got " // trim(t) // " rows)"
        end if
        n = size(v, 2, kind=int64)
        if (n <= 0_int64) return                    ! a zero-column fill is a defined no-op
        d0 = sph_fill_start("pf_random_fill_direction", draw, n)
        key = key_from(seed, sphere_direction_label)
        do k = 1_int64, n
            v(:, k) = sph_direction_of_key(key, stream, d0 + (k - 1_int64))
        end do
    end subroutine fill_direction

    !> `pf_random_fill_radec`'s body: `fill_direction`'s loop, each direction read as a sky position.
    pure subroutine fill_radec(seed, stream, ra, dec, draw)
        integer(int64), intent(in) :: seed           !! the stream family's seed
        integer(int64), intent(in) :: stream         !! the stream index
        real(real64), intent(out) :: ra(:)           !! right ascensions, degrees
        real(real64), intent(out) :: dec(:)          !! declinations, degrees
        integer(int64), intent(in), optional :: draw !! the caller's starting draw, present or not
        integer(int64) :: n, k, d0, key
        real(real64) :: v(3)                         ! named for `sph_radec`'s explicit-shape dummy
        character(len=24) :: t, t2
        if (size(ra, kind=int64) /= size(dec, kind=int64)) then
            write (t, '(i0)') size(ra, kind=int64)
            write (t2, '(i0)') size(dec, kind=int64)
            error stop "pf_random_fill_radec: ra and dec must have the same size (got " // trim(t) // &
                " and " // trim(t2) // ")"
        end if
        n = size(ra, kind=int64)
        if (n <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        d0 = sph_fill_start("pf_random_fill_radec", draw, n)
        key = key_from(seed, sphere_direction_label)
        do k = 1_int64, n
            v = sph_direction_of_key(key, stream, d0 + (k - 1_int64))
            call sph_radec(v, ra(k), dec(k))
        end do
    end subroutine fill_radec

    ! ================================================================================
    ! Bulk fills
    ! ================================================================================

    !> Fills `v` with consecutive `real64` values of one stream, starting at `draw`.
    !!
    !! Walks blocks rather than values, taking both of a block's pairs where it can, so a fill of
    !! `m` values enciphers about `m/2` blocks where `m` scalar calls would encipher `m`. The
    !! values are identical to those scalar calls either way -- that is what makes a prefix a
    !! prefix.
    !!
    !! **Shape: an alignment head, a TWO-BLOCK steady state, then a one-block and a one-value
    !! tail.** Two blocks rather than one because consecutive Philox blocks are independent, so
    !! enciphering two in the same body interleaves two 10-round dependency chains and fills the
    !! issue slots one chain leaves idle. No value moves -- the same blocks are computed in the
    !! same order. **Two is the measured optimum and more is worse**: machine C measured 1 / 2 / 3 /
    !! 4 blocks at 7.85 / 6.40 / 8.87 / 7.93 ns per value, register pressure giving back more than
    !! the extra parallelism buys past two, and that held whether the extra block state was named
    !! scalars or an array. Re-derive the shape of that curve before changing the count, rather
    !! than the winner.
    !!
    !! **The head is what removes the per-iteration parity test AND the overflow guard this loop
    !! used to need.** Aligning once means the steady state advances `blk` by increment, and `blk`
    !! tops out at `(huge - 1)/2`, so no index here can overflow -- where the previous form derived
    !! a position from `draw + k` each pass and needed a guard against forming `huge + 1` on the
    !! final, dead iteration. `position + 1` in the head cannot overflow either: it fires only when
    !! `position` is odd, and the largest odd `position` is `huge - 2`.
    pure subroutine fill_r64(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: position, blk
        integer(int64) :: w0, w1, w2, w3, x0, x1, x2, x3
        ! Both counters and the length are int64, and `size` is asked for that kind EXPLICITLY.
        ! `size(v)` defaults to a default-kind result, which wraps for an array of 2**31 elements
        ! or more -- and this fails silently rather than loudly: a wrapped negative length returns
        ! through the guard below having written nothing, and a wrapped small positive length
        ! (2**32 + 8 elements yields 8) fills a short prefix and leaves the rest of the caller's
        ! `intent(out)` array undefined. Neither raises anything. A 2**31-element `real64` array is
        ! 17 GB, which is ordinary for the data this library exists to handle, and no unit test can
        ! reach it -- `check_fill_size_kind` in tools/check_source_conventions.py is what keeps this
        ! from regressing, with bench/random_large_fill.sh as the end-to-end proof.
        integer(int64) :: k, m
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        k = 0_int64
        position = draw - 1_int64                   ! 0-based value index; `draw` >= 1, so >= 0
        ! Head: one value when `draw` lands on a block's SECOND pair, after which we are aligned.
        if (iand(position, 1_int64) /= 0_int64) then
            call random_block(seed, stream, ior(DOM_REAL64, ishft(position, -1)), w0, w1, w2, w3)
            k = 1_int64
            v(1) = to_real64(ior(ishft(w3, 32), w2))
            position = position + 1_int64
        end if
        blk = ishft(position, -1)
        do while (k + 4_int64 <= m)                 ! steady state: two blocks, four values
            call random_block(seed, stream, ior(DOM_REAL64, blk), w0, w1, w2, w3)
            call random_block(seed, stream, ior(DOM_REAL64, blk + 1_int64), x0, x1, x2, x3)
            v(k + 1_int64) = to_real64(ior(ishft(w1, 32), w0))
            v(k + 2_int64) = to_real64(ior(ishft(w3, 32), w2))
            v(k + 3_int64) = to_real64(ior(ishft(x1, 32), x0))
            v(k + 4_int64) = to_real64(ior(ishft(x3, 32), x2))
            k = k + 4_int64
            blk = blk + 2_int64
        end do
        do while (k + 2_int64 <= m)                 ! tail: whole blocks
            call random_block(seed, stream, ior(DOM_REAL64, blk), w0, w1, w2, w3)
            v(k + 1_int64) = to_real64(ior(ishft(w1, 32), w0))
            v(k + 2_int64) = to_real64(ior(ishft(w3, 32), w2))
            k = k + 2_int64
            blk = blk + 1_int64
        end do
        if (k < m) then                             ! tail: a final half-block
            call random_block(seed, stream, ior(DOM_REAL64, blk), w0, w1, w2, w3)
            v(m) = to_real64(ior(ishft(w1, 32), w0))
        end if
    end subroutine fill_r64

    !> Fills `v` with consecutive `real32` values of one stream, starting at `draw`.
    !!
    !! Four values to a block, since a `real32` value is one word. Same contract as `fill_r64`:
    !! identical to the matching scalar calls, so prefixes agree.
    !!
    !! **Same three-part shape as `fill_r64` -- head, two-block steady state, tail -- and it is
    !! worth more here than there** (machine C: 4.85 -> 3.15 ns per value, 1.54x, against 1.23x for
    !! `real64`). The extra gain is not extra batching: it is that the steady state no longer runs
    !! the `do while (slot <= 3 .and. k < m)` inner loop this subroutine used to carry on **every**
    !! block. That loop wrote through a local array indexed by a runtime `slot` and tested a
    !! compound condition four times per block, where an aligned block is simply four straight-line
    !! writes from named scalars. The head and tail still need slot handling and still have it, in
    !! the unrolled `if` form -- **`slot <= 0` is deliberately unreachable in the head** (which
    !! fires only for `slot /= 0`), and is written that way so the four lines read as one aligned
    !! pattern rather than three special cases.
    !!
    !! The words are named scalars rather than an array for the reason `random_block`'s header
    !! gives: an array whose subscript is not a compile-time constant is liable to be spilled, and
    !! spilling it here would give back exactly what removing the inner loop won.
    pure subroutine fill_r32(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        real(real32), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: position, blk
        integer(int64) :: c0, c1, c2, c3, d0, d1, d2, d3
        ! int64 counters and an explicit `kind=` on `size`, for the reason spelled out in
        ! `fill_r64`: a default-kind length wraps above 2**31 elements and fails silently, either
        ! writing nothing or writing a short prefix of the caller's `intent(out)` array.
        integer(int64) :: k, m
        integer :: slot                             ! 0..3 within one block; default kind is ample
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        k = 0_int64
        position = draw - 1_int64                   ! 0-based word index; `draw` >= 1, so >= 0
        blk = position / 4_int64
        slot = int(modulo(position, 4_int64), int32)
        ! Head: finish the first block when the start is not block-aligned.
        if (slot /= 0) then
            call random_block(seed, stream, ior(DOM_REAL32, blk), c0, c1, c2, c3)
            if (slot <= 0 .and. k < m) then
                ! Unreachable by construction -- the head runs only for `slot /= 0`, and `slot` is
                ! `modulo(position, 4)`, so it is never negative. Kept, and excluded rather than
                ! deleted, because the four arms are written as one aligned pattern; see this
                ! subroutine's own doc-comment, which says so at the design level.
                ! GCOVR_EXCL_START
                k = k + 1_int64
                v(k) = to_real32(c0)
                ! GCOVR_EXCL_STOP
            end if
            if (slot <= 1 .and. k < m) then
                k = k + 1_int64
                v(k) = to_real32(c1)
            end if
            if (slot <= 2 .and. k < m) then
                k = k + 1_int64
                v(k) = to_real32(c2)
            end if
            if (slot <= 3 .and. k < m) then
                k = k + 1_int64
                v(k) = to_real32(c3)
            end if
            blk = blk + 1_int64
        end if
        do while (k + 8_int64 <= m)                 ! steady state: two blocks, eight values
            call random_block(seed, stream, ior(DOM_REAL32, blk), c0, c1, c2, c3)
            call random_block(seed, stream, ior(DOM_REAL32, blk + 1_int64), d0, d1, d2, d3)
            v(k + 1_int64) = to_real32(c0)
            v(k + 2_int64) = to_real32(c1)
            v(k + 3_int64) = to_real32(c2)
            v(k + 4_int64) = to_real32(c3)
            v(k + 5_int64) = to_real32(d0)
            v(k + 6_int64) = to_real32(d1)
            v(k + 7_int64) = to_real32(d2)
            v(k + 8_int64) = to_real32(d3)
            k = k + 8_int64
            blk = blk + 2_int64
        end do
        do while (k + 4_int64 <= m)                 ! tail: whole blocks
            call random_block(seed, stream, ior(DOM_REAL32, blk), c0, c1, c2, c3)
            v(k + 1_int64) = to_real32(c0)
            v(k + 2_int64) = to_real32(c1)
            v(k + 3_int64) = to_real32(c2)
            v(k + 4_int64) = to_real32(c3)
            k = k + 4_int64
            blk = blk + 1_int64
        end do
        if (k < m) then                             ! tail: a final partial block, at most 3 values
            call random_block(seed, stream, ior(DOM_REAL32, blk), c0, c1, c2, c3)
            if (k < m) then
                k = k + 1_int64
                v(k) = to_real32(c0)
            end if
            if (k < m) then
                k = k + 1_int64
                v(k) = to_real32(c1)
            end if
            if (k < m) then
                k = k + 1_int64
                v(k) = to_real32(c2)
            end if
        end if
    end subroutine fill_r32

    !> Fills `v` with one draw of each of `size(v)` consecutive streams, starting at `i0`.
    !!
    !! **Why this is a plain loop over `random_block` and not a lane-blocked kernel.** Both were
    !! measured on machine B, over ten prototypes gated bit-identical to `pf_random_at` first. The
    !! win here is almost entirely from having a bulk entry point at all -- not from batching:
    !!
    !! | form | gfortran | ifx |
    !! |---|---|---|
    !! | scalar loop (what this replaces) | 19.74 ns | 14.95 ns |
    !! | **this: one stream per body, rounds a loop** | **15.76 (1.25x)** | **9.51 (1.57x)** |
    !! | 4 streams per body, rounds a loop | 19.71 (1.00x) | 11.42 (1.31x) |
    !! | 4 streams per body, rounds UNROLLED | 15.91 (1.24x) | 7.39 (2.02x) |
    !!
    !! So lane blocking is worth **nothing on gfortran** -- whose release profile carries
    !! `-funroll-loops`, and which is therefore indifferent to the round form -- and a further 29 %
    !! on ifx, but only when the ten rounds are also written out. So the rule "must use the
    !! unrolled kernel; with loop rounds it is a 1.3x-1.6x loss" is **an ifx-specific effect**,
    !! which is worth knowing before anyone quotes it as a general rule. Writing four lanes x ten
    !! rounds out costs roughly 800 lines per worker and a second hand-maintained copy of the
    !! cipher, which is exactly the duplication this module
    !! exists to avoid; `random_block`'s own header already schedules lane-blocked kernels for the
    !! phase that introduces the rest of the bulk tier. Take that work there, with a generator, not
    !! here.
    !!
    !! The `second` test is hoisted out of the loop rather than being recomputed per element, and
    !! `blk` with it: both are functions of `draw` alone.
    pure subroutine fill_streams_r64(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        real(real64), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: k, m, blk, w0, w1, w2, w3
        logical :: second                           ! does `draw` sit in its block's second pair?
        ! `size(v, kind=int64)` for the reason `fill_r64` spells out: a default-kind length wraps
        ! above 2**31 elements and fails silently.
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        blk = (draw - 1_int64) / 2_int64
        second = (modulo(draw - 1_int64, 2_int64) == 1_int64)
        ! **`i0 + (k - 1)`, parenthesised, and the brackets are load-bearing.** Fortran evaluates
        ! `i0 + k - 1` left to right, so at the documented boundary -- a fill whose last stream is
        ! exactly `huge(int64)`, which the suite tests -- the intermediate `i0 + k` forms
        ! `huge + 1` and overflows, wraps, and the `- 1` brings it back to the right answer. The
        ! result is correct on both compilers and the arithmetic is undefined, which is precisely
        ! the shape feature_risks.md Risk-94 records the optimiser exploiting two functions away.
        ! Since `k >= 1`, `k - 1` is non-negative and `i0 + (k - 1)` cannot exceed the sum the
        ! precondition already bounds. Found by UBSan; see feature_risks.md Risk-112.
        do k = 1_int64, m
            call random_block(seed, i0 + (k - 1_int64), ior(DOM_REAL64, blk), w0, w1, w2, w3)
            if (second) then
                v(k) = to_real64(ior(ishft(w3, 32), w2))
            else
                v(k) = to_real64(ior(ishft(w1, 32), w0))
            end if
        end do
    end subroutine fill_streams_r64

    !> Fills `v` with one `real32` draw of each of `size(v)` consecutive streams, starting at `i0`.
    !!
    !! One word of four per stream, so this is the least block-efficient entry point in the module
    !! -- see `pf_random_fill_streams`' own note, where that is stated as contract rather than as a
    !! shortcoming. Measured 21.96 -> 14.96 ns (gfortran) and 14.67 -> 9.19 (ifx) against the scalar
    !! loop it replaces. Same shape as `fill_streams_r64`: `blk` and `slot` are functions of `draw`
    !! alone and are hoisted -- and one stream per body wins here too, beating the 2- and 4-lane
    !! prototypes on both compilers, so the `real64` verdict below carries across unchanged.
    pure subroutine fill_streams_r32(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        real(real32), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: k, m, blk, c0, c1, c2, c3
        integer :: slot                             ! 0..3 within one block; default kind is ample
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        blk = (draw - 1_int64) / 4_int64
        slot = int(modulo(draw - 1_int64, 4_int64), int32)
        do k = 1_int64, m
            call random_block(seed, i0 + (k - 1_int64), ior(DOM_REAL32, blk), c0, c1, c2, c3)
            select case (slot)
            case (0)
                v(k) = to_real32(c0)
            case (1)
                v(k) = to_real32(c1)
            case (2)
                v(k) = to_real32(c2)
            case default
                v(k) = to_real32(c3)
            end select
        end do
    end subroutine fill_streams_r32

    !> Fills `v` with consecutive `integer(int64)` draws of one stream, starting at `draw`.
    !!
    !! **One enciphering serves two draws.** An integer draw has stride 2, exactly like its `real64`
    !! sibling, so draws `d` and `d+1` for odd `d` are the two pairs of one block. The values are
    !! identical to a loop of `int_at_impl` by construction -- the same blocks in the same order --
    !! and the `lo`/`hi` normalisation is done once per call rather than once per element.
    !!
    !! **Shape: an alignment head, a ONE-BLOCK steady state, then a one-value tail** -- the
    !! `fill_r64` shape, minus its two-block interleave. Aligning once is what removes the whole of
    !! the per-element index arithmetic this loop used to carry: a signed division `(d-1)/2`, a
    !! `modulo(d-1, 2)`, and a `blk /= held` test whose real cost was not the compare but the
    !! loop-carried dependency it put on the block state, which stops the compiler overlapping one
    !! iteration's enciphering with the next.
    !!
    !! **Measured in the library, before and after, on machine B (gfortran 15.2.1, `--profile
    !! release`, 10M values, best of 5): 15.72 -> 12.04 ns per value, 1.31x**, with every untouched
    !! arm flat across the two builds (`fill_r64` 8.67/8.68, the cipher floors 12.04/12.06 and
    !! 6.05/6.05) so the cross-build noise floor is about 0.5 % and the gain is 30 times it. Probe
    !! arms predicted 1.47x-1.67x; the library form does not reach that, and the difference is the
    !! usual one between a contained procedure the compiler may specialise freely and a module
    !! procedure it may not. **Quote 1.31x, not the prediction.**
    !!
    !! **A second measurement is load-bearing and must be repeated after any change here: the SCALAR
    !! draw.** `pf_random_int_at` shares `int_reduce` with this loop, and the first version of this
    !! restructure made it 9 % slower without touching it -- see `int_reduce_retry` for the mechanism
    !! and the one-command check. It now sits within 1.2 % (25.6 -> 25.9), which is the cost of the
    !! cold retry call and is the price of the 31 % here.
    !!
    !! **The interleave is deliberately NOT ported, and this is the one place the `real64` sibling
    !! must not be copied wholesale.** Two blocks per body is worth 1.23x there and is a *regression*
    !! here: measured 9.79 against 10.09 ns per value on machine B under gfortran, reproducibly and
    !! far above that machine's 0.5 % floor. The reason is visible in the arithmetic -- the interleave
    !! exists to fill issue slots one 10-round Philox chain leaves idle, and on the `real64` path the
    !! only post-cipher work is a single multiply, so they really are idle; here the Lemire reduction
    !! is already the second instruction stream. Machine A measures the same change +4 %, so its
    !! *sign* differs by architecture, which is on its own a reason not to carry it.
    !!
    !! A rejection is never served from the block in hand: `int_reduce` re-keys and goes back to
    !! `bits_of` under a derived key, exactly as the scalar entry point does. There is still exactly
    !! one copy of the rejection rule, which is what that split is for.
    !!
    !! An earlier revision could do none of this, because the integer generic then had stride 4 and
    !! consumed a whole block per value; that comment said a block-walking form "returns different
    !! values" -- true then, and no longer true now that the strides agree.
    pure subroutine fill_draws_i64(seed, stream, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: k, m, a, s, position, blk
        integer(int64) :: w0, w1, w2, w3
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        a = min(lo, hi)
        s = width_of(a, max(lo, hi))
        if (narrow32_ok(s)) then                    ! the 32-bit grid; see NARROW32_CAP
            call fill_draws32_i64(seed, stream, v, a, s, draw)
            return
        end if
        k = 0_int64
        position = draw - 1_int64                   ! 0-based value index; `draw` >= 1, so >= 0
        ! Head: one value when `draw` lands on a block's SECOND pair, after which we are aligned.
        if (iand(position, 1_int64) /= 0_int64) then
            call random_block(seed, stream, ior(DOM_INT_WIDE, ishft(position, -1)), w0, w1, w2, w3)
            k = 1_int64
            v(1) = int_reduce(ior(ishft(w3, 32), w2), a, s, seed, stream, draw)
            position = position + 1_int64
        end if
        blk = ishft(position, -1)
        ! Every draw index below is `draw + (something <= m - 1)`, with the inner sum parenthesised,
        ! so none can form `huge + 1` even when the fill ends exactly at the representable boundary
        ! -- the hazard `fill_streams_r64` spells out at length. Risk-112.
        do while (k + 2_int64 <= m)                 ! steady state: one block, two values
            call random_block(seed, stream, ior(DOM_INT_WIDE, blk), w0, w1, w2, w3)
            v(k + 1_int64) = int_reduce(ior(ishft(w1, 32), w0), a, s, seed, stream, draw + k)
            v(k + 2_int64) = int_reduce(ior(ishft(w3, 32), w2), a, s, seed, stream, &
                                        draw + (k + 1_int64))
            k = k + 2_int64
            blk = blk + 1_int64
        end do
        if (k < m) then                             ! tail: a final half-block
            call random_block(seed, stream, ior(DOM_INT_WIDE, blk), w0, w1, w2, w3)
            v(m) = int_reduce(ior(ishft(w1, 32), w0), a, s, seed, stream, draw + (m - 1_int64))
        end if
    end subroutine fill_draws_i64

    !> `fill_draws_i64` narrowed to `integer(int32)`.
    !!
    !! The result is inside `[min(lo,hi), max(lo,hi)]` by construction, so the narrowing is exact --
    !! the same argument `pf_random_int_at_i32` rests on. The head/steady-state/tail shape is the one
    !! `fill_draws_i64` documents, written out again rather than shared, because sharing it would
    !! mean materialising an `integer(int64)` temporary the size of `v`.
    pure subroutine fill_draws_i32(seed, stream, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int32), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: k, m, a, s, position, blk
        integer(int64) :: w0, w1, w2, w3
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        a = int(min(lo, hi), int64)
        s = width_of(a, int(max(lo, hi), int64))
        if (narrow32_ok(s)) then                    ! the 32-bit grid; see NARROW32_CAP
            call fill_draws32_i32(seed, stream, v, a, s, draw)
            return
        end if
        k = 0_int64
        position = draw - 1_int64                   ! 0-based value index; `draw` >= 1, so >= 0
        if (iand(position, 1_int64) /= 0_int64) then            ! head: see `fill_draws_i64`
            call random_block(seed, stream, ior(DOM_INT_WIDE, ishft(position, -1)), w0, w1, w2, w3)
            k = 1_int64
            v(1) = int(int_reduce(ior(ishft(w3, 32), w2), a, s, seed, stream, draw), int32)
            position = position + 1_int64
        end if
        blk = ishft(position, -1)
        do while (k + 2_int64 <= m)                 ! steady state: one block, two values
            call random_block(seed, stream, ior(DOM_INT_WIDE, blk), w0, w1, w2, w3)
            v(k + 1_int64) = int(int_reduce(ior(ishft(w1, 32), w0), a, s, seed, stream, &
                                            draw + k), int32)
            v(k + 2_int64) = int(int_reduce(ior(ishft(w3, 32), w2), a, s, seed, stream, &
                                            draw + (k + 1_int64)), int32)
            k = k + 2_int64
            blk = blk + 1_int64
        end do
        if (k < m) then                             ! tail: a final half-block
            call random_block(seed, stream, ior(DOM_INT_WIDE, blk), w0, w1, w2, w3)
            v(m) = int(int_reduce(ior(ishft(w1, 32), w0), a, s, seed, stream, &
                                  draw + (m - 1_int64)), int32)
        end if
    end subroutine fill_draws_i32

    !> Fills `v` with one `integer(int64)` draw of each of `size(v)` consecutive streams.
    !!
    !! Unlike `fill_draws_i64` this gives up nothing at all against the real-valued form on the same
    !! axis: that one already enciphered a block per value, because each element belongs to a
    !! different stream.
    !!
    !! **Do not "fix" this to hoist the loop-invariant setup out of the loop -- it was implemented,
    !! measured and reverted.** The body below re-derives, per element, the range normalisation
    !! `min(lo,hi)`/`width_of(...)` inside `int_at_impl`, and the block index and pair parity inside
    !! `bits_of`, all of which are constant across the loop; both `real64` siblings hoist exactly
    !! these and say so, so the asymmetry reads as an oversight. It is not, because **GCC already
    !! does it**: these private workers inline into the public specific (there is no
    !! `__parquet_random_MOD_fill_streams_i64` symbol at all), after which loop-invariant code motion
    !! lifts the whole setup. Hoisting by hand took a 373-instruction loop body to 364 -- neither
    !! version has a 128-bit subtract or a division inside the loop -- and measured 17.63 -> 17.54 ns
    !! per value on machine B (**1.005x**), with `fill_streams_i32` unchanged at 17.52 -> 17.53 and an
    !! untouched `fill_streams_r64` control flat at 14.71 across the two builds.
    !!
    !! The probe arms that predicted 1.04x-1.29x were comparing a *hoisted probe* arm against this
    !! *unhoisted library* one across the wrapper boundary, with no unhoisted probe arm to subtract;
    !! one added afterwards to settle it measures 25.12 against the hoisted 25.23, i.e. zero there
    !! too, and the same holds on both routes. A compiler that does NOT inline
    !! these workers would change the verdict, so this is a finding about the build and not about the
    !! source -- re-measure rather than assuming either answer.
    pure subroutine fill_streams_i64(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        integer(int64), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: k, m
        integer(int64) :: a, sw, st, nblk, xw, x0, x1, x2, x3
        integer :: nj
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        ! The narrow rule must be reached INLINE here, not through `int_at_impl`. That procedure
        ! keeps its own narrow arm out of line so the scalar entry point stays inlined, and a
        ! per-element call to it costs this loop 60 % (17.0 -> 27.2 ns per value, measured). The
        ! stream axis gains only the cheaper reduction -- one block per value either way -- so this
        ! is a small win here and a large loss if forgotten.
        a = min(lo, hi)
        sw = width_of(a, max(lo, hi))
        if (narrow32_ok(sw)) then
            ! `random_block` DIRECTLY, not via `bits32_of` -- that helper has three-plus call sites
            ! and GCC emits it out of line, which costs this loop a call per element (17.0 -> 27.2
            ! ns per value, measured). `fill_draws32_i64`'s steady state avoids it for the same
            ! reason. Block and word are functions of `draw` alone, so both hoist.
            nblk = ishft(draw - 1_int64, -2)
            nj = int(iand(draw - 1_int64, 3_int64), int32)
            do k = 1_int64, m
                st = i0 + (k - 1_int64)
                call random_block(seed, st, ior(DOM_INT_NARROW, nblk), x0, x1, x2, x3)
                select case (nj)
                case (0)
                    xw = x0
                case (1)
                    xw = x1
                case (2)
                    xw = x2
                case default
                    xw = x3
                end select
                v(k) = int_reduce32(xw, a, sw, seed, st, draw)
            end do
            return
        end if
        do k = 1_int64, m
            v(k) = int_at_impl(seed, i0 + (k - 1_int64), lo, hi, draw)
        end do
    end subroutine fill_streams_i64

    !> `fill_streams_i64` narrowed to `integer(int32)`; exact for the same reason.
    pure subroutine fill_streams_i32(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        integer(int32), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: k, m, a, b
        integer(int64) :: lw, sw, st, nblk, xw, x0, x1, x2, x3
        integer :: nj
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        a = int(lo, int64)
        b = int(hi, int64)
        lw = min(a, b)                              ! see `fill_streams_i64` for why this is inline
        sw = width_of(lw, max(a, b))
        if (narrow32_ok(sw)) then
            nblk = ishft(draw - 1_int64, -2)        ! see `fill_streams_i64` for why not `bits32_of`
            nj = int(iand(draw - 1_int64, 3_int64), int32)
            do k = 1_int64, m
                st = i0 + (k - 1_int64)
                call random_block(seed, st, ior(DOM_INT_NARROW, nblk), x0, x1, x2, x3)
                select case (nj)
                case (0)
                    xw = x0
                case (1)
                    xw = x1
                case (2)
                    xw = x2
                case default
                    xw = x3
                end select
                v(k) = int(int_reduce32(xw, lw, sw, seed, st, draw), int32)
            end do
            return
        end if
        do k = 1_int64, m
            v(k) = int(int_at_impl(seed, i0 + (k - 1_int64), a, b, draw), int32)
        end do
    end subroutine fill_streams_i32

    ! ================================================================================
    ! The integer rule -- exact rejection
    ! ================================================================================

    !> A uniform integer in `[min(lo,hi), max(lo,hi)]`, exactly unbiased.
    !!
    !! Lemire's multiply-shift with the exact rejection test, over the same 64-bit pattern
    !! `pf_random_bits_at` returns at this coordinate; `x * s` is formed as a full 128-bit product,
    !! whose high half is the answer's offset and whose low half decides acceptance. Only the last
    !! `2**64 mod s` candidates of the range are rejected, which is what makes the result exactly
    !! uniform rather than uniform to within 2**-64. The reduction itself lives in `int_reduce`, so
    !! that the bulk fills can reuse it against a candidate they enciphered once for two draws.
    !!
    !! A rejection RE-KEYS and re-enciphers the SAME counter, so consumption stays fixed in counter
    !! positions -- block ownership and prefix consistency are untouched and the answer is still a
    !! pure function of `(seed, i, draw)`. The loop is deliberately uncapped: accepting a candidate
    !! at a cap would reintroduce the bias the whole scheme exists to remove. It terminates with
    !! probability 1, the worst chain measured is 7, and at a range of a few million the retry
    !! probability is around 2**-40.
    !!
    !! **This reads exactly the two words `pf_random_at` and `pf_random_bits_at` read at the SAME
    !! coordinate -- stride 2, the same grid** -- so all three 64-bit generics agree on what draw
    !! `d` means, and separating two of them on the draw axis really does separate them. That is a
    !! deliberate contract choice, not an accident of the implementation; `pf_random32_at` still
    !! walks a finer grid of its own, so the full rule is stated once on the `pf_random_at`
    !! interface above and in `doc/pages/utilities/random.md`.
    !!
    !! An earlier revision gave this generic **stride 4**: draw `d` addressed the whole of block
    !! `d-1` and used only its first pair. That made `pf_random_int_at(seed, i, lo, hi, d)` read the
    !! same 64 bits as `pf_random_bits_at(seed, i, 2d-1)` -- verified 1000 of 1000 -- so an integer
    !! at draw 2 and a real at draw 3 were the same randomness, and "walk the draw axis" was unsafe
    !! for the next pairing anybody would write after the one the guide illustrated. It also left
    !! half of every enciphering unused, which is why a draw-axis integer fill could not amortise.
    !! Both are fixed by this mapping. Draw 1 is unchanged by the switch (block 0, first pair, under
    !! either rule); every draw from 2 up moved, which is why `pf_random_algorithm` is at `/v2`.
    !!
    !! What remains, and is contract: at ONE coordinate the three 64-bit generics are three
    !! *presentations* of the same 64 bits, not independent draws. The returned value still differs,
    !! because Lemire's reduction is a different function of those bits; but "different value" is
    !! not "independent", and at a small range the integer is a **deterministic function** of the
    !! real -- `pf_random_int_at(seed, i, 1, 6)` equals `1 + floor(6 * pf_random_at(seed, i))` for
    !! 20000 of 20000 streams. The rejection clause cannot rescue that, since at a realistic range
    !! the retry probability is around 2**-40, so the no-rejection case is effectively the only
    !! case. Take the two at different draws, or on different streams.
    pure function int_at_impl(seed, stream, lo, hi, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: a, s

        a = min(lo, hi)                             ! `lo > hi` swaps: the function is total
        s = width_of(a, max(lo, hi))                ! the width, read as an UNSIGNED 64-bit pattern
        if (narrow32_ok(s)) then
            ! **Out of line deliberately, and this shape was chosen by measurement over two
            ! rivals.** Inlining the narrow arm here puts two complete Philox encipherings in one
            ! body; GCC then gives up and pushes `int_at_impl` itself out of line, costing the WIDE
            ! scalar draw 13 %. Hoisting the enciphering above the fork so there is one
            ! `random_block` site and no call at all -- which sounds strictly better -- makes the
            ! body bigger still and costs gfortran the same 13 % while saving ifx 4 %. This shape
            ! costs gfortran 3.6 % and ifx 11.7 % on the wide path, which is the best worst case of
            ! the three, over all six measured shapes. See `feature_risks.md` Risk-115 for why
            ! nothing here may be "simplified" without re-measuring both compilers.
            r = int_at_narrow32(seed, stream, a, s, draw)
            return
        end if
        r = int_reduce(bits_of(seed, stream, draw, DOM_INT_WIDE), a, s, seed, stream, draw)
    end function int_at_impl

    !> Lemire's reduction with the exact rejection loop, over a candidate already in hand.
    !!
    !! Split out from `int_at_impl` so that `fill_draws_i64`/`fill_draws_i32` can supply a candidate
    !! they enciphered once for two draws. There is exactly one copy of the rejection rule, which is
    !! the point: a second copy could drift, and a drifted rejection rule is a biased generator that
    !! passes every structural test.
    !!
    !! A rejection re-keys and re-enciphers the same counter, so it is `bits_of` under a derived key
    !! -- consumption stays fixed in counter positions, and the answer is still a pure function of
    !! `(seed, stream, draw)`. Two draws sharing a block also share their retry blocks, at the two
    !! pairs they already occupy, so no new correlation is introduced by the sharing.
    pure function int_reduce(x0, a, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: x0            !! the first candidate's 64-bit pattern
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, as an unsigned pattern; 0 = full
        integer(int64), intent(in) :: seed          !! the stream family's seed, for a re-key
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: low, high

        if (s == 0_int64) then
            ! The whole int64 range: every pattern is in range, so there is nothing to reduce and
            ! nothing to reject. This is exactly `pf_random_bits_at` at the same coordinate.
            r = x0
            return
        end if

        call mulhilo64(x0, s, low, high)
        if (ult(low, s)) then
            ! Lemire's LAZY GUARD, and it is deliberately conservative: it fires whenever the
            ! candidate *might* be in the last partial block, and `int_reduce_retry` then computes
            ! the real threshold and decides. Being too eager costs a cold call; it cannot bias the
            ! result, because nothing here accepts or rejects anything.
            r = int_reduce_retry(x0, a, s, seed, stream, draw)
            return
        end if
        r = offset_by(a, high)
    end function int_reduce

    !> The rejection rule: the threshold, the retry loop, and the only copy of either.
    !!
    !! **Split out of `int_reduce` for a codegen reason, and the split is load-bearing.** This body
    !! contains `bits_of`, i.e. a whole Philox enciphering, so inlining it is cheap while a caller
    !! has one hot reduction site and ruinous when it has four. When `fill_draws_i64` was
    !! restructured into a two-values-per-block body, GCC's inline budget gave out and it emitted an
    !! out-of-line `int_reduce.isra.0` that the *scalar* entry point then had to call: measured
    !! 25.6 -> 28.0 ns per value for `pf_random_int_at` in a loop, a 9 % regression on public API
    !! caused by a change that touched only the bulk fill. With the cold half behind its own
    !! procedure, `int_reduce` is a multiply, a compare and an add, and inlines at every site again.
    !!
    !! **Check it after any change here** -- this must print nothing:
    !!
    !! ```bash
    !! objdump -d --no-show-raw-insn build/gfortran_*/parquet-fortran/src_parquet_random.f90.o \
    !!   | awk '/<__parquet_random_MOD_pf_random_int_at_i64>:/{p=1} p&&/^$/{exit} p' | grep call
    !! ```
    !!
    !! A rejection re-keys and re-enciphers the same counter, so it is `bits_of` under a derived key
    !! -- consumption stays fixed in counter positions, and the answer is still a pure function of
    !! `(seed, stream, draw)`. The loop is deliberately uncapped: accepting a candidate at a cap
    !! would reintroduce the bias the whole scheme exists to remove. It terminates with probability
    !! 1, and the worst chain measured is 7.
    pure function int_reduce_retry(x0, a, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: x0            !! the first candidate's 64-bit pattern
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, as an unsigned pattern; non-zero
        integer(int64), intent(in) :: seed          !! the stream family's seed, for a re-key
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: x, low, high, threshold, attempt
        ! Keeping this OUT of line is the whole point of the split; with one call site an optimiser
        ! will otherwise inline it straight back into `int_reduce` and undo it. Both directives are
        ! ordinary comments to a compiler that does not know them, so neither is a portability
        ! hazard, and a compiler that ignores both is merely back to the slower shape.
!GCC$ ATTRIBUTES noinline :: int_reduce_retry
!DIR$ ATTRIBUTES NOINLINE :: int_reduce_retry

        attempt = 0_int64
        x = x0
        call mulhilo64(x, s, low, high)
        ! The division is reached only when the caller's lazy guard fired, which at a realistic
        ! width happens with probability about 2**-40. Do not hoist it into `int_reduce`.
        threshold = umod_2p64(s)
        do while (ult(low, threshold))
            attempt = attempt + 1_int64
            x = bits_of(retry_key_of(seed, attempt), stream, draw, DOM_INT_WIDE)
            call mulhilo64(x, s, low, high)
        end do
        r = offset_by(a, high)
    end function int_reduce_retry

    !> `fill_draws_i64` on the 32-bit grid: FOUR values per enciphering.
    !!
    !! Same head/steady-state/tail shape as the 64-bit form, with the alignment now to a block of
    !! four rather than a pair. The head enciphers its block once per value, which costs at most
    !! three redundant encipherings for the whole call and keeps the steady state branch-free.
    pure subroutine fill_draws32_i64(seed, stream, v, a, s, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, `1 .. NARROW32_CAP`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: k, m, position, blk, w0, w1, w2, w3
        m = size(v, kind=int64)
        k = 0_int64
        position = draw - 1_int64
        do while (k < m .and. iand(position, 3_int64) /= 0_int64)        ! head, up to three values
            v(k + 1_int64) = int_reduce32(bits32_of(seed, stream, draw + k), a, s, seed, stream, draw + k)
            k = k + 1_int64
            position = position + 1_int64
        end do
        blk = ishft(position, -2)
        do while (k + 4_int64 <= m)                 ! steady state: one block, four values
            call random_block(seed, stream, ior(DOM_INT_NARROW, blk), w0, w1, w2, w3)
            v(k + 1_int64) = int_reduce32(w0, a, s, seed, stream, draw + k)
            v(k + 2_int64) = int_reduce32(w1, a, s, seed, stream, draw + (k + 1_int64))
            v(k + 3_int64) = int_reduce32(w2, a, s, seed, stream, draw + (k + 2_int64))
            v(k + 4_int64) = int_reduce32(w3, a, s, seed, stream, draw + (k + 3_int64))
            k = k + 4_int64
            blk = blk + 1_int64
        end do
        do while (k < m)                            ! tail, up to three values
            v(k + 1_int64) = int_reduce32(bits32_of(seed, stream, draw + k), a, s, seed, stream, draw + k)
            k = k + 1_int64
        end do
    end subroutine fill_draws32_i64

    !> `fill_draws32_i64` narrowed to `integer(int32)`.
    pure subroutine fill_draws32_i32(seed, stream, v, a, s, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int32), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, `1 .. NARROW32_CAP`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: k, m, position, blk, w0, w1, w2, w3
        m = size(v, kind=int64)
        k = 0_int64
        position = draw - 1_int64
        do while (k < m .and. iand(position, 3_int64) /= 0_int64)
            v(k + 1_int64) = int(int_reduce32(bits32_of(seed, stream, draw + k), a, s, seed, stream, draw + k), int32)
            k = k + 1_int64
            position = position + 1_int64
        end do
        blk = ishft(position, -2)
        do while (k + 4_int64 <= m)
            call random_block(seed, stream, ior(DOM_INT_NARROW, blk), w0, w1, w2, w3)
            v(k + 1_int64) = int(int_reduce32(w0, a, s, seed, stream, draw + k), int32)
            v(k + 2_int64) = int(int_reduce32(w1, a, s, seed, stream, &
                                              draw + (k + 1_int64)), int32)
            v(k + 3_int64) = int(int_reduce32(w2, a, s, seed, stream, &
                                              draw + (k + 2_int64)), int32)
            v(k + 4_int64) = int(int_reduce32(w3, a, s, seed, stream, &
                                              draw + (k + 3_int64)), int32)
            k = k + 4_int64
            blk = blk + 1_int64
        end do
        do while (k < m)
            v(k + 1_int64) = int(int_reduce32(bits32_of(seed, stream, draw + k), a, s, seed, stream, draw + k), int32)
            k = k + 1_int64
        end do
    end subroutine fill_draws32_i32

    !> The scalar narrow draw, kept out of line on purpose.
    !!
    !! See `int_at_impl`'s call site for why. The cost is one call on a path that then enciphers a
    !! whole Philox block, so it is small in relative terms; the alternative costs every *wide*
    !! scalar draw as well, which is far worse.
    pure function int_at_narrow32(seed, stream, a, s, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, `1 .. NARROW32_CAP`
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
!GCC$ ATTRIBUTES noinline :: int_at_narrow32
!DIR$ ATTRIBUTES NOINLINE :: int_at_narrow32

        r = int_reduce32(bits32_of(seed, stream, draw), a, s, seed, stream, draw)
    end function int_at_narrow32

    !> Whether the 32-bit candidate rule may serve this width.
    pure function narrow32_ok(s) result(ok)
        integer(int64), intent(in) :: s             !! the width, as an unsigned pattern; 0 = full
        logical :: ok                               !! `.true.` when a 32-bit candidate suffices
        ok = (s >= 1_int64 .and. s <= NARROW32_CAP)
    end function narrow32_ok

    !> The 32-bit word this draw addresses: block `(d-1)/4`, word
    !! `(d-1) mod 4` -- which is exactly the grid `pf_random32_at` walks.
    pure function bits32_of(seed, stream, draw) result(x)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index; `< 1` clamps to 1
        integer(int64) :: x                         !! the 32-bit candidate, in `[0, 2**32)`
        integer(int64) :: e, w0, w1, w2, w3
        e = max(draw, 1_int64) - 1_int64
        call random_block(seed, stream, ior(DOM_INT_NARROW, ishft(e, -2)), w0, w1, w2, w3)
        select case (int(iand(e, 3_int64), int32))
        case (0)
            x = w0
        case (1)
            x = w1
        case (2)
            x = w2
        case default
            x = w3
        end select
    end function bits32_of

    !> Lemire's reduction over a 32-bit candidate, exactly unbiased.
    !!
    !! `x < 2**32` and `s <= 2**24`, so `x * s` is below `2**56` and is an ordinary signed multiply:
    !! no 128-bit product, no unsigned compare, no fold back into an `int64` pattern. That is why
    !! this lever is worth more than the halved cipher work alone. The rejection test is the exact
    !! 32-bit analogue -- accept iff `low32(x*s) >= 2**32 mod s` -- so the result is exactly uniform
    !! rather than uniform to within `2**-32`.
    !!
    !! Split hot/cold exactly as `int_reduce`/`int_reduce_retry` are, and for the same reason: an
    !! A/B that let this one inline differently from the rule it is being compared against would be
    !! measuring the inline budget rather than the grid. See `feature_risks.md` Risk-114.
    !! **The threshold is LAZY, exactly as the 64-bit rule's is, and this is not a detail.** An
    !! eager `modulo(2**32, s)` is an integer division on every call. A bulk fill hoists it once and
    !! never notices; `pf_random_int_at` cannot, and paying it per call measured a **6 % regression
    !! on the scalar draw at every width, including widths the narrow grid never serves**. Deferring
    !! it behind the same conservative guard `int_reduce` uses -- fire whenever the candidate
    !! *might* be in the last partial block -- costs nothing and cannot bias anything, because
    !! nothing here accepts or rejects.
    pure function int_reduce32(x, a, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: x             !! the 32-bit candidate
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, `1 .. NARROW32_CAP`
        integer(int64), intent(in) :: seed          !! the stream family's seed, for a re-key
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: p
        p = x * s
        if (iand(p, M32) < s) then                  ! lazy guard: `int_reduce`'s, at 32 bits
            r = int_reduce32_retry(a, s, seed, stream, draw)
            return
        end if
        r = a + ishft(p, -32)
    end function int_reduce32

    !> The 32-bit threshold and rejection loop, kept out of line.
    pure function int_reduce32_retry(a, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width
        integer(int64), intent(in) :: seed          !! the stream family's seed, for a re-key
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: p, thr, attempt
!GCC$ ATTRIBUTES noinline :: int_reduce32_retry
!DIR$ ATTRIBUTES NOINLINE :: int_reduce32_retry

        ! Reached only when the caller's lazy guard fired, so the division is off the hot path.
        thr = modulo(TWO32, s)                      ! `2**32 mod s`; both operands positive
        p = bits32_of(seed, stream, draw) * s
        attempt = 0_int64
        do while (iand(p, M32) < thr)
            attempt = attempt + 1_int64
            ! Re-key and re-encipher the SAME counter, so consumption stays fixed in counter
            ! positions -- the rule `int_reduce_retry` follows, at the 32-bit grid's coordinate.
            p = bits32_of(retry_key_of(seed, attempt), stream, draw) * s
        end do
        r = a + ishft(p, -32)
    end function int_reduce32_retry

    !> The retry key for attempt `n` (1-based): a re-key, never a tweak.
    !!
    !! Two simpler spellings were measured and both alias real seed pairs -- an additive tweak
    !! collides one fixed-offset partner on a third of wide-range draws, and `mix64(ieor(seed, n))`
    !! collides EVERY consecutive (even, odd) seed pair, which is exactly what `seed = base + i`
    !! produces. This form measures zero agreements on every pair tested. It is contract, and must
    !! not be simplified.
    pure function retry_key_of(seed, n) result(k)
        integer(int64), intent(in) :: seed          !! the original seed
        integer(int64), intent(in) :: n             !! attempt number, 1 for the first retry
        integer(int64) :: k                         !! the key to re-encipher this counter under
        k = mix64(ieor(mix64(seed), ieor(n, RETRY_TAG)))
    end function retry_key_of

    !> `a - b` as a 64-bit pattern, computed on 32-bit halves so that nothing overflows.
    !!
    !! See `width_of` for why this exists rather than a plain `a - b`.
    !!
    !! **This and `add64` are called only from the `#else` arm of the route (e) fork, so on a
    !! compiler that has a 128-bit kind they are compiled and never entered.** Coverage here is
    !! measured under gfortran, which has one, so both will always report as uncovered -- and both
    !! are live under ifx, where they are the fix for the branch-deletion defect `width_of`
    !! describes. Neither is dead code; do not delete either on the strength of a coverage report.
    !! What asserts them is the suite's cross-implementation agreement sweep, run by a compiler that
    !! takes this arm.
    ! GCOVR_EXCL_START
    pure function sub64(a, b) result(r)
        integer(int64), intent(in) :: a             !! left operand, read as a bit pattern
        integer(int64), intent(in) :: b             !! right operand, read as a bit pattern
        integer(int64) :: r                         !! `a - b` modulo 2**64
        integer(int64) :: low, high
        low = iand(a, M32) - iand(b, M32)           ! strictly inside (-2**32, 2**32)
        high = ishft(a, -32) - ishft(b, -32)        ! both shifts are logical, so both are >= 0
        if (low < 0_int64) then
            low = low + 4294967296_int64
            high = high - 1_int64
        end if
        r = ior(ishft(iand(high, M32), 32), low)
    end function sub64
    ! GCOVR_EXCL_STOP

    !> `a + b` as a 64-bit pattern, computed on 32-bit halves so that nothing overflows.
    !!
    !! Reached only from the fork's `#else` arm, exactly as `sub64` is -- see its note on why this
    !! reports as uncovered on every compiler this project measures coverage with.
    ! GCOVR_EXCL_START
    pure function add64(a, b) result(r)
        integer(int64), intent(in) :: a             !! left operand, read as a bit pattern
        integer(int64), intent(in) :: b             !! right operand, read as a bit pattern
        integer(int64) :: r                         !! `a + b` modulo 2**64
        integer(int64) :: low, high
        low = iand(a, M32) + iand(b, M32)           ! below 2**33
        high = ishft(a, -32) + ishft(b, -32) + ishft(low, -32)   ! below 2**33 + 1
        r = ior(ishft(iand(high, M32), 32), iand(low, M32))
    end function add64
    ! GCOVR_EXCL_STOP

    !> `hi - lo + 1` as an unsigned 64-bit pattern; 0 means the full `int64` range.
    !!
    !! **This must not be written as the obvious `hi - lo + 1`, and the reason is a measured ifx
    !! finding rather than a precaution.** That subtraction overflows for any width above 2**63,
    !! and the wrapped pattern is exactly what is wanted -- but the overflow is undefined, and a
    !! compiler entitled to assume it cannot happen can also assume the result is positive, since
    !! `hi >= lo` here by construction. ifx 2026.1.1 does precisely that: it computes the width
    !! correctly and then **deletes `umod_2p64`'s `s < 0` branch as provably dead**, so a width at
    !! or above 2**63 takes the narrow-width path and produces a wrong rejection threshold. The
    !! draws stay inside `[lo, hi]` and stay plausible, so nothing but a comparison against an
    !! independent implementation notices.
    !!
    !! The lesson generalises past this one function: verifying that a compiler *wraps* an
    !! overflowing expression is not the same as verifying that it does not use the overflow's
    !! undefinedness to reason about the result somewhere else entirely.
    pure function width_of(lo, hi) result(s)
        integer(int64), intent(in) :: lo            !! the low end, already ordered
        integer(int64), intent(in) :: hi            !! the high end, already ordered
        integer(int64) :: s                         !! the width, read as unsigned
#ifdef PF_INT128
        ! The true width is up to 2**64, which no int64 can hold, so it is formed wide and folded
        ! into the two's-complement pattern that represents it. Nothing overflows.
        integer(k128) :: w
        w = iand(int(hi, k128) - int(lo, k128) + 1_k128, MASK64_128)
        if (w >= TWO63_128) w = w - TWO64_128
        s = int(w, int64)
#else
        s = add64(sub64(hi, lo), 1_int64)
#endif
    end function width_of

    !> `base + offset`, where `offset` is an unsigned 64-bit pattern known to keep the sum in range.
    pure function offset_by(base, offset) result(r)
        integer(int64), intent(in) :: base          !! the range's low end
        integer(int64), intent(in) :: offset        !! `high64(x*s)`, unsigned and below the width
        integer(int64) :: r                         !! a value inside the closed range
#ifdef PF_INT128
        ! The offset is below the width and the width is at most 2**64, so the mathematical sum is
        ! inside [lo, hi] and the narrowing is exact. Recovering the offset's unsigned value is a
        ! mask rather than a branch.
        r = int(int(base, k128) + iand(int(offset, k128), MASK64_128), int64)
#else
        ! Same reasoning as width_of: the sum overflows whenever the offset's pattern is negative,
        ! and the halves form is what keeps that from being undefined.
        r = add64(base, offset)
#endif
    end function offset_by

    !> `2**64 mod s`, for `s` read as an unsigned 64-bit pattern -- including at and above `2**63`.
    !!
    !! The natural spelling of this is correct only below `2**63`; above it, it computes a
    !! too-large threshold for two thirds of widths, which silently makes up to HALF of the
    !! requested range unreachable. That failure is invisible to every obvious test: the returned
    !! values are all still inside `[lo, hi]`, and they are still uniform over the values that do
    !! occur, so containment and chi-square both pass. Only a comparison against an independent
    !! reference over wide widths sees it.
    pure function umod_2p64(s) result(t)
        integer(int64), intent(in) :: s             !! the width, unsigned and non-zero
        integer(int64) :: t                         !! `2**64 mod s`
        integer(int64) :: a, h, r
        if (s < 0_int64) then
            ! s >= 2**63 unsigned. Then 2**64 - s <= 2**63 <= s, so the remainder IS 2**64 - s and
            ! there is nothing to reduce. `-s` is that pattern, and is representable for every such
            ! s except s == 2**63 exactly, where negation would overflow -- and where 2**63 divides
            ! 2**64, so the answer is 0. That value is taken out first.
            if (s == -huge(1_int64) - 1_int64) then
                t = 0_int64
            else
                t = -s
            end if
            return
        end if
        a = -s                                      ! the two's-complement pattern for 2**64 - s
        h = ishft(a, -1)                            ! floor((2**64 - s)/2); logical shift, so >= 0
        r = mod(h, s)
        if (r >= s - r) then                        ! double it modulo s without ever leaving [0, s)
            r = r - (s - r)
        else
            r = r + r
        end if
        if (iand(a, 1_int64) == 1_int64) then
            r = r + 1_int64
            if (r == s) r = 0_int64
        end if
        t = r
    end function umod_2p64

    !> The full 128-bit product of two unsigned 64-bit patterns, as a low and a high half.
    !!
    !! **Route (e) now covers this, so the wrapping arm is the `#else` arm only.** It used to be
    !! "UB site 2 of 2, and the only one carried on BOTH sides of the fork" -- each 32x32 partial
    !! product can exceed `huge(int64)` and wrap -- and where a 128-bit kind exists that is no
    !! longer so. The remaining wrapping sites are both on the `#else` arm: this one, and
    !! `random_block`'s multiplies. See `feature_risks.md` Risk-95.
    !!
    !! **Why only ONE operand is split, and why the obvious spelling is wrong.** The full unsigned
    !! product reaches nearly 2**128, which does NOT fit a signed 128-bit integer -- so
    !! `iand(int(a,k128), MASK64_128) * iand(int(b,k128), MASK64_128)` **overflows**, and would add
    !! a third wrapping site while appearing to remove one. It measures faster than what ships here
    !! (a single wide multiply against two) and must not be adopted on that basis: Risk-95's first
    !! rule is that a wrapping measurement is not evidence. Splitting `b` alone bounds each product
    !! by 2**96 and every intermediate below 2**97, which is provably in range.
    !!
    !! **Cost.** Two wide multiplies against four narrow ones plus their carries: `pf_random_int_at`
    !! at a narrow range measured 26.14 -> 25.03 ns and at a rejecting width 48.12 -> 41.64 on
    !! machine B (gfortran 14.2.1, release flags), the larger part of the second figure coming from
    !! `mul64_lo_strict` on the retry path rather than from here. So this arm is both faster and
    !! free of undefined behaviour, which is why it is taken despite the strict 16-bit-limb spelling
    !! having been rejected on cost (1.31x gfortran / 2.05x ifx for the whole integer path).
    !!
    !! **The `#else` arm keeps every word of its former warning.** It is guarded by the suite's
    !! cross-implementation agreement sweep and by nothing else -- that is the only thing that would
    !! notice a compiler starting to exploit it. The width arithmetic used to be a third such site,
    !! carried on the same reasoning -- that ifx had been verified to wrap -- and ifx was then
    !! caught using the overflow's undefinedness to delete a branch somewhere else entirely (see
    !! `width_of`). Nothing in that finding says this site is safe; it says the evidence thought to
    !! make it safe was never evidence about this question. Treat a future agreement failure on that
    !! arm as the expected outcome rather than as a surprise.
    pure subroutine mulhilo64(a, b, low, high)
        integer(int64), intent(in) :: a             !! one factor, read as unsigned
        integer(int64), intent(in) :: b             !! the other factor, read as unsigned
        integer(int64), intent(out) :: low          !! bits 0..63 of the product
        integer(int64), intent(out) :: high         !! bits 64..127 of the product
#ifdef PF_INT128
        integer(k128) :: au, bhi, blo, t0, t1, s, wl, wh
        ! ONE operand is split, not both, and that is what keeps this in range. The full unsigned
        ! product reaches nearly 2**128 and so does NOT fit a signed 128-bit integer -- forming it
        ! as a single wide multiply of two unsigned-masked operands overflows, and would be a THIRD
        ! wrapping site rather than the removal of one. Splitting `b` into 32-bit halves bounds each
        ! product by 2**96 and every intermediate below 2**97, so nothing here can overflow at all.
        au = iand(int(a, k128), MASK64_128)
        bhi = ishft(iand(int(b, k128), MASK64_128), -32)
        blo = iand(int(b, k128), M32_128)
        t0 = au * blo                               ! < 2**96
        t1 = au * bhi                               ! < 2**96
        ! a*b = (t1 >> 32)*2**64 + s, where s gathers t1's low limb and the whole of t0.
        s = t0 + ishft(iand(t1, M32_128), 32)       ! < 2**97
        wl = iand(s, MASK64_128)
        wh = ishft(t1, -32) + ishft(s, -64)         ! below 2**64 because the product is
        if (wl >= TWO63_128) wl = wl - TWO64_128
        if (wh >= TWO63_128) wh = wh - TWO64_128
        low = int(wl, int64)
        high = int(wh, int64)
#elif defined(PF_SAFE64)
        integer(int64) :: a0, a1, b0, b1
        integer(int64) :: h00, l00, h01, l01, h10, l10, h11, l11, col1, col2, col3
        a0 = iand(a, M32)
        a1 = ishft(a, -32)
        b0 = iand(b, M32)
        b1 = ishft(b, -32)
        ! Each partial product is taken as a (high, low) pair of 32-bit words, so no product is
        ! ever formed as a single value above 2**63 -- which is what the `#else` arm below does.
        call mul32x32(a0, b0, h00, l00)
        call mul32x32(a0, b1, h01, l01)
        call mul32x32(a1, b0, h10, l10)
        call mul32x32(a1, b1, h11, l11)
        ! Schoolbook columns in units of 2**32. Each is at most three words below 2**32 plus a
        ! carry below 4, so each stays under 2**34 and the carries propagate exactly. The two
        ! results are assembled with bit operations only, so a word with bit 63 set -- negative
        ! read as signed -- is formed without any arithmetic that could overflow.
        col1 = h00 + l10 + l01
        low = ior(ishft(iand(col1, M32), 32), l00)
        col2 = h10 + h01 + l11 + ishft(col1, -32)
        col3 = h11 + ishft(col2, -32)
        high = ior(ishft(iand(col3, M32), 32), iand(col2, M32))
#else
        integer(int64) :: a0, a1, b0, b1, p00, p01, p10, p11, mid, mid2
        a0 = iand(a, M32)
        a1 = ishft(a, -32)
        b0 = iand(b, M32)
        b1 = ishft(b, -32)
        p00 = a0 * b0
        p01 = a0 * b1
        p10 = a1 * b0
        p11 = a1 * b1
        mid = p10 + ishft(p00, -32)
        mid2 = iand(mid, M32) + p01
        low = ior(ishft(mid2, 32), iand(p00, M32))
        high = p11 + ishft(mid, -32) + ishft(mid2, -32)
#endif
    end subroutine mulhilo64

#ifdef PF_SAFE64
    !> The full product of two values below `2**32`, as a (high, low) pair of 32-bit words.
    !!
    !! Exists only on the `PF_SAFE64` arm, where a 32x32 product must not be formed as a single
    !! `int64`: at the top of the range it exceeds `huge(int64)`, which is undefined rather than
    !! merely wrapping. Halving `y` bounds `x * (y/2)` by `(2**32-1)(2**31-1)`, below 2**63, and
    !! the odd bit is added back as `x` or 0 without a branch. `s` stays below 2**33, so no
    !! intermediate can overflow at any input.
    pure subroutine mul32x32(x, y, high, low)
        integer(int64), intent(in) :: x             !! one factor, below `2**32`
        integer(int64), intent(in) :: y             !! the other factor, below `2**32`
        integer(int64), intent(out) :: high         !! bits 32..63 of the product
        integer(int64), intent(out) :: low          !! bits 0..31 of the product
        integer(int64) :: u, s
        u = x * ishft(y, -1)
        s = ishft(iand(u, M31), 1) + iand(x, -iand(y, 1_int64))
        low = iand(s, M32)
        high = ishft(u, -31) + ishft(s, -32)
    end subroutine mul32x32
#endif

    !> Unsigned `a < b` for two 64-bit patterns.
    !!
    !! Two operands with the SAME top bit compare the same way signed and unsigned, so the signed
    !! comparison already answers; operands whose top bits DIFFER compare the opposite way, because
    !! the one with the top bit set is the larger as unsigned and the smaller as signed. That is the
    !! whole rule, and it is what the body says: take the signed answer, and invert it exactly when
    !! the top bits differ. It cannot overflow and needs no wide kind.
    !!
    !! **Do NOT write this as `ieor(a, K) < ieor(b, K)` with `K` the sign bit** -- the shorter,
    !! obvious spelling, and what this was until it was found miscompiled. nagfor 7.2 cancels the
    !! common `ieor(., 2**63)` from both sides of the relational, which is invalid precisely because
    !! XOR
    !! with the sign bit REVERSES the order it maps, and the comparison collapses to the signed
    !! `a < b` -- the exact inverse of this function's contract for a pair straddling `2**63`. It
    !! computes both `ieor`s correctly and then compares them wrongly, so printing the operands
    !! shows nothing amiss; only a comparison reveals it. See `feature_risks.md` Risk-125 for the
    !! same defect's other two shapes, and CLAUDE.md's compiler-gotchas section.
    !!
    !! **What that cost, and why the shape is worth protecting.** `ult` is `int_reduce`'s lazy
    !! guard, so it is reached for every width above the narrow-32 cap. A wrong answer there sends
    !! the draw around the rejection path and returns a DIFFERENT value that is still inside
    !! `[lo, hi]` and still uniform-looking: 13 of the 38 golden integer rows, every one of them a
    !! width above `2**63`, and nothing but the frozen vectors could see it. Writing the rule out
    !! rather than reaching for the identity costs about 2.7 % on that path (41.9 -> 43.0 ns per
    !! draw, gfortran -O3; 58.2 -> 59.8 under nagfor -O3, measured against the fastest correct
    !! spelling) and removes the identity a compiler can mis-cancel. Keep it that way: a silent
    !! change to a frozen bit contract is not worth 2.7 %.
    pure function ult(a, b) result(r)
        integer(int64), intent(in) :: a             !! left operand, read as unsigned
        integer(int64), intent(in) :: b             !! right operand, read as unsigned
        logical :: r                                !! `.true.` if `a < b` as unsigned
        r = (a < b) .neqv. ((a < 0_int64) .neqv. (b < 0_int64))
    end function ult

    ! ================================================================================
    ! The mixer
    ! ================================================================================

    !> The SplitMix64 finaliser: a bijection on 64 bits with good avalanche.
    !!
    !! `mix64(0) == 0`, which is why `pf_random_key(0, 0)` is 0 and why every seed has exactly one
    !! label whose derived key is 0. Surprising, not a defect: a bijection has to send something to
    !! zero, and hiding it would cost the bijection.
    pure function mix64(x) result(z)
        integer(int64), intent(in) :: x             !! any 64-bit pattern
        integer(int64) :: z                         !! the mixed pattern
        z = x
        z = mul64_lo_strict(ieor(z, ishft(z, -30)), MIX_A)
        z = mul64_lo_strict(ieor(z, ishft(z, -27)), MIX_B)
        z = ieor(z, ishft(z, -31))
    end function mix64

    !> The low 64 bits of `a * b`, computed so that nothing ever overflows -- by the route (e) fork.
    !!
    !! **Neither arm may be replaced by a plain `int64` multiply, and this is blocker-grade rather
    !! than a precaution.** Written that way, gfortran folds the whole of `mix64` at `-O2` AND `-O3`,
    !! on three architectures and two major versions, to the single constant `z'7FFFFFFF00000000'`
    !! for every input -- so `pf_random_key` would return one key for every seed and every label,
    !! with no abort, no warning, and downstream output that still looks random.
    !!
    !! **Where a 128-bit kind exists the product is formed in it**, where two `int64` operands give
    !! at most 2**126 and so cannot overflow: there is no undefined behaviour left for an optimiser
    !! to fold from, which is why this arm does not reintroduce the miscompilation above. The low
    !! half is signedness-independent -- two's-complement multiplication gives
    !! `(a + 2**64 m)(b + 2**64 n) = ab (mod 2**64)` -- so the operands are used as they come and
    !! only the result is folded back into signed range, exactly as `width_of` does.
    !!
    !! **The `#else` arm keeps the 16-bit limbs**, and 16 is the load-bearing number: splitting into
    !! 32x32 products is NOT sufficient, since a 32x32 product still exceeds `int64`. On 16-bit limbs
    !! nothing exceeds 2**35, so that arm is correct by construction on any compiler at any
    !! optimisation level and needs no wide kind -- which is what makes it available to the compiler
    !! that has none.
    !!
    !! **Cost, and why it is not "once per stream family".** The limb form is sixteen 16x16 products;
    !! the wide form is one multiply. Measured on machine A (arm64, gfortran 15.2, release flags):
    !! `pf_random_key` 7.68 -> 4.66 ns (1.65x), and a rejecting-width `pf_random_int_at` 54.4 -> 42.4
    !! (1.28x) -- because `retry_key_of` calls `mix64` twice on **every retry**, so this sits on the
    !! integer draw's rejection path and not only on key derivation.
    pure function mul64_lo_strict(a, b) result(r)
        integer(int64), intent(in) :: a             !! one factor
        integer(int64), intent(in) :: b             !! the other factor
        integer(int64) :: r                         !! bits 0..63 of the product
#ifdef PF_INT128
        integer(k128) :: p
        p = iand(int(a, k128) * int(b, k128), MASK64_128)
        if (p >= TWO63_128) p = p - TWO64_128       ! fold to the signed pattern before narrowing
        r = int(p, int64)
#else
        integer(int64) :: a0, a1, a2, a3, b0, b1, b2, b3, acc, d0, d1, d2, d3
        a0 = iand(a, M16)
        a1 = iand(ishft(a, -16), M16)
        a2 = iand(ishft(a, -32), M16)
        a3 = iand(ishft(a, -48), M16)
        b0 = iand(b, M16)
        b1 = iand(ishft(b, -16), M16)
        b2 = iand(ishft(b, -32), M16)
        b3 = iand(ishft(b, -48), M16)
        ! Column by column, carrying as we go. The widest accumulator value is below 2**35, so no
        ! partial product, sum or carry can reach the sign bit.
        acc = a0 * b0
        d0 = iand(acc, M16)
        acc = ishft(acc, -16) + a0 * b1 + a1 * b0
        d1 = iand(acc, M16)
        acc = ishft(acc, -16) + a0 * b2 + a1 * b1 + a2 * b0
        d2 = iand(acc, M16)
        acc = ishft(acc, -16) + a0 * b3 + a1 * b2 + a2 * b1 + a3 * b0
        d3 = iand(acc, M16)
        r = ior(ior(d0, ishft(d1, 16)), ior(ishft(d2, 32), ishft(d3, 48)))
#endif
    end function mul64_lo_strict

    !> `pf_random_key`'s derivation, shared by both label kinds.
    pure function key_from(seed, label) result(r)
        integer(int64), intent(in) :: seed          !! the seed to derive from
        integer(int64), intent(in) :: label         !! which derived family
        integer(int64) :: r                         !! an independent seed
        r = mix64(ieor(mix64(seed), label))
    end function key_from

    ! ================================================================================
    ! Tier 1 -- the stateful stream
    ! ================================================================================
    !
    ! These live in this file rather than in a `parquet_random_stream` submodule, and the reason is
    ! a compiler fact rather than a preference. gfortran does not emit an out-of-line copy of a
    ! private module-contained procedure whose in-module calls it has all inlined, so a submodule
    ! calling `random_block`, `to_real64`, `int_at_impl` or any of the fill workers fails at LINK
    ! time with `undefined reference` -- confirmed here, and the shape CLAUDE.md's "A private
    ! procedure contained directly in a module ... fails at LINK time" note describes. The documented
    ! fix is to give each such helper an interface in the module and a body in a submodule, which
    ! for these eight would mean moving the cipher and its route (e) fork out of this file. That is
    ! exactly what must not happen: `random_block`, `mulhilo64`, `mul64_lo_strict` and `width_of`
    ! are what the whole correctness story rests on and belong together. So the split was dropped,
    ! not the helpers.
    !
    ! Two things below are load-bearing and easy to undo by accident.
    !
    ! The BLOCK CACHE is keyed on the block index (`self%blk`), never drained as a queue. That is
    ! what keeps it out of the contract: `%position` means a word index and nothing else, `%rewind`
    ! just sets it, and a stream restored from a saved `%position` is exact because the cache is
    ! derivable state that either matches or is replaced. A queue-shaped buffer would compute the
    ! same values while putting a buffer state into everything `%position` means.
    !
    ! The POSITION GUARDS (`advance_by`, `set_pos`) exist so that no arithmetic on `pos` can
    ! overflow. This module has already been caught once with a compiler using an overflowing
    ! expression's undefinedness to delete a branch far away (`feature_risks.md` Risk-94), so an
    ! unguarded `pos + 2` on the hot path would be a real regression rather than a theoretical one.

    !> `%seed` with no stream index: stream 0.
    pure subroutine stream_seed_base(self, seed)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reseed
        integer(int64), intent(in) :: seed              !! the stream family's seed
        call reseed(self, seed, 0_int64)
    end subroutine stream_seed_base

    !> `%seed` with an `integer(int32)` stream index; sign-extends, so any value is valid.
    pure subroutine stream_seed_i32(self, seed, stream)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reseed
        integer(int64), intent(in) :: seed              !! the stream family's seed
        integer(int32), intent(in) :: stream            !! which stream of that family
        call reseed(self, seed, int(stream, int64))
    end subroutine stream_seed_i32

    !> `%seed` with an `integer(int64)` stream index; every value is valid.
    pure subroutine stream_seed_i64(self, seed, stream)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reseed
        integer(int64), intent(in) :: seed              !! the stream family's seed
        integer(int64), intent(in) :: stream            !! which stream of that family
        call reseed(self, seed, stream)
    end subroutine stream_seed_i64

    !> The current 1-based word position.
    pure function stream_position(self) result(p)
        class(pf_random_stream), intent(in) :: self     !! the stream to query
        integer(int64) :: p                             !! 1-based word position
        p = self%pos + 1_int64
    end function stream_position

    !> `%rewind` with no argument: back to position 1.
    pure subroutine stream_rewind_base(self)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reposition
        call set_pos(self, 1_int64)
    end subroutine stream_rewind_base

    !> `%rewind` to an `integer(int32)` position.
    pure subroutine stream_rewind_i32(self, pos)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reposition
        integer(int32), intent(in) :: pos               !! 1-based word position
        call set_pos(self, int(pos, int64))
    end subroutine stream_rewind_i32

    !> `%rewind` to an `integer(int64)` position.
    pure subroutine stream_rewind_i64(self, pos)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reposition
        integer(int64), intent(in) :: pos               !! 1-based word position
        call set_pos(self, pos)
    end subroutine stream_rewind_i64

    !> The next `real64` in `[0, 1)`, advancing two words.
    pure subroutine stream_uniform(self, x)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: x                  !! a uniform draw in `[0, 1)`
        integer(int64) :: w0, w1
        call align_to_pair(self)
        call advance_by(self, 2_int64)
        call word_at(self, DOM_REAL64, self%pos - 2_int64, w0)
        call word_at(self, DOM_REAL64, self%pos - 1_int64, w1)
        x = to_real64(ior(ishft(w1, 32), w0))
    end subroutine stream_uniform

    !> The next `real32` in `[0, 1)`, advancing one word.
    pure subroutine stream_uniform32(self, x)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real32), intent(out) :: x                  !! a uniform draw in `[0, 1)`
        integer(int64) :: w
        call advance_by(self, 1_int64)
        call word_at(self, DOM_REAL32, self%pos - 1_int64, w)
        x = to_real32(w)
    end subroutine stream_uniform32

    !> The next 64 raw bits, advancing two words -- the same two `%uniform` would have read.
    pure subroutine stream_bits(self, b)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(out) :: b                !! 64 raw bits
        integer(int64) :: w0, w1
        call align_to_pair(self)
        call advance_by(self, 2_int64)
        call word_at(self, DOM_REAL64, self%pos - 2_int64, w0)
        call word_at(self, DOM_REAL64, self%pos - 1_int64, w1)
        b = ior(ishft(w1, 32), w0)
    end subroutine stream_bits

    !> `%int_range` for `integer(int64)` bounds and result.
    pure subroutine stream_int_range_i64(self, lo, hi, r)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(in) :: lo                !! one end of the closed range
        integer(int64), intent(in) :: hi                !! the other end; `lo > hi` is swapped
        integer(int64), intent(out) :: r                !! a uniform integer in the closed range
        integer(int64) :: d
        call take_pair(self, d)
        r = int_at_impl(self%key, self%stream, lo, hi, d)
    end subroutine stream_int_range_i64

    !> `%int_range` for `integer(int32)` bounds and result.
    !!
    !! The result is inside the closed range by construction, so the narrowing is exact -- the same
    !! argument `pf_random_int_at_i32` rests on.
    pure subroutine stream_int_range_i32(self, lo, hi, r)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int32), intent(in) :: lo                !! one end of the closed range
        integer(int32), intent(in) :: hi                !! the other end; `lo > hi` is swapped
        integer(int32), intent(out) :: r                !! a uniform integer in the closed range
        integer(int64) :: d
        call take_pair(self, d)
        r = int(int_at_impl(self%key, self%stream, int(lo, int64), int(hi, int64), d), int32)
    end subroutine stream_int_range_i32

    !> `%exp`: the next `Exp(1)` draw, taking one pair-aligned word pair.
    !!
    !! **Pair-aligned like `%int_range`, not free-running like `%uniform`**, and the promise that
    !! buys is unconditional: the value is exactly `pf_random_exp_at(seed, stream, d)` for the
    !! draw `d` it consumed, whatever the cursor was beforehand. `%uniform` agrees with its
    !! coordinate-addressed twin only while the cursor stays even, which it does unless a
    !! `%uniform32` has been mixed in. The price is at most one wasted word after such a mix.
    pure subroutine stream_exp(self, x)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: x                  !! an `Exp(1)` draw in `[0, 36.7368]`
        integer(int64) :: d
        call take_pair(self, d)
        x = exp_of(self%key, self%stream, d)
    end subroutine stream_exp

    !> `%exp_portable`: the same draw through the frozen logarithm. Not `pure`, per `exp_key`.
    subroutine stream_exp_portable(self, x)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: x                  !! an `Exp(1)` draw in `[0, 36.7368]`
        integer(int64) :: d
        call take_pair(self, d)
        x = exp_portable_of(self%key, self%stream, d)
    end subroutine stream_exp_portable

    !> `%normal`: the next Ziggurat normal, consuming as many pairs as its rejection loop needed.
    !!
    !! **The one producer here that cannot advance before it reads**, because how far to advance is
    !! what the read decides. The invariant every other producer keeps -- that an exhausted stream
    !! aborts having written and moved nothing -- is preserved anyway by computing into a local and
    !! assigning `x` only after `advance_by` has accepted the cost.
    pure subroutine stream_normal(self, x)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: x                  !! a standard normal draw
        integer(int64) :: d, pairs
        integer(int32) :: path
        real(real64) :: v
        call align_to_pair(self)
        d = self%pos / 2_int64 + 1_int64
        call zig_normal(self%key, self%stream, d, v, pairs, path)
        call advance_by(self, 2_int64 * pairs)
        x = v
    end subroutine stream_normal

    !> Where the tail arm switches from the uniform proposal to exponential tilting, for a
    !! standardised lower bound `a >= 0`. **Frozen contract; see `pf_normal_truncated_algorithm`.**
    !!
    !! **Derived, not fitted, and the derivation is here because the closed form hides it.** Both
    !! proposals cover the same target integral over `[a, b]`, so their expected costs are in the
    !! ratio of their envelope masses: `(b - a) * exp(-a*a/2)` for a uniform proposal whose
    !! envelope is anchored at `a`, against `exp(lam*lam/2 - lam*a) / lam` for the tilted one.
    !! Substituting `lam*lam = a*lam + 1` -- which is `lam`'s defining equation, see
    !! `normal_truncated_draw` -- turns the second into `exp(-a*a/2)` times the expression below,
    !! so the two cross exactly at `b - a = tn_threshold(a)`.
    !!
    !! **Written to cancel NOTHING, and that is load-bearing rather than fastidious.** The direct
    !! transcription of the algebra is `exp((2 + a*a - a*s)/4) * 2/(a + s)`, in which `a*a - a*s`
    !! is a difference of two nearly-equal large doubles: for `a` around `1e10` the two agree to
    !! every bit, the true difference is 0, and a compiler that CONTRACTS the pair into an FMA
    !! returns the rounding error of `a*a` instead -- some hundreds -- which comes back out of the
    !! `exp` as a threshold near `1e53`. That sends a wide tail interval into the uniform proposal,
    !! whose acceptance there underflows to zero, and the draw HANGS (`feature_risks.md`
    !! Risk-251). Rationalising `a - s = -4/(a + s)` removes the cancellation instead of guarding
    !! it, so no rounding barrier is needed and no compiler flag can reintroduce it. `hypot`
    !! rather than `sqrt(a*a + 4)` keeps `a*a` from overflowing for an `a` above `1.3e154`, which
    !! would otherwise make `s` infinite and the threshold 0.
    !!
    !! The result is positive and decreasing in `a`, behaving as `1/a` out in the tail; at `a = 0`
    !! it is `exp(0.5)`. Beyond `a` of about `6.7e7` it drops below one ulp of `a`, so no interval
    !! narrower than the threshold is representable there and the tilted case is the only one that
    !! can serve a far tail at all -- which is why the hang above was not a rare corner.
    pure function tn_threshold(a) result(t)
        real(real64), intent(in) :: a               !! standardised lower bound; `a >= 0`
        real(real64) :: t                           !! the interval width at which the two cross
        real(real64) :: s
        s = hypot(a, 2.0_real64)                    ! sqrt(a*a + 4), without overflowing
        t = (2.0_real64 / (a + s)) * exp(0.5_real64 - a / (a + s))
    end function tn_threshold

    !> One bound on the standardised scale: `(v - ctr)/scl`, saturating instead of overflowing.
    !!
    !! **The plain quotient is not representable for the bounds a caller actually writes.** A
    !! one-sided truncation is spelled `huge(1.0_real64)`, and standardising that by any `sigma`
    !! below one overflows; so does centring it on a `mu` of the opposite sign at the same
    !! magnitude. `IEEE_OVERFLOW` is silent under gfortran and ifx, where the result is the
    !! infinity this returns directly, and fatal under nagfor's default `-ieee=stop` -- so the
    !! promise that `huge(1.0_real64)` and a true infinity are the same truncation held only
    !! where the trap was masked. Saturating here is what makes it hold everywhere.
    !!
    !! Each guard is NESTED inside the sign test that makes its own threshold safe to form, not
    !! `.and.`-ed with it: `.and.` does not short-circuit, and `huge() + ctr` overflows for the
    !! sign of `ctr` the outer test excludes.
    !!
    !! An infinite or NaN `ctr` reaches the same answers as the plain arithmetic, because the
    !! thresholds it is compared against are infinite in the same direction: that keeps
    !! `normal_truncated_draw`'s one bound guard covering an infinite `mu` and a NaN anywhere.
    pure function tn_standardise(v, ctr, scl) result(z)
        real(real64), intent(in) :: v               !! the bound, in the caller's units
        real(real64), intent(in) :: ctr             !! the resolved `mu`, subtracted first
        real(real64), intent(in) :: scl             !! the resolved `sigma`; strictly positive
        real(real64) :: z                           !! the standardised bound, `+/-Infinity` where it saturates
        real(real64) :: num

        ! The centring, which overflows only for two values of opposite signs a full range apart.
        if (ctr < 0.0_real64) then
            if (v > huge(1.0_real64) + ctr) then
                z = ieee_value(0.0_real64, ieee_positive_inf)
                return
            end if
        else if (ctr > 0.0_real64) then
            if (v < -huge(1.0_real64) + ctr) then
                z = ieee_value(0.0_real64, ieee_negative_inf)
                return
            end if
        end if
        num = v - ctr

        ! The scaling, which overflows only below a `scl` of one -- where `huge()*scl` is the
        ! exact threshold and cannot overflow itself. A NaN fails both tests and falls through to
        ! the division, which propagates it.
        if (scl < 1.0_real64) then
            if (abs(num) > huge(1.0_real64) * scl) then
                if (num > 0.0_real64) then
                    z = ieee_value(0.0_real64, ieee_positive_inf)
                else
                    z = ieee_value(0.0_real64, ieee_negative_inf)
                end if
                return
            end if
        end if
        z = num / scl
    end function tn_standardise

    !> The standardised interval's width: `b - a`, saturating instead of overflowing.
    !!
    !! Two bounds a full range apart -- `[-huge, huge]`, which is how a caller says "do not
    !! truncate" -- have a width that is not representable, and every case rule below compares
    !! that width against a threshold of at most `exp(0.5)`. Saturating therefore decides each
    !! rule exactly as the unrepresentable width would, and as the infinite bounds already do.
    !! Requires `a < b`, which `normal_truncated_draw` has established; the width is invariant
    !! under the mirroring below, so one evaluation serves both orientations.
    pure function tn_width(a, b) result(w)
        real(real64), intent(in) :: a               !! standardised lower bound
        real(real64), intent(in) :: b               !! standardised upper bound; `b > a`
        real(real64) :: w                           !! `b - a`, or `+Infinity` where it saturates

        ! Nested, not `.and.`-ed: `huge() + a` overflows for the sign of `a` the outer test
        ! excludes. With `a >= 0` and `b > a` the difference is bounded by `b` and cannot overflow.
        if (a < 0.0_real64) then
            if (b > huge(1.0_real64) + a) then
                w = ieee_value(0.0_real64, ieee_positive_inf)
                return
            end if
        end if
        w = b - a
    end function tn_width

    !> The truncated normal draw, with the accepting case reported. See `stream_normal_truncated`.
    !!
    !! Robert (1995), *Simulation of truncated normal variables*, Statistics and Computing
    !! 5:121-125: three proposals and a **Phi-free** rule choosing between them. Phi-free matters
    !! twice -- a rule evaluating the normal CDF would put the case boundary at libm's mercy, and
    !! the case boundary decides the value.
    !!
    !! `path` is 1 naive, 2 uniform proposal, 3 exponential tilting, plus 3 more when the interval
    !! was mirrored -- so 5 and 6 are the latter two cases on a wholly non-positive interval. The
    !! offset encoding is `gamma_draw`'s, for the same reason: the mirror is orthogonal to the
    !! case, and overwriting `path` would make every mirrored draw's case invisible. `tries` counts
    !! proposals. Nothing in the library reads either.
    !!
    !! **`path == 4` cannot occur, by construction rather than by accident**: naive rejection is
    !! reachable only from the straddling arm, and the straddling arm is never mirrored. So the
    !! reachable set is exactly `{1, 2, 3, 5, 6}`, and a test enumerating the cases asserts those
    !! five -- not six. Renumbering to close the gap would put the mirror back into the case
    !! number, which is what this encoding exists to avoid.
    !!
    !! **The two guards are written as negated `>` and `<` tests, never as `<=` or `>=`.** Every
    !! comparison against a NaN is false, so the negated form refuses a NaN and the direct form
    !! waves it through -- the same idiom, for the same reason, as `gamma_draw`'s shape guard.
    !! The bound guard sits on the STANDARDISED bounds, which makes one test cover four defects:
    !! `lo > hi`, `lo == hi`, a NaN in `lo`, `hi` or `mu`, and an infinite `mu` (both bounds then
    !! collapse to one infinity). An interval that is non-empty in data units but collapses under
    !! a large `sigma` is refused there too, which is why the message names the standardisation.
    !!
    !! **Infinite bounds need no special case anywhere.** `b - a` is `+Infinity`, which satisfies
    !! every case rule's `>=`; `z <= b` is then always true; and `huge(1.0_real64)` behaves
    !! identically, so a caller need not reach for `ieee_arithmetic` to truncate on one side.
    !! That last equivalence is what `tn_standardise` and `tn_width` exist for: standardising a
    !! `huge()` bound, or spanning two of them, is not representable, and the plain arithmetic
    !! reaches the infinity by raising `IEEE_OVERFLOW` on the way -- which nagfor turns into an
    !! abort. They deliver the same value without raising it.
    pure subroutine normal_truncated_draw(self, lo, hi, x, path, tries, mu, sigma)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: lo              !! lower bound of the support, in `x`'s units
        real(real64), intent(in) :: hi              !! upper bound of the support, in `x`'s units
        real(real64), intent(out) :: x              !! the draw, satisfying `lo <= x <= hi`
        integer(int32), intent(out) :: path         !! 1/2/3 naive/uniform/tilted; +3 when mirrored
        integer(int64), intent(out) :: tries        !! proposals this draw consumed; at least 1
        real(real64), intent(in), optional :: mu    !! untruncated mean; default 0
        real(real64), intent(in), optional :: sigma !! untruncated standard deviation; default 1
        real(real64) :: a, b, m, t, z, u, e, lam, ctr, scl, w
        logical :: mirrored

        ctr = 0.0_real64
        scl = 1.0_real64
        if (present(mu)) ctr = mu
        if (present(sigma)) scl = sigma
        if (.not. (scl > 0.0_real64)) then
            error stop "pf_random_stream%normal_truncated: sigma must be strictly positive " // &
                       "(a scale of zero or less is not a distribution, and NaN is not a scale)"
        end if
        a = tn_standardise(lo, ctr, scl)
        b = tn_standardise(hi, ctr, scl)
        if (.not. (a < b)) then
            error stop "pf_random_stream%normal_truncated: the lower bound must be strictly " // &
                       "below the upper bound after centring and scaling (an empty or " // &
                       "single-point support is not a distribution, and NaN is not a bound)"
        end if
        w = tn_width(a, b)

        tries = 0_int64
        mirrored = .false.
        m = 0.0_real64
        if (a < 0.0_real64 .and. b > 0.0_real64) then
            ! The interval straddles the mode, so the target's maximum is interior, at 0.
            if (w >= trunc_straddle_crossover) then
                path = 1
                do
                    tries = tries + 1_int64
                    call stream_normal(self, z)
                    if (z >= a .and. z <= b) exit
                end do
                call tn_finish(z, 1.0_real64, ctr, scl, lo, hi, x)
                return
            end if
        else
            ! Wholly non-positive intervals are mirrored onto non-negative ones, so the three
            ! cases below need only the one orientation. `b <= 0` rather than `b < 0` keeps the
            ! `a < 0, b == 0` interval out of the straddling arm, where an envelope anchored at 0
            ! would be right but `tn_threshold` would not be consulted.
            if (b <= 0.0_real64) then
                t = a
                a = -b
                b = -t
                mirrored = .true.
            end if
            if (w >= tn_threshold(a)) then
                ! Exponential tilting. `lam` maximises the tilted envelope and solves
                ! `lam*lam = a*lam + 1`, so `lam >= a >= 0` always and the accept probability
                ! below never exceeds 1.
                path = 3
                lam = 0.5_real64 * (a + hypot(a, 2.0_real64))
                do
                    tries = tries + 1_int64
                    call stream_exp(self, e)
                    z = a + e / lam
                    call stream_uniform(self, u)
                    if (z <= b) then
                        t = z - lam
                        if (u <= exp(-0.5_real64 * t * t)) exit
                    end if
                end do
                call tn_finish(z, merge(-1.0_real64, 1.0_real64, mirrored), ctr, scl, lo, hi, x)
                if (mirrored) path = path + 3
                return
            end if
            m = a
        end if

        ! The uniform proposal, for an interval too narrow for its arm's alternative. `m` is the
        ! point of `[a, b]` nearest zero and therefore where the target is largest, so the accept
        ! probability is at most 1, with equality only at `m` -- getting `m` wrong returns a
        ! UNIFORM draw that no coarse test can tell from this one (`feature_risks.md` Risk-249).
        path = 2
        do
            tries = tries + 1_int64
            call stream_uniform(self, u)
            z = a + u * w
            call stream_uniform(self, u)
            ! `(m - z)*(m + z)` rather than `m*m - z*z`: the same quantity, formed without the
            ! cancellation that makes the second one meaningless for a large `m` -- see
            ! `tn_threshold`, where the identical hazard hangs the draw outright.
            if (u <= exp(0.5_real64 * (m - z) * (m + z))) exit
        end do
        call tn_finish(z, merge(-1.0_real64, 1.0_real64, mirrored), ctr, scl, lo, hi, x)
        if (mirrored) path = path + 3
    end subroutine normal_truncated_draw

    !> Un-mirrors, de-standardises and CLAMPS one truncated normal draw.
    !!
    !! **The clamp is the only thing that makes `lo <= x <= hi` true, and it is not redundant
    !! tidying for a later reader to delete** (`feature_risks.md` Risk-250). `z` is drawn inside
    !! `[(lo-mu)/sigma, (hi-mu)/sigma]`, but `mu + sigma*z` does not round back inside `[lo, hi]`
    !! whenever `mu` and `sigma` are not aligned with the bounds: measured over 1.27 M evaluations
    !! at independently chosen `mu`, `sigma` and bounds, 74 453 landed outside. The case that
    !! matters is a positivity truncation, where a draw on the lower boundary comes back negative
    !! and flows into a `sqrt`, a `log` or a physical count somewhere else entirely.
    !!
    !! **Two `if`s rather than `min`/`max`**, which compile to `minsd`/`maxsd` and raise
    !! `IEEE_INVALID` on a quiet NaN. Nothing here can be a NaN -- `normal_truncated_draw` refused
    !! every NaN input before drawing, and `sigma > 0` and `lam > 0` are guaranteed -- so this is
    !! belt and braces rather than a live hazard, but it costs nothing and nagfor unmasks the trap
    !! for the whole process. The clamp moves a value by at most one ulp of the bound.
    pure subroutine tn_finish(z, sgn, ctr, scl, lo, hi, x)
        real(real64), intent(in) :: z               !! the standardised draw, on the mirrored side
        real(real64), intent(in) :: sgn             !! `-1` if the interval was mirrored, else `1`
        real(real64), intent(in) :: ctr             !! the resolved `mu`
        real(real64), intent(in) :: scl             !! the resolved `sigma`
        real(real64), intent(in) :: lo              !! lower bound, in the caller's units
        real(real64), intent(in) :: hi              !! upper bound, in the caller's units
        real(real64), intent(out) :: x              !! the draw, clamped into `[lo, hi]`
        x = ctr + scl * (sgn * z)
        if (x < lo) x = lo
        if (x > hi) x = hi
    end subroutine tn_finish

    !> `%normal_truncated`: the next normal restricted to `[lo, hi]`.
    !!
    !! **`lo`, `hi`, `mu`, `sigma` and the result all live on ONE scale**, and that is the
    !! load-bearing decision of this interface. `%gamma` can leave the scale to the caller because
    !! scaling a Gamma draw commutes with drawing it; truncation does not commute so painlessly --
    !! the bounds have to be standardised before the draw, so an interface taking bounds on the
    !! standard scale would invite a caller to pass physical bounds beside a physical `mu` and get
    !! a plausible wrong answer with nothing to see.
    !!
    !! `mu` defaults to 0 and `sigma` to 1, so the two-bound call is the standard normal on
    !! `[lo, hi]`. For no bound on one side pass `huge(1.0_real64)` or a true `Infinity`; both work
    !! and give the same distribution.
    !!
    !! **The result is guaranteed inside `[lo, hi]`** -- see `tn_finish`, which is where that is
    !! actually made true.
    !!
    !! **Variable, unpredictable cost**, pair-aligned, joining `%normal`, `%gamma` and `%poisson`
    !! in what `%position` can and cannot promise (see `pf_random_stream`). Between one and about
    !! one and a half proposals per draw across the whole domain, and never more than about two,
    !! however far into the tail the interval sits -- which is the property that lets this be
    !! written without an iteration cap. Naive rejection alone would need some 32 000 normals per
    !! value at `lo = 4`, and would not terminate in practice further out.
    !!
    !! **Unlike `%normal`, an exhausted stream may abort part-way through the rejection loop**,
    !! having consumed some words. `%gamma` has the same property; the stronger "writes and moves
    !! nothing" invariant is not available to a producer built from several sub-draws.
    pure subroutine stream_normal_truncated(self, lo, hi, x, mu, sigma)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: lo              !! lower bound of the support, in `x`'s units
        real(real64), intent(in) :: hi              !! upper bound of the support, in `x`'s units
        real(real64), intent(out) :: x              !! the draw, satisfying `lo <= x <= hi`
        real(real64), intent(in), optional :: mu    !! untruncated mean; default 0
        real(real64), intent(in), optional :: sigma !! untruncated standard deviation; default 1
        integer(int32) :: path
        integer(int64) :: tries
        call normal_truncated_draw(self, lo, hi, x, path, tries, mu, sigma)
    end subroutine stream_normal_truncated

    !> `%gamma`: the next `Gamma(shape, 1)` draw, by Marsaglia-Tsang rejection.
    !!
    !! **Tier 1 only, deliberately** -- there is no `pf_random_gamma_at` and no bulk fill. The
    !! sub-stream construction sketched for a later phase would make them possible, and the surface
    !! is already wide; if they are ever wanted, the construction and its independence obligations
    !! are the same as the normal's.
    !!
    !! `shape` is the Gamma shape parameter `a`, strictly positive; the scale is 1, so a caller
    !! wanting `Gamma(a, theta)` multiplies by `theta`. The mean is `shape` and the variance is
    !! `shape`.
    !!
    !! **Two rejection loops, not one.** The inner one redraws a normal until `1 + c*x > 0`, which
    !! happens for about `Phi(-3*sqrt(a - 1/3))` of draws and so essentially never above `a = 1`;
    !! the outer one is the squeeze-then-log acceptance, which accepts about 95 % of candidates on
    !! the squeeze alone. Consumption is therefore variable and unpredictable -- see
    !! `pf_random_stream`'s note on what `%position` still guarantees.
    !!
    !! **`shape < 1` is supported through a boost**, `Gamma(a) = Gamma(a+1) * u**(1/a)`, at the
    !! cost of one extra uniform and a libm `pow`. Restricting the domain to `shape >= 1` instead
    !! was considered and rejected (Q3-4): a partial procedure is a worse API than a per-libm
    !! promise, and Gamma is per-libm regardless. **`1 - u` rather than `u`** in the boost, so the
    !! draw cannot come back exactly 0.
    pure subroutine stream_gamma(self, shape, r)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: shape               !! the shape parameter; must be > 0
        real(real64), intent(out) :: r                  !! a `Gamma(shape, 1)` draw, strictly positive
        integer(int32) :: path
        call gamma_draw(self, shape, r, path)
    end subroutine stream_gamma

    !> The Gamma draw, with the accepting branch reported. See `stream_gamma`.
    !!
    !! `path` is 1 when the squeeze accepted and 2 when the full logarithmic test did, plus 2 more
    !! when the `shape < 1` boost was applied -- so 3 and 4 are those same two branches under a
    !! boost. It exists so a test can assert every branch was reached; nothing in the library reads
    !! it.
    pure subroutine gamma_draw(self, shape, r, path, squeeze_ok)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: shape               !! the shape parameter; must be > 0
        real(real64), intent(out) :: r                  !! a `Gamma(shape, 1)` draw
        integer(int32), intent(out) :: path             !! 1/2 squeeze/log; +2 when boosted
        logical, intent(out), optional :: squeeze_ok    !! see below; test-only, costs a `log` when asked
        real(real64) :: a, d, c, x, v, u, boost
        logical :: boosted

        if (.not. (shape > 0.0_real64)) then
            error stop "pf_random_stream%gamma: the shape parameter must be strictly positive " // &
                       "(a Gamma distribution is not defined for shape <= 0, and NaN is not a shape)"
        end if
        call align_to_pair(self)
        a = shape
        boost = 1.0_real64
        boosted = .false.
        if (a < 1.0_real64) then
            ! Gamma(a) = Gamma(a+1) * U**(1/a). `1 - u` so the boost cannot be exactly 0.
            call stream_uniform(self, u)
            boost = (1.0_real64 - u) ** (1.0_real64 / a)
            a = a + 1.0_real64
            boosted = .true.
        end if
        d = a - 1.0_real64 / 3.0_real64
        c = 1.0_real64 / sqrt(9.0_real64 * d)
        do
            do
                call stream_normal(self, x)
                v = 1.0_real64 + c * x
                if (v > 0.0_real64) exit
            end do
            v = v * v * v
            call stream_uniform(self, u)
            ! The squeeze: cheap, and correct because `1 - 0.0331*x**4` lies below the true
            ! acceptance probability everywhere. It takes about 95 % of candidates.
            if (u < 1.0_real64 - 0.0331_real64 * x ** 4) then
                path = 1
                ! The squeeze is valid only if everything it accepts the full test would accept
                ! too. That is an EXACT property, so a test can check it directly rather than
                ! hoping a distributional gate resolves the distortion -- which it will not: an
                ! over-aggressive squeeze biases the draw by parts in ten thousand. Evaluated only
                ! when a caller asks, so the shipped path still costs one comparison.
                if (present(squeeze_ok)) &
                    squeeze_ok = log(u) < 0.5_real64 * x * x + d * (1.0_real64 - v + log(v))
                exit
            end if
            if (log(u) < 0.5_real64 * x * x + d * (1.0_real64 - v + log(v))) then
                path = 2
                if (present(squeeze_ok)) squeeze_ok = .true.   ! nothing was shortcut here
                exit
            end if
        end do
        r = boost * d * v
        ! The boost is orthogonal to which acceptance branch fired, so it is encoded as an offset
        ! rather than overwriting: 1/2 unboosted squeeze/log, 3/4 the same two under a boost. An
        ! earlier version set `path = 3` outright and made the inner branch invisible for every
        ! `shape < 1` draw -- which is exactly the half of the domain the boost exists for.
        if (boosted) path = path + 2
    end subroutine gamma_draw

    !> `%poisson` into an `integer(int32)`.
    !!
    !! Refuses a count that does not fit rather than narrowing it, which would silently return a
    !! plausible wrong number. Reaching that needs a `lambda` of order `2**31`, at which point the
    !! `int64` specific is what the caller wants.
    pure subroutine stream_poisson_i32(self, lambda, k)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: lambda              !! the mean; must be >= 0 and finite
        integer(int32), intent(out) :: k                !! a `Poisson(lambda)` count
        integer(int64) :: k64
        integer(int32) :: path
        call poisson_draw(self, lambda, k64, path)
        if (k64 > int(huge(1_int32), int64)) then
            error stop "pf_random_stream%poisson: the drawn count does not fit in an integer(int32); " // &
                       "pass an integer(int64) result for a lambda this large"
        end if
        k = int(k64, int32)
    end subroutine stream_poisson_i32

    !> `%poisson` into an `integer(int64)`.
    pure subroutine stream_poisson_i64(self, lambda, k)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: lambda              !! the mean; must be >= 0 and finite
        integer(int64), intent(out) :: k                !! a `Poisson(lambda)` count
        integer(int32) :: path
        call poisson_draw(self, lambda, k, path)
    end subroutine stream_poisson_i64

    !> The Poisson draw, with the algorithm and accepting branch reported.
    !!
    !! **Two algorithms with a frozen crossover** (`poisson_crossover`), and both must stay: Knuth's
    !! product of uniforms is exact and terminates unconditionally but costs one uniform per unit of
    !! `lambda`, while PTRS is constant-cost but needs `log_gamma` and a rejection loop. Neither is
    !! a fallback for the other -- which one runs is contract, published through
    !! `pf_poisson_algorithm`, because it decides the value.
    !!
    !! `path` is 1 for Knuth, 2 for a PTRS candidate taken by the fast acceptance region, and 3 for
    !! one taken by the full logarithmic test. Nothing in the library reads it.
    pure subroutine poisson_draw(self, lambda, k, path, squeeze_ok)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: lambda              !! the mean; must be >= 0 and finite
        integer(int64), intent(out) :: k                !! a `Poisson(lambda)` count
        integer(int32), intent(out) :: path             !! 1 Knuth, 2 PTRS fast, 3 PTRS log test
        logical, intent(out), optional :: squeeze_ok    !! see below; test-only, costs a `log` when asked
        real(real64) :: lim, p, u, uu, vv, us, b, aa, inv_alpha, v_r, kr

        if (.not. (lambda >= 0.0_real64)) then
            error stop "pf_random_stream%poisson: lambda must be at least 0 (a Poisson " // &
                       "distribution is not defined for a negative mean, and NaN is not a mean)"
        end if
        if (lambda > real(huge(1_int64), real64) / 16.0_real64) then
            error stop "pf_random_stream%poisson: lambda is so large that a drawn count could " // &
                       "overflow integer(int64); this is far past where a Poisson draw is meaningful"
        end if
        call align_to_pair(self)
        if (lambda < poisson_crossover) then
            ! Knuth: multiply uniforms until the product drops below exp(-lambda). Exact, and it
            ! terminates unconditionally because every uniform is strictly below 1. Costs
            ! `lambda + 1` uniforms on average, which is why it is not used above the crossover.
            path = 1
            if (present(squeeze_ok)) squeeze_ok = .true.       ! Knuth shortcuts nothing
            lim = exp(-lambda)
            p = 1.0_real64
            k = 0_int64
            do
                call stream_uniform(self, u)
                p = p * u
                if (p <= lim) exit
                k = k + 1_int64
            end do
            return
        end if
        ! PTRS (Hoermann 1993): transformed rejection with a squeeze. Two uniforms per candidate,
        ! about 99 % accepted, and the cost does not grow with lambda.
        b = 0.931_real64 + 2.53_real64 * sqrt(lambda)
        aa = -0.059_real64 + 0.02483_real64 * b
        inv_alpha = 1.1239_real64 + 1.1328_real64 / (b - 3.4_real64)
        v_r = 0.9277_real64 - 3.6224_real64 / (b - 2.0_real64)
        do
            call stream_uniform(self, uu)
            call stream_uniform(self, vv)
            uu = uu - 0.5_real64
            us = 0.5_real64 - abs(uu)
            ! `kind=int64` is load-bearing: bare `floor` returns a DEFAULT integer, so for a
            ! lambda above 2**31 the candidate wraps to a negative count and the draw comes back
            ! as -2147483648. Caught by scenario_random_poisson_int32_overflow, whose own control
            ! -- the same lambda into an int64 -- was returning the wrapped value too.
            kr = real(floor((2.0_real64 * aa / us + b) * uu + lambda + 0.43_real64, int64), real64)
            if (us >= 0.07_real64 .and. vv <= v_r) then
                path = 2
                ! PTRS's fast region is a rectangle the algorithm asserts lies wholly INSIDE the
                ! acceptance region, so the full test is skipped there. That containment is an
                ! exact property and is checkable directly -- which matters here more than
                ! anywhere, because a widened fast region biases the draw by parts in ten
                ! thousand and no feasible sample size resolves that. Evaluated only on request.
                if (present(squeeze_ok)) &
                    squeeze_ok = log(vv * inv_alpha / (aa / (us * us) + b)) <= &
                                 kr * log(lambda) - lambda - log_gamma(kr + 1.0_real64)
                k = int(kr, int64)
                return
            end if
            if (kr < 0.0_real64 .or. (us < 0.013_real64 .and. vv > us)) cycle
            if (log(vv * inv_alpha / (aa / (us * us) + b)) <= &
                kr * log(lambda) - lambda - log_gamma(kr + 1.0_real64)) then
                path = 3
                if (present(squeeze_ok)) squeeze_ok = .true.   ! nothing was shortcut here
                k = int(kr, int64)
                return
            end if
        end do
    end subroutine poisson_draw

    !> `%normal_portable`: the next polar normal. Not `pure`, per `exp_key`. See `stream_normal`.
    subroutine stream_normal_portable(self, x)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: x                  !! a standard normal draw
        integer(int64) :: d, pairs
        real(real64) :: v
        call align_to_pair(self)
        d = self%pos / 2_int64 + 1_int64
        call polar_normal(self%key, self%stream, d, v, pairs)
        call advance_by(self, 2_int64 * pairs)
        x = v
    end subroutine stream_normal_portable

    !> `%address`: the seed and the stream index this stream was given by `%seed`.
    !!
    !! **The one sphere-era binding that is not a producer.** It exists so a caller can compute a
    !! coordinate-addressed draw at a stream's own coordinates. It is a subroutine with two results
    !! rather than two functions because the names they would need are taken: `%seed` is the
    !! seeding generic, which cannot mix subroutines and functions, and `stream` is a component,
    !! which a binding may not share a name with. A stream never seeded answers `0, 0`.
    pure subroutine stream_address(self, seed, stream)
        class(pf_random_stream), intent(in) :: self     !! the stream to query
        integer(int64), intent(out) :: seed             !! the seed `%seed` was given
        integer(int64), intent(out) :: stream           !! the stream index `%seed` was given
        seed = self%key
        stream = self%stream
    end subroutine stream_address

    !> `%direction`: the next uniform direction, taking one block, block-aligned.
    !!
    !! The value is `pf_random_direction_at(seed, stream, d)` for the block `d` it consumed, whatever
    !! the cursor was beforehand; the raw words of that block are never read, so `%uniform` rewound
    !! onto it sees them fresh.
    pure subroutine stream_direction(self, v)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: v(3)               !! a unit vector uniform on the sphere
        integer(int64) :: d
        call take_block(self, d)
        v = direction_draw(self%key, self%stream, d)
    end subroutine stream_direction

    !> `%radec`: `%direction`'s point as a sky position in degrees, taking one block.
    pure subroutine stream_radec(self, ra, dec)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: ra                 !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec                !! declination, degrees, in `[-90, 90]`
        integer(int64) :: d
        real(real64) :: v(3)                            ! named for `sph_radec`'s explicit-shape dummy
        call take_block(self, d)
        v = direction_draw(self%key, self%stream, d)
        call sph_radec(v, ra, dec)
    end subroutine stream_radec

    !> `%disc`: the next direction within `radius` radians of `centre`, taking one block. See
    !! `pf_random_disc_at` for the arguments and their refusals.
    pure subroutine stream_disc(self, centre, radius, v, r_inner)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: centre(3)           !! the disc's centre; any nonzero finite length
        real(real64), intent(in) :: radius              !! angular radius, radians, at least 0
        real(real64), intent(out) :: v(3)               !! a unit vector uniform in the disc or ring
        real(real64), intent(in), optional :: r_inner   !! inner angular radius in `[0, radius]`
        integer(int64) :: d
        call take_block(self, d)
        v = disc_draw("pf_random_stream%disc", self%key, self%stream, d, centre, radius, r_inner)
    end subroutine stream_disc

    !> `%disc_radec`: `%disc` about a sky position, in degrees, taking one block. See
    !! `pf_random_disc_radec_at`.
    pure subroutine stream_disc_radec(self, ra0, dec0, radius_deg, ra, dec, r_inner_deg)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: ra0                 !! right ascension of the centre, degrees
        real(real64), intent(in) :: dec0                !! declination of the centre, degrees, in `[-90, 90]`
        real(real64), intent(in) :: radius_deg          !! angular radius, degrees, at least 0
        real(real64), intent(out) :: ra                 !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec                !! declination, degrees, in `[-90, 90]`
        real(real64), intent(in), optional :: r_inner_deg !! inner radius, degrees, in `[0, radius_deg]`
        integer(int64) :: d
        call take_block(self, d)
        call disc_radec_draw("pf_random_stream%disc_radec", self%key, self%stream, d, ra0, dec0, radius_deg, &
                             ra, dec, r_inner_deg)
    end subroutine stream_disc_radec

    !> `%ball`: the next point in the ball of `radius` about the origin, or in a shell, taking one
    !! block. See `pf_random_ball_at`.
    pure subroutine stream_ball(self, radius, p, r_inner)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: radius              !! the ball's radius; finite, at least 0
        real(real64), intent(out) :: p(3)               !! a point uniform in the ball or shell
        real(real64), intent(in), optional :: r_inner   !! the shell's inner radius in `[0, radius]`
        integer(int64) :: d
        call take_block(self, d)
        p = ball_draw("pf_random_stream%ball", self%key, self%stream, d, radius, r_inner)
    end subroutine stream_ball

    !> `%vmf`: the next von Mises-Fisher direction about `mu`, taking one block. See `pf_random_vmf_at`.
    pure subroutine stream_vmf(self, mu, kappa, v)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: mu(3)               !! the mean direction; any nonzero finite length
        real(real64), intent(in) :: kappa               !! the concentration; finite, at least 0
        real(real64), intent(out) :: v(3)               !! a unit vector
        integer(int64) :: d
        call take_block(self, d)
        v = vmf_draw("pf_random_stream%vmf", self%key, self%stream, d, mu, kappa)
    end subroutine stream_vmf

    !> `%vmf_radec`: `%vmf` about a sky position by Gaussian width, in degrees, taking one block. See
    !! `pf_random_vmf_radec_at`.
    pure subroutine stream_vmf_radec(self, ra0, dec0, sigma_deg, ra, dec)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(in) :: ra0                 !! right ascension of the centre, degrees
        real(real64), intent(in) :: dec0                !! declination of the centre, degrees, in `[-90, 90]`
        real(real64), intent(in) :: sigma_deg           !! per-axis Gaussian width, degrees; above 0
        real(real64), intent(out) :: ra                 !! right ascension, degrees, in `[0, 360)`
        real(real64), intent(out) :: dec                !! declination, degrees, in `[-90, 90]`
        integer(int64) :: d
        call take_block(self, d)
        call vmf_radec_draw("pf_random_stream%vmf_radec", self%key, self%stream, d, ra0, dec0, sigma_deg, ra, dec)
    end subroutine stream_vmf_radec

    !> `%rotation`: the next uniform rotation matrix, taking one block. See `pf_random_rotation_at`.
    pure subroutine stream_rotation(self, r)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: r(3, 3)            !! a proper rotation matrix
        integer(int64) :: d
        call take_block(self, d)
        r = rotation_draw(self%key, self%stream, d)
    end subroutine stream_rotation

    !> `%fill` for a `real64` array.
    !!
    !! Routes to `fill_r64` whenever the position is pair-aligned, because the bulk fills walk
    !! blocks rather than values and beat even the cached stream by 1.67-1.87x. The values are
    !! identical either way; only the number of encipherings differs.
    pure subroutine stream_fill_r64(self, v)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: v(:)               !! filled with the next `size(v)` values
        integer(int64) :: m, k, start
        real(real64) :: x
        m = size(v, kind=int64)
        if (m <= 0_int64) return                        ! a zero-sized fill is a defined no-op
        call advance_by(self, 2_int64 * m)              ! guard first: an abort writes nothing
        start = self%pos - 2_int64 * m
        if (modulo(start, 2_int64) == 0_int64) then
            call fill_r64(self%key, self%stream, v, start / 2_int64 + 1_int64)
        else
            ! Started mid-pair, so every value straddles a pair boundary -- a position no tier-2
            ! entry point addresses. Correct rather than fast, and rare.
            self%pos = start
            do k = 1_int64, m
                call stream_uniform(self, x)
                v(k) = x
            end do
        end if
    end subroutine stream_fill_r64

    !> `%fill` for a `real32` array. Every position is aligned for it: one value is one word.
    pure subroutine stream_fill_r32(self, v)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real32), intent(out) :: v(:)               !! filled with the next `size(v)` values
        integer(int64) :: m
        m = size(v, kind=int64)
        if (m <= 0_int64) return                        ! a zero-sized fill is a defined no-op
        call advance_by(self, m)
        call fill_r32(self%key, self%stream, v, self%pos - m + 1_int64)
    end subroutine stream_fill_r32

    !> `%fill` for an `integer(int64)` array; aligns to a pair first, exactly as `%int_range` does.
    pure subroutine stream_fill_i64(self, v, lo, hi)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(out) :: v(:)             !! filled with the next `size(v)` values
        integer(int64), intent(in) :: lo                !! one end of the closed range
        integer(int64), intent(in) :: hi                !! the other end; `lo > hi` is swapped
        integer(int64) :: m, d
        m = size(v, kind=int64)
        if (m <= 0_int64) return                        ! a zero-sized fill is a defined no-op
        call align_to_pair(self)
        d = self%pos / 2_int64 + 1_int64
        call advance_by(self, 2_int64 * m)
        call fill_draws_i64(self%key, self%stream, v, lo, hi, d)
    end subroutine stream_fill_i64

    !> `%fill` for an `integer(int32)` array.
    pure subroutine stream_fill_i32(self, v, lo, hi)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int32), intent(out) :: v(:)             !! filled with the next `size(v)` values
        integer(int32), intent(in) :: lo                !! one end of the closed range
        integer(int32), intent(in) :: hi                !! the other end; `lo > hi` is swapped
        integer(int64) :: m, d
        m = size(v, kind=int64)
        if (m <= 0_int64) return                        ! a zero-sized fill is a defined no-op
        call align_to_pair(self)
        d = self%pos / 2_int64 + 1_int64
        call advance_by(self, 2_int64 * m)
        call fill_draws_i32(self%key, self%stream, v, lo, hi, d)
    end subroutine stream_fill_i32

    !> Points the stream at `(seed, stream)`, position 1, holding nothing.
    !!
    !! Dropping the cache is required, not tidiness: `blk` indexes the *previous* family's blocks,
    !! and a reseeded stream that kept it would answer its next draw from the old seed's words.
    !! `-1` is unreachable as a real block index, since a position is never negative.
    pure subroutine reseed(self, seed, stream)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reseed
        integer(int64), intent(in) :: seed              !! the stream family's seed
        integer(int64), intent(in) :: stream            !! which stream of that family
        self%key = seed
        self%stream = stream
        self%pos = 0_int64
        self%blk = -1_int64
    end subroutine reseed

    !> Sets the 1-based position, refusing one that names no word.
    pure subroutine set_pos(self, pos)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reposition
        integer(int64), intent(in) :: pos               !! 1-based word position
        if (pos < 1_int64) then
            error stop "pf_random_stream%rewind: position must be at least 1 (positions are 1-based)"
        end if
        self%pos = pos - 1_int64
    end subroutine set_pos

    !> Reserves the next `w` words and advances past them; the reads then use `pos-w .. pos-1`.
    !!
    !! Advancing BEFORE reading is what makes the guard total: a producer that aborts here has
    !! written nothing and moved nothing, so an exhausted stream is left exactly where it was.
    pure subroutine advance_by(self, w)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(in) :: w                 !! words this producer consumes
        if (self%pos > stream_pos_max - w) then
            error stop "pf_random_stream: the stream is exhausted -- it addresses at most 2**63 words"
        end if
        self%pos = self%pos + w
    end subroutine advance_by

    !> Advances to the next word PAIR boundary if the stream is not already on one.
    !!
    !! `%int_range` and the integer fills need this because the integer rule addresses a word pair
    !! at a fixed grid -- draw `d` is words `2d-2, 2d-1` -- while `%uniform32` can leave the cursor
    !! on an odd word. Without it, an integer draw taken at word 1 would re-read word 0, a bit
    !! pattern an earlier producer had already handed out. Aligning costs at most ONE word, and is
    !! what keeps a stream's `%int_range` equal to the `pf_random_int_at` at the same coordinate.
    !!
    !! It cost up to three words while the integer generic had stride 4 and consumed a whole block
    !! per value; that whole-block alignment is `align_to_block` now, and the points-on-a-sphere
    !! producers are what use it.
    pure subroutine align_to_pair(self)
        class(pf_random_stream), intent(inout) :: self  !! the stream to align
        if (modulo(self%pos, 2_int64) /= 0_int64) call advance_by(self, 1_int64)
    end subroutine align_to_pair

    !> Aligns, then reserves one word pair, returning the 1-based draw index it occupies.
    pure subroutine take_pair(self, draw)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(out) :: draw             !! draw index of the pair reserved
        call align_to_pair(self)
        draw = self%pos / 2_int64 + 1_int64
        call advance_by(self, 2_int64)
    end subroutine take_pair

    !> Advances to the next whole-BLOCK boundary (a multiple of four words) if not already on one.
    !!
    !! The points-on-a-sphere producers need it: each addresses one whole block, so starting between
    !! blocks would re-address a block whose words an earlier producer had handed out. Costs at most
    !! three words.
    pure subroutine align_to_block(self)
        class(pf_random_stream), intent(inout) :: self  !! the stream to align
        integer(int64) :: r
        r = modulo(self%pos, 4_int64)
        if (r /= 0_int64) call advance_by(self, 4_int64 - r)
    end subroutine align_to_block

    !> Aligns to a block, then reserves it, returning the 1-based draw index the block occupies.
    pure subroutine take_block(self, draw)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(out) :: draw             !! draw index of the block reserved
        call align_to_block(self)
        draw = self%pos / 4_int64 + 1_int64
        call advance_by(self, 4_int64)
    end subroutine take_block

    !> One word of the stream, from the held block when it is the right one.
    !!
    !! The only place the cache is read or written. `p` is always a position this call's own
    !! `advance_by` has already checked, so it is in range by construction.
    pure subroutine word_at(self, dom, p, w)
        class(pf_random_stream), intent(inout) :: self  !! the stream holding the cache
        integer(int64), intent(in) :: dom               !! which word space to read; see `DOM_REAL64`
        integer(int64), intent(in) :: p                 !! 0-based word position within that space
        integer(int64), intent(out) :: w                !! that word, in `[0, 2**32)`
        integer(int64) :: want
        want = ior(dom, p / 4_int64)
        if (want /= self%blk) then
            call random_block(self%key, self%stream, want, self%c0, self%c1, self%c2, self%c3)
            self%blk = want
        end if
        select case (int(modulo(p, 4_int64), int32))
        case (0)
            w = self%c0
        case (1)
            w = self%c1
        case (2)
            w = self%c2
        case default
            w = self%c3
        end select
    end subroutine word_at


end module parquet_random ! GCOVR_EXCL_LINE
