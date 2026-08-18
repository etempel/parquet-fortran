!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Driver for `tools/check_exp_key.sh`: fingerprints the frozen `-log(u)` transform.
!>
!> Not an `fpm` target and not a test. It exists to be compiled standalone, under several
!> compilers and several optimisation settings, so that the one thing a unit test cannot check --
!> that the transform gives the SAME bits everywhere -- can be checked at all. See the script's
!> own header for why that matters.
!>
!> `--accuracy` instead reports the worst error against a `real128` reference, which is a
!> different question (is it close to `-log(u)`?) and is deliberately not what the script asserts.
program check_exp_key

    use iso_fortran_env, only: int64, real64, real128, output_unit
    use parquet_expkey, only: parquet_debug_exp_key

    implicit none

    ! Every exponent a race key can carry, times 256 mantissas each. `scale` is exact, so the
    ! inputs themselves are identical on every platform and only the transform can differ.
    integer, parameter :: kmax = 53, nm = 256
    integer(int64) :: fp
    real(real64) :: u, e, worst, err, ref
    integer :: k, j, nargs
    character(len=32) :: arg
    logical :: accuracy, contract

    accuracy = .false.
    contract = .false.
    nargs = command_argument_count()
    do j = 1, nargs
        call get_command_argument(j, arg)
        if (trim(arg) == '--accuracy') accuracy = .true.
        if (trim(arg) == '--contract') contract = .true.
    end do

    fp = 0_int64
    worst = 0.0_real64
    do k = 0, kmax
        do j = 0, nm - 1
            u = scale(0.5_real64 + real(j, real64) / real(2 * nm, real64), -k)
            e = parquet_debug_exp_key(u)
            fp = ieor(fp, transfer(e, 0_int64))
            fp = fp * 6364136223846793005_int64 + 1442695040888963407_int64
            if (accuracy) then
                ref = real(-log(real(u, real128)), real64)
                if (ref /= 0.0_real64) then
                    err = abs(e - ref) / spacing(ref)
                    if (err > worst) worst = err
                end if
            end if
        end do
    end do
    ! u = 1 exactly: the one value a reader checks by eye, and the only one whose answer is 0.
    e = parquet_debug_exp_key(1.0_real64)
    fp = ieor(fp, transfer(e, 0_int64))

    ! The 32-value subset `exp_key_contract_ok` re-derives at run time. Kept identical to the
    ! loop in src/parquet_random.f90; this is where its expected value comes from.
    if (contract) then
        fp = 0_int64
        do k = 0, 31
            u = scale(0.5_real64 + real(mod(k * 5, 8), real64) / 16.0_real64, -k)
            e = parquet_debug_exp_key(u)
            fp = ieor(fp, transfer(e, 0_int64))
            fp = fp * 6364136223846793005_int64 + 1442695040888963407_int64
        end do
        write(output_unit, '(a,i0)') 'CONTRACT_FP ', fp
        stop
    end if

    if (accuracy) then
        write(output_unit, '(a,f10.4)') 'WORST_ULP ', worst
        write(output_unit, '(a,es14.6)') 'AT_ONE    ', e
    else
        write(output_unit, '(a,i0)') 'EXPKEY_FINGERPRINT ', fp
    end if

end program check_exp_key
