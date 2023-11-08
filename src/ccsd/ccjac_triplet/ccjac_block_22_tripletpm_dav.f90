module ccjac_block_22_tripletpm_dav
use ccjac_block_22_tripletpm_dav_part1
use ccjac_block_22_tripletpm_dav_part2
use ccjac_block_22_tripletpm_dav_part3
use ccjac_block_22_tripletpm_dav_part4
use ccjac_block_22_tripletpm_dav_part5
use ccjac_block_22_tripletpm_dav_part6
implicit none
!
! File generated automatically on 2018-12-06 14:26:30 UTC.
!
contains
subroutine ccjac_22_tripletpm_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0)
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: bra0, ket0
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l
!
! Generating full CC Jacobian
!
call ccjac_22_tripletpm_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0)
call ccjac_22_tripletpm_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0)
call ccjac_22_tripletpm_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0)
call ccjac_22_tripletpm_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0)
call ccjac_22_tripletpm_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0)
call ccjac_22_tripletpm_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0)
end subroutine ccjac_22_tripletpm_dav
end module ccjac_block_22_tripletpm_dav
