module ccjac_block_32_dav
use ccjac_block_32_dav_part1
use ccjac_block_32_dav_part2
use ccjac_block_32_dav_part3
use ccjac_block_32_dav_part4
use ccjac_block_32_dav_part5
use ccjac_block_32_dav_part6
use ccjac_block_32_dav_part7
use ccjac_block_32_dav_part8
use ccjac_block_32_dav_part9
use ccjac_block_32_dav_part10
use ccjac_block_32_dav_part11
use ccjac_block_32_dav_part12
use ccjac_block_32_dav_part13
use ccjac_block_32_dav_part14
use cc_gparams 
use cc_gparams 
implicit none
!
! File generated automatically on 2013-07-13 00:51:16 UTC.
!
contains
subroutine ccjac_32_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: bra0, ket0, r3limits
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d, n0e, n1e
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m
!
! Generating full CC Jacobian
!
 ! print*,  'part1'
call ccjac_32_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part2'
call ccjac_32_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part3'
call ccjac_32_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part4'
call ccjac_32_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part5'
call ccjac_32_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part6'
call ccjac_32_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part7'
call ccjac_32_dav_part7(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part8'
call ccjac_32_dav_part8(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part9'
call ccjac_32_dav_part9(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part10'
call ccjac_32_dav_part10(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part11'
call ccjac_32_dav_part11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part12'
call ccjac_32_dav_part12(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part13'
call ccjac_32_dav_part13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
 ! print*, 'part4'
call ccjac_32_dav_part14(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m, bra0, ket0, r3limits)
end subroutine ccjac_32_dav
end module ccjac_block_32_dav
