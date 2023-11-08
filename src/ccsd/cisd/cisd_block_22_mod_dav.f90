module cisd_block_22_mod_dav
use cisd_block_22_mod_part1_dav
use cisd_block_22_mod_part2_dav
use cisd_block_22_mod_part3_dav
use cisd_block_22_mod_part4_dav
use cisd_block_22_mod_part5_dav
implicit none
!
! File generated automatically on 2014-11-13 22:32:53 UTC.
!
contains
subroutine cisd_block_22_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
 bra0, ket0, eorb, e_nuclear)
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: bra0, ket0
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l
real(F64), dimension(:), intent(in)                 :: eorb
real(F64), intent(in)                               :: e_nuclear
real(F64)                                           :: hci_ibra_iket
!
! Generating full CC Jacobian
!
call cisd_block_22_part1_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
 bra0, ket0, eorb, e_nuclear)
call cisd_block_22_part2_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
 bra0, ket0, eorb, e_nuclear)
call cisd_block_22_part3_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
 bra0, ket0, eorb, e_nuclear)
call cisd_block_22_part4_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
 bra0, ket0, eorb, e_nuclear)
call cisd_block_22_part5_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
 bra0, ket0, eorb, e_nuclear)
end subroutine cisd_block_22_dav
end module cisd_block_22_mod_dav
