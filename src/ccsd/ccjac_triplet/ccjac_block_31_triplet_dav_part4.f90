module ccjac_block_31_triplet_dav_part4
use cc_gparams
use v1_eom_cc3_31_triplet_trans
use v2_eom_cc3_31_triplet_trans
use v3_eom_cc3_31_triplet_trans
use v4_eom_cc3_31_triplet_trans
use v5_eom_cc3_31_triplet_trans
use v6_eom_cc3_31_triplet_trans
use v7_eom_cc3_31_triplet_trans
use v8_eom_cc3_31_triplet_trans
use v9_eom_cc3_31_triplet_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2017-01-20 13:57:18 UTC.
!
contains
subroutine ccjac_31_triplet_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
 nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, &
 n1k, n0l, n1l, bra0, ket0) 
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b
integer :: i, k
integer :: ai, bj, ck, dl
integer :: a1, i0
integer :: n0acd, n0ijl
integer :: n1acd, n1ijl
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
!
! Offset of the jacobian blocks
!
braoffset = bra0 - 1
ketoffset = ket0 - 1
!
! Number of occupied and virtual orbitals
! present in calculations
!
nocc = nocc1 - nocc0 + 1
nvirt = nvirt1 - nvirt0 + 1
npair = nocc * nvirt
nactive = nocc + nvirt
qbj  = 3 + 6 * npair
qbj2 = -3
qck  = 2 + 3 * npair * (2 + npair)
qck2 = -3 * (1 + npair)
q00  = -3 * npair * (3 + npair)
n0acd = max(n0a, n0c, n0d)
n0ijl = max(n0i, n0j, n0l)
n1acd = min(n1a, n1c, n1d)
n1ijl = min(n1i, n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakai: do k = n0k, n1k
b_aibiakai: do b = n0b, n1b
a1 = min(b - 1, n1acd)
a_aibiakai: do a = n0acd, a1
if (a == b) cycle a_aibiakai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakai: do i = i0, n1ijl
if (i == k) cycle i_aibiakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v7_eom_cc3_31_triplet_trans_aibiakai(t2, nocc, nactive, a, i, b, &
 k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakai
end do a_aibiakai
end do b_aibiakai
end do k_aibiakai
contains
!
! Locally visible functions
!
function mu3(ai, bj, ck)
!
! Compute compound three-electron index
! (assumed that ai >= bj >= ck)
!
integer :: mu3
integer, intent(in) :: ai, bj, ck
integer :: mu31, mu32
!
! Compound index is compouted relative
! to the first element of matrix block.
! Braoffset/ketoffset should be added
! to get the absolute position
!
mu31 = (qbj + qbj2 * bj) * bj 
mu32 = (qck + (qck2 + ck) * ck) * ck
mu3  = ai + (mu31 + mu32 + q00) / 6
end function mu3
end subroutine ccjac_31_triplet_dav_part4
end module ccjac_block_31_triplet_dav_part4
