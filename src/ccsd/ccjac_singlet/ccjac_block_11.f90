module ccjac_block_11
use eom_ccsd_11_trans
use davidson_main
 
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-15 01:32:59 UTC.
!
contains
 
subroutine ccjac_11(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
double precision, dimension(:,:), intent(out)       :: jac
double precision, dimension(:), intent(in)          :: eorb
double precision, dimension(:, :), intent(in)       :: t1
double precision, dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
integer :: a, b
integer :: i, j
integer :: ai, bj
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket
integer :: braoffset, ketoffset
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
!
! Elementary loop 1
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a
! No equalities independent of the above can hold.
!
j_aiaj: do j = nocc0, nocc1
a_aiaj: do a = nvirt0, nvirt1
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiaj: do i = nocc0, nocc1
if (i == j) cycle i_aiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + bj
jac(ibra, iket) = eom_ccsd_11_trans_aiaj(eorb, t2, t1, nocc, nactive, a, i, j)
end do i_aiaj
end do a_aiaj
end do j_aiaj
!
! Elementary loop 2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: j == i
! No equalities independent of the above can hold.
!
b_aibi: do b = nvirt0, nvirt1
a_aibi: do a = nvirt0, nvirt1
if (a == b) cycle a_aibi
i_aibi: do i = nocc0, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + bj
jac(ibra, iket) = eom_ccsd_11_trans_aibi(eorb, t2, t1, nocc, nactive, a, i, b)
end do i_aibi
end do a_aibi
end do b_aibi
!
! Elementary loop 3
! --------------------
! Free virtual indices: a
! Free occupied indices: i
! Equalities: b == a, j == i
! No equalities independent of the above can hold.
!
a_aiai: do a = nvirt0, nvirt1
i_aiai: do i = nocc0, nocc1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + bj
jac(ibra, iket) = eom_ccsd_11_trans_aiai(eorb, t2, t1, nocc, nactive, a, i)
end do i_aiai
end do a_aiai
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: none
! No equalities independent of the above can hold.
!
b_aibj: do b = nvirt0, nvirt1
j_aibj: do j = nocc0, nocc1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibj: do a = nvirt0, nvirt1
if (a == b) cycle a_aibj
i_aibj: do i = nocc0, nocc1
if (i == j) cycle i_aibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + bj
jac(ibra, iket) = eom_ccsd_11_trans_aibj(eorb, t2, t1, nocc, nactive, a, i, b, j)
end do i_aibj
end do a_aibj
end do j_aibj
end do b_aibj
end subroutine ccjac_11
end module ccjac_block_11
