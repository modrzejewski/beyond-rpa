module cisd_block_20_mod
use cisd20
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2014-11-10 21:35:46 UTC.
!
contains
subroutine cisd_block_20(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0i, &
 n1i, n0j, n1j, bra0, ket0) 
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b
integer, intent(in)                                 :: n0i, n1i, n0j, n1j
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
real(F64), dimension(:,:), intent(inout) :: hci
integer :: a, b
integer :: i, j
integer :: ai, bj
integer :: a0, i0
integer :: n0ab, n0ij
integer :: n1ab, n1ij
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
n0ab = max(n0a, n0b)
n0ij = max(n0i, n0j)
n1ab = min(n1a, n1b)
n1ij = min(n1i, n1j)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a
! No equalities independent of the above can hold.
!
j_aiaj: do j = n0j, n1j
a_aiaj: do a = n0ab, n1ab
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiaj: do i = i0, n1i
if (i == j) cycle i_aiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = 1
hci(ibra, iket) = cisd20_aiaj(a, i, j)
end do i_aiaj
end do a_aiaj
end do j_aiaj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: j == i
! No equalities independent of the above can hold.
!
b_aibi: do b = n0b, n1b
a0 = max(b + 1, n0a)
a_aibi: do a = a0, n1a
if (a == b) cycle a_aibi
i_aibi: do i = n0ij, n1ij
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = 1
hci(ibra, iket) = cisd20_aibi(a, i, b)
end do i_aibi
end do a_aibi
end do b_aibi
!
! Elementary loop  3
! --------------------
! Free virtual indices: a
! Free occupied indices: i
! Equalities: b == a, j == i
! No equalities independent of the above can hold.
!
a_aiai: do a = n0ab, n1ab
i_aiai: do i = n0ij, n1ij
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = 1
hci(ibra, iket) = cisd20_aiai(a, i)
end do i_aiai
end do a_aiai
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: none
! No equalities independent of the above can hold.
!
b_aibj: do b = n0b, n1b
j_aibj: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibj: do a = a0, n1a
if (a == b) cycle a_aibj
i_aibj: do i = n0i, n1i
if (i == j) cycle i_aibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = 1
hci(ibra, iket) = cisd20_aibj(a, i, b, j)
end do i_aibj
end do a_aibj
end do j_aibj
end do b_aibj
end subroutine cisd_block_20
end module cisd_block_20_mod
