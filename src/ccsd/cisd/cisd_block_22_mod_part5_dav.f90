module cisd_block_22_mod_part5_dav
use cisd22
use math_constants
use arithmetic
use cmpidx
use davidson_main
implicit none
!
! File generated automatically on 2014-11-13 22:32:53 UTC.
!
contains
subroutine cisd_block_22_part5_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, &
 bra0, ket0, eorb, e_nuclear) 
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l
integer, intent(in)                                 :: bra0, ket0
real(F64), dimension(:), intent(in)                 :: eorb
real(F64), intent(in)                               :: e_nuclear
real(F64) :: hci_ibra_iket
!
! Local variables
!
integer :: a, b, c
integer :: i
integer :: ai, bj, ck, dl
integer :: a0, a1
integer :: n0ab, n0abcd, n0abd, n0bcd, n0cd
integer :: n0ijkl
integer :: n1ab, n1abcd, n1abd, n1bcd, n1cd
integer :: n1ijkl
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
n0abcd = max(n0a, n0b, n0c, n0d)
n0abd = max(n0a, n0b, n0d)
n0bcd = max(n0b, n0c, n0d)
n0cd = max(n0c, n0d)
n0ijkl = max(n0i, n0j, n0k, n0l)
n1ab = min(n1a, n1b)
n1abcd = min(n1a, n1b, n1c, n1d)
n1abd = min(n1a, n1b, n1d)
n1bcd = min(n1b, n1c, n1d)
n1cd = min(n1c, n1d)
n1ijkl = min(n1i, n1j, n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i
! Equalities: c == b, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibibibi: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibibibi: do a = a0, n1a
if (a == b) cycle a_aibibibi
i_aibibibi: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibibibi(a, i, b)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibibibi
end do a_aibibibi
end do b_aibibibi
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i
! Equalities: b == a, d == c, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiaicici: do c = n0cd, n1cd
a_aiaicici: do a = n0ab, n1ab
if (a == c) cycle a_aiaicici
i_aiaicici: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiaicici(a, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiaicici
end do a_aiaicici
end do c_aiaicici
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i
! Equalities: b == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiaiciai: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiaiciai: do a = n0abd, a1
if (a == c) cycle a_aiaiciai
i_aiaiciai: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiaiciai(a, i, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiaiciai
end do a_aiaiciai
end do c_aiaiciai
!
! Elementary loop  4
! --------------------
! Free virtual indices: a
! Free occupied indices: i
! Equalities: b == a, c == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
a_aiaiaiai: do a = n0abcd, n1abcd
i_aiaiaiai: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiaiaiai(eorb, nocc, a, i)
hci_ibra_iket = hci_ibra_iket + e_nuclear
call sigup_diag(hci_ibra_iket, ibra)
end do i_aiaiaiai
end do a_aiaiaiai
end subroutine cisd_block_22_part5_dav
end module cisd_block_22_mod_part5_dav
