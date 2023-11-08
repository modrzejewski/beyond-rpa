module cisd_block_21_mod_dav
use cisd21
use math_constants
use arithmetic
use cmpidx
use davidson_main
implicit none
!
! File generated automatically on 2014-11-13 22:33:16 UTC.
!
contains
subroutine cisd_block_21_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, bra0, ket0, eorb, e_nuclear) 

procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k
integer, intent(in)                                 :: bra0, ket0
real(F64), dimension(:), intent(in)                 :: eorb
real(F64), intent(in)                               :: e_nuclear
real(F64) :: hci_ibra_iket
!
! Local variables
!
integer :: a, b, c
integer :: i, j, k
integer :: ai, bj, ck
integer :: a0, i0
integer :: n0ab, n0abc, n0ac, n0bc, n0ij
integer :: n0ijk, n0ik, n0jk
integer :: n1ab, n1abc, n1ac, n1bc, n1ij
integer :: n1ijk, n1ik, n1jk
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
n0abc = max(n0a, n0b, n0c)
n0ac = max(n0a, n0c)
n0bc = max(n0b, n0c)
n0ij = max(n0i, n0j)
n0ijk = max(n0i, n0j, n0k)
n0ik = max(n0i, n0k)
n0jk = max(n0j, n0k)
n1ab = min(n1a, n1b)
n1abc = min(n1a, n1b, n1c)
n1ac = min(n1a, n1c)
n1bc = min(n1b, n1c)
n1ij = min(n1i, n1j)
n1ijk = min(n1i, n1j, n1k)
n1ik = min(n1i, n1k)
n1jk = min(n1j, n1k)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a
! No equalities independent of the above can hold.
!
k_aibjak: do k = n0k, n1k
b_aibjak: do b = n0b, n1b
j_aibjak: do j = n0j, n1j
if (j == k) cycle j_aibjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjak: do a = a0, n1ac
if (a == b) cycle a_aibjak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibjak(i, b, j, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjak
end do a_aibjak
end do j_aibjak
end do b_aibjak
end do k_aibjak
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b
! No equalities independent of the above can hold.
!
k_aibjbk: do k = n0k, n1k
b_aibjbk: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbk: do j = n0j, n1j
if (j == k) cycle j_aibjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbk: do a = a0, n1a
if (a == b) cycle a_aibjbk
i_aibjbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibjbk(a, i, j, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjbk
end do a_aibjbk
end do j_aibjbk
end do b_aibjbk
end do k_aibjbk
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: k == j
! No equalities independent of the above can hold.
!
c_aibjcj: do c = n0c, n1c
b_aibjcj: do b = n0b, n1b
if (b == c) cycle b_aibjcj
j_aibjcj: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcj
i_aibjcj: do i = n0i, n1i
if (i == j) cycle i_aibjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibjcj(a, i, b, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjcj
end do a_aibjcj
end do j_aibjcj
end do b_aibjcj
end do c_aibjcj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: k == i
! No equalities independent of the above can hold.
!
c_aibjci: do c = n0c, n1c
b_aibjci: do b = n0b, n1b
if (b == c) cycle b_aibjci
j_aibjci: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjci
i_aibjci: do i = n0ik, n1ik
if (i == j) cycle i_aibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibjci(a, b, j, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjci
end do a_aibjci
end do j_aibjci
end do b_aibjci
end do c_aibjci
!
! Elementary loop  5
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k
! Equalities: b == a, c == a
! No equalities independent of the above can hold.
!
k_aiajak: do k = n0k, n1k
j_aiajak: do j = n0j, n1j
if (j == k) cycle j_aiajak
a_aiajak: do a = n0abc, n1abc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajak: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aiajak(a, i, j, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajak
end do a_aiajak
end do j_aiajak
end do k_aiajak
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, j == i
! No equalities independent of the above can hold.
!
k_aibiak: do k = n0k, n1k
b_aibiak: do b = n0b, n1b
a0 = max(b + 1, n0ac)
a_aibiak: do a = a0, n1ac
if (a == b) cycle a_aibiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiak: do i = n0ij, n1ij
if (i == k) cycle i_aibiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibiak(i, b, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibiak
end do a_aibiak
end do b_aibiak
end do k_aibiak
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, k == j
! No equalities independent of the above can hold.
!
b_aibjaj: do b = n0b, n1b
j_aibjaj: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaj: do a = a0, n1ac
if (a == b) cycle a_aibjaj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaj: do i = n0i, n1i
if (i == j) cycle i_aibjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibjaj(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjaj
end do a_aibjaj
end do j_aibjaj
end do b_aibjaj
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
b_aibjai: do b = n0b, n1b
j_aibjai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjai: do a = a0, n1ac
if (a == b) cycle a_aibjai
i_aibjai: do i = n0ik, n1ik
if (i == j) cycle i_aibjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibjai(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjai
end do a_aibjai
end do j_aibjai
end do b_aibjai
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, k == j
! No equalities independent of the above can hold.
!
c_aiajcj: do c = n0c, n1c
j_aiajcj: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcj: do a = n0ab, n1ab
if (a == c) cycle a_aiajcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcj: do i = i0, n1i
if (i == j) cycle i_aiajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aiajcj(a, i, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajcj
end do a_aiajcj
end do j_aiajcj
end do c_aiajcj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, k == i
! No equalities independent of the above can hold.
!
c_aiajci: do c = n0c, n1c
j_aiajci: do j = n0j, n1j
a_aiajci: do a = n0ab, n1ab
if (a == c) cycle a_aiajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajci: do i = i0, n1ik
if (i == j) cycle i_aiajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aiajci(a, j, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajci
end do a_aiajci
end do j_aiajci
end do c_aiajci
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, j == i
! No equalities independent of the above can hold.
!
k_aibibk: do k = n0k, n1k
b_aibibk: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibk: do a = a0, n1a
if (a == b) cycle a_aibibk
i_aibibk: do i = n0ij, n1ij
if (i == k) cycle i_aibibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibibk(a, i, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibibk
end do a_aibibk
end do b_aibibk
end do k_aibibk
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
b_aibjbj: do b = n0bc, n1bc
j_aibjbj: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbj: do a = a0, n1a
if (a == b) cycle a_aibjbj
i_aibjbj: do i = n0i, n1i
if (i == j) cycle i_aibjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibjbj(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjbj
end do a_aibjbj
end do j_aibjbj
end do b_aibjbj
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
b_aibjbi: do b = n0bc, n1bc
j_aibjbi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbi: do a = a0, n1a
if (a == b) cycle a_aibjbi
i_aibjbi: do i = n0ik, n1ik
if (i == j) cycle i_aibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibjbi(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjbi
end do a_aibjbi
end do j_aibjbi
end do b_aibjbi
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i
! Equalities: j == i, k == i
! No equalities independent of the above can hold.
!
c_aibici: do c = n0c, n1c
b_aibici: do b = n0b, n1b
if (b == c) cycle b_aibici
a0 = max(b + 1, n0a)
a_aibici: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibici
i_aibici: do i = n0ijk, n1ijk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibici(a, i, b, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibici
end do a_aibici
end do b_aibici
end do c_aibici
!
! Elementary loop  15
! --------------------
! Free virtual indices: a
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i
! No equalities independent of the above can hold.
!
k_aiaiak: do k = n0k, n1k
a_aiaiak: do a = n0abc, n1abc
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiak: do i = n0ij, n1ij
if (i == k) cycle i_aiaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aiaiak(a, i, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiaiak
end do a_aiaiak
end do k_aiaiak
!
! Elementary loop  16
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i
! Equalities: b == a, c == a, k == j
! No equalities independent of the above can hold.
!
j_aiajaj: do j = n0jk, n1jk
a_aiajaj: do a = n0abc, n1abc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajaj: do i = i0, n1i
if (i == j) cycle i_aiajaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aiajaj(a, i, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajaj
end do a_aiajaj
end do j_aiajaj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i
! No equalities independent of the above can hold.
!
j_aiajai: do j = n0j, n1j
a_aiajai: do a = n0abc, n1abc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajai: do i = i0, n1ik
if (i == j) cycle i_aiajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aiajai(a, i, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajai
end do a_aiajai
end do j_aiajai
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, j == i, k == i
! No equalities independent of the above can hold.
!
b_aibiai: do b = n0b, n1b
a0 = max(b + 1, n0ac)
a_aibiai: do a = a0, n1ac
if (a == b) cycle a_aibiai
i_aibiai: do i = n0ijk, n1ijk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibiai(a, i, b)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibiai
end do a_aibiai
end do b_aibiai
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i
! Equalities: b == a, j == i, k == i
! No equalities independent of the above can hold.
!
c_aiaici: do c = n0c, n1c
a_aiaici: do a = n0ab, n1ab
if (a == c) cycle a_aiaici
i_aiaici: do i = n0ijk, n1ijk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aiaici(a, i, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiaici
end do a_aiaici
end do c_aiaici
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i
! Equalities: c == b, j == i, k == i
! No equalities independent of the above can hold.
!
b_aibibi: do b = n0bc, n1bc
a0 = max(b + 1, n0a)
a_aibibi: do a = a0, n1a
if (a == b) cycle a_aibibi
i_aibibi: do i = n0ijk, n1ijk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aibibi(a, i, b)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibibi
end do a_aibibi
end do b_aibibi
!
! Elementary loop  21
! --------------------
! Free virtual indices: a
! Free occupied indices: i
! Equalities: b == a, c == a, j == i, k == i
! No equalities independent of the above can hold.
!
a_aiaiai: do a = n0abc, n1abc
i_aiaiai: do i = n0ijk, n1ijk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + ck
hci_ibra_iket = cisd21_aiaiai(a, i)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiaiai
end do a_aiaiai
end subroutine cisd_block_21_dav
end module cisd_block_21_mod_dav
