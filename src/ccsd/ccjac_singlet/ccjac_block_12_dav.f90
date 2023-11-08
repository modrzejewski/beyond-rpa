module ccjac_block_12_dav
use eom_ccsd_12_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams 
use cc_gparams 
implicit none
!
! File generated automatically on 2013-07-13 00:46:39 UTC.
!
contains
subroutine ccjac_12_dav(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, nvirt1, &
 n0a, n1a, n0b, n1b, n0c, n1c, n0i, n1i, n0j, n1j, n0k, n1k, bra0, ket0) 
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c
integer :: i, j, k
integer :: ai, bj, ck
integer :: a0, a1, b0, i0, i1, j0
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
! Elementary loop 1
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a
! No equalities independent of the above can hold.
!
c_aiajck: do c = n0c, n1c
k_aiajck: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajck: do j = n0j, n1j
if (j == k) cycle j_aiajck
a0 = max(c + 1, n0ab)
a_aiajck: do a = a0, n1ab
if (a == c) cycle a_aiajck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiajck(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiajck
end do a_aiajck
end do j_aiajck
end do k_aiajck
end do c_aiajck
!
! Elementary loop 2
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
a1 = min(b - 1, n1ac)
a_aibjak: do a = n0ac, a1
if (a == b) cycle a_aibjak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibjak(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibjak
end do a_aibjak
end do j_aibjak
end do b_aibjak
end do k_aibjak
!
! Elementary loop 3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: j == i
! No equalities independent of the above can hold.
!
c_aibick: do c = n0c, n1c
k_aibick: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibick: do b = b0, n1b
if (b == c) cycle b_aibick
a_aibick: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibick
i_aibick: do i = n0ij, n1ij
if (i == k) cycle i_aibick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibick(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibick
end do a_aibick
end do b_aibick
end do k_aibick
end do c_aibick
!
! Elementary loop 4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: k == i
! No equalities independent of the above can hold.
!
c_aibjci: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjci: do b = b0, n1b
if (b == c) cycle b_aibjci
j_aibjci: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjci
i_aibjci: do i = n0ik, n1ik
if (i == j) cycle i_aibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibjci(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibjci
end do a_aibjci
end do j_aibjci
end do b_aibjci
end do c_aibjci
!
! Elementary loop 5
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k
! Equalities: b == a, c == a
! No equalities independent of the above can hold.
!
k_aiajak: do k = n0k, n1k
j0 = max(k + 1, n0j)
j_aiajak: do j = j0, n1j
if (j == k) cycle j_aiajak
a_aiajak: do a = n0abc, n1abc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiajak(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiajak
end do a_aiajak
end do j_aiajak
end do k_aiajak
!
! Elementary loop 6
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, j == i
! No equalities independent of the above can hold.
!
k_aibibk: do k = n0k, n1k
b_aibibk: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibibk: do a = n0a, n1a
if (a == b) cycle a_aibibk
i0 = max(k + 1, n0ij)
i_aibibk: do i = i0, n1ij
if (i == k) cycle i_aibibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibibk(a, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibibk
end do a_aibibk
end do b_aibibk
end do k_aibibk
!
! Elementary loop 7
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
b_aibjbi: do b = n0bc, n1bc
j_aibjbi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjbi: do a = n0a, n1a
if (a == b) cycle a_aibjbi
i1 = min(j - 1, n1ik)
i_aibjbi: do i = n0ik, i1
if (i == j) cycle i_aibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibjbi(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibjbi
end do a_aibjbi
end do j_aibjbi
end do b_aibjbi
!
! Elementary loop 8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, j == i
! No equalities independent of the above can hold.
!
c_aiaick: do c = n0c, n1c
k_aiaick: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaick: do a = a0, n1ab
if (a == c) cycle a_aiaick
i_aiaick: do i = n0ij, n1ij
if (i == k) cycle i_aiaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiaick(nocc, a, i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiaick
end do a_aiaick
end do k_aiaick
end do c_aiaick
!
! Elementary loop 9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, k == j
! No equalities independent of the above can hold.
!
c_aiajcj: do c = n0c, n1c
j_aiajcj: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcj: do a = a0, n1ab
if (a == c) cycle a_aiajcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajcj: do i = n0i, n1i
if (i == j) cycle i_aiajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiajcj(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiajcj
end do a_aiajcj
end do j_aiajcj
end do c_aiajcj
!
! Elementary loop 10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, k == i
! No equalities independent of the above can hold.
!
c_aiajci: do c = n0c, n1c
j_aiajci: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajci: do a = a0, n1ab
if (a == c) cycle a_aiajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajci: do i = n0ik, n1ik
if (i == j) cycle i_aiajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiajci(nocc, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiajci
end do a_aiajci
end do j_aiajci
end do c_aiajci
!
! Elementary loop 11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, j == i
! No equalities independent of the above can hold.
!
k_aibiak: do k = n0k, n1k
b_aibiak: do b = n0b, n1b
a1 = min(b - 1, n1ac)
a_aibiak: do a = n0ac, a1
if (a == b) cycle a_aibiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiak: do i = n0ij, n1ij
if (i == k) cycle i_aibiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibiak(nocc, a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibiak
end do a_aibiak
end do b_aibiak
end do k_aibiak
!
! Elementary loop 12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, k == j
! No equalities independent of the above can hold.
!
b_aibjaj: do b = n0b, n1b
j_aibjaj: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaj: do a = n0ac, a1
if (a == b) cycle a_aibjaj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaj: do i = n0i, n1i
if (i == j) cycle i_aibjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibjaj(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibjaj
end do a_aibjaj
end do j_aibjaj
end do b_aibjaj
!
! Elementary loop 13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
b_aibjai: do b = n0b, n1b
j_aibjai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjai: do a = n0ac, a1
if (a == b) cycle a_aibjai
i_aibjai: do i = n0ik, n1ik
if (i == j) cycle i_aibjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibjai(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibjai
end do a_aibjai
end do j_aibjai
end do b_aibjai
!
! Elementary loop 14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i
! Equalities: j == i, k == i
! No equalities independent of the above can hold.
!
c_aibici: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibici: do b = b0, n1b
if (b == c) cycle b_aibici
a_aibici: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibici
i_aibici: do i = n0ijk, n1ijk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibici(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibici
end do a_aibici
end do b_aibici
end do c_aibici
!
! Elementary loop 15
! --------------------
! Free virtual indices: a
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i
! No equalities independent of the above can hold.
!
k_aiaiak: do k = n0k, n1k
a_aiaiak: do a = n0abc, n1abc
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaiak: do i = i0, n1ij
if (i == k) cycle i_aiaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiaiak(nocc, a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiaiak
end do a_aiaiak
end do k_aiaiak
!
! Elementary loop 16
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
i_aiajaj: do i = n0i, n1i
if (i == j) cycle i_aiajaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiajaj(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiajaj
end do a_aiajaj
end do j_aiajaj
!
! Elementary loop 17
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i
! No equalities independent of the above can hold.
!
j_aiajai: do j = n0j, n1j
a_aiajai: do a = n0abc, n1abc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajai: do i = n0ik, i1
if (i == j) cycle i_aiajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiajai(nocc, a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiajai
end do a_aiajai
end do j_aiajai
!
! Elementary loop 18
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i
! Equalities: c == b, j == i, k == i
! No equalities independent of the above can hold.
!
b_aibibi: do b = n0bc, n1bc
a_aibibi: do a = n0a, n1a
if (a == b) cycle a_aibibi
i_aibibi: do i = n0ijk, n1ijk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibibi(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibibi
end do a_aibibi
end do b_aibibi
!
! Elementary loop 19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i
! Equalities: b == a, j == i, k == i
! No equalities independent of the above can hold.
!
c_aiaici: do c = n0c, n1c
a0 = max(c + 1, n0ab)
a_aiaici: do a = a0, n1ab
if (a == c) cycle a_aiaici
i_aiaici: do i = n0ijk, n1ijk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiaici(nocc, a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiaici
end do a_aiaici
end do c_aiaici
!
! Elementary loop 20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, j == i, k == i
! No equalities independent of the above can hold.
!
b_aibiai: do b = n0b, n1b
a1 = min(b - 1, n1ac)
a_aibiai: do a = n0ac, a1
if (a == b) cycle a_aibiai
i_aibiai: do i = n0ijk, n1ijk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aibiai(nocc, a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aibiai
end do a_aibiai
end do b_aibiai
!
! Elementary loop 21
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
ibra = braoffset + ai
iket = ketoffset + &
((2 * npair - ck + 2) * (ck - 1)) / 2 + bj - ck + 1
jac_ibra_iket = eom_ccsd_12_trans_aiaiai(nocc, a, i)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
 ! jac_temp(ibra, iket) = jac_ibra_iket 
 

end do i_aiaiai
end do a_aiaiai
end subroutine ccjac_12_dav
end module ccjac_block_12_dav
