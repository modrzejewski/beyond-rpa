module cisd_block_22_mod_part3_dav
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
subroutine cisd_block_22_part3_dav(sigup_diag, sigup_nondiag, nocc0, nocc1, nvirt0, nvirt1, &
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
integer :: a, b, c, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: a0, b1, c0, i0, i1, j0, j1, k0
integer :: n0abc, n0abcd, n0ac, n0acd, n0bd
integer :: n0ij, n0ijk, n0ijkl, n0ijl, n0ik
integer :: n0ikl, n0il, n0jk, n0jkl, n0jl
integer :: n0kl
integer :: n1abc, n1abcd, n1ac, n1acd, n1bd
integer :: n1ij, n1ijk, n1ijkl, n1ijl, n1ik
integer :: n1ikl, n1il, n1jk, n1jkl, n1jl
integer :: n1kl
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
n0abc = max(n0a, n0b, n0c)
n0abcd = max(n0a, n0b, n0c, n0d)
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1abc = min(n1a, n1b, n1c)
n1abcd = min(n1a, n1b, n1c, n1d)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1bd = min(n1b, n1d)
n1ij = min(n1i, n1j)
n1ijk = min(n1i, n1j, n1k)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickbi: do c = n0c, n1c
k_aibickbi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(c - 1, n1bd)
b_aibickbi: do b = n0bd, b1
if (b == c) cycle b_aibickbi
a0 = max(b + 1, n0a)
a_aibickbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibickbi
i_aibickbi: do i = n0ijl, n1ijl
if (i == k) cycle i_aibickbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibickbi(a, i, c, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibickbi
end do a_aibickbi
end do b_aibickbi
end do k_aibickbi
end do c_aibickbi
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcibi: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibi: do b = n0bd, b1
if (b == c) cycle b_aibjcibi
j_aibjcibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibi
i_aibjcibi: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjcibi(a, i, j, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjcibi
end do a_aibjcibi
end do j_aibjcibi
end do b_aibjcibi
end do c_aibjcibi
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbi: do b = n0bd, b1
if (b == c) cycle b_aibjcjbi
j_aibjcjbi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbi
i_aibjcjbi: do i = n0il, n1il
if (i == j) cycle i_aibjcjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjcjbi(a, i, b, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjcjbi
end do a_aibjcjbi
end do j_aibjcjbi
end do b_aibjcjbi
end do c_aibjcjbi
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i
! Equalities: d == b, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbj: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbj: do b = n0bd, b1
if (b == c) cycle b_aibjcjbj
j_aibjcjbj: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbj
i_aibjcjbj: do i = n0i, n1i
if (i == j) cycle i_aibjcjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjcjbj(a, i, j, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjcjbj
end do a_aibjcjbj
end do j_aibjcjbj
end do b_aibjcjbj
end do c_aibjcjbj
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibj: do b = n0bd, b1
if (b == c) cycle b_aibjcibj
j_aibjcibj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibj
i_aibjcibj: do i = n0ik, n1ik
if (i == j) cycle i_aibjcibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjcibj(a, i, b, j, c)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjcibj
end do a_aibjcibj
end do j_aibjcibj
end do b_aibjcibj
end do c_aibjcibj
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, l
! Equalities: d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibicibl: do l = n0l, n1l
c_aibicibl: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibicibl: do b = n0bd, b1
if (b == c) cycle b_aibicibl
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibicibl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicibl
i_aibicibl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibicibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibicibl(a, i, c, l)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibicibl
end do a_aibicibl
end do b_aibicibl
end do c_aibicibl
end do l_aibicibl
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i
! Equalities: j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibicidi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibicidi: do c = c0, n1c
if (c == d) cycle c_aibicidi
b_aibicidi: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibicidi
a0 = max(b + 1, n0a)
a_aibicidi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibicidi
i_aibicidi: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibicidi(a, b, c, d)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibicidi
end do a_aibicidi
end do b_aibicidi
end do c_aibicidi
end do d_aibicidi
!
! Elementary loop  8
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aiajakai: do k = n0k, n1k
j_aiajakai: do j = n0j, n1j
if (j == k) cycle j_aiajakai
a_aiajakai: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajakai: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiajakai(a, i, j, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajakai
end do a_aiajakai
end do j_aiajakai
end do k_aiajakai
!
! Elementary loop  9
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, d == a, l == j
! No equalities independent of the above can hold.
!
k_aiajakaj: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aiajakaj: do j = n0jl, j1
if (j == k) cycle j_aiajakaj
a_aiajakaj: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakaj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiajakaj(a, i, j, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajakaj
end do a_aiajakaj
end do j_aiajakaj
end do k_aiajakaj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a
! Free occupied indices: k, i, j
! Equalities: b == a, c == a, d == a, l == k
! No equalities independent of the above can hold.
!
k_aiajakak: do k = n0kl, n1kl
j_aiajakak: do j = n0j, n1j
if (j == k) cycle j_aiajakak
a_aiajakak: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakak: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiajakak(i, j, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajakak
end do a_aiajakak
end do j_aiajakak
end do k_aiajakak
!
! Elementary loop  11
! --------------------
! Free virtual indices: a
! Free occupied indices: i, k, l
! Equalities: b == a, c == a, d == a, j == i
! No equalities independent of the above can hold.
!
l_aiaiakal: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aiaiakal: do k = k0, n1k
if (k == l) cycle k_aiaiakal
a_aiaiakal: do a = n0abcd, n1abcd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaiakal: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aiaiakal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiaiakal(i, k, l)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiaiakal
end do a_aiaiakal
end do k_aiaiakal
end do l_aiaiakal
!
! Elementary loop  12
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajaial: do l = n0l, n1l
j_aiajaial: do j = n0j, n1j
if (j == l) cycle j_aiajaial
a_aiajaial: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, l + 1, n0ik)
i_aiajaial: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiajaial(a, i, j, l)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajaial
end do a_aiajaial
end do j_aiajaial
end do l_aiajaial
!
! Elementary loop  13
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajajal: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aiajajal: do j = j0, n1jk
if (j == l) cycle j_aiajajal
a_aiajajal: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajal: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajajal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiajajal(a, i, j, l)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajajal
end do a_aiajajal
end do j_aiajajal
end do l_aiajajal
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakai: do k = n0k, n1k
b_aibiakai: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibiakai: do a = a0, n1acd
if (a == b) cycle a_aibiakai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijl)
i_aibiakai: do i = n0ijl, i1
if (i == k) cycle i_aibiakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibiakai(a, i, b, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibiakai
end do a_aibiakai
end do b_aibiakai
end do k_aibiakai
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjaiai: do b = n0b, n1b
j_aibjaiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiai: do a = a0, n1acd
if (a == b) cycle a_aibjaiai
i_aibjaiai: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjaiai(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjaiai
end do a_aibjaiai
end do j_aibjaiai
end do b_aibjaiai
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajai: do b = n0b, n1b
j_aibjajai: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajai: do a = a0, n1acd
if (a == b) cycle a_aibjajai
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aibjajai: do i = n0il, i1
if (i == j) cycle i_aibjajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjajai(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjajai
end do a_aibjajai
end do j_aibjajai
end do b_aibjajai
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, d == a, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjajaj: do b = n0b, n1b
j_aibjajaj: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajaj: do a = a0, n1acd
if (a == b) cycle a_aibjajaj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajaj: do i = n0i, n1i
if (i == j) cycle i_aibjajaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjajaj(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjajaj
end do a_aibjajaj
end do j_aibjajaj
end do b_aibjajaj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiaj: do b = n0b, n1b
j_aibjaiaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiaj: do a = a0, n1acd
if (a == b) cycle a_aibjaiaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjaiaj: do i = i0, n1ik
if (i == j) cycle i_aibjaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjaiaj(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjaiaj
end do a_aibjaiaj
end do j_aibjaiaj
end do b_aibjaiaj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaial: do l = n0l, n1l
b_aibiaial: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibiaial: do a = a0, n1acd
if (a == b) cycle a_aibiaial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijk)
i_aibiaial: do i = i0, n1ijk
if (i == l) cycle i_aibiaial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibiaial(a, i, b, l)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibiaial
end do a_aibiaial
end do b_aibiaial
end do l_aibiaial
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aiaiakdi: do d = n0d, n1d
k_aiaiakdi: do k = n0k, n1k
a0 = max(d + 1, n0abc)
a_aiaiakdi: do a = a0, n1abc
if (a == d) cycle a_aiaiakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiakdi: do i = n0ijl, n1ijl
if (i == k) cycle i_aiaiakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiaiakdi(a, i, k, d)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiaiakdi
end do a_aiaiakdi
end do k_aiaiakdi
end do d_aiaiakdi
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiajaidi: do d = n0d, n1d
j_aiajaidi: do j = n0j, n1j
a0 = max(d + 1, n0abc)
a_aiajaidi: do a = a0, n1abc
if (a == d) cycle a_aiajaidi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajaidi: do i = i0, n1ikl
if (i == j) cycle i_aiajaidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiajaidi(a, i, j, d)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajaidi
end do a_aiajaidi
end do j_aiajaidi
end do d_aiajaidi
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aiajajdi: do d = n0d, n1d
j_aiajajdi: do j = n0jk, n1jk
a0 = max(d + 1, n0abc)
a_aiajajdi: do a = a0, n1abc
if (a == d) cycle a_aiajajdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajajdi: do i = i0, n1il
if (i == j) cycle i_aiajajdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiajajdi(a, i, j, d)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajajdi
end do a_aiajajdi
end do j_aiajajdi
end do d_aiajajdi
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i
! Equalities: b == a, c == a, k == j, l == j
! No equalities independent of the above can hold.
!
d_aiajajdj: do d = n0d, n1d
j_aiajajdj: do j = n0jkl, n1jkl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajajdj: do a = a0, n1abc
if (a == d) cycle a_aiajajdj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajdj: do i = i0, n1i
if (i == j) cycle i_aiajajdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiajajdj(a, i, j, d)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajajdj
end do a_aiajajdj
end do j_aiajajdj
end do d_aiajajdj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj: do d = n0d, n1d
j_aiajaidj: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajaidj: do a = a0, n1abc
if (a == d) cycle a_aiajaidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidj: do i = i0, n1ik
if (i == j) cycle i_aiajaidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiajaidj(a, i, j, d)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiajaidj
end do a_aiajaidj
end do j_aiajaidj
end do d_aiajaidj
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, l
! Equalities: b == a, c == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aiaiaidl: do d = n0d, n1d
l_aiaiaidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiaiaidl: do a = a0, n1abc
if (a == d) cycle a_aiaiaidl
i_aiaiaidl: do i = n0ijk, n1ijk
if (i == l) cycle i_aiaiaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aiaiaidl(a, i, d, l)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aiaiaidl
end do a_aiaiaidl
end do l_aiaiaidl
end do d_aiaiaidl
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakbi: do k = n0k, n1k
b_aibiakbi: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiakbi: do a = a0, n1ac
if (a == b) cycle a_aibiakbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbi: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibiakbi(a, i, b, k)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibiakbi
end do a_aibiakbi
end do b_aibiakbi
end do k_aibiakbi
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjaibi: do b = n0bd, n1bd
j_aibjaibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibi: do a = a0, n1ac
if (a == b) cycle a_aibjaibi
i_aibjaibi: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjaibi(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjaibi
end do a_aibjaibi
end do j_aibjaibi
end do b_aibjaibi
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi: do b = n0bd, n1bd
j_aibjajbi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbi: do a = a0, n1ac
if (a == b) cycle a_aibjajbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbi: do i = n0il, n1il
if (i == j) cycle i_aibjajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjajbi(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjajbi
end do a_aibjajbi
end do j_aibjajbi
end do b_aibjajbi
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, d == b, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjajbj: do b = n0bd, n1bd
j_aibjajbj: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbj: do a = a0, n1ac
if (a == b) cycle a_aibjajbj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbj: do i = n0i, n1i
if (i == j) cycle i_aibjajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjajbj(a, i, b, j)
call sigup_nondiag(hci_ibra_iket, ibra, iket)
end do i_aibjajbj
end do a_aibjajbj
end do j_aibjajbj
end do b_aibjajbj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj: do b = n0bd, n1bd
j_aibjaibj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibj: do a = a0, n1ac
if (a == b) cycle a_aibjaibj
i_aibjaibj: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci_ibra_iket = cisd22_aibjaibj(eorb, nocc, a, i, b, j)
hci_ibra_iket = hci_ibra_iket + e_nuclear
call sigup_diag(hci_ibra_iket, ibra)
end do i_aibjaibj
end do a_aibjaibj
end do j_aibjaibj
end do b_aibjaibj
end subroutine cisd_block_22_part3_dav
end module cisd_block_22_mod_part3_dav
