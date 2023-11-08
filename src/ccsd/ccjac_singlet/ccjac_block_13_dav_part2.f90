module ccjac_block_13_dav_part2
use eom_cc3_13_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-21 13:11:34 UTC.
!
contains
subroutine ccjac_13_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0, offset) 
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
integer, intent(in)                                 :: offset
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: a0, a1, b0, i0, i1
integer :: n0ab, n0abc, n0acd, n0ad, n0bc
integer :: n0cd, n0ij, n0ijk, n0ijl, n0ik
integer :: n0ikl, n0il, n0jk, n0jl, n0kl
integer :: n1ab, n1abc, n1acd, n1ad, n1bc
integer :: n1cd, n1ij, n1ijk, n1ijl, n1ik
integer :: n1ikl, n1il, n1jk, n1jl, n1kl
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket, iket2, ibra2
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
integer :: nj, nb, nk, nc, nl, nd
integer :: mj, mb, mk, mc, ml, md
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

nj = n1j - n0j + 1
nb = n1b - n0b + 1
nk = n1k - n0k + 1
nc = n1c - n0c + 1
nl = n1l - n0l + 1
nd = n1d - n0d + 1
mj = 1
mb = nj
mk = nj * nb
mc = nj * nb * nk
ml = nj * nb * nk * nc
md = nj * nb * nk * nc * nl

n0ab = max(n0a, n0b)
n0abc = max(n0a, n0b, n0c)
n0acd = max(n0a, n0c, n0d)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)

n0cd = max(n0c, n0d)
n0ij = max(n0i, n0j)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1ab = min(n1a, n1b)
n1abc = min(n1a, n1b, n1c)
n1acd = min(n1a, n1c, n1d)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjciaj: do b = b0, n1b
if (b == c) cycle b_aibjciaj
j_aibjciaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ad)
a_aibjciaj: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibjciaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciaj: do i = n0ik, n1ik
if (i == j) cycle i_aibjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, c, i, a, j)
jac_ibra_iket = eom_cc3_13_trans_aibjciaj(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj
end do a_aibjciaj
end do j_aibjciaj
end do b_aibjciaj
end do c_aibjciaj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjciai: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjciai: do b = b0, n1b
if (b == c) cycle b_aibjciai
j_aibjciai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ad)
a_aibjciai: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibjciai
i_aibjciai: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, c, i, a, i)
jac_ibra_iket = eom_cc3_13_trans_aibjciai(i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciai
end do a_aibjciai
end do j_aibjciai
end do b_aibjciai
end do c_aibjciai
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == k
! No equalities independent of the above can hold.
!
c_aibickak: do c = n0c, n1c
k_aibickak: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickak: do b = b0, n1b
if (b == c) cycle b_aibickak
a1 = min(c - 1, n1ad)
a_aibickak: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibickak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibickak: do i = n0ij, n1ij
if (i == k) cycle i_aibickak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, c, k, a, k)
jac_ibra_iket = eom_cc3_13_trans_aibickak(b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickak
end do a_aibickak
end do b_aibickak
end do k_aibickak
end do c_aibickak
!
! Elementary loop  4
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
i0 = max(k + 1, n0ijl)
i_aiaiakdi: do i = i0, n1ijl
if (i == k) cycle i_aiaiakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, i, a, k, d, i)

jac_ibra_iket = eom_cc3_13_trans_aiaiakdi(a, i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakdi
end do a_aiaiakdi
end do k_aiaiakdi
end do d_aiaiakdi
!
! Elementary loop  5
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
i1 = min(j - 1, n1ik)
i_aiajaidj: do i = n0ik, i1
if (i == j) cycle i_aiajaidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, a, i, d, j)
jac_ibra_iket = eom_cc3_13_trans_aiajaidj(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidj
end do a_aiajaidj
end do j_aiajaidj
end do d_aiajaidj
!
! Elementary loop  6
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
i1 = min(j - 1, n1ikl)
i_aiajaidi: do i = n0ikl, i1
if (i == j) cycle i_aiajaidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, a, i, d, i)

jac_ibra_iket = eom_cc3_13_trans_aiajaidi(a, i, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidi
end do a_aiajaidi
end do j_aiajaidi
end do d_aiajaidi
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aiaiakdk: do d = n0d, n1d
k_aiaiakdk: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiaiakdk: do a = a0, n1abc
if (a == d) cycle a_aiaiakdk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaiakdk: do i = i0, n1ij
if (i == k) cycle i_aiaiakdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, i, a, k, d, k)
jac_ibra_iket = eom_cc3_13_trans_aiaiakdk(a, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakdk
end do a_aiaiakdk
end do k_aiaiakdk
end do d_aiaiakdk
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibibkai: do k = n0k, n1k
b_aibibkai: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibibkai: do a = n0ad, a1
if (a == b) cycle a_aibibkai
i0 = max(k + 1, n0ijl)
i_aibibkai: do i = i0, n1ijl
if (i == k) cycle i_aibibkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, b, k, a, i)
jac_ibra_iket = eom_cc3_13_trans_aibibkai(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkai
end do a_aibibkai
end do b_aibibkai
end do k_aibibkai
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiaj: do b = n0bc, n1bc
j_aibjbiaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibjbiaj: do a = n0ad, a1
if (a == b) cycle a_aibjbiaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjbiaj: do i = n0ik, i1
if (i == j) cycle i_aibjbiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, b, i, a, j)
jac_ibra_iket = eom_cc3_13_trans_aibjbiaj(b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiaj
end do a_aibjbiaj
end do j_aibjbiaj
end do b_aibjbiaj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjbiai: do b = n0bc, n1bc
j_aibjbiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibjbiai: do a = n0ad, a1
if (a == b) cycle a_aibjbiai
i1 = min(j - 1, n1ikl)
i_aibjbiai: do i = n0ikl, i1
if (i == j) cycle i_aibjbiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, b, i, a, i)
jac_ibra_iket = eom_cc3_13_trans_aibjbiai(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiai
end do a_aibjbiai
end do j_aibjbiai
end do b_aibjbiai
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkak: do k = n0kl, n1kl
b_aibibkak: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibibkak: do a = n0ad, a1
if (a == b) cycle a_aibibkak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibibkak: do i = i0, n1ij
if (i == k) cycle i_aibibkak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, b, k, a, k)
jac_ibra_iket = eom_cc3_13_trans_aibibkak(b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkak
end do a_aibibkak
end do b_aibibkak
end do k_aibibkak
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == c, j == i, l == i
! No equalities independent of the above can hold.
!
c_aiaickci: do c = n0cd, n1cd
k_aiaickci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickci: do a = a0, n1ab
if (a == c) cycle a_aiaickci
i1 = min(k - 1, n1ijl)
i_aiaickci: do i = n0ijl, i1
if (i == k) cycle i_aiaickci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, i, c, k, c, i)

jac_ibra_iket = eom_cc3_13_trans_aiaickci(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickci
end do a_aiaickci
end do k_aiaickci
end do c_aiaickci
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicj: do c = n0cd, n1cd
j_aiajcicj: do j = n0jl, n1jl
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcicj: do a = a0, n1ab
if (a == c) cycle a_aiajcicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcicj: do i = i0, n1ik
if (i == j) cycle i_aiajcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, c, i, c, j)

jac_ibra_iket = eom_cc3_13_trans_aiajcicj(j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicj
end do a_aiajcicj
end do j_aiajcicj
end do c_aiajcicj
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjci: do c = n0cd, n1cd
j_aiajcjci: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjci: do a = a0, n1ab
if (a == c) cycle a_aiajcjci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aiajcjci: do i = n0il, i1
if (i == j) cycle i_aiajcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, j, c, j, c, i)

jac_ibra_iket = eom_cc3_13_trans_aiajcjci(j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjci
end do a_aiajcjci
end do j_aiajcjci
end do c_aiajcjci
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l
! Equalities: b == a, d == c, j == i, k == i
! No equalities independent of the above can hold.
!
l_aiaicicl: do l = n0l, n1l
c_aiaicicl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaicicl: do a = a0, n1ab
if (a == c) cycle a_aiaicicl
i0 = max(l + 1, n0ijk)
i_aiaicicl: do i = i0, n1ijk
if (i == l) cycle i_aiaicicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(a, i, c, i, c, l)

jac_ibra_iket = eom_cc3_13_trans_aiaicicl(i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicicl
end do a_aiaicicl
end do c_aiaicicl
end do l_aiaicicl
!
! Elementary loop  16
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
i1 = min(k - 1, n1ijl)
i_aibiakai: do i = n0ijl, i1
if (i == k) cycle i_aibiakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, a, k, a, i)

jac_ibra_iket = eom_cc3_13_trans_aibiakai(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakai
end do a_aibiakai
end do b_aibiakai
end do k_aibiakai
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiaj: do b = n0b, n1b
j_aibjaiaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjaiaj: do a = n0acd, a1
if (a == b) cycle a_aibjaiaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjaiaj: do i = i0, n1ik
if (i == j) cycle i_aibjaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, a, i, a, j)

jac_ibra_iket = eom_cc3_13_trans_aibjaiaj(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiaj
end do a_aibjaiaj
end do j_aibjaiaj
end do b_aibjaiaj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajai: do b = n0b, n1b
j_aibjajai: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjajai: do a = n0acd, a1
if (a == b) cycle a_aibjajai
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aibjajai: do i = n0il, i1
if (i == j) cycle i_aibjajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, j, a, j, a, i)

jac_ibra_iket = eom_cc3_13_trans_aibjajai(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajai
end do a_aibjajai
end do j_aibjajai
end do b_aibjajai
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
a1 = min(b - 1, n1acd)
a_aibiaial: do a = n0acd, a1
if (a == b) cycle a_aibiaial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijk)
i_aibiaial: do i = i0, n1ijk
if (i == l) cycle i_aibiaial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
!iket = ketoffset + mu3(bj, ck, dl)
iket = ketoffset  + mu3_mem(b, i, a, i, a, l)

jac_ibra_iket = eom_cc3_13_trans_aibiaial(a, i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaial
end do a_aibiaial
end do b_aibiaial
end do l_aibiaial
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

function mu3_mem(b, j, c, k, d, l)
integer :: mu3_mem
integer, intent(in) :: d, l, b, j, c, k
mu3_mem = offset + (j-n0j)+mj + (b-n0b)*mb + (k-n0k)*mk + (c-n0c)*mc + (l-n0l)*ml  &
+ (d-n0d)*md

end function mu3_mem
end subroutine ccjac_13_dav_part2
end module ccjac_block_13_dav_part2
