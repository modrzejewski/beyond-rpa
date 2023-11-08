module ccjac_block_31_dav_part3
use eom_cc3_31_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-22 13:23:52 UTC.
!
contains
subroutine ccjac_31_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, b0, i0, i1, j1
integer :: n0ab, n0abd, n0ad, n0bc, n0bcd
integer :: n0bd, n0cd, n0ij, n0ijl, n0ik
integer :: n0ikl, n0il, n0jk, n0jkl, n0jl
integer :: n0kl
integer :: n1ab, n1abd, n1ad, n1bc, n1bcd
integer :: n1bd, n1cd, n1ij, n1ijl, n1ik
integer :: n1ikl, n1il, n1jk, n1jkl, n1jl
integer :: n1kl
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket, iket2, ibra2
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
integer :: ni, na, nj, nb, nk, nc
integer :: mi, ma, mj, mb, mk, mc
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
ni = n1i - n0i + 1
na = n1a - n0a + 1
nj = n1j - n0j + 1
nb = n1b - n0b + 1
nk = n1k - n0k + 1
nc = n1c - n0c + 1
mi = 1
ma = ni
mj = ni * na
mb = ni * na * nj
mk = ni * na * nj * nb
mc = ni * na * nj * nb * nk
n0ab = max(n0a, n0b)
n0abd = max(n0a, n0b, n0d)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0bd = max(n0b, n0d)
n0cd = max(n0c, n0d)
n0ij = max(n0i, n0j)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
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
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajcial: do l = n0l, n1l
c_aiajcial: do c = n0c, n1c
j_aiajcial: do j = n0j, n1j
if (j == l) cycle j_aiajcial
a0 = max(c + 1, n0abd)
a_aiajcial: do a = a0, n1abd
if (a == c) cycle a_aiajcial
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcial: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajcial(t2, nocc, nactive, a, i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcial
end do a_aiajcial
end do j_aiajcial
end do c_aiajcial
end do l_aiajcial
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == a, l == k
! No equalities independent of the above can hold.
!
c_aiajckak: do c = n0c, n1c
k_aiajckak: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckak: do j = n0j, j1
if (j == k) cycle j_aiajckak
a0 = max(c + 1, n0abd)
a_aiajckak: do a = a0, n1abd
if (a == c) cycle a_aiajckak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckak: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckak(t2, nocc, nactive, a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckak
end do a_aiajckak
end do j_aiajckak
end do k_aiajckak
end do c_aiajckak
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i
! No equalities independent of the above can hold.
!
c_aiajckai: do c = n0c, n1c
k_aiajckai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckai: do j = n0j, j1
if (j == k) cycle j_aiajckai
a0 = max(c + 1, n0abd)
a_aiajckai: do a = a0, n1abd
if (a == c) cycle a_aiajckai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckai: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckai(t2, nocc, nactive, a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckai
end do a_aiajckai
end do j_aiajckai
end do k_aiajckai
end do c_aiajckai
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i
! Equalities: b == a, k == j, l == j
! No equalities independent of the above can hold.
!
d_aiajcjdj: do d = n0d, n1d
c_aiajcjdj: do c = n0c, n1c
if (c == d) cycle c_aiajcjdj
j_aiajcjdj: do j = n0jkl, n1jkl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjdj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcjdj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjdj: do i = i0, n1i
if (i == j) cycle i_aiajcjdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aiajcjdj(t2, nocc, nactive, a, i, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdj
end do a_aiajcjdj
end do j_aiajcjdj
end do c_aiajcjdj
end do d_aiajcjdj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdi: do d = n0d, n1d
c_aiajcjdi: do c = n0c, n1c
if (c == d) cycle c_aiajcjdi
j_aiajcjdi: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjdi: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcjdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjdi: do i = i0, n1il
if (i == j) cycle i_aiajcjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aiajcjdi(t2, nocc, nactive, a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdi
end do a_aiajcjdi
end do j_aiajcjdi
end do c_aiajcjdi
end do d_aiajcjdi
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidj: do d = n0d, n1d
c_aiajcidj: do c = n0c, n1c
if (c == d) cycle c_aiajcidj
j_aiajcidj: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcidj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcidj: do i = i0, n1ik
if (i == j) cycle i_aiajcidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajcidj(t2, nocc, nactive, a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidj
end do a_aiajcidj
end do j_aiajcidj
end do c_aiajcidj
end do d_aiajcidj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiajcidi: do d = n0d, n1d
c_aiajcidi: do c = n0c, n1c
if (c == d) cycle c_aiajcidi
j_aiajcidi: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajcidi: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajcidi: do i = i0, n1ikl
if (i == j) cycle i_aiajcidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajcidi(t2, nocc, nactive, a, i, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidi
end do a_aiajcidi
end do j_aiajcidi
end do c_aiajcidi
end do d_aiajcidi
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: d == a, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjaj: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjcjaj: do b = b0, n1b
if (b == c) cycle b_aibjcjaj
j_aibjcjaj: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjaj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjaj: do i = n0i, n1i
if (i == j) cycle i_aibjcjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjaj(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaj
end do a_aibjcjaj
end do j_aibjcjaj
end do b_aibjcjaj
end do c_aibjcjaj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjcjai: do b = b0, n1b
if (b == c) cycle b_aibjcjai
j_aibjcjai: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjai: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjai
i_aibjcjai: do i = n0il, n1il
if (i == j) cycle i_aibjcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjai(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai
end do a_aibjcjai
end do j_aibjcjai
end do b_aibjcjai
end do c_aibjcjai
!
! Elementary loop  10
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
a0 = max(b + 1, n0ad)
a_aibjciaj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciaj: do i = n0ik, n1ik
if (i == j) cycle i_aibjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjciaj(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj
end do a_aibjciaj
end do j_aibjciaj
end do b_aibjciaj
end do c_aibjciaj
!
! Elementary loop  11
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
a0 = max(b + 1, n0ad)
a_aibjciai: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciai
i_aibjciai: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjciai(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciai
end do a_aibjciai
end do j_aibjciai
end do b_aibjciai
end do c_aibjciai
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i
! Equalities: d == b, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbj: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcjbj: do b = b0, n1bd
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjbj(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbj
end do a_aibjcjbj
end do j_aibjcjbj
end do b_aibjcjbj
end do c_aibjcjbj
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcjbi: do b = b0, n1bd
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjbi(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbi
end do a_aibjcjbi
end do j_aibjcjbi
end do b_aibjcjbi
end do c_aibjcjbi
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibj: do b = b0, n1bd
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcibj(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj
end do a_aibjcibj
end do j_aibjcibj
end do b_aibjcibj
end do c_aibjcibj
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcibi: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibi: do b = b0, n1bd
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcibi(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibi
end do a_aibjcibi
end do j_aibjcibi
end do b_aibjcibi
end do c_aibjcibi
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibibkbi: do k = n0k, n1k
b_aibibkbi: do b = n0bcd, n1bcd
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbi: do a = a0, n1a
if (a == b) cycle a_aibibkbi
i0 = max(k + 1, n0ijl)
i_aibibkbi: do i = i0, n1ijl
if (i == k) cycle i_aibibkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibibkbi(t2, nocc, nactive, a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbi
end do a_aibibkbi
end do b_aibibkbi
end do k_aibibkbi
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkbk: do k = n0kl, n1kl
b_aibibkbk: do b = n0bcd, n1bcd
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbk: do a = a0, n1a
if (a == b) cycle a_aibibkbk
i0 = max(k + 1, n0ij)
i_aibibkbk: do i = i0, n1ij
if (i == k) cycle i_aibibkbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibibkbk(t2, nocc, nactive, a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbk
end do a_aibibkbk
end do b_aibibkbk
end do k_aibibkbk
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbibj: do b = n0bcd, n1bcd
j_aibjbibj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibj: do a = a0, n1a
if (a == b) cycle a_aibjbibj
i1 = min(j - 1, n1ik)
i_aibjbibj: do i = n0ik, i1
if (i == j) cycle i_aibjbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbibj(t2, nocc, nactive, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibj
end do a_aibjbibj
end do j_aibjbibj
end do b_aibjbibj
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjbibi: do b = n0bcd, n1bcd
j_aibjbibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibi: do a = a0, n1a
if (a == b) cycle a_aibjbibi
i1 = min(j - 1, n1ikl)
i_aibjbibi: do i = n0ikl, i1
if (i == j) cycle i_aibjbibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbibi(t2, nocc, nactive, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibi
end do a_aibjbibi
end do j_aibjbibi
end do b_aibjbibi
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibibkai: do k = n0k, n1k
b_aibibkai: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkai: do a = a0, n1ad
if (a == b) cycle a_aibibkai
i0 = max(k + 1, n0ijl)
i_aibibkai: do i = i0, n1ijl
if (i == k) cycle i_aibibkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibibkai(t2, nocc, nactive, a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkai
end do a_aibibkai
end do b_aibibkai
end do k_aibibkai
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkak: do k = n0kl, n1kl
b_aibibkak: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkak: do a = a0, n1ad
if (a == b) cycle a_aibibkak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibibkak: do i = i0, n1ij
if (i == k) cycle i_aibibkak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibibkak(t2, nocc, nactive, a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkak
end do a_aibibkak
end do b_aibibkak
end do k_aibibkak
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiaj: do b = n0bc, n1bc
j_aibjbiaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiaj: do a = a0, n1ad
if (a == b) cycle a_aibjbiaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjbiaj: do i = n0ik, i1
if (i == j) cycle i_aibjbiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbiaj(t2, nocc, nactive, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiaj
end do a_aibjbiaj
end do j_aibjbiaj
end do b_aibjbiaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjbiai: do b = n0bc, n1bc
j_aibjbiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiai: do a = a0, n1ad
if (a == b) cycle a_aibjbiai
i1 = min(j - 1, n1ikl)
i_aibjbiai: do i = n0ikl, i1
if (i == j) cycle i_aibjbiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbiai(t2, nocc, nactive, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiai
end do a_aibjbiai
end do j_aibjbiai
end do b_aibjbiai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == c, k == j, l == j
! No equalities independent of the above can hold.
!
c_aiajcjcj: do c = n0cd, n1cd
j_aiajcjcj: do j = n0jkl, n1jkl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjcj: do a = a0, n1ab
if (a == c) cycle a_aiajcjcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjcj: do i = i0, n1i
if (i == j) cycle i_aiajcjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aiajcjcj(t2, nocc, nactive, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjcj
end do a_aiajcjcj
end do j_aiajcjcj
end do c_aiajcjcj
!
! Elementary loop  25
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
i0 = max(j + 1, n0il)
i_aiajcjci: do i = i0, n1il
if (i == j) cycle i_aiajcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aiajcjci(t2, nocc, nactive, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjci
end do a_aiajcjci
end do j_aiajcjci
end do c_aiajcjci
!
! Elementary loop  26
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajcicj(t2, nocc, nactive, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicj
end do a_aiajcicj
end do j_aiajcicj
end do c_aiajcicj
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiajcici: do c = n0cd, n1cd
j_aiajcici: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajcici: do a = a0, n1ab
if (a == c) cycle a_aiajcici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajcici: do i = i0, n1ikl
if (i == j) cycle i_aiajcici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajcici(t2, nocc, nactive, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcici
end do a_aiajcici
end do j_aiajcici
end do c_aiajcici
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == a, k == j, l == j
! No equalities independent of the above can hold.
!
c_aiajcjaj: do c = n0c, n1c
j_aiajcjaj: do j = n0jkl, n1jkl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjaj: do a = a0, n1abd
if (a == c) cycle a_aiajcjaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjaj: do i = i0, n1i
if (i == j) cycle i_aiajcjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aiajcjaj(t2, nocc, nactive, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaj
end do a_aiajcjaj
end do j_aiajcjaj
end do c_aiajcjaj
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjai: do c = n0c, n1c
j_aiajcjai: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjai: do a = a0, n1abd
if (a == c) cycle a_aiajcjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjai: do i = i0, n1il
if (i == j) cycle i_aiajcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aiajcjai(t2, nocc, nactive, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjai
end do a_aiajcjai
end do j_aiajcjai
end do c_aiajcjai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciaj: do c = n0c, n1c
j_aiajciaj: do j = n0jl, n1jl
a0 = max(c + 1, n0abd)
a_aiajciaj: do a = a0, n1abd
if (a == c) cycle a_aiajciaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciaj: do i = i0, n1ik
if (i == j) cycle i_aiajciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajciaj(t2, nocc, nactive, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaj
end do a_aiajciaj
end do j_aiajciaj
end do c_aiajciaj
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
function mu3_mem(a, i, b, j, c, k)
integer :: mu3_mem
integer, intent(in) :: a, i, b, j, c, k
mu3_mem = offset + (i-n0i)+mi + (a-n0a)*ma + (j-n0j)*mj + (b-n0b)*mb + (k-n0k)*mk  &
+ (c-n0c)*mc
end function mu3_mem
end subroutine ccjac_31_dav_part3
end module ccjac_block_31_dav_part3
