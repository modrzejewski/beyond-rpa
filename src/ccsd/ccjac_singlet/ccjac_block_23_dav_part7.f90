module ccjac_block_23_dav_part7
use eom_cc3_23_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-21 14:26:59 UTC.
!
contains
subroutine ccjac_23_dav_part7(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, n1j, n0k, &
 n1k, n0l, n1l, n0m, n1m, bra0, ket0, offset) 
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d, n0e, n1e
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l, n0m, n1m
integer, intent(in)                                 :: bra0, ket0
integer, intent(in)                                 :: offset
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, b0, b1, c0, i0, i1, j0, j1, k0, l1
integer :: n0abcd, n0acd, n0bd, n0bde, n0be
integer :: n0ijk, n0ijkm, n0ijl, n0ijlm, n0ijm
integer :: n0ik, n0ikm, n0il, n0ilm, n0im
integer :: n0jk, n0jkm, n0jl, n0jlm, n0jm
integer :: n0km, n0lm
integer :: n1abcd, n1acd, n1bd, n1bde, n1be
integer :: n1ijk, n1ijkm, n1ijl, n1ijlm, n1ijm
integer :: n1ik, n1ikm, n1il, n1ilm, n1im
integer :: n1jk, n1jkm, n1jl, n1jlm, n1jm
integer :: n1km, n1lm
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket, iket2, ibra2
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
integer :: nk, nc, nl, nd, nm, ne
integer :: mk, mc, ml, md, mm, me


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

nk = n1k - n0k + 1
nc = n1c - n0c + 1
nl = n1l - n0l + 1
nd = n1d - n0d + 1
nm = n1m - n0m + 1
ne = n1e - n0e + 1
mk = 1
mc = nk
ml = nk * nc
md = nk * nc * nl
mm = nk * nc * nl * nd
me = nk * nc * nl * nd * nm


n0abcd = max(n0a, n0b, n0c, n0d)
n0acd = max(n0a, n0c, n0d)
n0bd = max(n0b, n0d)
n0bde = max(n0b, n0d, n0e)
n0be = max(n0b, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0ilm = max(n0i, n0l, n0m)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0km = max(n0k, n0m)
n0lm = max(n0l, n0m)
n1abcd = min(n1a, n1b, n1c, n1d)
n1acd = min(n1a, n1c, n1d)
n1bd = min(n1b, n1d)
n1bde = min(n1b, n1d, n1e)
n1be = min(n1b, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1ilm = min(n1i, n1l, n1m)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jlm = min(n1j, n1l, n1m)
n1jm = min(n1j, n1m)
n1km = min(n1k, n1m)
n1lm = min(n1l, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjblbi: do l = n0l, n1l
c_aibjcjblbi: do c = n0c, n1c
b1 = min(c - 1, n1bde)
b_aibjcjblbi: do b = n0bde, b1
if (b == c) cycle b_aibjcjblbi
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jk)
j_aibjcjblbi: do j = n0jk, j1
if (j == l) cycle j_aibjcjblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjblbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjblbi
i1 = min(l - 1, n1im)
i_aibjcjblbi: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjcjblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, b, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjblbi(a, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjblbi
end do a_aibjcjblbi
end do j_aibjcjblbi
end do b_aibjcjblbi
end do c_aibjcjblbi
end do l_aibjcjblbi
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibickbiei: do e = n0e, n1e
c_aibickbiei: do c = n0c, n1c
if (c == e) cycle c_aibickbiei
k_aibickbiei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bd)
b1 = min(c - 1, n1bd)
b_aibickbiei: do b = b0, b1
if (b == c .or. b == e) cycle b_aibickbiei
a0 = max(b + 1, n0a)
a_aibickbiei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibickbiei
i_aibickbiei: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aibickbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, b, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibickbiei(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickbiei
end do a_aibickbiei
end do b_aibickbiei
end do k_aibickbiei
end do c_aibickbiei
end do e_aibickbiei
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjbiei: do e = n0e, n1e
c_aibjcjbiei: do c = n0c, n1c
if (c == e) cycle c_aibjcjbiei
b0 = max(e + 1, n0bd)
b1 = min(c - 1, n1bd)
b_aibjcjbiei: do b = b0, b1
if (b == c .or. b == e) cycle b_aibjcjbiei
j_aibjcjbiei: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbiei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjbiei
i_aibjcjbiei: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjcjbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, b, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjbiei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbiei
end do a_aibjcjbiei
end do j_aibjcjbiei
end do b_aibjcjbiei
end do c_aibjcjbiei
end do e_aibjcjbiei
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcibjej: do e = n0e, n1e
c_aibjcibjej: do c = n0c, n1c
if (c == e) cycle c_aibjcibjej
b0 = max(e + 1, n0bd)
b1 = min(c - 1, n1bd)
b_aibjcibjej: do b = b0, b1
if (b == c .or. b == e) cycle b_aibjcibjej
j_aibjcibjej: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibjej
i_aibjcibjej: do i = n0ik, n1ik
if (i == j) cycle i_aibjcibjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcibjej(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibjej
end do a_aibjcibjej
end do j_aibjcibjej
end do b_aibjcibjej
end do c_aibjcibjej
end do e_aibjcibjej
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcibjei: do e = n0e, n1e
c_aibjcibjei: do c = n0c, n1c
if (c == e) cycle c_aibjcibjei
b0 = max(e + 1, n0bd)
b1 = min(c - 1, n1bd)
b_aibjcibjei: do b = b0, b1
if (b == c .or. b == e) cycle b_aibjcibjei
j_aibjcibjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibjei
i_aibjcibjei: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjcibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcibjei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibjei
end do a_aibjcibjei
end do j_aibjcibjei
end do b_aibjcibjei
end do c_aibjcibjei
end do e_aibjcibjei
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, l
! Equalities: d == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiciblei: do e = n0e, n1e
l_aibiciblei: do l = n0l, n1l
c_aibiciblei: do c = n0c, n1c
if (c == e) cycle c_aibiciblei
b0 = max(e + 1, n0bd)
b1 = min(c - 1, n1bd)
b_aibiciblei: do b = b0, b1
if (b == c .or. b == e) cycle b_aibiciblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibiciblei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibiciblei
i_aibiciblei: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aibiciblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, b, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibiciblei(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciblei
end do a_aibiciblei
end do b_aibiciblei
end do c_aibiciblei
end do l_aibiciblei
end do e_aibiciblei
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k
! Equalities: e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibickdibi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibickdibi: do c = c0, n1c
if (c == d) cycle c_aibickdibi
k_aibickdibi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibickdibi: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibickdibi
a0 = max(b + 1, n0a)
a_aibickdibi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibickdibi
i_aibickdibi: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aibickdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, d, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibickdibi(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickdibi
end do a_aibickdibi
end do b_aibickdibi
end do k_aibickdibi
end do c_aibickdibi
end do d_aibickdibi
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdibi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdibi: do c = c0, n1c
if (c == d) cycle c_aibjcjdibi
b1 = min(d - 1, n1be)
b_aibjcjdibi: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcjdibi
j_aibjcjdibi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdibi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdibi
i_aibjcjdibi: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjcjdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjdibi(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdibi
end do a_aibjcjdibi
end do j_aibjcjdibi
end do b_aibjcjdibi
end do c_aibjcjdibi
end do d_aibjcjdibi
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdibj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdibj: do c = c0, n1c
if (c == d) cycle c_aibjcjdibj
b1 = min(d - 1, n1be)
b_aibjcjdibj: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcjdibj
j_aibjcjdibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdibj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdibj
i_aibjcjdibj: do i = n0il, n1il
if (i == j) cycle i_aibjcjdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, d, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcjdibj(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdibj
end do a_aibjcjdibj
end do j_aibjcjdibj
end do b_aibjcjdibj
end do c_aibjcjdibj
end do d_aibjcjdibj
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcidjbj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidjbj: do c = c0, n1c
if (c == d) cycle c_aibjcidjbj
b1 = min(d - 1, n1be)
b_aibjcidjbj: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidjbj
j_aibjcidjbj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidjbj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjbj
i_aibjcidjbj: do i = n0ik, n1ik
if (i == j) cycle i_aibjcidjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, j, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcidjbj(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidjbj
end do a_aibjcidjbj
end do j_aibjcidjbj
end do b_aibjcidjbj
end do c_aibjcidjbj
end do d_aibjcidjbj
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjbi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidjbi: do c = c0, n1c
if (c == d) cycle c_aibjcidjbi
b1 = min(d - 1, n1be)
b_aibjcidjbi: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidjbi
j_aibjcidjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidjbi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjbi
i_aibjcidjbi: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjcidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcidjbi(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidjbi
end do a_aibjcidjbi
end do j_aibjcidjbi
end do b_aibjcidjbi
end do c_aibjcidjbi
end do d_aibjcidjbi
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, l
! Equalities: e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibicidlbi: do d = n0d, n1d
l_aibicidlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aibicidlbi: do c = c0, n1c
if (c == d) cycle c_aibicidlbi
b1 = min(d - 1, n1be)
b_aibicidlbi: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibicidlbi
a0 = max(b + 1, n0a)
a_aibicidlbi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibicidlbi
i_aibicidlbi: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aibicidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, d, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibicidlbi(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicidlbi
end do a_aibicidlbi
end do b_aibicidlbi
end do c_aibicidlbi
end do l_aibicidlbi
end do d_aibicidlbi
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k, m
! Equalities: b == a, c == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
e_aiaiakaiem: do e = n0e, n1e
m_aiaiakaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aiaiakaiem: do k = n0k, n1k
if (k == m) cycle k_aiaiakaiem
a0 = max(e + 1, n0abcd)
a_aiaiakaiem: do a = a0, n1abcd
if (a == e) cycle a_aiaiakaiem
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, m - 1, n1ijl)
i_aiaiakaiem: do i = n0ijl, i1
if (i == k .or. i == m) cycle i_aiaiakaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiaiakaiem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakaiem
end do a_aiaiakaiem
end do k_aiaiakaiem
end do m_aiaiakaiem
end do e_aiaiakaiem
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiajakaiei: do e = n0e, n1e
k_aiajakaiei: do k = n0k, n1k
j_aiajakaiei: do j = n0j, n1j
if (j == k) cycle j_aiajakaiei
a0 = max(e + 1, n0abcd)
a_aiajakaiei: do a = a0, n1abcd
if (a == e) cycle a_aiajakaiei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0ilm)
i1 = min(k - 1, n1ilm)
i_aiajakaiei: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajakaiei(i, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakaiei
end do a_aiajakaiei
end do j_aiajakaiei
end do k_aiajakaiei
end do e_aiajakaiei
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k, j
! Equalities: b == a, c == a, d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiajakaiek: do e = n0e, n1e
k_aiajakaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j_aiajakaiek: do j = n0j, n1j
if (j == k) cycle j_aiajakaiek
a0 = max(e + 1, n0abcd)
a_aiajakaiek: do a = a0, n1abcd
if (a == e) cycle a_aiajakaiek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajakaiek: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, e, k)
jac_ibra_iket = eom_cc3_23_trans_aiajakaiek(j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakaiek
end do a_aiajakaiek
end do j_aiajakaiek
end do k_aiajakaiek
end do e_aiajakaiek
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajakaiej: do e = n0e, n1e
k_aiajakaiej: do k = n0k, n1k
j_aiajakaiej: do j = n0jm, n1jm
if (j == k) cycle j_aiajakaiej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajakaiej: do a = a0, n1abcd
if (a == e) cycle a_aiajakaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i1 = min(k - 1, j - 1, n1il)
i_aiajakaiej: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajakaiej(nocc, a, i, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakaiej
end do a_aiajakaiej
end do j_aiajakaiej
end do k_aiajakaiej
end do e_aiajakaiej
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, d == a, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajakajej: do e = n0e, n1e
k_aiajakajej: do k = n0k, n1k
j1 = min(k - 1, n1jlm)
j_aiajakajej: do j = n0jlm, j1
if (j == k) cycle j_aiajakajej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajakajej: do a = a0, n1abcd
if (a == e) cycle a_aiajakajej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakajej: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajakajej(i, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakajej
end do a_aiajakajej
end do j_aiajakajej
end do k_aiajakajej
end do e_aiajakajej
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aiajaiajem: do e = n0e, n1e
m_aiajaiajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
j1 = min(m - 1, n1jl)
j_aiajaiajem: do j = n0jl, j1
if (j == m) cycle j_aiajaiajem
a0 = max(e + 1, n0abcd)
a_aiajaiajem: do a = a0, n1abcd
if (a == e) cycle a_aiajaiajem
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaiajem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajaiajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiajaiajem(nocc, a, i, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaiajem
end do a_aiajaiajem
end do j_aiajaiajem
end do m_aiajaiajem
end do e_aiajaiajem
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajakajei: do e = n0e, n1e
k_aiajakajei: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aiajakajei: do j = n0jl, j1
if (j == k) cycle j_aiajakajei
a0 = max(e + 1, n0abcd)
a_aiajakajei: do a = a0, n1abcd
if (a == e) cycle a_aiajakajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajakajei: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajakajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajakajei(nocc, a, i, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakajei
end do a_aiajakajei
end do j_aiajakajei
end do k_aiajakajei
end do e_aiajakajei
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, k, i
! Equalities: b == a, c == a, d == a, l == j, m == k
! No equalities independent of the above can hold.
!
e_aiajakajek: do e = n0e, n1e
k_aiajakajek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajakajek: do j = n0jl, j1
if (j == k) cycle j_aiajakajek
a0 = max(e + 1, n0abcd)
a_aiajakajek: do a = a0, n1abcd
if (a == e) cycle a_aiajakajek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakajek: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, j, e, k)
jac_ibra_iket = eom_cc3_23_trans_aiajakajek(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakajek
end do a_aiajakajek
end do j_aiajakajek
end do k_aiajakajek
end do e_aiajakajek
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, l, j
! Equalities: b == a, c == a, d == a, k == i, m == l
! No equalities independent of the above can hold.
!
e_aiajaialel: do e = n0e, n1e
l_aiajaialel: do l = n0lm, n1lm
em = (e - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaialel: do j = n0j, n1j
if (j == l) cycle j_aiajaialel
a0 = max(e + 1, n0abcd)
a_aiajaialel: do a = a0, n1abcd
if (a == e) cycle a_aiajaialel
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, l + 1, n0ik)
i_aiajaialel: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaialel
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, e, l)
jac_ibra_iket = eom_cc3_23_trans_aiajaialel(j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaialel
end do a_aiajaialel
end do j_aiajaialel
end do l_aiajaialel
end do e_aiajaialel
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, l, i
! Equalities: b == a, c == a, d == a, k == j, m == l
! No equalities independent of the above can hold.
!
e_aiajajalel: do e = n0e, n1e
l_aiajajalel: do l = n0lm, n1lm
em = (e - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(l + 1, n0jk)
j_aiajajalel: do j = j0, n1jk
if (j == l) cycle j_aiajajalel
a0 = max(e + 1, n0abcd)
a_aiajajalel: do a = a0, n1abcd
if (a == e) cycle a_aiajajalel
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajalel: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajajalel
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, l, e, l)
jac_ibra_iket = eom_cc3_23_trans_aiajajalel(i, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajalel
end do a_aiajajalel
end do j_aiajajalel
end do l_aiajajalel
end do e_aiajajalel
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, l, m
! Equalities: b == a, c == a, d == a, j == i, k == i
! No equalities independent of the above can hold.
!
e_aiaiaialem: do e = n0e, n1e
m_aiaiaialem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l1 = min(m - 1, n1l)
l_aiaiaialem: do l = n0l, l1
if (l == m) cycle l_aiaiaialem
a0 = max(e + 1, n0abcd)
a_aiaiaialem: do a = a0, n1abcd
if (a == e) cycle a_aiaiaialem
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijk)
i_aiaiaialem: do i = i0, n1ijk
if (i == l .or. i == m) cycle i_aiaiaialem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, e, m)
jac_ibra_iket = eom_cc3_23_trans_aiaiaialem(i, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiaialem
end do a_aiaiaialem
end do l_aiaiaialem
end do m_aiaiaialem
end do e_aiaiaialem
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i, m == i
! No equalities independent of the above can hold.
!
e_aiajaialei: do e = n0e, n1e
l_aiajaialei: do l = n0l, n1l
j_aiajaialei: do j = n0j, n1j
if (j == l) cycle j_aiajaialei
a0 = max(e + 1, n0abcd)
a_aiajaialei: do a = a0, n1abcd
if (a == e) cycle a_aiajaialei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, l + 1, n0ikm)
i_aiajaialei: do i = i0, n1ikm
if (i == j .or. i == l) cycle i_aiajaialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajaialei(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaialei
end do a_aiajaialei
end do j_aiajaialei
end do l_aiajaialei
end do e_aiajaialei
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aiajaialej: do e = n0e, n1e
l_aiajaialej: do l = n0l, n1l
j0 = max(l + 1, n0jm)
j_aiajaialej: do j = j0, n1jm
if (j == l) cycle j_aiajaialej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajaialej: do a = a0, n1abcd
if (a == e) cycle a_aiajaialej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, l + 1, n0ik)
i_aiajaialej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajaialej(nocc, a, i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaialej
end do a_aiajaialej
end do j_aiajaialej
end do l_aiajaialej
end do e_aiajaialej
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, k, l
! Equalities: b == a, c == a, d == a, j == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiakalei: do e = n0e, n1e
l_aiaiakalei: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aiaiakalei: do k = k0, n1k
if (k == l) cycle k_aiaiakalei
a0 = max(e + 1, n0abcd)
a_aiaiakalei: do a = a0, n1abcd
if (a == e) cycle a_aiaiakalei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijm)
i_aiaiakalei: do i = i0, n1ijm
if (i == k .or. i == l) cycle i_aiaiakalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiaiakalei(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakalei
end do a_aiaiakalei
end do k_aiaiakalei
end do l_aiaiakalei
end do e_aiaiakalei
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajalei: do e = n0e, n1e
l_aiajajalei: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aiajajalei: do j = j0, n1jk
if (j == l) cycle j_aiajajalei
a0 = max(e + 1, n0abcd)
a_aiajajalei: do a = a0, n1abcd
if (a == e) cycle a_aiajajalei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, l + 1, n0im)
i_aiajajalei: do i = i0, n1im
if (i == j .or. i == l) cycle i_aiajajalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajajalei(nocc, a, i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajalei
end do a_aiajajalei
end do j_aiajajalei
end do l_aiajajalei
end do e_aiajajalei
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, e
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, d == a, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajajalej: do e = n0e, n1e
l_aiajajalej: do l = n0l, n1l
j0 = max(l + 1, n0jkm)
j_aiajajalej: do j = j0, n1jkm
if (j == l) cycle j_aiajajalej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajajalej: do a = a0, n1abcd
if (a == e) cycle a_aiajajalej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajalej: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajajalej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajajalej(i, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajalej
end do a_aiajajalej
end do j_aiajajalej
end do l_aiajajalej
end do e_aiajajalej
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, d == a, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakaibm: do m = n0m, n1m
k_aibiakaibm: do k = n0k, n1k
if (k == m) cycle k_aibiakaibm
b_aibiakaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibiakaibm: do a = a0, n1acd
if (a == b) cycle a_aibiakaibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, m - 1, n1ijl)
i_aibiakaibm: do i = n0ijl, i1
if (i == k .or. i == m) cycle i_aibiakaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibiakaibm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakaibm
end do a_aibiakaibm
end do b_aibiakaibm
end do k_aibiakaibm
end do m_aibiakaibm
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibjakaibi: do k = n0k, n1k
b_aibjakaibi: do b = n0be, n1be
j_aibjakaibi: do j = n0j, n1j
if (j == k) cycle j_aibjakaibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakaibi: do a = a0, n1acd
if (a == b) cycle a_aibjakaibi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ilm)
i_aibjakaibi: do i = n0ilm, i1
if (i == j .or. i == k) cycle i_aibjakaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakaibi(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakaibi
end do a_aibjakaibi
end do j_aibjakaibi
end do b_aibjakaibi
end do k_aibjakaibi
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

function mu3_mem(c, k, d, l, e, m)
integer :: mu3_mem
integer, intent(in) :: c, k, d, l, e, m
mu3_mem = offset + (k-n0k)+mk + (c-n0c)*mc + (l-n0l)*ml + (d-n0d)*md + (m-n0m)*mm  &
+ (e-n0e)*me
end function mu3_mem

end subroutine ccjac_23_dav_part7
end module ccjac_block_23_dav_part7
