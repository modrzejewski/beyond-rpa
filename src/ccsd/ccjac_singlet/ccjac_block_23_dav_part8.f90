module ccjac_block_23_dav_part8
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
subroutine ccjac_23_dav_part8(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a, b, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, d0, i0, i1, j0, j1, k0, l1
integer :: n0abc, n0acd, n0be, n0de, n0ijk
integer :: n0ijkm, n0ijlm, n0ijm, n0ik, n0ikm
integer :: n0il, n0ilm, n0im, n0jk, n0jkm
integer :: n0jl, n0jlm, n0jm, n0km, n0lm
integer :: n1abc, n1acd, n1be, n1de, n1ijk
integer :: n1ijkm, n1ijlm, n1ijm, n1ik, n1ikm
integer :: n1il, n1ilm, n1im, n1jk, n1jkm
integer :: n1jl, n1jlm, n1jm, n1km, n1lm
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


n0abc = max(n0a, n0b, n0c)
n0acd = max(n0a, n0c, n0d)
n0be = max(n0b, n0e)
n0de = max(n0d, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkm = max(n0i, n0j, n0k, n0m)
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
n1abc = min(n1a, n1b, n1c)
n1acd = min(n1a, n1c, n1d)
n1be = min(n1b, n1e)
n1de = min(n1d, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkm = min(n1i, n1j, n1k, n1m)
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
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajaibm: do m = n0m, n1m
b_aibjajaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajaibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjajaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajaibm: do a = a0, n1acd
if (a == b) cycle a_aibjajaibm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, m - 1, n1il)
i_aibjajaibm: do i = n0il, i1
if (i == j .or. i == m) cycle i_aibjajaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjajaibm(nocc, a, i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajaibm
end do a_aibjajaibm
end do j_aibjajaibm
end do b_aibjajaibm
end do m_aibjajaibm
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakaibj: do k = n0k, n1k
b_aibjakaibj: do b = n0be, n1be
j_aibjakaibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjakaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakaibj: do a = a0, n1acd
if (a == b) cycle a_aibjakaibj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, j - 1, n1il)
i_aibjakaibj: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakaibj(nocc, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakaibj
end do a_aibjakaibj
end do j_aibjakaibj
end do b_aibjakaibj
end do k_aibjakaibj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == a, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
k_aibjakajbj: do k = n0k, n1k
b_aibjakajbj: do b = n0be, n1be
j1 = min(k - 1, n1jlm)
j_aibjakajbj: do j = n0jlm, j1
if (j == k) cycle j_aibjakajbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakajbj: do a = a0, n1acd
if (a == b) cycle a_aibjakajbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, j, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakajbj(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakajbj
end do a_aibjakajbj
end do j_aibjakajbj
end do b_aibjakajbj
end do k_aibjakajbj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaiajbm: do m = n0m, n1m
b_aibjaiajbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j1 = min(m - 1, n1jl)
j_aibjaiajbm: do j = n0jl, j1
if (j == m) cycle j_aibjaiajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiajbm: do a = a0, n1acd
if (a == b) cycle a_aibjaiajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjaiajbm: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjaiajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaiajbm(nocc, a, i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiajbm
end do a_aibjaiajbm
end do j_aibjaiajbm
end do b_aibjaiajbm
end do m_aibjaiajbm
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjakajbi: do k = n0k, n1k
b_aibjakajbi: do b = n0be, n1be
j1 = min(k - 1, n1jl)
j_aibjakajbi: do j = n0jl, j1
if (j == k) cycle j_aibjakajbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakajbi: do a = a0, n1acd
if (a == b) cycle a_aibjakajbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aibjakajbi: do i = i0, n1im
if (i == j .or. i == k) cycle i_aibjakajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakajbi(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakajbi
end do a_aibjakajbi
end do j_aibjakajbi
end do b_aibjakajbi
end do k_aibjakajbi
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, d == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjakajbk: do k = n0km, n1km
b_aibjakajbk: do b = n0be, n1be
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aibjakajbk: do j = n0jl, j1
if (j == k) cycle j_aibjakajbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakajbk: do a = a0, n1acd
if (a == b) cycle a_aibjakajbk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, j, b, k)
jac_ibra_iket = eom_cc3_23_trans_aibjakajbk(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakajbk
end do a_aibjakajbk
end do j_aibjakajbk
end do b_aibjakajbk
end do k_aibjakajbk
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, l, i
! Equalities: c == a, d == a, e == b, k == j, m == l
! No equalities independent of the above can hold.
!
l_aibjajalbl: do l = n0lm, n1lm
b_aibjajalbl: do b = n0be, n1be
em = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(l + 1, n0jk)
j_aibjajalbl: do j = j0, n1jk
if (j == l) cycle j_aibjajalbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajalbl: do a = a0, n1acd
if (a == b) cycle a_aibjajalbl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajalbl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajalbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, l, b, l)
jac_ibra_iket = eom_cc3_23_trans_aibjajalbl(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajalbl
end do a_aibjajalbl
end do j_aibjajalbl
end do b_aibjajalbl
end do l_aibjajalbl
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l, m
! Equalities: c == a, d == a, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibiaialbm: do m = n0m, n1m
l1 = min(m - 1, n1l)
l_aibiaialbm: do l = n0l, l1
if (l == m) cycle l_aibiaialbm
b_aibiaialbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibiaialbm: do a = a0, n1acd
if (a == b) cycle a_aibiaialbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijk)
i_aibiaialbm: do i = i0, n1ijk
if (i == l .or. i == m) cycle i_aibiaialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibiaialbm(a, i, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaialbm
end do a_aibiaialbm
end do b_aibiaialbm
end do l_aibiaialbm
end do m_aibiaialbm
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjaialbi: do l = n0l, n1l
b_aibjaialbi: do b = n0be, n1be
j_aibjaialbi: do j = n0j, n1j
if (j == l) cycle j_aibjaialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaialbi: do a = a0, n1acd
if (a == b) cycle a_aibjaialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ikm)
i_aibjaialbi: do i = i0, n1ikm
if (i == j .or. i == l) cycle i_aibjaialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaialbi(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaialbi
end do a_aibjaialbi
end do j_aibjaialbi
end do b_aibjaialbi
end do l_aibjaialbi
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjaialbj: do l = n0l, n1l
b_aibjaialbj: do b = n0be, n1be
j0 = max(l + 1, n0jm)
j_aibjaialbj: do j = j0, n1jm
if (j == l) cycle j_aibjaialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaialbj: do a = a0, n1acd
if (a == b) cycle a_aibjaialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ik)
i_aibjaialbj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaialbj(nocc, a, i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaialbj
end do a_aibjaialbj
end do j_aibjaialbj
end do b_aibjaialbj
end do l_aibjaialbj
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == a, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibiakalbi: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibiakalbi: do k = k0, n1k
if (k == l) cycle k_aibiakalbi
b_aibiakalbi: do b = n0be, n1be
a0 = max(b + 1, n0acd)
a_aibiakalbi: do a = a0, n1acd
if (a == b) cycle a_aibiakalbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijm)
i_aibiakalbi: do i = i0, n1ijm
if (i == k .or. i == l) cycle i_aibiakalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakalbi(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakalbi
end do a_aibiakalbi
end do b_aibiakalbi
end do k_aibiakalbi
end do l_aibiakalbi
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjajalbi: do l = n0l, n1l
b_aibjajalbi: do b = n0be, n1be
j0 = max(l + 1, n0jk)
j_aibjajalbi: do j = j0, n1jk
if (j == l) cycle j_aibjajalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajalbi: do a = a0, n1acd
if (a == b) cycle a_aibjajalbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0im)
i_aibjajalbi: do i = i0, n1im
if (i == j .or. i == l) cycle i_aibjajalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajalbi(a, i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajalbi
end do a_aibjajalbi
end do j_aibjajalbi
end do b_aibjajalbi
end do l_aibjajalbi
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjajalbj: do l = n0l, n1l
b_aibjajalbj: do b = n0be, n1be
j0 = max(l + 1, n0jkm)
j_aibjajalbj: do j = j0, n1jkm
if (j == l) cycle j_aibjajalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajalbj: do a = a0, n1acd
if (a == b) cycle a_aibjajalbj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajalbj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajalbj(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajalbj
end do a_aibjajalbj
end do j_aibjajalbj
end do b_aibjajalbj
end do l_aibjajalbj
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakaiei: do e = n0e, n1e
k_aibiakaiei: do k = n0k, n1k
b_aibiakaiei: do b = n0b, n1b
if (b == e) cycle b_aibiakaiei
a0 = max(b + 1, e + 1, n0acd)
a_aibiakaiei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibiakaiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijlm)
i_aibiakaiei: do i = n0ijlm, i1
if (i == k) cycle i_aibiakaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, a, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakaiei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakaiei
end do a_aibiakaiei
end do b_aibiakaiei
end do k_aibiakaiei
end do e_aibiakaiei
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajaiei: do e = n0e, n1e
b_aibjajaiei: do b = n0b, n1b
if (b == e) cycle b_aibjajaiei
j_aibjajaiei: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjajaiei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjajaiei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ilm)
i_aibjajaiei: do i = n0ilm, i1
if (i == j) cycle i_aibjajaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajaiei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajaiei
end do a_aibjajaiei
end do j_aibjajaiei
end do b_aibjajaiei
end do e_aibjajaiei
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajaiej: do e = n0e, n1e
b_aibjajaiej: do b = n0b, n1b
if (b == e) cycle b_aibjajaiej
j_aibjajaiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjajaiej: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjajaiej
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aibjajaiej: do i = n0il, i1
if (i == j) cycle i_aibjajaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, a, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajaiej(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajaiej
end do a_aibjajaiej
end do j_aibjajaiej
end do b_aibjajaiej
end do e_aibjajaiej
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjaiajej: do e = n0e, n1e
b_aibjaiajej: do b = n0b, n1b
if (b == e) cycle b_aibjaiajej
j_aibjaiajej: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjaiajej: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjaiajej
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjaiajej: do i = i0, n1ik
if (i == j) cycle i_aibjaiajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaiajej(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiajej
end do a_aibjaiajej
end do j_aibjaiajej
end do b_aibjaiajej
end do e_aibjaiajej
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaiajei: do e = n0e, n1e
b_aibjaiajei: do b = n0b, n1b
if (b == e) cycle b_aibjaiajei
j_aibjaiajei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjaiajei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjaiajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aibjaiajei: do i = i0, n1ikm
if (i == j) cycle i_aibjaiajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaiajei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiajei
end do a_aibjaiajei
end do j_aibjaiajei
end do b_aibjaiajei
end do e_aibjaiajei
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l
! Equalities: c == a, d == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiaialei: do e = n0e, n1e
l_aibiaialei: do l = n0l, n1l
b_aibiaialei: do b = n0b, n1b
if (b == e) cycle b_aibiaialei
a0 = max(b + 1, e + 1, n0acd)
a_aibiaialei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibiaialei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijkm)
i_aibiaialei: do i = i0, n1ijkm
if (i == l) cycle i_aibiaialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibiaialei(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaialei
end do a_aibiaialei
end do b_aibiaialei
end do l_aibiaialei
end do e_aibiaialei
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, e == d, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajajdidm: do m = n0m, n1m
d_aiajajdidm: do d = n0de, n1de
em = (d - nvirt0) * nocc + (m - nocc0) + 1
j_aiajajdidm: do j = n0jk, n1jk
if (j == m) cycle j_aiajajdidm
a0 = max(d + 1, n0abc)
a_aiajajdidm: do a = a0, n1abc
if (a == d) cycle a_aiajajdidm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajajdidm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajajdidm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, d, m)
jac_ibra_iket = eom_cc3_23_trans_aiajajdidm(a, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajdidm
end do a_aiajajdidm
end do j_aiajajdidm
end do d_aiajajdidm
end do m_aiajajdidm
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, e == d, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajakdidj: do d = n0de, n1de
k_aiajakdidj: do k = n0k, n1k
j_aiajakdidj: do j = n0jm, n1jm
if (j == k) cycle j_aiajakdidj
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajakdidj: do a = a0, n1abc
if (a == d) cycle a_aiajakdidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aiajakdidj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajakdidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, d, j)
jac_ibra_iket = eom_cc3_23_trans_aiajakdidj(a, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakdidj
end do a_aiajakdidj
end do j_aiajakdidj
end do k_aiajakdidj
end do d_aiajakdidj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, e == d, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajaidjdm: do m = n0m, n1m
d_aiajaidjdm: do d = n0de, n1de
em = (d - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aiajaidjdm: do j = j0, n1jl
if (j == m) cycle j_aiajaidjdm
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajaidjdm: do a = a0, n1abc
if (a == d) cycle a_aiajaidjdm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i1 = min(j - 1, n1ik)
i_aiajaidjdm: do i = i0, i1
if (i == j .or. i == m) cycle i_aiajaidjdm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, d, m)
jac_ibra_iket = eom_cc3_23_trans_aiajaidjdm(a, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidjdm
end do a_aiajaidjdm
end do j_aiajaidjdm
end do d_aiajaidjdm
end do m_aiajaidjdm
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, e == d, k == i, m == j
! No equalities independent of the above can hold.
!
d_aiajaidldj: do d = n0de, n1de
l_aiajaidldj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aiajaidldj: do j = n0jm, j1
if (j == l) cycle j_aiajaidldj
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajaidldj: do a = a0, n1abc
if (a == d) cycle a_aiajaidldj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i1 = min(l - 1, n1ik)
i_aiajaidldj: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajaidldj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, d, j)
jac_ibra_iket = eom_cc3_23_trans_aiajaidldj(a, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidldj
end do a_aiajaidldj
end do j_aiajaidldj
end do l_aiajaidldj
end do d_aiajaidldj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, e == d, m == i, k == j
! No equalities independent of the above can hold.
!
d_aiajajdldi: do d = n0de, n1de
l_aiajajdldi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jk)
j_aiajajdldi: do j = n0jk, j1
if (j == l) cycle j_aiajajdldi
a0 = max(d + 1, n0abc)
a_aiajajdldi: do a = a0, n1abc
if (a == d) cycle a_aiajajdldi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajajdldi: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajajdldi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, l, d, i)
jac_ibra_iket = eom_cc3_23_trans_aiajajdldi(a, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajdldi
end do a_aiajajdldi
end do j_aiajajdldi
end do l_aiajajdldi
end do d_aiajajdldi
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiakdiei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiaiakdiei: do d = d0, n1d
if (d == e) cycle d_aiaiakdiei
k_aiaiakdiei: do k = n0k, n1k
a0 = max(d + 1, n0abc)
a_aiaiakdiei: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiaiakdiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiakdiei: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aiaiakdiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiaiakdiei(a, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakdiei
end do a_aiaiakdiei
end do k_aiaiakdiei
end do d_aiaiakdiei
end do e_aiaiakdiei
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajajdiei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajajdiei: do d = d0, n1d
if (d == e) cycle d_aiajajdiei
j_aiajajdiei: do j = n0jk, n1jk
a0 = max(d + 1, n0abc)
a_aiajajdiei: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajajdiei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ilm)
i_aiajajdiei: do i = i0, n1ilm
if (i == j) cycle i_aiajajdiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajajdiei(a, i, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajdiei
end do a_aiajajdiei
end do j_aiajajdiei
end do d_aiajajdiei
end do e_aiajajdiei
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajajdiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajajdiej: do d = d0, n1d
if (d == e) cycle d_aiajajdiej
j_aiajajdiej: do j = n0jkm, n1jkm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajajdiej: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajajdiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajajdiej: do i = i0, n1il
if (i == j) cycle i_aiajajdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajajdiej(a, j, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajdiej
end do a_aiajajdiej
end do j_aiajajdiej
end do d_aiajajdiej
end do e_aiajajdiej
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajaidjej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajaidjej: do d = d0, n1d
if (d == e) cycle d_aiajaidjej
j_aiajaidjej: do j = n0jlm, n1jlm
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajaidjej: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajaidjej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidjej: do i = i0, n1ik
if (i == j) cycle i_aiajaidjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajaidjej(a, j, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidjej
end do a_aiajaidjej
end do j_aiajaidjej
end do d_aiajaidjej
end do e_aiajaidjej
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajaidjei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajaidjei: do d = d0, n1d
if (d == e) cycle d_aiajaidjei
j_aiajaidjei: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajaidjei: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiajaidjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajaidjei: do i = i0, n1ikm
if (i == j) cycle i_aiajaidjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajaidjei(a, i, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidjei
end do a_aiajaidjei
end do j_aiajaidjei
end do d_aiajaidjei
end do e_aiajaidjei
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, l
! Equalities: b == a, c == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aiaiaidlei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiaiaidlei: do d = d0, n1d
if (d == e) cycle d_aiaiaidlei
l_aiaiaidlei: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiaiaidlei: do a = a0, n1abc
if (a == d .or. a == e) cycle a_aiaiaidlei
i_aiaiaidlei: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aiaiaidlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiaiaidlei(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiaidlei
end do a_aiaiaidlei
end do l_aiaiaidlei
end do d_aiaiaidlei
end do e_aiaiaidlei
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

end subroutine ccjac_23_dav_part8
end module ccjac_block_23_dav_part8
