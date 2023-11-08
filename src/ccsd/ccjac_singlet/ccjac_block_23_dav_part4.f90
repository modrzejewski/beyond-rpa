module ccjac_block_23_dav_part4
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
subroutine ccjac_23_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b0, b1, d0, i0, i1, j0, j1
integer :: n0ac, n0ae, n0bc, n0bcd, n0be
integer :: n0cd, n0de, n0ijk, n0ijkm, n0ijlm
integer :: n0ijm, n0ik, n0ikm, n0il, n0im
integer :: n0jk, n0jkm, n0jl, n0jlm, n0jm
integer :: n0km, n0lm
integer :: n1ac, n1ae, n1bc, n1bcd, n1be
integer :: n1cd, n1de, n1ijk, n1ijkm, n1ijlm
integer :: n1ijm, n1ik, n1ikm, n1il, n1im
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


n0ac = max(n0a, n0c)
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0de = max(n0d, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0km = max(n0k, n0m)
n0lm = max(n0l, n0m)
n1ac = min(n1a, n1c)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1de = min(n1d, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
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
! Free virtual indices: a, b, d
! Free occupied indices: j, k, i
! Equalities: c == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjakdjbk: do d = n0d, n1d
k_aibjakdjbk: do k = n0km, n1km
b1 = min(d - 1, n1be)
b_aibjakdjbk: do b = n0be, b1
if (b == d) cycle b_aibjakdjbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakdjbk: do j = n0jl, n1jl
if (j == k) cycle j_aibjakdjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdjbk: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdjbk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, j, b, k)
jac_ibra_iket = eom_cc3_23_trans_aibjakdjbk(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdjbk
end do a_aibjakdjbk
end do j_aibjakdjbk
end do b_aibjakdjbk
end do k_aibjakdjbk
end do d_aibjakdjbk
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l, j
! Equalities: c == a, e == b, k == i, m == l
! No equalities independent of the above can hold.
!
d_aibjaidlbl: do d = n0d, n1d
l_aibjaidlbl: do l = n0lm, n1lm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjaidlbl: do b = n0be, b1
if (b == d) cycle b_aibjaidlbl
em = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaidlbl: do j = n0j, n1j
if (j == l) cycle j_aibjaidlbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidlbl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidlbl
i_aibjaidlbl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaidlbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, b, l)
jac_ibra_iket = eom_cc3_23_trans_aibjaidlbl(j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidlbl
end do a_aibjaidlbl
end do j_aibjaidlbl
end do b_aibjaidlbl
end do l_aibjaidlbl
end do d_aibjaidlbl
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l, m
! Equalities: c == a, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibiaidlbm: do m = n0m, n1m
d_aibiaidlbm: do d = n0d, n1d
l_aibiaidlbm: do l = n0l, n1l
if (l == m) cycle l_aibiaidlbm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibiaidlbm: do b = n0be, b1
if (b == d) cycle b_aibiaidlbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidlbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidlbm
i_aibiaidlbm: do i = n0ijk, n1ijk
if (i == l .or. i == m) cycle i_aibiaidlbm
if (i > l .and. l > m) exit i_aibiaidlbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibiaidlbm(i, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidlbm
end do a_aibiaidlbm
end do b_aibiaidlbm
end do l_aibiaidlbm
end do d_aibiaidlbm
end do m_aibiaidlbm
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibjaidlbi: do d = n0d, n1d
l_aibjaidlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjaidlbi: do b = n0be, b1
if (b == d) cycle b_aibjaidlbi
j_aibjaidlbi: do j = n0j, n1j
if (j == l) cycle j_aibjaidlbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidlbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidlbi
i_aibjaidlbi: do i = n0ikm, n1ikm
if (i == j .or. i == l) cycle i_aibjaidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaidlbi(i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidlbi
end do a_aibjaidlbi
end do j_aibjaidlbi
end do b_aibjaidlbi
end do l_aibjaidlbi
end do d_aibjaidlbi
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjaidlbj: do d = n0d, n1d
l_aibjaidlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjaidlbj: do b = n0be, b1
if (b == d) cycle b_aibjaidlbj
j_aibjaidlbj: do j = n0jm, n1jm
if (j == l) cycle j_aibjaidlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidlbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidlbj
i_aibjaidlbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaidlbj
if (i > l .and. l > j) exit i_aibjaidlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaidlbj(nocc, a, i, b, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidlbj
end do a_aibjaidlbj
end do j_aibjaidlbj
end do b_aibjaidlbj
end do l_aibjaidlbj
end do d_aibjaidlbj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, l
! Equalities: c == a, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
d_aibiakdlbi: do d = n0d, n1d
l_aibiakdlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibiakdlbi: do k = n0k, n1k
if (k == l) cycle k_aibiakdlbi
b1 = min(d - 1, n1be)
b_aibiakdlbi: do b = n0be, b1
if (b == d) cycle b_aibiakdlbi
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdlbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdlbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdlbi: do i = n0ijm, n1ijm
if (i == k .or. i == l) cycle i_aibiakdlbi
if (k > l .and. l > i) cycle i_aibiakdlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakdlbi(i, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdlbi
end do a_aibiakdlbi
end do b_aibiakdlbi
end do k_aibiakdlbi
end do l_aibiakdlbi
end do d_aibiakdlbi
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l
! Equalities: c == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjajdlbj: do d = n0d, n1d
l_aibjajdlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjajdlbj: do b = n0be, b1
if (b == d) cycle b_aibjajdlbj
j_aibjajdlbj: do j = n0jkm, n1jkm
if (j == l) cycle j_aibjajdlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdlbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdlbj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdlbj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajdlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajdlbj(i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdlbj
end do a_aibjajdlbj
end do j_aibjajdlbj
end do b_aibjajdlbj
end do l_aibjajdlbj
end do d_aibjajdlbj
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakdiei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibiakdiei: do d = d0, n1d
if (d == e) cycle d_aibiakdiei
k_aibiakdiei: do k = n0k, n1k
b_aibiakdiei: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibiakdiei
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdiei: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibiakdiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdiei: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aibiakdiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakdiei(b, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdiei
end do a_aibiakdiei
end do b_aibiakdiei
end do k_aibiakdiei
end do d_aibiakdiei
end do e_aibiakdiei
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajdiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjajdiej: do d = d0, n1d
if (d == e) cycle d_aibjajdiej
b_aibjajdiej: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjajdiej
j_aibjajdiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdiej: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjajdiej
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdiej: do i = n0il, n1il
if (i == j) cycle i_aibjajdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajdiej(b, j, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdiej
end do a_aibjajdiej
end do j_aibjajdiej
end do b_aibjajdiej
end do d_aibjajdiej
end do e_aibjajdiej
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjaidjej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjaidjej: do d = d0, n1d
if (d == e) cycle d_aibjaidjej
b_aibjaidjej: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjaidjej
j_aibjaidjej: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidjej: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidjej
i_aibjaidjej: do i = n0ik, n1ik
if (i == j) cycle i_aibjaidjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaidjej(b, j, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidjej
end do a_aibjaidjej
end do j_aibjaidjej
end do b_aibjaidjej
end do d_aibjaidjej
end do e_aibjaidjej
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j
! Equalities: c == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaidjei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjaidjei: do d = d0, n1d
if (d == e) cycle d_aibjaidjei
b_aibjaidjei: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjaidjei
j_aibjaidjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidjei: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidjei
i_aibjaidjei: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjaidjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaidjei(i, b, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidjei
end do a_aibjaidjei
end do j_aibjaidjei
end do b_aibjaidjei
end do d_aibjaidjei
end do e_aibjaidjei
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, l
! Equalities: c == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiaidlei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibiaidlei: do d = d0, n1d
if (d == e) cycle d_aibiaidlei
l_aibiaidlei: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidlei: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibiaidlei
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidlei: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibiaidlei
i_aibiaidlei: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aibiaidlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibiaidlei(b, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidlei
end do a_aibiaidlei
end do b_aibiaidlei
end do l_aibiaidlei
end do d_aibiaidlei
end do e_aibiaidlei
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjbiem: do e = n0e, n1e
m_aibjbjbiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bcd)
b_aibjbjbiem: do b = b0, n1bcd
if (b == e) cycle b_aibjbjbiem
j_aibjbjbiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjbiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjbiem: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbjbiem
i1 = min(j - 1, m - 1, n1il)
i_aibjbjbiem: do i = n0il, i1
if (i == j .or. i == m) cycle i_aibjbjbiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, b, i, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjbjbiem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbiem
end do a_aibjbjbiem
end do j_aibjbjbiem
end do b_aibjbjbiem
end do m_aibjbjbiem
end do e_aibjbjbiem
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkbiej: do e = n0e, n1e
k_aibjbkbiej: do k = n0k, n1k
b0 = max(e + 1, n0bcd)
b_aibjbkbiej: do b = b0, n1bcd
if (b == e) cycle b_aibjbkbiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbiej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbkbiej
i1 = min(k - 1, j - 1, n1il)
i_aibjbkbiej: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, k, b, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjbkbiej(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbiej
end do a_aibjbkbiej
end do j_aibjbkbiej
end do b_aibjbkbiej
end do k_aibjbkbiej
end do e_aibjbkbiej
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbibjem: do e = n0e, n1e
m_aibjbibjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bcd)
b_aibjbibjem: do b = b0, n1bcd
if (b == e) cycle b_aibjbibjem
j1 = min(m - 1, n1jl)
j_aibjbibjem: do j = n0jl, j1
if (j == m) cycle j_aibjbibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibjem: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbibjem
i0 = max(j + 1, n0ik)
i_aibjbibjem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjbibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, b, j, e, m)
jac_ibra_iket = eom_cc3_23_trans_aibjbibjem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibjem
end do a_aibjbibjem
end do j_aibjbibjem
end do b_aibjbibjem
end do m_aibjbibjem
end do e_aibjbibjem
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbkbjei: do e = n0e, n1e
k_aibjbkbjei: do k = n0k, n1k
b0 = max(e + 1, n0bcd)
b_aibjbkbjei: do b = b0, n1bcd
if (b == e) cycle b_aibjbkbjei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aibjbkbjei: do j = n0jl, j1
if (j == k) cycle j_aibjbkbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbjei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbkbjei
i0 = max(j + 1, n0im)
i_aibjbkbjei: do i = i0, n1im
if (i == j .or. i == k) cycle i_aibjbkbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, k, b, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbkbjei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbjei
end do a_aibjbkbjei
end do j_aibjbkbjei
end do b_aibjbkbjei
end do k_aibjbkbjei
end do e_aibjbkbjei
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbiblej: do e = n0e, n1e
l_aibjbiblej: do l = n0l, n1l
b0 = max(e + 1, n0bcd)
b_aibjbiblej: do b = b0, n1bcd
if (b == e) cycle b_aibjbiblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(l + 1, n0jm)
j_aibjbiblej: do j = j0, n1jm
if (j == l) cycle j_aibjbiblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbiblej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbiblej
i0 = max(l + 1, n0ik)
i_aibjbiblej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbiblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, i, b, l, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjbiblej(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiblej
end do a_aibjbiblej
end do j_aibjbiblej
end do b_aibjbiblej
end do l_aibjbiblej
end do e_aibjbiblej
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjblei: do e = n0e, n1e
l_aibjbjblei: do l = n0l, n1l
b0 = max(e + 1, n0bcd)
b_aibjbjblei: do b = b0, n1bcd
if (b == e) cycle b_aibjbjblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(l + 1, n0jk)
j_aibjbjblei: do j = j0, n1jk
if (j == l) cycle j_aibjbjblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjblei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbjblei
i0 = max(l + 1, n0im)
i_aibjbjblei: do i = i0, n1im
if (i == j .or. i == l) cycle i_aibjbjblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, b, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbjblei(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjblei
end do a_aibjbjblei
end do j_aibjbjblei
end do b_aibjbjblei
end do l_aibjbjblei
end do e_aibjbjblei
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: e == a, d == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjciam: do m = n0m, n1m
c_aibjcjciam: do c = n0cd, n1cd
b_aibjcjciam: do b = n0b, n1b
if (b == c) cycle b_aibjcjciam
j_aibjcjciam: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjciam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjcjciam: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjciam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, m - 1, n1il)
i_aibjcjciam: do i = n0il, i1
if (i == j .or. i == m) cycle i_aibjcjciam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, c, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcjciam(b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjciam
end do a_aibjcjciam
end do j_aibjcjciam
end do b_aibjcjciam
end do c_aibjcjciam
end do m_aibjcjciam
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: e == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcicjam: do m = n0m, n1m
c_aibjcicjam: do c = n0cd, n1cd
b_aibjcicjam: do b = n0b, n1b
if (b == c) cycle b_aibjcicjam
j1 = min(m - 1, n1jl)
j_aibjcicjam: do j = n0jl, j1
if (j == m) cycle j_aibjcicjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjcicjam: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcicjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjcicjam: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjcicjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, j, a, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcicjam(b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicjam
end do a_aibjcicjam
end do j_aibjcicjam
end do b_aibjcicjam
end do c_aibjcicjam
end do m_aibjcicjam
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: e == a, d == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckcjai: do c = n0cd, n1cd
k_aibjckcjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckcjai: do b = n0b, n1b
if (b == c) cycle b_aibjckcjai
j1 = min(k - 1, n1jl)
j_aibjckcjai: do j = n0jl, j1
if (j == k) cycle j_aibjckcjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjckcjai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckcjai
i0 = max(j + 1, n0im)
i_aibjckcjai: do i = i0, n1im
if (i == j .or. i == k) cycle i_aibjckcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, c, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjckcjai(b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcjai
end do a_aibjckcjai
end do j_aibjckcjai
end do b_aibjckcjai
end do k_aibjckcjai
end do c_aibjckcjai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: e == a, d == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjclai: do l = n0l, n1l
c_aibjcjclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibjcjclai: do b = n0b, n1b
if (b == c) cycle b_aibjcjclai
j0 = max(l + 1, n0jk)
j_aibjcjclai: do j = j0, n1jk
if (j == l) cycle j_aibjcjclai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjcjclai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjclai
i0 = max(l + 1, n0im)
i_aibjcjclai: do i = i0, n1im
if (i == j .or. i == l) cycle i_aibjcjclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, c, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjclai(b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjclai
end do a_aibjcjclai
end do j_aibjcjclai
end do b_aibjcjclai
end do c_aibjcjclai
end do l_aibjcjclai
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: e == b, d == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjcibm: do m = n0m, n1m
c_aibjcjcibm: do c = n0cd, n1cd
b1 = min(c - 1, n1be)
b_aibjcjcibm: do b = n0be, b1
if (b == c) cycle b_aibjcjcibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjcibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjcibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcibm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcibm
i1 = min(j - 1, m - 1, n1il)
i_aibjcjcibm: do i = n0il, i1
if (i == j .or. i == m) cycle i_aibjcjcibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, c, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcjcibm(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcibm
end do a_aibjcjcibm
end do j_aibjcjcibm
end do b_aibjcjcibm
end do c_aibjcjcibm
end do m_aibjcjcibm
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: e == b, d == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckcibj: do c = n0cd, n1cd
k_aibjckcibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(c - 1, n1be)
b_aibjckcibj: do b = n0be, b1
if (b == c) cycle b_aibjckcibj
j_aibjckcibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckcibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckcibj
i1 = min(k - 1, j - 1, n1il)
i_aibjckcibj: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjckcibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, c, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjckcibj(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcibj
end do a_aibjckcibj
end do j_aibjckcibj
end do b_aibjckcibj
end do k_aibjckcibj
end do c_aibjckcibj
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: e == b, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcicjbm: do m = n0m, n1m
c_aibjcicjbm: do c = n0cd, n1cd
b1 = min(c - 1, n1be)
b_aibjcicjbm: do b = n0be, b1
if (b == c) cycle b_aibjcicjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j1 = min(m - 1, n1jl)
j_aibjcicjbm: do j = n0jl, j1
if (j == m) cycle j_aibjcicjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicjbm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicjbm
i0 = max(j + 1, n0ik)
i_aibjcicjbm: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjcicjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjcicjbm(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicjbm
end do a_aibjcicjbm
end do j_aibjcicjbm
end do b_aibjcicjbm
end do c_aibjcicjbm
end do m_aibjcicjbm
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: e == b, d == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjciclbj: do l = n0l, n1l
c_aibjciclbj: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(c - 1, n1be)
b_aibjciclbj: do b = n0be, b1
if (b == c) cycle b_aibjciclbj
j0 = max(l + 1, n0jm)
j_aibjciclbj: do j = j0, n1jm
if (j == l) cycle j_aibjciclbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciclbj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjciclbj
i0 = max(l + 1, n0ik)
i_aibjciclbj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjciclbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjciclbj(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciclbj
end do a_aibjciclbj
end do j_aibjciclbj
end do b_aibjciclbj
end do c_aibjciclbj
end do l_aibjciclbj
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j, m
! Equalities: c == b, e == d, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjbjdidm: do m = n0m, n1m
d_aibjbjdidm: do d = n0de, n1de
em = (d - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(d + 1, n0bc)
b_aibjbjdidm: do b = b0, n1bc
if (b == d) cycle b_aibjbjdidm
j_aibjbjdidm: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjdidm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdidm: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdidm
i0 = max(m + 1, j + 1, n0il)
i_aibjbjdidm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjdidm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, d, i, d, m)
jac_ibra_iket = eom_cc3_23_trans_aibjbjdidm(a, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdidm
end do a_aibjbjdidm
end do j_aibjbjdidm
end do b_aibjbjdidm
end do d_aibjbjdidm
end do m_aibjbjdidm
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j, k
! Equalities: c == b, e == d, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdidj: do d = n0de, n1de
k_aibjbkdidj: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdidj: do b = b0, n1bc
if (b == d) cycle b_aibjbkdidj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdidj: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkdidj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdidj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdidj
i0 = max(j + 1, k + 1, n0il)
i_aibjbkdidj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkdidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, k, d, i, d, j)
jac_ibra_iket = eom_cc3_23_trans_aibjbkdidj(a, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdidj
end do a_aibjbkdidj
end do j_aibjbkdidj
end do b_aibjbkdidj
end do k_aibjbkdidj
end do d_aibjbkdidj
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j, k
! Equalities: c == b, e == d, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdjdi: do d = n0de, n1de
k_aibjbkdjdi: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdjdi: do b = b0, n1bc
if (b == d) cycle b_aibjbkdjdi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkdjdi: do j = j0, n1jl
if (j == k) cycle j_aibjbkdjdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdjdi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdjdi
i1 = min(j - 1, n1im)
i_aibjbkdjdi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkdjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, k, d, j, d, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbkdjdi(a, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdjdi
end do a_aibjbkdjdi
end do j_aibjbkdjdi
end do b_aibjbkdjdi
end do k_aibjbkdjdi
end do d_aibjbkdjdi
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, d, a
! Free occupied indices: i, j, l
! Equalities: c == b, e == d, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdldi: do d = n0de, n1de
l_aibjbjdldi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(d + 1, n0bc)
b_aibjbjdldi: do b = b0, n1bc
if (b == d) cycle b_aibjbjdldi
j1 = min(l - 1, n1jk)
j_aibjbjdldi: do j = n0jk, j1
if (j == l) cycle j_aibjbjdldi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdldi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdldi
i1 = min(l - 1, n1im)
i_aibjbjdldi: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjbjdldi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(b, j, d, l, d, i)
jac_ibra_iket = eom_cc3_23_trans_aibjbjdldi(a, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdldi
end do a_aibjbjdldi
end do j_aibjbjdldi
end do b_aibjbjdldi
end do l_aibjbjdldi
end do d_aibjbjdldi
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


end subroutine ccjac_23_dav_part4
end module ccjac_block_23_dav_part4
