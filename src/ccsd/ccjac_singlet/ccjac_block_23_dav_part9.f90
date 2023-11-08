module ccjac_block_23_dav_part9
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
subroutine ccjac_23_dav_part9(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, b0, b1, i0, i1, j0, j1, k1, l0
integer :: n0ac, n0bd, n0bde, n0be, n0de
integer :: n0ijk, n0ijkl, n0ijkm, n0ijl, n0ijlm
integer :: n0ijm, n0ik, n0ikl, n0ikm, n0il
integer :: n0ilm, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jlm, n0jm, n0kl, n0km
integer :: n0lm
integer :: n1ac, n1bd, n1bde, n1be, n1de
integer :: n1ijk, n1ijkl, n1ijkm, n1ijl, n1ijlm
integer :: n1ijm, n1ik, n1ikl, n1ikm, n1il
integer :: n1ilm, n1im, n1jk, n1jkl, n1jkm
integer :: n1jl, n1jlm, n1jm, n1kl, n1km
integer :: n1lm
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
n0bd = max(n0b, n0d)
n0bde = max(n0b, n0d, n0e)
n0be = max(n0b, n0e)
n0de = max(n0d, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0ilm = max(n0i, n0l, n0m)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n0lm = max(n0l, n0m)
n1ac = min(n1a, n1c)
n1bd = min(n1b, n1d)
n1bde = min(n1b, n1d, n1e)
n1be = min(n1b, n1e)
n1de = min(n1d, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1ilm = min(n1i, n1l, n1m)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jlm = min(n1j, n1l, n1m)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
n1lm = min(n1l, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjaibibm: do m = n0m, n1m
b_aibjaibibm: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaibibm: do j = n0j, n1j
if (j == m) cycle j_aibjaibibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibibm: do a = a0, n1ac
if (a == b) cycle a_aibjaibibm
i0 = max(m + 1, n0ikl)
i_aibjaibibm: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjaibibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaibibm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibibm
end do a_aibjaibibm
end do j_aibjaibibm
end do b_aibjaibibm
end do m_aibjaibibm
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, d == b, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakbibm: do m = n0m, n1m
k_aibiakbibm: do k = n0k, n1k
if (k == m) cycle k_aibiakbibm
b_aibiakbibm: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiakbibm: do a = a0, n1ac
if (a == b) cycle a_aibiakbibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(m + 1, k + 1, n0ijl)
i_aibiakbibm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibiakbibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibiakbibm(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakbibm
end do a_aibiakbibm
end do b_aibiakbibm
end do k_aibiakbibm
end do m_aibiakbibm
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajbibm: do m = n0m, n1m
b_aibjajbibm: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajbibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjajbibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbibm: do a = a0, n1ac
if (a == b) cycle a_aibjajbibm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(m + 1, j + 1, n0il)
i_aibjajbibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjajbibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjajbibm(a, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbibm
end do a_aibjajbibm
end do j_aibjajbibm
end do b_aibjajbibm
end do m_aibjajbibm
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjakbibk: do k = n0km, n1km
b_aibjakbibk: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakbibk: do j = n0j, n1j
if (j == k) cycle j_aibjakbibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbibk: do a = a0, n1ac
if (a == b) cycle a_aibjakbibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i_aibjakbibk: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, b, k)
jac_ibra_iket = eom_cc3_23_trans_aibjakbibk(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbibk
end do a_aibjakbibk
end do j_aibjakbibk
end do b_aibjakbibk
end do k_aibjakbibk
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakbibj: do k = n0k, n1k
b_aibjakbibj: do b = n0bde, n1bde
j_aibjakbibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjakbibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbibj: do a = a0, n1ac
if (a == b) cycle a_aibjakbibj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aibjakbibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjakbibj(nocc, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbibj
end do a_aibjakbibj
end do j_aibjakbibj
end do b_aibjakbibj
end do k_aibjakbibj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, m
! Equalities: c == a, d == b, e == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjajbjbm: do m = n0m, n1m
b_aibjajbjbm: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jkl)
j_aibjajbjbm: do j = j0, n1jkl
if (j == m) cycle j_aibjajbjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbjbm: do a = a0, n1ac
if (a == b) cycle a_aibjajbjbm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbjbm: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjajbjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjajbjbm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbjbm
end do a_aibjajbjbm
end do j_aibjajbjbm
end do b_aibjajbjbm
end do m_aibjajbjbm
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjakbkbi: do k = n0kl, n1kl
b_aibjakbkbi: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakbkbi: do j = n0j, n1j
if (j == k) cycle j_aibjakbkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbkbi: do a = a0, n1ac
if (a == b) cycle a_aibjakbkbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aibjakbkbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, k, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakbkbi(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbkbi
end do a_aibjakbkbi
end do j_aibjakbkbi
end do b_aibjakbkbi
end do k_aibjakbkbi
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaibjbm: do m = n0m, n1m
b_aibjaibjbm: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjaibjbm: do j = j0, n1jl
if (j == m) cycle j_aibjaibjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibjbm: do a = a0, n1ac
if (a == b) cycle a_aibjaibjbm
i1 = min(j - 1, n1ik)
i_aibjaibjbm: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjaibjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, j, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibjaibjbm(nocc, a, i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibjbm
end do a_aibjaibjbm
end do j_aibjaibjbm
end do b_aibjaibjbm
end do m_aibjaibjbm
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjakbjbi: do k = n0k, n1k
b_aibjakbjbi: do b = n0bde, n1bde
j0 = max(k + 1, n0jl)
j_aibjakbjbi: do j = j0, n1jl
if (j == k) cycle j_aibjakbjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbjbi: do a = a0, n1ac
if (a == b) cycle a_aibjakbjbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjakbjbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjakbjbi(nocc, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbjbi
end do a_aibjakbjbi
end do j_aibjakbjbi
end do b_aibjakbjbi
end do k_aibjakbjbi
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l, m
! Equalities: c == a, d == b, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibiaiblbm: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aibiaiblbm: do l = l0, n1l
if (l == m) cycle l_aibiaiblbm
b_aibiaiblbm: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaiblbm: do a = a0, n1ac
if (a == b) cycle a_aibiaiblbm
i1 = min(l - 1, n1ijk)
i_aibiaiblbm: do i = n0ijk, i1
if (i == l .or. i == m) cycle i_aibiaiblbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibiaiblbm(i, b, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaiblbm
end do a_aibiaiblbm
end do b_aibiaiblbm
end do l_aibiaiblbm
end do m_aibiaiblbm
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjaiblbi: do l = n0l, n1l
b_aibjaiblbi: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblbi: do j = n0j, n1j
if (j == l) cycle j_aibjaiblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblbi: do a = a0, n1ac
if (a == b) cycle a_aibjaiblbi
i1 = min(l - 1, n1ikm)
i_aibjaiblbi: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjaiblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaiblbi(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiblbi
end do a_aibjaiblbi
end do j_aibjaiblbi
end do b_aibjaiblbi
end do l_aibjaiblbi
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjaiblbj: do l = n0l, n1l
b_aibjaiblbj: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjaiblbj: do j = n0jm, j1
if (j == l) cycle j_aibjaiblbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblbj: do a = a0, n1ac
if (a == b) cycle a_aibjaiblbj
i1 = min(l - 1, n1ik)
i_aibjaiblbj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjaiblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaiblbj(nocc, a, i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiblbj
end do a_aibjaiblbj
end do j_aibjaiblbj
end do b_aibjaiblbj
end do l_aibjaiblbj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibiakblbi: do l = n0l, n1l
k1 = min(l - 1, n1k)
k_aibiakblbi: do k = n0k, k1
if (k == l) cycle k_aibiakblbi
b_aibiakblbi: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiakblbi: do a = a0, n1ac
if (a == b) cycle a_aibiakblbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(l - 1, n1ijm)
i_aibiakblbi: do i = n0ijm, i1
if (i == k .or. i == l) cycle i_aibiakblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakblbi(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakblbi
end do a_aibiakblbi
end do b_aibiakblbi
end do k_aibiakblbi
end do l_aibiakblbi
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjajblbi: do l = n0l, n1l
b_aibjajblbi: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jk)
j_aibjajblbi: do j = n0jk, j1
if (j == l) cycle j_aibjajblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajblbi: do a = a0, n1ac
if (a == b) cycle a_aibjajblbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjajblbi: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjajblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajblbi(a, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajblbi
end do a_aibjajblbi
end do j_aibjajblbi
end do b_aibjajblbi
end do l_aibjajblbi
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjajblbj: do l = n0l, n1l
b_aibjajblbj: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jkm)
j_aibjajblbj: do j = n0jkm, j1
if (j == l) cycle j_aibjajblbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajblbj: do a = a0, n1ac
if (a == b) cycle a_aibjajblbj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajblbj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, l, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajblbj(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajblbj
end do a_aibjajblbj
end do j_aibjajblbj
end do b_aibjajblbj
end do l_aibjajblbj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakbiei: do e = n0e, n1e
k_aibiakbiei: do k = n0k, n1k
b0 = max(e + 1, n0bd)
b_aibiakbiei: do b = b0, n1bd
if (b == e) cycle b_aibiakbiei
a0 = max(b + 1, n0ac)
a_aibiakbiei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiakbiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbiei: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aibiakbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakbiei(nocc, a, i, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakbiei
end do a_aibiakbiei
end do b_aibiakbiei
end do k_aibiakbiei
end do e_aibiakbiei
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibiakbiek: do e = n0e, n1e
k_aibiakbiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibiakbiek: do b = b0, n1bd
if (b == e) cycle b_aibiakbiek
a0 = max(b + 1, n0ac)
a_aibiakbiek: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiakbiek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbiek: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, e, k)
jac_ibra_iket = eom_cc3_23_trans_aibiakbiek(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakbiek
end do a_aibiakbiek
end do b_aibiakbiek
end do k_aibiakbiek
end do e_aibiakbiek
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajbiei: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjajbiei: do b = b0, n1bd
if (b == e) cycle b_aibjajbiei
j_aibjajbiei: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbiei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajbiei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbiei: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjajbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajbiei(a, i, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbiei
end do a_aibjajbiei
end do j_aibjajbiei
end do b_aibjajbiei
end do e_aibjajbiei
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajbiej: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjajbiej: do b = b0, n1bd
if (b == e) cycle b_aibjajbiej
j_aibjajbiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbiej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajbiej
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbiej: do i = n0il, n1il
if (i == j) cycle i_aibjajbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajbiej(i, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbiej
end do a_aibjajbiej
end do j_aibjajbiej
end do b_aibjajbiej
end do e_aibjajbiej
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjaibjej: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjaibjej: do b = b0, n1bd
if (b == e) cycle b_aibjaibjej
j_aibjaibjej: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibjej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaibjej
i_aibjaibjej: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaibjej(nocc, a, i, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibjej
end do a_aibjaibjej
end do j_aibjaibjej
end do b_aibjaibjej
end do e_aibjaibjej
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaibjei: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjaibjei: do b = b0, n1bd
if (b == e) cycle b_aibjaibjei
j_aibjaibjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibjei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaibjei
i_aibjaibjei: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjaibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaibjei(nocc, a, i, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibjei
end do a_aibjaibjei
end do j_aibjaibjei
end do b_aibjaibjei
end do e_aibjaibjei
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i, m == l
! No equalities independent of the above can hold.
!
e_aibiaiblel: do e = n0e, n1e
l_aibiaiblel: do l = n0lm, n1lm
em = (e - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibiaiblel: do b = b0, n1bd
if (b == e) cycle b_aibiaiblel
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaiblel: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiaiblel
i_aibiaiblel: do i = n0ijk, n1ijk
if (i == l) cycle i_aibiaiblel
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, e, l)
jac_ibra_iket = eom_cc3_23_trans_aibiaiblel(i, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaiblel
end do a_aibiaiblel
end do b_aibiaiblel
end do l_aibiaiblel
end do e_aibiaiblel
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiaiblei: do e = n0e, n1e
l_aibiaiblei: do l = n0l, n1l
b0 = max(e + 1, n0bd)
b_aibiaiblei: do b = b0, n1bd
if (b == e) cycle b_aibiaiblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaiblei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiaiblei
i_aibiaiblei: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aibiaiblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aibiaiblei(nocc, a, i, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaiblei
end do a_aibiaiblei
end do b_aibiaiblei
end do l_aibiaiblei
end do e_aibiaiblei
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, m
! Equalities: c == a, e == d, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiaididm: do m = n0m, n1m
d_aibiaididm: do d = n0de, n1de
em = (d - nvirt0) * nocc + (m - nocc0) + 1
b_aibiaididm: do b = n0b, n1b
if (b == d) cycle b_aibiaididm
a0 = max(b + 1, d + 1, n0ac)
a_aibiaididm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaididm
i0 = max(m + 1, n0ijkl)
i_aibiaididm: do i = i0, n1ijkl
if (i == m) cycle i_aibiaididm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, i, d, m)
jac_ibra_iket = eom_cc3_23_trans_aibiaididm(b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaididm
end do a_aibiaididm
end do b_aibiaididm
end do d_aibiaididm
end do m_aibiaididm
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j
! Equalities: c == a, e == d, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjajdidj: do d = n0de, n1de
b_aibjajdidj: do b = n0b, n1b
if (b == d) cycle b_aibjajdidj
j_aibjajdidj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdidj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdidj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjajdidj: do i = i0, n1il
if (i == j) cycle i_aibjajdidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, d, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajdidj(b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdidj
end do a_aibjajdidj
end do j_aibjajdidj
end do b_aibjajdidj
end do d_aibjajdidj
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, j
! Equalities: c == a, e == d, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjajdjdi: do d = n0de, n1de
b_aibjajdjdi: do b = n0b, n1b
if (b == d) cycle b_aibjajdjdi
j_aibjajdjdi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdjdi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdjdi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjajdjdi: do i = n0im, i1
if (i == j) cycle i_aibjajdjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, j, d, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajdjdi(b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdjdi
end do a_aibjajdjdi
end do j_aibjajdjdi
end do b_aibjajdjdi
end do d_aibjajdjdi
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, d, b
! Free occupied indices: i, l
! Equalities: c == a, e == d, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibiaidldi: do d = n0de, n1de
l_aibiaidldi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidldi: do b = n0b, n1b
if (b == d) cycle b_aibiaidldi
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidldi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidldi
i1 = min(l - 1, n1ijkm)
i_aibiaidldi: do i = n0ijkm, i1
if (i == l) cycle i_aibiaidldi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, d, i)
jac_ibra_iket = eom_cc3_23_trans_aibiaidldi(b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidldi
end do a_aibiaidldi
end do b_aibiaidldi
end do l_aibiaidldi
end do d_aibiaidldi
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibiakdibi: do d = n0d, n1d
k_aibiakdibi: do k = n0k, n1k
b1 = min(d - 1, n1be)
b_aibiakdibi: do b = n0be, b1
if (b == d) cycle b_aibiakdibi
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdibi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdibi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdibi: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aibiakdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakdibi(nocc, a, i, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdibi
end do a_aibiakdibi
end do b_aibiakdibi
end do k_aibiakdibi
end do d_aibiakdibi
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibiakdibk: do d = n0d, n1d
k_aibiakdibk: do k = n0km, n1km
b1 = min(d - 1, n1be)
b_aibiakdibk: do b = n0be, b1
if (b == d) cycle b_aibiakdibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdibk: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdibk: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, d, i, b, k)
jac_ibra_iket = eom_cc3_23_trans_aibiakdibk(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdibk
end do a_aibiakdibk
end do b_aibiakdibk
end do k_aibiakdibk
end do d_aibiakdibk
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdibi: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjajdibi: do b = n0be, b1
if (b == d) cycle b_aibjajdibi
j_aibjajdibi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdibi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdibi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdibi: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjajdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajdibi(a, i, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdibi
end do a_aibjajdibi
end do j_aibjajdibi
end do b_aibjajdibi
end do d_aibjajdibi
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

end subroutine ccjac_23_dav_part9
end module ccjac_block_23_dav_part9
