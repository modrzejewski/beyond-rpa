module ccjac_block_23_dav_part13
use eom_cc3_23_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-21 14:27:00 UTC.
!
contains
subroutine ccjac_23_dav_part13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a, b, c, d
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, a1, i0, i1
integer :: n0abc, n0abde, n0abe, n0ac, n0bde
integer :: n0cd, n0de, n0ijkl, n0ijkm, n0ijl
integer :: n0ijlm, n0ijm, n0ik, n0ikl, n0ikm
integer :: n0il, n0im, n0jkl, n0jkm, n0jl
integer :: n0jlm, n0jm, n0kl, n0km
integer :: n1abc, n1abde, n1abe, n1ac, n1bde
integer :: n1cd, n1de, n1ijkl, n1ijkm, n1ijl
integer :: n1ijlm, n1ijm, n1ik, n1ikl, n1ikm
integer :: n1il, n1im, n1jkl, n1jkm, n1jl
integer :: n1jlm, n1jm, n1kl, n1km
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
n0abde = max(n0a, n0b, n0d, n0e)
n0abe = max(n0a, n0b, n0e)
n0ac = max(n0a, n0c)
n0bde = max(n0b, n0d, n0e)
n0cd = max(n0c, n0d)
n0de = max(n0d, n0e)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jkl = max(n0j, n0k, n0l)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1abc = min(n1a, n1b, n1c)
n1abde = min(n1a, n1b, n1d, n1e)
n1abe = min(n1a, n1b, n1e)
n1ac = min(n1a, n1c)
n1bde = min(n1b, n1d, n1e)
n1cd = min(n1c, n1d)
n1de = min(n1d, n1e)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jkl = min(n1j, n1k, n1l)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jlm = min(n1j, n1l, n1m)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, m
! Equalities: b == a, c == a, e == d, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiaiaididm: do m = n0m, n1m
d_aiaiaididm: do d = n0de, n1de
em = (d - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiaiaididm: do a = a0, n1abc
if (a == d) cycle a_aiaiaididm
i0 = max(m + 1, n0ijkl)
i_aiaiaididm: do i = i0, n1ijkl
if (i == m) cycle i_aiaiaididm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, i, d, m)
jac_ibra_iket = eom_cc3_23_trans_aiaiaididm(a, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiaididm
end do a_aiaiaididm
end do d_aiaiaididm
end do m_aiaiaididm
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, e == d, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajaididj: do d = n0de, n1de
j_aiajaididj: do j = n0jm, n1jm
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajaididj: do a = a0, n1abc
if (a == d) cycle a_aiajaididj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajaididj: do i = i0, n1ikl
if (i == j) cycle i_aiajaididj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, i, d, j)
jac_ibra_iket = eom_cc3_23_trans_aiajaididj(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaididj
end do a_aiajaididj
end do j_aiajaididj
end do d_aiajaididj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, e == d, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajajdidj: do d = n0de, n1de
j_aiajajdidj: do j = n0jkm, n1jkm
em = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajajdidj: do a = a0, n1abc
if (a == d) cycle a_aiajajdidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajajdidj: do i = i0, n1il
if (i == j) cycle i_aiajajdidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, d, i, d, j)
jac_ibra_iket = eom_cc3_23_trans_aiajajdidj(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajdidj
end do a_aiajajdidj
end do j_aiajajdidj
end do d_aiajajdidj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, l
! Equalities: b == a, c == a, e == d, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aiaiaidldi: do d = n0de, n1de
l_aiaiaidldi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiaiaidldi: do a = a0, n1abc
if (a == d) cycle a_aiaiaidldi
i1 = min(l - 1, n1ijkm)
i_aiaiaidldi: do i = n0ijkm, i1
if (i == l) cycle i_aiaiaidldi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, d, l, d, i)
jac_ibra_iket = eom_cc3_23_trans_aiaiaidldi(a, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiaidldi
end do a_aiaiaidldi
end do l_aiaiaidldi
end do d_aiaiaidldi
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, m
! Equalities: c == a, d == b, e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiaibibm: do m = n0m, n1m
b_aibiaibibm: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaibibm: do a = a0, n1ac
if (a == b) cycle a_aibiaibibm
i0 = max(m + 1, n0ijkl)
i_aibiaibibm: do i = i0, n1ijkl
if (i == m) cycle i_aibiaibibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, i, b, m)
jac_ibra_iket = eom_cc3_23_trans_aibiaibibm(nocc, a, i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaibibm
end do a_aibiaibibm
end do b_aibiaibibm
end do m_aibiaibibm
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
b_aibjaibibj: do b = n0bde, n1bde
j_aibjaibibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibibj: do a = a0, n1ac
if (a == b) cycle a_aibjaibibj
i0 = max(j + 1, n0ikl)
i_aibjaibibj: do i = i0, n1ikl
if (i == j) cycle i_aibjaibibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjaibibj(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibibj
end do a_aibjaibibj
end do j_aibjaibibj
end do b_aibjaibibj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibiakbibk: do k = n0km, n1km
b_aibiakbibk: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiakbibk: do a = a0, n1ac
if (a == b) cycle a_aibiakbibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakbibk: do i = i0, n1ijl
if (i == k) cycle i_aibiakbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, i, b, k)
jac_ibra_iket = eom_cc3_23_trans_aibiakbibk(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakbibk
end do a_aibiakbibk
end do b_aibiakbibk
end do k_aibiakbibk
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
b_aibjajbibj: do b = n0bde, n1bde
j_aibjajbibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbibj: do a = a0, n1ac
if (a == b) cycle a_aibjajbibj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjajbibj: do i = i0, n1il
if (i == j) cycle i_aibjajbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjajbibj(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbibj
end do a_aibjajbibj
end do j_aibjajbibj
end do b_aibjajbibj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjajbjbi: do b = n0bde, n1bde
j_aibjajbjbi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbjbi: do a = a0, n1ac
if (a == b) cycle a_aibjajbjbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjajbjbi: do i = n0im, i1
if (i == j) cycle i_aibjajbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, j, b, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjajbjbi(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbjbi
end do a_aibjajbjbi
end do j_aibjajbjbi
end do b_aibjajbjbi
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakbkbi: do k = n0kl, n1kl
b_aibiakbkbi: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiakbkbi: do a = a0, n1ac
if (a == b) cycle a_aibiakbkbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijm)
i_aibiakbkbi: do i = n0ijm, i1
if (i == k) cycle i_aibiakbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, k, b, k, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiakbkbi(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakbkbi
end do a_aibiakbkbi
end do b_aibiakbkbi
end do k_aibiakbkbi
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibjbi: do b = n0bde, n1bde
j_aibjaibjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibjbi: do a = a0, n1ac
if (a == b) cycle a_aibjaibjbi
i1 = min(j - 1, n1ikm)
i_aibjaibjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjaibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, j, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjaibjbi(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibjbi
end do a_aibjaibjbi
end do j_aibjaibjbi
end do b_aibjaibjbi
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiaiblbi: do l = n0l, n1l
b_aibiaiblbi: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaiblbi: do a = a0, n1ac
if (a == b) cycle a_aibiaiblbi
i1 = min(l - 1, n1ijkm)
i_aibiaiblbi: do i = n0ijkm, i1
if (i == l) cycle i_aibiaiblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(a, i, b, l, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibiaiblbi(nocc, a, i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaiblbi
end do a_aibiaiblbi
end do b_aibiaiblbi
end do l_aibiaiblbi
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, e == a, d == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiaickciai: do c = n0cd, n1cd
k_aiaickciai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiaickciai: do a = n0abe, a1
if (a == c) cycle a_aiaickciai
i1 = min(k - 1, n1ijlm)
i_aiaickciai: do i = n0ijlm, i1
if (i == k) cycle i_aiaickciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, c, i, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiaickciai(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickciai
end do a_aiaickciai
end do k_aiaickciai
end do c_aiaickciai
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, e == a, d == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcicjaj: do c = n0cd, n1cd
j_aiajcicjaj: do j = n0jlm, n1jlm
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiajcicjaj: do a = n0abe, a1
if (a == c) cycle a_aiajcicjaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcicjaj: do i = i0, n1ik
if (i == j) cycle i_aiajcicjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, j, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajcicjaj(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicjaj
end do a_aiajcicjaj
end do j_aiajcicjaj
end do c_aiajcicjaj
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, e == a, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicjai: do c = n0cd, n1cd
j_aiajcicjai: do j = n0jl, n1jl
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiajcicjai: do a = n0abe, a1
if (a == c) cycle a_aiajcicjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajcicjai: do i = i0, n1ikm
if (i == j) cycle i_aiajcicjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajcicjai(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicjai
end do a_aiajcicjai
end do j_aiajcicjai
end do c_aiajcicjai
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l
! Equalities: b == a, e == a, d == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiaiciclai: do l = n0l, n1l
c_aiaiciclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiaiciclai: do a = n0abe, a1
if (a == c) cycle a_aiaiciclai
i0 = max(l + 1, n0ijkm)
i_aiaiciclai: do i = i0, n1ijkm
if (i == l) cycle i_aiaiciclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, c, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiaiciclai(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiciclai
end do a_aiaiciclai
end do c_aiaiciclai
end do l_aiaiciclai
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, m
! Equalities: b == a, d == a, e == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiaiciaiam: do m = n0m, n1m
c_aiaiciaiam: do c = n0c, n1c
a1 = min(c - 1, n1abde)
a_aiaiciaiam: do a = n0abde, a1
if (a == c) cycle a_aiaiciaiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ijkl)
i_aiaiciaiam: do i = i0, n1ijkl
if (i == m) cycle i_aiaiciaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiaiciaiam(nocc, a, i, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiciaiam
end do a_aiaiciaiam
end do c_aiaiciaiam
end do m_aiaiciaiam
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajciaiaj: do c = n0c, n1c
j_aiajciaiaj: do j = n0jm, n1jm
a1 = min(c - 1, n1abde)
a_aiajciaiaj: do a = n0abde, a1
if (a == c) cycle a_aiajciaiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajciaiaj: do i = i0, n1ikl
if (i == j) cycle i_aiajciaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajciaiaj(nocc, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaiaj
end do a_aiajciaiaj
end do j_aiajciaiaj
end do c_aiajciaiaj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiaickaiak: do c = n0c, n1c
k_aiaickaiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiaickaiak: do a = n0abde, a1
if (a == c) cycle a_aiaickaiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aiaickaiak: do i = i0, n1ijl
if (i == k) cycle i_aiaickaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, a, k)
jac_ibra_iket = eom_cc3_23_trans_aiaickaiak(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickaiak
end do a_aiaickaiak
end do k_aiaickaiak
end do c_aiaickaiak
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjaiaj: do c = n0c, n1c
j_aiajcjaiaj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiajcjaiaj: do a = n0abde, a1
if (a == c) cycle a_aiajcjaiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjaiaj: do i = i0, n1il
if (i == j) cycle i_aiajcjaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajcjaiaj(nocc, a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaiaj
end do a_aiajcjaiaj
end do j_aiajcjaiaj
end do c_aiajcjaiaj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == a, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiaickakai: do c = n0c, n1c
k_aiaickakai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiaickakai: do a = n0abde, a1
if (a == c) cycle a_aiaickakai
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijm)
i_aiaickakai: do i = n0ijm, i1
if (i == k) cycle i_aiaickakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, k, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiaickakai(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickakai
end do a_aiaickakai
end do k_aiaickakai
end do c_aiaickakai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l
! Equalities: b == a, d == a, e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiaicialai: do l = n0l, n1l
c_aiaicialai: do c = n0c, n1c
a1 = min(c - 1, n1abde)
a_aiaicialai: do a = n0abde, a1
if (a == c) cycle a_aiaicialai
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijkm)
i_aiaicialai: do i = n0ijkm, i1
if (i == l) cycle i_aiaicialai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiaicialai(nocc, a, i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicialai
end do a_aiaicialai
end do c_aiaicialai
end do l_aiaicialai
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

end subroutine ccjac_23_dav_part13
end module ccjac_block_23_dav_part13
