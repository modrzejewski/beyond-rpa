module ccjac_block_32_dav_part4
use eom_cc3_32_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-22 13:44:23 UTC.
!
contains
subroutine ccjac_32_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a, b, c, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, b0, i1, j0, j1
integer :: n0ad, n0ade, n0bc, n0bce, n0be
integer :: n0ce, n0ik, n0ikl, n0ikm, n0il
integer :: n0ilm, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jlm, n0jm, n0kl, n0klm
integer :: n0km
integer :: n1ad, n1ade, n1bc, n1bce, n1be
integer :: n1ce, n1ik, n1ikl, n1ikm, n1il
integer :: n1ilm, n1im, n1jk, n1jkl, n1jkm
integer :: n1jl, n1jlm, n1jm, n1kl, n1klm
integer :: n1km
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
n0ad = max(n0a, n0d)
n0ade = max(n0a, n0d, n0e)
n0bc = max(n0b, n0c)
n0bce = max(n0b, n0c, n0e)
n0be = max(n0b, n0e)
n0ce = max(n0c, n0e)
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
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1ad = min(n1a, n1d)
n1ade = min(n1a, n1d, n1e)
n1bc = min(n1b, n1c)
n1bce = min(n1b, n1c, n1e)
n1be = min(n1b, n1e)
n1ce = min(n1c, n1e)
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
n1klm = min(n1k, n1l, n1m)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == a, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckakaj: do c = n0c, n1c
k_aibjckakaj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakaj: do b = b0, n1b
if (b == c) cycle b_aibjckakaj
j1 = min(k - 1, n1jm)
j_aibjckakaj: do j = n0jm, j1
if (j == k) cycle j_aibjckakaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjckakaj: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjckakaj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckakaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakaj
if (i > j .and. j > k) exit i_aibjckakaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakaj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakaj(a, i, b, c)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckakaj(a, i, b, c)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakaj(a, i, b, c)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckakaj(a, i, b, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakaj
end do a_aibjckakaj
end do j_aibjckakaj
end do b_aibjckakaj
end do k_aibjckakaj
end do c_aibjckakaj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: d == a, c == b, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjbkaibm: do m = n0m, n1m
k_aibjbkaibm: do k = n0k, n1k
if (k == m) cycle k_aibjbkaibm
b_aibjbkaibm: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkaibm: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjbkaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkaibm: do a = a0, n1ad
if (a == b) cycle a_aibjbkaibm
i1 = min(j - 1, n1il)
i_aibjbkaibm: do i = n0il, i1
if (i == j .or. i == k .or. i == m) cycle i_aibjbkaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkaibm(b, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkaibm
end do a_aibjbkaibm
end do j_aibjbkaibm
end do b_aibjbkaibm
end do k_aibjbkaibm
end do m_aibjbkaibm
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: d == a, c == b, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjbkalbi: do l = n0l, n1l
k_aibjbkalbi: do k = n0k, n1k
if (k == l) cycle k_aibjbkalbi
b_aibjbkalbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkalbi: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjbkalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkalbi: do a = a0, n1ad
if (a == b) cycle a_aibjbkalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjbkalbi: do i = n0im, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkalbi(b, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkalbi
end do a_aibjbkalbi
end do j_aibjbkalbi
end do b_aibjbkalbi
end do k_aibjbkalbi
end do l_aibjbkalbi
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: d == a, c == b, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjbkajbm: do m = n0m, n1m
k_aibjbkajbm: do k = n0k, n1k
if (k == m) cycle k_aibjbkajbm
b_aibjbkajbm: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkajbm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjbkajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkajbm: do a = a0, n1ad
if (a == b) cycle a_aibjbkajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkajbm: do i = n0i, i1
if (i == j .or. i == k .or. i == m) cycle i_aibjbkajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkajbm(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkajbm
end do a_aibjbkajbm
end do j_aibjbkajbm
end do b_aibjbkajbm
end do k_aibjbkajbm
end do m_aibjbkajbm
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: d == a, c == b, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjbkalbj: do l = n0l, n1l
k_aibjbkalbj: do k = n0k, n1k
if (k == l) cycle k_aibjbkalbj
b_aibjbkalbj: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkalbj: do j = j0, n1jm
if (j == k .or. j == l) cycle j_aibjbkalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkalbj: do a = a0, n1ad
if (a == b) cycle a_aibjbkalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkalbj: do i = n0i, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkalbj(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkalbj
end do a_aibjbkalbj
end do j_aibjbkalbj
end do b_aibjbkalbj
end do k_aibjbkalbj
end do l_aibjbkalbj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j, l
! Equalities: d == a, c == b, e == b, m == k
! No equalities independent of the above can hold.
!
l_aibjbkalbk: do l = n0l, n1l
k_aibjbkalbk: do k = n0km, n1km
if (k == l) cycle k_aibjbkalbk
b_aibjbkalbk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkalbk: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjbkalbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkalbk: do a = a0, n1ad
if (a == b) cycle a_aibjbkalbk
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkalbk: do i = n0i, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkalbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkalbk(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkalbk
end do a_aibjbkalbk
end do j_aibjbkalbk
end do b_aibjbkalbk
end do k_aibjbkalbk
end do l_aibjbkalbk
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkaiej: do e = n0e, n1e
k_aibjbkaiej: do k = n0k, n1k
b_aibjbkaiej: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkaiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkaiej: do j = j0, n1jm
if (j == k) cycle j_aibjbkaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkaiej: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkaiej
i1 = min(j - 1, n1il)
i_aibjbkaiej: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkaiej(b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkaiej
end do a_aibjbkaiej
end do j_aibjbkaiej
end do b_aibjbkaiej
end do k_aibjbkaiej
end do e_aibjbkaiej
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjbkaiek: do e = n0e, n1e
k_aibjbkaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkaiek: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkaiek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkaiek: do j = j0, n1j
if (j == k) cycle j_aibjbkaiek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkaiek: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkaiek
i1 = min(j - 1, n1il)
i_aibjbkaiek: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkaiek(b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkaiek
end do a_aibjbkaiek
end do j_aibjbkaiek
end do b_aibjbkaiek
end do k_aibjbkaiek
end do e_aibjbkaiek
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbkajei: do e = n0e, n1e
k_aibjbkajei: do k = n0k, n1k
b_aibjbkajei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkajei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkajei: do j = j0, n1jl
if (j == k) cycle j_aibjbkajei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkajei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjbkajei: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkajei(b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkajei
end do a_aibjbkajei
end do j_aibjbkajei
end do b_aibjbkajei
end do k_aibjbkajei
end do e_aibjbkajei
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, k, i
! Equalities: d == a, c == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjbkajek: do e = n0e, n1e
k_aibjbkajek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkajek: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkajek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkajek: do j = j0, n1jl
if (j == k) cycle j_aibjbkajek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkajek: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkajek
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkajek: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkajek(i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkajek
end do a_aibjbkajek
end do j_aibjbkajek
end do b_aibjbkajek
end do k_aibjbkajek
end do e_aibjbkajek
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjciaibm: do m = n0m, n1m
c_aibjciaibm: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciaibm: do b = b0, n1be
if (b == c) cycle b_aibjciaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjciaibm: do j = n0j, n1j
if (j == m) cycle j_aibjciaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaibm
i_aibjciaibm: do i = n0ikl, n1ikl
if (i == j .or. i == m) cycle i_aibjciaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaibm(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaibm
end do a_aibjciaibm
end do j_aibjciaibm
end do b_aibjciaibm
end do c_aibjciaibm
end do m_aibjciaibm
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckaibi: do c = n0c, n1c
k_aibjckaibi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckaibi: do b = b0, n1be
if (b == c) cycle b_aibjckaibi
j_aibjckaibi: do j = n0j, n1j
if (j == k) cycle j_aibjckaibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibi
i_aibjckaibi: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjckaibi
if (i > j .and. j > k) exit i_aibjckaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaibi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaibi(i, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaibi(i, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaibi(i, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaibi(i, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaibi
end do a_aibjckaibi
end do j_aibjckaibi
end do b_aibjckaibi
end do k_aibjckaibi
end do c_aibjckaibi
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaibm: do m = n0m, n1m
c_aibjcjaibm: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcjaibm: do b = b0, n1be
if (b == c) cycle b_aibjcjaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjaibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaibm
i_aibjcjaibm: do i = n0il, n1il
if (i == j .or. i == m) cycle i_aibjcjaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjaibm(j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaibm
end do a_aibjcjaibm
end do j_aibjcjaibm
end do b_aibjcjaibm
end do c_aibjcjaibm
end do m_aibjcjaibm
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaibj: do c = n0c, n1c
k_aibjckaibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckaibj: do b = b0, n1be
if (b == c) cycle b_aibjckaibj
j_aibjckaibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibj
i_aibjckaibj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaibj
if (i > j .and. j > k) exit i_aibjckaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaibj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaibj(a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaibj
end do a_aibjckaibj
end do j_aibjckaibj
end do b_aibjckaibj
end do k_aibjckaibj
end do c_aibjckaibj
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaibk: do c = n0c, n1c
k_aibjckaibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckaibk: do b = b0, n1be
if (b == c) cycle b_aibjckaibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckaibk: do j = n0j, n1j
if (j == k) cycle j_aibjckaibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibk
i_aibjckaibk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaibk
if (i > j .and. j > k) exit i_aibjckaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaibk
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaibk(b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaibk(a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaibk(b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaibk(b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaibk(a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaibk
end do a_aibjckaibk
end do j_aibjckaibk
end do b_aibjckaibk
end do k_aibjckaibk
end do c_aibjckaibk
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjcialbi: do l = n0l, n1l
c_aibjcialbi: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcialbi: do b = b0, n1be
if (b == c) cycle b_aibjcialbi
j_aibjcialbi: do j = n0j, n1j
if (j == l) cycle j_aibjcialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcialbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbi: do i = n0ikm, n1ikm
if (i == j .or. i == l) cycle i_aibjcialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcialbi(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialbi
end do a_aibjcialbi
end do j_aibjcialbi
end do b_aibjcialbi
end do c_aibjcialbi
end do l_aibjcialbi
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialbj: do l = n0l, n1l
c_aibjcialbj: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcialbj: do b = b0, n1be
if (b == c) cycle b_aibjcialbj
j_aibjcialbj: do j = n0jm, n1jm
if (j == l) cycle j_aibjcialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcialbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcialbj(i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialbj
end do a_aibjcialbj
end do j_aibjcialbj
end do b_aibjcialbj
end do c_aibjcialbj
end do l_aibjcialbj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckajbi: do c = n0c, n1c
k_aibjckajbi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckajbi: do b = b0, n1be
if (b == c) cycle b_aibjckajbi
j_aibjckajbi: do j = n0jl, n1jl
if (j == k) cycle j_aibjckajbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckajbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbi: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckajbi
if (i > j .and. j > k) exit i_aibjckajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajbi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckajbi(a, j, c, k)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajbi(a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajbi(i, b, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajbi
end do a_aibjckajbi
end do j_aibjckajbi
end do b_aibjckajbi
end do k_aibjckajbi
end do c_aibjckajbi
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckakbi: do c = n0c, n1c
k_aibjckakbi: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckakbi: do b = b0, n1be
if (b == c) cycle b_aibjckakbi
j_aibjckakbi: do j = n0j, n1j
if (j == k) cycle j_aibjckakbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckakbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakbi
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbi: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckakbi
if (i > j .and. j > k) exit i_aibjckakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakbi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakbi(a, i, b, j, c, k)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckakbi(a, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakbi(i, b, j, c)
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakbi
end do a_aibjckakbi
end do j_aibjckakbi
end do b_aibjckakbi
end do k_aibjckakbi
end do c_aibjckakbi
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, m
! Equalities: d == a, e == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjcjajbm: do m = n0m, n1m
c_aibjcjajbm: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcjajbm: do b = b0, n1be
if (b == c) cycle b_aibjcjajbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjajbm: do j = n0jkl, n1jkl
if (j == m) cycle j_aibjcjajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjajbm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajbm: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjcjajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajbm(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajbm
end do a_aibjcjajbm
end do j_aibjcjajbm
end do b_aibjcjajbm
end do c_aibjcjajbm
end do m_aibjcjajbm
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckajbj: do c = n0c, n1c
k_aibjckajbj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckajbj: do b = b0, n1be
if (b == c) cycle b_aibjckajbj
j_aibjckajbj: do j = n0jlm, n1jlm
if (j == k) cycle j_aibjckajbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckajbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajbj
if (i > j .and. j > k) exit i_aibjckajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajbj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckajbj(i, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckajbj(i, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckajbj(i, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajbj(i, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajbj
end do a_aibjckajbj
end do j_aibjckajbj
end do b_aibjckajbj
end do k_aibjckajbj
end do c_aibjckajbj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajbk: do c = n0c, n1c
k_aibjckajbk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckajbk: do b = b0, n1be
if (b == c) cycle b_aibjckajbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckajbk: do j = n0jl, n1jl
if (j == k) cycle j_aibjckajbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckajbk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajbk
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajbk
if (i > j .and. j > k) exit i_aibjckajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajbk
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckajbk(a, i, j, c)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajbk(i, b, c, k)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajbk(a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajbk
end do a_aibjckajbk
end do j_aibjckajbk
end do b_aibjckajbk
end do k_aibjckajbk
end do c_aibjckajbk
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjcjalbj: do l = n0l, n1l
c_aibjcjalbj: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcjalbj: do b = b0, n1be
if (b == c) cycle b_aibjcjalbj
j_aibjcjalbj: do j = n0jkm, n1jkm
if (j == l) cycle j_aibjcjalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjalbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalbj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjalbj(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjalbj
end do a_aibjcjalbj
end do j_aibjcjalbj
end do b_aibjcjalbj
end do c_aibjcjalbj
end do l_aibjcjalbj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckakbj: do c = n0c, n1c
k_aibjckakbj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckakbj: do b = b0, n1be
if (b == c) cycle b_aibjckakbj
j_aibjckakbj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckakbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakbj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakbj
if (i > j .and. j > k) exit i_aibjckakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakbj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakbj(a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckakbj(a, i, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckakbj(a, i, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakbj(a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckakbj(a, i, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakbj
end do a_aibjckakbj
end do j_aibjckakbj
end do b_aibjckakbj
end do k_aibjckakbj
end do c_aibjckakbj
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j
! Equalities: d == a, e == b, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckakbk: do c = n0c, n1c
k_aibjckakbk: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckakbk: do b = b0, n1be
if (b == c) cycle b_aibjckakbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckakbk: do j = n0j, n1j
if (j == k) cycle j_aibjckakbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckakbk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakbk
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakbk
if (i > j .and. j > k) exit i_aibjckakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakbk
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakbk(i, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckakbk(i, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakbk(i, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckakbk(i, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakbk
end do a_aibjckakbk
end do j_aibjckakbk
end do b_aibjckakbk
end do k_aibjckakbk
end do c_aibjckakbk
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: d == a, e == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjciaicm: do m = n0m, n1m
c_aibjciaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjciaicm: do b = b0, n1b
if (b == c) cycle b_aibjciaicm
j_aibjciaicm: do j = n0j, n1j
if (j == m) cycle j_aibjciaicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjciaicm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaicm
i_aibjciaicm: do i = n0ikl, n1ikl
if (i == j .or. i == m) cycle i_aibjciaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaicm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaicm
end do a_aibjciaicm
end do j_aibjciaicm
end do b_aibjciaicm
end do c_aibjciaicm
end do m_aibjciaicm
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: d == a, e == c, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckaici: do c = n0ce, n1ce
k_aibjckaici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaici: do b = b0, n1b
if (b == c) cycle b_aibjckaici
j_aibjckaici: do j = n0j, n1j
if (j == k) cycle j_aibjckaici
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckaici: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaici
i_aibjckaici: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjckaici
if (i > j .and. j > k) exit i_aibjckaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaici
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaici(i, b, j, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaici(i, b, j, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaici(i, b, j, k)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaici(i, b, j, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaici
end do a_aibjckaici
end do j_aibjckaici
end do b_aibjckaici
end do k_aibjckaici
end do c_aibjckaici
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: d == a, e == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaicm: do m = n0m, n1m
c_aibjcjaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjcjaicm: do b = b0, n1b
if (b == c) cycle b_aibjcjaicm
j_aibjcjaicm: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjaicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjaicm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaicm
i_aibjcjaicm: do i = n0il, n1il
if (i == j .or. i == m) cycle i_aibjcjaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjaicm(b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaicm
end do a_aibjcjaicm
end do j_aibjcjaicm
end do b_aibjcjaicm
end do c_aibjcjaicm
end do m_aibjcjaicm
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: d == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaicj: do c = n0ce, n1ce
k_aibjckaicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaicj: do b = b0, n1b
if (b == c) cycle b_aibjckaicj
j_aibjckaicj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckaicj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaicj
i_aibjckaicj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaicj
if (i > j .and. j > k) exit i_aibjckaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaicj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaicj(a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaicj(a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaicj(b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaicj(b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaicj(b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaicj
end do a_aibjckaicj
end do j_aibjckaicj
end do b_aibjckaicj
end do k_aibjckaicj
end do c_aibjckaicj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, j
! Equalities: d == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaick: do c = n0ce, n1ce
k_aibjckaick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaick: do b = b0, n1b
if (b == c) cycle b_aibjckaick
j_aibjckaick: do j = n0j, n1j
if (j == k) cycle j_aibjckaick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckaick: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaick
i_aibjckaick: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaick
if (i > j .and. j > k) exit i_aibjckaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaick
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaick(a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaick
end do a_aibjckaick
end do j_aibjckaick
end do b_aibjckaick
end do k_aibjckaick
end do c_aibjckaick
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
end subroutine ccjac_32_dav_part4
end module ccjac_block_32_dav_part4
