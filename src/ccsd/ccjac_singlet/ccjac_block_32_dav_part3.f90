module ccjac_block_32_dav_part3
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
subroutine ccjac_32_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b0, c0, c1, i0, i1, j0, j1
integer :: n0ab, n0abd, n0abe, n0ade, n0cd
integer :: n0ce, n0il, n0im, n0jl, n0jm
integer :: n0kl, n0km
integer :: n1ab, n1abd, n1abe, n1ade, n1cd
integer :: n1ce, n1il, n1im, n1jl, n1jm
integer :: n1kl, n1km
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
n0abe = max(n0a, n0b, n0e)
n0ade = max(n0a, n0d, n0e)
n0cd = max(n0c, n0d)
n0ce = max(n0c, n0e)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1abe = min(n1a, n1b, n1e)
n1ade = min(n1a, n1d, n1e)
n1cd = min(n1c, n1d)
n1ce = min(n1c, n1e)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, m
! Equalities: b == a, d == a, e == c, l == i
! No equalities independent of the above can hold.
!
m_aiajckaicm: do m = n0m, n1m
c_aiajckaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiajckaicm: do k = n0k, n1k
if (k == m) cycle k_aiajckaicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckaicm: do j = n0j, j1
if (j == k .or. j == m) cycle j_aiajckaicm
a0 = max(c + 1, n0abd)
a_aiajckaicm: do a = a0, n1abd
if (a == c) cycle a_aiajckaicm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckaicm: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aiajckaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaicm(a, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaicm
end do a_aiajckaicm
end do j_aiajckaicm
end do k_aiajckaicm
end do c_aiajckaicm
end do m_aiajckaicm
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, m
! Equalities: b == a, d == a, e == c, l == j
! No equalities independent of the above can hold.
!
m_aiajckajcm: do m = n0m, n1m
c_aiajckajcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiajckajcm: do k = n0k, n1k
if (k == m) cycle k_aiajckajcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckajcm: do j = n0jl, j1
if (j == k .or. j == m) cycle j_aiajckajcm
a0 = max(c + 1, n0abd)
a_aiajckajcm: do a = a0, n1abd
if (a == c) cycle a_aiajckajcm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckajcm: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajckajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckajcm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajcm
end do a_aiajckajcm
end do j_aiajckajcm
end do k_aiajckajcm
end do c_aiajckajcm
end do m_aiajckajcm
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, l
! Equalities: b == a, d == a, e == c, m == j
! No equalities independent of the above can hold.
!
l_aiajckalcj: do l = n0l, n1l
c_aiajckalcj: do c = n0ce, n1ce
k_aiajckalcj: do k = n0k, n1k
if (k == l) cycle k_aiajckalcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckalcj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aiajckalcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckalcj: do a = a0, n1abd
if (a == c) cycle a_aiajckalcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckalcj: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckalcj(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckalcj
end do a_aiajckalcj
end do j_aiajckalcj
end do k_aiajckalcj
end do c_aiajckalcj
end do l_aiajckalcj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, m
! Equalities: b == a, d == a, e == c, l == k
! No equalities independent of the above can hold.
!
m_aiajckakcm: do m = n0m, n1m
c_aiajckakcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiajckakcm: do k = n0kl, n1kl
if (k == m) cycle k_aiajckakcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckakcm: do j = n0j, j1
if (j == k .or. j == m) cycle j_aiajckakcm
a0 = max(c + 1, n0abd)
a_aiajckakcm: do a = a0, n1abd
if (a == c) cycle a_aiajckakcm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckakcm: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajckakcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakcm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakcm
end do a_aiajckakcm
end do j_aiajckakcm
end do k_aiajckakcm
end do c_aiajckakcm
end do m_aiajckakcm
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, l
! Equalities: b == a, d == a, e == c, m == k
! No equalities independent of the above can hold.
!
l_aiajckalck: do l = n0l, n1l
c_aiajckalck: do c = n0ce, n1ce
k_aiajckalck: do k = n0km, n1km
if (k == l) cycle k_aiajckalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckalck: do j = n0j, j1
if (j == k .or. j == l) cycle j_aiajckalck
a0 = max(c + 1, n0abd)
a_aiajckalck: do a = a0, n1abd
if (a == c) cycle a_aiajckalck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckalck: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckalck(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckalck
end do a_aiajckalck
end do j_aiajckalck
end do k_aiajckalck
end do c_aiajckalck
end do l_aiajckalck
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajckaiej: do e = n0e, n1e
c_aiajckaiej: do c = n0c, n1c
if (c == e) cycle c_aiajckaiej
k_aiajckaiej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckaiej: do j = n0jm, j1
if (j == k) cycle j_aiajckaiej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiajckaiej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckaiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaiej(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiej
end do a_aiajckaiej
end do j_aiajckaiej
end do k_aiajckaiej
end do c_aiajckaiej
end do e_aiajckaiej
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiajckaiek: do e = n0e, n1e
c_aiajckaiek: do c = n0c, n1c
if (c == e) cycle c_aiajckaiek
k_aiajckaiek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckaiek: do j = n0j, j1
if (j == k) cycle j_aiajckaiek
a0 = max(c + 1, e + 1, n0abd)
a_aiajckaiek: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckaiek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckaiek: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaiek(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiek
end do a_aiajckaiek
end do j_aiajckaiek
end do k_aiajckaiek
end do c_aiajckaiek
end do e_aiajckaiek
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajckajei: do e = n0e, n1e
c_aiajckajei: do c = n0c, n1c
if (c == e) cycle c_aiajckajei
k_aiajckajei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckajei: do j = n0jl, j1
if (j == k) cycle j_aiajckajei
a0 = max(c + 1, e + 1, n0abd)
a_aiajckajei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckajei: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckajei(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajei
end do a_aiajckajei
end do j_aiajckajei
end do k_aiajckajei
end do c_aiajckajei
end do e_aiajckajei
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, m == i, l == k
! No equalities independent of the above can hold.
!
e_aiajckakei: do e = n0e, n1e
c_aiajckakei: do c = n0c, n1c
if (c == e) cycle c_aiajckakei
k_aiajckakei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckakei: do j = n0j, j1
if (j == k) cycle j_aiajckakei
a0 = max(c + 1, e + 1, n0abd)
a_aiajckakei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckakei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckakei: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakei(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakei
end do a_aiajckakei
end do j_aiajckakei
end do k_aiajckakei
end do c_aiajckakei
end do e_aiajckakei
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, l == j, m == k
! No equalities independent of the above can hold.
!
e_aiajckajek: do e = n0e, n1e
c_aiajckajek: do c = n0c, n1c
if (c == e) cycle c_aiajckajek
k_aiajckajek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckajek: do j = n0jl, j1
if (j == k) cycle j_aiajckajek
a0 = max(c + 1, e + 1, n0abd)
a_aiajckajek: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckajek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckajek: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckajek(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajek
end do a_aiajckajek
end do j_aiajckajek
end do k_aiajckajek
end do c_aiajckajek
end do e_aiajckajek
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, m == j, l == k
! No equalities independent of the above can hold.
!
e_aiajckakej: do e = n0e, n1e
c_aiajckakej: do c = n0c, n1c
if (c == e) cycle c_aiajckakej
k_aiajckakej: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckakej: do j = n0jm, j1
if (j == k) cycle j_aiajckakej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiajckakej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckakej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckakej: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckakej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakej(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakej
end do a_aiajckakej
end do j_aiajckakej
end do k_aiajckakej
end do c_aiajckakej
end do e_aiajckakej
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajckdiaj: do d = n0d, n1d
c_aiajckdiaj: do c = n0c, n1c
if (c == d) cycle c_aiajckdiaj
k_aiajckdiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckdiaj: do j = n0jm, j1
if (j == k) cycle j_aiajckdiaj
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdiaj: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckdiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdiaj(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdiaj
end do a_aiajckdiaj
end do j_aiajckdiaj
end do k_aiajckdiaj
end do c_aiajckdiaj
end do d_aiajckdiaj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, j
! Equalities: b == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiajckdiak: do d = n0d, n1d
c_aiajckdiak: do c = n0c, n1c
if (c == d) cycle c_aiajckdiak
k_aiajckdiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckdiak: do j = n0j, j1
if (j == k) cycle j_aiajckdiak
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdiak: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdiak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckdiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdiak(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdiak
end do a_aiajckdiak
end do j_aiajckdiak
end do k_aiajckdiak
end do c_aiajckdiak
end do d_aiajckdiak
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajckdjai: do d = n0d, n1d
c_aiajckdjai: do c = n0c, n1c
if (c == d) cycle c_aiajckdjai
k_aiajckdjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckdjai: do j = n0jl, j1
if (j == k) cycle j_aiajckdjai
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdjai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckdjai: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdjai(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdjai
end do a_aiajckdjai
end do j_aiajckdjai
end do k_aiajckdjai
end do c_aiajckdjai
end do d_aiajckdjai
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, j
! Equalities: b == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
d_aiajckdkai: do d = n0d, n1d
c_aiajckdkai: do c = n0c, n1c
if (c == d) cycle c_aiajckdkai
k_aiajckdkai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckdkai: do j = n0j, j1
if (j == k) cycle j_aiajckdkai
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdkai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdkai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckdkai: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdkai(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdkai
end do a_aiajckdkai
end do j_aiajckdkai
end do k_aiajckdkai
end do c_aiajckdkai
end do d_aiajckdkai
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
d_aiajckdjak: do d = n0d, n1d
c_aiajckdjak: do c = n0c, n1c
if (c == d) cycle c_aiajckdjak
k_aiajckdjak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckdjak: do j = n0jl, j1
if (j == k) cycle j_aiajckdjak
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdjak: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdjak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckdjak: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckdjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdjak(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdjak
end do a_aiajckdjak
end do j_aiajckdjak
end do k_aiajckdjak
end do c_aiajckdjak
end do d_aiajckdjak
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == a, m == j, l == k
! No equalities independent of the above can hold.
!
d_aiajckdkaj: do d = n0d, n1d
c_aiajckdkaj: do c = n0c, n1c
if (c == d) cycle c_aiajckdkaj
k_aiajckdkaj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckdkaj: do j = n0jm, j1
if (j == k) cycle j_aiajckdkaj
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdkaj: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdkaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckdkaj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckdkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdkaj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdkaj
end do a_aiajckdkaj
end do j_aiajckdkaj
end do k_aiajckdkaj
end do c_aiajckdkaj
end do d_aiajckdkaj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajckcjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajckcjei: do c = c0, n1cd
if (c == e) cycle c_aiajckcjei
k_aiajckcjei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckcjei: do j = n0jl, j1
if (j == k) cycle j_aiajckcjei
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckcjei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajckcjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckcjei: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckcjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckcjei(a, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckcjei
end do a_aiajckcjei
end do j_aiajckcjei
end do k_aiajckcjei
end do c_aiajckcjei
end do e_aiajckcjei
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k, j
! Equalities: b == a, d == c, m == i, l == k
! No equalities independent of the above can hold.
!
e_aiajckckei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajckckei: do c = c0, n1cd
if (c == e) cycle c_aiajckckei
k_aiajckckei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckckei: do j = n0j, j1
if (j == k) cycle j_aiajckckei
a0 = max(c + 1, n0ab)
a_aiajckckei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajckckei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckckei: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckckei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckckei(a, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckckei
end do a_aiajckckei
end do j_aiajckckei
end do k_aiajckckei
end do c_aiajckckei
end do e_aiajckckei
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == c, l == j, m == k
! No equalities independent of the above can hold.
!
e_aiajckcjek: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajckcjek: do c = c0, n1cd
if (c == e) cycle c_aiajckcjek
k_aiajckcjek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckcjek: do j = n0jl, j1
if (j == k) cycle j_aiajckcjek
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckcjek: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajckcjek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckcjek: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckcjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckcjek(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckcjek
end do a_aiajckcjek
end do j_aiajckcjek
end do k_aiajckcjek
end do c_aiajckcjek
end do e_aiajckcjek
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == c, m == j, l == k
! No equalities independent of the above can hold.
!
e_aiajckckej: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajckckej: do c = c0, n1cd
if (c == e) cycle c_aiajckckej
k_aiajckckej: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckckej: do j = n0jm, j1
if (j == k) cycle j_aiajckckej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckckej: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajckckej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckckej: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckckej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckckej(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckckej
end do a_aiajckckej
end do j_aiajckckej
end do k_aiajckckej
end do c_aiajckckej
end do e_aiajckckej
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajckdicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajckdicj: do c = n0ce, c1
if (c == d) cycle c_aiajckdicj
k_aiajckdicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckdicj: do j = n0jm, j1
if (j == k) cycle j_aiajckdicj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckdicj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckdicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckdicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdicj(a, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdicj
end do a_aiajckdicj
end do j_aiajckdicj
end do k_aiajckdicj
end do c_aiajckdicj
end do d_aiajckdicj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, j
! Equalities: b == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiajckdick: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajckdick: do c = n0ce, c1
if (c == d) cycle c_aiajckdick
k_aiajckdick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckdick: do j = n0j, j1
if (j == k) cycle j_aiajckdick
a0 = max(c + 1, n0ab)
a_aiajckdick: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckdick: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckdick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdick(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdick
end do a_aiajckdick
end do j_aiajckdick
end do k_aiajckdick
end do c_aiajckdick
end do d_aiajckdick
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
d_aiajckdjck: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajckdjck: do c = n0ce, c1
if (c == d) cycle c_aiajckdjck
k_aiajckdjck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckdjck: do j = n0jl, j1
if (j == k) cycle j_aiajckdjck
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckdjck: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdjck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckdjck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckdjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdjck(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdjck
end do a_aiajckdjck
end do j_aiajckdjck
end do k_aiajckdjck
end do c_aiajckdjck
end do d_aiajckdjck
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
d_aiajckdkcj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajckdkcj: do c = n0ce, c1
if (c == d) cycle c_aiajckdkcj
k_aiajckdkcj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckdkcj: do j = n0jm, j1
if (j == k) cycle j_aiajckdkcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckdkcj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdkcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckdkcj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckdkcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckdkcj(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdkcj
end do a_aiajckdkcj
end do j_aiajckdkcj
end do k_aiajckdkcj
end do c_aiajckdkcj
end do d_aiajckdkcj
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaiaj: do c = n0c, n1c
k_aibjckaiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaiaj: do b = b0, n1b
if (b == c) cycle b_aibjckaiaj
j_aibjckaiaj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjckaiaj: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjckaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjckaiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckaiaj
if (i > j .and. j > k) exit i_aibjckaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaiaj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaiaj(a, b, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaiaj(a, b, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaiaj(a, b, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaiaj(a, b, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaiaj
end do a_aibjckaiaj
end do j_aibjckaiaj
end do b_aibjckaiaj
end do k_aibjckaiaj
end do c_aibjckaiaj
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaiak: do c = n0c, n1c
k_aibjckaiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaiak: do b = b0, n1b
if (b == c) cycle b_aibjckaiak
j_aibjckaiak: do j = n0j, n1j
if (j == k) cycle j_aibjckaiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjckaiak: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjckaiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i_aibjckaiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckaiak
if (i > j .and. j > k) exit i_aibjckaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaiak
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaiak(a, b, j, c)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaiak(a, b, j, c)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaiak(a, b, j, c)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaiak(a, b, j, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaiak
end do a_aibjckaiak
end do j_aibjckaiak
end do b_aibjckaiak
end do k_aibjckaiak
end do c_aibjckaiak
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckajai: do c = n0c, n1c
k_aibjckajai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajai: do b = b0, n1b
if (b == c) cycle b_aibjckajai
j_aibjckajai: do j = n0jl, n1jl
if (j == k) cycle j_aibjckajai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjckajai: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjckajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjckajai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckajai
if (i > j .and. j > k) exit i_aibjckajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajai
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckajai(a, b, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckajai(a, b, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckajai(a, b, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajai(a, b, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajai
end do a_aibjckajai
end do j_aibjckajai
end do b_aibjckajai
end do k_aibjckajai
end do c_aibjckajai
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckakai: do c = n0c, n1c
k_aibjckakai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakai: do b = b0, n1b
if (b == c) cycle b_aibjckakai
j_aibjckakai: do j = n0j, n1j
if (j == k) cycle j_aibjckakai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjckakai: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjckakai
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aibjckakai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckakai
if (i > j .and. j > k) exit i_aibjckakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakai
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakai(a, b, j, c)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckakai(a, b, j, c)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckakai(a, b, j, c)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckakai(a, b, j, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakai
end do a_aibjckakai
end do j_aibjckakai
end do b_aibjckakai
end do k_aibjckakai
end do c_aibjckakai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajak: do c = n0c, n1c
k_aibjckajak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajak: do b = b0, n1b
if (b == c) cycle b_aibjckajak
j0 = max(k + 1, n0jl)
j_aibjckajak: do j = j0, n1jl
if (j == k) cycle j_aibjckajak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjckajak: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjckajak
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckajak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajak
if (i > j .and. j > k) exit i_aibjckajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajak
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckajak(a, i, b, c)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajak(a, i, b, c)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckajak(a, i, b, c)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajak(a, i, b, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajak
end do a_aibjckajak
end do j_aibjckajak
end do b_aibjckajak
end do k_aibjckajak
end do c_aibjckajak
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
end subroutine ccjac_32_dav_part3
end module ccjac_block_32_dav_part3
