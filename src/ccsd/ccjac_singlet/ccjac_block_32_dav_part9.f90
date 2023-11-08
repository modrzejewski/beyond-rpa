module ccjac_block_32_dav_part9
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
subroutine ccjac_32_dav_part9(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a, c, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, a1, c0, c1, i0, i1, j1
integer :: n0ab, n0abd, n0abe, n0cd, n0cde
integer :: n0ce, n0ikl, n0iklm, n0ikm, n0il
integer :: n0im, n0jkl, n0jklm, n0jkm, n0jl
integer :: n0jlm, n0jm, n0kl, n0klm, n0km
integer :: n1ab, n1abd, n1abe, n1cd, n1cde
integer :: n1ce, n1ikl, n1iklm, n1ikm, n1il
integer :: n1im, n1jkl, n1jklm, n1jkm, n1jl
integer :: n1jlm, n1jm, n1kl, n1klm, n1km
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
n0cd = max(n0c, n0d)
n0cde = max(n0c, n0d, n0e)
n0ce = max(n0c, n0e)
n0ikl = max(n0i, n0k, n0l)
n0iklm = max(n0i, n0k, n0l, n0m)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jkl = max(n0j, n0k, n0l)
n0jklm = max(n0j, n0k, n0l, n0m)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1abe = min(n1a, n1b, n1e)
n1cd = min(n1c, n1d)
n1cde = min(n1c, n1d, n1e)
n1ce = min(n1c, n1e)
n1ikl = min(n1i, n1k, n1l)
n1iklm = min(n1i, n1k, n1l, n1m)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jkl = min(n1j, n1k, n1l)
n1jklm = min(n1j, n1k, n1l, n1m)
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
! Free virtual indices: a, c
! Free occupied indices: j, i, m
! Equalities: b == a, d == a, e == c, k == j, l == j
! No equalities independent of the above can hold.
!
m_aiajcjajcm: do m = n0m, n1m
c_aiajcjajcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajcjajcm: do j = n0jkl, n1jkl
if (j == m) cycle j_aiajcjajcm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjajcm: do a = a0, n1abd
if (a == c) cycle a_aiajcjajcm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjajcm: do i = i0, n1i
if (i == j .or. i == m) cycle i_aiajcjajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjajcm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjajcm
end do a_aiajcjajcm
end do j_aiajcjajcm
end do c_aiajcjajcm
end do m_aiajcjajcm
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, e == c, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajckajcj: do c = n0ce, n1ce
k_aiajckajcj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jlm)
j_aiajckajcj: do j = n0jlm, j1
if (j == k) cycle j_aiajckajcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckajcj: do a = a0, n1abd
if (a == c) cycle a_aiajckajcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckajcj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckajcj(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajcj
end do a_aiajckajcj
end do j_aiajckajcj
end do k_aiajckajcj
end do c_aiajckajcj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aiajckajck: do c = n0ce, n1ce
k_aiajckajck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckajck: do j = n0jl, j1
if (j == k) cycle j_aiajckajck
a0 = max(c + 1, n0abd)
a_aiajckajck: do a = a0, n1abd
if (a == c) cycle a_aiajckajck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckajck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckajck(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajck
end do a_aiajckajck
end do j_aiajckajck
end do k_aiajckajck
end do c_aiajckajck
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, e == c, k == j, m == j
! No equalities independent of the above can hold.
!
l_aiajcjalcj: do l = n0l, n1l
c_aiajcjalcj: do c = n0ce, n1ce
j_aiajcjalcj: do j = n0jkm, n1jkm
if (j == l) cycle j_aiajcjalcj
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjalcj: do a = a0, n1abd
if (a == c) cycle a_aiajcjalcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjalcj: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajcjalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjalcj(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjalcj
end do a_aiajcjalcj
end do j_aiajcjalcj
end do c_aiajcjalcj
end do l_aiajcjalcj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aiajckakcj: do c = n0ce, n1ce
k_aiajckakcj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckakcj: do j = n0jm, j1
if (j == k) cycle j_aiajckakcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckakcj: do a = a0, n1abd
if (a == c) cycle a_aiajckakcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckakcj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckakcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakcj(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakcj
end do a_aiajckakcj
end do j_aiajckakcj
end do k_aiajckakcj
end do c_aiajckakcj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == a, e == c, l == k, m == k
! No equalities independent of the above can hold.
!
c_aiajckakck: do c = n0ce, n1ce
k_aiajckakck: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckakck: do j = n0j, j1
if (j == k) cycle j_aiajckakck
a0 = max(c + 1, n0abd)
a_aiajckakck: do a = a0, n1abd
if (a == c) cycle a_aiajckakck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckakck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckakck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakck(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakck
end do a_aiajckakck
end do j_aiajckakck
end do k_aiajckakck
end do c_aiajckakck
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiajciaiei: do e = n0e, n1e
c_aiajciaiei: do c = n0c, n1c
if (c == e) cycle c_aiajciaiei
j_aiajciaiei: do j = n0j, n1j
a0 = max(c + 1, e + 1, n0abd)
a_aiajciaiei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajciaiei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0iklm)
i_aiajciaiei: do i = i0, n1iklm
if (i == j) cycle i_aiajciaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciaiei(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaiei
end do a_aiajciaiei
end do j_aiajciaiei
end do c_aiajciaiei
end do e_aiajciaiei
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajciaiej: do e = n0e, n1e
c_aiajciaiej: do c = n0c, n1c
if (c == e) cycle c_aiajciaiej
j_aiajciaiej: do j = n0jm, n1jm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiajciaiej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajciaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajciaiej: do i = i0, n1ikl
if (i == j) cycle i_aiajciaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciaiej(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaiej
end do a_aiajciaiej
end do j_aiajciaiej
end do c_aiajciaiej
end do e_aiajciaiej
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjaiej: do e = n0e, n1e
c_aiajcjaiej: do c = n0c, n1c
if (c == e) cycle c_aiajcjaiej
j_aiajcjaiej: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiajcjaiej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajcjaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjaiej: do i = i0, n1il
if (i == j) cycle i_aiajcjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjaiej(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaiej
end do a_aiajcjaiej
end do j_aiajcjaiej
end do c_aiajcjaiej
end do e_aiajcjaiej
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajciajei: do e = n0e, n1e
c_aiajciajei: do c = n0c, n1c
if (c == e) cycle c_aiajciajei
j_aiajciajei: do j = n0jl, n1jl
a0 = max(c + 1, e + 1, n0abd)
a_aiajciajei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajciajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajciajei: do i = i0, n1ikm
if (i == j) cycle i_aiajciajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciajei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajei
end do a_aiajciajei
end do j_aiajciajei
end do c_aiajciajei
end do e_aiajciajei
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aiajcjajei: do e = n0e, n1e
c_aiajcjajei: do c = n0c, n1c
if (c == e) cycle c_aiajcjajei
j_aiajcjajei: do j = n0jkl, n1jkl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiajcjajei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajcjajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajcjajei: do i = i0, n1im
if (i == j) cycle i_aiajcjajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjajei(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjajei
end do a_aiajcjajei
end do j_aiajcjajei
end do c_aiajcjajei
end do e_aiajcjajei
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, i
! Equalities: b == a, d == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjajej: do e = n0e, n1e
c_aiajcjajej: do c = n0c, n1c
if (c == e) cycle c_aiajcjajej
j_aiajcjajej: do j = n0jklm, n1jklm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiajcjajej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajcjajej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjajej: do i = i0, n1i
if (i == j) cycle i_aiajcjajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjajej(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjajej
end do a_aiajcjajej
end do j_aiajcjajej
end do c_aiajcjajej
end do e_aiajcjajej
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiajcidiai: do d = n0d, n1d
c_aiajcidiai: do c = n0c, n1c
if (c == d) cycle c_aiajcidiai
j_aiajcidiai: do j = n0j, n1j
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcidiai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcidiai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0iklm)
i_aiajcidiai: do i = i0, n1iklm
if (i == j) cycle i_aiajcidiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcidiai(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidiai
end do a_aiajcidiai
end do j_aiajcidiai
end do c_aiajcidiai
end do d_aiajcidiai
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajcidiaj: do d = n0d, n1d
c_aiajcidiaj: do c = n0c, n1c
if (c == d) cycle c_aiajcidiaj
j_aiajcidiaj: do j = n0jm, n1jm
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcidiaj: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcidiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajcidiaj: do i = i0, n1ikl
if (i == j) cycle i_aiajcidiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcidiaj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidiaj
end do a_aiajcidiaj
end do j_aiajcidiaj
end do c_aiajcidiaj
end do d_aiajcidiaj
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdiaj: do d = n0d, n1d
c_aiajcjdiaj: do c = n0c, n1c
if (c == d) cycle c_aiajcjdiaj
j_aiajcjdiaj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcjdiaj: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcjdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjdiaj: do i = i0, n1il
if (i == j) cycle i_aiajcjdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjdiaj(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdiaj
end do a_aiajcjdiaj
end do j_aiajcjdiaj
end do c_aiajcjdiaj
end do d_aiajcjdiaj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidjai: do d = n0d, n1d
c_aiajcidjai: do c = n0c, n1c
if (c == d) cycle c_aiajcidjai
j_aiajcidjai: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcidjai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcidjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajcidjai: do i = i0, n1ikm
if (i == j) cycle i_aiajcidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcidjai(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidjai
end do a_aiajcidjai
end do j_aiajcidjai
end do c_aiajcidjai
end do d_aiajcidjai
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aiajcjdjai: do d = n0d, n1d
c_aiajcjdjai: do c = n0c, n1c
if (c == d) cycle c_aiajcjdjai
j_aiajcjdjai: do j = n0jkl, n1jkl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcjdjai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcjdjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajcjdjai: do i = i0, n1im
if (i == j) cycle i_aiajcjdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjdjai(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdjai
end do a_aiajcjdjai
end do j_aiajcjdjai
end do c_aiajcjdjai
end do d_aiajcjdjai
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i
! Equalities: b == a, e == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdjaj: do d = n0d, n1d
c_aiajcjdjaj: do c = n0c, n1c
if (c == d) cycle c_aiajcjdjaj
j_aiajcjdjaj: do j = n0jklm, n1jklm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcjdjaj: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcjdjaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjdjaj: do i = i0, n1i
if (i == j) cycle i_aiajcjdjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjdjaj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdjaj
end do a_aiajcjdjaj
end do j_aiajcjdjaj
end do c_aiajcjdjaj
end do d_aiajcjdjaj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckcicj: do c = n0cde, n1cde
k_aiajckcicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckcicj: do j = n0jm, j1
if (j == k) cycle j_aiajckcicj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckcicj: do a = a0, n1ab
if (a == c) cycle a_aiajckcicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckcicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckcicj(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckcicj
end do a_aiajckcicj
end do j_aiajckcicj
end do k_aiajckcicj
end do c_aiajckcicj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == c, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckcick: do c = n0cde, n1cde
k_aiajckcick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckcick: do j = n0j, j1
if (j == k) cycle j_aiajckcick
a0 = max(c + 1, n0ab)
a_aiajckcick: do a = a0, n1ab
if (a == c) cycle a_aiajckcick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aiajckcick: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckcick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckcick(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckcick
end do a_aiajckcick
end do j_aiajckcick
end do k_aiajckcick
end do c_aiajckcick
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == c, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckckci: do c = n0cde, n1cde
k_aiajckckci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckckci: do j = n0j, j1
if (j == k) cycle j_aiajckckci
a0 = max(c + 1, n0ab)
a_aiajckckci: do a = a0, n1ab
if (a == c) cycle a_aiajckckci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(k - 1, n1im)
i_aiajckckci: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajckckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckckci(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckckci
end do a_aiajckckci
end do j_aiajckckci
end do k_aiajckckci
end do c_aiajckckci
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiajciciei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajciciei: do c = c0, n1cd
if (c == e) cycle c_aiajciciei
j_aiajciciei: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajciciei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajciciei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0iklm)
i_aiajciciei: do i = i0, n1iklm
if (i == j) cycle i_aiajciciei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciciei(a, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciciei
end do a_aiajciciei
end do j_aiajciciei
end do c_aiajciciei
end do e_aiajciciei
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajciciej: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajciciej: do c = c0, n1cd
if (c == e) cycle c_aiajciciej
j_aiajciciej: do j = n0jm, n1jm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajciciej: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajciciej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajciciej: do i = i0, n1ikl
if (i == j) cycle i_aiajciciej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciciej(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciciej
end do a_aiajciciej
end do j_aiajciciej
end do c_aiajciciej
end do e_aiajciciej
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjciej: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajcjciej: do c = c0, n1cd
if (c == e) cycle c_aiajcjciej
j_aiajcjciej: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjciej: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajcjciej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjciej: do i = i0, n1il
if (i == j) cycle i_aiajcjciej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjciej(a, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjciej
end do a_aiajcjciej
end do j_aiajcjciej
end do c_aiajcjciej
end do e_aiajcjciej
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajcicjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajcicjei: do c = c0, n1cd
if (c == e) cycle c_aiajcicjei
j_aiajcicjei: do j = n0jl, n1jl
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcicjei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajcicjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajcicjei: do i = i0, n1ikm
if (i == j) cycle i_aiajcicjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcicjei(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicjei
end do a_aiajcicjei
end do j_aiajcicjei
end do c_aiajcicjei
end do e_aiajcicjei
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aiajcjcjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajcjcjei: do c = c0, n1cd
if (c == e) cycle c_aiajcjcjei
j_aiajcjcjei: do j = n0jkl, n1jkl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjcjei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajcjcjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajcjcjei: do i = i0, n1im
if (i == j) cycle i_aiajcjcjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjcjei(a, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjcjei
end do a_aiajcjcjei
end do j_aiajcjcjei
end do c_aiajcjcjei
end do e_aiajcjcjei
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, i
! Equalities: b == a, d == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjcjej: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajcjcjej: do c = c0, n1cd
if (c == e) cycle c_aiajcjcjej
j_aiajcjcjej: do j = n0jklm, n1jklm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjcjej: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajcjcjej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjcjej: do i = i0, n1i
if (i == j) cycle i_aiajcjcjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjcjej(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjcjej
end do a_aiajcjcjej
end do j_aiajcjcjej
end do c_aiajcjcjej
end do e_aiajcjcjej
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiajcidici: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajcidici: do c = n0ce, c1
if (c == d) cycle c_aiajcidici
j_aiajcidici: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajcidici: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0iklm)
i_aiajcidici: do i = i0, n1iklm
if (i == j) cycle i_aiajcidici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcidici(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidici
end do a_aiajcidici
end do j_aiajcidici
end do c_aiajcidici
end do d_aiajcidici
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajcidicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajcidicj: do c = n0ce, c1
if (c == d) cycle c_aiajcidicj
j_aiajcidicj: do j = n0jm, n1jm
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcidicj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajcidicj: do i = i0, n1ikl
if (i == j) cycle i_aiajcidicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcidicj(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidicj
end do a_aiajcidicj
end do j_aiajcidicj
end do c_aiajcidicj
end do d_aiajcidicj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajcjdicj: do c = n0ce, c1
if (c == d) cycle c_aiajcjdicj
j_aiajcjdicj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjdicj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcjdicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjdicj: do i = i0, n1il
if (i == j) cycle i_aiajcjdicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjdicj(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdicj
end do a_aiajcjdicj
end do j_aiajcjdicj
end do c_aiajcjdicj
end do d_aiajcjdicj
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
end subroutine ccjac_32_dav_part9
end module ccjac_block_32_dav_part9
