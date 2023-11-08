module ccjac_block_32_dav_part8
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
subroutine ccjac_32_dav_part8(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, b0, c1, i0, i1, j0, j1
integer :: n0abd, n0abde, n0ce, n0ik, n0ikl
integer :: n0ikm, n0il, n0ilm, n0im, n0jk
integer :: n0jkl, n0jklm, n0jkm, n0jl, n0jlm
integer :: n0jm, n0kl, n0klm, n0km
integer :: n1abd, n1abde, n1ce, n1ik, n1ikl
integer :: n1ikm, n1il, n1ilm, n1im, n1jk
integer :: n1jkl, n1jklm, n1jkm, n1jl, n1jlm
integer :: n1jm, n1kl, n1klm, n1km
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
n0abd = max(n0a, n0b, n0d)
n0abde = max(n0a, n0b, n0d, n0e)
n0ce = max(n0c, n0e)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0ilm = max(n0i, n0l, n0m)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jklm = max(n0j, n0k, n0l, n0m)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1abd = min(n1a, n1b, n1d)
n1abde = min(n1a, n1b, n1d, n1e)
n1ce = min(n1c, n1e)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1ilm = min(n1i, n1l, n1m)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
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
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjci: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjcidjci: do c = n0ce, c1
if (c == d) cycle c_aibjcidjci
b0 = max(c + 1, n0b)
b_aibjcidjci: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidjci
j_aibjcidjci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidjci: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjci
i_aibjcidjci: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjcidjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcidjci(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidjci
end do a_aibjcidjci
end do j_aibjcidjci
end do b_aibjcidjci
end do c_aibjcidjci
end do d_aibjcidjci
!
! Elementary loop  2
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjcjdjci: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjcjdjci: do c = n0ce, c1
if (c == d) cycle c_aibjcjdjci
b0 = max(c + 1, n0b)
b_aibjcjdjci: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcjdjci
j_aibjcjdjci: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdjci: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdjci
i_aibjcjdjci: do i = n0im, n1im
if (i == j) cycle i_aibjcjdjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjdjci(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdjci
end do a_aibjcjdjci
end do j_aibjcjdjci
end do b_aibjcjdjci
end do c_aibjcjdjci
end do d_aibjcjdjci
!
! Elementary loop  3
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: j, i
! Equalities: e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdjcj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjcjdjcj: do c = n0ce, c1
if (c == d) cycle c_aibjcjdjcj
b0 = max(c + 1, n0b)
b_aibjcjdjcj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcjdjcj
j_aibjcjdjcj: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdjcj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdjcj
i_aibjcjdjcj: do i = n0i, n1i
if (i == j) cycle i_aibjcjdjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjdjcj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdjcj
end do a_aibjcjdjcj
end do j_aibjcjdjcj
end do b_aibjcjdjcj
end do c_aibjcjdjcj
end do d_aibjcjdjcj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiajciaiam: do m = n0m, n1m
c_aiajciaiam: do c = n0c, n1c
j_aiajciaiam: do j = n0j, n1j
if (j == m) cycle j_aiajciaiam
a0 = max(c + 1, n0abde)
a_aiajciaiam: do a = a0, n1abde
if (a == c) cycle a_aiajciaiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0ikl)
i_aiajciaiam: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aiajciaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciaiam(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaiam
end do a_aiajciaiam
end do j_aiajciaiam
end do c_aiajciaiam
end do m_aiajciaiam
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == a, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajckaiai: do c = n0c, n1c
k_aiajckaiai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckaiai: do j = n0j, j1
if (j == k) cycle j_aiajckaiai
a0 = max(c + 1, n0abde)
a_aiajckaiai: do a = a0, n1abde
if (a == c) cycle a_aiajckaiai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ilm)
i_aiajckaiai: do i = i0, n1ilm
if (i == j .or. i == k) cycle i_aiajckaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaiai(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiai
end do a_aiajckaiai
end do j_aiajckaiai
end do k_aiajckaiai
end do c_aiajckaiai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjaiam: do m = n0m, n1m
c_aiajcjaiam: do c = n0c, n1c
j_aiajcjaiam: do j = n0jk, n1jk
if (j == m) cycle j_aiajcjaiam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiajcjaiam: do a = a0, n1abde
if (a == c) cycle a_aiajcjaiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajcjaiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajcjaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjaiam(j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaiam
end do a_aiajcjaiam
end do j_aiajcjaiam
end do c_aiajcjaiam
end do m_aiajcjaiam
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckaiaj: do c = n0c, n1c
k_aiajckaiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckaiaj: do j = n0jm, j1
if (j == k) cycle j_aiajckaiaj
a0 = max(c + 1, n0abde)
a_aiajckaiaj: do a = a0, n1abde
if (a == c) cycle a_aiajckaiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckaiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaiaj(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiaj
end do a_aiajckaiaj
end do j_aiajckaiaj
end do k_aiajckaiaj
end do c_aiajckaiaj
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckaiak: do c = n0c, n1c
k_aiajckaiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckaiak: do j = n0j, j1
if (j == k) cycle j_aiajckaiak
a0 = max(c + 1, n0abde)
a_aiajckaiak: do a = a0, n1abde
if (a == c) cycle a_aiajckaiak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aiajckaiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaiak(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiak
end do a_aiajckaiak
end do j_aiajckaiak
end do k_aiajckaiak
end do c_aiajckaiak
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiajcialai: do l = n0l, n1l
c_aiajcialai: do c = n0c, n1c
j_aiajcialai: do j = n0j, n1j
if (j == l) cycle j_aiajcialai
a0 = max(c + 1, n0abde)
a_aiajcialai: do a = a0, n1abde
if (a == c) cycle a_aiajcialai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ikm)
i1 = min(l - 1, n1ikm)
i_aiajcialai: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajcialai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcialai(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcialai
end do a_aiajcialai
end do j_aiajcialai
end do c_aiajcialai
end do l_aiajcialai
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajciajam: do m = n0m, n1m
c_aiajciajam: do c = n0c, n1c
j0 = max(m + 1, n0jl)
j_aiajciajam: do j = j0, n1jl
if (j == m) cycle j_aiajciajam
a0 = max(c + 1, n0abde)
a_aiajciajam: do a = a0, n1abde
if (a == c) cycle a_aiajciajam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciajam: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajciajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciajam(i, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajam
end do a_aiajciajam
end do j_aiajciajam
end do c_aiajciajam
end do m_aiajciajam
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajcialaj: do l = n0l, n1l
c_aiajcialaj: do c = n0c, n1c
j1 = min(l - 1, n1jm)
j_aiajcialaj: do j = n0jm, j1
if (j == l) cycle j_aiajcialaj
a0 = max(c + 1, n0abde)
a_aiajcialaj: do a = a0, n1abde
if (a == c) cycle a_aiajcialaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcialaj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcialaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcialaj(i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcialaj
end do a_aiajcialaj
end do j_aiajcialaj
end do c_aiajcialaj
end do l_aiajcialaj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjalai: do l = n0l, n1l
c_aiajcjalai: do c = n0c, n1c
j_aiajcjalai: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjalai
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiajcjalai: do a = a0, n1abde
if (a == c) cycle a_aiajcjalai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajcjalai: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajcjalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjalai(j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjalai
end do a_aiajcjalai
end do j_aiajcjalai
end do c_aiajcjalai
end do l_aiajcjalai
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckakai: do c = n0c, n1c
k_aiajckakai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckakai: do j = n0j, j1
if (j == k) cycle j_aiajckakai
a0 = max(c + 1, n0abde)
a_aiajckakai: do a = a0, n1abde
if (a == c) cycle a_aiajckakai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(k - 1, n1im)
i_aiajckakai: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajckakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakai(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakai
end do a_aiajckakai
end do j_aiajckakai
end do k_aiajckakai
end do c_aiajckakai
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, m
! Equalities: b == a, d == a, e == a, k == j, l == j
! No equalities independent of the above can hold.
!
m_aiajcjajam: do m = n0m, n1m
c_aiajcjajam: do c = n0c, n1c
j0 = max(m + 1, n0jkl)
j_aiajcjajam: do j = j0, n1jkl
if (j == m) cycle j_aiajcjajam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiajcjajam: do a = a0, n1abde
if (a == c) cycle a_aiajcjajam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjajam: do i = i0, n1i
if (i == j .or. i == m) cycle i_aiajcjajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjajam(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjajam
end do a_aiajcjajam
end do j_aiajcjajam
end do c_aiajcjajam
end do m_aiajcjajam
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, e == a, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajckajaj: do c = n0c, n1c
k_aiajckajaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jlm)
j_aiajckajaj: do j = n0jlm, j1
if (j == k) cycle j_aiajckajaj
a0 = max(c + 1, n0abde)
a_aiajckajaj: do a = a0, n1abde
if (a == c) cycle a_aiajckajaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckajaj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckajaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckajaj(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajaj
end do a_aiajckajaj
end do j_aiajckajaj
end do k_aiajckajaj
end do c_aiajckajaj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
c_aiajckajak: do c = n0c, n1c
k_aiajckajak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j1 = min(k - 1, n1jl)
j_aiajckajak: do j = j0, j1
if (j == k) cycle j_aiajckajak
a0 = max(c + 1, n0abde)
a_aiajckajak: do a = a0, n1abde
if (a == c) cycle a_aiajckajak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckajak: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckajak(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajak
end do a_aiajckajak
end do j_aiajckajak
end do k_aiajckajak
end do c_aiajckajak
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, e == a, k == j, m == j
! No equalities independent of the above can hold.
!
l_aiajcjalaj: do l = n0l, n1l
c_aiajcjalaj: do c = n0c, n1c
j1 = min(l - 1, n1jkm)
j_aiajcjalaj: do j = n0jkm, j1
if (j == l) cycle j_aiajcjalaj
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiajcjalaj: do a = a0, n1abde
if (a == c) cycle a_aiajcjalaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjalaj: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajcjalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjalaj(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjalaj
end do a_aiajcjalaj
end do j_aiajcjalaj
end do c_aiajcjalaj
end do l_aiajcjalaj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == a, m == j, l == k
! No equalities independent of the above can hold.
!
c_aiajckakaj: do c = n0c, n1c
k_aiajckakaj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckakaj: do j = n0jm, j1
if (j == k) cycle j_aiajckakaj
a0 = max(c + 1, n0abde)
a_aiajckakaj: do a = a0, n1abde
if (a == c) cycle a_aiajckakaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckakaj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckakaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakaj(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakaj
end do a_aiajckakaj
end do j_aiajckakaj
end do k_aiajckakaj
end do c_aiajckakaj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == a, e == a, l == k, m == k
! No equalities independent of the above can hold.
!
c_aiajckakak: do c = n0c, n1c
k_aiajckakak: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckakak: do j = n0j, j1
if (j == k) cycle j_aiajckakak
a0 = max(c + 1, n0abde)
a_aiajckakak: do a = a0, n1abde
if (a == c) cycle a_aiajckakak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckakak: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckakak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakak(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakak
end do a_aiajckakak
end do j_aiajckakak
end do k_aiajckakak
end do c_aiajckakak
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiajciaicm: do m = n0m, n1m
c_aiajciaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajciaicm: do j = n0j, n1j
if (j == m) cycle j_aiajciaicm
a0 = max(c + 1, n0abd)
a_aiajciaicm: do a = a0, n1abd
if (a == c) cycle a_aiajciaicm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajciaicm: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aiajciaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciaicm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaicm
end do a_aiajciaicm
end do j_aiajciaicm
end do c_aiajciaicm
end do m_aiajciaicm
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajckaici: do c = n0ce, n1ce
k_aiajckaici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckaici: do j = n0j, j1
if (j == k) cycle j_aiajckaici
a0 = max(c + 1, n0abd)
a_aiajckaici: do a = a0, n1abd
if (a == c) cycle a_aiajckaici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ilm)
i_aiajckaici: do i = i0, n1ilm
if (i == j .or. i == k) cycle i_aiajckaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaici(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaici
end do a_aiajckaici
end do j_aiajckaici
end do k_aiajckaici
end do c_aiajckaici
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjaicm: do m = n0m, n1m
c_aiajcjaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajcjaicm: do j = n0jk, n1jk
if (j == m) cycle j_aiajcjaicm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjaicm: do a = a0, n1abd
if (a == c) cycle a_aiajcjaicm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjaicm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajcjaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjaicm(a, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaicm
end do a_aiajcjaicm
end do j_aiajcjaicm
end do c_aiajcjaicm
end do m_aiajcjaicm
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckaicj: do c = n0ce, n1ce
k_aiajckaicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckaicj: do j = n0jm, j1
if (j == k) cycle j_aiajckaicj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckaicj: do a = a0, n1abd
if (a == c) cycle a_aiajckaicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckaicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaicj(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaicj
end do a_aiajckaicj
end do j_aiajckaicj
end do k_aiajckaicj
end do c_aiajckaicj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckaick: do c = n0ce, n1ce
k_aiajckaick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckaick: do j = n0j, j1
if (j == k) cycle j_aiajckaick
a0 = max(c + 1, n0abd)
a_aiajckaick: do a = a0, n1abd
if (a == c) cycle a_aiajckaick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckaick: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaick(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaick
end do a_aiajckaick
end do j_aiajckaick
end do k_aiajckaick
end do c_aiajckaick
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiajcialci: do l = n0l, n1l
c_aiajcialci: do c = n0ce, n1ce
j_aiajcialci: do j = n0j, n1j
if (j == l) cycle j_aiajcialci
a0 = max(c + 1, n0abd)
a_aiajcialci: do a = a0, n1abd
if (a == c) cycle a_aiajcialci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajcialci: do i = i0, n1ikm
if (i == j .or. i == l) cycle i_aiajcialci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcialci(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcialci
end do a_aiajcialci
end do j_aiajcialci
end do c_aiajcialci
end do l_aiajcialci
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajciajcm: do m = n0m, n1m
c_aiajciajcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajciajcm: do j = n0jl, n1jl
if (j == m) cycle j_aiajciajcm
a0 = max(c + 1, n0abd)
a_aiajciajcm: do a = a0, n1abd
if (a == c) cycle a_aiajciajcm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciajcm: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajciajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciajcm(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajcm
end do a_aiajciajcm
end do j_aiajciajcm
end do c_aiajciajcm
end do m_aiajciajcm
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajcialcj: do l = n0l, n1l
c_aiajcialcj: do c = n0ce, n1ce
j_aiajcialcj: do j = n0jm, n1jm
if (j == l) cycle j_aiajcialcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcialcj: do a = a0, n1abd
if (a == c) cycle a_aiajcialcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcialcj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcialcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcialcj(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcialcj
end do a_aiajcialcj
end do j_aiajcialcj
end do c_aiajcialcj
end do l_aiajcialcj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajckajci: do c = n0ce, n1ce
k_aiajckajci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckajci: do j = n0jl, j1
if (j == k) cycle j_aiajckajci
a0 = max(c + 1, n0abd)
a_aiajckajci: do a = a0, n1abd
if (a == c) cycle a_aiajckajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckajci: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckajci(a, i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajci
end do a_aiajckajci
end do j_aiajckajci
end do k_aiajckajci
end do c_aiajckajci
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjalci: do l = n0l, n1l
c_aiajcjalci: do c = n0ce, n1ce
j_aiajcjalci: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjalci
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjalci: do a = a0, n1abd
if (a == c) cycle a_aiajcjalci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajcjalci: do i = i0, n1im
if (i == j .or. i == l) cycle i_aiajcjalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjalci(a, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjalci
end do a_aiajcjalci
end do j_aiajcjalci
end do c_aiajcjalci
end do l_aiajcjalci
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckakci: do c = n0ce, n1ce
k_aiajckakci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckakci: do j = n0j, j1
if (j == k) cycle j_aiajckakci
a0 = max(c + 1, n0abd)
a_aiajckakci: do a = a0, n1abd
if (a == c) cycle a_aiajckakci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajckakci: do i = i0, n1im
if (i == j .or. i == k) cycle i_aiajckakci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakci(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakci
end do a_aiajckakci
end do j_aiajckakci
end do k_aiajckakci
end do c_aiajckakci
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
end subroutine ccjac_32_dav_part8
end module ccjac_block_32_dav_part8
