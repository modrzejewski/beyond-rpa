module ccjac_block_32_dav_part13
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
subroutine ccjac_32_dav_part13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: i, j, k
integer :: ai, bj, ck, dl, em
integer :: a0, b0, b1, i0, i1
integer :: n0ab, n0abd, n0abde, n0bce, n0bd
integer :: n0cde, n0ce, n0ijl, n0ijlm, n0ijm
integer :: n0ik, n0ikl, n0iklm, n0ikm, n0il
integer :: n0ilm, n0im, n0jk, n0jkl, n0jklm
integer :: n0jkm, n0jl, n0jlm, n0jm, n0kl
integer :: n0km
integer :: n1ab, n1abd, n1abde, n1bce, n1bd
integer :: n1cde, n1ce, n1ijl, n1ijlm, n1ijm
integer :: n1ik, n1ikl, n1iklm, n1ikm, n1il
integer :: n1ilm, n1im, n1jk, n1jkl, n1jklm
integer :: n1jkm, n1jl, n1jlm, n1jm, n1kl
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
n0ab = max(n0a, n0b)
n0abd = max(n0a, n0b, n0d)
n0abde = max(n0a, n0b, n0d, n0e)
n0bce = max(n0b, n0c, n0e)
n0bd = max(n0b, n0d)
n0cde = max(n0c, n0d, n0e)
n0ce = max(n0c, n0e)
n0ijl = max(n0i, n0j, n0l)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0iklm = max(n0i, n0k, n0l, n0m)
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
n0km = max(n0k, n0m)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1abde = min(n1a, n1b, n1d, n1e)
n1bce = min(n1b, n1c, n1e)
n1bd = min(n1b, n1d)
n1cde = min(n1c, n1d, n1e)
n1ce = min(n1c, n1e)
n1ijl = min(n1i, n1j, n1l)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1iklm = min(n1i, n1k, n1l, n1m)
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
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i
! Equalities: d == b, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbjcj: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcjbjcj: do b = b0, n1bd
if (b == c) cycle b_aibjcjbjcj
j_aibjcjbjcj: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbjcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbjcj
i_aibjcjbjcj: do i = n0i, n1i
if (i == j) cycle i_aibjcjbjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbjcj(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbjcj
end do a_aibjcjbjcj
end do j_aibjcjbjcj
end do b_aibjcjbjcj
end do c_aibjcjbjcj
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibibkdibi: do d = n0d, n1d
k_aibibkdibi: do k = n0k, n1k
b1 = min(d - 1, n1bce)
b_aibibkdibi: do b = n0bce, b1
if (b == d) cycle b_aibibkdibi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkdibi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibkdibi
i0 = max(k + 1, n0ijlm)
i_aibibkdibi: do i = i0, n1ijlm
if (i == k) cycle i_aibibkdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkdibi(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdibi
end do a_aibibkdibi
end do b_aibibkdibi
end do k_aibibkdibi
end do d_aibibkdibi
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibibkdibk: do d = n0d, n1d
k_aibibkdibk: do k = n0km, n1km
b1 = min(d - 1, n1bce)
b_aibibkdibk: do b = n0bce, b1
if (b == d) cycle b_aibibkdibk
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkdibk: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibkdibk
i0 = max(k + 1, n0ijl)
i_aibibkdibk: do i = i0, n1ijl
if (i == k) cycle i_aibibkdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkdibk(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdibk
end do a_aibibkdibk
end do b_aibibkdibk
end do k_aibibkdibk
end do d_aibibkdibk
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibibkdkbi: do d = n0d, n1d
k_aibibkdkbi: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(d - 1, n1bce)
b_aibibkdkbi: do b = n0bce, b1
if (b == d) cycle b_aibibkdkbi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkdkbi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibkdkbi
i0 = max(k + 1, n0ijm)
i_aibibkdkbi: do i = i0, n1ijm
if (i == k) cycle i_aibibkdkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkdkbi(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdkbi
end do a_aibibkdkbi
end do b_aibibkdkbi
end do k_aibibkdkbi
end do d_aibibkdkbi
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjbidibi: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibjbidibi: do b = n0bce, b1
if (b == d) cycle b_aibjbidibi
j_aibjbidibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidibi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidibi
i1 = min(j - 1, n1iklm)
i_aibjbidibi: do i = n0iklm, i1
if (i == j) cycle i_aibjbidibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbidibi(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidibi
end do a_aibjbidibi
end do j_aibjbidibi
end do b_aibjbidibi
end do d_aibjbidibi
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbidibj: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibjbidibj: do b = n0bce, b1
if (b == d) cycle b_aibjbidibj
j_aibjbidibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidibj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidibj
i1 = min(j - 1, n1ikl)
i_aibjbidibj: do i = n0ikl, i1
if (i == j) cycle i_aibjbidibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbidibj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidibj
end do a_aibjbidibj
end do j_aibjbidibj
end do b_aibjbidibj
end do d_aibjbidibj
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidjbi: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibjbidjbi: do b = n0bce, b1
if (b == d) cycle b_aibjbidjbi
j_aibjbidjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidjbi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidjbi
i1 = min(j - 1, n1ikm)
i_aibjbidjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjbidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbidjbi(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidjbi
end do a_aibjbidjbi
end do j_aibjbidjbi
end do b_aibjbidjbi
end do d_aibjbidjbi
!
! Elementary loop  8
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjcicici: do c = n0cde, n1cde
b0 = max(c + 1, n0b)
b_aibjcicici: do b = b0, n1b
if (b == c) cycle b_aibjcicici
j_aibjcicici: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicici: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicici
i_aibjcicici: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjcicici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcicici(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicici
end do a_aibjcicici
end do j_aibjcicici
end do b_aibjcicici
end do c_aibjcicici
!
! Elementary loop  9
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcicicj: do c = n0cde, n1cde
b0 = max(c + 1, n0b)
b_aibjcicicj: do b = b0, n1b
if (b == c) cycle b_aibjcicicj
j_aibjcicicj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicicj
i0 = max(j + 1, n0ikl)
i_aibjcicicj: do i = i0, n1ikl
if (i == j) cycle i_aibjcicicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcicicj(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicicj
end do a_aibjcicicj
end do j_aibjcicicj
end do b_aibjcicicj
end do c_aibjcicicj
!
! Elementary loop  10
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjcicj: do c = n0cde, n1cde
b0 = max(c + 1, n0b)
b_aibjcjcicj: do b = b0, n1b
if (b == c) cycle b_aibjcjcicj
j_aibjcjcicj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcicj
i0 = max(j + 1, n0il)
i_aibjcjcicj: do i = i0, n1il
if (i == j) cycle i_aibjcjcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjcicj(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcicj
end do a_aibjcjcicj
end do j_aibjcjcicj
end do b_aibjcjcicj
end do c_aibjcjcicj
!
! Elementary loop  11
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicjci: do c = n0cde, n1cde
b0 = max(c + 1, n0b)
b_aibjcicjci: do b = b0, n1b
if (b == c) cycle b_aibjcicjci
j_aibjcicjci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicjci
i1 = min(j - 1, n1ikm)
i_aibjcicjci: do i = n0ikm, i1
if (i == j) cycle i_aibjcicjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcicjci(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicjci
end do a_aibjcicjci
end do j_aibjcicjci
end do b_aibjcicjci
end do c_aibjcicjci
!
! Elementary loop  12
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjcjci: do c = n0cde, n1cde
b0 = max(c + 1, n0b)
b_aibjcjcjci: do b = b0, n1b
if (b == c) cycle b_aibjcjcjci
j_aibjcjcjci: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcjci
i1 = min(j - 1, n1im)
i_aibjcjcjci: do i = n0im, i1
if (i == j) cycle i_aibjcjcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjcjci(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcjci
end do a_aibjcjcjci
end do j_aibjcjcjci
end do b_aibjcjcjci
end do c_aibjcjcjci
!
! Elementary loop  13
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i
! Equalities: d == c, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjcjcj: do c = n0cde, n1cde
b0 = max(c + 1, n0b)
b_aibjcjcjcj: do b = b0, n1b
if (b == c) cycle b_aibjcjcjcj
j_aibjcjcjcj: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcjcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcjcj
i_aibjcjcjcj: do i = n0i, n1i
if (i == j) cycle i_aibjcjcjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjcjcj(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcjcj
end do a_aibjcjcjcj
end do j_aibjcjcjcj
end do b_aibjcjcjcj
end do c_aibjcjcjcj
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajciaiai: do c = n0c, n1c
j_aiajciaiai: do j = n0j, n1j
a0 = max(c + 1, n0abde)
a_aiajciaiai: do a = a0, n1abde
if (a == c) cycle a_aiajciaiai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0iklm)
i_aiajciaiai: do i = i0, n1iklm
if (i == j) cycle i_aiajciaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciaiai(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaiai
end do a_aiajciaiai
end do j_aiajciaiai
end do c_aiajciaiai
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajciaiaj: do c = n0c, n1c
j_aiajciaiaj: do j = n0jm, n1jm
a0 = max(c + 1, n0abde)
a_aiajciaiaj: do a = a0, n1abde
if (a == c) cycle a_aiajciaiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajciaiaj: do i = i0, n1ikl
if (i == j) cycle i_aiajciaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciaiaj(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaiaj
end do a_aiajciaiaj
end do j_aiajciaiaj
end do c_aiajciaiaj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjaiai: do c = n0c, n1c
j_aiajcjaiai: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiajcjaiai: do a = a0, n1abde
if (a == c) cycle a_aiajcjaiai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ilm)
i_aiajcjaiai: do i = i0, n1ilm
if (i == j) cycle i_aiajcjaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjaiai(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaiai
end do a_aiajcjaiai
end do j_aiajcjaiai
end do c_aiajcjaiai
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjaiaj: do c = n0c, n1c
j_aiajcjaiaj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiajcjaiaj: do a = a0, n1abde
if (a == c) cycle a_aiajcjaiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjaiaj: do i = i0, n1il
if (i == j) cycle i_aiajcjaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjaiaj(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaiaj
end do a_aiajcjaiaj
end do j_aiajcjaiaj
end do c_aiajcjaiaj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajciajaj: do c = n0c, n1c
j_aiajciajaj: do j = n0jlm, n1jlm
a0 = max(c + 1, n0abde)
a_aiajciajaj: do a = a0, n1abde
if (a == c) cycle a_aiajciajaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciajaj: do i = i0, n1ik
if (i == j) cycle i_aiajciajaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciajaj(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajaj
end do a_aiajciajaj
end do j_aiajciajaj
end do c_aiajciajaj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == a, e == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjajaj: do c = n0c, n1c
j_aiajcjajaj: do j = n0jklm, n1jklm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiajcjajaj: do a = a0, n1abde
if (a == c) cycle a_aiajcjajaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjajaj: do i = i0, n1i
if (i == j) cycle i_aiajcjajaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjajaj(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjajaj
end do a_aiajcjajaj
end do j_aiajcjajaj
end do c_aiajcjajaj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajciaici: do c = n0ce, n1ce
j_aiajciaici: do j = n0j, n1j
a0 = max(c + 1, n0abd)
a_aiajciaici: do a = a0, n1abd
if (a == c) cycle a_aiajciaici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0iklm)
i_aiajciaici: do i = i0, n1iklm
if (i == j) cycle i_aiajciaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciaici(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaici
end do a_aiajciaici
end do j_aiajciaici
end do c_aiajciaici
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajciaicj: do c = n0ce, n1ce
j_aiajciaicj: do j = n0jm, n1jm
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajciaicj: do a = a0, n1abd
if (a == c) cycle a_aiajciaicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajciaicj: do i = i0, n1ikl
if (i == j) cycle i_aiajciaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciaicj(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaicj
end do a_aiajciaicj
end do j_aiajciaicj
end do c_aiajciaicj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjaici: do c = n0ce, n1ce
j_aiajcjaici: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjaici: do a = a0, n1abd
if (a == c) cycle a_aiajcjaici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ilm)
i_aiajcjaici: do i = i0, n1ilm
if (i == j) cycle i_aiajcjaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjaici(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaici
end do a_aiajcjaici
end do j_aiajcjaici
end do c_aiajcjaici
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjaicj: do c = n0ce, n1ce
j_aiajcjaicj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjaicj: do a = a0, n1abd
if (a == c) cycle a_aiajcjaicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjaicj: do i = i0, n1il
if (i == j) cycle i_aiajcjaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjaicj(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaicj
end do a_aiajcjaicj
end do j_aiajcjaicj
end do c_aiajcjaicj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciajci: do c = n0ce, n1ce
j_aiajciajci: do j = n0jl, n1jl
a0 = max(c + 1, n0abd)
a_aiajciajci: do a = a0, n1abd
if (a == c) cycle a_aiajciajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajciajci: do i = i0, n1ikm
if (i == j) cycle i_aiajciajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciajci(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajci
end do a_aiajciajci
end do j_aiajciajci
end do c_aiajciajci
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajciajcj: do c = n0ce, n1ce
j_aiajciajcj: do j = n0jlm, n1jlm
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajciajcj: do a = a0, n1abd
if (a == c) cycle a_aiajciajcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciajcj: do i = i0, n1ik
if (i == j) cycle i_aiajciajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajciajcj(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajcj
end do a_aiajciajcj
end do j_aiajciajcj
end do c_aiajciajcj
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aiajcjajci: do c = n0ce, n1ce
j_aiajcjajci: do j = n0jkl, n1jkl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjajci: do a = a0, n1abd
if (a == c) cycle a_aiajcjajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajcjajci: do i = i0, n1im
if (i == j) cycle i_aiajcjajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjajci(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjajci
end do a_aiajcjajci
end do j_aiajcjajci
end do c_aiajcjajci
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == a, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjajcj: do c = n0ce, n1ce
j_aiajcjajcj: do j = n0jklm, n1jklm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjajcj: do a = a0, n1abd
if (a == c) cycle a_aiajcjajcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjajcj: do i = i0, n1i
if (i == j) cycle i_aiajcjajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjajcj(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjajcj
end do a_aiajcjajcj
end do j_aiajcjajcj
end do c_aiajcjajcj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajcicici: do c = n0cde, n1cde
j_aiajcicici: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajcicici: do a = a0, n1ab
if (a == c) cycle a_aiajcicici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0iklm)
i_aiajcicici: do i = i0, n1iklm
if (i == j) cycle i_aiajcicici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcicici(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicici
end do a_aiajcicici
end do j_aiajcicici
end do c_aiajcicici
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajcicicj: do c = n0cde, n1cde
j_aiajcicicj: do j = n0jm, n1jm
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcicicj: do a = a0, n1ab
if (a == c) cycle a_aiajcicicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajcicicj: do i = i0, n1ikl
if (i == j) cycle i_aiajcicicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcicicj(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicicj
end do a_aiajcicicj
end do j_aiajcicicj
end do c_aiajcicicj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjcicj: do c = n0cde, n1cde
j_aiajcjcicj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjcicj: do a = a0, n1ab
if (a == c) cycle a_aiajcjcicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjcicj: do i = i0, n1il
if (i == j) cycle i_aiajcjcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjcicj(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjcicj
end do a_aiajcjcicj
end do j_aiajcjcicj
end do c_aiajcjcicj
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
end subroutine ccjac_32_dav_part13
end module ccjac_block_32_dav_part13
