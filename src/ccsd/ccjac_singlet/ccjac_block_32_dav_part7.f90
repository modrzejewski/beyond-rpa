module ccjac_block_32_dav_part7
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
subroutine ccjac_32_dav_part7(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: i, j, k
integer :: ai, bj, ck, dl, em
integer :: a0, b0, b1, c0, c1, i0, i1, j0, j1
integer :: n0bce, n0bd, n0be, n0cd, n0cde
integer :: n0ce, n0ikl, n0iklm, n0ikm, n0il
integer :: n0im, n0jkl, n0jklm, n0jkm, n0jl
integer :: n0jm, n0kl, n0km
integer :: n1bce, n1bd, n1be, n1cd, n1cde
integer :: n1ce, n1ikl, n1iklm, n1ikm, n1il
integer :: n1im, n1jkl, n1jklm, n1jkm, n1jl
integer :: n1jm, n1kl, n1km
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
n0bce = max(n0b, n0c, n0e)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
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
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1bce = min(n1b, n1c, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
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
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjcibiej: do e = n0e, n1e
c_aibjcibiej: do c = n0c, n1c
if (c == e) cycle c_aibjcibiej
b0 = max(c + 1, e + 1, n0bd)
b_aibjcibiej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcibiej
j_aibjcibiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibiej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibiej
i_aibjcibiej: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcibiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibiej(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibiej
end do a_aibjcibiej
end do j_aibjcibiej
end do b_aibjcibiej
end do c_aibjcibiej
end do e_aibjcibiej
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcibjei: do e = n0e, n1e
c_aibjcibjei: do c = n0c, n1c
if (c == e) cycle c_aibjcibjei
b0 = max(c + 1, e + 1, n0bd)
b_aibjcibjei: do b = b0, n1bd
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibjei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibjei
end do a_aibjcibjei
end do j_aibjcibjei
end do b_aibjcibjei
end do c_aibjcibjei
end do e_aibjcibjei
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjcjbjei: do e = n0e, n1e
c_aibjcjbjei: do c = n0c, n1c
if (c == e) cycle c_aibjcjbjei
b0 = max(c + 1, e + 1, n0bd)
b_aibjcjbjei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcjbjei
j_aibjcjbjei: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbjei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjbjei
i_aibjcjbjei: do i = n0im, n1im
if (i == j) cycle i_aibjcjbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbjei(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbjei
end do a_aibjcjbjei
end do j_aibjcjbjei
end do b_aibjcjbjei
end do c_aibjcjbjei
end do e_aibjcjbjei
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: j, i
! Equalities: d == b, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjbjej: do e = n0e, n1e
c_aibjcjbjej: do c = n0c, n1c
if (c == e) cycle c_aibjcjbjej
b0 = max(c + 1, e + 1, n0bd)
b_aibjcjbjej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcjbjej
j_aibjcjbjej: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbjej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjbjej
i_aibjcjbjej: do i = n0i, n1i
if (i == j) cycle i_aibjcjbjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbjej(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbjej
end do a_aibjcjbjej
end do j_aibjcjbjej
end do b_aibjcjbjej
end do c_aibjcjbjej
end do e_aibjcjbjej
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdibj: do d = n0d, n1d
k_aibjbkdibj: do k = n0k, n1k
b1 = min(d - 1, n1bce)
b_aibjbkdibj: do b = n0bce, b1
if (b == d) cycle b_aibjbkdibj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkdibj: do j = j0, n1jm
if (j == k) cycle j_aibjbkdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdibj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdibj
i1 = min(j - 1, n1il)
i_aibjbkdibj: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdibj(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdibj
end do a_aibjbkdibj
end do j_aibjbkdibj
end do b_aibjbkdibj
end do k_aibjbkdibj
end do d_aibjbkdibj
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k, j
! Equalities: c == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjbkdibk: do d = n0d, n1d
k_aibjbkdibk: do k = n0km, n1km
b1 = min(d - 1, n1bce)
b_aibjbkdibk: do b = n0bce, b1
if (b == d) cycle b_aibjbkdibk
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkdibk: do j = j0, n1j
if (j == k) cycle j_aibjbkdibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdibk: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdibk
i1 = min(j - 1, n1il)
i_aibjbkdibk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdibk(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdibk
end do a_aibjbkdibk
end do j_aibjbkdibk
end do b_aibjbkdibk
end do k_aibjbkdibk
end do d_aibjbkdibk
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdjbi: do d = n0d, n1d
k_aibjbkdjbi: do k = n0k, n1k
b1 = min(d - 1, n1bce)
b_aibjbkdjbi: do b = n0bce, b1
if (b == d) cycle b_aibjbkdjbi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkdjbi: do j = j0, n1jl
if (j == k) cycle j_aibjbkdjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdjbi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdjbi
i1 = min(j - 1, n1im)
i_aibjbkdjbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkdjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdjbi(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdjbi
end do a_aibjbkdjbi
end do j_aibjbkdjbi
end do b_aibjbkdjbi
end do k_aibjbkdjbi
end do d_aibjbkdjbi
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k, j
! Equalities: c == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdkbi: do d = n0d, n1d
k_aibjbkdkbi: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(d - 1, n1bce)
b_aibjbkdkbi: do b = n0bce, b1
if (b == d) cycle b_aibjbkdkbi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkdkbi: do j = j0, n1j
if (j == k) cycle j_aibjbkdkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdkbi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdkbi
i1 = min(j - 1, n1im)
i_aibjbkdkbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkdkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdkbi(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdkbi
end do a_aibjbkdkbi
end do j_aibjbkdkbi
end do b_aibjbkdkbi
end do k_aibjbkdkbi
end do d_aibjbkdkbi
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, k, i
! Equalities: c == b, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjbkdjbk: do d = n0d, n1d
k_aibjbkdjbk: do k = n0km, n1km
b1 = min(d - 1, n1bce)
b_aibjbkdjbk: do b = n0bce, b1
if (b == d) cycle b_aibjbkdjbk
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkdjbk: do j = j0, n1jl
if (j == k) cycle j_aibjbkdjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdjbk: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdjbk
i1 = min(j - 1, n1i)
i_aibjbkdjbk: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkdjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdjbk(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdjbk
end do a_aibjbkdjbk
end do j_aibjbkdjbk
end do b_aibjbkdjbk
end do k_aibjbkdjbk
end do d_aibjbkdjbk
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, k, i
! Equalities: c == b, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdkbj: do d = n0d, n1d
k_aibjbkdkbj: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(d - 1, n1bce)
b_aibjbkdkbj: do b = n0bce, b1
if (b == d) cycle b_aibjbkdkbj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkdkbj: do j = j0, n1jm
if (j == k) cycle j_aibjbkdkbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdkbj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdkbj
i1 = min(j - 1, n1i)
i_aibjbkdkbj: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkdkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdkbj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdkbj
end do a_aibjbkdkbj
end do j_aibjbkdkbj
end do b_aibjbkdkbj
end do k_aibjbkdkbj
end do d_aibjbkdkbj
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjcidibi: do d = n0d, n1d
c_aibjcidibi: do c = n0c, n1c
if (c == d) cycle c_aibjcidibi
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjcidibi: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjcidibi
j_aibjcidibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidibi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidibi
i_aibjcidibi: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjcidibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcidibi(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidibi
end do a_aibjcidibi
end do j_aibjcidibi
end do b_aibjcidibi
end do c_aibjcidibi
end do d_aibjcidibi
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidibj: do d = n0d, n1d
c_aibjcidibj: do c = n0c, n1c
if (c == d) cycle c_aibjcidibj
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjcidibj: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjcidibj
j_aibjcidibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidibj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidibj
i_aibjcidibj: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcidibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcidibj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidibj
end do a_aibjcidibj
end do j_aibjcidibj
end do b_aibjcidibj
end do c_aibjcidibj
end do d_aibjcidibj
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdibj: do d = n0d, n1d
c_aibjcjdibj: do c = n0c, n1c
if (c == d) cycle c_aibjcjdibj
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjcjdibj: do b = b0, b1
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjdibj(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdibj
end do a_aibjcjdibj
end do j_aibjcjdibj
end do b_aibjcjdibj
end do c_aibjcjdibj
end do d_aibjcjdibj
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjbi: do d = n0d, n1d
c_aibjcidjbi: do c = n0c, n1c
if (c == d) cycle c_aibjcidjbi
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjcidjbi: do b = b0, b1
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcidjbi(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidjbi
end do a_aibjcidjbi
end do j_aibjcidjbi
end do b_aibjcidjbi
end do c_aibjcidjbi
end do d_aibjcidjbi
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: j, i
! Equalities: e == b, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdjbj: do d = n0d, n1d
c_aibjcjdjbj: do c = n0c, n1c
if (c == d) cycle c_aibjcjdjbj
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjcjdjbj: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjcjdjbj
j_aibjcjdjbj: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdjbj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdjbj
i_aibjcjdjbj: do i = n0i, n1i
if (i == j) cycle i_aibjcjdjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjdjbj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdjbj
end do a_aibjcjdjbj
end do j_aibjcjdjbj
end do b_aibjcjdjbj
end do c_aibjcjdjbj
end do d_aibjcjdjbj
!
! Elementary loop  16
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckcicj: do c = n0cde, n1cde
k_aibjckcicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcicj: do b = b0, n1b
if (b == c) cycle b_aibjckcicj
j_aibjckcicj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckcicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckcicj
i0 = max(j + 1, n0il)
i_aibjckcicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckcicj
if (i > j .and. j > k) exit i_aibjckcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckcicj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckcicj(a, b, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckcicj(a, b, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckcicj(a, b, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckcicj(a, b, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcicj
end do a_aibjckcicj
end do j_aibjckcicj
end do b_aibjckcicj
end do k_aibjckcicj
end do c_aibjckcicj
!
! Elementary loop  17
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k, j
! Equalities: d == c, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckcick: do c = n0cde, n1cde
k_aibjckcick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcick: do b = b0, n1b
if (b == c) cycle b_aibjckcick
j_aibjckcick: do j = n0j, n1j
if (j == k) cycle j_aibjckcick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcick: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckcick
i0 = max(k + 1, n0il)
i_aibjckcick: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckcick
if (i > j .and. j > k) exit i_aibjckcick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckcick
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckcick(a, b, j, c)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckcick(a, b, j, c)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckcick(a, b, j, c)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckcick(a, b, j, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcick
end do a_aibjckcick
end do j_aibjckcick
end do b_aibjckcick
end do k_aibjckcick
end do c_aibjckcick
!
! Elementary loop  18
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckcjci: do c = n0cde, n1cde
k_aibjckcjci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjci: do b = b0, n1b
if (b == c) cycle b_aibjckcjci
j_aibjckcjci: do j = n0jl, n1jl
if (j == k) cycle j_aibjckcjci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckcjci
i1 = min(j - 1, n1im)
i_aibjckcjci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckcjci
if (i > j .and. j > k) exit i_aibjckcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckcjci
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckcjci(a, b, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckcjci(a, b, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckcjci(a, b, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckcjci(a, b, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcjci
end do a_aibjckcjci
end do j_aibjckcjci
end do b_aibjckcjci
end do k_aibjckcjci
end do c_aibjckcjci
!
! Elementary loop  19
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k, j
! Equalities: d == c, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckckci: do c = n0cde, n1cde
k_aibjckckci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckci: do b = b0, n1b
if (b == c) cycle b_aibjckckci
j_aibjckckci: do j = n0j, n1j
if (j == k) cycle j_aibjckckci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckckci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckckci
i1 = min(k - 1, n1im)
i_aibjckckci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckckci
if (i > j .and. j > k) exit i_aibjckckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckckci
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckckci(a, b, j, c)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckckci(a, b, j, c)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckckci(a, b, j, c)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckckci(a, b, j, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckckci
end do a_aibjckckci
end do j_aibjckckci
end do b_aibjckckci
end do k_aibjckckci
end do c_aibjckckci
!
! Elementary loop  20
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, k, i
! Equalities: d == c, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckcjck: do c = n0cde, n1cde
k_aibjckcjck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjck: do b = b0, n1b
if (b == c) cycle b_aibjckcjck
j0 = max(k + 1, n0jl)
j_aibjckcjck: do j = j0, n1jl
if (j == k) cycle j_aibjckcjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcjck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckcjck
i_aibjckcjck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckcjck
if (i > j .and. j > k) exit i_aibjckcjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckcjck
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckcjck(a, i, b, c)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckcjck(a, i, b, c)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckcjck(a, i, b, c)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckcjck(a, i, b, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcjck
end do a_aibjckcjck
end do j_aibjckcjck
end do b_aibjckcjck
end do k_aibjckcjck
end do c_aibjckcjck
!
! Elementary loop  21
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, k, i
! Equalities: d == c, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckckcj: do c = n0cde, n1cde
k_aibjckckcj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckcj: do b = b0, n1b
if (b == c) cycle b_aibjckckcj
j1 = min(k - 1, n1jm)
j_aibjckckcj: do j = n0jm, j1
if (j == k) cycle j_aibjckckcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckckcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckckcj
i_aibjckckcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckckcj
if (i > j .and. j > k) exit i_aibjckckcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckckcj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckckcj(a, i, b, c)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckckcj(a, i, b, c)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckckcj(a, i, b, c)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckckcj(a, i, b, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckckcj
end do a_aibjckckcj
end do j_aibjckckcj
end do b_aibjckckcj
end do k_aibjckckcj
end do c_aibjckckcj
!
! Elementary loop  22
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjciciei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjciciei: do c = c0, n1cd
if (c == e) cycle c_aibjciciei
b0 = max(c + 1, n0b)
b_aibjciciei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjciciei
j_aibjciciei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciciei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjciciei
i_aibjciciei: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjciciei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciciei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciciei
end do a_aibjciciei
end do j_aibjciciei
end do b_aibjciciei
end do c_aibjciciei
end do e_aibjciciei
!
! Elementary loop  23
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjciciej: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjciciej: do c = c0, n1cd
if (c == e) cycle c_aibjciciej
b0 = max(c + 1, n0b)
b_aibjciciej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjciciej
j_aibjciciej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciciej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjciciej
i_aibjciciej: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjciciej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciciej(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciciej
end do a_aibjciciej
end do j_aibjciciej
end do b_aibjciciej
end do c_aibjciciej
end do e_aibjciciej
!
! Elementary loop  24
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjciej: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjcjciej: do c = c0, n1cd
if (c == e) cycle c_aibjcjciej
b0 = max(c + 1, n0b)
b_aibjcjciej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjcjciej
j_aibjcjciej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjciej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjciej
i_aibjcjciej: do i = n0il, n1il
if (i == j) cycle i_aibjcjciej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjciej(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjciej
end do a_aibjcjciej
end do j_aibjcjciej
end do b_aibjcjciej
end do c_aibjcjciej
end do e_aibjcjciej
!
! Elementary loop  25
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcicjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjcicjei: do c = c0, n1cd
if (c == e) cycle c_aibjcicjei
b0 = max(c + 1, n0b)
b_aibjcicjei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjcicjei
j_aibjcicjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicjei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcicjei
i_aibjcicjei: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjcicjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcicjei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicjei
end do a_aibjcicjei
end do j_aibjcicjei
end do b_aibjcicjei
end do c_aibjcicjei
end do e_aibjcicjei
!
! Elementary loop  26
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjcjcjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjcjcjei: do c = c0, n1cd
if (c == e) cycle c_aibjcjcjei
b0 = max(c + 1, n0b)
b_aibjcjcjei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjcjcjei
j_aibjcjcjei: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcjei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjcjei
i_aibjcjcjei: do i = n0im, n1im
if (i == j) cycle i_aibjcjcjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjcjei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcjei
end do a_aibjcjcjei
end do j_aibjcjcjei
end do b_aibjcjcjei
end do c_aibjcjcjei
end do e_aibjcjcjei
!
! Elementary loop  27
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: j, i
! Equalities: d == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjcjej: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjcjcjej: do c = c0, n1cd
if (c == e) cycle c_aibjcjcjej
b0 = max(c + 1, n0b)
b_aibjcjcjej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjcjcjej
j_aibjcjcjej: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcjej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjcjej
i_aibjcjcjej: do i = n0i, n1i
if (i == j) cycle i_aibjcjcjej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjcjej(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcjej
end do a_aibjcjcjej
end do j_aibjcjcjej
end do b_aibjcjcjej
end do c_aibjcjcjej
end do e_aibjcjcjej
!
! Elementary loop  28
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjcidici: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjcidici: do c = n0ce, c1
if (c == d) cycle c_aibjcidici
b0 = max(c + 1, n0b)
b_aibjcidici: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidici
j_aibjcidici: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidici: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidici
i_aibjcidici: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjcidici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcidici(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidici
end do a_aibjcidici
end do j_aibjcidici
end do b_aibjcidici
end do c_aibjcidici
end do d_aibjcidici
!
! Elementary loop  29
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjcidicj: do c = n0ce, c1
if (c == d) cycle c_aibjcidicj
b0 = max(c + 1, n0b)
b_aibjcidicj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidicj
j_aibjcidicj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidicj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidicj
i_aibjcidicj: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcidicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcidicj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidicj
end do a_aibjcidicj
end do j_aibjcidicj
end do b_aibjcidicj
end do c_aibjcidicj
end do d_aibjcidicj
!
! Elementary loop  30
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjcjdicj: do c = n0ce, c1
if (c == d) cycle c_aibjcjdicj
b0 = max(c + 1, n0b)
b_aibjcjdicj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcjdicj
j_aibjcjdicj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdicj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdicj
i_aibjcjdicj: do i = n0il, n1il
if (i == j) cycle i_aibjcjdicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjdicj(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdicj
end do a_aibjcjdicj
end do j_aibjcjdicj
end do b_aibjcjdicj
end do c_aibjcjdicj
end do d_aibjcjdicj
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
end subroutine ccjac_32_dav_part7
end module ccjac_block_32_dav_part7
