module ccjac_block_32_dav_part10
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
subroutine ccjac_32_dav_part10(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, b0, c1, i0, i1, j0
integer :: n0ab, n0ad, n0ade, n0bc, n0bce
integer :: n0ce, n0ij, n0ijl, n0ijlm, n0ijm
integer :: n0ik, n0ikl, n0iklm, n0ikm, n0il
integer :: n0ilm, n0im, n0jkl, n0jklm, n0jkm
integer :: n0jl, n0jlm, n0jm, n0kl, n0klm
integer :: n0km
integer :: n1ab, n1ad, n1ade, n1bc, n1bce
integer :: n1ce, n1ij, n1ijl, n1ijlm, n1ijm
integer :: n1ik, n1ikl, n1iklm, n1ikm, n1il
integer :: n1ilm, n1im, n1jkl, n1jklm, n1jkm
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
n0ab = max(n0a, n0b)
n0ad = max(n0a, n0d)
n0ade = max(n0a, n0d, n0e)
n0bc = max(n0b, n0c)
n0bce = max(n0b, n0c, n0e)
n0ce = max(n0c, n0e)
n0ij = max(n0i, n0j)
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
n1ad = min(n1a, n1d)
n1ade = min(n1a, n1d, n1e)
n1bc = min(n1b, n1c)
n1bce = min(n1b, n1c, n1e)
n1ce = min(n1c, n1e)
n1ij = min(n1i, n1j)
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
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidjci: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajcidjci: do c = n0ce, c1
if (c == d) cycle c_aiajcidjci
j_aiajcidjci: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcidjci: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidjci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajcidjci: do i = i0, n1ikm
if (i == j) cycle i_aiajcidjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajcidjci(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidjci
end do a_aiajcidjci
end do j_aiajcidjci
end do c_aiajcidjci
end do d_aiajcidjci
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aiajcjdjci: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajcjdjci: do c = n0ce, c1
if (c == d) cycle c_aiajcjdjci
j_aiajcjdjci: do j = n0jkl, n1jkl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjdjci: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcjdjci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i_aiajcjdjci: do i = i0, n1im
if (i == j) cycle i_aiajcjdjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjdjci(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdjci
end do a_aiajcjdjci
end do j_aiajcjdjci
end do c_aiajcjdjci
end do d_aiajcjdjci
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i
! Equalities: b == a, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdjcj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajcjdjcj: do c = n0ce, c1
if (c == d) cycle c_aiajcjdjcj
j_aiajcjdjcj: do j = n0jklm, n1jklm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjdjcj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcjdjcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjdjcj: do i = i0, n1i
if (i == j) cycle i_aiajcjdjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjdjcj(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdjcj
end do a_aiajcjdjcj
end do j_aiajcjdjcj
end do c_aiajcjdjcj
end do d_aiajcjdjcj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, c == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjbkaiak: do k = n0km, n1km
b_aibjbkaiak: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkaiak: do j = j0, n1j
if (j == k) cycle j_aibjbkaiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjbkaiak: do a = a0, n1ade
if (a == b) cycle a_aibjbkaiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i1 = min(j - 1, n1il)
i_aibjbkaiak: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjbkaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkaiak(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkaiak
end do a_aibjbkaiak
end do j_aibjbkaiak
end do b_aibjbkaiak
end do k_aibjbkaiak
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, c == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjbkakai: do k = n0kl, n1kl
b_aibjbkakai: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkakai: do j = j0, n1j
if (j == k) cycle j_aibjbkakai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjbkakai: do a = a0, n1ade
if (a == b) cycle a_aibjbkakai
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, j - 1, n1im)
i_aibjbkakai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkakai(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkakai
end do a_aibjbkakai
end do j_aibjbkakai
end do b_aibjbkakai
end do k_aibjbkakai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: d == a, e == a, c == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjbkajak: do k = n0km, n1km
b_aibjbkajak: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkajak: do j = j0, n1jl
if (j == k) cycle j_aibjbkajak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjbkajak: do a = a0, n1ade
if (a == b) cycle a_aibjbkajak
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkajak: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkajak(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkajak
end do a_aibjbkajak
end do j_aibjbkajak
end do b_aibjbkajak
end do k_aibjbkajak
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjciaiai: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjciaiai: do b = b0, n1b
if (b == c) cycle b_aibjciaiai
j_aibjciaiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjciaiai: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjciaiai
i_aibjciaiai: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjciaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaiai(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaiai
end do a_aibjciaiai
end do j_aibjciaiai
end do b_aibjciaiai
end do c_aibjciaiai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaiaj: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjciaiaj: do b = b0, n1b
if (b == c) cycle b_aibjciaiaj
j_aibjciaiaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjciaiaj: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjciaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aibjciaiaj: do i = i0, n1ikl
if (i == j) cycle i_aibjciaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaiaj(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaiaj
end do a_aibjciaiaj
end do j_aibjciaiaj
end do b_aibjciaiaj
end do c_aibjciaiaj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaiaj: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjcjaiaj: do b = b0, n1b
if (b == c) cycle b_aibjcjaiaj
j_aibjcjaiaj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjcjaiaj: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjcjaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjcjaiaj: do i = i0, n1il
if (i == j) cycle i_aibjcjaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjaiaj(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaiaj
end do a_aibjcjaiaj
end do j_aibjcjaiaj
end do b_aibjcjaiaj
end do c_aibjcjaiaj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajai: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjciajai: do b = b0, n1b
if (b == c) cycle b_aibjciajai
j_aibjciajai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjciajai: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjciajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjciajai: do i = n0ikm, i1
if (i == j) cycle i_aibjciajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciajai(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajai
end do a_aibjciajai
end do j_aibjciajai
end do b_aibjciajai
end do c_aibjciajai
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajai: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjcjajai: do b = b0, n1b
if (b == c) cycle b_aibjcjajai
j_aibjcjajai: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjcjajai: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjcjajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjcjajai: do i = n0im, i1
if (i == j) cycle i_aibjcjajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajai(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajai
end do a_aibjcjajai
end do j_aibjcjajai
end do b_aibjcjajai
end do c_aibjcjajai
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: d == a, e == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjajaj: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjcjajaj: do b = b0, n1b
if (b == c) cycle b_aibjcjajaj
j_aibjcjajaj: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjcjajaj: do a = a0, n1ade
if (a == b .or. a == c) cycle a_aibjcjajaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajaj: do i = n0i, n1i
if (i == j) cycle i_aibjcjajaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajaj(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajaj
end do a_aibjcjajaj
end do j_aibjcjajaj
end do b_aibjcjajaj
end do c_aibjcjajaj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: d == a, c == b, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibibkaibm: do m = n0m, n1m
k_aibibkaibm: do k = n0k, n1k
if (k == m) cycle k_aibibkaibm
b_aibibkaibm: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkaibm: do a = a0, n1ad
if (a == b) cycle a_aibibkaibm
i0 = max(k + 1, n0ijl)
i_aibibkaibm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibibkaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkaibm(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkaibm
end do a_aibibkaibm
end do b_aibibkaibm
end do k_aibibkaibm
end do m_aibibkaibm
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibibkalbi: do l = n0l, n1l
k_aibibkalbi: do k = n0k, n1k
if (k == l) cycle k_aibibkalbi
b_aibibkalbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkalbi: do a = a0, n1ad
if (a == b) cycle a_aibibkalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibibkalbi: do i = i0, n1ijm
if (i == k .or. i == l) cycle i_aibibkalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkalbi(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkalbi
end do a_aibibkalbi
end do b_aibibkalbi
end do k_aibibkalbi
end do l_aibibkalbi
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: d == a, c == b, e == b, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibibkakbm: do m = n0m, n1m
k_aibibkakbm: do k = n0kl, n1kl
if (k == m) cycle k_aibibkakbm
b_aibibkakbm: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkakbm: do a = a0, n1ad
if (a == b) cycle a_aibibkakbm
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibibkakbm: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aibibkakbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkakbm(i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkakbm
end do a_aibibkakbm
end do b_aibibkakbm
end do k_aibibkakbm
end do m_aibibkakbm
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, e == b, j == i, m == k
! No equalities independent of the above can hold.
!
l_aibibkalbk: do l = n0l, n1l
k_aibibkalbk: do k = n0km, n1km
if (k == l) cycle k_aibibkalbk
b_aibibkalbk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkalbk: do a = a0, n1ad
if (a == b) cycle a_aibibkalbk
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibibkalbk: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibibkalbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkalbk(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkalbk
end do a_aibibkalbk
end do b_aibibkalbk
end do k_aibibkalbk
end do l_aibibkalbk
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjbiaibm: do m = n0m, n1m
b_aibjbiaibm: do b = n0bce, n1bce
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbiaibm: do j = n0j, n1j
if (j == m) cycle j_aibjbiaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiaibm: do a = a0, n1ad
if (a == b) cycle a_aibjbiaibm
i1 = min(j - 1, n1ikl)
i_aibjbiaibm: do i = n0ikl, i1
if (i == j .or. i == m) cycle i_aibjbiaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiaibm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiaibm
end do a_aibjbiaibm
end do j_aibjbiaibm
end do b_aibjbiaibm
end do m_aibjbiaibm
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibjbkaibi: do k = n0k, n1k
b_aibjbkaibi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkaibi: do j = j0, n1j
if (j == k) cycle j_aibjbkaibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkaibi: do a = a0, n1ad
if (a == b) cycle a_aibjbkaibi
i1 = min(j - 1, n1ilm)
i_aibjbkaibi: do i = n0ilm, i1
if (i == j .or. i == k) cycle i_aibjbkaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkaibi(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkaibi
end do a_aibjbkaibi
end do j_aibjbkaibi
end do b_aibjbkaibi
end do k_aibjbkaibi
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjbkaibj: do k = n0k, n1k
b_aibjbkaibj: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkaibj: do j = j0, n1jm
if (j == k) cycle j_aibjbkaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkaibj: do a = a0, n1ad
if (a == b) cycle a_aibjbkaibj
i1 = min(j - 1, n1il)
i_aibjbkaibj: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkaibj(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkaibj
end do a_aibjbkaibj
end do j_aibjbkaibj
end do b_aibjbkaibj
end do k_aibjbkaibj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjbkaibk: do k = n0km, n1km
b_aibjbkaibk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkaibk: do j = j0, n1j
if (j == k) cycle j_aibjbkaibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkaibk: do a = a0, n1ad
if (a == b) cycle a_aibjbkaibk
i1 = min(j - 1, n1il)
i_aibjbkaibk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkaibk(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkaibk
end do a_aibjbkaibk
end do j_aibjbkaibk
end do b_aibjbkaibk
end do k_aibjbkaibk
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjbialbi: do l = n0l, n1l
b_aibjbialbi: do b = n0bce, n1bce
j_aibjbialbi: do j = n0j, n1j
if (j == l) cycle j_aibjbialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbialbi: do a = a0, n1ad
if (a == b) cycle a_aibjbialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjbialbi: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjbialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbialbi(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbialbi
end do a_aibjbialbi
end do j_aibjbialbi
end do b_aibjbialbi
end do l_aibjbialbi
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjbiajbm: do m = n0m, n1m
b_aibjbiajbm: do b = n0bce, n1bce
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbiajbm: do j = n0jl, n1jl
if (j == m) cycle j_aibjbiajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiajbm: do a = a0, n1ad
if (a == b) cycle a_aibjbiajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjbiajbm: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjbiajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiajbm(i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiajbm
end do a_aibjbiajbm
end do j_aibjbiajbm
end do b_aibjbiajbm
end do m_aibjbiajbm
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjbialbj: do l = n0l, n1l
b_aibjbialbj: do b = n0bce, n1bce
j_aibjbialbj: do j = n0jm, n1jm
if (j == l) cycle j_aibjbialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbialbj: do a = a0, n1ad
if (a == b) cycle a_aibjbialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjbialbj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjbialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbialbj(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbialbj
end do a_aibjbialbj
end do j_aibjbialbj
end do b_aibjbialbj
end do l_aibjbialbj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjbkajbi: do k = n0k, n1k
b_aibjbkajbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkajbi: do j = j0, n1jl
if (j == k) cycle j_aibjbkajbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkajbi: do a = a0, n1ad
if (a == b) cycle a_aibjbkajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjbkajbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkajbi(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkajbi
end do a_aibjbkajbi
end do j_aibjbkajbi
end do b_aibjbkajbi
end do k_aibjbkajbi
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjbkakbi: do k = n0kl, n1kl
b_aibjbkakbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkakbi: do j = j0, n1j
if (j == k) cycle j_aibjbkakbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkakbi: do a = a0, n1ad
if (a == b) cycle a_aibjbkakbi
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjbkakbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkakbi(a, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkakbi
end do a_aibjbkakbi
end do j_aibjbkakbi
end do b_aibjbkakbi
end do k_aibjbkakbi
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: d == a, c == b, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
k_aibjbkajbj: do k = n0k, n1k
b_aibjbkajbj: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jlm)
j_aibjbkajbj: do j = j0, n1jlm
if (j == k) cycle j_aibjbkajbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkajbj: do a = a0, n1ad
if (a == b) cycle a_aibjbkajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkajbj: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkajbj(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkajbj
end do a_aibjbkajbj
end do j_aibjbkajbj
end do b_aibjbkajbj
end do k_aibjbkajbj
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: d == a, c == b, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjbkajbk: do k = n0km, n1km
b_aibjbkajbk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkajbk: do j = j0, n1jl
if (j == k) cycle j_aibjbkajbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkajbk: do a = a0, n1ad
if (a == b) cycle a_aibjbkajbk
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkajbk: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkajbk(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkajbk
end do a_aibjbkajbk
end do j_aibjbkajbk
end do b_aibjbkajbk
end do k_aibjbkajbk
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: d == a, c == b, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
k_aibjbkakbj: do k = n0kl, n1kl
b_aibjbkakbj: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkakbj: do j = j0, n1jm
if (j == k) cycle j_aibjbkakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkakbj: do a = a0, n1ad
if (a == b) cycle a_aibjbkakbj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkakbj: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkakbj(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkakbj
end do a_aibjbkakbj
end do j_aibjbkakbj
end do b_aibjbkakbj
end do k_aibjbkakbj
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: d == a, c == b, e == b, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibjbkakbk: do k = n0klm, n1klm
b_aibjbkakbk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkakbk: do j = j0, n1j
if (j == k) cycle j_aibjbkakbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkakbk: do a = a0, n1ad
if (a == b) cycle a_aibjbkakbk
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkakbk: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkakbk(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkakbk
end do a_aibjbkakbk
end do j_aibjbkakbk
end do b_aibjbkakbk
end do k_aibjbkakbk
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibibkaiei: do e = n0e, n1e
k_aibibkaiei: do k = n0k, n1k
b_aibibkaiei: do b = n0bc, n1bc
if (b == e) cycle b_aibibkaiei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibibkaiei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibkaiei
i0 = max(k + 1, n0ijlm)
i_aibibkaiei: do i = i0, n1ijlm
if (i == k) cycle i_aibibkaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkaiei(b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkaiei
end do a_aibibkaiei
end do b_aibibkaiei
end do k_aibibkaiei
end do e_aibibkaiei
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
end subroutine ccjac_32_dav_part10
end module ccjac_block_32_dav_part10
