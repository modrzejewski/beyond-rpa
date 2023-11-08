module ccjac_block_32_dav_part14
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
subroutine ccjac_32_dav_part14(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a, b, c
integer :: i, j, k
integer :: ai, bj, ck, dl, em
integer :: a0, i0, i1
integer :: n0ab, n0ad, n0ade, n0bc, n0bcde
integer :: n0bce, n0cde, n0ij, n0ijl, n0ijlm
integer :: n0ijm, n0ik, n0ikl, n0iklm, n0ikm
integer :: n0jklm, n0jl, n0jlm, n0jm, n0kl
integer :: n0klm, n0km
integer :: n1ab, n1ad, n1ade, n1bc, n1bcde
integer :: n1bce, n1cde, n1ij, n1ijl, n1ijlm
integer :: n1ijm, n1ik, n1ikl, n1iklm, n1ikm
integer :: n1jklm, n1jl, n1jlm, n1jm, n1kl
integer :: n1klm, n1km
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
n0bcde = max(n0b, n0c, n0d, n0e)
n0bce = max(n0b, n0c, n0e)
n0cde = max(n0c, n0d, n0e)
n0ij = max(n0i, n0j)
n0ijl = max(n0i, n0j, n0l)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0iklm = max(n0i, n0k, n0l, n0m)
n0ikm = max(n0i, n0k, n0m)
n0jklm = max(n0j, n0k, n0l, n0m)
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
n1bcde = min(n1b, n1c, n1d, n1e)
n1bce = min(n1b, n1c, n1e)
n1cde = min(n1c, n1d, n1e)
n1ij = min(n1i, n1j)
n1ijl = min(n1i, n1j, n1l)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1iklm = min(n1i, n1k, n1l, n1m)
n1ikm = min(n1i, n1k, n1m)
n1jklm = min(n1j, n1k, n1l, n1m)
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
! Free occupied indices: j, i
! Equalities: b == a, d == c, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjcjcj: do c = n0cde, n1cde
j_aiajcjcjcj: do j = n0jklm, n1jklm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjcjcj: do a = a0, n1ab
if (a == c) cycle a_aiajcjcjcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjcjcj: do i = i0, n1i
if (i == j) cycle i_aiajcjcjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aiajcjcjcj(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjcjcj
end do a_aiajcjcjcj
end do j_aiajcjcjcj
end do c_aiajcjcjcj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, e == a, c == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibibkaiai: do k = n0k, n1k
b_aibibkaiai: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibibkaiai: do a = a0, n1ade
if (a == b) cycle a_aibibkaiai
i0 = max(k + 1, n0ijlm)
i_aibibkaiai: do i = i0, n1ijlm
if (i == k) cycle i_aibibkaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkaiai(a, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkaiai
end do a_aibibkaiai
end do b_aibibkaiai
end do k_aibibkaiai
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, e == a, c == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibibkaiak: do k = n0km, n1km
b_aibibkaiak: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibibkaiak: do a = a0, n1ade
if (a == b) cycle a_aibibkaiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibibkaiak: do i = i0, n1ijl
if (i == k) cycle i_aibibkaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkaiak(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkaiak
end do a_aibibkaiak
end do b_aibibkaiak
end do k_aibibkaiak
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, e == a, c == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
b_aibjbiaiai: do b = n0bc, n1bc
j_aibjbiaiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjbiaiai: do a = a0, n1ade
if (a == b) cycle a_aibjbiaiai
i1 = min(j - 1, n1iklm)
i_aibjbiaiai: do i = n0iklm, i1
if (i == j) cycle i_aibjbiaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiaiai(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiaiai
end do a_aibjbiaiai
end do j_aibjbiaiai
end do b_aibjbiaiai
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, e == a, c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiajai: do b = n0bc, n1bc
j_aibjbiajai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a_aibjbiajai: do a = a0, n1ade
if (a == b) cycle a_aibjbiajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjbiajai: do i = n0ikm, i1
if (i == j) cycle i_aibjbiajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiajai(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiajai
end do a_aibjbiajai
end do j_aibjbiajai
end do b_aibjbiajai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibibkaibi: do k = n0k, n1k
b_aibibkaibi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkaibi: do a = a0, n1ad
if (a == b) cycle a_aibibkaibi
i0 = max(k + 1, n0ijlm)
i_aibibkaibi: do i = i0, n1ijlm
if (i == k) cycle i_aibibkaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkaibi(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkaibi
end do a_aibibkaibi
end do b_aibibkaibi
end do k_aibibkaibi
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibibkaibk: do k = n0km, n1km
b_aibibkaibk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkaibk: do a = a0, n1ad
if (a == b) cycle a_aibibkaibk
i0 = max(k + 1, n0ijl)
i_aibibkaibk: do i = i0, n1ijl
if (i == k) cycle i_aibibkaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkaibk(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkaibk
end do a_aibibkaibk
end do b_aibibkaibk
end do k_aibibkaibk
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkakbi: do k = n0kl, n1kl
b_aibibkakbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkakbi: do a = a0, n1ad
if (a == b) cycle a_aibibkakbi
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibibkakbi: do i = i0, n1ijm
if (i == k) cycle i_aibibkakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkakbi(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkakbi
end do a_aibibkakbi
end do b_aibibkakbi
end do k_aibibkakbi
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibibkakbk: do k = n0klm, n1klm
b_aibibkakbk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkakbk: do a = a0, n1ad
if (a == b) cycle a_aibibkakbk
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibibkakbk: do i = i0, n1ij
if (i == k) cycle i_aibibkakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkakbk(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkakbk
end do a_aibibkakbk
end do b_aibibkakbk
end do k_aibibkakbk
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
b_aibjbiaibi: do b = n0bce, n1bce
j_aibjbiaibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiaibi: do a = a0, n1ad
if (a == b) cycle a_aibjbiaibi
i1 = min(j - 1, n1iklm)
i_aibjbiaibi: do i = n0iklm, i1
if (i == j) cycle i_aibjbiaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiaibi(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiaibi
end do a_aibjbiaibi
end do j_aibjbiaibi
end do b_aibjbiaibi
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
b_aibjbiaibj: do b = n0bce, n1bce
j_aibjbiaibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiaibj: do a = a0, n1ad
if (a == b) cycle a_aibjbiaibj
i1 = min(j - 1, n1ikl)
i_aibjbiaibj: do i = n0ikl, i1
if (i == j) cycle i_aibjbiaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiaibj(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiaibj
end do a_aibjbiaibj
end do j_aibjbiaibj
end do b_aibjbiaibj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiajbi: do b = n0bce, n1bce
j_aibjbiajbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiajbi: do a = a0, n1ad
if (a == b) cycle a_aibjbiajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjbiajbi: do i = n0ikm, i1
if (i == j) cycle i_aibjbiajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiajbi(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiajbi
end do a_aibjbiajbi
end do j_aibjbiajbi
end do b_aibjbiajbi
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
b_aibjbiajbj: do b = n0bce, n1bce
j_aibjbiajbj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiajbj: do a = a0, n1ad
if (a == b) cycle a_aibjbiajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjbiajbj: do i = n0ik, i1
if (i == j) cycle i_aibjbiajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiajbj(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiajbj
end do a_aibjbiajbj
end do j_aibjbiajbj
end do b_aibjbiajbj
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibibkbibi: do k = n0k, n1k
b_aibibkbibi: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbibi: do a = a0, n1a
if (a == b) cycle a_aibibkbibi
i0 = max(k + 1, n0ijlm)
i_aibibkbibi: do i = i0, n1ijlm
if (i == k) cycle i_aibibkbibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkbibi(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbibi
end do a_aibibkbibi
end do b_aibibkbibi
end do k_aibibkbibi
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibibkbibk: do k = n0km, n1km
b_aibibkbibk: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbibk: do a = a0, n1a
if (a == b) cycle a_aibibkbibk
i0 = max(k + 1, n0ijl)
i_aibibkbibk: do i = i0, n1ijl
if (i == k) cycle i_aibibkbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkbibk(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbibk
end do a_aibibkbibk
end do b_aibibkbibk
end do k_aibibkbibk
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, e == b, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibibkbkbk: do k = n0klm, n1klm
b_aibibkbkbk: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbkbk: do a = a0, n1a
if (a == b) cycle a_aibibkbkbk
i0 = max(k + 1, n0ij)
i_aibibkbkbk: do i = i0, n1ij
if (i == k) cycle i_aibibkbkbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkbkbk(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbkbk
end do a_aibibkbkbk
end do b_aibibkbkbk
end do k_aibibkbkbk
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
b_aibjbibibi: do b = n0bcde, n1bcde
j_aibjbibibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibibi: do a = a0, n1a
if (a == b) cycle a_aibjbibibi
i1 = min(j - 1, n1iklm)
i_aibjbibibi: do i = n0iklm, i1
if (i == j) cycle i_aibjbibibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbibibi(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibibi
end do a_aibjbibibi
end do j_aibjbibibi
end do b_aibjbibibi
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbibjbi: do b = n0bcde, n1bcde
j_aibjbibjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibjbi: do a = a0, n1a
if (a == b) cycle a_aibjbibjbi
i1 = min(j - 1, n1ikm)
i_aibjbibjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjbibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbibjbi(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibjbi
end do a_aibjbibjbi
end do j_aibjbibjbi
end do b_aibjbibjbi
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
b_aibjbibjbj: do b = n0bcde, n1bcde
j_aibjbibjbj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibjbj: do a = a0, n1a
if (a == b) cycle a_aibjbibjbj
i1 = min(j - 1, n1ik)
i_aibjbibjbj: do i = n0ik, i1
if (i == j) cycle i_aibjbibjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbibjbj(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibjbj
end do a_aibjbibjbj
end do j_aibjbibjbj
end do b_aibjbibjbj
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
end subroutine ccjac_32_dav_part14
end module ccjac_block_32_dav_part14
