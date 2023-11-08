module ccjac_block_32_dav_part5
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
subroutine ccjac_32_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b0, i0, i1, j0, j1, k0
integer :: n0ad, n0ae, n0bc, n0bcde, n0ce
integer :: n0ik, n0ikl, n0iklm, n0ikm, n0il
integer :: n0im, n0jk, n0jkl, n0jklm, n0jkm
integer :: n0jl, n0jlm, n0jm, n0kl, n0klm
integer :: n0km
integer :: n1ad, n1ae, n1bc, n1bcde, n1ce
integer :: n1ik, n1ikl, n1iklm, n1ikm, n1il
integer :: n1im, n1jk, n1jkl, n1jklm, n1jkm
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
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bcde = max(n0b, n0c, n0d, n0e)
n0ce = max(n0c, n0e)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0iklm = max(n0i, n0k, n0l, n0m)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
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
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bcde = min(n1b, n1c, n1d, n1e)
n1ce = min(n1c, n1e)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1iklm = min(n1i, n1k, n1l, n1m)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
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
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: d == a, e == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjcialci: do l = n0l, n1l
c_aibjcialci: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjcialci: do b = b0, n1b
if (b == c) cycle b_aibjcialci
j_aibjcialci: do j = n0j, n1j
if (j == l) cycle j_aibjcialci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcialci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialci
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialci: do i = n0ikm, n1ikm
if (i == j .or. i == l) cycle i_aibjcialci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcialci(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialci
end do a_aibjcialci
end do j_aibjcialci
end do b_aibjcialci
end do c_aibjcialci
end do l_aibjcialci
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: d == a, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialcj: do l = n0l, n1l
c_aibjcialcj: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjcialcj: do b = b0, n1b
if (b == c) cycle b_aibjcialcj
j_aibjcialcj: do j = n0jm, n1jm
if (j == l) cycle j_aibjcialcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcialcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialcj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialcj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcialcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcialcj(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcialcj
end do a_aibjcialcj
end do j_aibjcialcj
end do b_aibjcialcj
end do c_aibjcialcj
end do l_aibjcialcj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: d == a, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckajci: do c = n0ce, n1ce
k_aibjckajci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajci: do b = b0, n1b
if (b == c) cycle b_aibjckajci
j_aibjckajci: do j = n0jl, n1jl
if (j == k) cycle j_aibjckajci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckajci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajci
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckajci
if (i > j .and. j > k) exit i_aibjckajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajci
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajci(i, b, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckajci(a, b, j, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajci(a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajci
end do a_aibjckajci
end do j_aibjckajci
end do b_aibjckajci
end do k_aibjckajci
end do c_aibjckajci
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: d == a, e == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjalci: do l = n0l, n1l
c_aibjcjalci: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjcjalci: do b = b0, n1b
if (b == c) cycle b_aibjcjalci
j_aibjcjalci: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjalci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjalci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjalci
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalci: do i = n0im, n1im
if (i == j .or. i == l) cycle i_aibjcjalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjalci(b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjalci
end do a_aibjcjalci
end do j_aibjcjalci
end do b_aibjcjalci
end do c_aibjcjalci
end do l_aibjcjalci
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, j
! Equalities: d == a, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckakci: do c = n0ce, n1ce
k_aibjckakci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakci: do b = b0, n1b
if (b == c) cycle b_aibjckakci
j_aibjckakci: do j = n0j, n1j
if (j == k) cycle j_aibjckakci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckakci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakci
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckakci
if (i > j .and. j > k) exit i_aibjckakci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakci
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakci(i, b, j, c)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakci(a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckakci(a, b, j, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakci
end do a_aibjckakci
end do j_aibjckakci
end do b_aibjckakci
end do k_aibjckakci
end do c_aibjckakci
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, m
! Equalities: d == a, e == c, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjcjajcm: do m = n0m, n1m
c_aibjcjajcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjcjajcm: do b = b0, n1b
if (b == c) cycle b_aibjcjajcm
j_aibjcjajcm: do j = n0jkl, n1jkl
if (j == m) cycle j_aibjcjajcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjajcm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjajcm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajcm: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjcjajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajcm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajcm
end do a_aibjcjajcm
end do j_aibjcjajcm
end do b_aibjcjajcm
end do c_aibjcjajcm
end do m_aibjcjajcm
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k
! Equalities: d == a, e == c, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckajcj: do c = n0ce, n1ce
k_aibjckajcj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajcj: do b = b0, n1b
if (b == c) cycle b_aibjckajcj
j_aibjckajcj: do j = n0jlm, n1jlm
if (j == k) cycle j_aibjckajcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckajcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajcj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajcj
if (i > j .and. j > k) exit i_aibjckajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajcj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckajcj(i, b, j, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckajcj(i, b, j, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajcj(i, b, j, k)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajcj(i, b, j, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajcj
end do a_aibjckajcj
end do j_aibjckajcj
end do b_aibjckajcj
end do k_aibjckajcj
end do c_aibjckajcj
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: d == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajck: do c = n0ce, n1ce
k_aibjckajck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajck: do b = b0, n1b
if (b == c) cycle b_aibjckajck
j_aibjckajck: do j = n0jl, n1jl
if (j == k) cycle j_aibjckajck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckajck: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajck
if (i > j .and. j > k) exit i_aibjckajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajck
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckajck(a, i, b, j)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckajck(a, i, b, j)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajck(a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckajck(a, i, b, j)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajck(a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajck
end do a_aibjckajck
end do j_aibjckajck
end do b_aibjckajck
end do k_aibjckajck
end do c_aibjckajck
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, l
! Equalities: d == a, e == c, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjcjalcj: do l = n0l, n1l
c_aibjcjalcj: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjcjalcj: do b = b0, n1b
if (b == c) cycle b_aibjcjalcj
j_aibjcjalcj: do j = n0jkm, n1jkm
if (j == l) cycle j_aibjcjalcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjalcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjalcj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjalcj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjalcj(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjalcj
end do a_aibjcjalcj
end do j_aibjcjalcj
end do b_aibjcjalcj
end do c_aibjcjalcj
end do l_aibjcjalcj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: d == a, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckakcj: do c = n0ce, n1ce
k_aibjckakcj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakcj: do b = b0, n1b
if (b == c) cycle b_aibjckakcj
j_aibjckakcj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckakcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckakcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakcj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakcj
if (i > j .and. j > k) exit i_aibjckakcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakcj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakcj(a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckakcj(a, i, b, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakcj(i, b, j, c)
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakcj
end do a_aibjckakcj
end do j_aibjckakcj
end do b_aibjckakcj
end do k_aibjckakcj
end do c_aibjckakcj
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j
! Equalities: d == a, e == c, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckakck: do c = n0ce, n1ce
k_aibjckakck: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakck: do b = b0, n1b
if (b == c) cycle b_aibjckakck
j_aibjckakck: do j = n0j, n1j
if (j == k) cycle j_aibjckakck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckakck: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakck
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakck
if (i > j .and. j > k) exit i_aibjckakck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakck
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakck(i, b, j, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckakck(i, b, j, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckakck(i, b, j, k)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckakck(i, b, j, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakck
end do a_aibjckakck
end do j_aibjckakck
end do b_aibjckakck
end do k_aibjckakck
end do c_aibjckakck
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjciaiei: do e = n0e, n1e
c_aibjciaiei: do c = n0c, n1c
if (c == e) cycle c_aibjciaiei
b0 = max(c + 1, n0b)
b_aibjciaiei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjciaiei
j_aibjciaiei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjciaiei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjciaiei
i_aibjciaiei: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjciaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaiei(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaiei
end do a_aibjciaiei
end do j_aibjciaiei
end do b_aibjciaiei
end do c_aibjciaiei
end do e_aibjciaiei
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjciaiej: do e = n0e, n1e
c_aibjciaiej: do c = n0c, n1c
if (c == e) cycle c_aibjciaiej
b0 = max(c + 1, n0b)
b_aibjciaiej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjciaiej
j_aibjciaiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjciaiej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjciaiej
i_aibjciaiej: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjciaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaiej(i, b, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaiej
end do a_aibjciaiej
end do j_aibjciaiej
end do b_aibjciaiej
end do c_aibjciaiej
end do e_aibjciaiej
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjaiej: do e = n0e, n1e
c_aibjcjaiej: do c = n0c, n1c
if (c == e) cycle c_aibjcjaiej
b0 = max(c + 1, n0b)
b_aibjcjaiej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjcjaiej
j_aibjcjaiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjcjaiej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjaiej
i_aibjcjaiej: do i = n0il, n1il
if (i == j) cycle i_aibjcjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjaiej(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaiej
end do a_aibjcjaiej
end do j_aibjcjaiej
end do b_aibjcjaiej
end do c_aibjcjaiej
end do e_aibjcjaiej
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjcjajei: do e = n0e, n1e
c_aibjcjajei: do c = n0c, n1c
if (c == e) cycle c_aibjcjajei
b0 = max(c + 1, n0b)
b_aibjcjajei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjcjajei
j_aibjcjajei: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjcjajei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajei: do i = n0im, n1im
if (i == j) cycle i_aibjcjajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajei(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajei
end do a_aibjcjajei
end do j_aibjcjajei
end do b_aibjcjajei
end do c_aibjcjajei
end do e_aibjcjajei
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: j, i
! Equalities: d == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjajej: do e = n0e, n1e
c_aibjcjajej: do c = n0c, n1c
if (c == e) cycle c_aibjcjajej
b0 = max(c + 1, n0b)
b_aibjcjajej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjcjajej
j_aibjcjajej: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjcjajej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjajej
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajej: do i = n0i, n1i
if (i == j) cycle i_aibjcjajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajej(i, b, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajej
end do a_aibjcjajej
end do j_aibjcjajej
end do b_aibjcjajej
end do c_aibjcjajej
end do e_aibjcjajej
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: e == a, c == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdiaj: do d = n0d, n1d
k_aibjbkdiaj: do k = n0k, n1k
b_aibjbkdiaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdiaj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkdiaj: do j = j0, n1jm
if (j == k) cycle j_aibjbkdiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdiaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aibjbkdiaj: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdiaj(b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdiaj
end do a_aibjbkdiaj
end do j_aibjbkdiaj
end do b_aibjbkdiaj
end do k_aibjbkdiaj
end do d_aibjbkdiaj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: e == a, c == b, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdjai: do d = n0d, n1d
k_aibjbkdjai: do k = n0k, n1k
b_aibjbkdjai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdjai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkdjai: do j = j0, n1jl
if (j == k) cycle j_aibjbkdjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdjai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdjai
i1 = min(j - 1, n1im)
i_aibjbkdjai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdjai(b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdjai
end do a_aibjbkdjai
end do j_aibjbkdjai
end do b_aibjbkdjai
end do k_aibjbkdjai
end do d_aibjbkdjai
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, j
! Equalities: e == a, c == b, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdkai: do d = n0d, n1d
k_aibjbkdkai: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkdkai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdkai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkdkai: do j = j0, n1j
if (j == k) cycle j_aibjbkdkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdkai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdkai
i1 = min(j - 1, n1im)
i_aibjbkdkai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdkai(b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdkai
end do a_aibjbkdkai
end do j_aibjbkdkai
end do b_aibjbkdkai
end do k_aibjbkdkai
end do d_aibjbkdkai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, k, i
! Equalities: e == a, c == b, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdkaj: do d = n0d, n1d
k_aibjbkdkaj: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkdkaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdkaj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkdkaj: do j = j0, n1jm
if (j == k) cycle j_aibjbkdkaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdkaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdkaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkdkaj: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkdkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkdkaj(i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdkaj
end do a_aibjbkdkaj
end do j_aibjbkdkaj
end do b_aibjbkdkaj
end do k_aibjbkdkaj
end do d_aibjbkdkaj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjcidiai: do d = n0d, n1d
c_aibjcidiai: do c = n0c, n1c
if (c == d) cycle c_aibjcidiai
b0 = max(c + 1, n0b)
b_aibjcidiai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidiai
j_aibjcidiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcidiai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidiai
i_aibjcidiai: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjcidiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcidiai(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidiai
end do a_aibjcidiai
end do j_aibjcidiai
end do b_aibjcidiai
end do c_aibjcidiai
end do d_aibjcidiai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdiaj: do d = n0d, n1d
c_aibjcjdiaj: do c = n0c, n1c
if (c == d) cycle c_aibjcjdiaj
b0 = max(c + 1, n0b)
b_aibjcjdiaj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcjdiaj
j_aibjcjdiaj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdiaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjdiaj: do i = n0il, n1il
if (i == j) cycle i_aibjcjdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjdiaj(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdiaj
end do a_aibjcjdiaj
end do j_aibjcjdiaj
end do b_aibjcjdiaj
end do c_aibjcjdiaj
end do d_aibjcjdiaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjai: do d = n0d, n1d
c_aibjcidjai: do c = n0c, n1c
if (c == d) cycle c_aibjcidjai
b0 = max(c + 1, n0b)
b_aibjcidjai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidjai
j_aibjcidjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcidjai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjai
i_aibjcidjai: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjcidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcidjai(i, b, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidjai
end do a_aibjcidjai
end do j_aibjcidjai
end do b_aibjcidjai
end do c_aibjcidjai
end do d_aibjcidjai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjcjdjai: do d = n0d, n1d
c_aibjcjdjai: do c = n0c, n1c
if (c == d) cycle c_aibjcjdjai
b0 = max(c + 1, n0b)
b_aibjcjdjai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcjdjai
j_aibjcjdjai: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdjai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdjai
i_aibjcjdjai: do i = n0im, n1im
if (i == j) cycle i_aibjcjdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjdjai(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdjai
end do a_aibjcjdjai
end do j_aibjcjdjai
end do b_aibjcjdjai
end do c_aibjcjdjai
end do d_aibjcjdjai
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i
! Equalities: e == a, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdjaj: do d = n0d, n1d
c_aibjcjdjaj: do c = n0c, n1c
if (c == d) cycle c_aibjcjdjaj
b0 = max(c + 1, n0b)
b_aibjcjdjaj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcjdjaj
j_aibjcjdjaj: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdjaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjdjaj: do i = n0i, n1i
if (i == j) cycle i_aibjcjdjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjdjaj(i, b, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdjaj
end do a_aibjcjdjaj
end do j_aibjcjdjaj
end do b_aibjcjdjaj
end do c_aibjcjdjaj
end do d_aibjcjdjaj
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k, m
! Equalities: c == b, d == b, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjbkbibm: do m = n0m, n1m
k_aibjbkbibm: do k = n0k, n1k
if (k == m) cycle k_aibjbkbibm
b_aibjbkbibm: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbibm: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjbkbibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbibm: do a = a0, n1a
if (a == b) cycle a_aibjbkbibm
i0 = max(m + 1, n0il)
i1 = min(j - 1, n1il)
i_aibjbkbibm: do i = i0, i1
if (i == j .or. i == k .or. i == m) cycle i_aibjbkbibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbibm(a, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbibm
end do a_aibjbkbibm
end do j_aibjbkbibm
end do b_aibjbkbibm
end do k_aibjbkbibm
end do m_aibjbkbibm
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k, l
! Equalities: c == b, d == b, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjbkblbi: do l = n0l, n1l
k_aibjbkblbi: do k = n0k, n1k
if (k == l) cycle k_aibjbkblbi
b_aibjbkblbi: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkblbi: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjbkblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkblbi: do a = a0, n1a
if (a == b) cycle a_aibjbkblbi
i1 = min(l - 1, j - 1, n1im)
i_aibjbkblbi: do i = n0im, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkblbi(a, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkblbi
end do a_aibjbkblbi
end do j_aibjbkblbi
end do b_aibjbkblbi
end do k_aibjbkblbi
end do l_aibjbkblbi
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k, m
! Equalities: c == b, d == b, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjbkbjbm: do m = n0m, n1m
k_aibjbkbjbm: do k = n0k, n1k
if (k == m) cycle k_aibjbkbjbm
b_aibjbkbjbm: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(k + 1, m + 1, n0jl)
j_aibjbkbjbm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjbkbjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbjbm: do a = a0, n1a
if (a == b) cycle a_aibjbkbjbm
i1 = min(j - 1, n1i)
i_aibjbkbjbm: do i = n0i, i1
if (i == j .or. i == k .or. i == m) cycle i_aibjbkbjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbjbm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbjbm
end do a_aibjbkbjbm
end do j_aibjbkbjbm
end do b_aibjbkbjbm
end do k_aibjbkbjbm
end do m_aibjbkbjbm
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k, l
! Equalities: c == b, d == b, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjbkblbj: do l = n0l, n1l
k_aibjbkblbj: do k = n0k, n1k
if (k == l) cycle k_aibjbkblbj
b_aibjbkblbj: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0jm)
j1 = min(l - 1, n1jm)
j_aibjbkblbj: do j = j0, j1
if (j == k .or. j == l) cycle j_aibjbkblbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkblbj: do a = a0, n1a
if (a == b) cycle a_aibjbkblbj
i1 = min(j - 1, n1i)
i_aibjbkblbj: do i = n0i, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkblbj(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkblbj
end do a_aibjbkblbj
end do j_aibjbkblbj
end do b_aibjbkblbj
end do k_aibjbkblbj
end do l_aibjbkblbj
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a
! Free occupied indices: k, i, j, m
! Equalities: c == b, d == b, e == b, l == k
! No equalities independent of the above can hold.
!
m_aibjbkbkbm: do m = n0m, n1m
k0 = max(m + 1, n0kl)
k_aibjbkbkbm: do k = k0, n1kl
if (k == m) cycle k_aibjbkbkbm
b_aibjbkbkbm: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbkbm: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjbkbkbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbkbm: do a = a0, n1a
if (a == b) cycle a_aibjbkbkbm
i1 = min(j - 1, n1i)
i_aibjbkbkbm: do i = n0i, i1
if (i == j .or. i == k .or. i == m) cycle i_aibjbkbkbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbkbm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbkbm
end do a_aibjbkbkbm
end do j_aibjbkbkbm
end do b_aibjbkbkbm
end do k_aibjbkbkbm
end do m_aibjbkbkbm
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
end subroutine ccjac_32_dav_part5
end module ccjac_block_32_dav_part5
