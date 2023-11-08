module ccjac_block_32_dav_part1
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
subroutine ccjac_32_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b0
integer :: n0ad, n0ae, n0bd, n0be, n0ce
integer :: n0il, n0im, n0jl, n0jm, n0kl
integer :: n0km
integer :: n1ad, n1ae, n1bd, n1be, n1ce
integer :: n1il, n1im, n1jl, n1jm, n1kl
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
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0ce = max(n0c, n0e)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
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
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, m
! Equalities: d == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjckaibm: do m = n0m, n1m
c_aibjckaibm: do c = n0c, n1c
k_aibjckaibm: do k = n0k, n1k
if (k == m) cycle k_aibjckaibm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckaibm: do b = b0, n1be
if (b == c) cycle b_aibjckaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjckaibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjckaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibm
i_aibjckaibm: do i = n0il, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjckaibm
if (i > j .and. j > k) exit i_aibjckaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaibm
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaibm(j, c, k, m)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaibm(j, c, k, m)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaibm(j, c, k, m)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaibm(j, c, k, m)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaibm(j, c, k, m)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaibm
end do a_aibjckaibm
end do j_aibjckaibm
end do b_aibjckaibm
end do k_aibjckaibm
end do c_aibjckaibm
end do m_aibjckaibm
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, l
! Equalities: d == a, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjckalbi: do l = n0l, n1l
c_aibjckalbi: do c = n0c, n1c
k_aibjckalbi: do k = n0k, n1k
if (k == l) cycle k_aibjckalbi
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckalbi: do b = b0, n1be
if (b == c) cycle b_aibjckalbi
j_aibjckalbi: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckalbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbi: do i = n0im, n1im
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbi
if (i > j .and. j > k) exit i_aibjckalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckalbi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckalbi(j, c, k, l)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckalbi(j, c, k, l)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckalbi
end do a_aibjckalbi
end do j_aibjckalbi
end do b_aibjckalbi
end do k_aibjckalbi
end do c_aibjckalbi
end do l_aibjckalbi
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, m
! Equalities: d == a, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjckajbm: do m = n0m, n1m
c_aibjckajbm: do c = n0c, n1c
k_aibjckajbm: do k = n0k, n1k
if (k == m) cycle k_aibjckajbm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckajbm: do b = b0, n1be
if (b == c) cycle b_aibjckajbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjckajbm: do j = n0jl, n1jl
if (j == k .or. j == m) cycle j_aibjckajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckajbm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckajbm
if (i > j .and. j > k) exit i_aibjckajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajbm
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajbm(i, c, k, m)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajbm(i, c, k, m)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajbm
end do a_aibjckajbm
end do j_aibjckajbm
end do b_aibjckajbm
end do k_aibjckajbm
end do c_aibjckajbm
end do m_aibjckajbm
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, l
! Equalities: d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjckalbj: do l = n0l, n1l
c_aibjckalbj: do c = n0c, n1c
k_aibjckalbj: do k = n0k, n1k
if (k == l) cycle k_aibjckalbj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckalbj: do b = b0, n1be
if (b == c) cycle b_aibjckalbj
j_aibjckalbj: do j = n0jm, n1jm
if (j == k .or. j == l) cycle j_aibjckalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckalbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbj
if (i > j .and. j > k) exit i_aibjckalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckalbj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckalbj(i, c, k, l)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckalbj(i, c, k, l)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckalbj(i, c, k, l)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckalbj(i, c, k, l)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckalbj(i, c, k, l)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckalbj
end do a_aibjckalbj
end do j_aibjckalbj
end do b_aibjckalbj
end do k_aibjckalbj
end do c_aibjckalbj
end do l_aibjckalbj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j, m
! Equalities: d == a, e == b, l == k
! No equalities independent of the above can hold.
!
m_aibjckakbm: do m = n0m, n1m
c_aibjckakbm: do c = n0c, n1c
k_aibjckakbm: do k = n0kl, n1kl
if (k == m) cycle k_aibjckakbm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckakbm: do b = b0, n1be
if (b == c) cycle b_aibjckakbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjckakbm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjckakbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckakbm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakbm
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckakbm
if (i > j .and. j > k) exit i_aibjckakbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakbm
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakbm(i, j, c, m)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakbm(i, j, c, m)
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakbm
end do a_aibjckakbm
end do j_aibjckakbm
end do b_aibjckakbm
end do k_aibjckakbm
end do c_aibjckakbm
end do m_aibjckakbm
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j, l
! Equalities: d == a, e == b, m == k
! No equalities independent of the above can hold.
!
l_aibjckalbk: do l = n0l, n1l
c_aibjckalbk: do c = n0c, n1c
k_aibjckalbk: do k = n0km, n1km
if (k == l) cycle k_aibjckalbk
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckalbk: do b = b0, n1be
if (b == c) cycle b_aibjckalbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckalbk: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckalbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckalbk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalbk
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbk: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbk
if (i > j .and. j > k) exit i_aibjckalbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckalbk
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckalbk(i, j, c, l)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckalbk(i, j, c, l)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckalbk
end do a_aibjckalbk
end do j_aibjckalbk
end do b_aibjckalbk
end do k_aibjckalbk
end do c_aibjckalbk
end do l_aibjckalbk
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k, m
! Equalities: d == a, e == c, l == i
! No equalities independent of the above can hold.
!
m_aibjckaicm: do m = n0m, n1m
c_aibjckaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckaicm: do k = n0k, n1k
if (k == m) cycle k_aibjckaicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaicm: do b = b0, n1b
if (b == c) cycle b_aibjckaicm
j_aibjckaicm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjckaicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckaicm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaicm
i_aibjckaicm: do i = n0il, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjckaicm
if (i > j .and. j > k) exit i_aibjckaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaicm
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaicm(b, j, k, m)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaicm(b, j, k, m)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaicm(b, j, k, m)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaicm(b, j, k, m)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaicm(b, j, k, m)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaicm
end do a_aibjckaicm
end do j_aibjckaicm
end do b_aibjckaicm
end do k_aibjckaicm
end do c_aibjckaicm
end do m_aibjckaicm
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k, l
! Equalities: d == a, e == c, m == i
! No equalities independent of the above can hold.
!
l_aibjckalci: do l = n0l, n1l
c_aibjckalci: do c = n0ce, n1ce
k_aibjckalci: do k = n0k, n1k
if (k == l) cycle k_aibjckalci
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckalci: do b = b0, n1b
if (b == c) cycle b_aibjckalci
j_aibjckalci: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckalci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckalci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalci
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalci: do i = n0im, n1im
if (i == j .or. i == k .or. i == l) cycle i_aibjckalci
if (i > j .and. j > k) exit i_aibjckalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckalci
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckalci(b, j, k, l)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckalci(b, j, k, l)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckalci
end do a_aibjckalci
end do j_aibjckalci
end do b_aibjckalci
end do k_aibjckalci
end do c_aibjckalci
end do l_aibjckalci
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k, m
! Equalities: d == a, e == c, l == j
! No equalities independent of the above can hold.
!
m_aibjckajcm: do m = n0m, n1m
c_aibjckajcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckajcm: do k = n0k, n1k
if (k == m) cycle k_aibjckajcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajcm: do b = b0, n1b
if (b == c) cycle b_aibjckajcm
j_aibjckajcm: do j = n0jl, n1jl
if (j == k .or. j == m) cycle j_aibjckajcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckajcm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajcm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajcm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckajcm
if (i > j .and. j > k) exit i_aibjckajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajcm
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajcm(i, b, k, m)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajcm(i, b, k, m)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajcm
end do a_aibjckajcm
end do j_aibjckajcm
end do b_aibjckajcm
end do k_aibjckajcm
end do c_aibjckajcm
end do m_aibjckajcm
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k, l
! Equalities: d == a, e == c, m == j
! No equalities independent of the above can hold.
!
l_aibjckalcj: do l = n0l, n1l
c_aibjckalcj: do c = n0ce, n1ce
k_aibjckalcj: do k = n0k, n1k
if (k == l) cycle k_aibjckalcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckalcj: do b = b0, n1b
if (b == c) cycle b_aibjckalcj
j_aibjckalcj: do j = n0jm, n1jm
if (j == k .or. j == l) cycle j_aibjckalcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckalcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalcj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalcj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalcj
if (i > j .and. j > k) exit i_aibjckalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckalcj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckalcj(i, b, k, l)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckalcj(i, b, k, l)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckalcj
end do a_aibjckalcj
end do j_aibjckalcj
end do b_aibjckalcj
end do k_aibjckalcj
end do c_aibjckalcj
end do l_aibjckalcj
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j, m
! Equalities: d == a, e == c, l == k
! No equalities independent of the above can hold.
!
m_aibjckakcm: do m = n0m, n1m
c_aibjckakcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckakcm: do k = n0kl, n1kl
if (k == m) cycle k_aibjckakcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakcm: do b = b0, n1b
if (b == c) cycle b_aibjckakcm
j_aibjckakcm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjckakcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckakcm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakcm
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakcm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckakcm
if (i > j .and. j > k) exit i_aibjckakcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakcm
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakcm(i, b, j, m)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakcm(i, b, j, m)
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakcm
end do a_aibjckakcm
end do j_aibjckakcm
end do b_aibjckakcm
end do k_aibjckakcm
end do c_aibjckakcm
end do m_aibjckakcm
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j, l
! Equalities: d == a, e == c, m == k
! No equalities independent of the above can hold.
!
l_aibjckalck: do l = n0l, n1l
c_aibjckalck: do c = n0ce, n1ce
k_aibjckalck: do k = n0km, n1km
if (k == l) cycle k_aibjckalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckalck: do b = b0, n1b
if (b == c) cycle b_aibjckalck
j_aibjckalck: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckalck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckalck: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalck
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalck: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalck
if (i > j .and. j > k) exit i_aibjckalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckalck
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckalck(i, b, j, l)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckalck(i, b, j, l)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckalck(i, b, j, l)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckalck(i, b, j, l)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckalck(i, b, j, l)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckalck
end do a_aibjckalck
end do j_aibjckalck
end do b_aibjckalck
end do k_aibjckalck
end do c_aibjckalck
end do l_aibjckalck
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, k
! Equalities: d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckaiej: do e = n0e, n1e
c_aibjckaiej: do c = n0c, n1c
if (c == e) cycle c_aibjckaiej
k_aibjckaiej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaiej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckaiej
j_aibjckaiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjckaiej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjckaiej
i_aibjckaiej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaiej
if (i > j .and. j > k) exit i_aibjckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaiej
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaiej(b, c, k, e)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaiej(b, c, k, e)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaiej(b, c, k, e)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaiej(b, c, k, e)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaiej(b, c, k, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaiej
end do a_aibjckaiej
end do j_aibjckaiej
end do b_aibjckaiej
end do k_aibjckaiej
end do c_aibjckaiej
end do e_aibjckaiej
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, k, j
! Equalities: d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjckaiek: do e = n0e, n1e
c_aibjckaiek: do c = n0c, n1c
if (c == e) cycle c_aibjckaiek
k_aibjckaiek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaiek: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckaiek
j_aibjckaiek: do j = n0j, n1j
if (j == k) cycle j_aibjckaiek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjckaiek: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjckaiek
i_aibjckaiek: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaiek
if (i > j .and. j > k) exit i_aibjckaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckaiek
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckaiek(b, j, c, e)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckaiek(b, j, c, e)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckaiek(b, j, c, e)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckaiek(b, j, c, e)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckaiek(b, j, c, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaiek
end do a_aibjckaiek
end do j_aibjckaiek
end do b_aibjckaiek
end do k_aibjckaiek
end do c_aibjckaiek
end do e_aibjckaiek
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, k
! Equalities: d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckajei: do e = n0e, n1e
c_aibjckajei: do c = n0c, n1c
if (c == e) cycle c_aibjckajei
k_aibjckajei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckajei
j_aibjckajei: do j = n0jl, n1jl
if (j == k) cycle j_aibjckajei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjckajei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjckajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckajei
if (i > j .and. j > k) exit i_aibjckajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajei
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajei(b, c, k, e)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajei(b, c, k, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajei
end do a_aibjckajei
end do j_aibjckajei
end do b_aibjckajei
end do k_aibjckajei
end do c_aibjckajei
end do e_aibjckajei
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, k, j
! Equalities: d == a, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjckakei: do e = n0e, n1e
c_aibjckakei: do c = n0c, n1c
if (c == e) cycle c_aibjckakei
k_aibjckakei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckakei
j_aibjckakei: do j = n0j, n1j
if (j == k) cycle j_aibjckakei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjckakei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjckakei
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckakei
if (i > j .and. j > k) exit i_aibjckakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakei
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakei(b, j, c, e)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakei(b, j, c, e)
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakei
end do a_aibjckakei
end do j_aibjckakei
end do b_aibjckakei
end do k_aibjckakei
end do c_aibjckakei
end do e_aibjckakei
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: j, k, i
! Equalities: d == a, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjckajek: do e = n0e, n1e
c_aibjckajek: do c = n0c, n1c
if (c == e) cycle c_aibjckajek
k_aibjckajek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajek: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckajek
j_aibjckajek: do j = n0jl, n1jl
if (j == k) cycle j_aibjckajek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjckajek: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjckajek
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajek
if (i > j .and. j > k) exit i_aibjckajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckajek
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckajek(i, b, c, e)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckajek(i, b, c, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckajek
end do a_aibjckajek
end do j_aibjckajek
end do b_aibjckajek
end do k_aibjckajek
end do c_aibjckajek
end do e_aibjckajek
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: j, k, i
! Equalities: d == a, m == j, l == k
! No equalities independent of the above can hold.
!
e_aibjckakej: do e = n0e, n1e
c_aibjckakej: do c = n0c, n1c
if (c == e) cycle c_aibjckakej
k_aibjckakej: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckakej
j_aibjckakej: do j = n0jm, n1jm
if (j == k) cycle j_aibjckakej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjckakej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjckakej
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakej: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakej
if (i > j .and. j > k) exit i_aibjckakej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckakej
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckakej(i, b, c, e)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckakej(i, b, c, e)
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckakej
end do a_aibjckakej
end do j_aibjckakej
end do b_aibjckakej
end do k_aibjckakej
end do c_aibjckakej
end do e_aibjckakej
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdiaj: do d = n0d, n1d
c_aibjckdiaj: do c = n0c, n1c
if (c == d) cycle c_aibjckdiaj
k_aibjckdiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdiaj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdiaj
j_aibjckdiaj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckdiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjckdiaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckdiaj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdiaj
if (i > j .and. j > k) exit i_aibjckdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdiaj
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdiaj(b, c, k, d)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdiaj(b, c, k, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdiaj
end do a_aibjckdiaj
end do j_aibjckdiaj
end do b_aibjckdiaj
end do k_aibjckdiaj
end do c_aibjckdiaj
end do d_aibjckdiaj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k, j
! Equalities: e == a, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjckdiak: do d = n0d, n1d
c_aibjckdiak: do c = n0c, n1c
if (c == d) cycle c_aibjckdiak
k_aibjckdiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdiak: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdiak
j_aibjckdiak: do j = n0j, n1j
if (j == k) cycle j_aibjckdiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjckdiak: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckdiak: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdiak
if (i > j .and. j > k) exit i_aibjckdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdiak
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdiak(b, j, c, d)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdiak(b, j, c, d)
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdiak
end do a_aibjckdiak
end do j_aibjckdiak
end do b_aibjckdiak
end do k_aibjckdiak
end do c_aibjckdiak
end do d_aibjckdiak
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjckdjai: do d = n0d, n1d
c_aibjckdjai: do c = n0c, n1c
if (c == d) cycle c_aibjckdjai
k_aibjckdjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdjai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdjai
j_aibjckdjai: do j = n0jl, n1jl
if (j == k) cycle j_aibjckdjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjckdjai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjai
i_aibjckdjai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckdjai
if (i > j .and. j > k) exit i_aibjckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdjai
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdjai(b, c, k, d)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdjai(b, c, k, d)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdjai(b, c, k, d)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdjai(b, c, k, d)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdjai(b, c, k, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdjai
end do a_aibjckdjai
end do j_aibjckdjai
end do b_aibjckdjai
end do k_aibjckdjai
end do c_aibjckdjai
end do d_aibjckdjai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k, j
! Equalities: e == a, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkai: do d = n0d, n1d
c_aibjckdkai: do c = n0c, n1c
if (c == d) cycle c_aibjckdkai
k_aibjckdkai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdkai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdkai
j_aibjckdkai: do j = n0j, n1j
if (j == k) cycle j_aibjckdkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjckdkai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdkai
i_aibjckdkai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckdkai
if (i > j .and. j > k) exit i_aibjckdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdkai
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdkai(b, j, c, d)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdkai(b, j, c, d)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdkai(b, j, c, d)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdkai(b, j, c, d)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdkai(b, j, c, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdkai
end do a_aibjckdkai
end do j_aibjckdkai
end do b_aibjckdkai
end do k_aibjckdkai
end do c_aibjckdkai
end do d_aibjckdkai
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, k, i
! Equalities: e == a, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjckdjak: do d = n0d, n1d
c_aibjckdjak: do c = n0c, n1c
if (c == d) cycle c_aibjckdjak
k_aibjckdjak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdjak: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdjak
j_aibjckdjak: do j = n0jl, n1jl
if (j == k) cycle j_aibjckdjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjckdjak: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckdjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdjak
if (i > j .and. j > k) exit i_aibjckdjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdjak
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdjak(i, b, c, d)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdjak(i, b, c, d)
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdjak
end do a_aibjckdjak
end do j_aibjckdjak
end do b_aibjckdjak
end do k_aibjckdjak
end do c_aibjckdjak
end do d_aibjckdjak
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, k, i
! Equalities: e == a, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkaj: do d = n0d, n1d
c_aibjckdkaj: do c = n0c, n1c
if (c == d) cycle c_aibjckdkaj
k_aibjckdkaj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdkaj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdkaj
j_aibjckdkaj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckdkaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjckdkaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdkaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckdkaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdkaj
if (i > j .and. j > k) exit i_aibjckdkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdkaj
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdkaj(i, b, c, d)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdkaj(i, b, c, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdkaj
end do a_aibjckdkaj
end do j_aibjckdkaj
end do b_aibjckdkaj
end do k_aibjckdkaj
end do c_aibjckdkaj
end do d_aibjckdkaj
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k, m
! Equalities: d == b, e == c, l == i
! No equalities independent of the above can hold.
!
m_aibjckbicm: do m = n0m, n1m
c_aibjckbicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckbicm: do k = n0k, n1k
if (k == m) cycle k_aibjckbicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbicm: do b = b0, n1bd
if (b == c) cycle b_aibjckbicm
j_aibjckbicm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjckbicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbicm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbicm
i_aibjckbicm: do i = n0il, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjckbicm
if (i > j .and. j > k) exit i_aibjckbicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbicm
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbicm(a, j, k, m)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbicm(a, j, k, m)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbicm
end do a_aibjckbicm
end do j_aibjckbicm
end do b_aibjckbicm
end do k_aibjckbicm
end do c_aibjckbicm
end do m_aibjckbicm
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k, l
! Equalities: d == b, e == c, m == i
! No equalities independent of the above can hold.
!
l_aibjckblci: do l = n0l, n1l
c_aibjckblci: do c = n0ce, n1ce
k_aibjckblci: do k = n0k, n1k
if (k == l) cycle k_aibjckblci
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckblci: do b = b0, n1bd
if (b == c) cycle b_aibjckblci
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjckblci: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckblci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckblci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckblci
i_aibjckblci: do i = n0im, n1im
if (i == j .or. i == k .or. i == l) cycle i_aibjckblci
if (i > j .and. j > k) exit i_aibjckblci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckblci
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckblci(a, j, k, l)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckblci(a, j, k, l)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckblci
end do a_aibjckblci
end do j_aibjckblci
end do b_aibjckblci
end do k_aibjckblci
end do c_aibjckblci
end do l_aibjckblci
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, k, m
! Equalities: d == b, e == c, l == j
! No equalities independent of the above can hold.
!
m_aibjckbjcm: do m = n0m, n1m
c_aibjckbjcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckbjcm: do k = n0k, n1k
if (k == m) cycle k_aibjckbjcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjcm: do b = b0, n1bd
if (b == c) cycle b_aibjckbjcm
j_aibjckbjcm: do j = n0jl, n1jl
if (j == k .or. j == m) cycle j_aibjckbjcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjcm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbjcm
i_aibjckbjcm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckbjcm
if (i > j .and. j > k) exit i_aibjckbjcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbjcm
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbjcm(a, i, k, m)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbjcm(a, i, k, m)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbjcm(a, i, k, m)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbjcm(a, i, k, m)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbjcm(a, i, k, m)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjcm
end do a_aibjckbjcm
end do j_aibjckbjcm
end do b_aibjckbjcm
end do k_aibjckbjcm
end do c_aibjckbjcm
end do m_aibjckbjcm
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, k, l
! Equalities: d == b, e == c, m == j
! No equalities independent of the above can hold.
!
l_aibjckblcj: do l = n0l, n1l
c_aibjckblcj: do c = n0ce, n1ce
k_aibjckblcj: do k = n0k, n1k
if (k == l) cycle k_aibjckblcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckblcj: do b = b0, n1bd
if (b == c) cycle b_aibjckblcj
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjckblcj: do j = n0jm, n1jm
if (j == k .or. j == l) cycle j_aibjckblcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckblcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckblcj
i_aibjckblcj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckblcj
if (i > j .and. j > k) exit i_aibjckblcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckblcj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckblcj(a, i, k, l)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckblcj(a, i, k, l)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckblcj
end do a_aibjckblcj
end do j_aibjckblcj
end do b_aibjckblcj
end do k_aibjckblcj
end do c_aibjckblcj
end do l_aibjckblcj
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: k, i, j, m
! Equalities: d == b, e == c, l == k
! No equalities independent of the above can hold.
!
m_aibjckbkcm: do m = n0m, n1m
c_aibjckbkcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckbkcm: do k = n0kl, n1kl
if (k == m) cycle k_aibjckbkcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbkcm: do b = b0, n1bd
if (b == c) cycle b_aibjckbkcm
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkcm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjckbkcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbkcm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbkcm
i_aibjckbkcm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckbkcm
if (i > j .and. j > k) exit i_aibjckbkcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbkcm
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbkcm(a, i, j, m)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbkcm(a, i, j, m)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbkcm
end do a_aibjckbkcm
end do j_aibjckbkcm
end do b_aibjckbkcm
end do k_aibjckbkcm
end do c_aibjckbkcm
end do m_aibjckbkcm
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: k, i, j, l
! Equalities: d == b, e == c, m == k
! No equalities independent of the above can hold.
!
l_aibjckblck: do l = n0l, n1l
c_aibjckblck: do c = n0ce, n1ce
k_aibjckblck: do k = n0km, n1km
if (k == l) cycle k_aibjckblck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckblck: do b = b0, n1bd
if (b == c) cycle b_aibjckblck
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjckblck: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckblck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckblck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckblck
i_aibjckblck: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckblck
if (i > j .and. j > k) exit i_aibjckblck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckblck
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckblck(a, i, j, l)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckblck(a, i, j, l)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckblck(a, i, j, l)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckblck(a, i, j, l)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckblck(a, i, j, l)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckblck
end do a_aibjckblck
end do j_aibjckblck
end do b_aibjckblck
end do k_aibjckblck
end do c_aibjckblck
end do l_aibjckblck
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
end subroutine ccjac_32_dav_part1
end module ccjac_block_32_dav_part1
