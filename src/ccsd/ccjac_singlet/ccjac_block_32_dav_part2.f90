module ccjac_block_32_dav_part2
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
subroutine ccjac_32_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, b0, b1, c0, c1, i0, i1, j0, j1, k0, k1
integer :: n0abde, n0bd, n0be, n0cd, n0ce
integer :: n0il, n0im, n0jl, n0jm, n0kl
integer :: n0km
integer :: n1abde, n1bd, n1be, n1cd, n1ce
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
n0abde = max(n0a, n0b, n0d, n0e)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0ce = max(n0c, n0e)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1abde = min(n1a, n1b, n1d, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
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
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, k
! Equalities: d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckbiej: do e = n0e, n1e
c_aibjckbiej: do c = n0c, n1c
if (c == e) cycle c_aibjckbiej
k_aibjckbiej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibjckbiej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbiej
j_aibjckbiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjckbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbiej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbiej
i_aibjckbiej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbiej
if (i > j .and. j > k) exit i_aibjckbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbiej
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbiej(a, c, k, e)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbiej(a, c, k, e)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbiej
end do a_aibjckbiej
end do j_aibjckbiej
end do b_aibjckbiej
end do k_aibjckbiej
end do c_aibjckbiej
end do e_aibjckbiej
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k, j
! Equalities: d == b, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjckbiek: do e = n0e, n1e
c_aibjckbiek: do c = n0c, n1c
if (c == e) cycle c_aibjckbiek
k_aibjckbiek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibjckbiek: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbiek
j_aibjckbiek: do j = n0j, n1j
if (j == k) cycle j_aibjckbiek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbiek: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbiek
i_aibjckbiek: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbiek
if (i > j .and. j > k) exit i_aibjckbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbiek
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbiek(a, j, c, e)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbiek(a, j, c, e)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbiek
end do a_aibjckbiek
end do j_aibjckbiek
end do b_aibjckbiek
end do k_aibjckbiek
end do c_aibjckbiek
end do e_aibjckbiek
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, k
! Equalities: d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckbjei: do e = n0e, n1e
c_aibjckbjei: do c = n0c, n1c
if (c == e) cycle c_aibjckbjei
k_aibjckbjei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibjckbjei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbjei
j_aibjckbjei: do j = n0jl, n1jl
if (j == k) cycle j_aibjckbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbjei
i_aibjckbjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbjei
if (i > j .and. j > k) exit i_aibjckbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbjei
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbjei(a, c, k, e)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbjei(a, c, k, e)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbjei(a, c, k, e)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbjei(a, c, k, e)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbjei(a, c, k, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjei
end do a_aibjckbjei
end do j_aibjckbjei
end do b_aibjckbjei
end do k_aibjckbjei
end do c_aibjckbjei
end do e_aibjckbjei
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k, j
! Equalities: d == b, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjckbkei: do e = n0e, n1e
c_aibjckbkei: do c = n0c, n1c
if (c == e) cycle c_aibjckbkei
k_aibjckbkei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibjckbkei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbkei
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkei: do j = n0j, n1j
if (j == k) cycle j_aibjckbkei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbkei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbkei
i_aibjckbkei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbkei
if (i > j .and. j > k) exit i_aibjckbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbkei
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbkei(a, j, c, e)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbkei(a, j, c, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbkei
end do a_aibjckbkei
end do j_aibjckbkei
end do b_aibjckbkei
end do k_aibjckbkei
end do c_aibjckbkei
end do e_aibjckbkei
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: j, k, i
! Equalities: d == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjckbjek: do e = n0e, n1e
c_aibjckbjek: do c = n0c, n1c
if (c == e) cycle c_aibjckbjek
k_aibjckbjek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibjckbjek: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbjek
j_aibjckbjek: do j = n0jl, n1jl
if (j == k) cycle j_aibjckbjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjek: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbjek
i_aibjckbjek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbjek
if (i > j .and. j > k) exit i_aibjckbjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbjek
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbjek(a, i, c, e)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbjek(a, i, c, e)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbjek(a, i, c, e)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbjek(a, i, c, e)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbjek(a, i, c, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjek
end do a_aibjckbjek
end do j_aibjckbjek
end do b_aibjckbjek
end do k_aibjckbjek
end do c_aibjckbjek
end do e_aibjckbjek
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: j, k, i
! Equalities: d == b, m == j, l == k
! No equalities independent of the above can hold.
!
e_aibjckbkej: do e = n0e, n1e
c_aibjckbkej: do c = n0c, n1c
if (c == e) cycle c_aibjckbkej
k_aibjckbkej: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibjckbkej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbkej
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkej: do j = n0jm, n1jm
if (j == k) cycle j_aibjckbkej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbkej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbkej
i_aibjckbkej: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbkej
if (i > j .and. j > k) exit i_aibjckbkej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbkej
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbkej(a, i, c, e)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbkej(a, i, c, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbkej
end do a_aibjckbkej
end do j_aibjckbkej
end do b_aibjckbkej
end do k_aibjckbkej
end do c_aibjckbkej
end do e_aibjckbkej
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, k
! Equalities: e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdibj: do d = n0d, n1d
c_aibjckdibj: do c = n0c, n1c
if (c == d) cycle c_aibjckdibj
k_aibjckdibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjckdibj: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjckdibj
j_aibjckdibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdibj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdibj
i_aibjckdibj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdibj
if (i > j .and. j > k) exit i_aibjckdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdibj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdibj(a, c, k, d)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdibj(a, c, k, d)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdibj(a, c, k, d)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdibj(a, c, k, d)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdibj(a, c, k, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdibj
end do a_aibjckdibj
end do j_aibjckdibj
end do b_aibjckdibj
end do k_aibjckdibj
end do c_aibjckdibj
end do d_aibjckdibj
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k, j
! Equalities: e == b, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjckdibk: do d = n0d, n1d
c_aibjckdibk: do c = n0c, n1c
if (c == d) cycle c_aibjckdibk
k_aibjckdibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjckdibk: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjckdibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckdibk: do j = n0j, n1j
if (j == k) cycle j_aibjckdibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdibk: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdibk
i_aibjckdibk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdibk
if (i > j .and. j > k) exit i_aibjckdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdibk
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdibk(a, j, c, d)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdibk(a, j, c, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdibk
end do a_aibjckdibk
end do j_aibjckdibk
end do b_aibjckdibk
end do k_aibjckdibk
end do c_aibjckdibk
end do d_aibjckdibk
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, k
! Equalities: e == b, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjckdjbi: do d = n0d, n1d
c_aibjckdjbi: do c = n0c, n1c
if (c == d) cycle c_aibjckdjbi
k_aibjckdjbi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjckdjbi: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjckdjbi
j_aibjckdjbi: do j = n0jl, n1jl
if (j == k) cycle j_aibjckdjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdjbi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjbi
i_aibjckdjbi: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckdjbi
if (i > j .and. j > k) exit i_aibjckdjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdjbi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdjbi(a, c, k, d)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdjbi(a, c, k, d)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdjbi
end do a_aibjckdjbi
end do j_aibjckdjbi
end do b_aibjckdjbi
end do k_aibjckdjbi
end do c_aibjckdjbi
end do d_aibjckdjbi
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k, j
! Equalities: e == b, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkbi: do d = n0d, n1d
c_aibjckdkbi: do c = n0c, n1c
if (c == d) cycle c_aibjckdkbi
k_aibjckdkbi: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjckdkbi: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjckdkbi
j_aibjckdkbi: do j = n0j, n1j
if (j == k) cycle j_aibjckdkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdkbi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdkbi
i_aibjckdkbi: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckdkbi
if (i > j .and. j > k) exit i_aibjckdkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdkbi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdkbi(a, j, c, d)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdkbi(a, j, c, d)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdkbi
end do a_aibjckdkbi
end do j_aibjckdkbi
end do b_aibjckdkbi
end do k_aibjckdkbi
end do c_aibjckdkbi
end do d_aibjckdkbi
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: j, k, i
! Equalities: e == b, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjckdjbk: do d = n0d, n1d
c_aibjckdjbk: do c = n0c, n1c
if (c == d) cycle c_aibjckdjbk
k_aibjckdjbk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjckdjbk: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjckdjbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckdjbk: do j = n0jl, n1jl
if (j == k) cycle j_aibjckdjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdjbk: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjbk
i_aibjckdjbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdjbk
if (i > j .and. j > k) exit i_aibjckdjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdjbk
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdjbk(a, i, c, d)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdjbk(a, i, c, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdjbk
end do a_aibjckdjbk
end do j_aibjckdjbk
end do b_aibjckdjbk
end do k_aibjckdjbk
end do c_aibjckdjbk
end do d_aibjckdjbk
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: j, k, i
! Equalities: e == b, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkbj: do d = n0d, n1d
c_aibjckdkbj: do c = n0c, n1c
if (c == d) cycle c_aibjckdkbj
k_aibjckdkbj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjckdkbj: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjckdkbj
j_aibjckdkbj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckdkbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdkbj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdkbj
i_aibjckdkbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdkbj
if (i > j .and. j > k) exit i_aibjckdkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdkbj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdkbj(a, i, c, d)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdkbj(a, i, c, d)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdkbj(a, i, c, d)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdkbj(a, i, c, d)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdkbj(a, i, c, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdkbj
end do a_aibjckdkbj
end do j_aibjckdkbj
end do b_aibjckdkbj
end do k_aibjckdkbj
end do c_aibjckdkbj
end do d_aibjckdkbj
!
! Elementary loop  13
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j, k
! Equalities: d == c, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckciej: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjckciej: do c = c0, n1cd
if (c == e) cycle c_aibjckciej
k_aibjckciej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckciej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckciej
j_aibjckciej: do j = n0jm, n1jm
if (j == k) cycle j_aibjckciej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckciej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckciej
i_aibjckciej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckciej
if (i > j .and. j > k) exit i_aibjckciej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckciej
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckciej(a, b, k, e)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckciej(a, b, k, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckciej
end do a_aibjckciej
end do j_aibjckciej
end do b_aibjckciej
end do k_aibjckciej
end do c_aibjckciej
end do e_aibjckciej
!
! Elementary loop  14
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, k, j
! Equalities: d == c, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjckciek: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjckciek: do c = c0, n1cd
if (c == e) cycle c_aibjckciek
k_aibjckciek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckciek: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckciek
j_aibjckciek: do j = n0j, n1j
if (j == k) cycle j_aibjckciek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckciek: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckciek
i_aibjckciek: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckciek
if (i > j .and. j > k) exit i_aibjckciek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckciek
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckciek(a, b, j, e)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckciek(a, b, j, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckciek
end do a_aibjckciek
end do j_aibjckciek
end do b_aibjckciek
end do k_aibjckciek
end do c_aibjckciek
end do e_aibjckciek
!
! Elementary loop  15
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j, k
! Equalities: d == c, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckcjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjckcjei: do c = c0, n1cd
if (c == e) cycle c_aibjckcjei
k_aibjckcjei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckcjei
j_aibjckcjei: do j = n0jl, n1jl
if (j == k) cycle j_aibjckcjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcjei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckcjei
i_aibjckcjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckcjei
if (i > j .and. j > k) exit i_aibjckcjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckcjei
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckcjei(a, b, k, e)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckcjei(a, b, k, e)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcjei
end do a_aibjckcjei
end do j_aibjckcjei
end do b_aibjckcjei
end do k_aibjckcjei
end do c_aibjckcjei
end do e_aibjckcjei
!
! Elementary loop  16
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, k, j
! Equalities: d == c, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjckckei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjckckei: do c = c0, n1cd
if (c == e) cycle c_aibjckckei
k_aibjckckei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckckei
j_aibjckckei: do j = n0j, n1j
if (j == k) cycle j_aibjckckei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckckei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckckei
i_aibjckckei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckckei
if (i > j .and. j > k) exit i_aibjckckei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckckei
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckckei(a, b, j, e)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckckei(a, b, j, e)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckckei(a, b, j, e)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckckei(a, b, j, e)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckckei(a, b, j, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckckei
end do a_aibjckckei
end do j_aibjckckei
end do b_aibjckckei
end do k_aibjckckei
end do c_aibjckckei
end do e_aibjckckei
!
! Elementary loop  17
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: j, k, i
! Equalities: d == c, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjckcjek: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjckcjek: do c = c0, n1cd
if (c == e) cycle c_aibjckcjek
k_aibjckcjek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjek: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckcjek
j_aibjckcjek: do j = n0jl, n1jl
if (j == k) cycle j_aibjckcjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcjek: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckcjek
i_aibjckcjek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckcjek
if (i > j .and. j > k) exit i_aibjckcjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckcjek
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckcjek(a, i, b, e)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckcjek(a, i, b, e)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcjek
end do a_aibjckcjek
end do j_aibjckcjek
end do b_aibjckcjek
end do k_aibjckcjek
end do c_aibjckcjek
end do e_aibjckcjek
!
! Elementary loop  18
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: j, k, i
! Equalities: d == c, m == j, l == k
! No equalities independent of the above can hold.
!
e_aibjckckej: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjckckej: do c = c0, n1cd
if (c == e) cycle c_aibjckckej
k_aibjckckej: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckckej
j_aibjckckej: do j = n0jm, n1jm
if (j == k) cycle j_aibjckckej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckckej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckckej
i_aibjckckej: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckckej
if (i > j .and. j > k) exit i_aibjckckej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckckej
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckckej(a, i, b, e)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckckej(a, i, b, e)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckckej(a, i, b, e)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckckej(a, i, b, e)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckckej(a, i, b, e)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckckej
end do a_aibjckckej
end do j_aibjckckej
end do b_aibjckckej
end do k_aibjckckej
end do c_aibjckckej
end do e_aibjckckej
!
! Elementary loop  19
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j, k
! Equalities: e == c, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjckdicj: do c = n0ce, c1
if (c == d) cycle c_aibjckdicj
k_aibjckdicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdicj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdicj
j_aibjckdicj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckdicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdicj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdicj
i_aibjckdicj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdicj
if (i > j .and. j > k) exit i_aibjckdicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdicj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdicj(a, b, k, d)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdicj(a, b, k, d)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdicj
end do a_aibjckdicj
end do j_aibjckdicj
end do b_aibjckdicj
end do k_aibjckdicj
end do c_aibjckdicj
end do d_aibjckdicj
!
! Elementary loop  20
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, k, j
! Equalities: e == c, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjckdick: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjckdick: do c = n0ce, c1
if (c == d) cycle c_aibjckdick
k_aibjckdick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdick: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdick
j_aibjckdick: do j = n0j, n1j
if (j == k) cycle j_aibjckdick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdick: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdick
i_aibjckdick: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdick
if (i > j .and. j > k) exit i_aibjckdick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdick
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdick(a, b, j, d)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdick(a, b, j, d)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdick(a, b, j, d)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdick(a, b, j, d)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdick(a, b, j, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdick
end do a_aibjckdick
end do j_aibjckdick
end do b_aibjckdick
end do k_aibjckdick
end do c_aibjckdick
end do d_aibjckdick
!
! Elementary loop  21
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j, k
! Equalities: e == c, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjckdjci: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjckdjci: do c = n0ce, c1
if (c == d) cycle c_aibjckdjci
k_aibjckdjci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdjci: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdjci
j_aibjckdjci: do j = n0jl, n1jl
if (j == k) cycle j_aibjckdjci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdjci: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjci
i_aibjckdjci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckdjci
if (i > j .and. j > k) exit i_aibjckdjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdjci
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdjci(a, b, k, d)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdjci(a, b, k, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdjci
end do a_aibjckdjci
end do j_aibjckdjci
end do b_aibjckdjci
end do k_aibjckdjci
end do c_aibjckdjci
end do d_aibjckdjci
!
! Elementary loop  22
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, k, j
! Equalities: e == c, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkci: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjckdkci: do c = n0ce, c1
if (c == d) cycle c_aibjckdkci
k_aibjckdkci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdkci: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdkci
j_aibjckdkci: do j = n0j, n1j
if (j == k) cycle j_aibjckdkci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdkci: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdkci
i_aibjckdkci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckdkci
if (i > j .and. j > k) exit i_aibjckdkci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdkci
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdkci(a, b, j, d)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdkci(a, b, j, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdkci
end do a_aibjckdkci
end do j_aibjckdkci
end do b_aibjckdkci
end do k_aibjckdkci
end do c_aibjckdkci
end do d_aibjckdkci
!
! Elementary loop  23
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: j, k, i
! Equalities: e == c, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjckdjck: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjckdjck: do c = n0ce, c1
if (c == d) cycle c_aibjckdjck
k_aibjckdjck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdjck: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdjck
j_aibjckdjck: do j = n0jl, n1jl
if (j == k) cycle j_aibjckdjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdjck: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjck
i_aibjckdjck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdjck
if (i > j .and. j > k) exit i_aibjckdjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdjck
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdjck(a, i, b, d)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdjck(a, i, b, d)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckdjck(a, i, b, d)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckdjck(a, i, b, d)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckdjck(a, i, b, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdjck
end do a_aibjckdjck
end do j_aibjckdjck
end do b_aibjckdjck
end do k_aibjckdjck
end do c_aibjckdjck
end do d_aibjckdjck
!
! Elementary loop  24
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: j, k, i
! Equalities: e == c, m == j, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkcj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjckdkcj: do c = n0ce, c1
if (c == d) cycle c_aibjckdkcj
k_aibjckdkcj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdkcj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdkcj
j_aibjckdkcj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckdkcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdkcj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdkcj
i_aibjckdkcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdkcj
if (i > j .and. j > k) exit i_aibjckdkcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckdkcj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckdkcj(a, i, b, d)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckdkcj(a, i, b, d)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdkcj
end do a_aibjckdkcj
end do j_aibjckdkcj
end do b_aibjckdkcj
end do k_aibjckdkcj
end do c_aibjckdkcj
end do d_aibjckdkcj
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, m
! Equalities: b == a, d == a, e == a, l == i
! No equalities independent of the above can hold.
!
m_aiajckaiam: do m = n0m, n1m
c_aiajckaiam: do c = n0c, n1c
k_aiajckaiam: do k = n0k, n1k
if (k == m) cycle k_aiajckaiam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckaiam: do j = n0j, j1
if (j == k .or. j == m) cycle j_aiajckaiam
a0 = max(c + 1, n0abde)
a_aiajckaiam: do a = a0, n1abde
if (a == c) cycle a_aiajckaiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajckaiam: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aiajckaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckaiam(j, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiam
end do a_aiajckaiam
end do j_aiajckaiam
end do k_aiajckaiam
end do c_aiajckaiam
end do m_aiajckaiam
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == a, e == a, m == i
! No equalities independent of the above can hold.
!
l_aiajckalai: do l = n0l, n1l
c_aiajckalai: do c = n0c, n1c
k_aiajckalai: do k = n0k, n1k
if (k == l) cycle k_aiajckalai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckalai: do j = n0j, j1
if (j == k .or. j == l) cycle j_aiajckalai
a0 = max(c + 1, n0abde)
a_aiajckalai: do a = a0, n1abde
if (a == c) cycle a_aiajckalai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajckalai: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aiajckalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckalai(j, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckalai
end do a_aiajckalai
end do j_aiajckalai
end do k_aiajckalai
end do c_aiajckalai
end do l_aiajckalai
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, m
! Equalities: b == a, d == a, e == a, l == j
! No equalities independent of the above can hold.
!
m_aiajckajam: do m = n0m, n1m
c_aiajckajam: do c = n0c, n1c
k_aiajckajam: do k = n0k, n1k
if (k == m) cycle k_aiajckajam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(m + 1, n0jl)
j1 = min(k - 1, n1jl)
j_aiajckajam: do j = j0, j1
if (j == k .or. j == m) cycle j_aiajckajam
a0 = max(c + 1, n0abde)
a_aiajckajam: do a = a0, n1abde
if (a == c) cycle a_aiajckajam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckajam: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajckajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckajam(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajam
end do a_aiajckajam
end do j_aiajckajam
end do k_aiajckajam
end do c_aiajckajam
end do m_aiajckajam
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, l
! Equalities: b == a, d == a, e == a, m == j
! No equalities independent of the above can hold.
!
l_aiajckalaj: do l = n0l, n1l
c_aiajckalaj: do c = n0c, n1c
k_aiajckalaj: do k = n0k, n1k
if (k == l) cycle k_aiajckalaj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(l - 1, k - 1, n1jm)
j_aiajckalaj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aiajckalaj
a0 = max(c + 1, n0abde)
a_aiajckalaj: do a = a0, n1abde
if (a == c) cycle a_aiajckalaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckalaj: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckalaj(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckalaj
end do a_aiajckalaj
end do j_aiajckalaj
end do k_aiajckalaj
end do c_aiajckalaj
end do l_aiajckalaj
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, m
! Equalities: b == a, d == a, e == a, l == k
! No equalities independent of the above can hold.
!
m_aiajckakam: do m = n0m, n1m
c_aiajckakam: do c = n0c, n1c
k0 = max(m + 1, n0kl)
k_aiajckakam: do k = k0, n1kl
if (k == m) cycle k_aiajckakam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckakam: do j = n0j, j1
if (j == k .or. j == m) cycle j_aiajckakam
a0 = max(c + 1, n0abde)
a_aiajckakam: do a = a0, n1abde
if (a == c) cycle a_aiajckakam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckakam: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajckakam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckakam(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakam
end do a_aiajckakam
end do j_aiajckakam
end do k_aiajckakam
end do c_aiajckakam
end do m_aiajckakam
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, l
! Equalities: b == a, d == a, e == a, m == k
! No equalities independent of the above can hold.
!
l_aiajckalak: do l = n0l, n1l
c_aiajckalak: do c = n0c, n1c
k1 = min(l - 1, n1km)
k_aiajckalak: do k = n0km, k1
if (k == l) cycle k_aiajckalak
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckalak: do j = n0j, j1
if (j == k .or. j == l) cycle j_aiajckalak
a0 = max(c + 1, n0abde)
a_aiajckalak: do a = a0, n1abde
if (a == c) cycle a_aiajckalak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckalak: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aiajckalak(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckalak
end do a_aiajckalak
end do j_aiajckalak
end do k_aiajckalak
end do c_aiajckalak
end do l_aiajckalak
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
end subroutine ccjac_32_dav_part2
end module ccjac_block_32_dav_part2
