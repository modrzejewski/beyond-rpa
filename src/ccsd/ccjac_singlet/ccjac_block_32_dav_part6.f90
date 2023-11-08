module ccjac_block_32_dav_part6
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
subroutine ccjac_32_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a, b, c, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, b0, i0, i1, j0, j1, k1
integer :: n0bcd, n0bcde, n0bd, n0bde, n0ce
integer :: n0ik, n0ikl, n0iklm, n0ikm, n0il
integer :: n0ilm, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jlm, n0jm, n0kl, n0klm
integer :: n0km
integer :: n1bcd, n1bcde, n1bd, n1bde, n1ce
integer :: n1ik, n1ikl, n1iklm, n1ikm, n1il
integer :: n1ilm, n1im, n1jk, n1jkl, n1jkm
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
n0bcd = max(n0b, n0c, n0d)
n0bcde = max(n0b, n0c, n0d, n0e)
n0bd = max(n0b, n0d)
n0bde = max(n0b, n0d, n0e)
n0ce = max(n0c, n0e)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0iklm = max(n0i, n0k, n0l, n0m)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0ilm = max(n0i, n0l, n0m)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1bcd = min(n1b, n1c, n1d)
n1bcde = min(n1b, n1c, n1d, n1e)
n1bd = min(n1b, n1d)
n1bde = min(n1b, n1d, n1e)
n1ce = min(n1c, n1e)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1iklm = min(n1i, n1k, n1l, n1m)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1ilm = min(n1i, n1l, n1m)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
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
! Free virtual indices: b, a
! Free occupied indices: k, i, j, l
! Equalities: c == b, d == b, e == b, m == k
! No equalities independent of the above can hold.
!
l_aibjbkblbk: do l = n0l, n1l
k1 = min(l - 1, n1km)
k_aibjbkblbk: do k = n0km, k1
if (k == l) cycle k_aibjbkblbk
b_aibjbkblbk: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkblbk: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjbkblbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkblbk: do a = a0, n1a
if (a == b) cycle a_aibjbkblbk
i1 = min(j - 1, n1i)
i_aibjbkblbk: do i = n0i, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkblbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkblbk(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkblbk
end do a_aibjbkblbk
end do j_aibjbkblbk
end do b_aibjbkblbk
end do k_aibjbkblbk
end do l_aibjbkblbk
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkbiej: do e = n0e, n1e
k_aibjbkbiej: do k = n0k, n1k
b0 = max(e + 1, n0bcd)
b_aibjbkbiej: do b = b0, n1bcd
if (b == e) cycle b_aibjbkbiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkbiej: do j = j0, n1jm
if (j == k) cycle j_aibjbkbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbiej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbkbiej
i1 = min(j - 1, n1il)
i_aibjbkbiej: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbiej(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbiej
end do a_aibjbkbiej
end do j_aibjbkbiej
end do b_aibjbkbiej
end do k_aibjbkbiej
end do e_aibjbkbiej
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k, j
! Equalities: c == b, d == b, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjbkbiek: do e = n0e, n1e
k_aibjbkbiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bcd)
b_aibjbkbiek: do b = b0, n1bcd
if (b == e) cycle b_aibjbkbiek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbiek: do j = j0, n1j
if (j == k) cycle j_aibjbkbiek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbiek: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbkbiek
i1 = min(j - 1, n1il)
i_aibjbkbiek: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbiek(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbiek
end do a_aibjbkbiek
end do j_aibjbkbiek
end do b_aibjbkbiek
end do k_aibjbkbiek
end do e_aibjbkbiek
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbkbjei: do e = n0e, n1e
k_aibjbkbjei: do k = n0k, n1k
b0 = max(e + 1, n0bcd)
b_aibjbkbjei: do b = b0, n1bcd
if (b == e) cycle b_aibjbkbjei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkbjei: do j = j0, n1jl
if (j == k) cycle j_aibjbkbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbjei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbkbjei
i1 = min(j - 1, n1im)
i_aibjbkbjei: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbjei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbjei
end do a_aibjbkbjei
end do j_aibjbkbjei
end do b_aibjbkbjei
end do k_aibjbkbjei
end do e_aibjbkbjei
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k, j
! Equalities: c == b, d == b, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjbkbkei: do e = n0e, n1e
k_aibjbkbkei: do k = n0kl, n1kl
b0 = max(e + 1, n0bcd)
b_aibjbkbkei: do b = b0, n1bcd
if (b == e) cycle b_aibjbkbkei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbkei: do j = j0, n1j
if (j == k) cycle j_aibjbkbkei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbkei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbkbkei
i1 = min(j - 1, n1im)
i_aibjbkbkei: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbkei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbkei
end do a_aibjbkbkei
end do j_aibjbkbkei
end do b_aibjbkbkei
end do k_aibjbkbkei
end do e_aibjbkbkei
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: j, k, i
! Equalities: c == b, d == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjbkbjek: do e = n0e, n1e
k_aibjbkbjek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bcd)
b_aibjbkbjek: do b = b0, n1bcd
if (b == e) cycle b_aibjbkbjek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkbjek: do j = j0, n1jl
if (j == k) cycle j_aibjbkbjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbjek: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbkbjek
i1 = min(j - 1, n1i)
i_aibjbkbjek: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkbjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbjek(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbjek
end do a_aibjbkbjek
end do j_aibjbkbjek
end do b_aibjbkbjek
end do k_aibjbkbjek
end do e_aibjbkbjek
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: j, k, i
! Equalities: c == b, d == b, m == j, l == k
! No equalities independent of the above can hold.
!
e_aibjbkbkej: do e = n0e, n1e
k_aibjbkbkej: do k = n0kl, n1kl
b0 = max(e + 1, n0bcd)
b_aibjbkbkej: do b = b0, n1bcd
if (b == e) cycle b_aibjbkbkej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkbkej: do j = j0, n1jm
if (j == k) cycle j_aibjbkbkej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbkej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbkbkej
i1 = min(j - 1, n1i)
i_aibjbkbkej: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkbkej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbkej(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbkej
end do a_aibjbkbkej
end do j_aibjbkbkej
end do b_aibjbkbkej
end do k_aibjbkbkej
end do e_aibjbkbkej
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckbibj: do c = n0c, n1c
k_aibjckbibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bde)
b_aibjckbibj: do b = b0, n1bde
if (b == c) cycle b_aibjckbibj
j_aibjckbibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckbibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbibj
i0 = max(j + 1, n0il)
i_aibjckbibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckbibj
if (i > j .and. j > k) exit i_aibjckbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbibj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbibj(a, b, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbibj(a, b, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbibj(a, b, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbibj(a, b, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbibj
end do a_aibjckbibj
end do j_aibjckbibj
end do b_aibjckbibj
end do k_aibjckbibj
end do c_aibjckbibj
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k, j
! Equalities: d == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckbibk: do c = n0c, n1c
k_aibjckbibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bde)
b_aibjckbibk: do b = b0, n1bde
if (b == c) cycle b_aibjckbibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbibk: do j = n0j, n1j
if (j == k) cycle j_aibjckbibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbibk: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbibk
i0 = max(k + 1, n0il)
i_aibjckbibk: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckbibk
if (i > j .and. j > k) exit i_aibjckbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbibk
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbibk(a, b, j, c)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbibk(a, b, j, c)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbibk(a, b, j, c)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbibk(a, b, j, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbibk
end do a_aibjckbibk
end do j_aibjckbibk
end do b_aibjckbibk
end do k_aibjckbibk
end do c_aibjckbibk
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckbjbi: do c = n0c, n1c
k_aibjckbjbi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bde)
b_aibjckbjbi: do b = b0, n1bde
if (b == c) cycle b_aibjckbjbi
j_aibjckbjbi: do j = n0jl, n1jl
if (j == k) cycle j_aibjckbjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbjbi
i1 = min(j - 1, n1im)
i_aibjckbjbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckbjbi
if (i > j .and. j > k) exit i_aibjckbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbjbi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbjbi(a, b, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbjbi(a, b, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbjbi(a, b, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbjbi(a, b, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjbi
end do a_aibjckbjbi
end do j_aibjckbjbi
end do b_aibjckbjbi
end do k_aibjckbjbi
end do c_aibjckbjbi
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k, j
! Equalities: d == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkbi: do c = n0c, n1c
k_aibjckbkbi: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bde)
b_aibjckbkbi: do b = b0, n1bde
if (b == c) cycle b_aibjckbkbi
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkbi: do j = n0j, n1j
if (j == k) cycle j_aibjckbkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbkbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbkbi
i1 = min(k - 1, n1im)
i_aibjckbkbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckbkbi
if (i > j .and. j > k) exit i_aibjckbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbkbi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbkbi(a, b, j, c)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbkbi(a, b, j, c)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbkbi(a, b, j, c)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbkbi(a, b, j, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbkbi
end do a_aibjckbkbi
end do j_aibjckbkbi
end do b_aibjckbkbi
end do k_aibjckbkbi
end do c_aibjckbkbi
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, k, i
! Equalities: d == b, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckbjbk: do c = n0c, n1c
k_aibjckbjbk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bde)
b_aibjckbjbk: do b = b0, n1bde
if (b == c) cycle b_aibjckbjbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjckbjbk: do j = j0, n1jl
if (j == k) cycle j_aibjckbjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjbk: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbjbk
i_aibjckbjbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbjbk
if (i > j .and. j > k) exit i_aibjckbjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbjbk
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbjbk(a, i, b, c)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbjbk(a, i, b, c)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbjbk(a, i, b, c)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbjbk(a, i, b, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjbk
end do a_aibjckbjbk
end do j_aibjckbjbk
end do b_aibjckbjbk
end do k_aibjckbjbk
end do c_aibjckbjbk
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, k, i
! Equalities: d == b, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkbj: do c = n0c, n1c
k_aibjckbkbj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bde)
b_aibjckbkbj: do b = b0, n1bde
if (b == c) cycle b_aibjckbkbj
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aibjckbkbj: do j = n0jm, j1
if (j == k) cycle j_aibjckbkbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbkbj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbkbj
i_aibjckbkbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbkbj
if (i > j .and. j > k) exit i_aibjckbkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbkbj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbkbj(a, i, b, c)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbkbj(a, i, b, c)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbkbj(a, i, b, c)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbkbj(a, i, b, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbkbj
end do a_aibjckbkbj
end do j_aibjckbkbj
end do b_aibjckbkbj
end do k_aibjckbkbj
end do c_aibjckbkbj
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: d == b, e == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjcibicm: do m = n0m, n1m
c_aibjcibicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjcibicm: do b = b0, n1bd
if (b == c) cycle b_aibjcibicm
j_aibjcibicm: do j = n0j, n1j
if (j == m) cycle j_aibjcibicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibicm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibicm
i_aibjcibicm: do i = n0ikl, n1ikl
if (i == j .or. i == m) cycle i_aibjcibicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibicm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibicm
end do a_aibjcibicm
end do j_aibjcibicm
end do b_aibjcibicm
end do c_aibjcibicm
end do m_aibjcibicm
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: d == b, e == c, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckbici: do c = n0ce, n1ce
k_aibjckbici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbici: do b = b0, n1bd
if (b == c) cycle b_aibjckbici
j_aibjckbici: do j = n0j, n1j
if (j == k) cycle j_aibjckbici
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbici: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbici
i_aibjckbici: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjckbici
if (i > j .and. j > k) exit i_aibjckbici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbici
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbici(a, i, j, k)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbici(a, i, j, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbici(a, i, j, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbici(a, i, j, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbici
end do a_aibjckbici
end do j_aibjckbici
end do b_aibjckbici
end do k_aibjckbici
end do c_aibjckbici
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: d == b, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckbicj: do c = n0ce, n1ce
k_aibjckbicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbicj: do b = b0, n1bd
if (b == c) cycle b_aibjckbicj
j_aibjckbicj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckbicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbicj
i_aibjckbicj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbicj
if (i > j .and. j > k) exit i_aibjckbicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbicj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbicj(a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbicj(a, i, b, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbicj(a, j, c, k)
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = ZERO
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbicj
end do a_aibjckbicj
end do j_aibjckbicj
end do b_aibjckbicj
end do k_aibjckbicj
end do c_aibjckbicj
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k, j
! Equalities: d == b, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckbick: do c = n0ce, n1ce
k_aibjckbick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbick: do b = b0, n1bd
if (b == c) cycle b_aibjckbick
j_aibjckbick: do j = n0j, n1j
if (j == k) cycle j_aibjckbick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbick: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbick
i_aibjckbick: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbick
if (i > j .and. j > k) exit i_aibjckbick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbick
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbick(a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbick(a, i, b, j)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbick(a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbick(a, i, b, j)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbick(a, i, b, j)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbick
end do a_aibjckbick
end do j_aibjckbick
end do b_aibjckbick
end do k_aibjckbick
end do c_aibjckbick
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: d == b, e == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjciblci: do l = n0l, n1l
c_aibjciblci: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjciblci: do b = b0, n1bd
if (b == c) cycle b_aibjciblci
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjciblci: do j = n0j, n1j
if (j == l) cycle j_aibjciblci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciblci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjciblci
i_aibjciblci: do i = n0ikm, n1ikm
if (i == j .or. i == l) cycle i_aibjciblci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciblci(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciblci
end do a_aibjciblci
end do j_aibjciblci
end do b_aibjciblci
end do c_aibjciblci
end do l_aibjciblci
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, m
! Equalities: d == b, e == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcibjcm: do m = n0m, n1m
c_aibjcibjcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjcibjcm: do b = b0, n1bd
if (b == c) cycle b_aibjcibjcm
j_aibjcibjcm: do j = n0jl, n1jl
if (j == m) cycle j_aibjcibjcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjcm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibjcm
i_aibjcibjcm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjcibjcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibjcm(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibjcm
end do a_aibjcibjcm
end do j_aibjcibjcm
end do b_aibjcibjcm
end do c_aibjcibjcm
end do m_aibjcibjcm
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: d == b, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjciblcj: do l = n0l, n1l
c_aibjciblcj: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjciblcj: do b = b0, n1bd
if (b == c) cycle b_aibjciblcj
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjciblcj: do j = n0jm, n1jm
if (j == l) cycle j_aibjciblcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciblcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjciblcj
i_aibjciblcj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjciblcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciblcj(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciblcj
end do a_aibjciblcj
end do j_aibjciblcj
end do b_aibjciblcj
end do c_aibjciblcj
end do l_aibjciblcj
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: d == b, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckbjci: do c = n0ce, n1ce
k_aibjckbjci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjci: do b = b0, n1bd
if (b == c) cycle b_aibjckbjci
j_aibjckbjci: do j = n0jl, n1jl
if (j == k) cycle j_aibjckbjci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbjci
i_aibjckbjci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbjci
if (i > j .and. j > k) exit i_aibjckbjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbjci
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbjci(a, i, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbjci(a, i, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbjci(a, i, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbjci(a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbjci(a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjci
end do a_aibjckbjci
end do j_aibjckbjci
end do b_aibjckbjci
end do k_aibjckbjci
end do c_aibjckbjci
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, l
! Equalities: d == b, e == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjblci: do l = n0l, n1l
c_aibjcjblci: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcjblci: do b = b0, n1bd
if (b == c) cycle b_aibjcjblci
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjblci: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjblci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjblci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjblci
i_aibjcjblci: do i = n0im, n1im
if (i == j .or. i == l) cycle i_aibjcjblci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjblci(a, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjblci
end do a_aibjcjblci
end do j_aibjcjblci
end do b_aibjcjblci
end do c_aibjcjblci
end do l_aibjcjblci
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k, j
! Equalities: d == b, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkci: do c = n0ce, n1ce
k_aibjckbkci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbkci: do b = b0, n1bd
if (b == c) cycle b_aibjckbkci
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkci: do j = n0j, n1j
if (j == k) cycle j_aibjckbkci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbkci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbkci
i_aibjckbkci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbkci
if (i > j .and. j > k) exit i_aibjckbkci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbkci
else if (k > i) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbkci(a, i, j, c)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbkci(a, b, j, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbkci(a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbkci
end do a_aibjckbkci
end do j_aibjckbkci
end do b_aibjckbkci
end do k_aibjckbkci
end do c_aibjckbkci
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, m
! Equalities: d == b, e == c, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjcjbjcm: do m = n0m, n1m
c_aibjcjbjcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjcjbjcm: do b = b0, n1bd
if (b == c) cycle b_aibjcjbjcm
j_aibjcjbjcm: do j = n0jkl, n1jkl
if (j == m) cycle j_aibjcjbjcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbjcm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbjcm
i_aibjcjbjcm: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjcjbjcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbjcm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbjcm
end do a_aibjcjbjcm
end do j_aibjcjbjcm
end do b_aibjcjbjcm
end do c_aibjcjbjcm
end do m_aibjcjbjcm
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, k
! Equalities: d == b, e == c, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckbjcj: do c = n0ce, n1ce
k_aibjckbjcj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjcj: do b = b0, n1bd
if (b == c) cycle b_aibjckbjcj
j_aibjckbjcj: do j = n0jlm, n1jlm
if (j == k) cycle j_aibjckbjcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbjcj
i_aibjckbjcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbjcj
if (i > j .and. j > k) exit i_aibjckbjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbjcj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbjcj(a, i, j, k)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbjcj(a, i, j, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbjcj(a, i, j, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbjcj(a, i, j, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjcj
end do a_aibjckbjcj
end do j_aibjckbjcj
end do b_aibjckbjcj
end do k_aibjckbjcj
end do c_aibjckbjcj
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, k, i
! Equalities: d == b, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckbjck: do c = n0ce, n1ce
k_aibjckbjck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjck: do b = b0, n1bd
if (b == c) cycle b_aibjckbjck
j_aibjckbjck: do j = n0jl, n1jl
if (j == k) cycle j_aibjckbjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbjck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbjck
i_aibjckbjck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbjck
if (i > j .and. j > k) exit i_aibjckbjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbjck
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbjck(a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbjck
end do a_aibjckbjck
end do j_aibjckbjck
end do b_aibjckbjck
end do k_aibjckbjck
end do c_aibjckbjck
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, l
! Equalities: d == b, e == c, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjcjblcj: do l = n0l, n1l
c_aibjcjblcj: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcjblcj: do b = b0, n1bd
if (b == c) cycle b_aibjcjblcj
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjblcj: do j = n0jkm, n1jkm
if (j == l) cycle j_aibjcjblcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjblcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjblcj
i_aibjcjblcj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjblcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjblcj(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjblcj
end do a_aibjcjblcj
end do j_aibjcjblcj
end do b_aibjcjblcj
end do c_aibjcjblcj
end do l_aibjcjblcj
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, k, i
! Equalities: d == b, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkcj: do c = n0ce, n1ce
k_aibjckbkcj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbkcj: do b = b0, n1bd
if (b == c) cycle b_aibjckbkcj
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkcj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckbkcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbkcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbkcj
i_aibjckbkcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbkcj
if (i > j .and. j > k) exit i_aibjckbkcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbkcj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbkcj(a, i, b, k)
else
jac_ibra_iket = v1_eom_cc3_32_trans_aibjckbkcj(a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = ZERO
else if (k > j) then
jac_ibra_iket = ZERO
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbkcj(a, i, j, c)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbkcj
end do a_aibjckbkcj
end do j_aibjckbkcj
end do b_aibjckbkcj
end do k_aibjckbkcj
end do c_aibjckbkcj
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: k, i, j
! Equalities: d == b, e == c, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckbkck: do c = n0ce, n1ce
k_aibjckbkck: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbkck: do b = b0, n1bd
if (b == c) cycle b_aibjckbkck
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbkck: do j = n0j, n1j
if (j == k) cycle j_aibjckbkck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbkck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbkck
i_aibjckbkck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbkck
if (i > j .and. j > k) exit i_aibjckbkck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
if (i > j) then
if (j > k) then
exit i_aibjckbkck
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_32_trans_aibjckbkck(a, i, j, k)
else
jac_ibra_iket = ZERO
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_32_trans_aibjckbkck(a, i, j, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_32_trans_aibjckbkck(a, i, j, k)
else
jac_ibra_iket = v4_eom_cc3_32_trans_aibjckbkck(a, i, j, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbkck
end do a_aibjckbkck
end do j_aibjckbkck
end do b_aibjckbkck
end do k_aibjckbkck
end do c_aibjckbkck
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjcibiei: do e = n0e, n1e
c_aibjcibiei: do c = n0c, n1c
if (c == e) cycle c_aibjcibiei
b0 = max(c + 1, e + 1, n0bd)
b_aibjcibiei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcibiei
j_aibjcibiei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibiei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibiei
i_aibjcibiei: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjcibiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibiei(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibiei
end do a_aibjcibiei
end do j_aibjcibiei
end do b_aibjcibiei
end do c_aibjcibiei
end do e_aibjcibiei
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
end subroutine ccjac_32_dav_part6
end module ccjac_block_32_dav_part6
