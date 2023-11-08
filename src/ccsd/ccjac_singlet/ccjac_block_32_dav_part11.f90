module ccjac_block_32_dav_part11
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
subroutine ccjac_32_dav_part11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, b0, i0, i1, k0, k1
integer :: n0ad, n0ae, n0bc, n0bcde, n0be
integer :: n0ce, n0ij, n0ijl, n0ijlm, n0ijm
integer :: n0ik, n0ikl, n0iklm, n0ikm, n0il
integer :: n0ilm, n0im, n0jk, n0jkl, n0jklm
integer :: n0jkm, n0jl, n0jlm, n0jm, n0kl
integer :: n0km
integer :: n1ad, n1ae, n1bc, n1bcde, n1be
integer :: n1ce, n1ij, n1ijl, n1ijlm, n1ijm
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
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bcde = max(n0b, n0c, n0d, n0e)
n0be = max(n0b, n0e)
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
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jklm = max(n0j, n0k, n0l, n0m)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bcde = min(n1b, n1c, n1d, n1e)
n1be = min(n1b, n1e)
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
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibibkaiek: do e = n0e, n1e
k_aibibkaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkaiek: do b = n0bc, n1bc
if (b == e) cycle b_aibibkaiek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibibkaiek: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibkaiek
i0 = max(k + 1, n0ijl)
i_aibibkaiek: do i = i0, n1ijl
if (i == k) cycle i_aibibkaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkaiek(i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkaiek
end do a_aibibkaiek
end do b_aibibkaiek
end do k_aibibkaiek
end do e_aibibkaiek
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibibkakei: do e = n0e, n1e
k_aibibkakei: do k = n0kl, n1kl
b_aibibkakei: do b = n0bc, n1bc
if (b == e) cycle b_aibibkakei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibibkakei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibkakei
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibibkakei: do i = i0, n1ijm
if (i == k) cycle i_aibibkakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkakei(i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkakei
end do a_aibibkakei
end do b_aibibkakei
end do k_aibibkakei
end do e_aibibkakei
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjbiaiei: do e = n0e, n1e
b_aibjbiaiei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbiaiei
j_aibjbiaiei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbiaiei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbiaiei
i1 = min(j - 1, n1iklm)
i_aibjbiaiei: do i = n0iklm, i1
if (i == j) cycle i_aibjbiaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiaiei(b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiaiei
end do a_aibjbiaiei
end do j_aibjbiaiei
end do b_aibjbiaiei
end do e_aibjbiaiei
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbiaiej: do e = n0e, n1e
b_aibjbiaiej: do b = n0bc, n1bc
if (b == e) cycle b_aibjbiaiej
j_aibjbiaiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbiaiej: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbiaiej
i1 = min(j - 1, n1ikl)
i_aibjbiaiej: do i = n0ikl, i1
if (i == j) cycle i_aibjbiaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiaiej(i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiaiej
end do a_aibjbiaiej
end do j_aibjbiaiej
end do b_aibjbiaiej
end do e_aibjbiaiej
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbiajei: do e = n0e, n1e
b_aibjbiajei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbiajei
j_aibjbiajei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbiajei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbiajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjbiajei: do i = n0ikm, i1
if (i == j) cycle i_aibjbiajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiajei(i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiajei
end do a_aibjbiajei
end do j_aibjbiajei
end do b_aibjbiajei
end do e_aibjbiajei
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjciaibi: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciaibi: do b = b0, n1be
if (b == c) cycle b_aibjciaibi
j_aibjciaibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciaibi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaibi
i_aibjciaibi: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjciaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaibi(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaibi
end do a_aibjciaibi
end do j_aibjciaibi
end do b_aibjciaibi
end do c_aibjciaibi
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaibj: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciaibj: do b = b0, n1be
if (b == c) cycle b_aibjciaibj
j_aibjciaibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciaibj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaibj
i_aibjciaibj: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjciaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaibj(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaibj
end do a_aibjciaibj
end do j_aibjciaibj
end do b_aibjciaibj
end do c_aibjciaibj
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjaibi: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcjaibi: do b = b0, n1be
if (b == c) cycle b_aibjcjaibi
j_aibjcjaibi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjaibi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaibi
i_aibjcjaibi: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjcjaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjaibi(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaibi
end do a_aibjcjaibi
end do j_aibjcjaibi
end do b_aibjcjaibi
end do c_aibjcjaibi
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaibj: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcjaibj: do b = b0, n1be
if (b == c) cycle b_aibjcjaibj
j_aibjcjaibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjaibj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaibj
i_aibjcjaibj: do i = n0il, n1il
if (i == j) cycle i_aibjcjaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjaibj(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaibj
end do a_aibjcjaibj
end do j_aibjcjaibj
end do b_aibjcjaibj
end do c_aibjcjaibj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajbi: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciajbi: do b = b0, n1be
if (b == c) cycle b_aibjciajbi
j_aibjciajbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciajbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbi: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjciajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciajbi(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajbi
end do a_aibjciajbi
end do j_aibjciajbi
end do b_aibjciajbi
end do c_aibjciajbi
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjciajbj: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciajbj: do b = b0, n1be
if (b == c) cycle b_aibjciajbj
j_aibjciajbj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciajbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbj: do i = n0ik, n1ik
if (i == j) cycle i_aibjciajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciajbj(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajbj
end do a_aibjciajbj
end do j_aibjciajbj
end do b_aibjciajbj
end do c_aibjciajbj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajbi: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcjajbi: do b = b0, n1be
if (b == c) cycle b_aibjcjajbi
j_aibjcjajbi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjajbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajbi: do i = n0im, n1im
if (i == j) cycle i_aibjcjajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajbi(i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajbi
end do a_aibjcjajbi
end do j_aibjcjajbi
end do b_aibjcjajbi
end do c_aibjcjajbi
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: d == a, e == b, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjajbj: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcjajbj: do b = b0, n1be
if (b == c) cycle b_aibjcjajbj
j_aibjcjajbj: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjajbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajbj: do i = n0i, n1i
if (i == j) cycle i_aibjcjajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajbj(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajbj
end do a_aibjcjajbj
end do j_aibjcjajbj
end do b_aibjcjajbj
end do c_aibjcjajbj
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjciaici: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjciaici: do b = b0, n1b
if (b == c) cycle b_aibjciaici
j_aibjciaici: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjciaici: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaici
i_aibjciaici: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjciaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaici(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaici
end do a_aibjciaici
end do j_aibjciaici
end do b_aibjciaici
end do c_aibjciaici
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaicj: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjciaicj: do b = b0, n1b
if (b == c) cycle b_aibjciaicj
j_aibjciaicj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjciaicj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaicj
i_aibjciaicj: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjciaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciaicj(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaicj
end do a_aibjciaicj
end do j_aibjciaicj
end do b_aibjciaicj
end do c_aibjciaicj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaicj: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjcjaicj: do b = b0, n1b
if (b == c) cycle b_aibjcjaicj
j_aibjcjaicj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjaicj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaicj
i_aibjcjaicj: do i = n0il, n1il
if (i == j) cycle i_aibjcjaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjaicj(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaicj
end do a_aibjcjaicj
end do j_aibjcjaicj
end do b_aibjcjaicj
end do c_aibjcjaicj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajci: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjciajci: do b = b0, n1b
if (b == c) cycle b_aibjciajci
j_aibjciajci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjciajci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajci
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajci: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjciajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciajci(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajci
end do a_aibjciajci
end do j_aibjciajci
end do b_aibjciajci
end do c_aibjciajci
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjciajcj: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjciajcj: do b = b0, n1b
if (b == c) cycle b_aibjciajcj
j_aibjciajcj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjciajcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajcj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajcj: do i = n0ik, n1ik
if (i == j) cycle i_aibjciajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjciajcj(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajcj
end do a_aibjciajcj
end do j_aibjciajcj
end do b_aibjciajcj
end do c_aibjciajcj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajci: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjcjajci: do b = b0, n1b
if (b == c) cycle b_aibjcjajci
j_aibjcjajci: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjajci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjajci
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajci: do i = n0im, n1im
if (i == j) cycle i_aibjcjajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajci(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajci
end do a_aibjcjajci
end do j_aibjcjajci
end do b_aibjcjajci
end do c_aibjcjajci
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i
! Equalities: d == a, e == c, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjajcj: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjcjajcj: do b = b0, n1b
if (b == c) cycle b_aibjcjajcj
j_aibjcjajcj: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjajcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjajcj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjajcj: do i = n0i, n1i
if (i == j) cycle i_aibjcjajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjajcj(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajcj
end do a_aibjcjajcj
end do j_aibjcjajcj
end do b_aibjcjajcj
end do c_aibjcjajcj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: e == a, c == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibibkdiai: do d = n0d, n1d
k_aibibkdiai: do k = n0k, n1k
b_aibibkdiai: do b = n0bc, n1bc
if (b == d) cycle b_aibibkdiai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibkdiai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibkdiai
i0 = max(k + 1, n0ijlm)
i_aibibkdiai: do i = i0, n1ijlm
if (i == k) cycle i_aibibkdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkdiai(b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdiai
end do a_aibibkdiai
end do b_aibibkdiai
end do k_aibibkdiai
end do d_aibibkdiai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: e == a, c == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibibkdiak: do d = n0d, n1d
k_aibibkdiak: do k = n0km, n1km
b_aibibkdiak: do b = n0bc, n1bc
if (b == d) cycle b_aibibkdiak
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibkdiak: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibkdiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibibkdiak: do i = i0, n1ijl
if (i == k) cycle i_aibibkdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkdiak(i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdiak
end do a_aibibkdiak
end do b_aibibkdiak
end do k_aibibkdiak
end do d_aibibkdiak
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: e == a, c == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibibkdkai: do d = n0d, n1d
k_aibibkdkai: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkdkai: do b = n0bc, n1bc
if (b == d) cycle b_aibibkdkai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibkdkai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibkdkai
i0 = max(k + 1, n0ijm)
i_aibibkdkai: do i = i0, n1ijm
if (i == k) cycle i_aibibkdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkdkai(i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdkai
end do a_aibibkdkai
end do b_aibibkdkai
end do k_aibibkdkai
end do d_aibibkdkai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjbidiai: do d = n0d, n1d
b_aibjbidiai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidiai
j_aibjbidiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidiai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidiai
i1 = min(j - 1, n1iklm)
i_aibjbidiai: do i = n0iklm, i1
if (i == j) cycle i_aibjbidiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbidiai(b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidiai
end do a_aibjbidiai
end do j_aibjbidiai
end do b_aibjbidiai
end do d_aibjbidiai
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbidiaj: do d = n0d, n1d
b_aibjbidiaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidiaj
j_aibjbidiaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidiaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aibjbidiaj: do i = n0ikl, i1
if (i == j) cycle i_aibjbidiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbidiaj(i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidiaj
end do a_aibjbidiaj
end do j_aibjbidiaj
end do b_aibjbidiaj
end do d_aibjbidiaj
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidjai: do d = n0d, n1d
b_aibjbidjai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidjai
j_aibjbidjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidjai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidjai
i1 = min(j - 1, n1ikm)
i_aibjbidjai: do i = n0ikm, i1
if (i == j) cycle i_aibjbidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbidjai(i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidjai
end do a_aibjbidjai
end do j_aibjbidjai
end do b_aibjbidjai
end do d_aibjbidjai
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, m
! Equalities: c == b, d == b, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibibkbibm: do m = n0m, n1m
k_aibibkbibm: do k = n0k, n1k
if (k == m) cycle k_aibibkbibm
b_aibibkbibm: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbibm: do a = a0, n1a
if (a == b) cycle a_aibibkbibm
i0 = max(k + 1, m + 1, n0ijl)
i_aibibkbibm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibibkbibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkbibm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbibm
end do a_aibibkbibm
end do b_aibibkbibm
end do k_aibibkbibm
end do m_aibibkbibm
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, l
! Equalities: c == b, d == b, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibibkblbi: do l = n0l, n1l
k_aibibkblbi: do k = n0k, n1k
if (k == l) cycle k_aibibkblbi
b_aibibkblbi: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkblbi: do a = a0, n1a
if (a == b) cycle a_aibibkblbi
i0 = max(k + 1, n0ijm)
i1 = min(l - 1, n1ijm)
i_aibibkblbi: do i = i0, i1
if (i == k .or. i == l) cycle i_aibibkblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkblbi(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkblbi
end do a_aibibkblbi
end do b_aibibkblbi
end do k_aibibkblbi
end do l_aibibkblbi
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, m
! Equalities: c == b, d == b, e == b, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibibkbkbm: do m = n0m, n1m
k0 = max(m + 1, n0kl)
k_aibibkbkbm: do k = k0, n1kl
if (k == m) cycle k_aibibkbkbm
b_aibibkbkbm: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbkbm: do a = a0, n1a
if (a == b) cycle a_aibibkbkbm
i0 = max(k + 1, n0ij)
i_aibibkbkbm: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aibibkbkbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkbkbm(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbkbm
end do a_aibibkbkbm
end do b_aibibkbkbm
end do k_aibibkbkbm
end do m_aibibkbkbm
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, l
! Equalities: c == b, d == b, e == b, j == i, m == k
! No equalities independent of the above can hold.
!
l_aibibkblbk: do l = n0l, n1l
k1 = min(l - 1, n1km)
k_aibibkblbk: do k = n0km, k1
if (k == l) cycle k_aibibkblbk
b_aibibkblbk: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkblbk: do a = a0, n1a
if (a == b) cycle a_aibibkblbk
i0 = max(k + 1, n0ij)
i_aibibkblbk: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibibkblbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkblbk(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkblbk
end do a_aibibkblbk
end do b_aibibkblbk
end do k_aibibkblbk
end do l_aibibkblbk
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
end subroutine ccjac_32_dav_part11
end module ccjac_block_32_dav_part11
