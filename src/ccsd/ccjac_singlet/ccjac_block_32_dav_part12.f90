module ccjac_block_32_dav_part12
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
subroutine ccjac_32_dav_part12(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, b0, i0, i1, j0, j1
integer :: n0bcd, n0bcde, n0bd, n0bde, n0ce
integer :: n0ijl, n0ijlm, n0ijm, n0ik, n0ikl
integer :: n0iklm, n0ikm, n0il, n0ilm, n0im
integer :: n0jk, n0jkl, n0jklm, n0jkm, n0jl
integer :: n0jlm, n0jm, n0kl, n0klm, n0km
integer :: n1bcd, n1bcde, n1bd, n1bde, n1ce
integer :: n1ijl, n1ijlm, n1ijm, n1ik, n1ikl
integer :: n1iklm, n1ikm, n1il, n1ilm, n1im
integer :: n1jk, n1jkl, n1jklm, n1jkm, n1jl
integer :: n1jlm, n1jm, n1kl, n1klm, n1km
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
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1bcd = min(n1b, n1c, n1d)
n1bcde = min(n1b, n1c, n1d, n1e)
n1bd = min(n1b, n1d)
n1bde = min(n1b, n1d, n1e)
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
n1klm = min(n1k, n1l, n1m)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjbibibm: do m = n0m, n1m
b_aibjbibibm: do b = n0bcde, n1bcde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbibibm: do j = n0j, n1j
if (j == m) cycle j_aibjbibibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibibm: do a = a0, n1a
if (a == b) cycle a_aibjbibibm
i0 = max(m + 1, n0ikl)
i1 = min(j - 1, n1ikl)
i_aibjbibibm: do i = i0, i1
if (i == j .or. i == m) cycle i_aibjbibibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbibibm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibibm
end do a_aibjbibibm
end do j_aibjbibibm
end do b_aibjbibibm
end do m_aibjbibibm
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibjbkbibi: do k = n0k, n1k
b_aibjbkbibi: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbibi: do j = j0, n1j
if (j == k) cycle j_aibjbkbibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbibi: do a = a0, n1a
if (a == b) cycle a_aibjbkbibi
i1 = min(j - 1, n1ilm)
i_aibjbkbibi: do i = n0ilm, i1
if (i == j .or. i == k) cycle i_aibjbkbibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbibi(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbibi
end do a_aibjbkbibi
end do j_aibjbkbibi
end do b_aibjbkbibi
end do k_aibjbkbibi
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjbkbibj: do k = n0k, n1k
b_aibjbkbibj: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjbkbibj: do j = j0, n1jm
if (j == k) cycle j_aibjbkbibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbibj: do a = a0, n1a
if (a == b) cycle a_aibjbkbibj
i0 = max(j + 1, n0il)
i1 = min(j - 1, n1il)
i_aibjbkbibj: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjbkbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbibj(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbibj
end do a_aibjbkbibj
end do j_aibjbkbibj
end do b_aibjbkbibj
end do k_aibjbkbibj
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, j
! Equalities: c == b, d == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjbkbibk: do k = n0km, n1km
b_aibjbkbibk: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbibk: do j = j0, n1j
if (j == k) cycle j_aibjbkbibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbibk: do a = a0, n1a
if (a == b) cycle a_aibjbkbibk
i0 = max(k + 1, n0il)
i1 = min(j - 1, n1il)
i_aibjbkbibk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjbkbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbibk(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbibk
end do a_aibjbkbibk
end do j_aibjbkbibk
end do b_aibjbkbibk
end do k_aibjbkbibk
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjbiblbi: do l = n0l, n1l
b_aibjbiblbi: do b = n0bcde, n1bcde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbiblbi: do j = n0j, n1j
if (j == l) cycle j_aibjbiblbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbiblbi: do a = a0, n1a
if (a == b) cycle a_aibjbiblbi
i1 = min(j - 1, l - 1, n1ikm)
i_aibjbiblbi: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjbiblbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiblbi(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiblbi
end do a_aibjbiblbi
end do j_aibjbiblbi
end do b_aibjbiblbi
end do l_aibjbiblbi
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, m
! Equalities: c == b, d == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjbibjbm: do m = n0m, n1m
b_aibjbibjbm: do b = n0bcde, n1bcde
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjbibjbm: do j = j0, n1jl
if (j == m) cycle j_aibjbibjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibjbm: do a = a0, n1a
if (a == b) cycle a_aibjbibjbm
i1 = min(j - 1, n1ik)
i_aibjbibjbm: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjbibjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbibjbm(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibjbm
end do a_aibjbibjbm
end do j_aibjbibjbm
end do b_aibjbibjbm
end do m_aibjbibjbm
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjbiblbj: do l = n0l, n1l
b_aibjbiblbj: do b = n0bcde, n1bcde
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjbiblbj: do j = n0jm, j1
if (j == l) cycle j_aibjbiblbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbiblbj: do a = a0, n1a
if (a == b) cycle a_aibjbiblbj
i1 = min(j - 1, n1ik)
i_aibjbiblbj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjbiblbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbiblbj(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbiblbj
end do a_aibjbiblbj
end do j_aibjbiblbj
end do b_aibjbiblbj
end do l_aibjbiblbj
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjbkbjbi: do k = n0k, n1k
b_aibjbkbjbi: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkbjbi: do j = j0, n1jl
if (j == k) cycle j_aibjbkbjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbjbi: do a = a0, n1a
if (a == b) cycle a_aibjbkbjbi
i1 = min(j - 1, n1im)
i_aibjbkbjbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbjbi(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbjbi
end do a_aibjbkbjbi
end do j_aibjbkbjbi
end do b_aibjbkbjbi
end do k_aibjbkbjbi
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, j
! Equalities: c == b, d == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjbkbkbi: do k = n0kl, n1kl
b_aibjbkbkbi: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbkbi: do j = j0, n1j
if (j == k) cycle j_aibjbkbkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbkbi: do a = a0, n1a
if (a == b) cycle a_aibjbkbkbi
i1 = min(k - 1, j - 1, n1im)
i_aibjbkbkbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbkbi(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbkbi
end do a_aibjbkbkbi
end do j_aibjbkbkbi
end do b_aibjbkbkbi
end do k_aibjbkbkbi
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k
! Equalities: c == b, d == b, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
k_aibjbkbjbj: do k = n0k, n1k
b_aibjbkbjbj: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jlm)
j_aibjbkbjbj: do j = j0, n1jlm
if (j == k) cycle j_aibjbkbjbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbjbj: do a = a0, n1a
if (a == b) cycle a_aibjbkbjbj
i1 = min(j - 1, n1i)
i_aibjbkbjbj: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkbjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbjbj(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbjbj
end do a_aibjbkbjbj
end do j_aibjbkbjbj
end do b_aibjbkbjbj
end do k_aibjbkbjbj
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, k, i
! Equalities: c == b, d == b, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjbkbjbk: do k = n0km, n1km
b_aibjbkbjbk: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkbjbk: do j = j0, n1jl
if (j == k) cycle j_aibjbkbjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbjbk: do a = a0, n1a
if (a == b) cycle a_aibjbkbjbk
i1 = min(j - 1, n1i)
i_aibjbkbjbk: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkbjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbjbk(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbjbk
end do a_aibjbkbjbk
end do j_aibjbkbjbk
end do b_aibjbkbjbk
end do k_aibjbkbjbk
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a
! Free occupied indices: k, i, j
! Equalities: c == b, d == b, e == b, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibjbkbkbk: do k = n0klm, n1klm
b_aibjbkbkbk: do b = n0bcde, n1bcde
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbkbk: do j = j0, n1j
if (j == k) cycle j_aibjbkbkbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbkbk: do a = a0, n1a
if (a == b) cycle a_aibjbkbkbk
i1 = min(j - 1, n1i)
i_aibjbkbkbk: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkbkbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbkbkbk(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbkbk
end do a_aibjbkbkbk
end do j_aibjbkbkbk
end do b_aibjbkbkbk
end do k_aibjbkbkbk
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibibkbiei: do e = n0e, n1e
k_aibibkbiei: do k = n0k, n1k
b0 = max(e + 1, n0bcd)
b_aibibkbiei: do b = b0, n1bcd
if (b == e) cycle b_aibibkbiei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbiei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibibkbiei
i0 = max(k + 1, n0ijlm)
i_aibibkbiei: do i = i0, n1ijlm
if (i == k) cycle i_aibibkbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkbiei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbiei
end do a_aibibkbiei
end do b_aibibkbiei
end do k_aibibkbiei
end do e_aibibkbiei
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibibkbiek: do e = n0e, n1e
k_aibibkbiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bcd)
b_aibibkbiek: do b = b0, n1bcd
if (b == e) cycle b_aibibkbiek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbiek: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibibkbiek
i0 = max(k + 1, n0ijl)
i_aibibkbiek: do i = i0, n1ijl
if (i == k) cycle i_aibibkbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkbiek(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbiek
end do a_aibibkbiek
end do b_aibibkbiek
end do k_aibibkbiek
end do e_aibibkbiek
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibibkbkei: do e = n0e, n1e
k_aibibkbkei: do k = n0kl, n1kl
b0 = max(e + 1, n0bcd)
b_aibibkbkei: do b = b0, n1bcd
if (b == e) cycle b_aibibkbkei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbkei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibibkbkei
i0 = max(k + 1, n0ijm)
i_aibibkbkei: do i = i0, n1ijm
if (i == k) cycle i_aibibkbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibibkbkei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbkei
end do a_aibibkbkei
end do b_aibibkbkei
end do k_aibibkbkei
end do e_aibibkbkei
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjbibiei: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbibiei: do b = b0, n1bcd
if (b == e) cycle b_aibjbibiei
j_aibjbibiei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibiei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbibiei
i1 = min(j - 1, n1iklm)
i_aibjbibiei: do i = n0iklm, i1
if (i == j) cycle i_aibjbibiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbibiei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibiei
end do a_aibjbibiei
end do j_aibjbibiei
end do b_aibjbibiei
end do e_aibjbibiei
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbibiej: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbibiej: do b = b0, n1bcd
if (b == e) cycle b_aibjbibiej
j_aibjbibiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibiej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbibiej
i1 = min(j - 1, n1ikl)
i_aibjbibiej: do i = n0ikl, i1
if (i == j) cycle i_aibjbibiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbibiej(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibiej
end do a_aibjbibiej
end do j_aibjbibiej
end do b_aibjbibiej
end do e_aibjbibiej
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbibjei: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbibjei: do b = b0, n1bcd
if (b == e) cycle b_aibjbibjei
j_aibjbibjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibjei: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbibjei
i1 = min(j - 1, n1ikm)
i_aibjbibjei: do i = n0ikm, i1
if (i == j) cycle i_aibjbibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v6_eom_cc3_32_trans_aibjbibjei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibjei
end do a_aibjbibjei
end do j_aibjbibjei
end do b_aibjbibjei
end do e_aibjbibjei
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjcibibi: do c = n0c, n1c
b0 = max(c + 1, n0bde)
b_aibjcibibi: do b = b0, n1bde
if (b == c) cycle b_aibjcibibi
j_aibjcibibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibibi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibibi
i_aibjcibibi: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjcibibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibibi(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibibi
end do a_aibjcibibi
end do j_aibjcibibi
end do b_aibjcibibi
end do c_aibjcibibi
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcibibj: do c = n0c, n1c
b0 = max(c + 1, n0bde)
b_aibjcibibj: do b = b0, n1bde
if (b == c) cycle b_aibjcibibj
j_aibjcibibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibibj
i0 = max(j + 1, n0ikl)
i_aibjcibibj: do i = i0, n1ikl
if (i == j) cycle i_aibjcibibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibibj(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibibj
end do a_aibjcibibj
end do j_aibjcibibj
end do b_aibjcibibj
end do c_aibjcibibj
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbibj: do c = n0c, n1c
b0 = max(c + 1, n0bde)
b_aibjcjbibj: do b = b0, n1bde
if (b == c) cycle b_aibjcjbibj
j_aibjcjbibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbibj
i0 = max(j + 1, n0il)
i_aibjcjbibj: do i = i0, n1il
if (i == j) cycle i_aibjcjbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbibj(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbibj
end do a_aibjcjbibj
end do j_aibjcjbibj
end do b_aibjcjbibj
end do c_aibjcjbibj
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjbi: do c = n0c, n1c
b0 = max(c + 1, n0bde)
b_aibjcibjbi: do b = b0, n1bde
if (b == c) cycle b_aibjcibjbi
j_aibjcibjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibjbi
i1 = min(j - 1, n1ikm)
i_aibjcibjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibjbi(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibjbi
end do a_aibjcibjbi
end do j_aibjcibjbi
end do b_aibjcibjbi
end do c_aibjcibjbi
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbjbi: do c = n0c, n1c
b0 = max(c + 1, n0bde)
b_aibjcjbjbi: do b = b0, n1bde
if (b == c) cycle b_aibjcjbjbi
j_aibjcjbjbi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbjbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbjbi
i1 = min(j - 1, n1im)
i_aibjcjbjbi: do i = n0im, i1
if (i == j) cycle i_aibjcjbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbjbi(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbjbi
end do a_aibjcjbjbi
end do j_aibjcjbjbi
end do b_aibjcjbjbi
end do c_aibjcjbjbi
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i
! Equalities: d == b, e == b, k == j, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbjbj: do c = n0c, n1c
b0 = max(c + 1, n0bde)
b_aibjcjbjbj: do b = b0, n1bde
if (b == c) cycle b_aibjcjbjbj
j_aibjcjbjbj: do j = n0jklm, n1jklm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbjbj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbjbj
i_aibjcjbjbj: do i = n0i, n1i
if (i == j) cycle i_aibjcjbjbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbjbj(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbjbj
end do a_aibjcjbjbj
end do j_aibjcjbjbj
end do b_aibjcjbjbj
end do c_aibjcjbjbj
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjcibici: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcibici: do b = b0, n1bd
if (b == c) cycle b_aibjcibici
j_aibjcibici: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibici: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibici
i_aibjcibici: do i = n0iklm, n1iklm
if (i == j) cycle i_aibjcibici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibici(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibici
end do a_aibjcibici
end do j_aibjcibici
end do b_aibjcibici
end do c_aibjcibici
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcibicj: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcibicj: do b = b0, n1bd
if (b == c) cycle b_aibjcibicj
j_aibjcibicj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibicj
i_aibjcibicj: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcibicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibicj(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibicj
end do a_aibjcibicj
end do j_aibjcibicj
end do b_aibjcibicj
end do c_aibjcibicj
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbici: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcjbici: do b = b0, n1bd
if (b == c) cycle b_aibjcjbici
j_aibjcjbici: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbici: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbici
i_aibjcjbici: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjcjbici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbici(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbici
end do a_aibjcjbici
end do j_aibjcjbici
end do b_aibjcjbici
end do c_aibjcjbici
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbicj: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcjbicj: do b = b0, n1bd
if (b == c) cycle b_aibjcjbicj
j_aibjcjbicj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbicj
i_aibjcjbicj: do i = n0il, n1il
if (i == j) cycle i_aibjcjbicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbicj(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbicj
end do a_aibjcjbicj
end do j_aibjcjbicj
end do b_aibjcjbicj
end do c_aibjcjbicj
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjci: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcibjci: do b = b0, n1bd
if (b == c) cycle b_aibjcibjci
j_aibjcibjci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibjci
i_aibjcibjci: do i = n0ikm, n1ikm
if (i == j) cycle i_aibjcibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcibjci(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibjci
end do a_aibjcibjci
end do j_aibjcibjci
end do b_aibjcibjci
end do c_aibjcibjci
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbjci: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcjbjci: do b = b0, n1bd
if (b == c) cycle b_aibjcjbjci
j_aibjcjbjci: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbjci
i_aibjcjbjci: do i = n0im, n1im
if (i == j) cycle i_aibjcjbjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + &
((2 * npair - em + 2) * (em - 1)) / 2 + dl - em + 1
jac_ibra_iket = v0_eom_cc3_32_trans_aibjcjbjci(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbjci
end do a_aibjcjbjci
end do j_aibjcjbjci
end do b_aibjcjbjci
end do c_aibjcjbjci
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
end subroutine ccjac_32_dav_part12
end module ccjac_block_32_dav_part12
