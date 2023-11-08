module ccjac_block_23_dav_part11
use eom_cc3_23_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-21 14:27:00 UTC.
!
contains
subroutine ccjac_23_dav_part11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a0, a1, i0, i1, j0, j1, k1, l0
integer :: n0abd, n0abde, n0ad, n0ade, n0be
integer :: n0ijk, n0ijkl, n0ijkm, n0ijl, n0ijlm
integer :: n0ijm, n0ik, n0ikl, n0ikm, n0il
integer :: n0ilm, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jlm, n0jm, n0kl, n0km
integer :: n1abd, n1abde, n1ad, n1ade, n1be
integer :: n1ijk, n1ijkl, n1ijkm, n1ijl, n1ijlm
integer :: n1ijm, n1ik, n1ikl, n1ikm, n1il
integer :: n1ilm, n1im, n1jk, n1jkl, n1jkm
integer :: n1jl, n1jlm, n1jm, n1kl, n1km
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket, iket2, ibra2
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
integer :: nk, nc, nl, nd, nm, ne
integer :: mk, mc, ml, md, mm, me

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

nk = n1k - n0k + 1
nc = n1c - n0c + 1
nl = n1l - n0l + 1
nd = n1d - n0d + 1
nm = n1m - n0m + 1
ne = n1e - n0e + 1
mk = 1
mc = nk
ml = nk * nc
md = nk * nc * nl
mm = nk * nc * nl * nd
me = nk * nc * nl * nd * nm

n0abd = max(n0a, n0b, n0d)
n0abde = max(n0a, n0b, n0d, n0e)
n0ad = max(n0a, n0d)
n0ade = max(n0a, n0d, n0e)
n0be = max(n0b, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
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
n0km = max(n0k, n0m)
n1abd = min(n1a, n1b, n1d)
n1abde = min(n1a, n1b, n1d, n1e)
n1ad = min(n1a, n1d)
n1ade = min(n1a, n1d, n1e)
n1be = min(n1b, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
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
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjaiam: do m = n0m, n1m
c_aiajcjaiam: do c = n0c, n1c
j_aiajcjaiam: do j = n0jk, n1jk
if (j == m) cycle j_aiajcjaiam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiajcjaiam: do a = n0abde, a1
if (a == c) cycle a_aiajcjaiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajcjaiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajcjaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajcjaiam(nocc, a, i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaiam
end do a_aiajcjaiam
end do j_aiajcjaiam
end do c_aiajcjaiam
end do m_aiajcjaiam
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckaiak: do c = n0c, n1c
k_aiajckaiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiak: do j = n0j, n1j
if (j == k) cycle j_aiajckaiak
a1 = min(c - 1, n1abde)
a_aiajckaiak: do a = n0abde, a1
if (a == c) cycle a_aiajckaiak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aiajckaiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, a, k)
jac_ibra_iket = eom_cc3_23_trans_aiajckaiak(j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiak
end do a_aiajckaiak
end do j_aiajckaiak
end do k_aiajckaiak
end do c_aiajckaiak
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckaiaj: do c = n0c, n1c
k_aiajckaiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaiaj: do j = n0jm, n1jm
if (j == k) cycle j_aiajckaiaj
a1 = min(c - 1, n1abde)
a_aiajckaiaj: do a = n0abde, a1
if (a == c) cycle a_aiajckaiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aiajckaiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajckaiaj(nocc, a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaiaj
end do a_aiajckaiaj
end do j_aiajckaiaj
end do k_aiajckaiaj
end do c_aiajckaiaj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, m
! Equalities: b == a, d == a, e == a, k == j, l == j
! No equalities independent of the above can hold.
!
m_aiajcjajam: do m = n0m, n1m
c_aiajcjajam: do c = n0c, n1c
j0 = max(m + 1, n0jkl)
j_aiajcjajam: do j = j0, n1jkl
if (j == m) cycle j_aiajcjajam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiajcjajam: do a = n0abde, a1
if (a == c) cycle a_aiajcjajam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjajam: do i = i0, n1i
if (i == j .or. i == m) cycle i_aiajcjajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, j, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajcjajam(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjajam
end do a_aiajcjajam
end do j_aiajcjajam
end do c_aiajcjajam
end do m_aiajcjajam
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckakai: do c = n0c, n1c
k_aiajckakai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckakai: do j = n0j, n1j
if (j == k) cycle j_aiajckakai
a1 = min(c - 1, n1abde)
a_aiajckakai: do a = n0abde, a1
if (a == c) cycle a_aiajckakai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(k - 1, n1im)
i_aiajckakai: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajckakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, k, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajckakai(j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakai
end do a_aiajckakai
end do j_aiajckakai
end do k_aiajckakai
end do c_aiajckakai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == a, m == j, l == k
! No equalities independent of the above can hold.
!
c_aiajckakaj: do c = n0c, n1c
k_aiajckakaj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajckakaj: do j = n0jm, j1
if (j == k) cycle j_aiajckakaj
a1 = min(c - 1, n1abde)
a_aiajckakaj: do a = n0abde, a1
if (a == c) cycle a_aiajckakaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckakaj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckakaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, k, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajckakaj(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckakaj
end do a_aiajckakaj
end do j_aiajckakaj
end do k_aiajckakaj
end do c_aiajckakaj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajciajam: do m = n0m, n1m
c_aiajciajam: do c = n0c, n1c
j0 = max(m + 1, n0jl)
j_aiajciajam: do j = j0, n1jl
if (j == m) cycle j_aiajciajam
a1 = min(c - 1, n1abde)
a_aiajciajam: do a = n0abde, a1
if (a == c) cycle a_aiajciajam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i1 = min(j - 1, n1ik)
i_aiajciajam: do i = i0, i1
if (i == j .or. i == m) cycle i_aiajciajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiajciajam(nocc, a, i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajam
end do a_aiajciajam
end do j_aiajciajam
end do c_aiajciajam
end do m_aiajciajam
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
c_aiajckajak: do c = n0c, n1c
k_aiajckajak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckajak: do j = j0, n1jl
if (j == k) cycle j_aiajckajak
a1 = min(c - 1, n1abde)
a_aiajckajak: do a = n0abde, a1
if (a == c) cycle a_aiajckajak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckajak: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, j, a, k)
jac_ibra_iket = eom_cc3_23_trans_aiajckajak(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckajak
end do a_aiajckajak
end do j_aiajckajak
end do k_aiajckajak
end do c_aiajckajak
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l, m
! Equalities: b == a, d == a, e == a, j == i, k == i
! No equalities independent of the above can hold.
!
m_aiaicialam: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aiaicialam: do l = l0, n1l
if (l == m) cycle l_aiaicialam
c_aiaicialam: do c = n0c, n1c
a1 = min(c - 1, n1abde)
a_aiaicialam: do a = n0abde, a1
if (a == c) cycle a_aiaicialam
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(l - 1, n1ijk)
i_aiaicialam: do i = n0ijk, i1
if (i == l .or. i == m) cycle i_aiaicialam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, a, m)
jac_ibra_iket = eom_cc3_23_trans_aiaicialam(i, c, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicialam
end do a_aiaicialam
end do c_aiaicialam
end do l_aiaicialam
end do m_aiaicialam
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiajcialai: do l = n0l, n1l
c_aiajcialai: do c = n0c, n1c
j_aiajcialai: do j = n0j, n1j
if (j == l) cycle j_aiajcialai
a1 = min(c - 1, n1abde)
a_aiajcialai: do a = n0abde, a1
if (a == c) cycle a_aiajcialai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ikm)
i1 = min(l - 1, n1ikm)
i_aiajcialai: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajcialai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajcialai(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcialai
end do a_aiajcialai
end do j_aiajcialai
end do c_aiajcialai
end do l_aiajcialai
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajcialaj: do l = n0l, n1l
c_aiajcialaj: do c = n0c, n1c
j1 = min(l - 1, n1jm)
j_aiajcialaj: do j = n0jm, j1
if (j == l) cycle j_aiajcialaj
a1 = min(c - 1, n1abde)
a_aiajcialaj: do a = n0abde, a1
if (a == c) cycle a_aiajcialaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i1 = min(l - 1, n1ik)
i_aiajcialaj: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajcialaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajcialaj(nocc, a, i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcialaj
end do a_aiajcialaj
end do j_aiajcialaj
end do c_aiajcialaj
end do l_aiajcialaj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, e == a, j == i, m == i
! No equalities independent of the above can hold.
!
l_aiaickalai: do l = n0l, n1l
c_aiaickalai: do c = n0c, n1c
k1 = min(l - 1, n1k)
k_aiaickalai: do k = n0k, k1
if (k == l) cycle k_aiaickalai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiaickalai: do a = n0abde, a1
if (a == c) cycle a_aiaickalai
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijm)
i_aiaickalai: do i = n0ijm, i1
if (i == k .or. i == l) cycle i_aiaickalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiaickalai(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickalai
end do a_aiaickalai
end do k_aiaickalai
end do c_aiaickalai
end do l_aiaickalai
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjalai: do l = n0l, n1l
c_aiajcjalai: do c = n0c, n1c
j1 = min(l - 1, n1jk)
j_aiajcjalai: do j = n0jk, j1
if (j == l) cycle j_aiajcjalai
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiajcjalai: do a = n0abde, a1
if (a == c) cycle a_aiajcjalai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajcjalai: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajcjalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aiajcjalai(nocc, a, i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjalai
end do a_aiajcjalai
end do j_aiajcjalai
end do c_aiajcjalai
end do l_aiajcjalai
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, e == a, k == j, m == j
! No equalities independent of the above can hold.
!
l_aiajcjalaj: do l = n0l, n1l
c_aiajcjalaj: do c = n0c, n1c
j1 = min(l - 1, n1jkm)
j_aiajcjalaj: do j = n0jkm, j1
if (j == l) cycle j_aiajcjalaj
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abde)
a_aiajcjalaj: do a = n0abde, a1
if (a == c) cycle a_aiajcjalaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjalaj: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajcjalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, l, a, j)
jac_ibra_iket = eom_cc3_23_trans_aiajcjalaj(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjalaj
end do a_aiajcjalaj
end do j_aiajcjalaj
end do c_aiajcjalaj
end do l_aiajcjalaj
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiaickaiei: do e = n0e, n1e
c_aiaickaiei: do c = n0c, n1c
if (c == e) cycle c_aiaickaiei
k_aiaickaiei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiaickaiei: do a = a0, a1
if (a == c .or. a == e) cycle a_aiaickaiei
i_aiaickaiei: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aiaickaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiaickaiei(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickaiei
end do a_aiaickaiei
end do k_aiaickaiei
end do c_aiaickaiei
end do e_aiaickaiei
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
e_aiajcjaiei: do e = n0e, n1e
c_aiajcjaiei: do c = n0c, n1c
if (c == e) cycle c_aiajcjaiei
j_aiajcjaiei: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajcjaiei: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajcjaiei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ilm)
i_aiajcjaiei: do i = i0, n1ilm
if (i == j) cycle i_aiajcjaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajcjaiei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaiei
end do a_aiajcjaiei
end do j_aiajcjaiei
end do c_aiajcjaiei
end do e_aiajcjaiei
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjaiej: do e = n0e, n1e
c_aiajcjaiej: do c = n0c, n1c
if (c == e) cycle c_aiajcjaiej
j_aiajcjaiej: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajcjaiej: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajcjaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjaiej: do i = i0, n1il
if (i == j) cycle i_aiajcjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajcjaiej(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaiej
end do a_aiajcjaiej
end do j_aiajcjaiej
end do c_aiajcjaiej
end do e_aiajcjaiej
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
e_aiajciajej: do e = n0e, n1e
c_aiajciajej: do c = n0c, n1c
if (c == e) cycle c_aiajciajej
j_aiajciajej: do j = n0jlm, n1jlm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajciajej: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajciajej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciajej: do i = i0, n1ik
if (i == j) cycle i_aiajciajej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, e, j)
jac_ibra_iket = eom_cc3_23_trans_aiajciajej(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajej
end do a_aiajciajej
end do j_aiajciajej
end do c_aiajciajej
end do e_aiajciajej
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajciajei: do e = n0e, n1e
c_aiajciajei: do c = n0c, n1c
if (c == e) cycle c_aiajciajei
j_aiajciajei: do j = n0jl, n1jl
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiajciajei: do a = a0, a1
if (a == c .or. a == e) cycle a_aiajciajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikm)
i_aiajciajei: do i = i0, n1ikm
if (i == j) cycle i_aiajciajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiajciajei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciajei
end do a_aiajciajei
end do j_aiajciajei
end do c_aiajciajei
end do e_aiajciajei
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, l
! Equalities: b == a, d == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aiaicialei: do e = n0e, n1e
l_aiaicialei: do l = n0l, n1l
c_aiaicialei: do c = n0c, n1c
if (c == e) cycle c_aiaicialei
a0 = max(e + 1, n0abd)
a1 = min(c - 1, n1abd)
a_aiaicialei: do a = a0, a1
if (a == c .or. a == e) cycle a_aiaicialei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaicialei: do i = n0ijkm, n1ijkm
if (i == l) cycle i_aiaicialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, e, i)
jac_ibra_iket = eom_cc3_23_trans_aiaicialei(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicialei
end do a_aiaicialei
end do c_aiaicialei
end do l_aiaicialei
end do e_aiaicialei
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, m
! Equalities: d == a, e == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiciaiam: do m = n0m, n1m
c_aibiciaiam: do c = n0c, n1c
b_aibiciaiam: do b = n0b, n1b
if (b == c) cycle b_aibiciaiam
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibiciaiam: do a = a0, a1
if (a == b .or. a == c) cycle a_aibiciaiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ijkl)
i_aibiciaiam: do i = i0, n1ijkl
if (i == m) cycle i_aibiciaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, i, a, m)
jac_ibra_iket = eom_cc3_23_trans_aibiciaiam(a, b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciaiam
end do a_aibiciaiam
end do b_aibiciaiam
end do c_aibiciaiam
end do m_aibiciaiam
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaiaj: do c = n0c, n1c
b_aibjciaiaj: do b = n0b, n1b
if (b == c) cycle b_aibjciaiaj
j_aibjciaiaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjciaiaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aibjciaiaj: do i = i0, n1ikl
if (i == j) cycle i_aibjciaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aibjciaiaj(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaiaj
end do a_aibjciaiaj
end do j_aibjciaiaj
end do b_aibjciaiaj
end do c_aibjciaiaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaiaj: do c = n0c, n1c
b_aibjcjaiaj: do b = n0b, n1b
if (b == c) cycle b_aibjcjaiaj
j_aibjcjaiaj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjcjaiaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjcjaiaj: do i = i0, n1il
if (i == j) cycle i_aibjcjaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, a, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcjaiaj(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaiaj
end do a_aibjcjaiaj
end do j_aibjcjaiaj
end do b_aibjcjaiaj
end do c_aibjcjaiaj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajai: do c = n0c, n1c
b_aibjcjajai: do b = n0b, n1b
if (b == c) cycle b_aibjcjajai
j_aibjcjajai: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjcjajai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjcjajai: do i = n0im, i1
if (i == j) cycle i_aibjcjajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjajai(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjajai
end do a_aibjcjajai
end do j_aibjcjajai
end do b_aibjcjajai
end do c_aibjcjajai
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajai: do c = n0c, n1c
b_aibjciajai: do b = n0b, n1b
if (b == c) cycle b_aibjciajai
j_aibjciajai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibjciajai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjciajai: do i = n0ikm, i1
if (i == j) cycle i_aibjciajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, j, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibjciajai(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciajai
end do a_aibjciajai
end do j_aibjciajai
end do b_aibjciajai
end do c_aibjciajai
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibicialai: do l = n0l, n1l
c_aibicialai: do c = n0c, n1c
b_aibicialai: do b = n0b, n1b
if (b == c) cycle b_aibicialai
a0 = max(b + 1, n0ade)
a1 = min(c - 1, n1ade)
a_aibicialai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibicialai
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijkm)
i_aibicialai: do i = n0ijkm, i1
if (i == l) cycle i_aibicialai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, i, a, l, a, i)
jac_ibra_iket = eom_cc3_23_trans_aibicialai(a, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicialai
end do a_aibicialai
end do b_aibicialai
end do c_aibicialai
end do l_aibicialai
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickaibi: do c = n0c, n1c
k_aibickaibi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickaibi: do b = n0be, n1be
if (b == c) cycle b_aibickaibi
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickaibi: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickaibi
i_aibickaibi: do i = n0ijlm, n1ijlm
if (i == k) cycle i_aibickaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibickaibi(nocc, a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickaibi
end do a_aibickaibi
end do b_aibickaibi
end do k_aibickaibi
end do c_aibickaibi
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickaibk: do c = n0c, n1c
k_aibickaibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickaibk: do b = n0be, n1be
if (b == c) cycle b_aibickaibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickaibk: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickaibk
i_aibickaibk: do i = n0ijl, n1ijl
if (i == k) cycle i_aibickaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, k, a, i, b, k)
jac_ibra_iket = eom_cc3_23_trans_aibickaibk(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickaibk
end do a_aibickaibk
end do b_aibickaibk
end do k_aibickaibk
end do c_aibickaibk
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, l == i, m == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjaibi: do c = n0c, n1c
b_aibjcjaibi: do b = n0be, n1be
if (b == c) cycle b_aibjcjaibi
j_aibjcjaibi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaibi: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjaibi
i_aibjcjaibi: do i = n0ilm, n1ilm
if (i == j) cycle i_aibjcjaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, b, i)
jac_ibra_iket = eom_cc3_23_trans_aibjcjaibi(nocc, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaibi
end do a_aibjcjaibi
end do j_aibjcjaibi
end do b_aibjcjaibi
end do c_aibjcjaibi
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaibj: do c = n0c, n1c
b_aibjcjaibj: do b = n0be, n1be
if (b == c) cycle b_aibjcjaibj
j_aibjcjaibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaibj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjaibj
i_aibjcjaibj: do i = n0il, n1il
if (i == j) cycle i_aibjcjaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
!iket = ketoffset + mu3(ck, dl, em)
iket = ketoffset + mu3_mem(c, j, a, i, b, j)
jac_ibra_iket = eom_cc3_23_trans_aibjcjaibj(nocc, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaibj
end do a_aibjcjaibj
end do j_aibjcjaibj
end do b_aibjcjaibj
end do c_aibjcjaibj
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
function mu3_mem(c, k, d, l, e, m)
integer :: mu3_mem
integer, intent(in) :: c, k, d, l, e, m
mu3_mem = offset + (k-n0k)+mk + (c-n0c)*mc + (l-n0l)*ml + (d-n0d)*md + (m-n0m)*mm  &
+ (e-n0e)*me
end function mu3_mem

end subroutine ccjac_23_dav_part11
end module ccjac_block_23_dav_part11
