module ccjac_block_32_tripletp_dav_part2
use v1_eom_cc3_32_tripletp_trans
use v2_eom_cc3_32_tripletp_trans
use v3_eom_cc3_32_tripletp_trans
use v4_eom_cc3_32_tripletp_trans
use v5_eom_cc3_32_tripletp_trans
use v6_eom_cc3_32_tripletp_trans
use v7_eom_cc3_32_tripletp_trans
use v8_eom_cc3_32_tripletp_trans
use v9_eom_cc3_32_tripletp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams 
implicit none
!
! File generated automatically on 2017-01-20 15:15:39 UTC.
!
contains
subroutine ccjac_32_tripletp_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
 nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0e, n1e, n0i, n1i, n0j, &
 n1j, n0k, n1k, n0l, n1l, n0m, n1m, bra0, ket0) 
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
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, a1, b0, b1, c1, i0, i1, j0, j1, k0, k1
integer :: n0acd, n0ace, n0bd, n0be, n0ce
integer :: n0ij, n0ijl, n0ijm, n0ik, n0ikl
integer :: n0ikm, n0il, n0im, n0jl, n0jm
integer :: n0kl, n0km
integer :: n1acd, n1ace, n1bd, n1be, n1ce
integer :: n1ij, n1ijl, n1ijm, n1ik, n1ikl
integer :: n1ikm, n1il, n1im, n1jl, n1jm
integer :: n1kl, n1km
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
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
n0acd = max(n0a, n0c, n0d)
n0ace = max(n0a, n0c, n0e)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0ce = max(n0c, n0e)
n0ij = max(n0i, n0j)
n0ijl = max(n0i, n0j, n0l)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1acd = min(n1a, n1c, n1d)
n1ace = min(n1a, n1c, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1ce = min(n1c, n1e)
n1ij = min(n1i, n1j)
n1ijl = min(n1i, n1j, n1l)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k, m
! Equalities: d == b, e == c, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibickbicm: do m = n0m, n1m
c_aibickbicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibickbicm: do k = n0k, n1k
if (k == m) cycle k_aibickbicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbicm: do b = b0, n1bd
if (b == c) cycle b_aibickbicm
a_aibickbicm: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbicm
i0 = max(k + 1, m + 1, n0ijl)
i_aibickbicm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibickbicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + m
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickbicm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbicm
end do a_aibickbicm
end do b_aibickbicm
end do k_aibickbicm
end do c_aibickbicm
end do m_aibickbicm
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k, l
! Equalities: d == b, e == c, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibickblci: do l = n0l, n1l
c_aibickblci: do c = n0ce, n1ce
k_aibickblci: do k = n0k, n1k
if (k == l) cycle k_aibickblci
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickblci: do b = b0, n1bd
if (b == c) cycle b_aibickblci
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibickblci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickblci
i0 = max(k + 1, n0ijm)
i1 = min(l - 1, n1ijm)
i_aibickblci: do i = i0, i1
if (i == k .or. i == l) cycle i_aibickblci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + i
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickblci(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickblci
end do a_aibickblci
end do b_aibickblci
end do k_aibickblci
end do c_aibickblci
end do l_aibickblci
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k, m
! Equalities: d == b, e == c, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibickbkcm: do m = n0m, n1m
c_aibickbkcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k0 = max(m + 1, n0kl)
k_aibickbkcm: do k = k0, n1kl
if (k == m) cycle k_aibickbkcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbkcm: do b = b0, n1bd
if (b == c) cycle b_aibickbkcm
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibickbkcm: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbkcm
i0 = max(k + 1, n0ij)
i_aibickbkcm: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aibickbkcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (c - nvirt0) * (k - 1) + m
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickbkcm(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbkcm
end do a_aibickbkcm
end do b_aibickbkcm
end do k_aibickbkcm
end do c_aibickbkcm
end do m_aibickbkcm
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k, l
! Equalities: d == b, e == c, j == i, m == k
! No equalities independent of the above can hold.
!
l_aibickblck: do l = n0l, n1l
c_aibickblck: do c = n0ce, n1ce
k1 = min(l - 1, n1km)
k_aibickblck: do k = n0km, k1
if (k == l) cycle k_aibickblck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickblck: do b = b0, n1bd
if (b == c) cycle b_aibickblck
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibickblck: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickblck
i0 = max(k + 1, n0ij)
i_aibickblck: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibickblck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickblck(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickblck
end do a_aibickblck
end do b_aibickblck
end do k_aibickblck
end do c_aibickblck
end do l_aibickblck
!
! Elementary loop  5
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
a_aibjcibicm: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibicm
i0 = max(m + 1, n0ikl)
i1 = min(j - 1, n1ikl)
i_aibjcibicm: do i = i0, i1
if (i == j .or. i == m) cycle i_aibjcibicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + m
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjcibicm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibicm
end do a_aibjcibicm
end do j_aibjcibicm
end do b_aibjcibicm
end do c_aibjcibicm
end do m_aibjcibicm
!
! Elementary loop  6
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
j0 = max(k + 1, n0jm)
j_aibjckbicj: do j = j0, n1jm
if (j == k) cycle j_aibjckbicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbicj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbicj
i0 = max(j + 1, n0il)
i_aibjckbicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckbicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckbicj(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbicj
end do a_aibjckbicj
end do j_aibjckbicj
end do b_aibjckbicj
end do k_aibjckbicj
end do c_aibjckbicj
!
! Elementary loop  7
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
j0 = max(k + 1, n0j)
j_aibjckbick: do j = j0, n1j
if (j == k) cycle j_aibjckbick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbick: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbick
i0 = max(k + 1, n0il)
i_aibjckbick: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckbick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckbick(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbick
end do a_aibjckbick
end do j_aibjckbick
end do b_aibjckbick
end do k_aibjckbick
end do c_aibjckbick
!
! Elementary loop  8
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
a_aibjciblci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjciblci
i1 = min(j - 1, l - 1, n1ikm)
i_aibjciblci: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjciblci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjciblci(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciblci
end do a_aibjciblci
end do j_aibjciblci
end do b_aibjciblci
end do c_aibjciblci
end do l_aibjciblci
!
! Elementary loop  9
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
j0 = max(m + 1, n0jl)
j_aibjcibjcm: do j = j0, n1jl
if (j == m) cycle j_aibjcibjcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjcm: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibjcm
i1 = min(j - 1, n1ik)
i_aibjcibjcm: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjcibjcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + m
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjcibjcm(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjcm
end do a_aibjcibjcm
end do j_aibjcibjcm
end do b_aibjcibjcm
end do c_aibjcibjcm
end do m_aibjcibjcm
!
! Elementary loop  10
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
j1 = min(l - 1, n1jm)
j_aibjciblcj: do j = n0jm, j1
if (j == l) cycle j_aibjciblcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciblcj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjciblcj
i1 = min(j - 1, n1ik)
i_aibjciblcj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjciblcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + j
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjciblcj(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciblcj
end do a_aibjciblcj
end do j_aibjciblcj
end do b_aibjciblcj
end do c_aibjciblcj
end do l_aibjciblcj
!
! Elementary loop  11
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
j0 = max(k + 1, n0jl)
j_aibjckbjci: do j = j0, n1jl
if (j == k) cycle j_aibjckbjci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbjci
i1 = min(j - 1, n1im)
i_aibjckbjci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckbjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckbjci(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjci
end do a_aibjckbjci
end do j_aibjckbjci
end do b_aibjckbjci
end do k_aibjckbjci
end do c_aibjckbjci
!
! Elementary loop  12
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
j0 = max(k + 1, n0j)
j_aibjckbkci: do j = j0, n1j
if (j == k) cycle j_aibjckbkci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbkci
i1 = min(k - 1, n1im)
i_aibjckbkci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckbkci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (c - nvirt0) * (k - 1) + i
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckbkci(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbkci
end do a_aibjckbkci
end do j_aibjckbkci
end do b_aibjckbkci
end do k_aibjckbkci
end do c_aibjckbkci
!
! Elementary loop  13
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
j0 = max(k + 1, n0jl)
j_aibjckbjck: do j = j0, n1jl
if (j == k) cycle j_aibjckbjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjck: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbjck
i_aibjckbjck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckbjck(a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjck
end do a_aibjckbjck
end do j_aibjckbjck
end do b_aibjckbjck
end do k_aibjckbjck
end do c_aibjckbjck
!
! Elementary loop  14
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, k
! Equalities: e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibickdick: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibickdick: do c = n0ce, c1
if (c == d) cycle c_aibickdick
k_aibickdick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickdick: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibickdick
a_aibickdick: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibickdick
i0 = max(k + 1, n0ijl)
i_aibickdick: do i = i0, n1ijl
if (i == k) cycle i_aibickdick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickdick(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdick
end do a_aibickdick
end do b_aibickdick
end do k_aibickdick
end do c_aibickdick
end do d_aibickdick
!
! Elementary loop  15
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjci: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjcidjci: do c = n0ce, c1
if (c == d) cycle c_aibjcidjci
b0 = max(c + 1, n0b)
b_aibjcidjci: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidjci
j_aibjcidjci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidjci: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjci
i1 = min(j - 1, n1ikm)
i_aibjcidjci: do i = n0ikm, i1
if (i == j) cycle i_aibjcidjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjcidjci(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidjci
end do a_aibjcidjci
end do j_aibjcidjci
end do b_aibjcidjci
end do c_aibjcidjci
end do d_aibjcidjci
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibickbiek: do e = n0e, n1e
c_aibickbiek: do c = n0c, n1c
if (c == e) cycle c_aibickbiek
k_aibickbiek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibickbiek: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibickbiek
a_aibickbiek: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibickbiek
i0 = max(k + 1, n0ijl)
i_aibickbiek: do i = i0, n1ijl
if (i == k) cycle i_aibickbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickbiek(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbiek
end do a_aibickbiek
end do b_aibickbiek
end do k_aibickbiek
end do c_aibickbiek
end do e_aibickbiek
!
! Elementary loop  17
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
a_aibjcibjei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibjei
i1 = min(j - 1, n1ikm)
i_aibjcibjei: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjcibjei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjei
end do a_aibjcibjei
end do j_aibjcibjei
end do b_aibjcibjei
end do c_aibjcibjei
end do e_aibjcibjei
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k
! Equalities: e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibickdibk: do d = n0d, n1d
c_aibickdibk: do c = n0c, n1c
if (c == d) cycle c_aibickdibk
k_aibickdibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibickdibk: do b = b0, b1
if (b == c .or. b == d) cycle b_aibickdibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibickdibk: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibickdibk
i0 = max(k + 1, n0ijl)
i_aibickdibk: do i = i0, n1ijl
if (i == k) cycle i_aibickdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickdibk(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdibk
end do a_aibickdibk
end do b_aibickdibk
end do k_aibickdibk
end do c_aibickdibk
end do d_aibickdibk
!
! Elementary loop  19
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
a_aibjcidjbi: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjbi
i1 = min(j - 1, n1ikm)
i_aibjcidjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjcidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjcidjbi(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidjbi
end do a_aibjcidjbi
end do j_aibjcidjbi
end do b_aibjcidjbi
end do c_aibjcidjbi
end do d_aibjcidjbi
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibiakaiek: do e = n0e, n1e
k_aibiakaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibiakaiek: do b = n0b, n1b
if (b == e) cycle b_aibiakaiek
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibiakaiek: do a = a0, a1
if (a == b .or. a == e) cycle a_aibiakaiek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakaiek: do i = i0, n1ijl
if (i == k) cycle i_aibiakaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = v7_eom_cc3_32_tripletp_trans_aibiakaiek(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakaiek
end do a_aibiakaiek
end do b_aibiakaiek
end do k_aibiakaiek
end do e_aibiakaiek
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaiajei: do e = n0e, n1e
b_aibjaiajei: do b = n0b, n1b
if (b == e) cycle b_aibjaiajei
j_aibjaiajei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibjaiajei: do a = a0, a1
if (a == b .or. a == e) cycle a_aibjaiajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjaiajei: do i = n0ikm, i1
if (i == j) cycle i_aibjaiajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = v8_eom_cc3_32_tripletp_trans_aibjaiajei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiajei
end do a_aibjaiajei
end do j_aibjaiajei
end do b_aibjaiajei
end do e_aibjaiajei
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, e == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakbiam: do m = n0m, n1m
k_aibiakbiam: do k = n0k, n1k
if (k == m) cycle k_aibiakbiam
b_aibiakbiam: do b = n0bd, n1bd
a1 = min(b - 1, n1ace)
a_aibiakbiam: do a = n0ace, a1
if (a == b) cycle a_aibiakbiam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, m + 1, n0ijl)
i_aibiakbiam: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibiakbiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = v7_eom_cc3_32_tripletp_trans_aibiakbiam(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbiam
end do a_aibiakbiam
end do b_aibiakbiam
end do k_aibiakbiam
end do m_aibiakbiam
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, e == a, d == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibiakblai: do l = n0l, n1l
k_aibiakblai: do k = n0k, n1k
if (k == l) cycle k_aibiakblai
b_aibiakblai: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibiakblai: do a = n0ace, a1
if (a == b) cycle a_aibiakblai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i1 = min(l - 1, n1ijm)
i_aibiakblai: do i = i0, i1
if (i == k .or. i == l) cycle i_aibiakblai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = v7_eom_cc3_32_tripletp_trans_aibiakblai(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakblai
end do a_aibiakblai
end do b_aibiakblai
end do k_aibiakblai
end do l_aibiakblai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, e == a, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibiakbkam: do m = n0m, n1m
k0 = max(m + 1, n0kl)
k_aibiakbkam: do k = k0, n1kl
if (k == m) cycle k_aibiakbkam
b_aibiakbkam: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibiakbkam: do a = n0ace, a1
if (a == b) cycle a_aibiakbkam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakbkam: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aibiakbkam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + m
jac_ibra_iket = v7_eom_cc3_32_tripletp_trans_aibiakbkam(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbkam
end do a_aibiakbkam
end do b_aibiakbkam
end do k_aibiakbkam
end do m_aibiakbkam
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, e == a, d == b, j == i, m == k
! No equalities independent of the above can hold.
!
l_aibiakblak: do l = n0l, n1l
k1 = min(l - 1, n1km)
k_aibiakblak: do k = n0km, k1
if (k == l) cycle k_aibiakblak
b_aibiakblak: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibiakblak: do a = n0ace, a1
if (a == b) cycle a_aibiakblak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakblak: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibiakblak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + k
jac_ibra_iket = v7_eom_cc3_32_tripletp_trans_aibiakblak(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakblak
end do a_aibiakblak
end do b_aibiakblak
end do k_aibiakblak
end do l_aibiakblak
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, e == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjaibiam: do m = n0m, n1m
b_aibjaibiam: do b = n0bd, n1bd
j_aibjaibiam: do j = n0j, n1j
if (j == m) cycle j_aibjaibiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaibiam: do a = n0ace, a1
if (a == b) cycle a_aibjaibiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ikl)
i1 = min(j - 1, n1ikl)
i_aibjaibiam: do i = i0, i1
if (i == j .or. i == m) cycle i_aibjaibiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = v8_eom_cc3_32_tripletp_trans_aibjaibiam(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibiam
end do a_aibjaibiam
end do j_aibjaibiam
end do b_aibjaibiam
end do m_aibjaibiam
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, e == a, d == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakbiaj: do k = n0k, n1k
b_aibjakbiaj: do b = n0bd, n1bd
j0 = max(k + 1, n0jm)
j_aibjakbiaj: do j = j0, n1jm
if (j == k) cycle j_aibjakbiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbiaj: do a = n0ace, a1
if (a == b) cycle a_aibjakbiaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakbiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakbiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakbiaj(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbiaj
end do a_aibjakbiaj
end do j_aibjakbiaj
end do b_aibjakbiaj
end do k_aibjakbiaj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, e == a, d == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjakbiak: do k = n0km, n1km
b_aibjakbiak: do b = n0bd, n1bd
j0 = max(k + 1, n0j)
j_aibjakbiak: do j = j0, n1j
if (j == k) cycle j_aibjakbiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbiak: do a = n0ace, a1
if (a == b) cycle a_aibjakbiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i_aibjakbiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakbiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakbiak(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbiak
end do a_aibjakbiak
end do j_aibjakbiak
end do b_aibjakbiak
end do k_aibjakbiak
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, e == a, d == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjaiblai: do l = n0l, n1l
b_aibjaiblai: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblai: do j = n0j, n1j
if (j == l) cycle j_aibjaiblai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaiblai: do a = n0ace, a1
if (a == b) cycle a_aibjaiblai
i1 = min(j - 1, l - 1, n1ikm)
i_aibjaiblai: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjaiblai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = v8_eom_cc3_32_tripletp_trans_aibjaiblai(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiblai
end do a_aibjaiblai
end do j_aibjaiblai
end do b_aibjaiblai
end do l_aibjaiblai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, e == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaibjam: do m = n0m, n1m
b_aibjaibjam: do b = n0bd, n1bd
j0 = max(m + 1, n0jl)
j_aibjaibjam: do j = j0, n1jl
if (j == m) cycle j_aibjaibjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaibjam: do a = n0ace, a1
if (a == b) cycle a_aibjaibjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjaibjam: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjaibjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = v8_eom_cc3_32_tripletp_trans_aibjaibjam(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibjam
end do a_aibjaibjam
end do j_aibjaibjam
end do b_aibjaibjam
end do m_aibjaibjam
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
end subroutine ccjac_32_tripletp_dav_part2
end module ccjac_block_32_tripletp_dav_part2
