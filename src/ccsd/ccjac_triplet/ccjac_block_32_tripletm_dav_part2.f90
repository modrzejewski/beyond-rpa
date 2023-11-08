module ccjac_block_32_tripletm_dav_part2
use v1_eom_cc3_32_tripletm_trans
use v2_eom_cc3_32_tripletm_trans
use v3_eom_cc3_32_tripletm_trans
use v4_eom_cc3_32_tripletm_trans
use v5_eom_cc3_32_tripletm_trans
use v6_eom_cc3_32_tripletm_trans
use v7_eom_cc3_32_tripletm_trans
use v8_eom_cc3_32_tripletm_trans
use v9_eom_cc3_32_tripletm_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams 
implicit none
!
! File generated automatically on 2017-01-20 15:10:32 UTC.
!
contains
subroutine ccjac_32_tripletm_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a, b, c, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, a1, b0, i0, i1, j0
integer :: n0abd, n0ac, n0ace, n0ae, n0bd
integer :: n0bde, n0ce, n0ij, n0ijm, n0ik
integer :: n0ikl, n0ikm, n0il, n0ilm, n0im
integer :: n0jl, n0jm, n0kl, n0klm, n0km
integer :: n1abd, n1ac, n1ace, n1ae, n1bd
integer :: n1bde, n1ce, n1ij, n1ijm, n1ik
integer :: n1ikl, n1ikm, n1il, n1ilm, n1im
integer :: n1jl, n1jm, n1kl, n1klm, n1km
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
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0ace = max(n0a, n0c, n0e)
n0ae = max(n0a, n0e)
n0bd = max(n0b, n0d)
n0bde = max(n0b, n0d, n0e)
n0ce = max(n0c, n0e)
n0ij = max(n0i, n0j)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0ilm = max(n0i, n0l, n0m)
n0im = max(n0i, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1ace = min(n1a, n1c, n1e)
n1ae = min(n1a, n1e)
n1bd = min(n1b, n1d)
n1bde = min(n1b, n1d, n1e)
n1ce = min(n1c, n1e)
n1ij = min(n1i, n1j)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1ilm = min(n1i, n1l, n1m)
n1im = min(n1i, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1klm = min(n1k, n1l, n1m)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, l
! Equalities: b == a, d == a, e == c, m == k
! No equalities independent of the above can hold.
!
l_aiajckalck: do l = n0l, n1l
c_aiajckalck: do c = n0ce, n1ce
k_aiajckalck: do k = n0km, n1km
if (k == l) cycle k_aiajckalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckalck: do j = j0, n1j
if (j == k .or. j == l) cycle j_aiajckalck
a0 = max(c + 1, n0abd)
a_aiajckalck: do a = a0, n1abd
if (a == c) cycle a_aiajckalck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckalck: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckalck(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckalck
end do a_aiajckalck
end do j_aiajckalck
end do k_aiajckalck
end do c_aiajckalck
end do l_aiajckalck
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, m
! Equalities: b == a, d == a, e == c, l == i
! No equalities independent of the above can hold.
!
m_aiajckaicm: do m = n0m, n1m
c_aiajckaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiajckaicm: do k = n0k, n1k
if (k == m) cycle k_aiajckaicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckaicm: do j = j0, n1j
if (j == k .or. j == m) cycle j_aiajckaicm
a0 = max(c + 1, n0abd)
a_aiajckaicm: do a = a0, n1abd
if (a == c) cycle a_aiajckaicm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaicm: do i = n0il, n1il
if (i == j .or. i == k .or. i == m) cycle i_aiajckaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckaicm(a, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaicm
end do a_aiajckaicm
end do j_aiajckaicm
end do k_aiajckaicm
end do c_aiajckaicm
end do m_aiajckaicm
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, l
! Equalities: b == a, d == a, e == c, m == j
! No equalities independent of the above can hold.
!
l_aiajckalcj: do l = n0l, n1l
c_aiajckalcj: do c = n0ce, n1ce
k_aiajckalcj: do k = n0k, n1k
if (k == l) cycle k_aiajckalcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aiajckalcj: do j = j0, n1jm
if (j == k .or. j == l) cycle j_aiajckalcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckalcj: do a = a0, n1abd
if (a == c) cycle a_aiajckalcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckalcj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckalcj(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckalcj
end do a_aiajckalcj
end do j_aiajckalcj
end do k_aiajckalcj
end do c_aiajckalcj
end do l_aiajckalcj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiajckaiek: do e = n0e, n1e
c_aiajckaiek: do c = n0c, n1c
if (c == e) cycle c_aiajckaiek
k_aiajckaiek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckaiek: do j = j0, n1j
if (j == k) cycle j_aiajckaiek
a0 = max(c + 1, e + 1, n0abd)
a_aiajckaiek: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckaiek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaiek: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckaiek(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaiek
end do a_aiajckaiek
end do j_aiajckaiek
end do k_aiajckaiek
end do c_aiajckaiek
end do e_aiajckaiek
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, m == i, l == k
! No equalities independent of the above can hold.
!
e_aiajckakei: do e = n0e, n1e
c_aiajckakei: do c = n0c, n1c
if (c == e) cycle c_aiajckakei
k_aiajckakei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckakei: do j = j0, n1j
if (j == k) cycle j_aiajckakei
a0 = max(c + 1, e + 1, n0abd)
a_aiajckakei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckakei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aiajckakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckakei(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckakei
end do a_aiajckakei
end do j_aiajckakei
end do k_aiajckakei
end do c_aiajckakei
end do e_aiajckakei
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajckajei: do e = n0e, n1e
c_aiajckajei: do c = n0c, n1c
if (c == e) cycle c_aiajckajei
k_aiajckajei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckajei: do j = j0, n1jl
if (j == k) cycle j_aiajckajei
a0 = max(c + 1, e + 1, n0abd)
a_aiajckajei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aiajckajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckajei(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajei
end do a_aiajckajei
end do j_aiajckajei
end do k_aiajckajei
end do c_aiajckajei
end do e_aiajckajei
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajckaiej: do e = n0e, n1e
c_aiajckaiej: do c = n0c, n1c
if (c == e) cycle c_aiajckaiej
k_aiajckaiej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aiajckaiej: do j = j0, n1jm
if (j == k) cycle j_aiajckaiej
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiajckaiej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaiej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckaiej(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaiej
end do a_aiajckaiej
end do j_aiajckaiej
end do k_aiajckaiej
end do c_aiajckaiej
end do e_aiajckaiej
!
! Elementary loop  8
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
j0 = max(k + 1, n0j)
j_aibjckbibk: do j = j0, n1j
if (j == k) cycle j_aibjckbibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbibk: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbibk
i0 = max(k + 1, n0il)
i_aibjckbibk: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbibk(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbibk
end do a_aibjckbibk
end do j_aibjckbibk
end do b_aibjckbibk
end do k_aibjckbibk
end do c_aibjckbibk
!
! Elementary loop  9
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
j0 = max(k + 1, n0j)
j_aibjckbkbi: do j = j0, n1j
if (j == k) cycle j_aibjckbkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkbi: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbkbi
i1 = min(k - 1, n1im)
i_aibjckbkbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbkbi(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbkbi
end do a_aibjckbkbi
end do j_aibjckbkbi
end do b_aibjckbkbi
end do k_aibjckbkbi
end do c_aibjckbkbi
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
j0 = max(k + 1, n0jl)
j_aibjckbjbi: do j = j0, n1jl
if (j == k) cycle j_aibjckbjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjbi: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbjbi
i1 = min(j - 1, n1im)
i_aibjckbjbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbjbi(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
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
j0 = max(k + 1, n0jm)
j_aibjckbibj: do j = j0, n1jm
if (j == k) cycle j_aibjckbibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbibj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbibj
i0 = max(j + 1, n0il)
i_aibjckbibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbibj(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbibj
end do a_aibjckbibj
end do j_aibjckbibj
end do b_aibjckbibj
end do k_aibjckbibj
end do c_aibjckbibj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j, m
! Equalities: c == a, e == a, d == b, l == k
! No equalities independent of the above can hold.
!
m_aibjakbkam: do m = n0m, n1m
k_aibjakbkam: do k = n0kl, n1kl
if (k == m) cycle k_aibjakbkam
b_aibjakbkam: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbkam: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjakbkam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbkam: do a = n0ace, a1
if (a == b) cycle a_aibjakbkam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjakbkam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakbkam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbkam(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbkam
end do a_aibjakbkam
end do j_aibjakbkam
end do b_aibjakbkam
end do k_aibjakbkam
end do m_aibjakbkam
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, e == a, d == b, m == i
! No equalities independent of the above can hold.
!
l_aibjakblai: do l = n0l, n1l
k_aibjakblai: do k = n0k, n1k
if (k == l) cycle k_aibjakblai
b_aibjakblai: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakblai: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjakblai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakblai: do a = n0ace, a1
if (a == b) cycle a_aibjakblai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakblai: do i = n0im, n1im
if (i == j .or. i == k .or. i == l) cycle i_aibjakblai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakblai(a, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakblai
end do a_aibjakblai
end do j_aibjakblai
end do b_aibjakblai
end do k_aibjakblai
end do l_aibjakblai
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: c == a, e == a, d == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakbjam: do m = n0m, n1m
k_aibjakbjam: do k = n0k, n1k
if (k == m) cycle k_aibjakbjam
b_aibjakbjam: do b = n0bd, n1bd
j0 = max(k + 1, n0jl)
j_aibjakbjam: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjakbjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbjam: do a = n0ace, a1
if (a == b) cycle a_aibjakbjam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjakbjam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakbjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbjam(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjam
end do a_aibjakbjam
end do j_aibjakbjam
end do b_aibjakbjam
end do k_aibjakbjam
end do m_aibjakbjam
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, j
! Equalities: c == a, d == b, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjakbkei: do e = n0e, n1e
k_aibjakbkei: do k = n0kl, n1kl
b0 = max(e + 1, n0bd)
b_aibjakbkei: do b = b0, n1bd
if (b == e) cycle b_aibjakbkei
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbkei: do j = j0, n1j
if (j == k) cycle j_aibjakbkei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbkei: do a = n0ac, a1
if (a == b .or. a == e) cycle a_aibjakbkei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbkei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbkei(a, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbkei
end do a_aibjakbkei
end do j_aibjakbkei
end do b_aibjakbkei
end do k_aibjakbkei
end do e_aibjakbkei
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjakbjei: do e = n0e, n1e
k_aibjakbjei: do k = n0k, n1k
b0 = max(e + 1, n0bd)
b_aibjakbjei: do b = b0, n1bd
if (b == e) cycle b_aibjakbjei
j0 = max(k + 1, n0jl)
j_aibjakbjei: do j = j0, n1jl
if (j == k) cycle j_aibjakbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbjei: do a = n0ac, a1
if (a == b .or. a == e) cycle a_aibjakbjei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbjei(a, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjei
end do a_aibjakbjei
end do j_aibjakbjei
end do b_aibjakbjei
end do k_aibjakbjei
end do e_aibjakbjei
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
j0 = max(k + 1, n0j)
j_aibjckbick: do j = j0, n1j
if (j == k) cycle j_aibjckbick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbick: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbick
i_aibjckbick: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbick(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
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
i_aibjckbkci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbkci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbkci(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbkci
end do a_aibjckbkci
end do j_aibjckbkci
end do b_aibjckbkci
end do k_aibjckbkci
end do c_aibjckbkci
!
! Elementary loop  19
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
i_aibjckbjci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbjci(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjci
end do a_aibjckbjci
end do j_aibjckbjci
end do b_aibjckbjci
end do k_aibjckbjci
end do c_aibjckbjci
!
! Elementary loop  20
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
i_aibjckbicj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbicj(a, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbicj
end do a_aibjckbicj
end do j_aibjckbicj
end do b_aibjckbicj
end do k_aibjckbicj
end do c_aibjckbicj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: e == a, d == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjciblai: do l = n0l, n1l
c_aibjciblai: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjciblai: do b = b0, n1bd
if (b == c) cycle b_aibjciblai
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjciblai: do j = n0j, n1j
if (j == l) cycle j_aibjciblai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjciblai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjciblai
i1 = min(j - 1, n1ikm)
i_aibjciblai: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjciblai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciblai(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciblai
end do a_aibjciblai
end do j_aibjciblai
end do b_aibjciblai
end do c_aibjciblai
end do l_aibjciblai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: e == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjcibiam: do m = n0m, n1m
c_aibjcibiam: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibiam: do b = b0, n1bd
if (b == c) cycle b_aibjcibiam
j_aibjcibiam: do j = n0j, n1j
if (j == m) cycle j_aibjcibiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjcibiam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcibiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aibjcibiam: do i = n0ikl, i1
if (i == j .or. i == m) cycle i_aibjcibiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibiam(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibiam
end do a_aibjcibiam
end do j_aibjcibiam
end do b_aibjcibiam
end do c_aibjcibiam
end do m_aibjcibiam
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: e == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcibjam: do m = n0m, n1m
c_aibjcibjam: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibjam: do b = b0, n1bd
if (b == c) cycle b_aibjcibjam
j_aibjcibjam: do j = n0jl, n1jl
if (j == m) cycle j_aibjcibjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjcibjam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcibjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjcibjam: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjcibjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibjam(i, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjam
end do a_aibjcibjam
end do j_aibjcibjam
end do b_aibjcibjam
end do c_aibjcibjam
end do m_aibjcibjam
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j
! Equalities: e == a, d == b, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckbkak: do c = n0c, n1c
k_aibjckbkak: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbkak: do b = b0, n1bd
if (b == c) cycle b_aibjckbkak
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckbkak: do j = j0, n1j
if (j == k) cycle j_aibjckbkak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbkak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbkak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckbkak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbkak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbkak(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbkak
end do a_aibjckbkak
end do j_aibjckbkak
end do b_aibjckbkak
end do k_aibjckbkak
end do c_aibjckbkak
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: e == a, d == b, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckbjak: do c = n0c, n1c
k_aibjckbjak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjak: do b = b0, n1bd
if (b == c) cycle b_aibjckbjak
j0 = max(k + 1, n0jl)
j_aibjckbjak: do j = j0, n1jl
if (j == k) cycle j_aibjckbjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbjak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbjak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckbjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbjak(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjak
end do a_aibjckbjak
end do j_aibjckbjak
end do b_aibjckbjak
end do k_aibjckbjak
end do c_aibjckbjak
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: e == a, d == b, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkai: do c = n0c, n1c
k_aibjckbkai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbkai: do b = b0, n1bd
if (b == c) cycle b_aibjckbkai
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckbkai: do j = j0, n1j
if (j == k) cycle j_aibjckbkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbkai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbkai
i_aibjckbkai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbkai(a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbkai
end do a_aibjckbkai
end do j_aibjckbkai
end do b_aibjckbkai
end do k_aibjckbkai
end do c_aibjckbkai
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, m
! Equalities: e == a, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibickbkam: do m = n0m, n1m
c_aibickbkam: do c = n0c, n1c
k_aibickbkam: do k = n0kl, n1kl
if (k == m) cycle k_aibickbkam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbkam: do b = b0, n1bd
if (b == c) cycle b_aibickbkam
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibickbkam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickbkam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickbkam: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aibickbkam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbkam(i, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbkam
end do a_aibickbkam
end do b_aibickbkam
end do k_aibickbkam
end do c_aibickbkam
end do m_aibickbkam
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: e == a, d == b, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckbkaj: do c = n0c, n1c
k_aibjckbkaj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbkaj: do b = b0, n1bd
if (b == c) cycle b_aibjckbkaj
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjckbkaj: do j = j0, n1jm
if (j == k) cycle j_aibjckbkaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbkaj: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbkaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckbkaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbkaj(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbkaj
end do a_aibjckbkaj
end do j_aibjckbkaj
end do b_aibjckbkaj
end do k_aibjckbkaj
end do c_aibjckbkaj
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: e == a, d == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibickblai: do l = n0l, n1l
c_aibickblai: do c = n0c, n1c
k_aibickblai: do k = n0k, n1k
if (k == l) cycle k_aibickblai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickblai: do b = b0, n1bd
if (b == c) cycle b_aibickblai
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibickblai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickblai
i0 = max(k + 1, n0ijm)
i_aibickblai: do i = i0, n1ijm
if (i == k .or. i == l) cycle i_aibickblai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickblai(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickblai
end do a_aibickblai
end do b_aibickblai
end do k_aibickblai
end do c_aibickblai
end do l_aibickblai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: e == a, d == b, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckbiai: do c = n0c, n1c
k_aibjckbiai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbiai: do b = b0, n1bd
if (b == c) cycle b_aibjckbiai
j0 = max(k + 1, n0j)
j_aibjckbiai: do j = j0, n1j
if (j == k) cycle j_aibjckbiai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbiai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbiai
i_aibjckbiai: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjckbiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbiai(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbiai
end do a_aibjckbiai
end do j_aibjckbiai
end do b_aibjckbiai
end do k_aibjckbiai
end do c_aibjckbiai
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
end subroutine ccjac_32_tripletm_dav_part2
end module ccjac_block_32_tripletm_dav_part2
