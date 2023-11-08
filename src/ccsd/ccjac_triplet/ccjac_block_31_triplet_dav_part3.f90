module ccjac_block_31_triplet_dav_part3
use cc_gparams
use v1_eom_cc3_31_triplet_trans
use v2_eom_cc3_31_triplet_trans
use v3_eom_cc3_31_triplet_trans
use v4_eom_cc3_31_triplet_trans
use v5_eom_cc3_31_triplet_trans
use v6_eom_cc3_31_triplet_trans
use v7_eom_cc3_31_triplet_trans
use v8_eom_cc3_31_triplet_trans
use v9_eom_cc3_31_triplet_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2017-01-20 13:57:18 UTC.
!
contains
subroutine ccjac_31_triplet_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
 nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, &
 n1k, n0l, n1l, bra0, ket0) 
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: a0, a1, b0, i0, i1, j0
integer :: n0ab, n0abd, n0ac, n0acd, n0ad
integer :: n0bd, n0cd, n0ij, n0ijl, n0ik
integer :: n0ikl, n0il, n0jl, n0kl
integer :: n1ab, n1abd, n1ac, n1acd, n1ad
integer :: n1bd, n1cd, n1ij, n1ijl, n1ik
integer :: n1ikl, n1il, n1jl, n1kl
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
n0ab = max(n0a, n0b)
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0ad = max(n0a, n0d)
n0bd = max(n0b, n0d)
n0cd = max(n0c, n0d)
n0ij = max(n0i, n0j)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ad = min(n1a, n1d)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == a, j == i
! No equalities independent of the above can hold.
!
l_aibiakal: do l = n0l, n1l
k_aibiakal: do k = n0k, n1k
if (k == l) cycle k_aibiakal
b_aibiakal: do b = n0b, n1b
a1 = min(b - 1, n1acd)
a_aibiakal: do a = n0acd, a1
if (a == b) cycle a_aibiakal
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakal: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibiakal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v7_eom_cc3_31_triplet_trans_aibiakal(t2, nocc, nactive, a, i, b, &
 k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakal
end do a_aibiakal
end do b_aibiakal
end do k_aibiakal
end do l_aibiakal
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == a, l == j
! No equalities independent of the above can hold.
!
k_aibjakaj: do k = n0k, n1k
b_aibjakaj: do b = n0b, n1b
j0 = max(k + 1, n0jl)
j_aibjakaj: do j = j0, n1jl
if (j == k) cycle j_aibjakaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjakaj: do a = n0acd, a1
if (a == b) cycle a_aibjakaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakaj(t2, nocc, nactive, a, i, b, &
 j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakaj
end do a_aibjakaj
end do j_aibjakaj
end do b_aibjakaj
end do k_aibjakaj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aibjakai: do k = n0k, n1k
b_aibjakai: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjakai: do j = j0, n1j
if (j == k) cycle j_aibjakai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjakai: do a = n0acd, a1
if (a == b) cycle a_aibjakai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakai: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakai(t2, nocc, nactive, a, i, b, &
 j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakai
end do a_aibjakai
end do j_aibjakai
end do b_aibjakai
end do k_aibjakai
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjaidi: do d = n0d, n1d
b_aibjaidi: do b = n0b, n1b
if (b == d) cycle b_aibjaidi
j_aibjaidi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaidi: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjaidi
i1 = min(j - 1, n1ikl)
i_aibjaidi: do i = n0ikl, i1
if (i == j) cycle i_aibjaidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v8_eom_cc3_31_triplet_trans_aibjaidi(t2, nocc, nactive, a, i, b, &
 j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaidi
end do a_aibjaidi
end do j_aibjaidi
end do b_aibjaidi
end do d_aibjaidi
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj: do d = n0d, n1d
b_aibjaidj: do b = n0b, n1b
if (b == d) cycle b_aibjaidj
j_aibjaidj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaidj: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjaidj
i1 = min(j - 1, n1ik)
i_aibjaidj: do i = n0ik, i1
if (i == j) cycle i_aibjaidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v8_eom_cc3_31_triplet_trans_aibjaidj(t2, nocc, nactive, a, i, b, &
 d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaidj
end do a_aibjaidj
end do j_aibjaidj
end do b_aibjaidj
end do d_aibjaidj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aibiakdk: do d = n0d, n1d
k_aibiakdk: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibiakdk: do b = n0b, n1b
if (b == d) cycle b_aibiakdk
a1 = min(b - 1, n1ac)
a_aibiakdk: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibiakdk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakdk: do i = i0, n1ij
if (i == k) cycle i_aibiakdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v7_eom_cc3_31_triplet_trans_aibiakdk(t2, nocc, nactive, a, i, b, &
 d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakdk
end do a_aibiakdk
end do b_aibiakdk
end do k_aibiakdk
end do d_aibiakdk
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibiakdi: do d = n0d, n1d
k_aibiakdi: do k = n0k, n1k
b_aibiakdi: do b = n0b, n1b
if (b == d) cycle b_aibiakdi
a1 = min(b - 1, n1ac)
a_aibiakdi: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibiakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakdi: do i = i0, n1ijl
if (i == k) cycle i_aibiakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v7_eom_cc3_31_triplet_trans_aibiakdi(t2, nocc, nactive, a, i, b, &
 k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakdi
end do a_aibiakdi
end do b_aibiakdi
end do k_aibiakdi
end do d_aibiakdi
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjciai: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjciai: do b = b0, n1b
if (b == c) cycle b_aibjciai
j_aibjciai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciai: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibjciai
i1 = min(j - 1, n1ikl)
i_aibjciai: do i = n0ikl, i1
if (i == j) cycle i_aibjciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjciai(t2, nocc, nactive, a, i, b, &
 j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciai
end do a_aibjciai
end do j_aibjciai
end do b_aibjciai
end do c_aibjciai
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjciaj: do b = b0, n1b
if (b == c) cycle b_aibjciaj
j_aibjciaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciaj: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibjciaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjciaj: do i = n0ik, i1
if (i == j) cycle i_aibjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjciaj(t2, nocc, nactive, a, i, b, &
 j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciaj
end do a_aibjciaj
end do j_aibjciaj
end do b_aibjciaj
end do c_aibjciaj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == k
! No equalities independent of the above can hold.
!
c_aibickak: do c = n0c, n1c
k_aibickak: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickak: do b = b0, n1b
if (b == c) cycle b_aibickak
a_aibickak: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibickak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickak: do i = i0, n1ij
if (i == k) cycle i_aibickak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickak(t2, nocc, nactive, a, i, b, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickak
end do a_aibickak
end do b_aibickak
end do k_aibickak
end do c_aibickak
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickai: do c = n0c, n1c
k_aibickai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickai: do b = b0, n1b
if (b == c) cycle b_aibickai
a_aibickai: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibickai
i0 = max(k + 1, n0ijl)
i_aibickai: do i = i0, n1ijl
if (i == k) cycle i_aibickai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickai(t2, nocc, nactive, a, i, b, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickai
end do a_aibickai
end do b_aibickai
end do k_aibickai
end do c_aibickai
!
! Elementary loop  12
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcici: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcici: do b = b0, n1b
if (b == c) cycle b_aibjcici
j_aibjcici: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcici: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcici
i1 = min(j - 1, n1ikl)
i_aibjcici: do i = n0ikl, i1
if (i == j) cycle i_aibjcici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjcici(t2, nocc, nactive, a, i, b, &
 j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcici
end do a_aibjcici
end do j_aibjcici
end do b_aibjcici
end do c_aibjcici
!
! Elementary loop  13
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcicj: do b = b0, n1b
if (b == c) cycle b_aibjcicj
j_aibjcicj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcicj
i1 = min(j - 1, n1ik)
i_aibjcicj: do i = n0ik, i1
if (i == j) cycle i_aibjcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjcicj(t2, nocc, nactive, a, i, b, &
 j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcicj
end do a_aibjcicj
end do j_aibjcicj
end do b_aibjcicj
end do c_aibjcicj
!
! Elementary loop  14
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k
! Equalities: d == c, j == i, l == k
! No equalities independent of the above can hold.
!
c_aibickck: do c = n0cd, n1cd
k_aibickck: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickck: do b = b0, n1b
if (b == c) cycle b_aibickck
a_aibickck: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickck
i0 = max(k + 1, n0ij)
i_aibickck: do i = i0, n1ij
if (i == k) cycle i_aibickck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickck(t2, nocc, nactive, a, i, b, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickck
end do a_aibickck
end do b_aibickck
end do k_aibickck
end do c_aibickck
!
! Elementary loop  15
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k
! Equalities: d == c, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickci: do c = n0cd, n1cd
k_aibickci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickci: do b = b0, n1b
if (b == c) cycle b_aibickci
a_aibickci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickci
i0 = max(k + 1, n0ijl)
i_aibickci: do i = i0, n1ijl
if (i == k) cycle i_aibickci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickci(t2, nocc, nactive, a, i, b, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickci
end do a_aibickci
end do b_aibickci
end do k_aibickci
end do c_aibickci
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiajciai: do c = n0c, n1c
j_aiajciai: do j = n0j, n1j
a0 = max(c + 1, n0abd)
a_aiajciai: do a = a0, n1abd
if (a == c) cycle a_aiajciai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aiajciai: do i = n0ikl, i1
if (i == j) cycle i_aiajciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_triplet_trans_aiajciai(t2, nocc, nactive, a, i, j, &
 c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciai
end do a_aiajciai
end do j_aiajciai
end do c_aiajciai
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciaj: do c = n0c, n1c
j_aiajciaj: do j = n0jl, n1jl
a0 = max(c + 1, n0abd)
a_aiajciaj: do a = a0, n1abd
if (a == c) cycle a_aiajciaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajciaj: do i = n0ik, i1
if (i == j) cycle i_aiajciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_triplet_trans_aiajciaj(t2, nocc, nactive, a, i, j, &
 c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciaj
end do a_aiajciaj
end do j_aiajciaj
end do c_aiajciaj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == k
! No equalities independent of the above can hold.
!
c_aiaickak: do c = n0c, n1c
k_aiaickak: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickak: do a = a0, n1abd
if (a == c) cycle a_aiaickak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaickak: do i = i0, n1ij
if (i == k) cycle i_aiaickak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v5_eom_cc3_31_triplet_trans_aiaickak(t2, nocc, nactive, a, i, c, &
 k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickak
end do a_aiaickak
end do k_aiaickak
end do c_aiaickak
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aiaickai: do c = n0c, n1c
k_aiaickai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickai: do a = a0, n1abd
if (a == c) cycle a_aiaickai
i0 = max(k + 1, n0ijl)
i_aiaickai: do i = i0, n1ijl
if (i == k) cycle i_aiaickai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v5_eom_cc3_31_triplet_trans_aiaickai(t2, nocc, nactive, a, i, c, &
 k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickai
end do a_aiaickai
end do k_aiaickai
end do c_aiaickai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiajcici: do c = n0cd, n1cd
j_aiajcici: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajcici: do a = a0, n1ab
if (a == c) cycle a_aiajcici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aiajcici: do i = n0ikl, i1
if (i == j) cycle i_aiajcici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_triplet_trans_aiajcici(t2, nocc, nactive, a, i, j, &
 c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcici
end do a_aiajcici
end do j_aiajcici
end do c_aiajcici
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicj: do c = n0cd, n1cd
j_aiajcicj: do j = n0jl, n1jl
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcicj: do a = a0, n1ab
if (a == c) cycle a_aiajcicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajcicj: do i = n0ik, i1
if (i == j) cycle i_aiajcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_triplet_trans_aiajcicj(t2, nocc, nactive, a, i, j, &
 c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcicj
end do a_aiajcicj
end do j_aiajcicj
end do c_aiajcicj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == c, j == i, l == k
! No equalities independent of the above can hold.
!
c_aiaickck: do c = n0cd, n1cd
k_aiaickck: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickck: do a = a0, n1ab
if (a == c) cycle a_aiaickck
i0 = max(k + 1, n0ij)
i_aiaickck: do i = i0, n1ij
if (i == k) cycle i_aiaickck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v5_eom_cc3_31_triplet_trans_aiaickck(t2, nocc, nactive, a, i, c, &
 k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickck
end do a_aiaickck
end do k_aiaickck
end do c_aiaickck
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == c, j == i, l == i
! No equalities independent of the above can hold.
!
c_aiaickci: do c = n0cd, n1cd
k_aiaickci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickci: do a = a0, n1ab
if (a == c) cycle a_aiaickci
i0 = max(k + 1, n0ijl)
i_aiaickci: do i = i0, n1ijl
if (i == k) cycle i_aiaickci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v5_eom_cc3_31_triplet_trans_aiaickci(t2, nocc, nactive, a, i, c, &
 k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickci
end do a_aiaickci
end do k_aiaickci
end do c_aiaickci
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjaibi: do b = n0bd, n1bd
j_aibjaibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaibi: do a = n0ac, a1
if (a == b) cycle a_aibjaibi
i1 = min(j - 1, n1ikl)
i_aibjaibi: do i = n0ikl, i1
if (i == j) cycle i_aibjaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v8_eom_cc3_31_triplet_trans_aibjaibi(t2, nocc, nactive, a, i, b, &
 j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibi
end do a_aibjaibi
end do j_aibjaibi
end do b_aibjaibi
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj: do b = n0bd, n1bd
j_aibjaibj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaibj: do a = n0ac, a1
if (a == b) cycle a_aibjaibj
i1 = min(j - 1, n1ik)
i_aibjaibj: do i = n0ik, i1
if (i == j) cycle i_aibjaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v8_eom_cc3_31_triplet_trans_aibjaibj(t2, nocc, nactive, a, i, b, &
 j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibj
end do a_aibjaibj
end do j_aibjaibj
end do b_aibjaibj
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakbk: do k = n0kl, n1kl
b_aibiakbk: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibiakbk: do a = n0ac, a1
if (a == b) cycle a_aibiakbk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakbk: do i = i0, n1ij
if (i == k) cycle i_aibiakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v7_eom_cc3_31_triplet_trans_aibiakbk(t2, nocc, nactive, a, i, b, &
 k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbk
end do a_aibiakbk
end do b_aibiakbk
end do k_aibiakbk
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakbi: do k = n0k, n1k
b_aibiakbi: do b = n0bd, n1bd
a1 = min(b - 1, n1ac)
a_aibiakbi: do a = n0ac, a1
if (a == b) cycle a_aibiakbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakbi: do i = i0, n1ijl
if (i == k) cycle i_aibiakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v7_eom_cc3_31_triplet_trans_aibiakbi(t2, nocc, nactive, a, i, b, &
 k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbi
end do a_aibiakbi
end do b_aibiakbi
end do k_aibiakbi
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjaiai: do b = n0b, n1b
j_aibjaiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjaiai: do a = n0acd, a1
if (a == b) cycle a_aibjaiai
i1 = min(j - 1, n1ikl)
i_aibjaiai: do i = n0ikl, i1
if (i == j) cycle i_aibjaiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v8_eom_cc3_31_triplet_trans_aibjaiai(t2, nocc, nactive, a, i, b, &
 j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiai
end do a_aibjaiai
end do j_aibjaiai
end do b_aibjaiai
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiaj: do b = n0b, n1b
j_aibjaiaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjaiaj: do a = n0acd, a1
if (a == b) cycle a_aibjaiaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjaiaj: do i = n0ik, i1
if (i == j) cycle i_aibjaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v8_eom_cc3_31_triplet_trans_aibjaiaj(t2, nocc, nactive, a, i, b, &
 j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiaj
end do a_aibjaiaj
end do j_aibjaiaj
end do b_aibjaiaj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakak: do k = n0kl, n1kl
b_aibiakak: do b = n0b, n1b
a1 = min(b - 1, n1acd)
a_aibiakak: do a = n0acd, a1
if (a == b) cycle a_aibiakak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakak: do i = i0, n1ij
if (i == k) cycle i_aibiakak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v7_eom_cc3_31_triplet_trans_aibiakak(t2, nocc, nactive, a, i, b, &
 k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakak
end do a_aibiakak
end do b_aibiakak
end do k_aibiakak
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
end subroutine ccjac_31_triplet_dav_part3
end module ccjac_block_31_triplet_dav_part3
