module ccjac_block_31_triplet_dav_part1
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
subroutine ccjac_31_triplet_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0bd, n0cd, n0ij, n0ik, n0il
integer :: n0jl, n0kl
integer :: n1ab, n1abd, n1ac, n1acd, n1ad
integer :: n1bd, n1cd, n1ij, n1ik, n1il
integer :: n1jl, n1kl
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
n0ik = max(n0i, n0k)
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
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k, l
! Equalities: d == b
! No equalities independent of the above can hold.
!
l_aibjckbl: do l = n0l, n1l
c_aibjckbl: do c = n0c, n1c
k_aibjckbl: do k = n0k, n1k
if (k == l) cycle k_aibjckbl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbl: do b = b0, n1bd
if (b == c) cycle b_aibjckbl
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckbl: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjckbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbl: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbl
i_aibjckbl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckbl(t2, nocc, nactive, a, i, j, &
 c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbl
end do a_aibjckbl
end do j_aibjckbl
end do b_aibjckbl
end do k_aibjckbl
end do c_aibjckbl
end do l_aibjckbl
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, l
! Equalities: d == a
! No equalities independent of the above can hold.
!
l_aibjckal: do l = n0l, n1l
c_aibjckal: do c = n0c, n1c
k_aibjckal: do k = n0k, n1k
if (k == l) cycle k_aibjckal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckal: do b = b0, n1b
if (b == c) cycle b_aibjckal
j0 = max(k + 1, n0j)
j_aibjckal: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjckal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckal: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibjckal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckal: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckal(t2, nocc, nactive, i, b, j, &
 c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckal
end do a_aibjckal
end do j_aibjckal
end do b_aibjckal
end do k_aibjckal
end do c_aibjckal
end do l_aibjckal
!
! Elementary loop  3
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k, l
! Equalities: d == c
! No equalities independent of the above can hold.
!
l_aibjckcl: do l = n0l, n1l
c_aibjckcl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aibjckcl: do k = n0k, n1k
if (k == l) cycle k_aibjckcl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcl: do b = b0, n1b
if (b == c) cycle b_aibjckcl
j0 = max(k + 1, n0j)
j_aibjckcl: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjckcl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcl: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckcl
i_aibjckcl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckcl(t2, nocc, nactive, a, i, b, &
 j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcl
end do a_aibjckcl
end do j_aibjckcl
end do b_aibjckcl
end do k_aibjckcl
end do c_aibjckcl
end do l_aibjckcl
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: k, i, j
! Equalities: l == k
! No equalities independent of the above can hold.
!
d_aibjckdk: do d = n0d, n1d
c_aibjckdk: do c = n0c, n1c
if (c == d) cycle c_aibjckdk
k_aibjckdk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdk: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdk
j0 = max(k + 1, n0j)
j_aibjckdk: do j = j0, n1j
if (j == k) cycle j_aibjckdk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdk: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdk
i_aibjckdk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckdk(t2, nocc, nactive, a, i, b, &
 j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdk
end do a_aibjckdk
end do j_aibjckdk
end do b_aibjckdk
end do k_aibjckdk
end do c_aibjckdk
end do d_aibjckdk
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i, k
! Equalities: l == j
! No equalities independent of the above can hold.
!
d_aibjckdj: do d = n0d, n1d
c_aibjckdj: do c = n0c, n1c
if (c == d) cycle c_aibjckdj
k_aibjckdj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdj
j0 = max(k + 1, n0jl)
j_aibjckdj: do j = j0, n1jl
if (j == k) cycle j_aibjckdj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdj: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdj
i_aibjckdj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckdj(t2, nocc, nactive, a, i, b, &
 c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdj
end do a_aibjckdj
end do j_aibjckdj
end do b_aibjckdj
end do k_aibjckdj
end do c_aibjckdj
end do d_aibjckdj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: l == i
! No equalities independent of the above can hold.
!
d_aibjckdi: do d = n0d, n1d
c_aibjckdi: do c = n0c, n1c
if (c == d) cycle c_aibjckdi
k_aibjckdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdi: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdi
j0 = max(k + 1, n0j)
j_aibjckdi: do j = j0, n1j
if (j == k) cycle j_aibjckdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdi: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdi
i_aibjckdi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckdi(t2, nocc, nactive, a, b, j, &
 c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdi
end do a_aibjckdi
end do j_aibjckdi
end do b_aibjckdi
end do k_aibjckdi
end do c_aibjckdi
end do d_aibjckdi
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == a
! No equalities independent of the above can hold.
!
l_aiajckal: do l = n0l, n1l
c_aiajckal: do c = n0c, n1c
k_aiajckal: do k = n0k, n1k
if (k == l) cycle k_aiajckal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckal: do j = j0, n1j
if (j == k .or. j == l) cycle j_aiajckal
a0 = max(c + 1, n0abd)
a_aiajckal: do a = a0, n1abd
if (a == c) cycle a_aiajckal
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckal: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckal(t2, nocc, nactive, a, i, j, &
 c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckal
end do a_aiajckal
end do j_aiajckal
end do k_aiajckal
end do c_aiajckal
end do l_aiajckal
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == c
! No equalities independent of the above can hold.
!
l_aiajckcl: do l = n0l, n1l
c_aiajckcl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aiajckcl: do k = n0k, n1k
if (k == l) cycle k_aiajckcl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckcl: do j = j0, n1j
if (j == k .or. j == l) cycle j_aiajckcl
a0 = max(c + 1, n0ab)
a_aiajckcl: do a = a0, n1ab
if (a == c) cycle a_aiajckcl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckcl(t2, nocc, nactive, a, i, j, &
 k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckcl
end do a_aiajckcl
end do j_aiajckcl
end do k_aiajckcl
end do c_aiajckcl
end do l_aiajckcl
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: k, i, j
! Equalities: b == a, l == k
! No equalities independent of the above can hold.
!
d_aiajckdk: do d = n0d, n1d
c_aiajckdk: do c = n0c, n1c
if (c == d) cycle c_aiajckdk
k_aiajckdk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckdk: do j = j0, n1j
if (j == k) cycle j_aiajckdk
a0 = max(c + 1, n0ab)
a_aiajckdk: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdk
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckdk(t2, nocc, nactive, a, i, j, &
 c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdk
end do a_aiajckdk
end do j_aiajckdk
end do k_aiajckdk
end do c_aiajckdk
end do d_aiajckdk
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i, k
! Equalities: b == a, l == j
! No equalities independent of the above can hold.
!
d_aiajckdj: do d = n0d, n1d
c_aiajckdj: do c = n0c, n1c
if (c == d) cycle c_aiajckdj
k_aiajckdj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckdj: do j = j0, n1jl
if (j == k) cycle j_aiajckdj
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckdj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckdj(t2, nocc, nactive, a, i, c, &
 k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdj
end do a_aiajckdj
end do j_aiajckdj
end do k_aiajckdj
end do c_aiajckdj
end do d_aiajckdj
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, l == i
! No equalities independent of the above can hold.
!
d_aiajckdi: do d = n0d, n1d
c_aiajckdi: do c = n0c, n1c
if (c == d) cycle c_aiajckdi
k_aiajckdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckdi: do j = j0, n1j
if (j == k) cycle j_aiajckdi
a0 = max(c + 1, n0ab)
a_aiajckdi: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckdi(t2, nocc, nactive, a, j, c, &
 k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdi
end do a_aiajckdi
end do j_aiajckdi
end do k_aiajckdi
end do c_aiajckdi
end do d_aiajckdi
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b
! No equalities independent of the above can hold.
!
l_aibjakbl: do l = n0l, n1l
k_aibjakbl: do k = n0k, n1k
if (k == l) cycle k_aibjakbl
b_aibjakbl: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbl: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjakbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbl: do a = n0ac, a1
if (a == b) cycle a_aibjakbl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakbl(t2, nocc, nactive, a, i, j, &
 k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbl
end do a_aibjakbl
end do j_aibjakbl
end do b_aibjakbl
end do k_aibjakbl
end do l_aibjakbl
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl: do l = n0l, n1l
c_aibjcibl: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibl: do b = b0, n1bd
if (b == c) cycle b_aibjcibl
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcibl: do j = n0j, n1j
if (j == l) cycle j_aibjcibl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibl: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibl
i1 = min(j - 1, n1ik)
i_aibjcibl: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjcibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjcibl(t2, nocc, nactive, a, i, j, &
 c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibl
end do a_aibjcibl
end do j_aibjcibl
end do b_aibjcibl
end do c_aibjcibl
end do l_aibjcibl
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: k, i, j
! Equalities: d == b, l == k
! No equalities independent of the above can hold.
!
c_aibjckbk: do c = n0c, n1c
k_aibjckbk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbk: do b = b0, n1bd
if (b == c) cycle b_aibjckbk
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckbk: do j = j0, n1j
if (j == k) cycle j_aibjckbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbk: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbk
i_aibjckbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckbk(t2, nocc, nactive, a, i, b, &
 j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbk
end do a_aibjckbk
end do j_aibjckbk
end do b_aibjckbk
end do k_aibjckbk
end do c_aibjckbk
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k, l
! Equalities: d == b, j == i
! No equalities independent of the above can hold.
!
l_aibickbl: do l = n0l, n1l
c_aibickbl: do c = n0c, n1c
k_aibickbl: do k = n0k, n1k
if (k == l) cycle k_aibickbl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbl: do b = b0, n1bd
if (b == c) cycle b_aibickbl
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a_aibickbl: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbl
i0 = max(k + 1, n0ij)
i_aibickbl: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibickbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickbl(t2, nocc, nactive, a, i, c, &
 k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbl
end do a_aibickbl
end do b_aibickbl
end do k_aibickbl
end do c_aibickbl
end do l_aibickbl
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj: do c = n0c, n1c
k_aibjckbj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbj: do b = b0, n1bd
if (b == c) cycle b_aibjckbj
j0 = max(k + 1, n0jl)
j_aibjckbj: do j = j0, n1jl
if (j == k) cycle j_aibjckbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbj
i_aibjckbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckbj(t2, nocc, nactive, a, i, b, &
 j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbj
end do a_aibjckbj
end do j_aibjckbj
end do b_aibjckbj
end do k_aibjckbj
end do c_aibjckbj
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, l == i
! No equalities independent of the above can hold.
!
c_aibjckbi: do c = n0c, n1c
k_aibjckbi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbi: do b = b0, n1bd
if (b == c) cycle b_aibjckbi
j0 = max(k + 1, n0j)
j_aibjckbi: do j = j0, n1j
if (j == k) cycle j_aibjckbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbi: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbi
i_aibjckbi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckbi(t2, nocc, nactive, a, i, b, &
 j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbi
end do a_aibjckbi
end do j_aibjckbi
end do b_aibjckbi
end do k_aibjckbi
end do c_aibjckbi
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == a
! No equalities independent of the above can hold.
!
l_aibjakal: do l = n0l, n1l
k_aibjakal: do k = n0k, n1k
if (k == l) cycle k_aibjakal
b_aibjakal: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjakal: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjakal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjakal: do a = n0acd, a1
if (a == b) cycle a_aibjakal
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjakal: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakal(t2, nocc, nactive, a, i, b, &
 j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakal
end do a_aibjakal
end do j_aibjakal
end do b_aibjakal
end do k_aibjakal
end do l_aibjakal
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: k, i, j
! Equalities: c == a, l == k
! No equalities independent of the above can hold.
!
d_aibjakdk: do d = n0d, n1d
k_aibjakdk: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjakdk: do b = n0b, n1b
if (b == d) cycle b_aibjakdk
j0 = max(k + 1, n0j)
j_aibjakdk: do j = j0, n1j
if (j == k) cycle j_aibjakdk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakdk: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjakdk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakdk(t2, nocc, nactive, a, i, b, &
 j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdk
end do a_aibjakdk
end do j_aibjakdk
end do b_aibjakdk
end do k_aibjakdk
end do d_aibjakdk
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k
! Equalities: c == a, l == j
! No equalities independent of the above can hold.
!
d_aibjakdj: do d = n0d, n1d
k_aibjakdj: do k = n0k, n1k
b_aibjakdj: do b = n0b, n1b
if (b == d) cycle b_aibjakdj
j0 = max(k + 1, n0jl)
j_aibjakdj: do j = j0, n1jl
if (j == k) cycle j_aibjakdj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakdj: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjakdj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakdj(t2, nocc, nactive, a, i, b, &
 k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdj
end do a_aibjakdj
end do j_aibjakdj
end do b_aibjakdj
end do k_aibjakdj
end do d_aibjakdj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, l == i
! No equalities independent of the above can hold.
!
d_aibjakdi: do d = n0d, n1d
k_aibjakdi: do k = n0k, n1k
b_aibjakdi: do b = n0b, n1b
if (b == d) cycle b_aibjakdi
j0 = max(k + 1, n0j)
j_aibjakdi: do j = j0, n1j
if (j == k) cycle j_aibjakdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakdi: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakdi(t2, nocc, nactive, a, b, j, &
 k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdi
end do a_aibjakdi
end do j_aibjakdi
end do b_aibjakdi
end do k_aibjakdi
end do d_aibjakdi
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjcial: do l = n0l, n1l
c_aibjcial: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjcial: do b = b0, n1b
if (b == c) cycle b_aibjcial
j_aibjcial: do j = n0j, n1j
if (j == l) cycle j_aibjcial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcial: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibjcial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjcial: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjcial(t2, nocc, nactive, i, b, j, &
 c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcial
end do a_aibjcial
end do j_aibjcial
end do b_aibjcial
end do c_aibjcial
end do l_aibjcial
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j
! Equalities: d == a, l == k
! No equalities independent of the above can hold.
!
c_aibjckak: do c = n0c, n1c
k_aibjckak: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckak: do b = b0, n1b
if (b == c) cycle b_aibjckak
j0 = max(k + 1, n0j)
j_aibjckak: do j = j0, n1j
if (j == k) cycle j_aibjckak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckak: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibjckak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckak(t2, nocc, nactive, a, i, b, &
 j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckak
end do a_aibjckak
end do j_aibjckak
end do b_aibjckak
end do k_aibjckak
end do c_aibjckak
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, j == i
! No equalities independent of the above can hold.
!
l_aibickal: do l = n0l, n1l
c_aibickal: do c = n0c, n1c
k_aibickal: do k = n0k, n1k
if (k == l) cycle k_aibickal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickal: do b = b0, n1b
if (b == c) cycle b_aibickal
a_aibickal: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibickal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickal: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibickal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickal(t2, nocc, nactive, i, b, c, &
 k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickal
end do a_aibickal
end do b_aibickal
end do k_aibickal
end do c_aibickal
end do l_aibickal
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, l == j
! No equalities independent of the above can hold.
!
c_aibjckaj: do c = n0c, n1c
k_aibjckaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaj: do b = b0, n1b
if (b == c) cycle b_aibjckaj
j0 = max(k + 1, n0jl)
j_aibjckaj: do j = j0, n1jl
if (j == k) cycle j_aibjckaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaj: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibjckaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckaj(t2, nocc, nactive, a, i, b, &
 j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaj
end do a_aibjckaj
end do j_aibjckaj
end do b_aibjckaj
end do k_aibjckaj
end do c_aibjckaj
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, l == i
! No equalities independent of the above can hold.
!
c_aibjckai: do c = n0c, n1c
k_aibjckai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckai: do b = b0, n1b
if (b == c) cycle b_aibjckai
j0 = max(k + 1, n0j)
j_aibjckai: do j = j0, n1j
if (j == k) cycle j_aibjckai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckai: do a = n0ad, n1ad
if (a == b .or. a == c) cycle a_aibjckai
i_aibjckai: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckai(t2, nocc, nactive, a, i, b, &
 j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckai
end do a_aibjckai
end do j_aibjckai
end do b_aibjckai
end do k_aibjckai
end do c_aibjckai
!
! Elementary loop  27
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, l
! Equalities: d == c, k == i
! No equalities independent of the above can hold.
!
l_aibjcicl: do l = n0l, n1l
c_aibjcicl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjcicl: do b = b0, n1b
if (b == c) cycle b_aibjcicl
j_aibjcicl: do j = n0j, n1j
if (j == l) cycle j_aibjcicl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicl: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcicl
i1 = min(j - 1, n1ik)
i_aibjcicl: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjcicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjcicl(t2, nocc, nactive, a, i, b, &
 j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcicl
end do a_aibjcicl
end do j_aibjcicl
end do b_aibjcicl
end do c_aibjcicl
end do l_aibjcicl
!
! Elementary loop  28
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: k, i, j
! Equalities: d == c, l == k
! No equalities independent of the above can hold.
!
c_aibjckck: do c = n0cd, n1cd
k_aibjckck: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckck: do b = b0, n1b
if (b == c) cycle b_aibjckck
j0 = max(k + 1, n0j)
j_aibjckck: do j = j0, n1j
if (j == k) cycle j_aibjckck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckck: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckck
i_aibjckck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckck(t2, nocc, nactive, a, i, b, &
 j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckck
end do a_aibjckck
end do j_aibjckck
end do b_aibjckck
end do k_aibjckck
end do c_aibjckck
!
! Elementary loop  29
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k, l
! Equalities: d == c, j == i
! No equalities independent of the above can hold.
!
l_aibickcl: do l = n0l, n1l
c_aibickcl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aibickcl: do k = n0k, n1k
if (k == l) cycle k_aibickcl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickcl: do b = b0, n1b
if (b == c) cycle b_aibickcl
a_aibickcl: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickcl
i0 = max(k + 1, n0ij)
i_aibickcl: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibickcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickcl(t2, nocc, nactive, a, i, b, &
 k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickcl
end do a_aibickcl
end do b_aibickcl
end do k_aibickcl
end do c_aibickcl
end do l_aibickcl
!
! Elementary loop  30
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i, k
! Equalities: d == c, l == j
! No equalities independent of the above can hold.
!
c_aibjckcj: do c = n0cd, n1cd
k_aibjckcj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcj: do b = b0, n1b
if (b == c) cycle b_aibjckcj
j0 = max(k + 1, n0jl)
j_aibjckcj: do j = j0, n1jl
if (j == k) cycle j_aibjckcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckcj
i_aibjckcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckcj(t2, nocc, nactive, a, i, b, &
 j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcj
end do a_aibjckcj
end do j_aibjckcj
end do b_aibjckcj
end do k_aibjckcj
end do c_aibjckcj
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
end subroutine ccjac_31_triplet_dav_part1
end module ccjac_block_31_triplet_dav_part1
