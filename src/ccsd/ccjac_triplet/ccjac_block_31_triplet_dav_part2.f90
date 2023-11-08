module ccjac_block_31_triplet_dav_part2
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
subroutine ccjac_31_triplet_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0ab, n0abd, n0ac, n0acd, n0bd
integer :: n0cd, n0ij, n0ijl, n0ik, n0ikl
integer :: n0il, n0jl, n0kl
integer :: n1ab, n1abd, n1ac, n1acd, n1bd
integer :: n1cd, n1ij, n1ijl, n1ik, n1ikl
integer :: n1il, n1jl, n1kl
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
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, l == i
! No equalities independent of the above can hold.
!
c_aibjckci: do c = n0cd, n1cd
k_aibjckci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckci: do b = b0, n1b
if (b == c) cycle b_aibjckci
j0 = max(k + 1, n0j)
j_aibjckci: do j = j0, n1j
if (j == k) cycle j_aibjckci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckci
i_aibjckci: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v9_eom_cc3_31_triplet_trans_aibjckci(t2, nocc, nactive, a, i, b, &
 j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckci
end do a_aibjckci
end do j_aibjckci
end do b_aibjckci
end do k_aibjckci
end do c_aibjckci
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjcidi: do d = n0d, n1d
c_aibjcidi: do c = n0c, n1c
if (c == d) cycle c_aibjcidi
b0 = max(c + 1, n0b)
b_aibjcidi: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidi
j_aibjcidi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidi: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidi
i1 = min(j - 1, n1ikl)
i_aibjcidi: do i = n0ikl, i1
if (i == j) cycle i_aibjcidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjcidi(t2, nocc, nactive, a, i, b, &
 j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidi
end do a_aibjcidi
end do j_aibjcidi
end do b_aibjcidi
end do c_aibjcidi
end do d_aibjcidi
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidj: do d = n0d, n1d
c_aibjcidj: do c = n0c, n1c
if (c == d) cycle c_aibjcidj
b0 = max(c + 1, n0b)
b_aibjcidj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidj
j_aibjcidj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidj: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidj
i1 = min(j - 1, n1ik)
i_aibjcidj: do i = n0ik, i1
if (i == j) cycle i_aibjcidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjcidj(t2, nocc, nactive, a, i, b, &
 c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidj
end do a_aibjcidj
end do j_aibjcidj
end do b_aibjcidj
end do c_aibjcidj
end do d_aibjcidj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k
! Equalities: j == i, l == k
! No equalities independent of the above can hold.
!
d_aibickdk: do d = n0d, n1d
c_aibickdk: do c = n0c, n1c
if (c == d) cycle c_aibickdk
k_aibickdk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickdk: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibickdk
a_aibickdk: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibickdk
i0 = max(k + 1, n0ij)
i_aibickdk: do i = i0, n1ij
if (i == k) cycle i_aibickdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickdk(t2, nocc, nactive, a, i, b, &
 c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdk
end do a_aibickdk
end do b_aibickdk
end do k_aibickdk
end do c_aibickdk
end do d_aibickdk
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k
! Equalities: j == i, l == i
! No equalities independent of the above can hold.
!
d_aibickdi: do d = n0d, n1d
c_aibickdi: do c = n0c, n1c
if (c == d) cycle c_aibickdi
k_aibickdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickdi: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibickdi
a_aibickdi: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibickdi
i0 = max(k + 1, n0ijl)
i_aibickdi: do i = i0, n1ijl
if (i == k) cycle i_aibickdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickdi(t2, nocc, nactive, a, i, b, &
 c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdi
end do a_aibickdi
end do b_aibickdi
end do k_aibickdi
end do c_aibickdi
end do d_aibickdi
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajcial: do l = n0l, n1l
c_aiajcial: do c = n0c, n1c
j_aiajcial: do j = n0j, n1j
if (j == l) cycle j_aiajcial
a0 = max(c + 1, n0abd)
a_aiajcial: do a = a0, n1abd
if (a == c) cycle a_aiajcial
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajcial: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aiajcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_triplet_trans_aiajcial(t2, nocc, nactive, a, i, j, &
 c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcial
end do a_aiajcial
end do j_aiajcial
end do c_aiajcial
end do l_aiajcial
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == a, l == k
! No equalities independent of the above can hold.
!
c_aiajckak: do c = n0c, n1c
k_aiajckak: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckak: do j = j0, n1j
if (j == k) cycle j_aiajckak
a0 = max(c + 1, n0abd)
a_aiajckak: do a = a0, n1abd
if (a == c) cycle a_aiajckak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckak(t2, nocc, nactive, a, i, j, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckak
end do a_aiajckak
end do j_aiajckak
end do k_aiajckak
end do c_aiajckak
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, j == i
! No equalities independent of the above can hold.
!
l_aiaickal: do l = n0l, n1l
c_aiaickal: do c = n0c, n1c
k_aiaickal: do k = n0k, n1k
if (k == l) cycle k_aiaickal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickal: do a = a0, n1abd
if (a == c) cycle a_aiaickal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaickal: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aiaickal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v5_eom_cc3_31_triplet_trans_aiaickal(t2, nocc, nactive, a, i, c, &
 k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickal
end do a_aiaickal
end do k_aiaickal
end do c_aiaickal
end do l_aiaickal
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, l == j
! No equalities independent of the above can hold.
!
c_aiajckaj: do c = n0c, n1c
k_aiajckaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckaj: do j = j0, n1jl
if (j == k) cycle j_aiajckaj
a0 = max(c + 1, n0abd)
a_aiajckaj: do a = a0, n1abd
if (a == c) cycle a_aiajckaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckaj(t2, nocc, nactive, a, i, j, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaj
end do a_aiajckaj
end do j_aiajckaj
end do k_aiajckaj
end do c_aiajckaj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i
! No equalities independent of the above can hold.
!
c_aiajckai: do c = n0c, n1c
k_aiajckai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckai: do j = j0, n1j
if (j == k) cycle j_aiajckai
a0 = max(c + 1, n0abd)
a_aiajckai: do a = a0, n1abd
if (a == c) cycle a_aiajckai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckai: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckai(t2, nocc, nactive, a, i, j, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckai
end do a_aiajckai
end do j_aiajckai
end do k_aiajckai
end do c_aiajckai
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == c, k == i
! No equalities independent of the above can hold.
!
l_aiajcicl: do l = n0l, n1l
c_aiajcicl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j_aiajcicl: do j = n0j, n1j
if (j == l) cycle j_aiajcicl
a0 = max(c + 1, n0ab)
a_aiajcicl: do a = a0, n1ab
if (a == c) cycle a_aiajcicl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajcicl: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aiajcicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_triplet_trans_aiajcicl(t2, nocc, nactive, a, i, j, &
 l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcicl
end do a_aiajcicl
end do j_aiajcicl
end do c_aiajcicl
end do l_aiajcicl
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == c, l == k
! No equalities independent of the above can hold.
!
c_aiajckck: do c = n0cd, n1cd
k_aiajckck: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckck: do j = j0, n1j
if (j == k) cycle j_aiajckck
a0 = max(c + 1, n0ab)
a_aiajckck: do a = a0, n1ab
if (a == c) cycle a_aiajckck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckck(t2, nocc, nactive, a, i, j, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckck
end do a_aiajckck
end do j_aiajckck
end do k_aiajckck
end do c_aiajckck
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == c, j == i
! No equalities independent of the above can hold.
!
l_aiaickcl: do l = n0l, n1l
c_aiaickcl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aiaickcl: do k = n0k, n1k
if (k == l) cycle k_aiaickcl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickcl: do a = a0, n1ab
if (a == c) cycle a_aiaickcl
i0 = max(k + 1, n0ij)
i_aiaickcl: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aiaickcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v5_eom_cc3_31_triplet_trans_aiaickcl(t2, nocc, nactive, a, i, k, &
 l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickcl
end do a_aiaickcl
end do k_aiaickcl
end do c_aiaickcl
end do l_aiaickcl
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == c, l == j
! No equalities independent of the above can hold.
!
c_aiajckcj: do c = n0cd, n1cd
k_aiajckcj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckcj: do j = j0, n1jl
if (j == k) cycle j_aiajckcj
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckcj: do a = a0, n1ab
if (a == c) cycle a_aiajckcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckcj(t2, nocc, nactive, a, i, j, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckcj
end do a_aiajckcj
end do j_aiajckcj
end do k_aiajckcj
end do c_aiajckcj
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, l == i
! No equalities independent of the above can hold.
!
c_aiajckci: do c = n0cd, n1cd
k_aiajckci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckci: do j = j0, n1j
if (j == k) cycle j_aiajckci
a0 = max(c + 1, n0ab)
a_aiajckci: do a = a0, n1ab
if (a == c) cycle a_aiajckci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckci: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v1_eom_cc3_31_triplet_trans_aiajckci(t2, nocc, nactive, a, i, j, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckci
end do a_aiajckci
end do j_aiajckci
end do k_aiajckci
end do c_aiajckci
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiajcidi: do d = n0d, n1d
c_aiajcidi: do c = n0c, n1c
if (c == d) cycle c_aiajcidi
j_aiajcidi: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajcidi: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aiajcidi: do i = n0ikl, i1
if (i == j) cycle i_aiajcidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_triplet_trans_aiajcidi(t2, nocc, nactive, a, i, j, &
 c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcidi
end do a_aiajcidi
end do j_aiajcidi
end do c_aiajcidi
end do d_aiajcidi
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidj: do d = n0d, n1d
c_aiajcidj: do c = n0c, n1c
if (c == d) cycle c_aiajcidj
j_aiajcidj: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcidj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajcidj: do i = n0ik, i1
if (i == j) cycle i_aiajcidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_triplet_trans_aiajcidj(t2, nocc, nactive, a, i, c, &
 d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcidj
end do a_aiajcidj
end do j_aiajcidj
end do c_aiajcidj
end do d_aiajcidj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aiaickdk: do d = n0d, n1d
c_aiaickdk: do c = n0c, n1c
if (c == d) cycle c_aiaickdk
k_aiaickdk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickdk: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiaickdk
i0 = max(k + 1, n0ij)
i_aiaickdk: do i = i0, n1ij
if (i == k) cycle i_aiaickdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v5_eom_cc3_31_triplet_trans_aiaickdk(t2, nocc, nactive, a, i, c, &
 d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickdk
end do a_aiaickdk
end do k_aiaickdk
end do c_aiaickdk
end do d_aiaickdk
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aiaickdi: do d = n0d, n1d
c_aiaickdi: do c = n0c, n1c
if (c == d) cycle c_aiaickdi
k_aiaickdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickdi: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiaickdi
i0 = max(k + 1, n0ijl)
i_aiaickdi: do i = i0, n1ijl
if (i == k) cycle i_aiaickdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v5_eom_cc3_31_triplet_trans_aiaickdi(t2, nocc, nactive, a, i, c, &
 k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickdi
end do a_aiaickdi
end do k_aiaickdi
end do c_aiaickdi
end do d_aiaickdi
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl: do l = n0l, n1l
b_aibjaibl: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaibl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaibl: do a = n0ac, a1
if (a == b) cycle a_aibjaibl
i1 = min(j - 1, n1ik)
i_aibjaibl: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjaibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v8_eom_cc3_31_triplet_trans_aibjaibl(t2, nocc, nactive, a, i, j, &
 l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibl
end do a_aibjaibl
end do j_aibjaibl
end do b_aibjaibl
end do l_aibjaibl
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: c == a, d == b, l == k
! No equalities independent of the above can hold.
!
k_aibjakbk: do k = n0kl, n1kl
b_aibjakbk: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbk: do j = j0, n1j
if (j == k) cycle j_aibjakbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbk: do a = n0ac, a1
if (a == b) cycle a_aibjakbk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakbk(t2, nocc, nactive, a, i, b, &
 j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbk
end do a_aibjakbk
end do j_aibjakbk
end do b_aibjakbk
end do k_aibjakbk
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibiakbl: do l = n0l, n1l
k_aibiakbl: do k = n0k, n1k
if (k == l) cycle k_aibiakbl
b_aibiakbl: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibiakbl: do a = n0ac, a1
if (a == b) cycle a_aibiakbl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakbl: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibiakbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v7_eom_cc3_31_triplet_trans_aibiakbl(t2, nocc, nactive, a, i, k, &
 l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbl
end do a_aibiakbl
end do b_aibiakbl
end do k_aibiakbl
end do l_aibiakbl
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj: do k = n0k, n1k
b_aibjakbj: do b = n0bd, n1bd
j0 = max(k + 1, n0jl)
j_aibjakbj: do j = j0, n1jl
if (j == k) cycle j_aibjakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbj: do a = n0ac, a1
if (a == b) cycle a_aibjakbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakbj(t2, nocc, nactive, a, i, b, &
 j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbj
end do a_aibjakbj
end do j_aibjakbj
end do b_aibjakbj
end do k_aibjakbj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi: do k = n0k, n1k
b_aibjakbi: do b = n0bd, n1bd
j0 = max(k + 1, n0j)
j_aibjakbi: do j = j0, n1j
if (j == k) cycle j_aibjakbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbi: do a = n0ac, a1
if (a == b) cycle a_aibjakbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakbi(t2, nocc, nactive, a, i, b, &
 j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbi
end do a_aibjakbi
end do j_aibjakbi
end do b_aibjakbi
end do k_aibjakbi
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcibi: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibi: do b = b0, n1bd
if (b == c) cycle b_aibjcibi
j_aibjcibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibi: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibi
i1 = min(j - 1, n1ikl)
i_aibjcibi: do i = n0ikl, i1
if (i == j) cycle i_aibjcibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjcibi(t2, nocc, nactive, a, i, b, &
 j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibi
end do a_aibjcibi
end do j_aibjcibi
end do b_aibjcibi
end do c_aibjcibi
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibj: do b = b0, n1bd
if (b == c) cycle b_aibjcibj
j_aibjcibj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibj
i1 = min(j - 1, n1ik)
i_aibjcibj: do i = n0ik, i1
if (i == j) cycle i_aibjcibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v4_eom_cc3_31_triplet_trans_aibjcibj(t2, nocc, nactive, a, i, b, &
 j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibj
end do a_aibjcibj
end do j_aibjcibj
end do b_aibjcibj
end do c_aibjcibj
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == k
! No equalities independent of the above can hold.
!
c_aibickbk: do c = n0c, n1c
k_aibickbk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbk: do b = b0, n1bd
if (b == c) cycle b_aibickbk
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibickbk: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbk
i0 = max(k + 1, n0ij)
i_aibickbk: do i = i0, n1ij
if (i == k) cycle i_aibickbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickbk(t2, nocc, nactive, a, i, b, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbk
end do a_aibickbk
end do b_aibickbk
end do k_aibickbk
end do c_aibickbk
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickbi: do c = n0c, n1c
k_aibickbi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbi: do b = b0, n1bd
if (b == c) cycle b_aibickbi
a_aibickbi: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbi
i0 = max(k + 1, n0ijl)
i_aibickbi: do i = i0, n1ijl
if (i == k) cycle i_aibickbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v3_eom_cc3_31_triplet_trans_aibickbi(t2, nocc, nactive, a, i, b, &
 c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbi
end do a_aibickbi
end do b_aibickbi
end do k_aibickbi
end do c_aibickbi
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjaial: do l = n0l, n1l
b_aibjaial: do b = n0b, n1b
j_aibjaial: do j = n0j, n1j
if (j == l) cycle j_aibjaial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjaial: do a = n0acd, a1
if (a == b) cycle a_aibjaial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjaial: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjaial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + dl
jac_ibra_iket = v8_eom_cc3_31_triplet_trans_aibjaial(t2, nocc, nactive, a, i, b, &
 j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaial
end do a_aibjaial
end do j_aibjaial
end do b_aibjaial
end do l_aibjaial
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: c == a, d == a, l == k
! No equalities independent of the above can hold.
!
k_aibjakak: do k = n0kl, n1kl
b_aibjakak: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjakak: do j = j0, n1j
if (j == k) cycle j_aibjakak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acd)
a_aibjakak: do a = n0acd, a1
if (a == b) cycle a_aibjakak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + dl
jac_ibra_iket = v2_eom_cc3_31_triplet_trans_aibjakak(t2, nocc, nactive, a, i, b, &
 j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakak
end do a_aibjakak
end do j_aibjakak
end do b_aibjakak
end do k_aibjakak
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
end subroutine ccjac_31_triplet_dav_part2
end module ccjac_block_31_triplet_dav_part2
