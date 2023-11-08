module ccjac_block_13_triplet_dav_part1
use cc_gparams
use eom_cc3_13_triplet_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2017-01-20 18:02:30 UTC.
!
contains
subroutine ccjac_13_triplet_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, c0, i0, i1, k0
integer :: n0ab, n0abc, n0abd, n0ac, n0ad
integer :: n0bc, n0bd, n0ij, n0ijk, n0ijl
integer :: n0ik, n0il, n0jk, n0jl
integer :: n1ab, n1abc, n1abd, n1ac, n1ad
integer :: n1bc, n1bd, n1ij, n1ijk, n1ijl
integer :: n1ik, n1il, n1jk, n1jl
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
n0abc = max(n0a, n0b, n0c)
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1ab = min(n1a, n1b)
n1abc = min(n1a, n1b, n1c)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1ij = min(n1i, n1j)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, l == i
! No equalities independent of the above can hold.
!
d_aiajckdi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajckdi: do c = c0, n1c
if (c == d) cycle c_aiajckdi
k_aiajckdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckdi: do j = n0j, n1j
if (j == k) cycle j_aiajckdi
a_aiajckdi: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajckdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(k - 1, n1il)
i_aiajckdi: do i = n0il, i1
if (i == j .or. i == k) cycle i_aiajckdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajckdi(j, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajckdi
end do a_aiajckdi
end do j_aiajckdi
end do k_aiajckdi
end do c_aiajckdi
end do d_aiajckdi
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, k == i
! No equalities independent of the above can hold.
!
d_aiajcidl: do d = n0d, n1d
l_aiajcidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aiajcidl: do c = c0, n1c
if (c == d) cycle c_aiajcidl
j_aiajcidl: do j = n0j, n1j
if (j == l) cycle j_aiajcidl
a_aiajcidl: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcidl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(l + 1, n0ik)
i_aiajcidl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajcidl(j, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajcidl
end do a_aiajcidl
end do j_aiajcidl
end do c_aiajcidl
end do l_aiajcidl
end do d_aiajcidl
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, l
! Equalities: c == a, j == i
! No equalities independent of the above can hold.
!
d_aibiakdl: do d = n0d, n1d
l_aibiakdl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k0 = max(l + 1, n0k)
k_aibiakdl: do k = k0, n1k
if (k == l) cycle k_aibiakdl
b_aibiakdl: do b = n0b, n1b
if (b == d) cycle b_aibiakdl
a0 = max(d + 1, n0ac)
a_aibiakdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibiakdl(b, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibiakdl
end do a_aibiakdl
end do b_aibiakdl
end do k_aibiakdl
end do l_aibiakdl
end do d_aibiakdl
!
! Elementary loop  4
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
j_aibjakdi: do j = n0j, n1j
if (j == k) cycle j_aibjakdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0ac)
a_aibjakdi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1il)
i_aibjakdi: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjakdi(b, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjakdi
end do a_aibjakdi
end do j_aibjakdi
end do b_aibjakdi
end do k_aibjakdi
end do d_aibjakdi
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
d_aibjaidl: do d = n0d, n1d
l_aibjaidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidl: do b = n0b, n1b
if (b == d) cycle b_aibjaidl
j_aibjaidl: do j = n0j, n1j
if (j == l) cycle j_aibjaidl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0ac)
a_aibjaidl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidl
i0 = max(l + 1, n0ik)
i_aibjaidl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjaidl(b, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjaidl
end do a_aibjaidl
end do j_aibjaidl
end do b_aibjaidl
end do l_aibjaidl
end do d_aibjaidl
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, j == i
! No equalities independent of the above can hold.
!
l_aibickal: do l = n0l, n1l
c_aibickal: do c = n0c, n1c
k0 = max(l + 1, n0k)
k_aibickal: do k = k0, n1k
if (k == l) cycle k_aibickal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickal: do b = n0b, n1b
if (b == c) cycle b_aibickal
a1 = min(c - 1, n1ad)
a_aibickal: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibickal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibickal: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibickal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibickal(b, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibickal
end do a_aibickal
end do b_aibickal
end do k_aibickal
end do c_aibickal
end do l_aibickal
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, l == i
! No equalities independent of the above can hold.
!
c_aibjckai: do c = n0c, n1c
k_aibjckai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckai: do b = n0b, n1b
if (b == c) cycle b_aibjckai
j_aibjckai: do j = n0j, n1j
if (j == k) cycle j_aibjckai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ad)
a_aibjckai: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibjckai
i1 = min(k - 1, n1il)
i_aibjckai: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjckai(b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjckai
end do a_aibjckai
end do j_aibjckai
end do b_aibjckai
end do k_aibjckai
end do c_aibjckai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjcial: do l = n0l, n1l
c_aibjcial: do c = n0c, n1c
b_aibjcial: do b = n0b, n1b
if (b == c) cycle b_aibjcial
j_aibjcial: do j = n0j, n1j
if (j == l) cycle j_aibjcial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ad)
a_aibjcial: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibjcial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ik)
i_aibjcial: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjcial(b, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjcial
end do a_aibjcial
end do j_aibjcial
end do b_aibjcial
end do c_aibjcial
end do l_aibjcial
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k, l
! Equalities: b == a, c == a, j == i
! No equalities independent of the above can hold.
!
d_aiaiakdl: do d = n0d, n1d
l_aiaiakdl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k0 = max(l + 1, n0k)
k_aiaiakdl: do k = k0, n1k
if (k == l) cycle k_aiaiakdl
a0 = max(d + 1, n0abc)
a_aiaiakdl: do a = a0, n1abc
if (a == d) cycle a_aiaiakdl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiaiakdl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aiaiakdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aiaiakdl(a, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiaiakdl
end do a_aiaiakdl
end do k_aiaiakdl
end do l_aiaiakdl
end do d_aiaiakdl
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, l == i
! No equalities independent of the above can hold.
!
d_aiajakdi: do d = n0d, n1d
k_aiajakdi: do k = n0k, n1k
j_aiajakdi: do j = n0j, n1j
if (j == k) cycle j_aiajakdi
a0 = max(d + 1, n0abc)
a_aiajakdi: do a = a0, n1abc
if (a == d) cycle a_aiajakdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1il)
i_aiajakdi: do i = n0il, i1
if (i == j .or. i == k) cycle i_aiajakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajakdi(a, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajakdi
end do a_aiajakdi
end do j_aiajakdi
end do k_aiajakdi
end do d_aiajakdi
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, k == i
! No equalities independent of the above can hold.
!
d_aiajaidl: do d = n0d, n1d
l_aiajaidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaidl: do j = n0j, n1j
if (j == l) cycle j_aiajaidl
a0 = max(d + 1, n0abc)
a_aiajaidl: do a = a0, n1abc
if (a == d) cycle a_aiajaidl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(l + 1, n0ik)
i_aiajaidl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajaidl(a, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajaidl
end do a_aiajaidl
end do j_aiajaidl
end do l_aiajaidl
end do d_aiajaidl
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkai: do k = n0k, n1k
b_aibjbkai: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkai: do j = n0j, n1j
if (j == k) cycle j_aibjbkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibjbkai: do a = n0ad, a1
if (a == b) cycle a_aibjbkai
i1 = min(k - 1, n1il)
i_aibjbkai: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjbkai(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjbkai
end do a_aibjbkai
end do j_aibjbkai
end do b_aibjbkai
end do k_aibjbkai
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbial: do l = n0l, n1l
b_aibjbial: do b = n0bc, n1bc
j_aibjbial: do j = n0j, n1j
if (j == l) cycle j_aibjbial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibjbial: do a = n0ad, a1
if (a == b) cycle a_aibjbial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ik)
i_aibjbial: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjbial(b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjbial
end do a_aibjbial
end do j_aibjbial
end do b_aibjbial
end do l_aibjbial
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, j == i
! No equalities independent of the above can hold.
!
l_aiaickal: do l = n0l, n1l
c_aiaickal: do c = n0c, n1c
k0 = max(l + 1, n0k)
k_aiaickal: do k = k0, n1k
if (k == l) cycle k_aiaickal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(c - 1, n1abd)
a_aiaickal: do a = n0abd, a1
if (a == c) cycle a_aiaickal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiaickal: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aiaickal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aiaickal(a, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiaickal
end do a_aiaickal
end do k_aiaickal
end do c_aiaickal
end do l_aiaickal
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i
! No equalities independent of the above can hold.
!
c_aiajckai: do c = n0c, n1c
k_aiajckai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckai: do j = n0j, n1j
if (j == k) cycle j_aiajckai
a1 = min(c - 1, n1abd)
a_aiajckai: do a = n0abd, a1
if (a == c) cycle a_aiajckai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(k - 1, n1il)
i_aiajckai: do i = n0il, i1
if (i == j .or. i == k) cycle i_aiajckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajckai(a, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajckai
end do a_aiajckai
end do j_aiajckai
end do k_aiajckai
end do c_aiajckai
!
! Elementary loop  16
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
a1 = min(c - 1, n1abd)
a_aiajcial: do a = n0abd, a1
if (a == c) cycle a_aiajcial
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ik)
i_aiajcial: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajcial(a, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajcial
end do a_aiajcial
end do j_aiajcial
end do c_aiajcial
end do l_aiajcial
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aiaickdi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiaickdi: do c = c0, n1c
if (c == d) cycle c_aiaickdi
k_aiaickdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a_aiaickdi: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiaickdi
i1 = min(k - 1, n1ijl)
i_aiaickdi: do i = n0ijl, i1
if (i == k) cycle i_aiaickdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aiaickdi(i, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiaickdi
end do a_aiaickdi
end do k_aiaickdi
end do c_aiaickdi
end do d_aiaickdi
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, l
! Equalities: b == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aiaicidl: do d = n0d, n1d
l_aiaicidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c0 = max(d + 1, n0c)
c_aiaicidl: do c = c0, n1c
if (c == d) cycle c_aiaicidl
a_aiaicidl: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiaicidl
i0 = max(l + 1, n0ijk)
i_aiaicidl: do i = i0, n1ijk
if (i == l) cycle i_aiaicidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aiaicidl(i, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiaicidl
end do a_aiaicidl
end do c_aiaicidl
end do l_aiaicidl
end do d_aiaicidl
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi: do k = n0k, n1k
b_aibjakbi: do b = n0bd, n1bd
j_aibjakbi: do j = n0j, n1j
if (j == k) cycle j_aibjakbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbi: do a = a0, n1ac
if (a == b) cycle a_aibjakbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1il)
i_aibjakbi: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjakbi(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjakbi
end do a_aibjakbi
end do j_aibjakbi
end do b_aibjakbi
end do k_aibjakbi
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
a0 = max(b + 1, n0ac)
a_aibjaibl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl
i0 = max(l + 1, n0ik)
i_aibjaibl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjaibl(b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjaibl
end do a_aibjaibl
end do j_aibjaibl
end do b_aibjaibl
end do l_aibjaibl
!
! Elementary loop  21
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
a0 = max(d + 1, n0ac)
a_aibiakdi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijl)
i_aibiakdi: do i = n0ijl, i1
if (i == k) cycle i_aibiakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibiakdi(i, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibiakdi
end do a_aibiakdi
end do b_aibiakdi
end do k_aibiakdi
end do d_aibiakdi
!
! Elementary loop  22
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
a0 = max(d + 1, n0ac)
a_aibjaidj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj
i0 = max(j + 1, n0ik)
i_aibjaidj: do i = i0, n1ik
if (i == j) cycle i_aibjaidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjaidj(b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjaidj
end do a_aibjaidj
end do j_aibjaidj
end do b_aibjaidj
end do d_aibjaidj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibiaidl: do d = n0d, n1d
l_aibiaidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidl: do b = n0b, n1b
if (b == d) cycle b_aibiaidl
a0 = max(d + 1, n0ac)
a_aibiaidl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidl
i0 = max(l + 1, n0ijk)
i_aibiaidl: do i = i0, n1ijk
if (i == l) cycle i_aibiaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibiaidl(i, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibiaidl
end do a_aibiaidl
end do b_aibiaidl
end do l_aibiaidl
end do d_aibiaidl
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi: do d = n0d, n1d
b_aibjajdi: do b = n0b, n1b
if (b == d) cycle b_aibjajdi
j_aibjajdi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0ac)
a_aibjajdi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aibjajdi: do i = n0il, i1
if (i == j) cycle i_aibjajdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjajdi(b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjajdi
end do a_aibjajdi
end do j_aibjajdi
end do b_aibjajdi
end do d_aibjajdi
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickai: do c = n0c, n1c
k_aibickai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickai: do b = n0b, n1b
if (b == c) cycle b_aibickai
a1 = min(c - 1, n1ad)
a_aibickai: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibickai
i1 = min(k - 1, n1ijl)
i_aibickai: do i = n0ijl, i1
if (i == k) cycle i_aibickai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibickai(i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibickai
end do a_aibickai
end do b_aibickai
end do k_aibickai
end do c_aibickai
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj: do c = n0c, n1c
b_aibjciaj: do b = n0b, n1b
if (b == c) cycle b_aibjciaj
j_aibjciaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ad)
a_aibjciaj: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibjciaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjciaj: do i = i0, n1ik
if (i == j) cycle i_aibjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjciaj(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjciaj
end do a_aibjciaj
end do j_aibjciaj
end do b_aibjciaj
end do c_aibjciaj
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibicial: do l = n0l, n1l
c_aibicial: do c = n0c, n1c
b_aibicial: do b = n0b, n1b
if (b == c) cycle b_aibicial
a1 = min(c - 1, n1ad)
a_aibicial: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibicial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijk)
i_aibicial: do i = i0, n1ijk
if (i == l) cycle i_aibicial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibicial(i, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibicial
end do a_aibicial
end do b_aibicial
end do c_aibicial
end do l_aibicial
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai: do c = n0c, n1c
b_aibjcjai: do b = n0b, n1b
if (b == c) cycle b_aibjcjai
j_aibjcjai: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ad)
a_aibjcjai: do a = n0ad, a1
if (a == b .or. a == c) cycle a_aibjcjai
i1 = min(j - 1, n1il)
i_aibjcjai: do i = n0il, i1
if (i == j) cycle i_aibjcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjcjai(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjcjai
end do a_aibjcjai
end do j_aibjcjai
end do b_aibjcjai
end do c_aibjcjai
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k
! Equalities: b == a, c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aiaiakdi: do d = n0d, n1d
k_aiaiakdi: do k = n0k, n1k
a0 = max(d + 1, n0abc)
a_aiaiakdi: do a = a0, n1abc
if (a == d) cycle a_aiaiakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijl)
i_aiaiakdi: do i = n0ijl, i1
if (i == k) cycle i_aiaiakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aiaiakdi(a, i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiaiakdi
end do a_aiaiakdi
end do k_aiaiakdi
end do d_aiaiakdi
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj: do d = n0d, n1d
j_aiajaidj: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajaidj: do a = a0, n1abc
if (a == d) cycle a_aiajaidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidj: do i = i0, n1ik
if (i == j) cycle i_aiajaidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajaidj(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajaidj
end do a_aiajaidj
end do j_aiajaidj
end do d_aiajaidj
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
end subroutine ccjac_13_triplet_dav_part1
end module ccjac_block_13_triplet_dav_part1
