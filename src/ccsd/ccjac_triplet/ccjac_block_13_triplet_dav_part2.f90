module ccjac_block_13_triplet_dav_part2
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
subroutine ccjac_13_triplet_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, i0, i1
integer :: n0abc, n0abd, n0ac, n0ad, n0bc
integer :: n0bd, n0ijk, n0ijl, n0ik, n0il
integer :: n0jk, n0jl
integer :: n1abc, n1abd, n1ac, n1ad, n1bc
integer :: n1bd, n1ijk, n1ijl, n1ik, n1il
integer :: n1jk, n1jl
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
n0abc = max(n0a, n0b, n0c)
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1abc = min(n1a, n1b, n1c)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, l
! Equalities: b == a, c == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aiaiaidl: do d = n0d, n1d
l_aiaiaidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiaiaidl: do a = a0, n1abc
if (a == d) cycle a_aiaiaidl
i0 = max(l + 1, n0ijk)
i_aiaiaidl: do i = i0, n1ijk
if (i == l) cycle i_aiaiaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aiaiaidl(a, i, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiaiaidl
end do a_aiaiaidl
end do l_aiaiaidl
end do d_aiaiaidl
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aiajajdi: do d = n0d, n1d
j_aiajajdi: do j = n0jk, n1jk
a0 = max(d + 1, n0abc)
a_aiajajdi: do a = a0, n1abc
if (a == d) cycle a_aiajajdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aiajajdi: do i = n0il, i1
if (i == j) cycle i_aiajajdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajajdi(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajajdi
end do a_aiajajdi
end do j_aiajajdi
end do d_aiajajdi
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibibkai: do k = n0k, n1k
b_aibibkai: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibibkai: do a = n0ad, a1
if (a == b) cycle a_aibibkai
i1 = min(k - 1, n1ijl)
i_aibibkai: do i = n0ijl, i1
if (i == k) cycle i_aibibkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibibkai(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibibkai
end do a_aibibkai
end do b_aibibkai
end do k_aibibkai
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiaj: do b = n0bc, n1bc
j_aibjbiaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibjbiaj: do a = n0ad, a1
if (a == b) cycle a_aibjbiaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjbiaj: do i = i0, n1ik
if (i == j) cycle i_aibjbiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjbiaj(b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjbiaj
end do a_aibjbiaj
end do j_aibjbiaj
end do b_aibjbiaj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: d == a, c == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibibial: do l = n0l, n1l
b_aibibial: do b = n0bc, n1bc
a1 = min(b - 1, n1ad)
a_aibibial: do a = n0ad, a1
if (a == b) cycle a_aibibial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijk)
i_aibibial: do i = i0, n1ijk
if (i == l) cycle i_aibibial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibibial(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibibial
end do a_aibibial
end do b_aibibial
end do l_aibibial
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjbjai: do b = n0bc, n1bc
j_aibjbjai: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ad)
a_aibjbjai: do a = n0ad, a1
if (a == b) cycle a_aibjbjai
i1 = min(j - 1, n1il)
i_aibjbjai: do i = n0il, i1
if (i == j) cycle i_aibjbjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjbjai(b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjbjai
end do a_aibjbjai
end do j_aibjbjai
end do b_aibjbjai
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aiaickai: do c = n0c, n1c
k_aiaickai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(c - 1, n1abd)
a_aiaickai: do a = n0abd, a1
if (a == c) cycle a_aiaickai
i1 = min(k - 1, n1ijl)
i_aiaickai: do i = n0ijl, i1
if (i == k) cycle i_aiaickai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aiaickai(a, i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiaickai
end do a_aiaickai
end do k_aiaickai
end do c_aiaickai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciaj: do c = n0c, n1c
j_aiajciaj: do j = n0jl, n1jl
a1 = min(c - 1, n1abd)
a_aiajciaj: do a = n0abd, a1
if (a == c) cycle a_aiajciaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciaj: do i = i0, n1ik
if (i == j) cycle i_aiajciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajciaj(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajciaj
end do a_aiajciaj
end do j_aiajciaj
end do c_aiajciaj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l
! Equalities: b == a, d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aiaicial: do l = n0l, n1l
c_aiaicial: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiaicial: do a = n0abd, a1
if (a == c) cycle a_aiaicial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ijk)
i_aiaicial: do i = i0, n1ijk
if (i == l) cycle i_aiaicial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aiaicial(a, i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiaicial
end do a_aiaicial
end do c_aiaicial
end do l_aiaicial
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjai: do c = n0c, n1c
j_aiajcjai: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abd)
a_aiajcjai: do a = n0abd, a1
if (a == c) cycle a_aiajcjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aiajcjai: do i = n0il, i1
if (i == j) cycle i_aiajcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aiajcjai(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aiajcjai
end do a_aiajcjai
end do j_aiajcjai
end do c_aiajcjai
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakbi: do k = n0k, n1k
b_aibiakbi: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiakbi: do a = a0, n1ac
if (a == b) cycle a_aibiakbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijl)
i_aibiakbi: do i = n0ijl, i1
if (i == k) cycle i_aibiakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibiakbi(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibiakbi
end do a_aibiakbi
end do b_aibiakbi
end do k_aibiakbi
!
! Elementary loop  12
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
a0 = max(b + 1, n0ac)
a_aibjaibj: do a = a0, n1ac
if (a == b) cycle a_aibjaibj
i0 = max(j + 1, n0ik)
i_aibjaibj: do i = i0, n1ik
if (i == j) cycle i_aibjaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjaibj(b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjaibj
end do a_aibjaibj
end do j_aibjaibj
end do b_aibjaibj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaibl: do l = n0l, n1l
b_aibiaibl: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaibl: do a = a0, n1ac
if (a == b) cycle a_aibiaibl
i0 = max(l + 1, n0ijk)
i_aibiaibl: do i = i0, n1ijk
if (i == l) cycle i_aibiaibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
jac_ibra_iket = eom_cc3_13_triplet_trans_aibiaibl(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibiaibl
end do a_aibiaibl
end do b_aibiaibl
end do l_aibiaibl
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi: do b = n0bd, n1bd
j_aibjajbi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbi: do a = a0, n1ac
if (a == b) cycle a_aibjajbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1il)
i_aibjajbi: do i = n0il, i1
if (i == j) cycle i_aibjajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + ai
iket = ketoffset + &
(bj - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_13_triplet_trans_aibjajbi(b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

end do i_aibjajbi
end do a_aibjajbi
end do j_aibjajbi
end do b_aibjajbi
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
end subroutine ccjac_13_triplet_dav_part2
end module ccjac_block_13_triplet_dav_part2
