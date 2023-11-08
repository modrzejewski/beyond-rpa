module ccjac_block_32_tripletm_dav_part5
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
subroutine ccjac_32_tripletm_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, c0, c1, i0, i1, j0
integer :: n0ace, n0ad, n0ade, n0ae, n0cd
integer :: n0ce, n0ij, n0ijl, n0ijlm, n0ijm
integer :: n0ik, n0ikl, n0iklm, n0ikm, n0il
integer :: n0ilm, n0im, n0jl, n0jlm, n0jm
integer :: n0kl, n0klm, n0km
integer :: n1ace, n1ad, n1ade, n1ae, n1cd
integer :: n1ce, n1ij, n1ijl, n1ijlm, n1ijm
integer :: n1ik, n1ikl, n1iklm, n1ikm, n1il
integer :: n1ilm, n1im, n1jl, n1jlm, n1jm
integer :: n1kl, n1klm, n1km
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
n0ace = max(n0a, n0c, n0e)
n0ad = max(n0a, n0d)
n0ade = max(n0a, n0d, n0e)
n0ae = max(n0a, n0e)
n0cd = max(n0c, n0d)
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
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1ade = min(n1a, n1d, n1e)
n1ae = min(n1a, n1e)
n1cd = min(n1c, n1d)
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
n1jl = min(n1j, n1l)
n1jlm = min(n1j, n1l, n1m)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1klm = min(n1k, n1l, n1m)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k
! Equalities: e == a, d == c, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckcjaj: do c = n0cd, n1cd
k_aibjckcjaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjaj: do b = b0, n1b
if (b == c) cycle b_aibjckcjaj
j0 = max(k + 1, n0jlm)
j_aibjckcjaj: do j = j0, n1jlm
if (j == k) cycle j_aibjckcjaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckcjaj: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckcjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckcjaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckcjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckcjaj(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcjaj
end do a_aibjckcjaj
end do j_aibjckcjaj
end do b_aibjckcjaj
end do k_aibjckcjaj
end do c_aibjckcjaj
!
! Elementary loop  2
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjciciei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjciciei: do c = c0, n1cd
if (c == e) cycle c_aibjciciei
b0 = max(c + 1, n0b)
b_aibjciciei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjciciei
j_aibjciciei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciciei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjciciei
i1 = min(j - 1, n1iklm)
i_aibjciciei: do i = n0iklm, i1
if (i == j) cycle i_aibjciciei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciciei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciciei
end do a_aibjciciei
end do j_aibjciciei
end do b_aibjciciei
end do c_aibjciciei
end do e_aibjciciei
!
! Elementary loop  3
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcicjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjcicjei: do c = c0, n1cd
if (c == e) cycle c_aibjcicjei
b0 = max(c + 1, n0b)
b_aibjcicjei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjcicjei
j_aibjcicjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicjei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcicjei
i1 = min(j - 1, n1ikm)
i_aibjcicjei: do i = n0ikm, i1
if (i == j) cycle i_aibjcicjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcicjei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcicjei
end do a_aibjcicjei
end do j_aibjcicjei
end do b_aibjcicjei
end do c_aibjcicjei
end do e_aibjcicjei
!
! Elementary loop  4
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, k
! Equalities: d == c, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibickckei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibickckei: do c = c0, n1cd
if (c == e) cycle c_aibickckei
k_aibickckei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickckei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibickckei
a_aibickckei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibickckei
i0 = max(k + 1, n0ijm)
i_aibickckei: do i = i0, n1ijm
if (i == k) cycle i_aibickckei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickckei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickckei
end do a_aibickckei
end do b_aibickckei
end do k_aibickckei
end do c_aibickckei
end do e_aibickckei
!
! Elementary loop  5
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, k
! Equalities: d == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibickciei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibickciei: do c = c0, n1cd
if (c == e) cycle c_aibickciei
k_aibickciei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickciei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibickciei
a_aibickciei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibickciei
i0 = max(k + 1, n0ijlm)
i_aibickciei: do i = i0, n1ijlm
if (i == k) cycle i_aibickciei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickciei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickciei
end do a_aibickciei
end do b_aibickciei
end do k_aibickciei
end do c_aibickciei
end do e_aibickciei
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, j
! Equalities: c == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjakdiak: do d = n0d, n1d
k_aibjakdiak: do k = n0km, n1km
b_aibjakdiak: do b = n0b, n1b
if (b == d) cycle b_aibjakdiak
j0 = max(k + 1, n0j)
j_aibjakdiak: do j = j0, n1j
if (j == k) cycle j_aibjakdiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, d - 1, n1ace)
a_aibjakdiak: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibjakdiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdiak: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakdiak(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdiak
end do a_aibjakdiak
end do j_aibjakdiak
end do b_aibjakdiak
end do k_aibjakdiak
end do d_aibjakdiak
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, j
! Equalities: c == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjakdkai: do d = n0d, n1d
k_aibjakdkai: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjakdkai: do b = n0b, n1b
if (b == d) cycle b_aibjakdkai
j0 = max(k + 1, n0j)
j_aibjakdkai: do j = j0, n1j
if (j == k) cycle j_aibjakdkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, d - 1, n1ace)
a_aibjakdkai: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibjakdkai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdkai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakdkai(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdkai
end do a_aibjakdkai
end do j_aibjakdkai
end do b_aibjakdkai
end do k_aibjakdkai
end do d_aibjakdkai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjakdjai: do d = n0d, n1d
k_aibjakdjai: do k = n0k, n1k
b_aibjakdjai: do b = n0b, n1b
if (b == d) cycle b_aibjakdjai
j0 = max(k + 1, n0jl)
j_aibjakdjai: do j = j0, n1jl
if (j == k) cycle j_aibjakdjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, d - 1, n1ace)
a_aibjakdjai: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibjakdjai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakdjai(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdjai
end do a_aibjakdjai
end do j_aibjakdjai
end do b_aibjakdjai
end do k_aibjakdjai
end do d_aibjakdjai
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjakdiaj: do d = n0d, n1d
k_aibjakdiaj: do k = n0k, n1k
b_aibjakdiaj: do b = n0b, n1b
if (b == d) cycle b_aibjakdiaj
j0 = max(k + 1, n0jm)
j_aibjakdiaj: do j = j0, n1jm
if (j == k) cycle j_aibjakdiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, d - 1, n1ace)
a_aibjakdiaj: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibjakdiaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakdiaj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakdiaj(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdiaj
end do a_aibjakdiaj
end do j_aibjakdiaj
end do b_aibjakdiaj
end do k_aibjakdiaj
end do d_aibjakdiaj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: d == a, e == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjcialci: do l = n0l, n1l
c_aibjcialci: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjcialci: do b = b0, n1b
if (b == c) cycle b_aibjcialci
j_aibjcialci: do j = n0j, n1j
if (j == l) cycle j_aibjcialci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjcialci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialci
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjcialci: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjcialci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcialci(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcialci
end do a_aibjcialci
end do j_aibjcialci
end do b_aibjcialci
end do c_aibjcialci
end do l_aibjcialci
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: d == a, e == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjciaicm: do m = n0m, n1m
c_aibjciaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjciaicm: do b = b0, n1b
if (b == c) cycle b_aibjciaicm
j_aibjciaicm: do j = n0j, n1j
if (j == m) cycle j_aibjciaicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjciaicm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaicm
i1 = min(j - 1, n1ikl)
i_aibjciaicm: do i = n0ikl, i1
if (i == j .or. i == m) cycle i_aibjciaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciaicm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciaicm
end do a_aibjciaicm
end do j_aibjciaicm
end do b_aibjciaicm
end do c_aibjciaicm
end do m_aibjciaicm
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: d == a, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialcj: do l = n0l, n1l
c_aibjcialcj: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjcialcj: do b = b0, n1b
if (b == c) cycle b_aibjcialcj
j_aibjcialcj: do j = n0jm, n1jm
if (j == l) cycle j_aibjcialcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjcialcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialcj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjcialcj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjcialcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcialcj(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcialcj
end do a_aibjcialcj
end do j_aibjcialcj
end do b_aibjcialcj
end do c_aibjcialcj
end do l_aibjcialcj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j
! Equalities: d == a, e == c, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckakck: do c = n0ce, n1ce
k_aibjckakck: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakck: do b = b0, n1b
if (b == c) cycle b_aibjckakck
j0 = max(k + 1, n0j)
j_aibjckakck: do j = j0, n1j
if (j == k) cycle j_aibjckakck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckakck: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakck
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckakck(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckakck
end do a_aibjckakck
end do j_aibjckakck
end do b_aibjckakck
end do k_aibjckakck
end do c_aibjckakck
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, l
! Equalities: d == a, e == c, j == i, m == k
! No equalities independent of the above can hold.
!
l_aibickalck: do l = n0l, n1l
c_aibickalck: do c = n0ce, n1ce
k_aibickalck: do k = n0km, n1km
if (k == l) cycle k_aibickalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickalck: do b = b0, n1b
if (b == c) cycle b_aibickalck
a0 = max(c + 1, n0ad)
a_aibickalck: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickalck
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickalck: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibickalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickalck(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickalck
end do a_aibickalck
end do b_aibickalck
end do k_aibickalck
end do c_aibickalck
end do l_aibickalck
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, j
! Equalities: d == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaick: do c = n0ce, n1ce
k_aibjckaick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaick: do b = b0, n1b
if (b == c) cycle b_aibjckaick
j0 = max(k + 1, n0j)
j_aibjckaick: do j = j0, n1j
if (j == k) cycle j_aibjckaick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckaick: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaick
i_aibjckaick: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaick(a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaick
end do a_aibjckaick
end do j_aibjckaick
end do b_aibjckaick
end do k_aibjckaick
end do c_aibjckaick
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: d == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajck: do c = n0ce, n1ce
k_aibjckajck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajck: do b = b0, n1b
if (b == c) cycle b_aibjckajck
j0 = max(k + 1, n0jl)
j_aibjckajck: do j = j0, n1jl
if (j == k) cycle j_aibjckajck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckajck: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckajck(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckajck
end do a_aibjckajck
end do j_aibjckajck
end do b_aibjckajck
end do k_aibjckajck
end do c_aibjckajck
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: d == a, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckakcj: do c = n0ce, n1ce
k_aibjckakcj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakcj: do b = b0, n1b
if (b == c) cycle b_aibjckakcj
j0 = max(k + 1, n0jm)
j_aibjckakcj: do j = j0, n1jm
if (j == k) cycle j_aibjckakcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckakcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakcj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckakcj(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckakcj
end do a_aibjckakcj
end do j_aibjckakcj
end do b_aibjckakcj
end do k_aibjckakcj
end do c_aibjckakcj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, l
! Equalities: d == a, e == c, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibickalci: do l = n0l, n1l
c_aibickalci: do c = n0ce, n1ce
k_aibickalci: do k = n0k, n1k
if (k == l) cycle k_aibickalci
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickalci: do b = b0, n1b
if (b == c) cycle b_aibickalci
a0 = max(c + 1, n0ad)
a_aibickalci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickalci
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibickalci: do i = i0, n1ijm
if (i == k .or. i == l) cycle i_aibickalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickalci(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickalci
end do a_aibickalci
end do b_aibickalci
end do k_aibickalci
end do c_aibickalci
end do l_aibickalci
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: d == a, e == c, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckaici: do c = n0ce, n1ce
k_aibjckaici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaici: do b = b0, n1b
if (b == c) cycle b_aibjckaici
j0 = max(k + 1, n0j)
j_aibjckaici: do j = j0, n1j
if (j == k) cycle j_aibjckaici
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckaici: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaici
i_aibjckaici: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjckaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaici(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaici
end do a_aibjckaici
end do j_aibjckaici
end do b_aibjckaici
end do k_aibjckaici
end do c_aibjckaici
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, m
! Equalities: d == a, e == c, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibickaicm: do m = n0m, n1m
c_aibickaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibickaicm: do k = n0k, n1k
if (k == m) cycle k_aibickaicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickaicm: do b = b0, n1b
if (b == c) cycle b_aibickaicm
a0 = max(c + 1, n0ad)
a_aibickaicm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickaicm
i0 = max(k + 1, n0ijl)
i_aibickaicm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibickaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickaicm(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaicm
end do a_aibickaicm
end do b_aibickaicm
end do k_aibickaicm
end do c_aibickaicm
end do m_aibickaicm
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: d == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaicj: do c = n0ce, n1ce
k_aibjckaicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaicj: do b = b0, n1b
if (b == c) cycle b_aibjckaicj
j0 = max(k + 1, n0jm)
j_aibjckaicj: do j = j0, n1jm
if (j == k) cycle j_aibjckaicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckaicj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaicj
i_aibjckaicj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaicj(a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaicj
end do a_aibjckaicj
end do j_aibjckaicj
end do b_aibjckaicj
end do k_aibjckaicj
end do c_aibjckaicj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k
! Equalities: d == a, e == c, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckajcj: do c = n0ce, n1ce
k_aibjckajcj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajcj: do b = b0, n1b
if (b == c) cycle b_aibjckajcj
j0 = max(k + 1, n0jlm)
j_aibjckajcj: do j = j0, n1jlm
if (j == k) cycle j_aibjckajcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckajcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajcj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckajcj(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckajcj
end do a_aibjckajcj
end do j_aibjckajcj
end do b_aibjckajcj
end do k_aibjckajcj
end do c_aibjckajcj
!
! Elementary loop  23
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjcidici: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjcidici: do c = n0ce, c1
if (c == d) cycle c_aibjcidici
b0 = max(c + 1, n0b)
b_aibjcidici: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidici
j_aibjcidici: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidici: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidici
i1 = min(j - 1, n1iklm)
i_aibjcidici: do i = n0iklm, i1
if (i == j) cycle i_aibjcidici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcidici(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidici
end do a_aibjcidici
end do j_aibjcidici
end do b_aibjcidici
end do c_aibjcidici
end do d_aibjcidici
!
! Elementary loop  24
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j
! Equalities: e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjcidicj: do c = n0ce, c1
if (c == d) cycle c_aibjcidicj
b0 = max(c + 1, n0b)
b_aibjcidicj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidicj
j_aibjcidicj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidicj: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidicj
i1 = min(j - 1, n1ikl)
i_aibjcidicj: do i = n0ikl, i1
if (i == j) cycle i_aibjcidicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcidicj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidicj
end do a_aibjcidicj
end do j_aibjcidicj
end do b_aibjcidicj
end do c_aibjcidicj
end do d_aibjcidicj
!
! Elementary loop  25
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
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickdick(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdick
end do a_aibickdick
end do b_aibickdick
end do k_aibickdick
end do c_aibickdick
end do d_aibickdick
!
! Elementary loop  26
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, k
! Equalities: e == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibickdici: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibickdici: do c = n0ce, c1
if (c == d) cycle c_aibickdici
k_aibickdici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickdici: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibickdici
a_aibickdici: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibickdici
i0 = max(k + 1, n0ijlm)
i_aibickdici: do i = i0, n1ijlm
if (i == k) cycle i_aibickdici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickdici(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdici
end do a_aibickdici
end do b_aibickdici
end do k_aibickdici
end do c_aibickdici
end do d_aibickdici
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaiak: do c = n0c, n1c
k_aibjckaiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaiak: do b = b0, n1b
if (b == c) cycle b_aibjckaiak
j0 = max(k + 1, n0j)
j_aibjckaiak: do j = j0, n1j
if (j == k) cycle j_aibjckaiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaiak: do a = n0ade, n1ade
if (a == b .or. a == c) cycle a_aibjckaiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i_aibjckaiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaiak(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaiak
end do a_aibjckaiak
end do j_aibjckaiak
end do b_aibjckaiak
end do k_aibjckaiak
end do c_aibjckaiak
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckakai: do c = n0c, n1c
k_aibjckakai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckakai: do b = b0, n1b
if (b == c) cycle b_aibjckakai
j0 = max(k + 1, n0j)
j_aibjckakai: do j = j0, n1j
if (j == k) cycle j_aibjckakai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckakai: do a = n0ade, n1ade
if (a == b .or. a == c) cycle a_aibjckakai
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aibjckakai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckakai(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckakai
end do a_aibjckakai
end do j_aibjckakai
end do b_aibjckakai
end do k_aibjckakai
end do c_aibjckakai
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckajai: do c = n0c, n1c
k_aibjckajai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajai: do b = b0, n1b
if (b == c) cycle b_aibjckajai
j0 = max(k + 1, n0jl)
j_aibjckajai: do j = j0, n1jl
if (j == k) cycle j_aibjckajai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckajai: do a = n0ade, n1ade
if (a == b .or. a == c) cycle a_aibjckajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjckajai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckajai(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckajai
end do a_aibjckajai
end do j_aibjckajai
end do b_aibjckajai
end do k_aibjckajai
end do c_aibjckajai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaiaj: do c = n0c, n1c
k_aibjckaiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaiaj: do b = b0, n1b
if (b == c) cycle b_aibjckaiaj
j0 = max(k + 1, n0jm)
j_aibjckaiaj: do j = j0, n1jm
if (j == k) cycle j_aibjckaiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckaiaj: do a = n0ade, n1ade
if (a == b .or. a == c) cycle a_aibjckaiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjckaiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaiaj(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaiaj
end do a_aibjckaiaj
end do j_aibjckaiaj
end do b_aibjckaiaj
end do k_aibjckaiaj
end do c_aibjckaiaj
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
end subroutine ccjac_32_tripletm_dav_part5
end module ccjac_block_32_tripletm_dav_part5
