module ccjac_block_32_tripletm_dav_part6
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
subroutine ccjac_32_tripletm_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, i0, i1, j0, j1, k0, k1
integer :: n0abd, n0abde, n0ad, n0ae, n0ce
integer :: n0ij, n0ijl, n0ijlm, n0ijm, n0ik
integer :: n0ikl, n0iklm, n0ikm, n0il, n0im
integer :: n0jl, n0jm, n0kl, n0klm, n0km
integer :: n1abd, n1abde, n1ad, n1ae, n1ce
integer :: n1ij, n1ijl, n1ijlm, n1ijm, n1ik
integer :: n1ikl, n1iklm, n1ikm, n1il, n1im
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
n0abde = max(n0a, n0b, n0d, n0e)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
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
n0im = max(n0i, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1abd = min(n1a, n1b, n1d)
n1abde = min(n1a, n1b, n1d, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
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
n1im = min(n1i, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1klm = min(n1k, n1l, n1m)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjciaiei: do e = n0e, n1e
c_aibjciaiei: do c = n0c, n1c
if (c == e) cycle c_aibjciaiei
b0 = max(c + 1, n0b)
b_aibjciaiei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjciaiei
j_aibjciaiei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0ad)
a_aibjciaiei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjciaiei
i1 = min(j - 1, n1iklm)
i_aibjciaiei: do i = n0iklm, i1
if (i == j) cycle i_aibjciaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciaiei(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciaiei
end do a_aibjciaiei
end do j_aibjciaiei
end do b_aibjciaiei
end do c_aibjciaiei
end do e_aibjciaiei
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjciaiej: do e = n0e, n1e
c_aibjciaiej: do c = n0c, n1c
if (c == e) cycle c_aibjciaiej
b0 = max(c + 1, n0b)
b_aibjciaiej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjciaiej
j_aibjciaiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0ad)
a_aibjciaiej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjciaiej
i1 = min(j - 1, n1ikl)
i_aibjciaiej: do i = n0ikl, i1
if (i == j) cycle i_aibjciaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciaiej(i, b, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciaiej
end do a_aibjciaiej
end do j_aibjciaiej
end do b_aibjciaiej
end do c_aibjciaiej
end do e_aibjciaiej
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibickaiek: do e = n0e, n1e
c_aibickaiek: do c = n0c, n1c
if (c == e) cycle c_aibickaiek
k_aibickaiek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickaiek: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibickaiek
a0 = max(e + 1, n0ad)
a_aibickaiek: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibickaiek
i0 = max(k + 1, n0ijl)
i_aibickaiek: do i = i0, n1ijl
if (i == k) cycle i_aibickaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickaiek(i, b, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaiek
end do a_aibickaiek
end do b_aibickaiek
end do k_aibickaiek
end do c_aibickaiek
end do e_aibickaiek
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibickaiei: do e = n0e, n1e
c_aibickaiei: do c = n0c, n1c
if (c == e) cycle c_aibickaiei
k_aibickaiei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickaiei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibickaiei
a0 = max(e + 1, n0ad)
a_aibickaiei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibickaiei
i0 = max(k + 1, n0ijlm)
i_aibickaiei: do i = i0, n1ijlm
if (i == k) cycle i_aibickaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickaiei(b, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaiei
end do a_aibickaiei
end do b_aibickaiei
end do k_aibickaiei
end do c_aibickaiei
end do e_aibickaiei
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjcidiai: do d = n0d, n1d
c_aibjcidiai: do c = n0c, n1c
if (c == d) cycle c_aibjcidiai
b0 = max(c + 1, n0b)
b_aibjcidiai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidiai
j_aibjcidiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1ae)
a_aibjcidiai: do a = n0ae, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidiai
i1 = min(j - 1, n1iklm)
i_aibjcidiai: do i = n0iklm, i1
if (i == j) cycle i_aibjcidiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcidiai(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidiai
end do a_aibjcidiai
end do j_aibjcidiai
end do b_aibjcidiai
end do c_aibjcidiai
end do d_aibjcidiai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjai: do d = n0d, n1d
c_aibjcidjai: do c = n0c, n1c
if (c == d) cycle c_aibjcidjai
b0 = max(c + 1, n0b)
b_aibjcidjai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidjai
j_aibjcidjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1ae)
a_aibjcidjai: do a = n0ae, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjai
i1 = min(j - 1, n1ikm)
i_aibjcidjai: do i = n0ikm, i1
if (i == j) cycle i_aibjcidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcidjai(i, b, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidjai
end do a_aibjcidjai
end do j_aibjcidjai
end do b_aibjcidjai
end do c_aibjcidjai
end do d_aibjcidjai
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k
! Equalities: e == a, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibickdkai: do d = n0d, n1d
c_aibickdkai: do c = n0c, n1c
if (c == d) cycle c_aibickdkai
k_aibickdkai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickdkai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibickdkai
a1 = min(d - 1, n1ae)
a_aibickdkai: do a = n0ae, a1
if (a == b .or. a == c .or. a == d) cycle a_aibickdkai
i0 = max(k + 1, n0ijm)
i_aibickdkai: do i = i0, n1ijm
if (i == k) cycle i_aibickdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickdkai(i, b, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdkai
end do a_aibickdkai
end do b_aibickdkai
end do k_aibickdkai
end do c_aibickdkai
end do d_aibickdkai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k
! Equalities: e == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibickdiai: do d = n0d, n1d
c_aibickdiai: do c = n0c, n1c
if (c == d) cycle c_aibickdiai
k_aibickdiai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickdiai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibickdiai
a1 = min(d - 1, n1ae)
a_aibickdiai: do a = n0ae, a1
if (a == b .or. a == c .or. a == d) cycle a_aibickdiai
i0 = max(k + 1, n0ijlm)
i_aibickdiai: do i = i0, n1ijlm
if (i == k) cycle i_aibickdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickdiai(b, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdiai
end do a_aibickdiai
end do b_aibickdiai
end do k_aibickdiai
end do c_aibickdiai
end do d_aibickdiai
!
! Elementary loop  9
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
a0 = max(c + 1, n0abde)
a_aiajcialai: do a = a0, n1abde
if (a == c) cycle a_aiajcialai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, l - 1, n1ikm)
i_aiajcialai: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aiajcialai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcialai(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcialai
end do a_aiajcialai
end do j_aiajcialai
end do c_aiajcialai
end do l_aiajcialai
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == a, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiajciaiam: do m = n0m, n1m
c_aiajciaiam: do c = n0c, n1c
j_aiajciaiam: do j = n0j, n1j
if (j == m) cycle j_aiajciaiam
a0 = max(c + 1, n0abde)
a_aiajciaiam: do a = a0, n1abde
if (a == c) cycle a_aiajciaiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ikl)
i1 = min(j - 1, n1ikl)
i_aiajciaiam: do i = i0, i1
if (i == j .or. i == m) cycle i_aiajciaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciaiam(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciaiam
end do a_aiajciaiam
end do j_aiajciaiam
end do c_aiajciaiam
end do m_aiajciaiam
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
a0 = max(c + 1, n0abde)
a_aiajcialaj: do a = a0, n1abde
if (a == c) cycle a_aiajcialaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajcialaj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aiajcialaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcialaj(i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcialaj
end do a_aiajcialaj
end do j_aiajcialaj
end do c_aiajcialaj
end do l_aiajcialaj
!
! Elementary loop  12
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
a0 = max(c + 1, n0abde)
a_aiajciajam: do a = a0, n1abde
if (a == c) cycle a_aiajciajam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajciajam: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aiajciajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciajam(i, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciajam
end do a_aiajciajam
end do j_aiajciajam
end do c_aiajciajam
end do m_aiajciajam
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, e == a, j == i, m == k
! No equalities independent of the above can hold.
!
l_aiaickalak: do l = n0l, n1l
c_aiaickalak: do c = n0c, n1c
k1 = min(l - 1, n1km)
k_aiaickalak: do k = n0km, k1
if (k == l) cycle k_aiaickalak
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiaickalak: do a = a0, n1abde
if (a == c) cycle a_aiaickalak
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaickalak: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aiaickalak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickalak(i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickalak
end do a_aiaickalak
end do k_aiaickalak
end do c_aiaickalak
end do l_aiaickalak
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckaiak: do c = n0c, n1c
k_aiajckaiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckaiak: do j = j0, n1j
if (j == k) cycle j_aiajckaiak
a0 = max(c + 1, n0abde)
a_aiajckaiak: do a = a0, n1abde
if (a == c) cycle a_aiajckaiak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i_aiajckaiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckaiak(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaiak
end do a_aiajckaiak
end do j_aiajckaiak
end do k_aiajckaiak
end do c_aiajckaiak
!
! Elementary loop  15
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
a0 = max(c + 1, n0abde)
a_aiajckajak: do a = a0, n1abde
if (a == c) cycle a_aiajckajak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckajak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckajak(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajak
end do a_aiajckajak
end do j_aiajckajak
end do k_aiajckajak
end do c_aiajckajak
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckakai: do c = n0c, n1c
k_aiajckakai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckakai: do j = j0, n1j
if (j == k) cycle j_aiajckakai
a0 = max(c + 1, n0abde)
a_aiajckakai: do a = a0, n1abde
if (a == c) cycle a_aiajckakai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aiajckakai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aiajckakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckakai(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckakai
end do a_aiajckakai
end do j_aiajckakai
end do k_aiajckakai
end do c_aiajckakai
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, m
! Equalities: b == a, d == a, e == a, j == i, l == k
! No equalities independent of the above can hold.
!
m_aiaickakam: do m = n0m, n1m
c_aiaickakam: do c = n0c, n1c
k0 = max(m + 1, n0kl)
k_aiaickakam: do k = k0, n1kl
if (k == m) cycle k_aiaickakam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiaickakam: do a = a0, n1abde
if (a == c) cycle a_aiaickakam
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaickakam: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aiaickakam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickakam(i, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickakam
end do a_aiaickakam
end do k_aiaickakam
end do c_aiaickakam
end do m_aiaickakam
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, e == a, j == i, m == i
! No equalities independent of the above can hold.
!
l_aiaickalai: do l = n0l, n1l
c_aiaickalai: do c = n0c, n1c
k_aiaickalai: do k = n0k, n1k
if (k == l) cycle k_aiaickalai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiaickalai: do a = a0, n1abde
if (a == c) cycle a_aiaickalai
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ijm)
i1 = min(l - 1, n1ijm)
i_aiaickalai: do i = i0, i1
if (i == k .or. i == l) cycle i_aiaickalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickalai(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickalai
end do a_aiaickalai
end do k_aiaickalai
end do c_aiaickalai
end do l_aiaickalai
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajckajai: do c = n0c, n1c
k_aiajckajai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckajai: do j = j0, n1jl
if (j == k) cycle j_aiajckajai
a0 = max(c + 1, n0abde)
a_aiajckajai: do a = a0, n1abde
if (a == c) cycle a_aiajckajai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aiajckajai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aiajckajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckajai(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajai
end do a_aiajckajai
end do j_aiajckajai
end do k_aiajckajai
end do c_aiajckajai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, m
! Equalities: b == a, d == a, e == a, j == i, l == i
! No equalities independent of the above can hold.
!
m_aiaickaiam: do m = n0m, n1m
c_aiaickaiam: do c = n0c, n1c
k_aiaickaiam: do k = n0k, n1k
if (k == m) cycle k_aiaickaiam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiaickaiam: do a = a0, n1abde
if (a == c) cycle a_aiaickaiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, m + 1, n0ijl)
i_aiaickaiam: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aiaickaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickaiam(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaiam
end do a_aiaickaiam
end do k_aiaickaiam
end do c_aiaickaiam
end do m_aiaickaiam
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckaiaj: do c = n0c, n1c
k_aiajckaiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aiajckaiaj: do j = j0, n1jm
if (j == k) cycle j_aiajckaiaj
a0 = max(c + 1, n0abde)
a_aiajckaiaj: do a = a0, n1abde
if (a == c) cycle a_aiajckaiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckaiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckaiaj(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaiaj
end do a_aiajckaiaj
end do j_aiajckaiaj
end do k_aiajckaiaj
end do c_aiajckaiaj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiajcialci: do l = n0l, n1l
c_aiajcialci: do c = n0ce, n1ce
j_aiajcialci: do j = n0j, n1j
if (j == l) cycle j_aiajcialci
a0 = max(c + 1, n0abd)
a_aiajcialci: do a = a0, n1abd
if (a == c) cycle a_aiajcialci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajcialci: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aiajcialci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcialci(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcialci
end do a_aiajcialci
end do j_aiajcialci
end do c_aiajcialci
end do l_aiajcialci
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiajciaicm: do m = n0m, n1m
c_aiajciaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajciaicm: do j = n0j, n1j
if (j == m) cycle j_aiajciaicm
a0 = max(c + 1, n0abd)
a_aiajciaicm: do a = a0, n1abd
if (a == c) cycle a_aiajciaicm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aiajciaicm: do i = n0ikl, i1
if (i == j .or. i == m) cycle i_aiajciaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciaicm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciaicm
end do a_aiajciaicm
end do j_aiajciaicm
end do c_aiajciaicm
end do m_aiajciaicm
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajcialcj: do l = n0l, n1l
c_aiajcialcj: do c = n0ce, n1ce
j_aiajcialcj: do j = n0jm, n1jm
if (j == l) cycle j_aiajcialcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcialcj: do a = a0, n1abd
if (a == c) cycle a_aiajcialcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajcialcj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aiajcialcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcialcj(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcialcj
end do a_aiajcialcj
end do j_aiajcialcj
end do c_aiajcialcj
end do l_aiajcialcj
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == a, e == c, l == k, m == k
! No equalities independent of the above can hold.
!
c_aiajckakck: do c = n0ce, n1ce
k_aiajckakck: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckakck: do j = j0, n1j
if (j == k) cycle j_aiajckakck
a0 = max(c + 1, n0abd)
a_aiajckakck: do a = a0, n1abd
if (a == c) cycle a_aiajckakck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckakck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckakck(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckakck
end do a_aiajckakck
end do j_aiajckakck
end do k_aiajckakck
end do c_aiajckakck
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, e == c, j == i, m == k
! No equalities independent of the above can hold.
!
l_aiaickalck: do l = n0l, n1l
c_aiaickalck: do c = n0ce, n1ce
k_aiaickalck: do k = n0km, n1km
if (k == l) cycle k_aiaickalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickalck: do a = a0, n1abd
if (a == c) cycle a_aiaickalck
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaickalck: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aiaickalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickalck(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickalck
end do a_aiaickalck
end do k_aiaickalck
end do c_aiaickalck
end do l_aiaickalck
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckaick: do c = n0ce, n1ce
k_aiajckaick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckaick: do j = j0, n1j
if (j == k) cycle j_aiajckaick
a0 = max(c + 1, n0abd)
a_aiajckaick: do a = a0, n1abd
if (a == c) cycle a_aiajckaick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaick: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckaick(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaick
end do a_aiajckaick
end do j_aiajckaick
end do k_aiajckaick
end do c_aiajckaick
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aiajckajck: do c = n0ce, n1ce
k_aiajckajck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckajck: do j = j0, n1jl
if (j == k) cycle j_aiajckajck
a0 = max(c + 1, n0abd)
a_aiajckajck: do a = a0, n1abd
if (a == c) cycle a_aiajckajck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckajck(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajck
end do a_aiajckajck
end do j_aiajckajck
end do k_aiajckajck
end do c_aiajckajck
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckakci: do c = n0ce, n1ce
k_aiajckakci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckakci: do j = j0, n1j
if (j == k) cycle j_aiajckakci
a0 = max(c + 1, n0abd)
a_aiajckakci: do a = a0, n1abd
if (a == c) cycle a_aiajckakci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aiajckakci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckakci(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckakci
end do a_aiajckakci
end do j_aiajckakci
end do k_aiajckakci
end do c_aiajckakci
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aiajckakcj: do c = n0ce, n1ce
k_aiajckakcj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aiajckakcj: do j = j0, n1jm
if (j == k) cycle j_aiajckakcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckakcj: do a = a0, n1abd
if (a == c) cycle a_aiajckakcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckakcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckakcj(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckakcj
end do a_aiajckakcj
end do j_aiajckakcj
end do k_aiajckakcj
end do c_aiajckakcj
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
end subroutine ccjac_32_tripletm_dav_part6
end module ccjac_block_32_tripletm_dav_part6
