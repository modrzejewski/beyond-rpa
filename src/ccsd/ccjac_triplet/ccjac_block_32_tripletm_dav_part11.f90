module ccjac_block_32_tripletm_dav_part11
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
subroutine ccjac_32_tripletm_dav_part11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a, b, c
integer :: i, j, k
integer :: ai, bj, ck, dl, em
integer :: a0, a1, b0, i0, i1
integer :: n0ab, n0abd, n0abde, n0ac, n0acde
integer :: n0ace, n0ad, n0ade, n0bd, n0bde
integer :: n0cde, n0ce, n0ij, n0ijl, n0ijlm
integer :: n0ijm, n0ik, n0ikl, n0iklm, n0ikm
integer :: n0jl, n0jlm, n0jm, n0kl, n0klm
integer :: n0km
integer :: n1ab, n1abd, n1abde, n1ac, n1acde
integer :: n1ace, n1ad, n1ade, n1bd, n1bde
integer :: n1cde, n1ce, n1ij, n1ijl, n1ijlm
integer :: n1ijm, n1ik, n1ikl, n1iklm, n1ikm
integer :: n1jl, n1jlm, n1jm, n1kl, n1klm
integer :: n1km
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
n0abde = max(n0a, n0b, n0d, n0e)
n0ac = max(n0a, n0c)
n0acde = max(n0a, n0c, n0d, n0e)
n0ace = max(n0a, n0c, n0e)
n0ad = max(n0a, n0d)
n0ade = max(n0a, n0d, n0e)
n0bd = max(n0b, n0d)
n0bde = max(n0b, n0d, n0e)
n0cde = max(n0c, n0d, n0e)
n0ce = max(n0c, n0e)
n0ij = max(n0i, n0j)
n0ijl = max(n0i, n0j, n0l)
n0ijlm = max(n0i, n0j, n0l, n0m)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0iklm = max(n0i, n0k, n0l, n0m)
n0ikm = max(n0i, n0k, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1abde = min(n1a, n1b, n1d, n1e)
n1ac = min(n1a, n1c)
n1acde = min(n1a, n1c, n1d, n1e)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1ade = min(n1a, n1d, n1e)
n1bd = min(n1b, n1d)
n1bde = min(n1b, n1d, n1e)
n1cde = min(n1c, n1d, n1e)
n1ce = min(n1c, n1e)
n1ij = min(n1i, n1j)
n1ijl = min(n1i, n1j, n1l)
n1ijlm = min(n1i, n1j, n1l, n1m)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1iklm = min(n1i, n1k, n1l, n1m)
n1ikm = min(n1i, n1k, n1m)
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
! Free occupied indices: i, k
! Equalities: d == a, e == c, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibickakck: do c = n0ce, n1ce
k_aibickakck: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickakck: do b = b0, n1b
if (b == c) cycle b_aibickakck
a0 = max(c + 1, n0ad)
a_aibickakck: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickakck
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickakck: do i = i0, n1ij
if (i == k) cycle i_aibickakck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickakck(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickakck
end do a_aibickakck
end do b_aibickakck
end do k_aibickakck
end do c_aibickakck
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: d == a, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickaick: do c = n0ce, n1ce
k_aibickaick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickaick: do b = b0, n1b
if (b == c) cycle b_aibickaick
a0 = max(c + 1, n0ad)
a_aibickaick: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickaick
i0 = max(k + 1, n0ijl)
i_aibickaick: do i = i0, n1ijl
if (i == k) cycle i_aibickaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickaick(a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaick
end do a_aibickaick
end do b_aibickaick
end do k_aibickaick
end do c_aibickaick
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: d == a, e == c, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibickakci: do c = n0ce, n1ce
k_aibickakci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickakci: do b = b0, n1b
if (b == c) cycle b_aibickakci
a0 = max(c + 1, n0ad)
a_aibickakci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickakci
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibickakci: do i = i0, n1ijm
if (i == k) cycle i_aibickakci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickakci(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickakci
end do a_aibickakci
end do b_aibickakci
end do k_aibickakci
end do c_aibickakci
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: d == a, e == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickaici: do c = n0ce, n1ce
k_aibickaici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickaici: do b = b0, n1b
if (b == c) cycle b_aibickaici
a0 = max(c + 1, n0ad)
a_aibickaici: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickaici
i0 = max(k + 1, n0ijlm)
i_aibickaici: do i = i0, n1ijlm
if (i == k) cycle i_aibickaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickaici(a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaici
end do a_aibickaici
end do b_aibickaici
end do k_aibickaici
end do c_aibickaici
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajai: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjciajai: do b = b0, n1b
if (b == c) cycle b_aibjciajai
j_aibjciajai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjciajai: do a = n0ade, n1ade
if (a == b .or. a == c) cycle a_aibjciajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjciajai: do i = n0ikm, i1
if (i == j) cycle i_aibjciajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciajai(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciajai
end do a_aibjciajai
end do j_aibjciajai
end do b_aibjciajai
end do c_aibjciajai
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickaiak: do c = n0c, n1c
k_aibickaiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickaiak: do b = b0, n1b
if (b == c) cycle b_aibickaiak
a_aibickaiak: do a = n0ade, n1ade
if (a == b .or. a == c) cycle a_aibickaiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibickaiak: do i = i0, n1ijl
if (i == k) cycle i_aibickaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickaiak(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaiak
end do a_aibickaiak
end do b_aibickaiak
end do k_aibickaiak
end do c_aibickaiak
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciajai: do c = n0c, n1c
j_aiajciajai: do j = n0jl, n1jl
a0 = max(c + 1, n0abde)
a_aiajciajai: do a = a0, n1abde
if (a == c) cycle a_aiajciajai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajciajai: do i = n0ikm, i1
if (i == j) cycle i_aiajciajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciajai(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciajai
end do a_aiajciajai
end do j_aiajciajai
end do c_aiajciajai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiaickaiak: do c = n0c, n1c
k_aiaickaiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abde)
a_aiaickaiak: do a = a0, n1abde
if (a == c) cycle a_aiaickaiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aiaickaiak: do i = i0, n1ijl
if (i == k) cycle i_aiaickaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickaiak(a, i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaiak
end do a_aiaickaiak
end do k_aiaickaiak
end do c_aiaickaiak
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajciaici: do c = n0ce, n1ce
j_aiajciaici: do j = n0j, n1j
a0 = max(c + 1, n0abd)
a_aiajciaici: do a = a0, n1abd
if (a == c) cycle a_aiajciaici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1iklm)
i_aiajciaici: do i = n0iklm, i1
if (i == j) cycle i_aiajciaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciaici(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciaici
end do a_aiajciaici
end do j_aiajciaici
end do c_aiajciaici
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciajci: do c = n0ce, n1ce
j_aiajciajci: do j = n0jl, n1jl
a0 = max(c + 1, n0abd)
a_aiajciajci: do a = a0, n1abd
if (a == c) cycle a_aiajciajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajciajci: do i = n0ikm, i1
if (i == j) cycle i_aiajciajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciajci(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciajci
end do a_aiajciajci
end do j_aiajciajci
end do c_aiajciajci
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajciaicj: do c = n0ce, n1ce
j_aiajciaicj: do j = n0jm, n1jm
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajciaicj: do a = a0, n1abd
if (a == c) cycle a_aiajciaicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aiajciaicj: do i = n0ikl, i1
if (i == j) cycle i_aiajciaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciaicj(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciaicj
end do a_aiajciaicj
end do j_aiajciaicj
end do c_aiajciaicj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajciajcj: do c = n0ce, n1ce
j_aiajciajcj: do j = n0jlm, n1jlm
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajciajcj: do a = a0, n1abd
if (a == c) cycle a_aiajciajcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aiajciajcj: do i = n0ik, i1
if (i == j) cycle i_aiajciajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciajcj(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciajcj
end do a_aiajciajcj
end do j_aiajciajcj
end do c_aiajciajcj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == c, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
c_aiaickakck: do c = n0ce, n1ce
k_aiaickakck: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickakck: do a = a0, n1abd
if (a == c) cycle a_aiaickakck
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaickakck: do i = i0, n1ij
if (i == k) cycle i_aiaickakck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickakck(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickakck
end do a_aiaickakck
end do k_aiaickakck
end do c_aiaickakck
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiaickaick: do c = n0ce, n1ce
k_aiaickaick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickaick: do a = a0, n1abd
if (a == c) cycle a_aiaickaick
i0 = max(k + 1, n0ijl)
i_aiaickaick: do i = i0, n1ijl
if (i == k) cycle i_aiaickaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickaick(a, i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaick
end do a_aiaickaick
end do k_aiaickaick
end do c_aiaickaick
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == c, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiaickakci: do c = n0ce, n1ce
k_aiaickakci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickakci: do a = a0, n1abd
if (a == c) cycle a_aiaickakci
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aiaickakci: do i = i0, n1ijm
if (i == k) cycle i_aiaickakci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickakci(a, i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickakci
end do a_aiaickakci
end do k_aiaickakci
end do c_aiaickakci
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiaickaici: do c = n0ce, n1ce
k_aiaickaici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickaici: do a = a0, n1abd
if (a == c) cycle a_aiaickaici
i0 = max(k + 1, n0ijlm)
i_aiaickaici: do i = i0, n1ijlm
if (i == k) cycle i_aiaickaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickaici(a, i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaici
end do a_aiaickaici
end do k_aiaickaici
end do c_aiaickaici
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibjbi: do b = n0bde, n1bde
j_aibjaibjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaibjbi: do a = n0ac, a1
if (a == b) cycle a_aibjaibjbi
i1 = min(j - 1, n1ikm)
i_aibjaibjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjaibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaibjbi(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibjbi
end do a_aibjaibjbi
end do j_aibjaibjbi
end do b_aibjaibjbi
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibiakbibk: do k = n0km, n1km
b_aibiakbibk: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibiakbibk: do a = n0ac, a1
if (a == b) cycle a_aibiakbibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakbibk: do i = i0, n1ijl
if (i == k) cycle i_aibiakbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakbibk(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbibk
end do a_aibiakbibk
end do b_aibiakbibk
end do k_aibiakbibk
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, e == a, d == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
b_aibjaibiai: do b = n0bd, n1bd
j_aibjaibiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaibiai: do a = n0ace, a1
if (a == b) cycle a_aibjaibiai
i1 = min(j - 1, n1iklm)
i_aibjaibiai: do i = n0iklm, i1
if (i == j) cycle i_aibjaibiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaibiai(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibiai
end do a_aibjaibiai
end do j_aibjaibiai
end do b_aibjaibiai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, e == a, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibjai: do b = n0bd, n1bd
j_aibjaibjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaibjai: do a = n0ace, a1
if (a == b) cycle a_aibjaibjai
i1 = min(j - 1, n1ikm)
i_aibjaibjai: do i = n0ikm, i1
if (i == j) cycle i_aibjaibjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaibjai(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibjai
end do a_aibjaibjai
end do j_aibjaibjai
end do b_aibjaibjai
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, e == a, d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
b_aibjaibiaj: do b = n0bd, n1bd
j_aibjaibiaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaibiaj: do a = n0ace, a1
if (a == b) cycle a_aibjaibiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aibjaibiaj: do i = n0ikl, i1
if (i == j) cycle i_aibjaibiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaibiaj(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibiaj
end do a_aibjaibiaj
end do j_aibjaibiaj
end do b_aibjaibiaj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, e == a, d == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
b_aibjaibjaj: do b = n0bd, n1bd
j_aibjaibjaj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaibjaj: do a = n0ace, a1
if (a == b) cycle a_aibjaibjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjaibjaj: do i = n0ik, i1
if (i == j) cycle i_aibjaibjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaibjaj(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibjaj
end do a_aibjaibjaj
end do j_aibjaibjaj
end do b_aibjaibjaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, e == a, d == b, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibiakbkak: do k = n0klm, n1klm
b_aibiakbkak: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibiakbkak: do a = n0ace, a1
if (a == b) cycle a_aibiakbkak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakbkak: do i = i0, n1ij
if (i == k) cycle i_aibiakbkak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakbkak(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbkak
end do a_aibiakbkak
end do b_aibiakbkak
end do k_aibiakbkak
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, e == a, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibiakbiak: do k = n0km, n1km
b_aibiakbiak: do b = n0bd, n1bd
a1 = min(b - 1, n1ace)
a_aibiakbiak: do a = n0ace, a1
if (a == b) cycle a_aibiakbiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakbiak: do i = i0, n1ijl
if (i == k) cycle i_aibiakbiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakbiak(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbiak
end do a_aibiakbiak
end do b_aibiakbiak
end do k_aibiakbiak
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, e == a, d == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakbkai: do k = n0kl, n1kl
b_aibiakbkai: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibiakbkai: do a = n0ace, a1
if (a == b) cycle a_aibiakbkai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibiakbkai: do i = i0, n1ijm
if (i == k) cycle i_aibiakbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakbkai(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbkai
end do a_aibiakbkai
end do b_aibiakbkai
end do k_aibiakbkai
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, e == a, d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibiakbiai: do k = n0k, n1k
b_aibiakbiai: do b = n0bd, n1bd
a1 = min(b - 1, n1ace)
a_aibiakbiai: do a = n0ace, a1
if (a == b) cycle a_aibiakbiai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijlm)
i_aibiakbiai: do i = i0, n1ijlm
if (i == k) cycle i_aibiakbiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakbiai(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbiai
end do a_aibiakbiai
end do b_aibiakbiai
end do k_aibiakbiai
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicjci: do c = n0cde, n1cde
j_aiajcicjci: do j = n0jl, n1jl
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcicjci: do a = a0, n1ab
if (a == c) cycle a_aiajcicjci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajcicjci: do i = n0ikm, i1
if (i == j) cycle i_aiajcicjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcicjci(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcicjci
end do a_aiajcicjci
end do j_aiajcicjci
end do c_aiajcicjci
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == c, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiaickcick: do c = n0cde, n1cde
k_aiaickcick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickcick: do a = a0, n1ab
if (a == c) cycle a_aiaickcick
i0 = max(k + 1, n0ijl)
i_aiaickcick: do i = i0, n1ijl
if (i == k) cycle i_aiaickcick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickcick(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickcick
end do a_aiaickcick
end do k_aiaickcick
end do c_aiaickcick
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiajai: do b = n0b, n1b
j_aibjaiajai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjaiajai: do a = n0acde, a1
if (a == b) cycle a_aibjaiajai
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjaiajai: do i = n0ikm, i1
if (i == j) cycle i_aibjaiajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaiajai(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiajai
end do a_aibjaiajai
end do j_aibjaiajai
end do b_aibjaiajai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, e == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibiakaiak: do k = n0km, n1km
b_aibiakaiak: do b = n0b, n1b
a1 = min(b - 1, n1acde)
a_aibiakaiak: do a = n0acde, a1
if (a == b) cycle a_aibiakaiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakaiak: do i = i0, n1ijl
if (i == k) cycle i_aibiakaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakaiak(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakaiak
end do a_aibiakaiak
end do b_aibiakaiak
end do k_aibiakaiak
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
end subroutine ccjac_32_tripletm_dav_part11
end module ccjac_block_32_tripletm_dav_part11
