module ccjac_block_32_tripletm_dav_part9
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
subroutine ccjac_32_tripletm_dav_part9(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, c0, c1, i0, i1, j0, j1, k0, k1
integer :: n0ab, n0ac, n0acde, n0ad, n0be
integer :: n0cd, n0cde, n0ce, n0ij, n0ijl
integer :: n0ijlm, n0ijm, n0ik, n0ikl, n0iklm
integer :: n0ikm, n0il, n0im, n0jl, n0jlm
integer :: n0jm, n0kl, n0klm, n0km
integer :: n1ab, n1ac, n1acde, n1ad, n1be
integer :: n1cd, n1cde, n1ce, n1ij, n1ijl
integer :: n1ijlm, n1ijm, n1ik, n1ikl, n1iklm
integer :: n1ikm, n1il, n1im, n1jl, n1jlm
integer :: n1jm, n1kl, n1klm, n1km
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
n0ac = max(n0a, n0c)
n0acde = max(n0a, n0c, n0d, n0e)
n0ad = max(n0a, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
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
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jl = max(n0j, n0l)
n0jlm = max(n0j, n0l, n0m)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1ab = min(n1a, n1b)
n1ac = min(n1a, n1c)
n1acde = min(n1a, n1c, n1d, n1e)
n1ad = min(n1a, n1d)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
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
n1il = min(n1i, n1l)
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
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckcicj: do c = n0cde, n1cde
k_aiajckcicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aiajckcicj: do j = j0, n1jm
if (j == k) cycle j_aiajckcicj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckcicj: do a = a0, n1ab
if (a == c) cycle a_aiajckcicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckcicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckcicj(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckcicj
end do a_aiajckcicj
end do j_aiajckcicj
end do k_aiajckcicj
end do c_aiajckcicj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiajciciei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajciciei: do c = c0, n1cd
if (c == e) cycle c_aiajciciei
j_aiajciciei: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajciciei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajciciei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1iklm)
i_aiajciciei: do i = n0iklm, i1
if (i == j) cycle i_aiajciciei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciciei(a, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciciei
end do a_aiajciciei
end do j_aiajciciei
end do c_aiajciciei
end do e_aiajciciei
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajcicjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajcicjei: do c = c0, n1cd
if (c == e) cycle c_aiajcicjei
j_aiajcicjei: do j = n0jl, n1jl
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcicjei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajcicjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajcicjei: do i = n0ikm, i1
if (i == j) cycle i_aiajcicjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcicjei(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcicjei
end do a_aiajcicjei
end do j_aiajcicjei
end do c_aiajcicjei
end do e_aiajcicjei
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k
! Equalities: b == a, d == c, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aiaickckei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiaickckei: do c = c0, n1cd
if (c == e) cycle c_aiaickckei
k_aiaickckei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickckei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiaickckei
i0 = max(k + 1, n0ijm)
i_aiaickckei: do i = i0, n1ijm
if (i == k) cycle i_aiaickckei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickckei(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickckei
end do a_aiaickckei
end do k_aiaickckei
end do c_aiaickckei
end do e_aiaickckei
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k
! Equalities: b == a, d == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiaickciei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiaickciei: do c = c0, n1cd
if (c == e) cycle c_aiaickciei
k_aiaickciei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickciei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiaickciei
i0 = max(k + 1, n0ijlm)
i_aiaickciei: do i = i0, n1ijlm
if (i == k) cycle i_aiaickciei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickciei(a, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickciei
end do a_aiaickciei
end do k_aiaickciei
end do c_aiaickciei
end do e_aiaickciei
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiajcidici: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajcidici: do c = n0ce, c1
if (c == d) cycle c_aiajcidici
j_aiajcidici: do j = n0j, n1j
a0 = max(c + 1, n0ab)
a_aiajcidici: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1iklm)
i_aiajcidici: do i = n0iklm, i1
if (i == j) cycle i_aiajcidici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcidici(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcidici
end do a_aiajcidici
end do j_aiajcidici
end do c_aiajcidici
end do d_aiajcidici
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajcidicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajcidicj: do c = n0ce, c1
if (c == d) cycle c_aiajcidicj
j_aiajcidicj: do j = n0jm, n1jm
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcidicj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aiajcidicj: do i = n0ikl, i1
if (i == j) cycle i_aiajcidicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcidicj(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcidicj
end do a_aiajcidicj
end do j_aiajcidicj
end do c_aiajcidicj
end do d_aiajcidicj
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiaickdick: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiaickdick: do c = n0ce, c1
if (c == d) cycle c_aiaickdick
k_aiaickdick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickdick: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiaickdick
i0 = max(k + 1, n0ijl)
i_aiaickdick: do i = i0, n1ijl
if (i == k) cycle i_aiaickdick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickdick(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickdick
end do a_aiaickdick
end do k_aiaickdick
end do c_aiaickdick
end do d_aiaickdick
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, e == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiaickdici: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiaickdici: do c = n0ce, c1
if (c == d) cycle c_aiaickdici
k_aiaickdici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickdici: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiaickdici
i0 = max(k + 1, n0ijlm)
i_aiaickdici: do i = i0, n1ijlm
if (i == k) cycle i_aiaickdici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickdici(a, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickdici
end do a_aiaickdici
end do k_aiaickdici
end do c_aiaickdici
end do d_aiaickdici
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjaidibi: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjaidibi: do b = n0be, b1
if (b == d) cycle b_aibjaidibi
j_aibjaidibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaidibi: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjaidibi
i1 = min(j - 1, n1iklm)
i_aibjaidibi: do i = n0iklm, i1
if (i == j) cycle i_aibjaidibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaidibi(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaidibi
end do a_aibjaidibi
end do j_aibjaidibi
end do b_aibjaidibi
end do d_aibjaidibi
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjaidibj: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjaidibj: do b = n0be, b1
if (b == d) cycle b_aibjaidibj
j_aibjaidibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaidibj: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjaidibj
i1 = min(j - 1, n1ikl)
i_aibjaidibj: do i = n0ikl, i1
if (i == j) cycle i_aibjaidibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaidibj(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaidibj
end do a_aibjaidibj
end do j_aibjaidibj
end do b_aibjaidibj
end do d_aibjaidibj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibiakdibk: do d = n0d, n1d
k_aibiakdibk: do k = n0km, n1km
b1 = min(d - 1, n1be)
b_aibiakdibk: do b = n0be, b1
if (b == d) cycle b_aibiakdibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibiakdibk: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibiakdibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakdibk: do i = i0, n1ijl
if (i == k) cycle i_aibiakdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakdibk(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakdibk
end do a_aibiakdibk
end do b_aibiakdibk
end do k_aibiakdibk
end do d_aibiakdibk
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibiakdibi: do d = n0d, n1d
k_aibiakdibi: do k = n0k, n1k
b1 = min(d - 1, n1be)
b_aibiakdibi: do b = n0be, b1
if (b == d) cycle b_aibiakdibi
a1 = min(b - 1, n1ac)
a_aibiakdibi: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibiakdibi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijlm)
i_aibiakdibi: do i = i0, n1ijlm
if (i == k) cycle i_aibiakdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakdibi(a, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakdibi
end do a_aibiakdibi
end do b_aibiakdibi
end do k_aibiakdibi
end do d_aibiakdibi
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjciaibi: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciaibi: do b = b0, n1be
if (b == c) cycle b_aibjciaibi
j_aibjciaibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciaibi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaibi
i1 = min(j - 1, n1iklm)
i_aibjciaibi: do i = n0iklm, i1
if (i == j) cycle i_aibjciaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciaibi(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciaibi
end do a_aibjciaibi
end do j_aibjciaibi
end do b_aibjciaibi
end do c_aibjciaibi
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajbi: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciajbi: do b = b0, n1be
if (b == c) cycle b_aibjciajbi
j_aibjciajbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciajbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjciajbi: do i = n0ikm, i1
if (i == j) cycle i_aibjciajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciajbi(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciajbi
end do a_aibjciajbi
end do j_aibjciajbi
end do b_aibjciajbi
end do c_aibjciajbi
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaibj: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciaibj: do b = b0, n1be
if (b == c) cycle b_aibjciaibj
j_aibjciaibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciaibj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaibj
i1 = min(j - 1, n1ikl)
i_aibjciaibj: do i = n0ikl, i1
if (i == j) cycle i_aibjciaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciaibj(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciaibj
end do a_aibjciaibj
end do j_aibjciaibj
end do b_aibjciaibj
end do c_aibjciaibj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjciajbj: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciajbj: do b = b0, n1be
if (b == c) cycle b_aibjciajbj
j_aibjciajbj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciajbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjciajbj: do i = n0ik, i1
if (i == j) cycle i_aibjciajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciajbj(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciajbj
end do a_aibjciajbj
end do j_aibjciajbj
end do b_aibjciajbj
end do c_aibjciajbj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == b, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibickakbk: do c = n0c, n1c
k_aibickakbk: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibickakbk: do b = b0, n1be
if (b == c) cycle b_aibickakbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibickakbk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickakbk
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickakbk: do i = i0, n1ij
if (i == k) cycle i_aibickakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickakbk(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickakbk
end do a_aibickakbk
end do b_aibickakbk
end do k_aibickakbk
end do c_aibickakbk
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickaibk: do c = n0c, n1c
k_aibickaibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibickaibk: do b = b0, n1be
if (b == c) cycle b_aibickaibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibickaibk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickaibk
i0 = max(k + 1, n0ijl)
i_aibickaibk: do i = i0, n1ijl
if (i == k) cycle i_aibickaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickaibk(a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaibk
end do a_aibickaibk
end do b_aibickaibk
end do k_aibickaibk
end do c_aibickaibk
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibickakbi: do c = n0c, n1c
k_aibickakbi: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibickakbi: do b = b0, n1be
if (b == c) cycle b_aibickakbi
a0 = max(b + 1, n0ad)
a_aibickakbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickakbi
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibickakbi: do i = i0, n1ijm
if (i == k) cycle i_aibickakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickakbi(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickakbi
end do a_aibickakbi
end do b_aibickakbi
end do k_aibickakbi
end do c_aibickakbi
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickaibi: do c = n0c, n1c
k_aibickaibi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibickaibi: do b = b0, n1be
if (b == c) cycle b_aibickaibi
a0 = max(b + 1, n0ad)
a_aibickaibi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickaibi
i0 = max(k + 1, n0ijlm)
i_aibickaibi: do i = i0, n1ijlm
if (i == k) cycle i_aibickaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickaibi(a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaibi
end do a_aibickaibi
end do b_aibickaibi
end do k_aibickaibi
end do c_aibickaibi
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == a, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjaialai: do l = n0l, n1l
b_aibjaialai: do b = n0b, n1b
j_aibjaialai: do j = n0j, n1j
if (j == l) cycle j_aibjaialai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjaialai: do a = n0acde, a1
if (a == b) cycle a_aibjaialai
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, l - 1, n1ikm)
i_aibjaialai: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjaialai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaialai(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaialai
end do a_aibjaialai
end do j_aibjaialai
end do b_aibjaialai
end do l_aibjaialai
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == a, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjaiaiam: do m = n0m, n1m
b_aibjaiaiam: do b = n0b, n1b
j_aibjaiaiam: do j = n0j, n1j
if (j == m) cycle j_aibjaiaiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjaiaiam: do a = n0acde, a1
if (a == b) cycle a_aibjaiaiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ikl)
i1 = min(j - 1, n1ikl)
i_aibjaiaiam: do i = i0, i1
if (i == j .or. i == m) cycle i_aibjaiaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaiaiam(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiaiam
end do a_aibjaiaiam
end do j_aibjaiaiam
end do b_aibjaiaiam
end do m_aibjaiaiam
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjaialaj: do l = n0l, n1l
b_aibjaialaj: do b = n0b, n1b
j1 = min(l - 1, n1jm)
j_aibjaialaj: do j = n0jm, j1
if (j == l) cycle j_aibjaialaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjaialaj: do a = n0acde, a1
if (a == b) cycle a_aibjaialaj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjaialaj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjaialaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaialaj(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaialaj
end do a_aibjaialaj
end do j_aibjaialaj
end do b_aibjaialaj
end do l_aibjaialaj
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaiajam: do m = n0m, n1m
b_aibjaiajam: do b = n0b, n1b
j0 = max(m + 1, n0jl)
j_aibjaiajam: do j = j0, n1jl
if (j == m) cycle j_aibjaiajam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjaiajam: do a = n0acde, a1
if (a == b) cycle a_aibjaiajam
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjaiajam: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjaiajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaiajam(i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiajam
end do a_aibjaiajam
end do j_aibjaiajam
end do b_aibjaiajam
end do m_aibjaiajam
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == a, e == a, j == i, m == k
! No equalities independent of the above can hold.
!
l_aibiakalak: do l = n0l, n1l
k1 = min(l - 1, n1km)
k_aibiakalak: do k = n0km, k1
if (k == l) cycle k_aibiakalak
b_aibiakalak: do b = n0b, n1b
a1 = min(b - 1, n1acde)
a_aibiakalak: do a = n0acde, a1
if (a == b) cycle a_aibiakalak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakalak: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibiakalak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakalak(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakalak
end do a_aibiakalak
end do b_aibiakalak
end do k_aibiakalak
end do l_aibiakalak
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjakaiak: do k = n0km, n1km
b_aibjakaiak: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjakaiak: do j = j0, n1j
if (j == k) cycle j_aibjakaiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakaiak: do a = n0acde, a1
if (a == b) cycle a_aibjakaiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i_aibjakaiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakaiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakaiak(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakaiak
end do a_aibjakaiak
end do j_aibjakaiak
end do b_aibjakaiak
end do k_aibjakaiak
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, d == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjakajak: do k = n0km, n1km
b_aibjakajak: do b = n0b, n1b
j0 = max(k + 1, n0jl)
j_aibjakajak: do j = j0, n1jl
if (j == k) cycle j_aibjakajak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakajak: do a = n0acde, a1
if (a == b) cycle a_aibjakajak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakajak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakajak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakajak(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakajak
end do a_aibjakajak
end do j_aibjakajak
end do b_aibjakajak
end do k_aibjakajak
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjakakai: do k = n0kl, n1kl
b_aibjakakai: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjakakai: do j = j0, n1j
if (j == k) cycle j_aibjakakai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakakai: do a = n0acde, a1
if (a == b) cycle a_aibjakakai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aibjakakai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakakai(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakakai
end do a_aibjakakai
end do j_aibjakakai
end do b_aibjakakai
end do k_aibjakakai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, d == a, e == a, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibiakakam: do m = n0m, n1m
k0 = max(m + 1, n0kl)
k_aibiakakam: do k = k0, n1kl
if (k == m) cycle k_aibiakakam
b_aibiakakam: do b = n0b, n1b
a1 = min(b - 1, n1acde)
a_aibiakakam: do a = n0acde, a1
if (a == b) cycle a_aibiakakam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakakam: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aibiakakam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakakam(i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakakam
end do a_aibiakakam
end do b_aibiakakam
end do k_aibiakakam
end do m_aibiakakam
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
end subroutine ccjac_32_tripletm_dav_part9
end module ccjac_block_32_tripletm_dav_part9
