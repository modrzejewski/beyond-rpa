module ccjac_block_32_tripletm_dav_part8
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
subroutine ccjac_32_tripletm_dav_part8(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: i, j, k, m
integer :: ai, bj, ck, dl, em
integer :: a0, a1, b0, i0, i1, j0
integer :: n0ab, n0abe, n0ac, n0ace, n0ae
integer :: n0bd, n0cde, n0ce, n0ij, n0ijl
integer :: n0ijlm, n0ijm, n0ik, n0ikl, n0iklm
integer :: n0ikm, n0il, n0im, n0jl, n0jlm
integer :: n0jm, n0kl, n0klm, n0km
integer :: n1ab, n1abe, n1ac, n1ace, n1ae
integer :: n1bd, n1cde, n1ce, n1ij, n1ijl
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
n0abe = max(n0a, n0b, n0e)
n0ac = max(n0a, n0c)
n0ace = max(n0a, n0c, n0e)
n0ae = max(n0a, n0e)
n0bd = max(n0b, n0d)
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
n1abe = min(n1a, n1b, n1e)
n1ac = min(n1a, n1c)
n1ace = min(n1a, n1c, n1e)
n1ae = min(n1a, n1e)
n1bd = min(n1b, n1d)
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
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, e == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakbiam: do m = n0m, n1m
k_aibiakbiam: do k = n0k, n1k
if (k == m) cycle k_aibiakbiam
b_aibiakbiam: do b = n0bd, n1bd
a1 = min(b - 1, n1ace)
a_aibiakbiam: do a = n0ace, a1
if (a == b) cycle a_aibiakbiam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakbiam: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibiakbiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakbiam(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbiam
end do a_aibiakbiam
end do b_aibiakbiam
end do k_aibiakbiam
end do m_aibiakbiam
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, e == a, d == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakbiaj: do k = n0k, n1k
b_aibjakbiaj: do b = n0bd, n1bd
j0 = max(k + 1, n0jm)
j_aibjakbiaj: do j = j0, n1jm
if (j == k) cycle j_aibjakbiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbiaj: do a = n0ace, a1
if (a == b) cycle a_aibjakbiaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakbiaj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakbiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbiaj(a, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbiaj
end do a_aibjakbiaj
end do j_aibjakbiaj
end do b_aibjakbiaj
end do k_aibjakbiaj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, e == a, d == b, l == j, m == j
! No equalities independent of the above can hold.
!
k_aibjakbjaj: do k = n0k, n1k
b_aibjakbjaj: do b = n0bd, n1bd
j0 = max(k + 1, n0jlm)
j_aibjakbjaj: do j = j0, n1jlm
if (j == k) cycle j_aibjakbjaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbjaj: do a = n0ace, a1
if (a == b) cycle a_aibjakbjaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakbjaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbjaj(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjaj
end do a_aibjakbjaj
end do j_aibjakbjaj
end do b_aibjakbjaj
end do k_aibjakbjaj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjaibiei: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjaibiei: do b = b0, n1bd
if (b == e) cycle b_aibjaibiei
j_aibjaibiei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaibiei: do a = n0ac, a1
if (a == b .or. a == e) cycle a_aibjaibiei
i1 = min(j - 1, n1iklm)
i_aibjaibiei: do i = n0iklm, i1
if (i == j) cycle i_aibjaibiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaibiei(a, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibiei
end do a_aibjaibiei
end do j_aibjaibiei
end do b_aibjaibiei
end do e_aibjaibiei
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaibjei: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjaibjei: do b = b0, n1bd
if (b == e) cycle b_aibjaibjei
j_aibjaibjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjaibjei: do a = n0ac, a1
if (a == b .or. a == e) cycle a_aibjaibjei
i1 = min(j - 1, n1ikm)
i_aibjaibjei: do i = n0ikm, i1
if (i == j) cycle i_aibjaibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaibjei(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibjei
end do a_aibjaibjei
end do j_aibjaibjei
end do b_aibjaibjei
end do e_aibjaibjei
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibiakbkei: do e = n0e, n1e
k_aibiakbkei: do k = n0kl, n1kl
b0 = max(e + 1, n0bd)
b_aibiakbkei: do b = b0, n1bd
if (b == e) cycle b_aibiakbkei
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibiakbkei: do a = n0ac, a1
if (a == b .or. a == e) cycle a_aibiakbkei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibiakbkei: do i = i0, n1ijm
if (i == k) cycle i_aibiakbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakbkei(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbkei
end do a_aibiakbkei
end do b_aibiakbkei
end do k_aibiakbkei
end do e_aibiakbkei
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakbiei: do e = n0e, n1e
k_aibiakbiei: do k = n0k, n1k
b0 = max(e + 1, n0bd)
b_aibiakbiei: do b = b0, n1bd
if (b == e) cycle b_aibiakbiei
a1 = min(b - 1, n1ac)
a_aibiakbiei: do a = n0ac, a1
if (a == b .or. a == e) cycle a_aibiakbiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijlm)
i_aibiakbiei: do i = i0, n1ijlm
if (i == k) cycle i_aibiakbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakbiei(a, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbiei
end do a_aibiakbiei
end do b_aibiakbiei
end do k_aibiakbiei
end do e_aibiakbiei
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjcibici: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcibici: do b = b0, n1bd
if (b == c) cycle b_aibjcibici
j_aibjcibici: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibici: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibici
i1 = min(j - 1, n1iklm)
i_aibjcibici: do i = n0iklm, i1
if (i == j) cycle i_aibjcibici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibici(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibici
end do a_aibjcibici
end do j_aibjcibici
end do b_aibjcibici
end do c_aibjcibici
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjci: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcibjci: do b = b0, n1bd
if (b == c) cycle b_aibjcibjci
j_aibjcibjci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibjci
i1 = min(j - 1, n1ikm)
i_aibjcibjci: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibjci(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjci
end do a_aibjcibjci
end do j_aibjcibjci
end do b_aibjcibjci
end do c_aibjcibjci
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcibicj: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcibicj: do b = b0, n1bd
if (b == c) cycle b_aibjcibicj
j_aibjcibicj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibicj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibicj
i1 = min(j - 1, n1ikl)
i_aibjcibicj: do i = n0ikl, i1
if (i == j) cycle i_aibjcibicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibicj(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibicj
end do a_aibjcibicj
end do j_aibjcibicj
end do b_aibjcibicj
end do c_aibjcibicj
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k
! Equalities: d == b, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickbick: do c = n0ce, n1ce
k_aibickbick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbick: do b = b0, n1bd
if (b == c) cycle b_aibickbick
a_aibickbick: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbick
i0 = max(k + 1, n0ijl)
i_aibickbick: do i = i0, n1ijl
if (i == k) cycle i_aibickbick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbick(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbick
end do a_aibickbick
end do b_aibickbick
end do k_aibickbick
end do c_aibickbick
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k
! Equalities: d == b, e == c, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibickbkci: do c = n0ce, n1ce
k_aibickbkci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbkci: do b = b0, n1bd
if (b == c) cycle b_aibickbkci
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibickbkci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbkci
i0 = max(k + 1, n0ijm)
i_aibickbkci: do i = i0, n1ijm
if (i == k) cycle i_aibickbkci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbkci(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbkci
end do a_aibickbkci
end do b_aibickbkci
end do k_aibickbkci
end do c_aibickbkci
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k
! Equalities: d == b, e == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickbici: do c = n0ce, n1ce
k_aibickbici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbici: do b = b0, n1bd
if (b == c) cycle b_aibickbici
a_aibickbici: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbici
i0 = max(k + 1, n0ijlm)
i_aibickbici: do i = i0, n1ijlm
if (i == k) cycle i_aibickbici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbici(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbici
end do a_aibickbici
end do b_aibickbici
end do k_aibickbici
end do c_aibickbici
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: e == a, d == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjcibiai: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibiai: do b = b0, n1bd
if (b == c) cycle b_aibjcibiai
j_aibjcibiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjcibiai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcibiai
i1 = min(j - 1, n1iklm)
i_aibjcibiai: do i = n0iklm, i1
if (i == j) cycle i_aibjcibiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibiai(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibiai
end do a_aibjcibiai
end do j_aibjcibiai
end do b_aibjcibiai
end do c_aibjcibiai
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: e == a, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjai: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibjai: do b = b0, n1bd
if (b == c) cycle b_aibjcibjai
j_aibjcibjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjcibjai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcibjai
i1 = min(j - 1, n1ikm)
i_aibjcibjai: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibjai(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjai
end do a_aibjcibjai
end do j_aibjcibjai
end do b_aibjcibjai
end do c_aibjcibjai
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: e == a, d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcibiaj: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibiaj: do b = b0, n1bd
if (b == c) cycle b_aibjcibiaj
j_aibjcibiaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjcibiaj: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcibiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aibjcibiaj: do i = n0ikl, i1
if (i == j) cycle i_aibjcibiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibiaj(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibiaj
end do a_aibjcibiaj
end do j_aibjcibiaj
end do b_aibjcibiaj
end do c_aibjcibiaj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: e == a, d == b, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcibjaj: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibjaj: do b = b0, n1bd
if (b == c) cycle b_aibjcibjaj
j_aibjcibjaj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjcibjaj: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcibjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjcibjaj: do i = n0ik, i1
if (i == j) cycle i_aibjcibjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibjaj(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjaj
end do a_aibjcibjaj
end do j_aibjcibjaj
end do b_aibjcibjaj
end do c_aibjcibjaj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: e == a, d == b, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibickbkak: do c = n0c, n1c
k_aibickbkak: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbkak: do b = b0, n1bd
if (b == c) cycle b_aibickbkak
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibickbkak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickbkak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickbkak: do i = i0, n1ij
if (i == k) cycle i_aibickbkak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbkak(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbkak
end do a_aibickbkak
end do b_aibickbkak
end do k_aibickbkak
end do c_aibickbkak
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: e == a, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickbiak: do c = n0c, n1c
k_aibickbiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbiak: do b = b0, n1bd
if (b == c) cycle b_aibickbiak
a1 = min(b - 1, n1ae)
a_aibickbiak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickbiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibickbiak: do i = i0, n1ijl
if (i == k) cycle i_aibickbiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbiak(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbiak
end do a_aibickbiak
end do b_aibickbiak
end do k_aibickbiak
end do c_aibickbiak
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: e == a, d == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibickbkai: do c = n0c, n1c
k_aibickbkai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbkai: do b = b0, n1bd
if (b == c) cycle b_aibickbkai
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibickbkai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickbkai
i0 = max(k + 1, n0ijm)
i_aibickbkai: do i = i0, n1ijm
if (i == k) cycle i_aibickbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbkai(a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbkai
end do a_aibickbkai
end do b_aibickbkai
end do k_aibickbkai
end do c_aibickbkai
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: e == a, d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickbiai: do c = n0c, n1c
k_aibickbiai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbiai: do b = b0, n1bd
if (b == c) cycle b_aibickbiai
a1 = min(b - 1, n1ae)
a_aibickbiai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickbiai
i0 = max(k + 1, n0ijlm)
i_aibickbiai: do i = i0, n1ijlm
if (i == k) cycle i_aibickbiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbiai(a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbiai
end do a_aibickbiai
end do b_aibickbiai
end do k_aibickbiai
end do c_aibickbiai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiajcidiai: do d = n0d, n1d
c_aiajcidiai: do c = n0c, n1c
if (c == d) cycle c_aiajcidiai
j_aiajcidiai: do j = n0j, n1j
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcidiai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcidiai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1iklm)
i_aiajcidiai: do i = n0iklm, i1
if (i == j) cycle i_aiajcidiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcidiai(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcidiai
end do a_aiajcidiai
end do j_aiajcidiai
end do c_aiajcidiai
end do d_aiajcidiai
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidjai: do d = n0d, n1d
c_aiajcidjai: do c = n0c, n1c
if (c == d) cycle c_aiajcidjai
j_aiajcidjai: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcidjai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcidjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajcidjai: do i = n0ikm, i1
if (i == j) cycle i_aiajcidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcidjai(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcidjai
end do a_aiajcidjai
end do j_aiajcidjai
end do c_aiajcidjai
end do d_aiajcidjai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajcidiaj: do d = n0d, n1d
c_aiajcidiaj: do c = n0c, n1c
if (c == d) cycle c_aiajcidiaj
j_aiajcidiaj: do j = n0jm, n1jm
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcidiaj: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcidiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aiajcidiaj: do i = n0ikl, i1
if (i == j) cycle i_aiajcidiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajcidiaj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcidiaj
end do a_aiajcidiaj
end do j_aiajcidiaj
end do c_aiajcidiaj
end do d_aiajcidiaj
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, e == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiaickdiak: do d = n0d, n1d
c_aiaickdiak: do c = n0c, n1c
if (c == d) cycle c_aiaickdiak
k_aiaickdiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiaickdiak: do a = a0, a1
if (a == c .or. a == d) cycle a_aiaickdiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aiaickdiak: do i = i0, n1ijl
if (i == k) cycle i_aiaickdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickdiak(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickdiak
end do a_aiaickdiak
end do k_aiaickdiak
end do c_aiaickdiak
end do d_aiaickdiak
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, e == a, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
d_aiaickdkai: do d = n0d, n1d
c_aiaickdkai: do c = n0c, n1c
if (c == d) cycle c_aiaickdkai
k_aiaickdkai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiaickdkai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiaickdkai
i0 = max(k + 1, n0ijm)
i_aiaickdkai: do i = i0, n1ijm
if (i == k) cycle i_aiaickdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickdkai(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickdkai
end do a_aiaickdkai
end do k_aiaickdkai
end do c_aiaickdkai
end do d_aiaickdkai
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, e == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aiaickdiai: do d = n0d, n1d
c_aiaickdiai: do c = n0c, n1c
if (c == d) cycle c_aiaickdiai
k_aiaickdiai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiaickdiai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiaickdiai
i0 = max(k + 1, n0ijlm)
i_aiaickdiai: do i = i0, n1ijlm
if (i == k) cycle i_aiaickdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickdiai(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickdiai
end do a_aiaickdiai
end do k_aiaickdiai
end do c_aiaickdiai
end do d_aiaickdiai
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == c, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckcick: do c = n0cde, n1cde
k_aiajckcick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckcick: do j = j0, n1j
if (j == k) cycle j_aiajckcick
a0 = max(c + 1, n0ab)
a_aiajckcick: do a = a0, n1ab
if (a == c) cycle a_aiajckcick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(k + 1, n0il)
i_aiajckcick: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckcick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckcick(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckcick
end do a_aiajckcick
end do j_aiajckcick
end do k_aiajckcick
end do c_aiajckcick
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == c, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckckci: do c = n0cde, n1cde
k_aiajckckci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckckci: do j = j0, n1j
if (j == k) cycle j_aiajckckci
a0 = max(c + 1, n0ab)
a_aiajckckci: do a = a0, n1ab
if (a == c) cycle a_aiajckckci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(k - 1, n1im)
i_aiajckckci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aiajckckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckckci(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckckci
end do a_aiajckckci
end do j_aiajckckci
end do k_aiajckckci
end do c_aiajckckci
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajckcjci: do c = n0cde, n1cde
k_aiajckcjci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckcjci: do j = j0, n1jl
if (j == k) cycle j_aiajckcjci
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckcjci: do a = a0, n1ab
if (a == c) cycle a_aiajckcjci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aiajckcjci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aiajckcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckcjci(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckcjci
end do a_aiajckcjci
end do j_aiajckcjci
end do k_aiajckcjci
end do c_aiajckcjci
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
end subroutine ccjac_32_tripletm_dav_part8
end module ccjac_block_32_tripletm_dav_part8
