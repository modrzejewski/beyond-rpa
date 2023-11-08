module ccjac_block_32_tripletm_dav_part3
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
subroutine ccjac_32_tripletm_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, c0, c1, i0, i1, j0
integer :: n0ab, n0abe, n0ac, n0ad, n0ae
integer :: n0bd, n0be, n0cd, n0ce, n0ij
integer :: n0ijl, n0ijlm, n0ijm, n0ik, n0ikl
integer :: n0iklm, n0ikm, n0il, n0ilm, n0im
integer :: n0jl, n0jlm, n0jm, n0kl, n0klm
integer :: n0km
integer :: n1ab, n1abe, n1ac, n1ad, n1ae
integer :: n1bd, n1be, n1cd, n1ce, n1ij
integer :: n1ijl, n1ijlm, n1ijm, n1ik, n1ikl
integer :: n1iklm, n1ikm, n1il, n1ilm, n1im
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
n0abe = max(n0a, n0b, n0e)
n0ac = max(n0a, n0c)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
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
n1ab = min(n1a, n1b)
n1abe = min(n1a, n1b, n1e)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
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
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: e == a, d == b, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckbjai: do c = n0c, n1c
k_aibjckbjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjai: do b = b0, n1bd
if (b == c) cycle b_aibjckbjai
j0 = max(k + 1, n0jl)
j_aibjckbjai: do j = j0, n1jl
if (j == k) cycle j_aibjckbjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbjai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbjai
i_aibjckbjai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbjai(a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjai
end do a_aibjckbjai
end do j_aibjckbjai
end do b_aibjckbjai
end do k_aibjckbjai
end do c_aibjckbjai
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, m
! Equalities: e == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibickbiam: do m = n0m, n1m
c_aibickbiam: do c = n0c, n1c
k_aibickbiam: do k = n0k, n1k
if (k == m) cycle k_aibickbiam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbiam: do b = b0, n1bd
if (b == c) cycle b_aibickbiam
a1 = min(b - 1, n1ae)
a_aibickbiam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickbiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibickbiam: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibickbiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbiam(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbiam
end do a_aibickbiam
end do b_aibickbiam
end do k_aibickbiam
end do c_aibickbiam
end do m_aibickbiam
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: e == a, d == b, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckbjaj: do c = n0c, n1c
k_aibjckbjaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjaj: do b = b0, n1bd
if (b == c) cycle b_aibjckbjaj
j0 = max(k + 1, n0jlm)
j_aibjckbjaj: do j = j0, n1jlm
if (j == k) cycle j_aibjckbjaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbjaj: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckbjaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbjaj(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjaj
end do a_aibjckbjaj
end do j_aibjckbjaj
end do b_aibjckbjaj
end do k_aibjckbjaj
end do c_aibjckbjaj
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjcibiei: do e = n0e, n1e
c_aibjcibiei: do c = n0c, n1c
if (c == e) cycle c_aibjcibiei
b0 = max(c + 1, e + 1, n0bd)
b_aibjcibiei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcibiei
j_aibjcibiei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibiei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibiei
i1 = min(j - 1, n1iklm)
i_aibjcibiei: do i = n0iklm, i1
if (i == j) cycle i_aibjcibiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibiei(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibiei
end do a_aibjcibiei
end do j_aibjcibiei
end do b_aibjcibiei
end do c_aibjcibiei
end do e_aibjcibiei
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcibjei: do e = n0e, n1e
c_aibjcibjei: do c = n0c, n1c
if (c == e) cycle c_aibjcibjei
b0 = max(c + 1, e + 1, n0bd)
b_aibjcibjei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcibjei
j_aibjcibjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibjei
i1 = min(j - 1, n1ikm)
i_aibjcibjei: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibjei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjei
end do a_aibjcibjei
end do j_aibjcibjei
end do b_aibjcibjei
end do c_aibjcibjei
end do e_aibjcibjei
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k
! Equalities: d == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibickbkei: do e = n0e, n1e
c_aibickbkei: do c = n0c, n1c
if (c == e) cycle c_aibickbkei
k_aibickbkei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibickbkei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibickbkei
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibickbkei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibickbkei
i0 = max(k + 1, n0ijm)
i_aibickbkei: do i = i0, n1ijm
if (i == k) cycle i_aibickbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbkei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbkei
end do a_aibickbkei
end do b_aibickbkei
end do k_aibickbkei
end do c_aibickbkei
end do e_aibickbkei
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibickbiei: do e = n0e, n1e
c_aibickbiei: do c = n0c, n1c
if (c == e) cycle c_aibickbiei
k_aibickbiei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibickbiei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibickbiei
a_aibickbiei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibickbiei
i0 = max(k + 1, n0ijlm)
i_aibickbiei: do i = i0, n1ijlm
if (i == k) cycle i_aibickbiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbiei(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbiei
end do a_aibickbiei
end do b_aibickbiei
end do k_aibickbiei
end do c_aibickbiei
end do e_aibickbiei
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, j
! Equalities: b == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiajckdiak: do d = n0d, n1d
c_aiajckdiak: do c = n0c, n1c
if (c == d) cycle c_aiajckdiak
k_aiajckdiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckdiak: do j = j0, n1j
if (j == k) cycle j_aiajckdiak
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdiak: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdiak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckdiak: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckdiak(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdiak
end do a_aiajckdiak
end do j_aiajckdiak
end do k_aiajckdiak
end do c_aiajckdiak
end do d_aiajckdiak
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, j
! Equalities: b == a, e == a, m == i, l == k
! No equalities independent of the above can hold.
!
d_aiajckdkai: do d = n0d, n1d
c_aiajckdkai: do c = n0c, n1c
if (c == d) cycle c_aiajckdkai
k_aiajckdkai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckdkai: do j = j0, n1j
if (j == k) cycle j_aiajckdkai
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdkai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdkai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdkai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aiajckdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckdkai(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdkai
end do a_aiajckdkai
end do j_aiajckdkai
end do k_aiajckdkai
end do c_aiajckdkai
end do d_aiajckdkai
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajckdjai: do d = n0d, n1d
c_aiajckdjai: do c = n0c, n1c
if (c == d) cycle c_aiajckdjai
k_aiajckdjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckdjai: do j = j0, n1jl
if (j == k) cycle j_aiajckdjai
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdjai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdjai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aiajckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckdjai(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdjai
end do a_aiajckdjai
end do j_aiajckdjai
end do k_aiajckdjai
end do c_aiajckdjai
end do d_aiajckdjai
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajckdiaj: do d = n0d, n1d
c_aiajckdiaj: do c = n0c, n1c
if (c == d) cycle c_aiajckdiaj
k_aiajckdiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aiajckdiaj: do j = j0, n1jm
if (j == k) cycle j_aiajckdiaj
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdiaj: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdiaj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckdiaj(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdiaj
end do a_aiajckdiaj
end do j_aiajckdiaj
end do k_aiajckdiaj
end do c_aiajckdiaj
end do d_aiajckdiaj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k, j
! Equalities: b == a, d == c, m == i, l == k
! No equalities independent of the above can hold.
!
e_aiajckckei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajckckei: do c = c0, n1cd
if (c == e) cycle c_aiajckckei
k_aiajckckei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckckei: do j = j0, n1j
if (j == k) cycle j_aiajckckei
a0 = max(c + 1, n0ab)
a_aiajckckei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajckckei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckckei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aiajckckei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckckei(a, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckckei
end do a_aiajckckei
end do j_aiajckckei
end do k_aiajckckei
end do c_aiajckckei
end do e_aiajckckei
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajckcjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajckcjei: do c = c0, n1cd
if (c == e) cycle c_aiajckcjei
k_aiajckcjei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckcjei: do j = j0, n1jl
if (j == k) cycle j_aiajckcjei
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckcjei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajckcjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aiajckcjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckcjei(a, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckcjei
end do a_aiajckcjei
end do j_aiajckcjei
end do k_aiajckcjei
end do c_aiajckcjei
end do e_aiajckcjei
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k, j
! Equalities: b == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiajckdick: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajckdick: do c = n0ce, c1
if (c == d) cycle c_aiajckdick
k_aiajckdick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckdick: do j = j0, n1j
if (j == k) cycle j_aiajckdick
a0 = max(c + 1, n0ab)
a_aiajckdick: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdick: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckdick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckdick(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdick
end do a_aiajckdick
end do j_aiajckdick
end do k_aiajckdick
end do c_aiajckdick
end do d_aiajckdick
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajckdicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajckdicj: do c = n0ce, c1
if (c == d) cycle c_aiajckdicj
k_aiajckdicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aiajckdicj: do j = j0, n1jm
if (j == k) cycle j_aiajckdicj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckdicj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdicj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckdicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckdicj(a, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdicj
end do a_aiajckdicj
end do j_aiajckdicj
end do k_aiajckdicj
end do c_aiajckdicj
end do d_aiajckdicj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, j
! Equalities: c == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjakdibk: do d = n0d, n1d
k_aibjakdibk: do k = n0km, n1km
b1 = min(d - 1, n1be)
b_aibjakdibk: do b = n0be, b1
if (b == d) cycle b_aibjakdibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakdibk: do j = j0, n1j
if (j == k) cycle j_aibjakdibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakdibk: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjakdibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakdibk(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdibk
end do a_aibjakdibk
end do j_aibjakdibk
end do b_aibjakdibk
end do k_aibjakdibk
end do d_aibjakdibk
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjakdibj: do d = n0d, n1d
k_aibjakdibj: do k = n0k, n1k
b1 = min(d - 1, n1be)
b_aibjakdibj: do b = n0be, b1
if (b == d) cycle b_aibjakdibj
j0 = max(k + 1, n0jm)
j_aibjakdibj: do j = j0, n1jm
if (j == k) cycle j_aibjakdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakdibj: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjakdibj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdibj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakdibj(a, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdibj
end do a_aibjakdibj
end do j_aibjakdibj
end do b_aibjakdibj
end do k_aibjakdibj
end do d_aibjakdibj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjcialbi: do l = n0l, n1l
c_aibjcialbi: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcialbi: do b = b0, n1be
if (b == c) cycle b_aibjcialbi
j_aibjcialbi: do j = n0j, n1j
if (j == l) cycle j_aibjcialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcialbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjcialbi: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjcialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcialbi(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcialbi
end do a_aibjcialbi
end do j_aibjcialbi
end do b_aibjcialbi
end do c_aibjcialbi
end do l_aibjcialbi
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjciaibm: do m = n0m, n1m
c_aibjciaibm: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciaibm: do b = b0, n1be
if (b == c) cycle b_aibjciaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjciaibm: do j = n0j, n1j
if (j == m) cycle j_aibjciaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaibm
i1 = min(j - 1, n1ikl)
i_aibjciaibm: do i = n0ikl, i1
if (i == j .or. i == m) cycle i_aibjciaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciaibm(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciaibm
end do a_aibjciaibm
end do j_aibjciaibm
end do b_aibjciaibm
end do c_aibjciaibm
end do m_aibjciaibm
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialbj: do l = n0l, n1l
c_aibjcialbj: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjcialbj: do b = b0, n1be
if (b == c) cycle b_aibjcialbj
j_aibjcialbj: do j = n0jm, n1jm
if (j == l) cycle j_aibjcialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcialbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjcialbj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjcialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcialbj(i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcialbj
end do a_aibjcialbj
end do j_aibjcialbj
end do b_aibjcialbj
end do c_aibjcialbj
end do l_aibjcialbj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j
! Equalities: d == a, e == b, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckakbk: do c = n0c, n1c
k_aibjckakbk: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckakbk: do b = b0, n1be
if (b == c) cycle b_aibjckakbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckakbk: do j = j0, n1j
if (j == k) cycle j_aibjckakbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckakbk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakbk
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckakbk(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckakbk
end do a_aibjckakbk
end do j_aibjckakbk
end do b_aibjckakbk
end do k_aibjckakbk
end do c_aibjckakbk
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, e == b, j == i, m == k
! No equalities independent of the above can hold.
!
l_aibickalbk: do l = n0l, n1l
c_aibickalbk: do c = n0c, n1c
k_aibickalbk: do k = n0km, n1km
if (k == l) cycle k_aibickalbk
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibickalbk: do b = b0, n1be
if (b == c) cycle b_aibickalbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibickalbk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickalbk
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickalbk: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibickalbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickalbk(i, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickalbk
end do a_aibickalbk
end do b_aibickalbk
end do k_aibickalbk
end do c_aibickalbk
end do l_aibickalbk
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaibk: do c = n0c, n1c
k_aibjckaibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckaibk: do b = b0, n1be
if (b == c) cycle b_aibjckaibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckaibk: do j = j0, n1j
if (j == k) cycle j_aibjckaibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibk
i_aibjckaibk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaibk(a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaibk
end do a_aibjckaibk
end do j_aibjckaibk
end do b_aibjckaibk
end do k_aibjckaibk
end do c_aibjckaibk
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajbk: do c = n0c, n1c
k_aibjckajbk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckajbk: do b = b0, n1be
if (b == c) cycle b_aibjckajbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjckajbk: do j = j0, n1jl
if (j == k) cycle j_aibjckajbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckajbk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajbk
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckajbk(i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckajbk
end do a_aibjckajbk
end do j_aibjckajbk
end do b_aibjckajbk
end do k_aibjckajbk
end do c_aibjckajbk
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckakbj: do c = n0c, n1c
k_aibjckakbj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckakbj: do b = b0, n1be
if (b == c) cycle b_aibjckakbj
j0 = max(k + 1, n0jm)
j_aibjckakbj: do j = j0, n1jm
if (j == k) cycle j_aibjckakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckakbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakbj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckakbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckakbj(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckakbj
end do a_aibjckakbj
end do j_aibjckakbj
end do b_aibjckakbj
end do k_aibjckakbj
end do c_aibjckakbj
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibickalbi: do l = n0l, n1l
c_aibickalbi: do c = n0c, n1c
k_aibickalbi: do k = n0k, n1k
if (k == l) cycle k_aibickalbi
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibickalbi: do b = b0, n1be
if (b == c) cycle b_aibickalbi
a0 = max(b + 1, n0ad)
a_aibickalbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibickalbi: do i = i0, n1ijm
if (i == k .or. i == l) cycle i_aibickalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickalbi(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickalbi
end do a_aibickalbi
end do b_aibickalbi
end do k_aibickalbi
end do c_aibickalbi
end do l_aibickalbi
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckaibi: do c = n0c, n1c
k_aibjckaibi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckaibi: do b = b0, n1be
if (b == c) cycle b_aibjckaibi
j0 = max(k + 1, n0j)
j_aibjckaibi: do j = j0, n1j
if (j == k) cycle j_aibjckaibi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibi
i_aibjckaibi: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjckaibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaibi(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaibi
end do a_aibjckaibi
end do j_aibjckaibi
end do b_aibjckaibi
end do k_aibjckaibi
end do c_aibjckaibi
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, m
! Equalities: d == a, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibickaibm: do m = n0m, n1m
c_aibickaibm: do c = n0c, n1c
k_aibickaibm: do k = n0k, n1k
if (k == m) cycle k_aibickaibm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibickaibm: do b = b0, n1be
if (b == c) cycle b_aibickaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibickaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickaibm
i0 = max(k + 1, n0ijl)
i_aibickaibm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibickaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickaibm(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaibm
end do a_aibickaibm
end do b_aibickaibm
end do k_aibickaibm
end do c_aibickaibm
end do m_aibickaibm
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaibj: do c = n0c, n1c
k_aibjckaibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckaibj: do b = b0, n1be
if (b == c) cycle b_aibjckaibj
j0 = max(k + 1, n0jm)
j_aibjckaibj: do j = j0, n1jm
if (j == k) cycle j_aibjckaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibj
i_aibjckaibj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaibj(a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaibj
end do a_aibjckaibj
end do j_aibjckaibj
end do b_aibjckaibj
end do k_aibjckaibj
end do c_aibjckaibj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, e == b, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjckajbj: do c = n0c, n1c
k_aibjckajbj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckajbj: do b = b0, n1be
if (b == c) cycle b_aibjckajbj
j0 = max(k + 1, n0jlm)
j_aibjckajbj: do j = j0, n1jlm
if (j == k) cycle j_aibjckajbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckajbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajbj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckajbj(i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckajbj
end do a_aibjckajbj
end do j_aibjckajbj
end do b_aibjckajbj
end do k_aibjckajbj
end do c_aibjckajbj
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
end subroutine ccjac_32_tripletm_dav_part3
end module ccjac_block_32_tripletm_dav_part3
