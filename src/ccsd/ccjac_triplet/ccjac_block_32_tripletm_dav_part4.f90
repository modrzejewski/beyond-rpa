module ccjac_block_32_tripletm_dav_part4
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
subroutine ccjac_32_tripletm_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, i0, i1, j0, j1, k0, k1
integer :: n0acd, n0acde, n0ae, n0be, n0cd
integer :: n0cde, n0ij, n0ijl, n0ijlm, n0ijm
integer :: n0ik, n0ikl, n0iklm, n0ikm, n0il
integer :: n0ilm, n0im, n0jl, n0jm, n0kl
integer :: n0klm, n0km
integer :: n1acd, n1acde, n1ae, n1be, n1cd
integer :: n1cde, n1ij, n1ijl, n1ijlm, n1ijm
integer :: n1ik, n1ikl, n1iklm, n1ikm, n1il
integer :: n1ilm, n1im, n1jl, n1jm, n1kl
integer :: n1klm, n1km
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
n0acd = max(n0a, n0c, n0d)
n0acde = max(n0a, n0c, n0d, n0e)
n0ae = max(n0a, n0e)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0cde = max(n0c, n0d, n0e)
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
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0klm = max(n0k, n0l, n0m)
n0km = max(n0k, n0m)
n1acd = min(n1a, n1c, n1d)
n1acde = min(n1a, n1c, n1d, n1e)
n1ae = min(n1a, n1e)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1cde = min(n1c, n1d, n1e)
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
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1klm = min(n1k, n1l, n1m)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjcidibi: do d = n0d, n1d
c_aibjcidibi: do c = n0c, n1c
if (c == d) cycle c_aibjcidibi
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjcidibi: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjcidibi
j_aibjcidibi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidibi: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidibi
i1 = min(j - 1, n1iklm)
i_aibjcidibi: do i = n0iklm, i1
if (i == j) cycle i_aibjcidibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcidibi(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidibi
end do a_aibjcidibi
end do j_aibjcidibi
end do b_aibjcidibi
end do c_aibjcidibi
end do d_aibjcidibi
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidibj: do d = n0d, n1d
c_aibjcidibj: do c = n0c, n1c
if (c == d) cycle c_aibjcidibj
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjcidibj: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjcidibj
j_aibjcidibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcidibj: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidibj
i1 = min(j - 1, n1ikl)
i_aibjcidibj: do i = n0ikl, i1
if (i == j) cycle i_aibjcidibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcidibj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcidibj
end do a_aibjcidibj
end do j_aibjcidibj
end do b_aibjcidibj
end do c_aibjcidibj
end do d_aibjcidibj
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k
! Equalities: e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibickdibk: do d = n0d, n1d
c_aibickdibk: do c = n0c, n1c
if (c == d) cycle c_aibickdibk
k_aibickdibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibickdibk: do b = b0, b1
if (b == c .or. b == d) cycle b_aibickdibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibickdibk: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibickdibk
i0 = max(k + 1, n0ijl)
i_aibickdibk: do i = i0, n1ijl
if (i == k) cycle i_aibickdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickdibk(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdibk
end do a_aibickdibk
end do b_aibickdibk
end do k_aibickdibk
end do c_aibickdibk
end do d_aibickdibk
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k
! Equalities: e == b, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibickdibi: do d = n0d, n1d
c_aibickdibi: do c = n0c, n1c
if (c == d) cycle c_aibickdibi
k_aibickdibi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibickdibi: do b = b0, b1
if (b == c .or. b == d) cycle b_aibickdibi
a_aibickdibi: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibickdibi
i0 = max(k + 1, n0ijlm)
i_aibickdibi: do i = i0, n1ijlm
if (i == k) cycle i_aibickdibi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickdibi(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickdibi
end do a_aibickdibi
end do b_aibickdibi
end do k_aibickdibi
end do c_aibickdibi
end do d_aibickdibi
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j, l
! Equalities: c == a, d == a, e == a, m == k
! No equalities independent of the above can hold.
!
l_aibjakalak: do l = n0l, n1l
k1 = min(l - 1, n1km)
k_aibjakalak: do k = n0km, k1
if (k == l) cycle k_aibjakalak
b_aibjakalak: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjakalak: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjakalak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakalak: do a = n0acde, a1
if (a == b) cycle a_aibjakalak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakalak: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakalak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakalak(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakalak
end do a_aibjakalak
end do j_aibjakalak
end do b_aibjakalak
end do k_aibjakalak
end do l_aibjakalak
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j, m
! Equalities: c == a, d == a, e == a, l == k
! No equalities independent of the above can hold.
!
m_aibjakakam: do m = n0m, n1m
k0 = max(m + 1, n0kl)
k_aibjakakam: do k = k0, n1kl
if (k == m) cycle k_aibjakakam
b_aibjakakam: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjakakam: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjakakam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakakam: do a = n0acde, a1
if (a == b) cycle a_aibjakakam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjakakam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakakam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakakam(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakakam
end do a_aibjakakam
end do j_aibjakakam
end do b_aibjakakam
end do k_aibjakakam
end do m_aibjakakam
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == a, e == a, m == i
! No equalities independent of the above can hold.
!
l_aibjakalai: do l = n0l, n1l
k_aibjakalai: do k = n0k, n1k
if (k == l) cycle k_aibjakalai
b_aibjakalai: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjakalai: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjakalai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakalai: do a = n0acde, a1
if (a == b) cycle a_aibjakalai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjakalai: do i = n0im, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjakalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakalai(b, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakalai
end do a_aibjakalai
end do j_aibjakalai
end do b_aibjakalai
end do k_aibjakalai
end do l_aibjakalai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: c == a, d == a, e == a, l == i
! No equalities independent of the above can hold.
!
m_aibjakaiam: do m = n0m, n1m
k_aibjakaiam: do k = n0k, n1k
if (k == m) cycle k_aibjakaiam
b_aibjakaiam: do b = n0b, n1b
j0 = max(k + 1, n0j)
j_aibjakaiam: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjakaiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakaiam: do a = n0acde, a1
if (a == b) cycle a_aibjakaiam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0il)
i_aibjakaiam: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjakaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakaiam(b, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakaiam
end do a_aibjakaiam
end do j_aibjakaiam
end do b_aibjakaiam
end do k_aibjakaiam
end do m_aibjakaiam
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == a, e == a, m == j
! No equalities independent of the above can hold.
!
l_aibjakalaj: do l = n0l, n1l
k_aibjakalaj: do k = n0k, n1k
if (k == l) cycle k_aibjakalaj
b_aibjakalaj: do b = n0b, n1b
j0 = max(k + 1, n0jm)
j1 = min(l - 1, n1jm)
j_aibjakalaj: do j = j0, j1
if (j == k .or. j == l) cycle j_aibjakalaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakalaj: do a = n0acde, a1
if (a == b) cycle a_aibjakalaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakalaj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakalaj(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakalaj
end do a_aibjakalaj
end do j_aibjakalaj
end do b_aibjakalaj
end do k_aibjakalaj
end do l_aibjakalaj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: c == a, d == a, e == a, l == j
! No equalities independent of the above can hold.
!
m_aibjakajam: do m = n0m, n1m
k_aibjakajam: do k = n0k, n1k
if (k == m) cycle k_aibjakajam
b_aibjakajam: do b = n0b, n1b
j0 = max(k + 1, m + 1, n0jl)
j_aibjakajam: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjakajam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakajam: do a = n0acde, a1
if (a == b) cycle a_aibjakajam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjakajam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakajam(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakajam
end do a_aibjakajam
end do j_aibjakajam
end do b_aibjakajam
end do k_aibjakajam
end do m_aibjakajam
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, j
! Equalities: c == a, d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjakaiek: do e = n0e, n1e
k_aibjakaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjakaiek: do b = n0b, n1b
if (b == e) cycle b_aibjakaiek
j0 = max(k + 1, n0j)
j_aibjakaiek: do j = j0, n1j
if (j == k) cycle j_aibjakaiek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibjakaiek: do a = a0, a1
if (a == b .or. a == e) cycle a_aibjakaiek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakaiek: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakaiek(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakaiek
end do a_aibjakaiek
end do j_aibjakaiek
end do b_aibjakaiek
end do k_aibjakaiek
end do e_aibjakaiek
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k, j
! Equalities: c == a, d == a, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjakakei: do e = n0e, n1e
k_aibjakakei: do k = n0kl, n1kl
b_aibjakakei: do b = n0b, n1b
if (b == e) cycle b_aibjakakei
j0 = max(k + 1, n0j)
j_aibjakakei: do j = j0, n1j
if (j == k) cycle j_aibjakakei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibjakakei: do a = a0, a1
if (a == b .or. a == e) cycle a_aibjakakei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakakei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakakei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakakei
end do a_aibjakakei
end do j_aibjakakei
end do b_aibjakakei
end do k_aibjakakei
end do e_aibjakakei
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjakajei: do e = n0e, n1e
k_aibjakajei: do k = n0k, n1k
b_aibjakajei: do b = n0b, n1b
if (b == e) cycle b_aibjakajei
j0 = max(k + 1, n0jl)
j_aibjakajei: do j = j0, n1jl
if (j == k) cycle j_aibjakajei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibjakajei: do a = a0, a1
if (a == b .or. a == e) cycle a_aibjakajei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakajei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakajei
end do a_aibjakajei
end do j_aibjakajei
end do b_aibjakajei
end do k_aibjakajei
end do e_aibjakajei
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakaiej: do e = n0e, n1e
k_aibjakaiej: do k = n0k, n1k
b_aibjakaiej: do b = n0b, n1b
if (b == e) cycle b_aibjakaiej
j0 = max(k + 1, n0jm)
j_aibjakaiej: do j = j0, n1jm
if (j == k) cycle j_aibjakaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibjakaiej: do a = a0, a1
if (a == b .or. a == e) cycle a_aibjakaiej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakaiej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakaiej(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakaiej
end do a_aibjakaiej
end do j_aibjakaiej
end do b_aibjakaiej
end do k_aibjakaiej
end do e_aibjakaiej
!
! Elementary loop  15
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k, j
! Equalities: d == c, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckcick: do c = n0cde, n1cde
k_aibjckcick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcick: do b = b0, n1b
if (b == c) cycle b_aibjckcick
j0 = max(k + 1, n0j)
j_aibjckcick: do j = j0, n1j
if (j == k) cycle j_aibjckcick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcick: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckcick
i0 = max(k + 1, n0il)
i_aibjckcick: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckcick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckcick(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcick
end do a_aibjckcick
end do j_aibjckcick
end do b_aibjckcick
end do k_aibjckcick
end do c_aibjckcick
!
! Elementary loop  16
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k, j
! Equalities: d == c, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckckci: do c = n0cde, n1cde
k_aibjckckci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckci: do b = b0, n1b
if (b == c) cycle b_aibjckckci
j0 = max(k + 1, n0j)
j_aibjckckci: do j = j0, n1j
if (j == k) cycle j_aibjckckci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckckci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckckci
i1 = min(k - 1, n1im)
i_aibjckckci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckckci(a, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckckci
end do a_aibjckckci
end do j_aibjckckci
end do b_aibjckckci
end do k_aibjckckci
end do c_aibjckckci
!
! Elementary loop  17
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckcjci: do c = n0cde, n1cde
k_aibjckcjci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjci: do b = b0, n1b
if (b == c) cycle b_aibjckcjci
j0 = max(k + 1, n0jl)
j_aibjckcjci: do j = j0, n1jl
if (j == k) cycle j_aibjckcjci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcjci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckcjci
i1 = min(j - 1, n1im)
i_aibjckcjci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjckcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckcjci(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcjci
end do a_aibjckcjci
end do j_aibjckcjci
end do b_aibjckcjci
end do k_aibjckcjci
end do c_aibjckcjci
!
! Elementary loop  18
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckcicj: do c = n0cde, n1cde
k_aibjckcicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcicj: do b = b0, n1b
if (b == c) cycle b_aibjckcicj
j0 = max(k + 1, n0jm)
j_aibjckcicj: do j = j0, n1jm
if (j == k) cycle j_aibjckcicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcicj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckcicj
i0 = max(j + 1, n0il)
i_aibjckcicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckcicj(a, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcicj
end do a_aibjckcicj
end do j_aibjckcicj
end do b_aibjckcicj
end do k_aibjckcicj
end do c_aibjckcicj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: e == a, d == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjciclai: do l = n0l, n1l
c_aibjciclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjciclai: do b = b0, n1b
if (b == c) cycle b_aibjciclai
j_aibjciclai: do j = n0j, n1j
if (j == l) cycle j_aibjciclai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjciclai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjciclai
i1 = min(j - 1, n1ikm)
i_aibjciclai: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjciclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciclai(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciclai
end do a_aibjciclai
end do j_aibjciclai
end do b_aibjciclai
end do c_aibjciclai
end do l_aibjciclai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: e == a, d == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjciciam: do m = n0m, n1m
c_aibjciciam: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjciciam: do b = b0, n1b
if (b == c) cycle b_aibjciciam
j_aibjciciam: do j = n0j, n1j
if (j == m) cycle j_aibjciciam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjciciam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjciciam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aibjciciam: do i = n0ikl, i1
if (i == j .or. i == m) cycle i_aibjciciam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciciam(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciciam
end do a_aibjciciam
end do j_aibjciciam
end do b_aibjciciam
end do c_aibjciciam
end do m_aibjciciam
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: e == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcicjam: do m = n0m, n1m
c_aibjcicjam: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcicjam: do b = b0, n1b
if (b == c) cycle b_aibjcicjam
j_aibjcicjam: do j = n0jl, n1jl
if (j == m) cycle j_aibjcicjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjcicjam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcicjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjcicjam: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjcicjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcicjam(i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcicjam
end do a_aibjcicjam
end do j_aibjcicjam
end do b_aibjcicjam
end do c_aibjcicjam
end do m_aibjcicjam
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j
! Equalities: e == a, d == c, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibjckckak: do c = n0cd, n1cd
k_aibjckckak: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckak: do b = b0, n1b
if (b == c) cycle b_aibjckckak
j0 = max(k + 1, n0j)
j_aibjckckak: do j = j0, n1j
if (j == k) cycle j_aibjckckak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckckak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckckak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckckak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckckak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckckak(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckckak
end do a_aibjckckak
end do j_aibjckckak
end do b_aibjckckak
end do k_aibjckckak
end do c_aibjckckak
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: e == a, d == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckcjak: do c = n0cd, n1cd
k_aibjckcjak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjak: do b = b0, n1b
if (b == c) cycle b_aibjckcjak
j0 = max(k + 1, n0jl)
j_aibjckcjak: do j = j0, n1jl
if (j == k) cycle j_aibjckcjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckcjak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckcjak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckcjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckcjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckcjak(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcjak
end do a_aibjckcjak
end do j_aibjckcjak
end do b_aibjckcjak
end do k_aibjckcjak
end do c_aibjckcjak
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, j
! Equalities: e == a, d == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckckai: do c = n0cd, n1cd
k_aibjckckai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckai: do b = b0, n1b
if (b == c) cycle b_aibjckckai
j0 = max(k + 1, n0j)
j_aibjckckai: do j = j0, n1j
if (j == k) cycle j_aibjckckai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckckai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckckai
i_aibjckckai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckckai(a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckckai
end do a_aibjckckai
end do j_aibjckckai
end do b_aibjckckai
end do k_aibjckckai
end do c_aibjckckai
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, m
! Equalities: e == a, d == c, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibickckam: do m = n0m, n1m
c_aibickckam: do c = n0cd, n1cd
k_aibickckam: do k = n0kl, n1kl
if (k == m) cycle k_aibickckam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickckam: do b = b0, n1b
if (b == c) cycle b_aibickckam
a1 = min(c - 1, n1ae)
a_aibickckam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickckam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickckam: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aibickckam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickckam(i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickckam
end do a_aibickckam
end do b_aibickckam
end do k_aibickckam
end do c_aibickckam
end do m_aibickckam
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: e == a, d == c, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckckaj: do c = n0cd, n1cd
k_aibjckckaj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckaj: do b = b0, n1b
if (b == c) cycle b_aibjckckaj
j0 = max(k + 1, n0jm)
j_aibjckckaj: do j = j0, n1jm
if (j == k) cycle j_aibjckckaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckckaj: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckckaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckckaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckckaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckckaj(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckckaj
end do a_aibjckckaj
end do j_aibjckckaj
end do b_aibjckckaj
end do k_aibjckckaj
end do c_aibjckckaj
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, l
! Equalities: e == a, d == c, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibickclai: do l = n0l, n1l
c_aibickclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aibickclai: do k = n0k, n1k
if (k == l) cycle k_aibickclai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickclai: do b = b0, n1b
if (b == c) cycle b_aibickclai
a1 = min(c - 1, n1ae)
a_aibickclai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickclai
i0 = max(k + 1, n0ijm)
i_aibickclai: do i = i0, n1ijm
if (i == k .or. i == l) cycle i_aibickclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickclai(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickclai
end do a_aibickclai
end do b_aibickclai
end do k_aibickclai
end do c_aibickclai
end do l_aibickclai
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: e == a, d == c, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjckciai: do c = n0cd, n1cd
k_aibjckciai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckciai: do b = b0, n1b
if (b == c) cycle b_aibjckciai
j0 = max(k + 1, n0j)
j_aibjckciai: do j = j0, n1j
if (j == k) cycle j_aibjckciai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckciai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckciai
i_aibjckciai: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjckciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckciai(i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckciai
end do a_aibjckciai
end do j_aibjckciai
end do b_aibjckciai
end do k_aibjckciai
end do c_aibjckciai
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: e == a, d == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjckcjai: do c = n0cd, n1cd
k_aibjckcjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjai: do b = b0, n1b
if (b == c) cycle b_aibjckcjai
j0 = max(k + 1, n0jl)
j_aibjckcjai: do j = j0, n1jl
if (j == k) cycle j_aibjckcjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckcjai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckcjai
i_aibjckcjai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckcjai(a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcjai
end do a_aibjckcjai
end do j_aibjckcjai
end do b_aibjckcjai
end do k_aibjckcjai
end do c_aibjckcjai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k, m
! Equalities: e == a, d == c, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibickciam: do m = n0m, n1m
c_aibickciam: do c = n0cd, n1cd
k_aibickciam: do k = n0k, n1k
if (k == m) cycle k_aibickciam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickciam: do b = b0, n1b
if (b == c) cycle b_aibickciam
a1 = min(c - 1, n1ae)
a_aibickciam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickciam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibickciam: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibickciam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickciam(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickciam
end do a_aibickciam
end do b_aibickciam
end do k_aibickciam
end do c_aibickciam
end do m_aibickciam
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
end subroutine ccjac_32_tripletm_dav_part4
end module ccjac_block_32_tripletm_dav_part4
