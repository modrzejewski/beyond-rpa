module ccjac_block_32_tripletm_dav_part1
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
subroutine ccjac_32_tripletm_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0abde, n0ad, n0ae, n0bd, n0be
integer :: n0cd, n0ce, n0il, n0im, n0jl
integer :: n0jm, n0kl, n0km
integer :: n1abde, n1ad, n1ae, n1bd, n1be
integer :: n1cd, n1ce, n1il, n1im, n1jl
integer :: n1jm, n1kl, n1km
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
n0abde = max(n0a, n0b, n0d, n0e)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0ce = max(n0c, n0e)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1abde = min(n1a, n1b, n1d, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1ce = min(n1c, n1e)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j, m
! Equalities: e == a, d == b, l == k
! No equalities independent of the above can hold.
!
m_aibjckbkam: do m = n0m, n1m
c_aibjckbkam: do c = n0c, n1c
k_aibjckbkam: do k = n0kl, n1kl
if (k == m) cycle k_aibjckbkam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbkam: do b = b0, n1bd
if (b == c) cycle b_aibjckbkam
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckbkam: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjckbkam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbkam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbkam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjckbkam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckbkam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbkam(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbkam
end do a_aibjckbkam
end do j_aibjckbkam
end do b_aibjckbkam
end do k_aibjckbkam
end do c_aibjckbkam
end do m_aibjckbkam
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, l
! Equalities: e == a, d == b, m == i
! No equalities independent of the above can hold.
!
l_aibjckblai: do l = n0l, n1l
c_aibjckblai: do c = n0c, n1c
k_aibjckblai: do k = n0k, n1k
if (k == l) cycle k_aibjckblai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckblai: do b = b0, n1bd
if (b == c) cycle b_aibjckblai
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckblai: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjckblai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckblai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckblai
i_aibjckblai: do i = n0im, n1im
if (i == j .or. i == k .or. i == l) cycle i_aibjckblai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckblai(j, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckblai
end do a_aibjckblai
end do j_aibjckblai
end do b_aibjckblai
end do k_aibjckblai
end do c_aibjckblai
end do l_aibjckblai
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, m
! Equalities: e == a, d == b, l == j
! No equalities independent of the above can hold.
!
m_aibjckbjam: do m = n0m, n1m
c_aibjckbjam: do c = n0c, n1c
k_aibjckbjam: do k = n0k, n1k
if (k == m) cycle k_aibjckbjam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjam: do b = b0, n1bd
if (b == c) cycle b_aibjckbjam
j0 = max(k + 1, n0jl)
j_aibjckbjam: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjckbjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbjam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjckbjam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckbjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbjam(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjam
end do a_aibjckbjam
end do j_aibjckbjam
end do b_aibjckbjam
end do k_aibjckbjam
end do c_aibjckbjam
end do m_aibjckbjam
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, k, j
! Equalities: d == b, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjckbkei: do e = n0e, n1e
c_aibjckbkei: do c = n0c, n1c
if (c == e) cycle c_aibjckbkei
k_aibjckbkei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibjckbkei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbkei
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckbkei: do j = j0, n1j
if (j == k) cycle j_aibjckbkei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbkei
i_aibjckbkei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbkei(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbkei
end do a_aibjckbkei
end do j_aibjckbkei
end do b_aibjckbkei
end do k_aibjckbkei
end do c_aibjckbkei
end do e_aibjckbkei
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, k
! Equalities: d == b, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckbjei: do e = n0e, n1e
c_aibjckbjei: do c = n0c, n1c
if (c == e) cycle c_aibjckbjei
k_aibjckbjei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibjckbjei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbjei
j0 = max(k + 1, n0jl)
j_aibjckbjei: do j = j0, n1jl
if (j == k) cycle j_aibjckbjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbjei
i_aibjckbjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckbjei(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjei
end do a_aibjckbjei
end do j_aibjckbjei
end do b_aibjckbjei
end do k_aibjckbjei
end do c_aibjckbjei
end do e_aibjckbjei
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j, l
! Equalities: d == a, e == b, m == k
! No equalities independent of the above can hold.
!
l_aibjckalbk: do l = n0l, n1l
c_aibjckalbk: do c = n0c, n1c
k_aibjckalbk: do k = n0km, n1km
if (k == l) cycle k_aibjckalbk
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckalbk: do b = b0, n1be
if (b == c) cycle b_aibjckalbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckalbk: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjckalbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckalbk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalbk
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbk: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckalbk(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckalbk
end do a_aibjckalbk
end do j_aibjckalbk
end do b_aibjckalbk
end do k_aibjckalbk
end do c_aibjckalbk
end do l_aibjckalbk
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, m
! Equalities: d == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjckaibm: do m = n0m, n1m
c_aibjckaibm: do c = n0c, n1c
k_aibjckaibm: do k = n0k, n1k
if (k == m) cycle k_aibjckaibm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckaibm: do b = b0, n1be
if (b == c) cycle b_aibjckaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckaibm: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjckaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibm
i_aibjckaibm: do i = n0il, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjckaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaibm(j, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaibm
end do a_aibjckaibm
end do j_aibjckaibm
end do b_aibjckaibm
end do k_aibjckaibm
end do c_aibjckaibm
end do m_aibjckaibm
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, l
! Equalities: d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjckalbj: do l = n0l, n1l
c_aibjckalbj: do c = n0c, n1c
k_aibjckalbj: do k = n0k, n1k
if (k == l) cycle k_aibjckalbj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckalbj: do b = b0, n1be
if (b == c) cycle b_aibjckalbj
j0 = max(k + 1, n0jm)
j_aibjckalbj: do j = j0, n1jm
if (j == k .or. j == l) cycle j_aibjckalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckalbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalbj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckalbj(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckalbj
end do a_aibjckalbj
end do j_aibjckalbj
end do b_aibjckalbj
end do k_aibjckalbj
end do c_aibjckalbj
end do l_aibjckalbj
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, k, j
! Equalities: e == b, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjckdibk: do d = n0d, n1d
c_aibjckdibk: do c = n0c, n1c
if (c == d) cycle c_aibjckdibk
k_aibjckdibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjckdibk: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjckdibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckdibk: do j = j0, n1j
if (j == k) cycle j_aibjckdibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdibk: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdibk
i_aibjckdibk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckdibk(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdibk
end do a_aibjckdibk
end do j_aibjckdibk
end do b_aibjckdibk
end do k_aibjckdibk
end do c_aibjckdibk
end do d_aibjckdibk
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, k
! Equalities: e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdibj: do d = n0d, n1d
c_aibjckdibj: do c = n0c, n1c
if (c == d) cycle c_aibjckdibj
k_aibjckdibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjckdibj: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjckdibj
j0 = max(k + 1, n0jm)
j_aibjckdibj: do j = j0, n1jm
if (j == k) cycle j_aibjckdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdibj: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdibj
i_aibjckdibj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckdibj(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdibj
end do a_aibjckdibj
end do j_aibjckdibj
end do b_aibjckdibj
end do k_aibjckdibj
end do c_aibjckdibj
end do d_aibjckdibj
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j, m
! Equalities: e == a, d == c, l == k
! No equalities independent of the above can hold.
!
m_aibjckckam: do m = n0m, n1m
c_aibjckckam: do c = n0cd, n1cd
k_aibjckckam: do k = n0kl, n1kl
if (k == m) cycle k_aibjckckam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckam: do b = b0, n1b
if (b == c) cycle b_aibjckckam
j0 = max(k + 1, n0j)
j_aibjckckam: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjckckam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckckam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckckam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjckckam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckckam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckckam(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckckam
end do a_aibjckckam
end do j_aibjckckam
end do b_aibjckckam
end do k_aibjckckam
end do c_aibjckckam
end do m_aibjckckam
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k, l
! Equalities: e == a, d == c, m == i
! No equalities independent of the above can hold.
!
l_aibjckclai: do l = n0l, n1l
c_aibjckclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aibjckclai: do k = n0k, n1k
if (k == l) cycle k_aibjckclai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckclai: do b = b0, n1b
if (b == c) cycle b_aibjckclai
j0 = max(k + 1, n0j)
j_aibjckclai: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjckclai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckclai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckclai
i_aibjckclai: do i = n0im, n1im
if (i == j .or. i == k .or. i == l) cycle i_aibjckclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckclai(b, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckclai
end do a_aibjckclai
end do j_aibjckclai
end do b_aibjckclai
end do k_aibjckclai
end do c_aibjckclai
end do l_aibjckclai
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k, m
! Equalities: e == a, d == c, l == j
! No equalities independent of the above can hold.
!
m_aibjckcjam: do m = n0m, n1m
c_aibjckcjam: do c = n0cd, n1cd
k_aibjckcjam: do k = n0k, n1k
if (k == m) cycle k_aibjckcjam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjam: do b = b0, n1b
if (b == c) cycle b_aibjckcjam
j0 = max(k + 1, n0jl)
j_aibjckcjam: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjckcjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckcjam: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckcjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjckcjam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckcjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckcjam(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcjam
end do a_aibjckcjam
end do j_aibjckcjam
end do b_aibjckcjam
end do k_aibjckcjam
end do c_aibjckcjam
end do m_aibjckcjam
!
! Elementary loop  14
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, k, j
! Equalities: d == c, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibjckckei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjckckei: do c = c0, n1cd
if (c == e) cycle c_aibjckckei
k_aibjckckei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckckei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckckei
j0 = max(k + 1, n0j)
j_aibjckckei: do j = j0, n1j
if (j == k) cycle j_aibjckckei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckckei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckckei
i_aibjckckei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckckei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckckei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckckei
end do a_aibjckckei
end do j_aibjckckei
end do b_aibjckckei
end do k_aibjckckei
end do c_aibjckckei
end do e_aibjckckei
!
! Elementary loop  15
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j, k
! Equalities: d == c, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjckcjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjckcjei: do c = c0, n1cd
if (c == e) cycle c_aibjckcjei
k_aibjckcjei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckcjei
j0 = max(k + 1, n0jl)
j_aibjckcjei: do j = j0, n1jl
if (j == k) cycle j_aibjckcjei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcjei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckcjei
i_aibjckcjei: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckcjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckcjei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcjei
end do a_aibjckcjei
end do j_aibjckcjei
end do b_aibjckcjei
end do k_aibjckcjei
end do c_aibjckcjei
end do e_aibjckcjei
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: k, i, j, l
! Equalities: d == a, e == c, m == k
! No equalities independent of the above can hold.
!
l_aibjckalck: do l = n0l, n1l
c_aibjckalck: do c = n0ce, n1ce
k_aibjckalck: do k = n0km, n1km
if (k == l) cycle k_aibjckalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckalck: do b = b0, n1b
if (b == c) cycle b_aibjckalck
j0 = max(k + 1, n0j)
j_aibjckalck: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjckalck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckalck: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalck
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalck: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckalck(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckalck
end do a_aibjckalck
end do j_aibjckalck
end do b_aibjckalck
end do k_aibjckalck
end do c_aibjckalck
end do l_aibjckalck
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k, m
! Equalities: d == a, e == c, l == i
! No equalities independent of the above can hold.
!
m_aibjckaicm: do m = n0m, n1m
c_aibjckaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckaicm: do k = n0k, n1k
if (k == m) cycle k_aibjckaicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaicm: do b = b0, n1b
if (b == c) cycle b_aibjckaicm
j0 = max(k + 1, n0j)
j_aibjckaicm: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjckaicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckaicm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaicm
i_aibjckaicm: do i = n0il, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjckaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaicm(b, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaicm
end do a_aibjckaicm
end do j_aibjckaicm
end do b_aibjckaicm
end do k_aibjckaicm
end do c_aibjckaicm
end do m_aibjckaicm
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, i, k, l
! Equalities: d == a, e == c, m == j
! No equalities independent of the above can hold.
!
l_aibjckalcj: do l = n0l, n1l
c_aibjckalcj: do c = n0ce, n1ce
k_aibjckalcj: do k = n0k, n1k
if (k == l) cycle k_aibjckalcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckalcj: do b = b0, n1b
if (b == c) cycle b_aibjckalcj
j0 = max(k + 1, n0jm)
j_aibjckalcj: do j = j0, n1jm
if (j == k .or. j == l) cycle j_aibjckalcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckalcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalcj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckalcj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckalcj(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckalcj
end do a_aibjckalcj
end do j_aibjckalcj
end do b_aibjckalcj
end do k_aibjckalcj
end do c_aibjckalcj
end do l_aibjckalcj
!
! Elementary loop  19
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, k, j
! Equalities: e == c, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibjckdick: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjckdick: do c = n0ce, c1
if (c == d) cycle c_aibjckdick
k_aibjckdick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdick: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdick
j0 = max(k + 1, n0j)
j_aibjckdick: do j = j0, n1j
if (j == k) cycle j_aibjckdick
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdick: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdick
i_aibjckdick: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckdick(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdick
end do a_aibjckdick
end do j_aibjckdick
end do b_aibjckdick
end do k_aibjckdick
end do c_aibjckdick
end do d_aibjckdick
!
! Elementary loop  20
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: i, j, k
! Equalities: e == c, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdicj: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjckdicj: do c = n0ce, c1
if (c == d) cycle c_aibjckdicj
k_aibjckdicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdicj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdicj
j0 = max(k + 1, n0jm)
j_aibjckdicj: do j = j0, n1jm
if (j == k) cycle j_aibjckdicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdicj: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdicj
i_aibjckdicj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckdicj(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdicj
end do a_aibjckdicj
end do j_aibjckdicj
end do b_aibjckdicj
end do k_aibjckdicj
end do c_aibjckdicj
end do d_aibjckdicj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, k, j
! Equalities: d == a, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibjckaiek: do e = n0e, n1e
c_aibjckaiek: do c = n0c, n1c
if (c == e) cycle c_aibjckaiek
k_aibjckaiek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaiek: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckaiek
j0 = max(k + 1, n0j)
j_aibjckaiek: do j = j0, n1j
if (j == k) cycle j_aibjckaiek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0ad)
a_aibjckaiek: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjckaiek
i_aibjckaiek: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaiek(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaiek
end do a_aibjckaiek
end do j_aibjckaiek
end do b_aibjckaiek
end do k_aibjckaiek
end do c_aibjckaiek
end do e_aibjckaiek
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, k
! Equalities: d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckaiej: do e = n0e, n1e
c_aibjckaiej: do c = n0c, n1c
if (c == e) cycle c_aibjckaiej
k_aibjckaiej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaiej: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckaiej
j0 = max(k + 1, n0jm)
j_aibjckaiej: do j = j0, n1jm
if (j == k) cycle j_aibjckaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0ad)
a_aibjckaiej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjckaiej
i_aibjckaiej: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckaiej(b, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckaiej
end do a_aibjckaiej
end do j_aibjckaiej
end do b_aibjckaiej
end do k_aibjckaiej
end do c_aibjckaiej
end do e_aibjckaiej
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k, j
! Equalities: e == a, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibjckdkai: do d = n0d, n1d
c_aibjckdkai: do c = n0c, n1c
if (c == d) cycle c_aibjckdkai
k_aibjckdkai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdkai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdkai
j0 = max(k + 1, n0j)
j_aibjckdkai: do j = j0, n1j
if (j == k) cycle j_aibjckdkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1ae)
a_aibjckdkai: do a = n0ae, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdkai
i_aibjckdkai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckdkai(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdkai
end do a_aibjckdkai
end do j_aibjckdkai
end do b_aibjckdkai
end do k_aibjckdkai
end do c_aibjckdkai
end do d_aibjckdkai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: e == a, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjckdjai: do d = n0d, n1d
c_aibjckdjai: do c = n0c, n1c
if (c == d) cycle c_aibjckdjai
k_aibjckdjai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdjai: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdjai
j0 = max(k + 1, n0jl)
j_aibjckdjai: do j = j0, n1jl
if (j == k) cycle j_aibjckdjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1ae)
a_aibjckdjai: do a = n0ae, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjai
i_aibjckdjai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjckdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v9_eom_cc3_32_tripletm_trans_aibjckdjai(b, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdjai
end do a_aibjckdjai
end do j_aibjckdjai
end do b_aibjckdjai
end do k_aibjckdjai
end do c_aibjckdjai
end do d_aibjckdjai
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, l
! Equalities: b == a, d == a, e == a, m == k
! No equalities independent of the above can hold.
!
l_aiajckalak: do l = n0l, n1l
c_aiajckalak: do c = n0c, n1c
k1 = min(l - 1, n1km)
k_aiajckalak: do k = n0km, k1
if (k == l) cycle k_aiajckalak
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckalak: do j = j0, n1j
if (j == k .or. j == l) cycle j_aiajckalak
a0 = max(c + 1, n0abde)
a_aiajckalak: do a = a0, n1abde
if (a == c) cycle a_aiajckalak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckalak: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckalak(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckalak
end do a_aiajckalak
end do j_aiajckalak
end do k_aiajckalak
end do c_aiajckalak
end do l_aiajckalak
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, m
! Equalities: b == a, d == a, e == a, l == k
! No equalities independent of the above can hold.
!
m_aiajckakam: do m = n0m, n1m
c_aiajckakam: do c = n0c, n1c
k0 = max(m + 1, n0kl)
k_aiajckakam: do k = k0, n1kl
if (k == m) cycle k_aiajckakam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckakam: do j = j0, n1j
if (j == k .or. j == m) cycle j_aiajckakam
a0 = max(c + 1, n0abde)
a_aiajckakam: do a = a0, n1abde
if (a == c) cycle a_aiajckakam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajckakam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajckakam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckakam(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckakam
end do a_aiajckakam
end do j_aiajckakam
end do k_aiajckakam
end do c_aiajckakam
end do m_aiajckakam
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == a, e == a, m == i
! No equalities independent of the above can hold.
!
l_aiajckalai: do l = n0l, n1l
c_aiajckalai: do c = n0c, n1c
k_aiajckalai: do k = n0k, n1k
if (k == l) cycle k_aiajckalai
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckalai: do j = j0, n1j
if (j == k .or. j == l) cycle j_aiajckalai
a0 = max(c + 1, n0abde)
a_aiajckalai: do a = a0, n1abde
if (a == c) cycle a_aiajckalai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aiajckalai: do i = n0im, i1
if (i == j .or. i == k .or. i == l) cycle i_aiajckalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckalai(j, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckalai
end do a_aiajckalai
end do j_aiajckalai
end do k_aiajckalai
end do c_aiajckalai
end do l_aiajckalai
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, m
! Equalities: b == a, d == a, e == a, l == i
! No equalities independent of the above can hold.
!
m_aiajckaiam: do m = n0m, n1m
c_aiajckaiam: do c = n0c, n1c
k_aiajckaiam: do k = n0k, n1k
if (k == m) cycle k_aiajckaiam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckaiam: do j = j0, n1j
if (j == k .or. j == m) cycle j_aiajckaiam
a0 = max(c + 1, n0abde)
a_aiajckaiam: do a = a0, n1abde
if (a == c) cycle a_aiajckaiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0il)
i_aiajckaiam: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aiajckaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckaiam(j, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaiam
end do a_aiajckaiam
end do j_aiajckaiam
end do k_aiajckaiam
end do c_aiajckaiam
end do m_aiajckaiam
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, l
! Equalities: b == a, d == a, e == a, m == j
! No equalities independent of the above can hold.
!
l_aiajckalaj: do l = n0l, n1l
c_aiajckalaj: do c = n0c, n1c
k_aiajckalaj: do k = n0k, n1k
if (k == l) cycle k_aiajckalaj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j1 = min(l - 1, n1jm)
j_aiajckalaj: do j = j0, j1
if (j == k .or. j == l) cycle j_aiajckalaj
a0 = max(c + 1, n0abde)
a_aiajckalaj: do a = a0, n1abde
if (a == c) cycle a_aiajckalaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckalaj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckalaj(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckalaj
end do a_aiajckalaj
end do j_aiajckalaj
end do k_aiajckalaj
end do c_aiajckalaj
end do l_aiajckalaj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, m
! Equalities: b == a, d == a, e == a, l == j
! No equalities independent of the above can hold.
!
m_aiajckajam: do m = n0m, n1m
c_aiajckajam: do c = n0c, n1c
k_aiajckajam: do k = n0k, n1k
if (k == m) cycle k_aiajckajam
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, m + 1, n0jl)
j_aiajckajam: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aiajckajam
a0 = max(c + 1, n0abde)
a_aiajckajam: do a = a0, n1abde
if (a == c) cycle a_aiajckajam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aiajckajam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajckajam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckajam(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajam
end do a_aiajckajam
end do j_aiajckajam
end do k_aiajckajam
end do c_aiajckajam
end do m_aiajckajam
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
end subroutine ccjac_32_tripletm_dav_part1
end module ccjac_block_32_tripletm_dav_part1
