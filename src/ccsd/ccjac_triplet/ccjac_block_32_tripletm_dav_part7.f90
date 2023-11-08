module ccjac_block_32_tripletm_dav_part7
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
subroutine ccjac_32_tripletm_dav_part7(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a, b, c, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, a1, b0, i0, i1, j0
integer :: n0abd, n0ac, n0ace, n0bd, n0bde
integer :: n0ce, n0ij, n0ijl, n0ijlm, n0ijm
integer :: n0ik, n0ikl, n0iklm, n0ikm, n0il
integer :: n0ilm, n0im, n0jl, n0jlm, n0jm
integer :: n0kl, n0klm, n0km
integer :: n1abd, n1ac, n1ace, n1bd, n1bde
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
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0ace = max(n0a, n0c, n0e)
n0bd = max(n0b, n0d)
n0bde = max(n0b, n0d, n0e)
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
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1ace = min(n1a, n1c, n1e)
n1bd = min(n1b, n1d)
n1bde = min(n1b, n1d, n1e)
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
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, e == c, j == i, m == i
! No equalities independent of the above can hold.
!
l_aiaickalci: do l = n0l, n1l
c_aiaickalci: do c = n0ce, n1ce
k_aiaickalci: do k = n0k, n1k
if (k == l) cycle k_aiaickalci
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickalci: do a = a0, n1abd
if (a == c) cycle a_aiaickalci
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aiaickalci: do i = i0, n1ijm
if (i == k .or. i == l) cycle i_aiaickalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickalci(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickalci
end do a_aiaickalci
end do k_aiaickalci
end do c_aiaickalci
end do l_aiaickalci
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, l == i, m == i
! No equalities independent of the above can hold.
!
c_aiajckaici: do c = n0ce, n1ce
k_aiajckaici: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckaici: do j = j0, n1j
if (j == k) cycle j_aiajckaici
a0 = max(c + 1, n0abd)
a_aiajckaici: do a = a0, n1abd
if (a == c) cycle a_aiajckaici
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaici: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aiajckaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckaici(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaici
end do a_aiajckaici
end do j_aiajckaici
end do k_aiajckaici
end do c_aiajckaici
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajckajci: do c = n0ce, n1ce
k_aiajckajci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckajci: do j = j0, n1jl
if (j == k) cycle j_aiajckajci
a0 = max(c + 1, n0abd)
a_aiajckajci: do a = a0, n1abd
if (a == c) cycle a_aiajckajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajci: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aiajckajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckajci(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajci
end do a_aiajckajci
end do j_aiajckajci
end do k_aiajckajci
end do c_aiajckajci
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, m
! Equalities: b == a, d == a, e == c, j == i, l == i
! No equalities independent of the above can hold.
!
m_aiaickaicm: do m = n0m, n1m
c_aiaickaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiaickaicm: do k = n0k, n1k
if (k == m) cycle k_aiaickaicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickaicm: do a = a0, n1abd
if (a == c) cycle a_aiaickaicm
i0 = max(k + 1, n0ijl)
i_aiaickaicm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aiaickaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickaicm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaicm
end do a_aiaickaicm
end do k_aiaickaicm
end do c_aiaickaicm
end do m_aiaickaicm
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckaicj: do c = n0ce, n1ce
k_aiajckaicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aiajckaicj: do j = j0, n1jm
if (j == k) cycle j_aiajckaicj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckaicj: do a = a0, n1abd
if (a == c) cycle a_aiajckaicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckaicj: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aiajckaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckaicj(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaicj
end do a_aiajckaicj
end do j_aiajckaicj
end do k_aiajckaicj
end do c_aiajckaicj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, e == c, l == j, m == j
! No equalities independent of the above can hold.
!
c_aiajckajcj: do c = n0ce, n1ce
k_aiajckajcj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jlm)
j_aiajckajcj: do j = j0, n1jlm
if (j == k) cycle j_aiajckajcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckajcj: do a = a0, n1abd
if (a == c) cycle a_aiajckajcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v1_eom_cc3_32_tripletm_trans_aiajckajcj(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajcj
end do a_aiajckajcj
end do j_aiajckajcj
end do k_aiajckajcj
end do c_aiajckajcj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiajciaiei: do e = n0e, n1e
c_aiajciaiei: do c = n0c, n1c
if (c == e) cycle c_aiajciaiei
j_aiajciaiei: do j = n0j, n1j
a0 = max(c + 1, e + 1, n0abd)
a_aiajciaiei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajciaiei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1iklm)
i_aiajciaiei: do i = n0iklm, i1
if (i == j) cycle i_aiajciaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciaiei(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciaiei
end do a_aiajciaiei
end do j_aiajciaiei
end do c_aiajciaiei
end do e_aiajciaiei
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajciajei: do e = n0e, n1e
c_aiajciajei: do c = n0c, n1c
if (c == e) cycle c_aiajciajei
j_aiajciajei: do j = n0jl, n1jl
a0 = max(c + 1, e + 1, n0abd)
a_aiajciajei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajciajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajciajei: do i = n0ikm, i1
if (i == j) cycle i_aiajciajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciajei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciajei
end do a_aiajciajei
end do j_aiajciajei
end do c_aiajciajei
end do e_aiajciajei
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajciaiej: do e = n0e, n1e
c_aiajciaiej: do c = n0c, n1c
if (c == e) cycle c_aiajciaiej
j_aiajciaiej: do j = n0jm, n1jm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiajciaiej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajciaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aiajciaiej: do i = n0ikl, i1
if (i == j) cycle i_aiajciaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v6_eom_cc3_32_tripletm_trans_aiajciaiej(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciaiej
end do a_aiajciaiej
end do j_aiajciaiej
end do c_aiajciaiej
end do e_aiajciaiej
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiaickaiek: do e = n0e, n1e
c_aiaickaiek: do c = n0c, n1c
if (c == e) cycle c_aiaickaiek
k_aiaickaiek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiaickaiek: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiaickaiek
i0 = max(k + 1, n0ijl)
i_aiaickaiek: do i = i0, n1ijl
if (i == k) cycle i_aiaickaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickaiek(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaiek
end do a_aiaickaiek
end do k_aiaickaiek
end do c_aiaickaiek
end do e_aiaickaiek
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aiaickakei: do e = n0e, n1e
c_aiaickakei: do c = n0c, n1c
if (c == e) cycle c_aiaickakei
k_aiaickakei: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiaickakei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiaickakei
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aiaickakei: do i = i0, n1ijm
if (i == k) cycle i_aiaickakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickakei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickakei
end do a_aiaickakei
end do k_aiaickakei
end do c_aiaickakei
end do e_aiaickakei
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aiaickaiei: do e = n0e, n1e
c_aiaickaiei: do c = n0c, n1c
if (c == e) cycle c_aiaickaiei
k_aiaickaiei: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiaickaiei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiaickaiei
i0 = max(k + 1, n0ijlm)
i_aiaickaiei: do i = i0, n1ijlm
if (i == k) cycle i_aiaickaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v5_eom_cc3_32_tripletm_trans_aiaickaiei(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaiei
end do a_aiaickaiei
end do k_aiaickaiei
end do c_aiaickaiei
end do e_aiaickaiei
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjakbibk: do k = n0km, n1km
b_aibjakbibk: do b = n0bde, n1bde
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbibk: do j = j0, n1j
if (j == k) cycle j_aibjakbibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbibk: do a = n0ac, a1
if (a == b) cycle a_aibjakbibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0il)
i_aibjakbibk: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbibk(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbibk
end do a_aibjakbibk
end do j_aibjakbibk
end do b_aibjakbibk
end do k_aibjakbibk
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjakbkbi: do k = n0kl, n1kl
b_aibjakbkbi: do b = n0bde, n1bde
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbkbi: do j = j0, n1j
if (j == k) cycle j_aibjakbkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbkbi: do a = n0ac, a1
if (a == b) cycle a_aibjakbkbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aibjakbkbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbkbi(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbkbi
end do a_aibjakbkbi
end do j_aibjakbkbi
end do b_aibjakbkbi
end do k_aibjakbkbi
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjakbjbi: do k = n0k, n1k
b_aibjakbjbi: do b = n0bde, n1bde
j0 = max(k + 1, n0jl)
j_aibjakbjbi: do j = j0, n1jl
if (j == k) cycle j_aibjakbjbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbjbi: do a = n0ac, a1
if (a == b) cycle a_aibjakbjbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjakbjbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakbjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbjbi(a, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjbi
end do a_aibjakbjbi
end do j_aibjakbjbi
end do b_aibjakbjbi
end do k_aibjakbjbi
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakbibj: do k = n0k, n1k
b_aibjakbibj: do b = n0bde, n1bde
j0 = max(k + 1, n0jm)
j_aibjakbibj: do j = j0, n1jm
if (j == k) cycle j_aibjakbibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbibj: do a = n0ac, a1
if (a == b) cycle a_aibjakbibj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakbibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakbibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbibj(a, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbibj
end do a_aibjakbibj
end do j_aibjakbibj
end do b_aibjakbibj
end do k_aibjakbibj
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjbi: do c = n0c, n1c
b0 = max(c + 1, n0bde)
b_aibjcibjbi: do b = b0, n1bde
if (b == c) cycle b_aibjcibjbi
j_aibjcibjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjbi: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibjbi
i1 = min(j - 1, n1ikm)
i_aibjcibjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcibjbi(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjbi
end do a_aibjcibjbi
end do j_aibjcibjbi
end do b_aibjcibjbi
end do c_aibjcibjbi
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickbibk: do c = n0c, n1c
k_aibickbibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bde)
b_aibickbibk: do b = b0, n1bde
if (b == c) cycle b_aibickbibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a_aibickbibk: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbibk
i0 = max(k + 1, n0ijl)
i_aibickbibk: do i = i0, n1ijl
if (i == k) cycle i_aibickbibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickbibk(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbibk
end do a_aibickbibk
end do b_aibickbibk
end do k_aibickbibk
end do c_aibickbibk
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, e == a, d == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjaiblai: do l = n0l, n1l
b_aibjaiblai: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblai: do j = n0j, n1j
if (j == l) cycle j_aibjaiblai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaiblai: do a = n0ace, a1
if (a == b) cycle a_aibjaiblai
i1 = min(j - 1, n1ikm)
i_aibjaiblai: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjaiblai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaiblai(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiblai
end do a_aibjaiblai
end do j_aibjaiblai
end do b_aibjaiblai
end do l_aibjaiblai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, e == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjaibiam: do m = n0m, n1m
b_aibjaibiam: do b = n0bd, n1bd
j_aibjaibiam: do j = n0j, n1j
if (j == m) cycle j_aibjaibiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaibiam: do a = n0ace, a1
if (a == b) cycle a_aibjaibiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aibjaibiam: do i = n0ikl, i1
if (i == j .or. i == m) cycle i_aibjaibiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaibiam(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibiam
end do a_aibjaibiam
end do j_aibjaibiam
end do b_aibjaibiam
end do m_aibjaibiam
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, e == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaibjam: do m = n0m, n1m
b_aibjaibjam: do b = n0bd, n1bd
j_aibjaibjam: do j = n0jl, n1jl
if (j == m) cycle j_aibjaibjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaibjam: do a = n0ace, a1
if (a == b) cycle a_aibjaibjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjaibjam: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aibjaibjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaibjam(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibjam
end do a_aibjaibjam
end do j_aibjaibjam
end do b_aibjaibjam
end do m_aibjaibjam
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: c == a, e == a, d == b, l == k, m == k
! No equalities independent of the above can hold.
!
k_aibjakbkak: do k = n0klm, n1klm
b_aibjakbkak: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbkak: do j = j0, n1j
if (j == k) cycle j_aibjakbkak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbkak: do a = n0ace, a1
if (a == b) cycle a_aibjakbkak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbkak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbkak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbkak(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbkak
end do a_aibjakbkak
end do j_aibjakbkak
end do b_aibjakbkak
end do k_aibjakbkak
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, e == a, d == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjakbiak: do k = n0km, n1km
b_aibjakbiak: do b = n0bd, n1bd
j0 = max(k + 1, n0j)
j_aibjakbiak: do j = j0, n1j
if (j == k) cycle j_aibjakbiak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbiak: do a = n0ace, a1
if (a == b) cycle a_aibjakbiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbiak: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakbiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbiak(a, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbiak
end do a_aibjakbiak
end do j_aibjakbiak
end do b_aibjakbiak
end do k_aibjakbiak
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, e == a, d == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjakbjak: do k = n0km, n1km
b_aibjakbjak: do b = n0bd, n1bd
j0 = max(k + 1, n0jl)
j_aibjakbjak: do j = j0, n1jl
if (j == k) cycle j_aibjakbjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbjak: do a = n0ace, a1
if (a == b) cycle a_aibjakbjak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbjak(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjak
end do a_aibjakbjak
end do j_aibjakbjak
end do b_aibjakbjak
end do k_aibjakbjak
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, e == a, d == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjakbkai: do k = n0kl, n1kl
b_aibjakbkai: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbkai: do j = j0, n1j
if (j == k) cycle j_aibjakbkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbkai: do a = n0ace, a1
if (a == b) cycle a_aibjakbkai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbkai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbkai(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbkai
end do a_aibjakbkai
end do j_aibjakbkai
end do b_aibjakbkai
end do k_aibjakbkai
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, e == a, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
m_aibiakbkam: do m = n0m, n1m
k_aibiakbkam: do k = n0kl, n1kl
if (k == m) cycle k_aibiakbkam
b_aibiakbkam: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibiakbkam: do a = n0ace, a1
if (a == b) cycle a_aibiakbkam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibiakbkam: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aibiakbkam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakbkam(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbkam
end do a_aibiakbkam
end do b_aibiakbkam
end do k_aibiakbkam
end do m_aibiakbkam
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, e == a, d == b, m == j, l == k
! No equalities independent of the above can hold.
!
k_aibjakbkaj: do k = n0kl, n1kl
b_aibjakbkaj: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aibjakbkaj: do j = j0, n1jm
if (j == k) cycle j_aibjakbkaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbkaj: do a = n0ace, a1
if (a == b) cycle a_aibjakbkaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakbkaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbkaj(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbkaj
end do a_aibjakbkaj
end do j_aibjakbkaj
end do b_aibjakbkaj
end do k_aibjakbkaj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, e == a, d == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibiakblai: do l = n0l, n1l
k_aibiakblai: do k = n0k, n1k
if (k == l) cycle k_aibiakblai
b_aibiakblai: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibiakblai: do a = n0ace, a1
if (a == b) cycle a_aibiakblai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibiakblai: do i = i0, n1ijm
if (i == k .or. i == l) cycle i_aibiakblai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakblai(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakblai
end do a_aibiakblai
end do b_aibiakblai
end do k_aibiakblai
end do l_aibiakblai
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, e == a, d == b, l == i, m == i
! No equalities independent of the above can hold.
!
k_aibjakbiai: do k = n0k, n1k
b_aibjakbiai: do b = n0bd, n1bd
j0 = max(k + 1, n0j)
j_aibjakbiai: do j = j0, n1j
if (j == k) cycle j_aibjakbiai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbiai: do a = n0ace, a1
if (a == b) cycle a_aibjakbiai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbiai: do i = n0ilm, n1ilm
if (i == j .or. i == k) cycle i_aibjakbiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbiai(a, i, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbiai
end do a_aibjakbiai
end do j_aibjakbiai
end do b_aibjakbiai
end do k_aibjakbiai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, e == a, d == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjakbjai: do k = n0k, n1k
b_aibjakbjai: do b = n0bd, n1bd
j0 = max(k + 1, n0jl)
j_aibjakbjai: do j = j0, n1jl
if (j == k) cycle j_aibjakbjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbjai: do a = n0ace, a1
if (a == b) cycle a_aibjakbjai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjai: do i = n0im, n1im
if (i == j .or. i == k) cycle i_aibjakbjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakbjai(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjai
end do a_aibjakbjai
end do j_aibjakbjai
end do b_aibjakbjai
end do k_aibjakbjai
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
end subroutine ccjac_32_tripletm_dav_part7
end module ccjac_block_32_tripletm_dav_part7
