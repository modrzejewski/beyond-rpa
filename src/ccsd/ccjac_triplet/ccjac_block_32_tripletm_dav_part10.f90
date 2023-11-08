module ccjac_block_32_tripletm_dav_part10
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
subroutine ccjac_32_tripletm_dav_part10(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, i0, i1, j0
integer :: n0acd, n0acde, n0ace, n0ad, n0ae
integer :: n0cd, n0cde, n0ce, n0ij, n0ijl
integer :: n0ijlm, n0ijm, n0ik, n0ikl, n0iklm
integer :: n0ikm, n0il, n0im, n0jl, n0jlm
integer :: n0jm, n0kl, n0klm, n0km
integer :: n1acd, n1acde, n1ace, n1ad, n1ae
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
n0acd = max(n0a, n0c, n0d)
n0acde = max(n0a, n0c, n0d, n0e)
n0ace = max(n0a, n0c, n0e)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
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
n1acd = min(n1a, n1c, n1d)
n1acde = min(n1a, n1c, n1d, n1e)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
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
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == a, e == a, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibiakalai: do l = n0l, n1l
k_aibiakalai: do k = n0k, n1k
if (k == l) cycle k_aibiakalai
b_aibiakalai: do b = n0b, n1b
a1 = min(b - 1, n1acde)
a_aibiakalai: do a = n0acde, a1
if (a == b) cycle a_aibiakalai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ijm)
i1 = min(l - 1, n1ijm)
i_aibiakalai: do i = i0, i1
if (i == k .or. i == l) cycle i_aibiakalai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakalai(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakalai
end do a_aibiakalai
end do b_aibiakalai
end do k_aibiakalai
end do l_aibiakalai
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == a, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjakajai: do k = n0k, n1k
b_aibjakajai: do b = n0b, n1b
j0 = max(k + 1, n0jl)
j_aibjakajai: do j = j0, n1jl
if (j == k) cycle j_aibjakajai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakajai: do a = n0acde, a1
if (a == b) cycle a_aibjakajai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjakajai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakajai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakajai(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakajai
end do a_aibjakajai
end do j_aibjakajai
end do b_aibjakajai
end do k_aibjakajai
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, d == a, e == a, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakaiam: do m = n0m, n1m
k_aibiakaiam: do k = n0k, n1k
if (k == m) cycle k_aibiakaiam
b_aibiakaiam: do b = n0b, n1b
a1 = min(b - 1, n1acde)
a_aibiakaiam: do a = n0acde, a1
if (a == b) cycle a_aibiakaiam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(k + 1, m + 1, n0ijl)
i_aibiakaiam: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibiakaiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakaiam(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakaiam
end do a_aibiakaiam
end do b_aibiakaiam
end do k_aibiakaiam
end do m_aibiakaiam
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakaiaj: do k = n0k, n1k
b_aibjakaiaj: do b = n0b, n1b
j0 = max(k + 1, n0jm)
j_aibjakaiaj: do j = j0, n1jm
if (j == k) cycle j_aibjakaiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1acde)
a_aibjakaiaj: do a = n0acde, a1
if (a == b) cycle a_aibjakaiaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakaiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakaiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v2_eom_cc3_32_tripletm_trans_aibjakaiaj(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakaiaj
end do a_aibjakaiaj
end do j_aibjakaiaj
end do b_aibjakaiaj
end do k_aibjakaiaj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibjaiaiei: do e = n0e, n1e
b_aibjaiaiei: do b = n0b, n1b
if (b == e) cycle b_aibjaiaiei
j_aibjaiaiei: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibjaiaiei: do a = a0, a1
if (a == b .or. a == e) cycle a_aibjaiaiei
i1 = min(j - 1, n1iklm)
i_aibjaiaiei: do i = n0iklm, i1
if (i == j) cycle i_aibjaiaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaiaiei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiaiei
end do a_aibjaiaiei
end do j_aibjaiaiei
end do b_aibjaiaiei
end do e_aibjaiaiei
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaiajei: do e = n0e, n1e
b_aibjaiajei: do b = n0b, n1b
if (b == e) cycle b_aibjaiajei
j_aibjaiajei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibjaiajei: do a = a0, a1
if (a == b .or. a == e) cycle a_aibjaiajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjaiajei: do i = n0ikm, i1
if (i == j) cycle i_aibjaiajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaiajei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiajei
end do a_aibjaiajei
end do j_aibjaiajei
end do b_aibjaiajei
end do e_aibjaiajei
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaiaiej: do e = n0e, n1e
b_aibjaiaiej: do b = n0b, n1b
if (b == e) cycle b_aibjaiaiej
j_aibjaiaiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibjaiaiej: do a = a0, a1
if (a == b .or. a == e) cycle a_aibjaiaiej
i1 = min(j - 1, n1ikl)
i_aibjaiaiej: do i = n0ikl, i1
if (i == j) cycle i_aibjaiaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaiaiej(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiaiej
end do a_aibjaiaiej
end do j_aibjaiaiej
end do b_aibjaiaiej
end do e_aibjaiaiej
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibiakaiek: do e = n0e, n1e
k_aibiakaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibiakaiek: do b = n0b, n1b
if (b == e) cycle b_aibiakaiek
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibiakaiek: do a = a0, a1
if (a == b .or. a == e) cycle a_aibiakaiek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakaiek: do i = i0, n1ijl
if (i == k) cycle i_aibiakaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakaiek(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakaiek
end do a_aibiakaiek
end do b_aibiakaiek
end do k_aibiakaiek
end do e_aibiakaiek
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibiakakei: do e = n0e, n1e
k_aibiakakei: do k = n0kl, n1kl
b_aibiakakei: do b = n0b, n1b
if (b == e) cycle b_aibiakakei
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibiakakei: do a = a0, a1
if (a == b .or. a == e) cycle a_aibiakakei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibiakakei: do i = i0, n1ijm
if (i == k) cycle i_aibiakakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakakei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakakei
end do a_aibiakakei
end do b_aibiakakei
end do k_aibiakakei
end do e_aibiakakei
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
e_aibiakaiei: do e = n0e, n1e
k_aibiakaiei: do k = n0k, n1k
b_aibiakaiei: do b = n0b, n1b
if (b == e) cycle b_aibiakaiei
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibiakaiei: do a = a0, a1
if (a == b .or. a == e) cycle a_aibiakaiei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijlm)
i_aibiakaiei: do i = i0, n1ijlm
if (i == k) cycle i_aibiakaiei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakaiei(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakaiei
end do a_aibiakaiei
end do b_aibiakaiei
end do k_aibiakaiei
end do e_aibiakaiei
!
! Elementary loop  11
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicjci: do c = n0cde, n1cde
b0 = max(c + 1, n0b)
b_aibjcicjci: do b = b0, n1b
if (b == c) cycle b_aibjcicjci
j_aibjcicjci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicjci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcicjci
i1 = min(j - 1, n1ikm)
i_aibjcicjci: do i = n0ikm, i1
if (i == j) cycle i_aibjcicjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcicjci(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcicjci
end do a_aibjcicjci
end do j_aibjcicjci
end do b_aibjcicjci
end do c_aibjcicjci
!
! Elementary loop  12
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k
! Equalities: d == c, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickcick: do c = n0cde, n1cde
k_aibickcick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickcick: do b = b0, n1b
if (b == c) cycle b_aibickcick
a_aibickcick: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickcick
i0 = max(k + 1, n0ijl)
i_aibickcick: do i = i0, n1ijl
if (i == k) cycle i_aibickcick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickcick(a, i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickcick
end do a_aibickcick
end do b_aibickcick
end do k_aibickcick
end do c_aibickcick
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjciciai: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjciciai: do b = b0, n1b
if (b == c) cycle b_aibjciciai
j_aibjciciai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjciciai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjciciai
i1 = min(j - 1, n1iklm)
i_aibjciciai: do i = n0iklm, i1
if (i == j) cycle i_aibjciciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciciai(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciciai
end do a_aibjciciai
end do j_aibjciciai
end do b_aibjciciai
end do c_aibjciciai
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicjai: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcicjai: do b = b0, n1b
if (b == c) cycle b_aibjcicjai
j_aibjcicjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjcicjai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcicjai
i1 = min(j - 1, n1ikm)
i_aibjcicjai: do i = n0ikm, i1
if (i == j) cycle i_aibjcicjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcicjai(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcicjai
end do a_aibjcicjai
end do j_aibjcicjai
end do b_aibjcicjai
end do c_aibjcicjai
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciciaj: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjciciaj: do b = b0, n1b
if (b == c) cycle b_aibjciciaj
j_aibjciciaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjciciaj: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjciciaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aibjciciaj: do i = n0ikl, i1
if (i == j) cycle i_aibjciciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciciaj(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciciaj
end do a_aibjciciaj
end do j_aibjciciaj
end do b_aibjciciaj
end do c_aibjciciaj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcicjaj: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcicjaj: do b = b0, n1b
if (b == c) cycle b_aibjcicjaj
j_aibjcicjaj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjcicjaj: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcicjaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjcicjaj: do i = n0ik, i1
if (i == j) cycle i_aibjcicjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjcicjaj(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcicjaj
end do a_aibjcicjaj
end do j_aibjcicjaj
end do b_aibjcicjaj
end do c_aibjcicjaj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: e == a, d == c, j == i, l == k, m == k
! No equalities independent of the above can hold.
!
c_aibickckak: do c = n0cd, n1cd
k_aibickckak: do k = n0klm, n1klm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickckak: do b = b0, n1b
if (b == c) cycle b_aibickckak
a1 = min(c - 1, n1ae)
a_aibickckak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickckak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibickckak: do i = i0, n1ij
if (i == k) cycle i_aibickckak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickckak(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickckak
end do a_aibickckak
end do b_aibickckak
end do k_aibickckak
end do c_aibickckak
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: e == a, d == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickciak: do c = n0cd, n1cd
k_aibickciak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickciak: do b = b0, n1b
if (b == c) cycle b_aibickciak
a1 = min(c - 1, n1ae)
a_aibickciak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickciak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibickciak: do i = i0, n1ijl
if (i == k) cycle i_aibickciak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickciak(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickciak
end do a_aibickciak
end do b_aibickciak
end do k_aibickciak
end do c_aibickciak
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: e == a, d == c, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibickckai: do c = n0cd, n1cd
k_aibickckai: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickckai: do b = b0, n1b
if (b == c) cycle b_aibickckai
a1 = min(c - 1, n1ae)
a_aibickckai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickckai
i0 = max(k + 1, n0ijm)
i_aibickckai: do i = i0, n1ijm
if (i == k) cycle i_aibickckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickckai(a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickckai
end do a_aibickckai
end do b_aibickckai
end do k_aibickckai
end do c_aibickckai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: e == a, d == c, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibickciai: do c = n0cd, n1cd
k_aibickciai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickciai: do b = b0, n1b
if (b == c) cycle b_aibickciai
a1 = min(c - 1, n1ae)
a_aibickciai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickciai
i0 = max(k + 1, n0ijlm)
i_aibickciai: do i = i0, n1ijlm
if (i == k) cycle i_aibickciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v3_eom_cc3_32_tripletm_trans_aibickciai(a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickciai
end do a_aibickciai
end do b_aibickciai
end do k_aibickciai
end do c_aibickciai
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == a, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibjaidiai: do d = n0d, n1d
b_aibjaidiai: do b = n0b, n1b
if (b == d) cycle b_aibjaidiai
j_aibjaidiai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, d - 1, n1ace)
a_aibjaidiai: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibjaidiai
i1 = min(j - 1, n1iklm)
i_aibjaidiai: do i = n0iklm, i1
if (i == j) cycle i_aibjaidiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaidiai(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaidiai
end do a_aibjaidiai
end do j_aibjaidiai
end do b_aibjaidiai
end do d_aibjaidiai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidjai: do d = n0d, n1d
b_aibjaidjai: do b = n0b, n1b
if (b == d) cycle b_aibjaidjai
j_aibjaidjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, d - 1, n1ace)
a_aibjaidjai: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibjaidjai
i1 = min(j - 1, n1ikm)
i_aibjaidjai: do i = n0ikm, i1
if (i == j) cycle i_aibjaidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaidjai(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaidjai
end do a_aibjaidjai
end do j_aibjaidjai
end do b_aibjaidjai
end do d_aibjaidjai
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjaidiaj: do d = n0d, n1d
b_aibjaidiaj: do b = n0b, n1b
if (b == d) cycle b_aibjaidiaj
j_aibjaidiaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, d - 1, n1ace)
a_aibjaidiaj: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibjaidiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikl)
i_aibjaidiaj: do i = n0ikl, i1
if (i == j) cycle i_aibjaidiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v8_eom_cc3_32_tripletm_trans_aibjaidiaj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaidiaj
end do a_aibjaidiaj
end do j_aibjaidiaj
end do b_aibjaidiaj
end do d_aibjaidiaj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibiakdiak: do d = n0d, n1d
k_aibiakdiak: do k = n0km, n1km
b_aibiakdiak: do b = n0b, n1b
if (b == d) cycle b_aibiakdiak
a1 = min(b - 1, d - 1, n1ace)
a_aibiakdiak: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibiakdiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakdiak: do i = i0, n1ijl
if (i == k) cycle i_aibiakdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakdiak(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakdiak
end do a_aibiakdiak
end do b_aibiakdiak
end do k_aibiakdiak
end do d_aibiakdiak
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == a, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibiakdkai: do d = n0d, n1d
k_aibiakdkai: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibiakdkai: do b = n0b, n1b
if (b == d) cycle b_aibiakdkai
a1 = min(b - 1, d - 1, n1ace)
a_aibiakdkai: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibiakdkai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijm)
i_aibiakdkai: do i = i0, n1ijm
if (i == k) cycle i_aibiakdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakdkai(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakdkai
end do a_aibiakdkai
end do b_aibiakdkai
end do k_aibiakdkai
end do d_aibiakdkai
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == a, j == i, l == i, m == i
! No equalities independent of the above can hold.
!
d_aibiakdiai: do d = n0d, n1d
k_aibiakdiai: do k = n0k, n1k
b_aibiakdiai: do b = n0b, n1b
if (b == d) cycle b_aibiakdiai
a1 = min(b - 1, d - 1, n1ace)
a_aibiakdiai: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibiakdiai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijlm)
i_aibiakdiai: do i = i0, n1ijlm
if (i == k) cycle i_aibiakdiai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v7_eom_cc3_32_tripletm_trans_aibiakdiai(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakdiai
end do a_aibiakdiai
end do b_aibiakdiai
end do k_aibiakdiai
end do d_aibiakdiai
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == i, m == i
! No equalities independent of the above can hold.
!
c_aibjciaici: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjciaici: do b = b0, n1b
if (b == c) cycle b_aibjciaici
j_aibjciaici: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjciaici: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaici
i1 = min(j - 1, n1iklm)
i_aibjciaici: do i = n0iklm, i1
if (i == j) cycle i_aibjciaici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciaici(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciaici
end do a_aibjciaici
end do j_aibjciaici
end do b_aibjciaici
end do c_aibjciaici
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajci: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjciajci: do b = b0, n1b
if (b == c) cycle b_aibjciajci
j_aibjciajci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjciajci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajci
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjciajci: do i = n0ikm, i1
if (i == j) cycle i_aibjciajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciajci(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciajci
end do a_aibjciajci
end do j_aibjciajci
end do b_aibjciajci
end do c_aibjciajci
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaicj: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjciaicj: do b = b0, n1b
if (b == c) cycle b_aibjciaicj
j_aibjciaicj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjciaicj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaicj
i1 = min(j - 1, n1ikl)
i_aibjciaicj: do i = n0ikl, i1
if (i == j) cycle i_aibjciaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciaicj(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciaicj
end do a_aibjciaicj
end do j_aibjciaicj
end do b_aibjciaicj
end do c_aibjciaicj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == j, m == j
! No equalities independent of the above can hold.
!
c_aibjciajcj: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjciajcj: do b = b0, n1b
if (b == c) cycle b_aibjciajcj
j_aibjciajcj: do j = n0jlm, n1jlm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjciajcj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajcj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjciajcj: do i = n0ik, i1
if (i == j) cycle i_aibjciajcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
((dl - 1) * (dl - 2)) / 2 + em
jac_ibra_iket = v4_eom_cc3_32_tripletm_trans_aibjciajcj(i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciajcj
end do a_aibjciajcj
end do j_aibjciajcj
end do b_aibjciajcj
end do c_aibjciajcj
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
end subroutine ccjac_32_tripletm_dav_part10
end module ccjac_block_32_tripletm_dav_part10
