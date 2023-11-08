module ccjac_block_23_tripletm_dav_part9
use eom_cc3_23_tripletm_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams 
implicit none
!
! File generated automatically on 2014-04-05 22:51:08 UTC.
!
contains
subroutine ccjac_23_tripletm_dav_part9(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, i0, i1, j0, j1, l0
integer :: n0abe, n0ac, n0acd, n0bd, n0be
integer :: n0cd, n0ce, n0ijk, n0ijkl, n0ijkm
integer :: n0ijl, n0ijm, n0ik, n0ikl, n0ikm
integer :: n0il, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jm, n0kl, n0km
integer :: n1abe, n1ac, n1acd, n1bd, n1be
integer :: n1cd, n1ce, n1ijk, n1ijkl, n1ijkm
integer :: n1ijl, n1ijm, n1ik, n1ikl, n1ikm
integer :: n1il, n1im, n1jk, n1jkl, n1jkm
integer :: n1jl, n1jm, n1kl, n1km
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
n0abe = max(n0a, n0b, n0e)
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0ce = max(n0c, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1abe = min(n1a, n1b, n1e)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1ce = min(n1c, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaibiej: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjaibiej: do b = b0, n1bd
if (b == e) cycle b_aibjaibiej
j_aibjaibiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibiej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaibiej
i0 = max(j + 1, n0ikl)
i_aibjaibiej: do i = i0, n1ikl
if (i == j) cycle i_aibjaibiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaibiej(nocc, a, i, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaibiej
end do a_aibjaibiej
end do j_aibjaibiej
end do b_aibjaibiej
end do e_aibjaibiej
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibiakbiek: do e = n0e, n1e
k_aibiakbiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibiakbiek: do b = b0, n1bd
if (b == e) cycle b_aibiakbiek
a0 = max(b + 1, n0ac)
a_aibiakbiek: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiakbiek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakbiek: do i = i0, n1ijl
if (i == k) cycle i_aibiakbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakbiek(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakbiek
end do a_aibiakbiek
end do b_aibiakbiek
end do k_aibiakbiek
end do e_aibiakbiek
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, m
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibiaibiem: do e = n0e, n1e
m_aibiaibiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibiaibiem: do b = b0, n1bd
if (b == e) cycle b_aibiaibiem
a0 = max(b + 1, n0ac)
a_aibiaibiem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiaibiem
i0 = max(m + 1, n0ijkl)
i_aibiaibiem: do i = i0, n1ijkl
if (i == m) cycle i_aibiaibiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaibiem(nocc, a, i, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaibiem
end do a_aibiaibiem
end do b_aibiaibiem
end do m_aibiaibiem
end do e_aibiaibiem
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, l
! Equalities: d == b, e == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiciblci: do l = n0l, n1l
c_aibiciblci: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibiciblci: do b = b0, n1bd
if (b == c) cycle b_aibiciblci
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibiciblci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibiciblci
i1 = min(l - 1, n1ijkm)
i_aibiciblci: do i = n0ijkm, i1
if (i == l) cycle i_aibiciblci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiciblci(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiciblci
end do a_aibiciblci
end do b_aibiciblci
end do c_aibiciblci
end do l_aibiciblci
!
! Elementary loop  5
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
a0 = max(b + 1, n0a)
a_aibjcibjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibjci
i1 = min(j - 1, n1ikm)
i_aibjcibjci: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcibjci(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcibjci
end do a_aibjcibjci
end do j_aibjcibjci
end do b_aibjcibjci
end do c_aibjcibjci
!
! Elementary loop  6
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
a0 = max(b + 1, n0a)
a_aibjcibicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibicj
i0 = max(j + 1, n0ikl)
i_aibjcibicj: do i = i0, n1ikl
if (i == j) cycle i_aibjcibicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcibicj(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcibicj
end do a_aibjcibicj
end do j_aibjcibicj
end do b_aibjcibicj
end do c_aibjcibicj
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, m
! Equalities: d == b, e == c, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibicibicm: do m = n0m, n1m
c_aibicibicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibicibicm: do b = b0, n1bd
if (b == c) cycle b_aibicibicm
a0 = max(b + 1, n0a)
a_aibicibicm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicibicm
i0 = max(m + 1, n0ijkl)
i_aibicibicm: do i = i0, n1ijkl
if (i == m) cycle i_aibicibicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicibicm(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicibicm
end do a_aibicibicm
end do b_aibicibicm
end do c_aibicibicm
end do m_aibicibicm
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, d == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjclai: do l = n0l, n1l
c_aiajcjclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j_aiajcjclai: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjclai
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiajcjclai: do a = n0abe, a1
if (a == c) cycle a_aiajcjclai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajcjclai: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajcjclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjclai(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjclai
end do a_aiajcjclai
end do j_aiajcjclai
end do c_aiajcjclai
end do l_aiajcjclai
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, e == a, d == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajciclaj: do l = n0l, n1l
c_aiajciclaj: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aiajciclaj: do j = n0jm, j1
if (j == l) cycle j_aiajciclaj
a1 = min(c - 1, n1abe)
a_aiajciclaj: do a = n0abe, a1
if (a == c) cycle a_aiajciclaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciclaj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajciclaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajciclaj(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajciclaj
end do a_aiajciclaj
end do j_aiajciclaj
end do c_aiajciclaj
end do l_aiajciclaj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, d == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjciam: do m = n0m, n1m
c_aiajcjciam: do c = n0cd, n1cd
j_aiajcjciam: do j = n0jk, n1jk
if (j == m) cycle j_aiajcjciam
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiajcjciam: do a = n0abe, a1
if (a == c) cycle a_aiajcjciam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajcjciam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajcjciam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjciam(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjciam
end do a_aiajcjciam
end do j_aiajcjciam
end do c_aiajcjciam
end do m_aiajcjciam
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, e == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajcicjam: do m = n0m, n1m
c_aiajcicjam: do c = n0cd, n1cd
j0 = max(m + 1, n0jl)
j_aiajcicjam: do j = j0, n1jl
if (j == m) cycle j_aiajcicjam
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiajcicjam: do a = n0abe, a1
if (a == c) cycle a_aiajcicjam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcicjam: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajcicjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcicjam(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcicjam
end do a_aiajcicjam
end do j_aiajcicjam
end do c_aiajcicjam
end do m_aiajcicjam
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajcjdiaj: do d = n0d, n1d
c_aiajcjdiaj: do c = n0c, n1c
if (c == d) cycle c_aiajcjdiaj
j_aiajcjdiaj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abe)
a_aiajcjdiaj: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcjdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjdiaj: do i = i0, n1il
if (i == j) cycle i_aiajcjdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjdiaj(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjdiaj
end do a_aiajcjdiaj
end do j_aiajcjdiaj
end do c_aiajcjdiaj
end do d_aiajcjdiaj
!
! Elementary loop  13
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
a1 = min(d - 1, n1abe)
a_aiajcidiaj: do a = n0abe, a1
if (a == c .or. a == d) cycle a_aiajcidiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajcidiaj: do i = i0, n1ikl
if (i == j) cycle i_aiajcidiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcidiaj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcidiaj
end do a_aiajcidiaj
end do j_aiajcidiaj
end do c_aiajcidiaj
end do d_aiajcidiaj
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == a, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibiakalbi: do l = n0l, n1l
k_aibiakalbi: do k = n0k, n1k
if (k == l) cycle k_aibiakalbi
b_aibiakalbi: do b = n0be, n1be
a0 = max(b + 1, n0acd)
a_aibiakalbi: do a = a0, n1acd
if (a == b) cycle a_aibiakalbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijm)
i_aibiakalbi: do i = n0ijm, i1
if (i == k .or. i == l) cycle i_aibiakalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakalbi(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakalbi
end do a_aibiakalbi
end do b_aibiakalbi
end do k_aibiakalbi
end do l_aibiakalbi
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjaialbi: do l = n0l, n1l
b_aibjaialbi: do b = n0be, n1be
j_aibjaialbi: do j = n0j, n1j
if (j == l) cycle j_aibjaialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaialbi: do a = a0, n1acd
if (a == b) cycle a_aibjaialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ikm)
i_aibjaialbi: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjaialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaialbi(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaialbi
end do a_aibjaialbi
end do j_aibjaialbi
end do b_aibjaialbi
end do l_aibjaialbi
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjakajbi: do k = n0k, n1k
b_aibjakajbi: do b = n0be, n1be
j_aibjakajbi: do j = n0jl, n1jl
if (j == k) cycle j_aibjakajbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakajbi: do a = a0, n1acd
if (a == b) cycle a_aibjakajbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjakajbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakajbi(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakajbi
end do a_aibjakajbi
end do j_aibjakajbi
end do b_aibjakajbi
end do k_aibjakajbi
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjajalbi: do l = n0l, n1l
b_aibjajalbi: do b = n0be, n1be
j_aibjajalbi: do j = n0jk, n1jk
if (j == l) cycle j_aibjajalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajalbi: do a = a0, n1acd
if (a == b) cycle a_aibjajalbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjajalbi: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjajalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajalbi(a, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajalbi
end do a_aibjajalbi
end do j_aibjajalbi
end do b_aibjajalbi
end do l_aibjajalbi
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjajalbj: do l = n0l, n1l
b_aibjajalbj: do b = n0be, n1be
j1 = min(l - 1, n1jkm)
j_aibjajalbj: do j = n0jkm, j1
if (j == l) cycle j_aibjajalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajalbj: do a = a0, n1acd
if (a == b) cycle a_aibjajalbj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajalbj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajalbj(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajalbj
end do a_aibjajalbj
end do j_aibjajalbj
end do b_aibjajalbj
end do l_aibjajalbj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakaibj: do k = n0k, n1k
b_aibjakaibj: do b = n0be, n1be
j_aibjakaibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjakaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakaibj: do a = a0, n1acd
if (a == b) cycle a_aibjakaibj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakaibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakaibj(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakaibj
end do a_aibjakaibj
end do j_aibjakaibj
end do b_aibjakaibj
end do k_aibjakaibj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjaialbj: do l = n0l, n1l
b_aibjaialbj: do b = n0be, n1be
j1 = min(l - 1, n1jm)
j_aibjaialbj: do j = n0jm, j1
if (j == l) cycle j_aibjaialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaialbj: do a = a0, n1acd
if (a == b) cycle a_aibjaialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjaialbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaialbj(nocc, a, i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaialbj
end do a_aibjaialbj
end do j_aibjaialbj
end do b_aibjaialbj
end do l_aibjaialbj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, d == a, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
k_aibjakakbj: do k = n0kl, n1kl
b_aibjakakbj: do b = n0be, n1be
j1 = min(k - 1, n1jm)
j_aibjakakbj: do j = n0jm, j1
if (j == k) cycle j_aibjakakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakakbj: do a = a0, n1acd
if (a == b) cycle a_aibjakakbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakakbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakakbj(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakakbj
end do a_aibjakakbj
end do j_aibjakakbj
end do b_aibjakakbj
end do k_aibjakakbj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, d == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjakajbk: do k = n0km, n1km
b_aibjakajbk: do b = n0be, n1be
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjakajbk: do j = j0, n1jl
if (j == k) cycle j_aibjakajbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakajbk: do a = a0, n1acd
if (a == b) cycle a_aibjakajbk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakajbk(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakajbk
end do a_aibjakajbk
end do j_aibjakajbk
end do b_aibjakajbk
end do k_aibjakajbk
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: c == a, d == a, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakaibm: do m = n0m, n1m
k_aibiakaibm: do k = n0k, n1k
if (k == m) cycle k_aibiakaibm
b_aibiakaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibiakaibm: do a = a0, n1acd
if (a == b) cycle a_aibiakaibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(m + 1, n0ijl)
i_aibiakaibm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibiakaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakaibm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakaibm
end do a_aibiakaibm
end do b_aibiakaibm
end do k_aibiakaibm
end do m_aibiakaibm
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjaiaibm: do m = n0m, n1m
b_aibjaiaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaiaibm: do j = n0j, n1j
if (j == m) cycle j_aibjaiaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiaibm: do a = a0, n1acd
if (a == b) cycle a_aibjaiaibm
i0 = max(m + 1, n0ikl)
i_aibjaiaibm: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjaiaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiaibm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiaibm
end do a_aibjaiaibm
end do j_aibjaiaibm
end do b_aibjaiaibm
end do m_aibjaiaibm
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajaibm: do m = n0m, n1m
b_aibjajaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajaibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjajaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajaibm: do a = a0, n1acd
if (a == b) cycle a_aibjajaibm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(m + 1, n0il)
i_aibjajaibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjajaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajaibm(a, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajaibm
end do a_aibjajaibm
end do j_aibjajaibm
end do b_aibjajaibm
end do m_aibjajaibm
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l, m
! Equalities: c == a, d == a, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibiaialbm: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aibiaialbm: do l = l0, n1l
if (l == m) cycle l_aibiaialbm
b_aibiaialbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibiaialbm: do a = a0, n1acd
if (a == b) cycle a_aibiaialbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibiaialbm: do i = n0ijk, n1ijk
if (i == l .or. i == m) cycle i_aibiaialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaialbm(a, i, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaialbm
end do a_aibiaialbm
end do b_aibiaialbm
end do l_aibiaialbm
end do m_aibiaialbm
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaiajbm: do m = n0m, n1m
b_aibjaiajbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjaiajbm: do j = j0, n1jl
if (j == m) cycle j_aibjaiajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiajbm: do a = a0, n1acd
if (a == b) cycle a_aibjaiajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaiajbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjaiajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiajbm(nocc, a, i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiajbm
end do a_aibjaiajbm
end do j_aibjaiajbm
end do b_aibjaiajbm
end do m_aibjaiajbm
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, m
! Equalities: c == a, d == a, e == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjajajbm: do m = n0m, n1m
b_aibjajajbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jkl)
j_aibjajajbm: do j = j0, n1jkl
if (j == m) cycle j_aibjajajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajajbm: do a = a0, n1acd
if (a == b) cycle a_aibjajajbm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajajbm: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjajajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajajbm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajajbm
end do a_aibjajajbm
end do j_aibjajajbm
end do b_aibjajajbm
end do m_aibjajajbm
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, l
! Equalities: e == b, d == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiciclbi: do l = n0l, n1l
c_aibiciclbi: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(c - 1, n1be)
b_aibiciclbi: do b = n0be, b1
if (b == c) cycle b_aibiciclbi
a0 = max(b + 1, n0a)
a_aibiciclbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibiciclbi
i1 = min(l - 1, n1ijkm)
i_aibiciclbi: do i = n0ijkm, i1
if (i == l) cycle i_aibiciclbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiciclbi(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiciclbi
end do a_aibiciclbi
end do b_aibiciclbi
end do c_aibiciclbi
end do l_aibiciclbi
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: e == b, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicjbi: do c = n0cd, n1cd
b1 = min(c - 1, n1be)
b_aibjcicjbi: do b = n0be, b1
if (b == c) cycle b_aibjcicjbi
j_aibjcicjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicjbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicjbi
i1 = min(j - 1, n1ikm)
i_aibjcicjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjcicjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcicjbi(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcicjbi
end do a_aibjcicjbi
end do j_aibjcicjbi
end do b_aibjcicjbi
end do c_aibjcicjbi
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
end subroutine ccjac_23_tripletm_dav_part9
end module ccjac_block_23_tripletm_dav_part9
