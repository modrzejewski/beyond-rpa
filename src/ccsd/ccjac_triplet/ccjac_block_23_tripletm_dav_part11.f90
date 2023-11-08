module ccjac_block_23_tripletm_dav_part11
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
subroutine ccjac_23_tripletm_dav_part11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, i0, i1
integer :: n0abcd, n0abce, n0abd, n0abe, n0acd
integer :: n0ace, n0ad, n0bce, n0be, n0cd
integer :: n0ce, n0ijkl, n0ijkm, n0ijl, n0ijm
integer :: n0ikl, n0ikm, n0il, n0im, n0jkl
integer :: n0jkm, n0jl, n0jm, n0kl, n0km
integer :: n1abcd, n1abce, n1abd, n1abe, n1acd
integer :: n1ace, n1ad, n1bce, n1be, n1cd
integer :: n1ce, n1ijkl, n1ijkm, n1ijl, n1ijm
integer :: n1ikl, n1ikm, n1il, n1im, n1jkl
integer :: n1jkm, n1jl, n1jm, n1kl, n1km
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
n0abcd = max(n0a, n0b, n0c, n0d)
n0abce = max(n0a, n0b, n0c, n0e)
n0abd = max(n0a, n0b, n0d)
n0abe = max(n0a, n0b, n0e)
n0acd = max(n0a, n0c, n0d)
n0ace = max(n0a, n0c, n0e)
n0ad = max(n0a, n0d)
n0bce = max(n0b, n0c, n0e)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0ce = max(n0c, n0e)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ijm = max(n0i, n0j, n0m)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jkl = max(n0j, n0k, n0l)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1abcd = min(n1a, n1b, n1c, n1d)
n1abce = min(n1a, n1b, n1c, n1e)
n1abd = min(n1a, n1b, n1d)
n1abe = min(n1a, n1b, n1e)
n1acd = min(n1a, n1c, n1d)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1bce = min(n1b, n1c, n1e)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1ce = min(n1c, n1e)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ijm = min(n1i, n1j, n1m)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jkl = min(n1j, n1k, n1l)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
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
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibjaidiaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjaidiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aibjaidiaj: do i = i0, n1ikl
if (i == j) cycle i_aibjaidiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidiaj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidiaj
end do a_aibjaidiaj
end do j_aibjaidiaj
end do b_aibjaidiaj
end do d_aibjaidiaj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, m
! Equalities: c == a, e == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiaidiam: do m = n0m, n1m
d_aibiaidiam: do d = n0d, n1d
b_aibiaidiam: do b = n0b, n1b
if (b == d) cycle b_aibiaidiam
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibiaidiam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibiaidiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ijkl)
i_aibiaidiam: do i = i0, n1ijkl
if (i == m) cycle i_aibiaidiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaidiam(a, b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaidiam
end do a_aibiaidiam
end do b_aibiaidiam
end do d_aibiaidiam
end do m_aibiaidiam
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, l
! Equalities: d == a, e == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibicialci: do l = n0l, n1l
c_aibicialci: do c = n0ce, n1ce
b_aibicialci: do b = n0b, n1b
if (b == c) cycle b_aibicialci
a0 = max(b + 1, c + 1, n0ad)
a_aibicialci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibicialci
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijkm)
i_aibicialci: do i = n0ijkm, i1
if (i == l) cycle i_aibicialci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicialci(b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicialci
end do a_aibicialci
end do b_aibicialci
end do c_aibicialci
end do l_aibicialci
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajci: do c = n0ce, n1ce
b_aibjcjajci: do b = n0b, n1b
if (b == c) cycle b_aibjcjajci
j_aibjcjajci: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjajci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjajci
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjcjajci: do i = n0im, i1
if (i == j) cycle i_aibjcjajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjajci(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjajci
end do a_aibjcjajci
end do j_aibjcjajci
end do b_aibjcjajci
end do c_aibjcjajci
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaicj: do c = n0ce, n1ce
b_aibjcjaicj: do b = n0b, n1b
if (b == c) cycle b_aibjcjaicj
j_aibjcjaicj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjaicj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaicj
i0 = max(j + 1, n0il)
i_aibjcjaicj: do i = i0, n1il
if (i == j) cycle i_aibjcjaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjaicj(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjaicj
end do a_aibjcjaicj
end do j_aibjcjaicj
end do b_aibjcjaicj
end do c_aibjcjaicj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, m
! Equalities: d == a, e == c, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiciaicm: do m = n0m, n1m
c_aibiciaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b_aibiciaicm: do b = n0b, n1b
if (b == c) cycle b_aibiciaicm
a0 = max(b + 1, c + 1, n0ad)
a_aibiciaicm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibiciaicm
i0 = max(m + 1, n0ijkl)
i_aibiciaicm: do i = i0, n1ijkl
if (i == m) cycle i_aibiciaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiciaicm(b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiciaicm
end do a_aibiciaicm
end do b_aibiciaicm
end do c_aibiciaicm
end do m_aibiciaicm
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajajaiej: do e = n0e, n1e
j_aiajajaiej: do j = n0jkm, n1jkm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajajaiej: do a = a0, n1abcd
if (a == e) cycle a_aiajajaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajajaiej: do i = i0, n1il
if (i == j) cycle i_aiajajaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajaiej(nocc, a, i, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajaiej
end do a_aiajajaiej
end do j_aiajajaiej
end do e_aiajajaiej
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajaiaiej: do e = n0e, n1e
j_aiajaiaiej: do j = n0jm, n1jm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abcd)
a_aiajaiaiej: do a = a0, n1abcd
if (a == e) cycle a_aiajaiaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajaiaiej: do i = i0, n1ikl
if (i == j) cycle i_aiajaiaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaiaiej(nocc, a, i, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaiaiej
end do a_aiajaiaiej
end do j_aiajaiaiej
end do e_aiajaiaiej
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajajdiaj: do d = n0d, n1d
j_aiajajdiaj: do j = n0jkm, n1jkm
a1 = min(d - 1, n1abce)
a_aiajajdiaj: do a = n0abce, a1
if (a == d) cycle a_aiajajdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajajdiaj: do i = i0, n1il
if (i == j) cycle i_aiajajdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajdiaj(nocc, a, i, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajdiaj
end do a_aiajajdiaj
end do j_aiajajdiaj
end do d_aiajajdiaj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajaidiaj: do d = n0d, n1d
j_aiajaidiaj: do j = n0jm, n1jm
a1 = min(d - 1, n1abce)
a_aiajaidiaj: do a = n0abce, a1
if (a == d) cycle a_aiajaidiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajaidiaj: do i = i0, n1ikl
if (i == j) cycle i_aiajaidiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaidiaj(nocc, a, i, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaidiaj
end do a_aiajaidiaj
end do j_aiajaidiaj
end do d_aiajaidiaj
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: d == a, c == b, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibibialbi: do l = n0l, n1l
b_aibibialbi: do b = n0bce, n1bce
a0 = max(b + 1, n0ad)
a_aibibialbi: do a = a0, n1ad
if (a == b) cycle a_aibibialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijkm)
i_aibibialbi: do i = n0ijkm, i1
if (i == l) cycle i_aibibialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibialbi(nocc, a, i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibialbi
end do a_aibibialbi
end do b_aibibialbi
end do l_aibibialbi
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkakbi: do k = n0kl, n1kl
b_aibibkakbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkakbi: do a = a0, n1ad
if (a == b) cycle a_aibibkakbi
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijm)
i_aibibkakbi: do i = n0ijm, i1
if (i == k) cycle i_aibibkakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkakbi(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkakbi
end do a_aibibkakbi
end do b_aibibkakbi
end do k_aibibkakbi
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbiajbi: do b = n0bce, n1bce
j_aibjbiajbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiajbi: do a = a0, n1ad
if (a == b) cycle a_aibjbiajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjbiajbi: do i = n0ikm, i1
if (i == j) cycle i_aibjbiajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbiajbi(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbiajbi
end do a_aibjbiajbi
end do j_aibjbiajbi
end do b_aibjbiajbi
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjbjajbi: do b = n0bce, n1bce
j_aibjbjajbi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbjajbi: do a = a0, n1ad
if (a == b) cycle a_aibjbjajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjbjajbi: do i = n0im, i1
if (i == j) cycle i_aibjbjajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjajbi(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjajbi
end do a_aibjbjajbi
end do j_aibjbjajbi
end do b_aibjbjajbi
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
b_aibjbjaibj: do b = n0bce, n1bce
j_aibjbjaibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbjaibj: do a = a0, n1ad
if (a == b) cycle a_aibjbjaibj
i0 = max(j + 1, n0il)
i_aibjbjaibj: do i = i0, n1il
if (i == j) cycle i_aibjbjaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjaibj(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjaibj
end do a_aibjbjaibj
end do j_aibjbjaibj
end do b_aibjbjaibj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: d == a, c == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
b_aibjbiaibj: do b = n0bce, n1bce
j_aibjbiaibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiaibj: do a = a0, n1ad
if (a == b) cycle a_aibjbiaibj
i0 = max(j + 1, n0ikl)
i_aibjbiaibj: do i = i0, n1ikl
if (i == j) cycle i_aibjbiaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbiaibj(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbiaibj
end do a_aibjbiaibj
end do j_aibjbiaibj
end do b_aibjbiaibj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: d == a, c == b, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibibkaibk: do k = n0km, n1km
b_aibibkaibk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkaibk: do a = a0, n1ad
if (a == b) cycle a_aibibkaibk
i0 = max(k + 1, n0ijl)
i_aibibkaibk: do i = i0, n1ijl
if (i == k) cycle i_aibibkaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkaibk(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkaibk
end do a_aibibkaibk
end do b_aibibkaibk
end do k_aibibkaibk
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, m
! Equalities: d == a, c == b, e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibibiaibm: do m = n0m, n1m
b_aibibiaibm: do b = n0bce, n1bce
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibiaibm: do a = a0, n1ad
if (a == b) cycle a_aibibiaibm
i0 = max(m + 1, n0ijkl)
i_aibibiaibm: do i = i0, n1ijkl
if (i == m) cycle i_aibibiaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibiaibm(nocc, a, i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibiaibm
end do a_aibibiaibm
end do b_aibibiaibm
end do m_aibibiaibm
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjaicj: do c = n0ce, n1ce
j_aiajcjaicj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjaicj: do a = a0, n1abd
if (a == c) cycle a_aiajcjaicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjaicj: do i = i0, n1il
if (i == j) cycle i_aiajcjaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjaicj(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjaicj
end do a_aiajcjaicj
end do j_aiajcjaicj
end do c_aiajcjaicj
!
! Elementary loop  20
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
i0 = max(j + 1, n0ikl)
i_aiajciaicj: do i = i0, n1ikl
if (i == j) cycle i_aiajciaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajciaicj(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajciaicj
end do a_aiajciaicj
end do j_aiajciaicj
end do c_aiajciaicj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, e == a, d == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aiajcjciaj: do c = n0cd, n1cd
j_aiajcjciaj: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abe)
a_aiajcjciaj: do a = n0abe, a1
if (a == c) cycle a_aiajcjciaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjciaj: do i = i0, n1il
if (i == j) cycle i_aiajcjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjciaj(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjciaj
end do a_aiajcjciaj
end do j_aiajcjciaj
end do c_aiajcjciaj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, e == a, d == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajciciaj: do c = n0cd, n1cd
j_aiajciciaj: do j = n0jm, n1jm
a1 = min(c - 1, n1abe)
a_aiajciciaj: do a = n0abe, a1
if (a == c) cycle a_aiajciciaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajciciaj: do i = i0, n1ikl
if (i == j) cycle i_aiajciciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajciciaj(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajciciaj
end do a_aiajciciaj
end do j_aiajciciaj
end do c_aiajciciaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == a, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiaialbi: do l = n0l, n1l
b_aibiaialbi: do b = n0be, n1be
a0 = max(b + 1, n0acd)
a_aibiaialbi: do a = a0, n1acd
if (a == b) cycle a_aibiaialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijkm)
i_aibiaialbi: do i = n0ijkm, i1
if (i == l) cycle i_aibiaialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaialbi(nocc, a, i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaialbi
end do a_aibiaialbi
end do b_aibiaialbi
end do l_aibiaialbi
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakakbi: do k = n0kl, n1kl
b_aibiakakbi: do b = n0be, n1be
a0 = max(b + 1, n0acd)
a_aibiakakbi: do a = a0, n1acd
if (a == b) cycle a_aibiakakbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijm)
i_aibiakakbi: do i = n0ijm, i1
if (i == k) cycle i_aibiakakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakakbi(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakakbi
end do a_aibiakakbi
end do b_aibiakakbi
end do k_aibiakakbi
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiajbi: do b = n0be, n1be
j_aibjaiajbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiajbi: do a = a0, n1acd
if (a == b) cycle a_aibjaiajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjaiajbi: do i = n0ikm, i1
if (i == j) cycle i_aibjaiajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiajbi(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiajbi
end do a_aibjaiajbi
end do j_aibjaiajbi
end do b_aibjaiajbi
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjajajbi: do b = n0be, n1be
j_aibjajajbi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajajbi: do a = a0, n1acd
if (a == b) cycle a_aibjajajbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjajajbi: do i = n0im, i1
if (i == j) cycle i_aibjajajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajajbi(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajajbi
end do a_aibjajajbi
end do j_aibjajajbi
end do b_aibjajajbi
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
b_aibjajaibj: do b = n0be, n1be
j_aibjajaibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajaibj: do a = a0, n1acd
if (a == b) cycle a_aibjajaibj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjajaibj: do i = i0, n1il
if (i == j) cycle i_aibjajaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajaibj(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajaibj
end do a_aibjajaibj
end do j_aibjajaibj
end do b_aibjajaibj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
b_aibjaiaibj: do b = n0be, n1be
j_aibjaiaibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiaibj: do a = a0, n1acd
if (a == b) cycle a_aibjaiaibj
i0 = max(j + 1, n0ikl)
i_aibjaiaibj: do i = i0, n1ikl
if (i == j) cycle i_aibjaiaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiaibj(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiaibj
end do a_aibjaiaibj
end do j_aibjaiaibj
end do b_aibjaiaibj
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibiakaibk: do k = n0km, n1km
b_aibiakaibk: do b = n0be, n1be
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibiakaibk: do a = a0, n1acd
if (a == b) cycle a_aibiakaibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakaibk: do i = i0, n1ijl
if (i == k) cycle i_aibiakaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakaibk(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakaibk
end do a_aibiakaibk
end do b_aibiakaibk
end do k_aibiakaibk
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, m
! Equalities: c == a, d == a, e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiaiaibm: do m = n0m, n1m
b_aibiaiaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibiaiaibm: do a = a0, n1acd
if (a == b) cycle a_aibiaiaibm
i0 = max(m + 1, n0ijkl)
i_aibiaiaibm: do i = i0, n1ijkl
if (i == m) cycle i_aibiaiaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaiaibm(nocc, a, i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaiaibm
end do a_aibiaiaibm
end do b_aibiaiaibm
end do m_aibiaiaibm
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
end subroutine ccjac_23_tripletm_dav_part11
end module ccjac_block_23_tripletm_dav_part11
