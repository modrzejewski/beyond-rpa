module ccjac_block_23_tripletm_dav_part5
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
subroutine ccjac_23_tripletm_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b1, d0, i0, i1, j0, j1, l0
integer :: n0ac, n0acd, n0ace, n0ad, n0ae
integer :: n0be, n0cd, n0ce, n0ijk, n0ijkl
integer :: n0ijkm, n0ijl, n0ik, n0ikl, n0ikm
integer :: n0il, n0im, n0jk, n0jkl, n0jl
integer :: n0jm, n0km
integer :: n1ac, n1acd, n1ace, n1ad, n1ae
integer :: n1be, n1cd, n1ce, n1ijk, n1ijkl
integer :: n1ijkm, n1ijl, n1ik, n1ikl, n1ikm
integer :: n1il, n1im, n1jk, n1jkl, n1jl
integer :: n1jm, n1km
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
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0ace = max(n0a, n0c, n0e)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0ce = max(n0c, n0e)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijkm = max(n0i, n0j, n0k, n0m)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0km = max(n0k, n0m)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1ce = min(n1c, n1e)
n1ijk = min(n1i, n1j, n1k)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijkm = min(n1i, n1j, n1k, n1m)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, k, i
! Equalities: c == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjakdjbk: do d = n0d, n1d
k_aibjakdjbk: do k = n0km, n1km
b1 = min(d - 1, n1be)
b_aibjakdjbk: do b = n0be, b1
if (b == d) cycle b_aibjakdjbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjakdjbk: do j = j0, n1jl
if (j == k) cycle j_aibjakdjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdjbk: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdjbk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjakdjbk(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjakdjbk
end do a_aibjakdjbk
end do j_aibjakdjbk
end do b_aibjakdjbk
end do k_aibjakdjbk
end do d_aibjakdjbk
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, m
! Equalities: c == a, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibiakdibm: do m = n0m, n1m
d_aibiakdibm: do d = n0d, n1d
k_aibiakdibm: do k = n0k, n1k
if (k == m) cycle k_aibiakdibm
b1 = min(d - 1, n1be)
b_aibiakdibm: do b = n0be, b1
if (b == d) cycle b_aibiakdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiakdibm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(m + 1, n0ijl)
i_aibiakdibm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibiakdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakdibm(i, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakdibm
end do a_aibiakdibm
end do b_aibiakdibm
end do k_aibiakdibm
end do d_aibiakdibm
end do m_aibiakdibm
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: c == a, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjaidibm: do m = n0m, n1m
d_aibjaidibm: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjaidibm: do b = n0be, b1
if (b == d) cycle b_aibjaidibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaidibm: do j = n0j, n1j
if (j == m) cycle j_aibjaidibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidibm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidibm
i0 = max(m + 1, n0ikl)
i_aibjaidibm: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjaidibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidibm(i, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidibm
end do a_aibjaidibm
end do j_aibjaidibm
end do b_aibjaidibm
end do d_aibjaidibm
end do m_aibjaidibm
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l, m
! Equalities: c == a, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibiaidlbm: do m = n0m, n1m
d_aibiaidlbm: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aibiaidlbm: do l = l0, n1l
if (l == m) cycle l_aibiaidlbm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibiaidlbm: do b = n0be, b1
if (b == d) cycle b_aibiaidlbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaidlbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidlbm
i_aibiaidlbm: do i = n0ijk, n1ijk
if (i == l .or. i == m) cycle i_aibiaidlbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaidlbm(i, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaidlbm
end do a_aibiaidlbm
end do b_aibiaidlbm
end do l_aibiaidlbm
end do d_aibiaidlbm
end do m_aibiaidlbm
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: c == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaidjbm: do m = n0m, n1m
d_aibjaidjbm: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjaidjbm: do b = n0be, b1
if (b == d) cycle b_aibjaidjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjaidjbm: do j = j0, n1jl
if (j == m) cycle j_aibjaidjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidjbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidjbm
i_aibjaidjbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjaidjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidjbm(nocc, a, i, b, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidjbm
end do a_aibjaidjbm
end do j_aibjaidjbm
end do b_aibjaidjbm
end do d_aibjaidjbm
end do m_aibjaidjbm
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, m
! Equalities: c == a, e == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjajdjbm: do m = n0m, n1m
d_aibjajdjbm: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjajdjbm: do b = n0be, b1
if (b == d) cycle b_aibjajdjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jkl)
j_aibjajdjbm: do j = j0, n1jkl
if (j == m) cycle j_aibjajdjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajdjbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdjbm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdjbm: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjajdjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajdjbm(i, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajdjbm
end do a_aibjajdjbm
end do j_aibjajdjbm
end do b_aibjajdjbm
end do d_aibjajdjbm
end do m_aibjajdjbm
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjalbi: do l = n0l, n1l
c_aibjcjalbi: do c = n0c, n1c
b_aibjcjalbi: do b = n0be, n1be
if (b == c) cycle b_aibjcjalbi
j_aibjcjalbi: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjalbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjcjalbi: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjcjalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjalbi(b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjalbi
end do a_aibjcjalbi
end do j_aibjcjalbi
end do b_aibjcjalbi
end do c_aibjcjalbi
end do l_aibjcjalbi
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjcialbj: do l = n0l, n1l
c_aibjcialbj: do c = n0c, n1c
b_aibjcialbj: do b = n0be, n1be
if (b == c) cycle b_aibjcialbj
j1 = min(l - 1, n1jm)
j_aibjcialbj: do j = n0jm, j1
if (j == l) cycle j_aibjcialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcialbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcialbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcialbj(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcialbj
end do a_aibjcialbj
end do j_aibjcialbj
end do b_aibjcialbj
end do c_aibjcialbj
end do l_aibjcialbj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaibm: do m = n0m, n1m
c_aibjcjaibm: do c = n0c, n1c
b_aibjcjaibm: do b = n0be, n1be
if (b == c) cycle b_aibjcjaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjaibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaibm
i0 = max(m + 1, n0il)
i_aibjcjaibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjaibm(b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjaibm
end do a_aibjcjaibm
end do j_aibjcjaibm
end do b_aibjcjaibm
end do c_aibjcjaibm
end do m_aibjcjaibm
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjciajbm: do m = n0m, n1m
c_aibjciajbm: do c = n0c, n1c
b_aibjciajbm: do b = n0be, n1be
if (b == c) cycle b_aibjciajbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjciajbm: do j = j0, n1jl
if (j == m) cycle j_aibjciajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciajbm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciajbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjciajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjciajbm(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjciajbm
end do a_aibjciajbm
end do j_aibjciajbm
end do b_aibjciajbm
end do c_aibjciajbm
end do m_aibjciajbm
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, l
! Equalities: e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibicidlbi: do d = n0d, n1d
l_aibicidlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibicidlbi: do c = n0c, n1c
if (c == d) cycle c_aibicidlbi
b1 = min(d - 1, n1be)
b_aibicidlbi: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibicidlbi
a0 = max(b + 1, n0a)
a_aibicidlbi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibicidlbi
i1 = min(l - 1, n1ijkm)
i_aibicidlbi: do i = n0ijkm, i1
if (i == l) cycle i_aibicidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicidlbi(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicidlbi
end do a_aibicidlbi
end do b_aibicidlbi
end do c_aibicidlbi
end do l_aibicidlbi
end do d_aibicidlbi
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidjbi: do d = n0d, n1d
c_aibjcidjbi: do c = n0c, n1c
if (c == d) cycle c_aibjcidjbi
b1 = min(d - 1, n1be)
b_aibjcidjbi: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidjbi
j_aibjcidjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidjbi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjbi
i1 = min(j - 1, n1ikm)
i_aibjcidjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjcidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcidjbi(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcidjbi
end do a_aibjcidjbi
end do j_aibjcidjbi
end do b_aibjcidjbi
end do c_aibjcidjbi
end do d_aibjcidjbi
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidibj: do d = n0d, n1d
c_aibjcidibj: do c = n0c, n1c
if (c == d) cycle c_aibjcidibj
b1 = min(d - 1, n1be)
b_aibjcidibj: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidibj
j_aibjcidibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidibj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidibj
i0 = max(j + 1, n0ikl)
i_aibjcidibj: do i = i0, n1ikl
if (i == j) cycle i_aibjcidibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcidibj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcidibj
end do a_aibjcidibj
end do j_aibjcidibj
end do b_aibjcidibj
end do c_aibjcidibj
end do d_aibjcidibj
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, m
! Equalities: e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibicidibm: do m = n0m, n1m
d_aibicidibm: do d = n0d, n1d
c_aibicidibm: do c = n0c, n1c
if (c == d) cycle c_aibicidibm
b1 = min(d - 1, n1be)
b_aibicidibm: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibicidibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibicidibm: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibicidibm
i0 = max(m + 1, n0ijkl)
i_aibicidibm: do i = i0, n1ijkl
if (i == m) cycle i_aibicidibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicidibm(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicidibm
end do a_aibicidibm
end do b_aibicidibm
end do c_aibicidibm
end do d_aibicidibm
end do m_aibicidibm
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajalei: do e = n0e, n1e
l_aibjajalei: do l = n0l, n1l
b_aibjajalei: do b = n0b, n1b
if (b == e) cycle b_aibjajalei
j_aibjajalei: do j = n0jk, n1jk
if (j == l) cycle j_aibjajalei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjajalei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjajalei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjajalei: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjajalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajalei(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajalei
end do a_aibjajalei
end do j_aibjajalei
end do b_aibjajalei
end do l_aibjajalei
end do e_aibjajalei
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaialej: do e = n0e, n1e
l_aibjaialej: do l = n0l, n1l
b_aibjaialej: do b = n0b, n1b
if (b == e) cycle b_aibjaialej
j1 = min(l - 1, n1jm)
j_aibjaialej: do j = n0jm, j1
if (j == l) cycle j_aibjaialej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjaialej: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjaialej
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjaialej: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaialej(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaialej
end do a_aibjaialej
end do j_aibjaialej
end do b_aibjaialej
end do l_aibjaialej
end do e_aibjaialej
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajaiem: do e = n0e, n1e
m_aibjajaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjajaiem: do b = n0b, n1b
if (b == e) cycle b_aibjajaiem
j_aibjajaiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjajaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjajaiem: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjajaiem
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(m + 1, n0il)
i_aibjajaiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjajaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajaiem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajaiem
end do a_aibjajaiem
end do j_aibjajaiem
end do b_aibjajaiem
end do m_aibjajaiem
end do e_aibjajaiem
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaiajem: do e = n0e, n1e
m_aibjaiajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjaiajem: do b = n0b, n1b
if (b == e) cycle b_aibjaiajem
j0 = max(m + 1, n0jl)
j_aibjaiajem: do j = j0, n1jl
if (j == m) cycle j_aibjaiajem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjaiajem: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjaiajem
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaiajem: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjaiajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiajem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiajem
end do a_aibjaiajem
end do j_aibjaiajem
end do b_aibjaiajem
end do m_aibjaiajem
end do e_aibjaiajem
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: e == a, d == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjclai: do l = n0l, n1l
c_aibjcjclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibjcjclai: do b = n0b, n1b
if (b == c) cycle b_aibjcjclai
j_aibjcjclai: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjclai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjcjclai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjclai
i1 = min(l - 1, n1im)
i_aibjcjclai: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjcjclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjclai(b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjclai
end do a_aibjcjclai
end do j_aibjcjclai
end do b_aibjcjclai
end do c_aibjcjclai
end do l_aibjcjclai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: e == a, d == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjciam: do m = n0m, n1m
c_aibjcjciam: do c = n0cd, n1cd
b_aibjcjciam: do b = n0b, n1b
if (b == c) cycle b_aibjcjciam
j_aibjcjciam: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjciam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjcjciam: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjciam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0il)
i_aibjcjciam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjciam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjciam(b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjciam
end do a_aibjcjciam
end do j_aibjcjciam
end do b_aibjcjciam
end do c_aibjcjciam
end do m_aibjcjciam
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == a, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdlai: do d = n0d, n1d
l_aibjajdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjajdlai: do b = n0b, n1b
if (b == d) cycle b_aibjajdlai
j_aibjajdlai: do j = n0jk, n1jk
if (j == l) cycle j_aibjajdlai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibjajdlai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjajdlai
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjajdlai: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjajdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajdlai(a, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajdlai
end do a_aibjajdlai
end do j_aibjajdlai
end do b_aibjajdlai
end do l_aibjajdlai
end do d_aibjajdlai
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjaidlaj: do d = n0d, n1d
l_aibjaidlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidlaj: do b = n0b, n1b
if (b == d) cycle b_aibjaidlaj
j1 = min(l - 1, n1jm)
j_aibjaidlaj: do j = n0jm, j1
if (j == l) cycle j_aibjaidlaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibjaidlaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjaidlaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjaidlaj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaidlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidlaj(a, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidlaj
end do a_aibjaidlaj
end do j_aibjaidlaj
end do b_aibjaidlaj
end do l_aibjaidlaj
end do d_aibjaidlaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: c == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajdiam: do m = n0m, n1m
d_aibjajdiam: do d = n0d, n1d
b_aibjajdiam: do b = n0b, n1b
if (b == d) cycle b_aibjajdiam
j_aibjajdiam: do j = n0jk, n1jk
if (j == m) cycle j_aibjajdiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibjajdiam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjajdiam
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0il)
i_aibjajdiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjajdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajdiam(a, b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajdiam
end do a_aibjajdiam
end do j_aibjajdiam
end do b_aibjajdiam
end do d_aibjajdiam
end do m_aibjajdiam
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: c == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaidjam: do m = n0m, n1m
d_aibjaidjam: do d = n0d, n1d
b_aibjaidjam: do b = n0b, n1b
if (b == d) cycle b_aibjaidjam
j0 = max(m + 1, n0jl)
j_aibjaidjam: do j = j0, n1jl
if (j == m) cycle j_aibjaidjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibjaidjam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjaidjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjaidjam: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjaidjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidjam(a, b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidjam
end do a_aibjaidjam
end do j_aibjaidjam
end do b_aibjaidjam
end do d_aibjaidjam
end do m_aibjaidjam
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, l
! Equalities: c == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiaidlei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibiaidlei: do d = d0, n1d
if (d == e) cycle d_aibiaidlei
l_aibiaidlei: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidlei: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibiaidlei
a0 = max(b + 1, n0ac)
a_aibiaidlei: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibiaidlei
i1 = min(l - 1, n1ijkm)
i_aibiaidlei: do i = n0ijkm, i1
if (i == l) cycle i_aibiaidlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaidlei(b, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaidlei
end do a_aibiaidlei
end do b_aibiaidlei
end do l_aibiaidlei
end do d_aibiaidlei
end do e_aibiaidlei
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j
! Equalities: c == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaidjei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjaidjei: do d = d0, n1d
if (d == e) cycle d_aibjaidjei
b_aibjaidjei: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjaidjei
j_aibjaidjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidjei: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidjei
i1 = min(j - 1, n1ikm)
i_aibjaidjei: do i = n0ikm, i1
if (i == j) cycle i_aibjaidjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidjei(i, b, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidjei
end do a_aibjaidjei
end do j_aibjaidjei
end do b_aibjaidjei
end do d_aibjaidjei
end do e_aibjaidjei
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaidiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjaidiej: do d = d0, n1d
if (d == e) cycle d_aibjaidiej
b_aibjaidiej: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjaidiej
j_aibjaidiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidiej: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidiej
i0 = max(j + 1, n0ikl)
i_aibjaidiej: do i = i0, n1ikl
if (i == j) cycle i_aibjaidiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidiej(i, b, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidiej
end do a_aibjaidiej
end do j_aibjaidiej
end do b_aibjaidiej
end do d_aibjaidiej
end do e_aibjaidiej
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, m
! Equalities: c == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibiaidiem: do e = n0e, n1e
m_aibiaidiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aibiaidiem: do d = d0, n1d
if (d == e) cycle d_aibiaidiem
b_aibiaidiem: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibiaidiem
a0 = max(b + 1, n0ac)
a_aibiaidiem: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibiaidiem
i0 = max(m + 1, n0ijkl)
i_aibiaidiem: do i = i0, n1ijkl
if (i == m) cycle i_aibiaidiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaidiem(b, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaidiem
end do a_aibiaidiem
end do b_aibiaidiem
end do d_aibiaidiem
end do m_aibiaidiem
end do e_aibiaidiem
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, l
! Equalities: d == a, e == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjcjalci: do l = n0l, n1l
c_aibjcjalci: do c = n0ce, n1ce
b_aibjcjalci: do b = n0b, n1b
if (b == c) cycle b_aibjcjalci
j_aibjcjalci: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjalci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjalci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjalci
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjcjalci: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjcjalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjalci(b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjalci
end do a_aibjcjalci
end do j_aibjcjalci
end do b_aibjcjalci
end do c_aibjcjalci
end do l_aibjcjalci
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, m
! Equalities: d == a, e == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjaicm: do m = n0m, n1m
c_aibjcjaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
b_aibjcjaicm: do b = n0b, n1b
if (b == c) cycle b_aibjcjaicm
j_aibjcjaicm: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjaicm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjcjaicm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaicm
i0 = max(m + 1, n0il)
i_aibjcjaicm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjaicm(b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjaicm
end do a_aibjcjaicm
end do j_aibjcjaicm
end do b_aibjcjaicm
end do c_aibjcjaicm
end do m_aibjcjaicm
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
end subroutine ccjac_23_tripletm_dav_part5
end module ccjac_block_23_tripletm_dav_part5
