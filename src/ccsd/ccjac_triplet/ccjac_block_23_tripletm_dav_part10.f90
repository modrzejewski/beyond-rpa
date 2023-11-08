module ccjac_block_23_tripletm_dav_part10
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
subroutine ccjac_23_tripletm_dav_part10(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b1, i0, i1
integer :: n0ac, n0acd, n0ace, n0ad, n0ae
integer :: n0be, n0cd, n0ijkl, n0ijkm, n0ijl
integer :: n0ijm, n0ikl, n0ikm, n0il, n0im
integer :: n0jkl, n0jkm, n0jl, n0jm, n0kl
integer :: n0km
integer :: n1ac, n1acd, n1ace, n1ad, n1ae
integer :: n1be, n1cd, n1ijkl, n1ijkm, n1ijl
integer :: n1ijm, n1ikl, n1ikm, n1il, n1im
integer :: n1jkl, n1jkm, n1jl, n1jm, n1kl
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
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0ace = max(n0a, n0c, n0e)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
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
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
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
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: e == b, d == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjcicibj: do c = n0cd, n1cd
b1 = min(c - 1, n1be)
b_aibjcicibj: do b = n0be, b1
if (b == c) cycle b_aibjcicibj
j_aibjcicibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicibj
i0 = max(j + 1, n0ikl)
i_aibjcicibj: do i = i0, n1ikl
if (i == j) cycle i_aibjcicibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcicibj(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcicibj
end do a_aibjcicibj
end do j_aibjcicibj
end do b_aibjcicibj
end do c_aibjcicibj
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, m
! Equalities: e == b, d == c, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibicicibm: do m = n0m, n1m
c_aibicicibm: do c = n0cd, n1cd
b1 = min(c - 1, n1be)
b_aibicicibm: do b = n0be, b1
if (b == c) cycle b_aibicicibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibicicibm: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicicibm
i0 = max(m + 1, n0ijkl)
i_aibicicibm: do i = i0, n1ijkl
if (i == m) cycle i_aibicicibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicicibm(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicicibm
end do a_aibicicibm
end do b_aibicicibm
end do c_aibicicibm
end do m_aibicicibm
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibiaidlbi: do d = n0d, n1d
l_aibiaidlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibiaidlbi: do b = n0be, b1
if (b == d) cycle b_aibiaidlbi
a0 = max(b + 1, n0ac)
a_aibiaidlbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidlbi
i1 = min(l - 1, n1ijkm)
i_aibiaidlbi: do i = n0ijkm, i1
if (i == l) cycle i_aibiaidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaidlbi(nocc, a, i, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaidlbi
end do a_aibiaidlbi
end do b_aibiaidlbi
end do l_aibiaidlbi
end do d_aibiaidlbi
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibiakdkbi: do d = n0d, n1d
k_aibiakdkbi: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibiakdkbi: do b = n0be, b1
if (b == d) cycle b_aibiakdkbi
a0 = max(b + 1, n0ac)
a_aibiakdkbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdkbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijm)
i_aibiakdkbi: do i = n0ijm, i1
if (i == k) cycle i_aibiakdkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakdkbi(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakdkbi
end do a_aibiakdkbi
end do b_aibiakdkbi
end do k_aibiakdkbi
end do d_aibiakdkbi
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidjbi: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjaidjbi: do b = n0be, b1
if (b == d) cycle b_aibjaidjbi
j_aibjaidjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidjbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidjbi
i1 = min(j - 1, n1ikm)
i_aibjaidjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjaidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidjbi(nocc, a, i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidjbi
end do a_aibjaidjbi
end do j_aibjaidjbi
end do b_aibjaidjbi
end do d_aibjaidjbi
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjajdjbi: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjajdjbi: do b = n0be, b1
if (b == d) cycle b_aibjajdjbi
j_aibjajdjbi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajdjbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdjbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjajdjbi: do i = n0im, i1
if (i == j) cycle i_aibjajdjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajdjbi(i, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajdjbi
end do a_aibjajdjbi
end do j_aibjajdjbi
end do b_aibjajdjbi
end do d_aibjajdjbi
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjajdibj: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjajdibj: do b = n0be, b1
if (b == d) cycle b_aibjajdibj
j_aibjajdibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajdibj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdibj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjajdibj: do i = i0, n1il
if (i == j) cycle i_aibjajdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajdibj(i, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajdibj
end do a_aibjajdibj
end do j_aibjajdibj
end do b_aibjajdibj
end do d_aibjajdibj
!
! Elementary loop  8
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
a0 = max(b + 1, n0ac)
a_aibjaidibj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidibj
i0 = max(j + 1, n0ikl)
i_aibjaidibj: do i = i0, n1ikl
if (i == j) cycle i_aibjaidibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidibj(nocc, a, i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidibj
end do a_aibjaidibj
end do j_aibjaidibj
end do b_aibjaidibj
end do d_aibjaidibj
!
! Elementary loop  9
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
a0 = max(b + 1, n0ac)
a_aibiakdibk: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakdibk: do i = i0, n1ijl
if (i == k) cycle i_aibiakdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakdibk(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakdibk
end do a_aibiakdibk
end do b_aibiakdibk
end do k_aibiakdibk
end do d_aibiakdibk
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, m
! Equalities: c == a, e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiaidibm: do m = n0m, n1m
d_aibiaidibm: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibiaidibm: do b = n0be, b1
if (b == d) cycle b_aibiaidibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaidibm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidibm
i0 = max(m + 1, n0ijkl)
i_aibiaidibm: do i = i0, n1ijkl
if (i == m) cycle i_aibiaidibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaidibm(nocc, a, i, b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaidibm
end do a_aibiaidibm
end do b_aibiaidibm
end do d_aibiaidibm
end do m_aibiaidibm
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibicialbi: do l = n0l, n1l
c_aibicialbi: do c = n0c, n1c
b_aibicialbi: do b = n0be, n1be
if (b == c) cycle b_aibicialbi
a0 = max(b + 1, n0ad)
a_aibicialbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibicialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijkm)
i_aibicialbi: do i = n0ijkm, i1
if (i == l) cycle i_aibicialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibicialbi(a, b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibicialbi
end do a_aibicialbi
end do b_aibicialbi
end do c_aibicialbi
end do l_aibicialbi
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajbi: do c = n0c, n1c
b_aibjciajbi: do b = n0be, n1be
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjciajbi(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjciajbi
end do a_aibjciajbi
end do j_aibjciajbi
end do b_aibjciajbi
end do c_aibjciajbi
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjajbi: do c = n0c, n1c
b_aibjcjajbi: do b = n0be, n1be
if (b == c) cycle b_aibjcjajbi
j_aibjcjajbi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjajbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjcjajbi: do i = n0im, i1
if (i == j) cycle i_aibjcjajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjajbi(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjajbi
end do a_aibjcjajbi
end do j_aibjcjajbi
end do b_aibjcjajbi
end do c_aibjcjajbi
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjaibj: do c = n0c, n1c
b_aibjcjaibj: do b = n0be, n1be
if (b == c) cycle b_aibjcjaibj
j_aibjcjaibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjaibj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjaibj
i0 = max(j + 1, n0il)
i_aibjcjaibj: do i = i0, n1il
if (i == j) cycle i_aibjcjaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjaibj(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjaibj
end do a_aibjcjaibj
end do j_aibjcjaibj
end do b_aibjcjaibj
end do c_aibjcjaibj
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaibj: do c = n0c, n1c
b_aibjciaibj: do b = n0be, n1be
if (b == c) cycle b_aibjciaibj
j_aibjciaibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciaibj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaibj
i0 = max(j + 1, n0ikl)
i_aibjciaibj: do i = i0, n1ikl
if (i == j) cycle i_aibjciaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjciaibj(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjciaibj
end do a_aibjciaibj
end do j_aibjciaibj
end do b_aibjciaibj
end do c_aibjciaibj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, m
! Equalities: d == a, e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiciaibm: do m = n0m, n1m
c_aibiciaibm: do c = n0c, n1c
b_aibiciaibm: do b = n0be, n1be
if (b == c) cycle b_aibiciaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibiciaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibiciaibm
i0 = max(m + 1, n0ijkl)
i_aibiciaibm: do i = i0, n1ijkl
if (i == m) cycle i_aibiciaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiciaibm(a, b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiciaibm
end do a_aibiciaibm
end do b_aibiciaibm
end do c_aibiciaibm
end do m_aibiciaibm
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l
! Equalities: c == a, d == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiaialei: do e = n0e, n1e
l_aibiaialei: do l = n0l, n1l
b_aibiaialei: do b = n0b, n1b
if (b == e) cycle b_aibiaialei
a0 = max(b + 1, e + 1, n0acd)
a_aibiaialei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibiaialei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijkm)
i_aibiaialei: do i = n0ijkm, i1
if (i == l) cycle i_aibiaialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaialei(a, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaialei
end do a_aibiaialei
end do b_aibiaialei
end do l_aibiaialei
end do e_aibiaialei
!
! Elementary loop  18
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
a0 = max(b + 1, e + 1, n0acd)
a_aibjaiajei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjaiajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjaiajei: do i = n0ikm, i1
if (i == j) cycle i_aibjaiajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiajei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiajei
end do a_aibjaiajei
end do j_aibjaiajei
end do b_aibjaiajei
end do e_aibjaiajei
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjajajei: do e = n0e, n1e
b_aibjajajei: do b = n0b, n1b
if (b == e) cycle b_aibjajajei
j_aibjajajei: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjajajei: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjajajei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjajajei: do i = n0im, i1
if (i == j) cycle i_aibjajajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajajei(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajajei
end do a_aibjajajei
end do j_aibjajajei
end do b_aibjajajei
end do e_aibjajajei
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajaiej: do e = n0e, n1e
b_aibjajaiej: do b = n0b, n1b
if (b == e) cycle b_aibjajaiej
j_aibjajaiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjajaiej: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjajaiej
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjajaiej: do i = i0, n1il
if (i == j) cycle i_aibjajaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajaiej(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajaiej
end do a_aibjajaiej
end do j_aibjajaiej
end do b_aibjajaiej
end do e_aibjajaiej
!
! Elementary loop  21
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
a0 = max(b + 1, e + 1, n0acd)
a_aibjaiaiej: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjaiaiej
i0 = max(j + 1, n0ikl)
i_aibjaiaiej: do i = i0, n1ikl
if (i == j) cycle i_aibjaiaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaiaiej(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaiaiej
end do a_aibjaiaiej
end do j_aibjaiaiej
end do b_aibjaiaiej
end do e_aibjaiaiej
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, m
! Equalities: c == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibiaiaiem: do e = n0e, n1e
m_aibiaiaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibiaiaiem: do b = n0b, n1b
if (b == e) cycle b_aibiaiaiem
a0 = max(b + 1, e + 1, n0acd)
a_aibiaiaiem: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibiaiaiem
i0 = max(m + 1, n0ijkl)
i_aibiaiaiem: do i = i0, n1ijkl
if (i == m) cycle i_aibiaiaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaiaiem(a, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaiaiem
end do a_aibiaiaiem
end do b_aibiaiaiem
end do m_aibiaiaiem
end do e_aibiaiaiem
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, l
! Equalities: e == a, d == c, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibiciclai: do l = n0l, n1l
c_aibiciclai: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b_aibiciclai: do b = n0b, n1b
if (b == c) cycle b_aibiciclai
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibiciclai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibiciclai
i1 = min(l - 1, n1ijkm)
i_aibiciclai: do i = n0ijkm, i1
if (i == l) cycle i_aibiciclai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiciclai(b, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiciclai
end do a_aibiciclai
end do b_aibiciclai
end do c_aibiciclai
end do l_aibiciclai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjcjai: do c = n0cd, n1cd
b_aibjcjcjai: do b = n0b, n1b
if (b == c) cycle b_aibjcjcjai
j_aibjcjcjai: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjcjcjai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjcjai
i1 = min(j - 1, n1im)
i_aibjcjcjai: do i = n0im, i1
if (i == j) cycle i_aibjcjcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjcjai(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjcjai
end do a_aibjcjcjai
end do j_aibjcjcjai
end do b_aibjcjcjai
end do c_aibjcjcjai
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjciaj: do c = n0cd, n1cd
b_aibjcjciaj: do b = n0b, n1b
if (b == c) cycle b_aibjcjciaj
j_aibjcjciaj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjcjciaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjciaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjcjciaj: do i = i0, n1il
if (i == j) cycle i_aibjcjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjcjciaj(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjcjciaj
end do a_aibjcjciaj
end do j_aibjcjciaj
end do b_aibjcjciaj
end do c_aibjcjciaj
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, m
! Equalities: e == a, d == c, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibiciciam: do m = n0m, n1m
c_aibiciciam: do c = n0cd, n1cd
b_aibiciciam: do b = n0b, n1b
if (b == c) cycle b_aibiciciam
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibiciciam: do a = a0, a1
if (a == b .or. a == c) cycle a_aibiciciam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ijkl)
i_aibiciciam: do i = i0, n1ijkl
if (i == m) cycle i_aibiciciam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiciciam(b, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiciciam
end do a_aibiciciam
end do b_aibiciciam
end do c_aibiciciam
end do m_aibiciciam
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, e == a, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibiaidlai: do d = n0d, n1d
l_aibiaidlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidlai: do b = n0b, n1b
if (b == d) cycle b_aibiaidlai
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibiaidlai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibiaidlai
i1 = min(l - 1, n1ijkm)
i_aibiaidlai: do i = n0ijkm, i1
if (i == l) cycle i_aibiaidlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaidlai(a, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaidlai
end do a_aibiaidlai
end do b_aibiaidlai
end do l_aibiaidlai
end do d_aibiaidlai
!
! Elementary loop  28
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
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibjaidjai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjaidjai
i1 = min(j - 1, n1ikm)
i_aibjaidjai: do i = n0ikm, i1
if (i == j) cycle i_aibjaidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaidjai(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaidjai
end do a_aibjaidjai
end do j_aibjaidjai
end do b_aibjaidjai
end do d_aibjaidjai
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == a, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjajdjai: do d = n0d, n1d
b_aibjajdjai: do b = n0b, n1b
if (b == d) cycle b_aibjajdjai
j_aibjajdjai: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibjajdjai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjajdjai
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjajdjai: do i = n0im, i1
if (i == j) cycle i_aibjajdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajdjai(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajdjai
end do a_aibjajdjai
end do j_aibjajdjai
end do b_aibjajdjai
end do d_aibjajdjai
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjajdiaj: do d = n0d, n1d
b_aibjajdiaj: do b = n0b, n1b
if (b == d) cycle b_aibjajdiaj
j_aibjajdiaj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibjajdiaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjajdiaj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjajdiaj: do i = i0, n1il
if (i == j) cycle i_aibjajdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajdiaj(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajdiaj
end do a_aibjajdiaj
end do j_aibjajdiaj
end do b_aibjajdiaj
end do d_aibjajdiaj
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
end subroutine ccjac_23_tripletm_dav_part10
end module ccjac_block_23_tripletm_dav_part10
