module ccjac_block_23_tripletm_dav_part8
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
subroutine ccjac_23_tripletm_dav_part8(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, i0, i1, j0, j1
integer :: n0abd, n0ac, n0ad, n0ae, n0bc
integer :: n0bce, n0bd, n0ce, n0ijkl, n0ijkm
integer :: n0ijl, n0ijm, n0ik, n0ikl, n0ikm
integer :: n0il, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jm, n0kl, n0km
integer :: n1abd, n1ac, n1ad, n1ae, n1bc
integer :: n1bce, n1bd, n1ce, n1ijkl, n1ijkm
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
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bce = max(n0b, n0c, n0e)
n0bd = max(n0b, n0d)
n0ce = max(n0c, n0e)
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
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bce = min(n1b, n1c, n1e)
n1bd = min(n1b, n1d)
n1ce = min(n1c, n1e)
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
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjbjdibj: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibjbjdibj: do b = n0bce, b1
if (b == d) cycle b_aibjbjdibj
j_aibjbjdibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdibj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdibj
i0 = max(j + 1, n0il)
i_aibjbjdibj: do i = i0, n1il
if (i == j) cycle i_aibjbjdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdibj(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdibj
end do a_aibjbjdibj
end do j_aibjbjdibj
end do b_aibjbjdibj
end do d_aibjbjdibj
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbidibj: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibjbidibj: do b = n0bce, b1
if (b == d) cycle b_aibjbidibj
j_aibjbidibj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidibj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidibj
i0 = max(j + 1, n0ikl)
i_aibjbidibj: do i = i0, n1ikl
if (i == j) cycle i_aibjbidibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbidibj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbidibj
end do a_aibjbidibj
end do j_aibjbidibj
end do b_aibjbidibj
end do d_aibjbidibj
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, m
! Equalities: c == b, e == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibibidibm: do m = n0m, n1m
d_aibibidibm: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibibidibm: do b = n0bce, b1
if (b == d) cycle b_aibibidibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibidibm: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibidibm
i0 = max(m + 1, n0ijkl)
i_aibibidibm: do i = i0, n1ijkl
if (i == m) cycle i_aibibidibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibidibm(a, b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibidibm
end do a_aibibidibm
end do b_aibibidibm
end do d_aibibidibm
end do m_aibibidibm
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l
! Equalities: d == a, c == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibibialei: do e = n0e, n1e
l_aibibialei: do l = n0l, n1l
b_aibibialei: do b = n0bc, n1bc
if (b == e) cycle b_aibibialei
a0 = max(b + 1, e + 1, n0ad)
a_aibibialei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibialei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijkm)
i_aibibialei: do i = n0ijkm, i1
if (i == l) cycle i_aibibialei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibialei(nocc, a, i, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibialei
end do a_aibibialei
end do b_aibibialei
end do l_aibibialei
end do e_aibibialei
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
e_aibibkakei: do e = n0e, n1e
k_aibibkakei: do k = n0kl, n1kl
b_aibibkakei: do b = n0bc, n1bc
if (b == e) cycle b_aibibkakei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibibkakei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibkakei
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijm)
i_aibibkakei: do i = n0ijm, i1
if (i == k) cycle i_aibibkakei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (e - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkakei(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkakei
end do a_aibibkakei
end do b_aibibkakei
end do k_aibibkakei
end do e_aibibkakei
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbiajei: do e = n0e, n1e
b_aibjbiajei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbiajei
j_aibjbiajei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbiajei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbiajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjbiajei: do i = n0ikm, i1
if (i == j) cycle i_aibjbiajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbiajei(i, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbiajei
end do a_aibjbiajei
end do j_aibjbiajei
end do b_aibjbiajei
end do e_aibjbiajei
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjbjajei: do e = n0e, n1e
b_aibjbjajei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjajei
j_aibjbjajei: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjajei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjajei
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjbjajei: do i = n0im, i1
if (i == j) cycle i_aibjbjajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjajei(nocc, a, i, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjajei
end do a_aibjbjajei
end do j_aibjbjajei
end do b_aibjbjajei
end do e_aibjbjajei
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjbjaiej: do e = n0e, n1e
b_aibjbjaiej: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjaiej
j_aibjbjaiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjaiej: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjaiej
i0 = max(j + 1, n0il)
i_aibjbjaiej: do i = i0, n1il
if (i == j) cycle i_aibjbjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjaiej(nocc, a, i, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjaiej
end do a_aibjbjaiej
end do j_aibjbjaiej
end do b_aibjbjaiej
end do e_aibjbjaiej
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: d == a, c == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbiaiej: do e = n0e, n1e
b_aibjbiaiej: do b = n0bc, n1bc
if (b == e) cycle b_aibjbiaiej
j_aibjbiaiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbiaiej: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbiaiej
i0 = max(j + 1, n0ikl)
i_aibjbiaiej: do i = i0, n1ikl
if (i == j) cycle i_aibjbiaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbiaiej(i, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbiaiej
end do a_aibjbiaiej
end do j_aibjbiaiej
end do b_aibjbiaiej
end do e_aibjbiaiej
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: d == a, c == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibibkaiek: do e = n0e, n1e
k_aibibkaiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkaiek: do b = n0bc, n1bc
if (b == e) cycle b_aibibkaiek
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibibkaiek: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibkaiek
i0 = max(k + 1, n0ijl)
i_aibibkaiek: do i = i0, n1ijl
if (i == k) cycle i_aibibkaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkaiek(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkaiek
end do a_aibibkaiek
end do b_aibibkaiek
end do k_aibibkaiek
end do e_aibibkaiek
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, m
! Equalities: d == a, c == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
e_aibibiaiem: do e = n0e, n1e
m_aibibiaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibibiaiem: do b = n0bc, n1bc
if (b == e) cycle b_aibibiaiem
a0 = max(b + 1, e + 1, n0ad)
a_aibibiaiem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibibiaiem
i0 = max(m + 1, n0ijkl)
i_aibibiaiem: do i = i0, n1ijkl
if (i == m) cycle i_aibibiaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibiaiem(nocc, a, i, b, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibiaiem
end do a_aibibiaiem
end do b_aibibiaiem
end do m_aibibiaiem
end do e_aibibiaiem
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: e == a, c == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibibidlai: do d = n0d, n1d
l_aibibidlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibibidlai: do b = n0bc, n1bc
if (b == d) cycle b_aibibidlai
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibidlai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibidlai
i1 = min(l - 1, n1ijkm)
i_aibibidlai: do i = n0ijkm, i1
if (i == l) cycle i_aibibidlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibidlai(nocc, a, i, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibidlai
end do a_aibibidlai
end do b_aibibidlai
end do l_aibibidlai
end do d_aibibidlai
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: e == a, c == b, j == i, m == i, l == k
! No equalities independent of the above can hold.
!
d_aibibkdkai: do d = n0d, n1d
k_aibibkdkai: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkdkai: do b = n0bc, n1bc
if (b == d) cycle b_aibibkdkai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibkdkai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibkdkai
i1 = min(k - 1, n1ijm)
i_aibibkdkai: do i = n0ijm, i1
if (i == k) cycle i_aibibkdkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkdkai(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkdkai
end do a_aibibkdkai
end do b_aibibkdkai
end do k_aibibkdkai
end do d_aibibkdkai
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidjai: do d = n0d, n1d
b_aibjbidjai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidjai
j_aibjbidjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidjai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidjai
i1 = min(j - 1, n1ikm)
i_aibjbidjai: do i = n0ikm, i1
if (i == j) cycle i_aibjbidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbidjai(i, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbidjai
end do a_aibjbidjai
end do j_aibjbidjai
end do b_aibjbidjai
end do d_aibjbidjai
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjbjdjai: do d = n0d, n1d
b_aibjbjdjai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdjai
j_aibjbjdjai: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdjai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdjai
i1 = min(j - 1, n1im)
i_aibjbjdjai: do i = n0im, i1
if (i == j) cycle i_aibjbjdjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdjai(nocc, a, i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdjai
end do a_aibjbjdjai
end do j_aibjbjdjai
end do b_aibjbjdjai
end do d_aibjbjdjai
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjbjdiaj: do d = n0d, n1d
b_aibjbjdiaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdiaj
j_aibjbjdiaj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdiaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjbjdiaj: do i = i0, n1il
if (i == j) cycle i_aibjbjdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdiaj(nocc, a, i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdiaj
end do a_aibjbjdiaj
end do j_aibjbjdiaj
end do b_aibjbjdiaj
end do d_aibjbjdiaj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: e == a, c == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbidiaj: do d = n0d, n1d
b_aibjbidiaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidiaj
j_aibjbidiaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidiaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aibjbidiaj: do i = i0, n1ikl
if (i == j) cycle i_aibjbidiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbidiaj(i, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbidiaj
end do a_aibjbidiaj
end do j_aibjbidiaj
end do b_aibjbidiaj
end do d_aibjbidiaj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: e == a, c == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibibkdiak: do d = n0d, n1d
k_aibibkdiak: do k = n0km, n1km
b_aibibkdiak: do b = n0bc, n1bc
if (b == d) cycle b_aibibkdiak
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibkdiak: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibkdiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibibkdiak: do i = i0, n1ijl
if (i == k) cycle i_aibibkdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkdiak(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkdiak
end do a_aibibkdiak
end do b_aibibkdiak
end do k_aibibkdiak
end do d_aibibkdiak
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, m
! Equalities: e == a, c == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibibidiam: do m = n0m, n1m
d_aibibidiam: do d = n0d, n1d
b_aibibidiam: do b = n0bc, n1bc
if (b == d) cycle b_aibibidiam
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibibidiam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibibidiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(m + 1, n0ijkl)
i_aibibidiam: do i = i0, n1ijkl
if (i == m) cycle i_aibibidiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibidiam(nocc, a, i, b, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibidiam
end do a_aibibidiam
end do b_aibibidiam
end do d_aibibidiam
end do m_aibibidiam
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, m == i, k == j
! No equalities independent of the above can hold.
!
l_aiajcjalci: do l = n0l, n1l
c_aiajcjalci: do c = n0ce, n1ce
j_aiajcjalci: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjalci
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjalci: do a = a0, n1abd
if (a == c) cycle a_aiajcjalci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aiajcjalci: do i = i0, i1
if (i == j .or. i == l) cycle i_aiajcjalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjalci(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjalci
end do a_aiajcjalci
end do j_aiajcjalci
end do c_aiajcjalci
end do l_aiajcjalci
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, k == i, m == j
! No equalities independent of the above can hold.
!
l_aiajcialcj: do l = n0l, n1l
c_aiajcialcj: do c = n0ce, n1ce
j1 = min(l - 1, n1jm)
j_aiajcialcj: do j = n0jm, j1
if (j == l) cycle j_aiajcialcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcialcj: do a = a0, n1abd
if (a == c) cycle a_aiajcialcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcialcj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcialcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcialcj(a, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcialcj
end do a_aiajcialcj
end do j_aiajcialcj
end do c_aiajcialcj
end do l_aiajcialcj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajcjaicm: do m = n0m, n1m
c_aiajcjaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajcjaicm: do j = n0jk, n1jk
if (j == m) cycle j_aiajcjaicm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajcjaicm: do a = a0, n1abd
if (a == c) cycle a_aiajcjaicm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajcjaicm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajcjaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjaicm(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjaicm
end do a_aiajcjaicm
end do j_aiajcjaicm
end do c_aiajcjaicm
end do m_aiajcjaicm
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajciajcm: do m = n0m, n1m
c_aiajciajcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aiajciajcm: do j = j0, n1jl
if (j == m) cycle j_aiajciajcm
a0 = max(c + 1, n0abd)
a_aiajciajcm: do a = a0, n1abd
if (a == c) cycle a_aiajciajcm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajciajcm: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajciajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajciajcm(a, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajciajcm
end do a_aiajciajcm
end do j_aiajciajcm
end do c_aiajciajcm
end do m_aiajciajcm
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajcjaiej: do e = n0e, n1e
c_aiajcjaiej: do c = n0c, n1c
if (c == e) cycle c_aiajcjaiej
j_aiajcjaiej: do j = n0jkm, n1jkm
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0abd)
a_aiajcjaiej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajcjaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjaiej: do i = i0, n1il
if (i == j) cycle i_aiajcjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajcjaiej(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajcjaiej
end do a_aiajcjaiej
end do j_aiajcjaiej
end do c_aiajcjaiej
end do e_aiajcjaiej
!
! Elementary loop  25
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
a0 = max(e + 1, n0abd)
a_aiajciaiej: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajciaiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajciaiej: do i = i0, n1ikl
if (i == j) cycle i_aiajciaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajciaiej(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajciaiej
end do a_aiajciaiej
end do j_aiajciaiej
end do c_aiajciaiej
end do e_aiajciaiej
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
e_aibiaiblei: do e = n0e, n1e
l_aibiaiblei: do l = n0l, n1l
b0 = max(e + 1, n0bd)
b_aibiaiblei: do b = b0, n1bd
if (b == e) cycle b_aibiaiblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiaiblei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiaiblei
i1 = min(l - 1, n1ijkm)
i_aibiaiblei: do i = n0ijkm, i1
if (i == l) cycle i_aibiaiblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiaiblei(nocc, a, i, b, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiaiblei
end do a_aibiaiblei
end do b_aibiaiblei
end do l_aibiaiblei
end do e_aibiaiblei
!
! Elementary loop  27
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
a0 = max(b + 1, n0ac)
a_aibiakbkei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibiakbkei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1ijm)
i_aibiakbkei: do i = n0ijm, i1
if (i == k) cycle i_aibiakbkei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (e - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibiakbkei(i, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibiakbkei
end do a_aibiakbkei
end do b_aibiakbkei
end do k_aibiakbkei
end do e_aibiakbkei
!
! Elementary loop  28
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
a0 = max(b + 1, n0ac)
a_aibjaibjei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaibjei
i1 = min(j - 1, n1ikm)
i_aibjaibjei: do i = n0ikm, i1
if (i == j) cycle i_aibjaibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjaibjei(nocc, a, i, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjaibjei
end do a_aibjaibjei
end do j_aibjaibjei
end do b_aibjaibjei
end do e_aibjaibjei
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
e_aibjajbjei: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjajbjei: do b = b0, n1bd
if (b == e) cycle b_aibjajbjei
j_aibjajbjei: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbjei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajbjei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjajbjei: do i = n0im, i1
if (i == j) cycle i_aibjajbjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajbjei(i, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajbjei
end do a_aibjajbjei
end do j_aibjajbjei
end do b_aibjajbjei
end do e_aibjajbjei
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjajbiej: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjajbiej: do b = b0, n1bd
if (b == e) cycle b_aibjajbiej
j_aibjajbiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbiej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajbiej
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjajbiej: do i = i0, n1il
if (i == j) cycle i_aibjajbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjajbiej(i, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjajbiej
end do a_aibjajbiej
end do j_aibjajbiej
end do b_aibjajbiej
end do e_aibjajbiej
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
end subroutine ccjac_23_tripletm_dav_part8
end module ccjac_block_23_tripletm_dav_part8
