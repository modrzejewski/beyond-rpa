module ccjac_block_23_tripletm_dav_part7
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
subroutine ccjac_23_tripletm_dav_part7(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a, b, d, e
integer :: i, j, k, l, m
integer :: ai, bj, ck, dl, em
integer :: a0, a1, b1, d0, i0, i1, j0, j1, l0
integer :: n0abc, n0abce, n0ad, n0bce, n0ijk
integer :: n0ijkm, n0ijl, n0ijm, n0ik, n0ikl
integer :: n0ikm, n0il, n0im, n0jk, n0jkl
integer :: n0jkm, n0jl, n0jm, n0kl, n0km
integer :: n1abc, n1abce, n1ad, n1bce, n1ijk
integer :: n1ijkm, n1ijl, n1ijm, n1ik, n1ikl
integer :: n1ikm, n1il, n1im, n1jk, n1jkl
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
n0abc = max(n0a, n0b, n0c)
n0abce = max(n0a, n0b, n0c, n0e)
n0ad = max(n0a, n0d)
n0bce = max(n0b, n0c, n0e)
n0ijk = max(n0i, n0j, n0k)
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
n1abc = min(n1a, n1b, n1c)
n1abce = min(n1a, n1b, n1c, n1e)
n1ad = min(n1a, n1d)
n1bce = min(n1b, n1c, n1e)
n1ijk = min(n1i, n1j, n1k)
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
! Free virtual indices: a, d
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, e == a, k == j, m == j
! No equalities independent of the above can hold.
!
d_aiajajdlaj: do d = n0d, n1d
l_aiajajdlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jkm)
j_aiajajdlaj: do j = n0jkm, j1
if (j == l) cycle j_aiajajdlaj
a1 = min(d - 1, n1abce)
a_aiajajdlaj: do a = n0abce, a1
if (a == d) cycle a_aiajajdlaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajdlaj: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajajdlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajdlaj(i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajdlaj
end do a_aiajajdlaj
end do j_aiajajdlaj
end do l_aiajajdlaj
end do d_aiajajdlaj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aiajakdiaj: do d = n0d, n1d
k_aiajakdiaj: do k = n0k, n1k
j_aiajakdiaj: do j = n0jm, n1jm
if (j == k) cycle j_aiajakdiaj
a1 = min(d - 1, n1abce)
a_aiajakdiaj: do a = n0abce, a1
if (a == d) cycle a_aiajakdiaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajakdiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajakdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakdiaj(i, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakdiaj
end do a_aiajakdiaj
end do j_aiajakdiaj
end do k_aiajakdiaj
end do d_aiajakdiaj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, e == a, k == i, m == j
! No equalities independent of the above can hold.
!
d_aiajaidlaj: do d = n0d, n1d
l_aiajaidlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aiajaidlaj: do j = n0jm, j1
if (j == l) cycle j_aiajaidlaj
a1 = min(d - 1, n1abce)
a_aiajaidlaj: do a = n0abce, a1
if (a == d) cycle a_aiajaidlaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidlaj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaidlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaidlaj(nocc, a, i, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaidlaj
end do a_aiajaidlaj
end do j_aiajaidlaj
end do l_aiajaidlaj
end do d_aiajaidlaj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, k, i
! Equalities: b == a, c == a, e == a, m == j, l == k
! No equalities independent of the above can hold.
!
d_aiajakdkaj: do d = n0d, n1d
k_aiajakdkaj: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aiajakdkaj: do j = n0jm, j1
if (j == k) cycle j_aiajakdkaj
a1 = min(d - 1, n1abce)
a_aiajakdkaj: do a = n0abce, a1
if (a == d) cycle a_aiajakdkaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakdkaj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakdkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakdkaj(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakdkaj
end do a_aiajakdkaj
end do j_aiajakdkaj
end do k_aiajakdkaj
end do d_aiajakdkaj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k, j
! Equalities: b == a, c == a, e == a, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiajakdiak: do d = n0d, n1d
k_aiajakdiak: do k = n0km, n1km
j_aiajakdiak: do j = n0j, n1j
if (j == k) cycle j_aiajakdiak
a1 = min(d - 1, n1abce)
a_aiajakdiak: do a = n0abce, a1
if (a == d) cycle a_aiajakdiak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aiajakdiak: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajakdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakdiak(j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakdiak
end do a_aiajakdiak
end do j_aiajakdiak
end do k_aiajakdiak
end do d_aiajakdiak
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, k, i
! Equalities: b == a, c == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
d_aiajakdjak: do d = n0d, n1d
k_aiajakdjak: do k = n0km, n1km
j0 = max(k + 1, n0jl)
j_aiajakdjak: do j = j0, n1jl
if (j == k) cycle j_aiajakdjak
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abce)
a_aiajakdjak: do a = n0abce, a1
if (a == d) cycle a_aiajakdjak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakdjak: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakdjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajakdjak(i, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajakdjak
end do a_aiajakdjak
end do j_aiajakdjak
end do k_aiajakdjak
end do d_aiajakdjak
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, e == a, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiajaidiam: do m = n0m, n1m
d_aiajaidiam: do d = n0d, n1d
j_aiajaidiam: do j = n0j, n1j
if (j == m) cycle j_aiajaidiam
a1 = min(d - 1, n1abce)
a_aiajaidiam: do a = n0abce, a1
if (a == d) cycle a_aiajaidiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0ikl)
i_aiajaidiam: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aiajaidiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaidiam(i, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaidiam
end do a_aiajaidiam
end do j_aiajaidiam
end do d_aiajaidiam
end do m_aiajaidiam
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aiajajdiam: do m = n0m, n1m
d_aiajajdiam: do d = n0d, n1d
j_aiajajdiam: do j = n0jk, n1jk
if (j == m) cycle j_aiajajdiam
a1 = min(d - 1, n1abce)
a_aiajajdiam: do a = n0abce, a1
if (a == d) cycle a_aiajajdiam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aiajajdiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aiajajdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajdiam(nocc, a, i, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajdiam
end do a_aiajajdiam
end do j_aiajajdiam
end do d_aiajajdiam
end do m_aiajajdiam
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, m
! Equalities: b == a, c == a, e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aiajaidjam: do m = n0m, n1m
d_aiajaidjam: do d = n0d, n1d
j0 = max(m + 1, n0jl)
j_aiajaidjam: do j = j0, n1jl
if (j == m) cycle j_aiajaidjam
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abce)
a_aiajaidjam: do a = n0abce, a1
if (a == d) cycle a_aiajaidjam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidjam: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aiajaidjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaidjam(nocc, a, i, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaidjam
end do a_aiajaidjam
end do j_aiajaidjam
end do d_aiajaidjam
end do m_aiajaidjam
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, m
! Equalities: b == a, c == a, e == a, k == j, l == j
! No equalities independent of the above can hold.
!
m_aiajajdjam: do m = n0m, n1m
d_aiajajdjam: do d = n0d, n1d
j0 = max(m + 1, n0jkl)
j_aiajajdjam: do j = j0, n1jkl
if (j == m) cycle j_aiajajdjam
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(d - 1, n1abce)
a_aiajajdjam: do a = n0abce, a1
if (a == d) cycle a_aiajajdjam
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajdjam: do i = i0, n1i
if (i == j .or. i == m) cycle i_aiajajdjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajdjam(i, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajdjam
end do a_aiajajdjam
end do j_aiajajdjam
end do d_aiajajdjam
end do m_aiajajdjam
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aiajajdiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajajdiej: do d = d0, n1d
if (d == e) cycle d_aiajajdiej
j_aiajajdiej: do j = n0jkm, n1jkm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajajdiej: do a = n0abc, n1abc
if (a == d .or. a == e) cycle a_aiajajdiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajajdiej: do i = i0, n1il
if (i == j) cycle i_aiajajdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajajdiej(a, j, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajajdiej
end do a_aiajajdiej
end do j_aiajajdiej
end do d_aiajajdiej
end do e_aiajajdiej
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, d, e
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aiajaidiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aiajaidiej: do d = d0, n1d
if (d == e) cycle d_aiajaidiej
j_aiajaidiej: do j = n0jm, n1jm
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a_aiajaidiej: do a = n0abc, n1abc
if (a == d .or. a == e) cycle a_aiajaidiej
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aiajaidiej: do i = i0, n1ikl
if (i == j) cycle i_aiajaidiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aiajaidiej(a, i, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aiajaidiej
end do a_aiajaidiej
end do j_aiajaidiej
end do d_aiajaidiej
end do e_aiajaidiej
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, e == b, j == i, m == i
! No equalities independent of the above can hold.
!
l_aibibkalbi: do l = n0l, n1l
k_aibibkalbi: do k = n0k, n1k
if (k == l) cycle k_aibibkalbi
b_aibibkalbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkalbi: do a = a0, n1ad
if (a == b) cycle a_aibibkalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ijm)
i_aibibkalbi: do i = n0ijm, i1
if (i == k .or. i == l) cycle i_aibibkalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkalbi(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkalbi
end do a_aibibkalbi
end do b_aibibkalbi
end do k_aibibkalbi
end do l_aibibkalbi
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjbialbi: do l = n0l, n1l
b_aibjbialbi: do b = n0bce, n1bce
j_aibjbialbi: do j = n0j, n1j
if (j == l) cycle j_aibjbialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbialbi: do a = a0, n1ad
if (a == b) cycle a_aibjbialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1ikm)
i_aibjbialbi: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aibjbialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbialbi(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbialbi
end do a_aibjbialbi
end do j_aibjbialbi
end do b_aibjbialbi
end do l_aibjbialbi
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, e == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjbkajbi: do k = n0k, n1k
b_aibjbkajbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkajbi: do j = n0jl, n1jl
if (j == k) cycle j_aibjbkajbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkajbi: do a = a0, n1ad
if (a == b) cycle a_aibjbkajbi
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjbkajbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkajbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkajbi(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkajbi
end do a_aibjbkajbi
end do j_aibjbkajbi
end do b_aibjbkajbi
end do k_aibjbkajbi
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjbkakbi: do k = n0kl, n1kl
b_aibjbkakbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkakbi: do j = n0j, n1j
if (j == k) cycle j_aibjbkakbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkakbi: do a = a0, n1ad
if (a == b) cycle a_aibjbkakbi
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aibjbkakbi: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjbkakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkakbi(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkakbi
end do a_aibjbkakbi
end do j_aibjbkakbi
end do b_aibjbkakbi
end do k_aibjbkakbi
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjbjalbi: do l = n0l, n1l
b_aibjbjalbi: do b = n0bce, n1bce
j_aibjbjalbi: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbjalbi: do a = a0, n1ad
if (a == b) cycle a_aibjbjalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(l - 1, n1im)
i_aibjbjalbi: do i = n0im, i1
if (i == j .or. i == l) cycle i_aibjbjalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjalbi(nocc, a, i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjalbi
end do a_aibjbjalbi
end do j_aibjbjalbi
end do b_aibjbjalbi
end do l_aibjbjalbi
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: d == a, c == b, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjbjalbj: do l = n0l, n1l
b_aibjbjalbj: do b = n0bce, n1bce
j1 = min(l - 1, n1jkm)
j_aibjbjalbj: do j = n0jkm, j1
if (j == l) cycle j_aibjbjalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbjalbj: do a = a0, n1ad
if (a == b) cycle a_aibjbjalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbjalbj: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjbjalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjalbj(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjalbj
end do a_aibjbjalbj
end do j_aibjbjalbj
end do b_aibjbjalbj
end do l_aibjbjalbj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjbkaibj: do k = n0k, n1k
b_aibjbkaibj: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkaibj: do a = a0, n1ad
if (a == b) cycle a_aibjbkaibj
i0 = max(j + 1, n0il)
i_aibjbkaibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkaibj(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkaibj
end do a_aibjbkaibj
end do j_aibjbkaibj
end do b_aibjbkaibj
end do k_aibjbkaibj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjbialbj: do l = n0l, n1l
b_aibjbialbj: do b = n0bce, n1bce
j1 = min(l - 1, n1jm)
j_aibjbialbj: do j = n0jm, j1
if (j == l) cycle j_aibjbialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbialbj: do a = a0, n1ad
if (a == b) cycle a_aibjbialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjbialbj: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjbialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbialbj(a, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbialbj
end do a_aibjbialbj
end do j_aibjbialbj
end do b_aibjbialbj
end do l_aibjbialbj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: d == a, c == b, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjbkaibk: do k = n0km, n1km
b_aibjbkaibk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaibk: do j = n0j, n1j
if (j == k) cycle j_aibjbkaibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkaibk: do a = a0, n1ad
if (a == b) cycle a_aibjbkaibk
i0 = max(k + 1, n0il)
i_aibjbkaibk: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbkaibk(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbkaibk
end do a_aibjbkaibk
end do j_aibjbkaibk
end do b_aibjbkaibk
end do k_aibjbkaibk
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, m
! Equalities: d == a, c == b, e == b, j == i, l == i
! No equalities independent of the above can hold.
!
m_aibibkaibm: do m = n0m, n1m
k_aibibkaibm: do k = n0k, n1k
if (k == m) cycle k_aibibkaibm
b_aibibkaibm: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkaibm: do a = a0, n1ad
if (a == b) cycle a_aibibkaibm
i0 = max(m + 1, n0ijl)
i_aibibkaibm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aibibkaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibkaibm(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibkaibm
end do a_aibibkaibm
end do b_aibibkaibm
end do k_aibibkaibm
end do m_aibibkaibm
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjbiaibm: do m = n0m, n1m
b_aibjbiaibm: do b = n0bce, n1bce
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbiaibm: do j = n0j, n1j
if (j == m) cycle j_aibjbiaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiaibm: do a = a0, n1ad
if (a == b) cycle a_aibjbiaibm
i0 = max(m + 1, n0ikl)
i_aibjbiaibm: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjbiaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbiaibm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbiaibm
end do a_aibjbiaibm
end do j_aibjbiaibm
end do b_aibjbiaibm
end do m_aibjbiaibm
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjbjaibm: do m = n0m, n1m
b_aibjbjaibm: do b = n0bce, n1bce
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbjaibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbjaibm: do a = a0, n1ad
if (a == b) cycle a_aibjbjaibm
i0 = max(m + 1, n0il)
i_aibjbjaibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjaibm(nocc, a, i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjaibm
end do a_aibjbjaibm
end do j_aibjbjaibm
end do b_aibjbjaibm
end do m_aibjbjaibm
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l, m
! Equalities: d == a, c == b, e == b, j == i, k == i
! No equalities independent of the above can hold.
!
m_aibibialbm: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aibibialbm: do l = l0, n1l
if (l == m) cycle l_aibibialbm
b_aibibialbm: do b = n0bce, n1bce
em = (b - nvirt0) * nocc + (m - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibialbm: do a = a0, n1ad
if (a == b) cycle a_aibibialbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibibialbm: do i = n0ijk, n1ijk
if (i == l .or. i == m) cycle i_aibibialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibialbm(i, b, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibialbm
end do a_aibibialbm
end do b_aibibialbm
end do l_aibibialbm
end do m_aibibialbm
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjbiajbm: do m = n0m, n1m
b_aibjbiajbm: do b = n0bce, n1bce
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjbiajbm: do j = j0, n1jl
if (j == m) cycle j_aibjbiajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbiajbm: do a = a0, n1ad
if (a == b) cycle a_aibjbiajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbiajbm: do i = n0ik, n1ik
if (i == j .or. i == m) cycle i_aibjbiajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbiajbm(a, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbiajbm
end do a_aibjbiajbm
end do j_aibjbiajbm
end do b_aibjbiajbm
end do m_aibjbiajbm
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, m
! Equalities: d == a, c == b, e == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjbjajbm: do m = n0m, n1m
b_aibjbjajbm: do b = n0bce, n1bce
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jkl)
j_aibjbjajbm: do j = j0, n1jkl
if (j == m) cycle j_aibjbjajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbjajbm: do a = a0, n1ad
if (a == b) cycle a_aibjbjajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjbjajbm: do i = n0i, n1i
if (i == j .or. i == m) cycle i_aibjbjajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjajbm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjajbm
end do a_aibjbjajbm
end do j_aibjbjajbm
end do b_aibjbjajbm
end do m_aibjbjajbm
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, l
! Equalities: c == b, e == b, j == i, k == i, m == i
! No equalities independent of the above can hold.
!
d_aibibidlbi: do d = n0d, n1d
l_aibibidlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1bce)
b_aibibidlbi: do b = n0bce, b1
if (b == d) cycle b_aibibidlbi
a0 = max(b + 1, n0a)
a_aibibidlbi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibidlbi
i1 = min(l - 1, n1ijkm)
i_aibibidlbi: do i = n0ijkm, i1
if (i == l) cycle i_aibibidlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibibidlbi(a, b, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibibidlbi
end do a_aibibidlbi
end do b_aibibidlbi
end do l_aibibidlbi
end do d_aibibidlbi
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidjbi: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibjbidjbi: do b = n0bce, b1
if (b == d) cycle b_aibjbidjbi
j_aibjbidjbi: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidjbi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidjbi
i1 = min(j - 1, n1ikm)
i_aibjbidjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjbidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbidjbi(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbidjbi
end do a_aibjbidjbi
end do j_aibjbidjbi
end do b_aibjbidjbi
end do d_aibjbidjbi
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, e == b, m == i, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjbjdjbi: do d = n0d, n1d
b1 = min(d - 1, n1bce)
b_aibjbjdjbi: do b = n0bce, b1
if (b == d) cycle b_aibjbjdjbi
j_aibjbjdjbi: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdjbi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdjbi
i1 = min(j - 1, n1im)
i_aibjbjdjbi: do i = n0im, i1
if (i == j) cycle i_aibjbjdjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = eom_cc3_23_tripletm_trans_aibjbjdjbi(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 


end do i_aibjbjdjbi
end do a_aibjbjdjbi
end do j_aibjbjdjbi
end do b_aibjbjdjbi
end do d_aibjbjdjbi
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
end subroutine ccjac_23_tripletm_dav_part7
end module ccjac_block_23_tripletm_dav_part7
