module ccjac_block_23_tripletp_dav_part6
use eom_cc3_23_tripletp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams 
implicit none
!
! File generated automatically on 2014-04-05 22:53:32 UTC.
!
contains
subroutine ccjac_23_tripletp_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0acd, n0ad, n0ae, n0bc, n0bce
integer :: n0bd, n0be, n0ce, n0ik, n0ikl
integer :: n0il, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jm, n0kl
integer :: n1acd, n1ad, n1ae, n1bc, n1bce
integer :: n1bd, n1be, n1ce, n1ik, n1ikl
integer :: n1il, n1im, n1jk, n1jkl, n1jkm
integer :: n1jl, n1jm, n1kl
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
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bce = max(n0b, n0c, n0e)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0ce = max(n0c, n0e)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jkm = max(n0j, n0k, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n1acd = min(n1a, n1c, n1d)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bce = min(n1b, n1c, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1ce = min(n1c, n1e)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jkm = min(n1j, n1k, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
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
i0 = max(j + 1, n0ik)
i_aibjbiajbm: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjbiajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbiajbm(i, b, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part6a', ibra, iket, jac_ibra_iket

end do i_aibjbiajbm
end do a_aibjbiajbm
end do j_aibjbiajbm
end do b_aibjbiajbm
end do m_aibjbiajbm
!
! Elementary loop  2
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
i0 = max(j + 1, n0i)
i_aibjbjajbm: do i = i0, n1i
if (i == j .or. i == m) cycle i_aibjbjajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjajbm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part62', ibra, iket, jac_ibra_iket


end do i_aibjbjajbm
end do a_aibjbjajbm
end do j_aibjbjajbm
end do b_aibjbjajbm
end do m_aibjbjajbm
!
! Elementary loop  3
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
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjbjalbi: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjbjalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjalbi(b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part63', ibra, iket, jac_ibra_iket


end do i_aibjbjalbi
end do a_aibjbjalbi
end do j_aibjbjalbi
end do b_aibjbjalbi
end do l_aibjbjalbi
!
! Elementary loop  4
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
i0 = max(j + 1, m + 1, n0il)
i_aibjbjaibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjaibm(b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part64', ibra, iket, jac_ibra_iket


end do i_aibjbjaibm
end do a_aibjbjaibm
end do j_aibjbjaibm
end do b_aibjbjaibm
end do m_aibjbjaibm
!
! Elementary loop  5
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
i0 = max(j + 1, n0im)
i1 = min(k - 1, n1im)
i_aibjbkakbi: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjbkakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkakbi(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part65', ibra, iket, jac_ibra_iket


end do i_aibjbkakbi
end do a_aibjbkakbi
end do j_aibjbkakbi
end do b_aibjbkakbi
end do k_aibjbkakbi
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: d == a, c == b, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
k_aibjbkakbj: do k = n0kl, n1kl
b_aibjbkakbj: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jm)
j_aibjbkakbj: do j = n0jm, j1
if (j == k) cycle j_aibjbkakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkakbj: do a = a0, n1ad
if (a == b) cycle a_aibjbkakbj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbkakbj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjbkakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkakbj(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part66', ibra, iket, jac_ibra_iket


end do i_aibjbkakbj
end do a_aibjbkakbj
end do j_aibjbkakbj
end do b_aibjbkakbj
end do k_aibjbkakbj
!
! Elementary loop  7
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkaibj(nocc, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part67', ibra, iket, jac_ibra_iket


end do i_aibjbkaibj
end do a_aibjbkaibj
end do j_aibjbkaibj
end do b_aibjbkaibj
end do k_aibjbkaibj
!
! Elementary loop  8
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjdibj(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part68', ibra, iket, jac_ibra_iket


end do i_aibjbjdibj
end do a_aibjbjdibj
end do j_aibjbjdibj
end do b_aibjbjdibj
end do d_aibjbjdibj
!
! Elementary loop  9
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbidibj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part69', ibra, iket, jac_ibra_iket


end do i_aibjbidibj
end do a_aibjbidibj
end do j_aibjbidibj
end do b_aibjbidibj
end do d_aibjbidibj
!
! Elementary loop  10
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjaiej(b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part610', ibra, iket, jac_ibra_iket


end do i_aibjbjaiej
end do a_aibjbjaiej
end do j_aibjbjaiej
end do b_aibjbjaiej
end do e_aibjbjaiej
!
! Elementary loop  11
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbiaiej(i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part611', ibra, iket, jac_ibra_iket


end do i_aibjbiaiej
end do a_aibjbiaiej
end do j_aibjbiaiej
end do b_aibjbiaiej
end do e_aibjbiaiej
!
! Elementary loop  12
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjdiaj(b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part612', ibra, iket, jac_ibra_iket


end do i_aibjbjdiaj
end do a_aibjbjdiaj
end do j_aibjbjdiaj
end do b_aibjbjdiaj
end do d_aibjbjdiaj
!
! Elementary loop  13
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbidiaj(i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part612', ibra, iket, jac_ibra_iket


end do i_aibjbidiaj
end do a_aibjbidiaj
end do j_aibjbidiaj
end do b_aibjbidiaj
end do d_aibjbidiaj
!
! Elementary loop  14
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjaicj(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part613', ibra, iket, jac_ibra_iket


end do i_aibjcjaicj
end do a_aibjcjaicj
end do j_aibjcjaicj
end do b_aibjcjaicj
end do c_aibjcjaicj
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciaicj: do c = n0ce, n1ce
b_aibjciaicj: do b = n0b, n1b
if (b == c) cycle b_aibjciaicj
j_aibjciaicj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjciaicj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaicj
i0 = max(j + 1, n0ikl)
i_aibjciaicj: do i = i0, n1ikl
if (i == j) cycle i_aibjciaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjciaicj(i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part614', ibra, iket, jac_ibra_iket


end do i_aibjciaicj
end do a_aibjciaicj
end do j_aibjciaicj
end do b_aibjciaicj
end do c_aibjciaicj
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjbicj: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcjbicj: do b = b0, n1bd
if (b == c) cycle b_aibjcjbicj
j_aibjcjbicj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbicj
i0 = max(j + 1, n0il)
i_aibjcjbicj: do i = i0, n1il
if (i == j) cycle i_aibjcjbicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjbicj(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part615', ibra, iket, jac_ibra_iket


end do i_aibjcjbicj
end do a_aibjcjbicj
end do j_aibjcjbicj
end do b_aibjcjbicj
end do c_aibjcjbicj
!
! Elementary loop  17
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcibicj(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part616', ibra, iket, jac_ibra_iket


end do i_aibjcibicj
end do a_aibjcibicj
end do j_aibjcibicj
end do b_aibjcibicj
end do c_aibjcibicj
!
! Elementary loop  18
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjaibj(nocc, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part617', ibra, iket, jac_ibra_iket


end do i_aibjcjaibj
end do a_aibjcjaibj
end do j_aibjcjaibj
end do b_aibjcjaibj
end do c_aibjcjaibj
!
! Elementary loop  19
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjciaibj(nocc, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part618', ibra, iket, jac_ibra_iket


end do i_aibjciaibj
end do a_aibjciaibj
end do j_aibjciaibj
end do b_aibjciaibj
end do c_aibjciaibj
!
! Elementary loop  20
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajaibj(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part619', ibra, iket, jac_ibra_iket


end do i_aibjajaibj
end do a_aibjajaibj
end do j_aibjajaibj
end do b_aibjajaibj
!
! Elementary loop  21
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaiaibj(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part620', ibra, iket, jac_ibra_iket


end do i_aibjaiaibj
end do a_aibjaiaibj
end do j_aibjaiaibj
end do b_aibjaiaibj
!
! Elementary loop  22
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjaibj(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 
 ! print*, a, i, b, j, b, j, a, i, b, j

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part621', ibra, iket, jac_ibra_iket


end do i_aibjbjaibj
end do a_aibjbjaibj
end do j_aibjbjaibj
end do b_aibjbjaibj
!
! Elementary loop  23
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbiaibj(nocc, a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part622', ibra, iket, jac_ibra_iket


end do i_aibjbiaibj
end do a_aibjbiaibj
end do j_aibjbiaibj
end do b_aibjbiaibj
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
end subroutine ccjac_23_tripletp_dav_part6
end module ccjac_block_23_tripletp_dav_part6
