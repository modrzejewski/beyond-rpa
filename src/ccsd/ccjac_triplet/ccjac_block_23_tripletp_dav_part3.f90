module ccjac_block_23_tripletp_dav_part3
use eom_cc3_23_tripletp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams 
implicit none
!
! File generated automatically on 2014-04-05 22:53:31 UTC.
!
contains
subroutine ccjac_23_tripletp_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0ac, n0acd, n0ace, n0ad, n0ae
integer :: n0bc, n0bcd, n0bce, n0bd, n0be
integer :: n0cd, n0ik, n0il, n0im, n0jk
integer :: n0jl, n0jm
integer :: n1ac, n1acd, n1ace, n1ad, n1ae
integer :: n1bc, n1bcd, n1bce, n1bd, n1be
integer :: n1cd, n1ik, n1il, n1im, n1jk
integer :: n1jl, n1jm
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
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0bce = max(n0b, n0c, n0e)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1bce = min(n1b, n1c, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == a, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjakalbi: do l = n0l, n1l
k_aibjakalbi: do k = n0k, n1k
if (k == l) cycle k_aibjakalbi
b_aibjakalbi: do b = n0be, n1be
j_aibjakalbi: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakalbi: do a = a0, n1acd
if (a == b) cycle a_aibjakalbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjakalbi: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjakalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakalbi(a, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjakalbi
end do a_aibjakalbi
end do j_aibjakalbi
end do b_aibjakalbi
end do k_aibjakalbi
end do l_aibjakalbi
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjakalbj: do l = n0l, n1l
k_aibjakalbj: do k = n0k, n1k
if (k == l) cycle k_aibjakalbj
b_aibjakalbj: do b = n0be, n1be
j1 = min(l - 1, n1jm)
j_aibjakalbj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjakalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakalbj: do a = a0, n1acd
if (a == b) cycle a_aibjakalbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjakalbj: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakalbj(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjakalbj
end do a_aibjakalbj
end do j_aibjakalbj
end do b_aibjakalbj
end do k_aibjakalbj
end do l_aibjakalbj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: c == a, d == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjakaibm: do m = n0m, n1m
k_aibjakaibm: do k = n0k, n1k
if (k == m) cycle k_aibjakaibm
b_aibjakaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakaibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjakaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakaibm: do a = a0, n1acd
if (a == b) cycle a_aibjakaibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjakaibm: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjakaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakaibm(a, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjakaibm
end do a_aibjakaibm
end do j_aibjakaibm
end do b_aibjakaibm
end do k_aibjakaibm
end do m_aibjakaibm
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: c == a, d == a, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakajbm: do m = n0m, n1m
k_aibjakajbm: do k = n0k, n1k
if (k == m) cycle k_aibjakajbm
b_aibjakajbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjakajbm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjakajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakajbm: do a = a0, n1acd
if (a == b) cycle a_aibjakajbm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjakajbm: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakajbm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjakajbm
end do a_aibjakajbm
end do j_aibjakajbm
end do b_aibjakajbm
end do k_aibjakajbm
end do m_aibjakajbm
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakaiej: do e = n0e, n1e
k_aibjakaiej: do k = n0k, n1k
b_aibjakaiej: do b = n0b, n1b
if (b == e) cycle b_aibjakaiej
j_aibjakaiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjakaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0acd)
a_aibjakaiej: do a = a0, n1acd
if (a == b .or. a == e) cycle a_aibjakaiej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakaiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakaiej(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjakaiej
end do a_aibjakaiej
end do j_aibjakaiej
end do b_aibjakaiej
end do k_aibjakaiej
end do e_aibjakaiej
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjakdiaj: do d = n0d, n1d
k_aibjakdiaj: do k = n0k, n1k
b_aibjakdiaj: do b = n0b, n1b
if (b == d) cycle b_aibjakdiaj
j_aibjakdiaj: do j = n0jm, n1jm
if (j == k) cycle j_aibjakdiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ace)
a1 = min(d - 1, n1ace)
a_aibjakdiaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjakdiaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakdiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakdiaj(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjakdiaj
end do a_aibjakdiaj
end do j_aibjakdiaj
end do b_aibjakdiaj
end do k_aibjakdiaj
end do d_aibjakdiaj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaiblej: do e = n0e, n1e
l_aibjaiblej: do l = n0l, n1l
b0 = max(e + 1, n0bd)
b_aibjaiblej: do b = b0, n1bd
if (b == e) cycle b_aibjaiblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjaiblej: do j = n0jm, j1
if (j == l) cycle j_aibjaiblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaiblej
i0 = max(j + 1, n0ik)
i_aibjaiblej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaiblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaiblej(nocc, a, i, b, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjaiblej
end do a_aibjaiblej
end do j_aibjaiblej
end do b_aibjaiblej
end do l_aibjaiblej
end do e_aibjaiblej
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaibjem: do e = n0e, n1e
m_aibjaibjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibjaibjem: do b = b0, n1bd
if (b == e) cycle b_aibjaibjem
j0 = max(m + 1, n0jl)
j_aibjaibjem: do j = j0, n1jl
if (j == m) cycle j_aibjaibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibjem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaibjem
i0 = max(j + 1, n0ik)
i_aibjaibjem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjaibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaibjem(nocc, a, i, b, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjaibjem
end do a_aibjaibjem
end do j_aibjaibjem
end do b_aibjaibjem
end do m_aibjaibjem
end do e_aibjaibjem
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajblei: do e = n0e, n1e
l_aibjajblei: do l = n0l, n1l
b0 = max(e + 1, n0bd)
b_aibjajblei: do b = b0, n1bd
if (b == e) cycle b_aibjajblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajblei: do j = n0jk, n1jk
if (j == l) cycle j_aibjajblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajblei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajblei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjajblei: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjajblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajblei(nocc, a, i, b, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjajblei
end do a_aibjajblei
end do j_aibjajblei
end do b_aibjajblei
end do l_aibjajblei
end do e_aibjajblei
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajbiem: do e = n0e, n1e
m_aibjajbiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibjajbiem: do b = b0, n1bd
if (b == e) cycle b_aibjajbiem
j_aibjajbiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjajbiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbiem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajbiem
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjajbiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjajbiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajbiem(nocc, a, i, b, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjajbiem
end do a_aibjajbiem
end do j_aibjajbiem
end do b_aibjajbiem
end do m_aibjajbiem
end do e_aibjajbiem
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakbiej: do e = n0e, n1e
k_aibjakbiej: do k = n0k, n1k
b0 = max(e + 1, n0bd)
b_aibjakbiej: do b = b0, n1bd
if (b == e) cycle b_aibjakbiej
j_aibjakbiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjakbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbiej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbiej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakbiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakbiej(nocc, a, i, b, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjakbiej
end do a_aibjakbiej
end do j_aibjakbiej
end do b_aibjakbiej
end do k_aibjakbiej
end do e_aibjakbiej
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjaidlbj: do d = n0d, n1d
l_aibjaidlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjaidlbj: do b = n0be, b1
if (b == d) cycle b_aibjaidlbj
j1 = min(l - 1, n1jm)
j_aibjaidlbj: do j = n0jm, j1
if (j == l) cycle j_aibjaidlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidlbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidlbj
i0 = max(j + 1, n0ik)
i_aibjaidlbj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaidlbj(nocc, a, i, b, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjaidlbj
end do a_aibjaidlbj
end do j_aibjaidlbj
end do b_aibjaidlbj
end do l_aibjaidlbj
end do d_aibjaidlbj
!
! Elementary loop  13
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
i0 = max(j + 1, n0ik)
i_aibjaidjbm: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjaidjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaidjbm(nocc, a, i, b, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjaidjbm
end do a_aibjaidjbm
end do j_aibjaidjbm
end do b_aibjaidjbm
end do d_aibjaidjbm
end do m_aibjaidjbm
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdlbi: do d = n0d, n1d
l_aibjajdlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjajdlbi: do b = n0be, b1
if (b == d) cycle b_aibjajdlbi
j_aibjajdlbi: do j = n0jk, n1jk
if (j == l) cycle j_aibjajdlbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajdlbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdlbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjajdlbi: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjajdlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajdlbi(nocc, a, i, b, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjajdlbi
end do a_aibjajdlbi
end do j_aibjajdlbi
end do b_aibjajdlbi
end do l_aibjajdlbi
end do d_aibjajdlbi
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: c == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajdibm: do m = n0m, n1m
d_aibjajdibm: do d = n0d, n1d
b1 = min(d - 1, n1be)
b_aibjajdibm: do b = n0be, b1
if (b == d) cycle b_aibjajdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajdibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjajdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajdibm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdibm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjajdibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjajdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajdibm(nocc, a, i, b, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjajdibm
end do a_aibjajdibm
end do j_aibjajdibm
end do b_aibjajdibm
end do d_aibjajdibm
end do m_aibjajdibm
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjakdibj: do d = n0d, n1d
k_aibjakdibj: do k = n0k, n1k
b1 = min(d - 1, n1be)
b_aibjakdibj: do b = n0be, b1
if (b == d) cycle b_aibjakdibj
j_aibjakdibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjakdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdibj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdibj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakdibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakdibj(nocc, a, i, b, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjakdibj
end do a_aibjakdibj
end do j_aibjakdibj
end do b_aibjakdibj
end do k_aibjakdibj
end do d_aibjakdibj
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkbiej: do e = n0e, n1e
k_aibjbkbiej: do k = n0k, n1k
b0 = max(e + 1, n0bcd)
b_aibjbkbiej: do b = b0, n1bcd
if (b == e) cycle b_aibjbkbiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbiej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbkbiej
i0 = max(j + 1, n0il)
i_aibjbkbiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkbiej(a, b, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbkbiej
end do a_aibjbkbiej
end do j_aibjbkbiej
end do b_aibjbkbiej
end do k_aibjbkbiej
end do e_aibjbkbiej
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: e == a, d == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckciaj: do c = n0cd, n1cd
k_aibjckciaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckciaj: do b = n0b, n1b
if (b == c) cycle b_aibjckciaj
j_aibjckciaj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckciaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjckciaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckciaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjckciaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckciaj(b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjckciaj
end do a_aibjckciaj
end do j_aibjckciaj
end do b_aibjckciaj
end do k_aibjckciaj
end do c_aibjckciaj
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: e == b, d == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckcibj: do c = n0cd, n1cd
k_aibjckcibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(c - 1, n1be)
b_aibjckcibj: do b = n0be, b1
if (b == c) cycle b_aibjckcibj
j_aibjckcibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckcibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckcibj
i0 = max(j + 1, n0il)
i_aibjckcibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckcibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckcibj(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjckcibj
end do a_aibjckcibj
end do j_aibjckcibj
end do b_aibjckcibj
end do k_aibjckcibj
end do c_aibjckcibj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: d == a, c == b, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjbkalbi: do l = n0l, n1l
k_aibjbkalbi: do k = n0k, n1k
if (k == l) cycle k_aibjbkalbi
b_aibjbkalbi: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkalbi: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjbkalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkalbi: do a = a0, n1ad
if (a == b) cycle a_aibjbkalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjbkalbi: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkalbi(b, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbkalbi
end do a_aibjbkalbi
end do j_aibjbkalbi
end do b_aibjbkalbi
end do k_aibjbkalbi
end do l_aibjbkalbi
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: d == a, c == b, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjbkalbj: do l = n0l, n1l
k_aibjbkalbj: do k = n0k, n1k
if (k == l) cycle k_aibjbkalbj
b_aibjbkalbj: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjbkalbj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjbkalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkalbj: do a = a0, n1ad
if (a == b) cycle a_aibjbkalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbkalbj: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjbkalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkalbj(i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbkalbj
end do a_aibjbkalbj
end do j_aibjbkalbj
end do b_aibjbkalbj
end do k_aibjbkalbj
end do l_aibjbkalbj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, m
! Equalities: d == a, c == b, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjbkaibm: do m = n0m, n1m
k_aibjbkaibm: do k = n0k, n1k
if (k == m) cycle k_aibjbkaibm
b_aibjbkaibm: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjbkaibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjbkaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkaibm: do a = a0, n1ad
if (a == b) cycle a_aibjbkaibm
i0 = max(j + 1, m + 1, n0il)
i_aibjbkaibm: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjbkaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkaibm(b, j, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbkaibm
end do a_aibjbkaibm
end do j_aibjbkaibm
end do b_aibjbkaibm
end do k_aibjbkaibm
end do m_aibjbkaibm
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: d == a, c == b, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjbkajbm: do m = n0m, n1m
k_aibjbkajbm: do k = n0k, n1k
if (k == m) cycle k_aibjbkajbm
b_aibjbkajbm: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjbkajbm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjbkajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkajbm: do a = a0, n1ad
if (a == b) cycle a_aibjbkajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbkajbm: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjbkajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkajbm(i, b, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbkajbm
end do a_aibjbkajbm
end do j_aibjbkajbm
end do b_aibjbkajbm
end do k_aibjbkajbm
end do m_aibjbkajbm
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdibj: do d = n0d, n1d
k_aibjbkdibj: do k = n0k, n1k
b1 = min(d - 1, n1bce)
b_aibjbkdibj: do b = n0bce, b1
if (b == d) cycle b_aibjbkdibj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdibj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdibj
i0 = max(j + 1, n0il)
i_aibjbkdibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkdibj(a, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbkdibj
end do a_aibjbkdibj
end do j_aibjbkdibj
end do b_aibjbkdibj
end do k_aibjbkdibj
end do d_aibjbkdibj
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbialej: do e = n0e, n1e
l_aibjbialej: do l = n0l, n1l
b_aibjbialej: do b = n0bc, n1bc
if (b == e) cycle b_aibjbialej
j1 = min(l - 1, n1jm)
j_aibjbialej: do j = n0jm, j1
if (j == l) cycle j_aibjbialej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbialej: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbialej
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjbialej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbialej(nocc, a, i, b, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbialej
end do a_aibjbialej
end do j_aibjbialej
end do b_aibjbialej
end do l_aibjbialej
end do e_aibjbialej
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbiajem: do e = n0e, n1e
m_aibjbiajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjbiajem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbiajem
j0 = max(m + 1, n0jl)
j_aibjbiajem: do j = j0, n1jl
if (j == m) cycle j_aibjbiajem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbiajem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbiajem
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjbiajem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjbiajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbiajem(nocc, a, i, b, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbiajem
end do a_aibjbiajem
end do j_aibjbiajem
end do b_aibjbiajem
end do m_aibjbiajem
end do e_aibjbiajem
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjalei: do e = n0e, n1e
l_aibjbjalei: do l = n0l, n1l
b_aibjbjalei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjalei
j_aibjbjalei: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjalei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjalei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjalei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjbjalei: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjbjalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjalei(nocc, a, i, b, j, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbjalei
end do a_aibjbjalei
end do j_aibjbjalei
end do b_aibjbjalei
end do l_aibjbjalei
end do e_aibjbjalei
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, m
! Equalities: d == a, c == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjaiem: do e = n0e, n1e
m_aibjbjaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
b_aibjbjaiem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjaiem
j_aibjbjaiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjaiem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjaiem
i0 = max(j + 1, m + 1, n0il)
i_aibjbjaiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjaiem(nocc, a, i, b, j, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbjaiem
end do a_aibjbjaiem
end do j_aibjbjaiem
end do b_aibjbjaiem
end do m_aibjbjaiem
end do e_aibjbjaiem
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkaiej: do e = n0e, n1e
k_aibjbkaiej: do k = n0k, n1k
b_aibjbkaiej: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkaiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkaiej: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkaiej
i0 = max(j + 1, n0il)
i_aibjbkaiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkaiej(nocc, a, i, b, j, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbkaiej
end do a_aibjbkaiej
end do j_aibjbkaiej
end do b_aibjbkaiej
end do k_aibjbkaiej
end do e_aibjbkaiej
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: e == a, c == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbidlaj: do d = n0d, n1d
l_aibjbidlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbidlaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidlaj
j1 = min(l - 1, n1jm)
j_aibjbidlaj: do j = n0jm, j1
if (j == l) cycle j_aibjbidlaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidlaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidlaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjbidlaj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbidlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbidlaj(nocc, a, i, b, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part3', ibra, iket, jac_ibra_iket
end do i_aibjbidlaj
end do a_aibjbidlaj
end do j_aibjbidlaj
end do b_aibjbidlaj
end do l_aibjbidlaj
end do d_aibjbidlaj
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
end subroutine ccjac_23_tripletp_dav_part3
end module ccjac_block_23_tripletp_dav_part3
