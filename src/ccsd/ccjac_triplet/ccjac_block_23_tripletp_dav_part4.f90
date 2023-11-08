module ccjac_block_23_tripletp_dav_part4
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
subroutine ccjac_23_tripletp_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0acd, n0ad, n0ae, n0bc, n0bd
integer :: n0be, n0ce, n0ik, n0ikl, n0ikm
integer :: n0il, n0im, n0jk, n0jkl, n0jkm
integer :: n0jl, n0jm, n0kl, n0km
integer :: n1acd, n1ad, n1ae, n1bc, n1bd
integer :: n1be, n1ce, n1ik, n1ikl, n1ikm
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
n0acd = max(n0a, n0c, n0d)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0ce = max(n0c, n0e)
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
n1acd = min(n1a, n1c, n1d)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1ce = min(n1c, n1e)
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
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: e == a, c == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjbidjam: do m = n0m, n1m
d_aibjbidjam: do d = n0d, n1d
b_aibjbidjam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidjam
j0 = max(m + 1, n0jl)
j_aibjbidjam: do j = j0, n1jl
if (j == m) cycle j_aibjbidjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidjam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjbidjam: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjbidjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbidjam(nocc, a, i, b, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjbidjam
end do a_aibjbidjam
end do j_aibjbidjam
end do b_aibjbidjam
end do d_aibjbidjam
end do m_aibjbidjam
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: e == a, c == b, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdlai: do d = n0d, n1d
l_aibjbjdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbjdlai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdlai
j_aibjbjdlai: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjdlai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdlai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdlai
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjbjdlai: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjbjdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjdlai(nocc, a, i, b, j, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjbjdlai
end do a_aibjbjdlai
end do j_aibjbjdlai
end do b_aibjbjdlai
end do l_aibjbjdlai
end do d_aibjbjdlai
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, m
! Equalities: e == a, c == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjbjdiam: do m = n0m, n1m
d_aibjbjdiam: do d = n0d, n1d
b_aibjbjdiam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdiam
j_aibjbjdiam: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjdiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdiam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjbjdiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjdiam(nocc, a, i, b, j, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjbjdiam
end do a_aibjbjdiam
end do j_aibjbjdiam
end do b_aibjbjdiam
end do d_aibjbjdiam
end do m_aibjbjdiam
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: e == a, c == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdiaj: do d = n0d, n1d
k_aibjbkdiaj: do k = n0k, n1k
b_aibjbkdiaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdiaj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdiaj: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkdiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdiaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjbkdiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkdiaj(nocc, a, i, b, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjbkdiaj
end do a_aibjbkdiaj
end do j_aibjbkdiaj
end do b_aibjbkdiaj
end do k_aibjbkdiaj
end do d_aibjbkdiaj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j, k
! Equalities: d == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaicj: do c = n0ce, n1ce
k_aibjckaicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaicj: do b = n0b, n1b
if (b == c) cycle b_aibjckaicj
j_aibjckaicj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, c + 1, n0ad)
a_aibjckaicj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaicj
i0 = max(j + 1, n0il)
i_aibjckaicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckaicj(b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjckaicj
end do a_aibjckaicj
end do j_aibjckaicj
end do b_aibjckaicj
end do k_aibjckaicj
end do c_aibjckaicj
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j, k
! Equalities: d == b, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckbicj: do c = n0ce, n1ce
k_aibjckbicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbicj: do b = b0, n1bd
if (b == c) cycle b_aibjckbicj
j_aibjckbicj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckbicj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbicj
i0 = max(j + 1, n0il)
i_aibjckbicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckbicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckbicj(a, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjckbicj
end do a_aibjckbicj
end do j_aibjckbicj
end do b_aibjckbicj
end do k_aibjckbicj
end do c_aibjckbicj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, e == b, k == i, m == i
! No equalities independent of the above can hold.
!
l_aibjcialbi: do l = n0l, n1l
c_aibjcialbi: do c = n0c, n1c
b_aibjcialbi: do b = n0be, n1be
if (b == c) cycle b_aibjcialbi
j_aibjcialbi: do j = n0j, n1j
if (j == l) cycle j_aibjcialbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcialbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ikm)
i1 = min(l - 1, n1ikm)
i_aibjcialbi: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjcialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcialbi(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcialbi
end do a_aibjcialbi
end do j_aibjcialbi
end do b_aibjcialbi
end do c_aibjcialbi
end do l_aibjcialbi
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, e == b, k == j, m == j
! No equalities independent of the above can hold.
!
l_aibjcjalbj: do l = n0l, n1l
c_aibjcjalbj: do c = n0c, n1c
b_aibjcjalbj: do b = n0be, n1be
if (b == c) cycle b_aibjcjalbj
j1 = min(l - 1, n1jkm)
j_aibjcjalbj: do j = n0jkm, j1
if (j == l) cycle j_aibjcjalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjalbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjcjalbj: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjcjalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjalbj(i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcjalbj
end do a_aibjcjalbj
end do j_aibjcjalbj
end do b_aibjcjalbj
end do c_aibjcjalbj
end do l_aibjcjalbj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibjckaibk: do c = n0c, n1c
k_aibjckaibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibk: do b = n0be, n1be
if (b == c) cycle b_aibjckaibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckaibk: do j = n0j, n1j
if (j == k) cycle j_aibjckaibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibk
i0 = max(j + 1, k + 1, n0il)
i_aibjckaibk: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckaibk(j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjckaibk
end do a_aibjckaibk
end do j_aibjckaibk
end do b_aibjckaibk
end do k_aibjckaibk
end do c_aibjckaibk
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajbk: do c = n0c, n1c
k_aibjckajbk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajbk: do b = n0be, n1be
if (b == c) cycle b_aibjckajbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjckajbk: do j = j0, n1jl
if (j == k) cycle j_aibjckajbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckajbk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajbk
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjckajbk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjckajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckajbk(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjckajbk
end do a_aibjckajbk
end do j_aibjckajbk
end do b_aibjckajbk
end do k_aibjckajbk
end do c_aibjckajbk
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, m
! Equalities: d == a, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjciaibm: do m = n0m, n1m
c_aibjciaibm: do c = n0c, n1c
b_aibjciaibm: do b = n0be, n1be
if (b == c) cycle b_aibjciaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjciaibm: do j = n0j, n1j
if (j == m) cycle j_aibjciaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjciaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciaibm
i0 = max(j + 1, m + 1, n0ikl)
i_aibjciaibm: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjciaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjciaibm(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjciaibm
end do a_aibjciaibm
end do j_aibjciaibm
end do b_aibjciaibm
end do c_aibjciaibm
end do m_aibjciaibm
!
! Elementary loop  12
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
i0 = max(j + 1, n0ik)
i_aibjcialbj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcialbj(nocc, a, i, b, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcialbj
end do a_aibjcialbj
end do j_aibjcialbj
end do b_aibjcialbj
end do c_aibjcialbj
end do l_aibjcialbj
!
! Elementary loop  13
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
i0 = max(j + 1, n0ik)
i_aibjciajbm: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjciajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjciajbm(nocc, a, i, b, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjciajbm
end do a_aibjciajbm
end do j_aibjciajbm
end do b_aibjciajbm
end do c_aibjciajbm
end do m_aibjciajbm
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, m
! Equalities: d == a, e == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjcjajbm: do m = n0m, n1m
c_aibjcjajbm: do c = n0c, n1c
b_aibjcjajbm: do b = n0be, n1be
if (b == c) cycle b_aibjcjajbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jkl)
j_aibjcjajbm: do j = j0, n1jkl
if (j == m) cycle j_aibjcjajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjajbm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjcjajbm: do i = i0, n1i
if (i == j .or. i == m) cycle i_aibjcjajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjajbm(i, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcjajbm
end do a_aibjcjajbm
end do j_aibjcjajbm
end do b_aibjcjajbm
end do c_aibjcjajbm
end do m_aibjcjajbm
!
! Elementary loop  15
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
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjcjalbi: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjcjalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjalbi(nocc, a, i, b, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcjalbi
end do a_aibjcjalbi
end do j_aibjcjalbi
end do b_aibjcjalbi
end do c_aibjcjalbi
end do l_aibjcjalbi
!
! Elementary loop  16
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
i0 = max(j + 1, m + 1, n0il)
i_aibjcjaibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjaibm(nocc, a, i, b, j, c, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcjaibm
end do a_aibjcjaibm
end do j_aibjcjaibm
end do b_aibjcjaibm
end do c_aibjcjaibm
end do m_aibjcjaibm
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, j
! Equalities: d == a, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
c_aibjckakbi: do c = n0c, n1c
k_aibjckakbi: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakbi: do b = n0be, n1be
if (b == c) cycle b_aibjckakbi
j_aibjckakbi: do j = n0j, n1j
if (j == k) cycle j_aibjckakbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckakbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakbi
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(k - 1, n1im)
i_aibjckakbi: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjckakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckakbi(j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjckakbi
end do a_aibjckakbi
end do j_aibjckakbi
end do b_aibjckakbi
end do k_aibjckakbi
end do c_aibjckakbi
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
c_aibjckakbj: do c = n0c, n1c
k_aibjckakbj: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckakbj: do b = n0be, n1be
if (b == c) cycle b_aibjckakbj
j1 = min(k - 1, n1jm)
j_aibjckakbj: do j = n0jm, j1
if (j == k) cycle j_aibjckakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckakbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckakbj
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjckakbj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjckakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckakbj(i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjckakbj
end do a_aibjckakbj
end do j_aibjckakbj
end do b_aibjckakbj
end do k_aibjckakbj
end do c_aibjckakbj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjckaibj: do c = n0c, n1c
k_aibjckaibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibj: do b = n0be, n1be
if (b == c) cycle b_aibjckaibj
j_aibjckaibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibj
i0 = max(j + 1, n0il)
i_aibjckaibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckaibj(nocc, a, i, b, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjckaibj
end do a_aibjckaibj
end do j_aibjckaibj
end do b_aibjckaibj
end do k_aibjckaibj
end do c_aibjckaibj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjaiej: do e = n0e, n1e
c_aibjcjaiej: do c = n0c, n1c
if (c == e) cycle c_aibjcjaiej
b_aibjcjaiej: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcjaiej
j_aibjcjaiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjcjaiej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjaiej
i0 = max(j + 1, n0il)
i_aibjcjaiej: do i = i0, n1il
if (i == j) cycle i_aibjcjaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjaiej(b, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcjaiej
end do a_aibjcjaiej
end do j_aibjcjaiej
end do b_aibjcjaiej
end do c_aibjcjaiej
end do e_aibjcjaiej
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjciaiej: do e = n0e, n1e
c_aibjciaiej: do c = n0c, n1c
if (c == e) cycle c_aibjciaiej
b_aibjciaiej: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjciaiej
j_aibjciaiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjciaiej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjciaiej
i0 = max(j + 1, n0ikl)
i_aibjciaiej: do i = i0, n1ikl
if (i == j) cycle i_aibjciaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjciaiej(i, b, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjciaiej
end do a_aibjciaiej
end do j_aibjciaiej
end do b_aibjciaiej
end do c_aibjciaiej
end do e_aibjciaiej
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdiaj: do d = n0d, n1d
c_aibjcjdiaj: do c = n0c, n1c
if (c == d) cycle c_aibjcjdiaj
b_aibjcjdiaj: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdiaj
j_aibjcjdiaj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdiaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjcjdiaj: do i = i0, n1il
if (i == j) cycle i_aibjcjdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjdiaj(b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcjdiaj
end do a_aibjcjdiaj
end do j_aibjcjdiaj
end do b_aibjcjdiaj
end do c_aibjcjdiaj
end do d_aibjcjdiaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: e == a, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidiaj: do d = n0d, n1d
c_aibjcidiaj: do c = n0c, n1c
if (c == d) cycle c_aibjcidiaj
b_aibjcidiaj: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidiaj
j_aibjcidiaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcidiaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aibjcidiaj: do i = i0, n1ikl
if (i == j) cycle i_aibjcidiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcidiaj(i, b, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcidiaj
end do a_aibjcidiaj
end do j_aibjcidiaj
end do b_aibjcidiaj
end do c_aibjcidiaj
end do d_aibjcidiaj
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjcjbiej: do e = n0e, n1e
c_aibjcjbiej: do c = n0c, n1c
if (c == e) cycle c_aibjcjbiej
b0 = max(e + 1, n0bd)
b_aibjcjbiej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcjbiej
j_aibjcjbiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbiej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjbiej
i0 = max(j + 1, n0il)
i_aibjcjbiej: do i = i0, n1il
if (i == j) cycle i_aibjcjbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjbiej(a, j, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcjbiej
end do a_aibjcjbiej
end do j_aibjcjbiej
end do b_aibjcjbiej
end do c_aibjcjbiej
end do e_aibjcjbiej
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjcibiej: do e = n0e, n1e
c_aibjcibiej: do c = n0c, n1c
if (c == e) cycle c_aibjcibiej
b0 = max(e + 1, n0bd)
b_aibjcibiej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcibiej
j_aibjcibiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibiej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibiej
i0 = max(j + 1, n0ikl)
i_aibjcibiej: do i = i0, n1ikl
if (i == j) cycle i_aibjcibiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcibiej(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcibiej
end do a_aibjcibiej
end do j_aibjcibiej
end do b_aibjcibiej
end do c_aibjcibiej
end do e_aibjcibiej
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j
! Equalities: e == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
d_aibjcjdibj: do d = n0d, n1d
c_aibjcjdibj: do c = n0c, n1c
if (c == d) cycle c_aibjcjdibj
b1 = min(d - 1, n1be)
b_aibjcjdibj: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcjdibj
j_aibjcjdibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdibj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdibj
i0 = max(j + 1, n0il)
i_aibjcjdibj: do i = i0, n1il
if (i == j) cycle i_aibjcjdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjdibj(a, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcjdibj
end do a_aibjcjdibj
end do j_aibjcjdibj
end do b_aibjcjdibj
end do c_aibjcjdibj
end do d_aibjcjdibj
!
! Elementary loop  27
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcidibj(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjcidibj
end do a_aibjcidibj
end do j_aibjcidibj
end do b_aibjcidibj
end do c_aibjcidibj
end do d_aibjcidibj
!
! Elementary loop  28
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
i0 = max(j + 1, n0ikm)
i1 = min(l - 1, n1ikm)
i_aibjaialbi: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjaialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaialbi(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjaialbi
end do a_aibjaialbi
end do j_aibjaialbi
end do b_aibjaialbi
end do l_aibjaialbi
!
! Elementary loop  29
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
i0 = max(j + 1, n0i)
i_aibjajalbj: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjajalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajalbj(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjajalbj
end do a_aibjajalbj
end do j_aibjajalbj
end do b_aibjajalbj
end do l_aibjajalbj
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == a, e == b, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibjakaibk: do k = n0km, n1km
b_aibjakaibk: do b = n0be, n1be
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakaibk: do j = n0j, n1j
if (j == k) cycle j_aibjakaibk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakaibk: do a = a0, n1acd
if (a == b) cycle a_aibjakaibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, k + 1, n0il)
i_aibjakaibk: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakaibk(a, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part4', ibra, iket, jac_ibra_iket
end do i_aibjakaibk
end do a_aibjakaibk
end do j_aibjakaibk
end do b_aibjakaibk
end do k_aibjakaibk
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
end subroutine ccjac_23_tripletp_dav_part4
end module ccjac_block_23_tripletp_dav_part4
