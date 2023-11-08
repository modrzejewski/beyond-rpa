module ccjac_block_23_tripletp_dav_part2
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
subroutine ccjac_23_tripletp_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, d0, i0, i1, j0, j1, l0
integer :: n0ad, n0ae, n0bc, n0bd, n0be
integer :: n0ik, n0il, n0im, n0jk, n0jl
integer :: n0jm
integer :: n1ad, n1ae, n1bc, n1bd, n1be
integer :: n1ik, n1il, n1im, n1jk, n1jl
integer :: n1jm
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
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, m
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjbidjem: do e = n0e, n1e
m_aibjbidjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aibjbidjem: do d = d0, n1d
if (d == e) cycle d_aibjbidjem
b_aibjbidjem: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibjbidjem
j0 = max(m + 1, n0jl)
j_aibjbidjem: do j = j0, n1jl
if (j == m) cycle j_aibjbidjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidjem: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbidjem
i0 = max(j + 1, n0ik)
i_aibjbidjem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjbidjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbidjem(a, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjbidjem
end do a_aibjbidjem
end do j_aibjbidjem
end do b_aibjbidjem
end do d_aibjbidjem
end do m_aibjbidjem
end do e_aibjbidjem
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, l
! Equalities: c == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjdlei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbjdlei: do d = d0, n1d
if (d == e) cycle d_aibjbjdlei
l_aibjbjdlei: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbjdlei: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibjbjdlei
j_aibjbjdlei: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjdlei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdlei: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdlei
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjbjdlei: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjbjdlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjdlei(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjbjdlei
end do a_aibjbjdlei
end do j_aibjbjdlei
end do b_aibjbjdlei
end do l_aibjbjdlei
end do d_aibjbjdlei
end do e_aibjbjdlei
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, m
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjbjdiem: do e = n0e, n1e
m_aibjbjdiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aibjbjdiem: do d = d0, n1d
if (d == e) cycle d_aibjbjdiem
b_aibjbjdiem: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibjbjdiem
j_aibjbjdiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjbjdiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdiem: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbjdiem
i0 = max(j + 1, m + 1, n0il)
i_aibjbjdiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjbjdiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjdiem(a, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjbjdiem
end do a_aibjbjdiem
end do j_aibjbjdiem
end do b_aibjbjdiem
end do d_aibjbjdiem
end do m_aibjbjdiem
end do e_aibjbjdiem
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, k
! Equalities: c == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbkdiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbkdiej: do d = d0, n1d
if (d == e) cycle d_aibjbkdiej
k_aibjbkdiej: do k = n0k, n1k
b_aibjbkdiej: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibjbkdiej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjbkdiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdiej: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbkdiej
i0 = max(j + 1, n0il)
i_aibjbkdiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkdiej(a, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjbkdiej
end do a_aibjbkdiej
end do j_aibjbkdiej
end do b_aibjbkdiej
end do k_aibjbkdiej
end do d_aibjbkdiej
end do e_aibjbkdiej
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l, m
! Equalities: d == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjcialbm: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aibjcialbm: do l = l0, n1l
if (l == m) cycle l_aibjcialbm
c_aibjcialbm: do c = n0c, n1c
b_aibjcialbm: do b = n0be, n1be
if (b == c) cycle b_aibjcialbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcialbm: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjcialbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcialbm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcialbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjcialbm: do i = i0, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjcialbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcialbm(j, c, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcialbm
end do a_aibjcialbm
end do j_aibjcialbm
end do b_aibjcialbm
end do c_aibjcialbm
end do l_aibjcialbm
end do m_aibjcialbm
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l, m
! Equalities: d == a, e == b, k == j
! No equalities independent of the above can hold.
!
m_aibjcjalbm: do m = n0m, n1m
l0 = max(m + 1, n0l)
l_aibjcjalbm: do l = l0, n1l
if (l == m) cycle l_aibjcjalbm
c_aibjcjalbm: do c = n0c, n1c
b_aibjcjalbm: do b = n0be, n1be
if (b == c) cycle b_aibjcjalbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjalbm: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aibjcjalbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjalbm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjalbm
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjcjalbm: do i = i0, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjcjalbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjalbm(i, c, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcjalbm
end do a_aibjcjalbm
end do j_aibjcjalbm
end do b_aibjcjalbm
end do c_aibjcjalbm
end do l_aibjcjalbm
end do m_aibjcjalbm
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, l
! Equalities: d == a, e == b, m == i
! No equalities independent of the above can hold.
!
l_aibjckalbi: do l = n0l, n1l
c_aibjckalbi: do c = n0c, n1c
k_aibjckalbi: do k = n0k, n1k
if (k == l) cycle k_aibjckalbi
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalbi: do b = n0be, n1be
if (b == c) cycle b_aibjckalbi
j_aibjckalbi: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckalbi: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalbi
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjckalbi: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckalbi(j, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjckalbi
end do a_aibjckalbi
end do j_aibjckalbi
end do b_aibjckalbi
end do k_aibjckalbi
end do c_aibjckalbi
end do l_aibjckalbi
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, l
! Equalities: d == a, e == b, m == j
! No equalities independent of the above can hold.
!
l_aibjckalbj: do l = n0l, n1l
c_aibjckalbj: do c = n0c, n1c
k_aibjckalbj: do k = n0k, n1k
if (k == l) cycle k_aibjckalbj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckalbj: do b = n0be, n1be
if (b == c) cycle b_aibjckalbj
j1 = min(l - 1, n1jm)
j_aibjckalbj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjckalbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckalbj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckalbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjckalbj: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckalbj(i, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjckalbj
end do a_aibjckalbj
end do j_aibjckalbj
end do b_aibjckalbj
end do k_aibjckalbj
end do c_aibjckalbj
end do l_aibjckalbj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, m
! Equalities: d == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjckaibm: do m = n0m, n1m
c_aibjckaibm: do c = n0c, n1c
k_aibjckaibm: do k = n0k, n1k
if (k == m) cycle k_aibjckaibm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaibm: do b = n0be, n1be
if (b == c) cycle b_aibjckaibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjckaibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjckaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaibm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaibm
i0 = max(j + 1, m + 1, n0il)
i_aibjckaibm: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjckaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckaibm(j, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjckaibm
end do a_aibjckaibm
end do j_aibjckaibm
end do b_aibjckaibm
end do k_aibjckaibm
end do c_aibjckaibm
end do m_aibjckaibm
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k, m
! Equalities: d == a, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjckajbm: do m = n0m, n1m
c_aibjckajbm: do c = n0c, n1c
k_aibjckajbm: do k = n0k, n1k
if (k == m) cycle k_aibjckajbm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckajbm: do b = n0be, n1be
if (b == c) cycle b_aibjckajbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjckajbm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjckajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckajbm: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjckajbm: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckajbm(i, c, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjckajbm
end do a_aibjckajbm
end do j_aibjckajbm
end do b_aibjckajbm
end do k_aibjckajbm
end do c_aibjckajbm
end do m_aibjckajbm
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, l
! Equalities: d == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjcialej: do e = n0e, n1e
l_aibjcialej: do l = n0l, n1l
c_aibjcialej: do c = n0c, n1c
if (c == e) cycle c_aibjcialej
b_aibjcialej: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcialej
j1 = min(l - 1, n1jm)
j_aibjcialej: do j = n0jm, j1
if (j == l) cycle j_aibjcialej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjcialej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcialej
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjcialej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcialej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcialej(b, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcialej
end do a_aibjcialej
end do j_aibjcialej
end do b_aibjcialej
end do c_aibjcialej
end do l_aibjcialej
end do e_aibjcialej
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, m
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjciajem: do e = n0e, n1e
m_aibjciajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibjciajem: do c = n0c, n1c
if (c == e) cycle c_aibjciajem
b_aibjciajem: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjciajem
j0 = max(m + 1, n0jl)
j_aibjciajem: do j = j0, n1jl
if (j == m) cycle j_aibjciajem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjciajem: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjciajem
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjciajem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjciajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjciajem(b, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjciajem
end do a_aibjciajem
end do j_aibjciajem
end do b_aibjciajem
end do c_aibjciajem
end do m_aibjciajem
end do e_aibjciajem
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, l
! Equalities: d == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjalei: do e = n0e, n1e
l_aibjcjalei: do l = n0l, n1l
c_aibjcjalei: do c = n0c, n1c
if (c == e) cycle c_aibjcjalei
b_aibjcjalei: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcjalei
j_aibjcjalei: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjalei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjcjalei: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjalei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjcjalei: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjcjalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjalei(b, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcjalei
end do a_aibjcjalei
end do j_aibjcjalei
end do b_aibjcjalei
end do c_aibjcjalei
end do l_aibjcjalei
end do e_aibjcjalei
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, m
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjaiem: do e = n0e, n1e
m_aibjcjaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibjcjaiem: do c = n0c, n1c
if (c == e) cycle c_aibjcjaiem
b_aibjcjaiem: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjcjaiem
j_aibjcjaiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjcjaiem: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjcjaiem
i0 = max(j + 1, m + 1, n0il)
i_aibjcjaiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjaiem(b, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcjaiem
end do a_aibjcjaiem
end do j_aibjcjaiem
end do b_aibjcjaiem
end do c_aibjcjaiem
end do m_aibjcjaiem
end do e_aibjcjaiem
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c, e
! Free occupied indices: i, j, k
! Equalities: d == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckaiej: do e = n0e, n1e
c_aibjckaiej: do c = n0c, n1c
if (c == e) cycle c_aibjckaiej
k_aibjckaiej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckaiej: do b = n0b, n1b
if (b == c .or. b == e) cycle b_aibjckaiej
j_aibjckaiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjckaiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjckaiej: do a = a0, n1ad
if (a == b .or. a == c .or. a == e) cycle a_aibjckaiej
i0 = max(j + 1, n0il)
i_aibjckaiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckaiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckaiej(b, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjckaiej
end do a_aibjckaiej
end do j_aibjckaiej
end do b_aibjckaiej
end do k_aibjckaiej
end do c_aibjckaiej
end do e_aibjckaiej
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, l
! Equalities: e == a, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidlaj: do d = n0d, n1d
l_aibjcidlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibjcidlaj: do c = n0c, n1c
if (c == d) cycle c_aibjcidlaj
b_aibjcidlaj: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidlaj
j1 = min(l - 1, n1jm)
j_aibjcidlaj: do j = n0jm, j1
if (j == l) cycle j_aibjcidlaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcidlaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidlaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjcidlaj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcidlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcidlaj(b, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcidlaj
end do a_aibjcidlaj
end do j_aibjcidlaj
end do b_aibjcidlaj
end do c_aibjcidlaj
end do l_aibjcidlaj
end do d_aibjcidlaj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, m
! Equalities: e == a, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcidjam: do m = n0m, n1m
d_aibjcidjam: do d = n0d, n1d
c_aibjcidjam: do c = n0c, n1c
if (c == d) cycle c_aibjcidjam
b_aibjcidjam: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidjam
j0 = max(m + 1, n0jl)
j_aibjcidjam: do j = j0, n1jl
if (j == m) cycle j_aibjcidjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcidjam: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjcidjam: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjcidjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcidjam(b, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcidjam
end do a_aibjcidjam
end do j_aibjcidjam
end do b_aibjcidjam
end do c_aibjcidjam
end do d_aibjcidjam
end do m_aibjcidjam
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, l
! Equalities: e == a, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdlai: do d = n0d, n1d
l_aibjcjdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibjcjdlai: do c = n0c, n1c
if (c == d) cycle c_aibjcjdlai
b_aibjcjdlai: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdlai
j_aibjcjdlai: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjdlai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdlai: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdlai
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjcjdlai: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjcjdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjdlai(b, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcjdlai
end do a_aibjcjdlai
end do j_aibjcjdlai
end do b_aibjcjdlai
end do c_aibjcjdlai
end do l_aibjcjdlai
end do d_aibjcjdlai
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, m
! Equalities: e == a, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjdiam: do m = n0m, n1m
d_aibjcjdiam: do d = n0d, n1d
c_aibjcjdiam: do c = n0c, n1c
if (c == d) cycle c_aibjcjdiam
b_aibjcjdiam: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdiam
j_aibjcjdiam: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjdiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjcjdiam: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjcjdiam: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjdiam(b, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcjdiam
end do a_aibjcjdiam
end do j_aibjcjdiam
end do b_aibjcjdiam
end do c_aibjcjdiam
end do d_aibjcjdiam
end do m_aibjcjdiam
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: e == a, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdiaj: do d = n0d, n1d
c_aibjckdiaj: do c = n0c, n1c
if (c == d) cycle c_aibjckdiaj
k_aibjckdiaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckdiaj: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjckdiaj
j_aibjckdiaj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckdiaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjckdiaj: do a = a0, a1
if (a == b .or. a == c .or. a == d) cycle a_aibjckdiaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjckdiaj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckdiaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckdiaj(b, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjckdiaj
end do a_aibjckdiaj
end do j_aibjckdiaj
end do b_aibjckdiaj
end do k_aibjckdiaj
end do c_aibjckdiaj
end do d_aibjckdiaj
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, l
! Equalities: d == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjciblej: do e = n0e, n1e
l_aibjciblej: do l = n0l, n1l
c_aibjciblej: do c = n0c, n1c
if (c == e) cycle c_aibjciblej
b0 = max(e + 1, n0bd)
b_aibjciblej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjciblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjciblej: do j = n0jm, j1
if (j == l) cycle j_aibjciblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjciblej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjciblej
i0 = max(j + 1, n0ik)
i_aibjciblej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjciblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjciblej(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjciblej
end do a_aibjciblej
end do j_aibjciblej
end do b_aibjciblej
end do c_aibjciblej
end do l_aibjciblej
end do e_aibjciblej
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, m
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcibjem: do e = n0e, n1e
m_aibjcibjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibjcibjem: do c = n0c, n1c
if (c == e) cycle c_aibjcibjem
b0 = max(e + 1, n0bd)
b_aibjcibjem: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcibjem
j0 = max(m + 1, n0jl)
j_aibjcibjem: do j = j0, n1jl
if (j == m) cycle j_aibjcibjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibjem: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcibjem
i0 = max(j + 1, n0ik)
i_aibjcibjem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjcibjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcibjem(a, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcibjem
end do a_aibjcibjem
end do j_aibjcibjem
end do b_aibjcibjem
end do c_aibjcibjem
end do m_aibjcibjem
end do e_aibjcibjem
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, l
! Equalities: d == b, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjblei: do e = n0e, n1e
l_aibjcjblei: do l = n0l, n1l
c_aibjcjblei: do c = n0c, n1c
if (c == e) cycle c_aibjcjblei
b0 = max(e + 1, n0bd)
b_aibjcjblei: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcjblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjblei: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjblei: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjblei
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjcjblei: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjcjblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjblei(a, c, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcjblei
end do a_aibjcjblei
end do j_aibjcjblei
end do b_aibjcjblei
end do c_aibjcjblei
end do l_aibjcjblei
end do e_aibjcjblei
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, m
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjcjbiem: do e = n0e, n1e
m_aibjcjbiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
c_aibjcjbiem: do c = n0c, n1c
if (c == e) cycle c_aibjcjbiem
b0 = max(e + 1, n0bd)
b_aibjcjbiem: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjcjbiem
j_aibjcjbiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjbiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbiem: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcjbiem
i0 = max(j + 1, m + 1, n0il)
i_aibjcjbiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjbiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjbiem(a, c, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcjbiem
end do a_aibjcjbiem
end do j_aibjcjbiem
end do b_aibjcjbiem
end do c_aibjcjbiem
end do m_aibjcjbiem
end do e_aibjcjbiem
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: i, j, k
! Equalities: d == b, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjckbiej: do e = n0e, n1e
c_aibjckbiej: do c = n0c, n1c
if (c == e) cycle c_aibjckbiej
k_aibjckbiej: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibjckbiej: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbiej
j_aibjckbiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjckbiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbiej: do a = a0, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbiej
i0 = max(j + 1, n0il)
i_aibjckbiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckbiej(a, c, k, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjckbiej
end do a_aibjckbiej
end do j_aibjckbiej
end do b_aibjckbiej
end do k_aibjckbiej
end do c_aibjckbiej
end do e_aibjckbiej
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, l
! Equalities: e == b, k == i, m == j
! No equalities independent of the above can hold.
!
d_aibjcidlbj: do d = n0d, n1d
l_aibjcidlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibjcidlbj: do c = n0c, n1c
if (c == d) cycle c_aibjcidlbj
b1 = min(d - 1, n1be)
b_aibjcidlbj: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidlbj
j1 = min(l - 1, n1jm)
j_aibjcidlbj: do j = n0jm, j1
if (j == l) cycle j_aibjcidlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidlbj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidlbj
i0 = max(j + 1, n0ik)
i_aibjcidlbj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcidlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcidlbj(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcidlbj
end do a_aibjcidlbj
end do j_aibjcidlbj
end do b_aibjcidlbj
end do c_aibjcidlbj
end do l_aibjcidlbj
end do d_aibjcidlbj
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, m
! Equalities: e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjcidjbm: do m = n0m, n1m
d_aibjcidjbm: do d = n0d, n1d
c_aibjcidjbm: do c = n0c, n1c
if (c == d) cycle c_aibjcidjbm
b1 = min(d - 1, n1be)
b_aibjcidjbm: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcidjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjcidjbm: do j = j0, n1jl
if (j == m) cycle j_aibjcidjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidjbm: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidjbm
i0 = max(j + 1, n0ik)
i_aibjcidjbm: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjcidjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcidjbm(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcidjbm
end do a_aibjcidjbm
end do j_aibjcidjbm
end do b_aibjcidjbm
end do c_aibjcidjbm
end do d_aibjcidjbm
end do m_aibjcidjbm
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, l
! Equalities: e == b, m == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdlbi: do d = n0d, n1d
l_aibjcjdlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
c_aibjcjdlbi: do c = n0c, n1c
if (c == d) cycle c_aibjcjdlbi
b1 = min(d - 1, n1be)
b_aibjcjdlbi: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcjdlbi
j_aibjcjdlbi: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjdlbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdlbi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdlbi
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjcjdlbi: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjcjdlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjdlbi(a, c, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcjdlbi
end do a_aibjcjdlbi
end do j_aibjcjdlbi
end do b_aibjcjdlbi
end do c_aibjcjdlbi
end do l_aibjcjdlbi
end do d_aibjcjdlbi
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, m
! Equalities: e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjcjdibm: do m = n0m, n1m
d_aibjcjdibm: do d = n0d, n1d
c_aibjcjdibm: do c = n0c, n1c
if (c == d) cycle c_aibjcjdibm
b1 = min(d - 1, n1be)
b_aibjcjdibm: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjcjdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjcjdibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjcjdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdibm: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdibm
i0 = max(j + 1, m + 1, n0il)
i_aibjcjdibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjcjdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjdibm(a, c, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjcjdibm
end do a_aibjcjdibm
end do j_aibjcjdibm
end do b_aibjcjdibm
end do c_aibjcjdibm
end do d_aibjcjdibm
end do m_aibjcjdibm
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: i, j, k
! Equalities: e == b, l == i, m == j
! No equalities independent of the above can hold.
!
d_aibjckdibj: do d = n0d, n1d
c_aibjckdibj: do c = n0c, n1c
if (c == d) cycle c_aibjckdibj
k_aibjckdibj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjckdibj: do b = n0be, b1
if (b == c .or. b == d) cycle b_aibjckdibj
j_aibjckdibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjckdibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdibj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdibj
i0 = max(j + 1, n0il)
i_aibjckdibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckdibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjckdibj(a, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part2', ibra, iket, jac_ibra_iket
end do i_aibjckdibj
end do a_aibjckdibj
end do j_aibjckdibj
end do b_aibjckdibj
end do k_aibjckdibj
end do c_aibjckdibj
end do d_aibjckdibj
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
end subroutine ccjac_23_tripletp_dav_part2
end module ccjac_block_23_tripletp_dav_part2
