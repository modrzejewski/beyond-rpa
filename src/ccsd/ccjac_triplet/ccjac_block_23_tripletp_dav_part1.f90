module ccjac_block_23_tripletp_dav_part1
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
subroutine ccjac_23_tripletp_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, d0, i0, i1, j0, j1, l0
integer :: n0ac, n0ad, n0ae, n0bc, n0bd
integer :: n0be, n0ik, n0il, n0im, n0jk
integer :: n0jl, n0jm
integer :: n1ac, n1ad, n1ae, n1bc, n1bd
integer :: n1be, n1ik, n1il, n1im, n1jk
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
n1ac = min(n1a, n1c)
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
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l, m
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
e_aibjaiblem: do e = n0e, n1e
m_aibjaiblem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aibjaiblem: do l = l0, n1l
if (l == m) cycle l_aibjaiblem
b0 = max(e + 1, n0bd)
b_aibjaiblem: do b = b0, n1bd
if (b == e) cycle b_aibjaiblem
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaiblem: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjaiblem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaiblem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaiblem
i0 = max(j + 1, n0ik)
i_aibjaiblem: do i = i0, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjaiblem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaiblem(j, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjaiblem
end do a_aibjaiblem
end do j_aibjaiblem
end do b_aibjaiblem
end do l_aibjaiblem
end do m_aibjaiblem
end do e_aibjaiblem
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, l, m
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
e_aibjajblem: do e = n0e, n1e
m_aibjajblem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aibjajblem: do l = l0, n1l
if (l == m) cycle l_aibjajblem
b0 = max(e + 1, n0bd)
b_aibjajblem: do b = b0, n1bd
if (b == e) cycle b_aibjajblem
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajblem: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aibjajblem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajblem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjajblem
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjajblem: do i = i0, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjajblem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajblem(i, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjajblem
end do a_aibjajblem
end do j_aibjajblem
end do b_aibjajblem
end do l_aibjajblem
end do m_aibjajblem
end do e_aibjajblem
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b, m == i
! No equalities independent of the above can hold.
!
e_aibjakblei: do e = n0e, n1e
l_aibjakblei: do l = n0l, n1l
k_aibjakblei: do k = n0k, n1k
if (k == l) cycle k_aibjakblei
b0 = max(e + 1, n0bd)
b_aibjakblei: do b = b0, n1bd
if (b == e) cycle b_aibjakblei
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjakblei: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakblei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakblei: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakblei
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjakblei: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjakblei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakblei(j, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjakblei
end do a_aibjakblei
end do j_aibjakblei
end do b_aibjakblei
end do k_aibjakblei
end do l_aibjakblei
end do e_aibjakblei
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k, l
! Equalities: c == a, d == b, m == j
! No equalities independent of the above can hold.
!
e_aibjakblej: do e = n0e, n1e
l_aibjakblej: do l = n0l, n1l
k_aibjakblej: do k = n0k, n1k
if (k == l) cycle k_aibjakblej
b0 = max(e + 1, n0bd)
b_aibjakblej: do b = b0, n1bd
if (b == e) cycle b_aibjakblej
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjakblej: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjakblej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakblej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakblej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjakblej: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakblej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakblej(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjakblej
end do a_aibjakblej
end do j_aibjakblej
end do b_aibjakblej
end do k_aibjakblej
end do l_aibjakblej
end do e_aibjakblej
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k, m
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
e_aibjakbiem: do e = n0e, n1e
m_aibjakbiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibjakbiem: do k = n0k, n1k
if (k == m) cycle k_aibjakbiem
b0 = max(e + 1, n0bd)
b_aibjakbiem: do b = b0, n1bd
if (b == e) cycle b_aibjakbiem
j_aibjakbiem: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjakbiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbiem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbiem
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjakbiem: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjakbiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakbiem(j, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjakbiem
end do a_aibjakbiem
end do j_aibjakbiem
end do b_aibjakbiem
end do k_aibjakbiem
end do m_aibjakbiem
end do e_aibjakbiem
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k, m
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
e_aibjakbjem: do e = n0e, n1e
m_aibjakbjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibjakbjem: do k = n0k, n1k
if (k == m) cycle k_aibjakbjem
b0 = max(e + 1, n0bd)
b_aibjakbjem: do b = b0, n1bd
if (b == e) cycle b_aibjakbjem
j0 = max(m + 1, n0jl)
j_aibjakbjem: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjakbjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbjem: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjakbjem
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjakbjem: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakbjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakbjem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjakbjem
end do a_aibjakbjem
end do j_aibjakbjem
end do b_aibjakbjem
end do k_aibjakbjem
end do m_aibjakbjem
end do e_aibjakbjem
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l, m
! Equalities: c == a, e == b, k == i
! No equalities independent of the above can hold.
!
m_aibjaidlbm: do m = n0m, n1m
d_aibjaidlbm: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aibjaidlbm: do l = l0, n1l
if (l == m) cycle l_aibjaidlbm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjaidlbm: do b = n0be, b1
if (b == d) cycle b_aibjaidlbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaidlbm: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjaidlbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidlbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidlbm
i0 = max(j + 1, n0ik)
i_aibjaidlbm: do i = i0, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjaidlbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaidlbm(j, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjaidlbm
end do a_aibjaidlbm
end do j_aibjaidlbm
end do b_aibjaidlbm
end do l_aibjaidlbm
end do d_aibjaidlbm
end do m_aibjaidlbm
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l, m
! Equalities: c == a, e == b, k == j
! No equalities independent of the above can hold.
!
m_aibjajdlbm: do m = n0m, n1m
d_aibjajdlbm: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aibjajdlbm: do l = l0, n1l
if (l == m) cycle l_aibjajdlbm
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b1 = min(d - 1, n1be)
b_aibjajdlbm: do b = n0be, b1
if (b == d) cycle b_aibjajdlbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajdlbm: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aibjajdlbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajdlbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdlbm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjajdlbm: do i = i0, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjajdlbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajdlbm(i, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjajdlbm
end do a_aibjajdlbm
end do j_aibjajdlbm
end do b_aibjajdlbm
end do l_aibjajdlbm
end do d_aibjajdlbm
end do m_aibjajdlbm
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k, l
! Equalities: c == a, e == b, m == i
! No equalities independent of the above can hold.
!
d_aibjakdlbi: do d = n0d, n1d
l_aibjakdlbi: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibjakdlbi: do k = n0k, n1k
if (k == l) cycle k_aibjakdlbi
b1 = min(d - 1, n1be)
b_aibjakdlbi: do b = n0be, b1
if (b == d) cycle b_aibjakdlbi
j_aibjakdlbi: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakdlbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdlbi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdlbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjakdlbi: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjakdlbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakdlbi(j, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjakdlbi
end do a_aibjakdlbi
end do j_aibjakdlbi
end do b_aibjakdlbi
end do k_aibjakdlbi
end do l_aibjakdlbi
end do d_aibjakdlbi
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k, l
! Equalities: c == a, e == b, m == j
! No equalities independent of the above can hold.
!
d_aibjakdlbj: do d = n0d, n1d
l_aibjakdlbj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibjakdlbj: do k = n0k, n1k
if (k == l) cycle k_aibjakdlbj
b1 = min(d - 1, n1be)
b_aibjakdlbj: do b = n0be, b1
if (b == d) cycle b_aibjakdlbj
j1 = min(l - 1, n1jm)
j_aibjakdlbj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjakdlbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdlbj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdlbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjakdlbj: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakdlbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakdlbj(i, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjakdlbj
end do a_aibjakdlbj
end do j_aibjakdlbj
end do b_aibjakdlbj
end do k_aibjakdlbj
end do l_aibjakdlbj
end do d_aibjakdlbj
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k, m
! Equalities: c == a, e == b, l == i
! No equalities independent of the above can hold.
!
m_aibjakdibm: do m = n0m, n1m
d_aibjakdibm: do d = n0d, n1d
k_aibjakdibm: do k = n0k, n1k
if (k == m) cycle k_aibjakdibm
b1 = min(d - 1, n1be)
b_aibjakdibm: do b = n0be, b1
if (b == d) cycle b_aibjakdibm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjakdibm: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjakdibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdibm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdibm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjakdibm: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjakdibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakdibm(j, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjakdibm
end do a_aibjakdibm
end do j_aibjakdibm
end do b_aibjakdibm
end do k_aibjakdibm
end do d_aibjakdibm
end do m_aibjakdibm
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k, m
! Equalities: c == a, e == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakdjbm: do m = n0m, n1m
d_aibjakdjbm: do d = n0d, n1d
k_aibjakdjbm: do k = n0k, n1k
if (k == m) cycle k_aibjakdjbm
b1 = min(d - 1, n1be)
b_aibjakdjbm: do b = n0be, b1
if (b == d) cycle b_aibjakdjbm
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjakdjbm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjakdjbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdjbm: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdjbm
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjakdjbm: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakdjbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakdjbm(i, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjakdjbm
end do a_aibjakdjbm
end do j_aibjakdjbm
end do b_aibjakdjbm
end do k_aibjakdjbm
end do d_aibjakdjbm
end do m_aibjakdjbm
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, l
! Equalities: c == a, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaidlej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjaidlej: do d = d0, n1d
if (d == e) cycle d_aibjaidlej
l_aibjaidlej: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidlej: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjaidlej
j1 = min(l - 1, n1jm)
j_aibjaidlej: do j = n0jm, j1
if (j == l) cycle j_aibjaidlej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidlej: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidlej
i0 = max(j + 1, n0ik)
i_aibjaidlej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidlej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaidlej(b, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjaidlej
end do a_aibjaidlej
end do j_aibjaidlej
end do b_aibjaidlej
end do l_aibjaidlej
end do d_aibjaidlej
end do e_aibjaidlej
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, m
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
e_aibjaidjem: do e = n0e, n1e
m_aibjaidjem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aibjaidjem: do d = d0, n1d
if (d == e) cycle d_aibjaidjem
b_aibjaidjem: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjaidjem
j0 = max(m + 1, n0jl)
j_aibjaidjem: do j = j0, n1jl
if (j == m) cycle j_aibjaidjem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaidjem: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjaidjem
i0 = max(j + 1, n0ik)
i_aibjaidjem: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjaidjem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaidjem(b, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjaidjem
end do a_aibjaidjem
end do j_aibjaidjem
end do b_aibjaidjem
end do d_aibjaidjem
end do m_aibjaidjem
end do e_aibjaidjem
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, l
! Equalities: c == a, m == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajdlei: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjajdlei: do d = d0, n1d
if (d == e) cycle d_aibjajdlei
l_aibjajdlei: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjajdlei: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjajdlei
j_aibjajdlei: do j = n0jk, n1jk
if (j == l) cycle j_aibjajdlei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajdlei: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjajdlei
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjajdlei: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjajdlei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajdlei(b, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjajdlei
end do a_aibjajdlei
end do j_aibjajdlei
end do b_aibjajdlei
end do l_aibjajdlei
end do d_aibjajdlei
end do e_aibjajdlei
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, m
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
e_aibjajdiem: do e = n0e, n1e
m_aibjajdiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
d0 = max(e + 1, n0d)
d_aibjajdiem: do d = d0, n1d
if (d == e) cycle d_aibjajdiem
b_aibjajdiem: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjajdiem
j_aibjajdiem: do j = n0jk, n1jk
if (j == m) cycle j_aibjajdiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajdiem: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjajdiem
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjajdiem: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjajdiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajdiem(b, d, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjajdiem
end do a_aibjajdiem
end do j_aibjajdiem
end do b_aibjajdiem
end do d_aibjajdiem
end do m_aibjajdiem
end do e_aibjajdiem
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, d, e
! Free occupied indices: i, j, k
! Equalities: c == a, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjakdiej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjakdiej: do d = d0, n1d
if (d == e) cycle d_aibjakdiej
k_aibjakdiej: do k = n0k, n1k
b_aibjakdiej: do b = n0b, n1b
if (b == d .or. b == e) cycle b_aibjakdiej
j_aibjakdiej: do j = n0jm, n1jm
if (j == k) cycle j_aibjakdiej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakdiej: do a = a0, n1ac
if (a == b .or. a == d .or. a == e) cycle a_aibjakdiej
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakdiej: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakdiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakdiej(b, k, d, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjakdiej
end do a_aibjakdiej
end do j_aibjakdiej
end do b_aibjakdiej
end do k_aibjakdiej
end do d_aibjakdiej
end do e_aibjakdiej
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, l, m
! Equalities: d == a, c == b, k == i
! No equalities independent of the above can hold.
!
e_aibjbialem: do e = n0e, n1e
m_aibjbialem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aibjbialem: do l = l0, n1l
if (l == m) cycle l_aibjbialem
b_aibjbialem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbialem
j_aibjbialem: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjbialem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbialem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbialem
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjbialem: do i = i0, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjbialem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbialem(j, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbialem
end do a_aibjbialem
end do j_aibjbialem
end do b_aibjbialem
end do l_aibjbialem
end do m_aibjbialem
end do e_aibjbialem
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, l, m
! Equalities: d == a, c == b, k == j
! No equalities independent of the above can hold.
!
e_aibjbjalem: do e = n0e, n1e
m_aibjbjalem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
l0 = max(m + 1, n0l)
l_aibjbjalem: do l = l0, n1l
if (l == m) cycle l_aibjbjalem
b_aibjbjalem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbjalem
j_aibjbjalem: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aibjbjalem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbjalem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbjalem
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbjalem: do i = i0, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjbjalem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjalem(i, l, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbjalem
end do a_aibjbjalem
end do j_aibjbjalem
end do b_aibjbjalem
end do l_aibjbjalem
end do m_aibjbjalem
end do e_aibjbjalem
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k, l
! Equalities: d == a, c == b, m == i
! No equalities independent of the above can hold.
!
e_aibjbkalei: do e = n0e, n1e
l_aibjbkalei: do l = n0l, n1l
k_aibjbkalei: do k = n0k, n1k
if (k == l) cycle k_aibjbkalei
b_aibjbkalei: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkalei
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkalei: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjbkalei
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkalei: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkalei
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjbkalei: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkalei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkalei(j, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbkalei
end do a_aibjbkalei
end do j_aibjbkalei
end do b_aibjbkalei
end do k_aibjbkalei
end do l_aibjbkalei
end do e_aibjbkalei
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k, l
! Equalities: d == a, c == b, m == j
! No equalities independent of the above can hold.
!
e_aibjbkalej: do e = n0e, n1e
l_aibjbkalej: do l = n0l, n1l
k_aibjbkalej: do k = n0k, n1k
if (k == l) cycle k_aibjbkalej
b_aibjbkalej: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkalej
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjbkalej: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjbkalej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkalej: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkalej
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbkalej: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjbkalej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkalej(i, k, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbkalej
end do a_aibjbkalej
end do j_aibjbkalej
end do b_aibjbkalej
end do k_aibjbkalej
end do l_aibjbkalej
end do e_aibjbkalej
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j, k, m
! Equalities: d == a, c == b, l == i
! No equalities independent of the above can hold.
!
e_aibjbkaiem: do e = n0e, n1e
m_aibjbkaiem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibjbkaiem: do k = n0k, n1k
if (k == m) cycle k_aibjbkaiem
b_aibjbkaiem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkaiem
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkaiem: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjbkaiem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkaiem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkaiem
i0 = max(j + 1, m + 1, n0il)
i_aibjbkaiem: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjbkaiem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkaiem(j, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbkaiem
end do a_aibjbkaiem
end do j_aibjbkaiem
end do b_aibjbkaiem
end do k_aibjbkaiem
end do m_aibjbkaiem
end do e_aibjbkaiem
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, i, k, m
! Equalities: d == a, c == b, l == j
! No equalities independent of the above can hold.
!
e_aibjbkajem: do e = n0e, n1e
m_aibjbkajem: do m = n0m, n1m
em = (e - nvirt0) * nocc + (m - nocc0) + 1
k_aibjbkajem: do k = n0k, n1k
if (k == m) cycle k_aibjbkajem
b_aibjbkajem: do b = n0bc, n1bc
if (b == e) cycle b_aibjbkajem
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjbkajem: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjbkajem
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, e + 1, n0ad)
a_aibjbkajem: do a = a0, n1ad
if (a == b .or. a == e) cycle a_aibjbkajem
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbkajem: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjbkajem
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkajem(i, k, e, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbkajem
end do a_aibjbkajem
end do j_aibjbkajem
end do b_aibjbkajem
end do k_aibjbkajem
end do m_aibjbkajem
end do e_aibjbkajem
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l, m
! Equalities: e == a, c == b, k == i
! No equalities independent of the above can hold.
!
m_aibjbidlam: do m = n0m, n1m
d_aibjbidlam: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aibjbidlam: do l = l0, n1l
if (l == m) cycle l_aibjbidlam
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbidlam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidlam
j_aibjbidlam: do j = n0j, n1j
if (j == l .or. j == m) cycle j_aibjbidlam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbidlam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbidlam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjbidlam: do i = i0, n1ik
if (i == j .or. i == l .or. i == m) cycle i_aibjbidlam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbidlam(j, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbidlam
end do a_aibjbidlam
end do j_aibjbidlam
end do b_aibjbidlam
end do l_aibjbidlam
end do d_aibjbidlam
end do m_aibjbidlam
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l, m
! Equalities: e == a, c == b, k == j
! No equalities independent of the above can hold.
!
m_aibjbjdlam: do m = n0m, n1m
d_aibjbjdlam: do d = n0d, n1d
l0 = max(m + 1, n0l)
l_aibjbjdlam: do l = l0, n1l
if (l == m) cycle l_aibjbjdlam
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbjdlam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbjdlam
j_aibjbjdlam: do j = n0jk, n1jk
if (j == l .or. j == m) cycle j_aibjbjdlam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbjdlam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbjdlam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbjdlam: do i = i0, n1i
if (i == j .or. i == l .or. i == m) cycle i_aibjbjdlam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjdlam(i, d, l, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbjdlam
end do a_aibjbjdlam
end do j_aibjbjdlam
end do b_aibjbjdlam
end do l_aibjbjdlam
end do d_aibjbjdlam
end do m_aibjbjdlam
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k, l
! Equalities: e == a, c == b, m == i
! No equalities independent of the above can hold.
!
d_aibjbkdlai: do d = n0d, n1d
l_aibjbkdlai: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibjbkdlai: do k = n0k, n1k
if (k == l) cycle k_aibjbkdlai
b_aibjbkdlai: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdlai
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdlai: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjbkdlai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdlai: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdlai
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjbkdlai: do i = i0, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkdlai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkdlai(j, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbkdlai
end do a_aibjbkdlai
end do j_aibjbkdlai
end do b_aibjbkdlai
end do k_aibjbkdlai
end do l_aibjbkdlai
end do d_aibjbkdlai
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k, l
! Equalities: e == a, c == b, m == j
! No equalities independent of the above can hold.
!
d_aibjbkdlaj: do d = n0d, n1d
l_aibjbkdlaj: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
k_aibjbkdlaj: do k = n0k, n1k
if (k == l) cycle k_aibjbkdlaj
b_aibjbkdlaj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdlaj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjbkdlaj: do j = n0jm, j1
if (j == k .or. j == l) cycle j_aibjbkdlaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdlaj: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdlaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbkdlaj: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjbkdlaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkdlaj(i, k, d, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbkdlaj
end do a_aibjbkdlaj
end do j_aibjbkdlaj
end do b_aibjbkdlaj
end do k_aibjbkdlaj
end do l_aibjbkdlaj
end do d_aibjbkdlaj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k, m
! Equalities: e == a, c == b, l == i
! No equalities independent of the above can hold.
!
m_aibjbkdiam: do m = n0m, n1m
d_aibjbkdiam: do d = n0d, n1d
k_aibjbkdiam: do k = n0k, n1k
if (k == m) cycle k_aibjbkdiam
b_aibjbkdiam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdiam
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdiam: do j = n0j, n1j
if (j == k .or. j == m) cycle j_aibjbkdiam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdiam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdiam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjbkdiam: do i = i0, n1il
if (i == j .or. i == k .or. i == m) cycle i_aibjbkdiam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkdiam(j, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbkdiam
end do a_aibjbkdiam
end do j_aibjbkdiam
end do b_aibjbkdiam
end do k_aibjbkdiam
end do d_aibjbkdiam
end do m_aibjbkdiam
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k, m
! Equalities: e == a, c == b, l == j
! No equalities independent of the above can hold.
!
m_aibjbkdjam: do m = n0m, n1m
d_aibjbkdjam: do d = n0d, n1d
k_aibjbkdjam: do k = n0k, n1k
if (k == m) cycle k_aibjbkdjam
b_aibjbkdjam: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdjam
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjbkdjam: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjbkdjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(d - 1, n1ae)
a_aibjbkdjam: do a = a0, a1
if (a == b .or. a == d) cycle a_aibjbkdjam
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbkdjam: do i = i0, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjbkdjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkdjam(i, k, d, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbkdjam
end do a_aibjbkdjam
end do j_aibjbkdjam
end do b_aibjbkdjam
end do k_aibjbkdjam
end do d_aibjbkdjam
end do m_aibjbkdjam
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, d, e
! Free occupied indices: i, j, l
! Equalities: c == b, k == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbidlej: do e = n0e, n1e
d0 = max(e + 1, n0d)
d_aibjbidlej: do d = d0, n1d
if (d == e) cycle d_aibjbidlej
l_aibjbidlej: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjbidlej: do b = n0bc, n1bc
if (b == d .or. b == e) cycle b_aibjbidlej
j1 = min(l - 1, n1jm)
j_aibjbidlej: do j = n0jm, j1
if (j == l) cycle j_aibjbidlej
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidlej: do a = a0, n1a
if (a == b .or. a == d .or. a == e) cycle a_aibjbidlej
i0 = max(j + 1, n0ik)
i_aibjbidlej: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbidlej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (l - 1) * (l - 2)) / 2 + (e - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbidlej(a, d, l, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part1', ibra, iket, jac_ibra_iket
end do i_aibjbidlej
end do a_aibjbidlej
end do j_aibjbidlej
end do b_aibjbidlej
end do l_aibjbidlej
end do d_aibjbidlej
end do e_aibjbidlej
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
end subroutine ccjac_23_tripletp_dav_part1
end module ccjac_block_23_tripletp_dav_part1
