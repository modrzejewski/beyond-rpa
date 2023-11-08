module ccjac_block_23_tripletp_dav_part5
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
subroutine ccjac_23_tripletp_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0bcd, n0bce, n0bd, n0be, n0cd
integer :: n0ik, n0ikl, n0ikm, n0il, n0im
integer :: n0jk, n0jkl, n0jkm, n0jl, n0jm
integer :: n0kl, n0km
integer :: n1ac, n1acd, n1ace, n1ad, n1ae
integer :: n1bcd, n1bce, n1bd, n1be, n1cd
integer :: n1ik, n1ikl, n1ikm, n1il, n1im
integer :: n1jk, n1jkl, n1jkm, n1jl, n1jm
integer :: n1kl, n1km
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
n0bcd = max(n0b, n0c, n0d)
n0bce = max(n0b, n0c, n0e)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
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
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bcd = min(n1b, n1c, n1d)
n1bce = min(n1b, n1c, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
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
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, d == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjakajbk: do k = n0km, n1km
b_aibjakajbk: do b = n0be, n1be
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjakajbk: do j = j0, n1jl
if (j == k) cycle j_aibjakajbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakajbk: do a = a0, n1acd
if (a == b) cycle a_aibjakajbk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjakajbk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjakajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakajbk(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjakajbk
end do a_aibjakajbk
end do j_aibjakajbk
end do b_aibjakajbk
end do k_aibjakajbk
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, k == i, l == i
! No equalities independent of the above can hold.
!
m_aibjaiaibm: do m = n0m, n1m
b_aibjaiaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjaiaibm: do j = n0j, n1j
if (j == m) cycle j_aibjaiaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiaibm: do a = a0, n1acd
if (a == b) cycle a_aibjaiaibm
i0 = max(j + 1, m + 1, n0ikl)
i_aibjaiaibm: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjaiaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaiaibm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjaiaibm
end do a_aibjaiaibm
end do j_aibjaiaibm
end do b_aibjaiaibm
end do m_aibjaiaibm
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjaialbj: do l = n0l, n1l
b_aibjaialbj: do b = n0be, n1be
j1 = min(l - 1, n1jm)
j_aibjaialbj: do j = n0jm, j1
if (j == l) cycle j_aibjaialbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaialbj: do a = a0, n1acd
if (a == b) cycle a_aibjaialbj
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjaialbj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaialbj(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjaialbj
end do a_aibjaialbj
end do j_aibjaialbj
end do b_aibjaialbj
end do l_aibjaialbj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, k == i, l == j
! No equalities independent of the above can hold.
!
m_aibjaiajbm: do m = n0m, n1m
b_aibjaiajbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jl)
j_aibjaiajbm: do j = j0, n1jl
if (j == m) cycle j_aibjaiajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaiajbm: do a = a0, n1acd
if (a == b) cycle a_aibjaiajbm
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aibjaiajbm: do i = i0, n1ik
if (i == j .or. i == m) cycle i_aibjaiajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaiajbm(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjaiajbm
end do a_aibjaiajbm
end do j_aibjaiajbm
end do b_aibjaiajbm
end do m_aibjaiajbm
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, m
! Equalities: c == a, d == a, e == b, k == j, l == j
! No equalities independent of the above can hold.
!
m_aibjajajbm: do m = n0m, n1m
b_aibjajajbm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j0 = max(m + 1, n0jkl)
j_aibjajajbm: do j = j0, n1jkl
if (j == m) cycle j_aibjajajbm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajajbm: do a = a0, n1acd
if (a == b) cycle a_aibjajajbm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjajajbm: do i = i0, n1i
if (i == j .or. i == m) cycle i_aibjajajbm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajajbm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjajajbm
end do a_aibjajajbm
end do j_aibjajajbm
end do b_aibjajajbm
end do m_aibjajajbm
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, e == b, m == i, k == j
! No equalities independent of the above can hold.
!
l_aibjajalbi: do l = n0l, n1l
b_aibjajalbi: do b = n0be, n1be
j_aibjajalbi: do j = n0jk, n1jk
if (j == l) cycle j_aibjajalbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajalbi: do a = a0, n1acd
if (a == b) cycle a_aibjajalbi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(l - 1, n1im)
i_aibjajalbi: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjajalbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajalbi(a, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjajalbi
end do a_aibjajalbi
end do j_aibjajalbi
end do b_aibjajalbi
end do l_aibjajalbi
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, m
! Equalities: c == a, d == a, e == b, l == i, k == j
! No equalities independent of the above can hold.
!
m_aibjajaibm: do m = n0m, n1m
b_aibjajaibm: do b = n0be, n1be
em = (b - nvirt0) * nocc + (m - nocc0) + 1
j_aibjajaibm: do j = n0jk, n1jk
if (j == m) cycle j_aibjajaibm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajaibm: do a = a0, n1acd
if (a == b) cycle a_aibjajaibm
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, m + 1, n0il)
i_aibjajaibm: do i = i0, n1il
if (i == j .or. i == m) cycle i_aibjajaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajaibm(a, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjajaibm
end do a_aibjajaibm
end do j_aibjajaibm
end do b_aibjajaibm
end do m_aibjajaibm
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, d == a, e == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjakakbi: do k = n0kl, n1kl
b_aibjakakbi: do b = n0be, n1be
j_aibjakakbi: do j = n0j, n1j
if (j == k) cycle j_aibjakakbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakakbi: do a = a0, n1acd
if (a == b) cycle a_aibjakakbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0im)
i1 = min(k - 1, n1im)
i_aibjakakbi: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakakbi(a, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjakakbi
end do a_aibjakakbi
end do j_aibjakakbi
end do b_aibjakakbi
end do k_aibjakakbi
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, d == a, e == b, m == j, l == k
! No equalities independent of the above can hold.
!
k_aibjakakbj: do k = n0kl, n1kl
b_aibjakakbj: do b = n0be, n1be
j1 = min(k - 1, n1jm)
j_aibjakakbj: do j = n0jm, j1
if (j == k) cycle j_aibjakakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakakbj: do a = a0, n1acd
if (a == b) cycle a_aibjakakbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjakakbj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjakakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakakbj(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjakakbj
end do a_aibjakakbj
end do j_aibjakakbj
end do b_aibjakakbj
end do k_aibjakakbj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, e == b, l == i, m == j
! No equalities independent of the above can hold.
!
k_aibjakaibj: do k = n0k, n1k
b_aibjakaibj: do b = n0be, n1be
j_aibjakaibj: do j = n0jm, n1jm
if (j == k) cycle j_aibjakaibj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakaibj: do a = a0, n1acd
if (a == b) cycle a_aibjakaibj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aibjakaibj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakaibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjakaibj(nocc, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjakaibj
end do a_aibjakaibj
end do j_aibjakaibj
end do b_aibjakaibj
end do k_aibjakaibj
!
! Elementary loop  11
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajaiej(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjajaiej
end do a_aibjajaiej
end do j_aibjajaiej
end do b_aibjajaiej
end do e_aibjajaiej
!
! Elementary loop  12
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaiaiej(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjaiaiej
end do a_aibjaiaiej
end do j_aibjaiaiej
end do b_aibjaiaiej
end do e_aibjaiaiej
!
! Elementary loop  13
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajdiaj(a, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjajdiaj
end do a_aibjajdiaj
end do j_aibjajdiaj
end do b_aibjajdiaj
end do d_aibjajdiaj
!
! Elementary loop  14
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaidiaj(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjaidiaj
end do a_aibjaidiaj
end do j_aibjaidiaj
end do b_aibjaidiaj
end do d_aibjaidiaj
!
! Elementary loop  15
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajbiej(a, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjajbiej
end do a_aibjajbiej
end do j_aibjajbiej
end do b_aibjajbiej
end do e_aibjajbiej
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjaibiej: do e = n0e, n1e
b0 = max(e + 1, n0bd)
b_aibjaibiej: do b = b0, n1bd
if (b == e) cycle b_aibjaibiej
j_aibjaibiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibiej: do a = a0, n1ac
if (a == b .or. a == e) cycle a_aibjaibiej
i0 = max(j + 1, n0ikl)
i_aibjaibiej: do i = i0, n1ikl
if (i == j) cycle i_aibjaibiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaibiej(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjaibiej
end do a_aibjaibiej
end do j_aibjaibiej
end do b_aibjaibiej
end do e_aibjaibiej
!
! Elementary loop  17
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjajdibj(a, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjajdibj
end do a_aibjajdibj
end do j_aibjajdibj
end do b_aibjajdibj
end do d_aibjajdibj
!
! Elementary loop  18
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjaidibj(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjaidibj
end do a_aibjaidibj
end do j_aibjaidibj
end do b_aibjaidibj
end do d_aibjaidibj
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
e_aibjbjbiej: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbjbiej: do b = b0, n1bcd
if (b == e) cycle b_aibjbjbiej
j_aibjbjbiej: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjbiej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbjbiej
i0 = max(j + 1, n0il)
i_aibjbjbiej: do i = i0, n1il
if (i == j) cycle i_aibjbjbiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjbiej(a, b, j, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjbjbiej
end do a_aibjbjbiej
end do j_aibjbjbiej
end do b_aibjbjbiej
end do e_aibjbjbiej
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a, e
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
e_aibjbibiej: do e = n0e, n1e
b0 = max(e + 1, n0bcd)
b_aibjbibiej: do b = b0, n1bcd
if (b == e) cycle b_aibjbibiej
j_aibjbibiej: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (e - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibiej: do a = a0, n1a
if (a == b .or. a == e) cycle a_aibjbibiej
i0 = max(j + 1, n0ikl)
i_aibjbibiej: do i = i0, n1ikl
if (i == j) cycle i_aibjbibiej
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbibiej(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjbibiej
end do a_aibjbibiej
end do j_aibjbibiej
end do b_aibjbibiej
end do e_aibjbibiej
!
! Elementary loop  21
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjciaj(b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjcjciaj
end do a_aibjcjciaj
end do j_aibjcjciaj
end do b_aibjcjciaj
end do c_aibjcjciaj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, k == i, l == i, m == j
! No equalities independent of the above can hold.
!
c_aibjciciaj: do c = n0cd, n1cd
b_aibjciciaj: do b = n0b, n1b
if (b == c) cycle b_aibjciciaj
j_aibjciciaj: do j = n0jm, n1jm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ae)
a1 = min(c - 1, n1ae)
a_aibjciciaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciciaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ikl)
i_aibjciciaj: do i = i0, n1ikl
if (i == j) cycle i_aibjciciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjciciaj(i, b, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjciciaj
end do a_aibjciciaj
end do j_aibjciciaj
end do b_aibjciciaj
end do c_aibjciciaj
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: e == b, d == c, l == i, k == j, m == j
! No equalities independent of the above can hold.
!
c_aibjcjcibj: do c = n0cd, n1cd
b1 = min(c - 1, n1be)
b_aibjcjcibj: do b = n0be, b1
if (b == c) cycle b_aibjcjcibj
j_aibjcjcibj: do j = n0jkm, n1jkm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
em = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcibj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcibj
i0 = max(j + 1, n0il)
i_aibjcjcibj: do i = i0, n1il
if (i == j) cycle i_aibjcjcibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcjcibj(a, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjcjcibj
end do a_aibjcjcibj
end do j_aibjcjcibj
end do b_aibjcjcibj
end do c_aibjcjcibj
!
! Elementary loop  24
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjcicibj(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjcicibj
end do a_aibjcicibj
end do j_aibjcicibj
end do b_aibjcicibj
end do c_aibjcicibj
!
! Elementary loop  25
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
i0 = max(j + 1, n0ikm)
i1 = min(l - 1, n1ikm)
i_aibjbialbi: do i = i0, i1
if (i == j .or. i == l) cycle i_aibjbialbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + i
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbialbi(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjbialbi
end do a_aibjbialbi
end do j_aibjbialbi
end do b_aibjbialbi
end do l_aibjbialbi
!
! Elementary loop  26
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
i0 = max(j + 1, n0i)
i_aibjbjalbj: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjbjalbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbjalbj(i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjbjalbj
end do a_aibjbjalbj
end do j_aibjbjalbj
end do b_aibjbjalbj
end do l_aibjbjalbj
!
! Elementary loop  27
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
i0 = max(j + 1, k + 1, n0il)
i_aibjbkaibk: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjbkaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkaibk(b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjbkaibk
end do a_aibjbkaibk
end do j_aibjbkaibk
end do b_aibjbkaibk
end do k_aibjbkaibk
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: d == a, c == b, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjbkajbk: do k = n0km, n1km
b_aibjbkajbk: do b = n0bce, n1bce
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkajbk: do j = j0, n1jl
if (j == k) cycle j_aibjbkajbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkajbk: do a = a0, n1ad
if (a == b) cycle a_aibjbkajbk
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aibjbkajbk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjbkajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + k
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbkajbk(i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjbkajbk
end do a_aibjbkajbk
end do j_aibjbkajbk
end do b_aibjbkajbk
end do k_aibjbkajbk
!
! Elementary loop  29
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
i0 = max(j + 1, m + 1, n0ikl)
i_aibjbiaibm: do i = i0, n1ikl
if (i == j .or. i == m) cycle i_aibjbiaibm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + m
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbiaibm(i, b, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjbiaibm
end do a_aibjbiaibm
end do j_aibjbiaibm
end do b_aibjbiaibm
end do m_aibjbiaibm
!
! Elementary loop  30
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
i0 = max(j + 1, n0ik)
i_aibjbialbj: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbialbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(ck - 1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (b - nvirt0) * (l - 1) + j
jac_ibra_iket = eom_cc3_23_tripletp_trans_aibjbialbj(i, b, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
 

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket

 ! if(abs(jac_ibra_iket).gt.1.d-10)  ! print*, 'part5', ibra, iket, jac_ibra_iket
end do i_aibjbialbj
end do a_aibjbialbj
end do j_aibjbialbj
end do b_aibjbialbj
end do l_aibjbialbj
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
end subroutine ccjac_23_tripletp_dav_part5
end module ccjac_block_23_tripletp_dav_part5
