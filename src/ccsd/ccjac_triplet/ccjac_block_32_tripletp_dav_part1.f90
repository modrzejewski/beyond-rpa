module ccjac_block_32_tripletp_dav_part1
use v1_eom_cc3_32_tripletp_trans
use v2_eom_cc3_32_tripletp_trans
use v3_eom_cc3_32_tripletp_trans
use v4_eom_cc3_32_tripletp_trans
use v5_eom_cc3_32_tripletp_trans
use v6_eom_cc3_32_tripletp_trans
use v7_eom_cc3_32_tripletp_trans
use v8_eom_cc3_32_tripletp_trans
use v9_eom_cc3_32_tripletp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
use cc_gparams 
implicit none
!
! File generated automatically on 2017-01-20 15:15:39 UTC.
!
contains
subroutine ccjac_32_tripletp_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, c0, c1, i0, i1, j0, j1, k0, k1
integer :: n0ab, n0abd, n0abe, n0ac, n0acd
integer :: n0ace, n0ad, n0ae, n0bd, n0be
integer :: n0cd, n0ce, n0ijl, n0ikm, n0jl
integer :: n0jm, n0kl, n0km
integer :: n1ab, n1abd, n1abe, n1ac, n1acd
integer :: n1ace, n1ad, n1ae, n1bd, n1be
integer :: n1cd, n1ce, n1ijl, n1ikm, n1jl
integer :: n1jm, n1kl, n1km
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
n0ab = max(n0a, n0b)
n0abd = max(n0a, n0b, n0d)
n0abe = max(n0a, n0b, n0e)
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0ace = max(n0a, n0c, n0e)
n0ad = max(n0a, n0d)
n0ae = max(n0a, n0e)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0cd = max(n0c, n0d)
n0ce = max(n0c, n0e)
n0ijl = max(n0i, n0j, n0l)
n0ikm = max(n0i, n0k, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1abe = min(n1a, n1b, n1e)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1ae = min(n1a, n1e)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1cd = min(n1c, n1d)
n1ce = min(n1c, n1e)
n1ijl = min(n1i, n1j, n1l)
n1ikm = min(n1i, n1k, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: j, k, i
! Equalities: d == c, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjckcjek: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjckcjek: do c = c0, n1cd
if (c == e) cycle c_aibjckcjek
k_aibjckcjek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjek: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjckcjek
j0 = max(k + 1, n0jl)
j_aibjckcjek: do j = j0, n1jl
if (j == k) cycle j_aibjckcjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckcjek: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckcjek
i_aibjckcjek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckcjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckcjek(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcjek
end do a_aibjckcjek
end do j_aibjckcjek
end do b_aibjckcjek
end do k_aibjckcjek
end do c_aibjckcjek
end do e_aibjckcjek
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, k, m
! Equalities: d == b, e == c, l == j
! No equalities independent of the above can hold.
!
m_aibjckbjcm: do m = n0m, n1m
c_aibjckbjcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aibjckbjcm: do k = n0k, n1k
if (k == m) cycle k_aibjckbjcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjcm: do b = b0, n1bd
if (b == c) cycle b_aibjckbjcm
j0 = max(k + 1, m + 1, n0jl)
j_aibjckbjcm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjckbjcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjcm: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbjcm
i_aibjckbjcm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckbjcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + m
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckbjcm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjcm
end do a_aibjckbjcm
end do j_aibjckbjcm
end do b_aibjckbjcm
end do k_aibjckbjcm
end do c_aibjckbjcm
end do m_aibjckbjcm
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: j, i, k, l
! Equalities: d == b, e == c, m == j
! No equalities independent of the above can hold.
!
l_aibjckblcj: do l = n0l, n1l
c_aibjckblcj: do c = n0ce, n1ce
k_aibjckblcj: do k = n0k, n1k
if (k == l) cycle k_aibjckblcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckblcj: do b = b0, n1bd
if (b == c) cycle b_aibjckblcj
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0jm)
j1 = min(l - 1, n1jm)
j_aibjckblcj: do j = j0, j1
if (j == k .or. j == l) cycle j_aibjckblcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckblcj: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckblcj
i_aibjckblcj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckblcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + j
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckblcj(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckblcj
end do a_aibjckblcj
end do j_aibjckblcj
end do b_aibjckblcj
end do k_aibjckblcj
end do c_aibjckblcj
end do l_aibjckblcj
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: k, i, j, m
! Equalities: d == b, e == c, l == k
! No equalities independent of the above can hold.
!
m_aibjckbkcm: do m = n0m, n1m
c_aibjckbkcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k0 = max(m + 1, n0kl)
k_aibjckbkcm: do k = k0, n1kl
if (k == m) cycle k_aibjckbkcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbkcm: do b = b0, n1bd
if (b == c) cycle b_aibjckbkcm
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckbkcm: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjckbkcm
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbkcm: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckbkcm
i_aibjckbkcm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjckbkcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (c - nvirt0) * (k - 1) + m
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckbkcm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbkcm
end do a_aibjckbkcm
end do j_aibjckbkcm
end do b_aibjckbkcm
end do k_aibjckbkcm
end do c_aibjckbkcm
end do m_aibjckbkcm
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: k, i, j, l
! Equalities: d == b, e == c, m == k
! No equalities independent of the above can hold.
!
l_aibjckblck: do l = n0l, n1l
c_aibjckblck: do c = n0ce, n1ce
k1 = min(l - 1, n1km)
k_aibjckblck: do k = n0km, k1
if (k == l) cycle k_aibjckblck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckblck: do b = b0, n1bd
if (b == c) cycle b_aibjckblck
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjckblck: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjckblck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckblck: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjckblck
i_aibjckblck: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckblck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckblck(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckblck
end do a_aibjckblck
end do j_aibjckblck
end do b_aibjckblck
end do k_aibjckblck
end do c_aibjckblck
end do l_aibjckblck
!
! Elementary loop  6
! --------------------
! Free virtual indices: c, a, b, d
! Free occupied indices: j, k, i
! Equalities: e == c, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjckdjck: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aibjckdjck: do c = n0ce, c1
if (c == d) cycle c_aibjckdjck
k_aibjckdjck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdjck: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdjck
j0 = max(k + 1, n0jl)
j_aibjckdjck: do j = j0, n1jl
if (j == k) cycle j_aibjckdjck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdjck: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjck
i_aibjckdjck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckdjck(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdjck
end do a_aibjckdjck
end do j_aibjckdjck
end do b_aibjckdjck
end do k_aibjckdjck
end do c_aibjckdjck
end do d_aibjckdjck
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, c, e
! Free occupied indices: j, k, i
! Equalities: d == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjckbjek: do e = n0e, n1e
c_aibjckbjek: do c = n0c, n1c
if (c == e) cycle c_aibjckbjek
k_aibjckbjek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, e + 1, n0bd)
b_aibjckbjek: do b = b0, n1bd
if (b == c .or. b == e) cycle b_aibjckbjek
j0 = max(k + 1, n0jl)
j_aibjckbjek: do j = j0, n1jl
if (j == k) cycle j_aibjckbjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckbjek: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjckbjek
i_aibjckbjek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckbjek(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjek
end do a_aibjckbjek
end do j_aibjckbjek
end do b_aibjckbjek
end do k_aibjckbjek
end do c_aibjckbjek
end do e_aibjckbjek
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, c, d
! Free occupied indices: j, k, i
! Equalities: e == b, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjckdjbk: do d = n0d, n1d
c_aibjckdjbk: do c = n0c, n1c
if (c == d) cycle c_aibjckdjbk
k_aibjckdjbk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b1 = min(d - 1, n1be)
b_aibjckdjbk: do b = b0, b1
if (b == c .or. b == d) cycle b_aibjckdjbk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjckdjbk: do j = j0, n1jl
if (j == k) cycle j_aibjckdjbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aibjckdjbk: do a = n0a, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdjbk
i_aibjckdjbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckdjbk(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckdjbk
end do a_aibjckdjbk
end do j_aibjckdjbk
end do b_aibjckdjbk
end do k_aibjckdjbk
end do c_aibjckdjbk
end do d_aibjckdjbk
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, k, i
! Equalities: c == a, d == a, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjakajek: do e = n0e, n1e
k_aibjakajek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b_aibjakajek: do b = n0b, n1b
if (b == e) cycle b_aibjakajek
j0 = max(k + 1, n0jl)
j_aibjakajek: do j = j0, n1jl
if (j == k) cycle j_aibjakajek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(e + 1, n0acd)
a1 = min(b - 1, n1acd)
a_aibjakajek: do a = a0, a1
if (a == b .or. a == e) cycle a_aibjakajek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakajek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + k
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakajek(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakajek
end do a_aibjakajek
end do j_aibjakajek
end do b_aibjakajek
end do k_aibjakajek
end do e_aibjakajek
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, m
! Equalities: c == a, e == a, d == b, l == j
! No equalities independent of the above can hold.
!
m_aibjakbjam: do m = n0m, n1m
k_aibjakbjam: do k = n0k, n1k
if (k == m) cycle k_aibjakbjam
b_aibjakbjam: do b = n0bd, n1bd
j0 = max(k + 1, m + 1, n0jl)
j_aibjakbjam: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aibjakbjam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbjam: do a = n0ace, a1
if (a == b) cycle a_aibjakbjam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjakbjam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakbjam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + m
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakbjam(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjam
end do a_aibjakbjam
end do j_aibjakbjam
end do b_aibjakbjam
end do k_aibjakbjam
end do m_aibjakbjam
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k, l
! Equalities: c == a, e == a, d == b, m == j
! No equalities independent of the above can hold.
!
l_aibjakblaj: do l = n0l, n1l
k_aibjakblaj: do k = n0k, n1k
if (k == l) cycle k_aibjakblaj
b_aibjakblaj: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0jm)
j1 = min(l - 1, n1jm)
j_aibjakblaj: do j = j0, j1
if (j == k .or. j == l) cycle j_aibjakblaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakblaj: do a = n0ace, a1
if (a == b) cycle a_aibjakblaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakblaj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakblaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakblaj(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakblaj
end do a_aibjakblaj
end do j_aibjakblaj
end do b_aibjakblaj
end do k_aibjakblaj
end do l_aibjakblaj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j, m
! Equalities: c == a, e == a, d == b, l == k
! No equalities independent of the above can hold.
!
m_aibjakbkam: do m = n0m, n1m
k0 = max(m + 1, n0kl)
k_aibjakbkam: do k = k0, n1kl
if (k == m) cycle k_aibjakbkam
b_aibjakbkam: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbkam: do j = j0, n1j
if (j == k .or. j == m) cycle j_aibjakbkam
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbkam: do a = n0ace, a1
if (a == b) cycle a_aibjakbkam
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (m - nocc0) + 1
i_aibjakbkam: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aibjakbkam
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + m
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakbkam(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbkam
end do a_aibjakbkam
end do j_aibjakbkam
end do b_aibjakbkam
end do k_aibjakbkam
end do m_aibjakbkam
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j, l
! Equalities: c == a, e == a, d == b, m == k
! No equalities independent of the above can hold.
!
l_aibjakblak: do l = n0l, n1l
k1 = min(l - 1, n1km)
k_aibjakblak: do k = n0km, k1
if (k == l) cycle k_aibjakblak
b_aibjakblak: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakblak: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjakblak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakblak: do a = n0ace, a1
if (a == b) cycle a_aibjakblak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakblak: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakblak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + k
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakblak(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakblak
end do a_aibjakblak
end do j_aibjakblak
end do b_aibjakblak
end do k_aibjakblak
end do l_aibjakblak
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, k, i
! Equalities: c == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
d_aibjakdjak: do d = n0d, n1d
k_aibjakdjak: do k = n0km, n1km
b_aibjakdjak: do b = n0b, n1b
if (b == d) cycle b_aibjakdjak
j0 = max(k + 1, n0jl)
j_aibjakdjak: do j = j0, n1jl
if (j == k) cycle j_aibjakdjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, d - 1, n1ace)
a_aibjakdjak: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibjakdjak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakdjak(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdjak
end do a_aibjakdjak
end do j_aibjakdjak
end do b_aibjakdjak
end do k_aibjakdjak
end do d_aibjakdjak
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: j, k, i
! Equalities: c == a, d == b, l == j, m == k
! No equalities independent of the above can hold.
!
e_aibjakbjek: do e = n0e, n1e
k_aibjakbjek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibjakbjek: do b = b0, n1bd
if (b == e) cycle b_aibjakbjek
j0 = max(k + 1, n0jl)
j_aibjakbjek: do j = j0, n1jl
if (j == k) cycle j_aibjakbjek
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ac)
a_aibjakbjek: do a = n0ac, a1
if (a == b .or. a == e) cycle a_aibjakbjek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + k
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakbjek(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjek
end do a_aibjakbjek
end do j_aibjakbjek
end do b_aibjakbjek
end do k_aibjakbjek
end do e_aibjakbjek
!
! Elementary loop  16
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
a1 = min(b - 1, n1ac)
a_aibjakdjbk: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjakdjbk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdjbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdjbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + k
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakdjbk(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakdjbk
end do a_aibjakdjbk
end do j_aibjakdjbk
end do b_aibjakdjbk
end do k_aibjakdjbk
end do d_aibjakdjbk
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, m
! Equalities: b == a, d == a, e == c, l == j
! No equalities independent of the above can hold.
!
m_aiajckajcm: do m = n0m, n1m
c_aiajckajcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiajckajcm: do k = n0k, n1k
if (k == m) cycle k_aiajckajcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, m + 1, n0jl)
j_aiajckajcm: do j = j0, n1jl
if (j == k .or. j == m) cycle j_aiajckajcm
a0 = max(c + 1, n0abd)
a_aiajckajcm: do a = a0, n1abd
if (a == c) cycle a_aiajckajcm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajcm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajckajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + m
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckajcm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajcm
end do a_aiajckajcm
end do j_aiajckajcm
end do k_aiajckajcm
end do c_aiajckajcm
end do m_aiajckajcm
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k, l
! Equalities: b == a, d == a, e == c, m == j
! No equalities independent of the above can hold.
!
l_aiajckalcj: do l = n0l, n1l
c_aiajckalcj: do c = n0ce, n1ce
k_aiajckalcj: do k = n0k, n1k
if (k == l) cycle k_aiajckalcj
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j1 = min(l - 1, n1jm)
j_aiajckalcj: do j = j0, j1
if (j == k .or. j == l) cycle j_aiajckalcj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckalcj: do a = a0, n1abd
if (a == c) cycle a_aiajckalcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckalcj: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + j
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckalcj(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckalcj
end do a_aiajckalcj
end do j_aiajckalcj
end do k_aiajckalcj
end do c_aiajckalcj
end do l_aiajckalcj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, m
! Equalities: b == a, d == a, e == c, l == k
! No equalities independent of the above can hold.
!
m_aiajckakcm: do m = n0m, n1m
c_aiajckakcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k0 = max(m + 1, n0kl)
k_aiajckakcm: do k = k0, n1kl
if (k == m) cycle k_aiajckakcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckakcm: do j = j0, n1j
if (j == k .or. j == m) cycle j_aiajckakcm
a0 = max(c + 1, n0abd)
a_aiajckakcm: do a = a0, n1abd
if (a == c) cycle a_aiajckakcm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckakcm: do i = n0i, n1i
if (i == j .or. i == k .or. i == m) cycle i_aiajckakcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (c - nvirt0) * (k - 1) + m
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckakcm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckakcm
end do a_aiajckakcm
end do j_aiajckakcm
end do k_aiajckakcm
end do c_aiajckakcm
end do m_aiajckakcm
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j, l
! Equalities: b == a, d == a, e == c, m == k
! No equalities independent of the above can hold.
!
l_aiajckalck: do l = n0l, n1l
c_aiajckalck: do c = n0ce, n1ce
k1 = min(l - 1, n1km)
k_aiajckalck: do k = n0km, k1
if (k == l) cycle k_aiajckalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckalck: do j = j0, n1j
if (j == k .or. j == l) cycle j_aiajckalck
a0 = max(c + 1, n0abd)
a_aiajckalck: do a = a0, n1abd
if (a == c) cycle a_aiajckalck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aiajckalck: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + k
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckalck(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckalck
end do a_aiajckalck
end do j_aiajckalck
end do k_aiajckalck
end do c_aiajckalck
end do l_aiajckalck
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, l == j, m == k
! No equalities independent of the above can hold.
!
e_aiajckajek: do e = n0e, n1e
c_aiajckajek: do c = n0c, n1c
if (c == e) cycle c_aiajckajek
k_aiajckajek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckajek: do j = j0, n1jl
if (j == k) cycle j_aiajckajek
a0 = max(c + 1, e + 1, n0abd)
a_aiajckajek: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajckajek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckajek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + k
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckajek(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajek
end do a_aiajckajek
end do j_aiajckajek
end do k_aiajckajek
end do c_aiajckajek
end do e_aiajckajek
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: d == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajck: do c = n0ce, n1ce
k_aibjckajck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckajck: do b = b0, n1b
if (b == c) cycle b_aibjckajck
j0 = max(k + 1, n0jl)
j_aibjckajck: do j = j0, n1jl
if (j == k) cycle j_aibjckajck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjckajck: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckajck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckajck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckajck(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckajck
end do a_aibjckajck
end do j_aibjckajck
end do b_aibjckajck
end do k_aibjckajck
end do c_aibjckajck
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: d == a, e == b, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckajbk: do c = n0c, n1c
k_aibjckajbk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibjckajbk: do b = b0, n1be
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
i_aibjckajbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckajbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckajbk(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckajbk
end do a_aibjckajbk
end do j_aibjckajbk
end do b_aibjckajbk
end do k_aibjckajbk
end do c_aibjckajbk
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == a, l == j, m == k
! No equalities independent of the above can hold.
!
d_aiajckdjak: do d = n0d, n1d
c_aiajckdjak: do c = n0c, n1c
if (c == d) cycle c_aiajckdjak
k_aiajckdjak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckdjak: do j = j0, n1jl
if (j == k) cycle j_aiajckdjak
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajckdjak: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajckdjak
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aiajckdjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckdjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckdjak(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdjak
end do a_aiajckdjak
end do j_aiajckdjak
end do k_aiajckdjak
end do c_aiajckdjak
end do d_aiajckdjak
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: j, k, i
! Equalities: b == a, d == c, l == j, m == k
! No equalities independent of the above can hold.
!
e_aiajckcjek: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajckcjek: do c = c0, n1cd
if (c == e) cycle c_aiajckcjek
k_aiajckcjek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckcjek: do j = j0, n1jl
if (j == k) cycle j_aiajckcjek
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckcjek: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajckcjek
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckcjek: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckcjek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + k
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckcjek(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckcjek
end do a_aiajckcjek
end do j_aiajckcjek
end do k_aiajckcjek
end do c_aiajckcjek
end do e_aiajckcjek
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, k, i
! Equalities: b == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
d_aiajckdjck: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajckdjck: do c = n0ce, c1
if (c == d) cycle c_aiajckdjck
k_aiajckdjck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckdjck: do j = j0, n1jl
if (j == k) cycle j_aiajckdjck
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckdjck: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdjck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckdjck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckdjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckdjck(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckdjck
end do a_aiajckdjck
end do j_aiajckdjck
end do k_aiajckdjck
end do c_aiajckdjck
end do d_aiajckdjck
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: j, k, i
! Equalities: e == a, d == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckcjak: do c = n0cd, n1cd
k_aibjckcjak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcjak: do b = b0, n1b
if (b == c) cycle b_aibjckcjak
j0 = max(k + 1, n0jl)
j_aibjckcjak: do j = j0, n1jl
if (j == k) cycle j_aibjckcjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjckcjak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckcjak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckcjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckcjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckcjak(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckcjak
end do a_aibjckcjak
end do j_aibjckcjak
end do b_aibjckcjak
end do k_aibjckcjak
end do c_aibjckcjak
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, k, i
! Equalities: e == a, d == b, l == j, m == k
! No equalities independent of the above can hold.
!
c_aibjckbjak: do c = n0c, n1c
k_aibjckbjak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbjak: do b = b0, n1bd
if (b == c) cycle b_aibjckbjak
j0 = max(k + 1, n0jl)
j_aibjckbjak: do j = j0, n1jl
if (j == k) cycle j_aibjckbjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjckbjak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjckbjak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckbjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
jac_ibra_iket = v9_eom_cc3_32_tripletp_trans_aibjckbjak(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjckbjak
end do a_aibjckbjak
end do j_aibjckbjak
end do b_aibjckbjak
end do k_aibjckbjak
end do c_aibjckbjak
!
! Elementary loop  29
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, k
! Equalities: d == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibickciek: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibickciek: do c = c0, n1cd
if (c == e) cycle c_aibickciek
k_aibickciek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickciek: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibickciek
a_aibickciek: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibickciek
i0 = max(k + 1, n0ijl)
i_aibickciek: do i = i0, n1ijl
if (i == k) cycle i_aibickciek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickciek(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickciek
end do a_aibickciek
end do b_aibickciek
end do k_aibickciek
end do c_aibickciek
end do e_aibickciek
!
! Elementary loop  30
! --------------------
! Free virtual indices: c, a, b, e
! Free occupied indices: i, j
! Equalities: d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aibjcicjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aibjcicjei: do c = c0, n1cd
if (c == e) cycle c_aibjcicjei
b0 = max(c + 1, n0b)
b_aibjcicjei: do b = b0, n1b
if (b == c .or. b == e) cycle b_aibjcicjei
j_aibjcicjei: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcicjei: do a = n0a, n1a
if (a == b .or. a == c .or. a == e) cycle a_aibjcicjei
i1 = min(j - 1, n1ikm)
i_aibjcicjei: do i = n0ikm, i1
if (i == j) cycle i_aibjcicjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjcicjei(a, i, b, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcicjei
end do a_aibjcicjei
end do j_aibjcicjei
end do b_aibjcicjei
end do c_aibjcicjei
end do e_aibjcicjei
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
end subroutine ccjac_32_tripletp_dav_part1
end module ccjac_block_32_tripletp_dav_part1
