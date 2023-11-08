module ccjac_block_32_tripletp_dav_part3
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
subroutine ccjac_32_tripletp_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a0, a1, b0, b1, i0, i1, j0, j1, k0, k1
integer :: n0abd, n0abe, n0ac, n0ace, n0ad
integer :: n0bd, n0be, n0ce, n0ij, n0ijl
integer :: n0ijm, n0ik, n0ikl, n0ikm, n0il
integer :: n0im, n0jl, n0jm, n0kl, n0km
integer :: n1abd, n1abe, n1ac, n1ace, n1ad
integer :: n1bd, n1be, n1ce, n1ij, n1ijl
integer :: n1ijm, n1ik, n1ikl, n1ikm, n1il
integer :: n1im, n1jl, n1jm, n1kl, n1km
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
n0abe = max(n0a, n0b, n0e)
n0ac = max(n0a, n0c)
n0ace = max(n0a, n0c, n0e)
n0ad = max(n0a, n0d)
n0bd = max(n0b, n0d)
n0be = max(n0b, n0e)
n0ce = max(n0c, n0e)
n0ij = max(n0i, n0j)
n0ijl = max(n0i, n0j, n0l)
n0ijm = max(n0i, n0j, n0m)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0ikm = max(n0i, n0k, n0m)
n0il = max(n0i, n0l)
n0im = max(n0i, n0m)
n0jl = max(n0j, n0l)
n0jm = max(n0j, n0m)
n0kl = max(n0k, n0l)
n0km = max(n0k, n0m)
n1abd = min(n1a, n1b, n1d)
n1abe = min(n1a, n1b, n1e)
n1ac = min(n1a, n1c)
n1ace = min(n1a, n1c, n1e)
n1ad = min(n1a, n1d)
n1bd = min(n1b, n1d)
n1be = min(n1b, n1e)
n1ce = min(n1c, n1e)
n1ij = min(n1i, n1j)
n1ijl = min(n1i, n1j, n1l)
n1ijm = min(n1i, n1j, n1m)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1ikm = min(n1i, n1k, n1m)
n1il = min(n1i, n1l)
n1im = min(n1i, n1m)
n1jl = min(n1j, n1l)
n1jm = min(n1j, n1m)
n1kl = min(n1k, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, e == a, d == b, k == i, m == j
! No equalities independent of the above can hold.
!
l_aibjaiblaj: do l = n0l, n1l
b_aibjaiblaj: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j1 = min(l - 1, n1jm)
j_aibjaiblaj: do j = n0jm, j1
if (j == l) cycle j_aibjaiblaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaiblaj: do a = n0ace, a1
if (a == b) cycle a_aibjaiblaj
em = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjaiblaj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjaiblaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (l - 1) * (l - 2)) / 2 + (a - nvirt0) * (l - 1) + j
jac_ibra_iket = v8_eom_cc3_32_tripletp_trans_aibjaiblaj(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaiblaj
end do a_aibjaiblaj
end do j_aibjaiblaj
end do b_aibjaiblaj
end do l_aibjaiblaj
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, e == a, d == b, m == i, l == j
! No equalities independent of the above can hold.
!
k_aibjakbjai: do k = n0k, n1k
b_aibjakbjai: do b = n0bd, n1bd
j0 = max(k + 1, n0jl)
j_aibjakbjai: do j = j0, n1jl
if (j == k) cycle j_aibjakbjai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbjai: do a = n0ace, a1
if (a == b) cycle a_aibjakbjai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1im)
i_aibjakbjai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakbjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakbjai(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjai
end do a_aibjakbjai
end do j_aibjakbjai
end do b_aibjakbjai
end do k_aibjakbjai
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, j
! Equalities: c == a, e == a, d == b, m == i, l == k
! No equalities independent of the above can hold.
!
k_aibjakbkai: do k = n0kl, n1kl
b_aibjakbkai: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjakbkai: do j = j0, n1j
if (j == k) cycle j_aibjakbkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbkai: do a = n0ace, a1
if (a == b) cycle a_aibjakbkai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aibjakbkai: do i = n0im, i1
if (i == j .or. i == k) cycle i_aibjakbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakbkai(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbkai
end do a_aibjakbkai
end do j_aibjakbkai
end do b_aibjakbkai
end do k_aibjakbkai
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, k, i
! Equalities: c == a, e == a, d == b, l == j, m == k
! No equalities independent of the above can hold.
!
k_aibjakbjak: do k = n0km, n1km
b_aibjakbjak: do b = n0bd, n1bd
j0 = max(k + 1, n0jl)
j_aibjakbjak: do j = j0, n1jl
if (j == k) cycle j_aibjakbjak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjakbjak: do a = n0ace, a1
if (a == b) cycle a_aibjakbjak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbjak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbjak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + k
jac_ibra_iket = v2_eom_cc3_32_tripletp_trans_aibjakbjak(a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjakbjak
end do a_aibjakbjak
end do j_aibjakbjak
end do b_aibjakbjak
end do k_aibjakbjak
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, e == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aibiakdiak: do d = n0d, n1d
k_aibiakdiak: do k = n0km, n1km
b_aibiakdiak: do b = n0b, n1b
if (b == d) cycle b_aibiakdiak
a1 = min(b - 1, d - 1, n1ace)
a_aibiakdiak: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibiakdiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakdiak: do i = i0, n1ijl
if (i == k) cycle i_aibiakdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = v7_eom_cc3_32_tripletp_trans_aibiakdiak(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakdiak
end do a_aibiakdiak
end do b_aibiakdiak
end do k_aibiakdiak
end do d_aibiakdiak
!
! Elementary loop  6
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
a1 = min(b - 1, d - 1, n1ace)
a_aibjaidjai: do a = n0ace, a1
if (a == b .or. a == d) cycle a_aibjaidjai
i1 = min(j - 1, n1ikm)
i_aibjaidjai: do i = n0ikm, i1
if (i == j) cycle i_aibjaidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = v8_eom_cc3_32_tripletp_trans_aibjaidjai(a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaidjai
end do a_aibjaidjai
end do j_aibjaidjai
end do b_aibjaidjai
end do d_aibjaidjai
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, e
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aibiakbiek: do e = n0e, n1e
k_aibiakbiek: do k = n0km, n1km
em = (e - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(e + 1, n0bd)
b_aibiakbiek: do b = b0, n1bd
if (b == e) cycle b_aibiakbiek
a1 = min(b - 1, n1ac)
a_aibiakbiek: do a = n0ac, a1
if (a == b .or. a == e) cycle a_aibiakbiek
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakbiek: do i = i0, n1ijl
if (i == k) cycle i_aibiakbiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = v7_eom_cc3_32_tripletp_trans_aibiakbiek(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbiek
end do a_aibiakbiek
end do b_aibiakbiek
end do k_aibiakbiek
end do e_aibiakbiek
!
! Elementary loop  8
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
a1 = min(b - 1, n1ac)
a_aibjaibjei: do a = n0ac, a1
if (a == b .or. a == e) cycle a_aibjaibjei
i1 = min(j - 1, n1ikm)
i_aibjaibjei: do i = n0ikm, i1
if (i == j) cycle i_aibjaibjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = v8_eom_cc3_32_tripletp_trans_aibjaibjei(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibjei
end do a_aibjaibjei
end do j_aibjaibjei
end do b_aibjaibjei
end do e_aibjaibjei
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
a1 = min(b - 1, n1ac)
a_aibiakdibk: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibiakdibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakdibk: do i = i0, n1ijl
if (i == k) cycle i_aibiakdibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = v7_eom_cc3_32_tripletp_trans_aibiakdibk(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakdibk
end do a_aibiakdibk
end do b_aibiakdibk
end do k_aibiakdibk
end do d_aibiakdibk
!
! Elementary loop  10
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
a1 = min(b - 1, n1ac)
a_aibjaidjbi: do a = n0ac, a1
if (a == b .or. a == d) cycle a_aibjaidjbi
i1 = min(j - 1, n1ikm)
i_aibjaidjbi: do i = n0ikm, i1
if (i == j) cycle i_aibjaidjbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = v8_eom_cc3_32_tripletp_trans_aibjaidjbi(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaidjbi
end do a_aibjaidjbi
end do j_aibjaidjbi
end do b_aibjaidjbi
end do d_aibjaidjbi
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, m
! Equalities: b == a, d == a, e == c, j == i, l == i
! No equalities independent of the above can hold.
!
m_aiaickaicm: do m = n0m, n1m
c_aiaickaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k_aiaickaicm: do k = n0k, n1k
if (k == m) cycle k_aiaickaicm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickaicm: do a = a0, n1abd
if (a == c) cycle a_aiaickaicm
i0 = max(k + 1, m + 1, n0ijl)
i_aiaickaicm: do i = i0, n1ijl
if (i == k .or. i == m) cycle i_aiaickaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + m
jac_ibra_iket = v5_eom_cc3_32_tripletp_trans_aiaickaicm(a, i, k, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaicm
end do a_aiaickaicm
end do k_aiaickaicm
end do c_aiaickaicm
end do m_aiaickaicm
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, e == c, j == i, m == i
! No equalities independent of the above can hold.
!
l_aiaickalci: do l = n0l, n1l
c_aiaickalci: do c = n0ce, n1ce
k_aiaickalci: do k = n0k, n1k
if (k == l) cycle k_aiaickalci
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickalci: do a = a0, n1abd
if (a == c) cycle a_aiaickalci
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ijm)
i1 = min(l - 1, n1ijm)
i_aiaickalci: do i = i0, i1
if (i == k .or. i == l) cycle i_aiaickalci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + i
jac_ibra_iket = v5_eom_cc3_32_tripletp_trans_aiaickalci(a, i, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickalci
end do a_aiaickalci
end do k_aiaickalci
end do c_aiaickalci
end do l_aiaickalci
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, m
! Equalities: b == a, d == a, e == c, j == i, l == k
! No equalities independent of the above can hold.
!
m_aiaickakcm: do m = n0m, n1m
c_aiaickakcm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
k0 = max(m + 1, n0kl)
k_aiaickakcm: do k = k0, n1kl
if (k == m) cycle k_aiaickakcm
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickakcm: do a = a0, n1abd
if (a == c) cycle a_aiaickakcm
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaickakcm: do i = i0, n1ij
if (i == k .or. i == m) cycle i_aiaickakcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (c - nvirt0) * (k - 1) + m
jac_ibra_iket = v5_eom_cc3_32_tripletp_trans_aiaickakcm(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickakcm
end do a_aiaickakcm
end do k_aiaickakcm
end do c_aiaickakcm
end do m_aiaickakcm
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, e == c, j == i, m == k
! No equalities independent of the above can hold.
!
l_aiaickalck: do l = n0l, n1l
c_aiaickalck: do c = n0ce, n1ce
k1 = min(l - 1, n1km)
k_aiaickalck: do k = n0km, k1
if (k == l) cycle k_aiaickalck
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickalck: do a = a0, n1abd
if (a == c) cycle a_aiaickalck
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aiaickalck: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aiaickalck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + k
jac_ibra_iket = v5_eom_cc3_32_tripletp_trans_aiaickalck(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickalck
end do a_aiaickalck
end do k_aiaickalck
end do c_aiaickalck
end do l_aiaickalck
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, m
! Equalities: b == a, d == a, e == c, k == i, l == i
! No equalities independent of the above can hold.
!
m_aiajciaicm: do m = n0m, n1m
c_aiajciaicm: do c = n0ce, n1ce
em = (c - nvirt0) * nocc + (m - nocc0) + 1
j_aiajciaicm: do j = n0j, n1j
if (j == m) cycle j_aiajciaicm
a0 = max(c + 1, n0abd)
a_aiajciaicm: do a = a0, n1abd
if (a == c) cycle a_aiajciaicm
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(m + 1, n0ikl)
i1 = min(j - 1, n1ikl)
i_aiajciaicm: do i = i0, i1
if (i == j .or. i == m) cycle i_aiajciaicm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + m
jac_ibra_iket = v6_eom_cc3_32_tripletp_trans_aiajciaicm(a, i, j, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciaicm
end do a_aiajciaicm
end do j_aiajciaicm
end do c_aiajciaicm
end do m_aiajciaicm
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, l == i, m == j
! No equalities independent of the above can hold.
!
c_aiajckaicj: do c = n0ce, n1ce
k_aiajckaicj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jm)
j_aiajckaicj: do j = j0, n1jm
if (j == k) cycle j_aiajckaicj
em = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiajckaicj: do a = a0, n1abd
if (a == c) cycle a_aiajckaicj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckaicj: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + j
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckaicj(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaicj
end do a_aiajckaicj
end do j_aiajckaicj
end do k_aiajckaicj
end do c_aiajckaicj
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == c, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiajckaick: do c = n0ce, n1ce
k_aiajckaick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckaick: do j = j0, n1j
if (j == k) cycle j_aiajckaick
a0 = max(c + 1, n0abd)
a_aiajckaick: do a = a0, n1abd
if (a == c) cycle a_aiajckaick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(k + 1, n0il)
i_aiajckaick: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckaick(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckaick
end do a_aiajckaick
end do j_aiajckaick
end do k_aiajckaick
end do c_aiajckaick
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, e == c, k == i, m == i
! No equalities independent of the above can hold.
!
l_aiajcialci: do l = n0l, n1l
c_aiajcialci: do c = n0ce, n1ce
j_aiajcialci: do j = n0j, n1j
if (j == l) cycle j_aiajcialci
a0 = max(c + 1, n0abd)
a_aiajcialci: do a = a0, n1abd
if (a == c) cycle a_aiajcialci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, l - 1, n1ikm)
i_aiajcialci: do i = n0ikm, i1
if (i == j .or. i == l) cycle i_aiajcialci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + i
jac_ibra_iket = v6_eom_cc3_32_tripletp_trans_aiajcialci(a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcialci
end do a_aiajcialci
end do j_aiajcialci
end do c_aiajcialci
end do l_aiajcialci
!
! Elementary loop  19
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
i1 = min(j - 1, n1ik)
i_aiajciajcm: do i = n0ik, i1
if (i == j .or. i == m) cycle i_aiajciajcm
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + m
jac_ibra_iket = v6_eom_cc3_32_tripletp_trans_aiajciajcm(a, i, m)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciajcm
end do a_aiajciajcm
end do j_aiajciajcm
end do c_aiajciajcm
end do m_aiajciajcm
!
! Elementary loop  20
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
i1 = min(j - 1, n1ik)
i_aiajcialcj: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aiajcialcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (l - 1) * (l - 2)) / 2 + (c - nvirt0) * (l - 1) + j
jac_ibra_iket = v6_eom_cc3_32_tripletp_trans_aiajcialcj(a, i, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcialcj
end do a_aiajcialcj
end do j_aiajcialcj
end do c_aiajcialcj
end do l_aiajcialcj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, e == c, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajckajci: do c = n0ce, n1ce
k_aiajckajci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckajci: do j = j0, n1jl
if (j == k) cycle j_aiajckajci
a0 = max(c + 1, n0abd)
a_aiajckajci: do a = a0, n1abd
if (a == c) cycle a_aiajckajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1im)
i_aiajckajci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aiajckajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckajci(a, i, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajci
end do a_aiajckajci
end do j_aiajckajci
end do k_aiajckajci
end do c_aiajckajci
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, j
! Equalities: b == a, d == a, e == c, m == i, l == k
! No equalities independent of the above can hold.
!
c_aiajckakci: do c = n0ce, n1ce
k_aiajckakci: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aiajckakci: do j = j0, n1j
if (j == k) cycle j_aiajckakci
a0 = max(c + 1, n0abd)
a_aiajckakci: do a = a0, n1abd
if (a == c) cycle a_aiajckakci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1im)
i_aiajckakci: do i = n0im, i1
if (i == j .or. i == k) cycle i_aiajckakci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (c - nvirt0) * (k - 1) + i
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckakci(a, i, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckakci
end do a_aiajckakci
end do j_aiajckakci
end do k_aiajckakci
end do c_aiajckakci
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, k, i
! Equalities: b == a, d == a, e == c, l == j, m == k
! No equalities independent of the above can hold.
!
c_aiajckajck: do c = n0ce, n1ce
k_aiajckajck: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aiajckajck: do j = j0, n1jl
if (j == k) cycle j_aiajckajck
a0 = max(c + 1, n0abd)
a_aiajckajck: do a = a0, n1abd
if (a == c) cycle a_aiajckajck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aiajckajck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aiajckajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
jac_ibra_iket = v1_eom_cc3_32_tripletp_trans_aiajckajck(a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajckajck
end do a_aiajckajck
end do j_aiajckajck
end do k_aiajckajck
end do c_aiajckajck
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiaickaiek: do e = n0e, n1e
c_aiaickaiek: do c = n0c, n1c
if (c == e) cycle c_aiaickaiek
k_aiaickaiek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, e + 1, n0abd)
a_aiaickaiek: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiaickaiek
i0 = max(k + 1, n0ijl)
i_aiaickaiek: do i = i0, n1ijl
if (i == k) cycle i_aiaickaiek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = v5_eom_cc3_32_tripletp_trans_aiaickaiek(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaiek
end do a_aiaickaiek
end do k_aiaickaiek
end do c_aiaickaiek
end do e_aiaickaiek
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajciajei: do e = n0e, n1e
c_aiajciajei: do c = n0c, n1c
if (c == e) cycle c_aiajciajei
j_aiajciajei: do j = n0jl, n1jl
a0 = max(c + 1, e + 1, n0abd)
a_aiajciajei: do a = a0, n1abd
if (a == c .or. a == e) cycle a_aiajciajei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajciajei: do i = n0ikm, i1
if (i == j) cycle i_aiajciajei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = v6_eom_cc3_32_tripletp_trans_aiajciajei(a, i, c, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciajei
end do a_aiajciajei
end do j_aiajciajei
end do c_aiajciajei
end do e_aiajciajei
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: d == a, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickaick: do c = n0ce, n1ce
k_aibickaick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickaick: do b = b0, n1b
if (b == c) cycle b_aibickaick
a0 = max(c + 1, n0ad)
a_aibickaick: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickaick
i0 = max(k + 1, n0ijl)
i_aibickaick: do i = i0, n1ijl
if (i == k) cycle i_aibickaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickaick(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaick
end do a_aibickaick
end do b_aibickaick
end do k_aibickaick
end do c_aibickaick
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: d == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajci: do c = n0ce, n1ce
b0 = max(c + 1, n0b)
b_aibjciajci: do b = b0, n1b
if (b == c) cycle b_aibjciajci
j_aibjciajci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ad)
a_aibjciajci: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjciajci
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aibjciajci: do i = n0ikm, i1
if (i == j) cycle i_aibjciajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjciajci(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciajci
end do a_aibjciajci
end do j_aibjciajci
end do b_aibjciajci
end do c_aibjciajci
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, e == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickaibk: do c = n0c, n1c
k_aibickaibk: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0be)
b_aibickaibk: do b = b0, n1be
if (b == c) cycle b_aibickaibk
em = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibickaibk: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibickaibk
i0 = max(k + 1, n0ijl)
i_aibickaibk: do i = i0, n1ijl
if (i == k) cycle i_aibickaibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickaibk(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickaibk
end do a_aibickaibk
end do b_aibickaibk
end do k_aibickaibk
end do c_aibickaibk
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, e == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciajbi: do c = n0c, n1c
b0 = max(c + 1, n0be)
b_aibjciajbi: do b = b0, n1be
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
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjciajbi(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjciajbi
end do a_aibjciajbi
end do j_aibjciajbi
end do b_aibjciajbi
end do c_aibjciajbi
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, e == a, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiaickdiak: do d = n0d, n1d
c_aiaickdiak: do c = n0c, n1c
if (c == d) cycle c_aiaickdiak
k_aiaickdiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiaickdiak: do a = a0, a1
if (a == c .or. a == d) cycle a_aiaickdiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aiaickdiak: do i = i0, n1ijl
if (i == k) cycle i_aiaickdiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = v5_eom_cc3_32_tripletp_trans_aiaickdiak(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickdiak
end do a_aiaickdiak
end do k_aiaickdiak
end do c_aiaickdiak
end do d_aiaickdiak
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
end subroutine ccjac_32_tripletp_dav_part3
end module ccjac_block_32_tripletp_dav_part3
