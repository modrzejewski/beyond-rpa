module ccjac_block_32_tripletp_dav_part4
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
subroutine ccjac_32_tripletp_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: i, j, k
integer :: ai, bj, ck, dl, em
integer :: a0, a1, b0, c0, c1, i0, i1
integer :: n0ab, n0abd, n0abe, n0ace, n0ae
integer :: n0bd, n0cd, n0ce, n0ijl, n0ikm
integer :: n0jl, n0km
integer :: n1ab, n1abd, n1abe, n1ace, n1ae
integer :: n1bd, n1cd, n1ce, n1ijl, n1ikm
integer :: n1jl, n1km
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
n0ace = max(n0a, n0c, n0e)
n0ae = max(n0a, n0e)
n0bd = max(n0b, n0d)
n0cd = max(n0c, n0d)
n0ce = max(n0c, n0e)
n0ijl = max(n0i, n0j, n0l)
n0ikm = max(n0i, n0k, n0m)
n0jl = max(n0j, n0l)
n0km = max(n0k, n0m)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1abe = min(n1a, n1b, n1e)
n1ace = min(n1a, n1c, n1e)
n1ae = min(n1a, n1e)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ce = min(n1c, n1e)
n1ijl = min(n1i, n1j, n1l)
n1ikm = min(n1i, n1k, n1m)
n1jl = min(n1j, n1l)
n1km = min(n1k, n1m)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == a, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidjai: do d = n0d, n1d
c_aiajcidjai: do c = n0c, n1c
if (c == d) cycle c_aiajcidjai
j_aiajcidjai: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0abe)
a1 = min(d - 1, n1abe)
a_aiajcidjai: do a = a0, a1
if (a == c .or. a == d) cycle a_aiajcidjai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajcidjai: do i = n0ikm, i1
if (i == j) cycle i_aiajcidjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = v6_eom_cc3_32_tripletp_trans_aiajcidjai(a, i, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcidjai
end do a_aiajcidjai
end do j_aiajcidjai
end do c_aiajcidjai
end do d_aiajcidjai
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, k
! Equalities: b == a, d == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
e_aiaickciek: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiaickciek: do c = c0, n1cd
if (c == e) cycle c_aiaickciek
k_aiaickciek: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (e - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickciek: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiaickciek
i0 = max(k + 1, n0ijl)
i_aiaickciek: do i = i0, n1ijl
if (i == k) cycle i_aiaickciek
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (e - nvirt0) * (i - 1) + k
jac_ibra_iket = v5_eom_cc3_32_tripletp_trans_aiaickciek(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickciek
end do a_aiaickciek
end do k_aiaickciek
end do c_aiaickciek
end do e_aiaickciek
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, c, e
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
e_aiajcicjei: do e = n0e, n1e
c0 = max(e + 1, n0cd)
c_aiajcicjei: do c = c0, n1cd
if (c == e) cycle c_aiajcicjei
j_aiajcicjei: do j = n0jl, n1jl
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcicjei: do a = a0, n1ab
if (a == c .or. a == e) cycle a_aiajcicjei
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajcicjei: do i = n0ikm, i1
if (i == j) cycle i_aiajcicjei
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (e - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (e - nvirt0) * (j - 1) + i
jac_ibra_iket = v6_eom_cc3_32_tripletp_trans_aiajcicjei(a, i, e)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcicjei
end do a_aiajcicjei
end do j_aiajcicjei
end do c_aiajcicjei
end do e_aiajcicjei
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
d_aiaickdick: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiaickdick: do c = n0ce, c1
if (c == d) cycle c_aiaickdick
k_aiaickdick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiaickdick: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiaickdick
i0 = max(k + 1, n0ijl)
i_aiaickdick: do i = i0, n1ijl
if (i == k) cycle i_aiaickdick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
jac_ibra_iket = v5_eom_cc3_32_tripletp_trans_aiaickdick(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickdick
end do a_aiaickdick
end do k_aiaickdick
end do c_aiaickdick
end do d_aiaickdick
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidjci: do d = n0d, n1d
c1 = min(d - 1, n1ce)
c_aiajcidjci: do c = n0ce, c1
if (c == d) cycle c_aiajcidjci
j_aiajcidjci: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcidjci: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajcidjci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajcidjci: do i = n0ikm, i1
if (i == j) cycle i_aiajcidjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (d - nvirt0) * (d - 1 - nvirt0)) / 4 + &
((d - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = v6_eom_cc3_32_tripletp_trans_aiajcidjci(a, i, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajcidjci
end do a_aiajcidjci
end do j_aiajcidjci
end do c_aiajcidjci
end do d_aiajcidjci
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, k
! Equalities: e == a, d == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickciak: do c = n0cd, n1cd
k_aibickciak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibickciak: do b = b0, n1b
if (b == c) cycle b_aibickciak
a1 = min(c - 1, n1ae)
a_aibickciak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickciak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibickciak: do i = i0, n1ijl
if (i == k) cycle i_aibickciak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickciak(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickciak
end do a_aibickciak
end do b_aibickciak
end do k_aibickciak
end do c_aibickciak
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c, b
! Free occupied indices: i, j
! Equalities: e == a, d == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicjai: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcicjai: do b = b0, n1b
if (b == c) cycle b_aibjcicjai
j_aibjcicjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1ae)
a_aibjcicjai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcicjai
i1 = min(j - 1, n1ikm)
i_aibjcicjai: do i = n0ikm, i1
if (i == j) cycle i_aibjcicjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjcicjai(a, i, b)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcicjai
end do a_aibjcicjai
end do j_aibjcicjai
end do b_aibjcicjai
end do c_aibjcicjai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: e == a, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickbiak: do c = n0c, n1c
k_aibickbiak: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbiak: do b = b0, n1bd
if (b == c) cycle b_aibickbiak
a1 = min(b - 1, n1ae)
a_aibickbiak: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibickbiak
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibickbiak: do i = i0, n1ijl
if (i == k) cycle i_aibickbiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickbiak(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbiak
end do a_aibickbiak
end do b_aibickbiak
end do k_aibickbiak
end do c_aibickbiak
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: e == a, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjai: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibjai: do b = b0, n1bd
if (b == c) cycle b_aibjcibjai
j_aibjcibjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ae)
a_aibjcibjai: do a = n0ae, a1
if (a == b .or. a == c) cycle a_aibjcibjai
i1 = min(j - 1, n1ikm)
i_aibjcibjai: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjcibjai(a, i, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjai
end do a_aibjcibjai
end do j_aibjcibjai
end do b_aibjcibjai
end do c_aibjcibjai
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, k
! Equalities: d == b, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aibickbick: do c = n0ce, n1ce
k_aibickbick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibickbick: do b = b0, n1bd
if (b == c) cycle b_aibickbick
a_aibickbick: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibickbick
i0 = max(k + 1, n0ijl)
i_aibickbick: do i = i0, n1ijl
if (i == k) cycle i_aibickbick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
jac_ibra_iket = v3_eom_cc3_32_tripletp_trans_aibickbick(a, i, b, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibickbick
end do a_aibickbick
end do b_aibickbick
end do k_aibickbick
end do c_aibickbick
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, c, a
! Free occupied indices: i, j
! Equalities: d == b, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibjci: do c = n0ce, n1ce
b0 = max(c + 1, n0bd)
b_aibjcibjci: do b = b0, n1bd
if (b == c) cycle b_aibjcibjci
j_aibjcibjci: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a_aibjcibjci: do a = n0a, n1a
if (a == b .or. a == c) cycle a_aibjcibjci
i1 = min(j - 1, n1ikm)
i_aibjcibjci: do i = n0ikm, i1
if (i == j) cycle i_aibjcibjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = v4_eom_cc3_32_tripletp_trans_aibjcibjci(a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjcibjci
end do a_aibjcibjci
end do j_aibjcibjci
end do b_aibjcibjci
end do c_aibjcibjci
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, e == a, d == b, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
k_aibiakbiak: do k = n0km, n1km
b_aibiakbiak: do b = n0bd, n1bd
a1 = min(b - 1, n1ace)
a_aibiakbiak: do a = n0ace, a1
if (a == b) cycle a_aibiakbiak
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
em = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(k + 1, n0ijl)
i_aibiakbiak: do i = i0, n1ijl
if (i == k) cycle i_aibiakbiak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + k
jac_ibra_iket = v7_eom_cc3_32_tripletp_trans_aibiakbiak(a, i, b, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibiakbiak
end do a_aibiakbiak
end do b_aibiakbiak
end do k_aibiakbiak
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, e == a, d == b, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibjai: do b = n0bd, n1bd
j_aibjaibjai: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(b - 1, n1ace)
a_aibjaibjai: do a = n0ace, a1
if (a == b) cycle a_aibjaibjai
i1 = min(j - 1, n1ikm)
i_aibjaibjai: do i = n0ikm, i1
if (i == j) cycle i_aibjaibjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
em = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
jac_ibra_iket = v8_eom_cc3_32_tripletp_trans_aibjaibjai(a, i, b, j)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aibjaibjai
end do a_aibjaibjai
end do j_aibjaibjai
end do b_aibjaibjai
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, e == c, j == i, l == i, m == k
! No equalities independent of the above can hold.
!
c_aiaickaick: do c = n0ce, n1ce
k_aiaickaick: do k = n0km, n1km
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
em = (c - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(c + 1, n0abd)
a_aiaickaick: do a = a0, n1abd
if (a == c) cycle a_aiaickaick
i0 = max(k + 1, n0ijl)
i_aiaickaick: do i = i0, n1ijl
if (i == k) cycle i_aiaickaick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (c - nvirt0) * (i - 1) + k
jac_ibra_iket = v5_eom_cc3_32_tripletp_trans_aiaickaick(a, i, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiaickaick
end do a_aiaickaick
end do k_aiaickaick
end do c_aiaickaick
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, e == c, k == i, m == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciajci: do c = n0ce, n1ce
j_aiajciajci: do j = n0jl, n1jl
a0 = max(c + 1, n0abd)
a_aiajciajci: do a = a0, n1abd
if (a == c) cycle a_aiajciajci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1ikm)
i_aiajciajci: do i = n0ikm, i1
if (i == j) cycle i_aiajciajci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
em = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
(ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + i
jac_ibra_iket = v6_eom_cc3_32_tripletp_trans_aiajciajci(a, i, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket) 
  
  
 
 ! print*, ibra, iket, jac_ibra_iket
end do i_aiajciajci
end do a_aiajciajci
end do j_aiajciajci
end do c_aiajciajci
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
end subroutine ccjac_32_tripletp_dav_part4
end module ccjac_block_32_tripletp_dav_part4
