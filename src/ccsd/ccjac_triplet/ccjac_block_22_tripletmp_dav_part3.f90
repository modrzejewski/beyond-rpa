module ccjac_block_22_tripletmp_dav_part3
use eom_ccsd_22_tripletmp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-06 14:24:58 UTC.
!
contains
subroutine ccjac_22_tripletmp_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
 nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, &
 n1k, n0l, n1l, bra0, ket0) 
procedure(dav_sigma_update_diag) :: sigup_diag
procedure(dav_sigma_update_right_nondiag) :: sigup_nondiag
real(F64), dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
 n0d, n1d
integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
 n0l, n1l
integer, intent(in)                                 :: bra0, ket0
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: nn0a, nn0b, nn0c, nn0i, nn0j, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b0, b1, c0, i0, i1, j0, j1
integer :: n0abc, n0abd, n0bc, n0ijk, n0ijl
integer :: n0ik, n0il, n0jk, n0jl
integer :: n1abc, n1abd, n1bc, n1ijk, n1ijl
integer :: n1ik, n1il, n1jk, n1jl
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket
integer :: braoffset, ketoffset
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
n0abc = max(n0a, n0b, n0c)
n0abd = max(n0a, n0b, n0d)
n0bc = max(n0b, n0c)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1abc = min(n1a, n1b, n1c)
n1abd = min(n1a, n1b, n1d)
n1bc = min(n1b, n1c)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, k
! Equalities: j == i, l == i
! No equalities independent of the above can hold.
!
d_aibickdi_aibckd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibickdi_aibckd: do c = c0, n1c
if (c == d) cycle c_aibickdi_aibckd
k_aibickdi_aibckd: do k = n0k, n1k
b_aibickdi_aibckd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibickdi_aibckd
a0 = max(b + 1, n0a)
a_aibickdi_aibckd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibickdi_aibckd
i1 = min(k - 1, n1ijl)
i_aibickdi_aibckd: do i = n0ijl, i1
if (i == k) cycle i_aibickdi_aibckd
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibickdi_aibckd(t2, nocc, nactive, a, &
 i, b, c, k, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickdi_aibckd
end do a_aibickdi_aibckd
end do b_aibickdi_aibckd
end do k_aibickdi_aibckd
end do c_aibickdi_aibckd
end do d_aibickdi_aibckd
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidj_abjcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidj_abjcd: do c = c0, n1c
if (c == d) cycle c_aibjcidj_abjcd
b_aibjcidj_abjcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidj_abjcd
j_aibjcidj_abjcd: do j = n0jl, n1jl
a0 = max(b + 1, n0a)
a_aibjcidj_abjcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidj_abjcd
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcidj_abjcd(t2, &
 nocc, nactive, a, b, j, c, d)
i0 = max(j + 1, n0ik)
i_aibjcidj_abjcd: do i = i0, n1ik
if (i == j) cycle i_aibjcidj_abjcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidj_abjcd
end do a_aibjcidj_abjcd
end do j_aibjcidj_abjcd
end do b_aibjcidj_abjcd
end do c_aibjcidj_abjcd
end do d_aibjcidj_abjcd
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidj_aibcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidj_aibcd: do c = c0, n1c
if (c == d) cycle c_aibjcidj_aibcd
b_aibjcidj_aibcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidj_aibcd
a0 = max(b + 1, n0a)
a_aibjcidj_aibcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidj_aibcd
i_aibjcidj_aibcd: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcidj_aibcd(t2, &
 nocc, nactive, a, i, b, c, d)
j1 = min(i - 1, n1jl)
j_aibjcidj_aibcd: do j = n0jl, j1
if (j == i) cycle j_aibjcidj_aibcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcidj_aibcd
end do i_aibjcidj_aibcd
end do a_aibjcidj_aibcd
end do b_aibjcidj_aibcd
end do c_aibjcidj_aibcd
end do d_aibjcidj_aibcd
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, l
! Equalities: j == i, k == i
! No equalities independent of the above can hold.
!
d_aibicidl_aibcdl: do d = n0d, n1d
l_aibicidl_aibcdl: do l = n0l, n1l
c0 = max(d + 1, n0c)
c_aibicidl_aibcdl: do c = c0, n1c
if (c == d) cycle c_aibicidl_aibcdl
b_aibicidl_aibcdl: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibicidl_aibcdl
a0 = max(b + 1, n0a)
a_aibicidl_aibcdl: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibicidl_aibcdl
i0 = max(l + 1, n0ijk)
i_aibicidl_aibcdl: do i = i0, n1ijk
if (i == l) cycle i_aibicidl_aibcdl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibicidl_aibcdl(t2, nocc, nactive, a, &
 i, b, c, d, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicidl_aibcdl
end do a_aibicidl_aibcdl
end do b_aibicidl_aibcdl
end do c_aibicidl_aibcdl
end do l_aibicidl_aibcdl
end do d_aibicidl_aibcdl
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdi_abjcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdi_abjcd: do c = c0, n1c
if (c == d) cycle c_aibjcjdi_abjcd
b_aibjcjdi_abjcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdi_abjcd
j_aibjcjdi_abjcd: do j = n0jk, n1jk
a0 = max(b + 1, n0a)
a_aibjcjdi_abjcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdi_abjcd
nn1i = min(j - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjdi_abjcd(t2, &
 nocc, nactive, a, b, j, c, d)
i1 = min(j - 1, n1il)
i_aibjcjdi_abjcd: do i = n0il, i1
if (i == j) cycle i_aibjcjdi_abjcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdi_abjcd
end do a_aibjcjdi_abjcd
end do j_aibjcjdi_abjcd
end do b_aibjcjdi_abjcd
end do c_aibjcjdi_abjcd
end do d_aibjcjdi_abjcd
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdi_aibcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdi_aibcd: do c = c0, n1c
if (c == d) cycle c_aibjcjdi_aibcd
b_aibjcjdi_aibcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdi_aibcd
a0 = max(b + 1, n0a)
a_aibjcjdi_aibcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdi_aibcd
i1 = min(j - 1, n1il)
i_aibjcjdi_aibcd: do i = n0il, i1
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjdi_aibcd(t2, &
 nocc, nactive, a, i, b, c, d)
j_aibjcjdi_aibcd: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjdi_aibcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjdi_aibcd
end do i_aibjcjdi_aibcd
end do a_aibjcjdi_aibcd
end do b_aibjcjdi_aibcd
end do c_aibjcjdi_aibcd
end do d_aibjcjdi_aibcd
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, l == j
! No equalities independent of the above can hold.
!
d_aiajakdj_aikd: do d = n0d, n1d
k_aiajakdj_aikd: do k = n0k, n1k
a0 = max(d + 1, n0abc)
a_aiajakdj_aikd: do a = a0, n1abc
if (a == d) cycle a_aiajakdj_aikd
i_aiajakdj_aikd: do i = n0i, n1i
if (i == k) cycle i_aiajakdj_aikd
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajakdj_aikd(t2, &
 nocc, nactive, a, i, k, d)
j1 = min(i - 1, k - 1, n1jl)
j_aiajakdj_aikd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aiajakdj_aikd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajakdj_aikd
end do i_aiajakdj_aikd
end do a_aiajakdj_aikd
end do k_aiajakdj_aikd
end do d_aiajakdj_aikd
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, l == j
! No equalities independent of the above can hold.
!
d_aiajakdj_aijkd: do d = n0d, n1d
k_aiajakdj_aijkd: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aiajakdj_aijkd: do j = n0jl, j1
if (j == k) cycle j_aiajakdj_aijkd
a0 = max(d + 1, n0abc)
a_aiajakdj_aijkd: do a = a0, n1abc
if (a == d) cycle a_aiajakdj_aijkd
i0 = max(j + 1, n0i)
i_aiajakdj_aijkd: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakdj_aijkd
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajakdj_aijkd(t2, nocc, nactive, a, &
 i, j, k, d)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakdj_aijkd
end do a_aiajakdj_aijkd
end do j_aiajakdj_aijkd
end do k_aiajakdj_aijkd
end do d_aiajakdj_aijkd
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, k == j
! No equalities independent of the above can hold.
!
d_aiajajdl_aidl: do d = n0d, n1d
l_aiajajdl_aidl: do l = n0l, n1l
a0 = max(d + 1, n0abc)
a_aiajajdl_aidl: do a = a0, n1abc
if (a == d) cycle a_aiajajdl_aidl
i_aiajajdl_aidl: do i = n0i, n1i
if (i == l) cycle i_aiajajdl_aidl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajajdl_aidl(t2, &
 nocc, nactive, a, i, d, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aiajajdl_aidl: do j = j0, j1
if (j == i .or. j == l) cycle j_aiajajdl_aidl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajajdl_aidl
end do i_aiajajdl_aidl
end do a_aiajajdl_aidl
end do l_aiajajdl_aidl
end do d_aiajajdl_aidl
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, k == j
! No equalities independent of the above can hold.
!
d_aiajajdl_aijdl: do d = n0d, n1d
l_aiajajdl_aijdl: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aiajajdl_aijdl: do j = j0, n1jk
if (j == l) cycle j_aiajajdl_aijdl
a0 = max(d + 1, n0abc)
a_aiajajdl_aijdl: do a = a0, n1abc
if (a == d) cycle a_aiajajdl_aijdl
i0 = max(j + 1, n0i)
i_aiajajdl_aijdl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajajdl_aijdl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajajdl_aijdl(t2, nocc, nactive, a, &
 i, j, d, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajdl_aijdl
end do a_aiajajdl_aijdl
end do j_aiajajdl_aijdl
end do l_aiajajdl_aijdl
end do d_aiajajdl_aijdl
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, l == i
! No equalities independent of the above can hold.
!
d_aiajakdi_ajkd: do d = n0d, n1d
k_aiajakdi_ajkd: do k = n0k, n1k
j_aiajakdi_ajkd: do j = n0j, n1j
if (j == k) cycle j_aiajakdi_ajkd
a0 = max(d + 1, n0abc)
a_aiajakdi_ajkd: do a = a0, n1abc
if (a == d) cycle a_aiajakdi_ajkd
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajakdi_ajkd(t2, &
 nocc, nactive, a, j, k, d)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajakdi_ajkd: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakdi_ajkd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakdi_ajkd
end do a_aiajakdi_ajkd
end do j_aiajakdi_ajkd
end do k_aiajakdi_ajkd
end do d_aiajakdi_ajkd
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, l == i
! No equalities independent of the above can hold.
!
d_aiajakdi_aijkd: do d = n0d, n1d
k_aiajakdi_aijkd: do k = n0k, n1k
j_aiajakdi_aijkd: do j = n0j, n1j
if (j == k) cycle j_aiajakdi_aijkd
a0 = max(d + 1, n0abc)
a_aiajakdi_aijkd: do a = a0, n1abc
if (a == d) cycle a_aiajakdi_aijkd
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajakdi_aijkd: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajakdi_aijkd
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajakdi_aijkd(t2, nocc, nactive, a, &
 i, j, k, d)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakdi_aijkd
end do a_aiajakdi_aijkd
end do j_aiajakdi_aijkd
end do k_aiajakdi_aijkd
end do d_aiajakdi_aijkd
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, k == i
! No equalities independent of the above can hold.
!
d_aiajaidl_ajdl: do d = n0d, n1d
l_aiajaidl_ajdl: do l = n0l, n1l
j_aiajaidl_ajdl: do j = n0j, n1j
if (j == l) cycle j_aiajaidl_ajdl
a0 = max(d + 1, n0abc)
a_aiajaidl_ajdl: do a = a0, n1abc
if (a == d) cycle a_aiajaidl_ajdl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajaidl_ajdl(t2, &
 nocc, nactive, a, j, d, l)
i0 = max(j + 1, l + 1, n0ik)
i_aiajaidl_ajdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaidl_ajdl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidl_ajdl
end do a_aiajaidl_ajdl
end do j_aiajaidl_ajdl
end do l_aiajaidl_ajdl
end do d_aiajaidl_ajdl
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, k == i
! No equalities independent of the above can hold.
!
d_aiajaidl_aijdl: do d = n0d, n1d
l_aiajaidl_aijdl: do l = n0l, n1l
j_aiajaidl_aijdl: do j = n0j, n1j
if (j == l) cycle j_aiajaidl_aijdl
a0 = max(d + 1, n0abc)
a_aiajaidl_aijdl: do a = a0, n1abc
if (a == d) cycle a_aiajaidl_aijdl
i0 = max(j + 1, l + 1, n0ik)
i_aiajaidl_aijdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaidl_aijdl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajaidl_aijdl(t2, nocc, nactive, a, &
 i, j, d, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidl_aijdl
end do a_aiajaidl_aijdl
end do j_aiajaidl_aijdl
end do l_aiajaidl_aijdl
end do d_aiajaidl_aijdl
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibibkdi_aikd: do d = n0d, n1d
k_aibibkdi_aikd: do k = n0k, n1k
a_aibibkdi_aikd: do a = n0a, n1a
if (a == d) cycle a_aibibkdi_aikd
i1 = min(k - 1, n1ijl)
i_aibibkdi_aikd: do i = n0ijl, i1
if (i == k) cycle i_aibibkdi_aikd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibibkdi_aikd(t2, &
 nocc, nactive, a, i, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibkdi_aikd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibkdi_aikd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibkdi_aikd
end do i_aibibkdi_aikd
end do a_aibibkdi_aikd
end do k_aibibkdi_aikd
end do d_aibibkdi_aikd
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibibkdi_aibkd: do d = n0d, n1d
k_aibibkdi_aibkd: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibibkdi_aibkd: do b = b0, n1bc
if (b == d) cycle b_aibibkdi_aibkd
a0 = max(b + 1, n0a)
a_aibibkdi_aibkd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibkdi_aibkd
i1 = min(k - 1, n1ijl)
i_aibibkdi_aibkd: do i = n0ijl, i1
if (i == k) cycle i_aibibkdi_aibkd
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibibkdi_aibkd(t2, nocc, nactive, a, &
 i, b, k, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdi_aibkd
end do a_aibibkdi_aibkd
end do b_aibibkdi_aibkd
end do k_aibibkdi_aibkd
end do d_aibibkdi_aibkd
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_aid: do d = n0d, n1d
a_aibjbidj_aid: do a = n0a, n1a
if (a == d) cycle a_aibjbidj_aid
i_aibjbidj_aid: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1j .ge. n0jl).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbidj_aid(t2, &
 nocc, nactive, a, i, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidj_aid: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidj_aid
j1 = min(i - 1, n1jl)
j_aibjbidj_aid: do j = n0jl, j1
if (j == i) cycle j_aibjbidj_aid
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbidj_aid
end do b_aibjbidj_aid
end do i_aibjbidj_aid
end do a_aibjbidj_aid
end do d_aibjbidj_aid
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_abjd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbidj_abjd: do b = b0, n1bc
if (b == d) cycle b_aibjbidj_abjd
j_aibjbidj_abjd: do j = n0jl, n1jl
a0 = max(b + 1, n0a)
a_aibjbidj_abjd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidj_abjd
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbidj_abjd(t2, &
 nocc, nactive, a, b, j, d)
i0 = max(j + 1, n0ik)
i_aibjbidj_abjd: do i = i0, n1ik
if (i == j) cycle i_aibjbidj_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_abjd
end do a_aibjbidj_abjd
end do j_aibjbidj_abjd
end do b_aibjbidj_abjd
end do d_aibjbidj_abjd
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_aibd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbidj_aibd: do b = b0, n1bc
if (b == d) cycle b_aibjbidj_aibd
a0 = max(b + 1, n0a)
a_aibjbidj_aibd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidj_aibd
i_aibjbidj_aibd: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbidj_aibd(t2, &
 nocc, nactive, a, i, b, d)
j1 = min(i - 1, n1jl)
j_aibjbidj_aibd: do j = n0jl, j1
if (j == i) cycle j_aibjbidj_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbidj_aibd
end do i_aibjbidj_aibd
end do a_aibjbidj_aibd
end do b_aibjbidj_aibd
end do d_aibjbidj_aibd
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_aijd: do d = n0d, n1d
j_aibjbidj_aijd: do j = n0jl, n1jl
a_aibjbidj_aijd: do a = n0a, n1a
if (a == d) cycle a_aibjbidj_aijd
i0 = max(j + 1, n0ik)
i_aibjbidj_aijd: do i = i0, n1ik
if (i == j) cycle i_aibjbidj_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbidj_aijd(t2, &
 nocc, nactive, a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidj_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidj_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidj_aijd
end do i_aibjbidj_aijd
end do a_aibjbidj_aijd
end do j_aibjbidj_aijd
end do d_aibjbidj_aijd
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, l
! Equalities: c == b, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibibidl_aidl: do d = n0d, n1d
l_aibibidl_aidl: do l = n0l, n1l
a_aibibidl_aidl: do a = n0a, n1a
if (a == d) cycle a_aibibidl_aidl
i0 = max(l + 1, n0ijk)
i_aibibidl_aidl: do i = i0, n1ijk
if (i == l) cycle i_aibibidl_aidl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibibidl_aidl(t2, &
 nocc, nactive, a, i, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibidl_aidl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibidl_aidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibidl_aidl
end do i_aibibidl_aidl
end do a_aibibidl_aidl
end do l_aibibidl_aidl
end do d_aibibidl_aidl
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, l
! Equalities: c == b, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibibidl_aibdl: do d = n0d, n1d
l_aibibidl_aibdl: do l = n0l, n1l
b0 = max(d + 1, n0bc)
b_aibibidl_aibdl: do b = b0, n1bc
if (b == d) cycle b_aibibidl_aibdl
a0 = max(b + 1, n0a)
a_aibibidl_aibdl: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibidl_aibdl
i0 = max(l + 1, n0ijk)
i_aibibidl_aibdl: do i = i0, n1ijk
if (i == l) cycle i_aibibidl_aibdl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibibidl_aibdl(t2, nocc, nactive, a, &
 i, b, d, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidl_aibdl
end do a_aibibidl_aibdl
end do b_aibibidl_aibdl
end do l_aibibidl_aibdl
end do d_aibibidl_aibdl
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_aid: do d = n0d, n1d
a_aibjbjdi_aid: do a = n0a, n1a
if (a == d) cycle a_aibjbjdi_aid
i1 = min(j - 1, n1il)
i_aibjbjdi_aid: do i = n0il, i1
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jk .ge. n0jk).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbjdi_aid(t2, &
 nocc, nactive, a, i, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdi_aid: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdi_aid
j_aibjbjdi_aid: do j = n0jk, n1jk
if (j == i) cycle j_aibjbjdi_aid
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdi_aid
end do b_aibjbjdi_aid
end do i_aibjbjdi_aid
end do a_aibjbjdi_aid
end do d_aibjbjdi_aid
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_abjd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdi_abjd: do b = b0, n1bc
if (b == d) cycle b_aibjbjdi_abjd
j_aibjbjdi_abjd: do j = n0jk, n1jk
a0 = max(b + 1, n0a)
a_aibjbjdi_abjd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdi_abjd
nn1i = min(j - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbjdi_abjd(t2, &
 nocc, nactive, a, b, j, d)
i1 = min(j - 1, n1il)
i_aibjbjdi_abjd: do i = n0il, i1
if (i == j) cycle i_aibjbjdi_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_abjd
end do a_aibjbjdi_abjd
end do j_aibjbjdi_abjd
end do b_aibjbjdi_abjd
end do d_aibjbjdi_abjd
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_aibd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdi_aibd: do b = b0, n1bc
if (b == d) cycle b_aibjbjdi_aibd
a0 = max(b + 1, n0a)
a_aibjbjdi_aibd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdi_aibd
i1 = min(j - 1, n1il)
i_aibjbjdi_aibd: do i = n0il, i1
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbjdi_aibd(t2, &
 nocc, nactive, a, i, b, d)
j_aibjbjdi_aibd: do j = n0jk, n1jk
if (j == i) cycle j_aibjbjdi_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdi_aibd
end do i_aibjbjdi_aibd
end do a_aibjbjdi_aibd
end do b_aibjbjdi_aibd
end do d_aibjbjdi_aibd
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_aijd: do d = n0d, n1d
j_aibjbjdi_aijd: do j = n0jk, n1jk
a_aibjbjdi_aijd: do a = n0a, n1a
if (a == d) cycle a_aibjbjdi_aijd
i1 = min(j - 1, n1il)
i_aibjbjdi_aijd: do i = n0il, i1
if (i == j) cycle i_aibjbjdi_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbjdi_aijd(t2, &
 nocc, nactive, a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdi_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdi_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdi_aijd
end do i_aibjbjdi_aijd
end do a_aibjbjdi_aijd
end do j_aibjbjdi_aijd
end do d_aibjbjdi_aijd
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, l == j
! No equalities independent of the above can hold.
!
c_aiajckaj_aick: do c = n0c, n1c
k_aiajckaj_aick: do k = n0k, n1k
a1 = min(c - 1, n1abd)
a_aiajckaj_aick: do a = n0abd, a1
if (a == c) cycle a_aiajckaj_aick
i_aiajckaj_aick: do i = n0i, n1i
if (i == k) cycle i_aiajckaj_aick
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajckaj_aick(t2, &
 nocc, nactive, a, i, c, k)
j1 = min(i - 1, k - 1, n1jl)
j_aiajckaj_aick: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aiajckaj_aick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajckaj_aick
end do i_aiajckaj_aick
end do a_aiajckaj_aick
end do k_aiajckaj_aick
end do c_aiajckaj_aick
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, l == j
! No equalities independent of the above can hold.
!
c_aiajckaj_aijck: do c = n0c, n1c
k_aiajckaj_aijck: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aiajckaj_aijck: do j = n0jl, j1
if (j == k) cycle j_aiajckaj_aijck
a1 = min(c - 1, n1abd)
a_aiajckaj_aijck: do a = n0abd, a1
if (a == c) cycle a_aiajckaj_aijck
i0 = max(j + 1, n0i)
i_aiajckaj_aijck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckaj_aijck
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajckaj_aijck(t2, nocc, nactive, a, &
 i, j, c, k)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaj_aijck
end do a_aiajckaj_aijck
end do j_aiajckaj_aijck
end do k_aiajckaj_aijck
end do c_aiajckaj_aijck
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajcjal_aicl: do l = n0l, n1l
c_aiajcjal_aicl: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiajcjal_aicl: do a = n0abd, a1
if (a == c) cycle a_aiajcjal_aicl
i_aiajcjal_aicl: do i = n0i, n1i
if (i == l) cycle i_aiajcjal_aicl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajcjal_aicl(t2, &
 nocc, nactive, a, i, c, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aiajcjal_aicl: do j = j0, j1
if (j == i .or. j == l) cycle j_aiajcjal_aicl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcjal_aicl
end do i_aiajcjal_aicl
end do a_aiajcjal_aicl
end do c_aiajcjal_aicl
end do l_aiajcjal_aicl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajcjal_aijcl: do l = n0l, n1l
c_aiajcjal_aijcl: do c = n0c, n1c
j0 = max(l + 1, n0jk)
j_aiajcjal_aijcl: do j = j0, n1jk
if (j == l) cycle j_aiajcjal_aijcl
a1 = min(c - 1, n1abd)
a_aiajcjal_aijcl: do a = n0abd, a1
if (a == c) cycle a_aiajcjal_aijcl
i0 = max(j + 1, n0i)
i_aiajcjal_aijcl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajcjal_aijcl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajcjal_aijcl(t2, nocc, nactive, a, &
 i, j, c, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjal_aijcl
end do a_aiajcjal_aijcl
end do j_aiajcjal_aijcl
end do c_aiajcjal_aijcl
end do l_aiajcjal_aijcl
end subroutine ccjac_22_tripletmp_dav_part3
end module ccjac_block_22_tripletmp_dav_part3
