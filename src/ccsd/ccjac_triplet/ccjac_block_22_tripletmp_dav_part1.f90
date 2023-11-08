module ccjac_block_22_tripletmp_dav_part1
use eom_ccsd_22_tripletmp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-06 14:24:57 UTC.
!
contains
subroutine ccjac_22_tripletmp_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0b, nn0c, nn0i, nn0j, nn0k, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b0, b1, c0, i0, i1, j0, j1, k0
integer :: n0ab, n0abc, n0abd, n0ac, n0ad
integer :: n0bc, n0bd, n0ij, n0ik, n0il
integer :: n0jk, n0jl
integer :: n1ab, n1abc, n1abd, n1ac, n1ad
integer :: n1bc, n1bd, n1ij, n1ik, n1il
integer :: n1jk, n1jl
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
n0ab = max(n0a, n0b)
n0abc = max(n0a, n0b, n0c)
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1ab = min(n1a, n1b)
n1abc = min(n1a, n1b, n1c)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1ij = min(n1i, n1j)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k, l
! Equalities: c == b
! No equalities independent of the above can hold.
!
d_aibjbkdl_aijkdl: do d = n0d, n1d
l_aibjbkdl_aijkdl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibjbkdl_aijkdl: do k = k0, n1k
if (k == l) cycle k_aibjbkdl_aijkdl
j_aibjbkdl_aijkdl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjbkdl_aijkdl
a_aibjbkdl_aijkdl: do a = n0a, n1a
if (a == d) cycle a_aibjbkdl_aijkdl
i_aibjbkdl_aijkdl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjbkdl_aijkdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbkdl_aijkdl(t2, &
 nocc, nactive, a, i, j, k, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdl_aijkdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdl_aijkdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbkdl_aijkdl
end do i_aibjbkdl_aijkdl
end do a_aibjbkdl_aijkdl
end do j_aibjbkdl_aijkdl
end do k_aibjbkdl_aijkdl
end do l_aibjbkdl_aijkdl
end do d_aibjbkdl_aijkdl
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k, l
! Equalities: d == b
! No equalities independent of the above can hold.
!
l_aibjckbl_aijckl: do l = n0l, n1l
c_aibjckbl_aijckl: do c = n0c, n1c
k0 = max(l + 1, n0k)
k_aibjckbl_aijckl: do k = k0, n1k
if (k == l) cycle k_aibjckbl_aijckl
j_aibjckbl_aijckl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckbl_aijckl
a_aibjckbl_aijckl: do a = n0a, n1a
if (a == c) cycle a_aibjckbl_aijckl
i_aibjckbl_aijckl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckbl_aijckl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckbl_aijckl(t2, &
 nocc, nactive, a, i, j, c, k, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbl_aijckl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbl_aijckl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjckbl_aijckl
end do i_aibjckbl_aijckl
end do a_aibjckbl_aijckl
end do j_aibjckbl_aijckl
end do k_aibjckbl_aijckl
end do c_aibjckbl_aijckl
end do l_aibjckbl_aijckl
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k, l
! Equalities: c == a
! No equalities independent of the above can hold.
!
d_aibjakdl_ibjkdl: do d = n0d, n1d
l_aibjakdl_ibjkdl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibjakdl_ibjkdl: do k = k0, n1k
if (k == l) cycle k_aibjakdl_ibjkdl
b_aibjakdl_ibjkdl: do b = n0b, n1b
if (b == d) cycle b_aibjakdl_ibjkdl
j_aibjakdl_ibjkdl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakdl_ibjkdl
i_aibjakdl_ibjkdl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakdl_ibjkdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakdl_ibjkdl(t2, &
 nocc, nactive, i, b, j, k, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdl_ibjkdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdl_ibjkdl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakdl_ibjkdl
end do i_aibjakdl_ibjkdl
end do j_aibjakdl_ibjkdl
end do b_aibjakdl_ibjkdl
end do k_aibjakdl_ibjkdl
end do l_aibjakdl_ibjkdl
end do d_aibjakdl_ibjkdl
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, l
! Equalities: d == a
! No equalities independent of the above can hold.
!
l_aibjckal_ibjckl: do l = n0l, n1l
c_aibjckal_ibjckl: do c = n0c, n1c
k0 = max(l + 1, n0k)
k_aibjckal_ibjckl: do k = k0, n1k
if (k == l) cycle k_aibjckal_ibjckl
b_aibjckal_ibjckl: do b = n0b, n1b
if (b == c) cycle b_aibjckal_ibjckl
j_aibjckal_ibjckl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckal_ibjckl
i_aibjckal_ibjckl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckal_ibjckl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckal_ibjckl(t2, &
 nocc, nactive, i, b, j, c, k, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckal_ibjckl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckal_ibjckl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckal_ibjckl
end do i_aibjckal_ibjckl
end do j_aibjckal_ibjckl
end do b_aibjckal_ibjckl
end do k_aibjckal_ibjckl
end do c_aibjckal_ibjckl
end do l_aibjckal_ibjckl
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i, k
! Equalities: l == j
! No equalities independent of the above can hold.
!
d_aibjckdj_aibckd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjckdj_aibckd: do c = c0, n1c
if (c == d) cycle c_aibjckdj_aibckd
k_aibjckdj_aibckd: do k = n0k, n1k
b_aibjckdj_aibckd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjckdj_aibckd
a0 = max(b + 1, n0a)
a_aibjckdj_aibckd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdj_aibckd
i_aibjckdj_aibckd: do i = n0i, n1i
if (i == k) cycle i_aibjckdj_aibckd
nn1j = min(k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckdj_aibckd(t2, &
 nocc, nactive, a, i, b, c, k, d)
j1 = min(k - 1, n1jl)
j_aibjckdj_aibckd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckdj_aibckd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckdj_aibckd
end do i_aibjckdj_aibckd
end do a_aibjckdj_aibckd
end do b_aibjckdj_aibckd
end do k_aibjckdj_aibckd
end do c_aibjckdj_aibckd
end do d_aibjckdj_aibckd
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i, l
! Equalities: k == j
! No equalities independent of the above can hold.
!
d_aibjcjdl_aibcdl: do d = n0d, n1d
l_aibjcjdl_aibcdl: do l = n0l, n1l
c0 = max(d + 1, n0c)
c_aibjcjdl_aibcdl: do c = c0, n1c
if (c == d) cycle c_aibjcjdl_aibcdl
b_aibjcjdl_aibcdl: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdl_aibcdl
a0 = max(b + 1, n0a)
a_aibjcjdl_aibcdl: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdl_aibcdl
i_aibjcjdl_aibcdl: do i = n0i, n1i
if (i == l) cycle i_aibjcjdl_aibcdl
nn0j = max(l + 1, n0jk)
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjdl_aibcdl(t2, &
 nocc, nactive, a, i, b, c, d, l)
j0 = max(l + 1, n0jk)
j_aibjcjdl_aibcdl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjcjdl_aibcdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjdl_aibcdl
end do i_aibjcjdl_aibcdl
end do a_aibjcjdl_aibcdl
end do b_aibjcjdl_aibcdl
end do c_aibjcjdl_aibcdl
end do l_aibjcjdl_aibcdl
end do d_aibjcjdl_aibcdl
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: l == i
! No equalities independent of the above can hold.
!
d_aibjckdi_abjckd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjckdi_abjckd: do c = c0, n1c
if (c == d) cycle c_aibjckdi_abjckd
k_aibjckdi_abjckd: do k = n0k, n1k
b_aibjckdi_abjckd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjckdi_abjckd
j_aibjckdi_abjckd: do j = n0j, n1j
if (j == k) cycle j_aibjckdi_abjckd
a0 = max(b + 1, n0a)
a_aibjckdi_abjckd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdi_abjckd
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckdi_abjckd(t2, &
 nocc, nactive, a, b, j, c, k, d)
i1 = min(k - 1, n1il)
i_aibjckdi_abjckd: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjckdi_abjckd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdi_abjckd
end do a_aibjckdi_abjckd
end do j_aibjckdi_abjckd
end do b_aibjckdi_abjckd
end do k_aibjckdi_abjckd
end do c_aibjckdi_abjckd
end do d_aibjckdi_abjckd
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, l
! Equalities: k == i
! No equalities independent of the above can hold.
!
d_aibjcidl_abjcdl: do d = n0d, n1d
l_aibjcidl_abjcdl: do l = n0l, n1l
c0 = max(d + 1, n0c)
c_aibjcidl_abjcdl: do c = c0, n1c
if (c == d) cycle c_aibjcidl_abjcdl
b_aibjcidl_abjcdl: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidl_abjcdl
j_aibjcidl_abjcdl: do j = n0j, n1j
if (j == l) cycle j_aibjcidl_abjcdl
a0 = max(b + 1, n0a)
a_aibjcidl_abjcdl: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidl_abjcdl
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcidl_abjcdl(t2, &
 nocc, nactive, a, b, j, c, d, l)
i0 = max(l + 1, n0ik)
i_aibjcidl_abjcdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcidl_abjcdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidl_abjcdl
end do a_aibjcidl_abjcdl
end do j_aibjcidl_abjcdl
end do b_aibjcidl_abjcdl
end do c_aibjcidl_abjcdl
end do l_aibjcidl_abjcdl
end do d_aibjcidl_abjcdl
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k, l
! Equalities: b == a, c == a
! No equalities independent of the above can hold.
!
d_aiajakdl_aijkdl: do d = n0d, n1d
l_aiajakdl_aijkdl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aiajakdl_aijkdl: do k = k0, n1k
if (k == l) cycle k_aiajakdl_aijkdl
j_aiajakdl_aijkdl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aiajakdl_aijkdl
a0 = max(d + 1, n0abc)
a_aiajakdl_aijkdl: do a = a0, n1abc
if (a == d) cycle a_aiajakdl_aijkdl
i0 = max(j + 1, n0i)
i_aiajakdl_aijkdl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajakdl_aijkdl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajakdl_aijkdl(t2, nocc, nactive, a, &
 i, j, k, d, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakdl_aijkdl
end do a_aiajakdl_aijkdl
end do j_aiajakdl_aijkdl
end do k_aiajakdl_aijkdl
end do l_aiajakdl_aijkdl
end do d_aiajakdl_aijkdl
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj_aikd: do d = n0d, n1d
k_aibjbkdj_aikd: do k = n0k, n1k
a_aibjbkdj_aikd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdj_aikd
i_aibjbkdj_aikd: do i = n0i, n1i
if (i == k) cycle i_aibjbkdj_aikd
nn1j = min(k - 1, n1jl)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1j .ge. n0jl).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbkdj_aikd(t2, &
 nocc, nactive, a, i, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdj_aikd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdj_aikd
j1 = min(k - 1, n1jl)
j_aibjbkdj_aikd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjbkdj_aikd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbkdj_aikd
end do b_aibjbkdj_aikd
end do i_aibjbkdj_aikd
end do a_aibjbkdj_aikd
end do k_aibjbkdj_aikd
end do d_aibjbkdj_aikd
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj_aijkd: do d = n0d, n1d
k_aibjbkdj_aijkd: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aibjbkdj_aijkd: do j = n0jl, j1
if (j == k) cycle j_aibjbkdj_aijkd
a_aibjbkdj_aijkd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdj_aijkd
i_aibjbkdj_aijkd: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjbkdj_aijkd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbkdj_aijkd(t2, &
 nocc, nactive, a, i, j, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdj_aijkd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdj_aijkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbkdj_aijkd
end do i_aibjbkdj_aijkd
end do a_aibjbkdj_aijkd
end do j_aibjbkdj_aijkd
end do k_aibjbkdj_aijkd
end do d_aibjbkdj_aijkd
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj_aibkd: do d = n0d, n1d
k_aibjbkdj_aibkd: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdj_aibkd: do b = b0, n1bc
if (b == d) cycle b_aibjbkdj_aibkd
a0 = max(b + 1, n0a)
a_aibjbkdj_aibkd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdj_aibkd
i_aibjbkdj_aibkd: do i = n0i, n1i
if (i == k) cycle i_aibjbkdj_aibkd
nn1j = min(k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbkdj_aibkd(t2, &
 nocc, nactive, a, i, b, k, d)
j1 = min(k - 1, n1jl)
j_aibjbkdj_aibkd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjbkdj_aibkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbkdj_aibkd
end do i_aibjbkdj_aibkd
end do a_aibjbkdj_aibkd
end do b_aibjbkdj_aibkd
end do k_aibjbkdj_aibkd
end do d_aibjbkdj_aibkd
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k, l
! Equalities: c == b, j == i
! No equalities independent of the above can hold.
!
d_aibibkdl_aikdl: do d = n0d, n1d
l_aibibkdl_aikdl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibibkdl_aikdl: do k = k0, n1k
if (k == l) cycle k_aibibkdl_aikdl
a_aibibkdl_aikdl: do a = n0a, n1a
if (a == d) cycle a_aibibkdl_aikdl
i_aibibkdl_aikdl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibibkdl_aikdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibibkdl_aikdl(t2, &
 nocc, nactive, a, i, k, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibkdl_aikdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibkdl_aikdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibkdl_aikdl
end do i_aibibkdl_aikdl
end do a_aibibkdl_aikdl
end do k_aibibkdl_aikdl
end do l_aibibkdl_aikdl
end do d_aibibkdl_aikdl
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, l
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdl_aidl: do d = n0d, n1d
l_aibjbjdl_aidl: do l = n0l, n1l
a_aibjbjdl_aidl: do a = n0a, n1a
if (a == d) cycle a_aibjbjdl_aidl
i_aibjbjdl_aidl: do i = n0i, n1i
if (i == l) cycle i_aibjbjdl_aidl
nn0j = max(l + 1, n0jk)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jk .ge. nn0j).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbjdl_aidl(t2, &
 nocc, nactive, a, i, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdl_aidl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdl_aidl
j0 = max(l + 1, n0jk)
j_aibjbjdl_aidl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjbjdl_aidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdl_aidl
end do b_aibjbjdl_aidl
end do i_aibjbjdl_aidl
end do a_aibjbjdl_aidl
end do l_aibjbjdl_aidl
end do d_aibjbjdl_aidl
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, l
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdl_aijdl: do d = n0d, n1d
l_aibjbjdl_aijdl: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aibjbjdl_aijdl: do j = j0, n1jk
if (j == l) cycle j_aibjbjdl_aijdl
a_aibjbjdl_aijdl: do a = n0a, n1a
if (a == d) cycle a_aibjbjdl_aijdl
i_aibjbjdl_aijdl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjbjdl_aijdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbjdl_aijdl(t2, &
 nocc, nactive, a, i, j, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdl_aijdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdl_aijdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdl_aijdl
end do i_aibjbjdl_aijdl
end do a_aibjbjdl_aijdl
end do j_aibjbjdl_aijdl
end do l_aibjbjdl_aijdl
end do d_aibjbjdl_aijdl
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, l
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdl_aibdl: do d = n0d, n1d
l_aibjbjdl_aibdl: do l = n0l, n1l
b0 = max(d + 1, n0bc)
b_aibjbjdl_aibdl: do b = b0, n1bc
if (b == d) cycle b_aibjbjdl_aibdl
a0 = max(b + 1, n0a)
a_aibjbjdl_aibdl: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdl_aibdl
i_aibjbjdl_aibdl: do i = n0i, n1i
if (i == l) cycle i_aibjbjdl_aibdl
nn0j = max(l + 1, n0jk)
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbjdl_aibdl(t2, &
 nocc, nactive, a, i, b, d, l)
j0 = max(l + 1, n0jk)
j_aibjbjdl_aibdl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjbjdl_aibdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdl_aibdl
end do i_aibjbjdl_aibdl
end do a_aibjbjdl_aibdl
end do b_aibjbjdl_aibdl
end do l_aibjbjdl_aibdl
end do d_aibjbjdl_aibdl
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, l == i
! No equalities independent of the above can hold.
!
d_aibjbkdi_abjkd: do d = n0d, n1d
k_aibjbkdi_abjkd: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdi_abjkd: do b = b0, n1bc
if (b == d) cycle b_aibjbkdi_abjkd
j_aibjbkdi_abjkd: do j = n0j, n1j
if (j == k) cycle j_aibjbkdi_abjkd
a0 = max(b + 1, n0a)
a_aibjbkdi_abjkd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdi_abjkd
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbkdi_abjkd(t2, &
 nocc, nactive, a, b, j, k, d)
i1 = min(k - 1, n1il)
i_aibjbkdi_abjkd: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkdi_abjkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdi_abjkd
end do a_aibjbkdi_abjkd
end do j_aibjbkdi_abjkd
end do b_aibjbkdi_abjkd
end do k_aibjbkdi_abjkd
end do d_aibjbkdi_abjkd
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, l == i
! No equalities independent of the above can hold.
!
d_aibjbkdi_aijkd: do d = n0d, n1d
k_aibjbkdi_aijkd: do k = n0k, n1k
j_aibjbkdi_aijkd: do j = n0j, n1j
if (j == k) cycle j_aibjbkdi_aijkd
a_aibjbkdi_aijkd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdi_aijkd
i1 = min(k - 1, n1il)
i_aibjbkdi_aijkd: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkdi_aijkd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbkdi_aijkd(t2, &
 nocc, nactive, a, i, j, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdi_aijkd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdi_aijkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbkdi_aijkd
end do i_aibjbkdi_aijkd
end do a_aibjbkdi_aijkd
end do j_aibjbkdi_aijkd
end do k_aibjbkdi_aijkd
end do d_aibjbkdi_aijkd
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, l
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
d_aibjbidl_abjdl: do d = n0d, n1d
l_aibjbidl_abjdl: do l = n0l, n1l
b0 = max(d + 1, n0bc)
b_aibjbidl_abjdl: do b = b0, n1bc
if (b == d) cycle b_aibjbidl_abjdl
j_aibjbidl_abjdl: do j = n0j, n1j
if (j == l) cycle j_aibjbidl_abjdl
a0 = max(b + 1, n0a)
a_aibjbidl_abjdl: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidl_abjdl
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbidl_abjdl(t2, &
 nocc, nactive, a, b, j, d, l)
i0 = max(l + 1, n0ik)
i_aibjbidl_abjdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbidl_abjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidl_abjdl
end do a_aibjbidl_abjdl
end do j_aibjbidl_abjdl
end do b_aibjbidl_abjdl
end do l_aibjbidl_abjdl
end do d_aibjbidl_abjdl
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, l
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
d_aibjbidl_aijdl: do d = n0d, n1d
l_aibjbidl_aijdl: do l = n0l, n1l
j_aibjbidl_aijdl: do j = n0j, n1j
if (j == l) cycle j_aibjbidl_aijdl
a_aibjbidl_aijdl: do a = n0a, n1a
if (a == d) cycle a_aibjbidl_aijdl
i0 = max(l + 1, n0ik)
i_aibjbidl_aijdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbidl_aijdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjbidl_aijdl(t2, &
 nocc, nactive, a, i, j, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidl_aijdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidl_aijdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidl_aijdl
end do i_aibjbidl_aijdl
end do a_aibjbidl_aijdl
end do j_aibjbidl_aijdl
end do l_aibjbidl_aijdl
end do d_aibjbidl_aijdl
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == a
! No equalities independent of the above can hold.
!
l_aiajckal_aijckl: do l = n0l, n1l
c_aiajckal_aijckl: do c = n0c, n1c
k0 = max(l + 1, n0k)
k_aiajckal_aijckl: do k = k0, n1k
if (k == l) cycle k_aiajckal_aijckl
j_aiajckal_aijckl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aiajckal_aijckl
a1 = min(c - 1, n1abd)
a_aiajckal_aijckl: do a = n0abd, a1
if (a == c) cycle a_aiajckal_aijckl
i0 = max(j + 1, n0i)
i_aiajckal_aijckl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckal_aijckl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajckal_aijckl(t2, nocc, nactive, a, &
 i, j, c, k, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckal_aijckl
end do a_aiajckal_aijckl
end do j_aiajckal_aijckl
end do k_aiajckal_aijckl
end do c_aiajckal_aijckl
end do l_aiajckal_aijckl
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i, k
! Equalities: b == a, l == j
! No equalities independent of the above can hold.
!
d_aiajckdj_aickd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajckdj_aickd: do c = c0, n1c
if (c == d) cycle c_aiajckdj_aickd
k_aiajckdj_aickd: do k = n0k, n1k
a_aiajckdj_aickd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajckdj_aickd
i_aiajckdj_aickd: do i = n0i, n1i
if (i == k) cycle i_aiajckdj_aickd
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajckdj_aickd(t2, &
 nocc, nactive, a, i, c, k, d)
j1 = min(i - 1, k - 1, n1jl)
j_aiajckdj_aickd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aiajckdj_aickd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajckdj_aickd
end do i_aiajckdj_aickd
end do a_aiajckdj_aickd
end do k_aiajckdj_aickd
end do c_aiajckdj_aickd
end do d_aiajckdj_aickd
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i, l
! Equalities: b == a, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdl_aicdl: do d = n0d, n1d
l_aiajcjdl_aicdl: do l = n0l, n1l
c0 = max(d + 1, n0c)
c_aiajcjdl_aicdl: do c = c0, n1c
if (c == d) cycle c_aiajcjdl_aicdl
a_aiajcjdl_aicdl: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcjdl_aicdl
i_aiajcjdl_aicdl: do i = n0i, n1i
if (i == l) cycle i_aiajcjdl_aicdl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajcjdl_aicdl(t2, &
 nocc, nactive, a, i, c, d, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aiajcjdl_aicdl: do j = j0, j1
if (j == i .or. j == l) cycle j_aiajcjdl_aicdl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcjdl_aicdl
end do i_aiajcjdl_aicdl
end do a_aiajcjdl_aicdl
end do c_aiajcjdl_aicdl
end do l_aiajcjdl_aicdl
end do d_aiajcjdl_aicdl
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, l == i
! No equalities independent of the above can hold.
!
d_aiajckdi_ajckd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajckdi_ajckd: do c = c0, n1c
if (c == d) cycle c_aiajckdi_ajckd
k_aiajckdi_ajckd: do k = n0k, n1k
j_aiajckdi_ajckd: do j = n0j, n1j
if (j == k) cycle j_aiajckdi_ajckd
a_aiajckdi_ajckd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajckdi_ajckd
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajckdi_ajckd(t2, &
 nocc, nactive, a, j, c, k, d)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajckdi_ajckd: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajckdi_ajckd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdi_ajckd
end do a_aiajckdi_ajckd
end do j_aiajckdi_ajckd
end do k_aiajckdi_ajckd
end do c_aiajckdi_ajckd
end do d_aiajckdi_ajckd
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, l
! Equalities: b == a, k == i
! No equalities independent of the above can hold.
!
d_aiajcidl_ajcdl: do d = n0d, n1d
l_aiajcidl_ajcdl: do l = n0l, n1l
c0 = max(d + 1, n0c)
c_aiajcidl_ajcdl: do c = c0, n1c
if (c == d) cycle c_aiajcidl_ajcdl
j_aiajcidl_ajcdl: do j = n0j, n1j
if (j == l) cycle j_aiajcidl_ajcdl
a_aiajcidl_ajcdl: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcidl_ajcdl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajcidl_ajcdl(t2, &
 nocc, nactive, a, j, c, d, l)
i0 = max(j + 1, l + 1, n0ik)
i_aiajcidl_ajcdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcidl_ajcdl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidl_ajcdl
end do a_aiajcidl_ajcdl
end do j_aiajcidl_ajcdl
end do c_aiajcidl_ajcdl
end do l_aiajcidl_ajcdl
end do d_aiajcidl_ajcdl
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b
! No equalities independent of the above can hold.
!
l_aibjakbl_ibjkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibjakbl_ibjkl: do k = k0, n1k
if (k == l) cycle k_aibjakbl_ibjkl
b_aibjakbl_ibjkl: do b = n0bd, n1bd
j_aibjakbl_ibjkl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakbl_ibjkl
i_aibjakbl_ibjkl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakbl_ibjkl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbl_ibjkl(t2, &
 nocc, nactive, i, b, j, k, l)
a0 = max(b + 1, n0ac)
a_aibjakbl_ibjkl: do a = a0, n1ac
if (a == b) cycle a_aibjakbl_ibjkl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbl_ibjkl
end do i_aibjakbl_ibjkl
end do j_aibjakbl_ibjkl
end do b_aibjakbl_ibjkl
end do k_aibjakbl_ibjkl
end do l_aibjakbl_ibjkl
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b
! No equalities independent of the above can hold.
!
l_aibjakbl_aijkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibjakbl_aijkl: do k = k0, n1k
if (k == l) cycle k_aibjakbl_aijkl
j_aibjakbl_aijkl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakbl_aijkl
a_aibjakbl_aijkl: do a = n0ac, n1ac
i_aibjakbl_aijkl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakbl_aijkl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbl_aijkl(t2, &
 nocc, nactive, a, i, j, k, l)
b1 = min(a - 1, n1bd)
b_aibjakbl_aijkl: do b = n0bd, b1
if (b == a) cycle b_aibjakbl_aijkl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjakbl_aijkl
end do i_aibjakbl_aijkl
end do a_aibjakbl_aijkl
end do j_aibjakbl_aijkl
end do k_aibjakbl_aijkl
end do l_aibjakbl_aijkl
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj_aick: do c = n0c, n1c
k_aibjckbj_aick: do k = n0k, n1k
a_aibjckbj_aick: do a = n0a, n1a
if (a == c) cycle a_aibjckbj_aick
i_aibjckbj_aick: do i = n0i, n1i
if (i == k) cycle i_aibjckbj_aick
nn1j = min(k - 1, n1jl)
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckbj_aick(t2, &
 nocc, nactive, a, i, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbj_aick: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbj_aick
j1 = min(k - 1, n1jl)
j_aibjckbj_aick: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckbj_aick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckbj_aick
end do b_aibjckbj_aick
end do i_aibjckbj_aick
end do a_aibjckbj_aick
end do k_aibjckbj_aick
end do c_aibjckbj_aick
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj_aijck: do c = n0c, n1c
k_aibjckbj_aijck: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aibjckbj_aijck: do j = n0jl, j1
if (j == k) cycle j_aibjckbj_aijck
a_aibjckbj_aijck: do a = n0a, n1a
if (a == c) cycle a_aibjckbj_aijck
i_aibjckbj_aijck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbj_aijck
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckbj_aijck(t2, &
 nocc, nactive, a, i, j, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbj_aijck: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbj_aijck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjckbj_aijck
end do i_aibjckbj_aijck
end do a_aibjckbj_aijck
end do j_aibjckbj_aijck
end do k_aibjckbj_aijck
end do c_aibjckbj_aijck
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj_aibck: do c = n0c, n1c
k_aibjckbj_aibck: do k = n0k, n1k
b1 = min(c - 1, n1bd)
b_aibjckbj_aibck: do b = n0bd, b1
if (b == c) cycle b_aibjckbj_aibck
a0 = max(b + 1, n0a)
a_aibjckbj_aibck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbj_aibck
i_aibjckbj_aibck: do i = n0i, n1i
if (i == k) cycle i_aibjckbj_aibck
nn1j = min(k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckbj_aibck(t2, &
 nocc, nactive, a, i, b, c, k)
j1 = min(k - 1, n1jl)
j_aibjckbj_aibck: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckbj_aibck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckbj_aibck
end do i_aibjckbj_aibck
end do a_aibjckbj_aibck
end do b_aibjckbj_aibck
end do k_aibjckbj_aibck
end do c_aibjckbj_aibck
end subroutine ccjac_22_tripletmp_dav_part1
end module ccjac_block_22_tripletmp_dav_part1
