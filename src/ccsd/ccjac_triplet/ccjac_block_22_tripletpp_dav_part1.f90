module ccjac_block_22_tripletpp_dav_part1
use eom_ccsd_22_tripletpp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-05 15:48:39 UTC.
!
contains
subroutine ccjac_22_tripletpp_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0ac, n0ad, n0bc, n0bd, n0ik
integer :: n0il, n0jk, n0jl
integer :: n1ac, n1ad, n1bc, n1bd, n1ik
integer :: n1il, n1jk, n1jl
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
n0ac = max(n0a, n0c)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
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
i0 = max(j + 1, n0i)
i_aibjakdl_ibjkdl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakdl_ibjkdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakdl_ibjkdl(t2, &
 nocc, nactive, i, b, j, k, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdl_ibjkdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdl_ibjkdl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  2
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
i0 = max(j + 1, n0i)
i_aibjckal_ibjckl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckal_ibjckl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckal_ibjckl(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  3
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
i0 = max(j + 1, n0i)
i_aibjbkdl_aijkdl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjbkdl_aijkdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbkdl_aijkdl(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  4
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
i0 = max(j + 1, n0i)
i_aibjckbl_aijckl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckbl_aijckl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckbl_aijckl(t2, &
 nocc, nactive, a, i, j, c, k, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbl_aijckl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbl_aijckl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  5
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
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckdi_abjckd(t2, &
 nocc, nactive, a, b, j, c, k, d)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjckdi_abjckd: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjckdi_abjckd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  6
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
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcidl_abjcdl(t2, &
 nocc, nactive, a, b, j, c, d, l)
i0 = max(j + 1, l + 1, n0ik)
i_aibjcidl_abjcdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcidl_abjcdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  7
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
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckdj_aibckd(t2, &
 nocc, nactive, a, i, b, c, k, d)
j1 = min(i - 1, k - 1, n1jl)
j_aibjckdj_aibckd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckdj_aibckd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  8
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
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcjdl_aibcdl(t2, &
 nocc, nactive, a, i, b, c, d, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjcjdl_aibcdl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjcjdl_aibcdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b
! No equalities independent of the above can hold.
!
l_aibjakbl_ijkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibjakbl_ijkl: do k = k0, n1k
if (k == l) cycle k_aibjakbl_ijkl
j_aibjakbl_ijkl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakbl_ijkl
i0 = max(j + 1, n0i)
i_aibjakbl_ijkl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakbl_ijkl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbl_ijkl(t2, &
 nocc, nactive, i, j, k, l)
b_aibjakbl_ijkl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbl_ijkl: do a = a0, n1ac
if (a == b) cycle a_aibjakbl_ijkl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbl_ijkl
end do b_aibjakbl_ijkl
end do i_aibjakbl_ijkl
end do j_aibjakbl_ijkl
end do k_aibjakbl_ijkl
end do l_aibjakbl_ijkl
!
! Elementary loop  10
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
i0 = max(j + 1, n0i)
i_aibjakbl_aijkl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakbl_aijkl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbl_aijkl(t2, &
 nocc, nactive, a, i, j, k, l)
b1 = min(a - 1, n1bd)
b_aibjakbl_aijkl: do b = n0bd, b1
if (b == a) cycle b_aibjakbl_aijkl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  11
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
i0 = max(j + 1, n0i)
i_aibjakbl_ibjkl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakbl_ibjkl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbl_ibjkl(t2, &
 nocc, nactive, i, b, j, k, l)
a0 = max(b + 1, n0ac)
a_aibjakbl_ibjkl: do a = a0, n1ac
if (a == b) cycle a_aibjakbl_ibjkl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, l == i
! No equalities independent of the above can hold.
!
d_aibjakdi_bjkd: do d = n0d, n1d
k_aibjakdi_bjkd: do k = n0k, n1k
b_aibjakdi_bjkd: do b = n0b, n1b
if (b == d) cycle b_aibjakdi_bjkd
j_aibjakdi_bjkd: do j = n0j, n1j
if (j == k) cycle j_aibjakdi_bjkd
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
nn0a = max(b + 1, d + 1, n0ac)
if ((nn1i .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakdi_bjkd(t2, &
 nocc, nactive, b, j, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdi_bjkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdi_bjkd
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakdi_bjkd: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakdi_bjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdi_bjkd
end do a_aibjakdi_bjkd
end do j_aibjakdi_bjkd
end do b_aibjakdi_bjkd
end do k_aibjakdi_bjkd
end do d_aibjakdi_bjkd
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, l == i
! No equalities independent of the above can hold.
!
d_aibjakdi_ibjkd: do d = n0d, n1d
k_aibjakdi_ibjkd: do k = n0k, n1k
b_aibjakdi_ibjkd: do b = n0b, n1b
if (b == d) cycle b_aibjakdi_ibjkd
j_aibjakdi_ibjkd: do j = n0j, n1j
if (j == k) cycle j_aibjakdi_ibjkd
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakdi_ibjkd: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakdi_ibjkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakdi_ibjkd(t2, &
 nocc, nactive, i, b, j, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdi_ibjkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdi_ibjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakdi_ibjkd
end do i_aibjakdi_ibjkd
end do j_aibjakdi_ibjkd
end do b_aibjakdi_ibjkd
end do k_aibjakdi_ibjkd
end do d_aibjakdi_ibjkd
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, l == i
! No equalities independent of the above can hold.
!
d_aibjakdi_abjkd: do d = n0d, n1d
k_aibjakdi_abjkd: do k = n0k, n1k
b_aibjakdi_abjkd: do b = n0b, n1b
if (b == d) cycle b_aibjakdi_abjkd
j_aibjakdi_abjkd: do j = n0j, n1j
if (j == k) cycle j_aibjakdi_abjkd
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdi_abjkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdi_abjkd
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakdi_abjkd(t2, &
 nocc, nactive, a, b, j, k, d)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakdi_abjkd: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakdi_abjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdi_abjkd
end do a_aibjakdi_abjkd
end do j_aibjakdi_abjkd
end do b_aibjakdi_abjkd
end do k_aibjakdi_abjkd
end do d_aibjakdi_abjkd
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
d_aibjaidl_bjdl: do d = n0d, n1d
l_aibjaidl_bjdl: do l = n0l, n1l
b_aibjaidl_bjdl: do b = n0b, n1b
if (b == d) cycle b_aibjaidl_bjdl
j_aibjaidl_bjdl: do j = n0j, n1j
if (j == l) cycle j_aibjaidl_bjdl
nn0i = max(j + 1, l + 1, n0ik)
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidl_bjdl(t2, &
 nocc, nactive, b, j, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidl_bjdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidl_bjdl
i0 = max(j + 1, l + 1, n0ik)
i_aibjaidl_bjdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidl_bjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidl_bjdl
end do a_aibjaidl_bjdl
end do j_aibjaidl_bjdl
end do b_aibjaidl_bjdl
end do l_aibjaidl_bjdl
end do d_aibjaidl_bjdl
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
d_aibjaidl_ibjdl: do d = n0d, n1d
l_aibjaidl_ibjdl: do l = n0l, n1l
b_aibjaidl_ibjdl: do b = n0b, n1b
if (b == d) cycle b_aibjaidl_ibjdl
j_aibjaidl_ibjdl: do j = n0j, n1j
if (j == l) cycle j_aibjaidl_ibjdl
i0 = max(j + 1, l + 1, n0ik)
i_aibjaidl_ibjdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidl_ibjdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidl_ibjdl(t2, &
 nocc, nactive, i, b, j, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidl_ibjdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidl_ibjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidl_ibjdl
end do i_aibjaidl_ibjdl
end do j_aibjaidl_ibjdl
end do b_aibjaidl_ibjdl
end do l_aibjaidl_ibjdl
end do d_aibjaidl_ibjdl
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
d_aibjaidl_abjdl: do d = n0d, n1d
l_aibjaidl_abjdl: do l = n0l, n1l
b_aibjaidl_abjdl: do b = n0b, n1b
if (b == d) cycle b_aibjaidl_abjdl
j_aibjaidl_abjdl: do j = n0j, n1j
if (j == l) cycle j_aibjaidl_abjdl
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidl_abjdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidl_abjdl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidl_abjdl(t2, &
 nocc, nactive, a, b, j, d, l)
i0 = max(j + 1, l + 1, n0ik)
i_aibjaidl_abjdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidl_abjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidl_abjdl
end do a_aibjaidl_abjdl
end do j_aibjaidl_abjdl
end do b_aibjaidl_abjdl
end do l_aibjaidl_abjdl
end do d_aibjaidl_abjdl
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k
! Equalities: c == a, l == j
! No equalities independent of the above can hold.
!
d_aibjakdj_ibkd: do d = n0d, n1d
k_aibjakdj_ibkd: do k = n0k, n1k
b_aibjakdj_ibkd: do b = n0b, n1b
if (b == d) cycle b_aibjakdj_ibkd
i_aibjakdj_ibkd: do i = n0i, n1i
if (i == k) cycle i_aibjakdj_ibkd
nn1j = min(i - 1, k - 1, n1jl)
nn0a = max(b + 1, d + 1, n0ac)
if ((nn1j .ge. n0jl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakdj_ibkd(t2, &
 nocc, nactive, i, b, k, d)
j1 = min(i - 1, k - 1, n1jl)
j_aibjakdj_ibkd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakdj_ibkd
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdj_ibkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdj_ibkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakdj_ibkd
end do j_aibjakdj_ibkd
end do i_aibjakdj_ibkd
end do b_aibjakdj_ibkd
end do k_aibjakdj_ibkd
end do d_aibjakdj_ibkd
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k
! Equalities: c == a, l == j
! No equalities independent of the above can hold.
!
d_aibjakdj_ibjkd: do d = n0d, n1d
k_aibjakdj_ibjkd: do k = n0k, n1k
b_aibjakdj_ibjkd: do b = n0b, n1b
if (b == d) cycle b_aibjakdj_ibjkd
j1 = min(k - 1, n1jl)
j_aibjakdj_ibjkd: do j = n0jl, j1
if (j == k) cycle j_aibjakdj_ibjkd
i0 = max(j + 1, n0i)
i_aibjakdj_ibjkd: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjakdj_ibjkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakdj_ibjkd(t2, &
 nocc, nactive, i, b, j, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdj_ibjkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdj_ibjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakdj_ibjkd
end do i_aibjakdj_ibjkd
end do j_aibjakdj_ibjkd
end do b_aibjakdj_ibjkd
end do k_aibjakdj_ibjkd
end do d_aibjakdj_ibjkd
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k
! Equalities: c == a, l == j
! No equalities independent of the above can hold.
!
d_aibjakdj_aibkd: do d = n0d, n1d
k_aibjakdj_aibkd: do k = n0k, n1k
b_aibjakdj_aibkd: do b = n0b, n1b
if (b == d) cycle b_aibjakdj_aibkd
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdj_aibkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdj_aibkd
i_aibjakdj_aibkd: do i = n0i, n1i
if (i == k) cycle i_aibjakdj_aibkd
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakdj_aibkd(t2, &
 nocc, nactive, a, i, b, k, d)
j1 = min(i - 1, k - 1, n1jl)
j_aibjakdj_aibkd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakdj_aibkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakdj_aibkd
end do i_aibjakdj_aibkd
end do a_aibjakdj_aibkd
end do b_aibjakdj_aibkd
end do k_aibjakdj_aibkd
end do d_aibjakdj_aibkd
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l
! Equalities: c == a, k == j
! No equalities independent of the above can hold.
!
d_aibjajdl_ibdl: do d = n0d, n1d
l_aibjajdl_ibdl: do l = n0l, n1l
b_aibjajdl_ibdl: do b = n0b, n1b
if (b == d) cycle b_aibjajdl_ibdl
i_aibjajdl_ibdl: do i = n0i, n1i
if (i == l) cycle i_aibjajdl_ibdl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
nn0a = max(b + 1, d + 1, n0ac)
if ((nn1j .ge. nn0j).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajdl_ibdl(t2, &
 nocc, nactive, i, b, d, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjajdl_ibdl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjajdl_ibdl
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdl_ibdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdl_ibdl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdl_ibdl
end do j_aibjajdl_ibdl
end do i_aibjajdl_ibdl
end do b_aibjajdl_ibdl
end do l_aibjajdl_ibdl
end do d_aibjajdl_ibdl
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l
! Equalities: c == a, k == j
! No equalities independent of the above can hold.
!
d_aibjajdl_ibjdl: do d = n0d, n1d
l_aibjajdl_ibjdl: do l = n0l, n1l
b_aibjajdl_ibjdl: do b = n0b, n1b
if (b == d) cycle b_aibjajdl_ibjdl
j0 = max(l + 1, n0jk)
j_aibjajdl_ibjdl: do j = j0, n1jk
if (j == l) cycle j_aibjajdl_ibjdl
i0 = max(j + 1, n0i)
i_aibjajdl_ibjdl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjajdl_ibjdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajdl_ibjdl(t2, &
 nocc, nactive, i, b, j, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdl_ibjdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdl_ibjdl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdl_ibjdl
end do i_aibjajdl_ibjdl
end do j_aibjajdl_ibjdl
end do b_aibjajdl_ibjdl
end do l_aibjajdl_ibjdl
end do d_aibjajdl_ibjdl
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, l
! Equalities: c == a, k == j
! No equalities independent of the above can hold.
!
d_aibjajdl_aibdl: do d = n0d, n1d
l_aibjajdl_aibdl: do l = n0l, n1l
b_aibjajdl_aibdl: do b = n0b, n1b
if (b == d) cycle b_aibjajdl_aibdl
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdl_aibdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdl_aibdl
i_aibjajdl_aibdl: do i = n0i, n1i
if (i == l) cycle i_aibjajdl_aibdl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajdl_aibdl(t2, &
 nocc, nactive, a, i, b, d, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjajdl_aibdl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjajdl_aibdl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajdl_aibdl
end do i_aibjajdl_aibdl
end do a_aibjajdl_aibdl
end do b_aibjajdl_aibdl
end do l_aibjajdl_aibdl
end do d_aibjajdl_aibdl
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, l == i
! No equalities independent of the above can hold.
!
c_aibjckai_bjck: do c = n0c, n1c
k_aibjckai_bjck: do k = n0k, n1k
b_aibjckai_bjck: do b = n0b, n1b
if (b == c) cycle b_aibjckai_bjck
j_aibjckai_bjck: do j = n0j, n1j
if (j == k) cycle j_aibjckai_bjck
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1i .ge. nn0i).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckai_bjck(t2, &
 nocc, nactive, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckai_bjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckai_bjck
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjckai_bjck: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjckai_bjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckai_bjck
end do a_aibjckai_bjck
end do j_aibjckai_bjck
end do b_aibjckai_bjck
end do k_aibjckai_bjck
end do c_aibjckai_bjck
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, l == i
! No equalities independent of the above can hold.
!
c_aibjckai_ibjck: do c = n0c, n1c
k_aibjckai_ibjck: do k = n0k, n1k
b_aibjckai_ibjck: do b = n0b, n1b
if (b == c) cycle b_aibjckai_ibjck
j_aibjckai_ibjck: do j = n0j, n1j
if (j == k) cycle j_aibjckai_ibjck
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjckai_ibjck: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjckai_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckai_ibjck(t2, &
 nocc, nactive, i, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckai_ibjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckai_ibjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckai_ibjck
end do i_aibjckai_ibjck
end do j_aibjckai_ibjck
end do b_aibjckai_ibjck
end do k_aibjckai_ibjck
end do c_aibjckai_ibjck
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, l == i
! No equalities independent of the above can hold.
!
c_aibjckai_abjck: do c = n0c, n1c
k_aibjckai_abjck: do k = n0k, n1k
b_aibjckai_abjck: do b = n0b, n1b
if (b == c) cycle b_aibjckai_abjck
j_aibjckai_abjck: do j = n0j, n1j
if (j == k) cycle j_aibjckai_abjck
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckai_abjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckai_abjck
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckai_abjck(t2, &
 nocc, nactive, a, b, j, c, k)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjckai_abjck: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjckai_abjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckai_abjck
end do a_aibjckai_abjck
end do j_aibjckai_abjck
end do b_aibjckai_abjck
end do k_aibjckai_abjck
end do c_aibjckai_abjck
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjcial_bjcl: do l = n0l, n1l
c_aibjcial_bjcl: do c = n0c, n1c
b_aibjcial_bjcl: do b = n0b, n1b
if (b == c) cycle b_aibjcial_bjcl
j_aibjcial_bjcl: do j = n0j, n1j
if (j == l) cycle j_aibjcial_bjcl
nn0i = max(j + 1, l + 1, n0ik)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1ik .ge. nn0i).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcial_bjcl(t2, &
 nocc, nactive, b, j, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcial_bjcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcial_bjcl
i0 = max(j + 1, l + 1, n0ik)
i_aibjcial_bjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcial_bjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcial_bjcl
end do a_aibjcial_bjcl
end do j_aibjcial_bjcl
end do b_aibjcial_bjcl
end do c_aibjcial_bjcl
end do l_aibjcial_bjcl
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjcial_ibjcl: do l = n0l, n1l
c_aibjcial_ibjcl: do c = n0c, n1c
b_aibjcial_ibjcl: do b = n0b, n1b
if (b == c) cycle b_aibjcial_ibjcl
j_aibjcial_ibjcl: do j = n0j, n1j
if (j == l) cycle j_aibjcial_ibjcl
i0 = max(j + 1, l + 1, n0ik)
i_aibjcial_ibjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcial_ibjcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcial_ibjcl(t2, &
 nocc, nactive, i, b, j, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcial_ibjcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcial_ibjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcial_ibjcl
end do i_aibjcial_ibjcl
end do j_aibjcial_ibjcl
end do b_aibjcial_ibjcl
end do c_aibjcial_ibjcl
end do l_aibjcial_ibjcl
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjcial_abjcl: do l = n0l, n1l
c_aibjcial_abjcl: do c = n0c, n1c
b_aibjcial_abjcl: do b = n0b, n1b
if (b == c) cycle b_aibjcial_abjcl
j_aibjcial_abjcl: do j = n0j, n1j
if (j == l) cycle j_aibjcial_abjcl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcial_abjcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcial_abjcl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcial_abjcl(t2, &
 nocc, nactive, a, b, j, c, l)
i0 = max(j + 1, l + 1, n0ik)
i_aibjcial_abjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcial_abjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcial_abjcl
end do a_aibjcial_abjcl
end do j_aibjcial_abjcl
end do b_aibjcial_abjcl
end do c_aibjcial_abjcl
end do l_aibjcial_abjcl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, l == j
! No equalities independent of the above can hold.
!
c_aibjckaj_ibck: do c = n0c, n1c
k_aibjckaj_ibck: do k = n0k, n1k
b_aibjckaj_ibck: do b = n0b, n1b
if (b == c) cycle b_aibjckaj_ibck
i_aibjckaj_ibck: do i = n0i, n1i
if (i == k) cycle i_aibjckaj_ibck
nn1j = min(i - 1, k - 1, n1jl)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1j .ge. n0jl).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckaj_ibck(t2, &
 nocc, nactive, i, b, c, k)
j1 = min(i - 1, k - 1, n1jl)
j_aibjckaj_ibck: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckaj_ibck
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaj_ibck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaj_ibck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckaj_ibck
end do j_aibjckaj_ibck
end do i_aibjckaj_ibck
end do b_aibjckaj_ibck
end do k_aibjckaj_ibck
end do c_aibjckaj_ibck
end subroutine ccjac_22_tripletpp_dav_part1
end module ccjac_block_22_tripletpp_dav_part1
