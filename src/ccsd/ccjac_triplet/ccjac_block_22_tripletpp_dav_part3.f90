module ccjac_block_22_tripletpp_dav_part3
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
subroutine ccjac_22_tripletpp_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0c, nn0i, nn0j, nn1b, nn1i, nn1j
integer :: a0, b1, c0, i0, i1, j0, j1
integer :: n0ac, n0bd, n0ik, n0il, n0jk
integer :: n0jl
integer :: n1ac, n1bd, n1ik, n1il, n1jk
integer :: n1jl
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
n0bd = max(n0b, n0d)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1ac = min(n1a, n1c)
n1bd = min(n1b, n1d)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcidj_aibcd(t2, &
 nocc, nactive, a, i, b, c, d)
j1 = min(i - 1, n1jl)
j_aibjcidj_aibcd: do j = n0jl, j1
if (j == i) cycle j_aibjcidj_aibcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcidj_abjcd(t2, &
 nocc, nactive, a, b, j, c, d)
i0 = max(j + 1, n0ik)
i_aibjcidj_abjcd: do i = i0, n1ik
if (i == j) cycle i_aibjcidj_abjcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_jk: do k = n0k, n1k
j_aibjakbi_jk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_jk
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (nn1i .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbi_jk(t2, &
 nocc, nactive, j, k)
b_aibjakbi_jk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbi_jk: do a = a0, n1ac
if (a == b) cycle a_aibjakbi_jk
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakbi_jk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakbi_jk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbi_jk
end do a_aibjakbi_jk
end do b_aibjakbi_jk
end do j_aibjakbi_jk
end do k_aibjakbi_jk
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_ajk: do k = n0k, n1k
j_aibjakbi_ajk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_ajk
a_aibjakbi_ajk: do a = n0ac, n1ac
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd).and. (nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbi_ajk(t2, &
 nocc, nactive, a, j, k)
b1 = min(a - 1, n1bd)
b_aibjakbi_ajk: do b = n0bd, b1
if (b == a) cycle b_aibjakbi_ajk
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakbi_ajk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakbi_ajk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbi_ajk
end do b_aibjakbi_ajk
end do a_aibjakbi_ajk
end do j_aibjakbi_ajk
end do k_aibjakbi_ajk
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_bjk: do k = n0k, n1k
b_aibjakbi_bjk: do b = n0bd, n1bd
j_aibjakbi_bjk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_bjk
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
nn0a = max(b + 1, n0ac)
if ((nn1i .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbi_bjk(t2, &
 nocc, nactive, b, j, k)
a0 = max(b + 1, n0ac)
a_aibjakbi_bjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbi_bjk
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakbi_bjk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakbi_bjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbi_bjk
end do a_aibjakbi_bjk
end do j_aibjakbi_bjk
end do b_aibjakbi_bjk
end do k_aibjakbi_bjk
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_ijk: do k = n0k, n1k
j_aibjakbi_ijk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_ijk
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakbi_ijk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakbi_ijk
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbi_ijk(t2, &
 nocc, nactive, i, j, k)
b_aibjakbi_ijk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbi_ijk: do a = a0, n1ac
if (a == b) cycle a_aibjakbi_ijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbi_ijk
end do b_aibjakbi_ijk
end do i_aibjakbi_ijk
end do j_aibjakbi_ijk
end do k_aibjakbi_ijk
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_aijk: do k = n0k, n1k
j_aibjakbi_aijk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_aijk
a_aibjakbi_aijk: do a = n0ac, n1ac
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakbi_aijk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakbi_aijk
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbi_aijk(t2, &
 nocc, nactive, a, i, j, k)
b1 = min(a - 1, n1bd)
b_aibjakbi_aijk: do b = n0bd, b1
if (b == a) cycle b_aibjakbi_aijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjakbi_aijk
end do i_aibjakbi_aijk
end do a_aibjakbi_aijk
end do j_aibjakbi_aijk
end do k_aibjakbi_aijk
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_ibjk: do k = n0k, n1k
b_aibjakbi_ibjk: do b = n0bd, n1bd
j_aibjakbi_ibjk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_ibjk
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakbi_ibjk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakbi_ibjk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbi_ibjk(t2, &
 nocc, nactive, i, b, j, k)
a0 = max(b + 1, n0ac)
a_aibjakbi_ibjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbi_ibjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbi_ibjk
end do i_aibjakbi_ibjk
end do j_aibjakbi_ibjk
end do b_aibjakbi_ibjk
end do k_aibjakbi_ibjk
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_abjk: do k = n0k, n1k
b_aibjakbi_abjk: do b = n0bd, n1bd
j_aibjakbi_abjk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_abjk
a0 = max(b + 1, n0ac)
a_aibjakbi_abjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbi_abjk
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbi_abjk(t2, &
 nocc, nactive, a, b, j, k)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakbi_abjk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakbi_abjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbi_abjk
end do a_aibjakbi_abjk
end do j_aibjakbi_abjk
end do b_aibjakbi_abjk
end do k_aibjakbi_abjk
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_jl: do l = n0l, n1l
j_aibjaibl_jl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_jl
nn0i = max(j + 1, l + 1, n0ik)
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibl_jl(t2, &
 nocc, nactive, j, l)
b_aibjaibl_jl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibl_jl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_jl
i0 = max(j + 1, l + 1, n0ik)
i_aibjaibl_jl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_jl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_jl
end do a_aibjaibl_jl
end do b_aibjaibl_jl
end do j_aibjaibl_jl
end do l_aibjaibl_jl
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_ajl: do l = n0l, n1l
j_aibjaibl_ajl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_ajl
a_aibjaibl_ajl: do a = n0ac, n1ac
nn0i = max(j + 1, l + 1, n0ik)
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibl_ajl(t2, &
 nocc, nactive, a, j, l)
b1 = min(a - 1, n1bd)
b_aibjaibl_ajl: do b = n0bd, b1
if (b == a) cycle b_aibjaibl_ajl
i0 = max(j + 1, l + 1, n0ik)
i_aibjaibl_ajl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_ajl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_ajl
end do b_aibjaibl_ajl
end do a_aibjaibl_ajl
end do j_aibjaibl_ajl
end do l_aibjaibl_ajl
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_bjl: do l = n0l, n1l
b_aibjaibl_bjl: do b = n0bd, n1bd
j_aibjaibl_bjl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_bjl
nn0i = max(j + 1, l + 1, n0ik)
nn0a = max(b + 1, n0ac)
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibl_bjl(t2, &
 nocc, nactive, b, j, l)
a0 = max(b + 1, n0ac)
a_aibjaibl_bjl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_bjl
i0 = max(j + 1, l + 1, n0ik)
i_aibjaibl_bjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_bjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_bjl
end do a_aibjaibl_bjl
end do j_aibjaibl_bjl
end do b_aibjaibl_bjl
end do l_aibjaibl_bjl
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_ijl: do l = n0l, n1l
j_aibjaibl_ijl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_ijl
i0 = max(j + 1, l + 1, n0ik)
i_aibjaibl_ijl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_ijl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibl_ijl(t2, &
 nocc, nactive, i, j, l)
b_aibjaibl_ijl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibl_ijl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_ijl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaibl_ijl
end do b_aibjaibl_ijl
end do i_aibjaibl_ijl
end do j_aibjaibl_ijl
end do l_aibjaibl_ijl
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_aijl: do l = n0l, n1l
j_aibjaibl_aijl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_aijl
a_aibjaibl_aijl: do a = n0ac, n1ac
i0 = max(j + 1, l + 1, n0ik)
i_aibjaibl_aijl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_aijl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibl_aijl(t2, &
 nocc, nactive, a, i, j, l)
b1 = min(a - 1, n1bd)
b_aibjaibl_aijl: do b = n0bd, b1
if (b == a) cycle b_aibjaibl_aijl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjaibl_aijl
end do i_aibjaibl_aijl
end do a_aibjaibl_aijl
end do j_aibjaibl_aijl
end do l_aibjaibl_aijl
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_ibjl: do l = n0l, n1l
b_aibjaibl_ibjl: do b = n0bd, n1bd
j_aibjaibl_ibjl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_ibjl
i0 = max(j + 1, l + 1, n0ik)
i_aibjaibl_ibjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_ibjl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibl_ibjl(t2, &
 nocc, nactive, i, b, j, l)
a0 = max(b + 1, n0ac)
a_aibjaibl_ibjl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_ibjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaibl_ibjl
end do i_aibjaibl_ibjl
end do j_aibjaibl_ibjl
end do b_aibjaibl_ibjl
end do l_aibjaibl_ibjl
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_abjl: do l = n0l, n1l
b_aibjaibl_abjl: do b = n0bd, n1bd
j_aibjaibl_abjl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_abjl
a0 = max(b + 1, n0ac)
a_aibjaibl_abjl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_abjl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibl_abjl(t2, &
 nocc, nactive, a, b, j, l)
i0 = max(j + 1, l + 1, n0ik)
i_aibjaibl_abjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_abjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_abjl
end do a_aibjaibl_abjl
end do j_aibjaibl_abjl
end do b_aibjaibl_abjl
end do l_aibjaibl_abjl
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_ik: do k = n0k, n1k
i_aibjakbj_ik: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_ik
nn1j = min(i - 1, k - 1, n1jl)
nn0a = max(n0bd + 1, n0ac)
if ((nn1j .ge. n0jl).and. (n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbj_ik(t2, &
 nocc, nactive, i, k)
b_aibjakbj_ik: do b = n0bd, n1bd
j1 = min(i - 1, k - 1, n1jl)
j_aibjakbj_ik: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakbj_ik
a0 = max(b + 1, n0ac)
a_aibjakbj_ik: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_ik
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbj_ik
end do j_aibjakbj_ik
end do b_aibjakbj_ik
end do i_aibjakbj_ik
end do k_aibjakbj_ik
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_aik: do k = n0k, n1k
a_aibjakbj_aik: do a = n0ac, n1ac
i_aibjakbj_aik: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_aik
nn1j = min(i - 1, k - 1, n1jl)
nn1b = min(a - 1, n1bd)
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbj_aik(t2, &
 nocc, nactive, a, i, k)
b1 = min(a - 1, n1bd)
b_aibjakbj_aik: do b = n0bd, b1
if (b == a) cycle b_aibjakbj_aik
j1 = min(i - 1, k - 1, n1jl)
j_aibjakbj_aik: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakbj_aik
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakbj_aik
end do b_aibjakbj_aik
end do i_aibjakbj_aik
end do a_aibjakbj_aik
end do k_aibjakbj_aik
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_ibk: do k = n0k, n1k
b_aibjakbj_ibk: do b = n0bd, n1bd
i_aibjakbj_ibk: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_ibk
nn1j = min(i - 1, k - 1, n1jl)
nn0a = max(b + 1, n0ac)
if ((nn1j .ge. n0jl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbj_ibk(t2, &
 nocc, nactive, i, b, k)
j1 = min(i - 1, k - 1, n1jl)
j_aibjakbj_ibk: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakbj_ibk
a0 = max(b + 1, n0ac)
a_aibjakbj_ibk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_ibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbj_ibk
end do j_aibjakbj_ibk
end do i_aibjakbj_ibk
end do b_aibjakbj_ibk
end do k_aibjakbj_ibk
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_ijk: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aibjakbj_ijk: do j = n0jl, j1
if (j == k) cycle j_aibjakbj_ijk
i0 = max(j + 1, n0i)
i_aibjakbj_ijk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjakbj_ijk
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbj_ijk(t2, &
 nocc, nactive, i, j, k)
b_aibjakbj_ijk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbj_ijk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_ijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbj_ijk
end do b_aibjakbj_ijk
end do i_aibjakbj_ijk
end do j_aibjakbj_ijk
end do k_aibjakbj_ijk
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_aijk: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aibjakbj_aijk: do j = n0jl, j1
if (j == k) cycle j_aibjakbj_aijk
a_aibjakbj_aijk: do a = n0ac, n1ac
i0 = max(j + 1, n0i)
i_aibjakbj_aijk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjakbj_aijk
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbj_aijk(t2, &
 nocc, nactive, a, i, j, k)
b1 = min(a - 1, n1bd)
b_aibjakbj_aijk: do b = n0bd, b1
if (b == a) cycle b_aibjakbj_aijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjakbj_aijk
end do i_aibjakbj_aijk
end do a_aibjakbj_aijk
end do j_aibjakbj_aijk
end do k_aibjakbj_aijk
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_ibjk: do k = n0k, n1k
b_aibjakbj_ibjk: do b = n0bd, n1bd
j1 = min(k - 1, n1jl)
j_aibjakbj_ibjk: do j = n0jl, j1
if (j == k) cycle j_aibjakbj_ibjk
i0 = max(j + 1, n0i)
i_aibjakbj_ibjk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjakbj_ibjk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbj_ibjk(t2, &
 nocc, nactive, i, b, j, k)
a0 = max(b + 1, n0ac)
a_aibjakbj_ibjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_ibjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbj_ibjk
end do i_aibjakbj_ibjk
end do j_aibjakbj_ibjk
end do b_aibjakbj_ibjk
end do k_aibjakbj_ibjk
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_aibk: do k = n0k, n1k
b_aibjakbj_aibk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbj_aibk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_aibk
i_aibjakbj_aibk: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_aibk
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjakbj_aibk(t2, &
 nocc, nactive, a, i, b, k)
j1 = min(i - 1, k - 1, n1jl)
j_aibjakbj_aibk: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakbj_aibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakbj_aibk
end do i_aibjakbj_aibk
end do a_aibjakbj_aibk
end do b_aibjakbj_aibk
end do k_aibjakbj_aibk
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_il: do l = n0l, n1l
i_aibjajbl_il: do i = n0i, n1i
if (i == l) cycle i_aibjajbl_il
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
nn0a = max(n0bd + 1, n0ac)
if ((nn1j .ge. nn0j).and. (n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajbl_il(t2, &
 nocc, nactive, i, l)
b_aibjajbl_il: do b = n0bd, n1bd
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjajbl_il: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjajbl_il
a0 = max(b + 1, n0ac)
a_aibjajbl_il: do a = a0, n1ac
if (a == b) cycle a_aibjajbl_il
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbl_il
end do j_aibjajbl_il
end do b_aibjajbl_il
end do i_aibjajbl_il
end do l_aibjajbl_il
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_ail: do l = n0l, n1l
a_aibjajbl_ail: do a = n0ac, n1ac
i_aibjajbl_ail: do i = n0i, n1i
if (i == l) cycle i_aibjajbl_ail
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
nn1b = min(a - 1, n1bd)
if ((nn1j .ge. nn0j).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajbl_ail(t2, &
 nocc, nactive, a, i, l)
b1 = min(a - 1, n1bd)
b_aibjajbl_ail: do b = n0bd, b1
if (b == a) cycle b_aibjajbl_ail
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjajbl_ail: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjajbl_ail
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbl_ail
end do b_aibjajbl_ail
end do i_aibjajbl_ail
end do a_aibjajbl_ail
end do l_aibjajbl_ail
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_ibl: do l = n0l, n1l
b_aibjajbl_ibl: do b = n0bd, n1bd
i_aibjajbl_ibl: do i = n0i, n1i
if (i == l) cycle i_aibjajbl_ibl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
nn0a = max(b + 1, n0ac)
if ((nn1j .ge. nn0j).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajbl_ibl(t2, &
 nocc, nactive, i, b, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjajbl_ibl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjajbl_ibl
a0 = max(b + 1, n0ac)
a_aibjajbl_ibl: do a = a0, n1ac
if (a == b) cycle a_aibjajbl_ibl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbl_ibl
end do j_aibjajbl_ibl
end do i_aibjajbl_ibl
end do b_aibjajbl_ibl
end do l_aibjajbl_ibl
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_ijl: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aibjajbl_ijl: do j = j0, n1jk
if (j == l) cycle j_aibjajbl_ijl
i0 = max(j + 1, n0i)
i_aibjajbl_ijl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjajbl_ijl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajbl_ijl(t2, &
 nocc, nactive, i, j, l)
b_aibjajbl_ijl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjajbl_ijl: do a = a0, n1ac
if (a == b) cycle a_aibjajbl_ijl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbl_ijl
end do b_aibjajbl_ijl
end do i_aibjajbl_ijl
end do j_aibjajbl_ijl
end do l_aibjajbl_ijl
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_aijl: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aibjajbl_aijl: do j = j0, n1jk
if (j == l) cycle j_aibjajbl_aijl
a_aibjajbl_aijl: do a = n0ac, n1ac
i0 = max(j + 1, n0i)
i_aibjajbl_aijl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjajbl_aijl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajbl_aijl(t2, &
 nocc, nactive, a, i, j, l)
b1 = min(a - 1, n1bd)
b_aibjajbl_aijl: do b = n0bd, b1
if (b == a) cycle b_aibjajbl_aijl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjajbl_aijl
end do i_aibjajbl_aijl
end do a_aibjajbl_aijl
end do j_aibjajbl_aijl
end do l_aibjajbl_aijl
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_ibjl: do l = n0l, n1l
b_aibjajbl_ibjl: do b = n0bd, n1bd
j0 = max(l + 1, n0jk)
j_aibjajbl_ibjl: do j = j0, n1jk
if (j == l) cycle j_aibjajbl_ibjl
i0 = max(j + 1, n0i)
i_aibjajbl_ibjl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjajbl_ibjl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajbl_ibjl(t2, &
 nocc, nactive, i, b, j, l)
a0 = max(b + 1, n0ac)
a_aibjajbl_ibjl: do a = a0, n1ac
if (a == b) cycle a_aibjajbl_ibjl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbl_ibjl
end do i_aibjajbl_ibjl
end do j_aibjajbl_ibjl
end do b_aibjajbl_ibjl
end do l_aibjajbl_ibjl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_aibl: do l = n0l, n1l
b_aibjajbl_aibl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjajbl_aibl: do a = a0, n1ac
if (a == b) cycle a_aibjajbl_aibl
i_aibjajbl_aibl: do i = n0i, n1i
if (i == l) cycle i_aibjajbl_aibl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjajbl_aibl(t2, &
 nocc, nactive, a, i, b, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjajbl_aibl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjajbl_aibl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbl_aibl
end do i_aibjajbl_aibl
end do a_aibjajbl_aibl
end do b_aibjajbl_aibl
end do l_aibjajbl_aibl
end subroutine ccjac_22_tripletpp_dav_part3
end module ccjac_block_22_tripletpp_dav_part3
