module ccjac_block_22_tripletpm_dav_part2
use eom_ccsd_22_tripletpm_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-06 14:26:29 UTC.
!
contains
subroutine ccjac_22_tripletpm_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0b, nn0i, nn0j, nn0k, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b0, b1, i0, i1, j0, j1, k0
integer :: n0ac, n0acd, n0ad, n0bc, n0cd
integer :: n0ik, n0il, n0jk, n0jl, n0kl
integer :: n1ac, n1acd, n1ad, n1bc, n1cd
integer :: n1ik, n1il, n1jk, n1jl, n1kl
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
n0acd = max(n0a, n0c, n0d)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0cd = max(n0c, n0d)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1cd = min(n1c, n1d)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
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
nn1j = min(i - 1, n1jk)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1j .ge. n0jk).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbjdl_aidl(t2, &
 nocc, nactive, a, i, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdl_aidl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdl_aidl
j1 = min(i - 1, n1jk)
j_aibjbjdl_aidl: do j = n0jk, j1
if (j == i .or. j == l) cycle j_aibjbjdl_aidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdl_aidl
end do b_aibjbjdl_aidl
end do i_aibjbjdl_aidl
end do a_aibjbjdl_aidl
end do l_aibjbjdl_aidl
end do d_aibjbjdl_aidl
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, l
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdl_aijdl: do d = n0d, n1d
l_aibjbjdl_aijdl: do l = n0l, n1l
j_aibjbjdl_aijdl: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjdl_aijdl
a_aibjbjdl_aijdl: do a = n0a, n1a
if (a == d) cycle a_aibjbjdl_aijdl
i0 = max(j + 1, n0i)
i_aibjbjdl_aijdl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjbjdl_aijdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbjdl_aijdl(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdl_aijdl
end do i_aibjbjdl_aijdl
end do a_aibjbjdl_aijdl
end do j_aibjbjdl_aijdl
end do l_aibjbjdl_aijdl
end do d_aibjbjdl_aijdl
!
! Elementary loop  3
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
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbjdl_aibdl(t2, &
 nocc, nactive, a, i, b, d, l)
j1 = min(i - 1, n1jk)
j_aibjbjdl_aibdl: do j = n0jk, j1
if (j == i .or. j == l) cycle j_aibjbjdl_aibdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdl_aibdl
end do i_aibjbjdl_aibdl
end do a_aibjbjdl_aibdl
end do b_aibjbjdl_aibdl
end do l_aibjbjdl_aibdl
end do d_aibjbjdl_aibdl
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == a
! No equalities independent of the above can hold.
!
l_aibjakal_aibjkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibjakal_aibjkl: do k = k0, n1k
if (k == l) cycle k_aibjakal_aibjkl
b_aibjakal_aibjkl: do b = n0b, n1b
j_aibjakal_aibjkl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakal_aibjkl
a0 = max(b + 1, n0acd)
a_aibjakal_aibjkl: do a = a0, n1acd
if (a == b) cycle a_aibjakal_aibjkl
i0 = max(j + 1, n0i)
i_aibjakal_aibjkl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakal_aibjkl
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakal_aibjkl(t2, nocc, nactive, a, &
 i, b, j, k, l)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakal_aibjkl
end do a_aibjakal_aibjkl
end do j_aibjakal_aibjkl
end do b_aibjakal_aibjkl
end do k_aibjakal_aibjkl
end do l_aibjakal_aibjkl
!
! Elementary loop  5
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
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1il .ge. nn0i).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjckai_bjck(t2, &
 nocc, nactive, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckai_bjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckai_bjck
i0 = max(j + 1, n0il)
i_aibjckai_bjck: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckai_bjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckai_bjck
end do a_aibjckai_bjck
end do j_aibjckai_bjck
end do b_aibjckai_bjck
end do k_aibjckai_bjck
end do c_aibjckai_bjck
!
! Elementary loop  6
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
i_aibjckai_ibjck: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckai_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjckai_ibjck(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckai_ibjck
end do i_aibjckai_ibjck
end do j_aibjckai_ibjck
end do b_aibjckai_ibjck
end do k_aibjckai_ibjck
end do c_aibjckai_ibjck
!
! Elementary loop  7
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
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjckai_abjck(t2, &
 nocc, nactive, a, b, j, c, k)
i0 = max(j + 1, n0il)
i_aibjckai_abjck: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjckai_abjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckai_abjck
end do a_aibjckai_abjck
end do j_aibjckai_abjck
end do b_aibjckai_abjck
end do k_aibjckai_abjck
end do c_aibjckai_abjck
!
! Elementary loop  8
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
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcial_abjcl(t2, &
 nocc, nactive, a, b, j, c, l)
i0 = max(j + 1, n0ik)
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcial_abjcl
end do a_aibjcial_abjcl
end do j_aibjcial_abjcl
end do b_aibjcial_abjcl
end do c_aibjcial_abjcl
end do l_aibjcial_abjcl
!
! Elementary loop  9
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
i0 = max(j + 1, n0ik)
i_aibjcial_ibjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcial_ibjcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcial_ibjcl(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcial_ibjcl
end do i_aibjcial_ibjcl
end do j_aibjcial_ibjcl
end do b_aibjcial_ibjcl
end do c_aibjcial_ibjcl
end do l_aibjcial_ibjcl
!
! Elementary loop  10
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
nn1j = min(i - 1, n1jl)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1j .ge. n0jl).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjckaj_ibck(t2, &
 nocc, nactive, i, b, c, k)
j1 = min(i - 1, n1jl)
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckaj_ibck
end do j_aibjckaj_ibck
end do i_aibjckaj_ibck
end do b_aibjckaj_ibck
end do k_aibjckaj_ibck
end do c_aibjckaj_ibck
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, l == j
! No equalities independent of the above can hold.
!
c_aibjckaj_ibjck: do c = n0c, n1c
k_aibjckaj_ibjck: do k = n0k, n1k
b_aibjckaj_ibjck: do b = n0b, n1b
if (b == c) cycle b_aibjckaj_ibjck
j_aibjckaj_ibjck: do j = n0jl, n1jl
if (j == k) cycle j_aibjckaj_ibjck
i0 = max(j + 1, n0i)
i_aibjckaj_ibjck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjckaj_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjckaj_ibjck(t2, &
 nocc, nactive, i, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaj_ibjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaj_ibjck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckaj_ibjck
end do i_aibjckaj_ibjck
end do j_aibjckaj_ibjck
end do b_aibjckaj_ibjck
end do k_aibjckaj_ibjck
end do c_aibjckaj_ibjck
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, l == j
! No equalities independent of the above can hold.
!
c_aibjckaj_aibck: do c = n0c, n1c
k_aibjckaj_aibck: do k = n0k, n1k
b_aibjckaj_aibck: do b = n0b, n1b
if (b == c) cycle b_aibjckaj_aibck
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaj_aibck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaj_aibck
i_aibjckaj_aibck: do i = n0i, n1i
if (i == k) cycle i_aibjckaj_aibck
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjckaj_aibck(t2, &
 nocc, nactive, a, i, b, c, k)
j1 = min(i - 1, n1jl)
j_aibjckaj_aibck: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckaj_aibck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckaj_aibck
end do i_aibjckaj_aibck
end do a_aibjckaj_aibck
end do b_aibjckaj_aibck
end do k_aibjckaj_aibck
end do c_aibjckaj_aibck
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j
! Equalities: d == a, l == k
! No equalities independent of the above can hold.
!
c_aibjckak_ibjck: do c = n0c, n1c
k_aibjckak_ibjck: do k = n0kl, n1kl
b_aibjckak_ibjck: do b = n0b, n1b
if (b == c) cycle b_aibjckak_ibjck
j_aibjckak_ibjck: do j = n0j, n1j
if (j == k) cycle j_aibjckak_ibjck
i0 = max(j + 1, n0i)
i_aibjckak_ibjck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjckak_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjckak_ibjck(t2, &
 nocc, nactive, i, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckak_ibjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckak_ibjck
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckak_ibjck
end do i_aibjckak_ibjck
end do j_aibjckak_ibjck
end do b_aibjckak_ibjck
end do k_aibjckak_ibjck
end do c_aibjckak_ibjck
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjcjal_aibcl: do l = n0l, n1l
c_aibjcjal_aibcl: do c = n0c, n1c
b_aibjcjal_aibcl: do b = n0b, n1b
if (b == c) cycle b_aibjcjal_aibcl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjal_aibcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjal_aibcl
i_aibjcjal_aibcl: do i = n0i, n1i
if (i == l) cycle i_aibjcjal_aibcl
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjal_aibcl(t2, &
 nocc, nactive, a, i, b, c, l)
j1 = min(i - 1, n1jk)
j_aibjcjal_aibcl: do j = n0jk, j1
if (j == i .or. j == l) cycle j_aibjcjal_aibcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjal_aibcl
end do i_aibjcjal_aibcl
end do a_aibjcjal_aibcl
end do b_aibjcjal_aibcl
end do c_aibjcjal_aibcl
end do l_aibjcjal_aibcl
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjcjal_ibjcl: do l = n0l, n1l
c_aibjcjal_ibjcl: do c = n0c, n1c
b_aibjcjal_ibjcl: do b = n0b, n1b
if (b == c) cycle b_aibjcjal_ibjcl
j_aibjcjal_ibjcl: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjal_ibjcl
i0 = max(j + 1, n0i)
i_aibjcjal_ibjcl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjcjal_ibjcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjal_ibjcl(t2, &
 nocc, nactive, i, b, j, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjal_ibjcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjal_ibjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjal_ibjcl
end do i_aibjcjal_ibjcl
end do j_aibjcjal_ibjcl
end do b_aibjcjal_ibjcl
end do c_aibjcjal_ibjcl
end do l_aibjcjal_ibjcl
!
! Elementary loop  16
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, l == i
! No equalities independent of the above can hold.
!
c_aibjckci_abjck: do c = n0cd, n1cd
k_aibjckci_abjck: do k = n0k, n1k
b_aibjckci_abjck: do b = n0b, n1b
if (b == c) cycle b_aibjckci_abjck
j_aibjckci_abjck: do j = n0j, n1j
if (j == k) cycle j_aibjckci_abjck
a0 = max(b + 1, n0a)
a_aibjckci_abjck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckci_abjck
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjckci_abjck(t2, &
 nocc, nactive, a, b, j, c, k)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjckci_abjck: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjckci_abjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckci_abjck
end do a_aibjckci_abjck
end do j_aibjckci_abjck
end do b_aibjckci_abjck
end do k_aibjckci_abjck
end do c_aibjckci_abjck
!
! Elementary loop  17
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, l
! Equalities: d == c, k == i
! No equalities independent of the above can hold.
!
l_aibjcicl_abjcl: do l = n0l, n1l
c_aibjcicl_abjcl: do c = n0cd, n1cd
b_aibjcicl_abjcl: do b = n0b, n1b
if (b == c) cycle b_aibjcicl_abjcl
j_aibjcicl_abjcl: do j = n0j, n1j
if (j == l) cycle j_aibjcicl_abjcl
a0 = max(b + 1, n0a)
a_aibjcicl_abjcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicl_abjcl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcicl_abjcl(t2, &
 nocc, nactive, a, b, j, c, l)
i0 = max(j + 1, l + 1, n0ik)
i_aibjcicl_abjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcicl_abjcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicl_abjcl
end do a_aibjcicl_abjcl
end do j_aibjcicl_abjcl
end do b_aibjcicl_abjcl
end do c_aibjcicl_abjcl
end do l_aibjcicl_abjcl
!
! Elementary loop  18
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i, k
! Equalities: d == c, l == j
! No equalities independent of the above can hold.
!
c_aibjckcj_aibck: do c = n0cd, n1cd
k_aibjckcj_aibck: do k = n0k, n1k
b_aibjckcj_aibck: do b = n0b, n1b
if (b == c) cycle b_aibjckcj_aibck
a0 = max(b + 1, n0a)
a_aibjckcj_aibck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckcj_aibck
i_aibjckcj_aibck: do i = n0i, n1i
if (i == k) cycle i_aibjckcj_aibck
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjckcj_aibck(t2, &
 nocc, nactive, a, i, b, c, k)
j1 = min(i - 1, k - 1, n1jl)
j_aibjckcj_aibck: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckcj_aibck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckcj_aibck
end do i_aibjckcj_aibck
end do a_aibjckcj_aibck
end do b_aibjckcj_aibck
end do k_aibjckcj_aibck
end do c_aibjckcj_aibck
!
! Elementary loop  19
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i, l
! Equalities: d == c, k == j
! No equalities independent of the above can hold.
!
l_aibjcjcl_aibcl: do l = n0l, n1l
c_aibjcjcl_aibcl: do c = n0cd, n1cd
b_aibjcjcl_aibcl: do b = n0b, n1b
if (b == c) cycle b_aibjcjcl_aibcl
a0 = max(b + 1, n0a)
a_aibjcjcl_aibcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcl_aibcl
i_aibjcjcl_aibcl: do i = n0i, n1i
if (i == l) cycle i_aibjcjcl_aibcl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjcl_aibcl(t2, &
 nocc, nactive, a, i, b, c, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjcjcl_aibcl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjcjcl_aibcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjcl_aibcl
end do i_aibjcjcl_aibcl
end do a_aibjcjcl_aibcl
end do b_aibjcjcl_aibcl
end do c_aibjcjcl_aibcl
end do l_aibjcjcl_aibcl
!
! Elementary loop  20
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
i_aibjakdi_ibjkd: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakdi_ibjkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakdi_ibjkd(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakdi_ibjkd
end do i_aibjakdi_ibjkd
end do j_aibjakdi_ibjkd
end do b_aibjakdi_ibjkd
end do k_aibjakdi_ibjkd
end do d_aibjakdi_ibjkd
!
! Elementary loop  21
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
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakdi_abjkd(t2, &
 nocc, nactive, a, b, j, k, d)
i0 = max(j + 1, n0il)
i_aibjakdi_abjkd: do i = i0, n1il
if (i == j .or. i == k) cycle i_aibjakdi_abjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakdi_abjkd
end do a_aibjakdi_abjkd
end do j_aibjakdi_abjkd
end do b_aibjakdi_abjkd
end do k_aibjakdi_abjkd
end do d_aibjakdi_abjkd
!
! Elementary loop  22
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
nn0i = max(j + 1, n0ik)
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaidl_bjdl(t2, &
 nocc, nactive, b, j, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidl_bjdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidl_bjdl
i0 = max(j + 1, n0ik)
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidl_bjdl
end do a_aibjaidl_bjdl
end do j_aibjaidl_bjdl
end do b_aibjaidl_bjdl
end do l_aibjaidl_bjdl
end do d_aibjaidl_bjdl
!
! Elementary loop  23
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
i0 = max(j + 1, n0ik)
i_aibjaidl_ibjdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidl_ibjdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaidl_ibjdl(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidl_ibjdl
end do i_aibjaidl_ibjdl
end do j_aibjaidl_ibjdl
end do b_aibjaidl_ibjdl
end do l_aibjaidl_ibjdl
end do d_aibjaidl_ibjdl
!
! Elementary loop  24
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
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaidl_abjdl(t2, &
 nocc, nactive, a, b, j, d, l)
i0 = max(j + 1, n0ik)
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidl_abjdl
end do a_aibjaidl_abjdl
end do j_aibjaidl_abjdl
end do b_aibjaidl_abjdl
end do l_aibjaidl_abjdl
end do d_aibjaidl_abjdl
!
! Elementary loop  25
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
j_aibjakdj_ibjkd: do j = n0jl, n1jl
if (j == k) cycle j_aibjakdj_ibjkd
i0 = max(j + 1, n0i)
i_aibjakdj_ibjkd: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjakdj_ibjkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakdj_ibjkd(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakdj_ibjkd
end do i_aibjakdj_ibjkd
end do j_aibjakdj_ibjkd
end do b_aibjakdj_ibjkd
end do k_aibjakdj_ibjkd
end do d_aibjakdj_ibjkd
!
! Elementary loop  26
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
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakdj_aibkd(t2, &
 nocc, nactive, a, i, b, k, d)
j1 = min(i - 1, n1jl)
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakdj_aibkd
end do i_aibjakdj_aibkd
end do a_aibjakdj_aibkd
end do b_aibjakdj_aibkd
end do k_aibjakdj_aibkd
end do d_aibjakdj_aibkd
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: k, i, j
! Equalities: c == a, l == k
! No equalities independent of the above can hold.
!
d_aibjakdk_ibjkd: do d = n0d, n1d
k_aibjakdk_ibjkd: do k = n0kl, n1kl
b_aibjakdk_ibjkd: do b = n0b, n1b
if (b == d) cycle b_aibjakdk_ibjkd
j_aibjakdk_ibjkd: do j = n0j, n1j
if (j == k) cycle j_aibjakdk_ibjkd
i0 = max(j + 1, n0i)
i_aibjakdk_ibjkd: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjakdk_ibjkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakdk_ibjkd(t2, &
 nocc, nactive, i, b, j, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdk_ibjkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdk_ibjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakdk_ibjkd
end do i_aibjakdk_ibjkd
end do j_aibjakdk_ibjkd
end do b_aibjakdk_ibjkd
end do k_aibjakdk_ibjkd
end do d_aibjakdk_ibjkd
!
! Elementary loop  28
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
nn1j = min(i - 1, n1jk)
nn0a = max(b + 1, d + 1, n0ac)
if ((nn1j .ge. n0jk).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajdl_ibdl(t2, &
 nocc, nactive, i, b, d, l)
j1 = min(i - 1, n1jk)
j_aibjajdl_ibdl: do j = n0jk, j1
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdl_ibdl
end do j_aibjajdl_ibdl
end do i_aibjajdl_ibdl
end do b_aibjajdl_ibdl
end do l_aibjajdl_ibdl
end do d_aibjajdl_ibdl
!
! Elementary loop  29
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
j_aibjajdl_ibjdl: do j = n0jk, n1jk
if (j == l) cycle j_aibjajdl_ibjdl
i0 = max(j + 1, n0i)
i_aibjajdl_ibjdl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjajdl_ibjdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajdl_ibjdl(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdl_ibjdl
end do i_aibjajdl_ibjdl
end do j_aibjajdl_ibjdl
end do b_aibjajdl_ibjdl
end do l_aibjajdl_ibjdl
end do d_aibjajdl_ibjdl
!
! Elementary loop  30
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
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajdl_aibdl(t2, &
 nocc, nactive, a, i, b, d, l)
j1 = min(i - 1, n1jk)
j_aibjajdl_aibdl: do j = n0jk, j1
if (j == i .or. j == l) cycle j_aibjajdl_aibdl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajdl_aibdl
end do i_aibjajdl_aibdl
end do a_aibjajdl_aibdl
end do b_aibjajdl_aibdl
end do l_aibjajdl_aibdl
end do d_aibjajdl_aibdl
end subroutine ccjac_22_tripletpm_dav_part2
end module ccjac_block_22_tripletpm_dav_part2
