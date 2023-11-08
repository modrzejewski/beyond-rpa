module ccjac_block_22_tripletmp_dav_part2
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
subroutine ccjac_22_tripletmp_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0i, nn0j, nn0k, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b1, i0, i1, j0, j1, k0
integer :: n0ac, n0ad, n0bd, n0ij, n0ik
integer :: n0il, n0jk, n0jl
integer :: n1ac, n1ad, n1bd, n1ij, n1ik
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
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1bd = min(n1b, n1d)
n1ij = min(n1i, n1j)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k, l
! Equalities: d == b, j == i
! No equalities independent of the above can hold.
!
l_aibickbl_aickl: do l = n0l, n1l
c_aibickbl_aickl: do c = n0c, n1c
k0 = max(l + 1, n0k)
k_aibickbl_aickl: do k = k0, n1k
if (k == l) cycle k_aibickbl_aickl
a_aibickbl_aickl: do a = n0a, n1a
if (a == c) cycle a_aibickbl_aickl
i_aibickbl_aickl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibickbl_aickl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibickbl_aickl(t2, &
 nocc, nactive, a, i, c, k, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibickbl_aickl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibickbl_aickl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibickbl_aickl
end do i_aibickbl_aickl
end do a_aibickbl_aickl
end do k_aibickbl_aickl
end do c_aibickbl_aickl
end do l_aibickbl_aickl
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl_aicl: do l = n0l, n1l
c_aibjcjbl_aicl: do c = n0c, n1c
a_aibjcjbl_aicl: do a = n0a, n1a
if (a == c) cycle a_aibjcjbl_aicl
i_aibjcjbl_aicl: do i = n0i, n1i
if (i == l) cycle i_aibjcjbl_aicl
nn0j = max(l + 1, n0jk)
nn1b = min(a - 1, c - 1, n1bd)
if ((n1jk .ge. nn0j).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjbl_aicl(t2, &
 nocc, nactive, a, i, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbl_aicl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbl_aicl
j0 = max(l + 1, n0jk)
j_aibjcjbl_aicl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjcjbl_aicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbl_aicl
end do b_aibjcjbl_aicl
end do i_aibjcjbl_aicl
end do a_aibjcjbl_aicl
end do c_aibjcjbl_aicl
end do l_aibjcjbl_aicl
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl_aijcl: do l = n0l, n1l
c_aibjcjbl_aijcl: do c = n0c, n1c
j0 = max(l + 1, n0jk)
j_aibjcjbl_aijcl: do j = j0, n1jk
if (j == l) cycle j_aibjcjbl_aijcl
a_aibjcjbl_aijcl: do a = n0a, n1a
if (a == c) cycle a_aibjcjbl_aijcl
i_aibjcjbl_aijcl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjbl_aijcl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjbl_aijcl(t2, &
 nocc, nactive, a, i, j, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbl_aijcl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbl_aijcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbl_aijcl
end do i_aibjcjbl_aijcl
end do a_aibjcjbl_aijcl
end do j_aibjcjbl_aijcl
end do c_aibjcjbl_aijcl
end do l_aibjcjbl_aijcl
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl_aibcl: do l = n0l, n1l
c_aibjcjbl_aibcl: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbl_aibcl: do b = n0bd, b1
if (b == c) cycle b_aibjcjbl_aibcl
a0 = max(b + 1, n0a)
a_aibjcjbl_aibcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbl_aibcl
i_aibjcjbl_aibcl: do i = n0i, n1i
if (i == l) cycle i_aibjcjbl_aibcl
nn0j = max(l + 1, n0jk)
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjbl_aibcl(t2, &
 nocc, nactive, a, i, b, c, l)
j0 = max(l + 1, n0jk)
j_aibjcjbl_aibcl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjcjbl_aibcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbl_aibcl
end do i_aibjcjbl_aibcl
end do a_aibjcjbl_aibcl
end do b_aibjcjbl_aibcl
end do c_aibjcjbl_aibcl
end do l_aibjcjbl_aibcl
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, l == i
! No equalities independent of the above can hold.
!
c_aibjckbi_abjck: do c = n0c, n1c
k_aibjckbi_abjck: do k = n0k, n1k
b1 = min(c - 1, n1bd)
b_aibjckbi_abjck: do b = n0bd, b1
if (b == c) cycle b_aibjckbi_abjck
j_aibjckbi_abjck: do j = n0j, n1j
if (j == k) cycle j_aibjckbi_abjck
a0 = max(b + 1, n0a)
a_aibjckbi_abjck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbi_abjck
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckbi_abjck(t2, &
 nocc, nactive, a, b, j, c, k)
i1 = min(k - 1, n1il)
i_aibjckbi_abjck: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjckbi_abjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbi_abjck
end do a_aibjckbi_abjck
end do j_aibjckbi_abjck
end do b_aibjckbi_abjck
end do k_aibjckbi_abjck
end do c_aibjckbi_abjck
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, l == i
! No equalities independent of the above can hold.
!
c_aibjckbi_aijck: do c = n0c, n1c
k_aibjckbi_aijck: do k = n0k, n1k
j_aibjckbi_aijck: do j = n0j, n1j
if (j == k) cycle j_aibjckbi_aijck
a_aibjckbi_aijck: do a = n0a, n1a
if (a == c) cycle a_aibjckbi_aijck
i1 = min(k - 1, n1il)
i_aibjckbi_aijck: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjckbi_aijck
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckbi_aijck(t2, &
 nocc, nactive, a, i, j, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbi_aijck: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbi_aijck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjckbi_aijck
end do i_aibjckbi_aijck
end do a_aibjckbi_aijck
end do j_aibjckbi_aijck
end do k_aibjckbi_aijck
end do c_aibjckbi_aijck
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl_abjcl: do l = n0l, n1l
c_aibjcibl_abjcl: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibl_abjcl: do b = n0bd, b1
if (b == c) cycle b_aibjcibl_abjcl
j_aibjcibl_abjcl: do j = n0j, n1j
if (j == l) cycle j_aibjcibl_abjcl
a0 = max(b + 1, n0a)
a_aibjcibl_abjcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibl_abjcl
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcibl_abjcl(t2, &
 nocc, nactive, a, b, j, c, l)
i0 = max(l + 1, n0ik)
i_aibjcibl_abjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcibl_abjcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibl_abjcl
end do a_aibjcibl_abjcl
end do j_aibjcibl_abjcl
end do b_aibjcibl_abjcl
end do c_aibjcibl_abjcl
end do l_aibjcibl_abjcl
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl_aijcl: do l = n0l, n1l
c_aibjcibl_aijcl: do c = n0c, n1c
j_aibjcibl_aijcl: do j = n0j, n1j
if (j == l) cycle j_aibjcibl_aijcl
a_aibjcibl_aijcl: do a = n0a, n1a
if (a == c) cycle a_aibjcibl_aijcl
i0 = max(l + 1, n0ik)
i_aibjcibl_aijcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcibl_aijcl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcibl_aijcl(t2, &
 nocc, nactive, a, i, j, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibl_aijcl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibl_aijcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibl_aijcl
end do i_aibjcibl_aijcl
end do a_aibjcibl_aijcl
end do j_aibjcibl_aijcl
end do c_aibjcibl_aijcl
end do l_aibjcibl_aijcl
!
! Elementary loop  9
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
i_aibjakdj_ibjkd: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdj_ibjkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakdj_ibjkd(t2, &
 nocc, nactive, i, b, j, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdj_ibjkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdj_ibjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  10
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
nn1j = min(k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakdj_aibkd(t2, &
 nocc, nactive, a, i, b, k, d)
j1 = min(k - 1, n1jl)
j_aibjakdj_aibkd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakdj_aibkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k, l
! Equalities: c == a, j == i
! No equalities independent of the above can hold.
!
d_aibiakdl_ibkdl: do d = n0d, n1d
l_aibiakdl_ibkdl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibiakdl_ibkdl: do k = k0, n1k
if (k == l) cycle k_aibiakdl_ibkdl
b_aibiakdl_ibkdl: do b = n0b, n1b
if (b == d) cycle b_aibiakdl_ibkdl
i_aibiakdl_ibkdl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakdl_ibkdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiakdl_ibkdl(t2, &
 nocc, nactive, i, b, k, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdl_ibkdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdl_ibkdl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakdl_ibkdl
end do i_aibiakdl_ibkdl
end do b_aibiakdl_ibkdl
end do k_aibiakdl_ibkdl
end do l_aibiakdl_ibkdl
end do d_aibiakdl_ibkdl
!
! Elementary loop  12
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
i_aibjajdl_ibjdl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajdl_ibjdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajdl_ibjdl(t2, &
 nocc, nactive, i, b, j, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdl_ibjdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdl_ibjdl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  13
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
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajdl_aibdl(t2, &
 nocc, nactive, a, i, b, d, l)
j0 = max(l + 1, n0jk)
j_aibjajdl_aibdl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjajdl_aibdl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  14
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
nn1i = min(k - 1, n1il)
nn0a = max(b + 1, d + 1, n0ac)
if ((nn1i .ge. n0il).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakdi_bjkd(t2, &
 nocc, nactive, b, j, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdi_bjkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdi_bjkd
i1 = min(k - 1, n1il)
i_aibjakdi_bjkd: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakdi_bjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  15
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
i1 = min(k - 1, n1il)
i_aibjakdi_ibjkd: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakdi_ibjkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakdi_ibjkd(t2, &
 nocc, nactive, i, b, j, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdi_ibjkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdi_ibjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  16
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
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakdi_abjkd(t2, &
 nocc, nactive, a, b, j, k, d)
i1 = min(k - 1, n1il)
i_aibjakdi_abjkd: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakdi_abjkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  17
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
nn0i = max(l + 1, n0ik)
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaidl_bjdl(t2, &
 nocc, nactive, b, j, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidl_bjdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidl_bjdl
i0 = max(l + 1, n0ik)
i_aibjaidl_bjdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidl_bjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  18
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
i0 = max(l + 1, n0ik)
i_aibjaidl_ibjdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidl_ibjdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaidl_ibjdl(t2, &
 nocc, nactive, i, b, j, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidl_ibjdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidl_ibjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  19
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
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaidl_abjdl(t2, &
 nocc, nactive, a, b, j, d, l)
i0 = max(l + 1, n0ik)
i_aibjaidl_abjdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaidl_abjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  20
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
j1 = min(k - 1, n1jl)
j_aibjckaj_ibjck: do j = n0jl, j1
if (j == k) cycle j_aibjckaj_ibjck
i_aibjckaj_ibjck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckaj_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckaj_ibjck(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckaj_ibjck
end do i_aibjckaj_ibjck
end do j_aibjckaj_ibjck
end do b_aibjckaj_ibjck
end do k_aibjckaj_ibjck
end do c_aibjckaj_ibjck
!
! Elementary loop  21
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
nn1j = min(k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckaj_aibck(t2, &
 nocc, nactive, a, i, b, c, k)
j1 = min(k - 1, n1jl)
j_aibjckaj_aibck: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckaj_aibck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckaj_aibck
end do i_aibjckaj_aibck
end do a_aibjckaj_aibck
end do b_aibjckaj_aibck
end do k_aibjckaj_aibck
end do c_aibjckaj_aibck
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, j == i
! No equalities independent of the above can hold.
!
l_aibickal_ibckl: do l = n0l, n1l
c_aibickal_ibckl: do c = n0c, n1c
k0 = max(l + 1, n0k)
k_aibickal_ibckl: do k = k0, n1k
if (k == l) cycle k_aibickal_ibckl
b_aibickal_ibckl: do b = n0b, n1b
if (b == c) cycle b_aibickal_ibckl
i_aibickal_ibckl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibickal_ibckl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibickal_ibckl(t2, &
 nocc, nactive, i, b, c, k, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickal_ibckl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickal_ibckl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibickal_ibckl
end do i_aibickal_ibckl
end do b_aibickal_ibckl
end do k_aibickal_ibckl
end do c_aibickal_ibckl
end do l_aibickal_ibckl
!
! Elementary loop  23
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
j0 = max(l + 1, n0jk)
j_aibjcjal_ibjcl: do j = j0, n1jk
if (j == l) cycle j_aibjcjal_ibjcl
i_aibjcjal_ibjcl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjal_ibjcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjal_ibjcl(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjal_ibjcl
end do i_aibjcjal_ibjcl
end do j_aibjcjal_ibjcl
end do b_aibjcjal_ibjcl
end do c_aibjcjal_ibjcl
end do l_aibjcjal_ibjcl
!
! Elementary loop  24
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
nn0j = max(l + 1, n0jk)
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjal_aibcl(t2, &
 nocc, nactive, a, i, b, c, l)
j0 = max(l + 1, n0jk)
j_aibjcjal_aibcl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjcjal_aibcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjal_aibcl
end do i_aibjcjal_aibcl
end do a_aibjcjal_aibcl
end do b_aibjcjal_aibcl
end do c_aibjcjal_aibcl
end do l_aibjcjal_aibcl
!
! Elementary loop  25
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
nn1i = min(k - 1, n1il)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1i .ge. n0il).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckai_bjck(t2, &
 nocc, nactive, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckai_bjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckai_bjck
i1 = min(k - 1, n1il)
i_aibjckai_bjck: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjckai_bjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  26
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
i1 = min(k - 1, n1il)
i_aibjckai_ibjck: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjckai_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckai_ibjck(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  27
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
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjckai_abjck(t2, &
 nocc, nactive, a, b, j, c, k)
i1 = min(k - 1, n1il)
i_aibjckai_abjck: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjckai_abjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  28
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
nn0i = max(l + 1, n0ik)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1ik .ge. nn0i).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcial_bjcl(t2, &
 nocc, nactive, b, j, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcial_bjcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcial_bjcl
i0 = max(l + 1, n0ik)
i_aibjcial_bjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcial_bjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  29
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
i0 = max(l + 1, n0ik)
i_aibjcial_ibjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcial_ibjcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcial_ibjcl(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  30
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
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcial_abjcl(t2, &
 nocc, nactive, a, b, j, c, l)
i0 = max(l + 1, n0ik)
i_aibjcial_abjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcial_abjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
end subroutine ccjac_22_tripletmp_dav_part2
end module ccjac_block_22_tripletmp_dav_part2
