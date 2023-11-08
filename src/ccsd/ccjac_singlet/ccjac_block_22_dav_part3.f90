module ccjac_block_22_dav_part3
use eom_ccsd_22_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-11-28 15:01:34 UTC.
!
contains
subroutine ccjac_22_dav_part3(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0) 
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
integer :: nn0a, nn0c, nn0i, nn0j, nn0k, nn1b, nn1j
integer :: a0, b1, c0, i0, j0, j1, k0
integer :: n0abc, n0abcd, n0bd, n0cd, n0ij
integer :: n0ijk, n0ijl, n0ik, n0ikl, n0il
integer :: n0jk, n0jkl, n0jl, n0kl
integer :: n1abc, n1abcd, n1bd, n1cd, n1ij
integer :: n1ijk, n1ijl, n1ik, n1ikl, n1il
integer :: n1jk, n1jkl, n1jl, n1kl
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
n0abcd = max(n0a, n0b, n0c, n0d)
n0bd = max(n0b, n0d)
n0cd = max(n0c, n0d)
n0ij = max(n0i, n0j)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1abc = min(n1a, n1b, n1c)
n1abcd = min(n1a, n1b, n1c, n1d)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
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
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_trans_aibjcjcl_aibcl(t2, nocc, nactive, &
 a, i, b, c, l)
j0 = max(l + 1, n0jk)
j_aibjcjcl_aibcl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjcjcl_aibcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjcl_aibcl
end do i_aibjcjcl_aibcl
end do a_aibjcjcl_aibcl
end do b_aibjcjcl_aibcl
end do c_aibjcjcl_aibcl
end do l_aibjcjcl_aibcl
!
! Elementary loop  2
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
nn1j = min(k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjckcj_aibck(t2, nocc, nactive, &
 a, i, b, c, k)
j1 = min(k - 1, n1jl)
j_aibjckcj_aibck: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckcj_aibck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckcj_aibck
end do i_aibjckcj_aibck
end do a_aibjckcj_aibck
end do b_aibjckcj_aibck
end do k_aibjckcj_aibck
end do c_aibjckcj_aibck
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl_ajcl: do l = n0l, n1l
c_aibjcibl_ajcl: do c = n0c, n1c
j_aibjcibl_ajcl: do j = n0j, n1j
if (j == l) cycle j_aibjcibl_ajcl
a_aibjcibl_ajcl: do a = n0a, n1a
if (a == c) cycle a_aibjcibl_ajcl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjcibl_ajcl(t2, &
 nocc, nactive, a, j, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibl_ajcl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibl_ajcl
i_aibjcibl_ajcl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcibl_ajcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibl_ajcl
end do b_aibjcibl_ajcl
end do a_aibjcibl_ajcl
end do j_aibjcibl_ajcl
end do c_aibjcibl_ajcl
end do l_aibjcibl_ajcl
!
! Elementary loop  4
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
i_aibjcibl_aijcl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcibl_aijcl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjcibl_aijcl(t2, nocc, nactive, &
 a, i, j, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibl_aijcl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibl_aijcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibl_aijcl
end do i_aibjcibl_aijcl
end do a_aibjcibl_aijcl
end do j_aibjcibl_aijcl
end do c_aibjcibl_aijcl
end do l_aibjcibl_aijcl
!
! Elementary loop  5
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
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjcibl_abjcl(t2, nocc, nactive, &
 a, b, j, c, l)
i_aibjcibl_abjcl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcibl_abjcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibl_abjcl
end do a_aibjcibl_abjcl
end do j_aibjcibl_abjcl
end do b_aibjcibl_abjcl
end do c_aibjcibl_abjcl
end do l_aibjcibl_abjcl
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k, l
! Equalities: d == b, j == i
! No equalities independent of the above can hold.
!
l_aibickbl_aickl: do l = n0l, n1l
c_aibickbl_aickl: do c = n0c, n1c
k_aibickbl_aickl: do k = n0k, n1k
if (k == l) cycle k_aibickbl_aickl
a_aibickbl_aickl: do a = n0a, n1a
if (a == c) cycle a_aibickbl_aickl
i_aibickbl_aickl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibickbl_aickl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibickbl_aickl(t2, nocc, nactive, &
 a, i, c, k, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibickbl_aickl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibickbl_aickl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibickbl_aickl
end do i_aibickbl_aickl
end do a_aibickbl_aickl
end do k_aibickbl_aickl
end do c_aibickbl_aickl
end do l_aibickbl_aickl
!
! Elementary loop  7
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
i_aibjckbi_aijck: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbi_aijck
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjckbi_aijck(t2, nocc, nactive, &
 a, i, j, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbi_aijck: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbi_aijck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjckbi_aijck
end do i_aibjckbi_aijck
end do a_aibjckbi_aijck
end do j_aibjckbi_aijck
end do k_aibjckbi_aijck
end do c_aibjckbi_aijck
!
! Elementary loop  8
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
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjckbi_abjck(t2, nocc, nactive, &
 a, b, j, c, k)
i_aibjckbi_abjck: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbi_abjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbi_abjck
end do a_aibjckbi_abjck
end do j_aibjckbi_abjck
end do b_aibjckbi_abjck
end do k_aibjckbi_abjck
end do c_aibjckbi_abjck
!
! Elementary loop  9
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
nn1b = min(a - 1, c - 1, n1bd)
if ((n1jk .ge. n0jk).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjcjbl_aicl(t2, &
 nocc, nactive, a, i, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbl_aicl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbl_aicl
j_aibjcjbl_aicl: do j = n0jk, n1jk
if (j == i .or. j == l) cycle j_aibjcjbl_aicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbl_aicl
end do b_aibjcjbl_aicl
end do i_aibjcjbl_aicl
end do a_aibjcjbl_aicl
end do c_aibjcjbl_aicl
end do l_aibjcjbl_aicl
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl_aijcl: do l = n0l, n1l
c_aibjcjbl_aijcl: do c = n0c, n1c
j_aibjcjbl_aijcl: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjbl_aijcl
a_aibjcjbl_aijcl: do a = n0a, n1a
if (a == c) cycle a_aibjcjbl_aijcl
i_aibjcjbl_aijcl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjbl_aijcl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjcjbl_aijcl(t2, nocc, nactive, &
 a, i, j, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbl_aijcl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbl_aijcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbl_aijcl
end do i_aibjcjbl_aijcl
end do a_aibjcjbl_aijcl
end do j_aibjcjbl_aijcl
end do c_aibjcjbl_aijcl
end do l_aibjcjbl_aijcl
!
! Elementary loop  11
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
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjcjbl_aibcl(t2, nocc, nactive, &
 a, i, b, c, l)
j_aibjcjbl_aibcl: do j = n0jk, n1jk
if (j == i .or. j == l) cycle j_aibjcjbl_aibcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbl_aibcl
end do i_aibjcjbl_aibcl
end do a_aibjcjbl_aibcl
end do b_aibjcjbl_aibcl
end do c_aibjcjbl_aibcl
end do l_aibjcjbl_aibcl
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: k, i, j
! Equalities: d == b, l == k
! No equalities independent of the above can hold.
!
c_aibjckbk_aijck: do c = n0c, n1c
k_aibjckbk_aijck: do k = n0kl, n1kl
j_aibjckbk_aijck: do j = n0j, n1j
if (j == k) cycle j_aibjckbk_aijck
a_aibjckbk_aijck: do a = n0a, n1a
if (a == c) cycle a_aibjckbk_aijck
i_aibjckbk_aijck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbk_aijck
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjckbk_aijck(t2, nocc, nactive, &
 a, i, j, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbk_aijck: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbk_aijck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjckbk_aijck
end do i_aibjckbk_aijck
end do a_aibjckbk_aijck
end do j_aibjckbk_aijck
end do k_aibjckbk_aijck
end do c_aibjckbk_aijck
!
! Elementary loop  13
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
nn1b = min(a - 1, c - 1, n1bd)
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjckbj_aick(t2, &
 nocc, nactive, a, i, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbj_aick: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbj_aick
j_aibjckbj_aick: do j = n0jl, n1jl
if (j == i .or. j == k) cycle j_aibjckbj_aick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckbj_aick
end do b_aibjckbj_aick
end do i_aibjckbj_aick
end do a_aibjckbj_aick
end do k_aibjckbj_aick
end do c_aibjckbj_aick
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj_aijck: do c = n0c, n1c
k_aibjckbj_aijck: do k = n0k, n1k
j_aibjckbj_aijck: do j = n0jl, n1jl
if (j == k) cycle j_aibjckbj_aijck
a_aibjckbj_aijck: do a = n0a, n1a
if (a == c) cycle a_aibjckbj_aijck
i_aibjckbj_aijck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbj_aijck
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjckbj_aijck(t2, nocc, nactive, &
 a, i, j, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbj_aijck: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbj_aijck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjckbj_aijck
end do i_aibjckbj_aijck
end do a_aibjckbj_aijck
end do j_aibjckbj_aijck
end do k_aibjckbj_aijck
end do c_aibjckbj_aijck
!
! Elementary loop  15
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
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjckbj_aibck(t2, nocc, nactive, &
 a, i, b, c, k)
j_aibjckbj_aibck: do j = n0jl, n1jl
if (j == i .or. j == k) cycle j_aibjckbj_aibck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckbj_aibck
end do i_aibjckbj_aibck
end do a_aibjckbj_aibck
end do b_aibjckbj_aibck
end do k_aibjckbj_aibck
end do c_aibjckbj_aibck
!
! Elementary loop  16
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
i_aibicidl_aibcdl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibicidl_aibcdl
jac_ibra_iket = eom_ccsd_22_trans_aibicidl_aibcdl(t2, nocc, nactive, a, i, b, c, &
 d, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicidl_aibcdl
end do a_aibicidl_aibcdl
end do b_aibicidl_aibcdl
end do c_aibicidl_aibcdl
end do l_aibicidl_aibcdl
end do d_aibicidl_aibcdl
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjcidi_aibjcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidi_aibjcd: do c = c0, n1c
if (c == d) cycle c_aibjcidi_aibjcd
b_aibjcidi_aibjcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidi_aibjcd
j_aibjcidi_aibjcd: do j = n0j, n1j
a0 = max(b + 1, n0a)
a_aibjcidi_aibjcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidi_aibjcd
i_aibjcidi_aibjcd: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcidi_aibjcd
jac_ibra_iket = eom_ccsd_22_trans_aibjcidi_aibjcd(t2, nocc, nactive, a, i, b, j, &
 c, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidi_aibjcd
end do a_aibjcidi_aibjcd
end do j_aibjcidi_aibjcd
end do b_aibjcidi_aibjcd
end do c_aibjcidi_aibjcd
end do d_aibjcidi_aibjcd
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidj_abcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidj_abcd: do c = c0, n1c
if (c == d) cycle c_aibjcidj_abcd
b_aibjcidj_abcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidj_abcd
a0 = max(b + 1, n0a)
a_aibjcidj_abcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidj_abcd
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjcidj_abcd(t2, &
 nocc, nactive, a, b, c, d)
j_aibjcidj_abcd: do j = n0jl, n1jl
i_aibjcidj_abcd: do i = n0ik, n1ik
if (i == j) cycle i_aibjcidj_abcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidj_abcd
end do j_aibjcidj_abcd
end do a_aibjcidj_abcd
end do b_aibjcidj_abcd
end do c_aibjcidj_abcd
end do d_aibjcidj_abcd
!
! Elementary loop  19
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
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjcidj_aibcd(t2, nocc, nactive, &
 a, i, b, c, d)
j_aibjcidj_aibcd: do j = n0jl, n1jl
if (j == i) cycle j_aibjcidj_aibcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcidj_aibcd
end do i_aibjcidj_aibcd
end do a_aibjcidj_aibcd
end do b_aibjcidj_aibcd
end do c_aibjcidj_aibcd
end do d_aibjcidj_aibcd
!
! Elementary loop  20
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
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjcidj_abjcd(t2, nocc, nactive, &
 a, b, j, c, d)
i_aibjcidj_abjcd: do i = n0ik, n1ik
if (i == j) cycle i_aibjcidj_abjcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidj_abjcd
end do a_aibjcidj_abjcd
end do j_aibjcidj_abjcd
end do b_aibjcidj_abjcd
end do c_aibjcidj_abjcd
end do d_aibjcidj_abjcd
!
! Elementary loop  21
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
i_aibickdi_aibckd: do i = n0ijl, n1ijl
if (i == k) cycle i_aibickdi_aibckd
jac_ibra_iket = eom_ccsd_22_trans_aibickdi_aibckd(t2, nocc, nactive, a, i, b, c, &
 k, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickdi_aibckd
end do a_aibickdi_aibckd
end do b_aibickdi_aibckd
end do k_aibickdi_aibckd
end do c_aibickdi_aibckd
end do d_aibickdi_aibckd
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdi_abcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdi_abcd: do c = c0, n1c
if (c == d) cycle c_aibjcjdi_abcd
b_aibjcjdi_abcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdi_abcd
a0 = max(b + 1, n0a)
a_aibjcjdi_abcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdi_abcd
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjcjdi_abcd(t2, &
 nocc, nactive, a, b, c, d)
j_aibjcjdi_abcd: do j = n0jk, n1jk
i_aibjcjdi_abcd: do i = n0il, n1il
if (i == j) cycle i_aibjcjdi_abcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdi_abcd
end do j_aibjcjdi_abcd
end do a_aibjcjdi_abcd
end do b_aibjcjdi_abcd
end do c_aibjcjdi_abcd
end do d_aibjcjdi_abcd
!
! Elementary loop  23
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
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjcjdi_abjcd(t2, nocc, nactive, &
 a, b, j, c, d)
i_aibjcjdi_abjcd: do i = n0il, n1il
if (i == j) cycle i_aibjcjdi_abjcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdi_abjcd
end do a_aibjcjdi_abjcd
end do j_aibjcjdi_abjcd
end do b_aibjcjdi_abjcd
end do c_aibjcjdi_abjcd
end do d_aibjcjdi_abjcd
!
! Elementary loop  24
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
i_aibjcjdi_aibcd: do i = n0il, n1il
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjcjdi_aibcd(t2, nocc, nactive, &
 a, i, b, c, d)
j_aibjcjdi_aibcd: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjdi_aibcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjdi_aibcd
end do i_aibjcjdi_aibcd
end do a_aibjcjdi_aibcd
end do b_aibjcjdi_aibcd
end do c_aibjcjdi_aibcd
end do d_aibjcjdi_aibcd
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i
! Equalities: k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjcjdj_aibjcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdj_aibjcd: do c = c0, n1c
if (c == d) cycle c_aibjcjdj_aibjcd
b_aibjcjdj_aibjcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdj_aibjcd
j_aibjcjdj_aibjcd: do j = n0jkl, n1jkl
a0 = max(b + 1, n0a)
a_aibjcjdj_aibjcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdj_aibjcd
i_aibjcjdj_aibjcd: do i = n0i, n1i
if (i == j) cycle i_aibjcjdj_aibjcd
jac_ibra_iket = eom_ccsd_22_trans_aibjcjdj_aibjcd(t2, nocc, nactive, a, i, b, j, &
 c, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdj_aibjcd
end do a_aibjcjdj_aibjcd
end do j_aibjcjdj_aibjcd
end do b_aibjcjdj_aibjcd
end do c_aibjcjdj_aibjcd
end do d_aibjcjdj_aibjcd
!
! Elementary loop  26
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k, l
! Equalities: b == a, c == a, d == a
! No equalities independent of the above can hold.
!
l_aiajakal_ijkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aiajakal_ijkl: do k = k0, n1k
if (k == l) cycle k_aiajakal_ijkl
j_aiajakal_ijkl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aiajakal_ijkl
i0 = max(j + 1, n0i)
i_aiajakal_ijkl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajakal_ijkl
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiajakal_ijkl(t2, nocc, &
 nactive, i, j, k, l)
a_aiajakal_ijkl: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajakal_ijkl
end do i_aiajakal_ijkl
end do j_aiajakal_ijkl
end do k_aiajakal_ijkl
end do l_aiajakal_ijkl
!
! Elementary loop  27
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k, l
! Equalities: b == a, c == a, d == a
! No equalities independent of the above can hold.
!
l_aiajakal_aijkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aiajakal_aijkl: do k = k0, n1k
if (k == l) cycle k_aiajakal_aijkl
j_aiajakal_aijkl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aiajakal_aijkl
a_aiajakal_aijkl: do a = n0abcd, n1abcd
i0 = max(j + 1, n0i)
i_aiajakal_aijkl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajakal_aijkl
jac_ibra_iket = eom_ccsd_22_trans_aiajakal_aijkl(t2, nocc, nactive, a, i, j, k, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakal_aijkl
end do a_aiajakal_aijkl
end do j_aiajakal_aijkl
end do k_aiajakal_aijkl
end do l_aiajakal_aijkl
!
! Elementary loop  28
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
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajaidl_ajdl(t2, nocc, nactive, &
 a, j, d, l)
i0 = max(j + 1, n0ik)
i_aiajaidl_ajdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaidl_ajdl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidl_ajdl
end do a_aiajaidl_ajdl
end do j_aiajaidl_ajdl
end do l_aiajaidl_ajdl
end do d_aiajaidl_ajdl
!
! Elementary loop  29
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
i0 = max(j + 1, n0ik)
i_aiajaidl_aijdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaidl_aijdl
jac_ibra_iket = eom_ccsd_22_trans_aiajaidl_aijdl(t2, nocc, nactive, a, i, j, d, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidl_aijdl
end do a_aiajaidl_aijdl
end do j_aiajaidl_aijdl
end do l_aiajaidl_aijdl
end do d_aiajaidl_aijdl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, k, l
! Equalities: b == a, c == a, j == i
! No equalities independent of the above can hold.
!
d_aiaiakdl_aikdl: do d = n0d, n1d
l_aiaiakdl_aikdl: do l = n0l, n1l
k_aiaiakdl_aikdl: do k = n0k, n1k
if (k == l) cycle k_aiaiakdl_aikdl
a0 = max(d + 1, n0abc)
a_aiaiakdl_aikdl: do a = a0, n1abc
if (a == d) cycle a_aiaiakdl_aikdl
i_aiaiakdl_aikdl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aiaiakdl_aikdl
jac_ibra_iket = eom_ccsd_22_trans_aiaiakdl_aikdl(t2, nocc, nactive, a, i, k, d, l)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakdl_aikdl
end do a_aiaiakdl_aikdl
end do k_aiaiakdl_aikdl
end do l_aiaiakdl_aikdl
end do d_aiaiakdl_aikdl
end subroutine ccjac_22_dav_part3
end module ccjac_block_22_dav_part3
