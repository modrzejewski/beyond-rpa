module ccjac_block_22_dav_part2
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
subroutine ccjac_22_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: nn0a, nn0b, nn0c, nn0i, nn0k, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b0, b1, c0, i0, i1, j1, k0
integer :: n0ab, n0ad, n0bc, n0bcd, n0cd
integer :: n0ij, n0ik, n0il, n0jk, n0jl
integer :: n0kl
integer :: n1ab, n1ad, n1bc, n1bcd, n1cd
integer :: n1ij, n1ik, n1il, n1jk, n1jl
integer :: n1kl
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
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0cd = max(n0c, n0d)
n0ij = max(n0i, n0j)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1ab = min(n1a, n1b)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
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
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aiajckdj_aickd(t2, nocc, nactive, &
 a, i, c, k, d)
j1 = min(i - 1, n1jl)
j_aiajckdj_aickd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aiajckdj_aickd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajckdj_aickd
end do i_aiajckdj_aickd
end do a_aiajckdj_aickd
end do k_aiajckdj_aickd
end do c_aiajckdj_aickd
end do d_aiajckdj_aickd
!
! Elementary loop  2
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
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1ik .ge. n0ik).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjcial_bjcl(t2, &
 nocc, nactive, b, j, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcial_bjcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcial_bjcl
i_aibjcial_bjcl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcial_bjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcial_bjcl
end do a_aibjcial_bjcl
end do j_aibjcial_bjcl
end do b_aibjcial_bjcl
end do c_aibjcial_bjcl
end do l_aibjcial_bjcl
!
! Elementary loop  3
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
i_aibjcial_ibjcl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcial_ibjcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjcial_ibjcl(t2, nocc, nactive, &
 i, b, j, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcial_ibjcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcial_ibjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcial_ibjcl
end do i_aibjcial_ibjcl
end do j_aibjcial_ibjcl
end do b_aibjcial_ibjcl
end do c_aibjcial_ibjcl
end do l_aibjcial_ibjcl
!
! Elementary loop  4
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
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjcial_abjcl(t2, nocc, nactive, &
 a, b, j, c, l)
i_aibjcial_abjcl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcial_abjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcial_abjcl
end do a_aibjcial_abjcl
end do j_aibjcial_abjcl
end do b_aibjcial_abjcl
end do c_aibjcial_abjcl
end do l_aibjcial_abjcl
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k, l
! Equalities: d == a, j == i
! No equalities independent of the above can hold.
!
l_aibickal_ibckl: do l = n0l, n1l
c_aibickal_ibckl: do c = n0c, n1c
k_aibickal_ibckl: do k = n0k, n1k
if (k == l) cycle k_aibickal_ibckl
b_aibickal_ibckl: do b = n0b, n1b
if (b == c) cycle b_aibickal_ibckl
i_aibickal_ibckl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibickal_ibckl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibickal_ibckl(t2, nocc, nactive, &
 i, b, c, k, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickal_ibckl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickal_ibckl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibickal_ibckl
end do i_aibickal_ibckl
end do b_aibickal_ibckl
end do k_aibickal_ibckl
end do c_aibickal_ibckl
end do l_aibickal_ibckl
!
! Elementary loop  6
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
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1il .ge. n0il).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjckai_bjck(t2, &
 nocc, nactive, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckai_bjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckai_bjck
i_aibjckai_bjck: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckai_bjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckai_bjck
end do a_aibjckai_bjck
end do j_aibjckai_bjck
end do b_aibjckai_bjck
end do k_aibjckai_bjck
end do c_aibjckai_bjck
!
! Elementary loop  7
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
i_aibjckai_ibjck: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckai_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjckai_ibjck(t2, nocc, nactive, &
 i, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckai_ibjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckai_ibjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckai_ibjck
end do i_aibjckai_ibjck
end do j_aibjckai_ibjck
end do b_aibjckai_ibjck
end do k_aibjckai_ibjck
end do c_aibjckai_ibjck
!
! Elementary loop  8
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
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjckai_abjck(t2, nocc, nactive, &
 a, b, j, c, k)
i_aibjckai_abjck: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckai_abjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckai_abjck
end do a_aibjckai_abjck
end do j_aibjckai_abjck
end do b_aibjckai_abjck
end do k_aibjckai_abjck
end do c_aibjckai_abjck
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjcjal_ibcl: do l = n0l, n1l
c_aibjcjal_ibcl: do c = n0c, n1c
b_aibjcjal_ibcl: do b = n0b, n1b
if (b == c) cycle b_aibjcjal_ibcl
i_aibjcjal_ibcl: do i = n0i, n1i
if (i == l) cycle i_aibjcjal_ibcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1jk .ge. n0jk).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjcjal_ibcl(t2, &
 nocc, nactive, i, b, c, l)
j_aibjcjal_ibcl: do j = n0jk, n1jk
if (j == i .or. j == l) cycle j_aibjcjal_ibcl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjal_ibcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjal_ibcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjal_ibcl
end do j_aibjcjal_ibcl
end do i_aibjcjal_ibcl
end do b_aibjcjal_ibcl
end do c_aibjcjal_ibcl
end do l_aibjcjal_ibcl
!
! Elementary loop  10
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
i_aibjcjal_ibjcl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjal_ibjcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjcjal_ibjcl(t2, nocc, nactive, &
 i, b, j, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjal_ibjcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjal_ibjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjal_ibjcl
end do i_aibjcjal_ibjcl
end do j_aibjcjal_ibjcl
end do b_aibjcjal_ibjcl
end do c_aibjcjal_ibjcl
end do l_aibjcjal_ibjcl
!
! Elementary loop  11
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
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjcjal_aibcl(t2, nocc, nactive, &
 a, i, b, c, l)
j_aibjcjal_aibcl: do j = n0jk, n1jk
if (j == i .or. j == l) cycle j_aibjcjal_aibcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjal_aibcl
end do i_aibjcjal_aibcl
end do a_aibjcjal_aibcl
end do b_aibjcjal_aibcl
end do c_aibjcjal_aibcl
end do l_aibjcjal_aibcl
!
! Elementary loop  12
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
i_aibjckak_ibjck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckak_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjckak_ibjck(t2, nocc, nactive, &
 i, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckak_ibjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckak_ibjck
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckak_ibjck
end do i_aibjckak_ibjck
end do j_aibjckak_ibjck
end do b_aibjckak_ibjck
end do k_aibjckak_ibjck
end do c_aibjckak_ibjck
!
! Elementary loop  13
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
i_aibjckaj_ibjck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckaj_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjckaj_ibjck(t2, nocc, nactive, &
 i, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaj_ibjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaj_ibjck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckaj_ibjck
end do i_aibjckaj_ibjck
end do j_aibjckaj_ibjck
end do b_aibjckaj_ibjck
end do k_aibjckaj_ibjck
end do c_aibjckaj_ibjck
!
! Elementary loop  14
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
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjckaj_aibck(t2, nocc, nactive, &
 a, i, b, c, k)
j_aibjckaj_aibck: do j = n0jl, n1jl
if (j == i .or. j == k) cycle j_aibjckaj_aibck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckaj_aibck
end do i_aibjckaj_aibck
end do a_aibjckaj_aibck
end do b_aibjckaj_aibck
end do k_aibjckaj_aibck
end do c_aibjckaj_aibck
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k, l
! Equalities: c == b, d == b
! No equalities independent of the above can hold.
!
l_aibjbkbl_aibjkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibjbkbl_aibjkl: do k = k0, n1k
if (k == l) cycle k_aibjbkbl_aibjkl
b_aibjbkbl_aibjkl: do b = n0bcd, n1bcd
j_aibjbkbl_aibjkl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjbkbl_aibjkl
a0 = max(b + 1, n0a)
a_aibjbkbl_aibjkl: do a = a0, n1a
if (a == b) cycle a_aibjbkbl_aibjkl
i_aibjbkbl_aibjkl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjbkbl_aibjkl
jac_ibra_iket = eom_ccsd_22_trans_aibjbkbl_aibjkl(t2, nocc, nactive, a, i, b, j, &
 k, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbl_aibjkl
end do a_aibjbkbl_aibjkl
end do j_aibjbkbl_aibjkl
end do b_aibjbkbl_aibjkl
end do k_aibjbkbl_aibjkl
end do l_aibjbkbl_aibjkl
!
! Elementary loop  16
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
i_aibjbidl_aijdl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjbidl_aijdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbidl_aijdl(t2, nocc, nactive, &
 a, i, j, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidl_aijdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidl_aijdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidl_aijdl
end do i_aibjbidl_aijdl
end do a_aibjbidl_aijdl
end do j_aibjbidl_aijdl
end do l_aibjbidl_aijdl
end do d_aibjbidl_aijdl
!
! Elementary loop  17
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
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjbidl_abjdl(t2, nocc, nactive, &
 a, b, j, d, l)
i_aibjbidl_abjdl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjbidl_abjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidl_abjdl
end do a_aibjbidl_abjdl
end do j_aibjbidl_abjdl
end do b_aibjbidl_abjdl
end do l_aibjbidl_abjdl
end do d_aibjbidl_abjdl
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k, l
! Equalities: c == b, j == i
! No equalities independent of the above can hold.
!
d_aibibkdl_aikdl: do d = n0d, n1d
l_aibibkdl_aikdl: do l = n0l, n1l
k_aibibkdl_aikdl: do k = n0k, n1k
if (k == l) cycle k_aibibkdl_aikdl
a_aibibkdl_aikdl: do a = n0a, n1a
if (a == d) cycle a_aibibkdl_aikdl
i_aibibkdl_aikdl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibibkdl_aikdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibibkdl_aikdl(t2, nocc, nactive, &
 a, i, k, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibkdl_aikdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibkdl_aikdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibkdl_aikdl
end do i_aibibkdl_aikdl
end do a_aibibkdl_aikdl
end do k_aibibkdl_aikdl
end do l_aibibkdl_aikdl
end do d_aibibkdl_aikdl
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, l == i
! No equalities independent of the above can hold.
!
d_aibjbkdi_ajkd: do d = n0d, n1d
k_aibjbkdi_ajkd: do k = n0k, n1k
j_aibjbkdi_ajkd: do j = n0j, n1j
if (j == k) cycle j_aibjbkdi_ajkd
a_aibjbkdi_ajkd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdi_ajkd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjbkdi_ajkd(t2, &
 nocc, nactive, a, j, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdi_ajkd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdi_ajkd
i_aibjbkdi_ajkd: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjbkdi_ajkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdi_ajkd
end do b_aibjbkdi_ajkd
end do a_aibjbkdi_ajkd
end do j_aibjbkdi_ajkd
end do k_aibjbkdi_ajkd
end do d_aibjbkdi_ajkd
!
! Elementary loop  20
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
i_aibjbkdi_aijkd: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjbkdi_aijkd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbkdi_aijkd(t2, nocc, nactive, &
 a, i, j, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdi_aijkd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdi_aijkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbkdi_aijkd
end do i_aibjbkdi_aijkd
end do a_aibjbkdi_aijkd
end do j_aibjbkdi_aijkd
end do k_aibjbkdi_aijkd
end do d_aibjbkdi_aijkd
!
! Elementary loop  21
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
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjbkdi_abjkd(t2, nocc, nactive, &
 a, b, j, k, d)
i_aibjbkdi_abjkd: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjbkdi_abjkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdi_abjkd
end do a_aibjbkdi_abjkd
end do j_aibjbkdi_abjkd
end do b_aibjbkdi_abjkd
end do k_aibjbkdi_abjkd
end do d_aibjbkdi_abjkd
!
! Elementary loop  22
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
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jk .ge. n0jk).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdl_aidl(t2, &
 nocc, nactive, a, i, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdl_aidl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdl_aidl
j_aibjbjdl_aidl: do j = n0jk, n1jk
if (j == i .or. j == l) cycle j_aibjbjdl_aidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdl_aidl
end do b_aibjbjdl_aidl
end do i_aibjbjdl_aidl
end do a_aibjbjdl_aidl
end do l_aibjbjdl_aidl
end do d_aibjbjdl_aidl
!
! Elementary loop  23
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
i_aibjbjdl_aijdl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjbjdl_aijdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdl_aijdl(t2, nocc, nactive, &
 a, i, j, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdl_aijdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdl_aijdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdl_aijdl
end do i_aibjbjdl_aijdl
end do a_aibjbjdl_aijdl
end do j_aibjbjdl_aijdl
end do l_aibjbjdl_aijdl
end do d_aibjbjdl_aijdl
!
! Elementary loop  24
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
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdl_aibdl(t2, nocc, nactive, &
 a, i, b, d, l)
j_aibjbjdl_aibdl: do j = n0jk, n1jk
if (j == i .or. j == l) cycle j_aibjbjdl_aibdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdl_aibdl
end do i_aibjbjdl_aibdl
end do a_aibjbjdl_aibdl
end do b_aibjbjdl_aibdl
end do l_aibjbjdl_aibdl
end do d_aibjbjdl_aibdl
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: k, i, j
! Equalities: c == b, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdk_aijkd: do d = n0d, n1d
k_aibjbkdk_aijkd: do k = n0kl, n1kl
j_aibjbkdk_aijkd: do j = n0j, n1j
if (j == k) cycle j_aibjbkdk_aijkd
a_aibjbkdk_aijkd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdk_aijkd
i_aibjbkdk_aijkd: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjbkdk_aijkd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbkdk_aijkd(t2, nocc, nactive, &
 a, i, j, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdk_aijkd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdk_aijkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbkdk_aijkd
end do i_aibjbkdk_aijkd
end do a_aibjbkdk_aijkd
end do j_aibjbkdk_aijkd
end do k_aibjbkdk_aijkd
end do d_aibjbkdk_aijkd
!
! Elementary loop  26
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
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jl .ge. n0jl).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbkdj_aikd(t2, &
 nocc, nactive, a, i, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdj_aikd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdj_aikd
j_aibjbkdj_aikd: do j = n0jl, n1jl
if (j == i .or. j == k) cycle j_aibjbkdj_aikd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbkdj_aikd
end do b_aibjbkdj_aikd
end do i_aibjbkdj_aikd
end do a_aibjbkdj_aikd
end do k_aibjbkdj_aikd
end do d_aibjbkdj_aikd
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj_aijkd: do d = n0d, n1d
k_aibjbkdj_aijkd: do k = n0k, n1k
j_aibjbkdj_aijkd: do j = n0jl, n1jl
if (j == k) cycle j_aibjbkdj_aijkd
a_aibjbkdj_aijkd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdj_aijkd
i_aibjbkdj_aijkd: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjbkdj_aijkd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbkdj_aijkd(t2, nocc, nactive, &
 a, i, j, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdj_aijkd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdj_aijkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbkdj_aijkd
end do i_aibjbkdj_aijkd
end do a_aibjbkdj_aijkd
end do j_aibjbkdj_aijkd
end do k_aibjbkdj_aijkd
end do d_aibjbkdj_aijkd
!
! Elementary loop  28
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
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjbkdj_aibkd(t2, nocc, nactive, &
 a, i, b, k, d)
j_aibjbkdj_aibkd: do j = n0jl, n1jl
if (j == i .or. j == k) cycle j_aibjbkdj_aibkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbkdj_aibkd
end do i_aibjbkdj_aibkd
end do a_aibjbkdj_aibkd
end do b_aibjbkdj_aibkd
end do k_aibjbkdj_aibkd
end do d_aibjbkdj_aibkd
!
! Elementary loop  29
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
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aibjcicl_abjcl(t2, nocc, nactive, &
 a, b, j, c, l)
i0 = max(l + 1, n0ik)
i_aibjcicl_abjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcicl_abjcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicl_abjcl
end do a_aibjcicl_abjcl
end do j_aibjcicl_abjcl
end do b_aibjcicl_abjcl
end do c_aibjcicl_abjcl
end do l_aibjcicl_abjcl
!
! Elementary loop  30
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
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjckci_abjck(t2, nocc, nactive, &
 a, b, j, c, k)
i1 = min(k - 1, n1il)
i_aibjckci_abjck: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjckci_abjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckci_abjck
end do a_aibjckci_abjck
end do j_aibjckci_abjck
end do b_aibjckci_abjck
end do k_aibjckci_abjck
end do c_aibjckci_abjck
end subroutine ccjac_22_dav_part2
end module ccjac_block_22_dav_part2
