module ccjac_block_22_dav_part9
use eom_ccsd_22_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-11-28 15:01:35 UTC.
!
contains
subroutine ccjac_22_dav_part9(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: nn0a, nn0c, nn0i, nn1b, nn1i
integer :: a0, b1, c0, i0, i1
integer :: n0abcd, n0bd, n0cd, n0ij, n0ijk
integer :: n0ijkl, n0ijl, n0ik, n0ikl, n0il
integer :: n0jk, n0jkl, n0jl, n0kl
integer :: n1abcd, n1bd, n1cd, n1ij, n1ijk
integer :: n1ijkl, n1ijl, n1ik, n1ikl, n1il
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
n0abcd = max(n0a, n0b, n0c, n0d)
n0bd = max(n0b, n0d)
n0cd = max(n0c, n0d)
n0ij = max(n0i, n0j)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1abcd = min(n1a, n1b, n1c, n1d)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
n1ijk = min(n1i, n1j, n1k)
n1ijkl = min(n1i, n1j, n1k, n1l)
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
! Free occupied indices: i, j
! Equalities: d == c, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjci_abc: do c = n0cd, n1cd
b_aibjcjci_abc: do b = n0b, n1b
if (b == c) cycle b_aibjcjci_abc
a0 = max(b + 1, n0a)
a_aibjcjci_abc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjci_abc
nn1i = min(n1jk - 1, n1il)
if ((n1jk .ge. n0jk).and. (nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjcjci_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcjci_abc: do j = n0jk, n1jk
i1 = min(j - 1, n1il)
i_aibjcjci_abc: do i = n0il, i1
if (i == j) cycle i_aibjcjci_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjci_abc
end do j_aibjcjci_abc
end do a_aibjcjci_abc
end do b_aibjcjci_abc
end do c_aibjcjci_abc
!
! Elementary loop  2
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjci_abjc: do c = n0cd, n1cd
b_aibjcjci_abjc: do b = n0b, n1b
if (b == c) cycle b_aibjcjci_abjc
j_aibjcjci_abjc: do j = n0jk, n1jk
a0 = max(b + 1, n0a)
a_aibjcjci_abjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjci_abjc
nn1i = min(j - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjcjci_abjc(t2, nocc, nactive, &
 a, b, j, c)
i1 = min(j - 1, n1il)
i_aibjcjci_abjc: do i = n0il, i1
if (i == j) cycle i_aibjcjci_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjci_abjc
end do a_aibjcjci_abjc
end do j_aibjcjci_abjc
end do b_aibjcjci_abjc
end do c_aibjcjci_abjc
!
! Elementary loop  3
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjci_aibc: do c = n0cd, n1cd
b_aibjcjci_aibc: do b = n0b, n1b
if (b == c) cycle b_aibjcjci_aibc
a0 = max(b + 1, n0a)
a_aibjcjci_aibc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjci_aibc
i1 = min(j - 1, n1il)
i_aibjcjci_aibc: do i = n0il, i1
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjcjci_aibc(t2, nocc, nactive, &
 a, i, b, c)
j_aibjcjci_aibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjci_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjci_aibc
end do i_aibjcjci_aibc
end do a_aibjcjci_aibc
end do b_aibjcjci_aibc
end do c_aibjcjci_aibc
!
! Elementary loop  4
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i
! Equalities: d == c, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjcj_aibjc: do c = n0cd, n1cd
b_aibjcjcj_aibjc: do b = n0b, n1b
if (b == c) cycle b_aibjcjcj_aibjc
j_aibjcjcj_aibjc: do j = n0jkl, n1jkl
a0 = max(b + 1, n0a)
a_aibjcjcj_aibjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcj_aibjc
i_aibjcjcj_aibjc: do i = n0i, n1i
if (i == j) cycle i_aibjcjcj_aibjc
jac_ibra_iket = eom_ccsd_22_trans_aibjcjcj_aibjc(t2, nocc, nactive, a, i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcj_aibjc
end do a_aibjcjcj_aibjc
end do j_aibjcjcj_aibjc
end do b_aibjcjcj_aibjc
end do c_aibjcjcj_aibjc
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, l
! Equalities: d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibicibl_aicl: do l = n0l, n1l
c_aibicibl_aicl: do c = n0c, n1c
a_aibicibl_aicl: do a = n0a, n1a
if (a == c) cycle a_aibicibl_aicl
i_aibicibl_aicl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibicibl_aicl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibicibl_aicl(t2, nocc, nactive, &
 a, i, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibicibl_aicl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibicibl_aicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibicibl_aicl
end do i_aibicibl_aicl
end do a_aibicibl_aicl
end do c_aibicibl_aicl
end do l_aibicibl_aicl
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, l
! Equalities: d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibicibl_aibcl: do l = n0l, n1l
c_aibicibl_aibcl: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibicibl_aibcl: do b = n0bd, b1
if (b == c) cycle b_aibicibl_aibcl
a0 = max(b + 1, n0a)
a_aibicibl_aibcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicibl_aibcl
i_aibicibl_aibcl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibicibl_aibcl
jac_ibra_iket = eom_ccsd_22_trans_aibicibl_aibcl(t2, nocc, nactive, a, i, b, c, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibl_aibcl
end do a_aibicibl_aibcl
end do b_aibicibl_aibcl
end do c_aibicibl_aibcl
end do l_aibicibl_aibcl
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcibi_aijc: do c = n0c, n1c
j_aibjcibi_aijc: do j = n0j, n1j
a_aibjcibi_aijc: do a = n0a, n1a
if (a == c) cycle a_aibjcibi_aijc
i_aibjcibi_aijc: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcibi_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjcibi_aijc(t2, nocc, nactive, &
 a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibi_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibi_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibi_aijc
end do i_aibjcibi_aijc
end do a_aibjcibi_aijc
end do j_aibjcibi_aijc
end do c_aibjcibi_aijc
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcibi_aibjc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibi_aibjc: do b = n0bd, b1
if (b == c) cycle b_aibjcibi_aibjc
j_aibjcibi_aibjc: do j = n0j, n1j
a0 = max(b + 1, n0a)
a_aibjcibi_aibjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibi_aibjc
i_aibjcibi_aibjc: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcibi_aibjc
jac_ibra_iket = eom_ccsd_22_trans_aibjcibi_aibjc(t2, nocc, nactive, a, i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibi_aibjc
end do a_aibjcibi_aibjc
end do j_aibjcibi_aibjc
end do b_aibjcibi_aibjc
end do c_aibjcibi_aibjc
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj_ac: do c = n0c, n1c
a_aibjcibj_ac: do a = n0a, n1a
if (a == c) cycle a_aibjcibj_ac
nn1b = min(a - 1, c - 1, n1bd)
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjcibj_ac(t2, &
 nocc, nactive, a, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_ac: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_ac
j_aibjcibj_ac: do j = n0jl, n1jl
i_aibjcibj_ac: do i = n0ik, n1ik
if (i == j) cycle i_aibjcibj_ac
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_ac
end do j_aibjcibj_ac
end do b_aibjcibj_ac
end do a_aibjcibj_ac
end do c_aibjcibj_ac
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj_aic: do c = n0c, n1c
a_aibjcibj_aic: do a = n0a, n1a
if (a == c) cycle a_aibjcibj_aic
i_aibjcibj_aic: do i = n0ik, n1ik
nn1b = min(a - 1, c - 1, n1bd)
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjcibj_aic(t2, &
 nocc, nactive, a, i, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_aic: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_aic
j_aibjcibj_aic: do j = n0jl, n1jl
if (j == i) cycle j_aibjcibj_aic
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcibj_aic
end do b_aibjcibj_aic
end do i_aibjcibj_aic
end do a_aibjcibj_aic
end do c_aibjcibj_aic
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj_ajc: do c = n0c, n1c
j_aibjcibj_ajc: do j = n0jl, n1jl
a_aibjcibj_ajc: do a = n0a, n1a
if (a == c) cycle a_aibjcibj_ajc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjcibj_ajc(t2, &
 nocc, nactive, a, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_ajc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_ajc
i_aibjcibj_ajc: do i = n0ik, n1ik
if (i == j) cycle i_aibjcibj_ajc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_ajc
end do b_aibjcibj_ajc
end do a_aibjcibj_ajc
end do j_aibjcibj_ajc
end do c_aibjcibj_ajc
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj_abc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibj_abc: do b = n0bd, b1
if (b == c) cycle b_aibjcibj_abc
a0 = max(b + 1, n0a)
a_aibjcibj_abc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibj_abc
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjcibj_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcibj_abc: do j = n0jl, n1jl
i_aibjcibj_abc: do i = n0ik, n1ik
if (i == j) cycle i_aibjcibj_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_abc
end do j_aibjcibj_abc
end do a_aibjcibj_abc
end do b_aibjcibj_abc
end do c_aibjcibj_abc
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj_aijc: do c = n0c, n1c
j_aibjcibj_aijc: do j = n0jl, n1jl
a_aibjcibj_aijc: do a = n0a, n1a
if (a == c) cycle a_aibjcibj_aijc
i_aibjcibj_aijc: do i = n0ik, n1ik
if (i == j) cycle i_aibjcibj_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjcibj_aijc(t2, nocc, nactive, &
 a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibj_aijc
end do i_aibjcibj_aijc
end do a_aibjcibj_aijc
end do j_aibjcibj_aijc
end do c_aibjcibj_aijc
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj_aibc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibj_aibc: do b = n0bd, b1
if (b == c) cycle b_aibjcibj_aibc
a0 = max(b + 1, n0a)
a_aibjcibj_aibc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibj_aibc
i_aibjcibj_aibc: do i = n0ik, n1ik
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjcibj_aibc(t2, nocc, nactive, &
 a, i, b, c)
j_aibjcibj_aibc: do j = n0jl, n1jl
if (j == i) cycle j_aibjcibj_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcibj_aibc
end do i_aibjcibj_aibc
end do a_aibjcibj_aibc
end do b_aibjcibj_aibc
end do c_aibjcibj_aibc
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcibj_abjc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibj_abjc: do b = n0bd, b1
if (b == c) cycle b_aibjcibj_abjc
j_aibjcibj_abjc: do j = n0jl, n1jl
a0 = max(b + 1, n0a)
a_aibjcibj_abjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibj_abjc
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjcibj_abjc(t2, nocc, nactive, &
 a, b, j, c)
i_aibjcibj_abjc: do i = n0ik, n1ik
if (i == j) cycle i_aibjcibj_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_abjc
end do a_aibjcibj_abjc
end do j_aibjcibj_abjc
end do b_aibjcibj_abjc
end do c_aibjcibj_abjc
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickbi_aick: do c = n0c, n1c
k_aibickbi_aick: do k = n0k, n1k
a_aibickbi_aick: do a = n0a, n1a
if (a == c) cycle a_aibickbi_aick
i_aibickbi_aick: do i = n0ijl, n1ijl
if (i == k) cycle i_aibickbi_aick
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibickbi_aick(t2, nocc, nactive, &
 a, i, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibickbi_aick: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibickbi_aick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibickbi_aick
end do i_aibickbi_aick
end do a_aibickbi_aick
end do k_aibickbi_aick
end do c_aibickbi_aick
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickbi_aibck: do c = n0c, n1c
k_aibickbi_aibck: do k = n0k, n1k
b1 = min(c - 1, n1bd)
b_aibickbi_aibck: do b = n0bd, b1
if (b == c) cycle b_aibickbi_aibck
a0 = max(b + 1, n0a)
a_aibickbi_aibck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibickbi_aibck
i_aibickbi_aibck: do i = n0ijl, n1ijl
if (i == k) cycle i_aibickbi_aibck
jac_ibra_iket = eom_ccsd_22_trans_aibickbi_aibck(t2, nocc, nactive, a, i, b, c, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickbi_aibck
end do a_aibickbi_aibck
end do b_aibickbi_aibck
end do k_aibickbi_aibck
end do c_aibickbi_aibck
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == k
! No equalities independent of the above can hold.
!
c_aibickbk_aick: do c = n0c, n1c
k_aibickbk_aick: do k = n0kl, n1kl
a_aibickbk_aick: do a = n0a, n1a
if (a == c) cycle a_aibickbk_aick
i_aibickbk_aick: do i = n0ij, n1ij
if (i == k) cycle i_aibickbk_aick
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibickbk_aick(t2, nocc, nactive, &
 a, i, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibickbk_aick: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibickbk_aick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibickbk_aick
end do i_aibickbk_aick
end do a_aibickbk_aick
end do k_aibickbk_aick
end do c_aibickbk_aick
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_aic: do c = n0c, n1c
a_aibjcjbi_aic: do a = n0a, n1a
if (a == c) cycle a_aibjcjbi_aic
i_aibjcjbi_aic: do i = n0il, n1il
nn1b = min(a - 1, c - 1, n1bd)
if ((n1jk .ge. n0jk).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjcjbi_aic(t2, &
 nocc, nactive, a, i, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbi_aic: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbi_aic
j_aibjcjbi_aic: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjbi_aic
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbi_aic
end do b_aibjcjbi_aic
end do i_aibjcjbi_aic
end do a_aibjcjbi_aic
end do c_aibjcjbi_aic
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_abc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbi_abc: do b = n0bd, b1
if (b == c) cycle b_aibjcjbi_abc
a0 = max(b + 1, n0a)
a_aibjcjbi_abc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbi_abc
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjcjbi_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcjbi_abc: do j = n0jk, n1jk
i_aibjcjbi_abc: do i = n0il, n1il
if (i == j) cycle i_aibjcjbi_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbi_abc
end do j_aibjcjbi_abc
end do a_aibjcjbi_abc
end do b_aibjcjbi_abc
end do c_aibjcjbi_abc
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_aijc: do c = n0c, n1c
j_aibjcjbi_aijc: do j = n0jk, n1jk
a_aibjcjbi_aijc: do a = n0a, n1a
if (a == c) cycle a_aibjcjbi_aijc
i_aibjcjbi_aijc: do i = n0il, n1il
if (i == j) cycle i_aibjcjbi_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjcjbi_aijc(t2, nocc, nactive, &
 a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbi_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbi_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbi_aijc
end do i_aibjcjbi_aijc
end do a_aibjcjbi_aijc
end do j_aibjcjbi_aijc
end do c_aibjcjbi_aijc
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_abjc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbi_abjc: do b = n0bd, b1
if (b == c) cycle b_aibjcjbi_abjc
j_aibjcjbi_abjc: do j = n0jk, n1jk
a0 = max(b + 1, n0a)
a_aibjcjbi_abjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbi_abjc
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjcjbi_abjc(t2, nocc, nactive, &
 a, b, j, c)
i_aibjcjbi_abjc: do i = n0il, n1il
if (i == j) cycle i_aibjcjbi_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbi_abjc
end do a_aibjcjbi_abjc
end do j_aibjcjbi_abjc
end do b_aibjcjbi_abjc
end do c_aibjcjbi_abjc
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_aibc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbi_aibc: do b = n0bd, b1
if (b == c) cycle b_aibjcjbi_aibc
a0 = max(b + 1, n0a)
a_aibjcjbi_aibc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbi_aibc
i_aibjcjbi_aibc: do i = n0il, n1il
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjcjbi_aibc(t2, nocc, nactive, &
 a, i, b, c)
j_aibjcjbi_aibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjbi_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbi_aibc
end do i_aibjcjbi_aibc
end do a_aibjcjbi_aibc
end do b_aibjcjbi_aibc
end do c_aibjcjbi_aibc
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i
! Equalities: d == b, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbj_aijc: do c = n0c, n1c
j_aibjcjbj_aijc: do j = n0jkl, n1jkl
a_aibjcjbj_aijc: do a = n0a, n1a
if (a == c) cycle a_aibjcjbj_aijc
i_aibjcjbj_aijc: do i = n0i, n1i
if (i == j) cycle i_aibjcjbj_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjcjbj_aijc(t2, nocc, nactive, &
 a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbj_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbj_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbj_aijc
end do i_aibjcjbj_aijc
end do a_aibjcjbj_aijc
end do j_aibjcjbj_aijc
end do c_aibjcjbj_aijc
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i
! Equalities: d == b, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjbj_aibjc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbj_aibjc: do b = n0bd, b1
if (b == c) cycle b_aibjcjbj_aibjc
j_aibjcjbj_aibjc: do j = n0jkl, n1jkl
a0 = max(b + 1, n0a)
a_aibjcjbj_aibjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbj_aibjc
i_aibjcjbj_aibjc: do i = n0i, n1i
if (i == j) cycle i_aibjcjbj_aibjc
jac_ibra_iket = eom_ccsd_22_trans_aibjcjbj_aibjc(t2, nocc, nactive, a, i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbj_aibjc
end do a_aibjcjbj_aibjc
end do j_aibjcjbj_aibjc
end do b_aibjcjbj_aibjc
end do c_aibjcjbj_aibjc
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i
! Equalities: j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibicidi_abcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibicidi_abcd: do c = c0, n1c
if (c == d) cycle c_aibicidi_abcd
b_aibicidi_abcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibicidi_abcd
a0 = max(b + 1, n0a)
a_aibicidi_abcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibicidi_abcd
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibicidi_abcd(t2, nocc, &
 nactive, a, b, c, d)
i_aibicidi_abcd: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicidi_abcd
end do a_aibicidi_abcd
end do b_aibicidi_abcd
end do c_aibicidi_abcd
end do d_aibicidi_abcd
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i
! Equalities: j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibicidi_aibcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibicidi_aibcd: do c = c0, n1c
if (c == d) cycle c_aibicidi_aibcd
b_aibicidi_aibcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibicidi_aibcd
a0 = max(b + 1, n0a)
a_aibicidi_aibcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibicidi_aibcd
i_aibicidi_aibcd: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aibicidi_aibcd(t2, nocc, nactive, a, i, b, c, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicidi_aibcd
end do a_aibicidi_aibcd
end do b_aibicidi_aibcd
end do c_aibicidi_aibcd
end do d_aibicidi_aibcd
!
! Elementary loop  28
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajaial_jl: do l = n0l, n1l
j_aiajaial_jl: do j = n0j, n1j
if (j == l) cycle j_aiajaial_jl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i).and. (n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiajaial_jl(t2, &
 nocc, nactive, j, l)
a_aiajaial_jl: do a = n0abcd, n1abcd
i0 = max(j + 1, l + 1, n0ik)
i_aiajaial_jl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaial_jl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaial_jl
end do a_aiajaial_jl
end do j_aiajaial_jl
end do l_aiajaial_jl
!
! Elementary loop  29
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajaial_ajl: do l = n0l, n1l
j_aiajaial_ajl: do j = n0j, n1j
if (j == l) cycle j_aiajaial_ajl
a_aiajaial_ajl: do a = n0abcd, n1abcd
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajaial_ajl(t2, nocc, nactive, &
 a, j, l)
i0 = max(j + 1, l + 1, n0ik)
i_aiajaial_ajl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaial_ajl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaial_ajl
end do a_aiajaial_ajl
end do j_aiajaial_ajl
end do l_aiajaial_ajl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajaial_ijl: do l = n0l, n1l
j_aiajaial_ijl: do j = n0j, n1j
if (j == l) cycle j_aiajaial_ijl
i0 = max(j + 1, l + 1, n0ik)
i_aiajaial_ijl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaial_ijl
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiajaial_ijl(t2, nocc, &
 nactive, i, j, l)
a_aiajaial_ijl: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajaial_ijl
end do i_aiajaial_ijl
end do j_aiajaial_ijl
end do l_aiajaial_ijl
end subroutine ccjac_22_dav_part9
end module ccjac_block_22_dav_part9
