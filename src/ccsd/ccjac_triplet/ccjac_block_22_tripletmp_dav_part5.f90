module ccjac_block_22_tripletmp_dav_part5
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
subroutine ccjac_22_tripletmp_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0i, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b1, i0, i1, j1
integer :: n0ac, n0ad, n0bd, n0ijk, n0ijl
integer :: n0ik, n0il, n0jk, n0jl
integer :: n1ac, n1ad, n1bd, n1ijk, n1ijl
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
n0ac = max(n0a, n0c)
n0ad = max(n0a, n0d)
n0bd = max(n0b, n0d)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1bd = min(n1b, n1d)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
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
i0 = max(l + 1, n0ijk)
i_aibicibl_aicl: do i = i0, n1ijk
if (i == l) cycle i_aibicibl_aicl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibicibl_aicl(t2, &
 nocc, nactive, a, i, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibicibl_aicl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibicibl_aicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibicibl_aicl
end do i_aibicibl_aicl
end do a_aibicibl_aicl
end do c_aibicibl_aicl
end do l_aibicibl_aicl
!
! Elementary loop  2
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
i0 = max(l + 1, n0ijk)
i_aibicibl_aibcl: do i = i0, n1ijk
if (i == l) cycle i_aibicibl_aibcl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibicibl_aibcl(t2, nocc, nactive, a, &
 i, b, c, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibl_aibcl
end do a_aibicibl_aibcl
end do b_aibicibl_aibcl
end do c_aibicibl_aibcl
end do l_aibicibl_aibcl
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_aic: do c = n0c, n1c
a_aibjcjbi_aic: do a = n0a, n1a
if (a == c) cycle a_aibjcjbi_aic
i1 = min(j - 1, n1il)
i_aibjcjbi_aic: do i = n0il, i1
nn1b = min(a - 1, c - 1, n1bd)
if ((n1jk .ge. n0jk).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjbi_aic(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbi_aic
end do b_aibjcjbi_aic
end do i_aibjcjbi_aic
end do a_aibjcjbi_aic
end do c_aibjcjbi_aic
!
! Elementary loop  4
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
nn1i = min(j - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjbi_abjc(t2, &
 nocc, nactive, a, b, j, c)
i1 = min(j - 1, n1il)
i_aibjcjbi_abjc: do i = n0il, i1
if (i == j) cycle i_aibjcjbi_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbi_abjc
end do a_aibjcjbi_abjc
end do j_aibjcjbi_abjc
end do b_aibjcjbi_abjc
end do c_aibjcjbi_abjc
!
! Elementary loop  5
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
i1 = min(j - 1, n1il)
i_aibjcjbi_aibc: do i = n0il, i1
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjbi_aibc(t2, &
 nocc, nactive, a, i, b, c)
j_aibjcjbi_aibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjbi_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbi_aibc
end do i_aibjcjbi_aibc
end do a_aibjcjbi_aibc
end do b_aibjcjbi_aibc
end do c_aibjcjbi_aibc
!
! Elementary loop  6
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
i1 = min(j - 1, n1il)
i_aibjcjbi_aijc: do i = n0il, i1
if (i == j) cycle i_aibjcjbi_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjbi_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbi_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbi_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbi_aijc
end do i_aibjcjbi_aijc
end do a_aibjcjbi_aijc
end do j_aibjcjbi_aijc
end do c_aibjcjbi_aijc
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibiakdi_ibkd: do d = n0d, n1d
k_aibiakdi_ibkd: do k = n0k, n1k
b_aibiakdi_ibkd: do b = n0b, n1b
if (b == d) cycle b_aibiakdi_ibkd
i1 = min(k - 1, n1ijl)
i_aibiakdi_ibkd: do i = n0ijl, i1
if (i == k) cycle i_aibiakdi_ibkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiakdi_ibkd(t2, &
 nocc, nactive, i, b, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdi_ibkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdi_ibkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakdi_ibkd
end do i_aibiakdi_ibkd
end do b_aibiakdi_ibkd
end do k_aibiakdi_ibkd
end do d_aibiakdi_ibkd
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibiakdi_aibkd: do d = n0d, n1d
k_aibiakdi_aibkd: do k = n0k, n1k
b_aibiakdi_aibkd: do b = n0b, n1b
if (b == d) cycle b_aibiakdi_aibkd
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdi_aibkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdi_aibkd
i1 = min(k - 1, n1ijl)
i_aibiakdi_aibkd: do i = n0ijl, i1
if (i == k) cycle i_aibiakdi_aibkd
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiakdi_aibkd(t2, nocc, nactive, a, &
 i, b, k, d)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakdi_aibkd
end do a_aibiakdi_aibkd
end do b_aibiakdi_aibkd
end do k_aibiakdi_aibkd
end do d_aibiakdi_aibkd
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_bjd: do d = n0d, n1d
b_aibjaidj_bjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_bjd
j_aibjaidj_bjd: do j = n0jl, n1jl
nn0i = max(j + 1, n0ik)
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaidj_bjd(t2, &
 nocc, nactive, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_bjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_bjd
i0 = max(j + 1, n0ik)
i_aibjaidj_bjd: do i = i0, n1ik
if (i == j) cycle i_aibjaidj_bjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_bjd
end do a_aibjaidj_bjd
end do j_aibjaidj_bjd
end do b_aibjaidj_bjd
end do d_aibjaidj_bjd
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_ibjd: do d = n0d, n1d
b_aibjaidj_ibjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_ibjd
j_aibjaidj_ibjd: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjaidj_ibjd: do i = i0, n1ik
if (i == j) cycle i_aibjaidj_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaidj_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_ibjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidj_ibjd
end do i_aibjaidj_ibjd
end do j_aibjaidj_ibjd
end do b_aibjaidj_ibjd
end do d_aibjaidj_ibjd
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_abjd: do d = n0d, n1d
b_aibjaidj_abjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_abjd
j_aibjaidj_abjd: do j = n0jl, n1jl
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_abjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_abjd
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaidj_abjd(t2, &
 nocc, nactive, a, b, j, d)
i0 = max(j + 1, n0ik)
i_aibjaidj_abjd: do i = i0, n1ik
if (i == j) cycle i_aibjaidj_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_abjd
end do a_aibjaidj_abjd
end do j_aibjaidj_abjd
end do b_aibjaidj_abjd
end do d_aibjaidj_abjd
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_aibd: do d = n0d, n1d
b_aibjaidj_aibd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_aibd
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_aibd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_aibd
i_aibjaidj_aibd: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaidj_aibd(t2, &
 nocc, nactive, a, i, b, d)
j1 = min(i - 1, n1jl)
j_aibjaidj_aibd: do j = n0jl, j1
if (j == i) cycle j_aibjaidj_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjaidj_aibd
end do i_aibjaidj_aibd
end do a_aibjaidj_aibd
end do b_aibjaidj_aibd
end do d_aibjaidj_aibd
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibiaidl_ibdl: do d = n0d, n1d
l_aibiaidl_ibdl: do l = n0l, n1l
b_aibiaidl_ibdl: do b = n0b, n1b
if (b == d) cycle b_aibiaidl_ibdl
i0 = max(l + 1, n0ijk)
i_aibiaidl_ibdl: do i = i0, n1ijk
if (i == l) cycle i_aibiaidl_ibdl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiaidl_ibdl(t2, &
 nocc, nactive, i, b, d, l)
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidl_ibdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidl_ibdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiaidl_ibdl
end do i_aibiaidl_ibdl
end do b_aibiaidl_ibdl
end do l_aibiaidl_ibdl
end do d_aibiaidl_ibdl
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibiaidl_aibdl: do d = n0d, n1d
l_aibiaidl_aibdl: do l = n0l, n1l
b_aibiaidl_aibdl: do b = n0b, n1b
if (b == d) cycle b_aibiaidl_aibdl
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidl_aibdl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidl_aibdl
i0 = max(l + 1, n0ijk)
i_aibiaidl_aibdl: do i = i0, n1ijk
if (i == l) cycle i_aibiaidl_aibdl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiaidl_aibdl(t2, nocc, nactive, a, &
 i, b, d, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidl_aibdl
end do a_aibiaidl_aibdl
end do b_aibiaidl_aibdl
end do l_aibiaidl_aibdl
end do d_aibiaidl_aibdl
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_bjd: do d = n0d, n1d
b_aibjajdi_bjd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_bjd
j_aibjajdi_bjd: do j = n0jk, n1jk
nn1i = min(j - 1, n1il)
nn0a = max(b + 1, d + 1, n0ac)
if ((nn1i .ge. n0il).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajdi_bjd(t2, &
 nocc, nactive, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_bjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_bjd
i1 = min(j - 1, n1il)
i_aibjajdi_bjd: do i = n0il, i1
if (i == j) cycle i_aibjajdi_bjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdi_bjd
end do a_aibjajdi_bjd
end do j_aibjajdi_bjd
end do b_aibjajdi_bjd
end do d_aibjajdi_bjd
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_ibjd: do d = n0d, n1d
b_aibjajdi_ibjd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_ibjd
j_aibjajdi_ibjd: do j = n0jk, n1jk
i1 = min(j - 1, n1il)
i_aibjajdi_ibjd: do i = n0il, i1
if (i == j) cycle i_aibjajdi_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajdi_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_ibjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdi_ibjd
end do i_aibjajdi_ibjd
end do j_aibjajdi_ibjd
end do b_aibjajdi_ibjd
end do d_aibjajdi_ibjd
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_abjd: do d = n0d, n1d
b_aibjajdi_abjd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_abjd
j_aibjajdi_abjd: do j = n0jk, n1jk
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_abjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_abjd
nn1i = min(j - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajdi_abjd(t2, &
 nocc, nactive, a, b, j, d)
i1 = min(j - 1, n1il)
i_aibjajdi_abjd: do i = n0il, i1
if (i == j) cycle i_aibjajdi_abjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdi_abjd
end do a_aibjajdi_abjd
end do j_aibjajdi_abjd
end do b_aibjajdi_abjd
end do d_aibjajdi_abjd
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_aibd: do d = n0d, n1d
b_aibjajdi_aibd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_aibd
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_aibd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_aibd
i1 = min(j - 1, n1il)
i_aibjajdi_aibd: do i = n0il, i1
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajdi_aibd(t2, &
 nocc, nactive, a, i, b, d)
j_aibjajdi_aibd: do j = n0jk, n1jk
if (j == i) cycle j_aibjajdi_aibd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajdi_aibd
end do i_aibjajdi_aibd
end do a_aibjajdi_aibd
end do b_aibjajdi_aibd
end do d_aibjajdi_aibd
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickai_ibck: do c = n0c, n1c
k_aibickai_ibck: do k = n0k, n1k
b_aibickai_ibck: do b = n0b, n1b
if (b == c) cycle b_aibickai_ibck
i1 = min(k - 1, n1ijl)
i_aibickai_ibck: do i = n0ijl, i1
if (i == k) cycle i_aibickai_ibck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibickai_ibck(t2, &
 nocc, nactive, i, b, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickai_ibck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickai_ibck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibickai_ibck
end do i_aibickai_ibck
end do b_aibickai_ibck
end do k_aibickai_ibck
end do c_aibickai_ibck
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickai_aibck: do c = n0c, n1c
k_aibickai_aibck: do k = n0k, n1k
b_aibickai_aibck: do b = n0b, n1b
if (b == c) cycle b_aibickai_aibck
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickai_aibck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickai_aibck
i1 = min(k - 1, n1ijl)
i_aibickai_aibck: do i = n0ijl, i1
if (i == k) cycle i_aibickai_aibck
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibickai_aibck(t2, nocc, nactive, a, &
 i, b, c, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickai_aibck
end do a_aibickai_aibck
end do b_aibickai_aibck
end do k_aibickai_aibck
end do c_aibickai_aibck
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj_bjc: do c = n0c, n1c
b_aibjciaj_bjc: do b = n0b, n1b
if (b == c) cycle b_aibjciaj_bjc
j_aibjciaj_bjc: do j = n0jl, n1jl
nn0i = max(j + 1, n0ik)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1ik .ge. nn0i).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjciaj_bjc(t2, &
 nocc, nactive, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_bjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_bjc
i0 = max(j + 1, n0ik)
i_aibjciaj_bjc: do i = i0, n1ik
if (i == j) cycle i_aibjciaj_bjc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj_bjc
end do a_aibjciaj_bjc
end do j_aibjciaj_bjc
end do b_aibjciaj_bjc
end do c_aibjciaj_bjc
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj_ibjc: do c = n0c, n1c
b_aibjciaj_ibjc: do b = n0b, n1b
if (b == c) cycle b_aibjciaj_ibjc
j_aibjciaj_ibjc: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjciaj_ibjc: do i = i0, n1ik
if (i == j) cycle i_aibjciaj_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjciaj_ibjc(t2, &
 nocc, nactive, i, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_ibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_ibjc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjciaj_ibjc
end do i_aibjciaj_ibjc
end do j_aibjciaj_ibjc
end do b_aibjciaj_ibjc
end do c_aibjciaj_ibjc
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj_abjc: do c = n0c, n1c
b_aibjciaj_abjc: do b = n0b, n1b
if (b == c) cycle b_aibjciaj_abjc
j_aibjciaj_abjc: do j = n0jl, n1jl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_abjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_abjc
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjciaj_abjc(t2, &
 nocc, nactive, a, b, j, c)
i0 = max(j + 1, n0ik)
i_aibjciaj_abjc: do i = i0, n1ik
if (i == j) cycle i_aibjciaj_abjc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj_abjc
end do a_aibjciaj_abjc
end do j_aibjciaj_abjc
end do b_aibjciaj_abjc
end do c_aibjciaj_abjc
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj_aibc: do c = n0c, n1c
b_aibjciaj_aibc: do b = n0b, n1b
if (b == c) cycle b_aibjciaj_aibc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_aibc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_aibc
i_aibjciaj_aibc: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjciaj_aibc(t2, &
 nocc, nactive, a, i, b, c)
j1 = min(i - 1, n1jl)
j_aibjciaj_aibc: do j = n0jl, j1
if (j == i) cycle j_aibjciaj_aibc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjciaj_aibc
end do i_aibjciaj_aibc
end do a_aibjciaj_aibc
end do b_aibjciaj_aibc
end do c_aibjciaj_aibc
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibicial_ibcl: do l = n0l, n1l
c_aibicial_ibcl: do c = n0c, n1c
b_aibicial_ibcl: do b = n0b, n1b
if (b == c) cycle b_aibicial_ibcl
i0 = max(l + 1, n0ijk)
i_aibicial_ibcl: do i = i0, n1ijk
if (i == l) cycle i_aibicial_ibcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibicial_ibcl(t2, &
 nocc, nactive, i, b, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibicial_ibcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibicial_ibcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibicial_ibcl
end do i_aibicial_ibcl
end do b_aibicial_ibcl
end do c_aibicial_ibcl
end do l_aibicial_ibcl
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibicial_aibcl: do l = n0l, n1l
c_aibicial_aibcl: do c = n0c, n1c
b_aibicial_aibcl: do b = n0b, n1b
if (b == c) cycle b_aibicial_aibcl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibicial_aibcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibicial_aibcl
i0 = max(l + 1, n0ijk)
i_aibicial_aibcl: do i = i0, n1ijk
if (i == l) cycle i_aibicial_aibcl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibicial_aibcl(t2, nocc, nactive, a, &
 i, b, c, l)
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicial_aibcl
end do a_aibicial_aibcl
end do b_aibicial_aibcl
end do c_aibicial_aibcl
end do l_aibicial_aibcl
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_bjc: do c = n0c, n1c
b_aibjcjai_bjc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_bjc
j_aibjcjai_bjc: do j = n0jk, n1jk
nn1i = min(j - 1, n1il)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1i .ge. n0il).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjai_bjc(t2, &
 nocc, nactive, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_bjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_bjc
i1 = min(j - 1, n1il)
i_aibjcjai_bjc: do i = n0il, i1
if (i == j) cycle i_aibjcjai_bjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_bjc
end do a_aibjcjai_bjc
end do j_aibjcjai_bjc
end do b_aibjcjai_bjc
end do c_aibjcjai_bjc
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_ibjc: do c = n0c, n1c
b_aibjcjai_ibjc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_ibjc
j_aibjcjai_ibjc: do j = n0jk, n1jk
i1 = min(j - 1, n1il)
i_aibjcjai_ibjc: do i = n0il, i1
if (i == j) cycle i_aibjcjai_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjai_ibjc(t2, &
 nocc, nactive, i, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_ibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_ibjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjai_ibjc
end do i_aibjcjai_ibjc
end do j_aibjcjai_ibjc
end do b_aibjcjai_ibjc
end do c_aibjcjai_ibjc
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_abjc: do c = n0c, n1c
b_aibjcjai_abjc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_abjc
j_aibjcjai_abjc: do j = n0jk, n1jk
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_abjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_abjc
nn1i = min(j - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjai_abjc(t2, &
 nocc, nactive, a, b, j, c)
i1 = min(j - 1, n1il)
i_aibjcjai_abjc: do i = n0il, i1
if (i == j) cycle i_aibjcjai_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_abjc
end do a_aibjcjai_abjc
end do j_aibjcjai_abjc
end do b_aibjcjai_abjc
end do c_aibjcjai_abjc
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_aibc: do c = n0c, n1c
b_aibjcjai_aibc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_aibc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_aibc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_aibc
i1 = min(j - 1, n1il)
i_aibjcjai_aibc: do i = n0il, i1
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcjai_aibc(t2, &
 nocc, nactive, a, i, b, c)
j_aibjcjai_aibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjai_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjai_aibc
end do i_aibjcjai_aibc
end do a_aibjcjai_aibc
end do b_aibjcjai_aibc
end do c_aibjcjai_aibc
end subroutine ccjac_22_tripletmp_dav_part5
end module ccjac_block_22_tripletmp_dav_part5
