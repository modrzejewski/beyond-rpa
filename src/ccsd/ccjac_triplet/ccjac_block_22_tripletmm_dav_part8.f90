module ccjac_block_22_tripletmm_dav_part8
use eom_ccsd_22_tripletmm_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-06 14:17:58 UTC.
!
contains
subroutine ccjac_22_tripletmm_dav_part8(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0b, nn0i, nn1b, nn1i, nn1j
integer :: a0, b0, b1, i0, i1, j1
integer :: n0bc, n0bd, n0cd, n0ijk, n0ijl
integer :: n0ik, n0ikl, n0il, n0jk, n0jkl
integer :: n0jl
integer :: n1bc, n1bd, n1cd, n1ijk, n1ijl
integer :: n1ik, n1ikl, n1il, n1jk, n1jkl
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
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0cd = max(n0c, n0d)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_ad: do d = n0d, n1d
a_aibjbjdi_ad: do a = n0a, n1a
if (a == d) cycle a_aibjbjdi_ad
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jk .ge. n0jk).and. (nn1b .ge. nn0b).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjdi_ad(t2, &
 nocc, nactive, a, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdi_ad: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdi_ad
j_aibjbjdi_ad: do j = n0jk, n1jk
i_aibjbjdi_ad: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_ad
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_ad
end do j_aibjbjdi_ad
end do b_aibjbjdi_ad
end do a_aibjbjdi_ad
end do d_aibjbjdi_ad
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_aid: do d = n0d, n1d
a_aibjbjdi_aid: do a = n0a, n1a
if (a == d) cycle a_aibjbjdi_aid
i_aibjbjdi_aid: do i = n0il, n1il
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jk .ge. n0jk).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjdi_aid(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdi_aid
end do b_aibjbjdi_aid
end do i_aibjbjdi_aid
end do a_aibjbjdi_aid
end do d_aibjbjdi_aid
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_ajd: do d = n0d, n1d
j_aibjbjdi_ajd: do j = n0jk, n1jk
a_aibjbjdi_ajd: do a = n0a, n1a
if (a == d) cycle a_aibjbjdi_ajd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjdi_ajd(t2, &
 nocc, nactive, a, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdi_ajd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdi_ajd
i_aibjbjdi_ajd: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_ajd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_ajd
end do b_aibjbjdi_ajd
end do a_aibjbjdi_ajd
end do j_aibjbjdi_ajd
end do d_aibjbjdi_ajd
!
! Elementary loop  4
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_abd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdi_abd: do b = b0, n1bc
if (b == d) cycle b_aibjbjdi_abd
a0 = max(b + 1, n0a)
a_aibjbjdi_abd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdi_abd
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjdi_abd(t2, &
 nocc, nactive, a, b, d)
j_aibjbjdi_abd: do j = n0jk, n1jk
i_aibjbjdi_abd: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_abd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_abd
end do j_aibjbjdi_abd
end do a_aibjbjdi_abd
end do b_aibjbjdi_abd
end do d_aibjbjdi_abd
!
! Elementary loop  5
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
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjdi_abjd(t2, &
 nocc, nactive, a, b, j, d)
i_aibjbjdi_abjd: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_abjd
end do a_aibjbjdi_abjd
end do j_aibjbjdi_abjd
end do b_aibjbjdi_abjd
end do d_aibjbjdi_abjd
!
! Elementary loop  6
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
i_aibjbjdi_aibd: do i = n0il, n1il
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjdi_aibd(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdi_aibd
end do i_aibjbjdi_aibd
end do a_aibjbjdi_aibd
end do b_aibjbjdi_aibd
end do d_aibjbjdi_aibd
!
! Elementary loop  7
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
i_aibjbjdi_aijd: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjdi_aijd(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdi_aijd
end do i_aibjbjdi_aijd
end do a_aibjbjdi_aijd
end do j_aibjbjdi_aijd
end do d_aibjbjdi_aijd
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i
! Equalities: c == b, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjbjdj_aijd: do d = n0d, n1d
j_aibjbjdj_aijd: do j = n0jkl, n1jkl
a_aibjbjdj_aijd: do a = n0a, n1a
if (a == d) cycle a_aibjbjdj_aijd
i_aibjbjdj_aijd: do i = n0i, n1i
if (i == j) cycle i_aibjbjdj_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjdj_aijd(t2, &
 nocc, nactive, a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdj_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdj_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdj_aijd
end do i_aibjbjdj_aijd
end do a_aibjbjdj_aijd
end do j_aibjbjdj_aijd
end do d_aibjbjdj_aijd
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i
! Equalities: c == b, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjbjdj_aibjd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdj_aibjd: do b = b0, n1bc
if (b == d) cycle b_aibjbjdj_aibjd
j_aibjbjdj_aibjd: do j = n0jkl, n1jkl
a0 = max(b + 1, n0a)
a_aibjbjdj_aibjd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdj_aibjd
i_aibjbjdj_aibjd: do i = n0i, n1i
if (i == j) cycle i_aibjbjdj_aibjd
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjdj_aibjd(t2, nocc, nactive, a, &
 i, b, j, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdj_aibjd
end do a_aibjbjdj_aibjd
end do j_aibjbjdj_aibjd
end do b_aibjbjdj_aibjd
end do d_aibjbjdj_aibjd
!
! Elementary loop  10
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, l
! Equalities: d == c, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibicicl_aibcl: do l = n0l, n1l
c_aibicicl_aibcl: do c = n0cd, n1cd
b_aibicicl_aibcl: do b = n0b, n1b
if (b == c) cycle b_aibicicl_aibcl
a0 = max(b + 1, n0a)
a_aibicicl_aibcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicicl_aibcl
i0 = max(l + 1, n0ijk)
i_aibicicl_aibcl: do i = i0, n1ijk
if (i == l) cycle i_aibicicl_aibcl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibicicl_aibcl(t2, nocc, nactive, a, &
 i, b, c, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicicl_aibcl
end do a_aibicicl_aibcl
end do b_aibicicl_aibcl
end do c_aibicicl_aibcl
end do l_aibicicl_aibcl
!
! Elementary loop  11
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj_abc: do c = n0cd, n1cd
b_aibjcicj_abc: do b = n0b, n1b
if (b == c) cycle b_aibjcicj_abc
a0 = max(b + 1, n0a)
a_aibjcicj_abc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicj_abc
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcicj_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcicj_abc: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjcicj_abc: do i = i0, n1ik
if (i == j) cycle i_aibjcicj_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicj_abc
end do j_aibjcicj_abc
end do a_aibjcicj_abc
end do b_aibjcicj_abc
end do c_aibjcicj_abc
!
! Elementary loop  12
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj_abjc: do c = n0cd, n1cd
b_aibjcicj_abjc: do b = n0b, n1b
if (b == c) cycle b_aibjcicj_abjc
j_aibjcicj_abjc: do j = n0jl, n1jl
a0 = max(b + 1, n0a)
a_aibjcicj_abjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicj_abjc
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcicj_abjc(t2, &
 nocc, nactive, a, b, j, c)
i0 = max(j + 1, n0ik)
i_aibjcicj_abjc: do i = i0, n1ik
if (i == j) cycle i_aibjcicj_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicj_abjc
end do a_aibjcicj_abjc
end do j_aibjcicj_abjc
end do b_aibjcicj_abjc
end do c_aibjcicj_abjc
!
! Elementary loop  13
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj_aibc: do c = n0cd, n1cd
b_aibjcicj_aibc: do b = n0b, n1b
if (b == c) cycle b_aibjcicj_aibc
a0 = max(b + 1, n0a)
a_aibjcicj_aibc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicj_aibc
i_aibjcicj_aibc: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcicj_aibc(t2, &
 nocc, nactive, a, i, b, c)
j1 = min(i - 1, n1jl)
j_aibjcicj_aibc: do j = n0jl, j1
if (j == i) cycle j_aibjcicj_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcicj_aibc
end do i_aibjcicj_aibc
end do a_aibjcicj_aibc
end do b_aibjcicj_aibc
end do c_aibjcicj_aibc
!
! Elementary loop  14
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k
! Equalities: d == c, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickci_aibck: do c = n0cd, n1cd
k_aibickci_aibck: do k = n0k, n1k
b_aibickci_aibck: do b = n0b, n1b
if (b == c) cycle b_aibickci_aibck
a0 = max(b + 1, n0a)
a_aibickci_aibck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibickci_aibck
i1 = min(k - 1, n1ijl)
i_aibickci_aibck: do i = n0ijl, i1
if (i == k) cycle i_aibickci_aibck
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibickci_aibck(t2, nocc, nactive, a, &
 i, b, c, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickci_aibck
end do a_aibickci_aibck
end do b_aibickci_aibck
end do k_aibickci_aibck
end do c_aibickci_aibck
!
! Elementary loop  15
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
if ((n1jk .ge. n0jk).and. (nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjci_abc(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjci_abc
end do j_aibjcjci_abc
end do a_aibjcjci_abc
end do b_aibjcjci_abc
end do c_aibjcjci_abc
!
! Elementary loop  16
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
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjci_abjc(t2, &
 nocc, nactive, a, b, j, c)
i1 = min(j - 1, n1il)
i_aibjcjci_abjc: do i = n0il, i1
if (i == j) cycle i_aibjcjci_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjci_abjc
end do a_aibjcjci_abjc
end do j_aibjcjci_abjc
end do b_aibjcjci_abjc
end do c_aibjcjci_abjc
!
! Elementary loop  17
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
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjci_aibc(t2, &
 nocc, nactive, a, i, b, c)
j_aibjcjci_aibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjci_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjci_aibc
end do i_aibjcjci_aibc
end do a_aibjcjci_aibc
end do b_aibjcjci_aibc
end do c_aibjcjci_aibc
!
! Elementary loop  18
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
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibicibl_aicl(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibicibl_aicl
end do i_aibicibl_aicl
end do a_aibicibl_aicl
end do c_aibicibl_aicl
end do l_aibicibl_aicl
!
! Elementary loop  19
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
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibicibl_aibcl(t2, nocc, nactive, a, &
 i, b, c, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibl_aibcl
end do a_aibicibl_aibcl
end do b_aibicibl_aibcl
end do c_aibicibl_aibcl
end do l_aibicibl_aibcl
!
! Elementary loop  20
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
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcibi_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibi_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibi_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibi_aijc
end do i_aibjcibi_aijc
end do a_aibjcibi_aijc
end do j_aibjcibi_aijc
end do c_aibjcibi_aijc
!
! Elementary loop  21
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
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcibi_aibjc(t2, nocc, nactive, a, &
 i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibi_aibjc
end do a_aibjcibi_aibjc
end do j_aibjcibi_aibjc
end do b_aibjcibi_aibjc
end do c_aibjcibi_aibjc
!
! Elementary loop  22
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
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcibj_ac(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_ac
end do j_aibjcibj_ac
end do b_aibjcibj_ac
end do a_aibjcibj_ac
end do c_aibjcibj_ac
!
! Elementary loop  23
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
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcibj_aic(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcibj_aic
end do b_aibjcibj_aic
end do i_aibjcibj_aic
end do a_aibjcibj_aic
end do c_aibjcibj_aic
!
! Elementary loop  24
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
if ((nn1b .ge. n0bd).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcibj_ajc(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_ajc
end do b_aibjcibj_ajc
end do a_aibjcibj_ajc
end do j_aibjcibj_ajc
end do c_aibjcibj_ajc
!
! Elementary loop  25
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
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcibj_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcibj_abc: do j = n0jl, n1jl
i_aibjcibj_abc: do i = n0ik, n1ik
if (i == j) cycle i_aibjcibj_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_abc
end do j_aibjcibj_abc
end do a_aibjcibj_abc
end do b_aibjcibj_abc
end do c_aibjcibj_abc
!
! Elementary loop  26
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
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcibj_abjc(t2, &
 nocc, nactive, a, b, j, c)
i_aibjcibj_abjc: do i = n0ik, n1ik
if (i == j) cycle i_aibjcibj_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_abjc
end do a_aibjcibj_abjc
end do j_aibjcibj_abjc
end do b_aibjcibj_abjc
end do c_aibjcibj_abjc
!
! Elementary loop  27
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
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcibj_aibc(t2, &
 nocc, nactive, a, i, b, c)
j_aibjcibj_aibc: do j = n0jl, n1jl
if (j == i) cycle j_aibjcibj_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcibj_aibc
end do i_aibjcibj_aibc
end do a_aibjcibj_aibc
end do b_aibjcibj_aibc
end do c_aibjcibj_aibc
!
! Elementary loop  28
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
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcibj_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibj_aijc
end do i_aibjcibj_aijc
end do a_aibjcibj_aijc
end do j_aibjcibj_aijc
end do c_aibjcibj_aijc
!
! Elementary loop  29
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
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibickbi_aick(t2, &
 nocc, nactive, a, i, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibickbi_aick: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibickbi_aick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibickbi_aick
end do i_aibickbi_aick
end do a_aibickbi_aick
end do k_aibickbi_aick
end do c_aibickbi_aick
!
! Elementary loop  30
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
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibickbi_aibck(t2, nocc, nactive, a, &
 i, b, c, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickbi_aibck
end do a_aibickbi_aibck
end do b_aibickbi_aibck
end do k_aibickbi_aibck
end do c_aibickbi_aibck
end subroutine ccjac_22_tripletmm_dav_part8
end module ccjac_block_22_tripletmm_dav_part8
