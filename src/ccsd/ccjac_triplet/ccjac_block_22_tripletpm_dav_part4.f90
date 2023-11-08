module ccjac_block_22_tripletpm_dav_part4
use eom_ccsd_22_tripletpm_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-06 14:26:30 UTC.
!
contains
subroutine ccjac_22_tripletpm_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0ac, n0acd, n0bc, n0bd, n0ik
integer :: n0ikl, n0il, n0jk, n0jkl, n0jl
integer :: n1ac, n1acd, n1bc, n1bd, n1ik
integer :: n1ikl, n1il, n1jk, n1jkl, n1jl
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
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_aijl: do l = n0l, n1l
j_aibjajbl_aijl: do j = n0jk, n1jk
if (j == l) cycle j_aibjajbl_aijl
a_aibjajbl_aijl: do a = n0ac, n1ac
i0 = max(j + 1, n0i)
i_aibjajbl_aijl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjajbl_aijl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbl_aijl(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjajbl_aijl
end do i_aibjajbl_aijl
end do a_aibjajbl_aijl
end do j_aibjajbl_aijl
end do l_aibjajbl_aijl
!
! Elementary loop  2
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
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbl_aibl(t2, &
 nocc, nactive, a, i, b, l)
j1 = min(i - 1, n1jk)
j_aibjajbl_aibl: do j = n0jk, j1
if (j == i .or. j == l) cycle j_aibjajbl_aibl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbl_aibl
end do i_aibjajbl_aibl
end do a_aibjajbl_aibl
end do b_aibjajbl_aibl
end do l_aibjajbl_aibl
!
! Elementary loop  3
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
i0 = max(j + 1, n0ikl)
i_aibjcibi_aijc: do i = i0, n1ikl
if (i == j) cycle i_aibjcibi_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcibi_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibi_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibi_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibi_aijc
end do i_aibjcibi_aijc
end do a_aibjcibi_aijc
end do j_aibjcibi_aijc
end do c_aibjcibi_aijc
!
! Elementary loop  4
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
i0 = max(j + 1, n0ikl)
i_aibjcibi_aibjc: do i = i0, n1ikl
if (i == j) cycle i_aibjcibi_aibjc
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcibi_aibjc(t2, nocc, nactive, a, &
 i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibi_aibjc
end do a_aibjcibi_aibjc
end do j_aibjcibi_aibjc
end do b_aibjcibi_aibjc
end do c_aibjcibi_aibjc
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j
! Equalities: d == b, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjbi_ajc: do c = n0c, n1c
j_aibjcjbi_ajc: do j = n0jk, n1jk
a_aibjcjbi_ajc: do a = n0a, n1a
if (a == c) cycle a_aibjcjbi_ajc
nn0i = max(j + 1, n0il)
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjbi_ajc(t2, &
 nocc, nactive, a, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbi_ajc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbi_ajc
i0 = max(j + 1, n0il)
i_aibjcjbi_ajc: do i = i0, n1il
if (i == j) cycle i_aibjcjbi_ajc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbi_ajc
end do b_aibjcjbi_ajc
end do a_aibjcjbi_ajc
end do j_aibjcjbi_ajc
end do c_aibjcjbi_ajc
!
! Elementary loop  6
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
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjbi_aibc(t2, &
 nocc, nactive, a, i, b, c)
j1 = min(i - 1, n1jk)
j_aibjcjbi_aibc: do j = n0jk, j1
if (j == i) cycle j_aibjcjbi_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbi_aibc
end do i_aibjcjbi_aibc
end do a_aibjcjbi_aibc
end do b_aibjcjbi_aibc
end do c_aibjcjbi_aibc
!
! Elementary loop  7
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
nn0i = max(j + 1, n0il)
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjbi_abjc(t2, &
 nocc, nactive, a, b, j, c)
i0 = max(j + 1, n0il)
i_aibjcjbi_abjc: do i = i0, n1il
if (i == j) cycle i_aibjcjbi_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbi_abjc
end do a_aibjcjbi_abjc
end do j_aibjcjbi_abjc
end do b_aibjcjbi_abjc
end do c_aibjcjbi_abjc
!
! Elementary loop  8
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
i0 = max(j + 1, n0il)
i_aibjcjbi_aijc: do i = i0, n1il
if (i == j) cycle i_aibjcjbi_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjbi_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbi_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbi_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbi_aijc
end do i_aibjcjbi_aijc
end do a_aibjcjbi_aijc
end do j_aibjcjbi_aijc
end do c_aibjcjbi_aijc
!
! Elementary loop  9
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
nn1j = min(i - 1, n1jl)
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcibj_aic(t2, &
 nocc, nactive, a, i, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_aic: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_aic
j1 = min(i - 1, n1jl)
j_aibjcibj_aic: do j = n0jl, j1
if (j == i) cycle j_aibjcibj_aic
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcibj_aic
end do b_aibjcibj_aic
end do i_aibjcibj_aic
end do a_aibjcibj_aic
end do c_aibjcibj_aic
!
! Elementary loop  10
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
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcibj_abjc(t2, &
 nocc, nactive, a, b, j, c)
i0 = max(j + 1, n0ik)
i_aibjcibj_abjc: do i = i0, n1ik
if (i == j) cycle i_aibjcibj_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_abjc
end do a_aibjcibj_abjc
end do j_aibjcibj_abjc
end do b_aibjcibj_abjc
end do c_aibjcibj_abjc
!
! Elementary loop  11
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
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcibj_aibc(t2, &
 nocc, nactive, a, i, b, c)
j1 = min(i - 1, n1jl)
j_aibjcibj_aibc: do j = n0jl, j1
if (j == i) cycle j_aibjcibj_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcibj_aibc
end do i_aibjcibj_aibc
end do a_aibjcibj_aibc
end do b_aibjcibj_aibc
end do c_aibjcibj_aibc
!
! Elementary loop  12
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
i0 = max(j + 1, n0ik)
i_aibjcibj_aijc: do i = i0, n1ik
if (i == j) cycle i_aibjcibj_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcibj_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibj_aijc
end do i_aibjcibj_aijc
end do a_aibjcibj_aijc
end do j_aibjcibj_aijc
end do c_aibjcibj_aijc
!
! Elementary loop  13
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
i0 = max(j + 1, n0i)
i_aibjcjbj_aijc: do i = i0, n1i
if (i == j) cycle i_aibjcjbj_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjbj_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbj_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbj_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbj_aijc
end do i_aibjcjbj_aijc
end do a_aibjcjbj_aijc
end do j_aibjcjbj_aijc
end do c_aibjcjbj_aijc
!
! Elementary loop  14
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
i0 = max(j + 1, n0i)
i_aibjcjbj_aibjc: do i = i0, n1i
if (i == j) cycle i_aibjcjbj_aibjc
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjbj_aibjc(t2, nocc, nactive, a, &
 i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbj_aibjc
end do a_aibjcjbj_aibjc
end do j_aibjcjbj_aibjc
end do b_aibjcjbj_aibjc
end do c_aibjcjbj_aibjc
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjbidi_aijd: do d = n0d, n1d
j_aibjbidi_aijd: do j = n0j, n1j
a_aibjbidi_aijd: do a = n0a, n1a
if (a == d) cycle a_aibjbidi_aijd
i0 = max(j + 1, n0ikl)
i_aibjbidi_aijd: do i = i0, n1ikl
if (i == j) cycle i_aibjbidi_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbidi_aijd(t2, &
 nocc, nactive, a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidi_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidi_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidi_aijd
end do i_aibjbidi_aijd
end do a_aibjbidi_aijd
end do j_aibjbidi_aijd
end do d_aibjbidi_aijd
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjbidi_aibjd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbidi_aibjd: do b = b0, n1bc
if (b == d) cycle b_aibjbidi_aibjd
j_aibjbidi_aibjd: do j = n0j, n1j
a0 = max(b + 1, n0a)
a_aibjbidi_aibjd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidi_aibjd
i0 = max(j + 1, n0ikl)
i_aibjbidi_aibjd: do i = i0, n1ikl
if (i == j) cycle i_aibjbidi_aibjd
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbidi_aibjd(t2, nocc, nactive, a, &
 i, b, j, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidi_aibjd
end do a_aibjbidi_aibjd
end do j_aibjbidi_aibjd
end do b_aibjbidi_aibjd
end do d_aibjbidi_aibjd
!
! Elementary loop  17
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
nn1j = min(i - 1, n1jk)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1j .ge. n0jk).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbjdi_aid(t2, &
 nocc, nactive, a, i, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdi_aid: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdi_aid
j1 = min(i - 1, n1jk)
j_aibjbjdi_aid: do j = n0jk, j1
if (j == i) cycle j_aibjbjdi_aid
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdi_aid
end do b_aibjbjdi_aid
end do i_aibjbjdi_aid
end do a_aibjbjdi_aid
end do d_aibjbjdi_aid
!
! Elementary loop  18
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
i0 = max(j + 1, n0il)
i_aibjbjdi_aijd: do i = i0, n1il
if (i == j) cycle i_aibjbjdi_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbjdi_aijd(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdi_aijd
end do i_aibjbjdi_aijd
end do a_aibjbjdi_aijd
end do j_aibjbjdi_aijd
end do d_aibjbjdi_aijd
!
! Elementary loop  19
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
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbjdi_aibd(t2, &
 nocc, nactive, a, i, b, d)
j1 = min(i - 1, n1jk)
j_aibjbjdi_aibd: do j = n0jk, j1
if (j == i) cycle j_aibjbjdi_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdi_aibd
end do i_aibjbjdi_aibd
end do a_aibjbjdi_aibd
end do b_aibjbjdi_aibd
end do d_aibjbjdi_aibd
!
! Elementary loop  20
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
nn0i = max(j + 1, n0il)
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbjdi_abjd(t2, &
 nocc, nactive, a, b, j, d)
i0 = max(j + 1, n0il)
i_aibjbjdi_abjd: do i = i0, n1il
if (i == j) cycle i_aibjbjdi_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_abjd
end do a_aibjbjdi_abjd
end do j_aibjbjdi_abjd
end do b_aibjbjdi_abjd
end do d_aibjbjdi_abjd
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_ajd: do d = n0d, n1d
j_aibjbidj_ajd: do j = n0jl, n1jl
a_aibjbidj_ajd: do a = n0a, n1a
if (a == d) cycle a_aibjbidj_ajd
nn0i = max(j + 1, n0ik)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbidj_ajd(t2, &
 nocc, nactive, a, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidj_ajd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidj_ajd
i0 = max(j + 1, n0ik)
i_aibjbidj_ajd: do i = i0, n1ik
if (i == j) cycle i_aibjbidj_ajd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_ajd
end do b_aibjbidj_ajd
end do a_aibjbidj_ajd
end do j_aibjbidj_ajd
end do d_aibjbidj_ajd
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_aijd: do d = n0d, n1d
j_aibjbidj_aijd: do j = n0jl, n1jl
a_aibjbidj_aijd: do a = n0a, n1a
if (a == d) cycle a_aibjbidj_aijd
i0 = max(j + 1, n0ik)
i_aibjbidj_aijd: do i = i0, n1ik
if (i == j) cycle i_aibjbidj_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbidj_aijd(t2, &
 nocc, nactive, a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidj_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidj_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidj_aijd
end do i_aibjbidj_aijd
end do a_aibjbidj_aijd
end do j_aibjbidj_aijd
end do d_aibjbidj_aijd
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_abjd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbidj_abjd: do b = b0, n1bc
if (b == d) cycle b_aibjbidj_abjd
j_aibjbidj_abjd: do j = n0jl, n1jl
a0 = max(b + 1, n0a)
a_aibjbidj_abjd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidj_abjd
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbidj_abjd(t2, &
 nocc, nactive, a, b, j, d)
i0 = max(j + 1, n0ik)
i_aibjbidj_abjd: do i = i0, n1ik
if (i == j) cycle i_aibjbidj_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_abjd
end do a_aibjbidj_abjd
end do j_aibjbidj_abjd
end do b_aibjbidj_abjd
end do d_aibjbidj_abjd
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_aibd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbidj_aibd: do b = b0, n1bc
if (b == d) cycle b_aibjbidj_aibd
a0 = max(b + 1, n0a)
a_aibjbidj_aibd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidj_aibd
i_aibjbidj_aibd: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbidj_aibd(t2, &
 nocc, nactive, a, i, b, d)
j1 = min(i - 1, n1jl)
j_aibjbidj_aibd: do j = n0jl, j1
if (j == i) cycle j_aibjbidj_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbidj_aibd
end do i_aibjbidj_aibd
end do a_aibjbidj_aibd
end do b_aibjbidj_aibd
end do d_aibjbidj_aibd
!
! Elementary loop  25
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
i0 = max(j + 1, n0i)
i_aibjbjdj_aijd: do i = i0, n1i
if (i == j) cycle i_aibjbjdj_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbjdj_aijd(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdj_aijd
end do i_aibjbjdj_aijd
end do a_aibjbjdj_aijd
end do j_aibjbjdj_aijd
end do d_aibjbjdj_aijd
!
! Elementary loop  26
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
i0 = max(j + 1, n0i)
i_aibjbjdj_aibjd: do i = i0, n1i
if (i == j) cycle i_aibjbjdj_aibjd
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbjdj_aibjd(t2, nocc, nactive, a, &
 i, b, j, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdj_aibjd
end do a_aibjbjdj_aibjd
end do j_aibjbjdj_aibjd
end do b_aibjbjdj_aibjd
end do d_aibjbjdj_aibjd
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aibjakai_abjk: do k = n0k, n1k
b_aibjakai_abjk: do b = n0b, n1b
j_aibjakai_abjk: do j = n0j, n1j
if (j == k) cycle j_aibjakai_abjk
a0 = max(b + 1, n0acd)
a_aibjakai_abjk: do a = a0, n1acd
if (a == b) cycle a_aibjakai_abjk
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakai_abjk(t2, &
 nocc, nactive, a, b, j, k)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakai_abjk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakai_abjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakai_abjk
end do a_aibjakai_abjk
end do j_aibjakai_abjk
end do b_aibjakai_abjk
end do k_aibjakai_abjk
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aibjakai_aibjk: do k = n0k, n1k
b_aibjakai_aibjk: do b = n0b, n1b
j_aibjakai_aibjk: do j = n0j, n1j
if (j == k) cycle j_aibjakai_aibjk
a0 = max(b + 1, n0acd)
a_aibjakai_aibjk: do a = a0, n1acd
if (a == b) cycle a_aibjakai_aibjk
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjakai_aibjk: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjakai_aibjk
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakai_aibjk(t2, nocc, nactive, a, &
 i, b, j, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakai_aibjk
end do a_aibjakai_aibjk
end do j_aibjakai_aibjk
end do b_aibjakai_aibjk
end do k_aibjakai_aibjk
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjaial_abjl: do l = n0l, n1l
b_aibjaial_abjl: do b = n0b, n1b
j_aibjaial_abjl: do j = n0j, n1j
if (j == l) cycle j_aibjaial_abjl
a0 = max(b + 1, n0acd)
a_aibjaial_abjl: do a = a0, n1acd
if (a == b) cycle a_aibjaial_abjl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaial_abjl(t2, &
 nocc, nactive, a, b, j, l)
i0 = max(j + 1, l + 1, n0ik)
i_aibjaial_abjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaial_abjl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaial_abjl
end do a_aibjaial_abjl
end do j_aibjaial_abjl
end do b_aibjaial_abjl
end do l_aibjaial_abjl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjaial_aibjl: do l = n0l, n1l
b_aibjaial_aibjl: do b = n0b, n1b
j_aibjaial_aibjl: do j = n0j, n1j
if (j == l) cycle j_aibjaial_aibjl
a0 = max(b + 1, n0acd)
a_aibjaial_aibjl: do a = a0, n1acd
if (a == b) cycle a_aibjaial_aibjl
i0 = max(j + 1, l + 1, n0ik)
i_aibjaial_aibjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaial_aibjl
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaial_aibjl(t2, nocc, nactive, a, &
 i, b, j, l)
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaial_aibjl
end do a_aibjaial_aibjl
end do j_aibjaial_aibjl
end do b_aibjaial_aibjl
end do l_aibjaial_aibjl
end subroutine ccjac_22_tripletpm_dav_part4
end module ccjac_block_22_tripletpm_dav_part4
