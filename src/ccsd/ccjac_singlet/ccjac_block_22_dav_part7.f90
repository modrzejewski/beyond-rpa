module ccjac_block_22_dav_part7
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
subroutine ccjac_22_dav_part7(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: nn0a, nn0c, nn0i, nn0j, nn0k, nn1a, nn1i, nn1j
integer :: a0, a1, c0, i0, i1, j0, j1, k0
integer :: n0ab, n0ad, n0bcd, n0ij, n0ijk
integer :: n0ijl, n0ik, n0ikl, n0il, n0jk
integer :: n0jkl, n0jl, n0kl
integer :: n1ab, n1ad, n1bcd, n1ij, n1ijk
integer :: n1ijl, n1ik, n1ikl, n1il, n1jk
integer :: n1jkl, n1jl, n1kl
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
n0bcd = max(n0b, n0c, n0d)
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
n1ab = min(n1a, n1b)
n1ad = min(n1a, n1d)
n1bcd = min(n1b, n1c, n1d)
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
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdi_aicd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcjdi_aicd: do c = c0, n1c
if (c == d) cycle c_aiajcjdi_aicd
a_aiajcjdi_aicd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcjdi_aicd
i_aiajcjdi_aicd: do i = n0il, n1il
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aiajcjdi_aicd(t2, nocc, nactive, &
 a, i, c, d)
j1 = min(i - 1, n1jk)
j_aiajcjdi_aicd: do j = n0jk, j1
if (j == i) cycle j_aiajcjdi_aicd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcjdi_aicd
end do i_aiajcjdi_aicd
end do a_aiajcjdi_aicd
end do c_aiajcjdi_aicd
end do d_aiajcjdi_aicd
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i
! Equalities: b == a, k == j, l == j
! No equalities independent of the above can hold.
!
d_aiajcjdj_aijcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcjdj_aijcd: do c = c0, n1c
if (c == d) cycle c_aiajcjdj_aijcd
j_aiajcjdj_aijcd: do j = n0jkl, n1jkl
a_aiajcjdj_aijcd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcjdj_aijcd
i0 = max(j + 1, n0i)
i_aiajcjdj_aijcd: do i = i0, n1i
if (i == j) cycle i_aiajcjdj_aijcd
jac_ibra_iket = eom_ccsd_22_trans_aiajcjdj_aijcd(t2, nocc, nactive, a, i, j, c, d)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdj_aijcd
end do a_aiajcjdj_aijcd
end do j_aiajcjdj_aijcd
end do c_aiajcjdj_aijcd
end do d_aiajcjdj_aijcd
!
! Elementary loop  3
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
i_aibicial_ibcl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibicial_ibcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibicial_ibcl(t2, nocc, nactive, &
 i, b, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibicial_ibcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibicial_ibcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibicial_ibcl
end do i_aibicial_ibcl
end do b_aibicial_ibcl
end do c_aibicial_ibcl
end do l_aibicial_ibcl
!
! Elementary loop  4
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
i_aibicial_aibcl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibicial_aibcl
jac_ibra_iket = eom_ccsd_22_trans_aibicial_aibcl(t2, nocc, nactive, a, i, b, c, l)
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicial_aibcl
end do a_aibicial_aibcl
end do b_aibicial_aibcl
end do c_aibicial_aibcl
end do l_aibicial_aibcl
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjciai_ibjc: do c = n0c, n1c
b_aibjciai_ibjc: do b = n0b, n1b
if (b == c) cycle b_aibjciai_ibjc
j_aibjciai_ibjc: do j = n0j, n1j
i_aibjciai_ibjc: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjciai_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjciai_ibjc(t2, nocc, nactive, &
 i, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciai_ibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciai_ibjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjciai_ibjc
end do i_aibjciai_ibjc
end do j_aibjciai_ibjc
end do b_aibjciai_ibjc
end do c_aibjciai_ibjc
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjciai_aibjc: do c = n0c, n1c
b_aibjciai_aibjc: do b = n0b, n1b
if (b == c) cycle b_aibjciai_aibjc
j_aibjciai_aibjc: do j = n0j, n1j
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciai_aibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciai_aibjc
i_aibjciai_aibjc: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjciai_aibjc
jac_ibra_iket = eom_ccsd_22_trans_aibjciai_aibjc(t2, nocc, nactive, a, i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciai_aibjc
end do a_aibjciai_aibjc
end do j_aibjciai_aibjc
end do b_aibjciai_aibjc
end do c_aibjciai_aibjc
!
! Elementary loop  7
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
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1ik .ge. n0ik).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjciaj_bjc(t2, &
 nocc, nactive, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_bjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_bjc
i_aibjciaj_bjc: do i = n0ik, n1ik
if (i == j) cycle i_aibjciaj_bjc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj_bjc
end do a_aibjciaj_bjc
end do j_aibjciaj_bjc
end do b_aibjciaj_bjc
end do c_aibjciaj_bjc
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj_abc: do c = n0c, n1c
b_aibjciaj_abc: do b = n0b, n1b
if (b == c) cycle b_aibjciaj_abc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_abc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_abc
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjciaj_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjciaj_abc: do j = n0jl, n1jl
i_aibjciaj_abc: do i = n0ik, n1ik
if (i == j) cycle i_aibjciaj_abc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj_abc
end do j_aibjciaj_abc
end do a_aibjciaj_abc
end do b_aibjciaj_abc
end do c_aibjciaj_abc
!
! Elementary loop  9
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
i_aibjciaj_ibjc: do i = n0ik, n1ik
if (i == j) cycle i_aibjciaj_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjciaj_ibjc(t2, nocc, nactive, &
 i, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_ibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_ibjc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjciaj_ibjc
end do i_aibjciaj_ibjc
end do j_aibjciaj_ibjc
end do b_aibjciaj_ibjc
end do c_aibjciaj_ibjc
!
! Elementary loop  10
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
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjciaj_aibc(t2, nocc, nactive, &
 a, i, b, c)
j_aibjciaj_aibc: do j = n0jl, n1jl
if (j == i) cycle j_aibjciaj_aibc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjciaj_aibc
end do i_aibjciaj_aibc
end do a_aibjciaj_aibc
end do b_aibjciaj_aibc
end do c_aibjciaj_aibc
!
! Elementary loop  11
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
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjciaj_abjc(t2, nocc, nactive, &
 a, b, j, c)
i_aibjciaj_abjc: do i = n0ik, n1ik
if (i == j) cycle i_aibjciaj_abjc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj_abjc
end do a_aibjciaj_abjc
end do j_aibjciaj_abjc
end do b_aibjciaj_abjc
end do c_aibjciaj_abjc
!
! Elementary loop  12
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
i_aibickai_ibck: do i = n0ijl, n1ijl
if (i == k) cycle i_aibickai_ibck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibickai_ibck(t2, nocc, nactive, &
 i, b, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickai_ibck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickai_ibck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibickai_ibck
end do i_aibickai_ibck
end do b_aibickai_ibck
end do k_aibickai_ibck
end do c_aibickai_ibck
!
! Elementary loop  13
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
i_aibickai_aibck: do i = n0ijl, n1ijl
if (i == k) cycle i_aibickai_aibck
jac_ibra_iket = eom_ccsd_22_trans_aibickai_aibck(t2, nocc, nactive, a, i, b, c, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickai_aibck
end do a_aibickai_aibck
end do b_aibickai_aibck
end do k_aibickai_aibck
end do c_aibickai_aibck
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == k
! No equalities independent of the above can hold.
!
c_aibickak_ibck: do c = n0c, n1c
k_aibickak_ibck: do k = n0kl, n1kl
b_aibickak_ibck: do b = n0b, n1b
if (b == c) cycle b_aibickak_ibck
i_aibickak_ibck: do i = n0ij, n1ij
if (i == k) cycle i_aibickak_ibck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibickak_ibck(t2, nocc, nactive, &
 i, b, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickak_ibck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickak_ibck
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibickak_ibck
end do i_aibickak_ibck
end do b_aibickak_ibck
end do k_aibickak_ibck
end do c_aibickak_ibck
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_bc: do c = n0c, n1c
b_aibjcjai_bc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_bc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjcjai_bc(t2, &
 nocc, nactive, b, c)
j_aibjcjai_bc: do j = n0jk, n1jk
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_bc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_bc
i_aibjcjai_bc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_bc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_bc
end do a_aibjcjai_bc
end do j_aibjcjai_bc
end do b_aibjcjai_bc
end do c_aibjcjai_bc
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_ibc: do c = n0c, n1c
b_aibjcjai_ibc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_ibc
i_aibjcjai_ibc: do i = n0il, n1il
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1jk .ge. n0jk).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjcjai_ibc(t2, &
 nocc, nactive, i, b, c)
j_aibjcjai_ibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjai_ibc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_ibc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_ibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjai_ibc
end do j_aibjcjai_ibc
end do i_aibjcjai_ibc
end do b_aibjcjai_ibc
end do c_aibjcjai_ibc
!
! Elementary loop  17
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
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1il .ge. n0il).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjcjai_bjc(t2, &
 nocc, nactive, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_bjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_bjc
i_aibjcjai_bjc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_bjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_bjc
end do a_aibjcjai_bjc
end do j_aibjcjai_bjc
end do b_aibjcjai_bjc
end do c_aibjcjai_bjc
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_abc: do c = n0c, n1c
b_aibjcjai_abc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_abc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_abc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_abc
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjcjai_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcjai_abc: do j = n0jk, n1jk
i_aibjcjai_abc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_abc
end do j_aibjcjai_abc
end do a_aibjcjai_abc
end do b_aibjcjai_abc
end do c_aibjcjai_abc
!
! Elementary loop  19
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
i_aibjcjai_ibjc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjcjai_ibjc(t2, nocc, nactive, &
 i, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_ibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_ibjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjai_ibjc
end do i_aibjcjai_ibjc
end do j_aibjcjai_ibjc
end do b_aibjcjai_ibjc
end do c_aibjcjai_ibjc
!
! Elementary loop  20
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
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjcjai_abjc(t2, nocc, nactive, &
 a, b, j, c)
i_aibjcjai_abjc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_abjc
end do a_aibjcjai_abjc
end do j_aibjcjai_abjc
end do b_aibjcjai_abjc
end do c_aibjcjai_abjc
!
! Elementary loop  21
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
i_aibjcjai_aibc: do i = n0il, n1il
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjcjai_aibc(t2, nocc, nactive, &
 a, i, b, c)
j_aibjcjai_aibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjai_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjai_aibc
end do i_aibjcjai_aibc
end do a_aibjcjai_aibc
end do b_aibjcjai_aibc
end do c_aibjcjai_aibc
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: d == a, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjaj_ibjc: do c = n0c, n1c
b_aibjcjaj_ibjc: do b = n0b, n1b
if (b == c) cycle b_aibjcjaj_ibjc
j_aibjcjaj_ibjc: do j = n0jkl, n1jkl
i_aibjcjaj_ibjc: do i = n0i, n1i
if (i == j) cycle i_aibjcjaj_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjcjaj_ibjc(t2, nocc, nactive, &
 i, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaj_ibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjaj_ibjc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjaj_ibjc
end do i_aibjcjaj_ibjc
end do j_aibjcjaj_ibjc
end do b_aibjcjaj_ibjc
end do c_aibjcjaj_ibjc
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: d == a, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjaj_aibjc: do c = n0c, n1c
b_aibjcjaj_aibjc: do b = n0b, n1b
if (b == c) cycle b_aibjcjaj_aibjc
j_aibjcjaj_aibjc: do j = n0jkl, n1jkl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaj_aibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjaj_aibjc
i_aibjcjaj_aibjc: do i = n0i, n1i
if (i == j) cycle i_aibjcjaj_aibjc
jac_ibra_iket = eom_ccsd_22_trans_aibjcjaj_aibjc(t2, nocc, nactive, a, i, b, j, c)
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaj_aibjc
end do a_aibjcjaj_aibjc
end do j_aibjcjaj_aibjc
end do b_aibjcjaj_aibjc
end do c_aibjcjaj_aibjc
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbibl_abjl: do l = n0l, n1l
b_aibjbibl_abjl: do b = n0bcd, n1bcd
j_aibjbibl_abjl: do j = n0j, n1j
if (j == l) cycle j_aibjbibl_abjl
a0 = max(b + 1, n0a)
a_aibjbibl_abjl: do a = a0, n1a
if (a == b) cycle a_aibjbibl_abjl
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aibjbibl_abjl(t2, nocc, nactive, &
 a, b, j, l)
i0 = max(l + 1, n0ik)
i_aibjbibl_abjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbibl_abjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibl_abjl
end do a_aibjbibl_abjl
end do j_aibjbibl_abjl
end do b_aibjbibl_abjl
end do l_aibjbibl_abjl
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbibl_aibjl: do l = n0l, n1l
b_aibjbibl_aibjl: do b = n0bcd, n1bcd
j_aibjbibl_aibjl: do j = n0j, n1j
if (j == l) cycle j_aibjbibl_aibjl
a0 = max(b + 1, n0a)
a_aibjbibl_aibjl: do a = a0, n1a
if (a == b) cycle a_aibjbibl_aibjl
i0 = max(l + 1, n0ik)
i_aibjbibl_aibjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbibl_aibjl
jac_ibra_iket = eom_ccsd_22_trans_aibjbibl_aibjl(t2, nocc, nactive, a, i, b, j, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibl_aibjl
end do a_aibjbibl_aibjl
end do j_aibjbibl_aibjl
end do b_aibjbibl_aibjl
end do l_aibjbibl_aibjl
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, l
! Equalities: c == b, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibibkbl_aibkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibibkbl_aibkl: do k = k0, n1k
if (k == l) cycle k_aibibkbl_aibkl
b_aibibkbl_aibkl: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibibkbl_aibkl: do a = a0, n1a
if (a == b) cycle a_aibibkbl_aibkl
i_aibibkbl_aibkl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibibkbl_aibkl
jac_ibra_iket = eom_ccsd_22_trans_aibibkbl_aibkl(t2, nocc, nactive, a, i, b, k, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbl_aibkl
end do a_aibibkbl_aibkl
end do b_aibibkbl_aibkl
end do k_aibibkbl_aibkl
end do l_aibibkbl_aibkl
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkbi_abjk: do k = n0k, n1k
b_aibjbkbi_abjk: do b = n0bcd, n1bcd
j_aibjbkbi_abjk: do j = n0j, n1j
if (j == k) cycle j_aibjbkbi_abjk
a0 = max(b + 1, n0a)
a_aibjbkbi_abjk: do a = a0, n1a
if (a == b) cycle a_aibjbkbi_abjk
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjbkbi_abjk(t2, nocc, nactive, &
 a, b, j, k)
i1 = min(k - 1, n1il)
i_aibjbkbi_abjk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkbi_abjk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbi_abjk
end do a_aibjbkbi_abjk
end do j_aibjbkbi_abjk
end do b_aibjbkbi_abjk
end do k_aibjbkbi_abjk
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkbi_aibjk: do k = n0k, n1k
b_aibjbkbi_aibjk: do b = n0bcd, n1bcd
j_aibjbkbi_aibjk: do j = n0j, n1j
if (j == k) cycle j_aibjbkbi_aibjk
a0 = max(b + 1, n0a)
a_aibjbkbi_aibjk: do a = a0, n1a
if (a == b) cycle a_aibjbkbi_aibjk
i1 = min(k - 1, n1il)
i_aibjbkbi_aibjk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkbi_aibjk
jac_ibra_iket = eom_ccsd_22_trans_aibjbkbi_aibjk(t2, nocc, nactive, a, i, b, j, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbi_aibjk
end do a_aibjbkbi_aibjk
end do j_aibjbkbi_aibjk
end do b_aibjbkbi_aibjk
end do k_aibjbkbi_aibjk
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, l
! Equalities: c == b, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjbjbl_aibl: do l = n0l, n1l
b_aibjbjbl_aibl: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibjbjbl_aibl: do a = a0, n1a
if (a == b) cycle a_aibjbjbl_aibl
i_aibjbjbl_aibl: do i = n0i, n1i
if (i == l) cycle i_aibjbjbl_aibl
nn0j = max(l + 1, n0jk)
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_trans_aibjbjbl_aibl(t2, nocc, nactive, &
 a, i, b, l)
j0 = max(l + 1, n0jk)
j_aibjbjbl_aibl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjbjbl_aibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjbl_aibl
end do i_aibjbjbl_aibl
end do a_aibjbjbl_aibl
end do b_aibjbjbl_aibl
end do l_aibjbjbl_aibl
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, l
! Equalities: c == b, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjbjbl_aibjl: do l = n0l, n1l
b_aibjbjbl_aibjl: do b = n0bcd, n1bcd
j0 = max(l + 1, n0jk)
j_aibjbjbl_aibjl: do j = j0, n1jk
if (j == l) cycle j_aibjbjbl_aibjl
a0 = max(b + 1, n0a)
a_aibjbjbl_aibjl: do a = a0, n1a
if (a == b) cycle a_aibjbjbl_aibjl
i_aibjbjbl_aibjl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjbjbl_aibjl
jac_ibra_iket = eom_ccsd_22_trans_aibjbjbl_aibjl(t2, nocc, nactive, a, i, b, j, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbl_aibjl
end do a_aibjbjbl_aibjl
end do j_aibjbjbl_aibjl
end do b_aibjbjbl_aibjl
end do l_aibjbjbl_aibjl
end subroutine ccjac_22_dav_part7
end module ccjac_block_22_dav_part7
