module ccjac_block_22_dav_part6
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
subroutine ccjac_22_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: nn0a, nn0c, nn0i, nn0j, nn1a, nn1i, nn1j
integer :: a0, a1, c0, i0, i1, j0, j1
integer :: n0ab, n0abd, n0ac, n0cd, n0ij
integer :: n0ijk, n0ijl, n0ik, n0ikl, n0il
integer :: n0jk, n0jkl, n0jl, n0kl
integer :: n1ab, n1abd, n1ac, n1cd, n1ij
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
n0ab = max(n0a, n0b)
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
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
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
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
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == k
! No equalities independent of the above can hold.
!
d_aibiakdk_ibkd: do d = n0d, n1d
k_aibiakdk_ibkd: do k = n0kl, n1kl
b_aibiakdk_ibkd: do b = n0b, n1b
if (b == d) cycle b_aibiakdk_ibkd
i_aibiakdk_ibkd: do i = n0ij, n1ij
if (i == k) cycle i_aibiakdk_ibkd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibiakdk_ibkd(t2, nocc, nactive, &
 i, b, k, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdk_ibkd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdk_ibkd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakdk_ibkd
end do i_aibiakdk_ibkd
end do b_aibiakdk_ibkd
end do k_aibiakdk_ibkd
end do d_aibiakdk_ibkd
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_abd: do d = n0d, n1d
b_aibjajdi_abd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_abd
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_abd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_abd
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjajdi_abd(t2, &
 nocc, nactive, a, b, d)
j_aibjajdi_abd: do j = n0jk, n1jk
i_aibjajdi_abd: do i = n0il, n1il
if (i == j) cycle i_aibjajdi_abd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdi_abd
end do j_aibjajdi_abd
end do a_aibjajdi_abd
end do b_aibjajdi_abd
end do d_aibjajdi_abd
!
! Elementary loop  3
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
nn0a = max(b + 1, d + 1, n0ac)
if ((n1il .ge. n0il).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjajdi_bjd(t2, &
 nocc, nactive, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_bjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_bjd
i_aibjajdi_bjd: do i = n0il, n1il
if (i == j) cycle i_aibjajdi_bjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdi_bjd
end do a_aibjajdi_bjd
end do j_aibjajdi_bjd
end do b_aibjajdi_bjd
end do d_aibjajdi_bjd
!
! Elementary loop  4
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
i_aibjajdi_ibjd: do i = n0il, n1il
if (i == j) cycle i_aibjajdi_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjajdi_ibjd(t2, nocc, nactive, &
 i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_ibjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdi_ibjd
end do i_aibjajdi_ibjd
end do j_aibjajdi_ibjd
end do b_aibjajdi_ibjd
end do d_aibjajdi_ibjd
!
! Elementary loop  5
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
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjajdi_abjd(t2, nocc, nactive, &
 a, b, j, d)
i_aibjajdi_abjd: do i = n0il, n1il
if (i == j) cycle i_aibjajdi_abjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdi_abjd
end do a_aibjajdi_abjd
end do j_aibjajdi_abjd
end do b_aibjajdi_abjd
end do d_aibjajdi_abjd
!
! Elementary loop  6
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
i_aibjajdi_aibd: do i = n0il, n1il
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjajdi_aibd(t2, nocc, nactive, &
 a, i, b, d)
j_aibjajdi_aibd: do j = n0jk, n1jk
if (j == i) cycle j_aibjajdi_aibd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajdi_aibd
end do i_aibjajdi_aibd
end do a_aibjajdi_aibd
end do b_aibjajdi_aibd
end do d_aibjajdi_aibd
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i
! Equalities: c == a, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjajdj_ibjd: do d = n0d, n1d
b_aibjajdj_ibjd: do b = n0b, n1b
if (b == d) cycle b_aibjajdj_ibjd
j_aibjajdj_ibjd: do j = n0jkl, n1jkl
i_aibjajdj_ibjd: do i = n0i, n1i
if (i == j) cycle i_aibjajdj_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjajdj_ibjd(t2, nocc, nactive, &
 i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdj_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdj_ibjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdj_ibjd
end do i_aibjajdj_ibjd
end do j_aibjajdj_ibjd
end do b_aibjajdj_ibjd
end do d_aibjajdj_ibjd
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i
! Equalities: c == a, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjajdj_aibjd: do d = n0d, n1d
b_aibjajdj_aibjd: do b = n0b, n1b
if (b == d) cycle b_aibjajdj_aibjd
j_aibjajdj_aibjd: do j = n0jkl, n1jkl
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdj_aibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdj_aibjd
i_aibjajdj_aibjd: do i = n0i, n1i
if (i == j) cycle i_aibjajdj_aibjd
jac_ibra_iket = eom_ccsd_22_trans_aibjajdj_aibjd(t2, nocc, nactive, a, i, b, j, d)
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdj_aibjd
end do a_aibjajdj_aibjd
end do j_aibjajdj_aibjd
end do b_aibjajdj_aibjd
end do d_aibjajdj_aibjd
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajcial_ajcl: do l = n0l, n1l
c_aiajcial_ajcl: do c = n0c, n1c
j_aiajcial_ajcl: do j = n0j, n1j
if (j == l) cycle j_aiajcial_ajcl
a1 = min(c - 1, n1abd)
a_aiajcial_ajcl: do a = n0abd, a1
if (a == c) cycle a_aiajcial_ajcl
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcial_ajcl(t2, nocc, nactive, &
 a, j, c, l)
i0 = max(j + 1, n0ik)
i_aiajcial_ajcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcial_ajcl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcial_ajcl
end do a_aiajcial_ajcl
end do j_aiajcial_ajcl
end do c_aiajcial_ajcl
end do l_aiajcial_ajcl
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajcial_aijcl: do l = n0l, n1l
c_aiajcial_aijcl: do c = n0c, n1c
j_aiajcial_aijcl: do j = n0j, n1j
if (j == l) cycle j_aiajcial_aijcl
a1 = min(c - 1, n1abd)
a_aiajcial_aijcl: do a = n0abd, a1
if (a == c) cycle a_aiajcial_aijcl
i0 = max(j + 1, n0ik)
i_aiajcial_aijcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcial_aijcl
jac_ibra_iket = eom_ccsd_22_trans_aiajcial_aijcl(t2, nocc, nactive, a, i, j, c, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcial_aijcl
end do a_aiajcial_aijcl
end do j_aiajcial_aijcl
end do c_aiajcial_aijcl
end do l_aiajcial_aijcl
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k, l
! Equalities: b == a, d == a, j == i
! No equalities independent of the above can hold.
!
l_aiaickal_aickl: do l = n0l, n1l
c_aiaickal_aickl: do c = n0c, n1c
k_aiaickal_aickl: do k = n0k, n1k
if (k == l) cycle k_aiaickal_aickl
a1 = min(c - 1, n1abd)
a_aiaickal_aickl: do a = n0abd, a1
if (a == c) cycle a_aiaickal_aickl
i_aiaickal_aickl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aiaickal_aickl
jac_ibra_iket = eom_ccsd_22_trans_aiaickal_aickl(t2, nocc, nactive, a, i, c, k, l)
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickal_aickl
end do a_aiaickal_aickl
end do k_aiaickal_aickl
end do c_aiaickal_aickl
end do l_aiaickal_aickl
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i
! No equalities independent of the above can hold.
!
c_aiajckai_ajck: do c = n0c, n1c
k_aiajckai_ajck: do k = n0k, n1k
j_aiajckai_ajck: do j = n0j, n1j
if (j == k) cycle j_aiajckai_ajck
a1 = min(c - 1, n1abd)
a_aiajckai_ajck: do a = n0abd, a1
if (a == c) cycle a_aiajckai_ajck
nn0i = max(j + 1, n0il)
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajckai_ajck(t2, nocc, nactive, &
 a, j, c, k)
i0 = max(j + 1, n0il)
i_aiajckai_ajck: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckai_ajck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckai_ajck
end do a_aiajckai_ajck
end do j_aiajckai_ajck
end do k_aiajckai_ajck
end do c_aiajckai_ajck
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i
! No equalities independent of the above can hold.
!
c_aiajckai_aijck: do c = n0c, n1c
k_aiajckai_aijck: do k = n0k, n1k
j_aiajckai_aijck: do j = n0j, n1j
if (j == k) cycle j_aiajckai_aijck
a1 = min(c - 1, n1abd)
a_aiajckai_aijck: do a = n0abd, a1
if (a == c) cycle a_aiajckai_aijck
i0 = max(j + 1, n0il)
i_aiajckai_aijck: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckai_aijck
jac_ibra_iket = eom_ccsd_22_trans_aiajckai_aijck(t2, nocc, nactive, a, i, j, c, k)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckai_aijck
end do a_aiajckai_aijck
end do j_aiajckai_aijck
end do k_aiajckai_aijck
end do c_aiajckai_aijck
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajcjal_aicl: do l = n0l, n1l
c_aiajcjal_aicl: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiajcjal_aicl: do a = n0abd, a1
if (a == c) cycle a_aiajcjal_aicl
i_aiajcjal_aicl: do i = n0i, n1i
if (i == l) cycle i_aiajcjal_aicl
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aiajcjal_aicl(t2, nocc, nactive, &
 a, i, c, l)
j1 = min(i - 1, n1jk)
j_aiajcjal_aicl: do j = n0jk, j1
if (j == i .or. j == l) cycle j_aiajcjal_aicl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcjal_aicl
end do i_aiajcjal_aicl
end do a_aiajcjal_aicl
end do c_aiajcjal_aicl
end do l_aiajcjal_aicl
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajcjal_aijcl: do l = n0l, n1l
c_aiajcjal_aijcl: do c = n0c, n1c
j_aiajcjal_aijcl: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjal_aijcl
a1 = min(c - 1, n1abd)
a_aiajcjal_aijcl: do a = n0abd, a1
if (a == c) cycle a_aiajcjal_aijcl
i0 = max(j + 1, n0i)
i_aiajcjal_aijcl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajcjal_aijcl
jac_ibra_iket = eom_ccsd_22_trans_aiajcjal_aijcl(t2, nocc, nactive, a, i, j, c, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjal_aijcl
end do a_aiajcjal_aijcl
end do j_aiajcjal_aijcl
end do c_aiajcjal_aijcl
end do l_aiajcjal_aijcl
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == a, l == k
! No equalities independent of the above can hold.
!
c_aiajckak_aijck: do c = n0c, n1c
k_aiajckak_aijck: do k = n0kl, n1kl
j_aiajckak_aijck: do j = n0j, n1j
if (j == k) cycle j_aiajckak_aijck
a1 = min(c - 1, n1abd)
a_aiajckak_aijck: do a = n0abd, a1
if (a == c) cycle a_aiajckak_aijck
i0 = max(j + 1, n0i)
i_aiajckak_aijck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckak_aijck
jac_ibra_iket = eom_ccsd_22_trans_aiajckak_aijck(t2, nocc, nactive, a, i, j, c, k)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckak_aijck
end do a_aiajckak_aijck
end do j_aiajckak_aijck
end do k_aiajckak_aijck
end do c_aiajckak_aijck
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, l == j
! No equalities independent of the above can hold.
!
c_aiajckaj_aick: do c = n0c, n1c
k_aiajckaj_aick: do k = n0k, n1k
a1 = min(c - 1, n1abd)
a_aiajckaj_aick: do a = n0abd, a1
if (a == c) cycle a_aiajckaj_aick
i_aiajckaj_aick: do i = n0i, n1i
if (i == k) cycle i_aiajckaj_aick
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aiajckaj_aick(t2, nocc, nactive, &
 a, i, c, k)
j1 = min(i - 1, n1jl)
j_aiajckaj_aick: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aiajckaj_aick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajckaj_aick
end do i_aiajckaj_aick
end do a_aiajckaj_aick
end do k_aiajckaj_aick
end do c_aiajckaj_aick
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, l == j
! No equalities independent of the above can hold.
!
c_aiajckaj_aijck: do c = n0c, n1c
k_aiajckaj_aijck: do k = n0k, n1k
j_aiajckaj_aijck: do j = n0jl, n1jl
if (j == k) cycle j_aiajckaj_aijck
a1 = min(c - 1, n1abd)
a_aiajckaj_aijck: do a = n0abd, a1
if (a == c) cycle a_aiajckaj_aijck
i0 = max(j + 1, n0i)
i_aiajckaj_aijck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckaj_aijck
jac_ibra_iket = eom_ccsd_22_trans_aiajckaj_aijck(t2, nocc, nactive, a, i, j, c, k)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaj_aijck
end do a_aiajckaj_aijck
end do j_aiajckaj_aijck
end do k_aiajckaj_aijck
end do c_aiajckaj_aijck
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == c, k == i
! No equalities independent of the above can hold.
!
l_aiajcicl_ajcl: do l = n0l, n1l
c_aiajcicl_ajcl: do c = n0cd, n1cd
j_aiajcicl_ajcl: do j = n0j, n1j
if (j == l) cycle j_aiajcicl_ajcl
a_aiajcicl_ajcl: do a = n0ab, n1ab
if (a == c) cycle a_aiajcicl_ajcl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcicl_ajcl(t2, nocc, nactive, &
 a, j, c, l)
i0 = max(j + 1, l + 1, n0ik)
i_aiajcicl_ajcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcicl_ajcl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicl_ajcl
end do a_aiajcicl_ajcl
end do j_aiajcicl_ajcl
end do c_aiajcicl_ajcl
end do l_aiajcicl_ajcl
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, l == i
! No equalities independent of the above can hold.
!
c_aiajckci_ajck: do c = n0cd, n1cd
k_aiajckci_ajck: do k = n0k, n1k
j_aiajckci_ajck: do j = n0j, n1j
if (j == k) cycle j_aiajckci_ajck
a_aiajckci_ajck: do a = n0ab, n1ab
if (a == c) cycle a_aiajckci_ajck
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajckci_ajck(t2, nocc, nactive, &
 a, j, c, k)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajckci_ajck: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajckci_ajck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckci_ajck
end do a_aiajckci_ajck
end do j_aiajckci_ajck
end do k_aiajckci_ajck
end do c_aiajckci_ajck
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == c, k == j
! No equalities independent of the above can hold.
!
l_aiajcjcl_aicl: do l = n0l, n1l
c_aiajcjcl_aicl: do c = n0cd, n1cd
a_aiajcjcl_aicl: do a = n0ab, n1ab
if (a == c) cycle a_aiajcjcl_aicl
i_aiajcjcl_aicl: do i = n0i, n1i
if (i == l) cycle i_aiajcjcl_aicl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_trans_aiajcjcl_aicl(t2, nocc, nactive, &
 a, i, c, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aiajcjcl_aicl: do j = j0, j1
if (j == i .or. j == l) cycle j_aiajcjcl_aicl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcjcl_aicl
end do i_aiajcjcl_aicl
end do a_aiajcjcl_aicl
end do c_aiajcjcl_aicl
end do l_aiajcjcl_aicl
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == c, l == j
! No equalities independent of the above can hold.
!
c_aiajckcj_aick: do c = n0cd, n1cd
k_aiajckcj_aick: do k = n0k, n1k
a_aiajckcj_aick: do a = n0ab, n1ab
if (a == c) cycle a_aiajckcj_aick
i_aiajckcj_aick: do i = n0i, n1i
if (i == k) cycle i_aiajckcj_aick
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aiajckcj_aick(t2, nocc, nactive, &
 a, i, c, k)
j1 = min(i - 1, k - 1, n1jl)
j_aiajckcj_aick: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aiajckcj_aick
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajckcj_aick
end do i_aiajckcj_aick
end do a_aiajckcj_aick
end do k_aiajckcj_aick
end do c_aiajckcj_aick
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, l
! Equalities: b == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aiaicidl_aicdl: do d = n0d, n1d
l_aiaicidl_aicdl: do l = n0l, n1l
c0 = max(d + 1, n0c)
c_aiaicidl_aicdl: do c = c0, n1c
if (c == d) cycle c_aiaicidl_aicdl
a_aiaicidl_aicdl: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiaicidl_aicdl
i_aiaicidl_aicdl: do i = n0ijk, n1ijk
if (i == l) cycle i_aiaicidl_aicdl
jac_ibra_iket = eom_ccsd_22_trans_aiaicidl_aicdl(t2, nocc, nactive, a, i, c, d, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicidl_aicdl
end do a_aiaicidl_aicdl
end do c_aiaicidl_aicdl
end do l_aiaicidl_aicdl
end do d_aiaicidl_aicdl
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiajcidi_aijcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcidi_aijcd: do c = c0, n1c
if (c == d) cycle c_aiajcidi_aijcd
j_aiajcidi_aijcd: do j = n0j, n1j
a_aiajcidi_aijcd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcidi_aijcd
i0 = max(j + 1, n0ikl)
i_aiajcidi_aijcd: do i = i0, n1ikl
if (i == j) cycle i_aiajcidi_aijcd
jac_ibra_iket = eom_ccsd_22_trans_aiajcidi_aijcd(t2, nocc, nactive, a, i, j, c, d)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidi_aijcd
end do a_aiajcidi_aijcd
end do j_aiajcidi_aijcd
end do c_aiajcidi_aijcd
end do d_aiajcidi_aijcd
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidj_acd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcidj_acd: do c = c0, n1c
if (c == d) cycle c_aiajcidj_acd
a_aiajcidj_acd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcidj_acd
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcidj_acd(t2, &
 nocc, nactive, a, c, d)
j_aiajcidj_acd: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aiajcidj_acd: do i = i0, n1ik
if (i == j) cycle i_aiajcidj_acd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidj_acd
end do j_aiajcidj_acd
end do a_aiajcidj_acd
end do c_aiajcidj_acd
end do d_aiajcidj_acd
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidj_aicd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcidj_aicd: do c = c0, n1c
if (c == d) cycle c_aiajcidj_aicd
a_aiajcidj_aicd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcidj_aicd
i_aiajcidj_aicd: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aiajcidj_aicd(t2, nocc, nactive, &
 a, i, c, d)
j1 = min(i - 1, n1jl)
j_aiajcidj_aicd: do j = n0jl, j1
if (j == i) cycle j_aiajcidj_aicd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcidj_aicd
end do i_aiajcidj_aicd
end do a_aiajcidj_aicd
end do c_aiajcidj_aicd
end do d_aiajcidj_aicd
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidj_ajcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcidj_ajcd: do c = c0, n1c
if (c == d) cycle c_aiajcidj_ajcd
j_aiajcidj_ajcd: do j = n0jl, n1jl
a_aiajcidj_ajcd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcidj_ajcd
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcidj_ajcd(t2, nocc, nactive, &
 a, j, c, d)
i0 = max(j + 1, n0ik)
i_aiajcidj_ajcd: do i = i0, n1ik
if (i == j) cycle i_aiajcidj_ajcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidj_ajcd
end do a_aiajcidj_ajcd
end do j_aiajcidj_ajcd
end do c_aiajcidj_ajcd
end do d_aiajcidj_ajcd
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, k
! Equalities: b == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aiaickdi_aickd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiaickdi_aickd: do c = c0, n1c
if (c == d) cycle c_aiaickdi_aickd
k_aiaickdi_aickd: do k = n0k, n1k
a_aiaickdi_aickd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiaickdi_aickd
i_aiaickdi_aickd: do i = n0ijl, n1ijl
if (i == k) cycle i_aiaickdi_aickd
jac_ibra_iket = eom_ccsd_22_trans_aiaickdi_aickd(t2, nocc, nactive, a, i, c, k, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickdi_aickd
end do a_aiaickdi_aickd
end do k_aiaickdi_aickd
end do c_aiaickdi_aickd
end do d_aiaickdi_aickd
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdi_acd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcjdi_acd: do c = c0, n1c
if (c == d) cycle c_aiajcjdi_acd
a_aiajcjdi_acd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcjdi_acd
nn0i = max(n0jk + 1, n0il)
if ((n1jk .ge. n0jk).and. (n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcjdi_acd(t2, &
 nocc, nactive, a, c, d)
j_aiajcjdi_acd: do j = n0jk, n1jk
i0 = max(j + 1, n0il)
i_aiajcjdi_acd: do i = i0, n1il
if (i == j) cycle i_aiajcjdi_acd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdi_acd
end do j_aiajcjdi_acd
end do a_aiajcjdi_acd
end do c_aiajcjdi_acd
end do d_aiajcjdi_acd
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdi_ajcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcjdi_ajcd: do c = c0, n1c
if (c == d) cycle c_aiajcjdi_ajcd
j_aiajcjdi_ajcd: do j = n0jk, n1jk
a_aiajcjdi_ajcd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcjdi_ajcd
nn0i = max(j + 1, n0il)
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcjdi_ajcd(t2, nocc, nactive, &
 a, j, c, d)
i0 = max(j + 1, n0il)
i_aiajcjdi_ajcd: do i = i0, n1il
if (i == j) cycle i_aiajcjdi_ajcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjdi_ajcd
end do a_aiajcjdi_ajcd
end do j_aiajcjdi_ajcd
end do c_aiajcjdi_ajcd
end do d_aiajcjdi_ajcd
end subroutine ccjac_22_dav_part6
end module ccjac_block_22_dav_part6
