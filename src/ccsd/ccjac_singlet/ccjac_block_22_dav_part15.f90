module ccjac_block_22_dav_part15
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
subroutine ccjac_22_dav_part15(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: i, j, k
integer :: ai, bj, ck, dl
integer :: nn0a, nn0i, nn1a, nn1b
integer :: a0, a1, b1, i0
integer :: n0ab, n0abc, n0abcd, n0abd, n0ac
integer :: n0acd, n0bcd, n0bd, n0cd, n0ij
integer :: n0ijkl, n0jkl, n0kl
integer :: n1ab, n1abc, n1abcd, n1abd, n1ac
integer :: n1acd, n1bcd, n1bd, n1cd, n1ij
integer :: n1ijkl, n1jkl, n1kl
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
n0abc = max(n0a, n0b, n0c)
n0abcd = max(n0a, n0b, n0c, n0d)
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0bcd = max(n0b, n0c, n0d)
n0bd = max(n0b, n0d)
n0cd = max(n0c, n0d)
n0ij = max(n0i, n0j)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0jkl = max(n0j, n0k, n0l)
n0kl = max(n0k, n0l)
n1ab = min(n1a, n1b)
n1abc = min(n1a, n1b, n1c)
n1abcd = min(n1a, n1b, n1c, n1d)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1bcd = min(n1b, n1c, n1d)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1jkl = min(n1j, n1k, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a
! Free occupied indices: i, k
! Equalities: b == a, c == a, d == a, j == i, l == k
! No equalities independent of the above can hold.
!
k_aiaiakak_ik: do k = n0kl, n1kl
i_aiaiakak_ik: do i = n0ij, n1ij
if (i == k) cycle i_aiaiakak_ik
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiaiakak_ik(t2, nocc, &
 nactive, i, k)
a_aiaiakak_ik: do a = n0abcd, n1abcd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiaiakak_ik
end do i_aiaiakak_ik
end do k_aiaiakak_ik
!
! Elementary loop  2
! --------------------
! Free virtual indices: a
! Free occupied indices: i, k
! Equalities: b == a, c == a, d == a, j == i, l == k
! No equalities independent of the above can hold.
!
k_aiaiakak_aik: do k = n0kl, n1kl
a_aiaiakak_aik: do a = n0abcd, n1abcd
i_aiaiakak_aik: do i = n0ij, n1ij
if (i == k) cycle i_aiaiakak_aik
jac_ibra_iket = eom_ccsd_22_trans_aiaiakak_aik(t2, nocc, nactive, a, i, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakak_aik
end do a_aiaiakak_aik
end do k_aiaiakak_aik
!
! Elementary loop  3
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i
! Equalities: b == a, c == a, d == a, k == j, l == j
! No equalities independent of the above can hold.
!
j_aiajajaj_ij: do j = n0jkl, n1jkl
i0 = max(j + 1, n0i)
i_aiajajaj_ij: do i = i0, n1i
if (i == j) cycle i_aiajajaj_ij
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiajajaj_ij(t2, nocc, &
 nactive, i, j)
a_aiajajaj_ij: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajajaj_ij
end do i_aiajajaj_ij
end do j_aiajajaj_ij
!
! Elementary loop  4
! --------------------
! Free virtual indices: a
! Free occupied indices: j, i
! Equalities: b == a, c == a, d == a, k == j, l == j
! No equalities independent of the above can hold.
!
j_aiajajaj_aij: do j = n0jkl, n1jkl
a_aiajajaj_aij: do a = n0abcd, n1abcd
i0 = max(j + 1, n0i)
i_aiajajaj_aij: do i = i0, n1i
if (i == j) cycle i_aiajajaj_aij
jac_ibra_iket = eom_ccsd_22_trans_aiajajaj_aij(t2, nocc, nactive, a, i, j)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajajaj_aij
end do a_aiajajaj_aij
end do j_aiajajaj_aij
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i
! Equalities: b == a, c == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiaiaidi_ad: do d = n0d, n1d
a0 = max(d + 1, n0abc)
a_aiaiaidi_ad: do a = a0, n1abc
if (a == d) cycle a_aiaiaidi_ad
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aiaiaidi_ad(t2, nocc, &
 nactive, a, d)
i_aiaiaidi_ad: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiaidi_ad
end do a_aiaiaidi_ad
end do d_aiaiaidi_ad
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i
! Equalities: b == a, c == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiaiaidi_aid: do d = n0d, n1d
a0 = max(d + 1, n0abc)
a_aiaiaidi_aid: do a = a0, n1abc
if (a == d) cycle a_aiaiaidi_aid
i_aiaiaidi_aid: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aiaiaidi_aid(t2, nocc, nactive, a, i, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiaidi_aid
end do a_aiaiaidi_aid
end do d_aiaiaidi_aid
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibiaiai_ab: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibiaiai_ab: do a = a0, n1acd
if (a == b) cycle a_aibiaiai_ab
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibiaiai_ab(t2, nocc, &
 nactive, a, b)
i_aibiaiai_ab: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaiai_ab
end do a_aibiaiai_ab
end do b_aibiaiai_ab
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibiaiai_aib: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibiaiai_aib: do a = a0, n1acd
if (a == b) cycle a_aibiaiai_aib
i_aibiaiai_aib: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aibiaiai_aib(t2, nocc, nactive, a, i, b)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaiai_aib
end do a_aibiaiai_aib
end do b_aibiaiai_aib
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
a_aibiaibi_a: do a = n0ac, n1ac
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibiaibi_a(t2, &
 nocc, nactive, a)
b1 = min(a - 1, n1bd)
b_aibiaibi_a: do b = n0bd, b1
if (b == a) cycle b_aibiaibi_a
i_aibiaibi_a: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibiaibi_a
end do b_aibiaibi_a
end do a_aibiaibi_a
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibiaibi_b: do b = n0bd, n1bd
nn0a = max(b + 1, n0ac)
if ((n1ijkl .ge. n0ijkl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibiaibi_b(t2, &
 nocc, nactive, b)
a0 = max(b + 1, n0ac)
a_aibiaibi_b: do a = a0, n1ac
if (a == b) cycle a_aibiaibi_b
i_aibiaibi_b: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibiaibi_b
end do a_aibiaibi_b
end do b_aibiaibi_b
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
i_aibiaibi_i: do i = n0ijkl, n1ijkl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibiaibi_i(t2, &
 nocc, nactive, i)
b_aibiaibi_i: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiaibi_i: do a = a0, n1ac
if (a == b) cycle a_aibiaibi_i
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibiaibi_i
end do b_aibiaibi_i
end do i_aibiaibi_i
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
a_aibiaibi_ai: do a = n0ac, n1ac
i_aibiaibi_ai: do i = n0ijkl, n1ijkl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibiaibi_ai(t2, nocc, nactive, &
 a, i)
b1 = min(a - 1, n1bd)
b_aibiaibi_ai: do b = n0bd, b1
if (b == a) cycle b_aibiaibi_ai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do b_aibiaibi_ai
end do i_aibiaibi_ai
end do a_aibiaibi_ai
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibiaibi_ib: do b = n0bd, n1bd
i_aibiaibi_ib: do i = n0ijkl, n1ijkl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibiaibi_ib(t2, nocc, nactive, &
 i, b)
a0 = max(b + 1, n0ac)
a_aibiaibi_ib: do a = a0, n1ac
if (a == b) cycle a_aibiaibi_ib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibiaibi_ib
end do i_aibiaibi_ib
end do b_aibiaibi_ib
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibiaibi_ab: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiaibi_ab: do a = a0, n1ac
if (a == b) cycle a_aibiaibi_ab
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibiaibi_ab(t2, nocc, &
 nactive, a, b)
i_aibiaibi_ab: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibiaibi_ab
end do a_aibiaibi_ab
end do b_aibiaibi_ab
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibiaibi_aib: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiaibi_aib: do a = a0, n1ac
if (a == b) cycle a_aibiaibi_aib
i_aibiaibi_aib: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aibiaibi_aib(t2, nocc, nactive, a, i, b)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibiaibi_aib
end do a_aibiaibi_aib
end do b_aibiaibi_aib
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i
! Equalities: b == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiaiciai_ac: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiaiciai_ac: do a = n0abd, a1
if (a == c) cycle a_aiaiciai_ac
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aiaiciai_ac(t2, nocc, &
 nactive, a, c)
i_aiaiciai_ac: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiciai_ac
end do a_aiaiciai_ac
end do c_aiaiciai_ac
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i
! Equalities: b == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiaiciai_aic: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiaiciai_aic: do a = n0abd, a1
if (a == c) cycle a_aiaiciai_aic
i_aiaiciai_aic: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aiaiciai_aic(t2, nocc, nactive, a, i, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiciai_aic
end do a_aiaiciai_aic
end do c_aiaiciai_aic
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i
! Equalities: b == a, d == c, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiaicici_ac: do c = n0cd, n1cd
a_aiaicici_ac: do a = n0ab, n1ab
if (a == c) cycle a_aiaicici_ac
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aiaicici_ac(t2, nocc, &
 nactive, a, c)
i_aiaicici_ac: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicici_ac
end do a_aiaicici_ac
end do c_aiaicici_ac
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i
! Equalities: b == a, d == c, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiaicici_aic: do c = n0cd, n1cd
a_aiaicici_aic: do a = n0ab, n1ab
if (a == c) cycle a_aiaicici_aic
i_aiaicici_aic: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aiaicici_aic(t2, nocc, nactive, a, i, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicici_aic
end do a_aiaicici_aic
end do c_aiaicici_aic
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i
! Equalities: c == b, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibibibi_ab: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibibibi_ab: do a = a0, n1a
if (a == b) cycle a_aibibibi_ab
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibibibi_ab(t2, nocc, &
 nactive, a, b)
i_aibibibi_ab: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibibi_ab
end do a_aibibibi_ab
end do b_aibibibi_ab
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i
! Equalities: c == b, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibibibi_aib: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibibibi_aib: do a = a0, n1a
if (a == b) cycle a_aibibibi_aib
i_aibibibi_aib: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aibibibi_aib(t2, nocc, nactive, a, i, b)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibibi_aib
end do a_aibibibi_aib
end do b_aibibibi_aib
!
! Elementary loop  22
! --------------------
! Free virtual indices: a
! Free occupied indices: i
! Equalities: b == a, c == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
a_aiaiaiai_a: do a = n0abcd, n1abcd
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aiaiaiai_a(t2, nocc, nactive, &
 a)
i_aiaiaiai_a: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiaiaiai_a
end do a_aiaiaiai_a
!
! Elementary loop  23
! --------------------
! Free virtual indices: a
! Free occupied indices: i
! Equalities: b == a, c == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
i_aiaiaiai_i: do i = n0ijkl, n1ijkl
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiaiaiai_i(t2, nocc, nactive, &
 i)
a_aiaiaiai_i: do a = n0abcd, n1abcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do a_aiaiaiai_i
end do i_aiaiaiai_i
!
! Elementary loop  24
! --------------------
! Free virtual indices: a
! Free occupied indices: i
! Equalities: b == a, c == a, d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
a_aiaiaiai_ai: do a = n0abcd, n1abcd
i_aiaiaiai_ai: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aiaiaiai_ai(t2, nocc, nactive, a, i)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiaiaiai_ai
end do a_aiaiaiai_ai
end subroutine ccjac_22_dav_part15
end module ccjac_block_22_dav_part15
