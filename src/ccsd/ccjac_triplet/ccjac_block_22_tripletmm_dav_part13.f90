module ccjac_block_22_tripletmm_dav_part13
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
subroutine ccjac_22_tripletmm_dav_part13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a, b, c
integer :: i, j
integer :: ai, bj, ck, dl
integer :: nn0a, nn0i, nn1b, nn1j
integer :: a0, b1, i0, j1
integer :: n0abcd, n0ac, n0bd, n0ijkl, n0ik
integer :: n0jl
integer :: n1abcd, n1ac, n1bd, n1ijkl, n1ik
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
n0abcd = max(n0a, n0b, n0c, n0d)
n0ac = max(n0a, n0c)
n0bd = max(n0b, n0d)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ik = max(n0i, n0k)
n0jl = max(n0j, n0l)
n1abcd = min(n1a, n1b, n1c, n1d)
n1ac = min(n1a, n1c)
n1bd = min(n1b, n1d)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ik = min(n1i, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i
! Equalities: d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibicibi_aic: do c = n0c, n1c
a_aibicibi_aic: do a = n0a, n1a
if (a == c) cycle a_aibicibi_aic
i_aibicibi_aic: do i = n0ijkl, n1ijkl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibicibi_aic(t2, &
 nocc, nactive, a, i, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibicibi_aic: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibicibi_aic
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibicibi_aic
end do i_aibicibi_aic
end do a_aibicibi_aic
end do c_aibicibi_aic
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i
! Equalities: d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibicibi_abc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibicibi_abc: do b = n0bd, b1
if (b == c) cycle b_aibicibi_abc
a0 = max(b + 1, n0a)
a_aibicibi_abc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicibi_abc
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibicibi_abc(t2, &
 nocc, nactive, a, b, c)
i_aibicibi_abc: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibi_abc
end do a_aibicibi_abc
end do b_aibicibi_abc
end do c_aibicibi_abc
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i
! Equalities: d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibicibi_aibc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibicibi_aibc: do b = n0bd, b1
if (b == c) cycle b_aibicibi_aibc
a0 = max(b + 1, n0a)
a_aibicibi_aibc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicibi_aibc
i_aibicibi_aibc: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibicibi_aibc(t2, nocc, nactive, a, i, &
 b, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibi_aibc
end do a_aibicibi_aibc
end do b_aibicibi_aibc
end do c_aibicibi_aibc
!
! Elementary loop  4
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
a_aiajaiaj_a: do a = n0abcd, n1abcd
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaiaj_a(t2, &
 nocc, nactive, a)
j_aiajaiaj_a: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aiajaiaj_a: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_a
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiajaiaj_a
end do j_aiajaiaj_a
end do a_aiajaiaj_a
!
! Elementary loop  5
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
i_aiajaiaj_i: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl).and. (n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaiaj_i(t2, &
 nocc, nactive, i)
j1 = min(i - 1, n1jl)
j_aiajaiaj_i: do j = n0jl, j1
if (j == i) cycle j_aiajaiaj_i
a_aiajaiaj_i: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do a_aiajaiaj_i
end do j_aiajaiaj_i
end do i_aiajaiaj_i
!
! Elementary loop  6
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
j_aiajaiaj_j: do j = n0jl, n1jl
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i).and. (n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaiaj_j(t2, &
 nocc, nactive, j)
a_aiajaiaj_j: do a = n0abcd, n1abcd
i0 = max(j + 1, n0ik)
i_aiajaiaj_j: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_j
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiajaiaj_j
end do a_aiajaiaj_j
end do j_aiajaiaj_j
!
! Elementary loop  7
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
a_aiajaiaj_ai: do a = n0abcd, n1abcd
i_aiajaiaj_ai: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaiaj_ai(t2, &
 nocc, nactive, a, i)
j1 = min(i - 1, n1jl)
j_aiajaiaj_ai: do j = n0jl, j1
if (j == i) cycle j_aiajaiaj_ai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do j_aiajaiaj_ai
end do i_aiajaiaj_ai
end do a_aiajaiaj_ai
!
! Elementary loop  8
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
j_aiajaiaj_aj: do j = n0jl, n1jl
a_aiajaiaj_aj: do a = n0abcd, n1abcd
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaiaj_aj(t2, &
 nocc, nactive, a, j)
i0 = max(j + 1, n0ik)
i_aiajaiaj_aj: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_aj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiajaiaj_aj
end do a_aiajaiaj_aj
end do j_aiajaiaj_aj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
j_aiajaiaj_ij: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aiajaiaj_ij: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_ij
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaiaj_ij(t2, &
 nocc, nactive, i, j)
a_aiajaiaj_ij: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do a_aiajaiaj_ij
end do i_aiajaiaj_ij
end do j_aiajaiaj_ij
!
! Elementary loop  10
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
j_aiajaiaj_aij: do j = n0jl, n1jl
a_aiajaiaj_aij: do a = n0abcd, n1abcd
i0 = max(j + 1, n0ik)
i_aiajaiaj_aij: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_aij
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajaiaj_aij(t2, nocc, nactive, a, i, &
 j)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiajaiaj_aij
end do a_aiajaiaj_aij
end do j_aiajaiaj_aij
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
a_aibiaibi_a: do a = n0ac, n1ac
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaibi_a(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibiaibi_a
end do b_aibiaibi_a
end do a_aibiaibi_a
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibiaibi_b: do b = n0bd, n1bd
nn0a = max(b + 1, n0ac)
if ((n1ijkl .ge. n0ijkl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaibi_b(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibiaibi_b
end do a_aibiaibi_b
end do b_aibiaibi_b
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
i_aibiaibi_i: do i = n0ijkl, n1ijkl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaibi_i(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibiaibi_i
end do b_aibiaibi_i
end do i_aibiaibi_i
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
a_aibiaibi_ai: do a = n0ac, n1ac
i_aibiaibi_ai: do i = n0ijkl, n1ijkl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaibi_ai(t2, &
 nocc, nactive, a, i)
b1 = min(a - 1, n1bd)
b_aibiaibi_ai: do b = n0bd, b1
if (b == a) cycle b_aibiaibi_ai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do b_aibiaibi_ai
end do i_aibiaibi_ai
end do a_aibiaibi_ai
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i
! Equalities: c == a, d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibiaibi_ib: do b = n0bd, n1bd
i_aibiaibi_ib: do i = n0ijkl, n1ijkl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaibi_ib(t2, &
 nocc, nactive, i, b)
a0 = max(b + 1, n0ac)
a_aibiaibi_ib: do a = a0, n1ac
if (a == b) cycle a_aibiaibi_ib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibiaibi_ib
end do i_aibiaibi_ib
end do b_aibiaibi_ib
!
! Elementary loop  16
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
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaibi_ab(t2, &
 nocc, nactive, a, b)
i_aibiaibi_ab: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibiaibi_ab
end do a_aibiaibi_ab
end do b_aibiaibi_ab
!
! Elementary loop  17
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
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaibi_aib(t2, nocc, nactive, a, i, &
 b)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibiaibi_aib
end do a_aibiaibi_aib
end do b_aibiaibi_aib
end subroutine ccjac_22_tripletmm_dav_part13
end module ccjac_block_22_tripletmm_dav_part13
