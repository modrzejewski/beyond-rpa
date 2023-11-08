module ccjac_block_22_tripletmp_dav_part6
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
subroutine ccjac_22_tripletmp_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0abc, n0abd, n0ac, n0bd, n0ijk
integer :: n0ijl, n0ik, n0il, n0jk, n0jl
integer :: n1abc, n1abd, n1ac, n1bd, n1ijk
integer :: n1ijl, n1ik, n1il, n1jk, n1jl
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
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0bd = max(n0b, n0d)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1abc = min(n1a, n1b, n1c)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
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
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj_ajd: do d = n0d, n1d
j_aiajaidj_ajd: do j = n0jl, n1jl
a0 = max(d + 1, n0abc)
a_aiajaidj_ajd: do a = a0, n1abc
if (a == d) cycle a_aiajaidj_ajd
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajaidj_ajd(t2, &
 nocc, nactive, a, j, d)
i0 = max(j + 1, n0ik)
i_aiajaidj_ajd: do i = i0, n1ik
if (i == j) cycle i_aiajaidj_ajd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidj_ajd
end do a_aiajaidj_ajd
end do j_aiajaidj_ajd
end do d_aiajaidj_ajd
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj_aid: do d = n0d, n1d
a0 = max(d + 1, n0abc)
a_aiajaidj_aid: do a = a0, n1abc
if (a == d) cycle a_aiajaidj_aid
i_aiajaidj_aid: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajaidj_aid(t2, &
 nocc, nactive, a, i, d)
j1 = min(i - 1, n1jl)
j_aiajaidj_aid: do j = n0jl, j1
if (j == i) cycle j_aiajaidj_aid
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajaidj_aid
end do i_aiajaidj_aid
end do a_aiajaidj_aid
end do d_aiajaidj_aid
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j
! Equalities: b == a, c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajaidj_aijd: do d = n0d, n1d
j_aiajaidj_aijd: do j = n0jl, n1jl
a0 = max(d + 1, n0abc)
a_aiajaidj_aijd: do a = a0, n1abc
if (a == d) cycle a_aiajaidj_aijd
i0 = max(j + 1, n0ik)
i_aiajaidj_aijd: do i = i0, n1ik
if (i == j) cycle i_aiajaidj_aijd
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajaidj_aijd(t2, nocc, nactive, a, i, &
 j, d)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaidj_aijd
end do a_aiajaidj_aijd
end do j_aiajaidj_aijd
end do d_aiajaidj_aijd
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciaj_ajc: do c = n0c, n1c
j_aiajciaj_ajc: do j = n0jl, n1jl
a1 = min(c - 1, n1abd)
a_aiajciaj_ajc: do a = n0abd, a1
if (a == c) cycle a_aiajciaj_ajc
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajciaj_ajc(t2, &
 nocc, nactive, a, j, c)
i0 = max(j + 1, n0ik)
i_aiajciaj_ajc: do i = i0, n1ik
if (i == j) cycle i_aiajciaj_ajc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaj_ajc
end do a_aiajciaj_ajc
end do j_aiajciaj_ajc
end do c_aiajciaj_ajc
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciaj_aic: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiajciaj_aic: do a = n0abd, a1
if (a == c) cycle a_aiajciaj_aic
i_aiajciaj_aic: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajciaj_aic(t2, &
 nocc, nactive, a, i, c)
j1 = min(i - 1, n1jl)
j_aiajciaj_aic: do j = n0jl, j1
if (j == i) cycle j_aiajciaj_aic
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajciaj_aic
end do i_aiajciaj_aic
end do a_aiajciaj_aic
end do c_aiajciaj_aic
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciaj_aijc: do c = n0c, n1c
j_aiajciaj_aijc: do j = n0jl, n1jl
a1 = min(c - 1, n1abd)
a_aiajciaj_aijc: do a = n0abd, a1
if (a == c) cycle a_aiajciaj_aijc
i0 = max(j + 1, n0ik)
i_aiajciaj_aijc: do i = i0, n1ik
if (i == j) cycle i_aiajciaj_aijc
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajciaj_aijc(t2, nocc, nactive, a, i, &
 j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaj_aijc
end do a_aiajciaj_aijc
end do j_aiajciaj_aijc
end do c_aiajciaj_aijc
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakbi_ibk: do k = n0k, n1k
b_aibiakbi_ibk: do b = n0bd, n1bd
i1 = min(k - 1, n1ijl)
i_aibiakbi_ibk: do i = n0ijl, i1
if (i == k) cycle i_aibiakbi_ibk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiakbi_ibk(t2, &
 nocc, nactive, i, b, k)
a0 = max(b + 1, n0ac)
a_aibiakbi_ibk: do a = a0, n1ac
if (a == b) cycle a_aibiakbi_ibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakbi_ibk
end do i_aibiakbi_ibk
end do b_aibiakbi_ibk
end do k_aibiakbi_ibk
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakbi_aik: do k = n0k, n1k
a_aibiakbi_aik: do a = n0ac, n1ac
i1 = min(k - 1, n1ijl)
i_aibiakbi_aik: do i = n0ijl, i1
if (i == k) cycle i_aibiakbi_aik
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiakbi_aik(t2, &
 nocc, nactive, a, i, k)
b1 = min(a - 1, n1bd)
b_aibiakbi_aik: do b = n0bd, b1
if (b == a) cycle b_aibiakbi_aik
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibiakbi_aik
end do i_aibiakbi_aik
end do a_aibiakbi_aik
end do k_aibiakbi_aik
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakbi_aibk: do k = n0k, n1k
b_aibiakbi_aibk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiakbi_aibk: do a = a0, n1ac
if (a == b) cycle a_aibiakbi_aibk
i1 = min(k - 1, n1ijl)
i_aibiakbi_aibk: do i = n0ijl, i1
if (i == k) cycle i_aibiakbi_aibk
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiakbi_aibk(t2, nocc, nactive, a, i, &
 b, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakbi_aibk
end do a_aibiakbi_aibk
end do b_aibiakbi_aibk
end do k_aibiakbi_aibk
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_bj: do b = n0bd, n1bd
j_aibjaibj_bj: do j = n0jl, n1jl
nn0i = max(j + 1, n0ik)
nn0a = max(b + 1, n0ac)
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibj_bj(t2, &
 nocc, nactive, b, j)
a0 = max(b + 1, n0ac)
a_aibjaibj_bj: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_bj
i0 = max(j + 1, n0ik)
i_aibjaibj_bj: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_bj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_bj
end do a_aibjaibj_bj
end do j_aibjaibj_bj
end do b_aibjaibj_bj
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
a_aibjaibj_ai: do a = n0ac, n1ac
i_aibjaibj_ai: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
nn1b = min(a - 1, n1bd)
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibj_ai(t2, &
 nocc, nactive, a, i)
b1 = min(a - 1, n1bd)
b_aibjaibj_ai: do b = n0bd, b1
if (b == a) cycle b_aibjaibj_ai
j1 = min(i - 1, n1jl)
j_aibjaibj_ai: do j = n0jl, j1
if (j == i) cycle j_aibjaibj_ai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do j_aibjaibj_ai
end do b_aibjaibj_ai
end do i_aibjaibj_ai
end do a_aibjaibj_ai
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_ibj: do b = n0bd, n1bd
j_aibjaibj_ibj: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjaibj_ibj: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_ibj
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibj_ibj(t2, &
 nocc, nactive, i, b, j)
a0 = max(b + 1, n0ac)
a_aibjaibj_ibj: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_ibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibjaibj_ibj
end do i_aibjaibj_ibj
end do j_aibjaibj_ibj
end do b_aibjaibj_ibj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
j_aibjaibj_aij: do j = n0jl, n1jl
a_aibjaibj_aij: do a = n0ac, n1ac
i0 = max(j + 1, n0ik)
i_aibjaibj_aij: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_aij
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibj_aij(t2, &
 nocc, nactive, a, i, j)
b1 = min(a - 1, n1bd)
b_aibjaibj_aij: do b = n0bd, b1
if (b == a) cycle b_aibjaibj_aij
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do b_aibjaibj_aij
end do i_aibjaibj_aij
end do a_aibjaibj_aij
end do j_aibjaibj_aij
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_abj: do b = n0bd, n1bd
j_aibjaibj_abj: do j = n0jl, n1jl
a0 = max(b + 1, n0ac)
a_aibjaibj_abj: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_abj
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibj_abj(t2, &
 nocc, nactive, a, b, j)
i0 = max(j + 1, n0ik)
i_aibjaibj_abj: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_abj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_abj
end do a_aibjaibj_abj
end do j_aibjaibj_abj
end do b_aibjaibj_abj
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_aib: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibj_aib: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_aib
i_aibjaibj_aib: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibj_aib(t2, &
 nocc, nactive, a, i, b)
j1 = min(i - 1, n1jl)
j_aibjaibj_aib: do j = n0jl, j1
if (j == i) cycle j_aibjaibj_aib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do j_aibjaibj_aib
end do i_aibjaibj_aib
end do a_aibjaibj_aib
end do b_aibjaibj_aib
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaibl_ibl: do l = n0l, n1l
b_aibiaibl_ibl: do b = n0bd, n1bd
i0 = max(l + 1, n0ijk)
i_aibiaibl_ibl: do i = i0, n1ijk
if (i == l) cycle i_aibiaibl_ibl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiaibl_ibl(t2, &
 nocc, nactive, i, b, l)
a0 = max(b + 1, n0ac)
a_aibiaibl_ibl: do a = a0, n1ac
if (a == b) cycle a_aibiaibl_ibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiaibl_ibl
end do i_aibiaibl_ibl
end do b_aibiaibl_ibl
end do l_aibiaibl_ibl
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaibl_ail: do l = n0l, n1l
a_aibiaibl_ail: do a = n0ac, n1ac
i0 = max(l + 1, n0ijk)
i_aibiaibl_ail: do i = i0, n1ijk
if (i == l) cycle i_aibiaibl_ail
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiaibl_ail(t2, &
 nocc, nactive, a, i, l)
b1 = min(a - 1, n1bd)
b_aibiaibl_ail: do b = n0bd, b1
if (b == a) cycle b_aibiaibl_ail
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibiaibl_ail
end do i_aibiaibl_ail
end do a_aibiaibl_ail
end do l_aibiaibl_ail
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaibl_aibl: do l = n0l, n1l
b_aibiaibl_aibl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiaibl_aibl: do a = a0, n1ac
if (a == b) cycle a_aibiaibl_aibl
i0 = max(l + 1, n0ijk)
i_aibiaibl_aibl: do i = i0, n1ijk
if (i == l) cycle i_aibiaibl_aibl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiaibl_aibl(t2, nocc, nactive, a, i, &
 b, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaibl_aibl
end do a_aibiaibl_aibl
end do b_aibiaibl_aibl
end do l_aibiaibl_aibl
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi_bj: do b = n0bd, n1bd
j_aibjajbi_bj: do j = n0jk, n1jk
nn1i = min(j - 1, n1il)
nn0a = max(b + 1, n0ac)
if ((nn1i .ge. n0il).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbi_bj(t2, &
 nocc, nactive, b, j)
a0 = max(b + 1, n0ac)
a_aibjajbi_bj: do a = a0, n1ac
if (a == b) cycle a_aibjajbi_bj
i1 = min(j - 1, n1il)
i_aibjajbi_bj: do i = n0il, i1
if (i == j) cycle i_aibjajbi_bj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbi_bj
end do a_aibjajbi_bj
end do j_aibjajbi_bj
end do b_aibjajbi_bj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
a_aibjajbi_ai: do a = n0ac, n1ac
i1 = min(j - 1, n1il)
i_aibjajbi_ai: do i = n0il, i1
nn1b = min(a - 1, n1bd)
if ((n1jk .ge. n0jk).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbi_ai(t2, &
 nocc, nactive, a, i)
b1 = min(a - 1, n1bd)
b_aibjajbi_ai: do b = n0bd, b1
if (b == a) cycle b_aibjajbi_ai
j_aibjajbi_ai: do j = n0jk, n1jk
if (j == i) cycle j_aibjajbi_ai
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbi_ai
end do b_aibjajbi_ai
end do i_aibjajbi_ai
end do a_aibjajbi_ai
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi_ibj: do b = n0bd, n1bd
j_aibjajbi_ibj: do j = n0jk, n1jk
i1 = min(j - 1, n1il)
i_aibjajbi_ibj: do i = n0il, i1
if (i == j) cycle i_aibjajbi_ibj
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbi_ibj(t2, &
 nocc, nactive, i, b, j)
a0 = max(b + 1, n0ac)
a_aibjajbi_ibj: do a = a0, n1ac
if (a == b) cycle a_aibjajbi_ibj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbi_ibj
end do i_aibjajbi_ibj
end do j_aibjajbi_ibj
end do b_aibjajbi_ibj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
j_aibjajbi_aij: do j = n0jk, n1jk
a_aibjajbi_aij: do a = n0ac, n1ac
i1 = min(j - 1, n1il)
i_aibjajbi_aij: do i = n0il, i1
if (i == j) cycle i_aibjajbi_aij
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbi_aij(t2, &
 nocc, nactive, a, i, j)
b1 = min(a - 1, n1bd)
b_aibjajbi_aij: do b = n0bd, b1
if (b == a) cycle b_aibjajbi_aij
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjajbi_aij
end do i_aibjajbi_aij
end do a_aibjajbi_aij
end do j_aibjajbi_aij
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi_abj: do b = n0bd, n1bd
j_aibjajbi_abj: do j = n0jk, n1jk
a0 = max(b + 1, n0ac)
a_aibjajbi_abj: do a = a0, n1ac
if (a == b) cycle a_aibjajbi_abj
nn1i = min(j - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbi_abj(t2, &
 nocc, nactive, a, b, j)
i1 = min(j - 1, n1il)
i_aibjajbi_abj: do i = n0il, i1
if (i == j) cycle i_aibjajbi_abj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbi_abj
end do a_aibjajbi_abj
end do j_aibjajbi_abj
end do b_aibjajbi_abj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi_aib: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjajbi_aib: do a = a0, n1ac
if (a == b) cycle a_aibjajbi_aib
i1 = min(j - 1, n1il)
i_aibjajbi_aib: do i = n0il, i1
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbi_aib(t2, &
 nocc, nactive, a, i, b)
j_aibjajbi_aib: do j = n0jk, n1jk
if (j == i) cycle j_aibjajbi_aib
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbi_aib
end do i_aibjajbi_aib
end do a_aibjajbi_aib
end do b_aibjajbi_aib
end subroutine ccjac_22_tripletmp_dav_part6
end module ccjac_block_22_tripletmp_dav_part6
