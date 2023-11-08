module ccjac_block_22_tripletpm_dav_part5
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
subroutine ccjac_22_tripletpm_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0i, nn0j, nn1a, nn1j
integer :: a0, a1, i0, j0, j1
integer :: n0ac, n0acd, n0ad, n0cd, n0ik
integer :: n0ikl, n0il, n0jk, n0jkl, n0jl
integer :: n1ac, n1acd, n1ad, n1cd, n1ik
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
n0ad = max(n0a, n0d)
n0cd = max(n0c, n0d)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ad = min(n1a, n1d)
n1cd = min(n1c, n1d)
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
! Free occupied indices: j, i, k
! Equalities: c == a, d == a, l == j
! No equalities independent of the above can hold.
!
k_aibjakaj_aibk: do k = n0k, n1k
b_aibjakaj_aibk: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibjakaj_aibk: do a = a0, n1acd
if (a == b) cycle a_aibjakaj_aibk
i_aibjakaj_aibk: do i = n0i, n1i
if (i == k) cycle i_aibjakaj_aibk
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakaj_aibk(t2, &
 nocc, nactive, a, i, b, k)
j1 = min(i - 1, k - 1, n1jl)
j_aibjakaj_aibk: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakaj_aibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakaj_aibk
end do i_aibjakaj_aibk
end do a_aibjakaj_aibk
end do b_aibjakaj_aibk
end do k_aibjakaj_aibk
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == a, l == j
! No equalities independent of the above can hold.
!
k_aibjakaj_aibjk: do k = n0k, n1k
b_aibjakaj_aibjk: do b = n0b, n1b
j1 = min(k - 1, n1jl)
j_aibjakaj_aibjk: do j = n0jl, j1
if (j == k) cycle j_aibjakaj_aibjk
a0 = max(b + 1, n0acd)
a_aibjakaj_aibjk: do a = a0, n1acd
if (a == b) cycle a_aibjakaj_aibjk
i0 = max(j + 1, n0i)
i_aibjakaj_aibjk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjakaj_aibjk
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjakaj_aibjk(t2, nocc, nactive, a, &
 i, b, j, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakaj_aibjk
end do a_aibjakaj_aibjk
end do j_aibjakaj_aibjk
end do b_aibjakaj_aibjk
end do k_aibjakaj_aibjk
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjajal_aibl: do l = n0l, n1l
b_aibjajal_aibl: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibjajal_aibl: do a = a0, n1acd
if (a == b) cycle a_aibjajal_aibl
i_aibjajal_aibl: do i = n0i, n1i
if (i == l) cycle i_aibjajal_aibl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajal_aibl(t2, &
 nocc, nactive, a, i, b, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjajal_aibl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjajal_aibl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajal_aibl
end do i_aibjajal_aibl
end do a_aibjajal_aibl
end do b_aibjajal_aibl
end do l_aibjajal_aibl
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjajal_aibjl: do l = n0l, n1l
b_aibjajal_aibjl: do b = n0b, n1b
j0 = max(l + 1, n0jk)
j_aibjajal_aibjl: do j = j0, n1jk
if (j == l) cycle j_aibjajal_aibjl
a0 = max(b + 1, n0acd)
a_aibjajal_aibjl: do a = a0, n1acd
if (a == b) cycle a_aibjajal_aibjl
i0 = max(j + 1, n0i)
i_aibjajal_aibjl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjajal_aibjl
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajal_aibjl(t2, nocc, nactive, a, &
 i, b, j, l)
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajal_aibjl
end do a_aibjajal_aibjl
end do j_aibjajal_aibjl
end do b_aibjajal_aibjl
end do l_aibjajal_aibjl
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
i0 = max(j + 1, n0ikl)
i_aibjciai_ibjc: do i = i0, n1ikl
if (i == j) cycle i_aibjciai_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjciai_ibjc(t2, &
 nocc, nactive, i, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciai_ibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciai_ibjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
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
i0 = max(j + 1, n0ikl)
i_aibjciai_aibjc: do i = i0, n1ikl
if (i == j) cycle i_aibjciai_aibjc
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjciai_aibjc(t2, nocc, nactive, a, &
 i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
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
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_bjc: do c = n0c, n1c
b_aibjcjai_bjc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_bjc
j_aibjcjai_bjc: do j = n0jk, n1jk
nn0i = max(j + 1, n0il)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1il .ge. nn0i).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjai_bjc(t2, &
 nocc, nactive, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_bjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_bjc
i0 = max(j + 1, n0il)
i_aibjcjai_bjc: do i = i0, n1il
if (i == j) cycle i_aibjcjai_bjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_bjc
end do a_aibjcjai_bjc
end do j_aibjcjai_bjc
end do b_aibjcjai_bjc
end do c_aibjcjai_bjc
!
! Elementary loop  8
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
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjai_aibc(t2, &
 nocc, nactive, a, i, b, c)
j1 = min(i - 1, n1jk)
j_aibjcjai_aibc: do j = n0jk, j1
if (j == i) cycle j_aibjcjai_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjai_aibc
end do i_aibjcjai_aibc
end do a_aibjcjai_aibc
end do b_aibjcjai_aibc
end do c_aibjcjai_aibc
!
! Elementary loop  9
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
nn0i = max(j + 1, n0il)
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjai_abjc(t2, &
 nocc, nactive, a, b, j, c)
i0 = max(j + 1, n0il)
i_aibjcjai_abjc: do i = i0, n1il
if (i == j) cycle i_aibjcjai_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_abjc
end do a_aibjcjai_abjc
end do j_aibjcjai_abjc
end do b_aibjcjai_abjc
end do c_aibjcjai_abjc
!
! Elementary loop  10
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
i0 = max(j + 1, n0il)
i_aibjcjai_ibjc: do i = i0, n1il
if (i == j) cycle i_aibjcjai_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjai_ibjc(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjai_ibjc
end do i_aibjcjai_ibjc
end do j_aibjcjai_ibjc
end do b_aibjcjai_ibjc
end do c_aibjcjai_ibjc
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj_ibc: do c = n0c, n1c
b_aibjciaj_ibc: do b = n0b, n1b
if (b == c) cycle b_aibjciaj_ibc
i_aibjciaj_ibc: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1j .ge. n0jl).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjciaj_ibc(t2, &
 nocc, nactive, i, b, c)
j1 = min(i - 1, n1jl)
j_aibjciaj_ibc: do j = n0jl, j1
if (j == i) cycle j_aibjciaj_ibc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_ibc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_ibc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjciaj_ibc
end do j_aibjciaj_ibc
end do i_aibjciaj_ibc
end do b_aibjciaj_ibc
end do c_aibjciaj_ibc
!
! Elementary loop  12
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjciaj_abjc(t2, &
 nocc, nactive, a, b, j, c)
i0 = max(j + 1, n0ik)
i_aibjciaj_abjc: do i = i0, n1ik
if (i == j) cycle i_aibjciaj_abjc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj_abjc
end do a_aibjciaj_abjc
end do j_aibjciaj_abjc
end do b_aibjciaj_abjc
end do c_aibjciaj_abjc
!
! Elementary loop  13
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjciaj_aibc(t2, &
 nocc, nactive, a, i, b, c)
j1 = min(i - 1, n1jl)
j_aibjciaj_aibc: do j = n0jl, j1
if (j == i) cycle j_aibjciaj_aibc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjciaj_aibc
end do i_aibjciaj_aibc
end do a_aibjciaj_aibc
end do b_aibjciaj_aibc
end do c_aibjciaj_aibc
!
! Elementary loop  14
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
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjciaj_ibjc(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjciaj_ibjc
end do i_aibjciaj_ibjc
end do j_aibjciaj_ibjc
end do b_aibjciaj_ibjc
end do c_aibjciaj_ibjc
!
! Elementary loop  15
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
i0 = max(j + 1, n0i)
i_aibjcjaj_ibjc: do i = i0, n1i
if (i == j) cycle i_aibjcjaj_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjaj_ibjc(t2, &
 nocc, nactive, i, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaj_ibjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjaj_ibjc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjaj_ibjc
end do i_aibjcjaj_ibjc
end do j_aibjcjaj_ibjc
end do b_aibjcjaj_ibjc
end do c_aibjcjaj_ibjc
!
! Elementary loop  16
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
i0 = max(j + 1, n0i)
i_aibjcjaj_aibjc: do i = i0, n1i
if (i == j) cycle i_aibjcjaj_aibjc
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcjaj_aibjc(t2, nocc, nactive, a, &
 i, b, j, c)
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaj_aibjc
end do a_aibjcjaj_aibjc
end do j_aibjcjaj_aibjc
end do b_aibjcjaj_aibjc
end do c_aibjcjaj_aibjc
!
! Elementary loop  17
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcicj_abjc(t2, &
 nocc, nactive, a, b, j, c)
i0 = max(j + 1, n0ik)
i_aibjcicj_abjc: do i = i0, n1ik
if (i == j) cycle i_aibjcicj_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicj_abjc
end do a_aibjcicj_abjc
end do j_aibjcicj_abjc
end do b_aibjcicj_abjc
end do c_aibjcicj_abjc
!
! Elementary loop  18
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjcicj_aibc(t2, &
 nocc, nactive, a, i, b, c)
j1 = min(i - 1, n1jl)
j_aibjcicj_aibc: do j = n0jl, j1
if (j == i) cycle j_aibjcicj_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcicj_aibc
end do i_aibjcicj_aibc
end do a_aibjcicj_aibc
end do b_aibjcicj_aibc
end do c_aibjcicj_aibc
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjaidi_ibjd: do d = n0d, n1d
b_aibjaidi_ibjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidi_ibjd
j_aibjaidi_ibjd: do j = n0j, n1j
i0 = max(j + 1, n0ikl)
i_aibjaidi_ibjd: do i = i0, n1ikl
if (i == j) cycle i_aibjaidi_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaidi_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidi_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidi_ibjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidi_ibjd
end do i_aibjaidi_ibjd
end do j_aibjaidi_ibjd
end do b_aibjaidi_ibjd
end do d_aibjaidi_ibjd
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjaidi_aibjd: do d = n0d, n1d
b_aibjaidi_aibjd: do b = n0b, n1b
if (b == d) cycle b_aibjaidi_aibjd
j_aibjaidi_aibjd: do j = n0j, n1j
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidi_aibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidi_aibjd
i0 = max(j + 1, n0ikl)
i_aibjaidi_aibjd: do i = i0, n1ikl
if (i == j) cycle i_aibjaidi_aibjd
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaidi_aibjd(t2, nocc, nactive, a, &
 i, b, j, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidi_aibjd
end do a_aibjaidi_aibjd
end do j_aibjaidi_aibjd
end do b_aibjaidi_aibjd
end do d_aibjaidi_aibjd
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi_ibd: do d = n0d, n1d
b_aibjajdi_ibd: do b = n0b, n1b
if (b == d) cycle b_aibjajdi_ibd
i_aibjajdi_ibd: do i = n0il, n1il
nn1j = min(i - 1, n1jk)
nn0a = max(b + 1, d + 1, n0ac)
if ((nn1j .ge. n0jk).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajdi_ibd(t2, &
 nocc, nactive, i, b, d)
j1 = min(i - 1, n1jk)
j_aibjajdi_ibd: do j = n0jk, j1
if (j == i) cycle j_aibjajdi_ibd
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_ibd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_ibd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdi_ibd
end do j_aibjajdi_ibd
end do i_aibjajdi_ibd
end do b_aibjajdi_ibd
end do d_aibjajdi_ibd
!
! Elementary loop  22
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
i0 = max(j + 1, n0il)
i_aibjajdi_ibjd: do i = i0, n1il
if (i == j) cycle i_aibjajdi_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajdi_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi_ibjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdi_ibjd
end do i_aibjajdi_ibjd
end do j_aibjajdi_ibjd
end do b_aibjajdi_ibjd
end do d_aibjajdi_ibjd
!
! Elementary loop  23
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
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajdi_aibd(t2, &
 nocc, nactive, a, i, b, d)
j1 = min(i - 1, n1jk)
j_aibjajdi_aibd: do j = n0jk, j1
if (j == i) cycle j_aibjajdi_aibd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajdi_aibd
end do i_aibjajdi_aibd
end do a_aibjajdi_aibd
end do b_aibjajdi_aibd
end do d_aibjajdi_aibd
!
! Elementary loop  24
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
nn0i = max(j + 1, n0il)
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajdi_abjd(t2, &
 nocc, nactive, a, b, j, d)
i0 = max(j + 1, n0il)
i_aibjajdi_abjd: do i = i0, n1il
if (i == j) cycle i_aibjajdi_abjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdi_abjd
end do a_aibjajdi_abjd
end do j_aibjajdi_abjd
end do b_aibjajdi_abjd
end do d_aibjajdi_abjd
!
! Elementary loop  25
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
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaidj_bjd(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_bjd
end do a_aibjaidj_bjd
end do j_aibjaidj_bjd
end do b_aibjaidj_bjd
end do d_aibjaidj_bjd
!
! Elementary loop  26
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
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaidj_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_ibjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidj_ibjd
end do i_aibjaidj_ibjd
end do j_aibjaidj_ibjd
end do b_aibjaidj_ibjd
end do d_aibjaidj_ibjd
!
! Elementary loop  27
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaidj_abjd(t2, &
 nocc, nactive, a, b, j, d)
i0 = max(j + 1, n0ik)
i_aibjaidj_abjd: do i = i0, n1ik
if (i == j) cycle i_aibjaidj_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_abjd
end do a_aibjaidj_abjd
end do j_aibjaidj_abjd
end do b_aibjaidj_abjd
end do d_aibjaidj_abjd
!
! Elementary loop  28
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaidj_aibd(t2, &
 nocc, nactive, a, i, b, d)
j1 = min(i - 1, n1jl)
j_aibjaidj_aibd: do j = n0jl, j1
if (j == i) cycle j_aibjaidj_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjaidj_aibd
end do i_aibjaidj_aibd
end do a_aibjaidj_aibd
end do b_aibjaidj_aibd
end do d_aibjaidj_aibd
!
! Elementary loop  29
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
i0 = max(j + 1, n0i)
i_aibjajdj_ibjd: do i = i0, n1i
if (i == j) cycle i_aibjajdj_ibjd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajdj_ibjd(t2, &
 nocc, nactive, i, b, j, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdj_ibjd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdj_ibjd
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajdj_ibjd
end do i_aibjajdj_ibjd
end do j_aibjajdj_ibjd
end do b_aibjajdj_ibjd
end do d_aibjajdj_ibjd
!
! Elementary loop  30
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
i0 = max(j + 1, n0i)
i_aibjajdj_aibjd: do i = i0, n1i
if (i == j) cycle i_aibjajdj_aibjd
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajdj_aibjd(t2, nocc, nactive, a, &
 i, b, j, d)
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajdj_aibjd
end do a_aibjajdj_aibjd
end do j_aibjajdj_aibjd
end do b_aibjajdj_aibjd
end do d_aibjajdj_aibjd
end subroutine ccjac_22_tripletpm_dav_part5
end module ccjac_block_22_tripletpm_dav_part5
