module ccjac_block_22_tripletpp_dav_part4
use eom_ccsd_22_tripletpp_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-05 15:48:39 UTC.
!
contains
subroutine ccjac_22_tripletpp_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: i, j
integer :: ai, bj, ck, dl
integer :: nn0a, nn0b, nn0i, nn1a, nn1b, nn1j
integer :: a0, a1, b0, b1, i0, j1
integer :: n0ac, n0ad, n0bc, n0bd, n0ik
integer :: n0jl
integer :: n1ac, n1ad, n1bc, n1bd, n1ik
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
n0ac = max(n0a, n0c)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0ik = max(n0i, n0k)
n0jl = max(n0j, n0l)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1ik = min(n1i, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_bd: do d = n0d, n1d
b_aibjaidj_bd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_bd
nn0i = max(n0jl + 1, n0ik)
nn0a = max(b + 1, d + 1, n0ac)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidj_bd(t2, &
 nocc, nactive, b, d)
j_aibjaidj_bd: do j = n0jl, n1jl
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_bd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_bd
i0 = max(j + 1, n0ik)
i_aibjaidj_bd: do i = i0, n1ik
if (i == j) cycle i_aibjaidj_bd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_bd
end do a_aibjaidj_bd
end do j_aibjaidj_bd
end do b_aibjaidj_bd
end do d_aibjaidj_bd
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_abd: do d = n0d, n1d
b_aibjaidj_abd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_abd
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_abd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_abd
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidj_abd(t2, &
 nocc, nactive, a, b, d)
j_aibjaidj_abd: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjaidj_abd: do i = i0, n1ik
if (i == j) cycle i_aibjaidj_abd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_abd
end do j_aibjaidj_abd
end do a_aibjaidj_abd
end do b_aibjaidj_abd
end do d_aibjaidj_abd
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj_ibd: do d = n0d, n1d
b_aibjaidj_ibd: do b = n0b, n1b
if (b == d) cycle b_aibjaidj_ibd
i_aibjaidj_ibd: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
nn0a = max(b + 1, d + 1, n0ac)
if ((nn1j .ge. n0jl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidj_ibd(t2, &
 nocc, nactive, i, b, d)
j1 = min(i - 1, n1jl)
j_aibjaidj_ibd: do j = n0jl, j1
if (j == i) cycle j_aibjaidj_ibd
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj_ibd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj_ibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidj_ibd
end do j_aibjaidj_ibd
end do i_aibjaidj_ibd
end do b_aibjaidj_ibd
end do d_aibjaidj_ibd
!
! Elementary loop  4
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
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidj_bjd(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_bjd
end do a_aibjaidj_bjd
end do j_aibjaidj_bjd
end do b_aibjaidj_bjd
end do d_aibjaidj_bjd
!
! Elementary loop  5
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
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidj_ibjd(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaidj_ibjd
end do i_aibjaidj_ibjd
end do j_aibjaidj_ibjd
end do b_aibjaidj_ibjd
end do d_aibjaidj_ibjd
!
! Elementary loop  6
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidj_aibd(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjaidj_aibd
end do i_aibjaidj_aibd
end do a_aibjaidj_aibd
end do b_aibjaidj_aibd
end do d_aibjaidj_aibd
!
! Elementary loop  7
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaidj_abjd(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaidj_abjd
end do a_aibjaidj_abjd
end do j_aibjaidj_abjd
end do b_aibjaidj_abjd
end do d_aibjaidj_abjd
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj_bc: do c = n0c, n1c
b_aibjciaj_bc: do b = n0b, n1b
if (b == c) cycle b_aibjciaj_bc
nn0i = max(n0jl + 1, n0ik)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjciaj_bc(t2, &
 nocc, nactive, b, c)
j_aibjciaj_bc: do j = n0jl, n1jl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_bc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_bc
i0 = max(j + 1, n0ik)
i_aibjciaj_bc: do i = i0, n1ik
if (i == j) cycle i_aibjciaj_bc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj_bc
end do a_aibjciaj_bc
end do j_aibjciaj_bc
end do b_aibjciaj_bc
end do c_aibjciaj_bc
!
! Elementary loop  9
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
if ((nn1j .ge. n0jl).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjciaj_ibc(t2, &
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
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjciaj_ibc
end do j_aibjciaj_ibc
end do i_aibjciaj_ibc
end do b_aibjciaj_ibc
end do c_aibjciaj_ibc
!
! Elementary loop  10
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
if ((n1ik .ge. nn0i).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjciaj_bjc(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
! Elementary loop  11
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
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjciaj_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjciaj_abc: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjciaj_abc: do i = i0, n1ik
if (i == j) cycle i_aibjciaj_abc
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj_abc
end do j_aibjciaj_abc
end do a_aibjciaj_abc
end do b_aibjciaj_abc
end do c_aibjciaj_abc
!
! Elementary loop  12
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
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjciaj_ibjc(t2, &
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
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjciaj_ibjc
end do i_aibjciaj_ibjc
end do j_aibjciaj_ibjc
end do b_aibjciaj_ibjc
end do c_aibjciaj_ibjc
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjciaj_aibc(t2, &
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
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
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
c_aibjciaj_abjc: do c = n0c, n1c
b_aibjciaj_abjc: do b = n0b, n1b
if (b == c) cycle b_aibjciaj_abjc
j_aibjciaj_abjc: do j = n0jl, n1jl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj_abjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj_abjc
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjciaj_abjc(t2, &
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
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjciaj_abjc
end do a_aibjciaj_abjc
end do j_aibjciaj_abjc
end do b_aibjciaj_abjc
end do c_aibjciaj_abjc
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_ad: do d = n0d, n1d
a_aibjbidj_ad: do a = n0a, n1a
if (a == d) cycle a_aibjbidj_ad
nn0i = max(n0jl + 1, n0ik)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jl .ge. n0jl).and. (nn1b .ge. nn0b).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidj_ad(t2, &
 nocc, nactive, a, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidj_ad: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidj_ad
j_aibjbidj_ad: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjbidj_ad: do i = i0, n1ik
if (i == j) cycle i_aibjbidj_ad
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_ad
end do j_aibjbidj_ad
end do b_aibjbidj_ad
end do a_aibjbidj_ad
end do d_aibjbidj_ad
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_abd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbidj_abd: do b = b0, n1bc
if (b == d) cycle b_aibjbidj_abd
a0 = max(b + 1, n0a)
a_aibjbidj_abd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidj_abd
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidj_abd(t2, &
 nocc, nactive, a, b, d)
j_aibjbidj_abd: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjbidj_abd: do i = i0, n1ik
if (i == j) cycle i_aibjbidj_abd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_abd
end do j_aibjbidj_abd
end do a_aibjbidj_abd
end do b_aibjbidj_abd
end do d_aibjbidj_abd
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_aid: do d = n0d, n1d
a_aibjbidj_aid: do a = n0a, n1a
if (a == d) cycle a_aibjbidj_aid
i_aibjbidj_aid: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1j .ge. n0jl).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidj_aid(t2, &
 nocc, nactive, a, i, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidj_aid: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidj_aid
j1 = min(i - 1, n1jl)
j_aibjbidj_aid: do j = n0jl, j1
if (j == i) cycle j_aibjbidj_aid
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbidj_aid
end do b_aibjbidj_aid
end do i_aibjbidj_aid
end do a_aibjbidj_aid
end do d_aibjbidj_aid
!
! Elementary loop  18
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
if ((nn1b .ge. nn0b).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidj_ajd(t2, &
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
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_ajd
end do b_aibjbidj_ajd
end do a_aibjbidj_ajd
end do j_aibjbidj_ajd
end do d_aibjbidj_ajd
!
! Elementary loop  19
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
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidj_aijd(t2, &
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
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidj_aijd
end do i_aibjbidj_aijd
end do a_aibjbidj_aijd
end do j_aibjbidj_aijd
end do d_aibjbidj_aijd
!
! Elementary loop  20
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidj_aibd(t2, &
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
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbidj_aibd
end do i_aibjbidj_aibd
end do a_aibjbidj_aibd
end do b_aibjbidj_aibd
end do d_aibjbidj_aibd
!
! Elementary loop  21
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidj_abjd(t2, &
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
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_abjd
end do a_aibjbidj_abjd
end do j_aibjbidj_abjd
end do b_aibjbidj_abjd
end do d_aibjbidj_abjd
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
nn0i = max(n0jl + 1, n0ik)
nn1b = min(a - 1, c - 1, n1bd)
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibj_ac(t2, &
 nocc, nactive, a, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_ac: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_ac
j_aibjcibj_ac: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjcibj_ac: do i = i0, n1ik
if (i == j) cycle i_aibjcibj_ac
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
nn1j = min(i - 1, n1jl)
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibj_aic(t2, &
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
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
nn0i = max(j + 1, n0ik)
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibj_ajc(t2, &
 nocc, nactive, a, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_ajc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_ajc
i0 = max(j + 1, n0ik)
i_aibjcibj_ajc: do i = i0, n1ik
if (i == j) cycle i_aibjcibj_ajc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibj_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcibj_abc: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjcibj_abc: do i = i0, n1ik
if (i == j) cycle i_aibjcibj_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
c_aibjcibj_aijc: do c = n0c, n1c
j_aibjcibj_aijc: do j = n0jl, n1jl
a_aibjcibj_aijc: do a = n0a, n1a
if (a == c) cycle a_aibjcibj_aijc
i0 = max(j + 1, n0ik)
i_aibjcibj_aijc: do i = i0, n1ik
if (i == j) cycle i_aibjcibj_aijc
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibj_aijc(t2, &
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
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibj_aijc
end do i_aibjcibj_aijc
end do a_aibjcibj_aijc
end do j_aibjcibj_aijc
end do c_aibjcibj_aijc
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
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibj_aibc(t2, &
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
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
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
c_aibjcibj_abjc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibj_abjc: do b = n0bd, b1
if (b == c) cycle b_aibjcibj_abjc
j_aibjcibj_abjc: do j = n0jl, n1jl
a0 = max(b + 1, n0a)
a_aibjcibj_abjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibj_abjc
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibj_abjc(t2, &
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
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibj_abjc
end do a_aibjcibj_abjc
end do j_aibjcibj_abjc
end do b_aibjcibj_abjc
end do c_aibjcibj_abjc
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
a_aibjaibj_a: do a = n0ac, n1ac
nn0i = max(n0jl + 1, n0ik)
nn1b = min(a - 1, n1bd)
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_a(t2, &
 nocc, nactive, a)
b1 = min(a - 1, n1bd)
b_aibjaibj_a: do b = n0bd, b1
if (b == a) cycle b_aibjaibj_a
j_aibjaibj_a: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjaibj_a: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_a
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_a
end do j_aibjaibj_a
end do b_aibjaibj_a
end do a_aibjaibj_a
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_b: do b = n0bd, n1bd
nn0i = max(n0jl + 1, n0ik)
nn0a = max(b + 1, n0ac)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_b(t2, &
 nocc, nactive, b)
j_aibjaibj_b: do j = n0jl, n1jl
a0 = max(b + 1, n0ac)
a_aibjaibj_b: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_b
i0 = max(j + 1, n0ik)
i_aibjaibj_b: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_b
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_b
end do a_aibjaibj_b
end do j_aibjaibj_b
end do b_aibjaibj_b
end subroutine ccjac_22_tripletpp_dav_part4
end module ccjac_block_22_tripletpp_dav_part4
