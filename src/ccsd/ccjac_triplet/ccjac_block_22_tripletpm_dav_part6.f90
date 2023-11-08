module ccjac_block_22_tripletpm_dav_part6
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
subroutine ccjac_22_tripletpm_dav_part6(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a, b
integer :: i, j
integer :: ai, bj, ck, dl
integer :: nn0a, nn0i, nn1b, nn1j
integer :: a0, b1, i0, j1
integer :: n0ac, n0acd, n0bcd, n0bd, n0ik
integer :: n0ikl, n0il, n0jk, n0jkl, n0jl
integer :: n1ac, n1acd, n1bcd, n1bd, n1ik
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
n0bcd = max(n0b, n0c, n0d)
n0bd = max(n0b, n0d)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1bcd = min(n1b, n1c, n1d)
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
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbibj_abj: do b = n0bcd, n1bcd
j_aibjbibj_abj: do j = n0jl, n1jl
a0 = max(b + 1, n0a)
a_aibjbibj_abj: do a = a0, n1a
if (a == b) cycle a_aibjbibj_abj
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbibj_abj(t2, &
 nocc, nactive, a, b, j)
i0 = max(j + 1, n0ik)
i_aibjbibj_abj: do i = i0, n1ik
if (i == j) cycle i_aibjbibj_abj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibj_abj
end do a_aibjbibj_abj
end do j_aibjbibj_abj
end do b_aibjbibj_abj
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbibj_aib: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibjbibj_aib: do a = a0, n1a
if (a == b) cycle a_aibjbibj_aib
i_aibjbibj_aib: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbibj_aib(t2, &
 nocc, nactive, a, i, b)
j1 = min(i - 1, n1jl)
j_aibjbibj_aib: do j = n0jl, j1
if (j == i) cycle j_aibjbibj_aib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbibj_aib
end do i_aibjbibj_aib
end do a_aibjbibj_aib
end do b_aibjbibj_aib
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbibj_aibj: do b = n0bcd, n1bcd
j_aibjbibj_aibj: do j = n0jl, n1jl
a0 = max(b + 1, n0a)
a_aibjbibj_aibj: do a = a0, n1a
if (a == b) cycle a_aibjbibj_aibj
i0 = max(j + 1, n0ik)
i_aibjbibj_aibj: do i = i0, n1ik
if (i == j) cycle i_aibjbibj_aibj
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjbibj_aibj(t2, nocc, nactive, a, i, &
 b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibj_aibj
end do a_aibjbibj_aibj
end do j_aibjbibj_aibj
end do b_aibjbibj_aibj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjaibi_ibj: do b = n0bd, n1bd
j_aibjaibi_ibj: do j = n0j, n1j
i0 = max(j + 1, n0ikl)
i_aibjaibi_ibj: do i = i0, n1ikl
if (i == j) cycle i_aibjaibi_ibj
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaibi_ibj(t2, &
 nocc, nactive, i, b, j)
a0 = max(b + 1, n0ac)
a_aibjaibi_ibj: do a = a0, n1ac
if (a == b) cycle a_aibjaibi_ibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaibi_ibj
end do i_aibjaibi_ibj
end do j_aibjaibi_ibj
end do b_aibjaibi_ibj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
j_aibjaibi_aij: do j = n0j, n1j
a_aibjaibi_aij: do a = n0ac, n1ac
i0 = max(j + 1, n0ikl)
i_aibjaibi_aij: do i = i0, n1ikl
if (i == j) cycle i_aibjaibi_aij
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaibi_aij(t2, &
 nocc, nactive, a, i, j)
b1 = min(a - 1, n1bd)
b_aibjaibi_aij: do b = n0bd, b1
if (b == a) cycle b_aibjaibi_aij
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjaibi_aij
end do i_aibjaibi_aij
end do a_aibjaibi_aij
end do j_aibjaibi_aij
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjaibi_aibj: do b = n0bd, n1bd
j_aibjaibi_aibj: do j = n0j, n1j
a0 = max(b + 1, n0ac)
a_aibjaibi_aibj: do a = a0, n1ac
if (a == b) cycle a_aibjaibi_aibj
i0 = max(j + 1, n0ikl)
i_aibjaibi_aibj: do i = i0, n1ikl
if (i == j) cycle i_aibjaibi_aibj
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaibi_aibj(t2, nocc, nactive, a, i, &
 b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibi_aibj
end do a_aibjaibi_aibj
end do j_aibjaibi_aibj
end do b_aibjaibi_aibj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi_ib: do b = n0bd, n1bd
i_aibjajbi_ib: do i = n0il, n1il
nn1j = min(i - 1, n1jk)
nn0a = max(b + 1, n0ac)
if ((nn1j .ge. n0jk).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbi_ib(t2, &
 nocc, nactive, i, b)
j1 = min(i - 1, n1jk)
j_aibjajbi_ib: do j = n0jk, j1
if (j == i) cycle j_aibjajbi_ib
a0 = max(b + 1, n0ac)
a_aibjajbi_ib: do a = a0, n1ac
if (a == b) cycle a_aibjajbi_ib
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbi_ib
end do j_aibjajbi_ib
end do i_aibjajbi_ib
end do b_aibjajbi_ib
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
j_aibjajbi_aj: do j = n0jk, n1jk
a_aibjajbi_aj: do a = n0ac, n1ac
nn0i = max(j + 1, n0il)
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbi_aj(t2, &
 nocc, nactive, a, j)
b1 = min(a - 1, n1bd)
b_aibjajbi_aj: do b = n0bd, b1
if (b == a) cycle b_aibjajbi_aj
i0 = max(j + 1, n0il)
i_aibjajbi_aj: do i = i0, n1il
if (i == j) cycle i_aibjajbi_aj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbi_aj
end do b_aibjajbi_aj
end do a_aibjajbi_aj
end do j_aibjajbi_aj
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi_ibj: do b = n0bd, n1bd
j_aibjajbi_ibj: do j = n0jk, n1jk
i0 = max(j + 1, n0il)
i_aibjajbi_ibj: do i = i0, n1il
if (i == j) cycle i_aibjajbi_ibj
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbi_ibj(t2, &
 nocc, nactive, i, b, j)
a0 = max(b + 1, n0ac)
a_aibjajbi_ibj: do a = a0, n1ac
if (a == b) cycle a_aibjajbi_ibj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbi_ibj
end do i_aibjajbi_ibj
end do j_aibjajbi_ibj
end do b_aibjajbi_ibj
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
j_aibjajbi_aij: do j = n0jk, n1jk
a_aibjajbi_aij: do a = n0ac, n1ac
i0 = max(j + 1, n0il)
i_aibjajbi_aij: do i = i0, n1il
if (i == j) cycle i_aibjajbi_aij
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbi_aij(t2, &
 nocc, nactive, a, i, j)
b1 = min(a - 1, n1bd)
b_aibjajbi_aij: do b = n0bd, b1
if (b == a) cycle b_aibjajbi_aij
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjajbi_aij
end do i_aibjajbi_aij
end do a_aibjajbi_aij
end do j_aibjajbi_aij
!
! Elementary loop  11
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
i_aibjajbi_aib: do i = n0il, n1il
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbi_aib(t2, &
 nocc, nactive, a, i, b)
j1 = min(i - 1, n1jk)
j_aibjajbi_aib: do j = n0jk, j1
if (j == i) cycle j_aibjajbi_aib
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbi_aib
end do i_aibjajbi_aib
end do a_aibjajbi_aib
end do b_aibjajbi_aib
!
! Elementary loop  12
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
nn0i = max(j + 1, n0il)
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbi_abj(t2, &
 nocc, nactive, a, b, j)
i0 = max(j + 1, n0il)
i_aibjajbi_abj: do i = i0, n1il
if (i == j) cycle i_aibjajbi_abj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbi_abj
end do a_aibjajbi_abj
end do j_aibjajbi_abj
end do b_aibjajbi_abj
!
! Elementary loop  13
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
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaibj_bj(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_bj
end do a_aibjaibj_bj
end do j_aibjaibj_bj
end do b_aibjaibj_bj
!
! Elementary loop  14
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
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaibj_ai(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do j_aibjaibj_ai
end do b_aibjaibj_ai
end do i_aibjaibj_ai
end do a_aibjaibj_ai
!
! Elementary loop  15
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
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaibj_ibj(t2, &
 nocc, nactive, i, b, j)
a0 = max(b + 1, n0ac)
a_aibjaibj_ibj: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_ibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibjaibj_ibj
end do i_aibjaibj_ibj
end do j_aibjaibj_ibj
end do b_aibjaibj_ibj
!
! Elementary loop  16
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
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaibj_aij(t2, &
 nocc, nactive, a, i, j)
b1 = min(a - 1, n1bd)
b_aibjaibj_aij: do b = n0bd, b1
if (b == a) cycle b_aibjaibj_aij
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do b_aibjaibj_aij
end do i_aibjaibj_aij
end do a_aibjaibj_aij
end do j_aibjaibj_aij
!
! Elementary loop  17
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaibj_abj(t2, &
 nocc, nactive, a, b, j)
i0 = max(j + 1, n0ik)
i_aibjaibj_abj: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_abj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_abj
end do a_aibjaibj_abj
end do j_aibjaibj_abj
end do b_aibjaibj_abj
!
! Elementary loop  18
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaibj_aib(t2, &
 nocc, nactive, a, i, b)
j1 = min(i - 1, n1jl)
j_aibjaibj_aib: do j = n0jl, j1
if (j == i) cycle j_aibjaibj_aib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do j_aibjaibj_aib
end do i_aibjaibj_aib
end do a_aibjaibj_aib
end do b_aibjaibj_aib
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, d == b, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjajbj_ibj: do b = n0bd, n1bd
j_aibjajbj_ibj: do j = n0jkl, n1jkl
i0 = max(j + 1, n0i)
i_aibjajbj_ibj: do i = i0, n1i
if (i == j) cycle i_aibjajbj_ibj
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbj_ibj(t2, &
 nocc, nactive, i, b, j)
a0 = max(b + 1, n0ac)
a_aibjajbj_ibj: do a = a0, n1ac
if (a == b) cycle a_aibjajbj_ibj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbj_ibj
end do i_aibjajbj_ibj
end do j_aibjajbj_ibj
end do b_aibjajbj_ibj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, d == b, k == j, l == j
! No equalities independent of the above can hold.
!
j_aibjajbj_aij: do j = n0jkl, n1jkl
a_aibjajbj_aij: do a = n0ac, n1ac
i0 = max(j + 1, n0i)
i_aibjajbj_aij: do i = i0, n1i
if (i == j) cycle i_aibjajbj_aij
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbj_aij(t2, &
 nocc, nactive, a, i, j)
b1 = min(a - 1, n1bd)
b_aibjajbj_aij: do b = n0bd, b1
if (b == a) cycle b_aibjajbj_aij
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjajbj_aij
end do i_aibjajbj_aij
end do a_aibjajbj_aij
end do j_aibjajbj_aij
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, d == b, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjajbj_aibj: do b = n0bd, n1bd
j_aibjajbj_aibj: do j = n0jkl, n1jkl
a0 = max(b + 1, n0ac)
a_aibjajbj_aibj: do a = a0, n1ac
if (a == b) cycle a_aibjajbj_aibj
i0 = max(j + 1, n0i)
i_aibjajbj_aibj: do i = i0, n1i
if (i == j) cycle i_aibjajbj_aibj
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjajbj_aibj(t2, nocc, nactive, a, i, &
 b, j)
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbj_aibj
end do a_aibjajbj_aibj
end do j_aibjajbj_aibj
end do b_aibjajbj_aibj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiaj_abj: do b = n0b, n1b
j_aibjaiaj_abj: do j = n0jl, n1jl
a0 = max(b + 1, n0acd)
a_aibjaiaj_abj: do a = a0, n1acd
if (a == b) cycle a_aibjaiaj_abj
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaiaj_abj(t2, &
 nocc, nactive, a, b, j)
i0 = max(j + 1, n0ik)
i_aibjaiaj_abj: do i = i0, n1ik
if (i == j) cycle i_aibjaiaj_abj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiaj_abj
end do a_aibjaiaj_abj
end do j_aibjaiaj_abj
end do b_aibjaiaj_abj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiaj_aib: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibjaiaj_aib: do a = a0, n1acd
if (a == b) cycle a_aibjaiaj_aib
i_aibjaiaj_aib: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaiaj_aib(t2, &
 nocc, nactive, a, i, b)
j1 = min(i - 1, n1jl)
j_aibjaiaj_aib: do j = n0jl, j1
if (j == i) cycle j_aibjaiaj_aib
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjaiaj_aib
end do i_aibjaiaj_aib
end do a_aibjaiaj_aib
end do b_aibjaiaj_aib
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiaj_aibj: do b = n0b, n1b
j_aibjaiaj_aibj: do j = n0jl, n1jl
a0 = max(b + 1, n0acd)
a_aibjaiaj_aibj: do a = a0, n1acd
if (a == b) cycle a_aibjaiaj_aibj
i0 = max(j + 1, n0ik)
i_aibjaiaj_aibj: do i = i0, n1ik
if (i == j) cycle i_aibjaiaj_aibj
jac_ibra_iket = eom_ccsd_22_tripletpm_trans_aibjaiaj_aibj(t2, nocc, nactive, a, i, &
 b, j)
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiaj_aibj
end do a_aibjaiaj_aibj
end do j_aibjaiaj_aibj
end do b_aibjaiaj_aibj
end subroutine ccjac_22_tripletpm_dav_part6
end module ccjac_block_22_tripletpm_dav_part6
