module ccjac_block_22_tripletpp_dav_part5
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
subroutine ccjac_22_tripletpp_dav_part5(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: n0ac, n0bd, n0ik, n0jl
integer :: n1ac, n1bd, n1ik, n1jl
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
n0bd = max(n0b, n0d)
n0ik = max(n0i, n0k)
n0jl = max(n0j, n0l)
n1ac = min(n1a, n1c)
n1bd = min(n1b, n1d)
n1ik = min(n1i, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
i_aibjaibj_i: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
nn0a = max(n0bd + 1, n0ac)
if ((nn1j .ge. n0jl).and. (n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_i(t2, &
 nocc, nactive, i)
b_aibjaibj_i: do b = n0bd, n1bd
j1 = min(i - 1, n1jl)
j_aibjaibj_i: do j = n0jl, j1
if (j == i) cycle j_aibjaibj_i
a0 = max(b + 1, n0ac)
a_aibjaibj_i: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_i
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
end do a_aibjaibj_i
end do j_aibjaibj_i
end do b_aibjaibj_i
end do i_aibjaibj_i
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
j_aibjaibj_j: do j = n0jl, n1jl
nn0i = max(j + 1, n0ik)
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_j(t2, &
 nocc, nactive, j)
b_aibjaibj_j: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibj_j: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_j
i0 = max(j + 1, n0ik)
i_aibjaibj_j: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_j
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
end do i_aibjaibj_j
end do a_aibjaibj_j
end do b_aibjaibj_j
end do j_aibjaibj_j
!
! Elementary loop  3
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
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_ai(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do j_aibjaibj_ai
end do b_aibjaibj_ai
end do i_aibjaibj_ai
end do a_aibjaibj_ai
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
j_aibjaibj_aj: do j = n0jl, n1jl
a_aibjaibj_aj: do a = n0ac, n1ac
nn0i = max(j + 1, n0ik)
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_aj(t2, &
 nocc, nactive, a, j)
b1 = min(a - 1, n1bd)
b_aibjaibj_aj: do b = n0bd, b1
if (b == a) cycle b_aibjaibj_aj
i0 = max(j + 1, n0ik)
i_aibjaibj_aj: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_aj
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
end do i_aibjaibj_aj
end do b_aibjaibj_aj
end do a_aibjaibj_aj
end do j_aibjaibj_aj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_ib: do b = n0bd, n1bd
i_aibjaibj_ib: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
nn0a = max(b + 1, n0ac)
if ((nn1j .ge. n0jl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_ib(t2, &
 nocc, nactive, i, b)
j1 = min(i - 1, n1jl)
j_aibjaibj_ib: do j = n0jl, j1
if (j == i) cycle j_aibjaibj_ib
a0 = max(b + 1, n0ac)
a_aibjaibj_ib: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_ib
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
end do a_aibjaibj_ib
end do j_aibjaibj_ib
end do i_aibjaibj_ib
end do b_aibjaibj_ib
!
! Elementary loop  6
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
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_bj(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_bj
end do a_aibjaibj_bj
end do j_aibjaibj_bj
end do b_aibjaibj_bj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_ab: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibj_ab: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_ab
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_ab(t2, &
 nocc, nactive, a, b)
j_aibjaibj_ab: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjaibj_ab: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_ab
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
end do i_aibjaibj_ab
end do j_aibjaibj_ab
end do a_aibjaibj_ab
end do b_aibjaibj_ab
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
j_aibjaibj_ij: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjaibj_ij: do i = i0, n1ik
if (i == j) cycle i_aibjaibj_ij
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_ij(t2, &
 nocc, nactive, i, j)
b_aibjaibj_ij: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibj_ij: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_ij
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
end do a_aibjaibj_ij
end do b_aibjaibj_ij
end do i_aibjaibj_ij
end do j_aibjaibj_ij
!
! Elementary loop  9
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
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_aij(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do b_aibjaibj_aij
end do i_aibjaibj_aij
end do a_aibjaibj_aij
end do j_aibjaibj_aij
!
! Elementary loop  10
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
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_ibj(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibjaibj_ibj
end do i_aibjaibj_ibj
end do j_aibjaibj_ibj
end do b_aibjaibj_ibj
!
! Elementary loop  11
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_aib(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do j_aibjaibj_aib
end do i_aibjaibj_aib
end do a_aibjaibj_aib
end do b_aibjaibj_aib
!
! Elementary loop  12
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjaibj_abj(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_abj
end do a_aibjaibj_abj
end do j_aibjaibj_abj
end do b_aibjaibj_abj
end subroutine ccjac_22_tripletpp_dav_part5
end module ccjac_block_22_tripletpp_dav_part5
