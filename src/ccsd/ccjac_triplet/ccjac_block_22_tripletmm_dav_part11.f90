module ccjac_block_22_tripletmm_dav_part11
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
subroutine ccjac_22_tripletmm_dav_part11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: i, j, k
integer :: ai, bj, ck, dl
integer :: nn0a, nn0i, nn1a, nn1b
integer :: a0, a1, b1, i0
integer :: n0abd, n0ac, n0bd, n0ij, n0ijkl
integer :: n0ijl, n0ik, n0ikl, n0il, n0jk
integer :: n0jkl, n0jl, n0kl
integer :: n1abd, n1ac, n1bd, n1ij, n1ijkl
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
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1bd = min(n1b, n1d)
n1ij = min(n1i, n1j)
n1ijkl = min(n1i, n1j, n1k, n1l)
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
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_bj: do b = n0bd, n1bd
j_aibjaibj_bj: do j = n0jl, n1jl
nn0a = max(b + 1, n0ac)
if ((n1ik .ge. n0ik).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibj_bj(t2, &
 nocc, nactive, b, j)
a0 = max(b + 1, n0ac)
a_aibjaibj_bj: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_bj
i_aibjaibj_bj: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_bj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_bj
end do a_aibjaibj_bj
end do j_aibjaibj_bj
end do b_aibjaibj_bj
!
! Elementary loop  2
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
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibj_ab(t2, &
 nocc, nactive, a, b)
j_aibjaibj_ab: do j = n0jl, n1jl
i_aibjaibj_ab: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_ab
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_ab
end do j_aibjaibj_ab
end do a_aibjaibj_ab
end do b_aibjaibj_ab
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
j_aibjaibj_ij: do j = n0jl, n1jl
i_aibjaibj_ij: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_ij
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibj_ij(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibjaibj_ij
end do b_aibjaibj_ij
end do i_aibjaibj_ij
end do j_aibjaibj_ij
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_ibj: do b = n0bd, n1bd
j_aibjaibj_ibj: do j = n0jl, n1jl
i_aibjaibj_ibj: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_ibj
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibj_ibj(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibjaibj_ibj
end do i_aibjaibj_ibj
end do j_aibjaibj_ibj
end do b_aibjaibj_ibj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
j_aibjaibj_aij: do j = n0jl, n1jl
a_aibjaibj_aij: do a = n0ac, n1ac
i_aibjaibj_aij: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_aij
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibj_aij(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do b_aibjaibj_aij
end do i_aibjaibj_aij
end do a_aibjaibj_aij
end do j_aibjaibj_aij
!
! Elementary loop  6
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
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibj_abj(t2, &
 nocc, nactive, a, b, j)
i_aibjaibj_abj: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_abj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_abj
end do a_aibjaibj_abj
end do j_aibjaibj_abj
end do b_aibjaibj_abj
!
! Elementary loop  7
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
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibj_aib(t2, &
 nocc, nactive, a, i, b)
j_aibjaibj_aib: do j = n0jl, n1jl
if (j == i) cycle j_aibjaibj_aib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_diag(jac_ibra_iket, ibra)
end do j_aibjaibj_aib
end do i_aibjaibj_aib
end do a_aibjaibj_aib
end do b_aibjaibj_aib
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakbi_ik: do k = n0k, n1k
i_aibiakbi_ik: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakbi_ik
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbi_ik(t2, &
 nocc, nactive, i, k)
b_aibiakbi_ik: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiakbi_ik: do a = a0, n1ac
if (a == b) cycle a_aibiakbi_ik
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakbi_ik
end do b_aibiakbi_ik
end do i_aibiakbi_ik
end do k_aibiakbi_ik
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakbi_aik: do k = n0k, n1k
a_aibiakbi_aik: do a = n0ac, n1ac
i_aibiakbi_aik: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakbi_aik
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbi_aik(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibiakbi_aik
end do i_aibiakbi_aik
end do a_aibiakbi_aik
end do k_aibiakbi_aik
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakbi_ibk: do k = n0k, n1k
b_aibiakbi_ibk: do b = n0bd, n1bd
i_aibiakbi_ibk: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakbi_ibk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbi_ibk(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakbi_ibk
end do i_aibiakbi_ibk
end do b_aibiakbi_ibk
end do k_aibiakbi_ibk
!
! Elementary loop  11
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
i_aibiakbi_aibk: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakbi_aibk
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbi_aibk(t2, nocc, nactive, a, i, &
 b, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakbi_aibk
end do a_aibiakbi_aibk
end do b_aibiakbi_aibk
end do k_aibiakbi_aibk
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakbk_ik: do k = n0kl, n1kl
i_aibiakbk_ik: do i = n0ij, n1ij
if (i == k) cycle i_aibiakbk_ik
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbk_ik(t2, &
 nocc, nactive, i, k)
b_aibiakbk_ik: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiakbk_ik: do a = a0, n1ac
if (a == b) cycle a_aibiakbk_ik
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakbk_ik
end do b_aibiakbk_ik
end do i_aibiakbk_ik
end do k_aibiakbk_ik
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakbk_ibk: do k = n0kl, n1kl
b_aibiakbk_ibk: do b = n0bd, n1bd
i_aibiakbk_ibk: do i = n0ij, n1ij
if (i == k) cycle i_aibiakbk_ibk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbk_ibk(t2, &
 nocc, nactive, i, b, k)
a0 = max(b + 1, n0ac)
a_aibiakbk_ibk: do a = a0, n1ac
if (a == b) cycle a_aibiakbk_ibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakbk_ibk
end do i_aibiakbk_ibk
end do b_aibiakbk_ibk
end do k_aibiakbk_ibk
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakbk_aik: do k = n0kl, n1kl
a_aibiakbk_aik: do a = n0ac, n1ac
i_aibiakbk_aik: do i = n0ij, n1ij
if (i == k) cycle i_aibiakbk_aik
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbk_aik(t2, &
 nocc, nactive, a, i, k)
b1 = min(a - 1, n1bd)
b_aibiakbk_aik: do b = n0bd, b1
if (b == a) cycle b_aibiakbk_aik
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibiakbk_aik
end do i_aibiakbk_aik
end do a_aibiakbk_aik
end do k_aibiakbk_aik
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
j_aibjajbi_ij: do j = n0jk, n1jk
i_aibjajbi_ij: do i = n0il, n1il
if (i == j) cycle i_aibjajbi_ij
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbi_ij(t2, &
 nocc, nactive, i, j)
b_aibjajbi_ij: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjajbi_ij: do a = a0, n1ac
if (a == b) cycle a_aibjajbi_ij
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbi_ij
end do b_aibjajbi_ij
end do i_aibjajbi_ij
end do j_aibjajbi_ij
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi_ab: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjajbi_ab: do a = a0, n1ac
if (a == b) cycle a_aibjajbi_ab
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbi_ab(t2, &
 nocc, nactive, a, b)
j_aibjajbi_ab: do j = n0jk, n1jk
i_aibjajbi_ab: do i = n0il, n1il
if (i == j) cycle i_aibjajbi_ab
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbi_ab
end do j_aibjajbi_ab
end do a_aibjajbi_ab
end do b_aibjajbi_ab
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajbi_ibj: do b = n0bd, n1bd
j_aibjajbi_ibj: do j = n0jk, n1jk
i_aibjajbi_ibj: do i = n0il, n1il
if (i == j) cycle i_aibjajbi_ibj
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbi_ibj(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbi_ibj
end do i_aibjajbi_ibj
end do j_aibjajbi_ibj
end do b_aibjajbi_ibj
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
j_aibjajbi_aij: do j = n0jk, n1jk
a_aibjajbi_aij: do a = n0ac, n1ac
i_aibjajbi_aij: do i = n0il, n1il
if (i == j) cycle i_aibjajbi_aij
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbi_aij(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjajbi_aij
end do i_aibjajbi_aij
end do a_aibjajbi_aij
end do j_aibjajbi_aij
!
! Elementary loop  19
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
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbi_abj(t2, &
 nocc, nactive, a, b, j)
i_aibjajbi_abj: do i = n0il, n1il
if (i == j) cycle i_aibjajbi_abj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbi_abj
end do a_aibjajbi_abj
end do j_aibjajbi_abj
end do b_aibjajbi_abj
!
! Elementary loop  20
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
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbi_aib(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbi_aib
end do i_aibjajbi_aib
end do a_aibjajbi_aib
end do b_aibjajbi_aib
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, d == b, k == j, l == j
! No equalities independent of the above can hold.
!
j_aibjajbj_ij: do j = n0jkl, n1jkl
i_aibjajbj_ij: do i = n0i, n1i
if (i == j) cycle i_aibjajbj_ij
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbj_ij(t2, &
 nocc, nactive, i, j)
b_aibjajbj_ij: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjajbj_ij: do a = a0, n1ac
if (a == b) cycle a_aibjajbj_ij
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbj_ij
end do b_aibjajbj_ij
end do i_aibjajbj_ij
end do j_aibjajbj_ij
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, d == b, k == j, l == j
! No equalities independent of the above can hold.
!
j_aibjajbj_aij: do j = n0jkl, n1jkl
a_aibjajbj_aij: do a = n0ac, n1ac
i_aibjajbj_aij: do i = n0i, n1i
if (i == j) cycle i_aibjajbj_aij
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbj_aij(t2, &
 nocc, nactive, a, i, j)
b1 = min(a - 1, n1bd)
b_aibjajbj_aij: do b = n0bd, b1
if (b == a) cycle b_aibjajbj_aij
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjajbj_aij
end do i_aibjajbj_aij
end do a_aibjajbj_aij
end do j_aibjajbj_aij
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, d == b, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjajbj_ibj: do b = n0bd, n1bd
j_aibjajbj_ibj: do j = n0jkl, n1jkl
i_aibjajbj_ibj: do i = n0i, n1i
if (i == j) cycle i_aibjajbj_ibj
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbj_ibj(t2, &
 nocc, nactive, i, b, j)
a0 = max(b + 1, n0ac)
a_aibjajbj_ibj: do a = a0, n1ac
if (a == b) cycle a_aibjajbj_ibj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbj_ibj
end do i_aibjajbj_ibj
end do j_aibjajbj_ibj
end do b_aibjajbj_ibj
!
! Elementary loop  24
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
i_aibjajbj_aibj: do i = n0i, n1i
if (i == j) cycle i_aibjajbj_aibj
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbj_aibj(t2, nocc, nactive, a, i, &
 b, j)
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajbj_aibj
end do a_aibjajbj_aibj
end do j_aibjajbj_aibj
end do b_aibjajbj_aibj
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i
! Equalities: c == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibiaidi_bd: do d = n0d, n1d
b_aibiaidi_bd: do b = n0b, n1b
if (b == d) cycle b_aibiaidi_bd
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ijkl .ge. n0ijkl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaidi_bd(t2, &
 nocc, nactive, b, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidi_bd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidi_bd
i_aibiaidi_bd: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidi_bd
end do a_aibiaidi_bd
end do b_aibiaidi_bd
end do d_aibiaidi_bd
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i
! Equalities: c == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibiaidi_abd: do d = n0d, n1d
b_aibiaidi_abd: do b = n0b, n1b
if (b == d) cycle b_aibiaidi_abd
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidi_abd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidi_abd
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaidi_abd(t2, &
 nocc, nactive, a, b, d)
i_aibiaidi_abd: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidi_abd
end do a_aibiaidi_abd
end do b_aibiaidi_abd
end do d_aibiaidi_abd
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i
! Equalities: c == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibiaidi_ibd: do d = n0d, n1d
b_aibiaidi_ibd: do b = n0b, n1b
if (b == d) cycle b_aibiaidi_ibd
i_aibiaidi_ibd: do i = n0ijkl, n1ijkl
nn0a = max(b + 1, d + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaidi_ibd(t2, &
 nocc, nactive, i, b, d)
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidi_ibd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidi_ibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiaidi_ibd
end do i_aibiaidi_ibd
end do b_aibiaidi_ibd
end do d_aibiaidi_ibd
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i
! Equalities: c == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibiaidi_aibd: do d = n0d, n1d
b_aibiaidi_aibd: do b = n0b, n1b
if (b == d) cycle b_aibiaidi_aibd
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidi_aibd: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidi_aibd
i_aibiaidi_aibd: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiaidi_aibd(t2, nocc, nactive, a, i, &
 b, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaidi_aibd
end do a_aibiaidi_aibd
end do b_aibiaidi_aibd
end do d_aibiaidi_aibd
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiajciai_aijc: do c = n0c, n1c
j_aiajciai_aijc: do j = n0j, n1j
a1 = min(c - 1, n1abd)
a_aiajciai_aijc: do a = n0abd, a1
if (a == c) cycle a_aiajciai_aijc
i0 = max(j + 1, n0ikl)
i_aiajciai_aijc: do i = i0, n1ikl
if (i == j) cycle i_aiajciai_aijc
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajciai_aijc(t2, nocc, nactive, a, i, &
 j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciai_aijc
end do a_aiajciai_aijc
end do j_aiajciai_aijc
end do c_aiajciai_aijc
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajciaj_ac: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiajciaj_ac: do a = n0abd, a1
if (a == c) cycle a_aiajciaj_ac
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajciaj_ac(t2, &
 nocc, nactive, a, c)
j_aiajciaj_ac: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aiajciaj_ac: do i = i0, n1ik
if (i == j) cycle i_aiajciaj_ac
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaj_ac
end do j_aiajciaj_ac
end do a_aiajciaj_ac
end do c_aiajciaj_ac
end subroutine ccjac_22_tripletmm_dav_part11
end module ccjac_block_22_tripletmm_dav_part11
