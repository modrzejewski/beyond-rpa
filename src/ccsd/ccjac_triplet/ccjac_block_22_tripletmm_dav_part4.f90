module ccjac_block_22_tripletmm_dav_part4
use eom_ccsd_22_tripletmm_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-12-06 14:17:57 UTC.
!
contains
subroutine ccjac_22_tripletmm_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: a, b, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: nn0a, nn0i, nn0j, nn0k, nn1b, nn1i, nn1j
integer :: a0, b1, i0, i1, j0, j1, k0
integer :: n0abc, n0ac, n0acd, n0bd, n0ij
integer :: n0ik, n0il, n0jk, n0jl, n0kl
integer :: n1abc, n1ac, n1acd, n1bd, n1ij
integer :: n1ik, n1il, n1jk, n1jl, n1kl
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
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1abc = min(n1a, n1b, n1c)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1bd = min(n1b, n1d)
n1ij = min(n1i, n1j)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, l == j
! No equalities independent of the above can hold.
!
d_aiajakdj_aikd: do d = n0d, n1d
k_aiajakdj_aikd: do k = n0k, n1k
a0 = max(d + 1, n0abc)
a_aiajakdj_aikd: do a = a0, n1abc
if (a == d) cycle a_aiajakdj_aikd
i_aiajakdj_aikd: do i = n0i, n1i
if (i == k) cycle i_aiajakdj_aikd
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakdj_aikd(t2, &
 nocc, nactive, a, i, k, d)
j1 = min(i - 1, n1jl)
j_aiajakdj_aikd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aiajakdj_aikd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajakdj_aikd
end do i_aiajakdj_aikd
end do a_aiajakdj_aikd
end do k_aiajakdj_aikd
end do d_aiajakdj_aikd
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, l == j
! No equalities independent of the above can hold.
!
d_aiajakdj_aijkd: do d = n0d, n1d
k_aiajakdj_aijkd: do k = n0k, n1k
j_aiajakdj_aijkd: do j = n0jl, n1jl
if (j == k) cycle j_aiajakdj_aijkd
a0 = max(d + 1, n0abc)
a_aiajakdj_aijkd: do a = a0, n1abc
if (a == d) cycle a_aiajakdj_aijkd
i0 = max(j + 1, n0i)
i_aiajakdj_aijkd: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakdj_aijkd
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajakdj_aijkd(t2, nocc, nactive, a, &
 i, j, k, d)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajakdj_aijkd
end do a_aiajakdj_aijkd
end do j_aiajakdj_aijkd
end do k_aiajakdj_aijkd
end do d_aiajakdj_aijkd
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjaial_abjl: do l = n0l, n1l
b_aibjaial_abjl: do b = n0b, n1b
j_aibjaial_abjl: do j = n0j, n1j
if (j == l) cycle j_aibjaial_abjl
a0 = max(b + 1, n0acd)
a_aibjaial_abjl: do a = a0, n1acd
if (a == b) cycle a_aibjaial_abjl
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaial_abjl(t2, &
 nocc, nactive, a, b, j, l)
i0 = max(l + 1, n0ik)
i_aibjaial_abjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaial_abjl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaial_abjl
end do a_aibjaial_abjl
end do j_aibjaial_abjl
end do b_aibjaial_abjl
end do l_aibjaial_abjl
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjaial_aibjl: do l = n0l, n1l
b_aibjaial_aibjl: do b = n0b, n1b
j_aibjaial_aibjl: do j = n0j, n1j
if (j == l) cycle j_aibjaial_aibjl
a0 = max(b + 1, n0acd)
a_aibjaial_aibjl: do a = a0, n1acd
if (a == b) cycle a_aibjaial_aibjl
i0 = max(l + 1, n0ik)
i_aibjaial_aibjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaial_aibjl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaial_aibjl(t2, nocc, nactive, a, &
 i, b, j, l)
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaial_aibjl
end do a_aibjaial_aibjl
end do j_aibjaial_aibjl
end do b_aibjaial_aibjl
end do l_aibjaial_aibjl
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == a, j == i
! No equalities independent of the above can hold.
!
l_aibiakal_aibkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibiakal_aibkl: do k = k0, n1k
if (k == l) cycle k_aibiakal_aibkl
b_aibiakal_aibkl: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibiakal_aibkl: do a = a0, n1acd
if (a == b) cycle a_aibiakal_aibkl
i_aibiakal_aibkl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakal_aibkl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakal_aibkl(t2, nocc, nactive, a, &
 i, b, k, l)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakal_aibkl
end do a_aibiakal_aibkl
end do b_aibiakal_aibkl
end do k_aibiakal_aibkl
end do l_aibiakal_aibkl
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aibjakai_abjk: do k = n0k, n1k
b_aibjakai_abjk: do b = n0b, n1b
j_aibjakai_abjk: do j = n0j, n1j
if (j == k) cycle j_aibjakai_abjk
a0 = max(b + 1, n0acd)
a_aibjakai_abjk: do a = a0, n1acd
if (a == b) cycle a_aibjakai_abjk
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakai_abjk(t2, &
 nocc, nactive, a, b, j, k)
i1 = min(k - 1, n1il)
i_aibjakai_abjk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakai_abjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakai_abjk
end do a_aibjakai_abjk
end do j_aibjakai_abjk
end do b_aibjakai_abjk
end do k_aibjakai_abjk
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aibjakai_aibjk: do k = n0k, n1k
b_aibjakai_aibjk: do b = n0b, n1b
j_aibjakai_aibjk: do j = n0j, n1j
if (j == k) cycle j_aibjakai_aibjk
a0 = max(b + 1, n0acd)
a_aibjakai_aibjk: do a = a0, n1acd
if (a == b) cycle a_aibjakai_aibjk
i1 = min(k - 1, n1il)
i_aibjakai_aibjk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakai_aibjk
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakai_aibjk(t2, nocc, nactive, a, &
 i, b, j, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakai_aibjk
end do a_aibjakai_aibjk
end do j_aibjakai_aibjk
end do b_aibjakai_aibjk
end do k_aibjakai_aibjk
!
! Elementary loop  8
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
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajal_aibl(t2, &
 nocc, nactive, a, i, b, l)
j0 = max(l + 1, n0jk)
j_aibjajal_aibl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjajal_aibl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajal_aibl
end do i_aibjajal_aibl
end do a_aibjajal_aibl
end do b_aibjajal_aibl
end do l_aibjajal_aibl
!
! Elementary loop  9
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
i_aibjajal_aibjl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajal_aibjl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajal_aibjl(t2, nocc, nactive, a, &
 i, b, j, l)
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajal_aibjl
end do a_aibjajal_aibjl
end do j_aibjajal_aibjl
end do b_aibjajal_aibjl
end do l_aibjajal_aibjl
!
! Elementary loop  10
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
nn1j = min(k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakaj_aibk(t2, &
 nocc, nactive, a, i, b, k)
j1 = min(k - 1, n1jl)
j_aibjakaj_aibk: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakaj_aibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakaj_aibk
end do i_aibjakaj_aibk
end do a_aibjakaj_aibk
end do b_aibjakaj_aibk
end do k_aibjakaj_aibk
!
! Elementary loop  11
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
i_aibjakaj_aibjk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakaj_aibjk
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakaj_aibjk(t2, nocc, nactive, a, &
 i, b, j, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakaj_aibjk
end do a_aibjakaj_aibjk
end do j_aibjakaj_aibjk
end do b_aibjakaj_aibjk
end do k_aibjakaj_aibjk
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_jl: do l = n0l, n1l
j_aibjaibl_jl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_jl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ik .ge. n0ik).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibl_jl(t2, &
 nocc, nactive, j, l)
b_aibjaibl_jl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibl_jl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_jl
i_aibjaibl_jl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_jl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_jl
end do a_aibjaibl_jl
end do b_aibjaibl_jl
end do j_aibjaibl_jl
end do l_aibjaibl_jl
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_ajl: do l = n0l, n1l
j_aibjaibl_ajl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_ajl
a_aibjaibl_ajl: do a = n0ac, n1ac
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibl_ajl(t2, &
 nocc, nactive, a, j, l)
b1 = min(a - 1, n1bd)
b_aibjaibl_ajl: do b = n0bd, b1
if (b == a) cycle b_aibjaibl_ajl
i_aibjaibl_ajl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_ajl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_ajl
end do b_aibjaibl_ajl
end do a_aibjaibl_ajl
end do j_aibjaibl_ajl
end do l_aibjaibl_ajl
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_bjl: do l = n0l, n1l
b_aibjaibl_bjl: do b = n0bd, n1bd
j_aibjaibl_bjl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_bjl
nn0a = max(b + 1, n0ac)
if ((n1ik .ge. n0ik).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibl_bjl(t2, &
 nocc, nactive, b, j, l)
a0 = max(b + 1, n0ac)
a_aibjaibl_bjl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_bjl
i_aibjaibl_bjl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_bjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_bjl
end do a_aibjaibl_bjl
end do j_aibjaibl_bjl
end do b_aibjaibl_bjl
end do l_aibjaibl_bjl
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_ijl: do l = n0l, n1l
j_aibjaibl_ijl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_ijl
i_aibjaibl_ijl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_ijl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibl_ijl(t2, &
 nocc, nactive, i, j, l)
b_aibjaibl_ijl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibl_ijl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_ijl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaibl_ijl
end do b_aibjaibl_ijl
end do i_aibjaibl_ijl
end do j_aibjaibl_ijl
end do l_aibjaibl_ijl
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_ibjl: do l = n0l, n1l
b_aibjaibl_ibjl: do b = n0bd, n1bd
j_aibjaibl_ibjl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_ibjl
i_aibjaibl_ibjl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_ibjl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibl_ibjl(t2, &
 nocc, nactive, i, b, j, l)
a0 = max(b + 1, n0ac)
a_aibjaibl_ibjl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_ibjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaibl_ibjl
end do i_aibjaibl_ibjl
end do j_aibjaibl_ibjl
end do b_aibjaibl_ibjl
end do l_aibjaibl_ibjl
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_aijl: do l = n0l, n1l
j_aibjaibl_aijl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_aijl
a_aibjaibl_aijl: do a = n0ac, n1ac
i_aibjaibl_aijl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_aijl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibl_aijl(t2, &
 nocc, nactive, a, i, j, l)
b1 = min(a - 1, n1bd)
b_aibjaibl_aijl: do b = n0bd, b1
if (b == a) cycle b_aibjaibl_aijl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjaibl_aijl
end do i_aibjaibl_aijl
end do a_aibjaibl_aijl
end do j_aibjaibl_aijl
end do l_aibjaibl_aijl
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl_abjl: do l = n0l, n1l
b_aibjaibl_abjl: do b = n0bd, n1bd
j_aibjaibl_abjl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl_abjl
a0 = max(b + 1, n0ac)
a_aibjaibl_abjl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_abjl
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjaibl_abjl(t2, &
 nocc, nactive, a, b, j, l)
i_aibjaibl_abjl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_abjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_abjl
end do a_aibjaibl_abjl
end do j_aibjaibl_abjl
end do b_aibjaibl_abjl
end do l_aibjaibl_abjl
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibiakbl_ikl: do l = n0l, n1l
k_aibiakbl_ikl: do k = n0k, n1k
if (k == l) cycle k_aibiakbl_ikl
i_aibiakbl_ikl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakbl_ikl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbl_ikl(t2, &
 nocc, nactive, i, k, l)
b_aibiakbl_ikl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiakbl_ikl: do a = a0, n1ac
if (a == b) cycle a_aibiakbl_ikl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakbl_ikl
end do b_aibiakbl_ikl
end do i_aibiakbl_ikl
end do k_aibiakbl_ikl
end do l_aibiakbl_ikl
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibiakbl_ibkl: do l = n0l, n1l
k_aibiakbl_ibkl: do k = n0k, n1k
if (k == l) cycle k_aibiakbl_ibkl
b_aibiakbl_ibkl: do b = n0bd, n1bd
i_aibiakbl_ibkl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakbl_ibkl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbl_ibkl(t2, &
 nocc, nactive, i, b, k, l)
a0 = max(b + 1, n0ac)
a_aibiakbl_ibkl: do a = a0, n1ac
if (a == b) cycle a_aibiakbl_ibkl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakbl_ibkl
end do i_aibiakbl_ibkl
end do b_aibiakbl_ibkl
end do k_aibiakbl_ibkl
end do l_aibiakbl_ibkl
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibiakbl_aikl: do l = n0l, n1l
k_aibiakbl_aikl: do k = n0k, n1k
if (k == l) cycle k_aibiakbl_aikl
a_aibiakbl_aikl: do a = n0ac, n1ac
i_aibiakbl_aikl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakbl_aikl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiakbl_aikl(t2, &
 nocc, nactive, a, i, k, l)
b1 = min(a - 1, n1bd)
b_aibiakbl_aikl: do b = n0bd, b1
if (b == a) cycle b_aibiakbl_aikl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibiakbl_aikl
end do i_aibiakbl_aikl
end do a_aibiakbl_aikl
end do k_aibiakbl_aikl
end do l_aibiakbl_aikl
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_ijk: do k = n0k, n1k
j_aibjakbi_ijk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_ijk
i_aibjakbi_ijk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakbi_ijk
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbi_ijk(t2, &
 nocc, nactive, i, j, k)
b_aibjakbi_ijk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbi_ijk: do a = a0, n1ac
if (a == b) cycle a_aibjakbi_ijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbi_ijk
end do b_aibjakbi_ijk
end do i_aibjakbi_ijk
end do j_aibjakbi_ijk
end do k_aibjakbi_ijk
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_ibjk: do k = n0k, n1k
b_aibjakbi_ibjk: do b = n0bd, n1bd
j_aibjakbi_ibjk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_ibjk
i_aibjakbi_ibjk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakbi_ibjk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbi_ibjk(t2, &
 nocc, nactive, i, b, j, k)
a0 = max(b + 1, n0ac)
a_aibjakbi_ibjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbi_ibjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbi_ibjk
end do i_aibjakbi_ibjk
end do j_aibjakbi_ibjk
end do b_aibjakbi_ibjk
end do k_aibjakbi_ibjk
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_aijk: do k = n0k, n1k
j_aibjakbi_aijk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_aijk
a_aibjakbi_aijk: do a = n0ac, n1ac
i_aibjakbi_aijk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakbi_aijk
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbi_aijk(t2, &
 nocc, nactive, a, i, j, k)
b1 = min(a - 1, n1bd)
b_aibjakbi_aijk: do b = n0bd, b1
if (b == a) cycle b_aibjakbi_aijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjakbi_aijk
end do i_aibjakbi_aijk
end do a_aibjakbi_aijk
end do j_aibjakbi_aijk
end do k_aibjakbi_aijk
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_abjk: do k = n0k, n1k
b_aibjakbi_abjk: do b = n0bd, n1bd
j_aibjakbi_abjk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_abjk
a0 = max(b + 1, n0ac)
a_aibjakbi_abjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbi_abjk
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbi_abjk(t2, &
 nocc, nactive, a, b, j, k)
i_aibjakbi_abjk: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakbi_abjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbi_abjk
end do a_aibjakbi_abjk
end do j_aibjakbi_abjk
end do b_aibjakbi_abjk
end do k_aibjakbi_abjk
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_ijl: do l = n0l, n1l
j_aibjajbl_ijl: do j = n0jk, n1jk
if (j == l) cycle j_aibjajbl_ijl
i_aibjajbl_ijl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajbl_ijl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbl_ijl(t2, &
 nocc, nactive, i, j, l)
b_aibjajbl_ijl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjajbl_ijl: do a = a0, n1ac
if (a == b) cycle a_aibjajbl_ijl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbl_ijl
end do b_aibjajbl_ijl
end do i_aibjajbl_ijl
end do j_aibjajbl_ijl
end do l_aibjajbl_ijl
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_ibjl: do l = n0l, n1l
b_aibjajbl_ibjl: do b = n0bd, n1bd
j_aibjajbl_ibjl: do j = n0jk, n1jk
if (j == l) cycle j_aibjajbl_ibjl
i_aibjajbl_ibjl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajbl_ibjl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbl_ibjl(t2, &
 nocc, nactive, i, b, j, l)
a0 = max(b + 1, n0ac)
a_aibjajbl_ibjl: do a = a0, n1ac
if (a == b) cycle a_aibjajbl_ibjl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbl_ibjl
end do i_aibjajbl_ibjl
end do j_aibjajbl_ibjl
end do b_aibjajbl_ibjl
end do l_aibjajbl_ibjl
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_aijl: do l = n0l, n1l
j_aibjajbl_aijl: do j = n0jk, n1jk
if (j == l) cycle j_aibjajbl_aijl
a_aibjajbl_aijl: do a = n0ac, n1ac
i_aibjajbl_aijl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajbl_aijl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbl_aijl(t2, &
 nocc, nactive, a, i, j, l)
b1 = min(a - 1, n1bd)
b_aibjajbl_aijl: do b = n0bd, b1
if (b == a) cycle b_aibjajbl_aijl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjajbl_aijl
end do i_aibjajbl_aijl
end do a_aibjajbl_aijl
end do j_aibjajbl_aijl
end do l_aibjajbl_aijl
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_aibl: do l = n0l, n1l
b_aibjajbl_aibl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjajbl_aibl: do a = a0, n1ac
if (a == b) cycle a_aibjajbl_aibl
i_aibjajbl_aibl: do i = n0i, n1i
if (i == l) cycle i_aibjajbl_aibl
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjajbl_aibl(t2, &
 nocc, nactive, a, i, b, l)
j_aibjajbl_aibl: do j = n0jk, n1jk
if (j == i .or. j == l) cycle j_aibjajbl_aibl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbl_aibl
end do i_aibjajbl_aibl
end do a_aibjajbl_aibl
end do b_aibjajbl_aibl
end do l_aibjajbl_aibl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: c == a, d == b, l == k
! No equalities independent of the above can hold.
!
k_aibjakbk_ijk: do k = n0kl, n1kl
j_aibjakbk_ijk: do j = n0j, n1j
if (j == k) cycle j_aibjakbk_ijk
i_aibjakbk_ijk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbk_ijk
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjakbk_ijk(t2, &
 nocc, nactive, i, j, k)
b_aibjakbk_ijk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbk_ijk: do a = a0, n1ac
if (a == b) cycle a_aibjakbk_ijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbk_ijk
end do b_aibjakbk_ijk
end do i_aibjakbk_ijk
end do j_aibjakbk_ijk
end do k_aibjakbk_ijk
end subroutine ccjac_22_tripletmm_dav_part4
end module ccjac_block_22_tripletmm_dav_part4
