module ccjac_block_22_tripletmp_dav_part4
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
subroutine ccjac_22_tripletmp_dav_part4(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0c, nn0i, nn0j, nn0k, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b1, c0, i0, i1, j0, j1, k0
integer :: n0ab, n0abd, n0ac, n0bd, n0ij
integer :: n0ijl, n0ik, n0il, n0jk, n0jl
integer :: n1ab, n1abd, n1ac, n1bd, n1ij
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
n0ab = max(n0a, n0b)
n0abd = max(n0a, n0b, n0d)
n0ac = max(n0a, n0c)
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1bd = min(n1b, n1d)
n1ij = min(n1i, n1j)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
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
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajckai_ajck(t2, &
 nocc, nactive, a, j, c, k)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aiajckai_ajck: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajckai_ajck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckai_ajck
end do a_aiajckai_ajck
end do j_aiajckai_ajck
end do k_aiajckai_ajck
end do c_aiajckai_ajck
!
! Elementary loop  2
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
i1 = min(k - 1, n1il)
i_aiajckai_aijck: do i = i0, i1
if (i == j .or. i == k) cycle i_aiajckai_aijck
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajckai_aijck(t2, nocc, nactive, a, &
 i, j, c, k)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckai_aijck
end do a_aiajckai_aijck
end do j_aiajckai_aijck
end do k_aiajckai_aijck
end do c_aiajckai_aijck
!
! Elementary loop  3
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
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajcial_ajcl(t2, &
 nocc, nactive, a, j, c, l)
i0 = max(j + 1, l + 1, n0ik)
i_aiajcial_ajcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcial_ajcl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcial_ajcl
end do a_aiajcial_ajcl
end do j_aiajcial_ajcl
end do c_aiajcial_ajcl
end do l_aiajcial_ajcl
!
! Elementary loop  4
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
i0 = max(j + 1, l + 1, n0ik)
i_aiajcial_aijcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcial_aijcl
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajcial_aijcl(t2, nocc, nactive, a, &
 i, j, c, l)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (a - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcial_aijcl
end do a_aiajcial_aijcl
end do j_aiajcial_aijcl
end do c_aiajcial_aijcl
end do l_aiajcial_aijcl
!
! Elementary loop  5
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajcidj_ajcd(t2, &
 nocc, nactive, a, j, c, d)
i0 = max(j + 1, n0ik)
i_aiajcidj_ajcd: do i = i0, n1ik
if (i == j) cycle i_aiajcidj_ajcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcidj_ajcd
end do a_aiajcidj_ajcd
end do j_aiajcidj_ajcd
end do c_aiajcidj_ajcd
end do d_aiajcidj_ajcd
!
! Elementary loop  6
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aiajcidj_aicd(t2, &
 nocc, nactive, a, i, c, d)
j1 = min(i - 1, n1jl)
j_aiajcidj_aicd: do j = n0jl, j1
if (j == i) cycle j_aiajcidj_aicd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcidj_aicd
end do i_aiajcidj_aicd
end do a_aiajcidj_aicd
end do c_aiajcidj_aicd
end do d_aiajcidj_aicd
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_aik: do k = n0k, n1k
a_aibjakbj_aik: do a = n0ac, n1ac
i_aibjakbj_aik: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_aik
nn1j = min(k - 1, n1jl)
nn1b = min(a - 1, n1bd)
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbj_aik(t2, &
 nocc, nactive, a, i, k)
b1 = min(a - 1, n1bd)
b_aibjakbj_aik: do b = n0bd, b1
if (b == a) cycle b_aibjakbj_aik
j1 = min(k - 1, n1jl)
j_aibjakbj_aik: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakbj_aik
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakbj_aik
end do b_aibjakbj_aik
end do i_aibjakbj_aik
end do a_aibjakbj_aik
end do k_aibjakbj_aik
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_ibjk: do k = n0k, n1k
b_aibjakbj_ibjk: do b = n0bd, n1bd
j1 = min(k - 1, n1jl)
j_aibjakbj_ibjk: do j = n0jl, j1
if (j == k) cycle j_aibjakbj_ibjk
i_aibjakbj_ibjk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbj_ibjk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbj_ibjk(t2, &
 nocc, nactive, i, b, j, k)
a0 = max(b + 1, n0ac)
a_aibjakbj_ibjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_ibjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbj_ibjk
end do i_aibjakbj_ibjk
end do j_aibjakbj_ibjk
end do b_aibjakbj_ibjk
end do k_aibjakbj_ibjk
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_aijk: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aibjakbj_aijk: do j = n0jl, j1
if (j == k) cycle j_aibjakbj_aijk
a_aibjakbj_aijk: do a = n0ac, n1ac
i_aibjakbj_aijk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbj_aijk
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbj_aijk(t2, &
 nocc, nactive, a, i, j, k)
b1 = min(a - 1, n1bd)
b_aibjakbj_aijk: do b = n0bd, b1
if (b == a) cycle b_aibjakbj_aijk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjakbj_aijk
end do i_aibjakbj_aijk
end do a_aibjakbj_aijk
end do j_aibjakbj_aijk
end do k_aibjakbj_aijk
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj_aibk: do k = n0k, n1k
b_aibjakbj_aibk: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjakbj_aibk: do a = a0, n1ac
if (a == b) cycle a_aibjakbj_aibk
i_aibjakbj_aibk: do i = n0i, n1i
if (i == k) cycle i_aibjakbj_aibk
nn1j = min(k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbj_aibk(t2, &
 nocc, nactive, a, i, b, k)
j1 = min(k - 1, n1jl)
j_aibjakbj_aibk: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjakbj_aibk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjakbj_aibk
end do i_aibjakbj_aibk
end do a_aibjakbj_aibk
end do b_aibjakbj_aibk
end do k_aibjakbj_aibk
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibiakbl_ibkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibiakbl_ibkl: do k = k0, n1k
if (k == l) cycle k_aibiakbl_ibkl
b_aibiakbl_ibkl: do b = n0bd, n1bd
i_aibiakbl_ibkl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakbl_ibkl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiakbl_ibkl(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiakbl_ibkl
end do i_aibiakbl_ibkl
end do b_aibiakbl_ibkl
end do k_aibiakbl_ibkl
end do l_aibiakbl_ibkl
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibiakbl_aikl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibiakbl_aikl: do k = k0, n1k
if (k == l) cycle k_aibiakbl_aikl
a_aibiakbl_aikl: do a = n0ac, n1ac
i_aibiakbl_aikl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakbl_aikl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibiakbl_aikl(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibiakbl_aikl
end do i_aibiakbl_aikl
end do a_aibiakbl_aikl
end do k_aibiakbl_aikl
end do l_aibiakbl_aikl
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_ail: do l = n0l, n1l
a_aibjajbl_ail: do a = n0ac, n1ac
i_aibjajbl_ail: do i = n0i, n1i
if (i == l) cycle i_aibjajbl_ail
nn0j = max(l + 1, n0jk)
nn1b = min(a - 1, n1bd)
if ((n1jk .ge. nn0j).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbl_ail(t2, &
 nocc, nactive, a, i, l)
b1 = min(a - 1, n1bd)
b_aibjajbl_ail: do b = n0bd, b1
if (b == a) cycle b_aibjajbl_ail
j0 = max(l + 1, n0jk)
j_aibjajbl_ail: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjajbl_ail
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbl_ail
end do b_aibjajbl_ail
end do i_aibjajbl_ail
end do a_aibjajbl_ail
end do l_aibjajbl_ail
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_ibjl: do l = n0l, n1l
b_aibjajbl_ibjl: do b = n0bd, n1bd
j0 = max(l + 1, n0jk)
j_aibjajbl_ibjl: do j = j0, n1jk
if (j == l) cycle j_aibjajbl_ibjl
i_aibjajbl_ibjl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajbl_ibjl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbl_ibjl(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjajbl_ibjl
end do i_aibjajbl_ibjl
end do j_aibjajbl_ibjl
end do b_aibjajbl_ibjl
end do l_aibjajbl_ibjl
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl_aijl: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aibjajbl_aijl: do j = j0, n1jk
if (j == l) cycle j_aibjajbl_aijl
a_aibjajbl_aijl: do a = n0ac, n1ac
i_aibjajbl_aijl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajbl_aijl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbl_aijl(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjajbl_aijl
end do i_aibjajbl_aijl
end do a_aibjajbl_aijl
end do j_aibjajbl_aijl
end do l_aibjajbl_aijl
!
! Elementary loop  16
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
nn0j = max(l + 1, n0jk)
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjajbl_aibl(t2, &
 nocc, nactive, a, i, b, l)
j0 = max(l + 1, n0jk)
j_aibjajbl_aibl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjajbl_aibl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajbl_aibl
end do i_aibjajbl_aibl
end do a_aibjajbl_aibl
end do b_aibjajbl_aibl
end do l_aibjajbl_aibl
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi_bjk: do k = n0k, n1k
b_aibjakbi_bjk: do b = n0bd, n1bd
j_aibjakbi_bjk: do j = n0j, n1j
if (j == k) cycle j_aibjakbi_bjk
nn1i = min(k - 1, n1il)
nn0a = max(b + 1, n0ac)
if ((nn1i .ge. n0il).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbi_bjk(t2, &
 nocc, nactive, b, j, k)
a0 = max(b + 1, n0ac)
a_aibjakbi_bjk: do a = a0, n1ac
if (a == b) cycle a_aibjakbi_bjk
i1 = min(k - 1, n1il)
i_aibjakbi_bjk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakbi_bjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbi_bjk
end do a_aibjakbi_bjk
end do j_aibjakbi_bjk
end do b_aibjakbi_bjk
end do k_aibjakbi_bjk
!
! Elementary loop  18
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
i1 = min(k - 1, n1il)
i_aibjakbi_ibjk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakbi_ibjk
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbi_ibjk(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjakbi_ibjk
end do i_aibjakbi_ibjk
end do j_aibjakbi_ibjk
end do b_aibjakbi_ibjk
end do k_aibjakbi_ibjk
!
! Elementary loop  19
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
i1 = min(k - 1, n1il)
i_aibjakbi_aijk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakbi_aijk
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbi_aijk(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjakbi_aijk
end do i_aibjakbi_aijk
end do a_aibjakbi_aijk
end do j_aibjakbi_aijk
end do k_aibjakbi_aijk
!
! Elementary loop  20
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
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjakbi_abjk(t2, &
 nocc, nactive, a, b, j, k)
i1 = min(k - 1, n1il)
i_aibjakbi_abjk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakbi_abjk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjakbi_abjk
end do a_aibjakbi_abjk
end do j_aibjakbi_abjk
end do b_aibjakbi_abjk
end do k_aibjakbi_abjk
!
! Elementary loop  21
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
nn0i = max(l + 1, n0ik)
nn0a = max(b + 1, n0ac)
if ((n1ik .ge. nn0i).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibl_bjl(t2, &
 nocc, nactive, b, j, l)
a0 = max(b + 1, n0ac)
a_aibjaibl_bjl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl_bjl
i0 = max(l + 1, n0ik)
i_aibjaibl_bjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_bjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_bjl
end do a_aibjaibl_bjl
end do j_aibjaibl_bjl
end do b_aibjaibl_bjl
end do l_aibjaibl_bjl
!
! Elementary loop  22
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
i0 = max(l + 1, n0ik)
i_aibjaibl_ibjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_ibjl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibl_ibjl(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaibl_ibjl
end do i_aibjaibl_ibjl
end do j_aibjaibl_ibjl
end do b_aibjaibl_ibjl
end do l_aibjaibl_ibjl
!
! Elementary loop  23
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
i0 = max(l + 1, n0ik)
i_aibjaibl_aijl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_aijl
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibl_aijl(t2, &
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
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjaibl_aijl
end do i_aibjaibl_aijl
end do a_aibjaibl_aijl
end do j_aibjaibl_aijl
end do l_aibjaibl_aijl
!
! Elementary loop  24
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
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjaibl_abjl(t2, &
 nocc, nactive, a, b, j, l)
i0 = max(l + 1, n0ik)
i_aibjaibl_abjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaibl_abjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibl_abjl
end do a_aibjaibl_abjl
end do j_aibjaibl_abjl
end do b_aibjaibl_abjl
end do l_aibjaibl_abjl
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickbi_aick: do c = n0c, n1c
k_aibickbi_aick: do k = n0k, n1k
a_aibickbi_aick: do a = n0a, n1a
if (a == c) cycle a_aibickbi_aick
i1 = min(k - 1, n1ijl)
i_aibickbi_aick: do i = n0ijl, i1
if (i == k) cycle i_aibickbi_aick
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibickbi_aick(t2, &
 nocc, nactive, a, i, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibickbi_aick: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibickbi_aick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibickbi_aick
end do i_aibickbi_aick
end do a_aibickbi_aick
end do k_aibickbi_aick
end do c_aibickbi_aick
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, k
! Equalities: d == b, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickbi_aibck: do c = n0c, n1c
k_aibickbi_aibck: do k = n0k, n1k
b1 = min(c - 1, n1bd)
b_aibickbi_aibck: do b = n0bd, b1
if (b == c) cycle b_aibickbi_aibck
a0 = max(b + 1, n0a)
a_aibickbi_aibck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibickbi_aibck
i1 = min(k - 1, n1ijl)
i_aibickbi_aibck: do i = n0ijl, i1
if (i == k) cycle i_aibickbi_aibck
jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibickbi_aibck(t2, nocc, nactive, a, &
 i, b, c, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickbi_aibck
end do a_aibickbi_aibck
end do b_aibickbi_aibck
end do k_aibickbi_aibck
end do c_aibickbi_aibck
!
! Elementary loop  27
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
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcibj_aic(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcibj_abjc(t2, &
 nocc, nactive, a, b, j, c)
i0 = max(j + 1, n0ik)
i_aibjcibj_abjc: do i = i0, n1ik
if (i == j) cycle i_aibjcibj_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcibj_aibc(t2, &
 nocc, nactive, a, i, b, c)
j1 = min(i - 1, n1jl)
j_aibjcibj_aibc: do j = n0jl, j1
if (j == i) cycle j_aibjcibj_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Elementary loop  30
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
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletmp_trans_aibjcibj_aijc(t2, &
 nocc, nactive, a, i, j, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibj_aijc: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibj_aijc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibj_aijc
end do i_aibjcibj_aijc
end do a_aibjcibj_aijc
end do j_aibjcibj_aijc
end do c_aibjcibj_aijc
end subroutine ccjac_22_tripletmp_dav_part4
end module ccjac_block_22_tripletmp_dav_part4
