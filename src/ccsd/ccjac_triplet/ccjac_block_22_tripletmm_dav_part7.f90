module ccjac_block_22_tripletmm_dav_part7
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
subroutine ccjac_22_tripletmm_dav_part7(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0b, nn0i, nn0j, nn0k, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b0, b1, i0, i1, j0, j1, k0
integer :: n0ad, n0bc, n0bcd, n0ij, n0ijk
integer :: n0ijl, n0ik, n0ikl, n0il, n0jk
integer :: n0jkl, n0jl, n0kl
integer :: n1ad, n1bc, n1bcd, n1ij, n1ijk
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
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0ij = max(n0i, n0j)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1ij = min(n1i, n1j)
n1ijk = min(n1i, n1j, n1k)
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
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == k
! No equalities independent of the above can hold.
!
c_aibickak_ibck: do c = n0c, n1c
k_aibickak_ibck: do k = n0kl, n1kl
b_aibickak_ibck: do b = n0b, n1b
if (b == c) cycle b_aibickak_ibck
i_aibickak_ibck: do i = n0ij, n1ij
if (i == k) cycle i_aibickak_ibck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibickak_ibck(t2, &
 nocc, nactive, i, b, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickak_ibck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickak_ibck
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibickak_ibck
end do i_aibickak_ibck
end do b_aibickak_ibck
end do k_aibickak_ibck
end do c_aibickak_ibck
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_bc: do c = n0c, n1c
b_aibjcjai_bc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_bc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjai_bc(t2, &
 nocc, nactive, b, c)
j_aibjcjai_bc: do j = n0jk, n1jk
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_bc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_bc
i_aibjcjai_bc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_bc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_bc
end do a_aibjcjai_bc
end do j_aibjcjai_bc
end do b_aibjcjai_bc
end do c_aibjcjai_bc
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_abc: do c = n0c, n1c
b_aibjcjai_abc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_abc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_abc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_abc
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjai_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcjai_abc: do j = n0jk, n1jk
i_aibjcjai_abc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_abc
end do j_aibjcjai_abc
end do a_aibjcjai_abc
end do b_aibjcjai_abc
end do c_aibjcjai_abc
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai_ibc: do c = n0c, n1c
b_aibjcjai_ibc: do b = n0b, n1b
if (b == c) cycle b_aibjcjai_ibc
i_aibjcjai_ibc: do i = n0il, n1il
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1jk .ge. n0jk).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjai_ibc(t2, &
 nocc, nactive, i, b, c)
j_aibjcjai_ibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjai_ibc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_ibc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_ibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjai_ibc
end do j_aibjcjai_ibc
end do i_aibjcjai_ibc
end do b_aibjcjai_ibc
end do c_aibjcjai_ibc
!
! Elementary loop  5
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
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1il .ge. n0il).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjai_bjc(t2, &
 nocc, nactive, b, j, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai_bjc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai_bjc
i_aibjcjai_bjc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_bjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_bjc
end do a_aibjcjai_bjc
end do j_aibjcjai_bjc
end do b_aibjcjai_bjc
end do c_aibjcjai_bjc
!
! Elementary loop  6
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
i_aibjcjai_ibjc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjai_ibjc(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjai_ibjc
end do i_aibjcjai_ibjc
end do j_aibjcjai_ibjc
end do b_aibjcjai_ibjc
end do c_aibjcjai_ibjc
!
! Elementary loop  7
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
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjai_abjc(t2, &
 nocc, nactive, a, b, j, c)
i_aibjcjai_abjc: do i = n0il, n1il
if (i == j) cycle i_aibjcjai_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjai_abjc
end do a_aibjcjai_abjc
end do j_aibjcjai_abjc
end do b_aibjcjai_abjc
end do c_aibjcjai_abjc
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
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjai_aibc(t2, &
 nocc, nactive, a, i, b, c)
j_aibjcjai_aibc: do j = n0jk, n1jk
if (j == i) cycle j_aibjcjai_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
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
! Free occupied indices: j, i
! Equalities: d == a, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjaj_ibjc: do c = n0c, n1c
b_aibjcjaj_ibjc: do b = n0b, n1b
if (b == c) cycle b_aibjcjaj_ibjc
j_aibjcjaj_ibjc: do j = n0jkl, n1jkl
i_aibjcjaj_ibjc: do i = n0i, n1i
if (i == j) cycle i_aibjcjaj_ibjc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjaj_ibjc(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjaj_ibjc
end do i_aibjcjaj_ibjc
end do j_aibjcjaj_ibjc
end do b_aibjcjaj_ibjc
end do c_aibjcjaj_ibjc
!
! Elementary loop  10
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
i_aibjcjaj_aibjc: do i = n0i, n1i
if (i == j) cycle i_aibjcjaj_aibjc
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjcjaj_aibjc(t2, nocc, nactive, a, &
 i, b, j, c)
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjaj_aibjc
end do a_aibjcjaj_aibjc
end do j_aibjcjaj_aibjc
end do b_aibjcjaj_aibjc
end do c_aibjcjaj_aibjc
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbibl_abjl: do l = n0l, n1l
b_aibjbibl_abjl: do b = n0bcd, n1bcd
j_aibjbibl_abjl: do j = n0j, n1j
if (j == l) cycle j_aibjbibl_abjl
a0 = max(b + 1, n0a)
a_aibjbibl_abjl: do a = a0, n1a
if (a == b) cycle a_aibjbibl_abjl
nn0i = max(l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbibl_abjl(t2, &
 nocc, nactive, a, b, j, l)
i0 = max(l + 1, n0ik)
i_aibjbibl_abjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbibl_abjl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibl_abjl
end do a_aibjbibl_abjl
end do j_aibjbibl_abjl
end do b_aibjbibl_abjl
end do l_aibjbibl_abjl
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbibl_aibjl: do l = n0l, n1l
b_aibjbibl_aibjl: do b = n0bcd, n1bcd
j_aibjbibl_aibjl: do j = n0j, n1j
if (j == l) cycle j_aibjbibl_aibjl
a0 = max(b + 1, n0a)
a_aibjbibl_aibjl: do a = a0, n1a
if (a == b) cycle a_aibjbibl_aibjl
i0 = max(l + 1, n0ik)
i_aibjbibl_aibjl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbibl_aibjl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbibl_aibjl(t2, nocc, nactive, a, &
 i, b, j, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibl_aibjl
end do a_aibjbibl_aibjl
end do j_aibjbibl_aibjl
end do b_aibjbibl_aibjl
end do l_aibjbibl_aibjl
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, l
! Equalities: c == b, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibibkbl_aibkl: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aibibkbl_aibkl: do k = k0, n1k
if (k == l) cycle k_aibibkbl_aibkl
b_aibibkbl_aibkl: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibibkbl_aibkl: do a = a0, n1a
if (a == b) cycle a_aibibkbl_aibkl
i_aibibkbl_aibkl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibibkbl_aibkl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibkbl_aibkl(t2, nocc, nactive, a, &
 i, b, k, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbl_aibkl
end do a_aibibkbl_aibkl
end do b_aibibkbl_aibkl
end do k_aibibkbl_aibkl
end do l_aibibkbl_aibkl
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkbi_abjk: do k = n0k, n1k
b_aibjbkbi_abjk: do b = n0bcd, n1bcd
j_aibjbkbi_abjk: do j = n0j, n1j
if (j == k) cycle j_aibjbkbi_abjk
a0 = max(b + 1, n0a)
a_aibjbkbi_abjk: do a = a0, n1a
if (a == b) cycle a_aibjbkbi_abjk
nn1i = min(k - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbkbi_abjk(t2, &
 nocc, nactive, a, b, j, k)
i1 = min(k - 1, n1il)
i_aibjbkbi_abjk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkbi_abjk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbi_abjk
end do a_aibjbkbi_abjk
end do j_aibjbkbi_abjk
end do b_aibjbkbi_abjk
end do k_aibjbkbi_abjk
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkbi_aibjk: do k = n0k, n1k
b_aibjbkbi_aibjk: do b = n0bcd, n1bcd
j_aibjbkbi_aibjk: do j = n0j, n1j
if (j == k) cycle j_aibjbkbi_aibjk
a0 = max(b + 1, n0a)
a_aibjbkbi_aibjk: do a = a0, n1a
if (a == b) cycle a_aibjbkbi_aibjk
i1 = min(k - 1, n1il)
i_aibjbkbi_aibjk: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkbi_aibjk
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbkbi_aibjk(t2, nocc, nactive, a, &
 i, b, j, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbi_aibjk
end do a_aibjbkbi_aibjk
end do j_aibjbkbi_aibjk
end do b_aibjbkbi_aibjk
end do k_aibjbkbi_aibjk
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, l
! Equalities: c == b, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjbjbl_aibl: do l = n0l, n1l
b_aibjbjbl_aibl: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibjbjbl_aibl: do a = a0, n1a
if (a == b) cycle a_aibjbjbl_aibl
i_aibjbjbl_aibl: do i = n0i, n1i
if (i == l) cycle i_aibjbjbl_aibl
nn0j = max(l + 1, n0jk)
if ((n1jk .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjbl_aibl(t2, &
 nocc, nactive, a, i, b, l)
j0 = max(l + 1, n0jk)
j_aibjbjbl_aibl: do j = j0, n1jk
if (j == i .or. j == l) cycle j_aibjbjbl_aibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjbl_aibl
end do i_aibjbjbl_aibl
end do a_aibjbjbl_aibl
end do b_aibjbjbl_aibl
end do l_aibjbjbl_aibl
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, l
! Equalities: c == b, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjbjbl_aibjl: do l = n0l, n1l
b_aibjbjbl_aibjl: do b = n0bcd, n1bcd
j0 = max(l + 1, n0jk)
j_aibjbjbl_aibjl: do j = j0, n1jk
if (j == l) cycle j_aibjbjbl_aibjl
a0 = max(b + 1, n0a)
a_aibjbjbl_aibjl: do a = a0, n1a
if (a == b) cycle a_aibjbjbl_aibjl
i_aibjbjbl_aibjl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjbjbl_aibjl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjbl_aibjl(t2, nocc, nactive, a, &
 i, b, j, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbl_aibjl
end do a_aibjbjbl_aibjl
end do j_aibjbjbl_aibjl
end do b_aibjbjbl_aibjl
end do l_aibjbjbl_aibjl
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k
! Equalities: c == b, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjbkbj_aibk: do k = n0k, n1k
b_aibjbkbj_aibk: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibjbkbj_aibk: do a = a0, n1a
if (a == b) cycle a_aibjbkbj_aibk
i_aibjbkbj_aibk: do i = n0i, n1i
if (i == k) cycle i_aibjbkbj_aibk
nn1j = min(k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbkbj_aibk(t2, &
 nocc, nactive, a, i, b, k)
j1 = min(k - 1, n1jl)
j_aibjbkbj_aibk: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjbkbj_aibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbkbj_aibk
end do i_aibjbkbj_aibk
end do a_aibjbkbj_aibk
end do b_aibjbkbj_aibk
end do k_aibjbkbj_aibk
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k
! Equalities: c == b, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjbkbj_aibjk: do k = n0k, n1k
b_aibjbkbj_aibjk: do b = n0bcd, n1bcd
j1 = min(k - 1, n1jl)
j_aibjbkbj_aibjk: do j = n0jl, j1
if (j == k) cycle j_aibjbkbj_aibjk
a0 = max(b + 1, n0a)
a_aibjbkbj_aibjk: do a = a0, n1a
if (a == b) cycle a_aibjbkbj_aibjk
i_aibjbkbj_aibjk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjbkbj_aibjk
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbkbj_aibjk(t2, nocc, nactive, a, &
 i, b, j, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbj_aibjk
end do a_aibjbkbj_aibjk
end do j_aibjbkbj_aibjk
end do b_aibjbkbj_aibjk
end do k_aibjbkbj_aibjk
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, l
! Equalities: c == b, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibibidl_aidl: do d = n0d, n1d
l_aibibidl_aidl: do l = n0l, n1l
a_aibibidl_aidl: do a = n0a, n1a
if (a == d) cycle a_aibibidl_aidl
i_aibibidl_aidl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibibidl_aidl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibidl_aidl(t2, &
 nocc, nactive, a, i, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibidl_aidl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibidl_aidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibidl_aidl
end do i_aibibidl_aidl
end do a_aibibidl_aidl
end do l_aibibidl_aidl
end do d_aibibidl_aidl
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, l
! Equalities: c == b, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibibidl_aibdl: do d = n0d, n1d
l_aibibidl_aibdl: do l = n0l, n1l
b0 = max(d + 1, n0bc)
b_aibibidl_aibdl: do b = b0, n1bc
if (b == d) cycle b_aibibidl_aibdl
a0 = max(b + 1, n0a)
a_aibibidl_aibdl: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibidl_aibdl
i_aibibidl_aibdl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibibidl_aibdl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibidl_aibdl(t2, nocc, nactive, a, &
 i, b, d, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidl_aibdl
end do a_aibibidl_aibdl
end do b_aibibidl_aibdl
end do l_aibibidl_aibdl
end do d_aibibidl_aibdl
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjbidi_aijd: do d = n0d, n1d
j_aibjbidi_aijd: do j = n0j, n1j
a_aibjbidi_aijd: do a = n0a, n1a
if (a == d) cycle a_aibjbidi_aijd
i_aibjbidi_aijd: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjbidi_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbidi_aijd(t2, &
 nocc, nactive, a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidi_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidi_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidi_aijd
end do i_aibjbidi_aijd
end do a_aibjbidi_aijd
end do j_aibjbidi_aijd
end do d_aibjbidi_aijd
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjbidi_aibjd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbidi_aibjd: do b = b0, n1bc
if (b == d) cycle b_aibjbidi_aibjd
j_aibjbidi_aibjd: do j = n0j, n1j
a0 = max(b + 1, n0a)
a_aibjbidi_aibjd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidi_aibjd
i_aibjbidi_aibjd: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjbidi_aibjd
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbidi_aibjd(t2, nocc, nactive, a, &
 i, b, j, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidi_aibjd
end do a_aibjbidi_aibjd
end do j_aibjbidi_aibjd
end do b_aibjbidi_aibjd
end do d_aibjbidi_aibjd
!
! Elementary loop  24
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
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbidj_abd(t2, &
 nocc, nactive, a, b, d)
j_aibjbidj_abd: do j = n0jl, n1jl
i_aibjbidj_abd: do i = n0ik, n1ik
if (i == j) cycle i_aibjbidj_abd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_abd
end do j_aibjbidj_abd
end do a_aibjbidj_abd
end do b_aibjbidj_abd
end do d_aibjbidj_abd
!
! Elementary loop  25
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
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbidj_abjd(t2, &
 nocc, nactive, a, b, j, d)
i_aibjbidj_abjd: do i = n0ik, n1ik
if (i == j) cycle i_aibjbidj_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_abjd
end do a_aibjbidj_abjd
end do j_aibjbidj_abjd
end do b_aibjbidj_abjd
end do d_aibjbidj_abjd
!
! Elementary loop  26
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
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbidj_aibd(t2, &
 nocc, nactive, a, i, b, d)
j_aibjbidj_aibd: do j = n0jl, n1jl
if (j == i) cycle j_aibjbidj_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbidj_aibd
end do i_aibjbidj_aibd
end do a_aibjbidj_aibd
end do b_aibjbidj_aibd
end do d_aibjbidj_aibd
!
! Elementary loop  27
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
i_aibjbidj_aijd: do i = n0ik, n1ik
if (i == j) cycle i_aibjbidj_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbidj_aijd(t2, &
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
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidj_aijd
end do i_aibjbidj_aijd
end do a_aibjbidj_aijd
end do j_aibjbidj_aijd
end do d_aibjbidj_aijd
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibibkdi_aikd: do d = n0d, n1d
k_aibibkdi_aikd: do k = n0k, n1k
a_aibibkdi_aikd: do a = n0a, n1a
if (a == d) cycle a_aibibkdi_aikd
i_aibibkdi_aikd: do i = n0ijl, n1ijl
if (i == k) cycle i_aibibkdi_aikd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibkdi_aikd(t2, &
 nocc, nactive, a, i, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibkdi_aikd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibkdi_aikd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibkdi_aikd
end do i_aibibkdi_aikd
end do a_aibibkdi_aikd
end do k_aibibkdi_aikd
end do d_aibibkdi_aikd
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibibkdi_aibkd: do d = n0d, n1d
k_aibibkdi_aibkd: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibibkdi_aibkd: do b = b0, n1bc
if (b == d) cycle b_aibibkdi_aibkd
a0 = max(b + 1, n0a)
a_aibibkdi_aibkd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibkdi_aibkd
i_aibibkdi_aibkd: do i = n0ijl, n1ijl
if (i == k) cycle i_aibibkdi_aibkd
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibkdi_aibkd(t2, nocc, nactive, a, &
 i, b, k, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdi_aibkd
end do a_aibibkdi_aibkd
end do b_aibibkdi_aibkd
end do k_aibibkdi_aibkd
end do d_aibibkdi_aibkd
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == k
! No equalities independent of the above can hold.
!
d_aibibkdk_aikd: do d = n0d, n1d
k_aibibkdk_aikd: do k = n0kl, n1kl
a_aibibkdk_aikd: do a = n0a, n1a
if (a == d) cycle a_aibibkdk_aikd
i_aibibkdk_aikd: do i = n0ij, n1ij
if (i == k) cycle i_aibibkdk_aikd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibkdk_aikd(t2, &
 nocc, nactive, a, i, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibkdk_aikd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibkdk_aikd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibkdk_aikd
end do i_aibibkdk_aikd
end do a_aibibkdk_aikd
end do k_aibibkdk_aikd
end do d_aibibkdk_aikd
end subroutine ccjac_22_tripletmm_dav_part7
end module ccjac_block_22_tripletmm_dav_part7
