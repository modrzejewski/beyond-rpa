module ccjac_block_22_tripletmm_dav_part12
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
subroutine ccjac_22_tripletmm_dav_part12(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0b, nn0i, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b0, b1, i0, i1, j1
integer :: n0ab, n0abd, n0ad, n0bc, n0bcd
integer :: n0bd, n0cd, n0ijk, n0ijkl, n0ijl
integer :: n0ik, n0il, n0jk, n0jkl, n0jl
integer :: n1ab, n1abd, n1ad, n1bc, n1bcd
integer :: n1bd, n1cd, n1ijk, n1ijkl, n1ijl
integer :: n1ik, n1il, n1jk, n1jkl, n1jl
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
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0bd = max(n0b, n0d)
n0cd = max(n0c, n0d)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ijk = min(n1i, n1j, n1k)
n1ijkl = min(n1i, n1j, n1k, n1l)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajciaj_aic(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajciaj_aic
end do i_aiajciaj_aic
end do a_aiajciaj_aic
end do c_aiajciaj_aic
!
! Elementary loop  2
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajciaj_ajc(t2, &
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
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaj_ajc
end do a_aiajciaj_ajc
end do j_aiajciaj_ajc
end do c_aiajciaj_ajc
!
! Elementary loop  3
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
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajciaj_aijc(t2, nocc, nactive, a, i, &
 j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaj_aijc
end do a_aiajciaj_aijc
end do j_aiajciaj_aijc
end do c_aiajciaj_aijc
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjai_ac: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiajcjai_ac: do a = n0abd, a1
if (a == c) cycle a_aiajcjai_ac
nn0i = max(n0jk + 1, n0il)
if ((n1jk .ge. n0jk).and. (n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajcjai_ac(t2, &
 nocc, nactive, a, c)
j_aiajcjai_ac: do j = n0jk, n1jk
i0 = max(j + 1, n0il)
i_aiajcjai_ac: do i = i0, n1il
if (i == j) cycle i_aiajcjai_ac
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjai_ac
end do j_aiajcjai_ac
end do a_aiajcjai_ac
end do c_aiajcjai_ac
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjai_aic: do c = n0c, n1c
a1 = min(c - 1, n1abd)
a_aiajcjai_aic: do a = n0abd, a1
if (a == c) cycle a_aiajcjai_aic
i_aiajcjai_aic: do i = n0il, n1il
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajcjai_aic(t2, &
 nocc, nactive, a, i, c)
j1 = min(i - 1, n1jk)
j_aiajcjai_aic: do j = n0jk, j1
if (j == i) cycle j_aiajcjai_aic
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcjai_aic
end do i_aiajcjai_aic
end do a_aiajcjai_aic
end do c_aiajcjai_aic
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjai_ajc: do c = n0c, n1c
j_aiajcjai_ajc: do j = n0jk, n1jk
a1 = min(c - 1, n1abd)
a_aiajcjai_ajc: do a = n0abd, a1
if (a == c) cycle a_aiajcjai_ajc
nn0i = max(j + 1, n0il)
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajcjai_ajc(t2, &
 nocc, nactive, a, j, c)
i0 = max(j + 1, n0il)
i_aiajcjai_ajc: do i = i0, n1il
if (i == j) cycle i_aiajcjai_ajc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjai_ajc
end do a_aiajcjai_ajc
end do j_aiajcjai_ajc
end do c_aiajcjai_ajc
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aiajcjai_aijc: do c = n0c, n1c
j_aiajcjai_aijc: do j = n0jk, n1jk
a1 = min(c - 1, n1abd)
a_aiajcjai_aijc: do a = n0abd, a1
if (a == c) cycle a_aiajcjai_aijc
i0 = max(j + 1, n0il)
i_aiajcjai_aijc: do i = i0, n1il
if (i == j) cycle i_aiajcjai_aijc
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajcjai_aijc(t2, nocc, nactive, a, i, &
 j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjai_aijc
end do a_aiajcjai_aijc
end do j_aiajcjai_aijc
end do c_aiajcjai_aijc
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == a, k == j, l == j
! No equalities independent of the above can hold.
!
c_aiajcjaj_aijc: do c = n0c, n1c
j_aiajcjaj_aijc: do j = n0jkl, n1jkl
a1 = min(c - 1, n1abd)
a_aiajcjaj_aijc: do a = n0abd, a1
if (a == c) cycle a_aiajcjaj_aijc
i0 = max(j + 1, n0i)
i_aiajcjaj_aijc: do i = i0, n1i
if (i == j) cycle i_aiajcjaj_aijc
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajcjaj_aijc(t2, nocc, nactive, a, i, &
 j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaj_aijc
end do a_aiajcjaj_aijc
end do j_aiajcjaj_aijc
end do c_aiajcjaj_aijc
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicj_ac: do c = n0cd, n1cd
a_aiajcicj_ac: do a = n0ab, n1ab
if (a == c) cycle a_aiajcicj_ac
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajcicj_ac(t2, &
 nocc, nactive, a, c)
j_aiajcicj_ac: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aiajcicj_ac: do i = i0, n1ik
if (i == j) cycle i_aiajcicj_ac
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicj_ac
end do j_aiajcicj_ac
end do a_aiajcicj_ac
end do c_aiajcicj_ac
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicj_ajc: do c = n0cd, n1cd
j_aiajcicj_ajc: do j = n0jl, n1jl
a_aiajcicj_ajc: do a = n0ab, n1ab
if (a == c) cycle a_aiajcicj_ajc
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajcicj_ajc(t2, &
 nocc, nactive, a, j, c)
i0 = max(j + 1, n0ik)
i_aiajcicj_ajc: do i = i0, n1ik
if (i == j) cycle i_aiajcicj_ajc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicj_ajc
end do a_aiajcicj_ajc
end do j_aiajcicj_ajc
end do c_aiajcicj_ajc
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aiajcicj_aic: do c = n0cd, n1cd
a_aiajcicj_aic: do a = n0ab, n1ab
if (a == c) cycle a_aiajcicj_aic
i_aiajcicj_aic: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aiajcicj_aic(t2, &
 nocc, nactive, a, i, c)
j1 = min(i - 1, n1jl)
j_aiajcicj_aic: do j = n0jl, j1
if (j == i) cycle j_aiajcicj_aic
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcicj_aic
end do i_aiajcicj_aic
end do a_aiajcicj_aic
end do c_aiajcicj_aic
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i
! Equalities: d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibiciai_bc: do c = n0c, n1c
b_aibiciai_bc: do b = n0b, n1b
if (b == c) cycle b_aibiciai_bc
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((n1ijkl .ge. n0ijkl).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiciai_bc(t2, &
 nocc, nactive, b, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibiciai_bc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibiciai_bc
i_aibiciai_bc: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciai_bc
end do a_aibiciai_bc
end do b_aibiciai_bc
end do c_aibiciai_bc
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i
! Equalities: d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibiciai_abc: do c = n0c, n1c
b_aibiciai_abc: do b = n0b, n1b
if (b == c) cycle b_aibiciai_abc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibiciai_abc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibiciai_abc
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiciai_abc(t2, &
 nocc, nactive, a, b, c)
i_aibiciai_abc: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciai_abc
end do a_aibiciai_abc
end do b_aibiciai_abc
end do c_aibiciai_abc
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i
! Equalities: d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibiciai_ibc: do c = n0c, n1c
b_aibiciai_ibc: do b = n0b, n1b
if (b == c) cycle b_aibiciai_ibc
i_aibiciai_ibc: do i = n0ijkl, n1ijkl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiciai_ibc(t2, &
 nocc, nactive, i, b, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibiciai_ibc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibiciai_ibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiciai_ibc
end do i_aibiciai_ibc
end do b_aibiciai_ibc
end do c_aibiciai_ibc
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i
! Equalities: d == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibiciai_aibc: do c = n0c, n1c
b_aibiciai_aibc: do b = n0b, n1b
if (b == c) cycle b_aibiciai_aibc
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibiciai_aibc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibiciai_aibc
i_aibiciai_aibc: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibiciai_aibc(t2, nocc, nactive, a, i, &
 b, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciai_aibc
end do a_aibiciai_aibc
end do b_aibiciai_aibc
end do c_aibiciai_aibc
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, l
! Equalities: c == b, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibibibl_aibl: do l = n0l, n1l
b_aibibibl_aibl: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibibibl_aibl: do a = a0, n1a
if (a == b) cycle a_aibibibl_aibl
i0 = max(l + 1, n0ijk)
i_aibibibl_aibl: do i = i0, n1ijk
if (i == l) cycle i_aibibibl_aibl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibibl_aibl(t2, nocc, nactive, a, i, &
 b, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibibl_aibl
end do a_aibibibl_aibl
end do b_aibibibl_aibl
end do l_aibibibl_aibl
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjbibj_ab: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibjbibj_ab: do a = a0, n1a
if (a == b) cycle a_aibjbibj_ab
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbibj_ab(t2, &
 nocc, nactive, a, b)
j_aibjbibj_ab: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjbibj_ab: do i = i0, n1ik
if (i == j) cycle i_aibjbibj_ab
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibj_ab
end do j_aibjbibj_ab
end do a_aibjbibj_ab
end do b_aibjbibj_ab
!
! Elementary loop  18
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbibj_aib(t2, &
 nocc, nactive, a, i, b)
j1 = min(i - 1, n1jl)
j_aibjbibj_aib: do j = n0jl, j1
if (j == i) cycle j_aibjbibj_aib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbibj_aib
end do i_aibjbibj_aib
end do a_aibjbibj_aib
end do b_aibjbibj_aib
!
! Elementary loop  19
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbibj_abj(t2, &
 nocc, nactive, a, b, j)
i0 = max(j + 1, n0ik)
i_aibjbibj_abj: do i = i0, n1ik
if (i == j) cycle i_aibjbibj_abj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibj_abj
end do a_aibjbibj_abj
end do j_aibjbibj_abj
end do b_aibjbibj_abj
!
! Elementary loop  20
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
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbibj_aibj(t2, nocc, nactive, a, i, &
 b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibj_aibj
end do a_aibjbibj_aibj
end do j_aibjbibj_aibj
end do b_aibjbibj_aibj
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibibkbi_aibk: do k = n0k, n1k
b_aibibkbi_aibk: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibibkbi_aibk: do a = a0, n1a
if (a == b) cycle a_aibibkbi_aibk
i1 = min(k - 1, n1ijl)
i_aibibkbi_aibk: do i = n0ijl, i1
if (i == k) cycle i_aibibkbi_aibk
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibkbi_aibk(t2, nocc, nactive, a, i, &
 b, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbi_aibk
end do a_aibibkbi_aibk
end do b_aibibkbi_aibk
end do k_aibibkbi_aibk
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjbjbi_ab: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibjbjbi_ab: do a = a0, n1a
if (a == b) cycle a_aibjbjbi_ab
nn1i = min(n1jk - 1, n1il)
if ((n1jk .ge. n0jk).and. (nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjbi_ab(t2, &
 nocc, nactive, a, b)
j_aibjbjbi_ab: do j = n0jk, n1jk
i1 = min(j - 1, n1il)
i_aibjbjbi_ab: do i = n0il, i1
if (i == j) cycle i_aibjbjbi_ab
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbi_ab
end do j_aibjbjbi_ab
end do a_aibjbjbi_ab
end do b_aibjbjbi_ab
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjbjbi_aib: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibjbjbi_aib: do a = a0, n1a
if (a == b) cycle a_aibjbjbi_aib
i1 = min(j - 1, n1il)
i_aibjbjbi_aib: do i = n0il, i1
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjbi_aib(t2, &
 nocc, nactive, a, i, b)
j_aibjbjbi_aib: do j = n0jk, n1jk
if (j == i) cycle j_aibjbjbi_aib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjbi_aib
end do i_aibjbjbi_aib
end do a_aibjbjbi_aib
end do b_aibjbjbi_aib
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjbjbi_abj: do b = n0bcd, n1bcd
j_aibjbjbi_abj: do j = n0jk, n1jk
a0 = max(b + 1, n0a)
a_aibjbjbi_abj: do a = a0, n1a
if (a == b) cycle a_aibjbjbi_abj
nn1i = min(j - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjbi_abj(t2, &
 nocc, nactive, a, b, j)
i1 = min(j - 1, n1il)
i_aibjbjbi_abj: do i = n0il, i1
if (i == j) cycle i_aibjbjbi_abj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbi_abj
end do a_aibjbjbi_abj
end do j_aibjbjbi_abj
end do b_aibjbjbi_abj
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjbjbi_aibj: do b = n0bcd, n1bcd
j_aibjbjbi_aibj: do j = n0jk, n1jk
a0 = max(b + 1, n0a)
a_aibjbjbi_aibj: do a = a0, n1a
if (a == b) cycle a_aibjbjbi_aibj
i1 = min(j - 1, n1il)
i_aibjbjbi_aibj: do i = n0il, i1
if (i == j) cycle i_aibjbjbi_aibj
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibjbjbi_aibj(t2, nocc, nactive, a, i, &
 b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbi_aibj
end do a_aibjbjbi_aibj
end do j_aibjbjbi_aibj
end do b_aibjbjbi_aibj
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i
! Equalities: c == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibibidi_ad: do d = n0d, n1d
a_aibibidi_ad: do a = n0a, n1a
if (a == d) cycle a_aibibidi_ad
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b).and. (n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibidi_ad(t2, &
 nocc, nactive, a, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibidi_ad: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibidi_ad
i_aibibidi_ad: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidi_ad
end do b_aibibidi_ad
end do a_aibibidi_ad
end do d_aibibidi_ad
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i
! Equalities: c == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibibidi_abd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibibidi_abd: do b = b0, n1bc
if (b == d) cycle b_aibibidi_abd
a0 = max(b + 1, n0a)
a_aibibidi_abd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibidi_abd
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibidi_abd(t2, &
 nocc, nactive, a, b, d)
i_aibibidi_abd: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidi_abd
end do a_aibibidi_abd
end do b_aibibidi_abd
end do d_aibibidi_abd
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i
! Equalities: c == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibibidi_aid: do d = n0d, n1d
a_aibibidi_aid: do a = n0a, n1a
if (a == d) cycle a_aibibidi_aid
i_aibibidi_aid: do i = n0ijkl, n1ijkl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibidi_aid(t2, &
 nocc, nactive, a, i, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibidi_aid: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibidi_aid
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibidi_aid
end do i_aibibidi_aid
end do a_aibibidi_aid
end do d_aibibidi_aid
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i
! Equalities: c == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibibidi_aibd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibibidi_aibd: do b = b0, n1bc
if (b == d) cycle b_aibibidi_aibd
a0 = max(b + 1, n0a)
a_aibibidi_aibd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibidi_aibd
i_aibibidi_aibd: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibibidi_aibd(t2, nocc, nactive, a, i, &
 b, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidi_aibd
end do a_aibibidi_aibd
end do b_aibibidi_aibd
end do d_aibibidi_aibd
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i
! Equalities: d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibicibi_ac: do c = n0c, n1c
a_aibicibi_ac: do a = n0a, n1a
if (a == c) cycle a_aibicibi_ac
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_tripletmm_trans_aibicibi_ac(t2, &
 nocc, nactive, a, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibicibi_ac: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibicibi_ac
i_aibicibi_ac: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((ai - 1) * (ai - 2)) / 2 + bj
iket = ketoffset + &
((ck - 1) * (ck - 2)) / 2 + dl
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibi_ac
end do b_aibicibi_ac
end do a_aibicibi_ac
end do c_aibicibi_ac
end subroutine ccjac_22_tripletmm_dav_part12
end module ccjac_block_22_tripletmm_dav_part12
