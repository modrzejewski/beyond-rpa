module ccjac_block_22_dav_part13
use eom_ccsd_22_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2018-11-28 15:01:35 UTC.
!
contains
subroutine ccjac_22_dav_part13(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0) 
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
integer :: nn0a, nn0c, nn0i, nn1a, nn1i, nn1j
integer :: a0, a1, c0, i0, i1, j1
integer :: n0ab, n0abd, n0ad, n0bcd, n0cd
integer :: n0ij, n0ijk, n0ijkl, n0ijl, n0ik
integer :: n0ikl, n0il, n0jk, n0jkl, n0jl
integer :: n0kl
integer :: n1ab, n1abd, n1ad, n1bcd, n1cd
integer :: n1ij, n1ijk, n1ijkl, n1ijl, n1ik
integer :: n1ikl, n1il, n1jk, n1jkl, n1jl
integer :: n1kl
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
n0bcd = max(n0b, n0c, n0d)
n0cd = max(n0c, n0d)
n0ij = max(n0i, n0j)
n0ijk = max(n0i, n0j, n0k)
n0ijkl = max(n0i, n0j, n0k, n0l)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1ad = min(n1a, n1d)
n1bcd = min(n1b, n1c, n1d)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
n1ijk = min(n1i, n1j, n1k)
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
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajciaj_ac(t2, &
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
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaj_ac
end do j_aiajciaj_ac
end do a_aiajciaj_ac
end do c_aiajciaj_ac
!
! Elementary loop  2
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aiajciaj_aic(t2, nocc, nactive, &
 a, i, c)
j1 = min(i - 1, n1jl)
j_aiajciaj_aic: do j = n0jl, j1
if (j == i) cycle j_aiajciaj_aic
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajciaj_aic
end do i_aiajciaj_aic
end do a_aiajciaj_aic
end do c_aiajciaj_aic
!
! Elementary loop  3
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajciaj_ajc(t2, nocc, nactive, &
 a, j, c)
i0 = max(j + 1, n0ik)
i_aiajciaj_ajc: do i = i0, n1ik
if (i == j) cycle i_aiajciaj_ajc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaj_ajc
end do a_aiajciaj_ajc
end do j_aiajciaj_ajc
end do c_aiajciaj_ajc
!
! Elementary loop  4
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
jac_ibra_iket = eom_ccsd_22_trans_aiajciaj_aijc(t2, nocc, nactive, a, i, j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajciaj_aijc
end do a_aiajciaj_aijc
end do j_aiajciaj_aijc
end do c_aiajciaj_aijc
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aiaickai_aick: do c = n0c, n1c
k_aiaickai_aick: do k = n0k, n1k
a1 = min(c - 1, n1abd)
a_aiaickai_aick: do a = n0abd, a1
if (a == c) cycle a_aiaickai_aick
i_aiaickai_aick: do i = n0ijl, n1ijl
if (i == k) cycle i_aiaickai_aick
jac_ibra_iket = eom_ccsd_22_trans_aiaickai_aick(t2, nocc, nactive, a, i, c, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickai_aick
end do a_aiaickai_aick
end do k_aiaickai_aick
end do c_aiaickai_aick
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == a, j == i, l == k
! No equalities independent of the above can hold.
!
c_aiaickak_aick: do c = n0c, n1c
k_aiaickak_aick: do k = n0kl, n1kl
a1 = min(c - 1, n1abd)
a_aiaickak_aick: do a = n0abd, a1
if (a == c) cycle a_aiaickak_aick
i_aiaickak_aick: do i = n0ij, n1ij
if (i == k) cycle i_aiaickak_aick
jac_ibra_iket = eom_ccsd_22_trans_aiaickak_aick(t2, nocc, nactive, a, i, c, k)
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickak_aick
end do a_aiaickak_aick
end do k_aiaickak_aick
end do c_aiaickak_aick
!
! Elementary loop  7
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
if ((n1jk .ge. n0jk).and. (n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcjai_ac(t2, &
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
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjai_ac
end do j_aiajcjai_ac
end do a_aiajcjai_ac
end do c_aiajcjai_ac
!
! Elementary loop  8
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
if ((nn1j .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aiajcjai_aic(t2, nocc, nactive, &
 a, i, c)
j1 = min(i - 1, n1jk)
j_aiajcjai_aic: do j = n0jk, j1
if (j == i) cycle j_aiajcjai_aic
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcjai_aic
end do i_aiajcjai_aic
end do a_aiajcjai_aic
end do c_aiajcjai_aic
!
! Elementary loop  9
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
if ((n1il .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcjai_ajc(t2, nocc, nactive, &
 a, j, c)
i0 = max(j + 1, n0il)
i_aiajcjai_ajc: do i = i0, n1il
if (i == j) cycle i_aiajcjai_ajc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjai_ajc
end do a_aiajcjai_ajc
end do j_aiajcjai_ajc
end do c_aiajcjai_ajc
!
! Elementary loop  10
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
jac_ibra_iket = eom_ccsd_22_trans_aiajcjai_aijc(t2, nocc, nactive, a, i, j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjai_aijc
end do a_aiajcjai_aijc
end do j_aiajcjai_aijc
end do c_aiajcjai_aijc
!
! Elementary loop  11
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
jac_ibra_iket = eom_ccsd_22_trans_aiajcjaj_aijc(t2, nocc, nactive, a, i, j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjaj_aijc
end do a_aiajcjaj_aijc
end do j_aiajcjaj_aijc
end do c_aiajcjaj_aijc
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, l
! Equalities: b == a, d == c, j == i, k == i
! No equalities independent of the above can hold.
!
l_aiaicicl_aicl: do l = n0l, n1l
c_aiaicicl_aicl: do c = n0cd, n1cd
a_aiaicicl_aicl: do a = n0ab, n1ab
if (a == c) cycle a_aiaicicl_aicl
i0 = max(l + 1, n0ijk)
i_aiaicicl_aicl: do i = i0, n1ijk
if (i == l) cycle i_aiaicicl_aicl
jac_ibra_iket = eom_ccsd_22_trans_aiaicicl_aicl(t2, nocc, nactive, a, i, c, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicicl_aicl
end do a_aiaicicl_aicl
end do c_aiaicicl_aicl
end do l_aiaicicl_aicl
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j
! Equalities: b == a, d == c, k == i, l == i
! No equalities independent of the above can hold.
!
c_aiajcici_aijc: do c = n0cd, n1cd
j_aiajcici_aijc: do j = n0j, n1j
a_aiajcici_aijc: do a = n0ab, n1ab
if (a == c) cycle a_aiajcici_aijc
i0 = max(j + 1, n0ikl)
i_aiajcici_aijc: do i = i0, n1ikl
if (i == j) cycle i_aiajcici_aijc
jac_ibra_iket = eom_ccsd_22_trans_aiajcici_aijc(t2, nocc, nactive, a, i, j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcici_aijc
end do a_aiajcici_aijc
end do j_aiajcici_aijc
end do c_aiajcici_aijc
!
! Elementary loop  14
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
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcicj_ac(t2, &
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
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicj_ac
end do j_aiajcicj_ac
end do a_aiajcicj_ac
end do c_aiajcicj_ac
!
! Elementary loop  15
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aiajcicj_aic(t2, nocc, nactive, &
 a, i, c)
j1 = min(i - 1, n1jl)
j_aiajcicj_aic: do j = n0jl, j1
if (j == i) cycle j_aiajcicj_aic
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aiajcicj_aic
end do i_aiajcicj_aic
end do a_aiajcicj_aic
end do c_aiajcicj_aic
!
! Elementary loop  16
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajcicj_ajc(t2, nocc, nactive, &
 a, j, c)
i0 = max(j + 1, n0ik)
i_aiajcicj_ajc: do i = i0, n1ik
if (i == j) cycle i_aiajcicj_ajc
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicj_ajc
end do a_aiajcicj_ajc
end do j_aiajcicj_ajc
end do c_aiajcicj_ajc
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, k
! Equalities: b == a, d == c, j == i, l == i
! No equalities independent of the above can hold.
!
c_aiaickci_aick: do c = n0cd, n1cd
k_aiaickci_aick: do k = n0k, n1k
a_aiaickci_aick: do a = n0ab, n1ab
if (a == c) cycle a_aiaickci_aick
i1 = min(k - 1, n1ijl)
i_aiaickci_aick: do i = n0ijl, i1
if (i == k) cycle i_aiaickci_aick
jac_ibra_iket = eom_ccsd_22_trans_aiaickci_aick(t2, nocc, nactive, a, i, c, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaickci_aick
end do a_aiaickci_aick
end do k_aiaickci_aick
end do c_aiaickci_aick
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i
! Equalities: b == a, d == c, k == j, l == j
! No equalities independent of the above can hold.
!
c_aiajcjcj_aijc: do c = n0cd, n1cd
j_aiajcjcj_aijc: do j = n0jkl, n1jkl
a_aiajcjcj_aijc: do a = n0ab, n1ab
if (a == c) cycle a_aiajcjcj_aijc
i0 = max(j + 1, n0i)
i_aiajcjcj_aijc: do i = i0, n1i
if (i == j) cycle i_aiajcjcj_aijc
jac_ibra_iket = eom_ccsd_22_trans_aiajcjcj_aijc(t2, nocc, nactive, a, i, j, c)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjcj_aijc
end do a_aiajcjcj_aijc
end do j_aiajcjcj_aijc
end do c_aiajcjcj_aijc
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i
! Equalities: b == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiaicidi_acd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiaicidi_acd: do c = c0, n1c
if (c == d) cycle c_aiaicidi_acd
a_aiaicidi_acd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiaicidi_acd
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aiaicidi_acd(t2, nocc, &
 nactive, a, c, d)
i_aiaicidi_acd: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicidi_acd
end do a_aiaicidi_acd
end do c_aiaicidi_acd
end do d_aiaicidi_acd
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i
! Equalities: b == a, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
d_aiaicidi_aicd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiaicidi_aicd: do c = c0, n1c
if (c == d) cycle c_aiaicidi_aicd
a_aiaicidi_aicd: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiaicidi_aicd
i_aiaicidi_aicd: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aiaicidi_aicd(t2, nocc, nactive, a, i, c, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaicidi_aicd
end do a_aiaicidi_aicd
end do c_aiaicidi_aicd
end do d_aiaicidi_aicd
!
! Elementary loop  21
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
if ((n1ijkl .ge. n0ijkl).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibiciai_bc(t2, &
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
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciai_bc
end do a_aibiciai_bc
end do b_aibiciai_bc
end do c_aibiciai_bc
!
! Elementary loop  22
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
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibiciai_ibc(t2, nocc, nactive, &
 i, b, c)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibiciai_ibc: do a = a0, a1
if (a == b .or. a == c) cycle a_aibiciai_ibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiciai_ibc
end do i_aibiciai_ibc
end do b_aibiciai_ibc
end do c_aibiciai_ibc
!
! Elementary loop  23
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
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibiciai_abc(t2, nocc, &
 nactive, a, b, c)
i_aibiciai_abc: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciai_abc
end do a_aibiciai_abc
end do b_aibiciai_abc
end do c_aibiciai_abc
!
! Elementary loop  24
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
jac_ibra_iket = eom_ccsd_22_trans_aibiciai_aibc(t2, nocc, nactive, a, i, b, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiciai_aibc
end do a_aibiciai_aibc
end do b_aibiciai_aibc
end do c_aibiciai_aibc
!
! Elementary loop  25
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
jac_ibra_iket = eom_ccsd_22_trans_aibibibl_aibl(t2, nocc, nactive, a, i, b, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibibl_aibl
end do a_aibibibl_aibl
end do b_aibibibl_aibl
end do l_aibibibl_aibl
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j
! Equalities: c == b, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjbibi_aibj: do b = n0bcd, n1bcd
j_aibjbibi_aibj: do j = n0j, n1j
a0 = max(b + 1, n0a)
a_aibjbibi_aibj: do a = a0, n1a
if (a == b) cycle a_aibjbibi_aibj
i_aibjbibi_aibj: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjbibi_aibj
jac_ibra_iket = eom_ccsd_22_trans_aibjbibi_aibj(t2, nocc, nactive, a, i, b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibi_aibj
end do a_aibjbibi_aibj
end do j_aibjbibi_aibj
end do b_aibjbibi_aibj
!
! Elementary loop  27
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
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aibjbibj_ab(t2, &
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
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibj_ab
end do j_aibjbibj_ab
end do a_aibjbibj_ab
end do b_aibjbibj_ab
!
! Elementary loop  28
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjbibj_aib(t2, nocc, nactive, &
 a, i, b)
j1 = min(i - 1, n1jl)
j_aibjbibj_aib: do j = n0jl, j1
if (j == i) cycle j_aibjbibj_aib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbibj_aib
end do i_aibjbibj_aib
end do a_aibjbibj_aib
end do b_aibjbibj_aib
!
! Elementary loop  29
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aibjbibj_abj(t2, nocc, nactive, &
 a, b, j)
i0 = max(j + 1, n0ik)
i_aibjbibj_abj: do i = i0, n1ik
if (i == j) cycle i_aibjbibj_abj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibj_abj
end do a_aibjbibj_abj
end do j_aibjbibj_abj
end do b_aibjbibj_abj
!
! Elementary loop  30
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
jac_ibra_iket = eom_ccsd_22_trans_aibjbibj_aibj(t2, nocc, nactive, a, i, b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibj_aibj
end do a_aibjbibj_aibj
end do j_aibjbibj_aibj
end do b_aibjbibj_aibj
end subroutine ccjac_22_dav_part13
end module ccjac_block_22_dav_part13
