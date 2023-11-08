module ccjac_block_22_dav_part11
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
subroutine ccjac_22_dav_part11(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: a, b
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: nn0a, nn0i, nn1b, nn1i, nn1j
integer :: a0, b1, i0, i1, j1
integer :: n0ac, n0acd, n0bd, n0ij, n0ijk
integer :: n0ijl, n0ik, n0ikl, n0il, n0jk
integer :: n0jkl, n0jl, n0kl
integer :: n1ac, n1acd, n1bd, n1ij, n1ijk
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
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0bd = max(n0b, n0d)
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
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1bd = min(n1b, n1d)
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
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaial_aibl: do l = n0l, n1l
b_aibiaial_aibl: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibiaial_aibl: do a = a0, n1acd
if (a == b) cycle a_aibiaial_aibl
i0 = max(l + 1, n0ijk)
i_aibiaial_aibl: do i = i0, n1ijk
if (i == l) cycle i_aibiaial_aibl
jac_ibra_iket = eom_ccsd_22_trans_aibiaial_aibl(t2, nocc, nactive, a, i, b, l)
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaial_aibl
end do a_aibiaial_aibl
end do b_aibiaial_aibl
end do l_aibiaial_aibl
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjaiai_aibj: do b = n0b, n1b
j_aibjaiai_aibj: do j = n0j, n1j
a0 = max(b + 1, n0acd)
a_aibjaiai_aibj: do a = a0, n1acd
if (a == b) cycle a_aibjaiai_aibj
i_aibjaiai_aibj: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaiai_aibj
jac_ibra_iket = eom_ccsd_22_trans_aibjaiai_aibj(t2, nocc, nactive, a, i, b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiai_aibj
end do a_aibjaiai_aibj
end do j_aibjaiai_aibj
end do b_aibjaiai_aibj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaiaj_ab: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibjaiaj_ab: do a = a0, n1acd
if (a == b) cycle a_aibjaiaj_ab
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aibjaiaj_ab(t2, &
 nocc, nactive, a, b)
j_aibjaiaj_ab: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjaiaj_ab: do i = i0, n1ik
if (i == j) cycle i_aibjaiaj_ab
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiaj_ab
end do j_aibjaiaj_ab
end do a_aibjaiaj_ab
end do b_aibjaiaj_ab
!
! Elementary loop  4
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
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aibjaiaj_abj(t2, nocc, nactive, &
 a, b, j)
i0 = max(j + 1, n0ik)
i_aibjaiaj_abj: do i = i0, n1ik
if (i == j) cycle i_aibjaiaj_abj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiaj_abj
end do a_aibjaiaj_abj
end do j_aibjaiaj_abj
end do b_aibjaiaj_abj
!
! Elementary loop  5
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjaiaj_aib(t2, nocc, nactive, &
 a, i, b)
j1 = min(i - 1, n1jl)
j_aibjaiaj_aib: do j = n0jl, j1
if (j == i) cycle j_aibjaiaj_aib
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjaiaj_aib
end do i_aibjaiaj_aib
end do a_aibjaiaj_aib
end do b_aibjaiaj_aib
!
! Elementary loop  6
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
jac_ibra_iket = eom_ccsd_22_trans_aibjaiaj_aibj(t2, nocc, nactive, a, i, b, j)
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaiaj_aibj
end do a_aibjaiaj_aibj
end do j_aibjaiaj_aibj
end do b_aibjaiaj_aibj
!
! Elementary loop  7
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
k_aibiakai_aibk: do k = n0k, n1k
b_aibiakai_aibk: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibiakai_aibk: do a = a0, n1acd
if (a == b) cycle a_aibiakai_aibk
i1 = min(k - 1, n1ijl)
i_aibiakai_aibk: do i = n0ijl, i1
if (i == k) cycle i_aibiakai_aibk
jac_ibra_iket = eom_ccsd_22_trans_aibiakai_aibk(t2, nocc, nactive, a, i, b, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakai_aibk
end do a_aibiakai_aibk
end do b_aibiakai_aibk
end do k_aibiakai_aibk
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k
! Equalities: c == a, d == a, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibiakak_aibk: do k = n0kl, n1kl
b_aibiakak_aibk: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibiakak_aibk: do a = a0, n1acd
if (a == b) cycle a_aibiakak_aibk
i_aibiakak_aibk: do i = n0ij, n1ij
if (i == k) cycle i_aibiakak_aibk
jac_ibra_iket = eom_ccsd_22_trans_aibiakak_aibk(t2, nocc, nactive, a, i, b, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiakak_aibk
end do a_aibiakak_aibk
end do b_aibiakak_aibk
end do k_aibiakak_aibk
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajai_ab: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibjajai_ab: do a = a0, n1acd
if (a == b) cycle a_aibjajai_ab
nn1i = min(n1jk - 1, n1il)
if ((n1jk .ge. n0jk).and. (nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjajai_ab(t2, &
 nocc, nactive, a, b)
j_aibjajai_ab: do j = n0jk, n1jk
i1 = min(j - 1, n1il)
i_aibjajai_ab: do i = n0il, i1
if (i == j) cycle i_aibjajai_ab
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajai_ab
end do j_aibjajai_ab
end do a_aibjajai_ab
end do b_aibjajai_ab
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajai_aib: do b = n0b, n1b
a0 = max(b + 1, n0acd)
a_aibjajai_aib: do a = a0, n1acd
if (a == b) cycle a_aibjajai_aib
i1 = min(j - 1, n1il)
i_aibjajai_aib: do i = n0il, i1
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjajai_aib(t2, nocc, nactive, &
 a, i, b)
j_aibjajai_aib: do j = n0jk, n1jk
if (j == i) cycle j_aibjajai_aib
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjajai_aib
end do i_aibjajai_aib
end do a_aibjajai_aib
end do b_aibjajai_aib
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajai_abj: do b = n0b, n1b
j_aibjajai_abj: do j = n0jk, n1jk
a0 = max(b + 1, n0acd)
a_aibjajai_abj: do a = a0, n1acd
if (a == b) cycle a_aibjajai_abj
nn1i = min(j - 1, n1il)
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjajai_abj(t2, nocc, nactive, &
 a, b, j)
i1 = min(j - 1, n1il)
i_aibjajai_abj: do i = n0il, i1
if (i == j) cycle i_aibjajai_abj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajai_abj
end do a_aibjajai_abj
end do j_aibjajai_abj
end do b_aibjajai_abj
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == a, l == i, k == j
! No equalities independent of the above can hold.
!
b_aibjajai_aibj: do b = n0b, n1b
j_aibjajai_aibj: do j = n0jk, n1jk
a0 = max(b + 1, n0acd)
a_aibjajai_aibj: do a = a0, n1acd
if (a == b) cycle a_aibjajai_aibj
i1 = min(j - 1, n1il)
i_aibjajai_aibj: do i = n0il, i1
if (i == j) cycle i_aibjajai_aibj
jac_ibra_iket = eom_ccsd_22_trans_aibjajai_aibj(t2, nocc, nactive, a, i, b, j)
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajai_aibj
end do a_aibjajai_aibj
end do j_aibjajai_aibj
end do b_aibjajai_aibj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i
! Equalities: c == a, d == a, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjajaj_aibj: do b = n0b, n1b
j_aibjajaj_aibj: do j = n0jkl, n1jkl
a0 = max(b + 1, n0acd)
a_aibjajaj_aibj: do a = a0, n1acd
if (a == b) cycle a_aibjajaj_aibj
i_aibjajaj_aibj: do i = n0i, n1i
if (i == j) cycle i_aibjajaj_aibj
jac_ibra_iket = eom_ccsd_22_trans_aibjajaj_aibj(t2, nocc, nactive, a, i, b, j)
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjajaj_aibj
end do a_aibjajaj_aibj
end do j_aibjajaj_aibj
end do b_aibjajaj_aibj
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaibl_il: do l = n0l, n1l
i_aibiaibl_il: do i = n0ijk, n1ijk
if (i == l) cycle i_aibiaibl_il
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibiaibl_il(t2, &
 nocc, nactive, i, l)
b_aibiaibl_il: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiaibl_il: do a = a0, n1ac
if (a == b) cycle a_aibiaibl_il
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiaibl_il
end do b_aibiaibl_il
end do i_aibiaibl_il
end do l_aibiaibl_il
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaibl_ail: do l = n0l, n1l
a_aibiaibl_ail: do a = n0ac, n1ac
i_aibiaibl_ail: do i = n0ijk, n1ijk
if (i == l) cycle i_aibiaibl_ail
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibiaibl_ail(t2, nocc, nactive, &
 a, i, l)
b1 = min(a - 1, n1bd)
b_aibiaibl_ail: do b = n0bd, b1
if (b == a) cycle b_aibiaibl_ail
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibiaibl_ail
end do i_aibiaibl_ail
end do a_aibiaibl_ail
end do l_aibiaibl_ail
!
! Elementary loop  16
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaibl_ibl: do l = n0l, n1l
b_aibiaibl_ibl: do b = n0bd, n1bd
i_aibiaibl_ibl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibiaibl_ibl
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibiaibl_ibl(t2, nocc, nactive, &
 i, b, l)
a0 = max(b + 1, n0ac)
a_aibiaibl_ibl: do a = a0, n1ac
if (a == b) cycle a_aibiaibl_ibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibiaibl_ibl
end do i_aibiaibl_ibl
end do b_aibiaibl_ibl
end do l_aibiaibl_ibl
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, l
! Equalities: c == a, d == b, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibiaibl_aibl: do l = n0l, n1l
b_aibiaibl_aibl: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibiaibl_aibl: do a = a0, n1ac
if (a == b) cycle a_aibiaibl_aibl
i_aibiaibl_aibl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibiaibl_aibl
jac_ibra_iket = eom_ccsd_22_trans_aibiaibl_aibl(t2, nocc, nactive, a, i, b, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibiaibl_aibl
end do a_aibiaibl_aibl
end do b_aibiaibl_aibl
end do l_aibiaibl_aibl
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
j_aibjaibi_ij: do j = n0j, n1j
i_aibjaibi_ij: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaibi_ij
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjaibi_ij(t2, &
 nocc, nactive, i, j)
b_aibjaibi_ij: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibi_ij: do a = a0, n1ac
if (a == b) cycle a_aibjaibi_ij
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaibi_ij
end do b_aibjaibi_ij
end do i_aibjaibi_ij
end do j_aibjaibi_ij
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
j_aibjaibi_aij: do j = n0j, n1j
a_aibjaibi_aij: do a = n0ac, n1ac
i_aibjaibi_aij: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaibi_aij
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjaibi_aij(t2, nocc, nactive, &
 a, i, j)
b1 = min(a - 1, n1bd)
b_aibjaibi_aij: do b = n0bd, b1
if (b == a) cycle b_aibjaibi_aij
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjaibi_aij
end do i_aibjaibi_aij
end do a_aibjaibi_aij
end do j_aibjaibi_aij
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == i
! No equalities independent of the above can hold.
!
b_aibjaibi_ibj: do b = n0bd, n1bd
j_aibjaibi_ibj: do j = n0j, n1j
i_aibjaibi_ibj: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaibi_ibj
nn0a = max(b + 1, n0ac)
if ((n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjaibi_ibj(t2, nocc, nactive, &
 i, b, j)
a0 = max(b + 1, n0ac)
a_aibjaibi_ibj: do a = a0, n1ac
if (a == b) cycle a_aibjaibi_ibj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjaibi_ibj
end do i_aibjaibi_ibj
end do j_aibjaibi_ibj
end do b_aibjaibi_ibj
!
! Elementary loop  21
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
i_aibjaibi_aibj: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaibi_aibj
jac_ibra_iket = eom_ccsd_22_trans_aibjaibi_aibj(t2, nocc, nactive, a, i, b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjaibi_aibj
end do a_aibjaibi_aibj
end do j_aibjaibi_aibj
end do b_aibjaibi_aibj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
a_aibjaibj_a: do a = n0ac, n1ac
nn1b = min(a - 1, n1bd)
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjaibj_a(t2, &
 nocc, nactive, a)
b1 = min(a - 1, n1bd)
b_aibjaibj_a: do b = n0bd, b1
if (b == a) cycle b_aibjaibj_a
j_aibjaibj_a: do j = n0jl, n1jl
i_aibjaibj_a: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_a
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_a
end do j_aibjaibj_a
end do b_aibjaibj_a
end do a_aibjaibj_a
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_b: do b = n0bd, n1bd
nn0a = max(b + 1, n0ac)
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjaibj_b(t2, &
 nocc, nactive, b)
j_aibjaibj_b: do j = n0jl, n1jl
a0 = max(b + 1, n0ac)
a_aibjaibj_b: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_b
i_aibjaibj_b: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_b
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_b
end do a_aibjaibj_b
end do j_aibjaibj_b
end do b_aibjaibj_b
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
i_aibjaibj_i: do i = n0ik, n1ik
nn0a = max(n0bd + 1, n0ac)
if ((n1jl .ge. n0jl).and. (n1bd .ge. n0bd).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjaibj_i(t2, &
 nocc, nactive, i)
b_aibjaibj_i: do b = n0bd, n1bd
j_aibjaibj_i: do j = n0jl, n1jl
if (j == i) cycle j_aibjaibj_i
a0 = max(b + 1, n0ac)
a_aibjaibj_i: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_i
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibjaibj_i
end do j_aibjaibj_i
end do b_aibjaibj_i
end do i_aibjaibj_i
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
j_aibjaibj_j: do j = n0jl, n1jl
nn0a = max(n0bd + 1, n0ac)
if ((n1bd .ge. n0bd).and. (n1ik .ge. n0ik).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjaibj_j(t2, &
 nocc, nactive, j)
b_aibjaibj_j: do b = n0bd, n1bd
a0 = max(b + 1, n0ac)
a_aibjaibj_j: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_j
i_aibjaibj_j: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_j
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_j
end do a_aibjaibj_j
end do b_aibjaibj_j
end do j_aibjaibj_j
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
a_aibjaibj_ai: do a = n0ac, n1ac
i_aibjaibj_ai: do i = n0ik, n1ik
nn1b = min(a - 1, n1bd)
if ((n1jl .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibjaibj_ai(t2, &
 nocc, nactive, a, i)
b1 = min(a - 1, n1bd)
b_aibjaibj_ai: do b = n0bd, b1
if (b == a) cycle b_aibjaibj_ai
j_aibjaibj_ai: do j = n0jl, n1jl
if (j == i) cycle j_aibjaibj_ai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do j_aibjaibj_ai
end do b_aibjaibj_ai
end do i_aibjaibj_ai
end do a_aibjaibj_ai
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
j_aibjaibj_aj: do j = n0jl, n1jl
a_aibjaibj_aj: do a = n0ac, n1ac
nn1b = min(a - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjaibj_aj(t2, &
 nocc, nactive, a, j)
b1 = min(a - 1, n1bd)
b_aibjaibj_aj: do b = n0bd, b1
if (b == a) cycle b_aibjaibj_aj
i_aibjaibj_aj: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_aj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_aj
end do b_aibjaibj_aj
end do a_aibjaibj_aj
end do j_aibjaibj_aj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_ib: do b = n0bd, n1bd
i_aibjaibj_ib: do i = n0ik, n1ik
nn0a = max(b + 1, n0ac)
if ((n1jl .ge. n0jl).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjaibj_ib(t2, &
 nocc, nactive, i, b)
j_aibjaibj_ib: do j = n0jl, n1jl
if (j == i) cycle j_aibjaibj_ib
a0 = max(b + 1, n0ac)
a_aibjaibj_ib: do a = a0, n1ac
if (a == b) cycle a_aibjaibj_ib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do a_aibjaibj_ib
end do j_aibjaibj_ib
end do i_aibjaibj_ib
end do b_aibjaibj_ib
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j
! Equalities: c == a, d == b, k == i, l == j
! No equalities independent of the above can hold.
!
b_aibjaibj_bj: do b = n0bd, n1bd
j_aibjaibj_bj: do j = n0jl, n1jl
nn0a = max(b + 1, n0ac)
if ((n1ik .ge. n0ik).and. (n1ac .ge. nn0a))jac_ibra_iket = eom_ccsd_22_trans_aibjaibj_bj(t2, &
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
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_bj
end do a_aibjaibj_bj
end do j_aibjaibj_bj
end do b_aibjaibj_bj
!
! Elementary loop  30
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
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjaibj_ab(t2, &
 nocc, nactive, a, b)
j_aibjaibj_ab: do j = n0jl, n1jl
i_aibjaibj_ab: do i = n0ik, n1ik
if (i == j) cycle i_aibjaibj_ab
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aibjaibj_ab
end do j_aibjaibj_ab
end do a_aibjaibj_ab
end do b_aibjaibj_ab
end subroutine ccjac_22_dav_part11
end module ccjac_block_22_dav_part11
