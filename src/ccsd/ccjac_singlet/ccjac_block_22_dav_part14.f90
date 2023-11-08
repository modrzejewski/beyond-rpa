module ccjac_block_22_dav_part14
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
subroutine ccjac_22_dav_part14(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: nn0a, nn0b, nn0i, nn1b, nn1i, nn1j
integer :: a0, b0, b1, i0, i1, j1
integer :: n0abcd, n0bc, n0bcd, n0bd, n0cd
integer :: n0ij, n0ijk, n0ijkl, n0ijl, n0ik
integer :: n0ikl, n0il, n0jk, n0jkl, n0jl
integer :: n0kl
integer :: n1abcd, n1bc, n1bcd, n1bd, n1cd
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
n0abcd = max(n0a, n0b, n0c, n0d)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0bd = max(n0b, n0d)
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
n1abcd = min(n1a, n1b, n1c, n1d)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1bd = min(n1b, n1d)
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
jac_ibra_iket = eom_ccsd_22_trans_aibibkbi_aibk(t2, nocc, nactive, a, i, b, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbi_aibk
end do a_aibibkbi_aibk
end do b_aibibkbi_aibk
end do k_aibibkbi_aibk
!
! Elementary loop  2
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k
! Equalities: c == b, d == b, j == i, l == k
! No equalities independent of the above can hold.
!
k_aibibkbk_aibk: do k = n0kl, n1kl
b_aibibkbk_aibk: do b = n0bcd, n1bcd
a0 = max(b + 1, n0a)
a_aibibkbk_aibk: do a = a0, n1a
if (a == b) cycle a_aibibkbk_aibk
i_aibibkbk_aibk: do i = n0ij, n1ij
if (i == k) cycle i_aibibkbk_aibk
jac_ibra_iket = eom_ccsd_22_trans_aibibkbk_aibk(t2, nocc, nactive, a, i, b, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbk_aibk
end do a_aibibkbk_aibk
end do b_aibibkbk_aibk
end do k_aibibkbk_aibk
!
! Elementary loop  3
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
if ((n1jk .ge. n0jk).and. (nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjbjbi_ab(t2, &
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
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbi_ab
end do j_aibjbjbi_ab
end do a_aibjbjbi_ab
end do b_aibjbjbi_ab
!
! Elementary loop  4
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
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjbjbi_aib(t2, nocc, nactive, &
 a, i, b)
j_aibjbjbi_aib: do j = n0jk, n1jk
if (j == i) cycle j_aibjbjbi_aib
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjbi_aib
end do i_aibjbjbi_aib
end do a_aibjbjbi_aib
end do b_aibjbjbi_aib
!
! Elementary loop  5
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
if ((nn1i .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjbjbi_abj(t2, nocc, nactive, &
 a, b, j)
i1 = min(j - 1, n1il)
i_aibjbjbi_abj: do i = n0il, i1
if (i == j) cycle i_aibjbjbi_abj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbi_abj
end do a_aibjbjbi_abj
end do j_aibjbjbi_abj
end do b_aibjbjbi_abj
!
! Elementary loop  6
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
jac_ibra_iket = eom_ccsd_22_trans_aibjbjbi_aibj(t2, nocc, nactive, a, i, b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbi_aibj
end do a_aibjbjbi_aibj
end do j_aibjbjbi_aibj
end do b_aibjbjbi_aibj
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i
! Equalities: c == b, d == b, k == j, l == j
! No equalities independent of the above can hold.
!
b_aibjbjbj_aibj: do b = n0bcd, n1bcd
j_aibjbjbj_aibj: do j = n0jkl, n1jkl
a0 = max(b + 1, n0a)
a_aibjbjbj_aibj: do a = a0, n1a
if (a == b) cycle a_aibjbjbj_aibj
i_aibjbjbj_aibj: do i = n0i, n1i
if (i == j) cycle i_aibjbjbj_aibj
jac_ibra_iket = eom_ccsd_22_trans_aibjbjbj_aibj(t2, nocc, nactive, a, i, b, j)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjbj_aibj
end do a_aibjbjbj_aibj
end do j_aibjbjbj_aibj
end do b_aibjbjbj_aibj
!
! Elementary loop  8
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
if ((nn1b .ge. nn0b).and. (n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibibidi_ad(t2, &
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
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidi_ad
end do b_aibibidi_ad
end do a_aibibidi_ad
end do d_aibibidi_ad
!
! Elementary loop  9
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
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibibidi_abd(t2, nocc, &
 nactive, a, b, d)
i_aibibidi_abd: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidi_abd
end do a_aibibidi_abd
end do b_aibibidi_abd
end do d_aibibidi_abd
!
! Elementary loop  10
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
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibibidi_aid(t2, nocc, nactive, &
 a, i, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibidi_aid: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibidi_aid
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibidi_aid
end do i_aibibidi_aid
end do a_aibibidi_aid
end do d_aibibidi_aid
!
! Elementary loop  11
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
jac_ibra_iket = eom_ccsd_22_trans_aibibidi_aibd(t2, nocc, nactive, a, i, b, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidi_aibd
end do a_aibibidi_aibd
end do b_aibibidi_aibd
end do d_aibibidi_aibd
!
! Elementary loop  12
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i
! Equalities: d == c, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibicici_abc: do c = n0cd, n1cd
b_aibicici_abc: do b = n0b, n1b
if (b == c) cycle b_aibicici_abc
a0 = max(b + 1, n0a)
a_aibicici_abc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicici_abc
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibicici_abc(t2, nocc, &
 nactive, a, b, c)
i_aibicici_abc: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicici_abc
end do a_aibicici_abc
end do b_aibicici_abc
end do c_aibicici_abc
!
! Elementary loop  13
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i
! Equalities: d == c, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibicici_aibc: do c = n0cd, n1cd
b_aibicici_aibc: do b = n0b, n1b
if (b == c) cycle b_aibicici_aibc
a0 = max(b + 1, n0a)
a_aibicici_aibc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicici_aibc
i_aibicici_aibc: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aibicici_aibc(t2, nocc, nactive, a, i, b, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicici_aibc
end do a_aibicici_aibc
end do b_aibicici_aibc
end do c_aibicici_aibc
!
! Elementary loop  14
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
if ((nn1b .ge. n0bd).and. (n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibicibi_ac(t2, &
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
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibi_ac
end do b_aibicibi_ac
end do a_aibicibi_ac
end do c_aibicibi_ac
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i
! Equalities: d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibicibi_aic: do c = n0c, n1c
a_aibicibi_aic: do a = n0a, n1a
if (a == c) cycle a_aibicibi_aic
i_aibicibi_aic: do i = n0ijkl, n1ijkl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_trans_aibicibi_aic(t2, nocc, nactive, &
 a, i, c)
b1 = min(a - 1, c - 1, n1bd)
b_aibicibi_aic: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibicibi_aic
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibicibi_aic
end do i_aibicibi_aic
end do a_aibicibi_aic
end do c_aibicibi_aic
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i
! Equalities: d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibicibi_abc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibicibi_abc: do b = n0bd, b1
if (b == c) cycle b_aibicibi_abc
a0 = max(b + 1, n0a)
a_aibicibi_abc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicibi_abc
if ((n1ijkl .ge. n0ijkl))jac_ibra_iket = eom_ccsd_22_trans_aibicibi_abc(t2, nocc, &
 nactive, a, b, c)
i_aibicibi_abc: do i = n0ijkl, n1ijkl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibi_abc
end do a_aibicibi_abc
end do b_aibicibi_abc
end do c_aibicibi_abc
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i
! Equalities: d == b, j == i, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibicibi_aibc: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibicibi_aibc: do b = n0bd, b1
if (b == c) cycle b_aibicibi_aibc
a0 = max(b + 1, n0a)
a_aibicibi_aibc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicibi_aibc
i_aibicibi_aibc: do i = n0ijkl, n1ijkl
jac_ibra_iket = eom_ccsd_22_trans_aibicibi_aibc(t2, nocc, nactive, a, i, b, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicibi_aibc
end do a_aibicibi_aibc
end do b_aibicibi_aibc
end do c_aibicibi_aibc
!
! Elementary loop  18
! --------------------
! Free virtual indices: a
! Free occupied indices: i, l
! Equalities: b == a, c == a, d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aiaiaial_il: do l = n0l, n1l
i0 = max(l + 1, n0ijk)
i_aiaiaial_il: do i = i0, n1ijk
if (i == l) cycle i_aiaiaial_il
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiaiaial_il(t2, nocc, &
 nactive, i, l)
a_aiaiaial_il: do a = n0abcd, n1abcd
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiaiaial_il
end do i_aiaiaial_il
end do l_aiaiaial_il
!
! Elementary loop  19
! --------------------
! Free virtual indices: a
! Free occupied indices: i, l
! Equalities: b == a, c == a, d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aiaiaial_ail: do l = n0l, n1l
a_aiaiaial_ail: do a = n0abcd, n1abcd
i0 = max(l + 1, n0ijk)
i_aiaiaial_ail: do i = i0, n1ijk
if (i == l) cycle i_aiaiaial_ail
jac_ibra_iket = eom_ccsd_22_trans_aiaiaial_ail(t2, nocc, nactive, a, i, l)
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiaial_ail
end do a_aiaiaial_ail
end do l_aiaiaial_ail
!
! Elementary loop  20
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == i
! No equalities independent of the above can hold.
!
j_aiajaiai_ij: do j = n0j, n1j
i0 = max(j + 1, n0ikl)
i_aiajaiai_ij: do i = i0, n1ikl
if (i == j) cycle i_aiajaiai_ij
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiajaiai_ij(t2, nocc, &
 nactive, i, j)
a_aiajaiai_ij: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiajaiai_ij
end do i_aiajaiai_ij
end do j_aiajaiai_ij
!
! Elementary loop  21
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == i
! No equalities independent of the above can hold.
!
j_aiajaiai_aij: do j = n0j, n1j
a_aiajaiai_aij: do a = n0abcd, n1abcd
i0 = max(j + 1, n0ikl)
i_aiajaiai_aij: do i = i0, n1ikl
if (i == j) cycle i_aiajaiai_aij
jac_ibra_iket = eom_ccsd_22_trans_aiajaiai_aij(t2, nocc, nactive, a, i, j)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajaiai_aij
end do a_aiajaiai_aij
end do j_aiajaiai_aij
!
! Elementary loop  22
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
a_aiajaiaj_a: do a = n0abcd, n1abcd
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajaiaj_a(t2, &
 nocc, nactive, a)
j_aiajaiaj_a: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aiajaiaj_a: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_a
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiajaiaj_a
end do j_aiajaiaj_a
end do a_aiajaiaj_a
!
! Elementary loop  23
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
i_aiajaiaj_i: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl).and. (n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiajaiaj_i(t2, &
 nocc, nactive, i)
j1 = min(i - 1, n1jl)
j_aiajaiaj_i: do j = n0jl, j1
if (j == i) cycle j_aiajaiaj_i
a_aiajaiaj_i: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do a_aiajaiaj_i
end do j_aiajaiaj_i
end do i_aiajaiaj_i
!
! Elementary loop  24
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
j_aiajaiaj_j: do j = n0jl, n1jl
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i).and. (n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiajaiaj_j(t2, &
 nocc, nactive, j)
a_aiajaiaj_j: do a = n0abcd, n1abcd
i0 = max(j + 1, n0ik)
i_aiajaiaj_j: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_j
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiajaiaj_j
end do a_aiajaiaj_j
end do j_aiajaiaj_j
!
! Elementary loop  25
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
a_aiajaiaj_ai: do a = n0abcd, n1abcd
i_aiajaiaj_ai: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aiajaiaj_ai(t2, nocc, nactive, &
 a, i)
j1 = min(i - 1, n1jl)
j_aiajaiaj_ai: do j = n0jl, j1
if (j == i) cycle j_aiajaiaj_ai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do j_aiajaiaj_ai
end do i_aiajaiaj_ai
end do a_aiajaiaj_ai
!
! Elementary loop  26
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
j_aiajaiaj_aj: do j = n0jl, n1jl
a_aiajaiaj_aj: do a = n0abcd, n1abcd
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aiajaiaj_aj(t2, nocc, nactive, &
 a, j)
i0 = max(j + 1, n0ik)
i_aiajaiaj_aj: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_aj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiajaiaj_aj
end do a_aiajaiaj_aj
end do j_aiajaiaj_aj
!
! Elementary loop  27
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
j_aiajaiaj_ij: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aiajaiaj_ij: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_ij
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiajaiaj_ij(t2, nocc, &
 nactive, i, j)
a_aiajaiaj_ij: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do a_aiajaiaj_ij
end do i_aiajaiaj_ij
end do j_aiajaiaj_ij
!
! Elementary loop  28
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j
! Equalities: b == a, c == a, d == a, k == i, l == j
! No equalities independent of the above can hold.
!
j_aiajaiaj_aij: do j = n0jl, n1jl
a_aiajaiaj_aij: do a = n0abcd, n1abcd
i0 = max(j + 1, n0ik)
i_aiajaiaj_aij: do i = i0, n1ik
if (i == j) cycle i_aiajaiaj_aij
jac_ibra_iket = eom_ccsd_22_trans_aiajaiaj_aij(t2, nocc, nactive, a, i, j)
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_diag(jac_ibra_iket, ibra)
end do i_aiajaiaj_aij
end do a_aiajaiaj_aij
end do j_aiajaiaj_aij
!
! Elementary loop  29
! --------------------
! Free virtual indices: a
! Free occupied indices: i, k
! Equalities: b == a, c == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
k_aiaiakai_ik: do k = n0k, n1k
i1 = min(k - 1, n1ijl)
i_aiaiakai_ik: do i = n0ijl, i1
if (i == k) cycle i_aiaiakai_ik
if ((n1abcd .ge. n0abcd))jac_ibra_iket = eom_ccsd_22_trans_aiaiakai_ik(t2, nocc, &
 nactive, i, k)
a_aiaiakai_ik: do a = n0abcd, n1abcd
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aiaiakai_ik
end do i_aiaiakai_ik
end do k_aiaiakai_ik
!
! Elementary loop  30
! --------------------
! Free virtual indices: a
! Free occupied indices: i, k
! Equalities: b == a, c == a, d == a, j == i, l == i
! No equalities independent of the above can hold.
!
k_aiaiakai_aik: do k = n0k, n1k
a_aiaiakai_aik: do a = n0abcd, n1abcd
i1 = min(k - 1, n1ijl)
i_aiaiakai_aik: do i = n0ijl, i1
if (i == k) cycle i_aiaiakai_aik
jac_ibra_iket = eom_ccsd_22_trans_aiaiakai_aik(t2, nocc, nactive, a, i, k)
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiaiakai_aik
end do a_aiaiakai_aik
end do k_aiaiakai_aik
end subroutine ccjac_22_dav_part14
end module ccjac_block_22_dav_part14
