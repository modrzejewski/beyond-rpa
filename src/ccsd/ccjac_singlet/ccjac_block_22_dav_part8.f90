module ccjac_block_22_dav_part8
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
subroutine ccjac_22_dav_part8(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: n0bc, n0bcd, n0cd, n0ij, n0ijk
integer :: n0ijl, n0ik, n0ikl, n0il, n0jk
integer :: n0jkl, n0jl, n0kl
integer :: n1bc, n1bcd, n1cd, n1ij, n1ijk
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
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0cd = max(n0c, n0d)
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
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1cd = min(n1c, n1d)
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
! Free virtual indices: b, a
! Free occupied indices: k, i, j
! Equalities: c == b, d == b, l == k
! No equalities independent of the above can hold.
!
k_aibjbkbk_aibjk: do k = n0kl, n1kl
b_aibjbkbk_aibjk: do b = n0bcd, n1bcd
j_aibjbkbk_aibjk: do j = n0j, n1j
if (j == k) cycle j_aibjbkbk_aibjk
a0 = max(b + 1, n0a)
a_aibjbkbk_aibjk: do a = a0, n1a
if (a == b) cycle a_aibjbkbk_aibjk
i_aibjbkbk_aibjk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjbkbk_aibjk
jac_ibra_iket = eom_ccsd_22_trans_aibjbkbk_aibjk(t2, nocc, nactive, a, i, b, j, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbk_aibjk
end do a_aibjbkbk_aibjk
end do j_aibjbkbk_aibjk
end do b_aibjbkbk_aibjk
end do k_aibjbkbk_aibjk
!
! Elementary loop  2
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
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjbkbj_aibk(t2, nocc, nactive, &
 a, i, b, k)
j1 = min(k - 1, n1jl)
j_aibjbkbj_aibk: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjbkbj_aibk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbkbj_aibk
end do i_aibjbkbj_aibk
end do a_aibjbkbj_aibk
end do b_aibjbkbj_aibk
end do k_aibjbkbj_aibk
!
! Elementary loop  3
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
jac_ibra_iket = eom_ccsd_22_trans_aibjbkbj_aibjk(t2, nocc, nactive, a, i, b, j, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbj_aibjk
end do a_aibjbkbj_aibjk
end do j_aibjbkbj_aibjk
end do b_aibjbkbj_aibjk
end do k_aibjbkbj_aibjk
!
! Elementary loop  4
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
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibibidl_aidl(t2, nocc, nactive, &
 a, i, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibidl_aidl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibidl_aidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibidl_aidl
end do i_aibibidl_aidl
end do a_aibibidl_aidl
end do l_aibibidl_aidl
end do d_aibibidl_aidl
!
! Elementary loop  5
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
jac_ibra_iket = eom_ccsd_22_trans_aibibidl_aibdl(t2, nocc, nactive, a, i, b, d, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibidl_aibdl
end do a_aibibidl_aibdl
end do b_aibibidl_aibdl
end do l_aibibidl_aibdl
end do d_aibibidl_aibdl
!
! Elementary loop  6
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
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbidi_aijd(t2, nocc, nactive, &
 a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidi_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidi_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidi_aijd
end do i_aibjbidi_aijd
end do a_aibjbidi_aijd
end do j_aibjbidi_aijd
end do d_aibjbidi_aijd
!
! Elementary loop  7
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
jac_ibra_iket = eom_ccsd_22_trans_aibjbidi_aibjd(t2, nocc, nactive, a, i, b, j, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidi_aibjd
end do a_aibjbidi_aibjd
end do j_aibjbidi_aibjd
end do b_aibjbidi_aibjd
end do d_aibjbidi_aibjd
!
! Elementary loop  8
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
if ((n1jl .ge. n0jl).and. (n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjbidj_abd(t2, &
 nocc, nactive, a, b, d)
j_aibjbidj_abd: do j = n0jl, n1jl
i_aibjbidj_abd: do i = n0ik, n1ik
if (i == j) cycle i_aibjbidj_abd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_abd
end do j_aibjbidj_abd
end do a_aibjbidj_abd
end do b_aibjbidj_abd
end do d_aibjbidj_abd
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj_aid: do d = n0d, n1d
a_aibjbidj_aid: do a = n0a, n1a
if (a == d) cycle a_aibjbidj_aid
i_aibjbidj_aid: do i = n0ik, n1ik
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jl .ge. n0jl).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbidj_aid(t2, &
 nocc, nactive, a, i, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidj_aid: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidj_aid
j_aibjbidj_aid: do j = n0jl, n1jl
if (j == i) cycle j_aibjbidj_aid
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbidj_aid
end do b_aibjbidj_aid
end do i_aibjbidj_aid
end do a_aibjbidj_aid
end do d_aibjbidj_aid
!
! Elementary loop  10
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
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbidj_aijd(t2, nocc, nactive, &
 a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidj_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidj_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidj_aijd
end do i_aibjbidj_aijd
end do a_aibjbidj_aijd
end do j_aibjbidj_aijd
end do d_aibjbidj_aijd
!
! Elementary loop  11
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
if ((n1jl .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjbidj_aibd(t2, nocc, nactive, &
 a, i, b, d)
j_aibjbidj_aibd: do j = n0jl, n1jl
if (j == i) cycle j_aibjbidj_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbidj_aibd
end do i_aibjbidj_aibd
end do a_aibjbidj_aibd
end do b_aibjbidj_aibd
end do d_aibjbidj_aibd
!
! Elementary loop  12
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
if ((n1ik .ge. n0ik))jac_ibra_iket = eom_ccsd_22_trans_aibjbidj_abjd(t2, nocc, nactive, &
 a, b, j, d)
i_aibjbidj_abjd: do i = n0ik, n1ik
if (i == j) cycle i_aibjbidj_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj_abjd
end do a_aibjbidj_abjd
end do j_aibjbidj_abjd
end do b_aibjbidj_abjd
end do d_aibjbidj_abjd
!
! Elementary loop  13
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
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibibkdi_aikd(t2, nocc, nactive, &
 a, i, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibkdi_aikd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibkdi_aikd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibkdi_aikd
end do i_aibibkdi_aikd
end do a_aibibkdi_aikd
end do k_aibibkdi_aikd
end do d_aibibkdi_aikd
!
! Elementary loop  14
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
jac_ibra_iket = eom_ccsd_22_trans_aibibkdi_aibkd(t2, nocc, nactive, a, i, b, k, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdi_aibkd
end do a_aibibkdi_aibkd
end do b_aibibkdi_aibkd
end do k_aibibkdi_aibkd
end do d_aibibkdi_aibkd
!
! Elementary loop  15
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
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibibkdk_aikd(t2, nocc, nactive, &
 a, i, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibibkdk_aikd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibibkdk_aikd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibibkdk_aikd
end do i_aibibkdk_aikd
end do a_aibibkdk_aikd
end do k_aibibkdk_aikd
end do d_aibibkdk_aikd
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_ad: do d = n0d, n1d
a_aibjbjdi_ad: do a = n0a, n1a
if (a == d) cycle a_aibjbjdi_ad
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jk .ge. n0jk).and. (nn1b .ge. nn0b).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdi_ad(t2, &
 nocc, nactive, a, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdi_ad: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdi_ad
j_aibjbjdi_ad: do j = n0jk, n1jk
i_aibjbjdi_ad: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_ad
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_ad
end do j_aibjbjdi_ad
end do b_aibjbjdi_ad
end do a_aibjbjdi_ad
end do d_aibjbjdi_ad
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_abd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdi_abd: do b = b0, n1bc
if (b == d) cycle b_aibjbjdi_abd
a0 = max(b + 1, n0a)
a_aibjbjdi_abd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdi_abd
if ((n1jk .ge. n0jk).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdi_abd(t2, &
 nocc, nactive, a, b, d)
j_aibjbjdi_abd: do j = n0jk, n1jk
i_aibjbjdi_abd: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_abd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_abd
end do j_aibjbjdi_abd
end do a_aibjbjdi_abd
end do b_aibjbjdi_abd
end do d_aibjbjdi_abd
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_ajd: do d = n0d, n1d
j_aibjbjdi_ajd: do j = n0jk, n1jk
a_aibjbjdi_ajd: do a = n0a, n1a
if (a == d) cycle a_aibjbjdi_ajd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b).and. (n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdi_ajd(t2, &
 nocc, nactive, a, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdi_ajd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdi_ajd
i_aibjbjdi_ajd: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_ajd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_ajd
end do b_aibjbjdi_ajd
end do a_aibjbjdi_ajd
end do j_aibjbjdi_ajd
end do d_aibjbjdi_ajd
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_aid: do d = n0d, n1d
a_aibjbjdi_aid: do a = n0a, n1a
if (a == d) cycle a_aibjbjdi_aid
i_aibjbjdi_aid: do i = n0il, n1il
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((n1jk .ge. n0jk).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdi_aid(t2, &
 nocc, nactive, a, i, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdi_aid: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdi_aid
j_aibjbjdi_aid: do j = n0jk, n1jk
if (j == i) cycle j_aibjbjdi_aid
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdi_aid
end do b_aibjbjdi_aid
end do i_aibjbjdi_aid
end do a_aibjbjdi_aid
end do d_aibjbjdi_aid
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_aijd: do d = n0d, n1d
j_aibjbjdi_aijd: do j = n0jk, n1jk
a_aibjbjdi_aijd: do a = n0a, n1a
if (a == d) cycle a_aibjbjdi_aijd
i_aibjbjdi_aijd: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdi_aijd(t2, nocc, nactive, &
 a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdi_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdi_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdi_aijd
end do i_aibjbjdi_aijd
end do a_aibjbjdi_aijd
end do j_aibjbjdi_aijd
end do d_aibjbjdi_aijd
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_abjd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdi_abjd: do b = b0, n1bc
if (b == d) cycle b_aibjbjdi_abjd
j_aibjbjdi_abjd: do j = n0jk, n1jk
a0 = max(b + 1, n0a)
a_aibjbjdi_abjd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdi_abjd
if ((n1il .ge. n0il))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdi_abjd(t2, nocc, nactive, &
 a, b, j, d)
i_aibjbjdi_abjd: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi_abjd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdi_abjd
end do a_aibjbjdi_abjd
end do j_aibjbjdi_abjd
end do b_aibjbjdi_abjd
end do d_aibjbjdi_abjd
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi_aibd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdi_aibd: do b = b0, n1bc
if (b == d) cycle b_aibjbjdi_aibd
a0 = max(b + 1, n0a)
a_aibjbjdi_aibd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdi_aibd
i_aibjbjdi_aibd: do i = n0il, n1il
if ((n1jk .ge. n0jk))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdi_aibd(t2, nocc, nactive, &
 a, i, b, d)
j_aibjbjdi_aibd: do j = n0jk, n1jk
if (j == i) cycle j_aibjbjdi_aibd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdi_aibd
end do i_aibjbjdi_aibd
end do a_aibjbjdi_aibd
end do b_aibjbjdi_aibd
end do d_aibjbjdi_aibd
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i
! Equalities: c == b, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjbjdj_aijd: do d = n0d, n1d
j_aibjbjdj_aijd: do j = n0jkl, n1jkl
a_aibjbjdj_aijd: do a = n0a, n1a
if (a == d) cycle a_aibjbjdj_aijd
i_aibjbjdj_aijd: do i = n0i, n1i
if (i == j) cycle i_aibjbjdj_aijd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_trans_aibjbjdj_aijd(t2, nocc, nactive, &
 a, i, j, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdj_aijd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdj_aijd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdj_aijd
end do i_aibjbjdj_aijd
end do a_aibjbjdj_aijd
end do j_aibjbjdj_aijd
end do d_aibjbjdj_aijd
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i
! Equalities: c == b, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjbjdj_aibjd: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdj_aibjd: do b = b0, n1bc
if (b == d) cycle b_aibjbjdj_aibjd
j_aibjbjdj_aibjd: do j = n0jkl, n1jkl
a0 = max(b + 1, n0a)
a_aibjbjdj_aibjd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdj_aibjd
i_aibjbjdj_aibjd: do i = n0i, n1i
if (i == j) cycle i_aibjbjdj_aibjd
jac_ibra_iket = eom_ccsd_22_trans_aibjbjdj_aibjd(t2, nocc, nactive, a, i, b, j, d)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbjdj_aibjd
end do a_aibjbjdj_aibjd
end do j_aibjbjdj_aibjd
end do b_aibjbjdj_aibjd
end do d_aibjbjdj_aibjd
!
! Elementary loop  25
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, l
! Equalities: d == c, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibicicl_aibcl: do l = n0l, n1l
c_aibicicl_aibcl: do c = n0cd, n1cd
b_aibicicl_aibcl: do b = n0b, n1b
if (b == c) cycle b_aibicicl_aibcl
a0 = max(b + 1, n0a)
a_aibicicl_aibcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibicicl_aibcl
i0 = max(l + 1, n0ijk)
i_aibicicl_aibcl: do i = i0, n1ijk
if (i == l) cycle i_aibicicl_aibcl
jac_ibra_iket = eom_ccsd_22_trans_aibicicl_aibcl(t2, nocc, nactive, a, i, b, c, l)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibicicl_aibcl
end do a_aibicicl_aibcl
end do b_aibicicl_aibcl
end do c_aibicicl_aibcl
end do l_aibicicl_aibcl
!
! Elementary loop  26
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcici_aibjc: do c = n0cd, n1cd
b_aibjcici_aibjc: do b = n0b, n1b
if (b == c) cycle b_aibjcici_aibjc
j_aibjcici_aibjc: do j = n0j, n1j
a0 = max(b + 1, n0a)
a_aibjcici_aibjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcici_aibjc
i_aibjcici_aibjc: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcici_aibjc
jac_ibra_iket = eom_ccsd_22_trans_aibjcici_aibjc(t2, nocc, nactive, a, i, b, j, c)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcici_aibjc
end do a_aibjcici_aibjc
end do j_aibjcici_aibjc
end do b_aibjcici_aibjc
end do c_aibjcici_aibjc
!
! Elementary loop  27
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj_abc: do c = n0cd, n1cd
b_aibjcicj_abc: do b = n0b, n1b
if (b == c) cycle b_aibjcicj_abc
a0 = max(b + 1, n0a)
a_aibjcicj_abc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicj_abc
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aibjcicj_abc(t2, &
 nocc, nactive, a, b, c)
j_aibjcicj_abc: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjcicj_abc: do i = i0, n1ik
if (i == j) cycle i_aibjcicj_abc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicj_abc
end do j_aibjcicj_abc
end do a_aibjcicj_abc
end do b_aibjcicj_abc
end do c_aibjcicj_abc
!
! Elementary loop  28
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj_aibc: do c = n0cd, n1cd
b_aibjcicj_aibc: do b = n0b, n1b
if (b == c) cycle b_aibjcicj_aibc
a0 = max(b + 1, n0a)
a_aibjcicj_aibc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicj_aibc
i_aibjcicj_aibc: do i = n0ik, n1ik
nn1j = min(i - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_trans_aibjcicj_aibc(t2, nocc, nactive, &
 a, i, b, c)
j1 = min(i - 1, n1jl)
j_aibjcicj_aibc: do j = n0jl, j1
if (j == i) cycle j_aibjcicj_aibc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcicj_aibc
end do i_aibjcicj_aibc
end do a_aibjcicj_aibc
end do b_aibjcicj_aibc
end do c_aibjcicj_aibc
!
! Elementary loop  29
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj_abjc: do c = n0cd, n1cd
b_aibjcicj_abjc: do b = n0b, n1b
if (b == c) cycle b_aibjcicj_abjc
j_aibjcicj_abjc: do j = n0jl, n1jl
a0 = max(b + 1, n0a)
a_aibjcicj_abjc: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicj_abjc
nn0i = max(j + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_trans_aibjcicj_abjc(t2, nocc, nactive, &
 a, b, j, c)
i0 = max(j + 1, n0ik)
i_aibjcicj_abjc: do i = i0, n1ik
if (i == j) cycle i_aibjcicj_abjc
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicj_abjc
end do a_aibjcicj_abjc
end do j_aibjcicj_abjc
end do b_aibjcicj_abjc
end do c_aibjcicj_abjc
!
! Elementary loop  30
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, k
! Equalities: d == c, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickci_aibck: do c = n0cd, n1cd
k_aibickci_aibck: do k = n0k, n1k
b_aibickci_aibck: do b = n0b, n1b
if (b == c) cycle b_aibickci_aibck
a0 = max(b + 1, n0a)
a_aibickci_aibck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibickci_aibck
i1 = min(k - 1, n1ijl)
i_aibickci_aibck: do i = n0ijl, i1
if (i == k) cycle i_aibickci_aibck
jac_ibra_iket = eom_ccsd_22_trans_aibickci_aibck(t2, nocc, nactive, a, i, b, c, k)
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibickci_aibck
end do a_aibickci_aibck
end do b_aibickci_aibck
end do k_aibickci_aibck
end do c_aibickci_aibck
end subroutine ccjac_22_dav_part8
end module ccjac_block_22_dav_part8
