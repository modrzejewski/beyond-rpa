module ccjac_block_22_tripletpp_dav_part2
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
subroutine ccjac_22_tripletpp_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, &
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
integer :: nn0a, nn0b, nn0c, nn0i, nn0j, nn1a, nn1b, nn1i, nn1j
integer :: a0, a1, b0, b1, c0, i0, i1, j0, j1
integer :: n0ad, n0bc, n0bd, n0ik, n0il
integer :: n0jk, n0jl
integer :: n1ad, n1bc, n1bd, n1ik, n1il
integer :: n1jk, n1jl
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
n0bd = max(n0b, n0d)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bd = min(n1b, n1d)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, l == j
! No equalities independent of the above can hold.
!
c_aibjckaj_ibjck: do c = n0c, n1c
k_aibjckaj_ibjck: do k = n0k, n1k
b_aibjckaj_ibjck: do b = n0b, n1b
if (b == c) cycle b_aibjckaj_ibjck
j1 = min(k - 1, n1jl)
j_aibjckaj_ibjck: do j = n0jl, j1
if (j == k) cycle j_aibjckaj_ibjck
i0 = max(j + 1, n0i)
i_aibjckaj_ibjck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjckaj_ibjck
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckaj_ibjck(t2, &
 nocc, nactive, i, b, j, c, k)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaj_ibjck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaj_ibjck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjckaj_ibjck
end do i_aibjckaj_ibjck
end do j_aibjckaj_ibjck
end do b_aibjckaj_ibjck
end do k_aibjckaj_ibjck
end do c_aibjckaj_ibjck
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, l == j
! No equalities independent of the above can hold.
!
c_aibjckaj_aibck: do c = n0c, n1c
k_aibjckaj_aibck: do k = n0k, n1k
b_aibjckaj_aibck: do b = n0b, n1b
if (b == c) cycle b_aibjckaj_aibck
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckaj_aibck: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckaj_aibck
i_aibjckaj_aibck: do i = n0i, n1i
if (i == k) cycle i_aibjckaj_aibck
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckaj_aibck(t2, &
 nocc, nactive, a, i, b, c, k)
j1 = min(i - 1, k - 1, n1jl)
j_aibjckaj_aibck: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckaj_aibck
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (a - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckaj_aibck
end do i_aibjckaj_aibck
end do a_aibjckaj_aibck
end do b_aibjckaj_aibck
end do k_aibjckaj_aibck
end do c_aibjckaj_aibck
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjcjal_ibcl: do l = n0l, n1l
c_aibjcjal_ibcl: do c = n0c, n1c
b_aibjcjal_ibcl: do b = n0b, n1b
if (b == c) cycle b_aibjcjal_ibcl
i_aibjcjal_ibcl: do i = n0i, n1i
if (i == l) cycle i_aibjcjal_ibcl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1j .ge. nn0j).and. (nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcjal_ibcl(t2, &
 nocc, nactive, i, b, c, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjcjal_ibcl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjcjal_ibcl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjal_ibcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjal_ibcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjal_ibcl
end do j_aibjcjal_ibcl
end do i_aibjcjal_ibcl
end do b_aibjcjal_ibcl
end do c_aibjcjal_ibcl
end do l_aibjcjal_ibcl
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjcjal_ibjcl: do l = n0l, n1l
c_aibjcjal_ibjcl: do c = n0c, n1c
b_aibjcjal_ibjcl: do b = n0b, n1b
if (b == c) cycle b_aibjcjal_ibjcl
j0 = max(l + 1, n0jk)
j_aibjcjal_ibjcl: do j = j0, n1jk
if (j == l) cycle j_aibjcjal_ibjcl
i0 = max(j + 1, n0i)
i_aibjcjal_ibjcl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjcjal_ibjcl
nn0a = max(b + 1, n0ad)
nn1a = min(c - 1, n1ad)
if ((nn1a .ge. nn0a))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcjal_ibjcl(t2, &
 nocc, nactive, i, b, j, c, l)
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjal_ibjcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjal_ibjcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do a_aibjcjal_ibjcl
end do i_aibjcjal_ibjcl
end do j_aibjcjal_ibjcl
end do b_aibjcjal_ibjcl
end do c_aibjcjal_ibjcl
end do l_aibjcjal_ibjcl
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjcjal_aibcl: do l = n0l, n1l
c_aibjcjal_aibcl: do c = n0c, n1c
b_aibjcjal_aibcl: do b = n0b, n1b
if (b == c) cycle b_aibjcjal_aibcl
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjal_aibcl: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjal_aibcl
i_aibjcjal_aibcl: do i = n0i, n1i
if (i == l) cycle i_aibjcjal_aibcl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcjal_aibcl(t2, &
 nocc, nactive, a, i, b, c, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjcjal_aibcl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjcjal_aibcl
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (a - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjal_aibcl
end do i_aibjcjal_aibcl
end do a_aibjcjal_aibcl
end do b_aibjcjal_aibcl
end do c_aibjcjal_aibcl
end do l_aibjcjal_aibcl
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, l == i
! No equalities independent of the above can hold.
!
d_aibjbkdi_ajkd: do d = n0d, n1d
k_aibjbkdi_ajkd: do k = n0k, n1k
j_aibjbkdi_ajkd: do j = n0j, n1j
if (j == k) cycle j_aibjbkdi_ajkd
a_aibjbkdi_ajkd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdi_ajkd
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b).and. (nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbkdi_ajkd(t2, &
 nocc, nactive, a, j, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdi_ajkd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdi_ajkd
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjbkdi_ajkd: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjbkdi_ajkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdi_ajkd
end do b_aibjbkdi_ajkd
end do a_aibjbkdi_ajkd
end do j_aibjbkdi_ajkd
end do k_aibjbkdi_ajkd
end do d_aibjbkdi_ajkd
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, l == i
! No equalities independent of the above can hold.
!
d_aibjbkdi_aijkd: do d = n0d, n1d
k_aibjbkdi_aijkd: do k = n0k, n1k
j_aibjbkdi_aijkd: do j = n0j, n1j
if (j == k) cycle j_aibjbkdi_aijkd
a_aibjbkdi_aijkd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdi_aijkd
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjbkdi_aijkd: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjbkdi_aijkd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbkdi_aijkd(t2, &
 nocc, nactive, a, i, j, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdi_aijkd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdi_aijkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbkdi_aijkd
end do i_aibjbkdi_aijkd
end do a_aibjbkdi_aijkd
end do j_aibjbkdi_aijkd
end do k_aibjbkdi_aijkd
end do d_aibjbkdi_aijkd
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, l == i
! No equalities independent of the above can hold.
!
d_aibjbkdi_abjkd: do d = n0d, n1d
k_aibjbkdi_abjkd: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdi_abjkd: do b = b0, n1bc
if (b == d) cycle b_aibjbkdi_abjkd
j_aibjbkdi_abjkd: do j = n0j, n1j
if (j == k) cycle j_aibjbkdi_abjkd
a0 = max(b + 1, n0a)
a_aibjbkdi_abjkd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdi_abjkd
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbkdi_abjkd(t2, &
 nocc, nactive, a, b, j, k, d)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjbkdi_abjkd: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjbkdi_abjkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdi_abjkd
end do a_aibjbkdi_abjkd
end do j_aibjbkdi_abjkd
end do b_aibjbkdi_abjkd
end do k_aibjbkdi_abjkd
end do d_aibjbkdi_abjkd
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, l
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
d_aibjbidl_ajdl: do d = n0d, n1d
l_aibjbidl_ajdl: do l = n0l, n1l
j_aibjbidl_ajdl: do j = n0j, n1j
if (j == l) cycle j_aibjbidl_ajdl
a_aibjbidl_ajdl: do a = n0a, n1a
if (a == d) cycle a_aibjbidl_ajdl
nn0i = max(j + 1, l + 1, n0ik)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidl_ajdl(t2, &
 nocc, nactive, a, j, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidl_ajdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidl_ajdl
i0 = max(j + 1, l + 1, n0ik)
i_aibjbidl_ajdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbidl_ajdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidl_ajdl
end do b_aibjbidl_ajdl
end do a_aibjbidl_ajdl
end do j_aibjbidl_ajdl
end do l_aibjbidl_ajdl
end do d_aibjbidl_ajdl
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, l
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
d_aibjbidl_aijdl: do d = n0d, n1d
l_aibjbidl_aijdl: do l = n0l, n1l
j_aibjbidl_aijdl: do j = n0j, n1j
if (j == l) cycle j_aibjbidl_aijdl
a_aibjbidl_aijdl: do a = n0a, n1a
if (a == d) cycle a_aibjbidl_aijdl
i0 = max(j + 1, l + 1, n0ik)
i_aibjbidl_aijdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbidl_aijdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidl_aijdl(t2, &
 nocc, nactive, a, i, j, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbidl_aijdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbidl_aijdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbidl_aijdl
end do i_aibjbidl_aijdl
end do a_aibjbidl_aijdl
end do j_aibjbidl_aijdl
end do l_aibjbidl_aijdl
end do d_aibjbidl_aijdl
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, l
! Equalities: c == b, k == i
! No equalities independent of the above can hold.
!
d_aibjbidl_abjdl: do d = n0d, n1d
l_aibjbidl_abjdl: do l = n0l, n1l
b0 = max(d + 1, n0bc)
b_aibjbidl_abjdl: do b = b0, n1bc
if (b == d) cycle b_aibjbidl_abjdl
j_aibjbidl_abjdl: do j = n0j, n1j
if (j == l) cycle j_aibjbidl_abjdl
a0 = max(b + 1, n0a)
a_aibjbidl_abjdl: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidl_abjdl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbidl_abjdl(t2, &
 nocc, nactive, a, b, j, d, l)
i0 = max(j + 1, l + 1, n0ik)
i_aibjbidl_abjdl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbidl_abjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidl_abjdl
end do a_aibjbidl_abjdl
end do j_aibjbidl_abjdl
end do b_aibjbidl_abjdl
end do l_aibjbidl_abjdl
end do d_aibjbidl_abjdl
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj_aikd: do d = n0d, n1d
k_aibjbkdj_aikd: do k = n0k, n1k
a_aibjbkdj_aikd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdj_aikd
i_aibjbkdj_aikd: do i = n0i, n1i
if (i == k) cycle i_aibjbkdj_aikd
nn1j = min(i - 1, k - 1, n1jl)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1j .ge. n0jl).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbkdj_aikd(t2, &
 nocc, nactive, a, i, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdj_aikd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdj_aikd
j1 = min(i - 1, k - 1, n1jl)
j_aibjbkdj_aikd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjbkdj_aikd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbkdj_aikd
end do b_aibjbkdj_aikd
end do i_aibjbkdj_aikd
end do a_aibjbkdj_aikd
end do k_aibjbkdj_aikd
end do d_aibjbkdj_aikd
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj_aijkd: do d = n0d, n1d
k_aibjbkdj_aijkd: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aibjbkdj_aijkd: do j = n0jl, j1
if (j == k) cycle j_aibjbkdj_aijkd
a_aibjbkdj_aijkd: do a = n0a, n1a
if (a == d) cycle a_aibjbkdj_aijkd
i0 = max(j + 1, n0i)
i_aibjbkdj_aijkd: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjbkdj_aijkd
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbkdj_aijkd(t2, &
 nocc, nactive, a, i, j, k, d)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbkdj_aijkd: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbkdj_aijkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbkdj_aijkd
end do i_aibjbkdj_aijkd
end do a_aibjbkdj_aijkd
end do j_aibjbkdj_aijkd
end do k_aibjbkdj_aijkd
end do d_aibjbkdj_aijkd
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj_aibkd: do d = n0d, n1d
k_aibjbkdj_aibkd: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdj_aibkd: do b = b0, n1bc
if (b == d) cycle b_aibjbkdj_aibkd
a0 = max(b + 1, n0a)
a_aibjbkdj_aibkd: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdj_aibkd
i_aibjbkdj_aibkd: do i = n0i, n1i
if (i == k) cycle i_aibjbkdj_aibkd
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbkdj_aibkd(t2, &
 nocc, nactive, a, i, b, k, d)
j1 = min(i - 1, k - 1, n1jl)
j_aibjbkdj_aibkd: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjbkdj_aibkd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (k - 1) * (k - 2)) / 2 + (d - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbkdj_aibkd
end do i_aibjbkdj_aibkd
end do a_aibjbkdj_aibkd
end do b_aibjbkdj_aibkd
end do k_aibjbkdj_aibkd
end do d_aibjbkdj_aibkd
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, l
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdl_aidl: do d = n0d, n1d
l_aibjbjdl_aidl: do l = n0l, n1l
a_aibjbjdl_aidl: do a = n0a, n1a
if (a == d) cycle a_aibjbjdl_aidl
i_aibjbjdl_aidl: do i = n0i, n1i
if (i == l) cycle i_aibjbjdl_aidl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1j .ge. nn0j).and. (nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbjdl_aidl(t2, &
 nocc, nactive, a, i, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdl_aidl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdl_aidl
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjbjdl_aidl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjbjdl_aidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdl_aidl
end do b_aibjbjdl_aidl
end do i_aibjbjdl_aidl
end do a_aibjbjdl_aidl
end do l_aibjbjdl_aidl
end do d_aibjbjdl_aidl
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, l
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdl_aijdl: do d = n0d, n1d
l_aibjbjdl_aijdl: do l = n0l, n1l
j0 = max(l + 1, n0jk)
j_aibjbjdl_aijdl: do j = j0, n1jk
if (j == l) cycle j_aibjbjdl_aijdl
a_aibjbjdl_aijdl: do a = n0a, n1a
if (a == d) cycle a_aibjbjdl_aijdl
i0 = max(j + 1, n0i)
i_aibjbjdl_aijdl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjbjdl_aijdl
nn0b = max(d + 1, n0bc)
nn1b = min(a - 1, n1bc)
if ((nn1b .ge. nn0b))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbjdl_aijdl(t2, &
 nocc, nactive, a, i, j, d, l)
b0 = max(d + 1, n0bc)
b1 = min(a - 1, n1bc)
b_aibjbjdl_aijdl: do b = b0, b1
if (b == a .or. b == d) cycle b_aibjbjdl_aijdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjbjdl_aijdl
end do i_aibjbjdl_aijdl
end do a_aibjbjdl_aijdl
end do j_aibjbjdl_aijdl
end do l_aibjbjdl_aijdl
end do d_aibjbjdl_aijdl
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, l
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdl_aibdl: do d = n0d, n1d
l_aibjbjdl_aibdl: do l = n0l, n1l
b0 = max(d + 1, n0bc)
b_aibjbjdl_aibdl: do b = b0, n1bc
if (b == d) cycle b_aibjbjdl_aibdl
a0 = max(b + 1, n0a)
a_aibjbjdl_aibdl: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdl_aibdl
i_aibjbjdl_aibdl: do i = n0i, n1i
if (i == l) cycle i_aibjbjdl_aibdl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjbjdl_aibdl(t2, &
 nocc, nactive, a, i, b, d, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjbjdl_aibdl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjbjdl_aibdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (d - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjbjdl_aibdl
end do i_aibjbjdl_aibdl
end do a_aibjbjdl_aibdl
end do b_aibjbjdl_aibdl
end do l_aibjbjdl_aibdl
end do d_aibjbjdl_aibdl
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, l == i
! No equalities independent of the above can hold.
!
c_aibjckbi_ajck: do c = n0c, n1c
k_aibjckbi_ajck: do k = n0k, n1k
j_aibjckbi_ajck: do j = n0j, n1j
if (j == k) cycle j_aibjckbi_ajck
a_aibjckbi_ajck: do a = n0a, n1a
if (a == c) cycle a_aibjckbi_ajck
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd).and. (nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckbi_ajck(t2, &
 nocc, nactive, a, j, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbi_ajck: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbi_ajck
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjckbi_ajck: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjckbi_ajck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbi_ajck
end do b_aibjckbi_ajck
end do a_aibjckbi_ajck
end do j_aibjckbi_ajck
end do k_aibjckbi_ajck
end do c_aibjckbi_ajck
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, l == i
! No equalities independent of the above can hold.
!
c_aibjckbi_aijck: do c = n0c, n1c
k_aibjckbi_aijck: do k = n0k, n1k
j_aibjckbi_aijck: do j = n0j, n1j
if (j == k) cycle j_aibjckbi_aijck
a_aibjckbi_aijck: do a = n0a, n1a
if (a == c) cycle a_aibjckbi_aijck
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjckbi_aijck: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjckbi_aijck
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckbi_aijck(t2, &
 nocc, nactive, a, i, j, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbi_aijck: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbi_aijck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjckbi_aijck
end do i_aibjckbi_aijck
end do a_aibjckbi_aijck
end do j_aibjckbi_aijck
end do k_aibjckbi_aijck
end do c_aibjckbi_aijck
!
! Elementary loop  20
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, l == i
! No equalities independent of the above can hold.
!
c_aibjckbi_abjck: do c = n0c, n1c
k_aibjckbi_abjck: do k = n0k, n1k
b1 = min(c - 1, n1bd)
b_aibjckbi_abjck: do b = n0bd, b1
if (b == c) cycle b_aibjckbi_abjck
j_aibjckbi_abjck: do j = n0j, n1j
if (j == k) cycle j_aibjckbi_abjck
a0 = max(b + 1, n0a)
a_aibjckbi_abjck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbi_abjck
nn0i = max(j + 1, n0il)
nn1i = min(k - 1, n1il)
if ((nn1i .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckbi_abjck(t2, &
 nocc, nactive, a, b, j, c, k)
i0 = max(j + 1, n0il)
i1 = min(k - 1, n1il)
i_aibjckbi_abjck: do i = i0, i1
if (i == j .or. i == k) cycle i_aibjckbi_abjck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + i
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbi_abjck
end do a_aibjckbi_abjck
end do j_aibjckbi_abjck
end do b_aibjckbi_abjck
end do k_aibjckbi_abjck
end do c_aibjckbi_abjck
!
! Elementary loop  21
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl_ajcl: do l = n0l, n1l
c_aibjcibl_ajcl: do c = n0c, n1c
j_aibjcibl_ajcl: do j = n0j, n1j
if (j == l) cycle j_aibjcibl_ajcl
a_aibjcibl_ajcl: do a = n0a, n1a
if (a == c) cycle a_aibjcibl_ajcl
nn0i = max(j + 1, l + 1, n0ik)
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibl_ajcl(t2, &
 nocc, nactive, a, j, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibl_ajcl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibl_ajcl
i0 = max(j + 1, l + 1, n0ik)
i_aibjcibl_ajcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcibl_ajcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibl_ajcl
end do b_aibjcibl_ajcl
end do a_aibjcibl_ajcl
end do j_aibjcibl_ajcl
end do c_aibjcibl_ajcl
end do l_aibjcibl_ajcl
!
! Elementary loop  22
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl_aijcl: do l = n0l, n1l
c_aibjcibl_aijcl: do c = n0c, n1c
j_aibjcibl_aijcl: do j = n0j, n1j
if (j == l) cycle j_aibjcibl_aijcl
a_aibjcibl_aijcl: do a = n0a, n1a
if (a == c) cycle a_aibjcibl_aijcl
i0 = max(j + 1, l + 1, n0ik)
i_aibjcibl_aijcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcibl_aijcl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibl_aijcl(t2, &
 nocc, nactive, a, i, j, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcibl_aijcl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcibl_aijcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcibl_aijcl
end do i_aibjcibl_aijcl
end do a_aibjcibl_aijcl
end do j_aibjcibl_aijcl
end do c_aibjcibl_aijcl
end do l_aibjcibl_aijcl
!
! Elementary loop  23
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl_abjcl: do l = n0l, n1l
c_aibjcibl_abjcl: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibl_abjcl: do b = n0bd, b1
if (b == c) cycle b_aibjcibl_abjcl
j_aibjcibl_abjcl: do j = n0j, n1j
if (j == l) cycle j_aibjcibl_abjcl
a0 = max(b + 1, n0a)
a_aibjcibl_abjcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibl_abjcl
nn0i = max(j + 1, l + 1, n0ik)
if ((n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcibl_abjcl(t2, &
 nocc, nactive, a, b, j, c, l)
i0 = max(j + 1, l + 1, n0ik)
i_aibjcibl_abjcl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjcibl_abjcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibl_abjcl
end do a_aibjcibl_abjcl
end do j_aibjcibl_abjcl
end do b_aibjcibl_abjcl
end do c_aibjcibl_abjcl
end do l_aibjcibl_abjcl
!
! Elementary loop  24
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj_aick: do c = n0c, n1c
k_aibjckbj_aick: do k = n0k, n1k
a_aibjckbj_aick: do a = n0a, n1a
if (a == c) cycle a_aibjckbj_aick
i_aibjckbj_aick: do i = n0i, n1i
if (i == k) cycle i_aibjckbj_aick
nn1j = min(i - 1, k - 1, n1jl)
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1j .ge. n0jl).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckbj_aick(t2, &
 nocc, nactive, a, i, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbj_aick: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbj_aick
j1 = min(i - 1, k - 1, n1jl)
j_aibjckbj_aick: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckbj_aick
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckbj_aick
end do b_aibjckbj_aick
end do i_aibjckbj_aick
end do a_aibjckbj_aick
end do k_aibjckbj_aick
end do c_aibjckbj_aick
!
! Elementary loop  25
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj_aijck: do c = n0c, n1c
k_aibjckbj_aijck: do k = n0k, n1k
j1 = min(k - 1, n1jl)
j_aibjckbj_aijck: do j = n0jl, j1
if (j == k) cycle j_aibjckbj_aijck
a_aibjckbj_aijck: do a = n0a, n1a
if (a == c) cycle a_aibjckbj_aijck
i0 = max(j + 1, n0i)
i_aibjckbj_aijck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aibjckbj_aijck
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckbj_aijck(t2, &
 nocc, nactive, a, i, j, c, k)
b1 = min(a - 1, c - 1, n1bd)
b_aibjckbj_aijck: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjckbj_aijck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjckbj_aijck
end do i_aibjckbj_aijck
end do a_aibjckbj_aijck
end do j_aibjckbj_aijck
end do k_aibjckbj_aijck
end do c_aibjckbj_aijck
!
! Elementary loop  26
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj_aibck: do c = n0c, n1c
k_aibjckbj_aibck: do k = n0k, n1k
b1 = min(c - 1, n1bd)
b_aibjckbj_aibck: do b = n0bd, b1
if (b == c) cycle b_aibjckbj_aibck
a0 = max(b + 1, n0a)
a_aibjckbj_aibck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbj_aibck
i_aibjckbj_aibck: do i = n0i, n1i
if (i == k) cycle i_aibjckbj_aibck
nn1j = min(i - 1, k - 1, n1jl)
if ((nn1j .ge. n0jl))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjckbj_aibck(t2, &
 nocc, nactive, a, i, b, c, k)
j1 = min(i - 1, k - 1, n1jl)
j_aibjckbj_aibck: do j = n0jl, j1
if (j == i .or. j == k) cycle j_aibjckbj_aibck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (k - 1) * (k - 2)) / 2 + (b - nvirt0) * (k - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjckbj_aibck
end do i_aibjckbj_aibck
end do a_aibjckbj_aibck
end do b_aibjckbj_aibck
end do k_aibjckbj_aibck
end do c_aibjckbj_aibck
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl_aicl: do l = n0l, n1l
c_aibjcjbl_aicl: do c = n0c, n1c
a_aibjcjbl_aicl: do a = n0a, n1a
if (a == c) cycle a_aibjcjbl_aicl
i_aibjcjbl_aicl: do i = n0i, n1i
if (i == l) cycle i_aibjcjbl_aicl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1j .ge. nn0j).and. (nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcjbl_aicl(t2, &
 nocc, nactive, a, i, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbl_aicl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbl_aicl
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjcjbl_aicl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjcjbl_aicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbl_aicl
end do b_aibjcjbl_aicl
end do i_aibjcjbl_aicl
end do a_aibjcjbl_aicl
end do c_aibjcjbl_aicl
end do l_aibjcjbl_aicl
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl_aijcl: do l = n0l, n1l
c_aibjcjbl_aijcl: do c = n0c, n1c
j0 = max(l + 1, n0jk)
j_aibjcjbl_aijcl: do j = j0, n1jk
if (j == l) cycle j_aibjcjbl_aijcl
a_aibjcjbl_aijcl: do a = n0a, n1a
if (a == c) cycle a_aibjcjbl_aijcl
i0 = max(j + 1, n0i)
i_aibjcjbl_aijcl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aibjcjbl_aijcl
nn1b = min(a - 1, c - 1, n1bd)
if ((nn1b .ge. n0bd))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcjbl_aijcl(t2, &
 nocc, nactive, a, i, j, c, l)
b1 = min(a - 1, c - 1, n1bd)
b_aibjcjbl_aijcl: do b = n0bd, b1
if (b == a .or. b == c) cycle b_aibjcjbl_aijcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do b_aibjcjbl_aijcl
end do i_aibjcjbl_aijcl
end do a_aibjcjbl_aijcl
end do j_aibjcjbl_aijcl
end do c_aibjcjbl_aijcl
end do l_aibjcjbl_aijcl
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl_aibcl: do l = n0l, n1l
c_aibjcjbl_aibcl: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbl_aibcl: do b = n0bd, b1
if (b == c) cycle b_aibjcjbl_aibcl
a0 = max(b + 1, n0a)
a_aibjcjbl_aibcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbl_aibcl
i_aibjcjbl_aibcl: do i = n0i, n1i
if (i == l) cycle i_aibjcjbl_aibcl
nn0j = max(l + 1, n0jk)
nn1j = min(i - 1, n1jk)
if ((nn1j .ge. nn0j))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcjbl_aibcl(t2, &
 nocc, nactive, a, i, b, c, l)
j0 = max(l + 1, n0jk)
j1 = min(i - 1, n1jk)
j_aibjcjbl_aibcl: do j = j0, j1
if (j == i .or. j == l) cycle j_aibjcjbl_aibcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (j - 1) * (j - 2)) / 2 + (b - nvirt0) * (j - 1) + l
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do j_aibjcjbl_aibcl
end do i_aibjcjbl_aibcl
end do a_aibjcjbl_aibcl
end do b_aibjcjbl_aibcl
end do c_aibjcjbl_aibcl
end do l_aibjcjbl_aibcl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidj_abcd: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidj_abcd: do c = c0, n1c
if (c == d) cycle c_aibjcidj_abcd
b_aibjcidj_abcd: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidj_abcd
a0 = max(b + 1, n0a)
a_aibjcidj_abcd: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidj_abcd
nn0i = max(n0jl + 1, n0ik)
if ((n1jl .ge. n0jl).and. (n1ik .ge. nn0i))jac_ibra_iket = eom_ccsd_22_tripletpp_trans_aibjcidj_abcd(t2, &
 nocc, nactive, a, b, c, d)
j_aibjcidj_abcd: do j = n0jl, n1jl
i0 = max(j + 1, n0ik)
i_aibjcidj_abcd: do i = i0, n1ik
if (i == j) cycle i_aibjcidj_abcd
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
ibra = braoffset + &
(nocc * (nocc - 1) * (a - nvirt0) * (a - 1 - nvirt0)) / 4 + &
((a - nvirt0) * (i - 1) * (i - 2)) / 2 + (b - nvirt0) * (i - 1) + j
iket = ketoffset + &
(nocc * (nocc - 1) * (c - nvirt0) * (c - 1 - nvirt0)) / 4 + &
((c - nvirt0) * (i - 1) * (i - 2)) / 2 + (d - nvirt0) * (i - 1) + j
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidj_abcd
end do j_aibjcidj_abcd
end do a_aibjcidj_abcd
end do b_aibjcidj_abcd
end do c_aibjcidj_abcd
end do d_aibjcidj_abcd
end subroutine ccjac_22_tripletpp_dav_part2
end module ccjac_block_22_tripletpp_dav_part2
