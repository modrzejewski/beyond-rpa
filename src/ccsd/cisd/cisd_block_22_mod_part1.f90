module cisd_block_22_mod_part1
use cisd22
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2014-11-10 21:36:57 UTC.
!
contains
subroutine cisd_block_22_part1(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, &
 n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0) 
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
real(F64), dimension(:,:), intent(inout) :: hci
integer :: a, b, c, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: a0, a1, b0, b1, c0, i0, i1, j0, j1, k0
integer :: n0abc, n0abcd, n0ac, n0acd, n0ad
integer :: n0bc, n0bd, n0ij, n0ik, n0il
integer :: n0jk, n0jl, n0kl
integer :: n1abc, n1abcd, n1ac, n1acd, n1ad
integer :: n1bc, n1bd, n1ij, n1ik, n1il
integer :: n1jk, n1jl, n1kl
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
n0abcd = max(n0a, n0b, n0c, n0d)
n0ac = max(n0a, n0c)
n0acd = max(n0a, n0c, n0d)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bd = max(n0b, n0d)
n0ij = max(n0i, n0j)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1abc = min(n1a, n1b, n1c)
n1abcd = min(n1a, n1b, n1c, n1d)
n1ac = min(n1a, n1c)
n1acd = min(n1a, n1c, n1d)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
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
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: c == a, d == b
! No equalities independent of the above can hold.
!
l_aibjakbl: do l = n0l, n1l
k_aibjakbl: do k = n0k, n1k
if (k == l) cycle k_aibjakbl
b_aibjakbl: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjakbl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjakbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbl: do a = a0, n1ac
if (a == b) cycle a_aibjakbl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjakbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjakbl(i, j, k, l)
end do i_aibjakbl
end do a_aibjakbl
end do j_aibjakbl
end do b_aibjakbl
end do k_aibjakbl
end do l_aibjakbl
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, k
! Equalities: c == a, l == i
! No equalities independent of the above can hold.
!
d_aibjakdi: do d = n0d, n1d
k_aibjakdi: do k = n0k, n1k
b_aibjakdi: do b = n0b, n1b
if (b == d) cycle b_aibjakdi
j_aibjakdi: do j = n0j, n1j
if (j == k) cycle j_aibjakdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjakdi(b, j, k, d)
end do i_aibjakdi
end do a_aibjakdi
end do j_aibjakdi
end do b_aibjakdi
end do k_aibjakdi
end do d_aibjakdi
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i, k
! Equalities: c == a, l == j
! No equalities independent of the above can hold.
!
d_aibjakdj: do d = n0d, n1d
k_aibjakdj: do k = n0k, n1k
b_aibjakdj: do b = n0b, n1b
if (b == d) cycle b_aibjakdj
j_aibjakdj: do j = n0jl, n1jl
if (j == k) cycle j_aibjakdj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjakdj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjakdj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakdj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjakdj(i, b, k, d)
end do i_aibjakdj
end do a_aibjakdj
end do j_aibjakdj
end do b_aibjakdj
end do k_aibjakdj
end do d_aibjakdj
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j, l
! Equalities: c == a, k == i
! No equalities independent of the above can hold.
!
d_aibjaidl: do d = n0d, n1d
l_aibjaidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibjaidl: do b = n0b, n1b
if (b == d) cycle b_aibjaidl
j_aibjaidl: do j = n0j, n1j
if (j == l) cycle j_aibjaidl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidl
i_aibjaidl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjaidl(b, j, d, l)
end do i_aibjaidl
end do a_aibjaidl
end do j_aibjaidl
end do b_aibjaidl
end do l_aibjaidl
end do d_aibjaidl
!
! Elementary loop  5
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, l == i
! No equalities independent of the above can hold.
!
d_aibjbkdi: do d = n0d, n1d
k_aibjbkdi: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdi: do b = b0, n1bc
if (b == d) cycle b_aibjbkdi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdi: do j = n0j, n1j
if (j == k) cycle j_aibjbkdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdi
i_aibjbkdi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjbkdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbkdi(a, j, k, d)
end do i_aibjbkdi
end do a_aibjbkdi
end do j_aibjbkdi
end do b_aibjbkdi
end do k_aibjbkdi
end do d_aibjbkdi
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj: do d = n0d, n1d
k_aibjbkdj: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibjbkdj: do b = b0, n1bc
if (b == d) cycle b_aibjbkdj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkdj: do j = n0jl, n1jl
if (j == k) cycle j_aibjbkdj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdj
i_aibjbkdj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjbkdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbkdj(a, i, k, d)
end do i_aibjbkdj
end do a_aibjbkdj
end do j_aibjbkdj
end do b_aibjbkdj
end do k_aibjbkdj
end do d_aibjbkdj
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, l
! Equalities: c == b, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdl: do d = n0d, n1d
l_aibjbjdl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(d + 1, n0bc)
b_aibjbjdl: do b = b0, n1bc
if (b == d) cycle b_aibjbjdl
j_aibjbjdl: do j = n0jk, n1jk
if (j == l) cycle j_aibjbjdl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdl: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdl
i_aibjbjdl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjbjdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbjdl(a, i, d, l)
end do i_aibjbjdl
end do a_aibjbjdl
end do j_aibjbjdl
end do b_aibjbjdl
end do l_aibjbjdl
end do d_aibjbjdl
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, l == i
! No equalities independent of the above can hold.
!
c_aibjckai: do c = n0c, n1c
k_aibjckai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibjckai: do b = n0b, n1b
if (b == c) cycle b_aibjckai
j_aibjckai: do j = n0j, n1j
if (j == k) cycle j_aibjckai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjckai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjckai
i_aibjckai: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjckai(b, j, c, k)
end do i_aibjckai
end do a_aibjckai
end do j_aibjckai
end do b_aibjckai
end do k_aibjckai
end do c_aibjckai
!
! Elementary loop  9
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjcial: do l = n0l, n1l
c_aibjcial: do c = n0c, n1c
b_aibjcial: do b = n0b, n1b
if (b == c) cycle b_aibjcial
j_aibjcial: do j = n0j, n1j
if (j == l) cycle j_aibjcial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcial: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcial: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcial(b, j, c, l)
end do i_aibjcial
end do a_aibjcial
end do j_aibjcial
end do b_aibjcial
end do c_aibjcial
end do l_aibjcial
!
! Elementary loop  10
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjcjal: do l = n0l, n1l
c_aibjcjal: do c = n0c, n1c
b_aibjcjal: do b = n0b, n1b
if (b == c) cycle b_aibjcjal
j_aibjcjal: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjal: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjal: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcjal(i, b, c, l)
end do i_aibjcjal
end do a_aibjcjal
end do j_aibjcjal
end do b_aibjcjal
end do c_aibjcjal
end do l_aibjcjal
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj: do c = n0c, n1c
k_aibjckbj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b1 = min(c - 1, n1bd)
b_aibjckbj: do b = n0bd, b1
if (b == c) cycle b_aibjckbj
j_aibjckbj: do j = n0jl, n1jl
if (j == k) cycle j_aibjckbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbj
i_aibjckbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjckbj(a, i, c, k)
end do i_aibjckbj
end do a_aibjckbj
end do j_aibjckbj
end do b_aibjckbj
end do k_aibjckbj
end do c_aibjckbj
!
! Elementary loop  12
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl: do l = n0l, n1l
c_aibjcibl: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcibl: do b = n0bd, b1
if (b == c) cycle b_aibjcibl
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcibl: do j = n0j, n1j
if (j == l) cycle j_aibjcibl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcibl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcibl
i_aibjcibl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcibl(a, j, c, l)
end do i_aibjcibl
end do a_aibjcibl
end do j_aibjcibl
end do b_aibjcibl
end do c_aibjcibl
end do l_aibjcibl
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl: do l = n0l, n1l
c_aibjcjbl: do c = n0c, n1c
b1 = min(c - 1, n1bd)
b_aibjcjbl: do b = n0bd, b1
if (b == c) cycle b_aibjcjbl
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjcjbl: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjbl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjbl
i_aibjcjbl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcjbl(a, i, c, l)
end do i_aibjcjbl
end do a_aibjcjbl
end do j_aibjcjbl
end do b_aibjcjbl
end do c_aibjcjbl
end do l_aibjcjbl
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcjdi: do c = c0, n1c
if (c == d) cycle c_aibjcjdi
b_aibjcjdi: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcjdi
j_aibjcjdi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdi
i_aibjcjdi: do i = n0il, n1il
if (i == j) cycle i_aibjcjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcjdi(a, b, c, d)
end do i_aibjcjdi
end do a_aibjcjdi
end do j_aibjcjdi
end do b_aibjcjdi
end do c_aibjcjdi
end do d_aibjcjdi
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aibjcidj: do c = c0, n1c
if (c == d) cycle c_aibjcidj
b_aibjcidj: do b = n0b, n1b
if (b == c .or. b == d) cycle b_aibjcidj
j_aibjcidj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidj
i_aibjcidj: do i = n0ik, n1ik
if (i == j) cycle i_aibjcidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcidj(a, b, c, d)
end do i_aibjcidj
end do a_aibjcidj
end do j_aibjcidj
end do b_aibjcidj
end do c_aibjcidj
end do d_aibjcidj
!
! Elementary loop  16
! --------------------
! Free virtual indices: a
! Free occupied indices: i, j, k, l
! Equalities: b == a, c == a, d == a
! No equalities independent of the above can hold.
!
l_aiajakal: do l = n0l, n1l
k0 = max(l + 1, n0k)
k_aiajakal: do k = k0, n1k
if (k == l) cycle k_aiajakal
j_aiajakal: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aiajakal
a_aiajakal: do a = n0abcd, n1abcd
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakal: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajakal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajakal(i, j, k, l)
end do i_aiajakal
end do a_aiajakal
end do j_aiajakal
end do k_aiajakal
end do l_aiajakal
!
! Elementary loop  17
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == a, l == i
! No equalities independent of the above can hold.
!
k_aibjakai: do k = n0k, n1k
b_aibjakai: do b = n0b, n1b
j_aibjakai: do j = n0j, n1j
if (j == k) cycle j_aibjakai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakai: do a = a0, n1acd
if (a == b) cycle a_aibjakai
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(k - 1, n1il)
i_aibjakai: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjakai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjakai(a, b, j, k)
end do i_aibjakai
end do a_aibjakai
end do j_aibjakai
end do b_aibjakai
end do k_aibjakai
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == a, l == j
! No equalities independent of the above can hold.
!
k_aibjakaj: do k = n0k, n1k
b_aibjakaj: do b = n0b, n1b
j1 = min(k - 1, n1jl)
j_aibjakaj: do j = n0jl, j1
if (j == k) cycle j_aibjakaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjakaj: do a = a0, n1acd
if (a == b) cycle a_aibjakaj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjakaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjakaj(a, i, b, k)
end do i_aibjakaj
end do a_aibjakaj
end do j_aibjakaj
end do b_aibjakaj
end do k_aibjakaj
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjaial: do l = n0l, n1l
b_aibjaial: do b = n0b, n1b
j_aibjaial: do j = n0j, n1j
if (j == l) cycle j_aibjaial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjaial: do a = a0, n1acd
if (a == b) cycle a_aibjaial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(l + 1, n0ik)
i_aibjaial: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjaial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjaial(a, b, j, l)
end do i_aibjaial
end do a_aibjaial
end do j_aibjaial
end do b_aibjaial
end do l_aibjaial
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjajal: do l = n0l, n1l
b_aibjajal: do b = n0b, n1b
j0 = max(l + 1, n0jk)
j_aibjajal: do j = j0, n1jk
if (j == l) cycle j_aibjajal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0acd)
a_aibjajal: do a = a0, n1acd
if (a == b) cycle a_aibjajal
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjajal: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjajal(a, i, b, l)
end do i_aibjajal
end do a_aibjajal
end do j_aibjajal
end do b_aibjajal
end do l_aibjajal
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, k
! Equalities: b == a, c == a, l == i
! No equalities independent of the above can hold.
!
d_aiajakdi: do d = n0d, n1d
k_aiajakdi: do k = n0k, n1k
j_aiajakdi: do j = n0j, n1j
if (j == k) cycle j_aiajakdi
a0 = max(d + 1, n0abc)
a_aiajakdi: do a = a0, n1abc
if (a == d) cycle a_aiajakdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajakdi: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajakdi(a, j, k, d)
end do i_aiajakdi
end do a_aiajakdi
end do j_aiajakdi
end do k_aiajakdi
end do d_aiajakdi
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, k
! Equalities: b == a, c == a, l == j
! No equalities independent of the above can hold.
!
d_aiajakdj: do d = n0d, n1d
k_aiajakdj: do k = n0k, n1k
j_aiajakdj: do j = n0jl, n1jl
if (j == k) cycle j_aiajakdj
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(d + 1, n0abc)
a_aiajakdj: do a = a0, n1abc
if (a == d) cycle a_aiajakdj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajakdj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajakdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajakdj(a, i, k, d)
end do i_aiajakdj
end do a_aiajakdj
end do j_aiajakdj
end do k_aiajakdj
end do d_aiajakdj
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, d
! Free occupied indices: i, j, l
! Equalities: b == a, c == a, k == i
! No equalities independent of the above can hold.
!
d_aiajaidl: do d = n0d, n1d
l_aiajaidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajaidl: do j = n0j, n1j
if (j == l) cycle j_aiajaidl
a0 = max(d + 1, n0abc)
a_aiajaidl: do a = a0, n1abc
if (a == d) cycle a_aiajaidl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajaidl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajaidl(a, j, d, l)
end do i_aiajaidl
end do a_aiajaidl
end do j_aiajaidl
end do l_aiajaidl
end do d_aiajaidl
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, d
! Free occupied indices: j, i, l
! Equalities: b == a, c == a, k == j
! No equalities independent of the above can hold.
!
d_aiajajdl: do d = n0d, n1d
l_aiajajdl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
j_aiajajdl: do j = n0jk, n1jk
if (j == l) cycle j_aiajajdl
a0 = max(d + 1, n0abc)
a_aiajajdl: do a = a0, n1abc
if (a == d) cycle a_aiajajdl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajajdl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajajdl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajajdl(a, i, d, l)
end do i_aiajajdl
end do a_aiajajdl
end do j_aiajajdl
end do l_aiajajdl
end do d_aiajajdl
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: c == a, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjakbi: do k = n0k, n1k
b_aibjakbi: do b = n0bd, n1bd
j_aibjakbi: do j = n0j, n1j
if (j == k) cycle j_aibjakbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbi: do a = a0, n1ac
if (a == b) cycle a_aibjakbi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjakbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjakbi(i, b, j, k)
end do i_aibjakbi
end do a_aibjakbi
end do j_aibjakbi
end do b_aibjakbi
end do k_aibjakbi
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: c == a, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjakbj: do k = n0k, n1k
b_aibjakbj: do b = n0bd, n1bd
j_aibjakbj: do j = n0jl, n1jl
if (j == k) cycle j_aibjakbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbj: do a = a0, n1ac
if (a == b) cycle a_aibjakbj
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjakbj(a, i, b, j, k)
end do i_aibjakbj
end do a_aibjakbj
end do j_aibjakbj
end do b_aibjakbj
end do k_aibjakbj
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: c == a, d == b, l == k
! No equalities independent of the above can hold.
!
k_aibjakbk: do k = n0kl, n1kl
b_aibjakbk: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjakbk: do j = n0j, n1j
if (j == k) cycle j_aibjakbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjakbk: do a = a0, n1ac
if (a == b) cycle a_aibjakbk
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjakbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjakbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjakbk(i, j, k)
end do i_aibjakbk
end do a_aibjakbk
end do j_aibjakbk
end do b_aibjakbk
end do k_aibjakbk
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: c == a, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibiakbl: do l = n0l, n1l
k_aibiakbl: do k = n0k, n1k
if (k == l) cycle k_aibiakbl
b_aibiakbl: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibiakbl: do a = a0, n1ac
if (a == b) cycle a_aibiakbl
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakbl: do i = n0ij, n1ij
if (i == k .or. i == l) cycle i_aibiakbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibiakbl(i, k, l)
end do i_aibiakbl
end do a_aibiakbl
end do b_aibiakbl
end do k_aibiakbl
end do l_aibiakbl
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: c == a, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjaibl: do l = n0l, n1l
b_aibjaibl: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjaibl: do j = n0j, n1j
if (j == l) cycle j_aibjaibl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjaibl: do a = a0, n1ac
if (a == b) cycle a_aibjaibl
i_aibjaibl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjaibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjaibl(a, i, b, j, l)
end do i_aibjaibl
end do a_aibjaibl
end do j_aibjaibl
end do b_aibjaibl
end do l_aibjaibl
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, l
! Equalities: c == a, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjajbl: do l = n0l, n1l
b_aibjajbl: do b = n0bd, n1bd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjajbl: do j = n0jk, n1jk
if (j == l) cycle j_aibjajbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ac)
a_aibjajbl: do a = a0, n1ac
if (a == b) cycle a_aibjajbl
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajbl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjajbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjajbl(a, i, j, l)
end do i_aibjajbl
end do a_aibjajbl
end do j_aibjajbl
end do b_aibjajbl
end do l_aibjajbl
end subroutine cisd_block_22_part1
end module cisd_block_22_mod_part1
