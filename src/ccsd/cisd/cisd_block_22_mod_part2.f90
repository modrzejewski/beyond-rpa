module cisd_block_22_mod_part2
use cisd22
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2014-11-10 21:36:57 UTC.
!
contains
subroutine cisd_block_22_part2(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, &
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
integer :: a0, a1, b0, c0, i0, i1, j0, j1
integer :: n0ab, n0abd, n0ac, n0ad, n0bc
integer :: n0bcd, n0cd, n0ijk, n0ijl, n0ik
integer :: n0ikl, n0il, n0jk, n0jkl, n0jl
integer :: n1ab, n1abd, n1ac, n1ad, n1bc
integer :: n1bcd, n1cd, n1ijk, n1ijl, n1ik
integer :: n1ikl, n1il, n1jk, n1jkl, n1jl
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
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0cd = max(n0c, n0d)
n0ijk = max(n0i, n0j, n0k)
n0ijl = max(n0i, n0j, n0l)
n0ik = max(n0i, n0k)
n0ikl = max(n0i, n0k, n0l)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jkl = max(n0j, n0k, n0l)
n0jl = max(n0j, n0l)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1ac = min(n1a, n1c)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1cd = min(n1c, n1d)
n1ijk = min(n1i, n1j, n1k)
n1ijl = min(n1i, n1j, n1l)
n1ik = min(n1i, n1k)
n1ikl = min(n1i, n1k, n1l)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jkl = min(n1j, n1k, n1l)
n1jl = min(n1j, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, k
! Equalities: c == a, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibiakdi: do d = n0d, n1d
k_aibiakdi: do k = n0k, n1k
b_aibiakdi: do b = n0b, n1b
if (b == d) cycle b_aibiakdi
a0 = max(b + 1, d + 1, n0ac)
a_aibiakdi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiakdi
ck = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibiakdi: do i = n0ijl, n1ijl
if (i == k) cycle i_aibiakdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibiakdi(i, b, k, d)
end do i_aibiakdi
end do a_aibiakdi
end do b_aibiakdi
end do k_aibiakdi
end do d_aibiakdi
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjaidi: do d = n0d, n1d
b_aibjaidi: do b = n0b, n1b
if (b == d) cycle b_aibjaidi
j_aibjaidi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidi
i_aibjaidi: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjaidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjaidi(i, b, j, d)
end do i_aibjaidi
end do a_aibjaidi
end do j_aibjaidi
end do b_aibjaidi
end do d_aibjaidi
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjajdi: do d = n0d, n1d
b_aibjajdi: do b = n0b, n1b
if (b == d) cycle b_aibjajdi
j_aibjajdi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdi: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdi
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdi: do i = n0il, n1il
if (i == j) cycle i_aibjajdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjajdi(a, b, j, d)
end do i_aibjajdi
end do a_aibjajdi
end do j_aibjajdi
end do b_aibjajdi
end do d_aibjajdi
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: j, i
! Equalities: c == a, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjajdj: do d = n0d, n1d
b_aibjajdj: do b = n0b, n1b
if (b == d) cycle b_aibjajdj
j_aibjajdj: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjajdj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjajdj
ck = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjajdj: do i = n0i, n1i
if (i == j) cycle i_aibjajdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjajdj(i, b, j, d)
end do i_aibjajdj
end do a_aibjajdj
end do j_aibjajdj
end do b_aibjajdj
end do d_aibjajdj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, j
! Equalities: c == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjaidj: do d = n0d, n1d
b_aibjaidj: do b = n0b, n1b
if (b == d) cycle b_aibjaidj
j_aibjaidj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, d + 1, n0ac)
a_aibjaidj: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibjaidj
i_aibjaidj: do i = n0ik, n1ik
if (i == j) cycle i_aibjaidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjaidj(a, i, b, j, d)
end do i_aibjaidj
end do a_aibjaidj
end do j_aibjaidj
end do b_aibjaidj
end do d_aibjaidj
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, d
! Free occupied indices: i, l
! Equalities: c == a, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibiaidl: do d = n0d, n1d
l_aibiaidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b_aibiaidl: do b = n0b, n1b
if (b == d) cycle b_aibiaidl
a0 = max(b + 1, d + 1, n0ac)
a_aibiaidl: do a = a0, n1ac
if (a == b .or. a == d) cycle a_aibiaidl
i_aibiaidl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibiaidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibiaidl(i, b, d, l)
end do i_aibiaidl
end do a_aibiaidl
end do b_aibiaidl
end do l_aibiaidl
end do d_aibiaidl
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkbi: do k = n0k, n1k
b_aibjbkbi: do b = n0bcd, n1bcd
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjbkbi: do j = n0j, n1j
if (j == k) cycle j_aibjbkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbi: do a = a0, n1a
if (a == b) cycle a_aibjbkbi
i1 = min(k - 1, n1il)
i_aibjbkbi: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbkbi(a, b, j, k)
end do i_aibjbkbi
end do a_aibjbkbi
end do j_aibjbkbi
end do b_aibjbkbi
end do k_aibjbkbi
!
! Elementary loop  8
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k
! Equalities: c == b, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjbkbj: do k = n0k, n1k
b_aibjbkbj: do b = n0bcd, n1bcd
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aibjbkbj: do j = n0jl, j1
if (j == k) cycle j_aibjbkbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbj: do a = a0, n1a
if (a == b) cycle a_aibjbkbj
i_aibjbkbj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjbkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbkbj(a, i, b, k)
end do i_aibjbkbj
end do a_aibjbkbj
end do j_aibjbkbj
end do b_aibjbkbj
end do k_aibjbkbj
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, l
! Equalities: c == b, d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbibl: do l = n0l, n1l
b_aibjbibl: do b = n0bcd, n1bcd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjbibl: do j = n0j, n1j
if (j == l) cycle j_aibjbibl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbibl: do a = a0, n1a
if (a == b) cycle a_aibjbibl
i0 = max(l + 1, n0ik)
i_aibjbibl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aibjbibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbibl(a, b, j, l)
end do i_aibjbibl
end do a_aibjbibl
end do j_aibjbibl
end do b_aibjbibl
end do l_aibjbibl
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, l
! Equalities: c == b, d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjbjbl: do l = n0l, n1l
b_aibjbjbl: do b = n0bcd, n1bcd
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(l + 1, n0jk)
j_aibjbjbl: do j = j0, n1jk
if (j == l) cycle j_aibjbjbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjbl: do a = a0, n1a
if (a == b) cycle a_aibjbjbl
i_aibjbjbl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjbjbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbjbl(a, i, b, l)
end do i_aibjbjbl
end do a_aibjbjbl
end do j_aibjbjbl
end do b_aibjbjbl
end do l_aibjbjbl
!
! Elementary loop  11
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjci: do c = n0cd, n1cd
b_aibjcjci: do b = n0b, n1b
if (b == c) cycle b_aibjcjci
j_aibjcjci: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjci
i1 = min(j - 1, n1il)
i_aibjcjci: do i = n0il, i1
if (i == j) cycle i_aibjcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcjci(a, b, c)
end do i_aibjcjci
end do a_aibjcjci
end do j_aibjcjci
end do b_aibjcjci
end do c_aibjcjci
!
! Elementary loop  12
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj: do c = n0cd, n1cd
b_aibjcicj: do b = n0b, n1b
if (b == c) cycle b_aibjcicj
j_aibjcicj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicj
i0 = max(j + 1, n0ik)
i_aibjcicj: do i = i0, n1ik
if (i == j) cycle i_aibjcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcicj(a, b, c)
end do i_aibjcicj
end do a_aibjcicj
end do j_aibjcicj
end do b_aibjcicj
end do c_aibjcicj
!
! Elementary loop  13
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibibkdi: do d = n0d, n1d
k_aibibkdi: do k = n0k, n1k
b0 = max(d + 1, n0bc)
b_aibibkdi: do b = b0, n1bc
if (b == d) cycle b_aibibkdi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkdi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibkdi
i_aibibkdi: do i = n0ijl, n1ijl
if (i == k) cycle i_aibibkdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibibkdi(a, i, k, d)
end do i_aibibkdi
end do a_aibibkdi
end do b_aibibkdi
end do k_aibibkdi
end do d_aibibkdi
!
! Elementary loop  14
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjbidi: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbidi: do b = b0, n1bc
if (b == d) cycle b_aibjbidi
j_aibjbidi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidi
i_aibjbidi: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjbidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbidi(a, i, j, d)
end do i_aibjbidi
end do a_aibjbidi
end do j_aibjbidi
end do b_aibjbidi
end do d_aibjbidi
!
! Elementary loop  15
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjbjdi: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdi: do b = b0, n1bc
if (b == d) cycle b_aibjbjdi
j_aibjbjdi: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdi
i_aibjbjdi: do i = n0il, n1il
if (i == j) cycle i_aibjbjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbjdi(a, i, b, j, d)
end do i_aibjbjdi
end do a_aibjbjdi
end do j_aibjbjdi
end do b_aibjbjdi
end do d_aibjbjdi
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i
! Equalities: c == b, k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjbjdj: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbjdj: do b = b0, n1bc
if (b == d) cycle b_aibjbjdj
j_aibjbjdj: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbjdj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbjdj
i_aibjbjdj: do i = n0i, n1i
if (i == j) cycle i_aibjbjdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbjdj(a, i, j, d)
end do i_aibjbjdj
end do a_aibjbjdj
end do j_aibjbjdj
end do b_aibjbjdj
end do d_aibjbjdj
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj: do d = n0d, n1d
b0 = max(d + 1, n0bc)
b_aibjbidj: do b = b0, n1bc
if (b == d) cycle b_aibjbidj
j_aibjbidj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidj
i_aibjbidj: do i = n0ik, n1ik
if (i == j) cycle i_aibjbidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjbidj(a, i, b, d)
end do i_aibjbidj
end do a_aibjbidj
end do j_aibjbidj
end do b_aibjbidj
end do d_aibjbidj
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, l
! Equalities: c == b, j == i, k == i
! No equalities independent of the above can hold.
!
d_aibibidl: do d = n0d, n1d
l_aibibidl: do l = n0l, n1l
dl = (d - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(d + 1, n0bc)
b_aibibidl: do b = b0, n1bc
if (b == d) cycle b_aibibidl
a0 = max(b + 1, n0a)
a_aibibidl: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibidl
i_aibibidl: do i = n0ijk, n1ijk
if (i == l) cycle i_aibibidl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibibidl(a, i, d, l)
end do i_aibibidl
end do a_aibibidl
end do b_aibibidl
end do l_aibibidl
end do d_aibibidl
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == a, l == i
! No equalities independent of the above can hold.
!
c_aiajckai: do c = n0c, n1c
k_aiajckai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckai: do j = n0j, n1j
if (j == k) cycle j_aiajckai
a1 = min(c - 1, n1abd)
a_aiajckai: do a = n0abd, a1
if (a == c) cycle a_aiajckai
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckai: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajckai(a, j, c, k)
end do i_aiajckai
end do a_aiajckai
end do j_aiajckai
end do k_aiajckai
end do c_aiajckai
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, l == j
! No equalities independent of the above can hold.
!
c_aiajckaj: do c = n0c, n1c
k_aiajckaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j_aiajckaj: do j = n0jl, n1jl
if (j == k) cycle j_aiajckaj
a1 = min(c - 1, n1abd)
a_aiajckaj: do a = n0abd, a1
if (a == c) cycle a_aiajckaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckaj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajckaj(a, i, c, k)
end do i_aiajckaj
end do a_aiajckaj
end do j_aiajckaj
end do k_aiajckaj
end do c_aiajckaj
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == a, k == i
! No equalities independent of the above can hold.
!
l_aiajcial: do l = n0l, n1l
c_aiajcial: do c = n0c, n1c
j_aiajcial: do j = n0j, n1j
if (j == l) cycle j_aiajcial
a1 = min(c - 1, n1abd)
a_aiajcial: do a = n0abd, a1
if (a == c) cycle a_aiajcial
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcial: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajcial(a, j, c, l)
end do i_aiajcial
end do a_aiajcial
end do j_aiajcial
end do c_aiajcial
end do l_aiajcial
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == a, k == j
! No equalities independent of the above can hold.
!
l_aiajcjal: do l = n0l, n1l
c_aiajcjal: do c = n0c, n1c
j_aiajcjal: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjal
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a1 = min(c - 1, n1abd)
a_aiajcjal: do a = n0abd, a1
if (a == c) cycle a_aiajcjal
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjal: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajcjal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajcjal(a, i, c, l)
end do i_aiajcjal
end do a_aiajcjal
end do j_aiajcjal
end do c_aiajcjal
end do l_aiajcjal
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, k
! Equalities: d == a, j == i, l == i
! No equalities independent of the above can hold.
!
c_aibickai: do c = n0c, n1c
k_aibickai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b_aibickai: do b = n0b, n1b
if (b == c) cycle b_aibickai
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibickai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibickai
i_aibickai: do i = n0ijl, n1ijl
if (i == k) cycle i_aibickai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibickai(i, b, c, k)
end do i_aibickai
end do a_aibickai
end do b_aibickai
end do k_aibickai
end do c_aibickai
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjciai: do c = n0c, n1c
b_aibjciai: do b = n0b, n1b
if (b == c) cycle b_aibjciai
j_aibjciai: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciai
i_aibjciai: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjciai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjciai(i, b, j, c)
end do i_aibjciai
end do a_aibjciai
end do j_aibjciai
end do b_aibjciai
end do c_aibjciai
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjai: do c = n0c, n1c
b_aibjcjai: do b = n0b, n1b
if (b == c) cycle b_aibjcjai
j_aibjcjai: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjai: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjai
i_aibjcjai: do i = n0il, n1il
if (i == j) cycle i_aibjcjai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcjai(a, i, b, j, c)
end do i_aibjcjai
end do a_aibjcjai
end do j_aibjcjai
end do b_aibjcjai
end do c_aibjcjai
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i
! Equalities: d == a, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjaj: do c = n0c, n1c
b_aibjcjaj: do b = n0b, n1b
if (b == c) cycle b_aibjcjaj
j_aibjcjaj: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjcjaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjcjaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjcjaj: do i = n0i, n1i
if (i == j) cycle i_aibjcjaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjcjaj(i, b, j, c)
end do i_aibjcjaj
end do a_aibjcjaj
end do j_aibjcjaj
end do b_aibjcjaj
end do c_aibjcjaj
!
! Elementary loop  27
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j
! Equalities: d == a, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjciaj: do c = n0c, n1c
b_aibjciaj: do b = n0b, n1b
if (b == c) cycle b_aibjciaj
j_aibjciaj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibjciaj: do a = a0, a1
if (a == b .or. a == c) cycle a_aibjciaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjciaj: do i = n0ik, n1ik
if (i == j) cycle i_aibjciaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibjciaj(a, b, j, c)
end do i_aibjciaj
end do a_aibjciaj
end do j_aibjciaj
end do b_aibjciaj
end do c_aibjciaj
!
! Elementary loop  28
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, l
! Equalities: d == a, j == i, k == i
! No equalities independent of the above can hold.
!
l_aibicial: do l = n0l, n1l
c_aibicial: do c = n0c, n1c
b_aibicial: do b = n0b, n1b
if (b == c) cycle b_aibicial
a0 = max(b + 1, n0ad)
a1 = min(c - 1, n1ad)
a_aibicial: do a = a0, a1
if (a == b .or. a == c) cycle a_aibicial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibicial: do i = n0ijk, n1ijk
if (i == l) cycle i_aibicial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aibicial(i, b, c, l)
end do i_aibicial
end do a_aibicial
end do b_aibicial
end do c_aibicial
end do l_aibicial
!
! Elementary loop  29
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, l == i, k == j
! No equalities independent of the above can hold.
!
d_aiajcjdi: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcjdi: do c = c0, n1c
if (c == d) cycle c_aiajcjdi
j_aiajcjdi: do j = n0jk, n1jk
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcjdi: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcjdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajcjdi: do i = i0, n1il
if (i == j) cycle i_aiajcjdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajcjdi(a, c, d)
end do i_aiajcjdi
end do a_aiajcjdi
end do j_aiajcjdi
end do c_aiajcjdi
end do d_aiajcjdi
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j
! Equalities: b == a, k == i, l == j
! No equalities independent of the above can hold.
!
d_aiajcidj: do d = n0d, n1d
c0 = max(d + 1, n0c)
c_aiajcidj: do c = c0, n1c
if (c == d) cycle c_aiajcidj
j_aiajcidj: do j = n0jl, n1jl
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a_aiajcidj: do a = n0ab, n1ab
if (a == c .or. a == d) cycle a_aiajcidj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcidj: do i = i0, n1ik
if (i == j) cycle i_aiajcidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
ibra = braoffset + &
((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
iket = ketoffset + &
((2 * npair - dl + 2) * (dl - 1)) / 2 + ck - dl + 1
hci(ibra, iket) = cisd22_aiajcidj(a, c, d)
end do i_aiajcidj
end do a_aiajcidj
end do j_aiajcidj
end do c_aiajcidj
end do d_aiajcidj
end subroutine cisd_block_22_part2
end module cisd_block_22_mod_part2
