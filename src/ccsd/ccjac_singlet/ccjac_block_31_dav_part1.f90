module ccjac_block_31_dav_part1
use eom_cc3_31_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-22 13:23:51 UTC.
!
contains
subroutine ccjac_31_dav_part1(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
 nvirt1, n0a, n1a, n0b, n1b, n0c, n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, &
 n1l, bra0, ket0, offset) 
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
integer, intent(in)                                 :: offset
!
! Local variables
!
real(F64) :: jac_ibra_iket
integer :: a, b, c, d
integer :: i, j, k, l
integer :: ai, bj, ck, dl
integer :: a0, b0, i0, i1, j0, j1
integer :: n0ab, n0abd, n0ad, n0bc, n0bcd
integer :: n0bd, n0cd, n0ik, n0il, n0jk
integer :: n0jl, n0kl
integer :: n1ab, n1abd, n1ad, n1bc, n1bcd
integer :: n1bd, n1cd, n1ik, n1il, n1jk
integer :: n1jl, n1kl
integer :: nocc, nvirt
integer :: npair, nactive
integer :: ibra, iket, iket2, ibra2
integer :: braoffset, ketoffset
integer :: qbj, qbj2
integer :: qck, qck2
integer :: q00
integer :: ni, na, nj, nb, nk, nc
integer :: mi, ma, mj, mb, mk, mc
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
qbj  = 3 + 6 * npair
qbj2 = -3
qck  = 2 + 3 * npair * (2 + npair)
qck2 = -3 * (1 + npair)
q00  = -3 * npair * (3 + npair)
ni = n1i - n0i + 1
na = n1a - n0a + 1
nj = n1j - n0j + 1
nb = n1b - n0b + 1
nk = n1k - n0k + 1
nc = n1c - n0c + 1
mi = 1
ma = ni
mj = ni * na
mb = ni * na * nj
mk = ni * na * nj * nb
mc = ni * na * nj * nb * nk
n0ab = max(n0a, n0b)
n0abd = max(n0a, n0b, n0d)
n0ad = max(n0a, n0d)
n0bc = max(n0b, n0c)
n0bcd = max(n0b, n0c, n0d)
n0bd = max(n0b, n0d)
n0cd = max(n0c, n0d)
n0ik = max(n0i, n0k)
n0il = max(n0i, n0l)
n0jk = max(n0j, n0k)
n0jl = max(n0j, n0l)
n0kl = max(n0k, n0l)
n1ab = min(n1a, n1b)
n1abd = min(n1a, n1b, n1d)
n1ad = min(n1a, n1d)
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ik = min(n1i, n1k)
n1il = min(n1i, n1l)
n1jk = min(n1j, n1k)
n1jl = min(n1j, n1l)
n1kl = min(n1k, n1l)
!
! Elementary loop  1
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k, l
! Equalities: d == c
! No equalities independent of the above can hold.
!
l_aibjckcl: do l = n0l, n1l
c_aibjckcl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aibjckcl: do k = n0k, n1k
if (k == l) cycle k_aibjckcl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcl: do b = b0, n1b
if (b == c) cycle b_aibjckcl
j_aibjckcl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckcl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckcl
i_aibjckcl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckcl
if (i > j .and. j > k) exit i_aibjckcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckcl
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckcl(t2, nocc, nactive, a, i, b, j, k, l)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckcl(t2, nocc, nactive, a, i, b, j, k, l)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckcl(t2, nocc, nactive, a, i, b, j, k, l)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckcl(t2, nocc, nactive, a, i, b, j, k, l)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckcl(t2, nocc, nactive, a, i, b, j, k, l)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcl
end do a_aibjckcl
end do j_aibjckcl
end do b_aibjckcl
end do k_aibjckcl
end do c_aibjckcl
end do l_aibjckcl
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k, l
! Equalities: d == a
! No equalities independent of the above can hold.
!
l_aibjckal: do l = n0l, n1l
c_aibjckal: do c = n0c, n1c
k_aibjckal: do k = n0k, n1k
if (k == l) cycle k_aibjckal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckal: do b = b0, n1b
if (b == c) cycle b_aibjckal
j_aibjckal: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckal: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjckal: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckal
if (i > j .and. j > k) exit i_aibjckal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckal
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckal(t2, nocc, nactive, i, b, j, c, k, l)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckal
end do a_aibjckal
end do j_aibjckal
end do b_aibjckal
end do k_aibjckal
end do c_aibjckal
end do l_aibjckal
!
! Elementary loop  3
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k, l
! Equalities: d == b
! No equalities independent of the above can hold.
!
l_aibjckbl: do l = n0l, n1l
c_aibjckbl: do c = n0c, n1c
k_aibjckbl: do k = n0k, n1k
if (k == l) cycle k_aibjckbl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbl: do b = b0, n1bd
if (b == c) cycle b_aibjckbl
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j_aibjckbl: do j = n0j, n1j
if (j == k .or. j == l) cycle j_aibjckbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbl
i_aibjckbl: do i = n0i, n1i
if (i == j .or. i == k .or. i == l) cycle i_aibjckbl
if (i > j .and. j > k) exit i_aibjckbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckbl
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckbl(t2, nocc, nactive, a, i, j, c, k, l)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbl
end do a_aibjckbl
end do j_aibjckbl
end do b_aibjckbl
end do k_aibjckbl
end do c_aibjckbl
end do l_aibjckbl
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i, k
! Equalities: l == j
! No equalities independent of the above can hold.
!
d_aibjckdj: do d = n0d, n1d
c_aibjckdj: do c = n0c, n1c
if (c == d) cycle c_aibjckdj
k_aibjckdj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdj
j_aibjckdj: do j = n0jl, n1jl
if (j == k) cycle j_aibjckdj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdj
i_aibjckdj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdj
if (i > j .and. j > k) exit i_aibjckdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckdj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckdj(t2, nocc, nactive, a, i, b, c, k, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdj
end do a_aibjckdj
end do j_aibjckdj
end do b_aibjckdj
end do k_aibjckdj
end do c_aibjckdj
end do d_aibjckdj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: k, i, j
! Equalities: l == k
! No equalities independent of the above can hold.
!
d_aibjckdk: do d = n0d, n1d
c_aibjckdk: do c = n0c, n1c
if (c == d) cycle c_aibjckdk
k_aibjckdk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdk: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdk
j_aibjckdk: do j = n0j, n1j
if (j == k) cycle j_aibjckdk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdk: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdk
i_aibjckdk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckdk
if (i > j .and. j > k) exit i_aibjckdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckdk
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckdk(t2, nocc, nactive, a, i, b, j, c, d)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckdk(t2, nocc, nactive, a, i, b, j, c, d)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckdk(t2, nocc, nactive, a, i, b, j, c, d)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckdk(t2, nocc, nactive, a, i, b, j, c, d)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckdk(t2, nocc, nactive, a, i, b, j, c, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdk
end do a_aibjckdk
end do j_aibjckdk
end do b_aibjckdk
end do k_aibjckdk
end do c_aibjckdk
end do d_aibjckdk
!
! Elementary loop  6
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j, k
! Equalities: l == i
! No equalities independent of the above can hold.
!
d_aibjckdi: do d = n0d, n1d
c_aibjckdi: do c = n0c, n1c
if (c == d) cycle c_aibjckdi
k_aibjckdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckdi: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjckdi
j_aibjckdi: do j = n0j, n1j
if (j == k) cycle j_aibjckdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckdi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjckdi
i_aibjckdi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckdi
if (i > j .and. j > k) exit i_aibjckdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckdi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckdi(t2, nocc, nactive, a, b, j, c, k, d)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckdi
end do a_aibjckdi
end do j_aibjckdi
end do b_aibjckdi
end do k_aibjckdi
end do c_aibjckdi
end do d_aibjckdi
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k, l
! Equalities: c == b, d == b
! No equalities independent of the above can hold.
!
l_aibjbkbl: do l = n0l, n1l
k_aibjbkbl: do k = n0k, n1k
if (k == l) cycle k_aibjbkbl
b_aibjbkbl: do b = n0bcd, n1bcd
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbl: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjbkbl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbl: do a = a0, n1a
if (a == b) cycle a_aibjbkbl
i1 = min(j - 1, n1i)
i_aibjbkbl: do i = n0i, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkbl(t2, nocc, nactive, a, i, b, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbl
end do a_aibjbkbl
end do j_aibjbkbl
end do b_aibjbkbl
end do k_aibjbkbl
end do l_aibjbkbl
!
! Elementary loop  8
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k, l
! Equalities: d == a, c == b
! No equalities independent of the above can hold.
!
l_aibjbkal: do l = n0l, n1l
k_aibjbkal: do k = n0k, n1k
if (k == l) cycle k_aibjbkal
b_aibjbkal: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkal: do j = j0, n1j
if (j == k .or. j == l) cycle j_aibjbkal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkal: do a = a0, n1ad
if (a == b) cycle a_aibjbkal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkal: do i = n0i, i1
if (i == j .or. i == k .or. i == l) cycle i_aibjbkal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkal(t2, nocc, nactive, i, b, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkal
end do a_aibjbkal
end do j_aibjbkal
end do b_aibjbkal
end do k_aibjbkal
end do l_aibjbkal
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: j, i, k
! Equalities: c == b, l == j
! No equalities independent of the above can hold.
!
d_aibjbkdj: do d = n0d, n1d
k_aibjbkdj: do k = n0k, n1k
b_aibjbkdj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdj
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkdj: do j = j0, n1jl
if (j == k) cycle j_aibjbkdj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdj
i1 = min(j - 1, n1i)
i_aibjbkdj: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkdj(t2, nocc, nactive, a, i, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdj
end do a_aibjbkdj
end do j_aibjbkdj
end do b_aibjbkdj
end do k_aibjbkdj
end do d_aibjbkdj
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: k, i, j
! Equalities: c == b, l == k
! No equalities independent of the above can hold.
!
d_aibjbkdk: do d = n0d, n1d
k_aibjbkdk: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibjbkdk: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdk
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkdk: do j = j0, n1j
if (j == k) cycle j_aibjbkdk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdk: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdk
i1 = min(j - 1, n1i)
i_aibjbkdk: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkdk(t2, nocc, nactive, a, i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdk
end do a_aibjbkdk
end do j_aibjbkdk
end do b_aibjbkdk
end do k_aibjbkdk
end do d_aibjbkdk
!
! Elementary loop  11
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j, k
! Equalities: c == b, l == i
! No equalities independent of the above can hold.
!
d_aibjbkdi: do d = n0d, n1d
k_aibjbkdi: do k = n0k, n1k
b_aibjbkdi: do b = n0bc, n1bc
if (b == d) cycle b_aibjbkdi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkdi: do j = j0, n1j
if (j == k) cycle j_aibjbkdi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkdi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbkdi
i1 = min(j - 1, n1il)
i_aibjbkdi: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkdi(t2, nocc, nactive, a, b, j, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkdi
end do a_aibjbkdi
end do j_aibjbkdi
end do b_aibjbkdi
end do k_aibjbkdi
end do d_aibjbkdi
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == c
! No equalities independent of the above can hold.
!
l_aiajckcl: do l = n0l, n1l
c_aiajckcl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
k_aiajckcl: do k = n0k, n1k
if (k == l) cycle k_aiajckcl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckcl: do j = n0j, j1
if (j == k .or. j == l) cycle j_aiajckcl
a0 = max(c + 1, n0ab)
a_aiajckcl: do a = a0, n1ab
if (a == c) cycle a_aiajckcl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckcl: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckcl(t2, nocc, nactive, a, i, j, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckcl
end do a_aiajckcl
end do j_aiajckcl
end do k_aiajckcl
end do c_aiajckcl
end do l_aiajckcl
!
! Elementary loop  13
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i, l
! Equalities: d == c, k == j
! No equalities independent of the above can hold.
!
l_aibjcjcl: do l = n0l, n1l
c_aibjcjcl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjcjcl: do b = b0, n1b
if (b == c) cycle b_aibjcjcl
j_aibjcjcl: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjcl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcl
i_aibjcjcl: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjcl(t2, nocc, nactive, a, i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcl
end do a_aibjcjcl
end do j_aibjcjcl
end do b_aibjcjcl
end do c_aibjcjcl
end do l_aibjcjcl
!
! Elementary loop  14
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i, k
! Equalities: d == c, l == j
! No equalities independent of the above can hold.
!
c_aibjckcj: do c = n0cd, n1cd
k_aibjckcj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckcj: do b = b0, n1b
if (b == c) cycle b_aibjckcj
j_aibjckcj: do j = n0jl, n1jl
if (j == k) cycle j_aibjckcj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckcj
i_aibjckcj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckcj
if (i > j .and. j > k) exit i_aibjckcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckcj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckcj(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckcj(t2, nocc, nactive, a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckcj(t2, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckcj(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckcj(t2, nocc, nactive, a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckcj
end do a_aibjckcj
end do j_aibjckcj
end do b_aibjckcj
end do k_aibjckcj
end do c_aibjckcj
!
! Elementary loop  15
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, l
! Equalities: d == c, k == i
! No equalities independent of the above can hold.
!
l_aibjcicl: do l = n0l, n1l
c_aibjcicl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjcicl: do b = b0, n1b
if (b == c) cycle b_aibjcicl
j_aibjcicl: do j = n0j, n1j
if (j == l) cycle j_aibjcicl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicl: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicl
i_aibjcicl: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcicl(t2, nocc, nactive, a, i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicl
end do a_aibjcicl
end do j_aibjcicl
end do b_aibjcicl
end do c_aibjcicl
end do l_aibjcicl
!
! Elementary loop  16
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: k, i, j
! Equalities: d == c, l == k
! No equalities independent of the above can hold.
!
c_aibjckck: do c = n0cd, n1cd
k_aibjckck: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckck: do b = b0, n1b
if (b == c) cycle b_aibjckck
j_aibjckck: do j = n0j, n1j
if (j == k) cycle j_aibjckck
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckck: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckck
i_aibjckck: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckck
if (i > j .and. j > k) exit i_aibjckck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckck
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckck(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckck(t2, nocc, nactive, a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckck(t2, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckck(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckck(t2, nocc, nactive, a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckck
end do a_aibjckck
end do j_aibjckck
end do b_aibjckck
end do k_aibjckck
end do c_aibjckck
!
! Elementary loop  17
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j, k
! Equalities: d == c, l == i
! No equalities independent of the above can hold.
!
c_aibjckci: do c = n0cd, n1cd
k_aibjckci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckci: do b = b0, n1b
if (b == c) cycle b_aibjckci
j_aibjckci: do j = n0j, n1j
if (j == k) cycle j_aibjckci
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckci
i_aibjckci: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckci
if (i > j .and. j > k) exit i_aibjckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckci
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckci(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckci(t2, nocc, nactive, a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckci(t2, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckci(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckci(t2, nocc, nactive, a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckci
end do a_aibjckci
end do j_aibjckci
end do b_aibjckci
end do k_aibjckci
end do c_aibjckci
!
! Elementary loop  18
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k, l
! Equalities: b == a, d == a
! No equalities independent of the above can hold.
!
l_aiajckal: do l = n0l, n1l
c_aiajckal: do c = n0c, n1c
k_aiajckal: do k = n0k, n1k
if (k == l) cycle k_aiajckal
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckal: do j = n0j, j1
if (j == k .or. j == l) cycle j_aiajckal
a0 = max(c + 1, n0abd)
a_aiajckal: do a = a0, n1abd
if (a == c) cycle a_aiajckal
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckal: do i = i0, n1i
if (i == j .or. i == k .or. i == l) cycle i_aiajckal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckal(t2, nocc, nactive, a, i, j, c, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckal
end do a_aiajckal
end do j_aiajckal
end do k_aiajckal
end do c_aiajckal
end do l_aiajckal
!
! Elementary loop  19
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: j, i, k
! Equalities: b == a, l == j
! No equalities independent of the above can hold.
!
d_aiajckdj: do d = n0d, n1d
c_aiajckdj: do c = n0c, n1c
if (c == d) cycle c_aiajckdj
k_aiajckdj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckdj: do j = n0jl, j1
if (j == k) cycle j_aiajckdj
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckdj: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckdj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckdj(t2, nocc, nactive, a, i, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdj
end do a_aiajckdj
end do j_aiajckdj
end do k_aiajckdj
end do c_aiajckdj
end do d_aiajckdj
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: k, i, j
! Equalities: b == a, l == k
! No equalities independent of the above can hold.
!
d_aiajckdk: do d = n0d, n1d
c_aiajckdk: do c = n0c, n1c
if (c == d) cycle c_aiajckdk
k_aiajckdk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckdk: do j = n0j, j1
if (j == k) cycle j_aiajckdk
a0 = max(c + 1, n0ab)
a_aiajckdk: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdk
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckdk: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckdk(t2, nocc, nactive, a, i, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdk
end do a_aiajckdk
end do j_aiajckdk
end do k_aiajckdk
end do c_aiajckdk
end do d_aiajckdk
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c, d
! Free occupied indices: i, j, k
! Equalities: b == a, l == i
! No equalities independent of the above can hold.
!
d_aiajckdi: do d = n0d, n1d
c_aiajckdi: do c = n0c, n1c
if (c == d) cycle c_aiajckdi
k_aiajckdi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckdi: do j = n0j, j1
if (j == k) cycle j_aiajckdi
a0 = max(c + 1, n0ab)
a_aiajckdi: do a = a0, n1ab
if (a == c .or. a == d) cycle a_aiajckdi
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckdi: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckdi(t2, nocc, nactive, a, j, c, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckdi
end do a_aiajckdi
end do j_aiajckdi
end do k_aiajckdi
end do c_aiajckdi
end do d_aiajckdi
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, l
! Equalities: d == a, k == j
! No equalities independent of the above can hold.
!
l_aibjcjal: do l = n0l, n1l
c_aibjcjal: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjcjal: do b = b0, n1b
if (b == c) cycle b_aibjcjal
j_aibjcjal: do j = n0jk, n1jk
if (j == l) cycle j_aibjcjal
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcjal: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcjal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcjal: do i = n0i, n1i
if (i == j .or. i == l) cycle i_aibjcjal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjal(t2, nocc, nactive, i, b, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjal
end do a_aibjcjal
end do j_aibjcjal
end do b_aibjcjal
end do c_aibjcjal
end do l_aibjcjal
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: j, i, k
! Equalities: d == a, l == j
! No equalities independent of the above can hold.
!
c_aibjckaj: do c = n0c, n1c
k_aibjckaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckaj: do b = b0, n1b
if (b == c) cycle b_aibjckaj
j_aibjckaj: do j = n0jl, n1jl
if (j == k) cycle j_aibjckaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckaj: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i_aibjckaj: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckaj
if (i > j .and. j > k) exit i_aibjckaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckaj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckaj(t2, nocc, nactive, a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckaj
end do a_aibjckaj
end do j_aibjckaj
end do b_aibjckaj
end do k_aibjckaj
end do c_aibjckaj
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, l
! Equalities: d == a, k == i
! No equalities independent of the above can hold.
!
l_aibjcial: do l = n0l, n1l
c_aibjcial: do c = n0c, n1c
b0 = max(c + 1, n0b)
b_aibjcial: do b = b0, n1b
if (b == c) cycle b_aibjcial
j_aibjcial: do j = n0j, n1j
if (j == l) cycle j_aibjcial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjcial: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjcial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i_aibjcial: do i = n0ik, n1ik
if (i == j .or. i == l) cycle i_aibjcial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcial(t2, nocc, nactive, i, b, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcial
end do a_aibjcial
end do j_aibjcial
end do b_aibjcial
end do c_aibjcial
end do l_aibjcial
!
! Elementary loop  25
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: k, i, j
! Equalities: d == a, l == k
! No equalities independent of the above can hold.
!
c_aibjckak: do c = n0c, n1c
k_aibjckak: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckak: do b = b0, n1b
if (b == c) cycle b_aibjckak
j_aibjckak: do j = n0j, n1j
if (j == k) cycle j_aibjckak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckak: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i_aibjckak: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckak
if (i > j .and. j > k) exit i_aibjckak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckak
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckak(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckak(t2, nocc, nactive, a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckak(t2, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckak(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckak(t2, nocc, nactive, a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckak
end do a_aibjckak
end do j_aibjckak
end do b_aibjckak
end do k_aibjckak
end do c_aibjckak
!
! Elementary loop  26
! --------------------
! Free virtual indices: a, b, c
! Free occupied indices: i, j, k
! Equalities: d == a, l == i
! No equalities independent of the above can hold.
!
c_aibjckai: do c = n0c, n1c
k_aibjckai: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0b)
b_aibjckai: do b = b0, n1b
if (b == c) cycle b_aibjckai
j_aibjckai: do j = n0j, n1j
if (j == k) cycle j_aibjckai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjckai: do a = a0, n1ad
if (a == b .or. a == c) cycle a_aibjckai
i_aibjckai: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckai
if (i > j .and. j > k) exit i_aibjckai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckai
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckai(t2, nocc, nactive, a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckai
end do a_aibjckai
end do j_aibjckai
end do b_aibjckai
end do k_aibjckai
end do c_aibjckai
!
! Elementary loop  27
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, l
! Equalities: d == b, k == j
! No equalities independent of the above can hold.
!
l_aibjcjbl: do l = n0l, n1l
c_aibjcjbl: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcjbl: do b = b0, n1bd
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjbl(t2, nocc, nactive, a, i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjbl
end do a_aibjcjbl
end do j_aibjcjbl
end do b_aibjcjbl
end do c_aibjcjbl
end do l_aibjcjbl
!
! Elementary loop  28
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: j, i, k
! Equalities: d == b, l == j
! No equalities independent of the above can hold.
!
c_aibjckbj: do c = n0c, n1c
k_aibjckbj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbj: do b = b0, n1bd
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
if (i > j .and. j > k) exit i_aibjckbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckbj
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckbj(t2, nocc, nactive, a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbj
end do a_aibjckbj
end do j_aibjckbj
end do b_aibjckbj
end do k_aibjckbj
end do c_aibjckbj
!
! Elementary loop  29
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: i, j, l
! Equalities: d == b, k == i
! No equalities independent of the above can hold.
!
l_aibjcibl: do l = n0l, n1l
c_aibjcibl: do c = n0c, n1c
b0 = max(c + 1, n0bd)
b_aibjcibl: do b = b0, n1bd
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcibl(t2, nocc, nactive, a, i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcibl
end do a_aibjcibl
end do j_aibjcibl
end do b_aibjcibl
end do c_aibjcibl
end do l_aibjcibl
!
! Elementary loop  30
! --------------------
! Free virtual indices: b, a, c
! Free occupied indices: k, i, j
! Equalities: d == b, l == k
! No equalities independent of the above can hold.
!
c_aibjckbk: do c = n0c, n1c
k_aibjckbk: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbk: do b = b0, n1bd
if (b == c) cycle b_aibjckbk
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j_aibjckbk: do j = n0j, n1j
if (j == k) cycle j_aibjckbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbk: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbk
i_aibjckbk: do i = n0i, n1i
if (i == j .or. i == k) cycle i_aibjckbk
if (i > j .and. j > k) exit i_aibjckbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckbk
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckbk(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckbk(t2, nocc, nactive, a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckbk(t2, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckbk(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckbk(t2, nocc, nactive, a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbk
end do a_aibjckbk
end do j_aibjckbk
end do b_aibjckbk
end do k_aibjckbk
end do c_aibjckbk
contains
!
! Locally visible functions
!
function mu3(ai, bj, ck)
!
! Compute compound three-electron index
! (assumed that ai >= bj >= ck)
!
integer :: mu3
integer, intent(in) :: ai, bj, ck
integer :: mu31, mu32
!
! Compound index is compouted relative
! to the first element of matrix block.
! Braoffset/ketoffset should be added
! to get the absolute position
!
mu31 = (qbj + qbj2 * bj) * bj 
mu32 = (qck + (qck2 + ck) * ck) * ck
mu3  = ai + (mu31 + mu32 + q00) / 6
end function mu3
function mu3_mem(a, i, b, j, c, k)
integer :: mu3_mem
integer, intent(in) :: a, i, b, j, c, k
mu3_mem = offset + (i-n0i)+mi + (a-n0a)*ma + (j-n0j)*mj + (b-n0b)*mb + (k-n0k)*mk  &
+ (c-n0c)*mc
end function mu3_mem
end subroutine ccjac_31_dav_part1
end module ccjac_block_31_dav_part1
