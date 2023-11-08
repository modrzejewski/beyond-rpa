module ccjac_block_31_dav_part2
use eom_cc3_31_trans
use davidson_main
use math_constants
use arithmetic
use cmpidx
implicit none
!
! File generated automatically on 2019-06-22 13:23:52 UTC.
!
contains
subroutine ccjac_31_dav_part2(sigup_diag, sigup_nondiag, t2, nocc0, nocc1, nvirt0, &
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
integer :: n0bd, n0cd, n0ij, n0ijl, n0ik
integer :: n0ikl, n0il, n0jk, n0jkl, n0jl
integer :: n0kl
integer :: n1ab, n1abd, n1ad, n1bc, n1bcd
integer :: n1bd, n1cd, n1ij, n1ijl, n1ik
integer :: n1ikl, n1il, n1jk, n1jkl, n1jl
integer :: n1kl
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
n0ij = max(n0i, n0j)
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
n1bc = min(n1b, n1c)
n1bcd = min(n1b, n1c, n1d)
n1bd = min(n1b, n1d)
n1cd = min(n1c, n1d)
n1ij = min(n1i, n1j)
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
! Free virtual indices: b, a, c
! Free occupied indices: i, j, k
! Equalities: d == b, l == i
! No equalities independent of the above can hold.
!
c_aibjckbi: do c = n0c, n1c
k_aibjckbi: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
b0 = max(c + 1, n0bd)
b_aibjckbi: do b = b0, n1bd
if (b == c) cycle b_aibjckbi
j_aibjckbi: do j = n0j, n1j
if (j == k) cycle j_aibjckbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjckbi: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjckbi
i_aibjckbi: do i = n0il, n1il
if (i == j .or. i == k) cycle i_aibjckbi
if (i > j .and. j > k) exit i_aibjckbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, k)
iket = ketoffset + dl
if (i > j) then
if (j > k) then
exit i_aibjckbi
else if (k > i) then
jac_ibra_iket = v3_eom_cc3_31_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v1_eom_cc3_31_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k)
end if
else if (i > k) then
jac_ibra_iket = v2_eom_cc3_31_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k)
else if (k > j) then
jac_ibra_iket = v5_eom_cc3_31_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k)
else
jac_ibra_iket = v4_eom_cc3_31_trans_aibjckbi(t2, nocc, nactive, a, i, b, j, c, k)
end if
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjckbi
end do a_aibjckbi
end do j_aibjckbi
end do b_aibjckbi
end do k_aibjckbi
end do c_aibjckbi
!
! Elementary loop  2
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: j, i
! Equalities: k == j, l == j
! No equalities independent of the above can hold.
!
d_aibjcjdj: do d = n0d, n1d
c_aibjcjdj: do c = n0c, n1c
if (c == d) cycle c_aibjcjdj
b0 = max(c + 1, n0b)
b_aibjcjdj: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcjdj
j_aibjcjdj: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjdj: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcjdj
i_aibjcjdj: do i = n0i, n1i
if (i == j) cycle i_aibjcjdj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjdj(t2, nocc, nactive, a, i, b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdj
end do a_aibjcjdj
end do j_aibjcjdj
end do b_aibjcjdj
end do c_aibjcjdj
end do d_aibjcjdj
!
! Elementary loop  3
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: l == i, k == j
! No equalities independent of the above can hold.
!
d_aibjcjdi: do d = n0d, n1d
c_aibjcjdi: do c = n0c, n1c
if (c == d) cycle c_aibjcjdi
b0 = max(c + 1, n0b)
b_aibjcjdi: do b = b0, n1b
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjdi(t2, nocc, nactive, a, b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjdi
end do a_aibjcjdi
end do j_aibjcjdi
end do b_aibjcjdi
end do c_aibjcjdi
end do d_aibjcjdi
!
! Elementary loop  4
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjcidj: do d = n0d, n1d
c_aibjcidj: do c = n0c, n1c
if (c == d) cycle c_aibjcidj
b0 = max(c + 1, n0b)
b_aibjcidj: do b = b0, n1b
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
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcidj(t2, nocc, nactive, a, i, b, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidj
end do a_aibjcidj
end do j_aibjcidj
end do b_aibjcidj
end do c_aibjcidj
end do d_aibjcidj
!
! Elementary loop  5
! --------------------
! Free virtual indices: a, b, c, d
! Free occupied indices: i, j
! Equalities: k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjcidi: do d = n0d, n1d
c_aibjcidi: do c = n0c, n1c
if (c == d) cycle c_aibjcidi
b0 = max(c + 1, n0b)
b_aibjcidi: do b = b0, n1b
if (b == c .or. b == d) cycle b_aibjcidi
j_aibjcidi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcidi: do a = a0, n1a
if (a == b .or. a == c .or. a == d) cycle a_aibjcidi
i_aibjcidi: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcidi(t2, nocc, nactive, a, i, b, j, c, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcidi
end do a_aibjcidi
end do j_aibjcidi
end do b_aibjcidi
end do c_aibjcidi
end do d_aibjcidi
!
! Elementary loop  6
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, k, l
! Equalities: c == b, d == b, j == i
! No equalities independent of the above can hold.
!
l_aibibkbl: do l = n0l, n1l
k_aibibkbl: do k = n0k, n1k
if (k == l) cycle k_aibibkbl
b_aibibkbl: do b = n0bcd, n1bcd
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (l - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkbl: do a = a0, n1a
if (a == b) cycle a_aibibkbl
i0 = max(k + 1, n0ij)
i_aibibkbl: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibibkbl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibibkbl(t2, nocc, nactive, a, i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkbl
end do a_aibibkbl
end do b_aibibkbl
end do k_aibibkbl
end do l_aibibkbl
!
! Elementary loop  7
! --------------------
! Free virtual indices: b, a
! Free occupied indices: j, i, k
! Equalities: c == b, d == b, l == j
! No equalities independent of the above can hold.
!
k_aibjbkbj: do k = n0k, n1k
b_aibjbkbj: do b = n0bcd, n1bcd
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkbj: do j = j0, n1jl
if (j == k) cycle j_aibjbkbj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbj: do a = a0, n1a
if (a == b) cycle a_aibjbkbj
i1 = min(j - 1, n1i)
i_aibjbkbj: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkbj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkbj(t2, nocc, nactive, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbj
end do a_aibjbkbj
end do j_aibjbkbj
end do b_aibjbkbj
end do k_aibjbkbj
!
! Elementary loop  8
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
i1 = min(j - 1, n1ik)
i_aibjbibl: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjbibl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbibl(t2, nocc, nactive, a, i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbibl
end do a_aibjbibl
end do j_aibjbibl
end do b_aibjbibl
end do l_aibjbibl
!
! Elementary loop  9
! --------------------
! Free virtual indices: b, a
! Free occupied indices: k, i, j
! Equalities: c == b, d == b, l == k
! No equalities independent of the above can hold.
!
k_aibjbkbk: do k = n0kl, n1kl
b_aibjbkbk: do b = n0bcd, n1bcd
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
dl = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbk: do j = j0, n1j
if (j == k) cycle j_aibjbkbk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbk: do a = a0, n1a
if (a == b) cycle a_aibjbkbk
i1 = min(j - 1, n1i)
i_aibjbkbk: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkbk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkbk(t2, nocc, nactive, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbk
end do a_aibjbkbk
end do j_aibjbkbk
end do b_aibjbkbk
end do k_aibjbkbk
!
! Elementary loop  10
! --------------------
! Free virtual indices: b, a
! Free occupied indices: i, j, k
! Equalities: c == b, d == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkbi: do k = n0k, n1k
b_aibjbkbi: do b = n0bcd, n1bcd
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkbi: do j = j0, n1j
if (j == k) cycle j_aibjbkbi
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbkbi: do a = a0, n1a
if (a == b) cycle a_aibjbkbi
i1 = min(j - 1, n1il)
i_aibjbkbi: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkbi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkbi(t2, nocc, nactive, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkbi
end do a_aibjbkbi
end do j_aibjbkbi
end do b_aibjbkbi
end do k_aibjbkbi
!
! Elementary loop  11
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, k, l
! Equalities: d == a, c == b, j == i
! No equalities independent of the above can hold.
!
l_aibibkal: do l = n0l, n1l
k_aibibkal: do k = n0k, n1k
if (k == l) cycle k_aibibkal
b_aibibkal: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibibkal: do a = a0, n1ad
if (a == b) cycle a_aibibkal
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(k + 1, n0ij)
i_aibibkal: do i = i0, n1ij
if (i == k .or. i == l) cycle i_aibibkal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibibkal(t2, nocc, nactive, i, b, k, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkal
end do a_aibibkal
end do b_aibibkal
end do k_aibibkal
end do l_aibibkal
!
! Elementary loop  12
! --------------------
! Free virtual indices: a, b
! Free occupied indices: j, i, k
! Equalities: d == a, c == b, l == j
! No equalities independent of the above can hold.
!
k_aibjbkaj: do k = n0k, n1k
b_aibjbkaj: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0jl)
j_aibjbkaj: do j = j0, n1jl
if (j == k) cycle j_aibjbkaj
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkaj: do a = a0, n1ad
if (a == b) cycle a_aibjbkaj
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkaj: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkaj(t2, nocc, nactive, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkaj
end do a_aibjbkaj
end do j_aibjbkaj
end do b_aibjbkaj
end do k_aibjbkaj
!
! Elementary loop  13
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, l
! Equalities: d == a, c == b, k == i
! No equalities independent of the above can hold.
!
l_aibjbial: do l = n0l, n1l
b_aibjbial: do b = n0bc, n1bc
j_aibjbial: do j = n0j, n1j
if (j == l) cycle j_aibjbial
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbial: do a = a0, n1ad
if (a == b) cycle a_aibjbial
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i1 = min(j - 1, n1ik)
i_aibjbial: do i = n0ik, i1
if (i == j .or. i == l) cycle i_aibjbial
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbial(t2, nocc, nactive, i, b, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbial
end do a_aibjbial
end do j_aibjbial
end do b_aibjbial
end do l_aibjbial
!
! Elementary loop  14
! --------------------
! Free virtual indices: a, b
! Free occupied indices: k, i, j
! Equalities: d == a, c == b, l == k
! No equalities independent of the above can hold.
!
k_aibjbkak: do k = n0kl, n1kl
b_aibjbkak: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkak: do j = j0, n1j
if (j == k) cycle j_aibjbkak
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkak: do a = a0, n1ad
if (a == b) cycle a_aibjbkak
dl = (a - nvirt0) * nocc + (k - nocc0) + 1
i1 = min(j - 1, n1i)
i_aibjbkak: do i = n0i, i1
if (i == j .or. i == k) cycle i_aibjbkak
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkak(t2, nocc, nactive, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkak
end do a_aibjbkak
end do j_aibjbkak
end do b_aibjbkak
end do k_aibjbkak
!
! Elementary loop  15
! --------------------
! Free virtual indices: a, b
! Free occupied indices: i, j, k
! Equalities: d == a, c == b, l == i
! No equalities independent of the above can hold.
!
k_aibjbkai: do k = n0k, n1k
b_aibjbkai: do b = n0bc, n1bc
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
j0 = max(k + 1, n0j)
j_aibjbkai: do j = j0, n1j
if (j == k) cycle j_aibjbkai
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0ad)
a_aibjbkai: do a = a0, n1ad
if (a == b) cycle a_aibjbkai
i1 = min(j - 1, n1il)
i_aibjbkai: do i = n0il, i1
if (i == j .or. i == k) cycle i_aibjbkai
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbkai(t2, nocc, nactive, a, i, b, j, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbkai
end do a_aibjbkai
end do j_aibjbkai
end do b_aibjbkai
end do k_aibjbkai
!
! Elementary loop  16
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == i
! No equalities independent of the above can hold.
!
d_aibibkdi: do d = n0d, n1d
k_aibibkdi: do k = n0k, n1k
b_aibibkdi: do b = n0bc, n1bc
if (b == d) cycle b_aibibkdi
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkdi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibkdi
i0 = max(k + 1, n0ijl)
i_aibibkdi: do i = i0, n1ijl
if (i == k) cycle i_aibibkdi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibibkdi(t2, nocc, nactive, a, i, b, k, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdi
end do a_aibibkdi
end do b_aibibkdi
end do k_aibibkdi
end do d_aibibkdi
!
! Elementary loop  17
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, k
! Equalities: c == b, j == i, l == k
! No equalities independent of the above can hold.
!
d_aibibkdk: do d = n0d, n1d
k_aibibkdk: do k = n0kl, n1kl
dl = (d - nvirt0) * nocc + (k - nocc0) + 1
b_aibibkdk: do b = n0bc, n1bc
if (b == d) cycle b_aibibkdk
ck = (b - nvirt0) * nocc + (k - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibibkdk: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibibkdk
i0 = max(k + 1, n0ij)
i_aibibkdk: do i = i0, n1ij
if (i == k) cycle i_aibibkdk
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
bj = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, i, b, k)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibibkdk(t2, nocc, nactive, a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibibkdk
end do a_aibibkdk
end do b_aibibkdk
end do k_aibibkdk
end do d_aibibkdk
!
! Elementary loop  18
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == j
! No equalities independent of the above can hold.
!
d_aibjbidj: do d = n0d, n1d
b_aibjbidj: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidj
j_aibjbidj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (d - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidj: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidj
i1 = min(j - 1, n1ik)
i_aibjbidj: do i = n0ik, i1
if (i == j) cycle i_aibjbidj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbidj(t2, nocc, nactive, a, i, b, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidj
end do a_aibjbidj
end do j_aibjbidj
end do b_aibjbidj
end do d_aibjbidj
!
! Elementary loop  19
! --------------------
! Free virtual indices: b, a, d
! Free occupied indices: i, j
! Equalities: c == b, k == i, l == i
! No equalities independent of the above can hold.
!
d_aibjbidi: do d = n0d, n1d
b_aibjbidi: do b = n0bc, n1bc
if (b == d) cycle b_aibjbidi
j_aibjbidi: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjbidi: do a = a0, n1a
if (a == b .or. a == d) cycle a_aibjbidi
i1 = min(j - 1, n1ikl)
i_aibjbidi: do i = n0ikl, i1
if (i == j) cycle i_aibjbidi
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (b - nvirt0) * nocc + (i - nocc0) + 1
dl = (d - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, b, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aibjbidi(t2, nocc, nactive, a, i, b, j, d)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjbidi
end do a_aibjbidi
end do j_aibjbidi
end do b_aibjbidi
end do d_aibjbidi
!
! Elementary loop  20
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, l
! Equalities: b == a, d == c, k == j
! No equalities independent of the above can hold.
!
l_aiajcjcl: do l = n0l, n1l
c_aiajcjcl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j_aiajcjcl: do j = n0jk, n1jk
if (j == l) cycle j_aiajcjcl
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajcjcl: do a = a0, n1ab
if (a == c) cycle a_aiajcjcl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjcl: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajcjcl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aiajcjcl(t2, nocc, nactive, a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjcl
end do a_aiajcjcl
end do j_aiajcjcl
end do c_aiajcjcl
end do l_aiajcjcl
!
! Elementary loop  21
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == c, l == j
! No equalities independent of the above can hold.
!
c_aiajckcj: do c = n0cd, n1cd
k_aiajckcj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckcj: do j = n0jl, j1
if (j == k) cycle j_aiajckcj
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(c + 1, n0ab)
a_aiajckcj: do a = a0, n1ab
if (a == c) cycle a_aiajckcj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckcj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckcj(t2, nocc, nactive, a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckcj
end do a_aiajckcj
end do j_aiajckcj
end do k_aiajckcj
end do c_aiajckcj
!
! Elementary loop  22
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, l
! Equalities: b == a, d == c, k == i
! No equalities independent of the above can hold.
!
l_aiajcicl: do l = n0l, n1l
c_aiajcicl: do c = n0cd, n1cd
dl = (c - nvirt0) * nocc + (l - nocc0) + 1
j_aiajcicl: do j = n0j, n1j
if (j == l) cycle j_aiajcicl
a0 = max(c + 1, n0ab)
a_aiajcicl: do a = a0, n1ab
if (a == c) cycle a_aiajcicl
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0ik)
i_aiajcicl: do i = i0, n1ik
if (i == j .or. i == l) cycle i_aiajcicl
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajcicl(t2, nocc, nactive, a, i, j, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcicl
end do a_aiajcicl
end do j_aiajcicl
end do c_aiajcicl
end do l_aiajcicl
!
! Elementary loop  23
! --------------------
! Free virtual indices: a, c
! Free occupied indices: k, i, j
! Equalities: b == a, d == c, l == k
! No equalities independent of the above can hold.
!
c_aiajckck: do c = n0cd, n1cd
k_aiajckck: do k = n0kl, n1kl
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
dl = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckck: do j = n0j, j1
if (j == k) cycle j_aiajckck
a0 = max(c + 1, n0ab)
a_aiajckck: do a = a0, n1ab
if (a == c) cycle a_aiajckck
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckck: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckck
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckck(t2, nocc, nactive, a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckck
end do a_aiajckck
end do j_aiajckck
end do k_aiajckck
end do c_aiajckck
!
! Elementary loop  24
! --------------------
! Free virtual indices: a, c
! Free occupied indices: i, j, k
! Equalities: b == a, d == c, l == i
! No equalities independent of the above can hold.
!
c_aiajckci: do c = n0cd, n1cd
k_aiajckci: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1j)
j_aiajckci: do j = n0j, j1
if (j == k) cycle j_aiajckci
a0 = max(c + 1, n0ab)
a_aiajckci: do a = a0, n1ab
if (a == c) cycle a_aiajckci
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0il)
i_aiajckci: do i = i0, n1il
if (i == j .or. i == k) cycle i_aiajckci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckci(t2, nocc, nactive, a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckci
end do a_aiajckci
end do j_aiajckci
end do k_aiajckci
end do c_aiajckci
!
! Elementary loop  25
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: j, i
! Equalities: d == c, k == j, l == j
! No equalities independent of the above can hold.
!
c_aibjcjcj: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcjcj: do b = b0, n1b
if (b == c) cycle b_aibjcjcj
j_aibjcjcj: do j = n0jkl, n1jkl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjcj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjcj
i_aibjcjcj: do i = n0i, n1i
if (i == j) cycle i_aibjcjcj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjcj(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjcj
end do a_aibjcjcj
end do j_aibjcjcj
end do b_aibjcjcj
end do c_aibjcjcj
!
! Elementary loop  26
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, l == i, k == j
! No equalities independent of the above can hold.
!
c_aibjcjci: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcjci: do b = b0, n1b
if (b == c) cycle b_aibjcjci
j_aibjcjci: do j = n0jk, n1jk
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
ck = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcjci: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcjci
i_aibjcjci: do i = n0il, n1il
if (i == j) cycle i_aibjcjci
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcjci(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcjci
end do a_aibjcjci
end do j_aibjcjci
end do b_aibjcjci
end do c_aibjcjci
!
! Elementary loop  27
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == j
! No equalities independent of the above can hold.
!
c_aibjcicj: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcicj: do b = b0, n1b
if (b == c) cycle b_aibjcicj
j_aibjcicj: do j = n0jl, n1jl
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
dl = (c - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcicj: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcicj
i_aibjcicj: do i = n0ik, n1ik
if (i == j) cycle i_aibjcicj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcicj(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcicj
end do a_aibjcicj
end do j_aibjcicj
end do b_aibjcicj
end do c_aibjcicj
!
! Elementary loop  28
! --------------------
! Free virtual indices: c, a, b
! Free occupied indices: i, j
! Equalities: d == c, k == i, l == i
! No equalities independent of the above can hold.
!
c_aibjcici: do c = n0cd, n1cd
b0 = max(c + 1, n0b)
b_aibjcici: do b = b0, n1b
if (b == c) cycle b_aibjcici
j_aibjcici: do j = n0j, n1j
bj = (b - nvirt0) * nocc + (j - nocc0) + 1
a0 = max(b + 1, n0a)
a_aibjcici: do a = a0, n1a
if (a == b .or. a == c) cycle a_aibjcici
i_aibjcici: do i = n0ikl, n1ikl
if (i == j) cycle i_aibjcici
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
ck = (c - nvirt0) * nocc + (i - nocc0) + 1
dl = (c - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, b, j, c, i)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aibjcici(t2, nocc, nactive, a, i, b, j, c)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aibjcici
end do a_aibjcici
end do j_aibjcici
end do b_aibjcici
end do c_aibjcici
!
! Elementary loop  29
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
a0 = max(c + 1, n0abd)
a_aiajcjal: do a = a0, n1abd
if (a == c) cycle a_aiajcjal
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (l - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajcjal: do i = i0, n1i
if (i == j .or. i == l) cycle i_aiajcjal
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, j)
iket = ketoffset + dl
jac_ibra_iket = v0_eom_cc3_31_trans_aiajcjal(t2, nocc, nactive, a, i, j, c, l)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajcjal
end do a_aiajcjal
end do j_aiajcjal
end do c_aiajcjal
end do l_aiajcjal
!
! Elementary loop  30
! --------------------
! Free virtual indices: a, c
! Free occupied indices: j, i, k
! Equalities: b == a, d == a, l == j
! No equalities independent of the above can hold.
!
c_aiajckaj: do c = n0c, n1c
k_aiajckaj: do k = n0k, n1k
ck = (c - nvirt0) * nocc + (k - nocc0) + 1
j1 = min(k - 1, n1jl)
j_aiajckaj: do j = n0jl, j1
if (j == k) cycle j_aiajckaj
a0 = max(c + 1, n0abd)
a_aiajckaj: do a = a0, n1abd
if (a == c) cycle a_aiajckaj
bj = (a - nvirt0) * nocc + (j - nocc0) + 1
dl = (a - nvirt0) * nocc + (j - nocc0) + 1
i0 = max(j + 1, n0i)
i_aiajckaj: do i = i0, n1i
if (i == j .or. i == k) cycle i_aiajckaj
ai = (a - nvirt0) * nocc + (i - nocc0) + 1
!ibra = braoffset + mu3(ai, bj, ck)
ibra = braoffset + mu3_mem(a, i, a, j, c, k)
iket = ketoffset + dl
jac_ibra_iket = v6_eom_cc3_31_trans_aiajckaj(t2, nocc, nactive, a, i, j, c, k)
call sigup_nondiag(jac_ibra_iket, ibra, iket)
end do i_aiajckaj
end do a_aiajckaj
end do j_aiajckaj
end do k_aiajckaj
end do c_aiajckaj
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
end subroutine ccjac_31_dav_part2
end module ccjac_block_31_dav_part2
